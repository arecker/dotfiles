#!/usr/bin/env python3
import json
import sys
import urllib.request
import urllib.parse

HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; steam-lookup/1.0)"}


def fetch_json(url):
    req = urllib.request.Request(url, headers=HEADERS)
    with urllib.request.urlopen(req, timeout=10) as resp:
        return json.load(resp)


def resolve_appid(query):
    if query.isdigit():
        return query, None
    url = "https://store.steampowered.com/api/storesearch/?" + urllib.parse.urlencode(
        {"term": query, "l": "english", "cc": "US"}
    )
    data = fetch_json(url)
    items = data.get("items") or []
    if not items:
        sys.exit(f'No Steam store results for "{query}"')
    return str(items[0]["id"]), items[0]["name"]


def get_appdetails(appid):
    url = f"https://store.steampowered.com/api/appdetails?appids={appid}&cc=US&l=english"
    data = fetch_json(url)
    entry = data.get(str(appid), {})
    if not entry.get("success"):
        return None
    return entry["data"]


def get_reviews(appid):
    url = (
        f"https://store.steampowered.com/appreviews/{appid}"
        "?json=1&language=all&purchase_type=all&num_per_page=0"
    )
    data = fetch_json(url)
    return data.get("query_summary")


def get_protondb(appid):
    url = f"https://www.protondb.com/api/v1/reports/summaries/{appid}.json"
    try:
        return fetch_json(url)
    except urllib.error.HTTPError:
        return None


def main():
    if len(sys.argv) < 2:
        sys.exit("usage: lookup.py <game name or steam appid>")
    query = " ".join(sys.argv[1:])

    appid, matched_name = resolve_appid(query)
    if matched_name:
        print(f'Matched "{query}" -> {matched_name} (appid {appid})\n')

    details = get_appdetails(appid)
    if details is None:
        sys.exit(f"appdetails lookup failed for appid {appid}")

    print(f"== {details.get('name')} (appid {appid}) ==")
    price = details.get("price_overview")
    if details.get("is_free"):
        print("Price: Free")
    elif price:
        print(f"Price: {price.get('final_formatted')}")
    else:
        print("Price: unknown / not available in US store")

    print(f"Genres: {', '.join(g['description'] for g in details.get('genres', []))}")
    print(f"Controller support: {details.get('controller_support', 'none listed')}")
    print(f"Required age: {details.get('required_age', 0)}")

    descriptors = details.get("content_descriptors") or {}
    notes = descriptors.get("notes")
    print(f"Mature content notes: {notes if notes else 'none listed'}")

    desc = details.get("short_description")
    if desc:
        print(f"\n{desc}\n")

    reviews = get_reviews(appid)
    if reviews:
        print(
            f"Reviews: {reviews.get('review_score_desc')} "
            f"({reviews.get('total_positive')} positive / {reviews.get('total_negative')} negative)"
        )

    proton = get_protondb(appid)
    if proton:
        print(
            f"ProtonDB (Steam Deck/Linux): {proton.get('tier')} "
            f"(confidence: {proton.get('confidence')}, {proton.get('total')} reports)"
        )
    else:
        print("ProtonDB: no reports found")


if __name__ == "__main__":
    main()
