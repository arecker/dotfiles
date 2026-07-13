---
name: vasa
description: Book VASA Fitness classes and KidCare (childcare) slots on gympayment.com for Alex from a plain-language request, e.g. "grab the 4:30 RED class tomorrow and book Rodney for kid care". Use whenever Alex asks to sign up for a class, HIIT / STUDIO RED, or reserve KidCare at VASA / gympayment.com.
---

# VASA class + KidCare booking

Drives `book.js` (Playwright) against gympayment.com. It logs in, finds the
matching class and/or KidCare slot, and books it.

## What you already know (don't ask again unless the request contradicts it)

- **Home location**: VASA Fitness Glendale Heights — hardcoded in the script, no need to ask.
- **Usual class**: whatever Alex calls it ("HIIT", "HIIT RED", "STUDIO RED", "RED") means
  **STUDIO RED HIIT**. The script's alias map already handles this.
- **Kids**: Rodney (10) and Miles (6). The member portal has a data glitch that lists
  "Miles Recker" twice (identical duplicate row) and Rodney once — harmless, the script
  matches by name so it's not visible in practice.
- **Usual pattern**: Alex books Rodney for KidCare; his wife books Miles separately.
  Default to `--kidcare rodney` unless Alex says otherwise or asks for both.
- **KidCare duration**: always 60 minutes, matching the 1-hour classes. Don't ask.
- **Station preference**: in the "Available Stations" modal, Alex prefers a Treadmill,
  and generally wants space between himself and other people already booked into
  stations — not right next to a stranger. `pickStation()` in `book.js` already handles
  this (prefers Treadmill, then maximizes grid distance from booked stations); no need
  to ask about it per-request.
- **Credentials**: username is `alex@reckerfamily.com`; the password comes from
  `pass vasa` at runtime inside `book.js` itself — never fetch or print it yourself.

## Setup (once)

```
cd ~/.claude/skills/vasa
[ -d node_modules ] || npm install
npx playwright install chromium   # only if not already installed
```

## Resolving dates

`book.js` resolves `--date` (`today`, `tomorrow`, or an explicit `YYYY-MM-DD`) internally
against **America/Chicago**, not the ambient environment clock (which is UTC and can
already be "tomorrow" there during Central evenings). It then always drives the site's
own "Custom date" / "Specified Date" fields with that explicit date, rather than clicking
the site's relative Today/Tomorrow buttons — those run on a different clock than Central
time and were confirmed to be a full day off on 2026-07-13 late in the evening. So: pass
`today`/`tomorrow` for convenience, or compute the exact date yourself (e.g. from
"Wednesday") and pass `--date YYYY-MM-DD` — both are equally reliable now.

Still worth a sanity check before `--confirm`: the resolved date is echoed in the
script's log line and visible on the confirmation screenshots ("Search Results for ...
on Wednesday, July 15" / the KidCare confirm dialog's Date column) — glance at it once
per new date to make sure the day-of-week matches what Alex actually asked for.

## Interpreting a request

Parse out:
- `--time` — the class start time, e.g. `"4:30 PM"`. Required for a class booking.
- `--date` — `today`, `tomorrow`, or `YYYY-MM-DD` (default `tomorrow`). For a named day
  like "Wednesday", compute the actual date in Central time yourself and pass it explicitly.
- `--class` — defaults to `"STUDIO RED HIIT"`, only needed if Alex asks for a different class type.
- `--kidcare` — `rodney`, `miles`, or `both`. Omit entirely if no childcare is needed.

Example: "can you grab the 4:30 RED class tomorrow and book Rodney for kid care during that time"
becomes:

```
node book.js --time "4:30 PM" --date tomorrow --kidcare rodney
```

"4:30 on Wednesday, book Rodney" becomes (after computing Wednesday's date):

```
node book.js --time "4:30 PM" --date 2026-07-15 --kidcare rodney
```

## Running it

Run from `~/.claude/skills/vasa`. **First run for any given booking should omit `--confirm`.**
The script deliberately stops one click short of the irreversible step:

- For classes: right before picking a workout station in the "Available Stations" modal
  (this is the step Alex means by "it always makes me fill it out").
- For KidCare: right before finalizing the reservation confirmation screen.

At that pause point it writes a screenshot and the relevant HTML to `/tmp/vasa-book/`.
**Read the screenshot back to Alex (or describe it) and get an explicit go-ahead before
rerunning with `--confirm`.** Once the flow has been verified once for a given
class/kidcare combination, it's reasonable to trust `--confirm` on subsequent identical
requests — but always report what was booked afterward (time, class, which kid) and
point at the final screenshot in `/tmp/vasa-book/` so Alex can double check.

If the script errors (no matching class/slot found, login failure, etc.), it saves a
screenshot to `/tmp/vasa-book/error.png` — check it before retrying blindly, the class
might just not be on the schedule at that time.

## Known limitations

- The station-picking (stations modal, `[title="Click to select this station"]`) and
  KidCare-confirmation flows have been verified against a real live booking
  (2026-07-13, STUDIO RED HIIT + Rodney's KidCare). If either stops matching — e.g. no
  stations modal appears, or the confirm button selector doesn't match — the site's
  markup has likely changed; check the saved screenshot/HTML in `/tmp/vasa-book/` and
  update `book.js` rather than guessing.
- Only tested at Glendale Heights with STUDIO RED HIIT. Other class types share the same
  UI patterns but haven't been run end-to-end.
