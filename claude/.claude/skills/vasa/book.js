#!/usr/bin/env node
// Books a VASA Fitness class and/or KidCare slot at gympayment.com.
// Credentials: username below (not secret), password pulled from `pass vasa` at runtime.
//
// Usage:
//   node book.js --time "4:30 PM" [--date tomorrow|today|YYYY-MM-DD] [--class "STUDIO RED HIIT"]
//                [--kidcare rodney|miles|both] [--headed] [--confirm]
//
// Dates are always resolved to an explicit YYYY-MM-DD (in America/Chicago) and entered
// via the site's own "Custom date" fields on both pages — the site's relative
// Today/Tomorrow buttons run on a different clock and can be a day off from Alex's
// actual Central-time "today", especially late in the evening. See SKILL.md.
//
// Without --confirm, the script stops just short of the final irreversible click
// (station pick / kidcare reservation confirm) and leaves a screenshot + saved HTML
// so the flow can be verified before it's trusted to run unattended.

const { chromium } = require('playwright');
const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const GYM_USER = process.env.GYM_USER || 'alex@reckerfamily.com';
const LOCATION_ID = 'object:41'; // VASA Fitness Glendale Heights 1063

const CLASS_TYPES = {
  'studio red hiit': 'object:157',
  'studio red': 'object:157',
  'red hiit': 'object:157',
  'red': 'object:157',
  'hiit red': 'object:157',
  'hiit': 'object:157', // Alex's "HIIT" always means STUDIO RED HIIT in practice
  'aqua': 'object:148',
  'cardio': 'object:149',
  'core': 'object:150',
  'cycle': 'object:151',
  'resistance': 'object:153',
  'senior': 'object:154',
  'studio flow': 'object:155',
  'studio lft': 'object:156',
};

const OUT_DIR = process.env.VASA_OUT_DIR || '/tmp/vasa-book';

function parseArgs(argv) {
  const args = { date: 'tomorrow', class: 'STUDIO RED HIIT', headed: false, confirm: false };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === '--headed') args.headed = true;
    else if (a === '--confirm') args.confirm = true;
    else if (a === '--time') args.time = argv[++i];
    else if (a === '--date') args.date = argv[++i];
    else if (a === '--class') args.class = argv[++i];
    else if (a === '--kidcare') args.kidcare = argv[++i];
    else if (a === '--kidcare-duration') args.kidcareDuration = argv[++i];
  }
  return args;
}

function getPassword() {
  return execFileSync('pass', ['vasa'], { encoding: 'utf8' }).split('\n')[0].trim();
}

function log(...msg) {
  console.log(new Date().toISOString().slice(11, 19), ...msg);
}

// Resolves 'today' / 'tomorrow' / an explicit YYYY-MM-DD into YYYY-MM-DD, anchored to
// America/Chicago regardless of what timezone this process is running in.
function resolveDate(dateArg) {
  if (/^\d{4}-\d{2}-\d{2}$/.test(dateArg)) return dateArg;

  const parts = new Intl.DateTimeFormat('en-US', {
    timeZone: 'America/Chicago', year: 'numeric', month: '2-digit', day: '2-digit',
  }).formatToParts(new Date()).reduce((acc, p) => ({ ...acc, [p.type]: p.value }), {});
  const centralToday = new Date(Date.UTC(+parts.year, +parts.month - 1, +parts.day));

  if (dateArg.toLowerCase() === 'tomorrow') centralToday.setUTCDate(centralToday.getUTCDate() + 1);
  else if (dateArg.toLowerCase() !== 'today') {
    throw new Error(`--date must be 'today', 'tomorrow', or YYYY-MM-DD, got "${dateArg}"`);
  }

  return centralToday.toISOString().slice(0, 10);
}

function isoToMDY(iso) {
  const [y, m, d] = iso.split('-');
  return `${m}/${d}/${y}`;
}

async function login(page) {
  const password = getPassword();
  await page.goto('https://gympayment.com/login.aspx?ReturnUrl=%2F', { waitUntil: 'load', timeout: 30000 });
  await page.fill('#UserName', GYM_USER);
  await page.fill('#Password', password);
  await Promise.all([
    page.waitForNavigation({ waitUntil: 'load', timeout: 30000 }).catch(() => {}),
    page.click('#ctl00_Main_Login_Login'),
  ]);
  await page.waitForTimeout(1500);
  if (page.url().includes('login.aspx')) {
    throw new Error('Login failed — still on login page. Check `pass vasa` and the account email.');
  }
  log('Logged in as', GYM_USER);
}

// classes.aspx: pick "Custom" on the date-range select, then fill both native
// type=date inputs (range start/end) with the same day to scope to just that date.
async function selectClassDate(page, isoDate) {
  const selects = await page.$$('select');
  let found = false;
  for (const s of selects) {
    const opts = await s.$$eval('option', os => os.map(o => o.text));
    if (opts.includes('Custom')) {
      await s.selectOption({ label: 'Custom' });
      found = true;
      break;
    }
  }
  if (!found) throw new Error('Could not find the date-range dropdown on classes.aspx');
  await page.waitForTimeout(800);

  const dateInputs = await page.$$('input[type=date]');
  if (dateInputs.length < 2) throw new Error(`Expected 2 date inputs after selecting Custom, found ${dateInputs.length}`);
  await dateInputs[0].fill(isoDate);
  await dateInputs[1].fill(isoDate);
}

// Picks a station in the open #stationsModal: prefer a Treadmill if one's free, and
// among candidates prefer whichever is farthest (grid distance) from any already-booked
// station, so Alex isn't working out shoulder-to-shoulder with a stranger.
async function pickStation(page) {
  const stations = await page.locator('#stationsModal .item-label').evaluateAll(labelNodes =>
    labelNodes.map(label => {
      const tile = label.closest('[title]');
      const style = tile.getAttribute('style') || '';
      const m = style.match(/grid-area:\s*(\d+)\s*\/\s*(\d+)/);
      const title = tile.getAttribute('title') || '';
      return {
        label: label.textContent.trim(),
        row: m ? +m[1] : null,
        col: m ? +m[2] : null,
        available: title === 'Click to select this station',
        booked: title === 'This station is already booked',
      };
    }).filter(s => s.row !== null && (s.available || s.booked))
  );

  const available = stations.filter(s => s.available);
  const booked = stations.filter(s => s.booked);
  if (available.length === 0) {
    throw new Error('No available stations found in the modal — class may be effectively full.');
  }

  const distanceToNearestBooked = (s) => booked.length === 0
    ? Infinity
    : Math.min(...booked.map(b => Math.hypot(s.row - b.row, s.col - b.col)));

  const treadmills = available.filter(s => /treadmill/i.test(s.label));
  const pool = treadmills.length > 0 ? treadmills : available;

  let best = pool[0];
  let bestScore = distanceToNearestBooked(best);
  for (const s of pool.slice(1)) {
    const score = distanceToNearestBooked(s);
    if (score > bestScore) { best = s; bestScore = score; }
  }

  log(`Picking station "${best.label}" (spacing score ${bestScore === Infinity ? 'n/a, class empty' : bestScore.toFixed(1)})`);
  await page.evaluate((chosenLabel) => {
    const labelNode = Array.from(document.querySelectorAll('#stationsModal .item-label'))
      .find(el => el.textContent.trim() === chosenLabel);
    labelNode.closest('[title]').click();
  }, best.label);
}

async function bookClass(page, args) {
  const classKey = args.class.trim().toLowerCase();
  const classValue = CLASS_TYPES[classKey];
  if (!classValue) {
    throw new Error(`Unknown class type "${args.class}". Known: ${Object.keys(CLASS_TYPES).join(', ')}`);
  }
  if (!args.time) throw new Error('--time is required, e.g. --time "4:30 PM"');
  const isoDate = resolveDate(args.date);

  log('Navigating to Class Sign Up...');
  await page.goto('https://gympayment.com/Scheduler/classes.aspx', { waitUntil: 'load', timeout: 30000 });
  await page.waitForTimeout(1500);

  await page.selectOption('#clubs', LOCATION_ID);
  await page.waitForTimeout(1500);
  await page.selectOption('#class-types', classValue);
  await page.waitForTimeout(500);
  await selectClassDate(page, isoDate);
  await page.waitForTimeout(500);

  await page.click('button:has-text("Search")');
  await page.waitForTimeout(2500);

  const row = page.locator('.search-result', { hasText: args.time });
  const count = await row.count();
  if (count === 0) {
    await page.screenshot({ path: path.join(OUT_DIR, 'class-search-no-match.png'), fullPage: true });
    throw new Error(`No "${args.class}" class starting at "${args.time}" found on ${isoDate}. Screenshot saved for review.`);
  }
  const target = row.first();
  const instructor = await target.locator('.class-details .col-lg-3').first().innerText().then(t => t.trim()).catch(() => 'unknown');
  log('Instructor:', instructor);

  log('Found matching class, clicking Sign Up...');
  await target.locator('button:has-text("Sign Up")').first().click();
  await page.waitForTimeout(800);

  // Some class types (plain classes) show an inline Confirm/Cancel; others (station-based,
  // e.g. STUDIO RED HIIT) skip straight to the stations modal. Handle whichever shows up.
  const confirmBtn = target.locator('button:has-text("Confirm")');
  const stationsModal = page.locator('#stationsModal');
  await Promise.race([
    confirmBtn.first().waitFor({ state: 'visible', timeout: 6000 }).catch(() => {}),
    stationsModal.waitFor({ state: 'visible', timeout: 6000 }).catch(() => {}),
  ]);

  if (await confirmBtn.first().isVisible().catch(() => false)) {
    log('Inline confirm step shown, clicking Confirm...');
    await confirmBtn.first().click();
    await page.waitForTimeout(1200);
  }

  if (await stationsModal.isVisible().catch(() => false)) {
    log('Stations modal appeared (the "fill it out" step) — screenshotting for review.');
    await page.screenshot({ path: path.join(OUT_DIR, 'stations-modal.png'), fullPage: true });
    fs.writeFileSync(path.join(OUT_DIR, 'stations-modal.html'), await stationsModal.innerHTML());

    if (!args.confirm) {
      log('Stopping before picking a station and confirming — rerun with --confirm once the station-picking logic is verified against stations-modal.html.');
      return { status: 'paused-at-stations-modal' };
    }

    await pickStation(page);
    await page.waitForTimeout(500);
    await page.locator('#stationsModal button:has-text("Sign Up")').click();
    await page.waitForTimeout(1500);
  }

  await page.screenshot({ path: path.join(OUT_DIR, 'class-booking-result.png'), fullPage: true });
  log('Class booking flow complete.');
  return { status: 'done', instructor };
}

async function bookKidcare(page, args) {
  const childName = args.kidcare.trim().toLowerCase();
  const duration = args.kidcareDuration || '60';
  if (!args.time) throw new Error('--time is required for kidcare, e.g. --time "4:30 PM"');
  const isoDate = resolveDate(args.date);

  log('Navigating to KidCare Sign Up...');
  await page.goto('https://gympayment.com/Scheduler/childcare.aspx', { waitUntil: 'load', timeout: 30000 });
  await page.waitForTimeout(1500);

  await page.selectOption('#location', LOCATION_ID);
  await page.waitForTimeout(1500);

  const names = childName === 'both' ? ['rodney', 'miles'] : [childName];
  for (const name of names) {
    await page.locator('tr', { hasText: new RegExp(name, 'i') }).first().click();
    await page.waitForTimeout(500);
  }

  // "Specified Date" (value 'dates') reveals a text input (name=startDate, MM/DD/YYYY)
  // rather than a native date picker.
  await page.selectOption('#date', 'dates');
  await page.waitForTimeout(800);
  await page.fill('input[name=startDate]', isoToMDY(isoDate));
  await page.keyboard.press('Tab');
  await page.waitForTimeout(500);

  const selects = await page.$$('select');
  for (const s of selects) {
    const opts = await s.$$eval('option', os => os.map(o => o.text));
    if (opts.includes(`${duration} min`)) {
      await s.selectOption({ label: `${duration} min` });
    }
  }
  await page.waitForTimeout(500);

  await page.click('button:has-text("Search")');
  await page.waitForTimeout(2500);

  const panel = page.locator('.panel', { has: page.locator('.panel-heading', { hasText: args.time }) });
  const count = await panel.count();
  if (count === 0) {
    await page.screenshot({ path: path.join(OUT_DIR, 'kidcare-search-no-match.png'), fullPage: true });
    throw new Error(`No KidCare slot starting at "${args.time}" found on ${isoDate}. Screenshot saved for review.`);
  }

  log('Found matching KidCare slot, clicking Reserve...');
  await panel.first().locator('button:has-text("Reserve")').click();
  await page.waitForTimeout(1200);

  await page.screenshot({ path: path.join(OUT_DIR, 'kidcare-confirmation-step.png'), fullPage: true });
  fs.writeFileSync(path.join(OUT_DIR, 'kidcare-confirmation-step.html'), await page.content());

  if (!args.confirm) {
    log('Stopping before finalizing the KidCare reservation — rerun with --confirm once kidcare-confirmation-step.html has been checked.');
    return { status: 'paused-at-confirmation' };
  }

  // Best-effort: click a final confirm/submit/reserve button on whatever loadConfirmation() rendered.
  const finalBtn = page.locator('button:has-text("Confirm"), button:has-text("Submit"), button:has-text("Reserve")').last();
  await finalBtn.click();
  await page.waitForTimeout(1500);

  await page.screenshot({ path: path.join(OUT_DIR, 'kidcare-booking-result.png'), fullPage: true });
  log('KidCare booking flow complete.');
  return { status: 'done' };
}

(async () => {
  const args = parseArgs(process.argv.slice(2));
  fs.mkdirSync(OUT_DIR, { recursive: true });

  const browser = await chromium.launch({ headless: !args.headed });
  const page = await browser.newPage();

  try {
    await login(page);

    if (args.time) {
      const classResult = await bookClass(page, args);
      log('Class result:', JSON.stringify(classResult));
    }

    if (args.kidcare) {
      const kidcareResult = await bookKidcare(page, args);
      log('KidCare result:', JSON.stringify(kidcareResult));
    }
  } catch (err) {
    await page.screenshot({ path: path.join(OUT_DIR, 'error.png'), fullPage: true }).catch(() => {});
    console.error('FAILED:', err.message);
    process.exitCode = 1;
  } finally {
    await browser.close();
  }
})();
