/**
 * Robust harvester for:
 *  - listing pages (bildausw2.php) that show thumbnail images
 *  - detail pages (mousepic.php) that open when a thumbnail is clicked
 *
 * Behavior:
 *  - If given a listing page, find thumbnail images, and for each:
 *      * attempt to click the thumbnail (catch popups or navigation)
 *      * or follow an available anchor href to the detail page
 *      * when on the detail page, capture network image responses > 10KB
 *  - If given a detail page directly, capture there
 *
 * Input:
 *  - TARGET_URL environment variable contains the target URL (avoids shell/argv truncation)
 *  - argv[0] = outDir, argv[1] = zoomAttempts, argv[2] = viewportW, argv[3] = viewportH, argv[4] = waitMs
 *
 * Output:
 *  - Writes responses into outDir/responses, metadata into outDir/captured_responses.json & .csv
 *
 * Notes:
 *  - Install puppeteer: npm install --no-save puppeteer
 *  - This script prefers clicking a thumbnail so JS can produce full 'xy' params in the detail URL.
 */

const fs = require('fs');
const path = require('path');
const puppeteer = require('puppeteer');

async function waitForMs(pageOrNull, ms) {
  if (pageOrNull && typeof pageOrNull.waitForTimeout === 'function') {
    return pageOrNull.waitForTimeout(ms);
  }
  return new Promise(r => setTimeout(r, ms));
}

function saveMetaCsv(outPath, meta) {
  const header = ['id','url','saved_path','content_type','size_bytes','status','timestamp'];
  const csvLines = [header.join(',')].concat(meta.map(m => [
    m.id,
    `"${(m.url || '').replace(/"/g, '""')}"`,
    `"${(m.saved_path || '').replace(/"/g,'""')}"`,
    m.content_type || '',
    m.size_bytes || 0,
    m.status || '',
    m.timestamp || ''
  ].join(',')));
  fs.writeFileSync(outPath, csvLines.join('\n'));
}

async function captureResponsesOnPage(page, outDir, opts) {
  const responsesDir = path.join(outDir, 'responses');
  fs.mkdirSync(responsesDir, { recursive: true });

  const saved = new Map();
  const meta = [];
  let counter = 0;

  page.on('response', async (response) => {
    try {
      const rurl = response.url();
      if (!rurl || saved.has(rurl)) return;
      const headers = response.headers();
      const ct = (headers['content-type'] || '').toLowerCase();
      const isImageCT = ct.startsWith('image/');
      const isImageExt = rurl.match(/\.(png|jpe?g|gif|webp|tif|tiff)([\?#]|$)/i);
      if (!(isImageCT || isImageExt)) return;
      const status = response.status();
      if (status < 200 || status >= 300) return;
      const buf = await response.buffer().catch(() => null);
      if (!buf || buf.length === 0) return;
      // respect >10KB requirement
      if (buf.length <= 10 * 1024) return;
      counter += 1;
      let ext = 'bin';
      if (isImageCT) {
        if (ct.includes('png')) ext = 'png';
        else if (ct.includes('jpeg') || ct.includes('jpg')) ext = 'jpg';
        else if (ct.includes('gif')) ext = 'gif';
        else if (ct.includes('webp')) ext = 'webp';
        else if (ct.includes('tiff')) ext = 'tif';
      } else if (isImageExt) {
        ext = rurl.split('.').pop().split(/\#|\?/)[0];
      }
      const safeName = `resp_${String(counter).padStart(4,'0')}.${ext}`;
      const outPath = path.join(responsesDir, safeName);
      fs.writeFileSync(outPath, buf);
      saved.set(rurl, outPath);
      meta.push({
        id: counter,
        url: rurl,
        saved_path: outPath,
        content_type: ct,
        size_bytes: buf.length,
        status: status,
        timestamp: new Date().toISOString()
      });
      console.log(`[saved] ${safeName} <- ${rurl} (${ct}, ${buf.length} bytes)`);
    } catch (e) {
      console.warn('response handler error:', e && e.message);
    }
  });

  // attempt zooms / waits to trigger tile loads if needed
  const zoomAttempts = opts.zoomAttempts || 3;
  const waitMs = opts.waitMs || 1200;

  for (let i = 0; i < zoomAttempts; i++) {
    await waitForMs(page, Math.max(waitMs, 800));
    // dispatch wheel at page center
    await page.evaluate(() => {
      const cx = window.innerWidth / 2, cy = window.innerHeight / 2;
      const ev = new WheelEvent('wheel', { deltaY: -200, clientX: cx, clientY: cy, bubbles: true, cancelable: true });
      document.dispatchEvent(ev);
    }).catch(()=>{});
    await waitForMs(page, Math.max(800, waitMs));
  }

  // final stabilization wait
  await waitForMs(page, 1200);

  // write meta files
  const jsonPath = path.join(outDir, 'captured_responses.json');
  fs.writeFileSync(jsonPath, JSON.stringify(meta, null, 2));
  saveMetaCsv(path.join(outDir, 'captured_responses.csv'), meta);

  return meta;
}

// new helper: find explicit mousepic.php links on a listing page
async function findMousepicLinks(page) {
  // return absolute hrefs as strings
  const links = await page.evaluate(() => {
    const out = [];
    Array.from(document.querySelectorAll('a')).forEach(a => {
      try {
        const href = a.getAttribute('href') || a.href || '';
        if (href && href.toLowerCase().includes('mousepic.php')) out.push(href);
      } catch (e){}
    });
    // also check onclick attributes that build mousepic.php urls
    Array.from(document.querySelectorAll('[onclick]')).forEach(el => {
      try {
        const v = el.getAttribute('onclick') || '';
        if (v && v.toLowerCase().includes('mousepic.php')) {
          // crude extraction of string URL from onclick content
          const m = v.match(/(mousepic\\.php[^'")\\s]+)/i);
          if (m && m[0]) out.push(m[0]);
        }
      } catch(e){}
    });
    return Array.from(new Set(out));
  });
  // convert relative to absolute on Node side
  const base = await page.url();
  return links.map(h => {
    try { return new URL(h, base).toString(); } catch(e){ return h; }
  }).filter(Boolean);
}

// patched harvestListing: prefer explicit mousepic.php links, open each and capture
async function harvestListing(page, browser, outDirBase, opts) {
  // try to find full mousepic.php detail links first
  const detailLinks = await findMousepicLinks(page).catch(()=>[]);
  if (detailLinks && detailLinks.length > 0) {
    console.log('Found mousepic.php detail links:', detailLinks.length);
    const results = [];
    for (let i = 0; i < detailLinks.length; i++) {
      const link = detailLinks[i];
      console.log(`Opening detail link ${i+1}/${detailLinks.length}: ${link}`);
      const itemOutDir = path.join(outDirBase, `detail_${String(i+1).padStart(3,'0')}`);
      fs.mkdirSync(itemOutDir, { recursive: true });
      const detailPage = await browser.newPage();
      await detailPage.setViewport({ width: opts.viewportW, height: opts.viewportH });
      try {
        // Use the captureResponsesDuringNavigation so the response handler is attached BEFORE navigation
        const meta = await captureResponsesDuringNavigation(detailPage, link, itemOutDir, opts);
        results.push({ link, responses: meta });
      } catch (e) {
        console.warn('Failed to capture detail link:', link, e && e.message);
      } finally {
        try { await detailPage.close(); } catch(_) {}
      }
    }
    return results;
  }

  // fallback: thumbnail-based harvesting (existing behavior)
  // find thumbnail images and annotate them with data-harvest-idx to reference later
  const thumbs = await page.evaluate(() => {
    const out = [];
    document.querySelectorAll('img').forEach((img, idx) => {
      try {
        const src = img.src || '';
        if (/\/photos\/tn\/|\/tn_/.test(src) || /tn_/.test((img.getAttribute('src')||''))) {
          const a = img.closest('a');
          out.push({
            idx,
            src,
            anchorHref: a ? a.href : null,
            hasOnclick: !!(img.onclick || (a && a.onclick) || img.getAttribute('onclick') || (a && a.getAttribute('onclick')))
          });
        }
      } catch (e) {}
    });
    // write data attributes to DOM for robust clicking later
    out.forEach(o => {
      const imgs = document.querySelectorAll('img');
      if (imgs[o.idx]) imgs[o.idx].setAttribute('data-harvest-idx', String(o.idx));
    });
    return out;
  });

  if (!thumbs || thumbs.length === 0) {
    console.log('No thumbnails detected on listing page.');
    return [];
  }

  console.log('Thumbnails found on listing page:', thumbs.length);

  const allMeta = [];

  for (let i = 0; i < thumbs.length; i++) {
    const t = thumbs[i];
    const selector = `img[data-harvest-idx="${t.idx}"]`;
    const itemOutDir = path.join(outDirBase, `thumb_${String(i+1).padStart(3,'0')}`);
    fs.mkdirSync(itemOutDir, { recursive: true });

    console.log(`Processing thumbnail ${i+1}/${thumbs.length} — selector: ${selector}`);

    // prepare to catch popup or navigation
    let targetPage = null;
    try {
      // attempt click first (so JS handlers that generate full mousepic URL run)
      const popupPromise = page.waitForEvent('popup', { timeout: 5000 }).catch(() => null);
      const navPromise = page.waitForNavigation({ timeout: 5000, waitUntil: 'networkidle2' }).catch(() => null);

      // click the thumbnail
      const clickResult = await page.evaluate((sel) => {
        const el = document.querySelector(sel);
        if (!el) return false;
        el.scrollIntoView({block:'center', inline:'center'});
        el.click();
        return true;
      }, selector);

      const popup = await popupPromise;
      const nav = await navPromise;

      if (popup) {
        targetPage = popup;
        console.log(' - opened in popup/tab');
      } else if (nav) {
        targetPage = page;
        console.log(' - page navigated (same tab)');
      } else {
        // click didn't create navigation/pop-up; try anchorHref if available
        if (t.anchorHref && t.anchorHref.length) {
          const newPage = await browser.newPage();
          await newPage.setViewport({ width: opts.viewportW, height: opts.viewportH });
          await newPage.goto(t.anchorHref, { waitUntil: 'networkidle2', timeout: 60000 }).catch(e => { console.warn('goto anchorHref failed:', e && e.message); });
          targetPage = newPage;
          console.log(' - opened anchor href directly');
        } else {
          console.log(' - could not open detail for this thumbnail (no popup/nav/href)');
        }
      }
    } catch (e) {
      console.warn('Error while trying to open thumbnail detail:', e && e.message);
    }

    if (!targetPage) {
      // nothing to capture
      continue;
    }

    // ensure target page is ready and has a full URL (with xy param if present)
    try {
      await targetPage.bringToFront().catch(()=>{});
      await targetPage.waitForLoadState?.('load')?.catch(()=>{}); // safe if available
    } catch(_) {}

    // allow extra time for JS-driven construction of full URL
    await waitForMs(targetPage, Math.max(1500, opts.waitMs || 1200));

    const actualUrl = targetPage.url ? targetPage.url() : (await (async ()=>{ try { return await targetPage.evaluate(() => location.href); } catch(e){return '';}})());
    console.log(' - detail URL:', actualUrl);

    // run capture on the detail page
    try {
      const meta = await captureResponsesOnPage(targetPage, itemOutDir, opts);
      // attach the detail page URL to meta entries
      meta.forEach(m => { m.detail_page = actualUrl; });
      // append meta to allMeta
      allMeta.push({ thumbIndex: i+1, thumbSrc: t.src, detailUrl: actualUrl, responses: meta });
    } catch (e) {
      console.warn(' - capture failed on detail page:', e && e.message);
    }

    // close popup/new page if it was a separate page
    try {
      if (targetPage !== page) {
        await targetPage.close().catch(()=>{});
      } else {
        // if navigated the main listing page, go back
        const afterGoBack = await page.goBack({ waitUntil: 'networkidle2', timeout: 60000 }).catch(()=>null);
        // re-wait a bit to stabilize and re-annotate thumbnail attributes if needed
        await waitForMs(page, 800);
      }
    } catch (e) {
      // ignore
    }
  }

  return allMeta;
}

// === ADDED: captureResponsesDuringNavigation =====================================
// Attach response handler BEFORE navigating to ensure we catch images requested during page load.
// Returns meta array (same shape as captureResponsesOnPage).
async function captureResponsesDuringNavigation(page, navigateToUrl, outDir, opts) {
  const responsesDir = path.join(outDir, 'responses');
  fs.mkdirSync(responsesDir, { recursive: true });

  const saved = new Map();
  const meta = [];
  let counter = 0;

  function onResponseFactory() {
    return async (response) => {
      try {
        const rurl = response.url();
        if (!rurl || saved.has(rurl)) return;
        const headers = response.headers() || {};
        const ct = (headers['content-type'] || '').toLowerCase();
        const isImageCT = ct.startsWith('image/');
        const isImageExt = !!rurl.match(/\.(png|jpe?g|gif|webp|tif|tiff)([\?#]|$)/i);
        if (!(isImageCT || isImageExt)) return;
        const status = response.status();
        if (status < 200 || status >= 300) return;
        const buf = await response.buffer().catch(() => null);
        if (!buf || buf.length === 0) return;
        // respect >10KB requirement
        if (buf.length <= 10 * 1024) return;
        counter += 1;
        let ext = 'bin';
        if (isImageCT) {
          if (ct.includes('png')) ext = 'png';
          else if (ct.includes('jpeg') || ct.includes('jpg')) ext = 'jpg';
          else if (ct.includes('gif')) ext = 'gif';
          else if (ct.includes('webp')) ext = 'webp';
          else if (ct.includes('tiff')) ext = 'tif';
        } else if (isImageExt) {
          ext = rurl.split('.').pop().split(/\#|\?/)[0];
        }
        const safeName = `resp_${String(counter).padStart(4,'0')}.${ext}`;
        const outPath = path.join(responsesDir, safeName);
        fs.writeFileSync(outPath, buf);
        saved.set(rurl, outPath);
        meta.push({
          id: counter,
          url: rurl,
          saved_path: outPath,
          content_type: ct,
          size_bytes: buf.length,
          status: status,
          timestamp: new Date().toISOString()
        });
        console.log(`[saved] ${safeName} <- ${rurl} (${ct || 'unknown'}, ${buf.length} bytes)`);
      } catch (e) {
        console.warn('response handler error:', e && e.message);
      }
    };
  }

  const onResponse = onResponseFactory();
  page.on('response', onResponse);

  try {
    console.log('Navigating to (capture-started):', navigateToUrl);
    await page.goto(navigateToUrl, { waitUntil: 'networkidle2', timeout: 120000 }).catch(e => { console.warn('goto warning:', e && e.message); });
    await waitForMs(page, Math.max(1500, opts.waitMs || 1200));

    // perform zooms / wheel events to trigger additional tile loads
    const zoomAttempts = opts.zoomAttempts || 3;
    const waitMs = opts.waitMs || 1200;
    for (let i = 0; i < zoomAttempts; i++) {
      await waitForMs(page, Math.max(waitMs, 800));
      await page.evaluate(() => {
        const cx = window.innerWidth / 2, cy = window.innerHeight / 2;
        const ev = new WheelEvent('wheel', { deltaY: -200, clientX: cx, clientY: cy, bubbles: true, cancelable: true });
        document.dispatchEvent(ev);
      }).catch(()=>{});
      await waitForMs(page, Math.max(800, waitMs));
    }

    // extra scroll to trigger lazy loads
    await page.evaluate(async () => {
      const total = Math.max(document.body.scrollHeight, window.innerHeight);
      const steps = 4;
      for (let i = 0; i < steps; i++) {
        window.scrollTo(0, Math.floor(total * (i / (steps - 1))));
        await new Promise(r => setTimeout(r, 300));
      }
      window.scrollTo(0, 0);
    }).catch(()=>{});
    await waitForMs(page, 1000);

    // finalize metadata files
    const jsonPath = path.join(outDir, 'captured_responses.json');
    fs.writeFileSync(jsonPath, JSON.stringify(meta, null, 2));
    saveMetaCsv(path.join(outDir, 'captured_responses.csv'), meta);

    return meta;
  } finally {
    try { page.removeListener('response', onResponse); } catch(e) {}
  }
}

async function main() {
  const argv = process.argv.slice(2);
  const envUrl = process.env.TARGET_URL;
  const url = envUrl && envUrl.length ? envUrl : (argv[0] || null);
  if (!url) {
    console.error('Usage: TARGET_URL="<URL>" node capture_tiles_puppeteer.js [outDir] [zoomAttempts] [viewportW] [viewportH] [waitMs]');
    process.exit(2);
  }
  const outDir = argv[0] || path.join('data','raw','images','puppeteer_capture');
  const zoomAttempts = parseInt(argv[1] || '3', 10);
  const viewportW = parseInt(argv[2] || '1600', 10);
  const viewportH = parseInt(argv[3] || '1200', 10);
  const waitMs = parseInt(argv[4] || '1200', 10);

  console.log('TARGET URL (used):', url);
  console.log('outDir:', outDir, 'zoomAttempts:', zoomAttempts, 'viewport:', viewportW, 'x', viewportH, 'waitMs:', waitMs);

  fs.mkdirSync(outDir, { recursive: true });

  const browser = await puppeteer.launch({
    headless: true,
    args: [
      '--no-sandbox',
      '--disable-setuid-sandbox',
      '--disable-dev-shm-usage',
      '--disable-extensions',
      '--disable-gpu',
      '--hide-scrollbars'
    ]
  });

  try {
    const page = await browser.newPage();
    await page.setViewport({ width: viewportW, height: viewportH, deviceScaleFactor: 1 });

    console.log('Navigating to', url);
    await page.goto(url, { waitUntil: 'networkidle2', timeout: 120000 }).catch(e => { throw new Error('Navigation failed: ' + e.message); });

    // detect whether page looks like a listing (many tn thumbnails) or a detail page
    const isListing = await page.evaluate(() => {
      const imgs = Array.from(document.querySelectorAll('img'));
      const tnCount = imgs.filter(i => (i.src || '').match(/\/photos\/tn\/|\/tn_/i) || (i.getAttribute('src')||'').match(/tn_/i)).length;
      return tnCount >= 2; // heuristic: two or more thumbnails => listing
    }).catch(() => false);

    if (isListing) {
      console.log('Page looks like a listing. Harvesting thumbnails...');
      const listingMeta = await harvestListing(page, browser, outDir, { zoomAttempts, viewportW, viewportH, waitMs });
      // write listing-level metadata
      const listingMetaPath = path.join(outDir, 'listing_harvest.json');
      fs.writeFileSync(listingMetaPath, JSON.stringify(listingMeta, null, 2));
      console.log('Wrote listing metadata to', listingMetaPath);
    } else {
      console.log('Page looks like a detail page. Capturing responses here.');
      const meta = await captureResponsesOnPage(page, outDir, { zoomAttempts, viewportW, viewportH, waitMs });
      const jsonPath = path.join(outDir, 'captured_responses.json');
      fs.writeFileSync(jsonPath, JSON.stringify(meta, null, 2));
      saveMetaCsv(path.join(outDir, 'captured_responses.csv'), meta);
      console.log('Captured', meta.length, 'responses on detail page.');
    }

    console.log('Done.');
    await page.close();
  } finally {
    try { await browser.close(); } catch (e) {}
  }
}

main().catch(err => {
  console.error('ERROR:', err && (err.stack || err.message));
  process.exit(1);
});