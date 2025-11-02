/**
 * Capture image/tile network responses from a viewer page (Puppeteer).
 *
 * Usage:
 *   node analysis/scripts/capture_tiles_puppeteer.js [out_dir] [zoomAttempts] [viewportW] [viewportH] [waitMs]
 *
 * The URL is read from the environment variable TARGET_URL to avoid shell/argv truncation.
 */

const fs = require('fs');
const path = require('path');
const puppeteer = require('puppeteer');

// Compatibility helper for waiting across Puppeteer versions
async function waitForMs(page, ms) {
  if (page && typeof page.waitForTimeout === 'function') {
    return page.waitForTimeout(ms);
  }
  if (page && typeof page.waitFor === 'function') {
    return page.waitFor(ms);
  }
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function main() {
  const argv = process.argv.slice(2);
  // read URL from env first to avoid argv truncation by shells
  const envUrl = process.env.TARGET_URL;
  const url = envUrl && envUrl.length ? envUrl : null;
  if (!url) {
    console.error('Usage: TARGET_URL="<URL>" node capture_tiles_puppeteer.js [out_dir] [zoomAttempts] [viewportW] [viewportH] [waitMs]');
    process.exit(2);
  }
  // argv[0] is outDir when invoked from R (we pass outDir as first arg)
  const outDir = argv[0] || path.join('data', 'raw', 'images', 'puppeteer_capture');
  const zoomAttempts = parseInt(argv[1] || '5', 10);
  const viewportW = parseInt(argv[2] || '1600', 10);
  const viewportH = parseInt(argv[3] || '1200', 10);
  const waitMs = parseInt(argv[4] || '1000', 10);

  console.log('TARGET URL (used):', url);
  fs.mkdirSync(outDir, { recursive: true });
  const responsesDir = path.join(outDir, 'responses');
  fs.mkdirSync(responsesDir, { recursive: true });

  const browser = await puppeteer.launch({
    headless: true,
    args: [
      '--no-sandbox',
      '--disable-setuid-sandbox',
      '--disable-dev-shm-usage',
      '--disable-extensions',
      '--disable-gpu',
      '--hide-scrollbars',
      '--disable-software-rasterizer'
    ]
  });

  try {
    const page = await browser.newPage();
    await page.setViewport({ width: viewportW, height: viewportH, deviceScaleFactor: 2 });

    // keep track of saved URLs to avoid duplicates
    const saved = new Map();
    let counter = 0;
    const meta = [];

    // Network response handler
    page.on('response', async (response) => {
      try {
        const rurl = response.url();
        if (!rurl || saved.has(rurl)) return;
        const headers = response.headers();
        const ct = (headers['content-type'] || '').toLowerCase();
        const isImageCT = ct.startsWith('image/');
        const isImageExt = rurl.match(/\.(png|jpe?g|gif|webp|tif|tiff)([\?#]|$)/i);
        if (isImageCT || isImageExt) {
          const status = response.status();
          if (status < 200 || status >= 300) return;
          const buf = await response.buffer().catch(() => null);
          if (!buf || buf.length === 0) return;
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
          console.log(`[saved] ${safeName} <- ${rurl}  (${ct || 'unknown'}, ${buf.length} bytes)`);
        }
      } catch (e) {
        console.warn('response handler error:', e && e.message);
      }
    });

    // open page and wait for network to settle
    console.log('Navigating to', url);
    await page.goto(url, { waitUntil: 'networkidle2', timeout: 120000 }).catch(e => { throw new Error('Navigation failed: ' + e.message); });
    await waitForMs(page, Math.max(2000, waitMs));

    // perform progressive scroll with larger delays to trigger lazy-loading reliably
    const totalScrollSteps = 12;
    const scrollDelay = Math.max(200, Math.round(Math.max(3000, waitMs * 3) / totalScrollSteps));
    await page.evaluate(async (steps, delay) => {
      for (let i = 0; i < steps; i++) {
        window.scrollTo(0, document.body.scrollHeight * (i / (steps - 1)));
        await new Promise(r => setTimeout(r, delay));
      }
      window.scrollTo(0, 0);
    }, totalScrollSteps, scrollDelay).catch(()=>{});
    await waitForMs(page, Math.max(2000, waitMs));

    // Heuristic: try to identify viewer element to dispatch wheel events to it; fallback to page center
    const viewerSelector = await page.evaluate(() => {
      const candidates = Array.from(document.querySelectorAll('*')).filter(el => {
        const id = (el.id || '').toLowerCase();
        const cls = (el.className || '').toString().toLowerCase();
        return id.includes('viewer') || id.includes('viewport') || cls.includes('viewer') || cls.includes('viewport') || el.tagName.toLowerCase() === 'canvas';
      });
      if (candidates.length) {
        const el = candidates[0];
        if (el.id) return `#${el.id}`;
        if (el.className) return '.' + el.className.split(' ').filter(Boolean)[0];
        return el.tagName.toLowerCase();
      }
      return null;
    });

    console.log('Viewer element selector heuristic:', viewerSelector || '<none found, using page center>');

    async function wheelZoomOnce() {
      if (viewerSelector) {
        await page.evaluate(async (sel) => {
          const el = document.querySelector(sel);
          if (!el) return;
          const rect = el.getBoundingClientRect();
          const cx = rect.left + rect.width / 2;
          const cy = rect.top + rect.height / 2;
          const ev = new WheelEvent('wheel', { deltaY: -200, clientX: cx, clientY: cy, bubbles: true, cancelable: true });
          el.dispatchEvent(ev);
        }, viewerSelector).catch(() => {});
      } else {
        await page.evaluate(() => {
          const cx = window.innerWidth / 2;
          const cy = window.innerHeight / 2;
          const ev = new WheelEvent('wheel', { deltaY: -200, clientX: cx, clientY: cy, bubbles: true, cancelable: true });
          document.dispatchEvent(ev);
        }).catch(() => {});
      }
    }

    for (let i = 0; i < zoomAttempts; i++) {
      console.log(`zoom attempt ${i + 1} / ${zoomAttempts} — waiting ${waitMs}ms, then dispatching wheel`);
      await waitForMs(page, Math.max(waitMs, 1200));
      try {
        const shotPath = path.join(outDir, `screenshot_zoom${String(i+1).padStart(2,'0')}.png`);
        await page.screenshot({ path: shotPath, fullPage: false }).catch(()=>{});
      } catch (_) {}
      await wheelZoomOnce();
      await waitForMs(page, Math.max(1200, waitMs));
    }

    // final wait for pending network requests
    await waitForMs(page, 2000);

    // write metadata file
    const metaPath = path.join(outDir, 'captured_responses.json');
    fs.writeFileSync(metaPath, JSON.stringify(meta, null, 2));
    console.log('Saved response metadata to', metaPath);

    const csvPath = path.join(outDir, 'captured_responses.csv');
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
    fs.writeFileSync(csvPath, csvLines.join('\n'));
    console.log('Saved CSV to', csvPath);

    await page.close();
    console.log('Done. Responses saved to', responsesDir);

  } finally {
    try { await browser.close(); } catch (e) { /* ignore close errors */ }
  }
}

main().catch(err => {
  console.error('ERROR:', err && (err.stack || err.message));
  process.exit(1);
});