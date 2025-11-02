#!/usr/bin/env node
/**
 * Extended image harvester for Hethport (listing + detail pages) with batch + metadata enrichment.
 *
 * Features:
 *  - Accept single URL (--url) OR batch via CSV (--csv with --csv-col column name).
 *  - Accept fund reference links: .../bildpraep.php?fundnr=XYZ -> expanded to listing .../bildausw2.php?n=XYZ
 *  - Extract detail links (mousepic.php) from listings.
 *  - Capture image responses > min_kb; choose largest as primary per detail.
 *  - Extract metadata:
 *      Inventarnummer (fundnr), image_id (bildnr), citation (page text heuristic),
 *      Inventarnummer_url (listing URL), image_url (detail URL),
 *      image_path (final processed path),
 *      image_size_bytes, width_px, height_px, dpi_x, dpi_y, width_cm, height_cm, capture_time.
 *  - Move primary image into data/processed/images/<site>/<Inventarnummer>/<image_id>/<image_id>.<ext>
 *  - Produce <site_name>_image_metadata.csv (and .json).
 *
 * Install deps:
 *   npm install --no-save puppeteer image-size exif-parser csv-parse
 */

const fs = require('fs');
const path = require('path');
const { parse: csvParse } = require('csv-parse/sync');
const puppeteer = require('puppeteer');
const imageSize = require('image-size');
let exifParser;
try { exifParser = require('exif-parser'); } catch { exifParser = null; }
let sharpLib = null;
try { sharpLib = require('sharp'); } catch { /* optional */ }
const { execSync } = require('child_process');
const sleep = ms => new Promise(res => setTimeout(res, ms));

// Ensure constant present (adjust if already defined elsewhere)
const DEFAULT_DPI_FALLBACK = 300;

function parseArgs() {
  const args = process.argv.slice(2);
  const out = {};
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a.startsWith('--')) {
      const key = a.replace(/^--/, '');
      const val = (i + 1 < args.length && !args[i + 1].startsWith('--')) ? args[++i] : 'true';
      out[key] = val;
    }
  }
  return out;
}

function ensureDir(p) { fs.mkdirSync(p, { recursive: true }); }

function expandFundLink(u) {
  // If matches bildpraep.php?fundnr=XYZ -> convert to listing on adwmainz.de
  try {
    const urlObj = new URL(u);
    if (urlObj.pathname.includes('bildpraep.php')) {
      const fund = urlObj.searchParams.get('fundnr');
      if (fund) {
        const decoded = decodeURIComponent(fund);
        // Always use adwmainz.de domain for listing
        return 'https://www.hethport.adwmainz.de/fotarch/bildausw2.php?n=' + encodeURIComponent(decoded);
      }
    }
  } catch {}
  return u;
}

function deriveSiteName(inputSite, firstUrl) {
  if (inputSite) return inputSite;
  try {
    const u = new URL(firstUrl);
    return (u.hostname.replace(/^www\./,'') || 'site')
      .replace(/[^a-z0-9._-]/gi,'_');
  } catch {
    return 'site';
  }
}

function parseCsvColumn(csvPath, col) {
  const raw = fs.readFileSync(csvPath, 'utf8');
  try {
    const records = csvParse(raw, { columns: true, skip_empty_lines: true });
    return records.map(r => (r[col] || '').trim()).filter(Boolean);
  } catch {
    // fallback naive parse
    const lines = raw.split(/\r?\n/).filter(l => l.trim().length);
    const header = lines.shift().split(',');
    const idx = header.indexOf(col);
    if (idx === -1) throw new Error(`Column ${col} not found in CSV.`);
    return lines.map(l => {
      const parts = l.split(',');
      return (parts[idx] || '').trim();
    }).filter(Boolean);
  }
}

function isoNow() { return new Date().toISOString(); }

function sanitizeSegment(s) {
  return (s || '').replace(/[/\\?%*:|"<>]/g, '_').trim() || 'unknown';
}

async function extractDetailLinks(page) {
  const links = await page.evaluate(() => {
    const out = new Set();
    document.querySelectorAll('a[href*="mousepic.php"]').forEach(a => {
      const href = a.getAttribute('href') || a.href;
      if (href) out.add(href);
    });
    // also attempt to parse onclick attributes
    document.querySelectorAll('[onclick]').forEach(el => {
      const oc = el.getAttribute('onclick') || '';
      const m = oc.match(/mousepic\.php[^"' )]+/i);
      if (m) out.add(m[0]);
    });
    return Array.from(out);
  }).catch(() => []);
  const base = page.url();
  return links.map(h => {
    try { return new URL(h, base).toString(); } catch { return h; }
  }).filter(Boolean);
}

async function extractCitation(page) {
  return await page.evaluate(() => {
    const candidates = [];
    const scan = (sel) => document.querySelectorAll(sel).forEach(el => {
      const txt = (el.innerText || '').trim();
      if (txt.match(/Zitier|Citation/i)) candidates.push(txt);
    });
    scan('p');
    scan('div');
    return candidates.sort((a,b)=>a.length-b.length)[0] || '';
  }).catch(()=> '');
}

function parseQueryParam(u, name) {
  try {
    const urlObj = new URL(u);
    return urlObj.searchParams.get(name);
  } catch {
    // fallback regex
    const m = u.match(new RegExp('[?&]' + name + '=([^&#]+)'));
    return m ? decodeURIComponent(m[1]) : null;
  }
}

function inferPrimaryImage(filesMeta) {
  if (!filesMeta.length) return null;
  return filesMeta.slice().sort((a,b)=> b.size_bytes - a.size_bytes)[0];
}

async function captureDetailImages(browser, detailUrl, opts) {
  const page = await browser.newPage();
  await page.setViewport({ width: opts.viewportW, height: opts.viewportH });
  const responsesDir = path.join(opts.tempRoot, 'responses');
  ensureDir(responsesDir);

  const minBytes = opts.min_kb * 1024;
  const saved = new Map();
  const meta = [];
  let counter = 0;

  const handler = async (response) => {
    try {
      const rurl = response.url();
      if (!rurl || saved.has(rurl)) return;
      const headers = response.headers() || {};
      const ct = (headers['content-type'] || '').toLowerCase();
      const status = response.status();
      const isImg = ct.startsWith('image/') || /\.(png|jpe?g|gif|webp|tif|tiff)([\?#]|$)/i.test(rurl);
      if (!isImg || status < 200 || status >= 300) return;
      const buf = await response.buffer().catch(()=>null);
      if (!buf || buf.length <= minBytes) return;
      counter += 1;
      let ext = 'bin';
      if (ct.includes('png')) ext = 'png';
      else if (ct.includes('jpeg') || ct.includes('jpg')) ext = 'jpg';
      else if (ct.includes('gif')) ext = 'gif';
      else if (ct.includes('webp')) ext = 'webp';
      else if (ct.includes('tif')) ext = 'tif';
      else {
        const m = rurl.match(/\.([a-z0-9]+)([\?#]|$)/i);
        if (m) ext = m[1].toLowerCase();
      }
      const fname = `resp_${String(counter).padStart(4,'0')}.${ext}`;
      const fpath = path.join(responsesDir, fname);
      fs.writeFileSync(fpath, buf);
      saved.set(rurl, fpath);
      // Dimension & DPI at capture time
      const dims = readDimsAndDpi(buf, fpath);
      meta.push({
        id: counter,
        url: rurl,
        path: fpath,
        content_type: ct,
        size_bytes: buf.length,
        timestamp: isoNow(),
        width_px: dims.width,
        height_px: dims.height,
        dpi_x: dims.dpiX,
        dpi_y: dims.dpiY,
        dpi_source: dims.dpiSource,
        _buf: buf
      });
      if (opts.verbose) console.log(`[saved] ${fname} <- ${rurl} (${buf.length} bytes)`);
    } catch (e) {
      if (opts.verbose) console.warn('response handler error:', e.message);
    }
  };

  page.on('response', handler);

  try {
    await page.goto(detailUrl, { waitUntil: 'networkidle2', timeout: 120000 });
    await sleep(opts.waitMs);
    await stimulatePage(page, opts.zoomAttempts || 0, Math.min(opts.waitMs, 1500));
  } catch (e) {
    console.warn('Detail navigation failed:', detailUrl, e.message);
  }

  // Allow late loads
  await sleep(800);
  await page.close().catch(()=>{});
  return meta;
}

function readDimsAndDpi(buf, filePath) {
  let width = null, height = null;
  let dpiX = null, dpiY = null, dpiSource = 'none';
  // 1) image-size
  try {
    const dim = imageSize(buf || filePath);
    if (dim && dim.width && dim.height) {
      width = dim.width;
      height = dim.height;
    }
  } catch {}
  // 2) Correct JFIF DPI (APP0) parsing
  try {
    if (buf && buf.length > 20 && buf[0] === 0xFF && buf[1] === 0xD8) {
      let pos = 2;
      while (pos + 9 < buf.length) {
        if (buf[pos] !== 0xFF) break;
        const marker = buf[pos + 1];
        pos += 2;
        if (marker === 0xDA) break; // Start of Scan
        if (pos + 1 >= buf.length) break;
        const segLen = (buf[pos] << 8) + buf[pos + 1];
        if (segLen < 2 || pos + segLen > buf.length) break;
        if (marker === 0xE0 && segLen >= 16) { // APP0
          // Layout: len(2) 'JFIF'(5 incl 0) verMajor(1) verMinor(1) units(1) Xden(2) Yden(2) ...
            const id = buf.slice(pos + 2, pos + 7).toString(); // 'JFIF\0'
            if (id.startsWith('JFIF')) {
              const units   = buf[pos + 7 + 2];      // skip version bytes at pos+7,pos+8
              const xDenPos = pos + 7 + 3;
              const yDenPos = pos + 7 + 5;
              const xDen = (buf[xDenPos] << 8) + buf[xDenPos + 1];
              const yDen = (buf[yDenPos] << 8) + buf[yDenPos + 1];
              if (xDen && yDen) {
                if (units === 1) { // dpi
                  dpiX = xDen; dpiY = yDen; dpiSource = 'jfif';
                } else if (units === 2) { // dpm -> convert
                  dpiX = Math.round(xDen / 39.37);
                  dpiY = Math.round(yDen / 39.37);
                  dpiSource = 'jfif_dpm';
                }
              }
            }
          break;
        }
        pos += segLen;
      }
    }
  } catch {}
  // 3) ImageMagick identify fallback (if installed & width still missing or dpi missing)
  if ((!width || !height || !dpiX || !dpiY) && filePath) {
    try {
      const out = execSync(`identify -format "%w %h %x %y" "${filePath}"`, { stdio: ['ignore','pipe','ignore'] })
        .toString().trim();
      // %x / %y may include units (e.g. 1200 or 1200.000000)
      const parts = out.split(/\s+/);
      if (parts.length >= 4) {
        const w = parseInt(parts[0], 10), h = parseInt(parts[1], 10);
        if (w && h) { width = width || w; height = height || h; }
        const dx = parseFloat(parts[2]), dy = parseFloat(parts[3]);
        if (dx && dy && (!dpiX || !dpiY)) {
          dpiX = dpiX || Math.round(dx);
          dpiY = dpiY || Math.round(dy);
          if (dpiSource === 'none') dpiSource = 'identify';
        }
      }
    } catch {}
  }
  return { width, height, dpiX, dpiY, dpiSource };
}

function enrichPrimaryImage(primaryMeta) {
  if (!primaryMeta) return {};
  let { width_px, height_px, dpi_x, dpi_y, dpi_source } = primaryMeta;
  if (!dpi_source) dpi_source = 'none';
  // Final fallback to ensure dimensions exist
  if ((!width_px || !height_px) && primaryMeta.path) {
    try {
      const dim = imageSize(primaryMeta.path);
      if (dim && dim.width && dim.height) {
        width_px = width_px || dim.width;
        height_px = height_px || dim.height;
      }
    } catch {}
  }
  // Normalize DPI mismatch
  if (dpi_x && dpi_y && (dpi_y > dpi_x * 3 || dpi_x > dpi_y * 3)) {
    const base = Math.min(dpi_x, dpi_y);
    dpi_x = base; dpi_y = base;
    if (dpi_source.startsWith('jfif')) dpi_source = 'jfif_normalized';
  }
  // Apply fallback DPI only if still missing AND we have pixel dims
  if ((!dpi_x || !dpi_y) && width_px && height_px) {
    dpi_x = dpi_x || DEFAULT_DPI_FALLBACK;
    dpi_y = dpi_y || DEFAULT_DPI_FALLBACK;
    if (dpi_source === 'none') dpi_source = 'fallback';
  }
  let width_cm = null, height_cm = null;
  if (width_px && height_px && dpi_x && dpi_y) {
    width_cm  = (width_px  / dpi_x) * 2.54;
    height_cm = (height_px / dpi_y) * 2.54;
  }
  return { width_px, height_px, dpi_x, dpi_y, width_cm, height_cm, dpi_source };
}

function writeUnifiedMetadata(outCsvPath, rows) {
  const header = [
    'Inventarnummer','image_id','citation','Inventarnummer_url','image_url',
    'image_path','image_size_bytes','width_px','height_px','dpi_x','dpi_y',
    'width_cm','height_cm','dpi_source','capture_time'
  ];
  const lines = [header.join(',')];
  rows.forEach(r => {
    const esc = v => {
      if (v === null || v === undefined) return '';
      const s = String(v);
      return /[",\n]/.test(s) ? `"${s.replace(/"/g,'""')}"` : s;
    };
    lines.push(header.map(h => esc(r[h])).join(','));
  });
  fs.writeFileSync(outCsvPath, lines.join('\n'));
  fs.writeFileSync(outCsvPath.replace(/\.csv$/i, '.json'), JSON.stringify(rows, null, 2));
}

// (Optional) helper to stimulate lazy loads
async function stimulatePage(page, zoomAttempts, waitMs) {
  for (let i = 0; i < Math.max(1, zoomAttempts); i++) {
    await sleep(waitMs);
    await page.evaluate(() => {
      const cx = window.innerWidth / 2, cy = window.innerHeight / 2;
      document.dispatchEvent(new WheelEvent('wheel', { deltaY: -250, clientX: cx, clientY: cy, bubbles: true }));
    });
  }
  // Scroll passes
  await page.evaluate(async () => {
    const total = Math.max(document.body.scrollHeight, window.innerHeight);
    const steps = 4;
    for (let i = 0; i < steps; i++) {
      window.scrollTo(0, Math.floor(total * (i / (steps - 1))));
      await new Promise(r => setTimeout(r, 300));
    }
    window.scrollTo(0, 0);
  });
}

function rebuildMetadataFromProcessed(processedRoot, processedImages, unifiedMeta, header, verbose, csvLookup, archiveName) {
  const walkSync = (dir, filelist = []) => {
    fs.readdirSync(dir).forEach(file => {
      const fullPath = path.join(dir, file);
      if (fs.statSync(fullPath).isDirectory()) {
        walkSync(fullPath, filelist);
      } else {
        filelist.push(fullPath);
      }
    });
    return filelist;
  };

  if (!fs.existsSync(processedRoot)) return;
  const files = walkSync(processedRoot).filter(f =>
    f.match(/\.(jpg|jpeg|png|gif|webp|tif|tiff)$/i)
  );

  for (const f of files) {
    // Expect: .../<Inventarnummer>/<image_id>/<image_id>.<ext>
    const rel = path.relative(processedRoot, f);
    const parts = rel.split(path.sep);
    if (parts.length < 3) continue;
    const fundnr = parts[0];
    const image_id = parts[1];
    const imageKey = normKey(fundnr, image_id);
    if (processedImages.has(imageKey)) continue; // already in set

    // Read image size and dimensions
    let size_bytes = 0, width_px = '', height_px = '', dpi_x = '', dpi_y = '', width_cm = '', height_cm = '', dpi_source = '';
    try {
      const buf = fs.readFileSync(f);
      size_bytes = buf.length;
      const dims = imageSize(buf);
      width_px = dims.width;
      height_px = dims.height;
      const dpi = readDimsAndDpi(buf, f);
      dpi_x = dpi.dpiX || '';
      dpi_y = dpi.dpiY || '';
      dpi_source = dpi.dpiSource || '';
      if (width_px && dpi_x) width_cm = (width_px / dpi_x * 2.54).toFixed(3);
      if (height_px && dpi_y) height_cm = (height_px / dpi_y * 2.54).toFixed(3);
    } catch {}

    // Use file's last modified time as capture_time
    let capture_time = '';
    try {
      const stats = fs.statSync(f);
      capture_time = stats.mtime.toISOString();
    } catch {}

    // Lookup Inventarnummer_url, image_url, citation from CSV if available
    let Inventarnummer_url = '', image_url = '', citation = '';
    if (csvLookup && csvLookup[imageKey]) {
      Inventarnummer_url = csvLookup[imageKey].Inventarnummer_url || '';
      image_url = csvLookup[imageKey].image_url || '';
      citation = csvLookup[imageKey].citation || '';
    }
    if (!citation && archiveName && image_id) {
      citation = `hethiter.net/: ${archiveName} ${image_id}`;
    }

    const metaRow = {
      Inventarnummer: fundnr,
      image_id: image_id,
      citation,
      Inventarnummer_url,
      image_url,
      image_path: f,
      image_size_bytes: size_bytes,
      width_px,
      height_px,
      dpi_x,
      dpi_y,
      width_cm,
      height_cm,
      dpi_source,
      capture_time
    };
    unifiedMeta.push(metaRow);
    processedImages.add(imageKey);
    if (verbose) console.log(`[REBUILD] Added metadata for: ${f}`);
  }
}

function removeRedundantSVGs(dir, verbose) {
  if (!fs.existsSync(dir)) return;
  fs.readdirSync(dir).forEach(file => {
    const fullPath = path.join(dir, file);
    if (fs.statSync(fullPath).isDirectory()) {
      removeRedundantSVGs(fullPath, verbose);
      // Remove empty folders
      if (fs.readdirSync(fullPath).length === 0) {
        fs.rmdirSync(fullPath);
        if (verbose) console.log(`[CLEAN] Removed empty folder: ${fullPath}`);
      }
    } else if (file.toLowerCase().endsWith('.svg')) {
      fs.unlinkSync(fullPath);
      if (verbose) console.log(`[CLEAN] Removed SVG: ${fullPath}`);
    }
  });
}

function copyRawToProcessedIfMissing(rawRoot, processedRoot, verbose) {
  // For each processed/<site>/<Inventarnummer>/<image_id> folder, if no jpg/png, but raw exists, copy it
  if (!fs.existsSync(processedRoot)) return;
  fs.readdirSync(processedRoot).forEach(inv => {
    const invDir = path.join(processedRoot, inv);
    if (!fs.statSync(invDir).isDirectory()) return;
    fs.readdirSync(invDir).forEach(imgid => {
      const imgDir = path.join(invDir, imgid);
      if (!fs.statSync(imgDir).isDirectory()) return;
      const hasImage = fs.readdirSync(imgDir).some(f => f.match(/\.(jpg|jpeg|png)$/i));
      if (!hasImage) {
        // Try to find in raw
        const rawDir = path.join(rawRoot, inv, imgid);
        if (fs.existsSync(rawDir)) {
          const rawImg = fs.readdirSync(rawDir).find(f => f.match(/\.(jpg|jpeg|png)$/i));
          if (rawImg) {
            const src = path.join(rawDir, rawImg);
            const dest = path.join(imgDir, rawImg);
            fs.copyFileSync(src, dest);
            if (verbose) console.log(`[COPY] Copied ${src} -> ${dest}`);
          }
        }
      }
    });
  });
}

function prompt(question) {
  const readline = require('readline-sync');
  return readline.question(question);
}

// Build a lookup map from CSV for Inventarnummer|image_id -> {Inventarnummer_url, image_url}
function buildCsvLookup(csvPath) {
  if (!csvPath || !fs.existsSync(csvPath)) return {};
  const raw = fs.readFileSync(csvPath, 'utf8');
  let records = [];
  try {
    records = csvParse(raw, { columns: true, skip_empty_lines: true });
  } catch {
    return {};
  }
  const map = {};
  records.forEach(r => {
    const key = (r['Inventarnummer'] || r['fundnr'] || '').trim() + '|' + (r['image_id'] || r['bildnr'] || '').trim();
    map[key] = {
      Inventarnummer_url: r['Inventarnummer_url'] || r['fundnr_url'] || '',
      image_url: r['image_url'] || ''
    };
  });
  return map;
}

function normKey(fundnr, bildnr) {
  return (fundnr || '')
    .replace(/_/g, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .toLowerCase() +
    '|' +
    (bildnr || '')
      .replace(/_/g, ' ')
      .replace(/\s+/g, ' ')
      .trim()
      .toLowerCase();
}

(async () => {
  const argv = parseArgs();
  const inputUrl = argv.url;
  const csvPath = argv.csv;
  const csvCol  = argv['csv-col'];
  const siteArg = argv.site;
  const zoomAttempts = parseInt(argv.zoomAttempts || '3', 10);
  const viewportW = parseInt(argv.viewportW || '1600', 10);
  const viewportH = parseInt(argv.viewportH || '1200', 10);
  const waitMs = parseInt(argv.waitMs || '1200', 10);
  const min_kb = parseInt(argv.min_kb || '10', 10);
  const verbose = argv.verbose !== 'false';

  if (!inputUrl && !csvPath) {
    console.error('Provide --url or --csv with --csv-col');
    process.exit(2);
  }
  let seeds = [];
  if (inputUrl) seeds.push(inputUrl.trim());
  if (csvPath) {
    if (!csvCol) {
      console.error('When using --csv you must specify --csv-col');
      process.exit(2);
    }
    const rows = parseCsvColumn(csvPath, csvCol);
    seeds = seeds.concat(rows);
  }
  seeds = Array.from(new Set(seeds.filter(Boolean)));

  const siteName = deriveSiteName(siteArg, seeds[0]);
  const rawRoot = argv.raw_root || path.join('data','raw','images', siteName);
  const processedRoot = argv.processed_root || path.join('data','processed','images', siteName);
  ensureDir(rawRoot);
  ensureDir(processedRoot);

  console.log('Site:', siteName);
  console.log('Seeds:', seeds.length);
  console.log('Raw root:', rawRoot);
  console.log('Processed root:', processedRoot);

  const browser = await puppeteer.launch({
    headless: true,
    args: [
      '--no-sandbox','--disable-setuid-sandbox','--disable-dev-shm-usage',
      '--disable-extensions','--disable-gpu'
    ]
  });

  const unifiedMeta = [];

  // --- SKIP LOGIC: Build processedImages set from metadata CSV ---
  const metaOutCsv = path.join(processedRoot, `${siteName}_image_metadata.csv`);
  let processedImages = new Set();
  if (fs.existsSync(metaOutCsv)) {
    const metaRaw = fs.readFileSync(metaOutCsv, 'utf8');
    const lines = metaRaw.split(/\r?\n/).slice(1); // skip header
    for (const line of lines) {
      if (!line.trim()) continue;
      const parts = line.split(',');
      if (parts.length >= 2) {
        processedImages.add(normKey(parts[0], parts[1]));
      }
    }
  }

  // Prompt for archive name if not provided
  let archiveName = argv.archive_name;
  if (!archiveName) {
    archiveName = prompt('Enter archive name for citation (e.g. "Alalah-Archiv"): ');
  }

  // Build CSV lookup for Inventarnummer_url and image_url
  let csvLookup = {};
  if (fs.existsSync(metaOutCsv)) {
    csvLookup = buildCsvLookup(metaOutCsv);
  } else if (csvPath) {
    csvLookup = buildCsvLookup(csvPath);
  }

  // Clean up SVGs and empty folders before processing
  removeRedundantSVGs(processedRoot, verbose);

  // Copy any missing images from raw to processed
  copyRawToProcessedIfMissing(rawRoot, processedRoot, verbose);

  // Rebuild metadata from processed images before downloading
  const header = [
    'Inventarnummer','image_id','citation','Inventarnummer_url','image_url',
    'image_path','image_size_bytes','width_px','height_px','dpi_x','dpi_y',
    'width_cm','height_cm','dpi_source','capture_time'
  ];
  rebuildMetadataFromProcessed(processedRoot, processedImages, unifiedMeta, header, verbose, csvLookup, archiveName);
  writeUnifiedMetadata(metaOutCsv, unifiedMeta);

  for (let si = 0; si < seeds.length; si++) {
    const seed = seeds[si];
    const listingOut = path.join(rawRoot, `seed_${String(si+1).padStart(3,'0')}`);
    ensureDir(listingOut);

    let page;
    try {
      page = await browser.newPage();
      await page.goto(seed, { waitUntil: 'networkidle2', timeout: 120000 });
    } catch (e) {
      console.warn('Navigation failed for seed:', seed, e.message);
      if (page) await page.close().catch(()=>{});
      continue;
    }

    const actualListingUrl = page.url();
    const isDetail = /mousepic\.php/i.test(actualListingUrl);

    let detailLinks = [];
    if (isDetail) {
      detailLinks = [actualListingUrl];
    } else {
      try {
        detailLinks = await page.evaluate(() => {
          return Array.from(document.querySelectorAll('a')).map(a => a.href).filter(h => h && h.includes('mousepic.php'));
        });
        if (verbose) console.log('Detail links found:', detailLinks.length);
      } catch (e) {
        console.warn('Failed to extract detail links:', e.message);
        detailLinks = [];
      }
    }
    await page.close().catch(()=>{});

    for (let di = 0; di < detailLinks.length; di++) {
      const dlink = detailLinks[di];
      const fundnr = parseQueryParam(dlink, 'fundnr');
      const bildnr = parseQueryParam(dlink, 'bildnr');
      const imageKey = normKey(fundnr, bildnr);

      // --- Robust skip logic with image_url update ---
      if (processedImages.has(imageKey)) {
        if (verbose) console.log(`[SKIP] Already processed image: ${imageKey}`);
        // Update missing fields in unifiedMeta
        for (let i = 0; i < unifiedMeta.length; i++) {
          const row = unifiedMeta[i];
          if (normKey(row.Inventarnummer, row.image_id) === imageKey) {
            // Always update these fields from the current context
            row.image_url = dlink;
            row.Inventarnummer_url = actualListingUrl;
            if (!row.citation && archiveName && bildnr) row.citation = `hethiter.net/: ${archiveName} ${bildnr}`;
            // Always update technical fields from file
            if (row.image_path && fs.existsSync(row.image_path)) {
              try {
                const buf = fs.readFileSync(row.image_path);
                const dims = imageSize(buf);
                row.width_px = dims.width || '';
                row.height_px = dims.height || '';
                const dpi = readDimsAndDpi(buf, row.image_path);
                row.dpi_x = dpi.dpiX || '';
                row.dpi_y = dpi.dpiY || '';
                row.dpi_source = dpi.dpiSource || '';
                row.width_cm = (row.width_px && row.dpi_x) ? (row.width_px / row.dpi_x * 2.54).toFixed(3) : '';
                row.height_cm = (row.height_px && row.dpi_y) ? (row.height_px / row.dpi_y * 2.54).toFixed(3) : '';
                const stats = fs.statSync(row.image_path);
                row.capture_time = stats.mtime.toISOString();
              } catch (e) {
                if (verbose) console.warn(`[WARN] Could not update technical fields for: ${row.image_path} (${e.message})`);
              }
            }
            break;
          }
        }
        continue;
      }

      // Predict processed image path and skip if file exists
      const invSeg = sanitizeSegment(fundnr || 'no_fund');
      const imgSeg = sanitizeSegment(bildnr || 'no_bild');
      const procDir = path.join(processedRoot, invSeg, imgSeg);
      ensureDir(procDir);

      let ext = 'jpg';
      if (dlink.match(/\.(png|jpe?g|gif|webp|tif|tiff|svg)([\?#]|$)/i)) {
        ext = dlink.match(/\.(png|jpe?g|gif|webp|tif|tiff|svg)([\?#]|$)/i)[1];
      }
      if (ext === 'svg') {
        if (verbose) console.log(`[SKIP] SVG file: ${dlink}`);
        continue;
      }
      const finalPath = path.join(procDir, `${imgSeg}.${ext}`);

      if (fs.existsSync(finalPath)) {
        if (verbose) console.log(`[SKIP] Image file already exists: ${finalPath}`);
        processedImages.add(imageKey);
        // Also update image_url for this case
        for (let i = 0; i < unifiedMeta.length; i++) {
          const row = unifiedMeta[i];
          if (normKey(row.Inventarnummer, row.image_id) === imageKey) {
            if (!row.image_url) {
              row.image_url = dlink;
              if (verbose) console.log(`[UPDATE] Added image_url for skipped image: ${imageKey}`);
            }
            break;
          }
        }
        continue;
      }

      console.log(`  [Detail ${di+1}/${detailLinks.length}] ${dlink}`);
      const tempRoot = path.join(listingOut, `detail_${String(di+1).padStart(3,'0')}`);
      ensureDir(tempRoot);

      const filesMeta = await captureDetailImages(browser, dlink, {
        viewportW, viewportH, waitMs, min_kb, verbose, tempRoot, zoomAttempts
      });

      if (!filesMeta.length) {
        if (verbose) console.log('    No qualifying images captured.');
        continue;
      }

      const primary = inferPrimaryImage(filesMeta);
      const { width_px, height_px, dpi_x, dpi_y, width_cm, height_cm, dpi_source } = enrichPrimaryImage(primary);

      const citation = await (async () => {
        let p;
        try {
          p = await browser.newPage();
          await p.setViewport({ width: 800, height: 600 });
          await p.goto(dlink, { waitUntil: 'domcontentloaded', timeout: 60000 });
          return await extractCitation(p);
        } catch {
          return '';
        } finally {
          if (p) try { await p.close(); } catch {}
        }
      })();

      // Move/copy primary into processed structure
      if (primary && primary.path && finalPath) {
        fs.copyFileSync(primary.path, finalPath);
      }

      // Compose metadata row
      const citationFormatted = `hethiter.net/: ${archiveName} ${bildnr || ''}`.trim();
      const metaRow = {
        Inventarnummer: fundnr || '',
        image_id: bildnr || '',
        citation: citationFormatted,
        Inventarnummer_url: seed,
        image_url: dlink,
        image_path: finalPath,
        image_size_bytes: primary.size_bytes,
        width_px: width_px ?? '',
        height_px: height_px ?? '',
        dpi_x: dpi_x ?? '',
        dpi_y: dpi_y ?? '',
        width_cm: width_cm != null ? width_cm.toFixed(3) : '',
        height_cm: height_cm != null ? height_cm.toFixed(3) : '',
        dpi_source: dpi_source || '',
        capture_time: primary.timestamp
      };

      // Remove any partial row for this imageKey
      for (let i = unifiedMeta.length - 1; i >= 0; i--) {
        if (normKey(unifiedMeta[i].Inventarnummer, unifiedMeta[i].image_id) === imageKey) {
          unifiedMeta.splice(i, 1);
        }
      }
      unifiedMeta.push(metaRow);
      processedImages.add(imageKey);

      // Drop buffer to reduce memory
      delete primary._buf;
    }

    // Auto-save metadata after each seed
    writeUnifiedMetadata(metaOutCsv, unifiedMeta);
  }

  // Write unified metadata (from this run) BEFORE the rebuild phase
  ensureDir(processedRoot);
  writeUnifiedMetadata(metaOutCsv, unifiedMeta);
  console.log('Wrote metadata:', metaOutCsv);
  console.log('Total records:', unifiedMeta.length);

  // --- REBUILD LOGIC: If enabled, scan processed images and rebuild metadata CSV ---
  if (argv.rebuild_metadata === 'true') {
    const rebuildStart = Date.now();
    const rebuildMeta = [];
    const header = [
      'Inventarnummer','image_id','citation','Inventarnummer_url','image_url',
      'image_path','image_size_bytes','width_px','height_px','dpi_x','dpi_y',
      'width_cm','height_cm','dpi_source','capture_time'
    ];
    rebuildMetadataFromProcessed(processedRoot, new Set(), rebuildMeta, header, verbose, csvLookup, archiveName);
    writeUnifiedMetadata(metaOutCsv, rebuildMeta);
    console.log('Rebuilt metadata from processed images:', rebuildMeta.length, 'records');
    const dur = Date.now() - rebuildStart;
    console.log('Rebuild duration: ' + (dur / 1000).toFixed(1) + ' seconds');
  }

  if (argv.inspect_existing === 'true') {
    console.log('Inspecting all existing images for technical metadata...');
    let updated = 0;
    for (const row of unifiedMeta) {
      if (row.image_path && fs.existsSync(row.image_path)) {
        try {
          const buf = fs.readFileSync(row.image_path);
          const dims = imageSize(buf);
          row.width_px = dims.width || '';
          row.height_px = dims.height || '';
          const dpi = readDimsAndDpi(buf, row.image_path);
          row.dpi_x = dpi.dpiX || '';
          row.dpi_y = dpi.dpiY || '';
          row.dpi_source = dpi.dpiSource || '';
          row.width_cm = (row.width_px && row.dpi_x) ? (row.width_px / row.dpi_x * 2.54).toFixed(3) : '';
          row.height_cm = (row.height_px && row.dpi_y) ? (row.height_px / row.dpi_y * 2.54).toFixed(3) : '';
          const stats = fs.statSync(row.image_path);
          row.capture_time = stats.mtime.toISOString();
          updated++;
        } catch (e) {
          if (verbose) console.warn(`[WARN] Could not inspect image: ${row.image_path} (${e.message})`);
        }
      }
    }
    writeUnifiedMetadata(metaOutCsv, unifiedMeta);
    console.log(`Updated technical metadata for ${updated} images.`);
  }

  await browser.close();
  console.log('Done.');
})().catch(err => {
  console.error('FATAL:', err && (err.stack || err.message));
  process.exit(1);
});