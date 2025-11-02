const fs = require('fs');
const path = require('path');
const imageSize = require('image-size');
const { execSync } = require('child_process');

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
  // 2) ImageMagick identify fallback (if installed & width still missing or dpi missing)
  if ((!width || !height || !dpiX || !dpiY) && filePath) {
    try {
      const out = execSync(`identify -format "%w %h %x %y" "${filePath}"`, { stdio: ['ignore','pipe','ignore'] })
        .toString().trim();
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

function walkImages(dir, cb) {
  fs.readdirSync(dir).forEach(file => {
    const full = path.join(dir, file);
    if (fs.statSync(full).isDirectory()) {
      walkImages(full, cb);
    } else if (full.match(/\.(jpg|jpeg|png|tif|tiff)$/i)) {
      cb(full);
    }
  });
}

const root = 'data/processed/images/AlT';
const rows = [];
walkImages(root, filePath => {
  const buf = fs.readFileSync(filePath);
  const dims = readDimsAndDpi(buf, filePath);
  let width_cm = '', height_cm = '';
  if (dims.width && dims.dpiX) width_cm = (dims.width / dims.dpiX * 2.54).toFixed(3);
  if (dims.height && dims.dpiY) height_cm = (dims.height / dims.dpiY * 2.54).toFixed(3);
  rows.push({
    image_path: filePath,
    width_px: dims.width || '',
    height_px: dims.height || '',
    dpi_x: dims.dpiX || '',
    dpi_y: dims.dpiY || '',
    width_cm,
    height_cm,
    dpi_source: dims.dpiSource
  });
});

const outCsv = 'AlT_image_technical_metadata.csv';
const header = 'image_path,width_px,height_px,dpi_x,dpi_y,width_cm,height_cm,dpi_source\n';
fs.writeFileSync(outCsv, header + rows.map(r =>
  [r.image_path, r.width_px, r.height_px, r.dpi_x, r.dpi_y, r.width_cm, r.height_cm, r.dpi_source].join(',')
).join('\n'));
console.log('Wrote:', outCsv);