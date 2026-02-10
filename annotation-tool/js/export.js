/**
 * Export Module for Cuneimorph Annotator
 *
 * Handles export in multiple formats:
 * 1. StereoMorph XML (.txt) — direct compatibility with existing R pipeline
 * 2. Cuneimorph JSON — full metadata + coordinates in the project schema
 * 3. TPS format — standard morphometrics interchange
 * 4. CSV — flat table for quick analysis
 */

import { fitControlPoints } from './analysis.js';
import { generateCurvesTab, toLegacyLandmarkId, toLegacyCurveId, fromLegacyLandmarkId, fromLegacyCurveId } from './landmark-protocol.js';

/**
 * Export a single specimen to StereoMorph XML format.
 * This produces files byte-compatible with readShapes() in R.
 */
export function exportStereoMorph(specimen) {
  const { landmarks, curves, nWedges, metadata } = specimen;

  // Separate fixed landmarks (named) from indexed landmarks
  // The StereoMorph format has named landmarks first, then indexed ones
  const namedLandmarks = [];
  const indexedLandmarks = [];

  // Build the landmark list in protocol order
  // Support both new (p_s_01) and legacy (tail_vertex01) naming
  for (let w = 1; w <= nWedges; w++) {
    const pad = String(w).padStart(2, '0');
    const types = [
      { newId: `p_s_${pad}`, legacyId: `tail_vertex${pad}` },
      { newId: `p_l_${pad}`, legacyId: `left_vertex${pad}` },
      { newId: `p_r_${pad}`, legacyId: `right_vertex${pad}` },
      { newId: `p_t_${pad}`, legacyId: `depth_point${pad}` }
    ];
    for (const { newId, legacyId } of types) {
      const coords = landmarks[newId] || landmarks[legacyId];
      if (coords) {
        namedLandmarks.push({
          name: legacyId,
          x: Math.round(coords[0]),
          y: Math.round(coords[1])
        });
      }
    }
  }

  // StereoMorph also stores curve endpoints as indexed landmarks
  // following the curves_tab convention
  const curvesTab = generateCurvesTab(nWedges);
  const allLandmarkNames = namedLandmarks.map(l => l.name);

  // Add indexed curve endpoint references (as in original data format)
  for (const ct of curvesTab) {
    const startLm = allLandmarkNames[ct.start - 1];
    const endLm = allLandmarkNames[ct.end - 1];
    if (startLm && landmarks[startLm]) {
      indexedLandmarks.push({
        name: ` ${ct.start}`,
        x: Math.round(landmarks[startLm][0]),
        y: Math.round(landmarks[startLm][1])
      });
    }
    if (endLm && landmarks[endLm]) {
      indexedLandmarks.push({
        name: ` ${ct.end}`,
        x: Math.round(landmarks[endLm][0]),
        y: Math.round(landmarks[endLm][1])
      });
    }
  }

  // De-duplicate indexed landmarks by index number
  const seenIndices = new Set();
  const uniqueIndexed = indexedLandmarks.filter(l => {
    const idx = l.name.trim();
    if (seenIndices.has(idx)) return false;
    seenIndices.add(idx);
    return true;
  });

  const totalLandmarks = namedLandmarks.length + uniqueIndexed.length;

  // Build XML string
  let xml = '<shapes type=list>\n';
  xml += '\t<scaling type=logical names=FALSE length=1 as.numeric=FALSE >NA</scaling>\n';

  // Landmarks
  xml += `\t<landmarks.pixel type=matrix rownames=TRUE colnames=FALSE nrow=${totalLandmarks} ncol=2 as.numeric=TRUE >\n`;
  for (const lm of namedLandmarks) {
    xml += `\t\t${lm.name}\t${lm.x}\t${lm.y}\n`;
  }
  for (const lm of uniqueIndexed) {
    xml += `\t\t${lm.name}\t${lm.x}\t${lm.y}\n`;
  }
  xml += '\t</landmarks.pixel>\n';

  // Curves - control points
  xml += '\t<curves.control type=list>\n';
  for (const ct of curvesTab) {
    const curveId = ct.name;
    // Try both new-style and legacy-style IDs
    const newStyleId = fromLegacyCurveId(curveId) || curveId;
    const curveData = curves[curveId] || curves[newStyleId];
    if (curveData && curveData.points && curveData.points.length > 0) {
      const controlPts = fitControlPoints(curveData.points, 5);
      xml += `\t\t<${curveId} type=matrix rownames=FALSE colnames=FALSE nrow=5 ncol=2 as.numeric=TRUE >\n`;
      for (const pt of controlPts) {
        xml += `\t\t\t${Math.round(pt[0])}\t${Math.round(pt[1])}\n`;
      }
      xml += `\t\t</${curveId}>\n`;
    }
  }
  xml += '\t</curves.control>\n';

  // Curves - pixel points (dense sampling)
  xml += '\t<curves.pixel type=list>\n';
  for (const ct of curvesTab) {
    const curveId = ct.name;
    const newStyleId2 = fromLegacyCurveId(curveId) || curveId;
    const curveData = curves[curveId] || curves[newStyleId2];
    if (curveData && curveData.points && curveData.points.length > 0) {
      const nPoints = curveData.points.length;
      xml += `\t\t<${curveId} type=matrix rownames=FALSE colnames=FALSE nrow=${nPoints} ncol=2 as.numeric=TRUE >\n`;
      for (const pt of curveData.points) {
        xml += `\t\t\t${Math.round(pt[0])}\t${Math.round(pt[1])}\n`;
      }
      xml += `\t\t</${curveId}>\n`;
    }
  }
  xml += '\t</curves.pixel>\n';

  xml += '</shapes>\n';

  return xml;
}

/**
 * Export a single specimen to Cuneimorph JSON format.
 * Full metadata + morphometric data according to the project schema.
 */
export function exportCuneimorphJSON(specimen) {
  const { landmarks, curves, nWedges, metadata, imageDimensions } = specimen;

  const record = {
    schema_version: '1.0.0',
    specimen_id: metadata.specimen_id || 'unknown',
    object_type: metadata.object_type || 'tablet',
    annotation_level: metadata.annotation_level || 'sign',
    source: {
      image_id: metadata.image_id || '',
      image_filename: metadata.image_filename || '',
      publication: metadata.publication || '',
      cdli_number: metadata.cdli_number || '',
      image_dpi: metadata.image_dpi || null
    },
    chronology: {
      period_broad: metadata.period_broad || 'uncertain',
      period_detail: metadata.period_detail || '',
      date_range_start: metadata.date_range_start ? parseInt(metadata.date_range_start) : null,
      date_range_end: metadata.date_range_end ? parseInt(metadata.date_range_end) : null
    },
    provenance: {
      site: metadata.site || '',
      ancient_name: metadata.ancient_name || '',
      region: metadata.region || '',
      archive_name: metadata.archive_name || '',
      museum: metadata.museum || '',
      museum_number: metadata.museum_number || ''
    },
    text: {
      language: metadata.language || '',
      script_tradition: metadata.script_tradition || '',
      genre: metadata.genre || ''
    },
    scribe: {
      scribe_name: metadata.scribe_name || '',
      attribution_confidence: metadata.attribution_confidence || 'unattributed'
    },
    sign_context: {
      sign_name: metadata.sign_name || '',
      sign_number_borger: metadata.sign_number_borger ? parseInt(metadata.sign_number_borger) : null,
      line_number: metadata.line_number || '',
      n_wedges: nWedges
    },
    annotation: {
      annotator: metadata.annotator || '',
      date: new Date().toISOString().split('T')[0],
      tool_version: 'Cuneimorph Annotator 1.0',
      confidence: metadata.confidence || null,
      notes: metadata.notes || '',
      review_status: 'draft'
    },
    morphometric_data: {
      landmarks: { ...landmarks },
      curves_control: {},
      curves_pixel: {},
      image_dimensions: imageDimensions || null
    }
  };

  // Add curve data
  for (const [id, curve] of Object.entries(curves)) {
    if (curve.points && curve.points.length > 0) {
      record.morphometric_data.curves_control[id] = fitControlPoints(curve.points, 5);
      record.morphometric_data.curves_pixel[id] = curve.points.map(p => [Math.round(p[0]), Math.round(p[1])]);
    }
  }

  return JSON.stringify(record, null, 2);
}

/**
 * Export specimen to TPS format (standard morphometrics interchange).
 */
export function exportTPS(specimen) {
  const { landmarks, metadata, nWedges } = specimen;

  let tps = '';
  tps += `LM=${Object.keys(landmarks).length}\n`;
  for (const [name, coords] of Object.entries(landmarks)) {
    tps += `${coords[0].toFixed(2)} ${coords[1].toFixed(2)}\n`;
  }
  tps += `ID=${metadata.specimen_id || 'unknown'}\n`;
  if (metadata.image_filename) {
    tps += `IMAGE=${metadata.image_filename}\n`;
  }
  tps += `SCALE=1\n`;

  return tps;
}

/**
 * Export multiple specimens to a CSV summary table.
 */
export function exportDatasetCSV(specimens) {
  if (specimens.length === 0) return '';

  // Collect all metadata keys across specimens
  const allKeys = new Set();
  for (const spec of specimens) {
    for (const key of Object.keys(spec.metadata || {})) {
      allKeys.add(key);
    }
  }

  const metaKeys = [...allKeys].sort();
  const header = ['specimen_id', 'n_wedges', 'n_landmarks', 'n_curves', ...metaKeys];
  const rows = [header.join(',')];

  for (const spec of specimens) {
    const row = [
      spec.metadata?.specimen_id || '',
      spec.nWedges || '',
      Object.keys(spec.landmarks || {}).length,
      Object.keys(spec.curves || {}).length,
      ...metaKeys.map(k => {
        const val = spec.metadata?.[k] || '';
        return typeof val === 'string' && val.includes(',') ? `"${val}"` : val;
      })
    ];
    rows.push(row.join(','));
  }

  return rows.join('\n');
}

/**
 * Export the full dataset as a project bundle (JSON).
 * Contains all specimens with full metadata and coordinates.
 */
export function exportProjectBundle(specimens, projectMetadata = {}) {
  const bundle = {
    cuneimorph_version: '1.0.0',
    export_date: new Date().toISOString(),
    project: projectMetadata,
    n_specimens: specimens.length,
    specimens: specimens.map(s => JSON.parse(exportCuneimorphJSON(s)))
  };
  return JSON.stringify(bundle, null, 2);
}

/**
 * Import a StereoMorph XML file and return structured data.
 */
export function importStereoMorph(xmlText) {
  const landmarks = {};
  const curvesControl = {};
  const curvesPixel = {};

  // Parse landmarks.pixel section
  const lmMatch = xmlText.match(/<landmarks\.pixel[^>]*>([\s\S]*?)<\/landmarks\.pixel>/);
  if (lmMatch) {
    const lines = lmMatch[1].trim().split('\n');
    for (const line of lines) {
      const parts = line.trim().split(/\s+/);
      if (parts.length >= 3) {
        const name = parts[0];
        const x = parseFloat(parts[1]);
        const y = parseFloat(parts[2]);
        if (!isNaN(x) && !isNaN(y) && name.includes('_')) {
          landmarks[name] = [x, y];
        }
      }
    }
  }

  // Parse curves.control section
  const ccMatch = xmlText.match(/<curves\.control[^>]*>([\s\S]*?)<\/curves\.control>/);
  if (ccMatch) {
    const curveMatches = ccMatch[1].matchAll(/<(\w+)[^>]*>([\s\S]*?)<\/\1>/g);
    for (const m of curveMatches) {
      const name = m[1];
      const points = m[2].trim().split('\n').map(line => {
        const parts = line.trim().split(/\s+/);
        return [parseFloat(parts[0]), parseFloat(parts[1])];
      }).filter(p => !isNaN(p[0]) && !isNaN(p[1]));
      curvesControl[name] = points;
    }
  }

  // Parse curves.pixel section
  const cpMatch = xmlText.match(/<curves\.pixel[^>]*>([\s\S]*?)<\/curves\.pixel>/);
  if (cpMatch) {
    const curveMatches = cpMatch[1].matchAll(/<(\w+)[^>]*>([\s\S]*?)<\/\1>/g);
    for (const m of curveMatches) {
      const name = m[1];
      const points = m[2].trim().split('\n').map(line => {
        const parts = line.trim().split(/\s+/);
        return [parseFloat(parts[0]), parseFloat(parts[1])];
      }).filter(p => !isNaN(p[0]) && !isNaN(p[1]));
      curvesPixel[name] = points;
    }
  }

  // Convert legacy landmark names to new protocol names
  const convertedLandmarks = {};
  for (const [name, coords] of Object.entries(landmarks)) {
    const newName = fromLegacyLandmarkId(name);
    convertedLandmarks[newName !== name ? newName : name] = coords;
  }

  // Convert legacy curve names
  const convertedCurvesControl = {};
  for (const [name, pts] of Object.entries(curvesControl)) {
    const newName = fromLegacyCurveId(name);
    convertedCurvesControl[newName !== name ? newName : name] = pts;
  }
  const convertedCurvesPixel = {};
  for (const [name, pts] of Object.entries(curvesPixel)) {
    const newName = fromLegacyCurveId(name);
    convertedCurvesPixel[newName !== name ? newName : name] = pts;
  }

  // Infer number of wedges from landmark names
  let maxWedge = 0;
  for (const name of Object.keys(convertedLandmarks)) {
    const match = name.match(/(\d+)$/);
    if (match) maxWedge = Math.max(maxWedge, parseInt(match[1]));
  }

  return { landmarks: convertedLandmarks, curvesControl: convertedCurvesControl, curvesPixel: convertedCurvesPixel, nWedges: maxWedge };
}

/**
 * Import a Cuneimorph JSON file.
 */
export function importCuneimorphJSON(jsonText) {
  const data = JSON.parse(jsonText);
  return {
    landmarks: data.morphometric_data?.landmarks || {},
    curvesControl: data.morphometric_data?.curves_control || {},
    curvesPixel: data.morphometric_data?.curves_pixel || {},
    nWedges: data.sign_context?.n_wedges || 3,
    metadata: {
      specimen_id: data.specimen_id,
      object_type: data.object_type,
      annotation_level: data.annotation_level,
      period_broad: data.chronology?.period_broad,
      site: data.provenance?.site,
      annotator: data.annotation?.annotator,
      ...data.source,
      ...data.chronology,
      ...data.provenance,
      ...data.text,
      ...data.scribe,
      ...data.sign_context,
      notes: data.annotation?.notes
    }
  };
}

/**
 * Trigger a file download in the browser.
 */
export function downloadFile(content, filename, mimeType = 'text/plain') {
  const blob = new Blob([content], { type: mimeType });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}
