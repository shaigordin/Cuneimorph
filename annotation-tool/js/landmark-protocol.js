/**
 * Cuneimorph Landmark Protocol Definition
 *
 * Terminology follows Cammarosano, Muller, Fisseler & Weichert (2014)
 * "Schriftmetrologie des Keils" (Table 1 / Abbildung 1), translated to English.
 *
 * The wedge (Keil) is modelled as an idealized tetrahedron with:
 *   - 4 Ecken (vertices/points): p_s, p_l, p_r, p_t
 *   - 6 Kanten (edges): 3 inner (e_s, e_l, e_r) + 3 outer (e_sa, e_la, e_ra)
 *   - 3 Seitenflachen (faces): A_s, A_l, A_r
 *   - 6 Innenwinkel + 3 Aussenwinkel + 1 Offnungswinkel
 *
 * Annotation tiers:
 *   Level 1 — MINIMAL:  4 vertices (Ecken) + 3 outer edges (Aussenkanten) only
 *   Level 2 — STANDARD: + 3 inner edges (Innenkanten) + directional edge
 *   Level 3 — FULL:     + faces, angles, stylus tip point
 *
 * Backward compatibility:
 *   Level 1 maps directly to the existing StereoMorph data
 *   (tail_vertex → p_s, left_vertex → p_l, right_vertex → p_r, depth_point → p_t)
 */

// ============================================================
// ANNOTATION TIER DEFINITIONS
// ============================================================

export const ANNOTATION_TIERS = {
  minimal: {
    id: 'minimal',
    label: 'Minimal',
    description: 'Vertices + outer edges only (4 points, 3 curves per wedge). Compatible with existing StereoMorph data.',
    landmarkTypes: ['p_s', 'p_l', 'p_r', 'p_t'],
    curveTypes: ['e_sa', 'e_la', 'e_ra']
  },
  standard: {
    id: 'standard',
    label: 'Standard',
    description: 'Adds inner edges and directional edge (4 points, 6 curves per wedge).',
    landmarkTypes: ['p_s', 'p_l', 'p_r', 'p_t'],
    curveTypes: ['e_sa', 'e_la', 'e_ra', 'e_s', 'e_l', 'e_r']
  },
  full: {
    id: 'full',
    label: 'Full',
    description: 'All geometric elements including inner angles and face centroids.',
    landmarkTypes: ['p_s', 'p_l', 'p_r', 'p_t'],
    curveTypes: ['e_sa', 'e_la', 'e_ra', 'e_s', 'e_l', 'e_r'],
    derivedAngles: ['alpha_s', 'alpha_l', 'alpha_r', 'beta_s', 'beta_l', 'beta_r', 'gamma']
  }
};

// ============================================================
// LANDMARK (POINT) DEFINITIONS — Punkte / Ecken
// ============================================================

export const LANDMARK_TYPES = {
  p_s: {
    label: 'Tail vertex',
    labelShort: 'p\u209B',
    german: 'Scheitelpunkt',
    description: 'Apex of the wedge tail where the stylus shaft meets the clay. The directional reference point.',
    type: 'I',
    color: '#e74c3c',
    symbol: 'circle',
    // Legacy mapping for backward compat with existing StereoMorph data
    legacyName: 'tail_vertex',
    templatePosition: { x: 50, y: 95 }
  },
  p_l: {
    label: 'Left vertex',
    labelShort: 'p\u2097',
    german: 'linke Ecke',
    description: 'Left corner of the wedge head triangle, on the tablet plane.',
    type: 'II',
    color: '#3498db',
    symbol: 'diamond',
    legacyName: 'left_vertex',
    templatePosition: { x: 15, y: 10 }
  },
  p_r: {
    label: 'Right vertex',
    labelShort: 'p\u1D63',
    german: 'rechte Ecke',
    description: 'Right corner of the wedge head triangle, on the tablet plane.',
    type: 'II',
    color: '#2ecc71',
    symbol: 'diamond',
    legacyName: 'right_vertex',
    templatePosition: { x: 85, y: 10 }
  },
  p_t: {
    label: 'Depth point',
    labelShort: 'p\u209C',
    german: 'Tiefpunkt',
    description: 'Deepest point of the wedge impression, furthest from the tablet plane.',
    type: 'III',
    color: '#f39c12',
    symbol: 'triangle',
    legacyName: 'depth_point',
    templatePosition: { x: 50, y: 40 }
  }
};

// ============================================================
// EDGE (CURVE) DEFINITIONS — Kanten
// ============================================================

export const CURVE_TYPES = {
  // --- Aussenkanten (outer edges) — Level 1 ---
  e_sa: {
    label: 'Top outer edge',
    labelShort: 'e\u209B\u2090',
    german: 'Stirnkante',
    description: 'Outer edge of the wedge head connecting left and right vertices (the visible "front" of the wedge).',
    color: '#9b59b6',
    startLandmark: 'p_l',
    endLandmark: 'p_r',
    tier: 'minimal',
    category: 'outer',
    legacyName: 'top_outer_edge'
  },
  e_la: {
    label: 'Left outer edge',
    labelShort: 'e\u2097\u2090',
    german: 'linke Aussenkante',
    description: 'Outer edge running from left vertex to tail vertex (left arm of the wedge).',
    color: '#1abc9c',
    startLandmark: 'p_l',
    endLandmark: 'p_s',
    tier: 'minimal',
    category: 'outer',
    legacyName: 'left_outer_edge'
  },
  e_ra: {
    label: 'Right outer edge',
    labelShort: 'e\u1D63\u2090',
    german: 'rechte Aussenkante',
    description: 'Outer edge running from right vertex to tail vertex (right arm of the wedge).',
    color: '#e67e22',
    startLandmark: 'p_r',
    endLandmark: 'p_s',
    tier: 'minimal',
    category: 'outer',
    legacyName: 'right_outer_edge'
  },

  // --- Innenkanten (inner edges) — Level 2 ---
  e_s: {
    label: 'Directional edge',
    labelShort: 'e\u209B',
    german: 'Richtungskante',
    description: 'Inner edge from depth point to tail vertex. The main axis / "spine" of the wedge.',
    color: '#ff6b6b',
    startLandmark: 'p_t',
    endLandmark: 'p_s',
    tier: 'standard',
    category: 'inner'
  },
  e_l: {
    label: 'Left inner edge',
    labelShort: 'e\u2097',
    german: 'linke Innenkante',
    description: 'Inner edge from depth point to left vertex.',
    color: '#48dbfb',
    startLandmark: 'p_t',
    endLandmark: 'p_l',
    tier: 'standard',
    category: 'inner'
  },
  e_r: {
    label: 'Right inner edge',
    labelShort: 'e\u1D63',
    german: 'rechte Innenkante',
    description: 'Inner edge from depth point to right vertex.',
    color: '#55efc4',
    startLandmark: 'p_t',
    endLandmark: 'p_r',
    tier: 'standard',
    category: 'inner'
  }
};

// ============================================================
// ANGLE DEFINITIONS — Winkel (computed, not annotated)
// ============================================================

export const ANGLE_TYPES = {
  // Innenwinkel (inner angles) — between inner edges
  alpha_s: { label: 'Top (inner) angle', german: 'Stirnwinkel', vertices: ['p_l', 'p_t', 'p_r'], category: 'inner' },
  alpha_l: { label: 'Left inner angle', german: 'linker Innenwinkel', vertices: ['p_s', 'p_t', 'p_l'], category: 'inner' },
  alpha_r: { label: 'Right inner angle', german: 'rechter Innenwinkel', vertices: ['p_s', 'p_t', 'p_r'], category: 'inner' },
  // Aussenwinkel (outer angles) — between outer edges
  beta_s: { label: 'Tail (outer) angle', german: 'Schaftwinkel', vertices: ['p_l', 'p_s', 'p_r'], category: 'outer' },
  beta_l: { label: 'Left outer angle', german: 'linker Aussenwinkel', vertices: ['p_s', 'p_l', 'p_r'], category: 'outer' },  // at p_l
  beta_r: { label: 'Right outer angle', german: 'rechter Aussenwinkel', vertices: ['p_s', 'p_r', 'p_l'], category: 'outer' }, // at p_r
  // Derived
  gamma: { label: 'Angle of aperture', german: 'Offnungswinkel', vertices: null, category: 'derived',
    description: 'Opening angle of the wedge measured on the cross-sectional plane E.' }
};

// ============================================================
// FACE DEFINITIONS — Flachen / Seitenflachen (reference only)
// ============================================================

export const FACE_TYPES = {
  A_s: { label: 'Top face', german: 'Stirnflache', vertices: ['p_l', 'p_r', 'p_t'] },
  A_l: { label: 'Left face', german: 'linke Seitenflache', vertices: ['p_l', 'p_s', 'p_t'] },
  A_r: { label: 'Right face', german: 'rechte Seitenflache', vertices: ['p_r', 'p_s', 'p_t'] }
};

// ============================================================
// SVG WEDGE TEMPLATE
// ============================================================

/**
 * Generate an SVG template of a wedge with the current landmark highlighted.
 * The template follows Abbildung 1 from Cammarosano et al. (2014).
 *
 * @param {string|null} highlightId - The landmark/curve ID to highlight (e.g., 'p_s', 'e_la')
 * @param {string} tier - Current annotation tier ('minimal', 'standard', 'full')
 * @param {Object} placedLandmarks - Map of placed landmark type ids for this wedge
 * @returns {string} SVG markup
 */
export function generateWedgeTemplate(highlightId, tier = 'minimal', placedLandmarks = {}) {
  const w = 160, h = 160;
  // Vertex positions in template space (percentages mapped to px)
  const pts = {
    p_l: { x: 25, y: 20 },
    p_r: { x: 135, y: 20 },
    p_s: { x: 80, y: 148 },
    p_t: { x: 80, y: 60 }
  };

  const isHighlighted = (id) => {
    if (!highlightId) return false;
    // Strip wedge number for comparison
    const base = highlightId.replace(/\d+$/, '');
    return base === id;
  };

  const isPlaced = (id) => !!placedLandmarks[id];

  let svg = `<svg viewBox="0 0 ${w} ${h}" class="wedge-template" xmlns="http://www.w3.org/2000/svg">`;

  // Background
  svg += `<rect width="${w}" height="${h}" fill="none"/>`;

  // Title
  svg += `<text x="${w/2}" y="12" text-anchor="middle" fill="#888" font-size="9" font-family="system-ui">Wedge geometry</text>`;

  // --- Draw edges ---
  const drawEdge = (id, from, to, color, dashed = false) => {
    const hl = isHighlighted(id);
    const strokeW = hl ? 3 : 1.5;
    const opacity = hl ? 1 : 0.4;
    const dashArray = dashed ? '4,3' : 'none';
    svg += `<line x1="${from.x}" y1="${from.y}" x2="${to.x}" y2="${to.y}" `;
    svg += `stroke="${color}" stroke-width="${strokeW}" opacity="${opacity}" `;
    if (dashed) svg += `stroke-dasharray="${dashArray}" `;
    svg += `/>`;
    // Label
    if (hl) {
      const mx = (from.x + to.x) / 2, my = (from.y + to.y) / 2;
      svg += `<text x="${mx + 6}" y="${my - 4}" fill="${color}" font-size="9" font-weight="bold" font-family="system-ui">${CURVE_TYPES[id]?.labelShort || id}</text>`;
    }
  };

  // Outer edges (always visible)
  drawEdge('e_sa', pts.p_l, pts.p_r, CURVE_TYPES.e_sa.color);
  drawEdge('e_la', pts.p_l, pts.p_s, CURVE_TYPES.e_la.color);
  drawEdge('e_ra', pts.p_r, pts.p_s, CURVE_TYPES.e_ra.color);

  // Inner edges (only if standard or full tier)
  if (tier !== 'minimal') {
    drawEdge('e_s', pts.p_t, pts.p_s, CURVE_TYPES.e_s.color, true);
    drawEdge('e_l', pts.p_t, pts.p_l, CURVE_TYPES.e_l.color, true);
    drawEdge('e_r', pts.p_t, pts.p_r, CURVE_TYPES.e_r.color, true);
  }

  // --- Draw faces (subtle fill) ---
  if (tier === 'full') {
    // Top face (green tint)
    svg += `<polygon points="${pts.p_l.x},${pts.p_l.y} ${pts.p_r.x},${pts.p_r.y} ${pts.p_t.x},${pts.p_t.y}" fill="#2ecc71" opacity="0.08"/>`;
    // Left face (blue tint)
    svg += `<polygon points="${pts.p_l.x},${pts.p_l.y} ${pts.p_s.x},${pts.p_s.y} ${pts.p_t.x},${pts.p_t.y}" fill="#3498db" opacity="0.08"/>`;
    // Right face (red tint)
    svg += `<polygon points="${pts.p_r.x},${pts.p_r.y} ${pts.p_s.x},${pts.p_s.y} ${pts.p_t.x},${pts.p_t.y}" fill="#e74c3c" opacity="0.08"/>`;
  }

  // --- Draw vertices ---
  const drawVertex = (id, pt, def) => {
    const hl = isHighlighted(id);
    const placed = isPlaced(id);
    const r = hl ? 8 : 5;
    const opacity = hl ? 1 : (placed ? 0.8 : 0.35);
    const strokeColor = hl ? '#fff' : (placed ? '#fff' : '#666');
    const strokeW = hl ? 2 : 1;

    // Pulsing animation ring for highlighted
    if (hl) {
      svg += `<circle cx="${pt.x}" cy="${pt.y}" r="${r + 5}" fill="none" stroke="${def.color}" stroke-width="1.5" opacity="0.5">`;
      svg += `<animate attributeName="r" values="${r+3};${r+8};${r+3}" dur="1.5s" repeatCount="indefinite"/>`;
      svg += `<animate attributeName="opacity" values="0.6;0.15;0.6" dur="1.5s" repeatCount="indefinite"/>`;
      svg += `</circle>`;
    }

    // Shape
    svg += `<g opacity="${opacity}">`;
    if (def.symbol === 'diamond') {
      svg += `<polygon points="${pt.x},${pt.y-r} ${pt.x+r},${pt.y} ${pt.x},${pt.y+r} ${pt.x-r},${pt.y}" `;
      svg += `fill="${def.color}" stroke="${strokeColor}" stroke-width="${strokeW}"/>`;
    } else if (def.symbol === 'triangle') {
      svg += `<polygon points="${pt.x},${pt.y-r} ${pt.x+r*0.87},${pt.y+r*0.5} ${pt.x-r*0.87},${pt.y+r*0.5}" `;
      svg += `fill="${def.color}" stroke="${strokeColor}" stroke-width="${strokeW}"/>`;
    } else {
      svg += `<circle cx="${pt.x}" cy="${pt.y}" r="${r}" fill="${def.color}" stroke="${strokeColor}" stroke-width="${strokeW}"/>`;
    }
    svg += `</g>`;

    // Label
    const labelX = pt.x + (pt.x < w/2 ? -14 : 10);
    const labelY = pt.y + (pt.y < h/2 ? -6 : 14);
    const weight = hl ? 'bold' : 'normal';
    const labelColor = hl ? def.color : '#999';
    svg += `<text x="${labelX}" y="${labelY}" fill="${labelColor}" font-size="10" font-weight="${weight}" font-family="system-ui">${def.labelShort}</text>`;

    // Checkmark for placed
    if (placed && !hl) {
      svg += `<text x="${pt.x + r + 2}" y="${pt.y - r}" fill="#2ecc71" font-size="10">&#x2713;</text>`;
    }
  };

  drawVertex('p_l', pts.p_l, LANDMARK_TYPES.p_l);
  drawVertex('p_r', pts.p_r, LANDMARK_TYPES.p_r);
  drawVertex('p_t', pts.p_t, LANDMARK_TYPES.p_t);
  drawVertex('p_s', pts.p_s, LANDMARK_TYPES.p_s);

  // --- Angle arcs (full tier only) ---
  if (tier === 'full') {
    // Tail angle arc (beta_s) at p_s
    const hl_beta = isHighlighted('beta_s');
    svg += `<path d="M ${pts.p_s.x - 15},${pts.p_s.y - 12} A 15 15 0 0 1 ${pts.p_s.x + 15},${pts.p_s.y - 12}" `;
    svg += `fill="none" stroke="#ff9ff3" stroke-width="${hl_beta ? 2 : 1}" opacity="${hl_beta ? 1 : 0.3}"/>`;
  }

  svg += '</svg>';
  return svg;
}

// ============================================================
// SEQUENCE GENERATORS
// ============================================================

/**
 * Generate the landmark sequence for annotation, respecting the tier.
 */
export function generateLandmarkSequence(nWedges, tier = 'minimal') {
  const tierDef = ANNOTATION_TIERS[tier] || ANNOTATION_TIERS.minimal;
  const sequence = [];
  for (let w = 1; w <= nWedges; w++) {
    const pad = String(w).padStart(2, '0');
    for (const typeId of tierDef.landmarkTypes) {
      const def = LANDMARK_TYPES[typeId];
      if (!def) continue;
      sequence.push({
        id: `${typeId}_${pad}`,
        wedge: w,
        type: typeId,
        legacyId: `${def.legacyName}${pad}`,
        ...def
      });
    }
  }
  return sequence;
}

/**
 * Generate the curve sequence for annotation, respecting the tier.
 */
export function generateCurveSequence(nWedges, tier = 'minimal') {
  const tierDef = ANNOTATION_TIERS[tier] || ANNOTATION_TIERS.minimal;
  const sequence = [];
  for (let w = 1; w <= nWedges; w++) {
    const pad = String(w).padStart(2, '0');
    for (const typeId of tierDef.curveTypes) {
      const def = CURVE_TYPES[typeId];
      if (!def) continue;
      sequence.push({
        id: `${typeId}_${pad}`,
        wedge: w,
        type: typeId,
        startLandmarkId: `${def.startLandmark}_${pad}`,
        endLandmarkId: `${def.endLandmark}_${pad}`,
        legacyId: def.legacyName ? `${def.legacyName}${pad}` : null,
        ...def
      });
    }
  }
  return sequence;
}

/**
 * Generate the curves_tab mapping (for StereoMorph export compatibility).
 */
export function generateCurvesTab(nWedges, tier = 'minimal') {
  const landmarks = generateLandmarkSequence(nWedges, tier);
  const curves = generateCurveSequence(nWedges, tier);

  const landmarkIndex = {};
  landmarks.forEach((lm, i) => { landmarkIndex[lm.id] = i + 1; });

  return curves.map(c => ({
    name: c.legacyId || c.id,
    start: landmarkIndex[c.startLandmarkId],
    end: landmarkIndex[c.endLandmarkId]
  }));
}

/**
 * Compute angles from placed landmarks (for Full tier).
 * Returns an object of angle name -> degrees.
 */
export function computeAngles(landmarks, wedgeNum) {
  const pad = String(wedgeNum).padStart(2, '0');
  const get = (id) => landmarks[`${id}_${pad}`];

  const angles = {};
  for (const [id, def] of Object.entries(ANGLE_TYPES)) {
    if (!def.vertices) continue;
    const [aId, bId, cId] = def.vertices;
    const a = get(aId), b = get(bId), c = get(cId);
    if (!a || !b || !c) continue;

    // Angle at vertex b between rays b->a and b->c
    const ba = [a[0] - b[0], a[1] - b[1]];
    const bc = [c[0] - b[0], c[1] - b[1]];
    const dot = ba[0] * bc[0] + ba[1] * bc[1];
    const magA = Math.hypot(ba[0], ba[1]);
    const magC = Math.hypot(bc[0], bc[1]);
    if (magA > 0 && magC > 0) {
      angles[id] = Math.acos(Math.max(-1, Math.min(1, dot / (magA * magC)))) * (180 / Math.PI);
    }
  }
  return angles;
}

// ============================================================
// METADATA FIELDS
// ============================================================

export const METADATA_FIELDS = {
  minimum: [
    { key: 'specimen_id', label: 'Specimen ID', type: 'text', required: true, placeholder: 'e.g., ugarit_RS15062_sign01' },
    { key: 'object_type', label: 'Object Type', type: 'select', required: true,
      options: ['tablet', 'prism', 'cylinder', 'envelope', 'bulla', 'cone', 'brick', 'other_carrier'] },
    { key: 'annotation_level', label: 'Annotation Level', type: 'select', required: true,
      options: ['sign', 'carrier'] },
    { key: 'period_broad', label: 'Period', type: 'select', required: true,
      options: ['ED', 'OAkk', 'Ur_III', 'OB', 'OA', 'MB', 'MA', 'LB', 'NA', 'NB', 'Achaemenid', 'Seleucid', 'uncertain'] },
    { key: 'site', label: 'Site', type: 'text', required: true, placeholder: 'e.g., Ras Shamra, Bogazkoy' },
    { key: 'annotator', label: 'Annotator', type: 'text', required: true, placeholder: 'Your initials or name' },
  ],
  extended: [
    { key: 'publication', label: 'Publication', type: 'text', placeholder: 'Bibliographic reference' },
    { key: 'cdli_number', label: 'CDLI Number', type: 'text', placeholder: 'P000000' },
    { key: 'ancient_name', label: 'Ancient Name', type: 'text', placeholder: 'e.g., Hattusa, Ugarit' },
    { key: 'region', label: 'Region', type: 'text', placeholder: 'e.g., Northern Levant' },
    { key: 'archive_name', label: 'Archive', type: 'text', placeholder: 'e.g., House of Urtenu' },
    { key: 'museum', label: 'Museum', type: 'text', placeholder: 'Holding institution' },
    { key: 'museum_number', label: 'Museum Number', type: 'text', placeholder: 'Accession number' },
    { key: 'language', label: 'Language', type: 'select',
      options: ['Sumerian', 'Akkadian', 'Hittite', 'Ugaritic', 'Elamite', 'Hurrian', 'bilingual', 'other'] },
    { key: 'script_tradition', label: 'Script Tradition', type: 'text', placeholder: 'e.g., Ugarit Akkadian' },
    { key: 'genre', label: 'Genre', type: 'select',
      options: ['administrative', 'legal', 'letter', 'literary', 'ritual', 'lexical', 'royal_inscription', 'omen', 'medical', 'mathematical', 'other'] },
    { key: 'sign_name', label: 'Sign Name', type: 'text', placeholder: 'e.g., AN, KI, DINGIR' },
    { key: 'sign_number_borger', label: 'MesZL Number', type: 'number', placeholder: 'Borger number' },
    { key: 'line_number', label: 'Line Number', type: 'text', placeholder: 'e.g., obv. 3, rev. ii 12' },
    { key: 'scribe_name', label: 'Scribe Name', type: 'text', placeholder: 'If known' },
    { key: 'attribution_confidence', label: 'Attribution Confidence', type: 'select',
      options: ['certain', 'probable', 'possible', 'unattributed'] },
    { key: 'period_detail', label: 'Period Detail', type: 'text', placeholder: 'e.g., LB IIA' },
    { key: 'date_range_start', label: 'Date Start (BCE)', type: 'number', placeholder: '-1350' },
    { key: 'date_range_end', label: 'Date End (BCE)', type: 'number', placeholder: '-1185' },
    { key: 'notes', label: 'Notes', type: 'textarea', placeholder: 'Observations about sign quality, condition...' },
  ]
};

// ============================================================
// CONSTANTS
// ============================================================

export const CURVE_CONTROL_POINTS = 5;
export const MAX_WEDGES = 9;
export const DEFAULT_TIER = 'minimal';

/**
 * Map new protocol IDs to legacy StereoMorph IDs (for export).
 */
export function toLegacyLandmarkId(id) {
  return id
    .replace(/^p_s_/, 'tail_vertex')
    .replace(/^p_l_/, 'left_vertex')
    .replace(/^p_r_/, 'right_vertex')
    .replace(/^p_t_/, 'depth_point');
}

export function toLegacyCurveId(id) {
  return id
    .replace(/^e_sa_/, 'top_outer_edge')
    .replace(/^e_la_/, 'left_outer_edge')
    .replace(/^e_ra_/, 'right_outer_edge');
}

/**
 * Map legacy StereoMorph IDs to new protocol IDs (for import).
 */
export function fromLegacyLandmarkId(legacyId) {
  return legacyId
    .replace(/^tail_vertex(\d+)$/, 'p_s_$1')
    .replace(/^left_vertex(\d+)$/, 'p_l_$1')
    .replace(/^right_vertex(\d+)$/, 'p_r_$1')
    .replace(/^depth_point(\d+)$/, 'p_t_$1');
}

export function fromLegacyCurveId(legacyId) {
  return legacyId
    .replace(/^top_outer_edge(\d+)$/, 'e_sa_$1')
    .replace(/^left_outer_edge(\d+)$/, 'e_la_$1')
    .replace(/^right_outer_edge(\d+)$/, 'e_ra_$1');
}
