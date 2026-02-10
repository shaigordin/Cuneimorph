/**
 * Cuneimorph Landmark Protocol Definition
 *
 * Defines the geometric morphometric landmark scheme for cuneiform signs.
 * Based on the wedge decomposition protocol: each wedge has 4 fixed landmarks
 * and 3 curves connecting them.
 *
 * Landmark Types (Bookstein classification adapted for paleography):
 *   Type I  - Structural intersections (tail_vertex: where stylus enters clay)
 *   Type II - Extremal points (left_vertex, right_vertex: wedge head extremities)
 *   Type III - Constructed points (depth_point: interior depth reference)
 *
 * Protocol supports up to 9 wedges per sign (36 landmarks + 27 curves maximum).
 * The actual number of wedges is set per-specimen during annotation.
 */

export const LANDMARK_TYPES = {
  tail_vertex: {
    label: 'Tail Vertex',
    description: 'Point where the stylus shaft enters the clay (wedge tail/start)',
    type: 'I',
    color: '#e74c3c',
    symbol: 'circle'
  },
  left_vertex: {
    label: 'Left Vertex',
    description: 'Left extremity of the wedge head impression',
    type: 'II',
    color: '#3498db',
    symbol: 'diamond'
  },
  right_vertex: {
    label: 'Right Vertex',
    description: 'Right extremity of the wedge head impression',
    type: 'II',
    color: '#2ecc71',
    symbol: 'diamond'
  },
  depth_point: {
    label: 'Depth Point',
    description: 'Interior reference point marking maximum depth of wedge impression',
    type: 'III',
    color: '#f39c12',
    symbol: 'triangle'
  }
};

export const CURVE_TYPES = {
  top_outer_edge: {
    label: 'Top Edge',
    description: 'Outer edge connecting left_vertex to right_vertex (wedge head arc)',
    color: '#9b59b6',
    startLandmark: 'left_vertex',
    endLandmark: 'right_vertex'
  },
  left_outer_edge: {
    label: 'Left Edge',
    description: 'Outer edge from left_vertex to tail_vertex (left arm of wedge)',
    color: '#1abc9c',
    startLandmark: 'left_vertex',
    endLandmark: 'tail_vertex'
  },
  right_outer_edge: {
    label: 'Right Edge',
    description: 'Outer edge from right_vertex to tail_vertex (right arm of wedge)',
    color: '#e67e22',
    startLandmark: 'right_vertex',
    endLandmark: 'tail_vertex'
  }
};

/**
 * Generate the full landmark sequence for a given number of wedges.
 * Returns an ordered array of landmark definitions that drives the annotation UI.
 */
export function generateLandmarkSequence(nWedges) {
  const sequence = [];
  for (let w = 1; w <= nWedges; w++) {
    const pad = String(w).padStart(2, '0');
    for (const [type, def] of Object.entries(LANDMARK_TYPES)) {
      sequence.push({
        id: `${type}${pad}`,
        wedge: w,
        type: type,
        ...def
      });
    }
  }
  return sequence;
}

/**
 * Generate the curve sequence for a given number of wedges.
 */
export function generateCurveSequence(nWedges) {
  const sequence = [];
  for (let w = 1; w <= nWedges; w++) {
    const pad = String(w).padStart(2, '0');
    for (const [type, def] of Object.entries(CURVE_TYPES)) {
      sequence.push({
        id: `${type}${pad}`,
        wedge: w,
        type: type,
        startLandmarkId: `${def.startLandmark}${pad}`,
        endLandmarkId: `${def.endLandmark}${pad}`,
        ...def
      });
    }
  }
  return sequence;
}

/**
 * Generate the curves_tab mapping (matching curves_tab.txt format).
 * Maps curve names to start/end landmark indices (1-based).
 */
export function generateCurvesTab(nWedges) {
  const landmarks = generateLandmarkSequence(nWedges);
  const curves = generateCurveSequence(nWedges);

  const landmarkIndex = {};
  landmarks.forEach((lm, i) => { landmarkIndex[lm.id] = i + 1; });

  return curves.map(c => ({
    name: c.id,
    start: landmarkIndex[c.startLandmarkId],
    end: landmarkIndex[c.endLandmarkId]
  }));
}

/**
 * Metadata field definitions for the annotation form.
 * Organized by minimum (required) and extended (optional) fields.
 */
export const METADATA_FIELDS = {
  minimum: [
    { key: 'specimen_id', label: 'Specimen ID', type: 'text', required: true, placeholder: 'e.g., ugarit_RS15062_sign01' },
    { key: 'object_type', label: 'Object Type', type: 'select', required: true,
      options: ['tablet', 'prism', 'cylinder', 'envelope', 'bulla', 'cone', 'brick', 'other_carrier'] },
    { key: 'annotation_level', label: 'Annotation Level', type: 'select', required: true,
      options: ['sign', 'carrier'] },
    { key: 'period_broad', label: 'Period', type: 'select', required: true,
      options: ['ED', 'OAkk', 'Ur_III', 'OB', 'OA', 'MB', 'MA', 'LB', 'NA', 'NB', 'Achaemenid', 'Seleucid', 'uncertain'] },
    { key: 'site', label: 'Site', type: 'text', required: true, placeholder: 'e.g., Ras Shamra, Boghazköy' },
    { key: 'annotator', label: 'Annotator', type: 'text', required: true, placeholder: 'Your initials or name' },
  ],
  extended: [
    { key: 'publication', label: 'Publication', type: 'text', placeholder: 'Bibliographic reference' },
    { key: 'cdli_number', label: 'CDLI Number', type: 'text', placeholder: 'P000000' },
    { key: 'ancient_name', label: 'Ancient Name', type: 'text', placeholder: 'e.g., Hattuša, Ugarit' },
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

/**
 * Default number of control points per curve (for Bézier interpolation).
 */
export const CURVE_CONTROL_POINTS = 5;

/**
 * Maximum wedges supported by the protocol.
 */
export const MAX_WEDGES = 9;
