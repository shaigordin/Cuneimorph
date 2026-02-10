/**
 * Cuneimorph Annotator — Main Application
 *
 * Orchestrates the annotation workflow:
 *   1. Image loading
 *   2. Metadata entry (minimum → extended)
 *   3. Wedge count selection
 *   4. Guided landmark placement (wedge-by-wedge)
 *   5. Curve drawing per wedge
 *   6. Live analysis preview (Procrustes + PCA)
 *   7. Export in multiple formats
 */

import { CanvasEngine } from './canvas-engine.js';
import {
  generateLandmarkSequence, generateCurveSequence,
  LANDMARK_TYPES, CURVE_TYPES, METADATA_FIELDS, MAX_WEDGES
} from './landmark-protocol.js';
import { MorphoAnalysis } from './analysis.js';
import {
  exportStereoMorph, exportCuneimorphJSON, exportTPS,
  exportDatasetCSV, exportProjectBundle,
  importStereoMorph, importCuneimorphJSON,
  downloadFile
} from './export.js';

class CuneimorphAnnotator {
  constructor() {
    // Core state
    this.canvas = null;
    this.analysis = new MorphoAnalysis();
    this.mode = 'landmark'; // 'landmark' | 'curve' | 'navigate'
    this.nWedges = 3;

    // Current specimen
    this.currentLandmarks = {};  // id -> [x, y]
    this.currentCurves = {};     // id -> { points: [[x,y],...] }
    this.metadata = {};
    this.imageDimensions = null;
    this.imageFileName = '';
    this.annotationStartTime = null;

    // Protocol sequences
    this.landmarkSequence = [];
    this.curveSequence = [];
    this.currentLandmarkIndex = 0;
    this.currentCurveIndex = 0;

    // Dataset (multi-specimen)
    this.dataset = [];  // array of completed specimens

    // Undo stack
    this.undoStack = [];

    this._init();
  }

  _init() {
    // Wait for DOM
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => this._setup());
    } else {
      this._setup();
    }
  }

  _setup() {
    // Canvas
    const canvasEl = document.getElementById('annotation-canvas');
    this.canvas = new CanvasEngine(canvasEl);

    // Canvas click handler
    this.canvas.onClick = (ix, iy, e) => this._handleCanvasClick(ix, iy, e);
    this.canvas.onLandmarkDrag = (id, x, y) => this._handleLandmarkDrag(id, x, y);
    this.canvas.onCursorMove = (ix, iy) => this._updateCoordinateDisplay(ix, iy);

    // Setup all UI event listeners
    this._setupImageLoading();
    this._setupWedgeSelector();
    this._setupModeControls();
    this._setupMetadataForm();
    this._setupExportControls();
    this._setupDatasetControls();
    this._setupKeyboardShortcuts();
    this._setupImportControls();

    // Initial state
    this._updateWedgeCount(3);
    this._renderProtocolGuide();
    this._updateAnalysisPanel();
  }

  // --- Image Loading ---

  _setupImageLoading() {
    const loadBtn = document.getElementById('btn-load-image');
    const fileInput = document.getElementById('file-input');
    const dropZone = document.getElementById('annotation-canvas');

    loadBtn?.addEventListener('click', () => fileInput?.click());
    fileInput?.addEventListener('change', (e) => {
      if (e.target.files[0]) this._loadImageFile(e.target.files[0]);
    });

    // Drag and drop
    dropZone?.addEventListener('dragover', (e) => { e.preventDefault(); e.dataTransfer.dropEffect = 'copy'; });
    dropZone?.addEventListener('drop', (e) => {
      e.preventDefault();
      const file = e.dataTransfer.files[0];
      if (file && file.type.startsWith('image/')) {
        this._loadImageFile(file);
      }
    });
  }

  async _loadImageFile(file) {
    this.imageFileName = file.name;
    const dims = await this.canvas.loadImageFromFile(file);
    this.imageDimensions = dims;
    this.annotationStartTime = Date.now();

    // Update UI
    document.getElementById('image-info').textContent =
      `${file.name} (${dims.width}×${dims.height})`;
    document.getElementById('specimen-id-input').value =
      file.name.replace(/\.[^.]+$/, '');

    this._showPanel('panel-metadata');
  }

  // --- Wedge Count ---

  _setupWedgeSelector() {
    const selector = document.getElementById('wedge-count');
    selector?.addEventListener('change', (e) => {
      this._updateWedgeCount(parseInt(e.target.value));
    });
  }

  _updateWedgeCount(n) {
    this.nWedges = n;
    this.landmarkSequence = generateLandmarkSequence(n);
    this.curveSequence = generateCurveSequence(n);
    this.currentLandmarkIndex = 0;
    this.currentCurveIndex = 0;
    this._renderProtocolGuide();
    this._updateProgressBar();
  }

  // --- Mode Controls ---

  _setupModeControls() {
    document.getElementById('btn-mode-landmark')?.addEventListener('click', () => this._setMode('landmark'));
    document.getElementById('btn-mode-curve')?.addEventListener('click', () => this._setMode('curve'));
    document.getElementById('btn-mode-navigate')?.addEventListener('click', () => this._setMode('navigate'));
    document.getElementById('btn-undo')?.addEventListener('click', () => this._undo());
    document.getElementById('btn-clear-all')?.addEventListener('click', () => this._clearAll());
    document.getElementById('btn-fit-view')?.addEventListener('click', () => {
      this.canvas.fitToView();
      this.canvas.render();
    });
    document.getElementById('btn-toggle-labels')?.addEventListener('click', () => {
      this.canvas.showLabels = !this.canvas.showLabels;
      this.canvas.render();
    });
  }

  _setMode(mode) {
    this.mode = mode;
    // Cancel any active curve drawing
    if (mode !== 'curve') {
      this.canvas.cancelCurveDraw();
    }
    // Update button states
    for (const m of ['landmark', 'curve', 'navigate']) {
      const btn = document.getElementById(`btn-mode-${m}`);
      btn?.classList.toggle('active', m === mode);
    }
    // Update cursor
    const canvasEl = document.getElementById('annotation-canvas');
    canvasEl.style.cursor = mode === 'navigate' ? 'grab' : 'crosshair';

    this._renderProtocolGuide();
  }

  // --- Canvas Click Handling ---

  _handleCanvasClick(ix, iy, e) {
    if (this.mode === 'navigate') return;

    if (this.mode === 'landmark') {
      this._placeLandmark(ix, iy);
    } else if (this.mode === 'curve') {
      this._handleCurveClick(ix, iy, e);
    }
  }

  _placeLandmark(ix, iy) {
    if (this.currentLandmarkIndex >= this.landmarkSequence.length) {
      // All landmarks placed — auto-switch to curve mode
      this._setMode('curve');
      return;
    }

    const lmDef = this.landmarkSequence[this.currentLandmarkIndex];

    // Store
    this.currentLandmarks[lmDef.id] = [ix, iy];

    // Push undo
    this.undoStack.push({ type: 'landmark', id: lmDef.id, index: this.currentLandmarkIndex });

    // Draw on canvas
    this.canvas.setLandmark(lmDef.id, ix, iy, {
      color: lmDef.color,
      symbol: lmDef.symbol,
      wedge: lmDef.wedge
    });

    // Advance
    this.currentLandmarkIndex++;

    // Highlight current wedge
    if (this.currentLandmarkIndex < this.landmarkSequence.length) {
      this.canvas.highlightWedge = this.landmarkSequence[this.currentLandmarkIndex].wedge;
    } else {
      this.canvas.highlightWedge = null;
    }

    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();
    this.canvas.render();
  }

  _handleCurveClick(ix, iy, e) {
    if (this.currentCurveIndex >= this.curveSequence.length) return;

    const curveDef = this.curveSequence[this.currentCurveIndex];

    if (!this.canvas.activeCurve) {
      // Start a new curve
      this.canvas.startCurveDraw(curveDef.id, {
        color: curveDef.color,
        label: curveDef.label
      });
    }

    // Add point
    this.canvas.addCurvePoint(ix, iy);

    // Double-click or right-click to finish curve
    // (We handle finishing via keyboard 'Enter' or button)
    this._renderProtocolGuide();
  }

  finishCurrentCurve() {
    if (!this.canvas.activeCurve) return;
    const curve = this.canvas.finishCurveDraw();
    if (curve) {
      this.currentCurves[curve.id] = { points: curve.points };
      this.undoStack.push({ type: 'curve', id: curve.id, index: this.currentCurveIndex });
      this.currentCurveIndex++;
    }
    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();
  }

  _handleLandmarkDrag(id, x, y) {
    this.currentLandmarks[id] = [x, y];
    this._updateAnalysisPanel();
  }

  // --- Undo ---

  _undo() {
    if (this.undoStack.length === 0) return;

    const action = this.undoStack.pop();
    if (action.type === 'landmark') {
      delete this.currentLandmarks[action.id];
      this.canvas.removeLandmark(action.id);
      this.currentLandmarkIndex = action.index;
    } else if (action.type === 'curve') {
      delete this.currentCurves[action.id];
      this.canvas.removeCurve(action.id);
      this.currentCurveIndex = action.index;
    }

    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();
    this.canvas.render();
  }

  _clearAll() {
    if (!confirm('Clear all landmarks and curves for this specimen?')) return;
    this.currentLandmarks = {};
    this.currentCurves = {};
    this.undoStack = [];
    this.currentLandmarkIndex = 0;
    this.currentCurveIndex = 0;
    this.canvas.clearAll();
    this._setMode('landmark');
    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();
  }

  // --- Metadata Form ---

  _setupMetadataForm() {
    const form = document.getElementById('metadata-form');
    if (!form) return;

    // Render minimum fields
    this._renderMetadataFields(form, 'minimum');

    // Toggle extended fields
    const toggleBtn = document.getElementById('btn-toggle-extended');
    const extDiv = document.getElementById('metadata-extended');
    toggleBtn?.addEventListener('click', () => {
      extDiv.classList.toggle('hidden');
      if (!extDiv.classList.contains('hidden') && !extDiv.dataset.rendered) {
        this._renderMetadataFields(extDiv, 'extended');
        extDiv.dataset.rendered = 'true';
      }
      toggleBtn.textContent = extDiv.classList.contains('hidden')
        ? 'Show extended fields'
        : 'Hide extended fields';
    });

    // Collect metadata on change
    form.addEventListener('input', () => this._collectMetadata());
  }

  _renderMetadataFields(container, level) {
    const fields = METADATA_FIELDS[level];
    for (const field of fields) {
      const group = document.createElement('div');
      group.className = 'form-group';

      const label = document.createElement('label');
      label.textContent = field.label;
      label.htmlFor = `meta-${field.key}`;
      if (field.required) {
        const req = document.createElement('span');
        req.className = 'required';
        req.textContent = ' *';
        label.appendChild(req);
      }
      group.appendChild(label);

      let input;
      if (field.type === 'select') {
        input = document.createElement('select');
        const emptyOpt = document.createElement('option');
        emptyOpt.value = '';
        emptyOpt.textContent = `Select ${field.label.toLowerCase()}...`;
        input.appendChild(emptyOpt);
        for (const opt of field.options) {
          const option = document.createElement('option');
          option.value = opt;
          option.textContent = opt;
          input.appendChild(option);
        }
      } else if (field.type === 'textarea') {
        input = document.createElement('textarea');
        input.rows = 3;
        input.placeholder = field.placeholder || '';
      } else {
        input = document.createElement('input');
        input.type = field.type;
        input.placeholder = field.placeholder || '';
      }
      input.id = `meta-${field.key}`;
      input.name = field.key;
      group.appendChild(input);
      container.appendChild(group);
    }
  }

  _collectMetadata() {
    const form = document.getElementById('metadata-form');
    if (!form) return;
    const formData = new FormData(form);
    this.metadata = {};
    for (const [key, value] of formData.entries()) {
      if (value) this.metadata[key] = value;
    }
    // Add specimen ID from header input
    const specInput = document.getElementById('specimen-id-input');
    if (specInput?.value) {
      this.metadata.specimen_id = specInput.value;
    }
    this.metadata.image_filename = this.imageFileName;
  }

  // --- Protocol Guide ---

  _renderProtocolGuide() {
    const guide = document.getElementById('protocol-guide');
    if (!guide) return;

    let html = '';

    if (this.mode === 'landmark') {
      if (this.currentLandmarkIndex < this.landmarkSequence.length) {
        const current = this.landmarkSequence[this.currentLandmarkIndex];
        html += `<div class="guide-current">`;
        html += `<div class="guide-label">Place landmark:</div>`;
        html += `<div class="guide-landmark" style="border-color: ${current.color}">`;
        html += `<strong>${current.id}</strong>`;
        html += `<div class="guide-desc">${current.description}</div>`;
        html += `<div class="guide-wedge">Wedge ${current.wedge} of ${this.nWedges}</div>`;
        html += `</div></div>`;

        // Next up preview
        if (this.currentLandmarkIndex + 1 < this.landmarkSequence.length) {
          const next = this.landmarkSequence[this.currentLandmarkIndex + 1];
          html += `<div class="guide-next">Next: <span style="color:${next.color}">${next.id}</span></div>`;
        }
      } else {
        html += `<div class="guide-complete">All landmarks placed. Switch to curve mode (C) or export.</div>`;
      }
    } else if (this.mode === 'curve') {
      if (this.currentCurveIndex < this.curveSequence.length) {
        const current = this.curveSequence[this.currentCurveIndex];
        const isDrawing = !!this.canvas.activeCurve;
        html += `<div class="guide-current">`;
        html += `<div class="guide-label">${isDrawing ? 'Drawing curve:' : 'Draw curve:'}</div>`;
        html += `<div class="guide-landmark" style="border-color: ${current.color}">`;
        html += `<strong>${current.id}</strong>`;
        html += `<div class="guide-desc">${current.description}</div>`;
        html += `<div class="guide-wedge">From ${current.startLandmarkId} → ${current.endLandmarkId}</div>`;
        html += `</div></div>`;
        if (isDrawing) {
          html += `<div class="guide-hint">Click points along the edge. Press <kbd>Enter</kbd> or click <button class="btn-small btn-finish-curve" id="btn-finish-curve-inline">Finish</button> when done.</div>`;
        } else {
          html += `<div class="guide-hint">Click to start placing points along this edge.</div>`;
        }
      } else {
        html += `<div class="guide-complete">All curves drawn. Ready to export.</div>`;
      }
    } else {
      html += `<div class="guide-hint">Navigation mode. Scroll to zoom, drag to pan. Press <kbd>L</kbd> for landmarks, <kbd>C</kbd> for curves.</div>`;
    }

    guide.innerHTML = html;

    // Re-attach inline finish button
    document.getElementById('btn-finish-curve-inline')?.addEventListener('click', () => this.finishCurrentCurve());
  }

  _updateProgressBar() {
    const totalSteps = this.landmarkSequence.length + this.curveSequence.length;
    const completedSteps = this.currentLandmarkIndex + this.currentCurveIndex;
    const pct = totalSteps > 0 ? (completedSteps / totalSteps * 100) : 0;

    const bar = document.getElementById('progress-fill');
    const text = document.getElementById('progress-text');
    if (bar) bar.style.width = `${pct}%`;
    if (text) text.textContent = `${completedSteps} / ${totalSteps}`;
  }

  _updateCoordinateDisplay(ix, iy) {
    const display = document.getElementById('coord-display');
    if (display) {
      display.textContent = `x: ${Math.round(ix)}  y: ${Math.round(iy)}`;
    }
  }

  // --- Analysis Panel ---

  _updateAnalysisPanel() {
    const panel = document.getElementById('analysis-content');
    if (!panel) return;

    const nPlaced = Object.keys(this.currentLandmarks).length;
    const nCurves = Object.keys(this.currentCurves).length;

    // Basic stats for current specimen
    let html = '<div class="analysis-section">';
    html += `<h4>Current Specimen</h4>`;
    html += `<div class="stat-row"><span>Landmarks:</span><span>${nPlaced} / ${this.landmarkSequence.length}</span></div>`;
    html += `<div class="stat-row"><span>Curves:</span><span>${nCurves} / ${this.curveSequence.length}</span></div>`;

    if (nPlaced >= 4) {
      // Compute centroid size for current
      const coords = Object.values(this.currentLandmarks);
      const cx = coords.reduce((s, p) => s + p[0], 0) / coords.length;
      const cy = coords.reduce((s, p) => s + p[1], 0) / coords.length;
      const cs = Math.sqrt(coords.reduce((s, p) => s + (p[0] - cx) ** 2 + (p[1] - cy) ** 2, 0));
      html += `<div class="stat-row"><span>Centroid size:</span><span>${cs.toFixed(2)}</span></div>`;
    }
    html += '</div>';

    // Dataset stats
    if (this.dataset.length > 0) {
      html += '<div class="analysis-section">';
      html += `<h4>Dataset (${this.dataset.length} specimens)</h4>`;

      // Run analysis if we have enough specimens
      if (this.dataset.length >= 2) {
        // Update analysis with all dataset specimens
        this.analysis = new MorphoAnalysis();
        for (const spec of this.dataset) {
          this.analysis.setSpecimen(spec.metadata.specimen_id || spec.id, spec.landmarks, spec.metadata);
        }

        // Include current specimen if it has enough landmarks
        if (nPlaced === this.landmarkSequence.length) {
          this.analysis.setSpecimen(
            this.metadata.specimen_id || 'current',
            this.currentLandmarks,
            this.metadata
          );
        }

        const gpa = this.analysis.runGPA();
        if (gpa) {
          html += `<div class="stat-row"><span>Procrustes aligned:</span><span>${gpa.nSpecimens} specimens</span></div>`;

          // Mean Procrustes distance
          const meanDist = gpa.procDistances.reduce((s, d) => s + d, 0) / gpa.procDistances.length;
          html += `<div class="stat-row"><span>Mean Proc. dist:</span><span>${meanDist.toFixed(4)}</span></div>`;

          // PCA
          const pca = this.analysis.runPCA();
          if (pca && pca.nComponents > 0) {
            html += `<div class="stat-row"><span>PC1 variance:</span><span>${(pca.varExplained[0] * 100).toFixed(1)}%</span></div>`;
            if (pca.nComponents > 1) {
              html += `<div class="stat-row"><span>PC2 variance:</span><span>${(pca.varExplained[1] * 100).toFixed(1)}%</span></div>`;
            }
            html += this._renderMiniPCA(pca, gpa);
          }
        }
      } else {
        html += '<div class="stat-row"><span>Need ≥2 specimens for Procrustes analysis</span></div>';
      }
      html += '</div>';
    }

    // Dataset list
    if (this.dataset.length > 0) {
      html += '<div class="analysis-section">';
      html += '<h4>Specimens in Dataset</h4>';
      html += '<div class="dataset-list">';
      for (let i = 0; i < this.dataset.length; i++) {
        const spec = this.dataset[i];
        const id = spec.metadata?.specimen_id || `specimen_${i + 1}`;
        html += `<div class="dataset-item" data-index="${i}">`;
        html += `<span class="dataset-id">${id}</span>`;
        html += `<span class="dataset-meta">${Object.keys(spec.landmarks).length} lm</span>`;
        html += `<button class="btn-tiny btn-remove-specimen" data-index="${i}" title="Remove">×</button>`;
        html += `</div>`;
      }
      html += '</div></div>';
    }

    panel.innerHTML = html;

    // Attach remove handlers
    panel.querySelectorAll('.btn-remove-specimen').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const idx = parseInt(e.target.dataset.index);
        this.dataset.splice(idx, 1);
        this._updateAnalysisPanel();
      });
    });
  }

  _renderMiniPCA(pca, gpa) {
    if (!pca || pca.nComponents < 1) return '';

    // Build a small SVG scatter plot
    const w = 240, h = 180, pad = 30;

    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
    for (const scores of pca.scores) {
      if (scores[0] < minX) minX = scores[0];
      if (scores[0] > maxX) maxX = scores[0];
      if (pca.nComponents > 1) {
        if (scores[1] < minY) minY = scores[1];
        if (scores[1] > maxY) maxY = scores[1];
      }
    }

    // Pad range
    const rangeX = (maxX - minX) || 1;
    const rangeY = pca.nComponents > 1 ? ((maxY - minY) || 1) : 1;
    minX -= rangeX * 0.15; maxX += rangeX * 0.15;
    if (pca.nComponents > 1) { minY -= rangeY * 0.15; maxY += rangeY * 0.15; }

    const scaleX = (w - pad * 2) / (maxX - minX);
    const scaleY = pca.nComponents > 1 ? (h - pad * 2) / (maxY - minY) : 1;

    let svg = `<svg viewBox="0 0 ${w} ${h}" class="pca-plot">`;

    // Axes
    svg += `<line x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}" stroke="#555" stroke-width="1"/>`;
    svg += `<line x1="${pad}" y1="${pad}" x2="${pad}" y2="${h - pad}" stroke="#555" stroke-width="1"/>`;

    // Labels
    svg += `<text x="${w / 2}" y="${h - 4}" text-anchor="middle" fill="#999" font-size="10">PC1 (${(pca.varExplained[0] * 100).toFixed(1)}%)</text>`;
    if (pca.nComponents > 1) {
      svg += `<text x="8" y="${h / 2}" text-anchor="middle" fill="#999" font-size="10" transform="rotate(-90, 8, ${h / 2})">PC2 (${(pca.varExplained[1] * 100).toFixed(1)}%)</text>`;
    }

    // Points
    const colors = ['#e74c3c', '#3498db', '#2ecc71', '#f39c12', '#9b59b6', '#1abc9c', '#e67e22', '#34495e'];
    for (let i = 0; i < pca.scores.length; i++) {
      const x = pad + (pca.scores[i][0] - minX) * scaleX;
      const y = pca.nComponents > 1
        ? (h - pad) - (pca.scores[i][1] - minY) * scaleY
        : h / 2;
      const color = colors[i % colors.length];
      svg += `<circle cx="${x}" cy="${y}" r="5" fill="${color}" stroke="#fff" stroke-width="1.5"/>`;
      // Label
      const label = pca.ids[i].length > 12 ? pca.ids[i].slice(0, 12) + '…' : pca.ids[i];
      svg += `<text x="${x + 8}" y="${y + 3}" fill="#ccc" font-size="9">${label}</text>`;
    }

    svg += '</svg>';
    return svg;
  }

  // --- Export Controls ---

  _setupExportControls() {
    document.getElementById('btn-export-stereomorph')?.addEventListener('click', () => this._exportCurrent('stereomorph'));
    document.getElementById('btn-export-json')?.addEventListener('click', () => this._exportCurrent('json'));
    document.getElementById('btn-export-tps')?.addEventListener('click', () => this._exportCurrent('tps'));
    document.getElementById('btn-export-dataset-csv')?.addEventListener('click', () => this._exportDataset('csv'));
    document.getElementById('btn-export-dataset-bundle')?.addEventListener('click', () => this._exportDataset('bundle'));
  }

  _exportCurrent(format) {
    this._collectMetadata();
    const specimen = {
      landmarks: this.currentLandmarks,
      curves: this.currentCurves,
      nWedges: this.nWedges,
      metadata: this.metadata,
      imageDimensions: this.imageDimensions
    };

    const id = this.metadata.specimen_id || 'specimen';

    switch (format) {
      case 'stereomorph': {
        const content = exportStereoMorph(specimen);
        downloadFile(content, `${id}.txt`, 'text/plain');
        break;
      }
      case 'json': {
        const content = exportCuneimorphJSON(specimen);
        downloadFile(content, `${id}.json`, 'application/json');
        break;
      }
      case 'tps': {
        const content = exportTPS(specimen);
        downloadFile(content, `${id}.tps`, 'text/plain');
        break;
      }
    }
  }

  _exportDataset(format) {
    switch (format) {
      case 'csv': {
        const content = exportDatasetCSV(this.dataset);
        downloadFile(content, 'cuneimorph_dataset.csv', 'text/csv');
        break;
      }
      case 'bundle': {
        const content = exportProjectBundle(this.dataset, { name: 'Cuneimorph annotation session' });
        downloadFile(content, 'cuneimorph_project.json', 'application/json');
        break;
      }
    }
  }

  // --- Dataset Controls ---

  _setupDatasetControls() {
    document.getElementById('btn-save-to-dataset')?.addEventListener('click', () => this._saveCurrentToDataset());
    document.getElementById('btn-new-specimen')?.addEventListener('click', () => this._newSpecimen());
  }

  _saveCurrentToDataset() {
    this._collectMetadata();

    if (Object.keys(this.currentLandmarks).length === 0) {
      alert('No landmarks placed yet.');
      return;
    }

    const duration = this.annotationStartTime
      ? Math.round((Date.now() - this.annotationStartTime) / 1000)
      : null;

    const specimen = {
      id: this.metadata.specimen_id || `specimen_${this.dataset.length + 1}`,
      landmarks: { ...this.currentLandmarks },
      curves: { ...this.currentCurves },
      nWedges: this.nWedges,
      metadata: { ...this.metadata, duration_seconds: duration },
      imageDimensions: this.imageDimensions
    };

    this.dataset.push(specimen);
    this._updateAnalysisPanel();

    // Visual feedback
    const btn = document.getElementById('btn-save-to-dataset');
    if (btn) {
      btn.textContent = 'Saved!';
      btn.classList.add('btn-success');
      setTimeout(() => { btn.textContent = 'Save to Dataset'; btn.classList.remove('btn-success'); }, 1500);
    }
  }

  _newSpecimen() {
    if (Object.keys(this.currentLandmarks).length > 0) {
      if (!confirm('Start new specimen? Current unsaved landmarks will be lost.')) return;
    }

    this.currentLandmarks = {};
    this.currentCurves = {};
    this.undoStack = [];
    this.currentLandmarkIndex = 0;
    this.currentCurveIndex = 0;
    this.imageDimensions = null;
    this.imageFileName = '';
    this.annotationStartTime = null;
    this.canvas.clearAll();
    this.canvas.image = null;
    this._setMode('landmark');
    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();

    document.getElementById('image-info').textContent = 'No image loaded';
    document.getElementById('specimen-id-input').value = '';
    this.canvas.render();
  }

  // --- Import Controls ---

  _setupImportControls() {
    document.getElementById('btn-import-annotation')?.addEventListener('click', () => {
      document.getElementById('import-file-input')?.click();
    });

    document.getElementById('import-file-input')?.addEventListener('change', (e) => {
      const file = e.target.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = (ev) => this._importAnnotation(ev.target.result, file.name);
      reader.readAsText(file);
      e.target.value = ''; // reset for re-import
    });
  }

  _importAnnotation(text, filename) {
    let data;
    try {
      if (filename.endsWith('.json')) {
        data = importCuneimorphJSON(text);
      } else {
        data = importStereoMorph(text);
      }
    } catch (err) {
      alert(`Failed to import: ${err.message}`);
      return;
    }

    // Apply imported data
    this._updateWedgeCount(data.nWedges);
    document.getElementById('wedge-count').value = data.nWedges;

    // Place landmarks on canvas
    this.currentLandmarks = data.landmarks;
    for (const [id, coords] of Object.entries(data.landmarks)) {
      const match = id.match(/^(\w+?)(\d+)$/);
      if (match) {
        const type = match[1];
        const wedge = parseInt(match[2]);
        const def = LANDMARK_TYPES[type] || {};
        this.canvas.setLandmark(id, coords[0], coords[1], {
          color: def.color || '#fff',
          symbol: def.symbol || 'circle',
          wedge
        });
      }
    }
    this.currentLandmarkIndex = Object.keys(data.landmarks).length;

    // Place curves (use pixel data if available, otherwise control)
    const curveSource = Object.keys(data.curvesPixel).length > 0 ? data.curvesPixel : data.curvesControl;
    for (const [id, points] of Object.entries(curveSource)) {
      this.currentCurves[id] = { points };
      const match = id.match(/^(\w+?)(\d+)$/);
      const type = match ? match[1] : id;
      const def = CURVE_TYPES[type] || {};
      this.canvas.setCurve(id, points, { color: def.color || '#9b59b6', label: def.label || id });
    }
    this.currentCurveIndex = Object.keys(this.currentCurves).length;

    // Apply metadata if available
    if (data.metadata) {
      for (const [key, value] of Object.entries(data.metadata)) {
        const input = document.querySelector(`[name="${key}"]`);
        if (input && value) input.value = value;
      }
      if (data.metadata.specimen_id) {
        document.getElementById('specimen-id-input').value = data.metadata.specimen_id;
      }
    }

    this._renderProtocolGuide();
    this._updateProgressBar();
    this._updateAnalysisPanel();
    this.canvas.render();
  }

  // --- Keyboard Shortcuts ---

  _setupKeyboardShortcuts() {
    document.addEventListener('keydown', (e) => {
      // Don't capture when typing in input fields
      if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' || e.target.tagName === 'SELECT') return;

      switch (e.key) {
        case 'l': case 'L':
          this._setMode('landmark');
          break;
        case 'c': case 'C':
          this._setMode('curve');
          break;
        case 'n': case 'N':
          this._setMode('navigate');
          break;
        case 'Enter':
          if (this.mode === 'curve') this.finishCurrentCurve();
          break;
        case 'Escape':
          if (this.mode === 'curve') {
            this.canvas.cancelCurveDraw();
            this._renderProtocolGuide();
          }
          break;
        case 'z':
          if (e.ctrlKey || e.metaKey) {
            e.preventDefault();
            this._undo();
          }
          break;
        case 'f': case 'F':
          this.canvas.fitToView();
          this.canvas.render();
          break;
        case 't': case 'T':
          this.canvas.showLabels = !this.canvas.showLabels;
          this.canvas.render();
          break;
      }
    });
  }

  // --- Panel visibility ---

  _showPanel(id) {
    // Expand the metadata panel when showing it
    const panel = document.getElementById(id);
    if (panel) panel.classList.remove('collapsed');
  }
}

// Initialize
window.app = new CuneimorphAnnotator();
