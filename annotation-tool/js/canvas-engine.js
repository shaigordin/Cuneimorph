/**
 * Canvas Engine for the Cuneimorph Annotator
 *
 * Handles: image rendering, pan/zoom, coordinate transforms,
 * landmark rendering, curve rendering, hit-testing.
 * All coordinates internally stored as image-space pixels.
 */

export class CanvasEngine {
  constructor(canvasElement) {
    this.canvas = canvasElement;
    this.ctx = canvasElement.getContext('2d');

    // Image state
    this.image = null;
    this.imageWidth = 0;
    this.imageHeight = 0;

    // View transform
    this.offsetX = 0;
    this.offsetY = 0;
    this.scale = 1;
    this.minScale = 0.1;
    this.maxScale = 20;

    // Interaction state
    this.isPanning = false;
    this.panStartX = 0;
    this.panStartY = 0;
    this.draggedLandmark = null;

    // Data
    this.landmarks = new Map();   // id -> {x, y, ...def}
    this.curves = new Map();      // id -> {points: [[x,y],...], ...def}
    this.activeCurve = null;      // curve currently being drawn

    // Display settings
    this.landmarkRadius = 6;
    this.showLabels = true;
    this.showCurves = true;
    this.highlightWedge = null;
    this.crosshairActive = false;

    this._setupEventListeners();
    this._resizeCanvas();

    // Handle window resize
    this._resizeObserver = new ResizeObserver(() => this._resizeCanvas());
    this._resizeObserver.observe(this.canvas.parentElement);
  }

  // --- Public API ---

  loadImage(src) {
    return new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => {
        this.image = img;
        this.imageWidth = img.naturalWidth;
        this.imageHeight = img.naturalHeight;
        this.fitToView();
        this.render();
        resolve({ width: img.naturalWidth, height: img.naturalHeight });
      };
      img.onerror = reject;
      img.src = src;
    });
  }

  loadImageFromFile(file) {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = (e) => this.loadImage(e.target.result).then(resolve).catch(reject);
      reader.onerror = reject;
      reader.readAsDataURL(file);
    });
  }

  fitToView() {
    if (!this.image) return;
    const padding = 40;
    const scaleX = (this.canvas.width - padding * 2) / this.imageWidth;
    const scaleY = (this.canvas.height - padding * 2) / this.imageHeight;
    this.scale = Math.min(scaleX, scaleY);
    this.offsetX = (this.canvas.width - this.imageWidth * this.scale) / 2;
    this.offsetY = (this.canvas.height - this.imageHeight * this.scale) / 2;
  }

  setLandmark(id, x, y, def) {
    this.landmarks.set(id, { x, y, ...def });
    this.render();
  }

  removeLandmark(id) {
    this.landmarks.delete(id);
    this.render();
  }

  getLandmark(id) {
    return this.landmarks.get(id);
  }

  setCurve(id, points, def) {
    this.curves.set(id, { points: [...points], ...def });
    this.render();
  }

  removeCurve(id) {
    this.curves.delete(id);
    this.render();
  }

  clearAll() {
    this.landmarks.clear();
    this.curves.clear();
    this.activeCurve = null;
    this.render();
  }

  startCurveDraw(id, def) {
    this.activeCurve = { id, points: [], ...def };
  }

  addCurvePoint(x, y) {
    if (!this.activeCurve) return;
    this.activeCurve.points.push([x, y]);
    this.render();
  }

  finishCurveDraw() {
    if (!this.activeCurve) return null;
    const curve = this.activeCurve;
    if (curve.points.length >= 2) {
      this.curves.set(curve.id, {
        points: curve.points,
        color: curve.color,
        label: curve.label
      });
    }
    this.activeCurve = null;
    this.render();
    return curve;
  }

  cancelCurveDraw() {
    this.activeCurve = null;
    this.render();
  }

  // Coordinate transforms
  imageToCanvas(ix, iy) {
    return [
      ix * this.scale + this.offsetX,
      iy * this.scale + this.offsetY
    ];
  }

  canvasToImage(cx, cy) {
    return [
      (cx - this.offsetX) / this.scale,
      (cy - this.offsetY) / this.scale
    ];
  }

  getMouseImageCoords(e) {
    const rect = this.canvas.getBoundingClientRect();
    const cx = e.clientX - rect.left;
    const cy = e.clientY - rect.top;
    return this.canvasToImage(cx, cy);
  }

  // Hit test against placed landmarks
  hitTestLandmark(cx, cy, threshold = 12) {
    let closest = null;
    let closestDist = threshold;
    for (const [id, lm] of this.landmarks) {
      const [lx, ly] = this.imageToCanvas(lm.x, lm.y);
      const d = Math.hypot(cx - lx, cy - ly);
      if (d < closestDist) {
        closestDist = d;
        closest = id;
      }
    }
    return closest;
  }

  // --- Rendering ---

  render() {
    const ctx = this.ctx;
    const w = this.canvas.width;
    const h = this.canvas.height;

    // Clear
    ctx.fillStyle = '#1a1a2e';
    ctx.fillRect(0, 0, w, h);

    if (!this.image) {
      this._drawDropZone();
      return;
    }

    // Draw image
    ctx.save();
    ctx.imageSmoothingEnabled = this.scale < 1;
    ctx.drawImage(
      this.image,
      this.offsetX, this.offsetY,
      this.imageWidth * this.scale,
      this.imageHeight * this.scale
    );
    ctx.restore();

    // Draw curves
    if (this.showCurves) {
      for (const [id, curve] of this.curves) {
        this._drawCurve(curve, false);
      }
      // Active curve being drawn
      if (this.activeCurve && this.activeCurve.points.length > 0) {
        this._drawCurve(this.activeCurve, true);
      }
    }

    // Draw landmarks
    for (const [id, lm] of this.landmarks) {
      const isHighlighted = this.highlightWedge !== null && lm.wedge === this.highlightWedge;
      this._drawLandmark(id, lm, isHighlighted);
    }

    // Crosshair
    if (this.crosshairActive && this._lastMouseX !== undefined) {
      this._drawCrosshair(this._lastMouseX, this._lastMouseY);
    }
  }

  _drawDropZone() {
    const ctx = this.ctx;
    const w = this.canvas.width;
    const h = this.canvas.height;

    ctx.strokeStyle = '#4a4a6a';
    ctx.lineWidth = 2;
    ctx.setLineDash([10, 10]);
    const margin = 40;
    ctx.strokeRect(margin, margin, w - margin * 2, h - margin * 2);
    ctx.setLineDash([]);

    ctx.fillStyle = '#8888aa';
    ctx.font = '18px system-ui, sans-serif';
    ctx.textAlign = 'center';
    ctx.fillText('Drop an image here or click "Load Image"', w / 2, h / 2 - 10);
    ctx.font = '14px system-ui, sans-serif';
    ctx.fillStyle = '#666688';
    ctx.fillText('Supports JPG, PNG, TIFF', w / 2, h / 2 + 20);
  }

  _drawLandmark(id, lm, highlighted) {
    const ctx = this.ctx;
    const [cx, cy] = this.imageToCanvas(lm.x, lm.y);
    const r = this.landmarkRadius * (highlighted ? 1.5 : 1);

    // Outer glow for highlighted
    if (highlighted) {
      ctx.beginPath();
      ctx.arc(cx, cy, r + 4, 0, Math.PI * 2);
      ctx.fillStyle = lm.color + '40';
      ctx.fill();
    }

    // Shape based on landmark type
    ctx.beginPath();
    if (lm.symbol === 'diamond') {
      ctx.moveTo(cx, cy - r);
      ctx.lineTo(cx + r, cy);
      ctx.lineTo(cx, cy + r);
      ctx.lineTo(cx - r, cy);
      ctx.closePath();
    } else if (lm.symbol === 'triangle') {
      ctx.moveTo(cx, cy - r);
      ctx.lineTo(cx + r * 0.87, cy + r * 0.5);
      ctx.lineTo(cx - r * 0.87, cy + r * 0.5);
      ctx.closePath();
    } else {
      ctx.arc(cx, cy, r, 0, Math.PI * 2);
    }

    ctx.fillStyle = lm.color;
    ctx.fill();
    ctx.strokeStyle = '#ffffff';
    ctx.lineWidth = 1.5;
    ctx.stroke();

    // Label
    if (this.showLabels) {
      const labelText = id.replace(/(\d+)$/, ' $1');
      ctx.font = '11px system-ui, sans-serif';
      ctx.fillStyle = '#ffffff';
      ctx.strokeStyle = '#000000';
      ctx.lineWidth = 3;
      ctx.textAlign = 'left';
      ctx.strokeText(labelText, cx + r + 4, cy + 4);
      ctx.fillText(labelText, cx + r + 4, cy + 4);
    }
  }

  _drawCurve(curve, isActive) {
    const ctx = this.ctx;
    if (curve.points.length < 2) {
      // Single point — draw dot
      if (curve.points.length === 1) {
        const [cx, cy] = this.imageToCanvas(curve.points[0][0], curve.points[0][1]);
        ctx.beginPath();
        ctx.arc(cx, cy, 3, 0, Math.PI * 2);
        ctx.fillStyle = curve.color || '#9b59b6';
        ctx.fill();
      }
      return;
    }

    ctx.beginPath();
    const [sx, sy] = this.imageToCanvas(curve.points[0][0], curve.points[0][1]);
    ctx.moveTo(sx, sy);

    for (let i = 1; i < curve.points.length; i++) {
      const [px, py] = this.imageToCanvas(curve.points[i][0], curve.points[i][1]);
      ctx.lineTo(px, py);
    }

    ctx.strokeStyle = curve.color || '#9b59b6';
    ctx.lineWidth = isActive ? 2.5 : 2;
    ctx.globalAlpha = isActive ? 0.8 : 0.6;
    if (isActive) {
      ctx.setLineDash([6, 3]);
    }
    ctx.stroke();
    ctx.setLineDash([]);
    ctx.globalAlpha = 1;

    // Draw control points on curves
    for (const pt of curve.points) {
      const [px, py] = this.imageToCanvas(pt[0], pt[1]);
      ctx.beginPath();
      ctx.arc(px, py, 2.5, 0, Math.PI * 2);
      ctx.fillStyle = curve.color || '#9b59b6';
      ctx.globalAlpha = 0.7;
      ctx.fill();
      ctx.globalAlpha = 1;
    }
  }

  _drawCrosshair(mx, my) {
    const ctx = this.ctx;
    ctx.strokeStyle = '#ffffff44';
    ctx.lineWidth = 0.5;

    ctx.beginPath();
    ctx.moveTo(mx, 0);
    ctx.lineTo(mx, this.canvas.height);
    ctx.moveTo(0, my);
    ctx.lineTo(this.canvas.width, my);
    ctx.stroke();

    // Show image coordinates
    const [ix, iy] = this.canvasToImage(mx, my);
    if (ix >= 0 && iy >= 0 && ix <= this.imageWidth && iy <= this.imageHeight) {
      ctx.font = '11px monospace';
      ctx.fillStyle = '#ffffffaa';
      ctx.textAlign = 'left';
      ctx.fillText(`${Math.round(ix)}, ${Math.round(iy)}`, mx + 12, my - 8);
    }
  }

  // --- Event handling ---

  _setupEventListeners() {
    this.canvas.addEventListener('wheel', (e) => this._onWheel(e), { passive: false });
    this.canvas.addEventListener('mousedown', (e) => this._onMouseDown(e));
    this.canvas.addEventListener('mousemove', (e) => this._onMouseMove(e));
    this.canvas.addEventListener('mouseup', (e) => this._onMouseUp(e));
    this.canvas.addEventListener('mouseleave', () => {
      this.crosshairActive = false;
      this.render();
    });
    this.canvas.addEventListener('mouseenter', () => {
      this.crosshairActive = true;
    });

    // Touch support
    this.canvas.addEventListener('touchstart', (e) => this._onTouchStart(e), { passive: false });
    this.canvas.addEventListener('touchmove', (e) => this._onTouchMove(e), { passive: false });
    this.canvas.addEventListener('touchend', (e) => this._onTouchEnd(e));

    // External click handler set by the app
    this.onClick = null;
    this.onLandmarkDrag = null;
    this.onCursorMove = null;
  }

  _onWheel(e) {
    e.preventDefault();
    const rect = this.canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;

    const zoomFactor = e.deltaY < 0 ? 1.15 : 1 / 1.15;
    const newScale = Math.max(this.minScale, Math.min(this.maxScale, this.scale * zoomFactor));

    // Zoom toward cursor
    this.offsetX = mx - (mx - this.offsetX) * (newScale / this.scale);
    this.offsetY = my - (my - this.offsetY) * (newScale / this.scale);
    this.scale = newScale;

    this.render();
  }

  _onMouseDown(e) {
    if (e.button === 1 || (e.button === 0 && e.altKey)) {
      // Middle click or Alt+click for panning
      this.isPanning = true;
      this.panStartX = e.clientX - this.offsetX;
      this.panStartY = e.clientY - this.offsetY;
      this.canvas.style.cursor = 'grabbing';
      return;
    }

    if (e.button === 0) {
      // Check if clicking an existing landmark for drag
      const rect = this.canvas.getBoundingClientRect();
      const cx = e.clientX - rect.left;
      const cy = e.clientY - rect.top;
      const hit = this.hitTestLandmark(cx, cy);
      if (hit && e.shiftKey) {
        this.draggedLandmark = hit;
        this.canvas.style.cursor = 'move';
        return;
      }
    }
  }

  _onMouseMove(e) {
    const rect = this.canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;
    this._lastMouseX = mx;
    this._lastMouseY = my;

    if (this.isPanning) {
      this.offsetX = e.clientX - this.panStartX;
      this.offsetY = e.clientY - this.panStartY;
      this.render();
      return;
    }

    if (this.draggedLandmark) {
      const [ix, iy] = this.canvasToImage(mx, my);
      const lm = this.landmarks.get(this.draggedLandmark);
      if (lm) {
        lm.x = ix;
        lm.y = iy;
        if (this.onLandmarkDrag) {
          this.onLandmarkDrag(this.draggedLandmark, ix, iy);
        }
      }
      this.render();
      return;
    }

    // Cursor feedback
    if (this.onCursorMove) {
      const [ix, iy] = this.canvasToImage(mx, my);
      this.onCursorMove(ix, iy);
    }
    this.render();
  }

  _onMouseUp(e) {
    if (this.isPanning) {
      this.isPanning = false;
      this.canvas.style.cursor = '';
      return;
    }

    if (this.draggedLandmark) {
      this.draggedLandmark = null;
      this.canvas.style.cursor = '';
      return;
    }

    if (e.button === 0 && this.onClick) {
      const [ix, iy] = this.getMouseImageCoords(e);
      // Only trigger if within image bounds
      if (ix >= 0 && iy >= 0 && ix <= this.imageWidth && iy <= this.imageHeight) {
        this.onClick(ix, iy, e);
      }
    }
  }

  _onTouchStart(e) {
    if (e.touches.length === 1) {
      const touch = e.touches[0];
      this.isPanning = true;
      this.panStartX = touch.clientX - this.offsetX;
      this.panStartY = touch.clientY - this.offsetY;
    } else if (e.touches.length === 2) {
      this._pinchStart = this._getPinchData(e);
    }
    e.preventDefault();
  }

  _onTouchMove(e) {
    if (e.touches.length === 1 && this.isPanning) {
      const touch = e.touches[0];
      this.offsetX = touch.clientX - this.panStartX;
      this.offsetY = touch.clientY - this.panStartY;
      this.render();
    } else if (e.touches.length === 2 && this._pinchStart) {
      const pinch = this._getPinchData(e);
      const scaleChange = pinch.distance / this._pinchStart.distance;
      const newScale = Math.max(this.minScale, Math.min(this.maxScale, this._pinchStart.scale * scaleChange));
      this.offsetX = pinch.midX - (this._pinchStart.midX - this._pinchStart.offsetX) * (newScale / this._pinchStart.scale);
      this.offsetY = pinch.midY - (this._pinchStart.midY - this._pinchStart.offsetY) * (newScale / this._pinchStart.scale);
      this.scale = newScale;
      this.render();
    }
    e.preventDefault();
  }

  _onTouchEnd(e) {
    this.isPanning = false;
    this._pinchStart = null;
  }

  _getPinchData(e) {
    const t1 = e.touches[0], t2 = e.touches[1];
    const rect = this.canvas.getBoundingClientRect();
    return {
      distance: Math.hypot(t2.clientX - t1.clientX, t2.clientY - t1.clientY),
      midX: (t1.clientX + t2.clientX) / 2 - rect.left,
      midY: (t1.clientY + t2.clientY) / 2 - rect.top,
      scale: this.scale,
      offsetX: this.offsetX,
      offsetY: this.offsetY
    };
  }

  _resizeCanvas() {
    const parent = this.canvas.parentElement;
    if (!parent) return;
    const dpr = window.devicePixelRatio || 1;
    const rect = parent.getBoundingClientRect();
    this.canvas.width = rect.width * dpr;
    this.canvas.height = rect.height * dpr;
    this.canvas.style.width = rect.width + 'px';
    this.canvas.style.height = rect.height + 'px';
    this.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    // Adjust stored dimensions to CSS pixels
    this.canvas.width = rect.width;
    this.canvas.height = rect.height;
    this.render();
  }

  destroy() {
    this._resizeObserver?.disconnect();
  }
}
