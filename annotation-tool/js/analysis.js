/**
 * Client-side Geometric Morphometric Analysis
 *
 * Implements Procrustes superimposition + PCA in pure JavaScript.
 * Inspired by the MomX / Momocs ecosystem and compatible with geomorph outputs.
 *
 * No external dependencies — uses standard linear algebra operations.
 * Designed for real-time feedback during annotation sessions.
 */

export class MorphoAnalysis {
  constructor() {
    this.specimens = new Map(); // id -> { landmarks: [[x,y],...], metadata: {} }
    this.results = null;
  }

  /**
   * Add or update a specimen's landmarks.
   * @param {string} id - Specimen identifier
   * @param {Object} landmarks - Map of landmark names to [x,y] coordinates
   * @param {Object} metadata - Specimen metadata
   */
  setSpecimen(id, landmarks, metadata = {}) {
    // Convert named landmarks to ordered array (consistent ordering by name)
    const names = Object.keys(landmarks).sort();
    const coords = names.map(n => [...landmarks[n]]);
    this.specimens.set(id, { landmarks: coords, names, metadata, raw: landmarks });
  }

  removeSpecimen(id) {
    this.specimens.delete(id);
  }

  /**
   * Run Generalized Procrustes Analysis on all loaded specimens.
   * Returns aligned coordinates, centroid sizes, and consensus shape.
   */
  runGPA(maxIterations = 50, tolerance = 1e-8) {
    const ids = [...this.specimens.keys()];
    if (ids.length < 2) return null;

    const n = ids.length;
    const spec0 = this.specimens.get(ids[0]);
    const p = spec0.landmarks.length; // number of landmarks
    const k = 2; // dimensions (2D)

    // Check all specimens have same number of landmarks
    for (const id of ids) {
      if (this.specimens.get(id).landmarks.length !== p) {
        console.warn(`Specimen ${id} has ${this.specimens.get(id).landmarks.length} landmarks, expected ${p}`);
        return null;
      }
    }

    // Step 1: Extract coordinate matrices and compute centroid sizes
    let matrices = ids.map(id => {
      const lm = this.specimens.get(id).landmarks;
      return lm.map(pt => [pt[0], pt[1]]);
    });

    const centroidSizes = matrices.map(m => this._centroidSize(m));

    // Step 2: Center all specimens (translate centroid to origin)
    matrices = matrices.map(m => this._center(m));

    // Step 3: Scale to unit centroid size
    matrices = matrices.map((m, i) => {
      const cs = centroidSizes[i];
      return m.map(pt => [pt[0] / cs, pt[1] / cs]);
    });

    // Step 4: Iterative Procrustes alignment
    // Use first specimen as initial reference
    let reference = matrices[0].map(pt => [...pt]);
    let prevSS = Infinity;

    for (let iter = 0; iter < maxIterations; iter++) {
      // Align all to reference
      const aligned = matrices.map(m => this._procrustesRotate(m, reference));

      // Compute new consensus (mean shape)
      const consensus = this._meanShape(aligned);

      // Compute sum of squared distances
      let ss = 0;
      for (const m of aligned) {
        for (let i = 0; i < p; i++) {
          ss += (m[i][0] - consensus[i][0]) ** 2 + (m[i][1] - consensus[i][1]) ** 2;
        }
      }

      matrices = aligned;
      reference = consensus;

      if (Math.abs(prevSS - ss) < tolerance) break;
      prevSS = ss;
    }

    // Step 5: Compute Procrustes distances from consensus
    const procDistances = matrices.map(m => {
      let d = 0;
      for (let i = 0; i < p; i++) {
        d += (m[i][0] - reference[i][0]) ** 2 + (m[i][1] - reference[i][1]) ** 2;
      }
      return Math.sqrt(d);
    });

    this.results = {
      ids,
      aligned: matrices,
      consensus: reference,
      centroidSizes,
      procDistances,
      nLandmarks: p,
      nSpecimens: n
    };

    return this.results;
  }

  /**
   * Run PCA on Procrustes-aligned coordinates.
   * Returns PC scores, eigenvalues, and variance explained.
   */
  runPCA() {
    if (!this.results) return null;

    const { aligned, ids, nLandmarks } = this.results;
    const n = aligned.length;
    const p = nLandmarks * 2; // flatten x,y to single vector

    // Flatten aligned coordinates to data matrix (n specimens × p variables)
    const data = aligned.map(m => {
      const flat = [];
      for (const pt of m) {
        flat.push(pt[0], pt[1]);
      }
      return flat;
    });

    // Center the data (subtract column means)
    const means = new Array(p).fill(0);
    for (const row of data) {
      for (let j = 0; j < p; j++) means[j] += row[j];
    }
    for (let j = 0; j < p; j++) means[j] /= n;

    const centered = data.map(row => row.map((v, j) => v - means[j]));

    // Compute covariance matrix (p × p)
    // For efficiency when n < p, compute the n×n matrix and use duality
    if (n < p) {
      // Small-n PCA: compute T = (1/(n-1)) * X * X^T (n×n matrix)
      const T = [];
      for (let i = 0; i < n; i++) {
        T[i] = [];
        for (let j = 0; j < n; j++) {
          let s = 0;
          for (let k = 0; k < p; k++) s += centered[i][k] * centered[j][k];
          T[i][j] = s / Math.max(1, n - 1);
        }
      }

      // Eigendecomposition of T using Jacobi iteration
      const { eigenvalues, eigenvectors } = this._symmetricEigen(T);

      // Convert to PC scores and loadings
      // PC scores = sqrt(eigenvalues) * eigenvectors of T
      const nComponents = Math.min(n - 1, p);
      const scores = [];
      const varExplained = [];
      const totalVar = eigenvalues.reduce((s, v) => s + Math.max(0, v), 0);

      for (let i = 0; i < n; i++) {
        const pcScores = [];
        for (let c = 0; c < nComponents; c++) {
          pcScores.push(eigenvectors[i][c] * Math.sqrt(Math.max(0, eigenvalues[c]) * (n - 1)));
        }
        scores.push(pcScores);
      }

      for (let c = 0; c < nComponents; c++) {
        varExplained.push(eigenvalues[c] / totalVar);
      }

      this.results.pca = {
        scores,
        eigenvalues: eigenvalues.slice(0, nComponents),
        varExplained: varExplained.slice(0, nComponents),
        nComponents,
        ids
      };
    } else {
      // Standard PCA with covariance matrix
      const cov = [];
      for (let i = 0; i < p; i++) {
        cov[i] = [];
        for (let j = 0; j < p; j++) {
          let s = 0;
          for (let k = 0; k < n; k++) s += centered[k][i] * centered[k][j];
          cov[i][j] = s / Math.max(1, n - 1);
        }
      }

      const { eigenvalues, eigenvectors: loadings } = this._symmetricEigen(cov);
      const nComponents = Math.min(n - 1, p);

      const scores = centered.map(row => {
        const s = [];
        for (let c = 0; c < nComponents; c++) {
          let dot = 0;
          for (let j = 0; j < p; j++) dot += row[j] * loadings[j][c];
          s.push(dot);
        }
        return s;
      });

      const totalVar = eigenvalues.reduce((s, v) => s + Math.max(0, v), 0);
      const varExplained = eigenvalues.slice(0, nComponents).map(v => v / totalVar);

      this.results.pca = {
        scores,
        eigenvalues: eigenvalues.slice(0, nComponents),
        varExplained,
        nComponents,
        ids
      };
    }

    return this.results.pca;
  }

  /**
   * Compute per-landmark variance across specimens.
   * Useful for identifying which landmarks vary most.
   */
  landmarkVariance() {
    if (!this.results) return null;
    const { aligned, consensus, nLandmarks } = this.results;
    const n = aligned.length;

    const variance = [];
    for (let i = 0; i < nLandmarks; i++) {
      let v = 0;
      for (const m of aligned) {
        v += (m[i][0] - consensus[i][0]) ** 2 + (m[i][1] - consensus[i][1]) ** 2;
      }
      variance.push(v / n);
    }
    return variance;
  }

  /**
   * Compute morphological disparity (Procrustes variance).
   */
  disparity() {
    if (!this.results) return null;
    const { procDistances } = this.results;
    const mean = procDistances.reduce((s, d) => s + d, 0) / procDistances.length;
    const variance = procDistances.reduce((s, d) => s + (d - mean) ** 2, 0) / procDistances.length;
    return { mean, variance, sd: Math.sqrt(variance) };
  }

  // --- Internal math utilities ---

  _centroidSize(matrix) {
    const n = matrix.length;
    let cx = 0, cy = 0;
    for (const pt of matrix) { cx += pt[0]; cy += pt[1]; }
    cx /= n; cy /= n;
    let ss = 0;
    for (const pt of matrix) {
      ss += (pt[0] - cx) ** 2 + (pt[1] - cy) ** 2;
    }
    return Math.sqrt(ss);
  }

  _center(matrix) {
    const n = matrix.length;
    let cx = 0, cy = 0;
    for (const pt of matrix) { cx += pt[0]; cy += pt[1]; }
    cx /= n; cy /= n;
    return matrix.map(pt => [pt[0] - cx, pt[1] - cy]);
  }

  _procrustesRotate(target, reference) {
    // Optimal rotation using SVD of cross-covariance matrix
    // M = ref^T * target
    const p = target.length;

    let m00 = 0, m01 = 0, m10 = 0, m11 = 0;
    for (let i = 0; i < p; i++) {
      m00 += reference[i][0] * target[i][0];
      m01 += reference[i][0] * target[i][1];
      m10 += reference[i][1] * target[i][0];
      m11 += reference[i][1] * target[i][1];
    }

    // SVD of 2x2 matrix: fast analytical solution
    const { U, V } = this._svd2x2(m00, m01, m10, m11);

    // Rotation = V * U^T
    const r00 = V[0][0] * U[0][0] + V[0][1] * U[0][1];
    const r01 = V[0][0] * U[1][0] + V[0][1] * U[1][1];
    const r10 = V[1][0] * U[0][0] + V[1][1] * U[0][1];
    const r11 = V[1][0] * U[1][0] + V[1][1] * U[1][1];

    // Check for reflection
    const det = r00 * r11 - r01 * r10;
    if (det < 0) {
      // Flip sign of last column of V
      return target.map(pt => [
        pt[0] * r00 + pt[1] * r10,
        -(pt[0] * r01 + pt[1] * r11)
      ]);
    }

    return target.map(pt => [
      pt[0] * r00 + pt[1] * r10,
      pt[0] * r01 + pt[1] * r11
    ]);
  }

  _svd2x2(a, b, c, d) {
    // Analytical SVD for 2x2 matrix [[a,b],[c,d]]
    const ata00 = a * a + c * c;
    const ata01 = a * b + c * d;
    const ata11 = b * b + d * d;

    // Eigenvalues of A^T A
    const trace = ata00 + ata11;
    const det = ata00 * ata11 - ata01 * ata01;
    const disc = Math.sqrt(Math.max(0, trace * trace / 4 - det));
    const s1sq = trace / 2 + disc;
    const s2sq = trace / 2 - disc;

    // Eigenvectors of A^T A (= V)
    let V;
    if (Math.abs(ata01) > 1e-12) {
      const v1 = [ata01, s1sq - ata00];
      const v2 = [ata01, s2sq - ata00];
      const n1 = Math.hypot(v1[0], v1[1]);
      const n2 = Math.hypot(v2[0], v2[1]);
      V = [
        [v1[0] / n1, v2[0] / n2],
        [v1[1] / n1, v2[1] / n2]
      ];
    } else {
      V = [[1, 0], [0, 1]];
    }

    // U = A * V * S^-1
    const s1 = Math.sqrt(Math.max(0, s1sq));
    const s2 = Math.sqrt(Math.max(0, s2sq));

    let U;
    if (s1 > 1e-12) {
      const u00 = (a * V[0][0] + b * V[1][0]) / s1;
      const u10 = (c * V[0][0] + d * V[1][0]) / s1;
      if (s2 > 1e-12) {
        const u01 = (a * V[0][1] + b * V[1][1]) / s2;
        const u11 = (c * V[0][1] + d * V[1][1]) / s2;
        U = [[u00, u01], [u10, u11]];
      } else {
        U = [[u00, -u10], [u10, u00]];
      }
    } else {
      U = [[1, 0], [0, 1]];
    }

    return { U, V, S: [s1, s2] };
  }

  _meanShape(matrices) {
    const n = matrices.length;
    const p = matrices[0].length;
    const mean = new Array(p);
    for (let i = 0; i < p; i++) {
      let sx = 0, sy = 0;
      for (const m of matrices) { sx += m[i][0]; sy += m[i][1]; }
      mean[i] = [sx / n, sy / n];
    }
    return mean;
  }

  /**
   * Symmetric eigendecomposition using Jacobi iteration.
   * Returns eigenvalues (descending) and corresponding eigenvectors.
   */
  _symmetricEigen(A, maxIter = 200) {
    const n = A.length;

    // Deep copy
    const M = A.map(row => [...row]);
    // Eigenvector matrix (starts as identity)
    const V = Array.from({ length: n }, (_, i) =>
      Array.from({ length: n }, (_, j) => (i === j ? 1 : 0))
    );

    for (let iter = 0; iter < maxIter; iter++) {
      // Find largest off-diagonal element
      let maxVal = 0, mi = 0, mj = 1;
      for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
          if (Math.abs(M[i][j]) > maxVal) {
            maxVal = Math.abs(M[i][j]);
            mi = i; mj = j;
          }
        }
      }

      if (maxVal < 1e-14) break;

      // Compute rotation angle
      const theta = 0.5 * Math.atan2(2 * M[mi][mj], M[mi][mi] - M[mj][mj]);
      const c = Math.cos(theta), s = Math.sin(theta);

      // Apply Givens rotation to M and V
      for (let k = 0; k < n; k++) {
        const mik = M[mi][k], mjk = M[mj][k];
        M[mi][k] = c * mik + s * mjk;
        M[mj][k] = -s * mik + c * mjk;
      }
      for (let k = 0; k < n; k++) {
        const mki = M[k][mi], mkj = M[k][mj];
        M[k][mi] = c * mki + s * mkj;
        M[k][mj] = -s * mki + c * mkj;
      }
      for (let k = 0; k < n; k++) {
        const vki = V[k][mi], vkj = V[k][mj];
        V[k][mi] = c * vki + s * vkj;
        V[k][mj] = -s * vki + c * vkj;
      }
    }

    // Extract eigenvalues and sort descending
    const eigenvalues = M.map((row, i) => row[i]);
    const indices = eigenvalues.map((_, i) => i).sort((a, b) => eigenvalues[b] - eigenvalues[a]);

    const sortedEigenvalues = indices.map(i => eigenvalues[i]);
    const sortedEigenvectors = V.map(row => indices.map(i => row[i]));

    return { eigenvalues: sortedEigenvalues, eigenvectors: sortedEigenvectors };
  }
}

/**
 * Generate Bézier control points from a polyline for smooth curve export.
 * Reduces a dense pixel curve to n control points using uniform sampling + smoothing.
 */
export function fitControlPoints(pixelPoints, nControlPoints = 5) {
  if (pixelPoints.length <= nControlPoints) return pixelPoints.map(p => [...p]);

  const result = [];
  for (let i = 0; i < nControlPoints; i++) {
    const t = i / (nControlPoints - 1);
    const idx = t * (pixelPoints.length - 1);
    const lo = Math.floor(idx);
    const hi = Math.min(lo + 1, pixelPoints.length - 1);
    const frac = idx - lo;
    result.push([
      pixelPoints[lo][0] * (1 - frac) + pixelPoints[hi][0] * frac,
      pixelPoints[lo][1] * (1 - frac) + pixelPoints[hi][1] * frac
    ]);
  }
  return result;
}
