/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Smart Sheet Engine
 *   1. seBuildTable         : locates the true header block (multi-row / merged
 *                             cells), labels every column, builds clean rows.
 *   2. seAnalyzeTable       : (a) classifies the file as DRAINAGE / SEWERAGE /
 *                             HYBRID / UNKNOWN from several independent signals,
 *                             (b) maps each engineering column by header text
 *                             AND by what the cells actually contain,
 *                             (c) works out how split ID columns combine
 *                             (e.g. "MH" + "1" -> MH-01, "EXTG" + "MH 1" -> EXTG MH-01,
 *                             "STP", "OUTFALL", "CROSSING 1", etc.).
 * ============================================================================ */

const SE_NUM_RE = /^-?\d+(\.\d+)?$/;
const SE_MARKER_RE = /^\(\s*\d+\s*\)$/;
const SE_UNIT_RE = /^\(?\s*(m|mm|ha|m2|m\u00b2|m3\/min|m3\/s|m\/s|m\/sec|m\/km|lps|l\/s|yr|yrs|min|mins|mm\/hr|%|nos?\.?)\s*\)?$/i;

// Matches standard and special infrastructure IDs: MH1, EXTG MH1, EXTG, STP, SUMP 1, OUTFALL, CROSSING 1, FMH1, PS1, etc.
const SE_MH_FULL_RE = /^(?:(?:EXTG\.?|EXISTING|EXT|PROP\.?|PROPOSED|NEW|TO|FROM)\s*[-_.]?\s*)?(?:MH|FMH|SMH|DMH|WMH|STP|PS|SPS|SUMP|OUTFALL|OUTLET|CROSSING|CULVERT|IC)\s*[-_.]?\s*\d*[A-Za-z]?(?:\s*\((?:EXTG\.?|EXISTING|STP|PROP\.?|NEW)\))?$/i;

// Detects special tags anywhere in a row (EXTG, EXISTING, STP, OUTFALL, SUMP, CROSSING, PUMP STATION, FMH, TIE-IN)
const SE_SPECIAL_TAG_EXTRACT_RE = /\b(EXTG\.?\s*MH\s*[-_.]?\s*\d+[A-Za-z]?|EXISTING\s*MH\s*[-_.]?\s*\d+[A-Za-z]?|EXTG\.?\s*MH|EXISTING\s*MH|PROPOSED\s*STP|EXTG\.?\s*STP|STP\s*[-_.]?\s*\d*[A-Za-z]?|EXTG\.?|EXISTING|OUTFALL\s*[-_.]?\s*\d*[A-Za-z]?|OUTLET\s*[-_.]?\s*\d*[A-Za-z]?|PUMP\s*STATION|SPS\s*[-_.]?\s*\d*|FMH\s*[-_.]?\s*\d*[A-Za-z]?|SUMP\s*[-_.]?\s*\d*[A-Za-z]?|CROSSING\s*[-_.]?\s*\d*[A-Za-z]?|TIE[\s-]*IN)\b/i;

const seText = (v) => String(v === null || v === undefined ? '' : v).replace(/\s+/g, ' ').trim();

const seColLetter = (idx) => {
  let n = idx + 1;
  let s = '';
  while (n > 0) {
    const m = (n - 1) % 26;
    s = String.fromCharCode(65 + m) + s;
    n = Math.floor((n - 1) / 26);
  }
  return s;
};

// Joins a split ID ("MH" + "1", "EXTG" + "1", "EXTG. MH" + "1", "STP" + "", "A" + "1", "1" + "A") into one clean CAD ID.
const composeId = (mainRaw, subRaw) => {
  const main = String(mainRaw === undefined || mainRaw === null ? '' : mainRaw).replace(/\s+/g, ' ').trim().replace(/\.0+$/, '');
  const sub = String(subRaw === undefined || subRaw === null ? '' : subRaw).replace(/\s+/g, ' ').trim().replace(/\.0+$/, '');
  if (!sub || sub === '-') return main === '-' ? '' : main;
  if (!main || main === '-') return sub;

  const uMain = main.toUpperCase();
  const uSub = sub.toUpperCase();

  if (uMain === uSub) return main;
  if (uSub.startsWith(uMain) && /^[\s\-_.]/.test(uSub.slice(uMain.length))) return sub;

  // "MH" + "1" -> "MH-1" | "MH" + "EXTG 1" -> "EXTG MH-1"
  if (/^MH$/i.test(main)) {
    if (/^(EXTG|EXISTING|STP)/i.test(sub)) return `${sub} MH`.replace(/\s+/g, ' ').trim();
    return `MH-${sub}`;
  }

  // "EXTG" / "EXTG. MH" / "EXISTING MH" / "STP" / "PROP" + "1" or "MH 1"
  if (/^(EXTG\.?|EXISTING|EXT|EXTG\.?\s*MH|EXISTING\s*MH|STP|PROP\.?|PROPOSED|NEW|SUMP|OUTFALL|CROSSING|FMH|PS|SPS)$/i.test(main)) {
    if (/^\d+[A-Za-z]?$/.test(sub)) {
      if (/^(EXTG\.?|EXISTING|EXT)$/i.test(main)) return `EXTG MH-${sub}`;
      if (/MH$/i.test(main)) return `EXTG MH-${sub}`;
      return `${uMain.replace(/\.$/, '')} ${sub}`;
    }
    return `${uMain.replace(/\.$/, '')} ${sub}`;
  }

  // If sub is a special tag like "EXTG" or "STP" and main is "MH 1" or "1"
  if (/^(EXTG\.?|EXISTING|STP|PROP\.?|NEW|OUTFALL|SUMP)$/i.test(sub)) {
    return `${main} (${uSub.replace(/\.$/, '')})`;
  }

  // Short letter prefix + number/alnum (e.g. "A" + "1" -> "A1", "D" + "01" -> "D01")
  if (/^[A-Za-z]{1,4}$/.test(main) && /^[A-Za-z0-9]+$/.test(sub)) {
    return uSub.startsWith(uMain) ? sub : `${main}${sub}`;
  }

  // Number + sub-letter (e.g. "1" + "A" -> "1A")
  if (/^\d+$/.test(main) && /^[A-Za-z]{1,2}$/.test(sub)) {
    return `${main}${sub.toUpperCase()}`;
  }

  return `${main} ${sub}`;
};

// Scans a row for special tags (EXTG, STP, OUTFALL, SUMP, CROSSING, etc.) in ID or descriptor cells
// and merges them into the ID if present in that row.
const seEnrichIdWithRowTags = (baseId, row, columnMap, columns) => {
  let id = String(baseId || '').replace(/\s+/g, ' ').trim();

  const extraTags = [];
  const numericCols = new Set([
    columnMap.size, columnMap.length, columnMap.gradient,
    columnMap.gl, columnMap.il, columnMap.depth,
    columnMap.drainIlUp, columnMap.drainIlDown, columnMap.backdrop, columnMap.pipeType,
  ].filter(Boolean));

  columns.forEach((colKey, idx) => {
    if (numericCols.has(colKey)) return;
    const cellVal = seText(row[colKey]);
    if (!cellVal || cellVal === '-' || SE_NUM_RE.test(cellVal)) return;
    if (/^(PE\s+FROM|TOTAL|NOTE|VALUES\s+INDICATED|REMEMBER\s+TO|NEW\s+PIPE|OLD\s+PIPE|SEWER\s+IL\s+UNDER)/i.test(cellVal)) return;
    const isEarlyOrRemarkCol = idx <= 4 || /remark|status|location|building|type|receive|note|desc/i.test(colKey);
    if (!isEarlyOrRemarkCol) return;

    const mTag = cellVal.match(SE_SPECIAL_TAG_EXTRACT_RE);
    if (mTag) {
      let tag = mTag[1].toUpperCase().replace(/\s+/g, ' ').replace(/\.$/, '').trim();
      if (tag === 'EXISTING') tag = 'EXTG';
      if (tag === 'EXISTING MH' || tag === 'EXTG. MH') tag = 'EXTG MH';
      if (!extraTags.includes(tag)) extraTags.push(tag);
    }
  });

  if (extraTags.length === 0) return id;

  if (!id || id === '?') {
    return extraTags.join(' ');
  }

  const uId = id.toUpperCase();
  extraTags.forEach(tag => {
    const coreWord = tag.split(/[\s-]/)[0];
    if (!uId.includes(coreWord)) {
      if (tag === 'EXTG' || tag === 'EXTG MH') {
        id = `EXTG ${id}`;
      } else {
        id = `${id} (${tag})`;
      }
    }
  });

  return id.replace(/\s+/g, ' ').trim();
};

const SE_HEADER_KEY_RES = [
  /manhole\s*no|mh\s*no|drain\s*name|drain\s*id|pipe\s*id|sewer\s*id|^no\.?$|^id$/,
  /\bsize\b|\bdia(meter)?\b|u-?drain/,
  /\blength\b|\blen\b/,
  /\bgrad(e|ient)?\b|\bslope\b/,
  /invert|\bil\b/,
  /ground\s*level|\bgl\b/,
  /\bdepth\b/,
  /back\s*-?\s*drop/,
  /pipe\s*type|material/,
  /capacity/,
  /velocity/,
  /type\s*of\s*(drain|building)/,
];

const seHeaderRowScore = (cells) => {
  let hit = 0;
  SE_HEADER_KEY_RES.forEach(re => {
    if (cells.some(c => re.test(String(c).toLowerCase()))) hit++;
  });
  return hit;
};

// True when a row looks like real data rather than a header / note row.
const seIsDataLikeRow = (cells) => {
  if (cells.some(c => SE_MH_FULL_RE.test(c))) return true;
  const nums = cells.filter(c => SE_NUM_RE.test(c));
  if (nums.length === 0) return false;
  const vals = nums.map(Number);
  const isCounterRow = vals.length >= 2 && vals.every((v, i) => (i === 0 ? v === 1 : v === vals[i - 1] + 1));
  return !isCounterRow;
};

function seBuildTable({ aoa, merges = [], rowOffset = 0, colOffset = 0, sheetName = '', fileName = '' }) {
  const rowsText = aoa.map(r => (r || []).map(seText));
  if (rowsText.length === 0) return null;

  const scanLimit = Math.min(rowsText.length, 45);
  let leafRow = -1;
  let bestScore = 0;
  for (let i = 0; i < scanLimit; i++) {
    const sc = seHeaderRowScore(rowsText[i].filter(Boolean));
    if (sc > bestScore) {
      bestScore = sc;
      leafRow = i;
    }
  }
  if (leafRow < 0 || bestScore < 2) {
    leafRow = rowsText.findIndex(r => r.filter(Boolean).length >= 3);
    if (leafRow < 0) return null;
  }

  let lastHdr = leafRow;
  for (let r = leafRow + 1; r < Math.min(rowsText.length, leafRow + 10); r++) {
    const cells = rowsText[r].filter(Boolean);
    if (cells.length === 0) continue;
    if (seIsDataLikeRow(cells)) break;
    if (cells.length === 1 && cells[0].length > 12) break;
    lastHdr = r;
  }

  const nCols = rowsText.reduce((mx, r) => Math.max(mx, r.length), 0);
  const H = [];
  for (let r = leafRow; r <= lastHdr; r++) {
    H.push(Array.from({ length: nCols }, (_, c) => rowsText[r][c] || ''));
  }

  merges.forEach(m => {
    const origin = rowsText[m.s.r] ? rowsText[m.s.r][m.s.c] : '';
    if (!origin) return;
    if (m.e.c - m.s.c + 1 > 14) return;
    for (let r = Math.max(m.s.r, leafRow); r <= Math.min(m.e.r, lastHdr); r++) {
      for (let c = m.s.c; c <= m.e.c; c++) {
        if (c < nCols && !H[r - leafRow][c]) H[r - leafRow][c] = origin;
      }
    }
  });

  const usable = (t) => t && !SE_MARKER_RE.test(t) && !SE_UNIT_RE.test(t) && !/^1\s*:\s*x$/i.test(t) && !(t.length === 1 && /[A-Za-z0-9]/.test(t));
  const labels = [];
  for (let c = 0; c < nCols; c++) {
    const parts = [];
    H.forEach(hr => {
      const t = hr[c];
      if (usable(t) && !parts.includes(t)) parts.push(t);
    });
    if (parts.length === 0) {
      for (let r = leafRow - 1; r >= Math.max(0, leafRow - 3); r--) {
        const t = rowsText[r][c];
        if (usable(t) && t.length <= 40 && !SE_NUM_RE.test(t)) {
          parts.push(t);
          break;
        }
      }
    }
    labels.push(parts.join(' '));
  }

  const dataRows = [];
  for (let r = lastHdr + 1; r < rowsText.length; r++) {
    if (rowsText[r].some(Boolean)) dataRows.push(rowsText[r]);
  }

  const keepCols = [];
  for (let c = 0; c < nCols; c++) {
    if (labels[c] || dataRows.some(row => row[c])) keepCols.push(c);
  }

  const usedKeys = new Set();
  const headers = [];
  const headerInfo = [];
  keepCols.forEach(c => {
    const letter = seColLetter(c + colOffset);
    let key = labels[c] || `Col ${letter}`;
    if (usedKeys.has(key)) key = `${key} [${letter}]`;
    usedKeys.add(key);
    headers.push(key);
    headerInfo.push({ key, label: labels[c] || '', letter, index: c });
  });

  const rows = dataRows.map(row => {
    const obj = {};
    keepCols.forEach((c, i) => {
      obj[headers[i]] = row[c] || '';
    });
    return obj;
  });

  const metaText = [
    fileName,
    sheetName,
    ...rowsText.slice(0, leafRow).map(r => r.filter(Boolean).join(' ')),
  ].join(' ');

  return { headers, rows, headerInfo, metaText, headerRow: leafRow + rowOffset + 1 };
}

function seBuildTableFromSheet(ws, { sheetName = '', fileName = '' } = {}) {
  if (!ws || !ws['!ref']) return null;
  const range = XLSX.utils.decode_range(ws['!ref']);
  const aoa = XLSX.utils.sheet_to_json(ws, { header: 1, defval: '', raw: false, blankrows: true });
  const merges = (ws['!merges'] || [])
    .map(m => ({
      s: { r: m.s.r - range.s.r, c: m.s.c - range.s.c },
      e: { r: m.e.r - range.s.r, c: m.e.c - range.s.c },
    }))
    .filter(m => m.s.r >= 0 && m.s.c >= 0);
  return seBuildTable({ aoa, merges, rowOffset: range.s.r, colOffset: range.s.c, sheetName, fileName });
}

// -- Column profiling (what do the cells actually contain?) --
function seProfile(rows, key) {
  const vals = [];
  for (let i = 0; i < Math.min(rows.length, 400); i++) {
    const v = seText(rows[i][key]);
    if (v !== '' && v !== '-') vals.push(v);
  }
  const n = vals.length;
  const nums = vals
    .map(v => (SE_NUM_RE.test(v.replace(/,/g, '')) ? parseFloat(v.replace(/,/g, '')) : NaN))
    .filter(x => !isNaN(x));
  const sorted = [...nums].sort((a, b) => a - b);
  const uniq = new Set(vals).size;
  return {
    n,
    vals,
    numRatio: n ? nums.length / n : 0,
    median: sorted.length ? sorted[Math.floor(sorted.length / 2)] : NaN,
    unique: uniq,
    uniqueRatio: n ? uniq / n : 0,
    ratio: (re) => (n ? vals.filter(v => re.test(v)).length / n : 0),
  };
}

const SE_FIELD_SPECS = {
  size: {
    pos: [[/u-?drain\s*(width|size)|pipe\s*size|drain\s*size|pipe\s*dia|\bdia(meter)?\b|\bsize\b|\u00f8|\u2300/, 10]],
    neg: /capacity|velocity|roughness|\bks\b|time|area|coeff|rainfall|intensity|flow|height|peak|type/,
  },
  length: {
    pos: [[/\blength\b|\blen\b/, 10]],
    neg: /overland|time|roughness|\bks\b|capacity|velocity|travel|flow\s*len/,
  },
  gradient: {
    pos: [[/\bgrad(e|ient)?\b|\bslope\b|1\s*:\s*x|\bfall\b/, 10]],
    neg: /hydraulic|peak|velocity|capacity|\bflow\b|rainfall|intensity|check/,
  },
  gl: {
    pos: [[/ground\s*level|\bgl\b|cover\s*level|surface\s*level|\brl\b/, 10]],
    adj: [[/upstream|u\/s/, 3], [/downstream|d\/s/, -3]],
  },
  il: {
    pos: [[/invert\s*level|\binvert\b|\bil\b/, 10]],
    neg: /upstream|downstream|u\/s|d\/s|check|soffit/,
  },
  drainIlUp: {
    pos: [[/(invert|\bil\b).*(upstream|u\/s)|(upstream|u\/s).*(invert|\bil\b)/, 12]],
  },
  drainIlDown: {
    pos: [[/(invert|\bil\b).*(downstream|d\/s)|(downstream|d\/s).*(invert|\bil\b)/, 12]],
  },
  depth: {
    pos: [[/depth\s*of\s*manhole|\bmh\s*depth|\bdepth\b/, 10]],
    adj: [[/manhole|\bmh\b/, 3], [/upstream/, 1], [/downstream/, -2]],
  },
  pipeType: {
    pos: [[/pipe\s*type|type\s*of\s*pipe|pipe\s*material|\bmaterial\b|\bmat\b/, 10]],
    neg: /type\s*of\s*(drain|building)/,
  },
  backdrop: {
    pos: [[/back\s*-?\s*drop|\bb\/d\b/, 10], [/\bdrop\b/, 5]],
    neg: /capacity|velocity/,
  },
};

const SE_MATERIAL_RE = /^(VCP|RCP|HDPE|UPVC|PVC|DIP|DI|GRP|PE|CI|ST|STEEL|PP|ACP|CONCRETE)\b/i;

function seHeaderScore(field, label) {
  const spec = SE_FIELD_SPECS[field];
  const t = String(label || '').toLowerCase();
  if (!t) return 0;
  if (spec.neg && spec.neg.test(t)) return 0;
  let best = 0;
  spec.pos.forEach(([re, w]) => {
    if (re.test(t)) best = Math.max(best, w);
  });
  if (best === 0) return 0;
  (spec.adj || []).forEach(([re, w]) => {
    if (re.test(t)) best += w;
  });
  return best;
}

function seContentScore(field, prof, nextProf) {
  if (!prof || prof.n === 0) return 0;
  const isLevel = ['gl', 'il', 'drainIlUp', 'drainIlDown', 'depth', 'backdrop'].includes(field);
  if (isLevel) return prof.numRatio >= 0.6 ? 3 : -8;
  if (field === 'size') {
    if (prof.numRatio < 0.6) return -8;
    return prof.median >= 40 && prof.median <= 3000 ? 4 : -3;
  }
  if (field === 'length') {
    if (prof.numRatio < 0.6) return -8;
    return prof.median >= 0.2 && prof.median <= 500 ? 3 : -2;
  }
  if (field === 'gradient') {
    if (prof.numRatio >= 0.6) {
      if (prof.median >= 20) return 3;
      if (prof.ratio(/^1$/) >= 0.6 && nextProf && nextProf.ratio(/^:$/) >= 0.6) return 3;
      return -3;
    }
    return prof.ratio(/^1\s*[:\/]\s*\d+/) >= 0.6 ? 3 : -6;
  }
  if (field === 'pipeType') {
    if (prof.numRatio >= 0.6) return -8;
    return prof.ratio(SE_MATERIAL_RE) >= 0.5 ? 6 : 0;
  }
  return 0;
}

const SE_ID_HDR_RE = /manhole|mh\s*no|drain\s*name|drain\s*id|pipe\s*id|sewer\s*id|\bid\b|\bno\.?(\s|$)|\bname\b|node|chamber|structure/;

// Works out which column(s) form the ID, and whether two columns must be joined.
function seDetectIdColumns(headers, headerInfo, rows, profile, excluded) {
  const cands = [];
  headers.forEach((h, i) => {
    if (i < 10 && !excluded.has(h)) cands.push(i);
  });

  const kinds = {};
  cands.forEach(i => {
    const p = profile(headers[i]);
    kinds[i] = {
      p,
      label: String((headerInfo[i] && headerInfo[i].label) || headers[i]).toLowerCase(),
      mhFull: p.ratio(SE_MH_FULL_RE),
      mhPrefix: p.ratio(/^(MH|EXTG\.?|EXISTING|EXTG\.?\s*MH|STP|FMH|SMH|SUMP|OUTFALL|CROSSING)$/i),
      prefix: p.unique <= 12 ? p.ratio(/^[A-Za-z.\s]{1,10}$/) : 0,
      int: p.ratio(/^\d{1,4}(\.0+)?$/),
      alnum: p.ratio(/^([A-Za-z.\s]{1,10}\s*[-_]?\s*\d+[A-Za-z]?|\d+[A-Za-z]{1,2}|STP|EXTG.*|OUTFALL.*|CROSSING.*|SUMP.*)$/i),
      letter1: p.ratio(/^[A-Za-z]$/),
    };
    kinds[i].hdr = SE_ID_HDR_RE.test(kinds[i].label);
  });

  const options = [];

  cands.forEach(i => {
    const k = kinds[i];
    if (k.p.n < 2) return;
    if (k.mhFull >= 0.4) options.push({ score: 22 + (k.hdr ? 10 : 0), main: i, sub: -1 });
    else if (k.alnum >= 0.4) options.push({ score: 14 + (k.hdr ? 10 : 0), main: i, sub: -1 });
    else if (k.int >= 0.5 && k.hdr) options.push({ score: 12, main: i, sub: -1 });
    else if (k.hdr && k.p.uniqueRatio >= 0.7 && k.numRatio !== 1) options.push({ score: 8, main: i, sub: -1 });
  });

  cands.forEach(i => {
    const a = kinds[i];
    if (a.p.n < 2) return;
    [1, 2].forEach(step => {
      const j = i + step;
      if (!kinds[j] || kinds[j].p.n < 2) return;
      const b = kinds[j];
      let base = 0;
      if (a.prefix >= 0.4 && b.int >= 0.4) base = 26;
      else if (a.prefix >= 0.4 && b.alnum >= 0.4) base = 22;
      else if (a.int >= 0.4 && b.letter1 >= 0.4) base = 22;
      if (!base) return;

      let both = 0;
      for (let r = 0; r < Math.min(rows.length, 400); r++) {
        if (seText(rows[r][headers[i]]) && seText(rows[r][headers[j]])) both++;
      }
      if (both / Math.max(a.p.n, b.p.n) < 0.4) return;

      const hasHdr = a.hdr || b.hdr;
      const mhPrefix = a.mhPrefix >= 0.4;
      const subUnique = b.p.uniqueRatio >= 0.4;
      if (!hasHdr && !mhPrefix && !subUnique) return;

      options.push({
        score: base + (hasHdr ? 10 : 0) + (mhPrefix ? 8 : 0) + b.p.uniqueRatio * 6 - (step - 1),
        main: i,
        sub: j,
      });
    });
  });

  if (options.length === 0) {
    const fallback = cands.find(i => kinds[i] && kinds[i].hdr);
    return fallback === undefined ? null : { main: fallback, sub: -1, score: 1 };
  }
  options.sort((x, y) => (y.score - x.score) || (x.main - y.main));
  return options[0];
}

const SE_SEWER_SIGNALS = [
  [/sewer(age)?|sewage/, 8, 'Sewer / Sewage wording'],
  [/\biwk\b/, 6, 'IWK'],
  [/manhole/, 5, 'Manhole headers'],
  [/peak\s*factor|cumulative\s*peak\s*flow/, 6, 'Peak Factor / Peak Flow'],
  [/population|\(\s*pe\s*\)|accumulative\s*pe|\bpe\b/, 5, 'Population (PE)'],
  [/back\s*-?\s*drop/, 4, 'Backdrop'],
  [/vcp|vitrified/, 2, 'VCP pipe'],
  [/reticulation/, 3, 'Reticulation'],
];

const SE_DRAIN_SIGNALS = [
  [/storm\s*(water\s*)?drain(age)?|stormwater/, 8, 'Storm Drainage wording'],
  [/\bmasma\b|\bmsma\b/, 6, 'MSMA / MASMA'],
  [/catchment/, 5, 'Catchment'],
  [/runoff/, 5, 'Runoff coefficient'],
  [/time\s*of\s*concentration|overland/, 5, 'Time of concentration'],
  [/rainfall|intensity|return\s*period/, 4, 'Rainfall / ARI'],
  [/u-?drain/, 6, 'U-Drain'],
  [/drain\s*name|drain\s*id/, 6, 'Drain Name'],
  [/type\s*of\s*drain|culvert/, 5, 'Type of Drain'],
  [/crossing/, 2, 'Crossing'],
];

function seAnalyzeTable({ headers, rows, headerInfo = null, metaText = '' }) {
  const info = headerInfo && headerInfo.length === headers.length
    ? headerInfo
    : headers.map((h, i) => ({ key: h, label: h, letter: seColLetter(i), index: i }));

  const profCache = {};
  const profile = (key) => {
    if (!profCache[key]) profCache[key] = seProfile(rows, key);
    return profCache[key];
  };

  const mapping = {
    drainId: '', drainIdSub: '', size: '', length: '', gradient: '',
    drainIlUp: '', drainIlDown: '', sewerId: '', sewerIdSub: '',
    gl: '', il: '', depth: '', pipeType: '', backdrop: '',
  };

  const cands = [];
  Object.keys(SE_FIELD_SPECS).forEach(field => {
    headers.forEach((key, i) => {
      const hs = seHeaderScore(field, info[i].label || key);
      if (hs <= 0) return;
      const next = headers[i + 1] ? profile(headers[i + 1]) : null;
      const total = hs + seContentScore(field, profile(key), next);
      if (total >= 8) cands.push({ field, key, idx: i, score: total });
    });
  });
  cands.sort((a, b) => (b.score - a.score) || (a.idx - b.idx));
  const usedCols = new Set();
  cands.forEach(c => {
    if (mapping[c.field] || usedCols.has(c.key)) return;
    mapping[c.field] = c.key;
    usedCols.add(c.key);
  });

  if (mapping.gradient) {
    const gi = headers.indexOf(mapping.gradient);
    const p0 = profile(headers[gi]);
    const p1 = headers[gi + 1] ? profile(headers[gi + 1]) : null;
    const p2 = headers[gi + 2] ? profile(headers[gi + 2]) : null;
    if (p0.ratio(/^1$/) >= 0.6 && p1 && p1.ratio(/^:$/) >= 0.6 && p2 && p2.numRatio >= 0.6) {
      mapping.gradient = headers[gi + 2];
    }
  }

  const reasons = [];
  const notes = [];
  if (mapping.gl && mapping.il) {
    let ok = 0;
    let bad = 0;
    rows.slice(0, 400).forEach(r => {
      const g = parseFloat(seText(r[mapping.gl]));
      const l = parseFloat(seText(r[mapping.il]));
      if (!isNaN(g) && !isNaN(l)) (g >= l ? ok++ : bad++);
    });
    if (bad > ok && bad >= 2) {
      const tmp = mapping.gl;
      mapping.gl = mapping.il;
      mapping.il = tmp;
      notes.push('GL and IL columns were swapped after checking the levels (GL must be above IL)');
    }
  }

  const idPick = seDetectIdColumns(headers, info, rows, profile, usedCols);
  const idInfo = { kind: 'none', mainKey: '', subKey: '', samples: [] };
  if (idPick) {
    mapping.drainId = mapping.sewerId = headers[idPick.main];
    idInfo.mainKey = headers[idPick.main];
    idInfo.kind = 'single';
    if (idPick.sub >= 0) {
      mapping.drainIdSub = mapping.sewerIdSub = headers[idPick.sub];
      idInfo.subKey = headers[idPick.sub];
      idInfo.kind = 'pair';
    }
  }

  const composedIds = [];
  rows.slice(0, 400).forEach(r => {
    const rawCombined = composeId(seText(r[mapping.sewerId]), mapping.sewerIdSub ? seText(r[mapping.sewerIdSub]) : '');
    const enriched = seEnrichIdWithRowTags(rawCombined, r, mapping, headers);
    if (enriched && !/^PE\s+FROM/i.test(enriched)) composedIds.push(enriched);
  });
  idInfo.samples = composedIds.filter((v, i, a) => a.indexOf(v) === i).slice(0, 5);

  let sew = 0;
  let drn = 0;
  const sewHits = [];
  const drnHits = [];
  const corpus = `${metaText} ${info.map(x => x.label || x.key).join(' ')}`.toLowerCase();
  SE_SEWER_SIGNALS.forEach(([re, w, name]) => { if (re.test(corpus)) { sew += w; sewHits.push(name); } });
  SE_DRAIN_SIGNALS.forEach(([re, w, name]) => { if (re.test(corpus)) { drn += w; drnHits.push(name); } });

  if (mapping.backdrop) { sew += 2; sewHits.push('Backdrop column'); }
  if (mapping.drainIlUp && mapping.drainIlDown) { drn += 4; drnHits.push('Invert Level Up/Downstream pair'); }

  const hasSewerCols = Boolean(mapping.gl || mapping.il || mapping.depth);
  const hasDrainCols = Boolean(mapping.size || mapping.length || mapping.gradient);

  const mhRatio = composedIds.length ? composedIds.filter(v => /(^|\b)(MH|STP|FMH)/i.test(v)).length / composedIds.length : 0;
  const drainStyleRatio = composedIds.length
    ? composedIds.filter(v => !/(^|\b)(MH|STP|FMH)/i.test(v) && /^([A-Za-z]{1,3}[-\s]?\d+[A-Za-z]?|\d+[A-Za-z]{1,2}|CROSSING.*|OUTFALL.*|SUMP.*)$/i.test(v)).length / composedIds.length
    : 0;
  if (mhRatio >= 0.3) { sew += 8; sewHits.push(`MH/STP IDs in ${Math.round(mhRatio * 100)}% of rows`); }
  if (drainStyleRatio >= 0.5 && (drn > 0 || (mapping.size && mapping.length))) {
    drn += 5;
    drnHits.push(`Drain-style IDs in ${Math.round(drainStyleRatio * 100)}% of rows`);
  }

  const diff = sew - drn;

  let mode;
  if (!hasSewerCols && !hasDrainCols) {
    mode = 'UNKNOWN';
  } else if (mhRatio >= 0.1 && mhRatio <= 0.9 && drainStyleRatio >= 0.1 && hasSewerCols && hasDrainCols) {
    mode = 'HYBRID';
  } else if (Math.max(sew, drn) < 6) {
    mode = 'UNKNOWN';
  } else if (Math.abs(diff) < 4) {
    mode = 'UNKNOWN';
  } else {
    mode = diff > 0 ? 'SEWERAGE' : 'DRAINAGE';
  }

  let confidence = 'NONE';
  if (mode === 'HYBRID') confidence = 'MEDIUM';
  else if (mode !== 'UNKNOWN') confidence = Math.abs(diff) >= 12 ? 'HIGH' : (Math.abs(diff) >= 7 ? 'MEDIUM' : 'LOW');

  if (mode === 'SEWERAGE' || mode === 'HYBRID') reasons.push(`Sewerage: ${sewHits.join(', ') || 'none'}`);
  if (mode === 'DRAINAGE' || mode === 'HYBRID') reasons.push(`Drainage: ${drnHits.join(', ') || 'none'}`);
  if (mode === 'UNKNOWN') reasons.push(`Not enough evidence (sewerage ${sew} vs drainage ${drn})`);

  return {
    mapping,
    mode,
    confidence,
    reasons,
    notes,
    scores: { sew, drn },
    idInfo,
    evidence: { sew: sewHits, drn: drnHits },
  };
}

window.SE_NUM_RE = SE_NUM_RE;
window.SE_MH_FULL_RE = SE_MH_FULL_RE;
window.SE_SPECIAL_TAG_EXTRACT_RE = SE_SPECIAL_TAG_EXTRACT_RE;
window.seText = seText;
window.seColLetter = seColLetter;
window.composeId = composeId;
window.seEnrichIdWithRowTags = seEnrichIdWithRowTags;
window.seHeaderRowScore = seHeaderRowScore;
window.seBuildTable = seBuildTable;
window.seBuildTableFromSheet = seBuildTableFromSheet;
window.seAnalyzeTable = seAnalyzeTable;
