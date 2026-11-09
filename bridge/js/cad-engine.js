/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — CAD & Longitudinal Section Engine
 *   1. CSVUPDATE Engine  : Plan View MTEXT formatting, smart pipe-length
 *                          multi-line wrapping, EXTG/STP/CROSSING ID normalizer,
 *                          and Lowest Invert Level (IL) deduplication.
 *   2. SEWSECTION Engine : Longitudinal Section profile chain builder,
 *                          200m distance splitter, and Auto +10m Datum calculator.
 * ============================================================================ */

function buildDrainageCadString(id, size, length, gradient, settings) {
  const widthVal = parseFloat(settings.textWidthFactor) || 0.5;
  const widthPrefix = `{\\W${widthVal};`;
  const sizeDisplay = size || '600';
  const gradDisplay = gradient || '1000';

  const lenNum = parseFloat(length);
  const hasValidLen = !isNaN(lenNum) && lenNum >= 0;
  const lenDisplay = hasValidLen
    ? (Number.isInteger(lenNum) ? lenNum.toString() : lenNum.toFixed(1))
    : '0';

  const isShort = hasValidLen && lenNum <= settings.shortPipeThreshold;
  const isMedium = hasValidLen && lenNum > settings.shortPipeThreshold && lenNum <= settings.mediumPipeThreshold;

  let cadString = '';
  let conditionBadge = '';

  if (settings.smartLengthFormatting && isShort) {
    cadString = `${widthPrefix}${id};\\P${sizeDisplay}%%c\\P${lenDisplay}m\\P1:${gradDisplay}}`;
    conditionBadge = `Short Pipe (<= ${settings.shortPipeThreshold}m, 4-Line)`;
  } else if (settings.smartLengthFormatting && isMedium) {
    cadString = `${widthPrefix}${id}; ${sizeDisplay}%%c\\P${lenDisplay}m 1:${gradDisplay}}`;
    conditionBadge = `Medium Pipe (<= ${settings.mediumPipeThreshold}m, 2-Line)`;
  } else {
    cadString = `${widthPrefix}${id}; ${sizeDisplay}%%c ${lenDisplay}m 1:${gradDisplay}}`;
    conditionBadge = settings.smartLengthFormatting
      ? `Long Pipe (> ${settings.mediumPipeThreshold}m, 1-Line)`
      : 'Smart Formatting OFF (1-Line)';
  }

  return { cadString, conditionBadge, sizeDisplay, lenDisplay, gradDisplay };
}

function buildSewerageCadString(id, gl, il, depth, settings) {
  const widthVal = parseFloat(settings.textWidthFactor) || 0.5;
  const widthPrefix = `{\\W${widthVal};`;
  const numGL = parseFloat(gl);
  const numIL = parseFloat(il);
  let numDepth = parseFloat(depth);

  if (!isNaN(numGL) && !isNaN(numIL)) {
    numDepth = numGL - numIL;
  }

  const glDisplay = !isNaN(numGL) ? numGL.toFixed(2) : (gl || '00.00');
  const ilDisplay = !isNaN(numIL) ? numIL.toFixed(2) : (il || '00.00');
  const depthDisplay = !isNaN(numDepth) ? numDepth.toFixed(2) : (depth || '00.00');

  const cadString = `${widthPrefix}${id};\\PGL: ${glDisplay}m\\PIL: ${ilDisplay}m\\PDEPTH: ${depthDisplay}m}`;
  return { cadString, glDisplay, ilDisplay, depthDisplay };
}

// Standardizes Sewer / Drain ID while preserving EXTG, STP, OUTFALL, SUMP, CROSSING, etc.
function normalizeCadId(rawId, isSewerRow) {
  const s = String(rawId || '').replace(/\s+/g, ' ').trim();
  if (!s) return '';

  if (isSewerRow) {
    // 1. Pure number: "1" -> "MH-01"
    const mBare = s.match(/^0*(\d+)(?:\.0+)?([A-Za-z]?)$/);
    if (mBare) return `MH-${mBare[1].padStart(2, '0')}${mBare[2].toUpperCase()}`;

    // 2. Standard MH with optional prefix/suffix: e.g. "MH 1", "EXTG MH 1", "MH 1 (EXTG)", "MH1 STP"
    const mPref = s.match(/^(?:(EXTG\.?|EXISTING|PROP\.?|NEW)\s*[-_.]?\s*)?MH\s*[-_.]?\s*0*(\d+)(?:\.0+)?([A-Za-z]?)(?:\s*\(?([A-Za-z0-9.\s-]+)\)?)?$/i);
    if (mPref) {
      const lead = mPref[1] ? `${mPref[1].toUpperCase().replace(/\.$/, '').replace('EXISTING', 'EXTG')} ` : '';
      const num = mPref[2].padStart(2, '0');
      const sub = (mPref[3] || '').toUpperCase();
      const tail = mPref[4] ? ` (${mPref[4].toUpperCase().trim()})` : '';
      return `${lead}MH-${num}${sub}${tail}`;
    }

    // 3. Special prefix + number without "MH" (e.g. "EXTG 1" -> "EXTG MH-01", "STP 1" -> "STP-01", "FMH 1" -> "FMH-01")
    const mSpecNum = s.match(/^(EXTG\.?|EXISTING|STP|FMH|SMH|PS|SPS|SUMP|OUTFALL)\s*[-_.]?\s*0*(\d+)(?:\.0+)?([A-Za-z]?)$/i);
    if (mSpecNum) {
      const tag = mSpecNum[1].toUpperCase().replace(/\.$/, '').replace('EXISTING', 'EXTG');
      const num = mSpecNum[2].padStart(2, '0');
      const sub = (mSpecNum[3] || '').toUpperCase();
      return tag === 'EXTG' ? `EXTG MH-${num}${sub}` : `${tag}-${num}${sub}`;
    }

    // 4. Standalone special structure (e.g. "STP", "EXTG MH", "OUTFALL", "PUMP STATION")
    return s.toUpperCase();
  } else {
    // Drainage ID normalization (preserves EXTG, CROSSING, OUTFALL, SUMP, etc.)
    const mDrn = s.match(/^([a-zA-Z]{1,3})\s*[-_]?\s*(\d+)$/);
    if (mDrn) {
      const prefix = mDrn[1].toUpperCase();
      const num = mDrn[2].padStart(2, '0');
      return `${prefix}${num}`;
    }
    return s.toUpperCase();
  }
}

function formatCadRow(row, { effectiveMode, columnMap, columns, settings }) {
  if (effectiveMode === 'UNKNOWN') return null;

  const sewerIdBase = composeId(row[columnMap.sewerId], columnMap.sewerIdSub ? row[columnMap.sewerIdSub] : '');
  const drainIdBase = composeId(row[columnMap.drainId], columnMap.drainIdSub ? row[columnMap.drainIdSub] : '');

  const sewerIdRaw = seEnrichIdWithRowTags(sewerIdBase, row, columnMap, columns);
  const drainIdRaw = seEnrichIdWithRowTags(drainIdBase, row, columnMap, columns);

  let glRaw = String(row[columnMap.gl] || '').trim();
  let ilRaw = String(row[columnMap.il] || '').trim();
  let depthRaw = String(row[columnMap.depth] || '').trim();
  let sizeRaw = String(row[columnMap.size] || '').trim();
  let lengthRaw = String(row[columnMap.length] || '').trim();
  let gradRaw = String(row[columnMap.gradient] || '').trim();

  if (gradRaw === '1' && columnMap.gradient) {
    const gIdx = columns.indexOf(columnMap.gradient);
    if (gIdx !== -1 && gIdx + 2 < columns.length) {
      const colonCell = String(row[columns[gIdx + 1]] || '').trim();
      const denomCell = String(row[columns[gIdx + 2]] || '').trim();
      if (colonCell === ':' && /^\d+(\.\d+)?$/.test(denomCell)) {
        gradRaw = denomCell;
      }
    }
  }

  let ilUpRaw = String(row[columnMap.drainIlUp] || '').trim();
  let ilDownRaw = String(row[columnMap.drainIlDown] || '').trim();

  const cleanNumber = (val) => {
    if (!val) return '';
    if (val.includes('#DIV/0!') || val.includes('#REF!') || val.includes('#VALUE!')) return '?';
    return val.replace(/[^0-9.-]/g, '');
  };

  sizeRaw = cleanNumber(sizeRaw);
  lengthRaw = cleanNumber(lengthRaw);
  glRaw = cleanNumber(glRaw);
  ilRaw = cleanNumber(ilRaw);
  depthRaw = cleanNumber(depthRaw);
  ilUpRaw = cleanNumber(ilUpRaw);
  ilDownRaw = cleanNumber(ilDownRaw);

  if (settings.cleanGradientPrefix) {
    gradRaw = gradRaw.replace(/^1\s*[:/]\s*/i, '');
  }
  gradRaw = cleanNumber(gradRaw);

  const rawIdCheck = (sewerIdRaw || drainIdRaw).toUpperCase();
  if (!rawIdCheck || rawIdCheck === 'D' || rawIdCheck === 'ID' || rawIdCheck === 'MANHOLE' || rawIdCheck.startsWith('PE FROM')) {
    return null;
  }

  const hasSewerData = Boolean((glRaw && glRaw !== '?') || (ilRaw && ilRaw !== '?') || (depthRaw && depthRaw !== '?'));
  const hasDrainData = Boolean((sizeRaw && sizeRaw !== '?') || (lengthRaw && lengthRaw !== '?') || (gradRaw && gradRaw !== '?'));

  if (!hasSewerData && !hasDrainData) return null;

  let isSewerRow = false;
  if (effectiveMode === 'SEWERAGE') {
    if (!hasSewerData) return null;
    isSewerRow = true;
  } else if (effectiveMode === 'DRAINAGE') {
    if (!hasDrainData && !hasSewerData) return null;
    isSewerRow = false;
  } else {
    isSewerRow = hasSewerData && (!hasDrainData || /(^|\b)(MH|STP|FMH)/i.test(sewerIdRaw));
  }

  const effectiveId = normalizeCadId(isSewerRow ? (sewerIdRaw || drainIdRaw) : (drainIdRaw || sewerIdRaw), isSewerRow);
  if (!effectiveId || effectiveId === '?') return null;

  const candidateILs = [ilRaw, ilDownRaw, ilUpRaw]
    .map(v => parseFloat(v))
    .filter(n => !isNaN(n));
  const effectiveIL = candidateILs.length > 0 ? Math.min(...candidateILs) : NaN;

  const hasSpecialTag = SE_SPECIAL_TAG_EXTRACT_RE.test(effectiveId);

  if (isSewerRow) {
    const sewBuilt = buildSewerageCadString(
      effectiveId,
      glRaw,
      !isNaN(effectiveIL) ? effectiveIL.toFixed(2) : ilRaw,
      depthRaw,
      settings
    );
    const visualLines = sewBuilt.cadString.replace(/^\{\\W[\d.]+;/, '').replace(/\}$/, '').replace(/%%c/g, 'Ø').split('\\P');
    return {
      id: effectiveId,
      category: 'SEWERAGE',
      cadString: sewBuilt.cadString,
      conditionBadge: hasSpecialTag ? 'Special Node (EXTG/STP Included)' : 'Manhole (4-Line Label)',
      visualLines,
      effectiveIL,
      drainDetails: { size: sizeRaw, length: lengthRaw, gradient: gradRaw, il: !isNaN(effectiveIL) ? effectiveIL.toFixed(2) : '' },
      sewerDetails: { gl: sewBuilt.glDisplay, il: sewBuilt.ilDisplay, depth: sewBuilt.depthDisplay },
      rawRow: row,
    };
  } else {
    const drnBuilt = buildDrainageCadString(effectiveId, sizeRaw, lengthRaw, gradRaw, settings);
    const visualLines = drnBuilt.cadString.replace(/^\{\\W[\d.]+;/, '').replace(/\}$/, '').replace(/%%c/g, 'Ø').split('\\P');
    return {
      id: effectiveId,
      category: 'DRAINAGE',
      cadString: drnBuilt.cadString,
      conditionBadge: hasSpecialTag ? `${drnBuilt.conditionBadge} • EXTG/Special` : drnBuilt.conditionBadge,
      visualLines,
      effectiveIL,
      drainDetails: {
        size: drnBuilt.sizeDisplay,
        length: drnBuilt.lenDisplay,
        gradient: drnBuilt.gradDisplay,
        il: !isNaN(effectiveIL) ? effectiveIL.toFixed(2) : '',
      },
      sewerDetails: { gl: glRaw, il: ilRaw, depth: depthRaw },
      rawRow: row,
    };
  }
}

function processAndDeduplicateRows(rawRows, { effectiveMode, columnMap, columns, settings }) {
  const rawFormatted = rawRows
    .map(r => formatCadRow(r, { effectiveMode, columnMap, columns, settings }))
    .filter(Boolean);
  const uniqueMap = new Map();

  rawFormatted.forEach(item => {
    const cleanId = item.id.toUpperCase();

    if (!uniqueMap.has(cleanId)) {
      uniqueMap.set(cleanId, { ...item, duplicateCount: 1 });
    } else {
      const existing = uniqueMap.get(cleanId);
      const dupCount = (existing.duplicateCount || 1) + 1;
      const existingIL = existing.effectiveIL;
      const newIL = item.effectiveIL;

      const newIsLower = (!isNaN(newIL) && (isNaN(existingIL) || newIL < existingIL));

      if (item.category === 'SEWERAGE') {
        if (newIsLower) {
          const gl = parseFloat(item.sewerDetails.gl) || parseFloat(existing.sewerDetails.gl) || 0;
          const sewBuilt = buildSewerageCadString(item.id, gl.toFixed(2), newIL.toFixed(2), '', settings);
          const updatedItem = {
            ...item,
            duplicateCount: dupCount,
            effectiveIL: newIL,
            cadString: sewBuilt.cadString,
            sewerDetails: { gl: sewBuilt.glDisplay, il: sewBuilt.ilDisplay, depth: sewBuilt.depthDisplay },
            conditionBadge: `Merged ${dupCount} Rows (Lowest IL: ${newIL.toFixed(2)}m)`,
            visualLines: sewBuilt.cadString.replace(/^\{\\W[\d.]+;/, '').replace(/\}$/, '').split('\\P'),
          };
          uniqueMap.set(cleanId, updatedItem);
        } else {
          const bestILText = !isNaN(existingIL) ? `${existingIL.toFixed(2)}m` : 'Kept';
          existing.duplicateCount = dupCount;
          existing.conditionBadge = `Merged ${dupCount} Rows (Lowest IL: ${bestILText})`;
        }
      } else {
        if (newIsLower) {
          const updatedItem = {
            ...item,
            duplicateCount: dupCount,
            effectiveIL: newIL,
            conditionBadge: `${item.conditionBadge} • Lowest IL (${newIL.toFixed(2)}m of ${dupCount})`,
          };
          uniqueMap.set(cleanId, updatedItem);
        } else {
          const bestILText = !isNaN(existingIL) ? `${existingIL.toFixed(2)}m` : 'Kept';
          existing.duplicateCount = dupCount;
          if (!existing.conditionBadge.includes('Lowest IL')) {
            existing.conditionBadge = `${existing.conditionBadge} • Lowest IL (${bestILText} of ${dupCount})`;
          }
        }
      }
    }
  });

  return Array.from(uniqueMap.values());
}

// -- 2. SEWSECTION Longitudinal Profile Engine --
function computeSectionDatum(nodes, settings) {
  if (!settings.secAutoDatum) {
    return parseFloat(settings.secManualDatum) || 0;
  }
  const minIL = Math.min(...nodes.map(n => n.ilOut));
  const baseU10 = parseFloat(settings.secBaseDatumUnder10) || 0;
  let dVal = minIL >= 10.0
    ? Math.floor(minIL / 10.0) * 10.0 + baseU10
    : baseU10;
  if (dVal > minIL - 1.0) {
    dVal = Math.max(0, Math.floor(minIL - 2.0));
  }
  return dVal;
}

function buildSewSections(rawRows, { columnMap, columns, settings }) {
  const limitM = parseFloat(settings.secMaxDist) || 200;
  const splitMode = settings.secSplitMode || 'CROSS';
  const allSections = [];
  let curRun = [];
  let pendingPipe = null;
  let cumDist = 0.0;

  const cleanNum = (val) => {
    if (val === null || val === undefined) return NaN;
    const s = String(val).trim();
    if (!s || s === '-' || /#DIV|#REF|#VALUE|#N\/A/i.test(s)) return NaN;
    const cleaned = s.replace(/[^0-9.-]/g, '');
    return cleaned ? parseFloat(cleaned) : NaN;
  };

  const formatMhTag = (rawPrefix, rawSub, row) => {
    const baseCombined = composeId(rawPrefix, rawSub).trim();
    const combined = seEnrichIdWithRowTags(baseCombined, row, columnMap, columns);
    if (!combined) return '';
    if (/^(PE\s+FROM|TOTAL|REMARK|NOTE|MANHOLE|MH\s*NO|BRANCH|LOCATION)/i.test(combined)) return '';

    const mNum = combined.match(/^0*(\d+)(?:\.0+)?([A-Za-z]?)$/);
    if (mNum) {
      return `MH${mNum[1]}${mNum[2].toUpperCase()}`;
    }

    const mMh = combined.match(/^(?:(EXTG\.?|EXISTING|PROP\.?|NEW)\s*[-_.]?\s*)?MH\s*[-_.]?\s*0*(\d+)(?:\.0+)?([A-Za-z]?)(?:\s*\(?([A-Za-z0-9.\s-]+)\)?)?$/i);
    if (mMh) {
      const lead = mMh[1] ? `${mMh[1].toUpperCase().replace(/\.$/, '').replace('EXISTING', 'EXTG')} ` : '';
      const num = mMh[2];
      const sub = (mMh[3] || '').toUpperCase();
      const tail = mMh[4] ? ` (${mMh[4].toUpperCase().trim()})` : '';
      return `${lead}MH${num}${sub}${tail}`;
    }

    const mNumTag = combined.match(/^0*(\d+)(?:\.0+)?([A-Za-z]?)\s*\(([A-Za-z0-9.\s-]+)\)$/);
    if (mNumTag) {
      return `MH${mNumTag[1]}${mNumTag[2].toUpperCase()} (${mNumTag[3].toUpperCase().trim()})`;
    }

    const mSpecNum = combined.match(/^(EXTG\.?|EXISTING|STP|FMH|SMH|PS|SPS|SUMP|OUTFALL|CROSSING)\s*[-_.]?\s*0*(\d+)(?:\.0+)?([A-Za-z]?)$/i);
    if (mSpecNum) {
      const tag = mSpecNum[1].toUpperCase().replace(/\.$/, '').replace('EXISTING', 'EXTG');
      const num = mSpecNum[2];
      const sub = (mSpecNum[3] || '').toUpperCase();
      return tag === 'EXTG' ? `EXTG MH${num}${sub}` : `${tag}${num}${sub}`;
    }

    if (SE_SPECIAL_TAG_EXTRACT_RE.test(combined) || /^[A-Za-z0-9][A-Za-z0-9\s\-_.()]{0,18}$/.test(combined)) {
      return combined.toUpperCase().replace(/\s+/g, ' ').trim();
    }

    return '';
  };

  rawRows.forEach((row) => {
    const idMainCol = columnMap.sewerId || columnMap.drainId;
    const idSubCol = columnMap.sewerId ? columnMap.sewerIdSub : columnMap.drainIdSub;
    const rawA = idMainCol ? row[idMainCol] : '';
    const rawB = idSubCol ? row[idSubCol] : '';
    const rawCheck = `${String(rawA || '')} ${String(rawB || '')}`.trim().toUpperCase();

    if (rawCheck.startsWith('PE FROM') || rawCheck.startsWith('BRANCH')) {
      if (curRun.length >= 2) {
        allSections.push(curRun);
      }
      curRun = [];
      pendingPipe = null;
      cumDist = 0.0;
      return;
    }

    const mhID = formatMhTag(rawA, rawB, row);
    if (!mhID) return;

    const lenVal = cleanNum(row[columnMap.length]);
    const sizeVal = cleanNum(row[columnMap.size]);
    let glVal = cleanNum(row[columnMap.gl]);
    let ilVal = cleanNum(row[columnMap.il]);
    const depthColVal = cleanNum(row[columnMap.depth]);
    const bdVal = cleanNum(row[columnMap.backdrop]);
    let matVal = String(row[columnMap.pipeType] || '').trim().toUpperCase();
    if (!matVal || matVal === '-') matVal = 'VCP';

    if (isNaN(glVal) && !isNaN(ilVal)) {
      if (!isNaN(depthColVal) && depthColVal > 0) {
        glVal = ilVal + depthColVal;
      } else if (curRun.length > 0 && SE_SPECIAL_TAG_EXTRACT_RE.test(mhID)) {
        glVal = curRun[curRun.length - 1].gl;
      }
    } else if (isNaN(ilVal) && !isNaN(glVal) && !isNaN(depthColVal)) {
      ilVal = glVal - depthColVal;
    }

    let gradRaw = String(row[columnMap.gradient] || '').trim();
    if (gradRaw === '1' && columnMap.gradient) {
      const gIdx = columns.indexOf(columnMap.gradient);
      if (gIdx !== -1 && gIdx + 2 < columns.length) {
        const colonCell = String(row[columns[gIdx + 1]] || '').trim();
        const denomCell = String(row[columns[gIdx + 2]] || '').trim();
        if (colonCell === ':' && !isNaN(cleanNum(denomCell))) {
          gradRaw = denomCell;
        }
      }
    }
    gradRaw = gradRaw.replace(/^1\s*[:/]\s*/i, '');
    const gradNum = cleanNum(gradRaw);
    const gradStr = !isNaN(gradNum) && gradNum > 0 ? String(Math.round(gradNum)) : '200';
    const sizeStr = !isNaN(sizeVal) && sizeVal > 0 ? String(Math.round(sizeVal)) : '225';

    if (isNaN(glVal) || isNaN(ilVal)) return;

    // Case 1: Same Manhole ID as last node in curRun
    if (curRun.length > 0 && curRun[curRun.length - 1].id === mhID) {
      const lastNode = curRun[curRun.length - 1];
      if (ilVal < lastNode.ilIn - 0.005) {
        lastNode.ilOut = ilVal;
      }
      if (!isNaN(bdVal) && bdVal > 0.005 && Math.abs(lastNode.ilIn - lastNode.ilOut) < 0.005) {
        lastNode.ilIn = lastNode.ilOut + bdVal;
      }
      if (!isNaN(sizeVal) && sizeVal > 0) lastNode.size = sizeStr;
      if (matVal) lastNode.mat = matVal;
      if (!isNaN(gradNum) && gradNum > 0) lastNode.grad = gradStr;

      if (!isNaN(lenVal) && lenVal > 0) {
        lastNode.outLen = lenVal;
        pendingPipe = { len: lenVal, size: sizeStr, mat: matVal, grad: gradStr };
      }
    }
    // Case 2: Downstream Manhole arriving via pendingPipe
    else if (curRun.length > 0 && pendingPipe) {
      const nextDist = cumDist + pendingPipe.len;

      if (splitMode === 'STRICT' && nextDist > limitM && curRun.length >= 2) {
        allSections.push(curRun);
        const prevLast = curRun[curRun.length - 1];
        curRun = [{
          ...prevLast,
          dist: 0.0,
          ilIn: prevLast.ilOut,
          ilOut: prevLast.ilOut,
        }];
        cumDist = pendingPipe.len;
      } else {
        cumDist = nextDist;
      }

      const ilIn = (!isNaN(bdVal) && bdVal > 0.005) ? (ilVal + bdVal) : ilVal;
      const newNode = {
        id: mhID,
        dist: cumDist,
        gl: glVal,
        ilIn,
        ilOut: ilVal,
        depth: glVal - ilVal,
        bd: (!isNaN(bdVal) && bdVal > 0.005) ? bdVal : 0,
        size: sizeStr,
        mat: matVal,
        grad: gradStr,
        outLen: (!isNaN(lenVal) && lenVal > 0) ? lenVal : null,
      };
      curRun.push(newNode);

      if (!isNaN(lenVal) && lenVal > 0) {
        pendingPipe = { len: lenVal, size: sizeStr, mat: matVal, grad: gradStr };
      } else {
        pendingPipe = null;
      }

      if (splitMode === 'CROSS' && cumDist >= limitM && pendingPipe) {
        allSections.push(curRun);
        curRun = [{
          ...newNode,
          dist: 0.0,
          ilIn: newNode.ilOut,
          ilOut: newNode.ilOut,
          bd: 0,
        }];
        cumDist = 0.0;
      }
    }
    // Case 3: Start of a New Branch
    else {
      if (curRun.length >= 2) {
        allSections.push(curRun);
      }
      cumDist = 0.0;
      const ilIn = (!isNaN(bdVal) && bdVal > 0.005) ? (ilVal + bdVal) : ilVal;
      const firstNode = {
        id: mhID,
        dist: 0.0,
        gl: glVal,
        ilIn,
        ilOut: ilVal,
        depth: glVal - ilVal,
        bd: (!isNaN(bdVal) && bdVal > 0.005) ? bdVal : 0,
        size: sizeStr,
        mat: matVal,
        grad: gradStr,
        outLen: (!isNaN(lenVal) && lenVal > 0) ? lenVal : null,
      };
      curRun = [firstNode];
      if (!isNaN(lenVal) && lenVal > 0) {
        pendingPipe = { len: lenVal, size: sizeStr, mat: matVal, grad: gradStr };
      } else {
        pendingPipe = null;
      }
    }
  });

  if (curRun.length >= 2) {
    allSections.push(curRun);
  }

  return allSections.map((nodes, idx) => {
    const datum = computeSectionDatum(nodes, settings);
    const titleMHs = nodes.map(n => n.id).join(', ');
    const totalLength = nodes[nodes.length - 1].dist;
    return {
      index: idx + 1,
      title: `LONG. SECTION OF ${titleMHs}`,
      datum,
      totalLength,
      nodes,
    };
  });
}

function filterCadRows(processedRows, { filterCategory, searchTerm }) {
  return processedRows.filter(item => {
    if (filterCategory === 'DRAINAGE' && item.category !== 'DRAINAGE') return false;
    if (filterCategory === 'SEWERAGE' && item.category !== 'SEWERAGE') return false;
    if (filterCategory === 'SHORT' && !item.conditionBadge.includes('Short')) return false;
    if (filterCategory === 'MEDIUM' && !item.conditionBadge.includes('Medium')) return false;
    if (filterCategory === 'LONG' && !item.conditionBadge.includes('Long')) return false;
    if (filterCategory === 'MERGED' && !(item.duplicateCount > 1)) return false;

    if (!searchTerm.trim()) return true;
    const s = searchTerm.toLowerCase();
    return (
      item.id.toLowerCase().includes(s) ||
      item.cadString.toLowerCase().includes(s) ||
      item.conditionBadge.toLowerCase().includes(s)
    );
  });
}

function computeCadStats(processedRows, sewSections) {
  const total = processedRows.length;
  const drainageCount = processedRows.filter(r => r.category === 'DRAINAGE').length;
  const sewerageCount = processedRows.filter(r => r.category === 'SEWERAGE').length;
  const shortCount = processedRows.filter(r => r.conditionBadge.includes('Short')).length;
  const medCount = processedRows.filter(r => r.conditionBadge.includes('Medium')).length;
  const longCount = processedRows.filter(r => r.conditionBadge.includes('Long') || r.conditionBadge.includes('OFF')).length;
  const mergedCount = processedRows.filter(r => r.duplicateCount > 1).length;
  const totalSecNodes = sewSections.reduce((acc, s) => acc + s.nodes.length, 0);
  return { total, drainageCount, sewerageCount, shortCount, medCount, longCount, mergedCount, totalSecNodes };
}

window.buildDrainageCadString = buildDrainageCadString;
window.buildSewerageCadString = buildSewerageCadString;
window.normalizeCadId = normalizeCadId;
window.formatCadRow = formatCadRow;
window.processAndDeduplicateRows = processAndDeduplicateRows;
window.computeSectionDatum = computeSectionDatum;
window.buildSewSections = buildSewSections;
window.filterCadRows = filterCadRows;
window.computeCadStats = computeCadStats;
