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

// Evaluates Civil Engineering QA/QC rules for a row or node
function evaluateQaWarnings({ isSewerRow, gl, il, depth, ilIn, ilOut, ilUp, ilDown, bd, grad }) {
  const warnings = [];
  const numGL = parseFloat(gl);
  const numIL = parseFloat(il);
  const numDepth = !isNaN(parseFloat(depth))
    ? parseFloat(depth)
    : (!isNaN(numGL) && !isNaN(numIL) ? numGL - numIL : NaN);
  const numIlIn = parseFloat(ilIn);
  const numIlOut = parseFloat(ilOut);
  const numIlUp = parseFloat(ilUp);
  const numIlDown = parseFloat(ilDown);
  const numBd = parseFloat(bd);
  const numGrad = parseFloat(grad);

  // 1. Reverse pipe slope / backfall (FromIL_Out < ToIL_In or Upstream IL < Downstream IL)
  if (!isNaN(numIlUp) && !isNaN(numIlDown) && numIlUp < numIlDown - 0.001) {
    warnings.push(`Backfall (Up IL ${numIlUp.toFixed(2)}m < Down IL ${numIlDown.toFixed(2)}m)`);
  }
  if (!isNaN(numIlIn) && !isNaN(numIlOut) && numIlIn < numIlOut - 0.001) {
    warnings.push(`Reverse MH Drop (IL In ${numIlIn.toFixed(2)}m < IL Out ${numIlOut.toFixed(2)}m)`);
  }

  // 2. Shallow depth (< 1.2m)
  if (isSewerRow && !isNaN(numDepth) && numDepth > 0 && numDepth < 1.2) {
    warnings.push(`Shallow Depth (${numDepth.toFixed(2)}m < 1.20m)`);
  }

  // 3. Backdrop manhole requirement (ilIn - ilOut >= 0.60m)
  const dropVal = !isNaN(numBd) && numBd > 0
    ? numBd
    : (!isNaN(numIlIn) && !isNaN(numIlOut) ? (numIlIn - numIlOut) : NaN);
  if (isSewerRow && !isNaN(dropVal) && dropVal >= 0.60) {
    warnings.push(`Backdrop Required (Drop ${dropVal.toFixed(2)}m >= 0.60m)`);
  }

  // 4. Extreme gradients (< 1:15 or > 1:500)
  if (!isNaN(numGrad) && numGrad > 0) {
    if (numGrad < 15) {
      warnings.push(`Extreme Steep Gradient (1:${Math.round(numGrad)} < 1:15)`);
    } else if (numGrad > 500) {
      warnings.push(`Extreme Flat Gradient (1:${Math.round(numGrad)} > 1:500)`);
    }
  }

  return warnings;
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
  let bdRaw = String(row[columnMap.backdrop] || '').trim();

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
  bdRaw = cleanNumber(bdRaw);

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
  const maxCandidateIL = candidateILs.length > 0 ? Math.max(...candidateILs) : NaN;

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
    const qaWarnings = evaluateQaWarnings({
      isSewerRow: true,
      gl: sewBuilt.glDisplay,
      il: sewBuilt.ilDisplay,
      depth: sewBuilt.depthDisplay,
      ilIn: !isNaN(maxCandidateIL) ? maxCandidateIL : ilUpRaw,
      ilOut: !isNaN(effectiveIL) ? effectiveIL : ilDownRaw,
      ilUp: ilUpRaw,
      ilDown: ilDownRaw,
      bd: bdRaw,
      grad: gradRaw,
    });
    return {
      id: effectiveId,
      category: 'SEWERAGE',
      cadString: sewBuilt.cadString,
      conditionBadge: hasSpecialTag ? 'Special Node (EXTG/STP Included)' : 'Manhole (4-Line Label)',
      visualLines,
      effectiveIL,
      maxIL: maxCandidateIL,
      qaWarnings,
      drainDetails: { size: sizeRaw, length: lengthRaw, gradient: gradRaw, il: !isNaN(effectiveIL) ? effectiveIL.toFixed(2) : '' },
      sewerDetails: { gl: sewBuilt.glDisplay, il: sewBuilt.ilDisplay, depth: sewBuilt.depthDisplay },
      rawRow: row,
    };
  } else {
    const drnBuilt = buildDrainageCadString(effectiveId, sizeRaw, lengthRaw, gradRaw, settings);
    const visualLines = drnBuilt.cadString.replace(/^\{\\W[\d.]+;/, '').replace(/\}$/, '').replace(/%%c/g, 'Ø').split('\\P');
    const qaWarnings = evaluateQaWarnings({
      isSewerRow: false,
      gl: glRaw,
      il: !isNaN(effectiveIL) ? effectiveIL : ilRaw,
      depth: depthRaw,
      ilUp: ilUpRaw,
      ilDown: ilDownRaw,
      bd: bdRaw,
      grad: drnBuilt.gradDisplay,
    });
    return {
      id: effectiveId,
      category: 'DRAINAGE',
      cadString: drnBuilt.cadString,
      conditionBadge: hasSpecialTag ? `${drnBuilt.conditionBadge} • EXTG/Special` : drnBuilt.conditionBadge,
      visualLines,
      effectiveIL,
      maxIL: maxCandidateIL,
      qaWarnings,
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

  // Check sequential connected rows for reverse pipe slope / backfall (FromIL_Out < ToIL_In)
  for (let i = 0; i < rawFormatted.length - 1; i++) {
    const curr = rawFormatted[i];
    const next = rawFormatted[i + 1];
    const spanLen = parseFloat(curr.drainDetails && curr.drainDetails.length);
    if (
      curr.id.toUpperCase() !== next.id.toUpperCase() &&
      !isNaN(spanLen) && spanLen > 0 &&
      !isNaN(curr.effectiveIL) && !isNaN(next.effectiveIL) &&
      curr.effectiveIL < next.effectiveIL - 0.001
    ) {
      const bfMsg = `Backfall (${curr.id} IL ${curr.effectiveIL.toFixed(2)}m < ${next.id} IL ${next.effectiveIL.toFixed(2)}m)`;
      if (!curr.qaWarnings.includes(bfMsg)) {
        curr.qaWarnings.push(bfMsg);
      }
    }
  }

  rawFormatted.forEach(item => {
    const cleanId = item.id.toUpperCase();

    if (!uniqueMap.has(cleanId)) {
      uniqueMap.set(cleanId, { ...item, duplicateCount: 1, qaWarnings: [...(item.qaWarnings || [])] });
    } else {
      const existing = uniqueMap.get(cleanId);
      const dupCount = (existing.duplicateCount || 1) + 1;
      const existingIL = existing.effectiveIL;
      const newIL = item.effectiveIL;
      const combinedMaxIL = Math.max(
        !isNaN(existing.maxIL) ? existing.maxIL : (!isNaN(existingIL) ? existingIL : -Infinity),
        !isNaN(item.maxIL) ? item.maxIL : (!isNaN(newIL) ? newIL : -Infinity)
      );
      const combinedMinIL = Math.min(
        !isNaN(existingIL) ? existingIL : Infinity,
        !isNaN(newIL) ? newIL : Infinity
      );

      const mergedWarnings = Array.from(new Set([...(existing.qaWarnings || []), ...(item.qaWarnings || [])]));
      if (item.category === 'SEWERAGE' && isFinite(combinedMaxIL) && isFinite(combinedMinIL)) {
        const drop = combinedMaxIL - combinedMinIL;
        if (drop >= 0.60 && !mergedWarnings.some(w => w.startsWith('Backdrop Required'))) {
          mergedWarnings.push(`Backdrop Required (Drop ${drop.toFixed(2)}m >= 0.60m)`);
        }
      }

      const newIsLower = (!isNaN(newIL) && (isNaN(existingIL) || newIL < existingIL));

      if (item.category === 'SEWERAGE') {
        if (newIsLower) {
          const gl = parseFloat(item.sewerDetails.gl) || parseFloat(existing.sewerDetails.gl) || 0;
          const sewBuilt = buildSewerageCadString(item.id, gl.toFixed(2), newIL.toFixed(2), '', settings);
          const recomputedWarnings = evaluateQaWarnings({
            isSewerRow: true,
            gl: sewBuilt.glDisplay,
            il: sewBuilt.ilDisplay,
            depth: sewBuilt.depthDisplay,
            ilIn: isFinite(combinedMaxIL) ? combinedMaxIL : NaN,
            ilOut: newIL,
            grad: item.drainDetails.gradient || existing.drainDetails.gradient,
          });
          const allWarnings = Array.from(new Set([...mergedWarnings.filter(w => !w.startsWith('Shallow Depth')), ...recomputedWarnings]));
          const updatedItem = {
            ...item,
            duplicateCount: dupCount,
            effectiveIL: newIL,
            maxIL: isFinite(combinedMaxIL) ? combinedMaxIL : newIL,
            qaWarnings: allWarnings,
            cadString: sewBuilt.cadString,
            sewerDetails: { gl: sewBuilt.glDisplay, il: sewBuilt.ilDisplay, depth: sewBuilt.depthDisplay },
            conditionBadge: `Merged ${dupCount} Rows (Lowest IL: ${newIL.toFixed(2)}m)`,
            visualLines: sewBuilt.cadString.replace(/^\{\\W[\d.]+;/, '').replace(/\}$/, '').split('\\P'),
          };
          uniqueMap.set(cleanId, updatedItem);
        } else {
          const bestILText = !isNaN(existingIL) ? `${existingIL.toFixed(2)}m` : 'Kept';
          existing.duplicateCount = dupCount;
          existing.maxIL = isFinite(combinedMaxIL) ? combinedMaxIL : existing.maxIL;
          existing.qaWarnings = mergedWarnings;
          existing.conditionBadge = `Merged ${dupCount} Rows (Lowest IL: ${bestILText})`;
        }
      } else {
        if (newIsLower) {
          const updatedItem = {
            ...item,
            duplicateCount: dupCount,
            effectiveIL: newIL,
            maxIL: isFinite(combinedMaxIL) ? combinedMaxIL : newIL,
            qaWarnings: mergedWarnings,
            conditionBadge: `${item.conditionBadge} • Lowest IL (${newIL.toFixed(2)}m of ${dupCount})`,
          };
          uniqueMap.set(cleanId, updatedItem);
        } else {
          const bestILText = !isNaN(existingIL) ? `${existingIL.toFixed(2)}m` : 'Kept';
          existing.duplicateCount = dupCount;
          existing.qaWarnings = mergedWarnings;
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
      pendingPipe = null;
      return;
    }

    let mhID = formatMhTag(rawA, rawB, row);
    const lenVal = cleanNum(row[columnMap.length]);
    const sizeVal = cleanNum(row[columnMap.size]);
    let glVal = cleanNum(row[columnMap.gl]);
    let ilVal = cleanNum(row[columnMap.il]);
    const depthColVal = cleanNum(row[columnMap.depth]);
    const bdVal = cleanNum(row[columnMap.backdrop]);
    let matVal = String(row[columnMap.pipeType] || '').trim().toUpperCase();
    if (!matVal || matVal === '-') matVal = 'VCP';

    if (!mhID && curRun.length > 0 && (!isNaN(glVal) || !isNaN(ilVal) || !isNaN(lenVal))) {
      mhID = curRun[curRun.length - 1].id;
    }
    if (!mhID) return;

    if (isNaN(glVal) && !isNaN(ilVal)) {
      if (!isNaN(depthColVal) && depthColVal > 0) {
        glVal = ilVal + depthColVal;
      } else if (curRun.length > 0 && (curRun[curRun.length - 1].id === mhID || SE_SPECIAL_TAG_EXTRACT_RE.test(mhID))) {
        glVal = curRun[curRun.length - 1].gl;
      }
    } else if (isNaN(ilVal) && !isNaN(glVal)) {
      if (!isNaN(depthColVal) && depthColVal > 0) {
        ilVal = glVal - depthColVal;
      } else if (curRun.length > 0 && curRun[curRun.length - 1].id === mhID) {
        ilVal = curRun[curRun.length - 1].ilOut;
      }
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

    // Case 1: Same Manhole ID as last node in curRun -> update to Highest GL & Lowest IL
    if (curRun.length > 0 && curRun[curRun.length - 1].id === mhID) {
      const lastNode = curRun[curRun.length - 1];
      lastNode.gl = Math.max(lastNode.gl, glVal);
      lastNode.ilIn = Math.max(lastNode.ilIn, ilVal);
      lastNode.ilOut = Math.min(lastNode.ilOut, ilVal);
      lastNode.depth = lastNode.gl - lastNode.ilOut;
      lastNode.bd = (lastNode.ilIn - lastNode.ilOut) > 0.005 ? (lastNode.ilIn - lastNode.ilOut) : 0;

      if (!isNaN(sizeVal) && sizeVal > 0) lastNode.size = sizeStr;
      if (matVal) lastNode.mat = matVal;
      if (!isNaN(gradNum) && gradNum > 0) lastNode.grad = gradStr;

      if (curRun.length === 1 && allSections.length > 0) {
        const prevSec = allSections[allSections.length - 1];
        const prevLast = prevSec[prevSec.length - 1];
        if (prevLast && prevLast.id === mhID) {
          prevLast.gl = Math.max(prevLast.gl, lastNode.gl);
          prevLast.ilIn = Math.max(prevLast.ilIn, lastNode.ilIn);
          prevLast.ilOut = Math.min(prevLast.ilOut, lastNode.ilOut);
          prevLast.depth = prevLast.gl - prevLast.ilOut;
          prevLast.bd = (prevLast.ilIn - prevLast.ilOut) > 0.005 ? (prevLast.ilIn - prevLast.ilOut) : 0;
        }
      }

      if (!isNaN(lenVal) && lenVal > 0) {
        lastNode.outLen = lenVal;
        pendingPipe = { len: lenVal, size: sizeStr, mat: matVal, grad: gradStr };
      }
    }
    // Case 2: Downstream Manhole arriving via pendingPipe
    else if (curRun.length > 0 && pendingPipe) {
      const nextDist = cumDist + pendingPipe.len;

        if (
          curRun.length >= 2 &&
          ((splitMode === 'STRICT' && nextDist > limitM) ||
            (splitMode === 'CROSS' && cumDist >= limitM))
        ) {
          allSections.push(curRun);
          const prevLast = curRun[curRun.length - 1];
          curRun = [{
            ...prevLast,
            dist: 0.0,
            ilIn: prevLast.ilOut,
            ilOut: prevLast.ilOut,
            bd: 0,
          }];
          cumDist = pendingPipe.len;
        } else {
          cumDist = nextDist;
        }

        const hasSingleRowBd = !isNaN(bdVal) && bdVal > 0.005 && (isNaN(lenVal) || lenVal <= 0);
        const ilIn = hasSingleRowBd ? (ilVal + bdVal) : ilVal;
        const newNode = {
          id: mhID,
          dist: cumDist,
          gl: glVal,
          ilIn,
          ilOut: ilVal,
          depth: glVal - ilVal,
          bd: hasSingleRowBd ? bdVal : 0,
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
      }
      // Case 3: Start of a New Branch
      else {
        if (curRun.length >= 2) {
          allSections.push(curRun);
        }
        cumDist = 0.0;
        const hasSingleRowBd = !isNaN(bdVal) && bdVal > 0.005 && (isNaN(lenVal) || lenVal <= 0);
        const ilIn = hasSingleRowBd ? (ilVal + bdVal) : ilVal;
        const firstNode = {
          id: mhID,
          dist: 0.0,
          gl: glVal,
          ilIn,
          ilOut: ilVal,
          depth: glVal - ilVal,
          bd: hasSingleRowBd ? bdVal : 0,
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
    nodes.forEach((node, nIdx) => {
      const nodeWarnings = [];
      const spanWarnings = [];
      const nodeDepth = node.gl - node.ilOut;
      const nodeDrop = node.ilIn - node.ilOut;

      // 1. Shallow depth (< 1.2m)
      if (!isNaN(nodeDepth) && nodeDepth > 0 && nodeDepth < 1.2) {
        nodeWarnings.push(`Shallow Depth (${nodeDepth.toFixed(2)}m < 1.20m)`);
      }

      // 2. Backdrop manhole requirement (ilIn - ilOut >= 0.60m)
      if (!isNaN(nodeDrop) && nodeDrop >= 0.60) {
        nodeWarnings.push(`Backdrop Required (Drop ${nodeDrop.toFixed(2)}m >= 0.60m)`);
      }

      // Outgoing pipe span checks (to next node in section)
      if (nIdx < nodes.length - 1) {
        const nextNode = nodes[nIdx + 1];
        // 3. Reverse pipe slope / backfall (FromIL_Out < ToIL_In)
        if (!isNaN(node.ilOut) && !isNaN(nextNode.ilIn) && node.ilOut < nextNode.ilIn - 0.001) {
          const bfMsg = `Backfall (${node.id} IL ${node.ilOut.toFixed(2)}m < ${nextNode.id} IL ${nextNode.ilIn.toFixed(2)}m)`;
          nodeWarnings.push(bfMsg);
          spanWarnings.push(bfMsg);
        }

        // 4. Extreme gradients (< 1:15 or > 1:500)
        const gNum = parseFloat(node.grad);
        if (!isNaN(gNum) && gNum > 0) {
          if (gNum < 15) {
            const stMsg = `Extreme Steep Gradient (1:${Math.round(gNum)} < 1:15)`;
            nodeWarnings.push(stMsg);
            spanWarnings.push(stMsg);
          } else if (gNum > 500) {
            const flMsg = `Extreme Flat Gradient (1:${Math.round(gNum)} > 1:500)`;
            nodeWarnings.push(flMsg);
            spanWarnings.push(flMsg);
          }
        }
      }

      node.qaWarnings = nodeWarnings;
      node.spanQaWarnings = spanWarnings;
    });

    const datum = computeSectionDatum(nodes, settings);
    const titleMHs = nodes.map(n => n.id).join(', ');
    const totalLength = nodes[nodes.length - 1].dist;
    const qaWarningCount = nodes.reduce((acc, n) => acc + (n.qaWarnings ? n.qaWarnings.length : 0), 0);
    return {
      index: idx + 1,
      title: `LONG. SECTION OF ${titleMHs}`,
      datum,
      totalLength,
      qaWarningCount,
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
    if (filterCategory === 'QA_WARN' && !(item.qaWarnings && item.qaWarnings.length > 0)) return false;

    if (!searchTerm.trim()) return true;
    const s = searchTerm.toLowerCase();
    return (
      item.id.toLowerCase().includes(s) ||
      item.cadString.toLowerCase().includes(s) ||
      item.conditionBadge.toLowerCase().includes(s) ||
      (item.qaWarnings && item.qaWarnings.some(w => w.toLowerCase().includes(s)))
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
  const qaWarningCount = processedRows.filter(r => r.qaWarnings && r.qaWarnings.length > 0).length;
  const totalSecNodes = sewSections.reduce((acc, s) => acc + s.nodes.length, 0);
  const totalSecWarnings = sewSections.reduce((acc, s) => acc + (s.qaWarningCount || 0), 0);
  return {
    total,
    drainageCount,
    sewerageCount,
    shortCount,
    medCount,
    longCount,
    mergedCount,
    qaWarningCount,
    totalSecNodes,
    totalSecWarnings,
  };
}

window.buildDrainageCadString = buildDrainageCadString;
window.buildSewerageCadString = buildSewerageCadString;
window.normalizeCadId = normalizeCadId;
window.evaluateQaWarnings = evaluateQaWarnings;
window.formatCadRow = formatCadRow;
window.processAndDeduplicateRows = processAndDeduplicateRows;
window.computeSectionDatum = computeSectionDatum;
window.buildSewSections = buildSewSections;
window.filterCadRows = filterCadRows;
window.computeCadStats = computeCadStats;
