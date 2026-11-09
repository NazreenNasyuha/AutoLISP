/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Step 1 Discipline & Column Mapping Panel
 * ============================================================================ */

function DetectionPanel({
  rawRows,
  userDesignMode,
  setUserDesignMode,
  detectedMode,
  detectionConfidence,
  detectionReason,
  detectionScores,
  detectionNotes,
  idInfo,
  targetTool,
  setTargetTool,
  effectiveMode,
  columnMap,
  setColumnMap,
  columns,
  processedRows,
  sewSections,
  showManualMapper,
  setShowManualMapper,
}) {
  if (!rawRows || rawRows.length === 0) return null;

  const META = {
    DRAINAGE: { icon: '🌊', label: 'Road Drainage', badge: 'bg-cyan-500/15 text-cyan-300 border-cyan-500/40', box: 'border-cyan-500/40' },
    SEWERAGE: { icon: '🕳️', label: 'Sewerage', badge: 'bg-emerald-500/15 text-emerald-300 border-emerald-500/40', box: 'border-emerald-500/40' },
    HYBRID: { icon: '🔀', label: 'Mixed Drainage + Sewerage', badge: 'bg-violet-500/15 text-violet-300 border-violet-500/40', box: 'border-violet-500/40' },
    UNKNOWN: { icon: '❓', label: 'Could not tell', badge: 'bg-amber-500/15 text-amber-300 border-amber-500/40', box: 'border-amber-500/50' },
  };
  const shown = userDesignMode === 'AUTO' ? detectedMode : userDesignMode;
  const meta = META[shown] || META.UNKNOWN;
  const isOverride = userDesignMode !== 'AUTO' && userDesignMode !== detectedMode;
  const needsChoice = userDesignMode === 'AUTO' && detectedMode === 'UNKNOWN' && targetTool !== 'SEWSECTION';
  const wrongForSection = targetTool === 'SEWSECTION' && userDesignMode === 'AUTO' && detectedMode === 'DRAINAGE';

  const need = targetTool === 'SEWSECTION'
    ? [['sewerId', 'Manhole ID'], ['gl', 'Ground Level'], ['il', 'Invert Level'], ['length', 'Length']]
    : shown === 'SEWERAGE' ? [['sewerId', 'Manhole ID'], ['gl', 'Ground Level'], ['il', 'Invert Level']]
    : shown === 'DRAINAGE' ? [['drainId', 'Drain ID'], ['size', 'Size'], ['length', 'Length'], ['gradient', 'Gradient']]
    : shown === 'HYBRID' ? [['drainId', 'ID'], ['gl', 'Ground Level'], ['il', 'Invert Level'], ['size', 'Size']]
    : [];
  const missing = need.filter(([k]) => !columnMap[k]).map(([, label]) => label);
  const previewIds = targetTool === 'SEWSECTION'
    ? idInfo.samples
    : processedRows.slice(0, 5).map(r => r.id);

  return (
    <>
      {/* 3B. STEP 1 - DISCIPLINE DETECTION (Drainage vs Sewerage) */}
      <div className={`bg-cadslate-900 border ${meta.box} rounded-xl p-4 space-y-3 shadow-lg`}>
        <div className="flex flex-col md:flex-row md:items-center justify-between gap-2">
          <div className="flex flex-wrap items-center gap-2">
            <span className="text-xs font-bold uppercase tracking-wider text-slate-400">Step 1 · File Type</span>
            <span className={`text-xs font-bold px-3 py-1 rounded-full border ${meta.badge}`}>
              {meta.icon} {meta.label}
              {userDesignMode === 'AUTO' && detectedMode !== 'UNKNOWN' ? ` · ${detectionConfidence} confidence` : ''}
              {isOverride ? ' · chosen by you' : ''}
            </span>
          </div>
          {isOverride && (
            <button
              type="button"
              onClick={() => setUserDesignMode('AUTO')}
              className="text-xs text-cadcyan-400 hover:text-cadcyan-300 font-semibold self-start md:self-center"
            >
              ↺ Back to auto-detect ({detectedMode})
            </button>
          )}
          {!needsChoice && shown === 'SEWERAGE' && sewSections.length > 0 && (
            <div className="flex flex-wrap items-center gap-1.5">
              <button
                type="button"
                onClick={() => setTargetTool('CSVUPDATE')}
                className={`px-2.5 py-1 rounded-lg text-xs font-bold border transition ${
                  targetTool === 'CSVUPDATE'
                    ? 'bg-emerald-500 text-black border-emerald-400'
                    : 'bg-cadslate-950 text-slate-300 border-cadslate-700 hover:text-white'
                }`}
              >
                🕳️ Plan View Labels ({processedRows.length} MHs)
              </button>
              <button
                type="button"
                onClick={() => setTargetTool('SEWSECTION')}
                className={`px-2.5 py-1 rounded-lg text-xs font-bold border transition ${
                  targetTool === 'SEWSECTION'
                    ? 'bg-cadcyan-500 text-black border-cadcyan-400'
                    : 'bg-cadslate-950 text-slate-300 border-cadslate-700 hover:text-white'
                }`}
              >
                📐 Long. Section ({sewSections.length} Profile{sewSections.length > 1 ? 's' : ''})
              </button>
            </div>
          )}
        </div>

        <p className="text-xs text-slate-400 leading-relaxed">
          {detectionReason}
          {detectionScores && (detectionScores.sew > 0 || detectionScores.drn > 0)
            ? ` (score: sewerage ${detectionScores.sew} · drainage ${detectionScores.drn})`
            : ''}
        </p>

        {needsChoice && (
          <div className="bg-amber-950/30 border border-amber-500/40 rounded-lg p-3 space-y-2">
            <div className="text-xs font-semibold text-amber-300">
              I could not tell from the headers or the IDs whether this sheet is Drainage or Sewerage. Nothing is processed until you choose:
            </div>
            <div className="flex flex-wrap gap-2">
              <button
                type="button"
                onClick={() => setUserDesignMode('DRAINAGE')}
                className="px-3 py-1.5 rounded-lg text-xs font-bold bg-cadcyan-500 text-black hover:bg-cadcyan-400"
              >
                🌊 It is Drainage
              </button>
              <button
                type="button"
                onClick={() => setUserDesignMode('SEWERAGE')}
                className="px-3 py-1.5 rounded-lg text-xs font-bold bg-emerald-500 text-black hover:bg-emerald-400"
              >
                🕳️ It is Sewerage
              </button>
              <button
                type="button"
                onClick={() => setUserDesignMode('HYBRID')}
                className="px-3 py-1.5 rounded-lg text-xs font-bold bg-cadslate-800 text-slate-200 border border-cadslate-700 hover:bg-cadslate-700"
              >
                🔀 Both in one sheet
              </button>
            </div>
          </div>
        )}

        {wrongForSection && (
          <div className="bg-rose-950/30 border border-rose-500/40 rounded-lg p-3 flex flex-col sm:flex-row sm:items-center justify-between gap-2">
            <div className="text-xs text-rose-300">
              This looks like a <strong>Drainage</strong> file, but Long Section (SEWSECTION) only works with sewerage manhole data.
            </div>
            <button
              type="button"
              onClick={() => setTargetTool('CSVUPDATE')}
              className="px-3 py-1.5 rounded-lg text-xs font-bold bg-rose-500 text-black hover:bg-rose-400 self-start sm:self-center"
            >
              Switch to CSVUPDATE
            </button>
          </div>
        )}

        {detectionNotes.map((n, i) => (
          <div key={i} className="text-xs text-cadcyan-300 bg-cadcyan-950/30 border border-cadcyan-500/30 rounded-lg px-3 py-2">
            ℹ {n}
          </div>
        ))}

        {idInfo.kind !== 'none' && (
          <div className="text-xs text-slate-300 flex flex-wrap items-center gap-1.5">
            <span className="text-slate-400">ID is built from:</span>
            <code className="px-1.5 py-0.5 rounded bg-cadslate-950 border border-cadslate-700 text-cadcyan-300">
              {idInfo.mainKey}
            </code>
            {idInfo.kind === 'pair' && (
              <>
                <span className="text-slate-500">+</span>
                <code className="px-1.5 py-0.5 rounded bg-cadslate-950 border border-cadslate-700 text-cadcyan-300">
                  {idInfo.subKey}
                </code>
              </>
            )}
            <span className="text-slate-500">+ Row Tags (EXTG/STP)</span>
            {previewIds.length > 0 && (
              <>
                <span className="text-slate-500">→</span>
                <span className="font-mono text-emerald-300">{previewIds.join(', ')} …</span>
              </>
            )}
          </div>
        )}

        {missing.length > 0 && shown !== 'UNKNOWN' && (
          <div className="text-xs text-amber-300 bg-amber-950/30 border border-amber-500/40 rounded-lg px-3 py-2">
            ⚠ Could not find a column for: <strong>{missing.join(', ')}</strong>. Open <em>Customize Column Mapping</em> below and pick it manually.
          </div>
        )}
      </div>

      {/* 4. "DETECTED COLUMNS" SUMMARY BOX */}
      <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl p-5 space-y-4 shadow-xl">
        <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2 border-b border-cadslate-800 pb-3">
          <div className="flex items-center space-x-3">
            <div className="w-8 h-8 rounded-lg bg-cadcyan-500/15 border border-cadcyan-500/30 text-cadcyan-400 flex items-center justify-center">
              <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M13 10V3L4 14h7v7l9-11h-7z" />
              </svg>
            </div>
            <div>
              <h4 className="text-sm font-bold text-white flex flex-wrap items-center gap-2">
                <span>Smart Header Auto-Detection Summary</span>
                <span className="text-[10px] font-bold px-2.5 py-0.5 rounded-full bg-cadcyan-500/15 text-cadcyan-300 border border-cadcyan-500/40">
                  Target Tool: {targetTool} ({effectiveMode})
                </span>
              </h4>
              <p className="text-xs text-slate-400">
                {targetTool === 'SEWSECTION'
                  ? 'Mapping Manhole/EXTG/STP ID, Pipe Length, Size, Gradient, Pipe Type (VCP/RCP), GL, IL & Backdrop for Longitudinal Section'
                  : `Signals: ${detectionReason} • Duplicate rows automatically merge by lowest Invert Level`}
              </p>
            </div>
          </div>

          <button
            type="button"
            onClick={() => setShowManualMapper(!showManualMapper)}
            className="inline-flex items-center space-x-1 text-xs text-cadcyan-400 hover:text-cadcyan-300 self-start sm:self-center font-semibold bg-cadslate-800 px-3 py-1.5 rounded-lg border border-cadslate-700 transition"
          >
            <span>{showManualMapper ? 'Hide Manual Mapper' : 'Customize Column Mapping'}</span>
            <svg className={`w-3.5 h-3.5 transition transform ${showManualMapper ? 'rotate-180' : ''}`} fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M19 9l-7 7-7-7" />
            </svg>
          </button>
        </div>

        {/* Visual Status Chips */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Pipe Details Group */}
          <div
            className={`bg-cadslate-950/60 border rounded-lg p-3.5 space-y-2 transition ${
              effectiveMode === 'DRAINAGE' || targetTool === 'SEWSECTION'
                ? 'border-cadcyan-500/60 ring-1 ring-cadcyan-500/20'
                : 'border-cadslate-800/80'
            }`}
          >
            <div className="flex items-center justify-between text-xs font-semibold text-cadcyan-400">
              <span className="flex items-center space-x-1.5">
                <span className="w-2 h-2 rounded-full bg-cadcyan-400 inline-block"></span>
                <span>{targetTool === 'SEWSECTION' ? 'A. Sewer Pipe Segment Columns' : 'A. Drainage Design Columns'}</span>
              </span>
              <span className="text-[11px] text-slate-400 font-normal">
                {targetTool === 'SEWSECTION' ? 'Length, Pipe Size, Gradient, Pipe Type' : 'ID, Size, Length, Gradient'}
              </span>
            </div>

            <div className="grid grid-cols-2 gap-2 text-xs">
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">{targetTool === 'SEWSECTION' ? 'Material:' : 'ID:'}</span>
                {targetTool === 'SEWSECTION' ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-cadcyan-300">
                    {columnMap.pipeType ? `✓ ${columnMap.pipeType}` : 'Default: VCP'}
                  </span>
                ) : columnMap.drainId ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-cadcyan-300">
                    ✓ {columnMap.drainId}{columnMap.drainIdSub ? ` + ${columnMap.drainIdSub}` : ''}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">Size:</span>
                {columnMap.size ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-cadcyan-300">
                    ✓ {columnMap.size}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">Length:</span>
                {columnMap.length ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-cadcyan-300">
                    ✓ {columnMap.length}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">Gradient:</span>
                {columnMap.gradient ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-cadcyan-300">
                    ✓ {columnMap.gradient}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
            </div>
          </div>

          {/* Sewerage Manhole Group */}
          <div
            className={`bg-cadslate-950/60 border rounded-lg p-3.5 space-y-2 transition ${
              effectiveMode === 'SEWERAGE'
                ? 'border-cadcyan-500/60 ring-1 ring-cadcyan-500/20'
                : 'border-cadslate-800/80'
            }`}
          >
            <div className="flex items-center justify-between text-xs font-semibold text-cademerald-400">
              <span className="flex items-center space-x-1.5">
                <span className="w-2 h-2 rounded-full bg-emerald-400 inline-block"></span>
                <span>B. Sewerage Manhole &amp; Level Columns</span>
              </span>
              <span className="text-[11px] text-slate-400 font-normal">
                {targetTool === 'SEWSECTION' ? 'ID (MH/EXTG/STP), GL, IL, Backdrop' : 'ID (MH/EXTG/STP), GL, Lowest IL, Depth'}
              </span>
            </div>

            <div className="grid grid-cols-2 gap-2 text-xs">
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">MH ID:</span>
                {columnMap.sewerId ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-emerald-300">
                    ✓ {columnMap.sewerId}{columnMap.sewerIdSub ? ` + ${columnMap.sewerIdSub}` : ''}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">GL:</span>
                {columnMap.gl ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-emerald-300">
                    ✓ {columnMap.gl}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">IL:</span>
                {columnMap.il ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-emerald-300">
                    ✓ {columnMap.il}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Not Found</span>
                )}
              </div>
              <div className="bg-cadslate-900 border border-cadslate-800 rounded p-2 flex items-center justify-between">
                <span className="text-slate-400">{targetTool === 'SEWSECTION' ? 'Backdrop:' : 'Depth:'}</span>
                {targetTool === 'SEWSECTION' ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-emerald-300">
                    {columnMap.backdrop ? `✓ ${columnMap.backdrop}` : 'Auto (IL step)'}
                  </span>
                ) : columnMap.depth ? (
                  <span className="font-mono font-medium truncate ml-1 text-[11px] text-emerald-300">
                    ✓ {columnMap.depth}
                  </span>
                ) : (
                  <span className="text-amber-400 text-[11px] italic">Calc: (GL-IL)</span>
                )}
              </div>
            </div>
          </div>
        </div>

        {/* Collapsible Manual Mapping Selectors */}
        {showManualMapper && (
          <div className="pt-3 border-t border-cadslate-800 space-y-3">
            <p className="text-xs text-slate-400 font-medium">
              Manually select which Excel column corresponds to each engineering parameter:
            </p>
            <div className="grid grid-cols-2 sm:grid-cols-5 gap-3 text-xs">
              <div>
                <label className="block text-slate-400 mb-1">Drain ID</label>
                <select
                  value={columnMap.drainId}
                  onChange={(e) => setColumnMap({ ...columnMap, drainId: e.target.value, drainIdSub: '' })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-cadcyan-500 font-mono"
                >
                  <option value="">-- Auto/None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Manhole ID</label>
                <select
                  value={columnMap.sewerId}
                  onChange={(e) => setColumnMap({ ...columnMap, sewerId: e.target.value, sewerIdSub: '' })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- Auto/None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Drain ID + part 2</label>
                <select
                  value={columnMap.drainIdSub}
                  onChange={(e) => setColumnMap({ ...columnMap, drainIdSub: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-cadcyan-500 font-mono"
                >
                  <option value="">-- None (single column) --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Manhole ID + part 2</label>
                <select
                  value={columnMap.sewerIdSub}
                  onChange={(e) => setColumnMap({ ...columnMap, sewerIdSub: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- None (single column) --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Pipe Size</label>
                <select
                  value={columnMap.size}
                  onChange={(e) => setColumnMap({ ...columnMap, size: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-cadcyan-500 font-mono"
                >
                  <option value="">-- None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Length</label>
                <select
                  value={columnMap.length}
                  onChange={(e) => setColumnMap({ ...columnMap, length: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-cadcyan-500 font-mono"
                >
                  <option value="">-- None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Gradient</label>
                <select
                  value={columnMap.gradient}
                  onChange={(e) => setColumnMap({ ...columnMap, gradient: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-cadcyan-500 font-mono"
                >
                  <option value="">-- None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Ground Level (GL)</label>
                <select
                  value={columnMap.gl}
                  onChange={(e) => setColumnMap({ ...columnMap, gl: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Invert Level (IL)</label>
                <select
                  value={columnMap.il}
                  onChange={(e) => setColumnMap({ ...columnMap, il: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Depth</label>
                <select
                  value={columnMap.depth}
                  onChange={(e) => setColumnMap({ ...columnMap, depth: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- None / Compute --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Pipe Material (VCP/RCP)</label>
                <select
                  value={columnMap.pipeType}
                  onChange={(e) => setColumnMap({ ...columnMap, pipeType: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- Default (VCP) --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
              <div>
                <label className="block text-slate-400 mb-1">Backdrop (B/D IL)</label>
                <select
                  value={columnMap.backdrop}
                  onChange={(e) => setColumnMap({ ...columnMap, backdrop: e.target.value })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded p-1.5 text-white focus:border-emerald-500 font-mono"
                >
                  <option value="">-- Auto/None --</option>
                  {columns.map(c => <option key={c} value={c}>{c}</option>)}
                </select>
              </div>
            </div>
          </div>
        )}
      </div>
    </>
  );
}

window.DetectionPanel = DetectionPanel;
