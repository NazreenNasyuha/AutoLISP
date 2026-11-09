/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Workflow & Parameter Bar
 * ============================================================================ */

function WorkflowBar({
  targetTool,
  setTargetTool,
  userDesignMode,
  setUserDesignMode,
  detectedMode,
  detectionReason,
  settings,
  setSettings,
}) {
  return (
    <>
      {/* 2. TARGET AUTOCAD LISP COMMAND SELECTOR (CSVUPDATE vs SEWSECTION) */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {/* Card 1: CSVUPDATE */}
        <div
          onClick={() => setTargetTool('CSVUPDATE')}
          className={`cursor-pointer rounded-2xl p-4 border-2 transition flex items-start space-x-3.5 shadow-lg ${
            targetTool === 'CSVUPDATE'
              ? 'border-cadcyan-500 bg-cadcyan-950/30 ring-2 ring-cadcyan-500/20'
              : 'border-cadslate-800 bg-cadslate-900 hover:border-cadslate-700'
          }`}
        >
          <div
            className={`w-11 h-11 rounded-xl flex items-center justify-center flex-shrink-0 font-mono font-bold text-sm ${
              targetTool === 'CSVUPDATE'
                ? 'bg-cadcyan-500 text-black shadow-md'
                : 'bg-cadslate-800 text-slate-400 border border-cadslate-700'
            }`}
          >
            CSV
          </div>
          <div className="flex-1 min-w-0">
            <div className="flex items-center justify-between">
              <span className="text-sm font-bold text-white flex items-center space-x-2">
                <span>1. CSVUPDATE Workflow</span>
                <span className="text-[10px] font-mono px-2 py-0.5 rounded bg-cadslate-800 text-cadcyan-400 border border-cadslate-700">
                  Plan View Labels
                </span>
              </span>
              {targetTool === 'CSVUPDATE' && (
                <span className="text-[10px] font-bold uppercase px-2 py-0.5 rounded-full bg-cadcyan-500 text-black">
                  Active
                </span>
              )}
            </div>
            <p className="text-xs text-slate-400 mt-1 leading-relaxed">
              Batch updates <strong className="text-white">Road Drainage</strong> &amp;{' '}
              <strong className="text-white">Sewerage Manhole</strong> text/multileaders in Plan View (including{' '}
              <strong className="text-cadcyan-400">EXTG, STP, OUTFALL, CROSSING</strong>). Merges duplicate IDs using the{' '}
              <strong className="text-cadcyan-400">Lowest Invert Level</strong>.
            </p>
          </div>
        </div>

        {/* Card 2: SEWSECTION */}
        <div
          onClick={() => setTargetTool('SEWSECTION')}
          className={`cursor-pointer rounded-2xl p-4 border-2 transition flex items-start space-x-3.5 shadow-lg ${
            targetTool === 'SEWSECTION'
              ? 'border-cadcyan-500 bg-cadcyan-950/30 ring-2 ring-cadcyan-500/20'
              : 'border-cadslate-800 bg-cadslate-900 hover:border-cadslate-700'
          }`}
        >
          <div
            className={`w-11 h-11 rounded-xl flex items-center justify-center flex-shrink-0 font-mono font-bold text-sm ${
              targetTool === 'SEWSECTION'
                ? 'bg-cadcyan-500 text-black shadow-md'
                : 'bg-cadslate-800 text-slate-400 border border-cadslate-700'
            }`}
          >
            SEC
          </div>
          <div className="flex-1 min-w-0">
            <div className="flex items-center justify-between">
              <span className="text-sm font-bold text-white flex items-center space-x-2">
                <span>2. SEWSECTION Workflow</span>
                <span className="text-[10px] font-mono px-2 py-0.5 rounded bg-cadslate-800 text-cadcyan-400 border border-cadslate-700">
                  Longitudinal Section
                </span>
              </span>
              {targetTool === 'SEWSECTION' && (
                <span className="text-[10px] font-bold uppercase px-2 py-0.5 rounded-full bg-cadcyan-500 text-black">
                  Active
                </span>
              )}
            </div>
            <p className="text-xs text-slate-400 mt-1 leading-relaxed">
              Generates <strong className="text-white">Sewerage Longitudinal Section</strong> profiles (
              <code className="text-cadcyan-400">LONG. SECTION OF MH...</code>) including{' '}
              <strong className="text-cadcyan-400">EXTG &amp; STP</strong> nodes,{' '}
              <strong className="text-white">200m Distance Limit</strong> splitting, and{' '}
              <strong className="text-cadcyan-400">Auto +10m Datum</strong>.
            </p>
          </div>
        </div>
      </div>

      {/* 2B. DISCIPLINE / LONG-SECTION SETTINGS BAR */}
      {targetTool === 'CSVUPDATE' ? (
        <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl p-4 flex flex-col lg:flex-row items-start lg:items-center justify-between gap-4 shadow-lg">
          <div>
            <div className="text-xs font-bold uppercase tracking-wider text-cadcyan-400 flex items-center space-x-2">
              <span>CSVUPDATE Engineering Design Mode</span>
              <span className="text-[11px] font-normal text-slate-400">
                (
                {userDesignMode === 'AUTO'
                  ? `Smart Auto-Detected: ${detectedMode} • ${detectionReason}`
                  : `Manually Locked: ${userDesignMode}`}
                )
              </span>
            </div>
            <p className="text-xs text-slate-400 mt-0.5">
              Choose whether this spreadsheet is for <strong className="text-white">Road Drainage Design</strong> or{' '}
              <strong className="text-white">Sewerage Design</strong>. Special tags (
              <strong className="text-cadcyan-400">EXTG, STP, OUTFALL, CROSSING, SUMP</strong>) in any row are
              automatically detected and included in the ID.
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-1.5 bg-cadslate-950 p-1.5 rounded-xl border border-cadslate-800 w-full lg:w-auto">
            <button
              type="button"
              onClick={() => setUserDesignMode('AUTO')}
              className={`flex-1 lg:flex-initial px-3 py-1.5 rounded-lg text-xs font-bold transition ${
                userDesignMode === 'AUTO'
                  ? 'bg-cadcyan-500 text-black shadow-md'
                  : 'text-slate-300 hover:text-white hover:bg-cadslate-850'
              }`}
            >
              ⚡ Smart Auto ({detectedMode === 'UNKNOWN' ? 'not sure' : detectedMode})
            </button>
            <button
              type="button"
              onClick={() => setUserDesignMode('DRAINAGE')}
              className={`flex-1 lg:flex-initial px-3 py-1.5 rounded-lg text-xs font-bold transition ${
                userDesignMode === 'DRAINAGE'
                  ? 'bg-cadcyan-500 text-black shadow-md'
                  : 'text-slate-300 hover:text-white hover:bg-cadslate-850'
              }`}
            >
              🌊 Drainage Design
            </button>
            <button
              type="button"
              onClick={() => setUserDesignMode('SEWERAGE')}
              className={`flex-1 lg:flex-initial px-3 py-1.5 rounded-lg text-xs font-bold transition ${
                userDesignMode === 'SEWERAGE'
                  ? 'bg-cadcyan-500 text-black shadow-md'
                  : 'text-slate-300 hover:text-white hover:bg-cadslate-850'
              }`}
            >
              🕳️ Sewerage Design
            </button>
            <button
              type="button"
              onClick={() => setUserDesignMode('HYBRID')}
              className={`flex-1 lg:flex-initial px-3 py-1.5 rounded-lg text-xs font-bold transition ${
                userDesignMode === 'HYBRID'
                  ? 'bg-cadcyan-500 text-black shadow-md'
                  : 'text-slate-300 hover:text-white hover:bg-cadslate-850'
              }`}
            >
              🔀 Dual / Mixed
            </button>
          </div>
        </div>
      ) : (
        <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl p-4 flex flex-col lg:flex-row items-start lg:items-center justify-between gap-4 shadow-lg">
          <div>
            <div className="text-xs font-bold uppercase tracking-wider text-cadcyan-400 flex items-center space-x-2">
              <span>SEWSECTION Longitudinal Profile Parameters</span>
              <span className="text-[11px] font-normal text-slate-400">
                (HOR 1:{settings.secHorScale} • VER 1:{settings.secVerScale} • Limit: {settings.secMaxDist}m •{' '}
                {settings.secAutoDatum ? 'Auto +10m Datum' : `Manual Datum ${settings.secManualDatum}m`})
              </span>
            </div>
            <p className="text-xs text-slate-400 mt-0.5">
              Configure scales, <strong className="text-white">200m distance splitting</strong>, and{' '}
              <strong className="text-cadcyan-400">Automatic +10m Datum</strong>. Automatically detects and includes{' '}
              <strong className="text-cadcyan-400">EXTG, STP, FMH &amp; PS</strong> nodes in the section chain.
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-2 text-xs w-full lg:w-auto">
            <div className="flex items-center space-x-1.5 bg-cadslate-950 px-2.5 py-1.5 rounded-lg border border-cadslate-800">
              <span className="text-slate-400">HOR:</span>
              <select
                value={settings.secHorScale}
                onChange={(e) => setSettings({ ...settings, secHorScale: parseInt(e.target.value, 10) || 1000 })}
                className="bg-transparent text-white font-mono font-bold focus:outline-none"
              >
                <option value={200} className="bg-cadslate-900">1:200</option>
                <option value={500} className="bg-cadslate-900">1:500</option>
                <option value={1000} className="bg-cadslate-900">1:1000</option>
                <option value={1500} className="bg-cadslate-900">1:1500</option>
                <option value={2000} className="bg-cadslate-900">1:2000</option>
              </select>
            </div>

            <div className="flex items-center space-x-1 bg-cadslate-950 p-1 rounded-lg border border-cadslate-800">
              <span className="text-slate-400 px-1.5">VER:</span>
              {[200, 500, 1000].map((v) => (
                <button
                  key={v}
                  type="button"
                  onClick={() => setSettings({ ...settings, secVerScale: v })}
                  className={`px-2 py-1 rounded font-mono font-bold transition ${
                    settings.secVerScale === v
                      ? 'bg-cadcyan-500 text-black'
                      : 'text-slate-300 hover:text-white'
                  }`}
                >
                  1:{v}
                </button>
              ))}
            </div>

            <div className="flex items-center space-x-1.5 bg-cadslate-950 px-2.5 py-1.5 rounded-lg border border-cadslate-800">
              <span className="text-slate-400">Max Dist:</span>
              <input
                type="number"
                value={settings.secMaxDist}
                onChange={(e) => setSettings({ ...settings, secMaxDist: parseFloat(e.target.value) || 200 })}
                className="w-14 bg-transparent text-cadcyan-400 font-mono font-bold focus:outline-none text-right"
              />
              <span className="text-slate-400">m</span>
            </div>

            <button
              type="button"
              onClick={() => setSettings({ ...settings, secAutoDatum: !settings.secAutoDatum })}
              className={`px-2.5 py-1.5 rounded-lg font-bold border transition ${
                settings.secAutoDatum
                  ? 'bg-cadcyan-500/15 border-cadcyan-500/40 text-cadcyan-300'
                  : 'bg-cadslate-950 border-cadslate-800 text-slate-400'
              }`}
            >
              {settings.secAutoDatum ? '✓ Auto Datum (+10m)' : 'Manual Datum'}
            </button>
          </div>
        </div>
      )}
    </>
  );
}

window.WorkflowBar = WorkflowBar;
