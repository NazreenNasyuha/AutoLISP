/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Sticky Export Footer
 * ============================================================================ */

function Footer({
  targetTool,
  setTargetTool,
  sewSections,
  processedRows,
  effectiveMode,
  stats,
  exportSewSectionCSV,
  exportMtextCSV,
  exportCSV,
}) {
  return (
    <footer className="border-t border-cadslate-800 bg-cadslate-900/95 backdrop-blur p-4 sticky bottom-0 z-40 transition-colors">
      <div className="max-w-7xl mx-auto flex flex-col sm:flex-row items-center justify-between gap-4">
        <div className="text-xs text-slate-400 flex items-center space-x-2">
          <span className="w-2.5 h-2.5 rounded-full bg-cadcyan-500 animate-pulse"></span>
          <span>
            {targetTool === 'SEWSECTION'
              ? sewSections.length > 0
                ? `SEWSECTION Mode: Ready to export ${sewSections.length} Longitudinal Section(s) (${stats.totalSecNodes} Manhole/EXTG/STP nodes)`
                : 'SEWSECTION Mode: Upload a Sewerage calculation spreadsheet or load sample'
              : processedRows.length > 0
              ? `CSVUPDATE Mode: Ready to export ${processedRows.length} unique ${effectiveMode} items (Lowest IL applied to ${stats.mergedCount} duplicate IDs)`
              : 'CSVUPDATE Mode: Awaiting spreadsheet upload or sample selection'}
          </span>
        </div>

        <div className="flex flex-wrap items-center gap-3 w-full sm:w-auto justify-end">
          {targetTool === 'SEWSECTION' ? (
            <>
              <button
                type="button"
                onClick={() => setTargetTool('CSVUPDATE')}
                className="inline-flex items-center justify-center space-x-1.5 px-4 py-2.5 rounded-xl font-medium text-xs border border-cadslate-700 bg-cadslate-800 hover:bg-cadslate-750 text-slate-200 transition"
              >
                <span>Switch to CSVUPDATE</span>
              </button>

              <button
                type="button"
                onClick={exportSewSectionCSV}
                disabled={sewSections.length === 0}
                className={`w-full sm:w-auto inline-flex items-center justify-center space-x-2 px-6 py-3 rounded-xl font-bold text-sm tracking-wide shadow-xl transition ${
                  sewSections.length > 0
                    ? 'bg-gradient-to-r from-[#00b050] to-[#009142] hover:from-[#00c85b] hover:to-[#00b050] text-black shadow-cadcyan-500/20 cursor-pointer scale-100 hover:scale-[1.02]'
                    : 'bg-cadslate-800 text-slate-500 cursor-not-allowed'
                }`}
              >
                <svg className="w-5 h-5 stroke-[2.5]" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                </svg>
                <span>Export SEWSECTION .CSV (Longitudinal Profile)</span>
              </button>
            </>
          ) : (
            <>
              {sewSections.length > 0 && (
                <button
                  type="button"
                  onClick={exportSewSectionCSV}
                  className="inline-flex items-center justify-center space-x-1.5 px-3.5 py-2.5 rounded-xl font-semibold text-xs border border-cadcyan-500/40 bg-cadcyan-950/50 hover:bg-cadcyan-900/60 text-cadcyan-300 transition"
                  title="Also export Longitudinal Section CSV for SEWSECTION"
                >
                  <span>Export SEWSECTION .CSV</span>
                </button>
              )}

              <button
                type="button"
                onClick={exportMtextCSV}
                disabled={processedRows.length === 0}
                className={`inline-flex items-center justify-center space-x-2 px-4 py-2.5 rounded-xl font-medium text-xs tracking-wide transition border ${
                  processedRows.length > 0
                    ? 'border-cadslate-700 bg-cadslate-800 hover:bg-cadslate-750 text-slate-200 hover:text-white cursor-pointer'
                    : 'border-cadslate-800 bg-cadslate-900 text-slate-500 cursor-not-allowed'
                }`}
              >
                <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
                <span>Export 2-Col MTEXT (.CSV)</span>
              </button>

              <button
                type="button"
                onClick={exportCSV}
                disabled={processedRows.length === 0}
                className={`w-full sm:w-auto inline-flex items-center justify-center space-x-2 px-6 py-3 rounded-xl font-bold text-sm tracking-wide shadow-xl transition ${
                  processedRows.length > 0
                    ? 'bg-gradient-to-r from-[#00b050] to-[#009142] hover:from-[#00c85b] hover:to-[#00b050] text-black shadow-cadcyan-500/20 cursor-pointer scale-100 hover:scale-[1.02]'
                    : 'bg-cadslate-800 text-slate-500 cursor-not-allowed'
                }`}
              >
                <svg className="w-5 h-5 stroke-[2.5]" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                </svg>
                <span>Export CSVUPDATE Raw .CSV (5-Column)</span>
              </button>
            </>
          )}
        </div>
      </div>
    </footer>
  );
}

window.Footer = Footer;
