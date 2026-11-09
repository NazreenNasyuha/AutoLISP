/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — CSVUPDATE Plan View Table & Filter Bar
 * ============================================================================ */

function CsvUpdateView({
  targetTool,
  processedRows,
  filteredRows,
  stats,
  filterCategory,
  setFilterCategory,
  searchTerm,
  setSearchTerm,
  copyEntireCSV,
  copySingleString,
}) {
  if (targetTool !== 'CSVUPDATE' || !processedRows || processedRows.length === 0) return null;

  return (
    <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl overflow-hidden shadow-2xl space-y-4">
      {/* Statistics & Filters Bar */}
      <div className="p-4 bg-cadslate-850 border-b border-cadslate-800 flex flex-col md:flex-row items-stretch md:items-center justify-between gap-3">
        <div className="flex flex-wrap items-center gap-1.5 text-xs">
          <button
            type="button"
            onClick={() => setFilterCategory('ALL')}
            className={`px-3 py-1.5 rounded-lg font-semibold transition ${
              filterCategory === 'ALL'
                ? 'bg-cadcyan-500 text-black shadow'
                : 'bg-cadslate-800 text-slate-300 hover:bg-cadslate-750'
            }`}
          >
            All ({stats.total})
          </button>
          {stats.drainageCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('DRAINAGE')}
              className={`px-3 py-1.5 rounded-lg font-semibold transition ${
                filterCategory === 'DRAINAGE'
                  ? 'bg-cadcyan-500 text-black shadow'
                  : 'bg-cadslate-800 text-slate-300 hover:bg-cadslate-750'
              }`}
            >
              Drainage ({stats.drainageCount})
            </button>
          )}
          {stats.sewerageCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('SEWERAGE')}
              className={`px-3 py-1.5 rounded-lg font-semibold transition ${
                filterCategory === 'SEWERAGE'
                  ? 'bg-cadcyan-500 text-black shadow'
                  : 'bg-cadslate-800 text-slate-300 hover:bg-cadslate-750'
              }`}
            >
              Sewerage ({stats.sewerageCount})
            </button>
          )}
          {stats.mergedCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('MERGED')}
              className={`px-2.5 py-1.5 rounded-lg font-semibold transition ${
                filterCategory === 'MERGED'
                  ? 'bg-amber-500 text-black shadow'
                  : 'bg-amber-950/50 text-amber-300 border border-amber-500/30 hover:bg-amber-900/50'
              }`}
            >
              Lowest-IL Merged ({stats.mergedCount})
            </button>
          )}
          {stats.shortCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('SHORT')}
              className={`px-2.5 py-1.5 rounded-lg font-medium transition ${
                filterCategory === 'SHORT'
                  ? 'bg-cadcyan-600 text-white shadow'
                  : 'bg-cadslate-800 text-slate-400 hover:bg-cadslate-750'
              }`}
            >
              Short (4-Line): {stats.shortCount}
            </button>
          )}
          {stats.medCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('MEDIUM')}
              className={`px-2.5 py-1.5 rounded-lg font-medium transition ${
                filterCategory === 'MEDIUM'
                  ? 'bg-cadcyan-600 text-white shadow'
                  : 'bg-cadslate-800 text-slate-400 hover:bg-cadslate-750'
              }`}
            >
              Med (2-Line): {stats.medCount}
            </button>
          )}
          {stats.longCount > 0 && (
            <button
              type="button"
              onClick={() => setFilterCategory('LONG')}
              className={`px-2.5 py-1.5 rounded-lg font-medium transition ${
                filterCategory === 'LONG'
                  ? 'bg-cadcyan-600 text-white shadow'
                  : 'bg-cadslate-800 text-slate-400 hover:bg-cadslate-750'
              }`}
            >
              Long (1-Line): {stats.longCount}
            </button>
          )}
        </div>

        {/* Search Input & Quick Copy */}
        <div className="flex items-center space-x-2">
          <div className="relative flex-1 md:w-64">
            <input
              type="text"
              placeholder="Search ID (e.g. EXTG, STP, MH)..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg pl-8 pr-3 py-1.5 text-xs text-white placeholder-slate-500 focus:outline-none focus:border-cadcyan-500"
            />
            <svg className="w-4 h-4 text-slate-500 absolute left-2.5 top-2" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
            </svg>
          </div>

          <button
            type="button"
            onClick={copyEntireCSV}
            className="px-3 py-1.5 text-xs font-semibold text-slate-200 bg-cadslate-800 hover:bg-cadslate-750 border border-cadslate-700 rounded-lg transition flex items-center space-x-1.5 whitespace-nowrap"
          >
            <svg className="w-3.5 h-3.5 text-cadcyan-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M8 5H6a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2v-1M8 5a2 2 0 002 2h2a2 2 0 002-2M8 5a2 2 0 012-2h2a2 2 0 012 2m0 0h2a2 2 0 012 2v3m2 4H10m0 0l3-3m-3 3l3 3" />
            </svg>
            <span>Copy CSV</span>
          </button>
        </div>
      </div>

      {/* The Preview Table */}
      <div className="overflow-x-auto">
        <table className="w-full text-left text-xs border-collapse">
          <thead>
            <tr className="bg-cadslate-950 text-slate-400 border-b border-cadslate-800 uppercase tracking-wider font-semibold">
              <th className="py-3 px-4 w-12 text-center">#</th>
              <th className="py-3 px-4 w-28">Discipline</th>
              <th className="py-3 px-4 w-36">CAD ID</th>
              <th className="py-3 px-4 w-64">Extracted Parameters</th>
              <th className="py-3 px-4">Generated AutoCAD MTEXT String</th>
              <th className="py-3 px-4 w-52">Visual CAD Render</th>
              <th className="py-3 px-4 w-16 text-center">Copy</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-cadslate-800/60 font-mono">
            {filteredRows.map((row, idx) => (
              <tr key={idx} className="hover:bg-cadslate-800/40 transition">
                <td className="py-3 px-4 text-center text-slate-500 font-sans text-[11px]">
                  {idx + 1}
                </td>

                <td className="py-3 px-4 font-sans">
                  {row.category === 'SEWERAGE' ? (
                    <span className="inline-flex items-center px-2 py-0.5 rounded text-[10px] font-bold bg-emerald-500/15 text-emerald-400 border border-emerald-500/30">
                      Sewerage
                    </span>
                  ) : (
                    <span className="inline-flex items-center px-2 py-0.5 rounded text-[10px] font-bold bg-cadcyan-500/15 text-cadcyan-400 border border-cadcyan-500/30">
                      Drainage
                    </span>
                  )}
                </td>

                <td className="py-3 px-4 font-bold text-white tracking-wide">
                  <span className="bg-cadslate-950 px-2 py-1 rounded border border-cadslate-800 text-cadcyan-300">
                    {row.id}
                  </span>
                </td>

                <td className="py-3 px-4 font-sans text-xs">
                  {row.category === 'SEWERAGE' ? (
                    <div className="space-y-0.5 text-slate-300">
                      <div><span className="text-slate-500">GL:</span> {row.sewerDetails.gl || '—'}m</div>
                      <div><span className="text-slate-500">Lowest IL:</span> <strong className="text-cadcyan-400">{row.sewerDetails.il || '—'}m</strong></div>
                      <div><span className="text-slate-500">Depth:</span> {row.sewerDetails.depth || '—'}m</div>
                    </div>
                  ) : (
                    <div className="space-y-0.5 text-slate-300">
                      <div><span className="text-slate-500">Dia:</span> {row.drainDetails.size || '—'}%%c</div>
                      <div><span className="text-slate-500">Len:</span> {row.drainDetails.length || '—'}m</div>
                      <div>
                        <span className="text-slate-500">Slope:</span> 1:{row.drainDetails.gradient || '—'}
                        {row.drainDetails.il && (
                          <span className="ml-2 text-[11px] text-cadcyan-400">(Min IL: {row.drainDetails.il}m)</span>
                        )}
                      </div>
                    </div>
                  )}
                  <div className={`text-[10px] italic mt-1 font-mono ${row.duplicateCount > 1 ? 'text-amber-400 font-semibold' : 'text-slate-400'}`}>
                    {row.conditionBadge}
                  </div>
                </td>

                <td className="py-3 px-4">
                  <div className="bg-cadslate-950 text-cadcyan-300 px-3 py-2 rounded-lg border border-cadslate-800 text-[11px] break-all select-all shadow-inner">
                    {row.cadString}
                  </div>
                </td>

                <td className="py-3 px-4">
                  <div className="cad-preview-box p-2.5 rounded-lg border border-cadslate-800 text-xs shadow-inner leading-relaxed font-sans">
                    {row.visualLines.map((line, lidx) => (
                      <div key={lidx} className="cad-preview-line whitespace-nowrap truncate font-medium text-slate-100">
                        {line}
                      </div>
                    ))}
                  </div>
                </td>

                <td className="py-3 px-4 text-center">
                  <button
                    type="button"
                    onClick={() => copySingleString(row.cadString)}
                    className="p-1.5 rounded-md hover:bg-cadslate-750 text-slate-400 hover:text-white transition"
                    title="Copy CAD string"
                  >
                    <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                    </svg>
                  </button>
                </td>
              </tr>
            ))}
          </tbody>
        </table>

        {filteredRows.length === 0 && (
          <div className="text-center py-12 text-slate-500 font-sans text-sm">
            No records match the active search or category filter.
          </div>
        )}
      </div>
    </div>
  );
}

window.CsvUpdateView = CsvUpdateView;
