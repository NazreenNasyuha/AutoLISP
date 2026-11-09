/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Upload Dropzone & Worksheet Tabs
 * ============================================================================ */

function UploadZone({
  isDragging,
  setIsDragging,
  fileInputRef,
  handleFile,
  loadDemo,
  downloadTemplate,
  fileInfo,
  rawRows,
  targetTool,
  sewSections,
  stats,
  processedRows,
  activeSheet,
  setActiveSheet,
  setActiveSecIdx,
  parseSheet,
  workbook,
}) {
  return (
    <>
      {/* 3. MASSIVE DRAG-AND-DROP UPLOAD ZONE */}
      <div
        onDragOver={(e) => {
          e.preventDefault();
          setIsDragging(true);
        }}
        onDragLeave={() => setIsDragging(false)}
        onDrop={(e) => {
          e.preventDefault();
          setIsDragging(false);
          if (e.dataTransfer.files && e.dataTransfer.files[0]) {
            handleFile(e.dataTransfer.files[0]);
          }
        }}
        className={`relative border-2 border-dashed rounded-2xl p-8 sm:p-10 text-center transition duration-200 group cursor-pointer ${
          isDragging
            ? 'border-cadcyan-400 bg-cadcyan-950/30 scale-[1.005]'
            : 'border-cadslate-700 hover:border-cadcyan-500/70 bg-cadslate-900/70 hover:bg-cadslate-900'
        }`}
        onClick={() => fileInputRef.current && fileInputRef.current.click()}
      >
        <input
          ref={fileInputRef}
          type="file"
          accept=".xlsx, .xls, .xlsm, .csv"
          className="hidden"
          onChange={(e) => {
            if (e.target.files && e.target.files[0]) {
              handleFile(e.target.files[0]);
            }
          }}
        />

        <div className="max-w-xl mx-auto space-y-4">
          <div className="w-16 h-16 mx-auto rounded-2xl bg-cadslate-800 border border-cadslate-700 flex items-center justify-center text-cadcyan-400 group-hover:scale-110 group-hover:border-cadcyan-500/60 transition duration-200 shadow-xl">
            <svg className="w-8 h-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12" />
            </svg>
          </div>

          <div>
            <h3 className="text-lg font-bold text-white group-hover:text-cadcyan-400 transition">
              Drag &amp; Drop JRK Engineering Excel Spreadsheet
            </h3>
            <p className="text-sm text-slate-400 mt-1">
              Supports <span className="text-cadcyan-400 font-mono font-semibold">.xlsx</span>,{' '}
              <span className="text-cadcyan-400 font-mono font-semibold">.xls</span>, and{' '}
              <span className="text-cadcyan-400 font-mono font-semibold">.xlsm</span> calculation sheets for{' '}
              <strong className="text-white">CSVUPDATE</strong> &amp; <strong className="text-white">SEWSECTION</strong>
            </p>
          </div>

          <div className="flex flex-wrap items-center justify-center gap-2 pt-1 text-xs text-slate-400">
            <span className="inline-flex items-center px-2.5 py-1 rounded-md bg-cadslate-800 border border-cadslate-700">
              ⚡ Detects EXTG, STP, OUTFALL &amp; CROSSING
            </span>
            <span className="inline-flex items-center px-2.5 py-1 rounded-md bg-cadslate-800 border border-cadslate-700">
              📉 Lowest Invert Level Deduplication
            </span>
            <span className="inline-flex items-center px-2.5 py-1 rounded-md bg-cadslate-800 border border-cadslate-700">
              📐 200m Long. Section Auto-Splitter
            </span>
          </div>

          {/* Instant 1-Click Demo Buttons */}
          <div
            className="pt-4 border-t border-cadslate-800/80 flex flex-wrap items-center justify-center gap-2"
            onClick={(e) => e.stopPropagation()}
          >
            <span className="text-xs text-slate-400 font-medium mr-1">Quick Test:</span>
            <button
              type="button"
              onClick={() => loadDemo('drainage')}
              className="px-2.5 py-1 text-xs font-semibold text-cadcyan-300 bg-cadcyan-950/50 hover:bg-cadcyan-900/60 border border-cadcyan-500/30 rounded-md transition"
            >
              Sample Drainage (CSVUPDATE)
            </button>
            <button
              type="button"
              onClick={() => loadDemo('sewerage')}
              className="px-2.5 py-1 text-xs font-semibold text-cademerald-400 bg-emerald-950/50 hover:bg-emerald-900/60 border border-emerald-500/30 rounded-md transition"
            >
              Sample Sewerage (EXTG &amp; STP)
            </button>
            <button
              type="button"
              onClick={() => loadDemo('sewsection')}
              className="px-2.5 py-1 text-xs font-semibold text-fuchsia-300 bg-fuchsia-950/50 hover:bg-fuchsia-900/60 border border-fuchsia-500/30 rounded-md transition"
            >
              Sample Long. Section (SEWSECTION)
            </button>
            <button
              type="button"
              onClick={downloadTemplate}
              className="px-2.5 py-1 text-xs font-medium text-slate-300 bg-cadslate-800 hover:bg-cadslate-750 border border-cadslate-700 rounded-md transition flex items-center space-x-1"
            >
              <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
              </svg>
              <span>Download Template</span>
            </button>
          </div>
        </div>
      </div>

      {/* ACTIVE SPREADSHEET TABS & METADATA */}
      {fileInfo && (
        <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl p-4 flex flex-col md:flex-row items-start md:items-center justify-between gap-3 shadow-lg">
          <div className="flex items-center space-x-3">
            <div className="w-9 h-9 rounded-lg bg-cadcyan-500/15 border border-cadcyan-500/30 text-cadcyan-400 flex items-center justify-center">
              <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
              </svg>
            </div>
            <div>
              <div className="flex items-center space-x-2">
                <span className="text-sm font-bold text-white">{fileInfo.name}</span>
                <span className="text-xs text-slate-400 font-mono">({fileInfo.size})</span>
              </div>
              <div className="text-xs text-slate-400">
                Found {rawRows.length} raw rows •{' '}
                {targetTool === 'SEWSECTION'
                  ? `Built ${sewSections.length} Longitudinal Section(s) (${stats.totalSecNodes} Manhole/EXTG/STP nodes)`
                  : `Extracted ${processedRows.length} unique CAD items for CSVUPDATE`}
              </div>
            </div>
          </div>

          {fileInfo.sheetNames && fileInfo.sheetNames.length > 1 && (
            <div className="flex flex-wrap items-center gap-1.5 text-xs">
              <span className="text-slate-400 mr-1">Worksheet:</span>
              <div className="flex flex-wrap bg-cadslate-950 p-1 rounded-lg border border-cadslate-800 gap-1">
                {fileInfo.sheetNames.map((sn) => (
                  <button
                    key={sn}
                    type="button"
                    onClick={() => {
                      setActiveSheet(sn);
                      setActiveSecIdx(0);
                      parseSheet(workbook, sn, fileInfo.name);
                    }}
                    className={`px-2.5 py-1 rounded-md transition font-semibold ${
                      activeSheet === sn
                        ? 'bg-cadcyan-500 text-black shadow'
                        : 'text-slate-400 hover:text-white'
                    }`}
                  >
                    {sn}
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>
      )}
    </>
  );
}

window.UploadZone = UploadZone;
