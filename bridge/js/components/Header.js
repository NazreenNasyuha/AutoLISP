/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Header Navigation Bar
 * ============================================================================ */

function Header({
  theme,
  setTheme,
  setIsGuideOpen,
  setIsFeedbackOpen,
  setIsSettingsOpen,
}) {
  return (
    <header className="border-b border-cadslate-800 bg-cadslate-900/95 backdrop-blur sticky top-0 z-40 transition-colors">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 h-16 flex items-center justify-between gap-2">
        <div className="flex items-center space-x-3.5">
          <div>
            <div className="flex items-center space-x-2">
              <h1 className="text-sm sm:text-base font-bold text-white tracking-tight">
                AutoCAD Smart XLS-to-CSV Bridge
              </h1>
              <span className="text-[10px] uppercase font-bold tracking-wider px-2 py-0.5 rounded-full bg-cadcyan-500/15 text-cadcyan-400 border border-cadcyan-500/30">
                v3.1 JRK
              </span>
            </div>
            <p className="text-[11px] text-slate-400 hidden sm:block">
              ODI Infrastructure Suite • CSVUPDATE (Plan Labels) &amp; SEWSECTION (Longitudinal Sections)
            </p>
          </div>
        </div>

        {/* Action Buttons & Dark/Light Mode Toggle */}
        <div className="flex items-center space-x-2 sm:space-x-2.5">
          <button
            type="button"
            onClick={() => setTheme(theme === 'dark' ? 'light' : 'dark')}
            className="inline-flex items-center space-x-1.5 px-3 py-1.5 text-xs font-semibold text-slate-200 bg-cadslate-800 hover:bg-cadslate-750 rounded-lg border border-cadslate-700 transition"
            title={theme === 'dark' ? 'Switch to Light Mode' : 'Switch to Night (Dark) Mode'}
          >
            {theme === 'dark' ? (
              <>
                <svg className="w-4 h-4 text-amber-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z" />
                </svg>
                <span className="hidden md:inline">Light Mode</span>
              </>
            ) : (
              <>
                <svg className="w-4 h-4 text-cadcyan-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z" />
                </svg>
                <span className="hidden md:inline">Night Mode</span>
              </>
            )}
          </button>

          <button
            type="button"
            onClick={() => setIsGuideOpen(true)}
            className="hidden lg:inline-flex items-center space-x-1.5 px-3 py-1.5 text-xs font-medium text-slate-300 hover:text-white bg-cadslate-800 hover:bg-cadslate-750 rounded-lg border border-cadslate-700 transition"
          >
            <svg className="w-4 h-4 text-cadcyan-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
            <span>CAD LISP Guide</span>
          </button>

          <button
            type="button"
            onClick={() => setIsFeedbackOpen(true)}
            className="inline-flex items-center space-x-1.5 px-3 py-1.5 text-xs font-semibold text-amber-300 bg-amber-950/50 hover:bg-amber-900/50 border border-amber-500/40 rounded-lg shadow-sm transition"
          >
            <svg className="w-4 h-4 text-amber-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
            </svg>
            <span className="hidden sm:inline">Report Bug</span>
          </button>

          <button
            type="button"
            onClick={() => setIsSettingsOpen(true)}
            className="inline-flex items-center space-x-1.5 px-3.5 py-1.5 text-xs font-semibold text-cadcyan-300 bg-cadcyan-950/60 hover:bg-cadcyan-900/60 border border-cadcyan-500/40 rounded-lg shadow-sm transition"
          >
            <svg className="w-4 h-4 text-cadcyan-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
            </svg>
            <span className="hidden sm:inline">Settings</span>
          </button>
        </div>
      </div>
    </header>
  );
}

window.Header = Header;
