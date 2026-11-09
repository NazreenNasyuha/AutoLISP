/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Modals & Toast Notification
 *   Includes SettingsModal, FeedbackModal, GuideModal, and Toast.
 * ============================================================================ */

function SettingsModal({ isOpen, onClose, settings, setSettings, showToast }) {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
      <div className="bg-cadslate-900 border border-cadslate-700 rounded-2xl max-w-md w-full p-6 shadow-2xl space-y-6">
        <div className="flex items-center justify-between border-b border-cadslate-800 pb-3">
          <div className="flex items-center space-x-2 text-white font-bold text-base">
            <svg className="w-5 h-5 text-cadcyan-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4" />
            </svg>
            <span>Formatting &amp; Profile Settings</span>
          </div>
          <button type="button" onClick={onClose} className="text-slate-400 hover:text-white p-1 rounded-lg">
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>

        <div className="space-y-4 text-xs">
          <div>
            <label className="block text-slate-300 font-semibold mb-1">
              CSVUPDATE Text Width Factor:{' '}
              <span className="text-cadcyan-400 font-mono font-bold">{settings.textWidthFactor}</span>
            </label>
            <input
              type="number"
              step="0.05"
              min="0.1"
              max="2.0"
              value={settings.textWidthFactor}
              onChange={(e) => setSettings({ ...settings, textWidthFactor: parseFloat(e.target.value) || 0.5 })}
              className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2.5 text-white font-mono focus:border-cadcyan-500"
            />
          </div>

          <div className="grid grid-cols-2 gap-3">
            <div>
              <label className="block text-slate-300 font-semibold mb-1">
                Short Pipe (m):{' '}
                <span className="text-cadcyan-400 font-mono font-bold">{settings.shortPipeThreshold}m</span>
              </label>
              <input
                type="number"
                step="0.5"
                value={settings.shortPipeThreshold}
                onChange={(e) => setSettings({ ...settings, shortPipeThreshold: parseFloat(e.target.value) || 6 })}
                className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2 text-white font-mono"
              />
            </div>
            <div>
              <label className="block text-slate-300 font-semibold mb-1">
                Medium Pipe (m):{' '}
                <span className="text-cadcyan-400 font-mono font-bold">{settings.mediumPipeThreshold}m</span>
              </label>
              <input
                type="number"
                step="0.5"
                value={settings.mediumPipeThreshold}
                onChange={(e) => setSettings({ ...settings, mediumPipeThreshold: parseFloat(e.target.value) || 14 })}
                className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2 text-white font-mono"
              />
            </div>
          </div>

          <div className="pt-2 border-t border-cadslate-800 space-y-3">
            <div className="font-bold text-cadcyan-400 uppercase tracking-wider text-[11px]">
              SEWSECTION Longitudinal Profile Defaults
            </div>
            <div className="grid grid-cols-2 gap-3">
              <div>
                <label className="block text-slate-300 font-semibold mb-1">Max Section Limit (m)</label>
                <input
                  type="number"
                  value={settings.secMaxDist}
                  onChange={(e) => setSettings({ ...settings, secMaxDist: parseFloat(e.target.value) || 200 })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2 text-white font-mono"
                />
              </div>
              <div>
                <label className="block text-slate-300 font-semibold mb-1">Base Datum if &lt; 10m (m)</label>
                <input
                  type="number"
                  value={settings.secBaseDatumUnder10}
                  onChange={(e) => setSettings({ ...settings, secBaseDatumUnder10: parseFloat(e.target.value) || 0 })}
                  className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2 text-white font-mono"
                />
              </div>
            </div>
          </div>
        </div>

        <div className="flex items-center justify-between pt-4 border-t border-cadslate-800">
          <button
            type="button"
            onClick={() => setSettings(DEFAULT_SETTINGS)}
            className="text-xs text-slate-400 hover:text-white"
          >
            Reset Defaults
          </button>
          <button
            type="button"
            onClick={() => {
              onClose();
              showToast('Settings saved!');
            }}
            className="px-4 py-2 bg-cadcyan-500 hover:bg-cadcyan-400 text-black font-bold rounded-lg text-xs transition"
          >
            Done &amp; Save
          </button>
        </div>
      </div>
    </div>
  );
}

function FeedbackModal({
  isOpen,
  onClose,
  feedbackType,
  setFeedbackType,
  feedbackText,
  setFeedbackText,
  sendFeedbackEmail,
}) {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
      <div className="bg-cadslate-900 border border-cadslate-700 rounded-2xl max-w-md w-full p-6 shadow-2xl space-y-5">
        <div className="flex items-center justify-between border-b border-cadslate-800 pb-3">
          <div className="flex items-center space-x-2 text-white font-bold text-base">
            <svg className="w-5 h-5 text-amber-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
            </svg>
            <span>Submit Feedback or Report a Bug</span>
          </div>
          <button type="button" onClick={onClose} className="text-slate-400 hover:text-white p-1 rounded-lg">
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>

        <div className="space-y-4 text-xs">
          <div>
            <label className="block text-slate-300 font-semibold mb-1">Issue Type</label>
            <select
              value={feedbackType}
              onChange={(e) => setFeedbackType(e.target.value)}
              className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2.5 text-white focus:border-amber-500"
            >
              <option>Bug Report</option>
              <option>Feature Request</option>
              <option>Excel Parsing Error</option>
            </select>
          </div>
          <div>
            <label className="block text-slate-300 font-semibold mb-1">Description</label>
            <textarea
              rows={6}
              value={feedbackText}
              onChange={(e) => setFeedbackText(e.target.value)}
              placeholder="Describe the Excel file, column, or AutoLISP command issue..."
              className="w-full bg-cadslate-950 border border-cadslate-700 rounded-lg p-2.5 text-white focus:border-amber-500 resize-y"
            />
          </div>
        </div>

        <div className="flex items-center justify-end space-x-3 pt-4 border-t border-cadslate-800">
          <button type="button" onClick={onClose} className="px-4 py-2 text-xs text-slate-400 hover:text-white">
            Cancel
          </button>
          <button
            type="button"
            onClick={sendFeedbackEmail}
            className="px-4 py-2 bg-amber-500 hover:bg-amber-400 text-black font-bold rounded-lg text-xs transition"
          >
            Send via Email
          </button>
        </div>
      </div>
    </div>
  );
}

function GuideModal({ isOpen, onClose }) {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm">
      <div className="bg-cadslate-900 border border-cadslate-700 rounded-2xl max-w-lg w-full p-6 shadow-2xl space-y-5">
        <div className="flex items-center justify-between border-b border-cadslate-800 pb-3">
          <div className="flex items-center space-x-2 text-white font-bold text-base">
            <svg className="w-5 h-5 text-cadcyan-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
            </svg>
            <span>AutoCAD / GstarCAD Workflow (CSVUPDATE &amp; SEWSECTION)</span>
          </div>
          <button type="button" onClick={onClose} className="text-slate-400 hover:text-white p-1 rounded-lg">
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>

        <div className="space-y-3 text-xs text-slate-300 leading-relaxed">
          <div className="p-3 rounded-lg bg-cadslate-950 border border-cadslate-800">
            <div className="font-bold text-cadcyan-400 mb-1">Workflow A: Plan View Updates (CSVUPDATE)</div>
            <p className="text-slate-400">
              1. Select <strong className="text-white">&quot;1. CSVUPDATE Workflow&quot;</strong> at the top.<br />
              2. Drop your Drainage or Sewerage Excel file and click{' '}
              <strong className="text-white">&quot;Export CSVUPDATE Raw .CSV&quot;</strong>.<br />
              3. In CAD, run <code className="text-cadcyan-400">CSVUPDATE</code>, select{' '}
              <code className="text-cadcyan-400">AutoCAD_Smart_Raw.csv</code>, and click a sample text on the target layer.
            </p>
          </div>
          <div className="p-3 rounded-lg bg-cadslate-950 border border-cadslate-800">
            <div className="font-bold text-cadcyan-400 mb-1">Workflow B: Sewerage Longitudinal Sections (SEWSECTION)</div>
            <p className="text-slate-400">
              1. Select <strong className="text-white">&quot;2. SEWSECTION Workflow&quot;</strong> at the top.<br />
              2. Drop your Sewerage calculation sheet, preview the 200m split sections, and click{' '}
              <strong className="text-white">&quot;Export SEWSECTION .CSV&quot;</strong>.<br />
              3. In CAD, run <code className="text-cadcyan-400">SEWSECTION</code> (or{' '}
              <code className="text-cadcyan-400">LONGSEC</code>), choose your scales (
              <code className="text-white">1:1000</code> Hor,{' '}
              <code className="text-white">1:200 / 1:500 / 1:1000</code> Ver), select{' '}
              <code className="text-cadcyan-400">AutoCAD_SewSection.csv</code>, and click an insertion point!
            </p>
          </div>
        </div>

        <div className="text-right pt-2 border-t border-cadslate-800">
          <button
            type="button"
            onClick={onClose}
            className="px-4 py-2 bg-cadcyan-500 hover:bg-cadcyan-400 text-black font-bold rounded-lg text-xs transition"
          >
            Got it
          </button>
        </div>
      </div>
    </div>
  );
}

function Toast({ toastMsg }) {
  if (!toastMsg) return null;

  return (
    <div className="fixed bottom-20 right-6 z-50 bg-cadcyan-500 text-black font-bold text-xs px-4 py-2.5 rounded-xl shadow-2xl flex items-center space-x-2">
      <svg className="w-4 h-4 stroke-[2.5]" fill="none" viewBox="0 0 24 24" stroke="currentColor">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" />
      </svg>
      <span>{toastMsg}</span>
    </div>
  );
}

window.SettingsModal = SettingsModal;
window.FeedbackModal = FeedbackModal;
window.GuideModal = GuideModal;
window.Toast = Toast;
