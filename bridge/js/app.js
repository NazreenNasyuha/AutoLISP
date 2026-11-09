/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Main Application Controller
 * ============================================================================ */

const { useState, useEffect, useMemo, useRef } = React;

function App() {
  // 0. Theme State (Dark Mode is Default)
  const [theme, setTheme] = useState(() => {
    try {
      return localStorage.getItem(THEME_KEY) || 'dark';
    } catch (e) {
      return 'dark';
    }
  });

  useEffect(() => {
    const rootEl = document.documentElement;
    if (theme === 'light') {
      rootEl.classList.remove('dark');
      rootEl.classList.add('light');
    } else {
      rootEl.classList.remove('light');
      rootEl.classList.add('dark');
    }
    try {
      localStorage.setItem(THEME_KEY, theme);
    } catch (e) {}
  }, [theme]);

  // 1. Target AutoCAD LISP Tool State ('CSVUPDATE' or 'SEWSECTION')
  const [targetTool, setTargetTool] = useState('CSVUPDATE');
  const [activeSecIdx, setActiveSecIdx] = useState(0);

  // 2. Settings state
  const [settings, setSettings] = useState(() => {
    try {
      const saved = localStorage.getItem(STORAGE_KEY);
      return saved ? { ...DEFAULT_SETTINGS, ...JSON.parse(saved) } : DEFAULT_SETTINGS;
    } catch (e) {
      return DEFAULT_SETTINGS;
    }
  });

  useEffect(() => {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(settings));
    } catch (e) {}
  }, [settings]);

  // 3. Modals & Notifications
  const [isSettingsOpen, setIsSettingsOpen] = useState(false);
  const [isGuideOpen, setIsGuideOpen] = useState(false);
  const [showManualMapper, setShowManualMapper] = useState(false);
  const [isFeedbackOpen, setIsFeedbackOpen] = useState(false);
  const [feedbackType, setFeedbackType] = useState('Bug Report');
  const [feedbackText, setFeedbackText] = useState('');
  const [toastMsg, setToastMsg] = useState(null);

  const showToast = (msg) => {
    setToastMsg(msg);
    setTimeout(() => setToastMsg(null), 3500);
  };

  const sendFeedbackEmail = () => {
    const to = 'nazreen@jrksb.com';
    const subject = `[JRK CAD Bridge] ${feedbackType}`;
    window.location.href = `mailto:${to}?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(feedbackText)}`;
    setIsFeedbackOpen(false);
  };

  // 4. Excel Parsing & Smart Detection States
  const [fileInfo, setFileInfo] = useState(null);
  const [activeSheet, setActiveSheet] = useState('');
  const [workbook, setWorkbook] = useState(null);
  const [rawRows, setRawRows] = useState([]);
  const [columns, setColumns] = useState([]);

  const [columnMap, setColumnMap] = useState({
    drainId: '',
    drainIdSub: '',
    size: '',
    length: '',
    gradient: '',
    drainIlUp: '',
    drainIlDown: '',
    sewerId: '',
    sewerIdSub: '',
    gl: '',
    il: '',
    depth: '',
    pipeType: '',
    backdrop: '',
  });

  const [detectedMode, setDetectedMode] = useState('UNKNOWN');
  const [detectionConfidence, setDetectionConfidence] = useState('NONE');
  const [detectionScores, setDetectionScores] = useState({ sew: 0, drn: 0 });
  const [detectionNotes, setDetectionNotes] = useState([]);
  const [idInfo, setIdInfo] = useState({ kind: 'none', mainKey: '', subKey: '', samples: [] });
  const [userDesignMode, setUserDesignMode] = useState('AUTO');
  const [detectionReason, setDetectionReason] = useState('Ready for upload');

  const effectiveMode = targetTool === 'SEWSECTION'
    ? 'SEWERAGE'
    : (userDesignMode === 'AUTO' ? detectedMode : userDesignMode);

  // 5. UI Search & Filter
  const [searchTerm, setSearchTerm] = useState('');
  const [filterCategory, setFilterCategory] = useState('ALL');
  const [isDragging, setIsDragging] = useState(false);
  const fileInputRef = useRef(null);

  // 6. Smart Discipline Classifier & Column Detector
  const detectColumns = (headerList, sampleRows = [], metaText = '', headerInfo = null) => {
    const res = seAnalyzeTable({ headers: headerList, rows: sampleRows, headerInfo, metaText });
    const notes = [...res.notes];

    if (res.mode === 'DRAINAGE' && targetTool === 'SEWSECTION') {
      setTargetTool('CSVUPDATE');
      notes.push('This is a drainage file and Long Section is sewerage-only, so the tool was switched to CSVUPDATE automatically.');
    }

    setColumnMap(res.mapping);
    setDetectedMode(res.mode);
    setDetectionConfidence(res.confidence);
    setDetectionScores(res.scores);
    setDetectionNotes(notes);
    setIdInfo(res.idInfo);
    setDetectionReason(res.reasons.length > 0 ? res.reasons.join(' • ') : `Auto-detected ${res.mode}`);
    return res;
  };

  // 7. Excel File & Worksheet Parsing via SheetJS
  const parseSheet = (wb, sheetName, fileNameOverride = '') => {
    const ws = wb.Sheets[sheetName];
    if (!ws) return null;

    const table = seBuildTableFromSheet(ws, {
      sheetName,
      fileName: fileNameOverride || (fileInfo ? fileInfo.name : ''),
    });

    if (!table || table.rows.length === 0) {
      showToast(`Warning: Sheet "${sheetName}" is empty.`);
      setRawRows([]);
      setColumns([]);
      return null;
    }

    setColumns(table.headers);
    setRawRows(table.rows);
    setUserDesignMode('AUTO');
    return detectColumns(table.headers, table.rows, table.metaText, table.headerInfo);
  };

  const handleFile = (file) => {
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const data = new Uint8Array(e.target.result);
        const wb = XLSX.read(data, { type: 'array' });

        let bestSheet = wb.SheetNames[0];
        let bestScore = -1;
        for (const sn of wb.SheetNames) {
          if (/^(dni|copypaste|cover|instruction)/i.test(sn.trim())) continue;
          const previewArr = XLSX.utils.sheet_to_json(wb.Sheets[sn], { header: 1, defval: '', raw: false }).slice(0, 45);
          const score = previewArr.reduce((mx, r) => Math.max(mx, seHeaderRowScore(r.map(seText).filter(Boolean))), 0);
          if (score > bestScore) {
            bestScore = score;
            bestSheet = sn;
          }
        }

        setWorkbook(wb);
        setFileInfo({
          name: file.name,
          size: (file.size / 1024).toFixed(1) + ' KB',
          sheetNames: wb.SheetNames,
        });

        setActiveSheet(bestSheet);
        setActiveSecIdx(0);
        const res = parseSheet(wb, bestSheet, file.name);
        const verdict = res
          ? (res.mode === 'UNKNOWN' ? 'please choose Drainage or Sewerage' : `detected ${res.mode}`)
          : 'no table found';
        showToast(`Loaded "${file.name}" (${bestSheet}) - ${verdict}`);
      } catch (err) {
        console.error('Error parsing Excel file:', err);
        alert('Failed to parse Excel file. Please ensure it is a valid .xls, .xlsx, or .xlsm file.');
      }
    };
    reader.readAsArrayBuffer(file);
  };

  // 8. Demo Loaders
  const loadDemo = (type) => {
    let data, name, headers;
    if (type === 'drainage') {
      data = DEMO_DRAINAGE_DATA;
      name = 'MSMA_Road_Drainage_Schedule.xlsx';
      headers = Object.keys(DEMO_DRAINAGE_DATA[0]);
    } else if (type === 'sewerage' || type === 'sewsection') {
      data = DEMO_SEWERAGE_DATA;
      name = 'JRK_Sewerage_LongSection_Calculation.xlsx';
      headers = Object.keys(DEMO_SEWERAGE_DATA[0]);
      if (type === 'sewsection') {
        setTargetTool('SEWSECTION');
      }
    } else {
      data = DEMO_MIXED_DATA;
      name = 'Infrastructure_Combined_Schedule.xlsx';
      headers = Object.keys(DEMO_MIXED_DATA[0]);
    }

    setActiveSecIdx(0);
    setUserDesignMode('AUTO');
    setColumns(headers);
    setRawRows(data);
    setFileInfo({
      name: `${name} (Sample)`,
      size: '14.8 KB',
      sheetNames: ['Schedule'],
    });
    setActiveSheet('Schedule');
    detectColumns(headers, data, name);
    showToast(`Loaded sample ${type} schedule!`);
  };

  // 9. Memoized CAD Processing & Longitudinal Section Chains
  const processedRows = useMemo(
    () => processAndDeduplicateRows(rawRows, { effectiveMode, columnMap, columns, settings }),
    [rawRows, columnMap, columns, settings, effectiveMode]
  );

  const sewSections = useMemo(
    () => buildSewSections(rawRows, { columnMap, columns, settings }),
    [rawRows, columnMap, columns, settings]
  );

  const filteredRows = useMemo(
    () => filterCadRows(processedRows, { filterCategory, searchTerm }),
    [processedRows, filterCategory, searchTerm]
  );

  const stats = useMemo(
    () => computeCadStats(processedRows, sewSections),
    [processedRows, sewSections]
  );

  // 10. Render Layout
  return (
    <div className="flex flex-col min-h-screen">
      <Header
        theme={theme}
        setTheme={setTheme}
        setIsGuideOpen={setIsGuideOpen}
        setIsFeedbackOpen={setIsFeedbackOpen}
        setIsSettingsOpen={setIsSettingsOpen}
      />

      <main className="flex-1 max-w-7xl w-full mx-auto px-4 sm:px-6 lg:px-8 py-6 space-y-6">
        <WorkflowBar
          targetTool={targetTool}
          setTargetTool={setTargetTool}
          userDesignMode={userDesignMode}
          setUserDesignMode={setUserDesignMode}
          detectedMode={detectedMode}
          detectionReason={detectionReason}
          settings={settings}
          setSettings={setSettings}
        />

        <UploadZone
          isDragging={isDragging}
          setIsDragging={setIsDragging}
          fileInputRef={fileInputRef}
          handleFile={handleFile}
          loadDemo={loadDemo}
          downloadTemplate={() => downloadTemplateWorkbook(showToast)}
          fileInfo={fileInfo}
          rawRows={rawRows}
          targetTool={targetTool}
          sewSections={sewSections}
          stats={stats}
          processedRows={processedRows}
          activeSheet={activeSheet}
          setActiveSheet={setActiveSheet}
          setActiveSecIdx={setActiveSecIdx}
          parseSheet={parseSheet}
          workbook={workbook}
        />

        <DetectionPanel
          rawRows={rawRows}
          userDesignMode={userDesignMode}
          setUserDesignMode={setUserDesignMode}
          detectedMode={detectedMode}
          detectionConfidence={detectionConfidence}
          detectionReason={detectionReason}
          detectionScores={detectionScores}
          detectionNotes={detectionNotes}
          idInfo={idInfo}
          targetTool={targetTool}
          setTargetTool={setTargetTool}
          effectiveMode={effectiveMode}
          columnMap={columnMap}
          setColumnMap={setColumnMap}
          columns={columns}
          processedRows={processedRows}
          sewSections={sewSections}
          showManualMapper={showManualMapper}
          setShowManualMapper={setShowManualMapper}
        />

        <SewSectionView
          targetTool={targetTool}
          rawRows={rawRows}
          sewSections={sewSections}
          activeSecIdx={activeSecIdx}
          setActiveSecIdx={setActiveSecIdx}
          settings={settings}
          loadDemo={loadDemo}
        />

        <CsvUpdateView
          targetTool={targetTool}
          processedRows={processedRows}
          filteredRows={filteredRows}
          stats={stats}
          filterCategory={filterCategory}
          setFilterCategory={setFilterCategory}
          searchTerm={searchTerm}
          setSearchTerm={setSearchTerm}
          copyEntireCSV={() => copyAllRowsToClipboard(processedRows, showToast)}
          copySingleString={(str) => copyTextToClipboard(str, showToast)}
        />
      </main>

      <Footer
        targetTool={targetTool}
        setTargetTool={setTargetTool}
        sewSections={sewSections}
        processedRows={processedRows}
        effectiveMode={effectiveMode}
        stats={stats}
        exportSewSectionCSV={() => exportSewSectionCSVFile(sewSections, showToast)}
        exportMtextCSV={() => exportMtextCSVFile(processedRows, showToast)}
        exportCSV={() => exportRawCSV(processedRows, showToast)}
      />

      <SettingsModal
        isOpen={isSettingsOpen}
        onClose={() => setIsSettingsOpen(false)}
        settings={settings}
        setSettings={setSettings}
        showToast={showToast}
      />

      <FeedbackModal
        isOpen={isFeedbackOpen}
        onClose={() => setIsFeedbackOpen(false)}
        feedbackType={feedbackType}
        setFeedbackType={setFeedbackType}
        feedbackText={feedbackText}
        setFeedbackText={setFeedbackText}
        sendFeedbackEmail={sendFeedbackEmail}
      />

      <GuideModal
        isOpen={isGuideOpen}
        onClose={() => setIsGuideOpen(false)}
      />

      <Toast toastMsg={toastMsg} />
    </div>
  );
}

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(<App />);
