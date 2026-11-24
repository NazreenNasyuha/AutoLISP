/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Constants & Sample Datasets
 * ============================================================================ */

const STORAGE_KEY = 'autocad_smart_bridge_settings';
const THEME_KEY = 'autocad_smart_bridge_theme';

const DEFAULT_SETTINGS = {
  textWidthFactor: 0.5,
  shortPipeThreshold: 6,
  mediumPipeThreshold: 14,
  smartLengthFormatting: true,
  cleanGradientPrefix: true,
  // Hydraulic & Civil QA/QC settings (SPAN MSIG / MSMA)
  minSewerVelocity: 0.8,
  minDrainVelocity: 0.6,
  maxPipeVelocity: 4.0,
  minCoverDepth: 1.2,
  // SEWSECTION settings
  secHorScale: 1000,
  secVerScale: 200,
  secMaxDist: 200,
  secAutoDatum: true,
  secBaseDatumUnder10: 2,
  secManualDatum: 0,
  secSplitMode: 'CROSS', // 'CROSS' = split at MH >= 200m, 'STRICT' = split before exceeding 200m
};

function sanitizeSettings(raw) {
  if (!raw || typeof raw !== 'object') return { ...DEFAULT_SETTINGS };
  const num = (val, fallback, min, max) => {
    const n = parseFloat(val);
    if (!Number.isFinite(n)) return fallback;
    return Math.min(max, Math.max(min, n));
  };
  return {
    textWidthFactor: num(raw.textWidthFactor, DEFAULT_SETTINGS.textWidthFactor, 0.1, 2.0),
    shortPipeThreshold: num(raw.shortPipeThreshold, DEFAULT_SETTINGS.shortPipeThreshold, 0.5, 100),
    mediumPipeThreshold: num(raw.mediumPipeThreshold, DEFAULT_SETTINGS.mediumPipeThreshold, 1.0, 200),
    smartLengthFormatting: typeof raw.smartLengthFormatting === 'boolean' ? raw.smartLengthFormatting : DEFAULT_SETTINGS.smartLengthFormatting,
    cleanGradientPrefix: typeof raw.cleanGradientPrefix === 'boolean' ? raw.cleanGradientPrefix : DEFAULT_SETTINGS.cleanGradientPrefix,
    minSewerVelocity: num(raw.minSewerVelocity, DEFAULT_SETTINGS.minSewerVelocity, 0.1, 3.0),
    minDrainVelocity: num(raw.minDrainVelocity, DEFAULT_SETTINGS.minDrainVelocity, 0.1, 3.0),
    maxPipeVelocity: num(raw.maxPipeVelocity, DEFAULT_SETTINGS.maxPipeVelocity, 1.0, 15.0),
    minCoverDepth: num(raw.minCoverDepth, DEFAULT_SETTINGS.minCoverDepth, 0.3, 10.0),
    secHorScale: [200, 500, 1000, 1500, 2000].includes(Number(raw.secHorScale)) ? Number(raw.secHorScale) : DEFAULT_SETTINGS.secHorScale,
    secVerScale: [200, 500, 1000].includes(Number(raw.secVerScale)) ? Number(raw.secVerScale) : DEFAULT_SETTINGS.secVerScale,
    secMaxDist: num(raw.secMaxDist, DEFAULT_SETTINGS.secMaxDist, 10, 5000),
    secAutoDatum: typeof raw.secAutoDatum === 'boolean' ? raw.secAutoDatum : DEFAULT_SETTINGS.secAutoDatum,
    secBaseDatumUnder10: num(raw.secBaseDatumUnder10, DEFAULT_SETTINGS.secBaseDatumUnder10, -50, 50),
    secManualDatum: num(raw.secManualDatum, DEFAULT_SETTINGS.secManualDatum, -100, 1000),
    secSplitMode: raw.secSplitMode === 'STRICT' ? 'STRICT' : 'CROSS',
  };
}

// Realistic Demo Datasets for 1-Click Testing (Includes EXTG, STP & CROSSING examples)
const DEMO_DRAINAGE_DATA = [
  { 'Drain Name': '1A', 'U-Drain Width / Ø': '600', 'Length': 4.5, 'Grade': '1000', 'Invert Level (Upstream)': '8.70', 'Invert Level (Downstream)': '8.55', 'Remarks': '' },
  { 'Drain Name': '1B', 'U-Drain Width / Ø': '600', 'Length': 6.0, 'Grade': '1000', 'Invert Level (Upstream)': '8.55', 'Invert Level (Downstream)': '8.40', 'Remarks': '' },
  { 'Drain Name': '1B', 'U-Drain Width / Ø': '600', 'Length': 10.0, 'Grade': '800', 'Invert Level (Upstream)': '8.40', 'Invert Level (Downstream)': '8.15', 'Remarks': '' },
  { 'Drain Name': '1C', 'U-Drain Width / Ø': '900', 'Length': 12.0, 'Grade': '800', 'Invert Level (Upstream)': '8.15', 'Invert Level (Downstream)': '7.95', 'Remarks': '' },
  { 'Drain Name': 'CROSSING 1', 'U-Drain Width / Ø': '1200', 'Length': 14.0, 'Grade': '500', 'Invert Level (Upstream)': '7.95', 'Invert Level (Downstream)': '7.70', 'Remarks': '' },
  { 'Drain Name': '1E', 'U-Drain Width / Ø': '1200', 'Length': 22.5, 'Grade': '500', 'Invert Level (Upstream)': '7.70', 'Invert Level (Downstream)': '7.40', 'Remarks': 'EXTG SUMP' },
  { 'Drain Name': 'OUTFALL 1', 'U-Drain Width / Ø': '1500', 'Length': 30.0, 'Grade': '400', 'Invert Level (Upstream)': '7.40', 'Invert Level (Downstream)': '7.05', 'Remarks': 'EXTG' },
];

const DEMO_SEWERAGE_DATA = [
  { 'Manhole No.': 'MH1', 'Type of Building': 'Shoplot', 'Length': '65', 'Pipe Size': '300', 'Gradient': '294', 'Pipe Type': 'VCP', 'Ground Level': '6.60', 'Invert Level': '4.74', 'Depth of Manhole': '1.86', 'Backdrop': '' },
  { 'Manhole No.': 'MH2', 'Type of Building': 'Shoplot', 'Length': '65', 'Pipe Size': '300', 'Gradient': '294', 'Pipe Type': 'VCP', 'Ground Level': '6.75', 'Invert Level': '4.52', 'Depth of Manhole': '2.23', 'Backdrop': '' },
  { 'Manhole No.': 'MH3', 'Type of Building': 'Shoplot', 'Length': '65', 'Pipe Size': '300', 'Gradient': '294', 'Pipe Type': 'VCP', 'Ground Level': '6.65', 'Invert Level': '4.30', 'Depth of Manhole': '2.35', 'Backdrop': '' },
  { 'Manhole No.': 'MH4', 'Type of Building': 'Shoplot', 'Length': '27', 'Pipe Size': '225', 'Gradient': '204', 'Pipe Type': 'VCP', 'Ground Level': '6.50', 'Invert Level': '4.08', 'Depth of Manhole': '2.42', 'Backdrop': '' },
  { 'Manhole No.': 'EXTG MH8', 'Type of Building': 'EXTG', 'Length': '', 'Pipe Size': '', 'Gradient': '', 'Pipe Type': '', 'Ground Level': '6.40', 'Invert Level': '3.62', 'Depth of Manhole': '2.78', 'Backdrop': '0.33' },
  { 'Manhole No.': 'MH21', 'Type of Building': 'Terrace', 'Length': '55', 'Pipe Size': '300', 'Gradient': '294', 'Pipe Type': 'VCP', 'Ground Level': '16.00', 'Invert Level': '14.12', 'Depth of Manhole': '1.88', 'Backdrop': '' },
  { 'Manhole No.': 'MH26', 'Type of Building': 'Terrace', 'Length': '9',  'Pipe Size': '375', 'Gradient': '400', 'Pipe Type': 'VCP', 'Ground Level': '15.80', 'Invert Level': '13.93', 'Depth of Manhole': '1.87', 'Backdrop': '' },
  { 'Manhole No.': 'MH27', 'Type of Building': 'Terrace', 'Length': '39', 'Pipe Size': '375', 'Gradient': '294', 'Pipe Type': 'VCP', 'Ground Level': '15.80', 'Invert Level': '13.91', 'Depth of Manhole': '1.89', 'Backdrop': '' },
  { 'Manhole No.': 'MH28', 'Type of Building': 'Terrace', 'Length': '12', 'Pipe Size': '375', 'Gradient': '400', 'Pipe Type': 'VCP', 'Ground Level': '15.70', 'Invert Level': '13.78', 'Depth of Manhole': '1.92', 'Backdrop': '' },
  { 'Manhole No.': 'MH30', 'Type of Building': 'Terrace', 'Length': '19', 'Pipe Size': '375', 'Gradient': '400', 'Pipe Type': 'VCP', 'Ground Level': '15.70', 'Invert Level': '13.75', 'Depth of Manhole': '1.95', 'Backdrop': '' },
  { 'Manhole No.': 'STP',  'Type of Building': 'STP',     'Length': '',   'Pipe Size': '',    'Gradient': '',    'Pipe Type': '',    'Ground Level': '15.55', 'Invert Level': '13.70', 'Depth of Manhole': '1.85', 'Backdrop': '' },
];

const DEMO_MIXED_DATA = [
  { 'ID': 'D01', 'Size': '600', 'Length': '5.2', 'Gradient': '1000', 'GL': '', 'IL': '7.32', 'Depth': '' },
  { 'ID': 'EXTG D02', 'Size': '900', 'Length': '10.5', 'Gradient': '800', 'GL': '', 'IL': '7.15', 'Depth': '' },
  { 'ID': 'OUTFALL', 'Size': '1200', 'Length': '24.0', 'Gradient': '500', 'GL': '', 'IL': '6.90', 'Depth': '' },
  { 'ID': 'MH-01', 'Size': '', 'Length': '', 'Gradient': '', 'GL': '42.10', 'IL': '39.80', 'Depth': '2.30' },
  { 'ID': 'EXTG MH-02', 'Size': '', 'Length': '', 'Gradient': '', 'GL': '41.80', 'IL': '39.00', 'Depth': '2.80' },
  { 'ID': 'STP', 'Size': '', 'Length': '', 'Gradient': '', 'GL': '41.20', 'IL': '38.00', 'Depth': '3.20' },
];

function escapeCSV(val) {
  if (val === null || val === undefined) return '';
  let str = String(val);
  // Prevent CSV Formula Injection for non-numeric strings starting with =, +, -, @, \t, \r
  if (/^[=+\-@\t\r]/.test(str) && !/^[+-]?\d+(\.\d+)?$/.test(str.trim())) {
    str = `'${str}`;
  }
  if (str.includes(',') || str.includes('"') || str.includes('\n') || str.includes('\r')) {
    return `"${str.replace(/"/g, '""')}"`;
  }
  return str;
}

window.STORAGE_KEY = STORAGE_KEY;
window.THEME_KEY = THEME_KEY;
window.DEFAULT_SETTINGS = DEFAULT_SETTINGS;
window.sanitizeSettings = sanitizeSettings;
window.DEMO_DRAINAGE_DATA = DEMO_DRAINAGE_DATA;
window.DEMO_SEWERAGE_DATA = DEMO_SEWERAGE_DATA;
window.DEMO_MIXED_DATA = DEMO_MIXED_DATA;
window.escapeCSV = escapeCSV;
