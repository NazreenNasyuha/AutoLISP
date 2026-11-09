/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — CSV & Template Exporters
 *   Exports formatted data for AutoCAD CSVUPDATE (5-col Raw & 2-col MTEXT),
 *   SEWSECTION (9-col Longitudinal Profile), and Excel Schedule Templates.
 * ============================================================================ */

function triggerCsvDownload(csvContent, filename) {
  const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.setAttribute('href', url);
  link.setAttribute('download', filename);
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

// 1. 5-Column Raw CSV Exporter for CSVUPDATE (csvupdate.lsp)
function exportRawCSV(processedRows, showToast) {
  if (!processedRows || processedRows.length === 0) {
    alert('No data to export.');
    return;
  }

  const csvLines = processedRows.map(item => {
    const id = escapeCSV(item.id);
    const category = escapeCSV(item.category);
    let val1, val2, val3;
    if (item.category === 'SEWERAGE') {
      val1 = escapeCSV(item.sewerDetails.gl || '00.00');
      val2 = escapeCSV(item.sewerDetails.il || '00.00');
      val3 = escapeCSV(item.sewerDetails.depth || '00.00');
    } else {
      val1 = escapeCSV(item.drainDetails.size || '600');
      val2 = escapeCSV(item.drainDetails.length || '0');
      val3 = escapeCSV(item.drainDetails.gradient || '1000');
    }
    return `${id},${val1},${val2},${val3},${category}`;
  });

  triggerCsvDownload(csvLines.join('\r\n'), 'AutoCAD_Smart_Raw.csv');
  if (showToast) showToast('Exported "AutoCAD_Smart_Raw.csv" for CSVUPDATE!');
}

// 2. 2-Column MTEXT CSV Exporter
function exportMtextCSVFile(processedRows, showToast) {
  if (!processedRows || processedRows.length === 0) {
    alert('No data to export.');
    return;
  }

  const csvLines = processedRows.map(item => {
    const col1 = escapeCSV(item.id);
    const col2 = escapeCSV(item.cadString);
    return `${col1},${col2}`;
  });

  triggerCsvDownload(csvLines.join('\r\n'), 'AutoCAD_Smart_Import.csv');
  if (showToast) showToast('Exported "AutoCAD_Smart_Import.csv" (2-column MTEXT)!');
}

// 3. Dedicated Exporter for SEWSECTION (sewsection.lsp)
function exportSewSectionCSVFile(sewSections, showToast) {
  if (!sewSections || sewSections.length === 0) {
    alert('No valid Sewerage Longitudinal Section chains found to export.');
    return;
  }

  const header = 'Manhole No.,Length,Pipe Size,Gradient,Pipe Type,Ground Level,Invert Level,Depth of Manhole,Backdrop';
  const lines = [header];

  sewSections.forEach((sec) => {
    sec.nodes.forEach((node, nIdx) => {
      const isLast = nIdx === sec.nodes.length - 1;
      const nextNode = !isLast ? sec.nodes[nIdx + 1] : null;
      const segLen = !isLast ? (nextNode.dist - node.dist) : '';
      const bdDiff = node.ilIn - node.ilOut;
      const bdStr = bdDiff > 0.005 ? bdDiff.toFixed(2) : '';
      const depthVal = (node.gl - node.ilOut).toFixed(2);

      lines.push([
        escapeCSV(node.id),
        escapeCSV(segLen !== '' ? Number(segLen.toFixed(2)) : ''),
        escapeCSV(!isLast ? node.size : ''),
        escapeCSV(!isLast ? node.grad : ''),
        escapeCSV(!isLast ? node.mat : ''),
        escapeCSV(node.gl.toFixed(2)),
        escapeCSV(node.ilOut.toFixed(2)),
        escapeCSV(depthVal),
        escapeCSV(bdStr),
      ].join(','));
    });
  });

  triggerCsvDownload(lines.join('\r\n'), 'AutoCAD_SewSection.csv');
  if (showToast) {
    showToast(`Exported "AutoCAD_SewSection.csv" (${sewSections.length} Longitudinal Section(s))!`);
  }
}

// 4. Download Sample Excel Schedule Template
function downloadTemplateWorkbook(showToast) {
  const wb = XLSX.utils.book_new();
  const wsDrain = XLSX.utils.json_to_sheet(DEMO_DRAINAGE_DATA);
  const wsSewer = XLSX.utils.json_to_sheet(DEMO_SEWERAGE_DATA);
  XLSX.utils.book_append_sheet(wb, wsDrain, 'Drainage_Pipes');
  XLSX.utils.book_append_sheet(wb, wsSewer, 'Sewer_LongSection');
  XLSX.writeFile(wb, 'JRK_AutoCAD_Smart_Schedule_Template.xlsx');
  if (showToast) showToast('Downloaded: JRK_AutoCAD_Smart_Schedule_Template.xlsx');
}

// 5. Clipboard Helpers
function copyAllRowsToClipboard(processedRows, showToast) {
  if (!processedRows || processedRows.length === 0) return;
  const csvContent = processedRows
    .map(item => `${escapeCSV(item.id)},${escapeCSV(item.cadString)}`)
    .join('\r\n');
  navigator.clipboard.writeText(csvContent).then(() => {
    if (showToast) showToast('Copied 2-column CSV directly to Clipboard!');
  });
}

function copyTextToClipboard(str, showToast) {
  navigator.clipboard.writeText(str).then(() => {
    if (showToast) showToast('Copied CAD string to Clipboard!');
  });
}

window.exportRawCSV = exportRawCSV;
window.exportMtextCSVFile = exportMtextCSVFile;
window.exportSewSectionCSVFile = exportSewSectionCSVFile;
window.downloadTemplateWorkbook = downloadTemplateWorkbook;
window.copyAllRowsToClipboard = copyAllRowsToClipboard;
window.copyTextToClipboard = copyTextToClipboard;
