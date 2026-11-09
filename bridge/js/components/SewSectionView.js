/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — SEWSECTION Longitudinal Profile View
 * ============================================================================ */

function SewSectionView({
  targetTool,
  rawRows,
  sewSections,
  activeSecIdx,
  setActiveSecIdx,
  settings,
  loadDemo,
}) {
  if (targetTool !== 'SEWSECTION' || !rawRows || rawRows.length === 0) return null;

  const currentPreviewSection = sewSections[activeSecIdx] || sewSections[0] || null;

  return (
    <div className="space-y-6">
      {sewSections.length === 0 ? (
        <div className="bg-cadslate-900 border border-amber-500/40 rounded-xl p-8 text-center space-y-3">
          <div className="text-sm font-bold text-amber-400">
            No Sewerage Manhole Chains Detected in Current Sheet
          </div>
          <p className="text-xs text-slate-400 max-w-lg mx-auto">
            To generate a Longitudinal Section (<code className="text-cadcyan-400">SEWSECTION</code>), the spreadsheet
            needs Manhole/EXTG/STP IDs, Pipe Lengths, Ground Levels (GL), and Invert Levels (IL).
          </p>
          <button
            type="button"
            onClick={() => loadDemo('sewsection')}
            className="px-4 py-2 bg-cadcyan-500 hover:bg-cadcyan-400 text-black font-bold rounded-lg text-xs transition"
          >
            Load Sample Sewerage Long. Section
          </button>
        </div>
      ) : (
        <div className="bg-cadslate-900 border border-cadslate-800 rounded-xl overflow-hidden shadow-2xl space-y-5 p-5">
          {/* Section Tabs Bar */}
          <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-3 border-b border-cadslate-800 pb-4">
            <div>
              <h3 className="text-sm font-bold text-white flex items-center space-x-2">
                <span>Generated Longitudinal Sections ({sewSections.length})</span>
                <span className="text-[11px] font-mono px-2 py-0.5 rounded bg-cadcyan-500/15 text-cadcyan-400 border border-cadcyan-500/30">
                  Max {settings.secMaxDist}m Split
                </span>
              </h3>
              <p className="text-xs text-slate-400 mt-0.5">
                Click any section tab below to inspect its CAD profile preview, Auto Datum, and manhole/EXTG/STP chainage table.
              </p>
            </div>

            <div className="flex flex-wrap gap-1.5">
              {sewSections.map((sec, idx) => (
                <button
                  key={idx}
                  type="button"
                  onClick={() => setActiveSecIdx(idx)}
                  className={`px-3 py-1.5 rounded-lg text-xs font-mono font-bold transition ${
                    activeSecIdx === idx
                      ? 'bg-cadcyan-500 text-black shadow-md'
                      : 'bg-cadslate-950 text-slate-300 border border-cadslate-800 hover:border-cadcyan-500/40'
                  }`}
                >
                  Sec {sec.index}: {sec.nodes[0].id}→{sec.nodes[sec.nodes.length - 1].id} ({sec.totalLength.toFixed(0)}m)
                </button>
              ))}
            </div>
          </div>

          {/* Visual AutoCAD Longitudinal Section Canvas Simulation */}
          {currentPreviewSection && (() => {
            const sec = currentPreviewSection;
            const datum = sec.datum;
            const maxGL = Math.max(...sec.nodes.map(n => n.gl));
            const elevSpan = Math.max(4.0, (maxGL - datum) + 2.0);
            const totalDist = Math.max(10.0, sec.totalLength);

            const svgW = 920;
            const svgH = 340;
            const leftHdrW = 195;
            const plotW = svgW - leftHdrW - 35;
            const yDatum = 165;
            const plotTop = 35;
            const yScale = (yDatum - plotTop - 15) / elevSpan;

            const xAt = (d) => leftHdrW + (d / totalDist) * plotW;
            const yAt = (elev) => yDatum - (elev - datum) * yScale;

            return (
              <div className="space-y-4">
                <div className="cad-preview-box rounded-xl border border-cadslate-800 p-4 overflow-x-auto shadow-inner">
                  <svg viewBox={`0 0 ${svgW} ${svgH}`} className="w-full min-w-[740px] h-auto select-none font-mono">
                    {/* Datum & Table Horizontal Lines (Layer TITLE = White) */}
                    <line x1="12" y1={yDatum} x2={leftHdrW + plotW} y2={yDatum} stroke="#f8fafc" strokeWidth="1.2" />
                    <line x1="12" y1={yDatum + 30} x2={leftHdrW + plotW} y2={yDatum + 30} stroke="#f8fafc" strokeWidth="1.2" />
                    <line x1="12" y1={yDatum + 60} x2={leftHdrW + plotW} y2={yDatum + 60} stroke="#f8fafc" strokeWidth="1.2" />
                    <line x1="12" y1={yDatum + 90} x2={leftHdrW + plotW} y2={yDatum + 90} stroke="#f8fafc" strokeWidth="1.2" />

                    {/* Left Table Labels */}
                    <text x="16" y={yDatum - 6} fill="#f8fafc" fontSize="10" fontWeight="bold">
                      DATUM {Number.isInteger(datum) ? datum : datum.toFixed(1)}m
                    </text>
                    <text x="16" y={yDatum + 13} fill="#f8fafc" fontSize="8.5">MANHOLES INVERT LEVEL</text>
                    <text x="16" y={yDatum + 24} fill="#f8fafc" fontSize="8.5">IN METERS</text>
                    <line x1="145" y1={yDatum + 15} x2="185" y2={yDatum + 15} stroke="#ff00ff" strokeWidth="1.5" />

                    <text x="16" y={yDatum + 43} fill="#f8fafc" fontSize="8.5">PROPOSED ROAD LEVEL</text>
                    <text x="16" y={yDatum + 54} fill="#f8fafc" fontSize="8.5">IN METERS</text>
                    <line x1="145" y1={yDatum + 45} x2="185" y2={yDatum + 45} stroke="#facc15" strokeWidth="1.5" strokeDasharray="5,3" />

                    <text x="16" y={yDatum + 78} fill="#f8fafc" fontSize="8.5">DISTANCE IN METERS</text>

                    {/* Profile Lines & Manhole Verticals */}
                    {sec.nodes.map((node, i) => {
                      const x = xAt(node.dist);
                      const yGl = yAt(node.gl);
                      const yIlIn = yAt(node.ilIn);
                      const yIlOut = yAt(node.ilOut);
                      const next = i < sec.nodes.length - 1 ? sec.nodes[i + 1] : null;
                      const xNext = next ? xAt(next.dist) : null;
                      const yGlNext = next ? yAt(next.gl) : null;
                      const yIlNextIn = next ? yAt(next.ilIn) : null;

                      return (
                        <g key={i}>
                          <line x1={x} y1={yDatum + 90} x2={x} y2="20" stroke="#f8fafc" strokeWidth="1" />
                          <line x1={x - 2.5} y1={yIlOut} x2={x - 2.5} y2={yGl} stroke="#f8fafc" strokeWidth="1" />
                          <line x1={x + 2.5} y1={yIlOut} x2={x + 2.5} y2={yGl} stroke="#f8fafc" strokeWidth="1" />

                          <text
                            x={x - 4}
                            y="42"
                            fill="#ff00ff"
                            fontSize="9"
                            fontWeight="bold"
                            transform={`rotate(-90 ${x - 4} 42)`}
                          >
                            {node.id}
                          </text>

                          {i > 0 && (node.ilIn - node.ilOut) > 0.01 && (
                            <line x1={x} y1={yIlIn} x2={x} y2={yIlOut} stroke="#ff00ff" strokeWidth="1.8" />
                          )}

                          {next && (
                            <>
                              <line x1={x} y1={yIlOut} x2={xNext} y2={yIlNextIn} stroke="#ff00ff" strokeWidth="1.6" />
                              <line x1={x} y1={yGl} x2={xNext} y2={yGlNext} stroke="#facc15" strokeWidth="1.5" strokeDasharray="6,4" />
                              <text
                                x={(x + xNext) / 2}
                                y={yDatum + 18}
                                fill="#f8fafc"
                                fontSize="8"
                                textAnchor="middle"
                              >
                                {`${node.size}Ø ${node.mat} 1:${node.grad}`}
                              </text>
                            </>
                          )}

                          <text
                            x={x + 8}
                            y={yDatum + 24}
                            fill="#f8fafc"
                            fontSize="7.5"
                            transform={`rotate(-90 ${x + 8} ${yDatum + 24})`}
                          >
                            {node.ilOut.toFixed(2)}
                          </text>
                          <text
                            x={x + 8}
                            y={yDatum + 54}
                            fill="#f8fafc"
                            fontSize="7.5"
                            transform={`rotate(-90 ${x + 8} ${yDatum + 54})`}
                          >
                            {node.gl.toFixed(2)}
                          </text>
                          <text
                            x={x + 8}
                            y={yDatum + 84}
                            fill="#f8fafc"
                            fontSize="7.5"
                            transform={`rotate(-90 ${x + 8} ${yDatum + 84})`}
                          >
                            {node.dist.toFixed(2)}
                          </text>
                        </g>
                      );
                    })}

                    <text x="16" y={yDatum + 122} fill="#f8fafc" fontSize="14" fontWeight="bold" letterSpacing="1">
                      {sec.title}
                    </text>
                    <text x="20" y={yDatum + 136} fill="#94a3b8" fontSize="8.5">
                      SCALE   HOR = 1:{settings.secHorScale}    VER = 1:{settings.secVerScale}
                    </text>
                  </svg>
                </div>

                {/* Section Node Schedule Table */}
                <div className="overflow-x-auto border border-cadslate-800 rounded-xl">
                  <table className="w-full text-left text-xs border-collapse">
                    <thead>
                      <tr className="bg-cadslate-950 text-slate-400 border-b border-cadslate-800 uppercase tracking-wider font-semibold">
                        <th className="py-2.5 px-4">#</th>
                        <th className="py-2.5 px-4">Manhole / Structure ID</th>
                        <th className="py-2.5 px-4">Chainage Dist (m)</th>
                        <th className="py-2.5 px-4">Road Level GL (m)</th>
                        <th className="py-2.5 px-4">Invert Level IL (m)</th>
                        <th className="py-2.5 px-4">Depth (m)</th>
                        <th className="py-2.5 px-4">Backdrop (B/D IL)</th>
                        <th className="py-2.5 px-4">Outgoing Pipe Spec</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-cadslate-800/60 font-mono">
                      {sec.nodes.map((n, nIdx) => {
                        const isLast = nIdx === sec.nodes.length - 1;
                        const segLen = !isLast ? (sec.nodes[nIdx + 1].dist - n.dist) : null;
                        const hasBd = (n.ilIn - n.ilOut) > 0.005;
                        return (
                          <tr key={nIdx} className="hover:bg-cadslate-800/40 transition">
                            <td className="py-2.5 px-4 text-slate-500">{nIdx + 1}</td>
                            <td className="py-2.5 px-4 font-bold text-fuchsia-400">{n.id}</td>
                            <td className="py-2.5 px-4 text-white font-semibold">{n.dist.toFixed(2)}m</td>
                            <td className="py-2.5 px-4 text-amber-300">{n.gl.toFixed(2)}m</td>
                            <td className="py-2.5 px-4 text-cadcyan-300 font-bold">{n.ilOut.toFixed(2)}m</td>
                            <td className="py-2.5 px-4 text-slate-300">{(n.gl - n.ilOut).toFixed(2)}m</td>
                            <td className="py-2.5 px-4">
                              {hasBd ? (
                                <span className="px-2 py-0.5 rounded bg-fuchsia-950/50 text-fuchsia-300 border border-fuchsia-500/30 text-[11px]">
                                  B/D IL: {n.ilIn.toFixed(2)}m (Drop {(n.ilIn - n.ilOut).toFixed(2)}m)
                                </span>
                              ) : (
                                <span className="text-slate-500">—</span>
                              )}
                            </td>
                            <td className="py-2.5 px-4">
                              {!isLast ? (
                                <span className="text-white bg-cadslate-950 px-2.5 py-1 rounded border border-cadslate-800">
                                  {n.size}Ø {n.mat} 1:{n.grad} ({segLen.toFixed(2)}m)
                                </span>
                              ) : (
                                <span className="text-slate-500 italic">End of Section ({sec.totalLength.toFixed(2)}m)</span>
                              )}
                            </td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                </div>
              </div>
            );
          })()}
        </div>
      )}
    </div>
  );
}

window.SewSectionView = SewSectionView;
