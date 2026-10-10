# Civil & Infrastructure AutoLISP Suite

[![AutoCAD](https://img.shields.io/badge/AutoCAD-2000--2026-0696D7?logo=autodesk&logoColor=white)](https://www.autodesk.com/)
[![AutoCAD LT](https://img.shields.io/badge/AutoCAD%20LT-2024%2B%20(LISP)-orange)](https://www.autodesk.com/)
[![GstarCAD](https://img.shields.io/badge/GstarCAD-Compatible-2E7D32)](https://www.gstarcad.net/)
[![Platform](https://img.shields.io/badge/Platform-Windows-0078D6?logo=windows&logoColor=white)](https://microsoft.com)
[![Engine](https://img.shields.io/badge/Language-Vanilla%20AutoLISP-red)](https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-24C7BA23-7F52-47EB-A694-87C2E5BD92EE)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A battle-tested, production-grade suite of **16 AutoLISP tools** and integrated **Engineering Web Bridge** tailored for civil infrastructure, road drainage design (**MSMA compliance**), water reticulation hydraulic modeling (**EPANET**), earthwork catchment delineation, and high-speed drafting automation.

Engineered from the ground up to be **100% vanilla AutoLISP**—eliminating fragile COM/ActiveX (`vla-`/`vlax-`) dependencies to guarantee native, crash-free performance across **AutoCAD, AutoCAD LT (2024+), GstarCAD, ZWCAD, and BricsCAD**.

---

## Table of Contents

- [Key Features](#key-features)
- [Quick Start & Installation](#quick-start--installation)
- [Command Reference](#command-reference)
- [Detailed Module Documentation](#detailed-module-documentation)
  - [1. Road Drainage & Sewerage (`PIPEC`, `SEWPIPEC`, `GUIDEOFFSET`)](#1-road-drainage--sewerage-pipec-sewpipec-guideoffset)
  - [2. Catchment Hydrology & Roof Ridge (`CATCHMENTAREA`)](#2-catchment-hydrology--roof-ridge-catchmentarea)
  - [3. EPANET Hydraulic Modeling (`EPANODE`, `EPALINK`)](#3-epanet-hydraulic-modeling-epanode-epalink)
  - [4. Quantity Take-Off & Measurement (`GETAREA*`, `GETALLAREAM`, `GETLENGTH`)](#4-quantity-take-off--measurement-getarea-getallaream-getlength)
  - [5. Drafting Accelerators (`TSEQ`, `R180`, `REPSIM`)](#5-drafting-accelerators-tseq-r180-repsim)
  - [6. Excel-to-CAD Automation Pipeline (Web Bridge & `CSVUPDATE`)](#6-excel-to-cad-automation-pipeline-web-bridge--csvupdate)
- [Architecture & Design Principles](#architecture--design-principles)
- [Customization](#customization)
- [License](#license)

---

## Key Features

- **Universal CAD Parity**: Written entirely in core AutoLISP (`entmake`, `entget`, `entmod`). Runs smoothly on standard AutoCAD, modern AutoCAD LT, and alternative CAD engines like GstarCAD.
- **WCS/UCS Coordinate Safety**: Every coordinate transaction accounts for current UCS vs World Coordinate System (WCS) using `(trans ...)`. Commands never draw skewed geometry in rotated views.
- **Zero Memory Leaks**: 100% of variables and internal helper lambdas are localized in function signatures (`/ ...`), keeping the AutoCAD runtime pristine across multi-drawing sessions.
- **Self-Healing Error Handling**: Restores system variables (`CMDECHO`, `OSMODE`, `CMLEADERSTYLE`) and active UCS orientations immediately if a command is cancelled or interrupted via `ESC`.
- **Integrated Clipboard Pipeline**: Copies measurements directly to the Windows OS clipboard via stream-safe buffers (`clip.exe`), ready for instant pasting into Excel or calculation sheets.

---

## Quick Start & Installation

### Option 1: One-Click Master Loader (Recommended)
1. Clone or download this repository.
2. Drag and drop [`load-all.lsp`](load-all.lsp) directly into your active CAD drawing window, **OR** type:
   ```lisp
   (load "load-all.lsp")
   ```
3. All 12 routines will load instantly, and a formatted command summary table will print to your CAD command line.

### Option 2: Automatic Startup via `APPLOAD`
1. Open AutoCAD or GstarCAD and run the command `APPLOAD`.
2. Under **Startup Suite** (the briefcase icon), click **Contents...**.
3. Click **Add...** and select either [`load-all.lsp`](load-all.lsp) or individual `.lsp` files.
4. The routines will load automatically every time any drawing opens.

### Option 3: Integration into `acaddoc.lsp`
Add the following line to your firm's central `acaddoc.lsp` or `gcad.lsp`:
```lisp
(load "C:/path/to/AutoLISP/load-all.lsp")
```

---

## Command Reference

| Command | Source File | Description | Primary Use Case |
| :--- | :--- | :--- | :--- |
| `PIPEC` | [`pipec.lsp`](pipec.lsp) | Auto-split road drain generator with sumps, flow arrows, pipe labels & SIL leader | Road Drainage |
| `SEWPIPEC` | [`sewpipec.lsp`](sewpipec.lsp) | Auto-split sewer reticulation generator with manholes & IL leaders | Sewerage Reticulation |
| `GUIDEOFFSET` | [`guideoffset.lsp`](guideoffset.lsp) | Trace perimeter and create outward offset guide polyline | Road & Drain Reserve |
| `CATCHMENTAREA` | [`catchmentarea.lsp`](catchmentarea.lsp) | Delineate 4-quadrant house/road catchment boundary with roof ridge projection | MSMA Hydrology & Runoff |
| `EPANODE` | [`epanode.lsp`](epanode.lsp) | Parse EPANET `.rpt` to auto-label node Elevation, Head & Pressure | Water Reticulation Nodes |
| `EPALINK` | [`epalink.lsp`](epalink.lsp) | Parse EPANET `.rpt` to auto-label pipe Diameter, Velocity & Headloss | Water Reticulation Pipes |
| `GETAREA` | [`getarea.lsp`](getarea.lsp) | Extract polyline area in all units ($mm^2$, $m^2$, $ha$, $ac$) to clipboard | Multi-Unit BQ Quantities |
| `GETALLAREAM` | [`getallaream.lsp`](getallaream.lsp) | Batch select multiple polylines, sum total area in $m^2$ & inject text label | Earthwork / Platform Take-off |
| `GETAREAM` | [`getaream.lsp`](getaream.lsp) | Extract single polyline area in $m^2$, inject centroid text & copy to clipboard | Pavement / Lot Take-off |
| `GETAREAHA` | [`getareaha.lsp`](getareaha.lsp) | Extract polyline area in Hectares ($ha$), inject centroid text & copy to clipboard | Masterplan Zoning / Land Area |
| `GETAREAACRE` | [`getareaacre.lsp`](getareaacre.lsp) | Extract polyline area in Acres ($ac$), inject centroid text & copy to clipboard | Land Titling / Survey |
| `GETLENGTH` | [`getlength.lsp`](getlength.lsp) | Multi-point continuous distance accumulator copied to clipboard | Pipe / Kerb / Road Runs |
| `TSEQ` | [`tseq.lsp`](tseq.lsp) | Smart sequencer auto-incrementing IDs while protecting MText pipe formatting | Manhole & Lot Renumbering |
| `R180` | [`r180.lsp`](r180.lsp) | In-place 180° entity flip on click around true geometric centroid | Text & Block Alignment |
| `REPSIM` | [`repsim.lsp`](repsim.lsp) | Drawing-wide find-and-replace for identical text with layer restriction | Network Re-labeling |
| `CSVUPDATE` | [`csvupdate.lsp`](csvupdate.lsp) | Batch replace placeholder text with formatted strings from XLS-to-CSV Bridge | Excel Schedule Importer |

---

## Detailed Module Documentation

### 1. Road Drainage & Sewerage (`PIPEC`, `SEWPIPEC`, `GUIDEOFFSET`)

#### `PIPEC` & `SEWPIPEC`
Automates the drafting of road drainage and sewer reticulation networks per Malaysian Urban Stormwater Management Manual (**MSMA**) standards or regional authority requirements:
- **Automatic Run Splitting**: Long runs exceeding maximum manhole spacing (default: $30\text{ m}$) are automatically partitioned into equal, compliant sub-segments.
- **Sump / Manhole Detection & Deduplication**: Checks within spatial radius to prevent duplicate structures from being drawn over existing ones.
- **Invert Level (SIL / IL) Leaders**: Generates native `MULTILEADER` entities matching style `1000-T2` (closed filled arrow, 2000mm arrowhead, 2000mm landing distance, 1000mm landing gap, middle of top line attachment).
  - **Dynamic Line Following**: Leader text and landing lines automatically align parallel with the pipe gradient (`txtAngRad`) at any angle ($0^\circ, 45^\circ, 90^\circ$).
  - **Left / Right Text Justification**: Supports `Justify: Right` (text on left of landing, leader dogleg on right) or `Justify: Left` (text on right of landing, leader dogleg on left).
  - **Perpendicular Offset Side**: Configurable leader branch side (`Left` at $+90^\circ$ or `Right` at $-90^\circ$ perpendicular to pipe).
  - **On-the-Fly Keystroke Toggles**: Type `J` (Justify) or `S` (Side) at any point prompt to toggle side and justification on the fly without exiting. Includes automatic fallback to `LINE` + `MTEXT` on CAD platforms without multileader support.

#### `GUIDEOFFSET`
- Allows continuous point-by-point boundary tracing to generate an offset guideline on a dedicated layer (`DRN-GUIDE-1500`).
- Tracing clockwise automatically offsets the boundary outwards, providing instant road reserve clearances without manual trimming.

---

### 2. Catchment Hydrology & Roof Ridge (`CATCHMENTAREA`)

#### `CATCHMENTAREA`
Delineates runoff contribution boundaries for stormwater drainage sizing:
- Prompts the user to pick 4 inner house/building footprint corners, 4 outer road/boundary corners, and 2 roof ridge reference points.
- Automatically projects the roof ridge onto the major building axis.
- Uses a centroid-based counter-clockwise selection sort (vanilla LISP, zero COM) to match inner and outer polygons.
- Generates 4 closed `LWPOLYLINE` catchments on layer `DRN-TOTALAREA` ready for instant area extraction and rational formula ($Q = \frac{C \cdot I \cdot A}{360}$) runoff calculations.

---

### 3. EPANET Hydraulic Modeling (`EPANODE`, `EPALINK`)

#### `EPANODE`
Parses EPANET simulation report (`.rpt`) files and automatically annotates junction nodes in CAD:
- Select an EPANET `.rpt` simulation file through native file dialog.
- Click node text labels or insertion points to query simulated **Elevation ($m$)**, **Hydraulic Head ($m$)**, and **Pressure ($m$)**.
- Generates oriented, formatted multi-line MTEXT callouts without requiring manual data copying.

#### `EPALINK`
Parses EPANET simulation report (`.rpt`) files and automatically annotates pipe links in CAD:
- Reads pipe link calculation sections from `.rpt` files.
- Click link identifier labels to generate hydraulic callouts including **Length ($m$)**, **Diameter ($mm$)**, **Flow Velocity ($m/s$)**, and **Headloss Gradient ($m/km$)**.

---

### 4. Quantity Take-Off & Measurement (`GETAREA*`, `GETALLAREAM`, `GETLENGTH`)

- **`GETAREA`**: One-click polyline area extraction. Converts internal CAD area into a formatted summary with all units:
  ```
  152500000.00 mm2 / 152.500 m2 / 0.015 ha / 0.038 ac
  ```
  Copies summary directly to the Windows clipboard for instant pasting into tender documents or Excel bills of quantities.
- **`GETALLAREAM`**: Batch-selects multiple polylines (`ssget`), sums their total area, scales from $mm^2$ to $m^2$, prompts for an insertion point, and injects a formatted total text label into the drawing.
- **`GETAREAM`**: Calculates polyline area in square meters ($m^2$), automatically calculates the polyline vertex centroid, injects a middle-centered text label (`X.XXXX m2`), and copies the numeric value to clipboard.
- **`GETAREAHA`**: Calculates polyline area in Hectares ($ha$), injects a middle-centered text label (`X.XXXX ha`), and copies the numeric value to clipboard.
- **`GETAREAACRE`**: Calculates polyline area in Acres ($ac$), injects a middle-centered text label (`X.XXXX ac`), and copies the numeric value to clipboard.
- **`GETLENGTH`**: Continuous point-to-point accumulator for measuring curves, kerbs, and pipe alignments. Prints total distance and places the meter value into the clipboard.

---

### 5. Drafting Accelerators (`TSEQ`, `R180`, `REPSIM`)

- **`TSEQ` (Smart Sequence Incrementor)**:
  - Select any source `TEXT` or `MTEXT` entity containing letters or numbers (e.g. `MH-01`, `LOT 100`, `Node-A`).
  - Automatically parses numerical or alphabetical suffixes, zero-pads integers (`01` $\rightarrow$ `02`), and allows rapid click-to-place copying.
  - Clones entities using native `entmake` rather than command-line `COPY` for instant execution without osnap interference.
- **`R180` (In-Place 180° Flip)**:
  - Click any text, block, line, or polyline to flip it 180° around its true geometric center or midpoint.
  - Eliminates the need to manually invoke `ROTATE`, specify basepoints, and type `180`.
- **`REPSIM` (Replace Similar Text)**:
  - Select a sample text object to open a lightweight DCL dialog.
  - Replaces all matching strings drawing-wide, with an optional toggle to restrict replacement to the selected entity's layer.
  - Includes an automatic command-line fallback if loaded in CAD environments lacking dynamic DCL support.

---

### 6. Excel-to-CAD Automation Pipeline (Web Bridge & `CSVUPDATE`)

Bridging engineering calculation spreadsheets (pipe sizing, hydraulic gradients, invert levels) directly into drafting drawings without manual typing or error-prone copy-pasting:

```
┌─────────────────────────────────┐
│  Engineering Schedule (Excel)   │
│  (.xlsx / .xls calculation run) │
└────────────────┬────────────────┘
                 │ Drag & drop into browser
                 ▼
┌─────────────────────────────────┐
│   AutoCAD XLS-to-CSV Bridge     │
│  (index.html / React + SheetJS) │
│  - Flexible Column Mapper       │
│  - MTEXT Rule Engine            │
└────────────────┬────────────────┘
                 │ Exports 2-column CSV (ID, CAD_String)
                 ▼
┌─────────────────────────────────┐
│     AutoLISP: c:csvupdate       │
│  - In-memory Hash Dictionary    │
│  - Interactive Text Selection   │
│  - Instant DXF (entmod) Update  │
└────────────────┬────────────────┘
                 │ Batch replaces P1, P2, S1...
                 ▼
┌─────────────────────────────────┐
│   Production-Ready CAD Drawing  │
│   Formatted MTEXT Annotations   │
└─────────────────────────────────┘
```

#### AutoCAD XLS-to-CSV Bridge ([`index.html`](index.html) & [`bridge/index.html`](bridge/index.html))
- **Zero-Installation Web App**: Self-contained client-side single-page application built with React 18, Tailwind CSS, Lucide Icons, and SheetJS (`xlsx`). Runs locally simply by double-clicking [`index.html`](index.html) or loading it via GitHub Pages.
- **Smart Column Auto-Mapping**: Automatically detects and maps required engineering columns:
  1. `ID` — CAD placeholder text (e.g., `P1`, `P2`, `S1`, `MH-01`).
  2. `Type` — Component discriminator (`Drain` / `Pipe` vs `SIL`).
  3. `Code` — Drainage/pipe class code (e.g., `A01`, `RC1`).
  4. `Size` — Diameter / dimension in mm (e.g., `600`, `900`).
  5. `Length` — Segment length in metres (e.g., `12`).
  6. `Gradient` — Slope ratio or invert level (e.g., `1000` for 1:1000, or `25.50` for SIL).
- **Civil Engineering MTEXT Formatting Rules**:
  - **Sump Invert Level (`SIL`)**: Formats into `{\W0.5;SIL[Gradient]}` (e.g., `{\W0.5;SIL24.50}`).
  - **Short Pipe ($\le 6\text{ m}$)**: Formats into 4-line vertical stack `{\W0.5;[Code]\P%%c[Size]\P[Length]m\P1:[Gradient]}`.
  - **Medium Pipe ($\le 14\text{ m}$)**: Formats into 2-line condensed stack `{\W0.5;[Code]-%%c[Size]\P[Length]m-1:[Gradient]}`.
  - **Long Pipe ($> 14\text{ m}$)**: Formats into 1-line linear label `{\W0.5;[Code]-%%c[Size]-[Length]m-1:[Gradient]}`.
- **Configurable Settings**: In-app modal with real-time preview allowing customization of width factor (`\W0.5;`), length thresholds ($6\text{ m}$, $14\text{ m}$), and auto-stacking override toggles (saved to `localStorage`).
- **One-Click Export**: Generates a standard 2-column CSV mapping file ready for CAD ingestion.

#### `CSVUPDATE` ([`csvupdate.lsp`](csvupdate.lsp))
- Prompts the drafter via standard file dialog (`getfiled`) to select the exported `.csv` file.
- Reads and parses the file into an in-memory association list / hash dictionary `dict` with uppercase keys for case-insensitive matching.
- Prompts the drafter to select the target text placeholders (`TEXT` and `MTEXT`) via `ssget` (supporting window/crossing selections or typing `ALL`).
- Scans each selected entity, strips any surrounding whitespace, looks up the corresponding engineering string from `dict`, and applies native DXF modification (`entmod`) to Group Code 1.
- Reports the exact number of updated placeholders in the command line window.

---

## Architecture & Design Principles

```
┌──────────────────────────────────────────────────────────┐
│                   AutoCAD / GstarCAD                     │
└────────────────────────────┬─────────────────────────────┘
                             │ Pure AutoLISP API
             ┌───────────────┴───────────────┐
             ▼                               ▼
   ┌───────────────────┐           ┌───────────────────┐
   │  Database Control │           │ Coordinate Math   │
   │  - entmake        │           │ - (trans pt 1 0)  │
   │  - entmod         │           │ - (trans pt 0 1)  │
   │  - entget         │           │ - 2D WCS Z-Zero   │
   └─────────┬─────────┘           └─────────┬─────────┘
             │                               │
             └───────────────┬───────────────┘
                             ▼
   ┌───────────────────────────────────────────────────┐
   │         Zero-Dependency Native Execution          │
   │  - No COM/ActiveX (vla- / vlax-)                  │
   │  - No external DLLs or .NET runtimes              │
   │  - Native Windows clip.exe stream integration     │
   └───────────────────────────────────────────────────┘
```

1. **Vanilla LISP Standard**: Many modern AutoLISP routines fail in AutoCAD LT or GstarCAD because they rely on ActiveX (`vlax-get-acad-object`, `vla-put-color`). This suite operates strictly on the DXF database (`entmake`, `entget`, `entmod`), guaranteeing 100% platform portability.
2. **Wildcard Escaping**: AutoCAD's `ssget` filter interprets `#`, `@`, `*`, `?`, and `~` as wildcard tokens. Layers beginning with civil prefixes (such as `#JRK - RD Drain Line`) are escaped automatically (`#` $\rightarrow$ `` `# ``) to prevent selection failures.
3. **Robust Error Trapping**: Every command features a localized `*error*` routine. If a drafter hits `ESC` halfway through a command, active commands are closed, temporary UCS rotations are undone, and all system variables are restored to their original states.

---

## Customization

Every file features an isolated, easily editable `USER SETTINGS` header block. You do not need to read through the execution logic to adjust layers, colors, text styles, or dimensioning rules:

```lisp
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq unitsPerM 1000.0)      ;; drawing units per metre (1000 = mm, 1.0 = m)
  (setq maxSegM   30.0)        ;; max pipe run (m) before auto-inserting sump

  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)          ;; 4 = Cyan
  (setq drainWidth 250.0)      ;; Polyline global width

  (setq sumpLayer "#JRK - RD Drain Manhole_Sump")
  (setq sumpColor 4)
  (setq sumpRad 600.0)         ;; Radius of sump circle

  (setq textLayer "#JRK - RD Drain Text")
  (setq textColor 7)           ;; 7 = White / Black
  (setq txtHgt 2000.0)
  (setq txtStyle "1000-T2")
  (setq txtWidth 0.5)

  (setq mlLayer     "#JRK - RD Drain Text IL")
  (setq mlStyle     "1000-T2")         ;; Multileader style
  (setq mlText      "{\\W0.5;SIL00.00}")
  (setq mlJustify   "Right")           ;; "Right" (text left of landing) or "Left"
  (setq mlSide      "Left")            ;; Offset side from pipe: "Left" (+90) or "Right" (-90)
  (setq mlLeadLen   3000.0)            ;; Leader offset from sump rim
  (setq mlLandDist  2000.0)            ;; Horizontal landing distance
  (setq mlArrowSize 2000.0)            ;; Arrowhead size
  (setq mlLandGap   1000.0)            ;; Landing gap
  ;; =========================================================================
```

Simply update the values to match your organization's CAD layer standards.

---

## Contributing

Contributions, bug reports, and civil engineering feature requests are welcome!
1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/HydraulicProfile`)
3. Commit your Changes (`git commit -m 'Add hydraulic long-section generator'`)
4. Push to the Branch (`git push origin feature/HydraulicProfile`)
5. Open a Pull Request

---

## License

Distributed under the **MIT License**. See [`LICENSE`](LICENSE) for more information.

---

**Developed & Maintained by [Nazreen Nasyuha](https://github.com/NazreenNasyuha)**  
*Specializing in Infrastructure Engineering, Hydraulic Modeling & CAD Automation.*
