# Civil & Infrastructure AutoLISP Suite

[![AutoCAD](https://img.shields.io/badge/AutoCAD-2000--2026-0696D7?logo=autodesk&logoColor=white)](https://www.autodesk.com/)
[![AutoCAD LT](https://img.shields.io/badge/AutoCAD%20LT-2024%2B%20(LISP)-orange)](https://www.autodesk.com/)
[![GstarCAD](https://img.shields.io/badge/GstarCAD-Compatible-2E7D32)](https://www.gstarcad.net/)
[![Platform](https://img.shields.io/badge/Platform-Windows-0078D6?logo=windows&logoColor=white)](https://microsoft.com)
[![Engine](https://img.shields.io/badge/Language-Vanilla%20AutoLISP-red)](https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-24C7BA23-7F52-47EB-A694-87C2E5BD92EE)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A battle-tested, production-grade suite of **12 AutoLISP tools** tailored for civil infrastructure, road drainage design (**MSMA compliance**), water reticulation hydraulic modeling (**EPANET**), earthwork catchment delineation, and high-speed drafting automation.

Engineered from the ground up to be **100% vanilla AutoLISP**—eliminating fragile COM/ActiveX (`vla-`/`vlax-`) dependencies to guarantee native, crash-free performance across **AutoCAD, AutoCAD LT (2024+), GstarCAD, ZWCAD, and BricsCAD**.

---

## Table of Contents

- [Key Features](#key-features)
- [Quick Start & Installation](#quick-start--installation)
- [Command Reference](#command-reference)
- [Detailed Module Documentation](#detailed-module-documentation)
  - [1. Road Drainage & Sewerage (`PIPEC`, `GUIDEOFFSET`)](#1-road-drainage--sewerage-pipec-guideoffset)
  - [2. Catchment Hydrology & Roof Ridge (`CATCHMENTAREA`)](#2-catchment-hydrology--roof-ridge-catchmentarea)
  - [3. EPANET Hydraulic Modeling (`EPATABLE`)](#3-epanet-hydraulic-modeling-epatable)
  - [4. Quantity Take-Off & Measurement (`GETAREA*`, `GETLENGTH`)](#4-quantity-take-off--measurement-getarea-getlength)
  - [5. Drafting Accelerators (`TSEQ`, `R180`, `REPSIM`)](#5-drafting-accelerators-tseq-r180-repsim)
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
| `PIPEC` | [`pipec.lsp`](pipec.lsp) | Auto-split road drain generator with sumps, flow arrows, pipe labels & SIL leader | Drainage / Sewerage |
| `GUIDEOFFSET` | [`guideoffset.lsp`](guideoffset.lsp) | Trace perimeter and create outward offset guide polyline | Road & Drain Reserve |
| `CATCHMENTAREA` | [`catchmentarea.lsp`](catchmentarea.lsp) | Delineate 4-quadrant house/road catchment boundary with roof ridge projection | MSMA Hydrology & Runoff |
| `EPATABLE` | [`epatable.lsp`](epatable.lsp) | Parse EPANET `.rpt` simulation file into oriented CAD result tables | Water Reticulation |
| `GETAREA` | [`getarea.lsp`](getarea.lsp) | Extract polyline area in all units ($mm^2$, $m^2$, $ha$, $ac$) to clipboard | Bill of Quantities (BQ) |
| `GETAREAM` | [`getaream.lsp`](getaream.lsp) | Extract polyline area in square meters ($m^2$) to clipboard | Earthwork / Platform Take-off |
| `GETAREAHA` | [`getareaha.lsp`](getareaha.lsp) | Extract polyline area in Hectares ($ha$) to clipboard | Masterplan Zoning / Land Area |
| `GETAREAACRE` | [`getareaacre.lsp`](getareaacre.lsp) | Extract polyline area in Acres ($ac$) to clipboard | Land Titling / Survey |
| `GETLENGTH` | [`getlength.lsp`](getlength.lsp) | Multi-point continuous distance accumulator copied to clipboard | Pipe / Kerb / Road Runs |
| `TSEQ` | [`tseq.lsp`](tseq.lsp) | Sequential increment text copier (`A01` $\rightarrow$ `A02`, `1` $\rightarrow$ `2`) | Manhole & Lot Numbering |
| `R180` | [`r180.lsp`](r180.lsp) | In-place 180° entity flip on click around true geometric centroid | Text & Block Alignment |
| `REPSIM` | [`repsim.lsp`](repsim.lsp) | Drawing-wide find-and-replace for identical text with layer restriction | Network Re-labeling |

---

## Detailed Module Documentation

### 1. Road Drainage & Sewerage (`PIPEC`, `GUIDEOFFSET`)

#### `PIPEC`
Automates the drafting of road drainage networks per Malaysian Urban Stormwater Management Manual (**MSMA**) standards or regional authority requirements:
- **Automatic Run Splitting**: Long runs exceeding maximum manhole spacing (default: $30\text{ m}$) are automatically partitioned into equal, compliant sub-segments.
- **Sump Detection & Deduplication**: Checks within a $100\text{ mm}$ spatial radius to prevent duplicate sump circles from being drawn over existing ones.
- **Invert Level (SIL) Leaders**: Automatically generates rotated Sump Invert Level leaders (`SIL00.00`) oriented parallel to the drainage gradient. Includes automatic fallback from `MLEADER` to `LINE` + `MTEXT` on CAD platforms without multileader support.
- **Anti-Inversion Text**: Dynamically rotates text so annotations are always readable from the bottom or right of the drawing sheet (never upside-down).
- **Collision Avoidance**: Tests spatial bounds above and below the pipe centerline; if text space above is obstructed, annotation automatically shifts below.
- **Adaptive Stacking**: Formats pipe annotation based on segment length:
  - $\le 6\text{ m}$: 4-line vertical stack (`A01;` / `600%%c` / `6m` / `1:100`)
  - $\le 14\text{ m}$: 2-line condensed stack
  - $> 14\text{ m}$: Single-line inline label

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

### 3. EPANET Hydraulic Modeling (`EPATABLE`)

#### `EPATABLE`
Bridges hydraulic network simulations directly into submission drawings:
- Opens and parses standard EPANET `.rpt` ASCII output files without external Excel converters.
- Filters for junction/node results, extracting **Base Demand ($Q$ in $L/s$)**, **Hydraulic Saturated Level ($HSL$ in $m$)**, and **Residual Pressure ($RP$ in $m$)**.
- Generates an oriented CAD table block for each node at user-specified insertion points and rotation angles.
- **Value-Keyed Block Naming**: Formats block definitions with node values (e.g., `EpaTbl_J101_H1000_35-2_1-5_22-4`), ensuring that re-running simulations with updated heads will never display stale cached blocks.

---

### 4. Quantity Take-Off & Measurement (`GETAREA*`, `GETLENGTH`)

- **`GETAREA`**: One-click polyline area extraction. Converts the internal CAD area into a formatted string with all units:
  ```
  152500000.00 mm2 / 152.500 m2 / 0.015 ha / 0.038 ac
  ```
  Copies result directly to the Windows clipboard for instant pasting into tender documents or Excel bills of quantities.
- **`GETAREAM`**: Copies area strictly in square meters ($m^2$, default 4 decimal places).
- **`GETAREAHA`**: Copies area strictly in Hectares ($ha$).
- **`GETAREAACRE`**: Copies area strictly in Acres ($ac$).
- **`GETLENGTH`**: Continuous point-to-point accumulator for measuring curves, kerbs, and pipe alignments. Prints the total distance and places the meter value into the clipboard.

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
