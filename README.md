# Civil & Infrastructure AutoLISP Suite

[![AutoCAD](https://img.shields.io/badge/AutoCAD-2000--2026-0696D7?logo=autodesk&logoColor=white)](https://www.autodesk.com/)
[![AutoCAD LT](https://img.shields.io/badge/AutoCAD%20LT-2024%2B%20(LISP)-orange)](https://www.autodesk.com/)
[![GstarCAD](https://img.shields.io/badge/GstarCAD-Compatible-2E7D32)](https://www.gstarcad.net/)
[![Live App](https://img.shields.io/badge/Live%20App-GitHub%20Pages-brightgreen?logo=github&logoColor=white)](https://nazreennasyuha.github.io/AutoLISP/)
[![Engine](https://img.shields.io/badge/Language-Vanilla%20AutoLISP-red)](https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-24C7BA23-7F52-47EB-A694-87C2E5BD92EE)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> 🚀 **Live Web Application**: Launch the [AutoCAD Smart XLS-to-CSV Bridge](https://nazreennasyuha.github.io/AutoLISP/) directly in your browser.

A battle-tested, production-grade suite of **15 specialized AutoLISP tools** and an integrated **Engineering Web Bridge** tailored for civil infrastructure, road drainage design (**MSMA compliance**), municipal sewerage reticulation, water supply hydraulic modeling (**EPANET**), earthwork catchment hydrology, and high-speed drafting automation.

Engineered from the ground up to be **100% vanilla AutoLISP**—eliminating fragile COM/ActiveX (`vla-`/`vlax-`) dependencies to guarantee native, crash-free performance across **AutoCAD, AutoCAD LT (2024+), GstarCAD, ZWCAD, and BricsCAD**.

---

## Repository Architecture

```text
AutoLISP/
├── index.html                   # Live Web Bridge (Single-page app for GitHub Pages)
├── bridge/
│   └── index.html               # Mirrored Web Bridge app
├── load-all.lsp                 # Master One-Click Loader (root entrypoint)
├── LISPS/                       # Dedicated directory for all 15 AutoLISP routines
│   ├── catchmentarea.lsp        # 4-corner catchment & roof ridge splitter
│   ├── csvupdate.lsp            # 5-column CSV injection & red QA/QC verification
│   ├── epalink.lsp              # EPANET pipe results hexagon & text annotator
│   ├── epanode.lsp              # EPANET junction node results table generator
│   ├── getallaream.lsp          # Batch closed polyline area summation (m2)
│   ├── getarea.lsp              # Polyline area breakdown (m2, ha, ac) + clipboard
│   ├── getareaacre.lsp          # Dedicated acre (ac) area calculator + clipboard
│   ├── getareaha.lsp            # Dedicated hectare (ha) area calculator + clipboard
│   ├── getaream.lsp             # Dedicated square meter (m2) area calculator + clipboard
│   ├── getlength.lsp            # Cumulative distance accumulator + clipboard
│   ├── guideoffset.lsp          # Click-to-trace parcel guide offset generator
│   ├── pipec.lsp                # Road drainage generator (sumps, arrows, SIL leaders)
│   ├── r180.lsp                 # Single-click 180-degree entity flip about centroid
│   ├── sewpipec.lsp             # Sewerage network generator (manholes, IL leaders)
│   └── tseq.lsp                 # Smart alphanumeric sequence renumbering engine
├── 404.html                     # GitHub Pages fallback routing
├── .nojekyll                    # Disables Jekyll processing for GitHub Pages
└── README.md                    # Complete documentation and user manual
```

---

## Table of Contents

- [Key Features](#key-features)
- [Quick Start & Installation](#quick-start--installation)
- [Master Command Cheatsheet](#master-command-cheatsheet)
- [Step-by-Step User Manual](#step-by-step-user-manual)
  - [1. Road Drainage Network Generator (`PIPEC`)](#1-road-drainage-network-generator-pipec)
  - [2. Sewer Reticulation Generator (`SEWPIPEC`)](#2-sewer-reticulation-generator-sewpipec)
  - [3. Continuous Parcel Guide Offset (`GUIDEOFFSET`)](#3-continuous-parcel-guide-offset-guideoffset)
  - [4. Catchment Hydrology & Roof Ridge Delineation (`CATCHMENTAREA`)](#4-catchment-hydrology--roof-ridge-delineation-catchmentarea)
  - [5. Smart Excel CSV Data Injection & QA/QC (`CSVUPDATE`)](#5-smart-excel-csv-data-injection--qaqc-csvupdate)
  - [6. EPANET Pipe Simulation Annotator (`EPALINK`)](#6-epanet-pipe-simulation-annotator-epalink)
  - [7. EPANET Node Simulation Table Generator (`EPANODE`)](#7-epanet-node-simulation-table-generator-epanode)
  - [8. Batch Polyline Area Summation (`GETALLAREAM`)](#8-batch-polyline-area-summation-getallaream)
  - [9. Multi-Unit Polyline Area Breakdown (`GETAREA`)](#9-multi-unit-polyline-area-breakdown-getarea)
  - [10. Dedicated Square Meter Area Extraction (`GETAREAM`)](#10-dedicated-square-meter-area-extraction-getaream)
  - [11. Dedicated Hectare Area Extraction (`GETAREAHA`)](#11-dedicated-hectare-area-extraction-getareaha)
  - [12. Dedicated Acre Area Extraction (`GETAREAACRE`)](#12-dedicated-acre-area-extraction-getareaacre)
  - [13. Cumulative Path Distance Measurement (`GETLENGTH`)](#13-cumulative-path-distance-measurement-getlength)
  - [14. In-Place 180° Entity Reversal (`R180`)](#14-in-place-180-entity-reversal-r180)
  - [15. Smart Alphanumeric Sequence Renumbering (`TSEQ`)](#15-smart-alphanumeric-sequence-renumbering-tseq)
- [AutoCAD Smart XLS-to-CSV Web Bridge Guide](#autocad-smart-xls-to-csv-web-bridge-guide)
- [Architecture & Design Principles](#architecture--design-principles)
- [Customization & Layer Configuration](#customization--layer-configuration)
- [License](#license)

---

## Key Features

- **Universal CAD Parity**: Written in pure AutoLISP (`entmake`, `entget`, `entmod`). Runs smoothly on standard AutoCAD, modern AutoCAD LT (2024+), GstarCAD, ZWCAD, and BricsCAD.
- **WCS/UCS Coordinate Safety**: Every coordinate transaction transforms between UCS and World Coordinate System (WCS) using `(trans ...)`. Commands never draw skewed geometry in rotated views.
- **Zero Memory Leaks**: 100% of variables and helper lambdas are strictly localized in function headers (`/ ...`), keeping the AutoCAD runtime pristine.
- **Self-Healing Error Trapping**: Restores system variables (`CMDECHO`, `OSMODE`, `CMLEADERSTYLE`) and active UCS orientations immediately if a command is cancelled or interrupted with `ESC`.
- **Integrated Clipboard Pipeline**: Copies measurements directly to the Windows OS clipboard (`clip.exe`), ready for instant pasting into Excel, calculation sheets, or tender documents.

---

## Quick Start & Installation

### Option 1: One-Click Master Loader (Recommended)
1. Clone or download this repository.
2. Drag and drop [`load-all.lsp`](load-all.lsp) directly into your active CAD drawing window, **OR** type in the command line:
   ```lisp
   (load "load-all.lsp")
   ```
3. All 15 automation modules inside `LISPS/` will compile into memory automatically, and a formatted command summary table will print to your CAD command line.

### Option 2: Automatic Startup via `APPLOAD`
1. Open AutoCAD or GstarCAD and type `APPLOAD`.
2. Under **Startup Suite** (the briefcase icon), click **Contents...**.
3. Click **Add...** and select [`load-all.lsp`](load-all.lsp).
4. The entire suite will load automatically every time any drawing opens.

### Option 3: Integration into `acaddoc.lsp` or `gcad.lsp`
Add the following line to your firm's central `acaddoc.lsp` (AutoCAD) or `gcad.lsp` (GstarCAD):
```lisp
(load "C:/path/to/AutoLISP/load-all.lsp")
```

### Option 4: Configuring AutoCAD Support File Search Path
If you prefer loading individual scripts by name:
1. Open AutoCAD and type `OPTIONS`.
2. Navigate to the **Files** tab $\rightarrow$ **Support File Search Path**.
3. Click **Add...** $\rightarrow$ **Browse...** and select the `AutoLISP/LISPS` folder.
4. Click **Apply** and **OK**.
5. You can now load any script anytime by typing `(load "pipec")`, `(load "sewpipec")`, etc.

---

## Master Command Cheatsheet

| Command | File in `LISPS/` | Description | User Inputs / Triggers | Generated Output |
| :--- | :--- | :--- | :--- | :--- |
| `PIPEC` | [`pipec.lsp`](LISPS/pipec.lsp) | Road drainage network generator | Pick Start Pt $\rightarrow$ Pick End Pt (with auto 30m splitting) | Centerline, R=600 sumps, flow arrows, pipe label, SIL Multileader |
| `SEWPIPEC` | [`sewpipec.lsp`](LISPS/sewpipec.lsp) | Sewer reticulation network generator | Pick Start Pt $\rightarrow$ Pick End Pt (with auto 100m splitting) | Pipe line, R=750 manholes, flow arrows, VCP label, 4-line MH Multileader |
| `GUIDEOFFSET` | [`guideoffset.lsp`](LISPS/guideoffset.lsp) | Closed parcel guide offset tracer | Offset distance $\rightarrow$ Pick corners $\rightarrow$ Pick offset side | Closed offset polyline on `DRN-GUIDE-{dist}` (Color 8) |
| `CATCHMENTAREA` | [`catchmentarea.lsp`](LISPS/catchmentarea.lsp) | 4-corner catchment & roof ridge divider | 4 Inner Pts $\rightarrow$ 4 Outer Pts $\rightarrow$ 2 Ridge Pts | 4 closed polylines on `DRN-TOTALAREA` + centered m² text |
| `CSVUPDATE` | [`csvupdate.lsp`](LISPS/csvupdate.lsp) | Excel CSV smart data injection | Select 5-column CSV $\rightarrow$ Select CAD text/mleaders | Injects Drain/Sewer data + draws red QA/QC bounding boxes |
| `EPALINK` | [`epalink.lsp`](LISPS/epalink.lsp) | EPANET pipe hydraulic annotator | Select `.rpt` file $\rightarrow$ Text height $\rightarrow$ Pick pipe & angle | Magenta hexagon (ID) + Red hydraulic text (`L=... Q=... Vs=... HL=...`) |
| `EPANODE` | [`epanode.lsp`](LISPS/epanode.lsp) | EPANET junction node table annotator | Select `.rpt` file $\rightarrow$ Text height $\rightarrow$ Pick node position | 4-row engineering table block (Node ID, HSL, Demand Q, Pressure RP) |
| `GETALLAREAM` | [`getallaream.lsp`](LISPS/getallaream.lsp) | Batch polyline area summation | Select closed polylines (window/crossing) | Centered `X.XXX m2` text on each polyline + Grand Total in prompt |
| `GETAREA` | [`getarea.lsp`](LISPS/getarea.lsp) | Universal multi-unit area query | Click any closed polyline | Centered `m2 / ha / ac` text + full string copied to clipboard |
| `GETAREAM` | [`getaream.lsp`](LISPS/getaream.lsp) | Dedicated square meter ($m^2$) extraction | Click any closed polyline | Centered `X.XXXX m2` text + numeric $m^2$ copied to clipboard |
| `GETAREAHA` | [`getareaha.lsp`](LISPS/getareaha.lsp) | Dedicated hectare ($ha$) extraction | Click any closed polyline | Centered `X.XXXX ha` text + numeric $ha$ copied to clipboard |
| `GETAREAACRE` | [`getareaacre.lsp`](LISPS/getareaacre.lsp) | Dedicated acre ($ac$) extraction | Click any closed polyline | Centered `X.XXXX ac` text + numeric $ac$ copied to clipboard |
| `GETLENGTH` | [`getlength.lsp`](LISPS/getlength.lsp) | Continuous 2D path distance accumulator | Pick initial point $\rightarrow$ Pick consecutive points $\rightarrow$ Enter | Flat 2D distance printed to prompt + meters copied to clipboard |
| `R180` | [`r180.lsp`](LISPS/r180.lsp) | Single-click 180° in-place rotation | Click entity in continuous loop $\rightarrow$ Enter | Rotates entity 180° around true centroid (Line, Polyline, Text, Block) |
| `TSEQ` | [`tseq.lsp`](LISPS/tseq.lsp) | Smart alphanumeric sequencer | Enter start sequence (e.g. `D1`, `MH01`) $\rightarrow$ Click labels | Replaces ID only, preserves width factor (`\W0.5;`) & multi-line formatting |

---

## Step-by-Step User Manual

### 1. Road Drainage Network Generator (`PIPEC`)
**File:** [`LISPS/pipec.lsp`](LISPS/pipec.lsp) | **Command:** `PIPEC`

Automates the drafting of roadside concrete stormwater drains and pipe culverts complying with Malaysian **MSMA** guidelines.

```text
[Start Sump] ────(Flow Arrow)──── [Pipe Annotation] ──── [End Sump]
     │                                                        │
 [SIL Leader]                                             [SIL Leader]
```

#### Step-by-Step Instructions:
1. Type `PIPEC` and press Enter.
2. **First-run Configuration Prompt**:
   - `Configure settings? [Yes/No] <N>:` Press Enter to keep defaults or `Y` to configure:
     - Starting Drain Code (e.g. `D1` or `1`)
     - Pipe/Drain Size (e.g. `600` mm)
     - Gradient (e.g. `1000` for 1:1000)
3. **Pick Start Point**: Click the beginning of your road drain alignment (e.g., at an existing sump or culvert inlet).
4. **Pick Next Point**: Click the next junction or discharge outlet.
5. **Automatic Execution**:
   - If the distance exceeds 30m, `PIPEC` divides the run into equal sub-segments ($\le 30\text{m}$) and places intermediate sumps.
   - Draws circular sumps ($R=600\text{mm}$) on layer `#JRK - RD Drain Manhole_Sump` (Color 4 - Cyan).
   - Draws centerline drainage pipes neatly trimmed to the sump rims on layer `#JRK - RD Drain Line` (Color 4 - Cyan).
   - Inserts directional flow arrows on layer `#JRK - RD Drain FLOW` (Color 4 - Cyan).
   - Generates collision-aware pipe text (`{\W0.5;D1;600mm;28.5m;1000}`) on layer `#JRK - RD Drain Text` (Color 7).
   - Generates parametric `MULTILEADER` annotations (`{\W0.5;SIL00.00}`) on layer `#JRK - RD Drain Text IL` using style `1000-T2`.
6. **Live Undo**: Type `U` during any pick prompt to undo the previous segment without exiting.

---

### 2. Sewer Reticulation Generator (`SEWPIPEC`)
**File:** [`LISPS/sewpipec.lsp`](LISPS/sewpipec.lsp) | **Command:** `SEWPIPEC`

Drafts municipal gravity sewerage reticulation networks with standard vitrified clay pipes (VCP) and precast circular manholes.

```text
[MH-01] ════════(Flow Arrow)════════ [VCP Pipe Label] ════════ [MH-02]
   │                                                              │
[4-Line Leader]                                                [4-Line Leader]
(MH-01 \P GL00.00 \P IL00.00 \P DEPTH0.00)
```

#### Step-by-Step Instructions:
1. Type `SEWPIPEC` and press Enter.
2. **Configuration Prompt**:
   - Enter Starting Manhole ID (e.g., `MH-01` or `SMH-10`).
   - Enter Pipe Material (default: `VCP`).
   - Enter Pipe Diameter (default: `225` mm).
   - Enter Pipe Gradient (default: `100` for 1:100).
3. **Pick Start Point**: Click the location of the first sewer manhole.
4. **Pick Next Point**: Click the downstream manhole location.
5. **Automatic Execution**:
   - If span exceeds 100m, auto-partitions the pipe run into equal spans $\le 100\text{m}$.
   - Inserts circular manholes ($R=750\text{mm}$) on layer `#JRK - Sewer Manhole` (Color 6 - Magenta).
   - Draws pipe centerline on layer `#JRK - Sewer Pipe Line` (Color 6 - Magenta).
   - Inserts directional flow arrows on layer `#JRK - Sewer Pipe FLOW` (Color 6 - Magenta).
   - Formats pipe annotation (`{\W0.5;VCP 225mm (45.0m) 1:100}`) on layer `#JRK - Sewer Pipe Text` (Color 7).
   - Inserts 4-line parametric `MULTILEADER` on layer `#JRK - Sewer Manhole Text IL` using style `750-T2`:
     ```text
     {\W0.5;MH-01\PGL00.00\PIL00.00\PDEPTH0.00}
     ```
   - Automatically increments the manhole ID for the next pick (e.g., `MH-01` $\rightarrow$ `MH-02`).

---

### 3. Continuous Parcel Guide Offset (`GUIDEOFFSET`)
**File:** [`LISPS/guideoffset.lsp`](LISPS/guideoffset.lsp) | **Command:** `GUIDEOFFSET`

Quickly traces building setbacks, road reserves, and parcel boundaries at a fixed offset to generate clean drainage alignment guide lines.

#### Step-by-Step Instructions:
1. Type `GUIDEOFFSET` and press Enter.
2. Enter the desired offset distance (default: `1500` drawing units).
3. Click the first corner of the parcel boundary.
4. Click subsequent corners in clockwise or counter-clockwise order.
5. Press Enter to seal and close the boundary.
6. Click inside or outside to indicate the offset direction.
7. The offset polyline is placed on layer `DRN-GUIDE-{distance}` in Color 8 (Dark Gray), and the temporary trace polyline is deleted automatically.

---

### 4. Catchment Hydrology & Roof Ridge Delineation (`CATCHMENTAREA`)
**File:** [`LISPS/catchmentarea.lsp`](LISPS/catchmentarea.lsp) | **Command:** `CATCHMENTAREA`

Delineates 4-quadrant road and roof runoff contribution boundaries for stormwater drainage sizing.

```text
       Outer Parcel Boundary (Road / Fence)
┌───────────────────────────────────────────────┐
│     \                   |                   / │
│      \  Quadrant 1      |  Quadrant 2      /  │
│       ┌─────────────────┴─────────────────┐   │
│       │           Roof Ridge Line         │   │
│       │ ═════════════════════════════════ │   │
│       │       Inner House Footprint       │   │
│       └─────────────────┬─────────────────┘   │
│      /  Quadrant 3      |  Quadrant 4      \  │
│     /                   |                   \ │
└───────────────────────────────────────────────┘
```

#### Step-by-Step Instructions:
1. Type `CATCHMENTAREA` and press Enter.
2. **Click 4 Inner Footprint Corners**: Click the 4 corners of the house or building footprint.
3. **Click 4 Outer Boundary Corners**: Click the 4 corners of the outer parcel or road reserve.
4. **Click 2 Points along Roof Ridge**: Click the start and end points of the main roof apex.
5. **Automatic Execution**:
   - The script sorts all coordinates counter-clockwise, projects the roof ridge onto the principal axis, and splits the lot into 4 closed catchments on layer `DRN-TOTALAREA` (Color 4 - Cyan).
   - Calculates each catchment's area in square meters ($m^2$) and places a centered text annotation inside each quadrant.

---

### 5. Smart Excel CSV Data Injection & QA/QC (`CSVUPDATE`)
**File:** [`LISPS/csvupdate.lsp`](LISPS/csvupdate.lsp) | **Command:** `CSVUPDATE`

Injects processed calculation results from the Web Bridge directly into CAD drawings, updating both standard pipe text and parametric Multileaders, while highlighting every modified entity with a red QA/QC verification box.

```text
┌────────────────────────────────────────────────────────┐
│  AutoCAD_Smart_Raw.csv (Exported from Web Bridge)      │
│  ID,Val1,Val2,Val3,Category                            │
│  D1,600,25.0,1000,DRAINAGE                             │
│  MH-01,15.50,13.20,2.30,SEWERAGE                       │
└───────────────────────────┬────────────────────────────┘
                            │ Run CSVUPDATE in CAD
                            ▼
      ┌────────────────────────────────────────────────┐
      │  AutoCAD Drawing Updated:                      │
      │  - D1 pipe text updated to 600mm, 25.0m, 1000  │
      │  - MH-01 multileader updated with GL, IL, DEPTH│
      │  - Red QA/QC bounding boxes drawn on modified  │
      │    entities for fast visual audit!             │
      └────────────────────────────────────────────────┘
```

#### Step-by-Step Instructions:
1. Export the 5-column `AutoCAD_Smart_Raw.csv` from the [Web Bridge](https://nazreennasyuha.github.io/AutoLISP/).
2. In AutoCAD, type `CSVUPDATE` and press Enter.
3. In the file browser dialog, select `AutoCAD_Smart_Raw.csv`.
4. Prompt: `Select TEXT / MTEXT / MULTILEADERS to update (or Enter for ALL):`.
   - Window-select a portion of the drawing, or press Enter to process the entire drawing.
5. **Automatic Ingestion**:
   - Matches Drainage labels by Drain ID and updates diameter, length, and slope.
   - Matches Sewerage Multileaders by Manhole ID and injects Ground Level (`GL`), Invert Level (`IL`), and `DEPTH`.
   - Draws a temporary red bounding box around every updated entity on layer `QAQC-UPDATE` (Color 1 - Red) so you can review changes instantly.

---

### 6. EPANET Pipe Simulation Annotator (`EPALINK`)
**File:** [`LISPS/epalink.lsp`](LISPS/epalink.lsp) | **Command:** `EPALINK`

Parses EPANET hydraulic simulation reports (`.rpt` or `.txt`) and automatically creates rotated engineering callout blocks for water pipes.

```text
     ┌─────┐
    /       \
   │  P-101  │  L=150.00m, Q=18.50lps, Vs=1.25m/s, HL=4.20m/km
    \       /
     └─────┘
  (Magenta Hex)                  (Red Hydraulic Data)
```

#### Step-by-Step Instructions:
1. Type `EPALINK` and press Enter.
2. Select the EPANET `.rpt` or `.txt` simulation report.
3. Enter Text Height (default: `1000.0`).
4. The script displays progress: `[1/45] Click insertion point for Pipe P-101:`.
   - Click insertion point on pipe.
   - Click a second point along the pipe to orient the block (or press Enter to reuse the previous angle).
5. A dynamic block is created with a magenta hexagon containing the Pipe ID, followed by red text displaying Length ($L$), Flow ($Q$), Velocity ($V_s$), and Headloss ($HL$).

---

### 7. EPANET Node Simulation Table Generator (`EPANODE`)
**File:** [`LISPS/epanode.lsp`](LISPS/epanode.lsp) | **Command:** `EPANODE`

Parses EPANET simulation reports and generates 4-row engineering tables for water network junction nodes.

```text
┌───────────────────────┬───────────────────────┐
│ J-101                 │ NODE SUMMARY          │
├───────────────────────┼───────────────────────┤
│ HSL                   │ 45.20m                │
├───────────────────────┼───────────────────────┤
│ Q                     │ 3.50lps               │
├───────────────────────┼───────────────────────┤
│ RP                    │ 22.40m                │
└───────────────────────┴───────────────────────┘
```

#### Step-by-Step Instructions:
1. Type `EPANODE` and press Enter.
2. Select your EPANET `.rpt` or `.txt` simulation report.
3. Enter Text Height (default: `1000.0`).
4. Click the top-left corner where the table should be placed.
5. Click rotation angle (or press Enter for $0^\circ$).
6. A clean, 4-row table block is inserted detailing Node ID, Hydraulic Head (`HSL`), Demand (`Q`), and Residual Pressure (`RP`).

---

### 8. Batch Polyline Area Summation (`GETALLAREAM`)
**File:** [`LISPS/getallaream.lsp`](LISPS/getallaream.lsp) | **Command:** `GETALLAREAM`

Calculates areas for dozens of building platforms or road reserve parcels simultaneously.

#### Step-by-Step Instructions:
1. Type `GETALLAREAM` and press Enter.
2. Window-select all target closed polylines.
3. The script iterates through each polyline, places an individual text annotation (`X.XXX m2`) at its centroid, and sums the total.
4. Total area is printed to the command prompt:
   ```text
   >> Processed 24 polylines. TOTAL AREA: 15420.350 m2
   ```

---

### 9. Multi-Unit Polyline Area Breakdown (`GETAREA`)
**File:** [`LISPS/getarea.lsp`](LISPS/getarea.lsp) | **Command:** `GETAREA`

Provides an instant multi-unit area query for planning and authority submissions.

#### Step-by-Step Instructions:
1. Type `GETAREA` and press Enter.
2. Click any closed polyline.
3. Inserts a centroid text label with square meters, hectares, and acres:
   ```text
   1250.450 m2 / 0.125 ha / 0.309 ac
   ```
4. The full string is automatically copied to your Windows Clipboard.

---

### 10. Dedicated Square Meter Area Extraction (`GETAREAM`)
**File:** [`LISPS/getaream.lsp`](LISPS/getaream.lsp) | **Command:** `GETAREAM`

#### Step-by-Step Instructions:
1. Type `GETAREAM` and click any closed polyline.
2. Injects centered label `X.XXXX m2` at the polygon centroid.
3. Copies the numeric $m^2$ value directly to the Windows Clipboard for instant pasting (`Ctrl+V`) into Excel.

---

### 11. Dedicated Hectare Area Extraction (`GETAREAHA`)
**File:** [`LISPS/getareaha.lsp`](LISPS/getareaha.lsp) | **Command:** `GETAREAHA`

#### Step-by-Step Instructions:
1. Type `GETAREAHA` and click any closed polyline.
2. Injects centered label `X.XXXX ha` at the polygon centroid.
3. Copies the numeric hectare value to the Windows Clipboard.

---

### 12. Dedicated Acre Area Extraction (`GETAREAACRE`)
**File:** [`LISPS/getareaacre.lsp`](LISPS/getareaacre.lsp) | **Command:** `GETAREAACRE`

#### Step-by-Step Instructions:
1. Type `GETAREAACRE` and click any closed polyline.
2. Injects centered label `X.XXXX ac` at the polygon centroid.
3. Copies the numeric acre value to the Windows Clipboard.

---

### 13. Cumulative Path Distance Measurement (`GETLENGTH`)
**File:** [`LISPS/getlength.lsp`](LISPS/getlength.lsp) | **Command:** `GETLENGTH`

Measures complex, non-linear alignment paths (kerbs, fences, pipes) without drawing throwaway polylines.

#### Step-by-Step Instructions:
1. Type `GETLENGTH` and press Enter.
2. Click the start point of your alignment.
3. Click consecutive turn points with real-time rubber-band feedback.
4. Press Enter to end the path.
5. The 2D Euclidean distance (flattening elevation $Z$ to prevent slope distortion) is printed to the command prompt and immediately copied to the Windows Clipboard.

---

### 14. In-Place 180° Entity Reversal (`R180`)
**File:** [`LISPS/r180.lsp`](LISPS/r180.lsp) | **Command:** `R180`

Eliminates the tedious `ROTATE` $\rightarrow$ pick center $\rightarrow$ `180` command cycle when flipping flow arrows, text, and blocks.

#### Step-by-Step Instructions:
1. Type `R180` and press Enter.
2. Click any entity:
   - **Line**: Rotates 180° about its exact 3D midpoint.
   - **Polyline**: Rotates 180° about its vertex bounding box center.
   - **Text / MText / Block**: Rotates 180° about its primary insertion base point.
3. The command remains in an active loop: continue clicking any other reversed entities.
4. Press Enter or Spacebar to exit.

---

### 15. Smart Alphanumeric Sequence Renumbering (`TSEQ`)
**File:** [`LISPS/tseq.lsp`](LISPS/tseq.lsp) | **Command:** `TSEQ`

Quickly renumbers pipe or manhole runs while protecting MText formatting codes, width factors, and engineering parameters.

#### Step-by-Step Instructions:
1. Type `TSEQ` and press Enter.
2. Enter the starting sequence string (e.g. `D1`, `MH01`, `P-100`, or `A`).
3. Click each drain text or manhole Multileader along the run:
   - For Drainage: Updates `{\W0.5;D1;600mm;25m;1000}` $\rightarrow$ `{\W0.5;D2;600mm;25m;1000}`.
   - For Sewerage: Updates `{\W0.5;MH01\PGL00.00\P...}` $\rightarrow$ `{\W0.5;MH02\PGL00.00\P...}`.
4. Automatically retains zero-padding (`01` $\rightarrow$ `02` $\rightarrow$ `...` $\rightarrow$ `10`).
5. Press Enter or Spacebar when finished.

---

## AutoCAD Smart XLS-to-CSV Web Bridge Guide

The [Web Application](https://nazreennasyuha.github.io/AutoLISP/) acts as the intelligence layer between civil design spreadsheets and AutoCAD drawings:

```text
┌─────────────────────────────────┐
│  Engineering Schedule (Excel)   │
│  (.xlsx / .xls / .xlsm run)     │
└────────────────┬────────────────┘
                 │ Drag & drop into browser
                 ▼
┌─────────────────────────────────┐
│   AutoCAD XLS-to-CSV Bridge     │
│  (index.html / React + SheetJS) │
│  - Smart Row Skipper            │
│  - Fuzzy Header Auto-Detection  │
│  - Manhole Deduplication Engine │
└────────────────┬────────────────┘
                 │ Exports 5-column CSV (AutoCAD_Smart_Raw.csv)
                 ▼
┌─────────────────────────────────┐
│     AutoLISP: c:csvupdate       │
│  - Reads 5-column raw data      │
│  - Updates Text & Multileaders  │
│  - Highlights with Red QA/QC Box│
└────────────────┬────────────────┘
                 │
                 ▼
┌─────────────────────────────────┐
│   Production-Ready CAD Drawing  │
└─────────────────────────────────┘
```

### Key Capabilities of the Web Bridge:
1. **Smart Row Skipper**:
   - Engineering spreadsheets frequently feature company letterheads, project titles, client logos, and blank lines before the real table headers appear.
   - The parser automatically scans rows top-down, detecting key civil engineering keywords:
     `manhole no`, `pipe size`, `drain id`, `drain name`, `diameter`, `length`, `gradient`, `grade`, `invert level`, `ground level`.
   - Automatically locks onto the true table header row and ignores all preceding letterhead clutter.
2. **Fuzzy Header Auto-Detection**:
   - Case-insensitive, whitespace-agnostic header mapping.
   - **Drainage Columns**: Maps `ID` ("ID", "Drain Name", "Pipe ID"), `Size` ("Size", "Dia", "Diameter"), `Length` ("Length", "Len"), and `Gradient` ("Gradient", "Grad", "1:").
   - **Sewerage Columns**: Maps `ID` ("MH", "Manhole", "Sewer ID"), `GL` ("GL", "Ground Level"), `IL` ("IL", "Invert Level"), and `Depth` ("Depth").
3. **Smart Manhole Deduplication**:
   - Sewer schedules frequently list the same manhole twice (as the outfall of one line and the inlet of another).
   - When duplicate Manhole IDs are detected, the deduplication engine automatically compares Invert Levels and **keeps the lowest IL (deepest connection)**, recalculating depth accordingly.
4. **Export Engine**:
   - Exports the standardized 5-column file: `AutoCAD_Smart_Raw.csv` (`ID,Val1,Val2,Val3,Category`), ready for direct batch injection by `CSVUPDATE`.

---

## Architecture & Design Principles

1. **Pure Vanilla LISP Standard**: Many modern AutoLISP routines fail in AutoCAD LT or GstarCAD because they rely on ActiveX (`vlax-get-acad-object`, `vla-put-color`). This suite operates strictly on core DXF structures (`entmake`, `entget`, `entmod`), guaranteeing 100% platform portability.
2. **Wildcard Escaping**: AutoCAD's `ssget` filter interprets `#`, `@`, `*`, `?`, and `~` as wildcard tokens. Layers beginning with civil prefixes (such as `#JRK - RD Drain Line`) are escaped automatically (`#` $\rightarrow$ `` `# ``) to prevent selection failures.
3. **Robust Error Trapping**: Every command features a localized `*error*` handler. If a drafter hits `ESC` halfway through a command, active commands are closed, temporary UCS rotations are undone, and all system variables are restored to their original states.

---

## Customization & Layer Configuration

Every script features an isolated, easily editable `USER SETTINGS` header block at the top of the file. You do not need to read through the execution logic to adjust layers, colors, text styles, or dimensioning rules:

```lisp
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq unitsPerM 1000.0)      ;; Drawing units per metre (1000 = mm, 1.0 = m)
  (setq maxSegM   30.0)        ;; Max pipe run (m) before auto-inserting sump

  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)          ;; 4 = Cyan

  (setq sumpLayer "#JRK - RD Drain Manhole_Sump")
  (setq sumpColor 4)
  (setq sumpRad 600.0)         ;; Sump radius (mm)

  (setq textLayer "#JRK - RD Drain Text")
  (setq textColor 7)           ;; 7 = White / Black
  (setq txtHgt 2000.0)
  (setq txtStyle "1000-T2")
  (setq txtWidth 0.5)

  (setq mlLayer     "#JRK - RD Drain Text IL")
  (setq mlStyle     "1000-T2")         ;; Multileader style
  (setq mlText      "{\\W0.5;SIL00.00}")
  (setq mlJustify   "Right")           ;; "Right" or "Left"
  (setq mlSide      "Right")           ;; "Left" (+90) or "Right" (-90)
  (setq mlLeadLen   3000.0)            ;; Leader offset from sump rim
  (setq mlLandDist  2000.0)            ;; Landing distance
  (setq mlArrowSize 2000.0)            ;; Arrowhead size
  (setq mlLandGap   1000.0)            ;; Landing gap
  ;; =========================================================================
```

Simply update the values to match your organization's CAD layer standards.

---

## Contributing

Contributions, bug reports, and civil engineering feature requests are welcome!
1. Fork the Project.
2. Create your Feature Branch (`git checkout -b feature/HydraulicProfile`).
3. Commit your Changes (`git commit -m 'Add hydraulic long-section generator'`).
4. Push to the Branch (`git push origin feature/HydraulicProfile`).
5. Open a Pull Request.

---

## License

Distributed under the **MIT License**. See [`LICENSE`](LICENSE) for more information.

---

**Developed & Maintained by [Nazreen Nasyuha](https://github.com/NazreenNasyuha)**  
*Specializing in Infrastructure Engineering, Hydraulic Modeling & CAD Automation.*
