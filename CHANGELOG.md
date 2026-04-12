# Changelog

All notable changes to RNAcross are documented in this file.

---

## [3.0.0] — 2026-04-12

### Added

- **"What's New" Version Modal**
  - A pop-up now appears after the splash screen to inform users of new features, bug fixes, and changes.
  - Only shows once per version — returning users won't see it again unless a new version is released.
  - A bullhorn icon (📢) in the navbar lets users reopen it at any time.
  - Content is fully driven by a single config file (`R/00_version_init.R`) — no hardcoded text elsewhere.

- **Universal Plot Export System**
  - Every plot in the app now has a **Download Plot** button that opens an export modal.
  - Users can choose **format** (PNG, JPEG, PDF, SVG), **dimensions** (width × height in inches), and **resolution** (DPI).
  - Works reliably on both **Windows** and **macOS** with no extra software required.

### Changed

- **How downloads work under the hood**
  - Plots rendered with **ggplot2** (PCA, Ridgeline, Gene Group publication heatmaps) are generated server-side and delivered as a direct file download.
  - Plots rendered with **Plotly** (single-species expression, combined expression, ortholog heatmap, gene group interactive) are exported client-side using Plotly's built-in image export — no dependency on external tools like Orca or Kaleido.
  - PDF and SVG are available for ggplot-based plots. Plotly-based plots support PNG, JPEG, and SVG.

- **Download buttons** across all tabs were standardized to use the same modal workflow.

### Fixed

- Plot downloads no longer save as empty HTML files on Windows.
- Export modal is now vertically centered on screen.
- "Start Fresh" (session reset) correctly re-triggers the version modal for first-time viewing.
