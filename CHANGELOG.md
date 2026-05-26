# Changelog

All notable changes to RNAcross are documented in this file.

---

## [3.1.0] — 2026-05-26

### Added

- **YGOB + CGOB Pillars Search Integration**
  - Orphan genes, or genes not assigned to an Orthologous Group (OG), can now be found!
  - Developed a new search engine structure: Finding an OG assignment via the `gene_lookup` table goes first, synteny YGOB + CGOB (in Rdata) goes second, and a final fallback displays all genes that are truly orphaned.
  - Redid the gene plotting functions to work with this new query structure.
  - Restructured the Gene Query Hub to account for the new search hierarchy; it will now report the specific status of the gene (e.g., OG-assigned, syntenic match, or true orphan).

- **Similarity Search Module**
  - Search and overlay genes with similar temporal expression profiles across species!
  - Implements shape-search to find profile similarities based on timepoint progression.
  - Included statistical rigor: results show Pearson correlation coefficients, null percentiles, and permutation-based P-values.
  - In single-species similarity mode, the query gene is exclusively plotted as a black dashed line and is filtered out from the result set.
  - Removed species abbreviations (e.g., `(cg)`) from the legend specifically in the similarity search module to reduce clutter.
  - Tooltips added directly to the column headers in the data table for quick synopsis of statistical metrics upon hovering.

- **Interactive Plotly Styling**
  - Debuted a new feature using plotly.js to allow for click-and-customize features on the gene expression plots.
  - Currently available exclusively for the Gene Group Analysis line plot. 

### Changed

- **Species Binomial Nomenclature**
  - Species names now appear in italics to properly follow binomial nomenclature on all outputs where species names are reported.

### Fixed

- **Splash Screen Logo Aspect Ratio**
  - Fixed an issue where the logo wouldn't stretch fully across the screen on non-16:9 displays, resulting in letterboxing (black/blue bars).
  - The SVG already had `preserveAspectRatio="xMidYMid slice"` in its source file, but the `initializeSplash()` JavaScript was overriding it to `meet`. The JavaScript now correctly uses `slice`, ensuring full bleed and no gaps regardless of screen dimensions.

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
