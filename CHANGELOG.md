# Changelog

All notable changes to RNAcross are documented in this file.

## [3.1.3] - 2026-07-03

### Added

- **Fixed Y-Axis Range Control**
  - You can now lock the Y-axis (expression) range on the line plots so figures stay on a consistent scale for comparison.
  - **Point-and-click:** clicking directly on the Y-axis of a Gene Group line plot opens the Interactive Aesthetic Editor with a Y-Axis Range slider, seeded to the plot's current range and updating it live.
  - **Global defined setting:** a new "Fixed Y-axis Range" section in the Plot Settings modal (Line Plots tab) accepts an exact Y min / Y max that applies across the Gene Group, Single Species, and Comparative line plots, both on screen and in their exports, for users who want a consistent axis across all of their plots.

### Fixed

- **Gene Group Analysis: "object 'sc_anno' not found"**
  - Fixed an error (reported by lab members) that crashed the Gene Group Analysis when a queried gene fell back to the synteny-aided search, most visible when analyzing S. cerevisiae gene sets. A leftover lookup call was removed and a species-code reference in the synteny grouping was corrected, resolving the same latent crash across every module that uses the gene query.

- **Gene Group Publication Plot Export**
  - The publication-mode export of the Gene Group heatmap no longer errors. The export path now mirrors the on-screen renderer: the color scale, timepoint handling, and gene-category annotations are all defined before drawing, and the panels are no longer built twice.

### Changed

- **Automatic Dependency Installation**
  - On a fresh machine, the app now checks for and installs any missing packages on startup, including the Bioconductor-only ones (ComplexHeatmap, ggtree, treeio), so the app launches without first hunting down "there is no package called ..." errors.

## [3.1.2] - 2026-06-09

### Added
  - Integrated the new 2026 S. cerevisiae sequencing datasets (WT and Δppx1 Δppn1).
  - Added a global UI toggle in the Plot Settings modal to seamlessly switch between the 2023 data, WT S288C 2026 (yH545), and Δppx1 Δppn1 S288C 2026 (yH1053) datasets.
  - Contrast Modes: Side-by-side comparison modes (2026 WT vs Mutant, and 2023 WT vs 2026 WT) available with optional display scaling (Z-score, Center).
  - The S. cerevisiae dataset selection is saved in the browser session (`localStorage`), meaning users do not have to re-select it upon reloading the app.
  - Plugged the new data to the main `get_species_data()` function that underlies data retrieval; selects what users toggle upon request
  - Redesigned the intro splash: clicking the yeast cells now grows a phylogenetic cladogram upward from the click point, one branch generation per click, replacing the previous RNA-helix reveal. Added a new click sound effect.

## [3.1.1] - 2026-05-29

### Added

- **Legend Customization in the Interactive Aesthetic Editor**
  - The legend can now be repositioned with the editor's X/Y sliders staying in sync.
  - Added full legend font control: font family, size, color, and bold.
  - Font styling can be applied independently to the **legend title** ("Gene") and the **legend item labels** (the gene names), or to both.

### Fixed

- **Per-Gene Line Colors Reverting (Plot Settings)**
  - In the Multi-Gene / Gene Group settings, custom per-gene line colors no longer revert to defaults when the Plot Settings modal is reopened (Thanks Jess). The pickers now read the saved colors reactively (defaults are populated outside the renderer), and a palette observer no longer overwrites customized colors on reopen. The same fix was applied to the Similarity per-gene color pickers.

- **Aesthetic Editor: Legend Controls Had No Effect**
  - The legend X position, Y position, orientation, and background controls now actually update the plot (the server-side handlers were missing).

- **Aesthetic Editor: Edits Reverting to Default**
  - Editor changes no longer reset when clicking elsewhere on the plot or when reopening the editor. Plot re-renders are decoupled from editor selection state, manual interactions are preserved across redraws (`uirevision`), and each control is now seeded from the element's current live values so reopening reflects and re-applies the existing styling instead of defaults!!!!

- **Aesthetic Editor: Bold Not Working**
  - Legend bold now applies reliably. Bolding uses HTML markup (legend title text and trace names) which Plotly.js reliably honors.
---

## [3.1.0] - 2026-05-26

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

- **Enter button functionality**
  - Everyone knows that a good app allows you to press enter to initialize... and our's does too, now.
  - Pressing enter while typing in a search or input box triggers that tab's primary action; i.e., searching a gene, analyzing a gene group, running PCA, or generating a heatmap/ridgeline
  - Uses the same global key listener most apps use via JS. (Our style is thanks to Kryptech YouTube where I got the execution from) 

### Changed

- **Species Binomial Nomenclature**
  - Species names now appear in italics to properly follow binomial nomenclature on all outputs where species names are reported.

- **Plotlys as Javascript elements**
  - Plotlys are now directly rendered as Java elements. There is no R redraw via ggplot.
  - Allows for added features and customization since controls are seeded from the plotly render, meaning the output reflects custom states rather than ggplot to plotly defaults

### Fixed

- **Splash Screen Logo Aspect Ratio**
  - Fixed an issue where the logo wouldn't stretch fully across the screen on non-16:9 displays, resulting in letterboxing (black/blue bars).
  - The SVG already had `preserveAspectRatio="xMidYMid slice"` in its source file, but the `initializeSplash()` JavaScript was overriding it to `meet`. The JavaScript now correctly uses `slice`, ensuring full bleed and no gaps regardless of screen dimensions.

---

## [3.0.0] - 2026-04-12

### Added

- **"What's New" Version Modal**
  - A pop-up now appears after the splash screen to inform users of new features, bug fixes, and changes.
  - Only shows once per version; returning users won't see it again unless a new version is released.
  - A bullhorn icon (📢) in the navbar lets users reopen it at any time.
  - Content is fully driven by a single config file (`R/00_version_init.R`), with no hardcoded text elsewhere.

- **Universal Plot Export System**
  - Every plot in the app now has a **Download Plot** button that opens an export modal.
  - Users can choose **format** (PNG, JPEG, PDF, SVG), **dimensions** (width × height in inches), and **resolution** (DPI).
  - Works reliably on both **Windows** and **macOS** with no extra software required.

### Changed

- **How downloads work under the hood**
  - Plots rendered with **ggplot2** (PCA, Ridgeline, Gene Group publication heatmaps) are generated server-side and delivered as a direct file download.
  - Plots rendered with **Plotly** (single-species expression, combined expression, ortholog heatmap, gene group interactive) are exported client-side using Plotly's built-in image export.
  - PDF and SVG are available for ggplot-based plots. Plotly-based plots support PNG, JPEG, and SVG.

- **Download buttons** across all tabs were standardized to use the same modal workflow.

### Fixed

- Plot downloads no longer save as empty HTML files on Windows.
- Export modal is now vertically centered on screen.
- "Start Fresh" (session reset) correctly re-triggers the version modal for first-time viewing.
