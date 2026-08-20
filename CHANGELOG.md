# Changelog

All notable changes to RNAcross are documented in this file.

## [Unreleased]

## [3.2.0] - 2026-08-19

### Added

- **Search-first navigation: one bar, and a command palette**
  - The nine-tab strip and the title row beneath it are now a single 60px bar, halving the space above the tab content.
  - `Ctrl+K` (`⌘K` on macOS) opens a command palette that takes both gene queries and tool names. Type `PHO81` and press Enter to run the orthogroup query, or `heatmap` to jump to the Cross-Species Heatmap. Arrow keys move, Escape closes, `⌘Enter` opens in a new browser tab.
  - Gene suggestions are matched server-side, prefix first, one row per orthogroup, with the gene ID and species coverage.
  - The three tools you last used stay on the bar as chips, alongside an "All tools" grid.
  - The gene in scope sits in the search field itself with its HOG accession and coverage badge, an `x` to clear it, and an Export button beside it.
  - The Gene Query Hub panel is retired; its search is now in the bar and available from every tab.

- **Gene Explorer landing states**
  - With no cached query: a launchpad of all nine tools, your recent genes, and the dataset in scope.
  - With a cached query: a resume card offering your last gene back with its orthogroup, coverage and age, one click from the Gene Explorer, Comparative View, or Find Similar Profiles. "Start fresh" clears the gene without wiping the rest of your session.
  - Example gene chips are validated against the loaded data at startup, so an upload never shows a dead chip.

- **Design-agnostic data handling (scale-aware dispatch)**
  - The app no longer assumes the primary axis is a time course. A dose series, a stage series, or a genotype panel now orders, standardizes, and normalizes correctly, driven by the uploaded study design instead of by parsing time units out of the labels.
  - New design accessors: `condition_column_in()` / `condition_of()` (read the axis from whatever column the user named it), `condition_positions()` (declared numeric spacing for an interval axis, level rank otherwise), `condition_order()`, and `condition_codes()`.
  - Uploads keep their own column names. Ingest reads the declared axis column rather than requiring one literally called `Timepoint`, and only `Sample` is now a required column in sample metadata; a missing `Replicate` is a warning, not an error.
  - Axis titles and the PCA legend now read the design's label, so a dose plot is labelled "Dose", not "Timepoint".
  - Analyses dispatch on the axis scale: interpolation in the similarity search only runs on an interval axis (a nominal axis matches levels by label instead), and PCA trajectory arrows and short axis codes are suppressed when the axis has no meaningful order.
  - The heatmap's standardized (T01-T10) axis and its raw-column ordering both follow the design's level order. Previously every dose label parsed to 0 minutes and collapsed into a single column.

- **Gene tree upload (Step 5b)**
  - Uploaded datasets can now supply the newick gene trees the Gene Explorer draws. Previously trees existed only inside the bundled RData, so an uploaded dataset could never show one.
  - Accepts an OrthoFinder `Gene_Trees/` folder zipped, loose `.nwk` / `.txt` files named after their orthogroup, or a two-column table of orthogroup and newick, which is the easiest shape to produce when fetching trees from an external orthology resource rather than running OrthoFinder.
  - Validation reports how many trees parsed and how many match an uploaded orthogroup. Unparseable input is skipped with a warning rather than failing the upload, and an orthogroup with no tree still degrades to the existing "no tree available" panel.

- **Data Upload refreshed for design-agnostic data**
  - Step 3 no longer claims `Timepoint` and `Replicate` are required. It now states the real contract: `Sample` plus one column describing each sample, named whatever you call it, with `Replicate` optional.
  - Validation reports the condition-column candidates it found per species, so it is clear which column you are being asked to pick.
  - New Step 6, "Describe Your Design", with a button that jumps to the wizard and a live status panel showing the axis, its scale type, its level count, and its baseline. Validating without a design now sends you to that step rather than straight to Process.

### Fixed

- **A failed gene search left the Gene Explorer blank**
  - The results panel was hidden but the previous hit stayed in memory, so the tab went empty. A failed search now clears the previous result, returns to the launchpad, and reports "not found" in the search bar.

- **Duplicated and conflicting layout rules**
  - `.footer` was defined twice with contradictory positioning, and 80px of page padding was reserved for a footer that is not fixed. Removed, and tab panes are now measured against the real chrome height.

- **Uploads processed without a design silently used the stock time course**
  - `current_study_design()` falls back to the built-in GRE design, so processing custom data before applying a design quietly factored it against `0min ... 8h`, producing empty plots with no error. Processing now infers a design from the sample metadata instead: it picks the condition column every species shares, decides interval versus nominal from whether the level labels carry a numeric progression, orders the levels by that progression, takes the first level as the baseline, and selects orthology or identity alignment from the species count. The inference is announced and flagged in the status panel so it is never mistaken for a design you chose.

- **Publication-mode PCA failed to draw with labels enabled**
  - The trajectory and label layers reused the plot's global aesthetics, which carry `text = Sample` for the plotly tooltip, but their own summarised data has no `Sample` column. The result was "object 'Sample' not found" and a blank canvas. Both layers now set `inherit.aes = FALSE`.

- **Unreplicated uploads crashed the similarity search**
  - `build_consensus_wide()` grouped by a `Replicate` column, so sample metadata without one failed with "Column `Replicate` is not found". Replicates are now read through `replicate_of()`, which synthesizes a single level when the column is absent, and every long-format frame carries one either way. Sample sheets may now omit `Replicate` entirely, or supply it for some species and not others.

- **Consensus matrix column order for non-time axes**
  - `build_consensus_wide()` laid columns out in group order, which is alphabetical for a character axis, so a dose series came back as 0uM, 100uM, 10uM, 1uM. The axis is now ordered before grouping. Time courses were unaffected, since their labels were already factors.

- **Find Similar Profiles: editable plot + ggprism publication export**
  - The similarity overlay is now wired into the Interactive Aesthetic Editor, like the Gene Group plot. Click the title, either axis, the gridlines, the plot background, a trace, or the legend to edit it in place.
  - The legend is fully editable: position (sliders or drag), orientation, background, and independent font family / size / color / bold for the legend title and the item labels. The legend now carries an editable title ("Gene"), and clicking a legend entry selects that series so its color, width, dash, marker size, and display mode can be edited from the legend itself.
  - **Edits carry into the publication figure.** Every edit is recorded semantically (per plot, per trace name) and replayed onto the ggprism panels, so the "Publication (ggprism)" tab and both of its exports (trajectory, null distribution) reproduce what was built on screen: titles and axis labels, fonts, axis-line and gridline styling, Y-axis range, legend placement and typography, background colors, and per-series color / line width / line style / marker size.
  - Edits also survive a re-render of the interactive plot (changing settings or re-running a search no longer wipes them).

- **Phylogenetic tree: aesthetic editor + export**
  - The Gene Explorer tree now has an "Edit appearance" panel covering the title (text, size, color, bold, show/hide), tip labels (size, alignment, label space, color by species or a single color), the species tip colors, branches (color, width), node points (show, color, size), the legend (show, position, title, font sizes, text color), and the plot background.
  - Added "Export tree", offering PNG / JPEG / PDF / SVG with width, height, and DPI, and rendering exactly the styling shown on screen. The default export height scales with the number of tips.
  - The species tip colors are the app-wide species colors, so editing them here keeps the tree consistent with every other plot (and stays in sync with the Plot Settings pickers).

- **Standard RNAcross species colors**
  - The four-species set now defaults to fixed hues instead of palette-derived ones: *S. cerevisiae* `#377EB8` (blue), *C. glabrata* `#E41A1C` (red), *C. albicans* `#4DAF4A` (green), *K. lactis* `#FF7F00` (orange). A new "RNAcross Standard" entry in the species palette dropdown restores them at any time; the other palettes behave as before.

### Fixed

- **Phylogenetic tree fails to draw on ggplot2 4.0+**
  - ggplot2 4.0 removed the internal `is.waive()` that ggtree still calls when tip labels are aligned, so the Gene Explorer tree died with "Problem while converting geom to grob" (and took the tree export with it). The app now supplies the function only when it is missing, which matters most on fresh installs, since startup auto-installs the newest ggplot2.

- **Phylogenetic tree rendering**
  - Tip labels are no longer clipped: the horizontal space reserved for them is now derived from the drawn tree, the longest label, and the label font size, instead of a fixed constant ("Label space" in the editor scales it further).
  - Replaced three-digit hex colors (`#999`, `#ddd`, `#444`, `#666`) in the tree theme, which R's graphics engine rejects as an invalid RGB specification.

- **Aesthetic Editor: controls no longer push defaults onto the plot**
  - The editor's controls are now seeded from the clicked element's live values instead of hardcoded defaults, so opening the panel (or clicking a second element) no longer silently restyled the plot to Arial 12.
  - Axis edits now address the correct plotly axis key, so axis title text, font, and color changes apply where previously only the Y-axis range did.
  - The Display Mode and Marker Size controls, which had no effect, are now wired up.

### Changed

- **S. cerevisiae 2026 data reprocessed per strain** (`data/07312026-updated.RData`, replaces `06092026-updated.RData`)
  - The 2026 Scer data was previously stored as one jointly-normalized 65-sample matrix mixing WT S288C (yH545) with the Δppx1 Δppn1 double KO (yH1053), so the two strains shared TMM factors and a single rlog fit. Each strain is now normalized independently, following the same pipeline (`rowSums(counts >= 10) >= 2` → TMM → log2CPM; DESeq2 `rlog(blind = FALSE)` on `~ condition + timepoint`, filtered to `rowSums(counts) >= 10`).
  - The 2026 WT set is now the unsuffixed standard: `sc_anno`, `sc_lcpm`, `sc_rlog`, `sc_sample_info`. The double KO carries a `_KO` suffix. The 2023 data is unchanged and stays year-suffixed as `sc_*_2023`.
  - `sc_anno` extends the 2023 S288C annotation with the 202 gene IDs present in the 2026 matrices but absent from it.
  - No app features changed. The dataset toggle (2023 / yH545 / yH1053) and both contrast modes behave as before; contrast views now intersect gene sets per matrix, since the separately-filtered strains no longer share a gene universe.
  - Gene query, synteny aid, orthogroup lookup, and the 2023 data path are untouched.

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
