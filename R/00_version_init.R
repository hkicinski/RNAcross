app_version_history <- list(
  "3.1.2" = list(
    version      = "3.1.2",
    release_date = "2026-06-09",
    title        = "S. cerevisiae 2026 Datasets",
    message      = c(
      "Integrated the new 2026 S. cerevisiae sequencing datasets (WT and Δppx1 Δppn1) and added a global UI toggle to switch between them and the 2023 data, alongside new contrast modes for side-by-side comparison."
    ),
    features   = c(
      "S. cerevisiae Dataset Toggle: Added a new option in the Plot Settings modal under Expression Data Source to globally toggle between the 2023 data, WT S288C 2026 (yH545) data, and Δppx1 Δppn1 S288C 2026 (yH1053) data.",
      "Contrast Modes: Side-by-side comparison modes (2026 WT vs Mutant, and 2023 WT vs 2026 WT) available with optional display scaling (Z-score, Center).",
      "Dataset Persistence: The selected S. cerevisiae dataset is now cached in your browser session and will persist across app reloads."
    ),
    bug_fixes  = c(),
    show_changelog = TRUE,
    changelog_url  = "https://github.com/hkicinski/RNAcross/blob/main/CHANGELOG.md",
    show_tutorial  = FALSE
  ),
  "3.1.1" = list(
    version      = "3.1.1",
    release_date = "2026-05-29",
    title        = "Legend Formatting & Settings Patches",
    message      = c(
      "A minor patch adding requested features to the Interactive Aesthetic Editor and fixing several edge case bugs where custom settings would revert."
    ),
    features   = c(
      "Legend Customization in the Interactive Aesthetic Editor: The legend can now be repositioned with the editor's X/Y sliders staying in sync.",
      "Legend Font Control: Full control over font family, size, color, and bold for both the legend title and item labels independently."
    ),
    bug_fixes  = c(
      "Per-Gene Line Colors Reverting: In the Multi-Gene and Similarity tabs, custom per-gene line colors no longer revert to defaults when the Plot Settings modal is reopened.",
      "Aesthetic Editor Controls: Legend X/Y, orientation, and background controls now properly update the plot.",
      "Aesthetic Editor Reverting: Editor changes no longer reset when clicking elsewhere or reopening the editor. Plot re-renders are decoupled from selection state.",
      "Aesthetic Editor Bold: Legend bold now applies reliably using HTML markup."
    ),
    show_changelog = TRUE,
    changelog_url  = "https://github.com/hkicinski/RNAcross/blob/main/CHANGELOG.md",
    show_tutorial  = FALSE
  ),
  "3.1.0" = list(
    version = "3.1.0",
    release_date = "2026-05-26",
    title = "App Updates",
    message = c("This release focuses on major feature additions for RNAcross, along with UI bug fixes."),
    features = c(
      "YGOB + CGOB Pillars Search: Orphan genes or genes lacking Orthologous Group (OG) assignments can now be discovered. The search engine uses a fallback structure (gene_lookup table -> synteny data -> true orphans), and the gene query hub reports the specific status of each gene.",
      "Similarity Search: Search and overlay genes with similar temporal expression profiles (shapes) across single or multiple species. Results include rigorous statistical metrics: Pearson R, permutation P-values, and null percentiles.",
      "Interactive Plot Styling: Debuted a click-and-customize feature using Plotly.js for the Gene Group Analysis line plot, allowing on-the-fly aesthetic editing of plot traces."
    ),
    bug_fixes = c(
      "Splash Screen Logo Fix: Fixed letterboxing and black bars on non-16:9 screens by switching SVG scaling from 'meet' to 'slice' for a full-bleed splash screen.",
      "Species Formatting: Ensured species names properly follow binomial nomenclature (italics) across all outputs and UI elements."
    ),
    show_changelog = TRUE,
    changelog_url = "https://github.com/hkicinski/RNAcross/blob/main/CHANGELOG.md",
    show_tutorial = FALSE
  ),
  "3.0.0" = list(
    version = "3.0.0",
    release_date = "2026-04-12",
    title = "App Updates",
    message = c("This release introduced the version modal and a universal plot export system, and standardized how downloads work across the app."),
    features = c(
      "What's New Version Modal: A pop-up now appears after the splash screen to inform users of new features and fixes, with a bullhorn icon to reopen it anytime.",
      "Universal Plot Export System: Every plot now has a Download Plot button offering format (PNG/JPEG/PDF/SVG), dimensions, and resolution (DPI), working on both Windows and macOS with no extra software."
    ),
    bug_fixes = c(
      "Plot downloads no longer save as empty HTML files on Windows.",
      "Export modal is now vertically centered on screen.",
      "'Start Fresh' (session reset) correctly re-triggers the version modal for first-time viewing."
    ),
    show_changelog = TRUE,
    changelog_url = "https://github.com/hkicinski/RNAcross/blob/main/CHANGELOG.md",
    show_tutorial = FALSE
  )
)

# Backward-compat: existing code references app_version_info
app_version_info <- app_version_history[[1]]   # newest entry

build_version_entry <- function(info) {
  content <- tagList(
    tags$h5(sprintf("v%s — %s", info$version,
                    format(as.Date(info$release_date), "%B %d, %Y"))),
    lapply(info$message, p)
  )
  if (length(info$features) > 0)
    content <- tagList(content, h5(icon("star"), " New Features"),
                       tags$ul(lapply(info$features, tags$li)))
  if (length(info$bug_fixes) > 0)
    content <- tagList(content, h5(icon("wrench"), " Bug Fixes"),
                       tags$ul(lapply(info$bug_fixes, tags$li)))
  if (isTRUE(info$show_tutorial))
    content <- tagList(content,
      actionButton("launch_tutorial_from_modal",
        label = tagList(icon("question-circle"), " Guided Tutorial"),
        class = "btn-primary mt-2"))
  content
}

show_plot_export_modal <- function(download_id, title = "Export Plot",
                                   default_width = 10, default_height = 8,
                                   formats = c("PNG" = "png", "JPEG" = "jpeg",
                                               "PDF" = "pdf", "SVG" = "svg")) {
  showModal(modalDialog(
    title = title,
    selectInput(
      paste0(download_id, "_format"), "Format:",
      choices = formats,
      selected = formats[[1]]
    ),
    numericInput(
      paste0(download_id, "_width"), "Width (inches):",
      value = default_width, min = 1, max = 50
    ),
    numericInput(
      paste0(download_id, "_height"), "Height (inches):",
      value = default_height, min = 1, max = 50
    ),
    numericInput(
      paste0(download_id, "_dpi"), "Resolution (DPI):",
      value = 300, min = 72, max = 1200
    ),
    tags$small(
      class = "text-muted",
      "DPI applies to PNG and JPEG only. PDF and SVG are vector formats."
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        paste0(download_id, "_confirm"), "Download",
        icon = icon("download"), class = "btn-primary"
      )
    ),
    easyClose = TRUE
  ))
}
