app_version_info <- list(
  version      = "3.1.0",
  release_date = "2026-05-26",
  title        = "Welcome! App Updates",
  message      = c(
    "This release focuses on feature release for RNAcross, as well as bug fixes prevailing on the UI. Given the amount of new content, the app will now be in version 3.1.0."
  ),
  features   = c(
    "YGOB + CGOB Pillars Search: Orphan genes or genes lacking Orthologous Group (OG) assignments can now be discovered. The search engine uses a fallback structure (gene_lookup table → synteny data → true orphans) and the gene query hub will report the specific status of the gene.",
    "Similarity Search: Search and overlay genes with similar temporal expression profiles (shapes) across single or multiple species. Results include rigorous statistical metrics including Pearson R, permutation P-Values, and null percentiles.",
    "Interactive Plot Styling: Debuted a new click-and-customize feature using Plotly.js for the Gene Group Analysis line plot, allowing on-the-fly aesthetic editing of plot traces."
  ),
  bug_fixes  = c(
    "Splash Screen Logo Fix: Fixed letterboxing and black bars on non-16:9 screens by changing SVG scaling behavior from 'meet' to 'slice', ensuring a full-bleed splash screen regardless of display size.",
    "Species Formatting: Ensured species names properly follow binomial nomenclature (italics) across all outputs and UI elements."
  ),
  show_changelog = TRUE,
  changelog_url  = "https://github.com/hkicinski/RNAcross/blob/main/CHANGELOG.md",
  show_tutorial  = FALSE
)

build_version_modal <- function(info) {
  content <- tagList(
    lapply(info$message, p)
  )

  if (!is.null(info$features) && length(info$features) > 0) {
    content <- tagList(
      content,
      h5(icon("star"), " New Features"),
      tags$ul(lapply(info$features, tags$li))
    )
  }

  if (!is.null(info$bug_fixes) && length(info$bug_fixes) > 0) {
    content <- tagList(
      content,
      h5(icon("wrench"), " Bug Fixes"),
      tags$ul(lapply(info$bug_fixes, tags$li))
    )
  }

  buttons <- list()

  if (isTRUE(info$show_changelog)) {
    buttons <- c(buttons, list(
      tags$a(
        href = info$changelog_url,
        target = "_blank",
        class = "btn btn-outline-secondary",
        icon("scroll"), " View Change Log"
      )
    ))
  }

  if (isTRUE(info$show_tutorial)) {
    buttons <- c(buttons, list(
      actionButton(
        "launch_tutorial_from_modal",
        label = tagList(icon("question-circle"), " Guided Tutorial"),
        class = "btn-primary"
      )
    ))
  }

  if (length(buttons) > 0) {
    content <- tagList(
      content,
      div(
        class = "d-flex gap-2 justify-content-end mt-3",
        tagList(buttons)
      )
    )
  }

  modal <- modalDialog(
    content,
    title = tagList(icon("bullhorn"), sprintf(" %s — v%s", info$title, info$version)),
    easyClose = TRUE,
    size = "m",
    footer = modalButton("Got it!")
  )
  modal$children[[1]]$attribs$class <- paste(
    modal$children[[1]]$attribs$class, "modal-dialog-centered"
  )
  modal
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
