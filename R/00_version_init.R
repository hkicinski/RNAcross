app_version_info <- list(
  version      = "3.0.0",
  release_date = "2026-04-12",
  title        = "Welcome! App Updates",
  message      = c(
    "This release focuses on streamlining the RNAcross update scope. For each new update, users will have a chance to see any new bug fixes or feature changes made to the app!",
    "You may be thinking: why version 3.0.0? Many changes have been made since the inception of this app. Now, I am making sure to document each so that you know each capability!"
  ),
  features   = NULL,
  bug_fixes  = c(
    "Download buttons now functional with configurable resolution, dimensions, and file type selection (.jpeg, .png, .pdf)"
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
