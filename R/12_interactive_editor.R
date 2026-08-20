#' Interactive Plotly Aesthetic Editor Server Logic
#'
#' Edits are also recorded in plot_styles[[plot_id]] for re-render replay and ggprism export.

#' Initialize Interactive Editor module
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
setup_interactive_editor <- function(input, output, session) {

  # State for custom aesthetics (isolated per-session)
  rv_plot_aesthetics <- reactiveValues(
    current_element = NULL,
    current_axis_id = NULL,
    current_plot_id = NULL,
    current_trace_name = NULL,
    current_trace_index = NULL,
    current_values = list(),   # live values of the clicked element, for seeding controls
    styles = list(), # Holds the semantic keyed styles
    plot_styles = list(),      # edits per plot id, for replay + ggprism export
    seeds = list(),            # values the controls were seeded with
    sel_rev = 0,               # bumped per click; the panel rebuilds on this
    legend_visible = TRUE,     # tracks current legend visibility for the toggle
    legend_margin_r = NULL     # right margin captured while legend is visible, for restore
  )

  editor_fonts <- c("Arial", "Times New Roman", "Courier New", "Verdana",
                    "Tahoma", "Helvetica", "Open Sans")

  #clicked axis id ("x", "y2") -> plotly layout key ("xaxis", "yaxis2")
  editor_axis_key <- function(ax, el = NULL) {
    if (is.null(ax) || !nzchar(ax)) ax <- if (!is.null(el) && startsWith(el, "y")) "y" else "x"
    if (grepl("^(x|y)axis", ax)) return(ax)
    sub("^([xy])", "\\1axis", ax)
  }

  strip_bold <- function(x) if (is.null(x)) NULL else gsub("^<b>|</b>$", "", x)

  #control values seeded from the element's live plotly values, so opening the panel is a no-op
  editor_seeds <- function(el, cv, trace_name) {
    s <- list()
    if (grepl("axis", el) || grepl("grid", el)) {
      is_grid <- grepl("grid", el)
      s$editor_axis_width <- if (is_grid) cv$axis_grid_width %||% 1 else cv$axis_line_width %||% 1
      s$editor_axis_color <- if (is_grid) cv$axis_grid_color %||% "#CCCCCC" else cv$axis_line_color %||% "#000000"
      s$editor_axis_fontfamily <- cv$axis_title_family %||% "Arial"
      s$editor_axis_fontsize   <- cv$axis_title_size %||% 12
      s$editor_axis_fontcolor  <- cv$axis_title_color %||% "#000000"
      s$editor_axis_fontbold   <- isTRUE(cv$axis_title_bold)
      s$editor_axis_text       <- strip_bold(cv$axis_title_text) %||% trace_name %||% ""
      yr <- suppressWarnings(as.numeric(unlist(cv$yaxis_range %||% cv$axis_range)))
      if (length(yr) < 2 || any(!is.finite(yr))) yr <- c(0, 15)
      s$editor_yaxis_range <- yr[1:2]
    } else if (el == "legend") {
      s$editor_legend_orientation <- cv$legend_orientation %||% "v"
      s$editor_legend_x  <- cv$legend_x %||% 1.05
      s$editor_legend_y  <- cv$legend_y %||% 1
      s$editor_legend_bg <- cv$legend_bgcolor %||% "transparent"
      s$editor_legend_title_text       <- cv$legend_title_text %||% ""
      s$editor_legend_title_fontfamily <- cv$legend_title_fontfamily %||% "Arial"
      s$editor_legend_title_fontsize   <- cv$legend_title_fontsize %||% 12
      s$editor_legend_title_fontcolor  <- cv$legend_title_fontcolor %||% "#000000"
      s$editor_legend_title_bold       <- isTRUE(cv$legend_title_bold)
      s$editor_legend_item_fontfamily  <- cv$legend_item_fontfamily %||% "Arial"
      s$editor_legend_item_fontsize    <- cv$legend_item_fontsize %||% 12
      s$editor_legend_item_fontcolor   <- cv$legend_item_fontcolor %||% "#000000"
      s$editor_legend_item_bold        <- isTRUE(cv$legend_item_bold)
    } else if (el == "title" || el == "text_element") {
      s$editor_title_text       <- strip_bold(cv$title_text) %||% trace_name %||% ""
      s$editor_title_fontfamily <- cv$title_family %||% "Arial"
      s$editor_title_fontsize   <- cv$title_size %||% 14
      s$editor_title_color      <- cv$title_color %||% "#000000"
      s$editor_title_fontbold   <- isTRUE(cv$title_bold)
    } else if (el == "background") {
      s$editor_bg_plot  <- cv$bg_plot %||% "#FFFFFF"
      s$editor_bg_paper <- cv$bg_paper %||% "#FFFFFF"
    } else if (el == "trace") {
      s$editor_trace_mode       <- cv$trace_mode %||% "lines+markers"
      s$editor_trace_width      <- cv$trace_width %||% 2
      s$editor_trace_dash       <- cv$trace_dash %||% "solid"
      s$editor_trace_markersize <- cv$trace_markersize %||% 6
      s$editor_trace_color      <- if (length(cv$trace_color) > 0) cv$trace_color[[1]] else "#000000"
    }
    s
  }

  #TRUE only if the value differs from its seed, so a panel re-render is not an edit
  edited <- function(id) {
    val <- input[[id]]
    if (is.null(val)) return(FALSE)
    if (is.atomic(val) && length(val) == 1 && is.na(val)) return(FALSE)
    seed <- isolate(rv_plot_aesthetics$seeds[[id]])
    if (is.null(seed)) return(TRUE)
    same <- if (is.logical(seed) || is.logical(val)) {
      isTRUE(val) == isTRUE(seed)
    } else if (is.numeric(seed) && suppressWarnings(!any(is.na(as.numeric(val))))) {
      isTRUE(all.equal(as.numeric(val), as.numeric(seed)))
    } else {
      identical(tolower(as.character(val)), tolower(as.character(seed)))
    }
    if (same) return(FALSE)
    #advance the seed so toggling a value back also registers
    rv_plot_aesthetics$seeds[[id]] <- val
    TRUE
  }

  #persist an edit under the active plot id
  record_style <- function(group, values, item = NULL) {
    pid <- isolate(rv_plot_aesthetics$current_plot_id)
    if (is.null(pid) || !nzchar(pid)) return(invisible(NULL))
    st <- isolate(rv_plot_aesthetics$plot_styles[[pid]]) %||% list()
    if (is.null(item)) {
      node <- st[[group]] %||% list()
      for (nm in names(values)) node[[nm]] <- values[[nm]]
      st[[group]] <- node
    } else {
      grp <- st[[group]] %||% list()
      node <- grp[[item]] %||% list()
      for (nm in names(values)) node[[nm]] <- values[[nm]]
      grp[[item]] <- node
      st[[group]] <- grp
    }
    rv_plot_aesthetics$plot_styles[[pid]] <- st
    invisible(NULL)
  }

  relayout <- function(update) {
    if (length(update) == 0) return(invisible(NULL))
    session$sendCustomMessage("editor_relayout", list(
      plot_id = isolate(rv_plot_aesthetics$current_plot_id),
      update = update
    ))
  }

  # Make the panel draggable, once at load
  shinyjs::runjs("if(window.makePanelDraggable) window.makePanelDraggable('plot_aesthetic_editor', 'plot_editor_header');")

  # Handle element clicks
  observeEvent(input$plot_element_clicked, {
    click_data <- input$plot_element_clicked

    if (click_data$element == "none") {
      # Deselect
      shinyjs::hide("plot_aesthetic_editor")
      rv_plot_aesthetics$current_element <- NULL
      return()
    }

    # Update active selection
    rv_plot_aesthetics$current_element <- click_data$element
    rv_plot_aesthetics$current_axis_id <- click_data$axis_id
    rv_plot_aesthetics$current_plot_id <- click_data$plot_id
    rv_plot_aesthetics$current_trace_name <- click_data$trace_name
    rv_plot_aesthetics$current_trace_index <- click_data$trace_index
    rv_plot_aesthetics$current_values <- click_data$current %||% list()

    #track legend visibility + its right margin, so the toggle can restore the space
    cur <- click_data$current %||% list()
    vis <- if (!is.null(cur$showlegend)) isTRUE(cur$showlegend) else TRUE
    rv_plot_aesthetics$legend_visible <- vis
    if (vis && !is.null(cur$margin_r)) {
      rv_plot_aesthetics$legend_margin_r <- cur$margin_r
    }

    #seed the controls before the panel renders
    seeds <- editor_seeds(click_data$element, cur, click_data$trace_name)
    seeds$editor_show_legend <- vis
    rv_plot_aesthetics$seeds <- seeds
    rv_plot_aesthetics$sel_rev <- rv_plot_aesthetics$sel_rev + 1

    # Show the editor panel
    shinyjs::show("plot_aesthetic_editor")
  })

  # Show / hide the legend (and reclaim its space for the plot)
  observeEvent(input$editor_show_legend, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_show_legend")) return()
    show <- isTRUE(input$editor_show_legend)
    update <- list(showlegend = show)
    if (show) {
      # restore the right margin captured while the legend was visible
      if (!is.null(rv_plot_aesthetics$legend_margin_r)) {
        update[["margin.r"]] <- rv_plot_aesthetics$legend_margin_r
      }
    } else {
      # collapse the right margin so the plot expands into the legend's space
      update[["margin.r"]] <- 40
    }
    rv_plot_aesthetics$legend_visible <- show
    record_style("legend", list(show = show, margin_r = update[["margin.r"]]))
    relayout(update)
  }, ignoreInit = TRUE)

  # Close button
  observeEvent(input$close_editor_btn, {
    shinyjs::hide("plot_aesthetic_editor")
    rv_plot_aesthetics$current_element <- NULL
  })

  # Render the dynamic UI based on selection
  output$plot_editor_ui <- renderUI({
    #rebuild on new selection only; depending on seeds would re-render mid-drag
    rv_plot_aesthetics$sel_rev
    req(rv_plot_aesthetics$current_element)
    el <- isolate(rv_plot_aesthetics$current_element)
    sd <- isolate(rv_plot_aesthetics$seeds)

    # Generate a user-friendly title
    selection_title <- switch(el,
      "xaxis_title" = "X-Axis Title",
      "yaxis_title" = "Y-Axis Title",
      "xaxis_tick" = "X-Axis Ticks/Labels",
      "yaxis_tick" = "Y-Axis Ticks/Labels",
      "xaxis_grid" = "X-Axis Gridlines",
      "yaxis_grid" = "Y-Axis Gridlines",
      "legend" = "Legend",
      "title" = "Plot Title",
      "text_element" = "Text/Label",
      "background" = "Plot Background",
      "trace" = paste("Trace:", rv_plot_aesthetics$current_trace_name),
      "Unknown"
    )

    header_ui <- div(
      class = "alert alert-info py-2 mb-3",
      style = "font-size: 0.9em;",
      strong("Currently Selected:"), br(),
      selection_title
    )

    controls_ui <- tagList()
    #keep a font the plot already uses selectable
    font_choices <- function(current) unique(c(editor_fonts, current[nzchar(current %||% "")]))

    if (grepl("axis", el) || grepl("grid", el)) {
      # Axis controls
      controls_ui <- tagList(
        sliderInput("editor_axis_width", "Line Width", min = 0, max = 5,
                    value = sd$editor_axis_width %||% 1, step = 0.5),
        colourpicker::colourInput("editor_axis_color", "Color",
                    value = sd$editor_axis_color %||% "#000000", showColour = "background")
      )
      if (grepl("title", el) || grepl("tick", el)) {
        controls_ui <- tagList(
          controls_ui,
          selectInput("editor_axis_fontfamily", "Font Family",
                      choices = font_choices(sd$editor_axis_fontfamily),
                      selected = sd$editor_axis_fontfamily %||% "Arial"),
          numericInput("editor_axis_fontsize", "Font Size", value = sd$editor_axis_fontsize %||% 12, min = 8, max = 72),
          colourpicker::colourInput("editor_axis_fontcolor", "Font Color",
                      value = sd$editor_axis_fontcolor %||% "#000000", showColour = "background"),
          checkboxInput("editor_axis_fontbold", "Bold Text", value = isTRUE(sd$editor_axis_fontbold))
        )
      }
      if (grepl("title", el)) {
        controls_ui <- tagList(
          controls_ui,
          textInput("editor_axis_text", "Title Text", value = sd$editor_axis_text %||% "")
        )
      }
      if (grepl("yaxis", el)) {
        yr <- suppressWarnings(as.numeric(sd$editor_yaxis_range))
        if (length(yr) < 2 || any(!is.finite(yr))) yr <- c(0, 15)
        lo <- yr[[1]]; hi <- yr[[2]]
        span <- max(1, hi - lo)
        controls_ui <- tagList(
          controls_ui,
          tags$hr(),
          tags$strong("Y-Axis Range"),
          sliderInput("editor_yaxis_range", NULL,
                      min = floor(lo - span), max = ceiling(hi + span),
                      value = c(lo, hi), step = max(0.1, round(span / 40, 1)))
        )
      }
    } else if (el == "legend") {
      controls_ui <- tagList(
        selectInput("editor_legend_orientation", "Orientation",
                    choices = c("Vertical" = "v", "Horizontal" = "h"),
                    selected = sd$editor_legend_orientation %||% "v"),
        sliderInput("editor_legend_x", "X Position", min = -0.5, max = 1.5,
                    value = sd$editor_legend_x %||% 1.05, step = 0.05),
        sliderInput("editor_legend_y", "Y Position", min = -0.5, max = 1.5,
                    value = sd$editor_legend_y %||% 1, step = 0.05),
        colourpicker::colourInput("editor_legend_bg", "Background",
                    value = sd$editor_legend_bg %||% "transparent",
                    showColour = "background", allowTransparent = TRUE),

        tags$hr(), tags$strong("Legend Title"),
        textInput("editor_legend_title_text", "Title Text", value = sd$editor_legend_title_text %||% ""),
        selectInput("editor_legend_title_fontfamily", "Title Font",
                    choices = font_choices(sd$editor_legend_title_fontfamily),
                    selected = sd$editor_legend_title_fontfamily %||% "Arial"),
        numericInput("editor_legend_title_fontsize", "Title Size", value = sd$editor_legend_title_fontsize %||% 12, min = 6, max = 72),
        colourpicker::colourInput("editor_legend_title_fontcolor", "Title Color",
                    value = sd$editor_legend_title_fontcolor %||% "#000000", showColour = "background"),
        checkboxInput("editor_legend_title_bold", "Bold Title", value = isTRUE(sd$editor_legend_title_bold)),

        tags$hr(), tags$strong("Legend Item Font (series labels)"),
        selectInput("editor_legend_item_fontfamily", "Item Font",
                    choices = font_choices(sd$editor_legend_item_fontfamily),
                    selected = sd$editor_legend_item_fontfamily %||% "Arial"),
        numericInput("editor_legend_item_fontsize", "Item Size", value = sd$editor_legend_item_fontsize %||% 12, min = 6, max = 72),
        colourpicker::colourInput("editor_legend_item_fontcolor", "Item Color",
                    value = sd$editor_legend_item_fontcolor %||% "#000000", showColour = "background"),
        checkboxInput("editor_legend_item_bold", "Bold Labels", value = isTRUE(sd$editor_legend_item_bold))
      )
    } else if (el == "title" || el == "text_element") {
      controls_ui <- tagList(
        textInput("editor_title_text", "Text", value = sd$editor_title_text %||% ""),
        selectInput("editor_title_fontfamily", "Font Family",
                    choices = font_choices(sd$editor_title_fontfamily),
                    selected = sd$editor_title_fontfamily %||% "Arial"),
        numericInput("editor_title_fontsize", "Font Size", value = sd$editor_title_fontsize %||% 14, min = 8, max = 72),
        colourpicker::colourInput("editor_title_color", "Color", value = sd$editor_title_color %||% "#000000", showColour = "background"),
        checkboxInput("editor_title_fontbold", "Bold Text", value = isTRUE(sd$editor_title_fontbold))
      )
    } else if (el == "background") {
      controls_ui <- tagList(
        colourpicker::colourInput("editor_bg_plot", "Plot Area Color", value = sd$editor_bg_plot %||% "#FFFFFF", showColour = "background"),
        colourpicker::colourInput("editor_bg_paper", "Outer Area Color", value = sd$editor_bg_paper %||% "#FFFFFF", showColour = "background")
      )
    } else if (el == "trace") {
      controls_ui <- tagList(
        selectInput("editor_trace_mode", "Display Mode",
                    choices = c("Lines & Markers" = "lines+markers", "Lines Only" = "lines", "Markers Only" = "markers"),
                    selected = sd$editor_trace_mode %||% "lines+markers"),
        sliderInput("editor_trace_width", "Line Width", min = 0, max = 10,
                    value = sd$editor_trace_width %||% 2, step = 0.5),
        selectInput("editor_trace_dash", "Line Style",
                    choices = c("Solid" = "solid", "Dashed" = "dash", "Dotted" = "dot", "Dash-Dot" = "dashdot"),
                    selected = sd$editor_trace_dash %||% "solid"),
        sliderInput("editor_trace_markersize", "Marker Size", min = 1, max = 20,
                    value = sd$editor_trace_markersize %||% 6, step = 1),
        colourpicker::colourInput("editor_trace_color", "Color",
                    value = sd$editor_trace_color %||% "#000000", showColour = "background")
      )
    }

    footer_ui <- div(
      class = "mt-4 pt-3 border-top",
      checkboxInput("editor_show_legend", "Show legend on plot",
                    value = isTRUE(isolate(rv_plot_aesthetics$legend_visible))),
      helpText("Uncheck to remove the legend and give the plot more space."),
      actionButton("editor_export_btn", "Export Plot", icon = icon("download"), class = "btn-primary w-100 mb-2"),
      actionButton("editor_reset_btn", "Reset to Defaults", icon = icon("undo"), class = "btn-outline-danger w-100")
    )

    tagList(header_ui, controls_ui, footer_ui)
  })

  # --- Apply Aesthetics Handlers ---
  #each handler pushes to plotly and records the edit for the ggprism export

  # Axis / gridline line width
  observeEvent(input$editor_axis_width, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_axis_width")) return()
    el <- rv_plot_aesthetics$current_element
    ax <- editor_axis_key(rv_plot_aesthetics$current_axis_id, el)

    update <- list()
    if (grepl("grid", el)) {
      update[[paste0(ax, ".gridwidth")]] <- input$editor_axis_width
      record_style(ax, list(grid_width = input$editor_axis_width))
    } else {
      update[[paste0(ax, ".linewidth")]] <- input$editor_axis_width
      record_style(ax, list(line_width = input$editor_axis_width))
    }
    relayout(update)
  }, ignoreInit = TRUE)

  observeEvent(input$editor_yaxis_range, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_yaxis_range")) return()
    rng <- suppressWarnings(as.numeric(input$editor_yaxis_range))
    if (length(rng) < 2 || any(!is.finite(rng))) return()
    ax <- editor_axis_key(rv_plot_aesthetics$current_axis_id, "yaxis")
    update <- list()
    update[[paste0(ax, ".range")]] <- c(rng[[1]], rng[[2]])
    record_style(ax, list(range = c(rng[[1]], rng[[2]])))
    relayout(update)
  }, ignoreInit = TRUE)

  # Axis / gridline color
  observeEvent(input$editor_axis_color, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_axis_color")) return()
    el <- rv_plot_aesthetics$current_element
    ax <- editor_axis_key(rv_plot_aesthetics$current_axis_id, el)

    update <- list()
    if (grepl("grid", el)) {
      update[[paste0(ax, ".gridcolor")]] <- input$editor_axis_color
      record_style(ax, list(grid_color = input$editor_axis_color))
    } else {
      update[[paste0(ax, ".linecolor")]] <- input$editor_axis_color
      record_style(ax, list(line_color = input$editor_axis_color))
    }
    relayout(update)
  }, ignoreInit = TRUE)

  # Axis title text / bold
  observeEvent(list(input$editor_axis_text, input$editor_axis_fontbold), {
    req(rv_plot_aesthetics$current_plot_id)
    changed_text <- edited("editor_axis_text")
    changed_bold <- edited("editor_axis_fontbold")
    if (!changed_text && !changed_bold) return()
    ax <- editor_axis_key(rv_plot_aesthetics$current_axis_id, rv_plot_aesthetics$current_element)
    txt <- input$editor_axis_text %||% ""
    bold <- isTRUE(input$editor_axis_fontbold)
    record_style(ax, list(title = txt, title_bold = bold))
    update <- list()
    update[[paste0(ax, ".title.text")]] <- if (bold) paste0("<b>", txt, "</b>") else txt
    relayout(update)
  }, ignoreInit = TRUE)

  # Axis font (family / size / color), applied to title and ticks
  observeEvent(list(input$editor_axis_fontfamily, input$editor_axis_fontsize, input$editor_axis_fontcolor), {
    req(rv_plot_aesthetics$current_plot_id)
    ax <- editor_axis_key(rv_plot_aesthetics$current_axis_id, rv_plot_aesthetics$current_element)
    update <- list(); rec <- list()

    if (edited("editor_axis_fontfamily")) {
      update[[paste0(ax, ".title.font.family")]] <- input$editor_axis_fontfamily
      update[[paste0(ax, ".tickfont.family")]] <- input$editor_axis_fontfamily
      rec$title_family <- input$editor_axis_fontfamily
      rec$tick_family <- input$editor_axis_fontfamily
    }
    if (edited("editor_axis_fontsize")) {
      update[[paste0(ax, ".title.font.size")]] <- input$editor_axis_fontsize
      update[[paste0(ax, ".tickfont.size")]] <- input$editor_axis_fontsize
      rec$title_size <- input$editor_axis_fontsize
      rec$tick_size <- input$editor_axis_fontsize
    }
    if (edited("editor_axis_fontcolor")) {
      update[[paste0(ax, ".title.font.color")]] <- input$editor_axis_fontcolor
      update[[paste0(ax, ".tickfont.color")]] <- input$editor_axis_fontcolor
      rec$title_color <- input$editor_axis_fontcolor
      rec$tick_color <- input$editor_axis_fontcolor
    }

    if (length(rec) > 0) record_style(ax, rec)
    relayout(update)
  }, ignoreInit = TRUE)

  # Plot title text / bold
  observeEvent(list(input$editor_title_text, input$editor_title_fontbold), {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_title_text") && !edited("editor_title_fontbold")) return()
    txt <- input$editor_title_text %||% ""
    bold <- isTRUE(input$editor_title_fontbold)
    record_style("title", list(text = txt, bold = bold))
    relayout(list("title.text" = if (bold) paste0("<b>", txt, "</b>") else txt))
  }, ignoreInit = TRUE)

  # Plot title font styling
  observeEvent(list(input$editor_title_fontfamily, input$editor_title_fontsize, input$editor_title_color), {
    req(rv_plot_aesthetics$current_plot_id)
    update <- list(); rec <- list()

    if (edited("editor_title_fontfamily")) {
      update[["title.font.family"]] <- input$editor_title_fontfamily
      rec$family <- input$editor_title_fontfamily
    }
    if (edited("editor_title_fontsize")) {
      update[["title.font.size"]] <- input$editor_title_fontsize
      rec$size <- input$editor_title_fontsize
    }
    if (edited("editor_title_color")) {
      update[["title.font.color"]] <- input$editor_title_color
      rec$color <- input$editor_title_color
    }

    if (length(rec) > 0) record_style("title", rec)
    relayout(update)
  }, ignoreInit = TRUE)

  # Background
  observeEvent(input$editor_bg_plot, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_bg_plot")) return()
    record_style("background", list(plot = input$editor_bg_plot))
    relayout(list(plot_bgcolor = input$editor_bg_plot))
  }, ignoreInit = TRUE)

  observeEvent(input$editor_bg_paper, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_bg_paper")) return()
    record_style("background", list(paper = input$editor_bg_paper))
    relayout(list(paper_bgcolor = input$editor_bg_paper))
  }, ignoreInit = TRUE)

  # Legend orientation
  observeEvent(input$editor_legend_orientation, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_legend_orientation")) return()
    record_style("legend", list(orientation = input$editor_legend_orientation))
    relayout(list("legend.orientation" = input$editor_legend_orientation))
  }, ignoreInit = TRUE)

  # Legend X position
  observeEvent(input$editor_legend_x, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_legend_x")) return()
    record_style("legend", list(x = input$editor_legend_x))
    relayout(list("legend.x" = input$editor_legend_x))
  }, ignoreInit = TRUE)

  # Legend Y position
  observeEvent(input$editor_legend_y, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_legend_y")) return()
    record_style("legend", list(y = input$editor_legend_y))
    relayout(list("legend.y" = input$editor_legend_y))
  }, ignoreInit = TRUE)

  # Legend background
  observeEvent(input$editor_legend_bg, {
    req(rv_plot_aesthetics$current_plot_id)
    if (!edited("editor_legend_bg")) return()
    record_style("legend", list(bgcolor = input$editor_legend_bg))
    relayout(list("legend.bgcolor" = input$editor_legend_bg))
  }, ignoreInit = TRUE)

  # Legend title text (+ bold wrap)
  observeEvent(list(input$editor_legend_title_text, input$editor_legend_title_bold), {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    if (!edited("editor_legend_title_text") && !edited("editor_legend_title_bold")) return()
    txt <- input$editor_legend_title_text %||% ""
    bold <- isTRUE(input$editor_legend_title_bold)
    record_style("legend", list(title_text = txt, title_bold = bold))
    relayout(list("legend.title.text" = if (bold) paste0("<b>", txt, "</b>") else txt))
  }, ignoreInit = TRUE)

  # Legend TITLE font (family / size / color)
  observeEvent(list(input$editor_legend_title_fontfamily, input$editor_legend_title_fontsize,
                    input$editor_legend_title_fontcolor), {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    update <- list(); rec <- list()
    if (edited("editor_legend_title_fontfamily")) {
      update[["legend.title.font.family"]] <- input$editor_legend_title_fontfamily
      rec$title_family <- input$editor_legend_title_fontfamily
    }
    if (edited("editor_legend_title_fontsize")) {
      update[["legend.title.font.size"]] <- input$editor_legend_title_fontsize
      rec$title_size <- input$editor_legend_title_fontsize
    }
    if (edited("editor_legend_title_fontcolor")) {
      update[["legend.title.font.color"]] <- input$editor_legend_title_fontcolor
      rec$title_color <- input$editor_legend_title_fontcolor
    }
    if (length(rec) > 0) record_style("legend", rec)
    relayout(update)
  }, ignoreInit = TRUE)

  # Legend ITEM font (family / size / color)
  observeEvent(list(input$editor_legend_item_fontfamily, input$editor_legend_item_fontsize,
                    input$editor_legend_item_fontcolor), {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    update <- list(); rec <- list()
    if (edited("editor_legend_item_fontfamily")) {
      update[["legend.font.family"]] <- input$editor_legend_item_fontfamily
      rec$item_family <- input$editor_legend_item_fontfamily
    }
    if (edited("editor_legend_item_fontsize")) {
      update[["legend.font.size"]] <- input$editor_legend_item_fontsize
      rec$item_size <- input$editor_legend_item_fontsize
    }
    if (edited("editor_legend_item_fontcolor")) {
      update[["legend.font.color"]] <- input$editor_legend_item_fontcolor
      rec$item_color <- input$editor_legend_item_fontcolor
    }
    if (length(rec) > 0) record_style("legend", rec)
    relayout(update)
  }, ignoreInit = TRUE)

  # Legend ITEM bold (HTML wrap of each trace name)
  observeEvent(input$editor_legend_item_bold, {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    if (!edited("editor_legend_item_bold")) return()
    record_style("legend", list(item_bold = isTRUE(input$editor_legend_item_bold)))
    session$sendCustomMessage("editor_legend_items_bold",
      list(plot_id = rv_plot_aesthetics$current_plot_id, bold = isTRUE(input$editor_legend_item_bold)))
  }, ignoreInit = TRUE)

  # Sync sliders when the legend is dragged directly on the plot
  observeEvent(input$editor_legend_dragged, {
    d <- input$editor_legend_dragged
    if (!is.null(d$x)) updateSliderInput(session, "editor_legend_x", value = round(as.numeric(d$x), 2))
    if (!is.null(d$y)) updateSliderInput(session, "editor_legend_y", value = round(as.numeric(d$y), 2))
  }, ignoreInit = TRUE)

  # --- Trace styling ---
  restyle_current_trace <- function(update) {
    idx <- isolate(rv_plot_aesthetics$current_trace_index)
    #unresolved legend entry: still recorded by name, but nothing to restyle live
    if (is.null(idx) || is.na(idx) || idx < 0) return(invisible(NULL))
    session$sendCustomMessage("editor_restyle", list(
      plot_id = isolate(rv_plot_aesthetics$current_plot_id),
      update = update,
      trace_indices = list(idx)
    ))
  }
  #key traces by name: names survive a re-render, indices do not
  record_trace <- function(values) {
    nm <- isolate(rv_plot_aesthetics$current_trace_name)
    if (is.null(nm) || !nzchar(nm)) return(invisible(NULL))
    record_style("traces", values, item = gsub("^<b>|</b>$", "", nm))
  }

  observeEvent(input$editor_trace_width, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    if (!edited("editor_trace_width")) return()
    restyle_current_trace(list("line.width" = input$editor_trace_width))
    record_trace(list(width = input$editor_trace_width))
    #legacy store, used by the gene-group re-render
    rv_plot_aesthetics$styles[[rv_plot_aesthetics$current_trace_name]] <- list(line_width = input$editor_trace_width)
  }, ignoreInit = TRUE)

  observeEvent(input$editor_trace_dash, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    if (!edited("editor_trace_dash")) return()
    restyle_current_trace(list("line.dash" = input$editor_trace_dash))
    record_trace(list(dash = input$editor_trace_dash))
  }, ignoreInit = TRUE)

  observeEvent(input$editor_trace_color, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    if (!edited("editor_trace_color")) return()
    restyle_current_trace(list(
      "line.color" = input$editor_trace_color,
      "marker.color" = input$editor_trace_color
    ))
    record_trace(list(color = input$editor_trace_color))
  }, ignoreInit = TRUE)

  observeEvent(input$editor_trace_markersize, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    if (!edited("editor_trace_markersize")) return()
    restyle_current_trace(list("marker.size" = input$editor_trace_markersize))
    record_trace(list(marker_size = input$editor_trace_markersize))
  }, ignoreInit = TRUE)

  observeEvent(input$editor_trace_mode, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    if (!edited("editor_trace_mode")) return()
    restyle_current_trace(list("mode" = input$editor_trace_mode))
    record_trace(list(mode = input$editor_trace_mode))
  }, ignoreInit = TRUE)

  # Reset
  observeEvent(input$editor_reset_btn, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_reset", list(
      plot_id = rv_plot_aesthetics$current_plot_id
    ))
    # Clear R state
    rv_plot_aesthetics$styles <- list()
    rv_plot_aesthetics$plot_styles[[rv_plot_aesthetics$current_plot_id]] <- NULL
    shinyjs::hide("plot_aesthetic_editor")
    rv_plot_aesthetics$current_element <- NULL
  })

  # Export Modal
  observeEvent(input$editor_export_btn, {
    req(rv_plot_aesthetics$current_plot_id)
    showModal(modalDialog(
      title = "Export Customized Plot",
      selectInput("editor_export_fmt", "Format", choices = c("PNG" = "png", "SVG" = "svg")),
      numericInput("editor_export_w", "Width (inches)", value = 10, min = 1, max = 60, step = 0.5),
      numericInput("editor_export_h", "Height (inches)", value = 8, min = 1, max = 60, step = 0.5),
      numericInput("editor_export_dpi", "Resolution (DPI)", value = 300, min = 72, max = 1200, step = 1),
      helpText("Set the physical size in inches and the print resolution (300 DPI is standard for posters). The label sizes you set in the editor stay the same physical size no matter the DPI."),
      footer = tagList(
        actionButton("editor_export_confirm", "Download", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$editor_export_confirm, {
    req(rv_plot_aesthetics$current_plot_id)
    removeModal()

    w_in <- input$editor_export_w;  if (is.null(w_in) || is.na(w_in) || w_in <= 0) w_in <- 10
    h_in <- input$editor_export_h;  if (is.null(h_in) || is.na(h_in) || h_in <= 0) h_in <- 8
    dpi  <- input$editor_export_dpi; if (is.null(dpi) || is.na(dpi) || dpi <= 0) dpi <- 300

    #layout on an inches*96 canvas, scale by dpi/96, so fonts keep a fixed physical size
    session$sendCustomMessage("editor_export", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      format = input$editor_export_fmt,
      width = round(w_in * 96),
      height = round(h_in * 96),
      scale = dpi / 96,
      filename = paste0("customized_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    ))
  })

  return(rv_plot_aesthetics)
}
