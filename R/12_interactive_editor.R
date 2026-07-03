#' Interactive Plotly Aesthetic Editor Server Logic
#'
#' Handles the reactive state, dynamic UI rendering, and custom message dispatch
#' for the client-side Plotly editor.

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
    styles = list() # Holds the semantic keyed styles
  )

  # Make the panel draggable using our vanilla JS function
  # We do this once when the UI is loaded
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
    
    # Show the editor panel
    shinyjs::show("plot_aesthetic_editor")
  })
  
  # Close button
  observeEvent(input$close_editor_btn, {
    shinyjs::hide("plot_aesthetic_editor")
    rv_plot_aesthetics$current_element <- NULL
  })
  
  # Render the dynamic UI based on selection
  output$plot_editor_ui <- renderUI({
    req(rv_plot_aesthetics$current_element)
    el <- rv_plot_aesthetics$current_element
    
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
    
    cv <- rv_plot_aesthetics$current_values
    controls_ui <- tagList()
    
    if (grepl("axis", el) || grepl("grid", el)) {
      # Axis controls
      controls_ui <- tagList(
        sliderInput("editor_axis_width", "Line Width", min = 0, max = 5, value = 1, step = 0.5),
        colourpicker::colourInput("editor_axis_color", "Color", value = "#000000", showColour = "background")
      )
      if (grepl("title", el) || grepl("tick", el)) {
        controls_ui <- tagList(
          controls_ui,
          selectInput("editor_axis_fontfamily", "Font Family", choices = c("Arial", "Times New Roman", "Courier New", "Verdana", "Tahoma", "Helvetica", "Open Sans"), selected = "Arial"),
          numericInput("editor_axis_fontsize", "Font Size", value = 12, min = 8, max = 24),
          colourpicker::colourInput("editor_axis_fontcolor", "Font Color", value = "#000000", showColour = "background"),
          checkboxInput("editor_axis_fontbold", "Bold Text", value = FALSE)
        )
      }
      if (grepl("title", el)) {
        controls_ui <- tagList(
          controls_ui,
          textInput("editor_axis_text", "Title Text", value = rv_plot_aesthetics$current_trace_name)
        )
      }
      if (grepl("yaxis", el)) {
        yr <- suppressWarnings(as.numeric(unlist(cv$yaxis_range)))
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
      fams <- c("Arial", "Times New Roman", "Courier New", "Verdana", "Tahoma", "Helvetica", "Open Sans")
      controls_ui <- tagList(
        selectInput("editor_legend_orientation", "Orientation",
                    choices = c("Vertical" = "v", "Horizontal" = "h"),
                    selected = cv$legend_orientation %||% "v"),
        sliderInput("editor_legend_x", "X Position", min = -0.5, max = 1.5,
                    value = cv$legend_x %||% 1.05, step = 0.05),
        sliderInput("editor_legend_y", "Y Position", min = -0.5, max = 1.5,
                    value = cv$legend_y %||% 1, step = 0.05),
        colourpicker::colourInput("editor_legend_bg", "Background",
                    value = cv$legend_bgcolor %||% "rgba(255,255,255,0)", showColour = "background"),

        tags$hr(), tags$strong("Legend Title Font (\u201cGene\u201d)"),
        selectInput("editor_legend_title_fontfamily", "Title Font", choices = fams,
                    selected = cv$legend_title_fontfamily %||% "Arial"),
        numericInput("editor_legend_title_fontsize", "Title Size", value = cv$legend_title_fontsize %||% 12, min = 6, max = 36),
        colourpicker::colourInput("editor_legend_title_fontcolor", "Title Color",
                    value = cv$legend_title_fontcolor %||% "#000000", showColour = "background"),
        checkboxInput("editor_legend_title_bold", "Bold Title", value = isTRUE(cv$legend_title_bold)),

        tags$hr(), tags$strong("Legend Item Font (gene labels)"),
        selectInput("editor_legend_item_fontfamily", "Item Font", choices = fams,
                    selected = cv$legend_item_fontfamily %||% "Arial"),
        numericInput("editor_legend_item_fontsize", "Item Size", value = cv$legend_item_fontsize %||% 12, min = 6, max = 36),
        colourpicker::colourInput("editor_legend_item_fontcolor", "Item Color",
                    value = cv$legend_item_fontcolor %||% "#000000", showColour = "background"),
        checkboxInput("editor_legend_item_bold", "Bold Labels", value = isTRUE(cv$legend_item_bold))
      )
    } else if (el == "title" || el == "text_element") {
      controls_ui <- tagList(
        textInput("editor_title_text", "Text", value = rv_plot_aesthetics$current_trace_name),
        selectInput("editor_title_fontfamily", "Font Family", choices = c("Arial", "Times New Roman", "Courier New", "Verdana", "Tahoma", "Helvetica", "Open Sans"), selected = "Arial"),
        numericInput("editor_title_fontsize", "Font Size", value = cv$title_size %||% 14, min = 8, max = 36),
        colourpicker::colourInput("editor_title_color", "Color", value = cv$title_color %||% "#000000", showColour = "background"),
        checkboxInput("editor_title_fontbold", "Bold Text", value = FALSE)
      )
    } else if (el == "background") {
      controls_ui <- tagList(
        colourpicker::colourInput("editor_bg_plot", "Plot Area Color", value = cv$bg_plot %||% "#FFFFFF", showColour = "background"),
        colourpicker::colourInput("editor_bg_paper", "Outer Area Color", value = cv$bg_paper %||% "#FFFFFF", showColour = "background")
      )
    } else if (el == "trace") {
      controls_ui <- tagList(
        selectInput("editor_trace_mode", "Display Mode",
                    choices = c("Lines & Markers" = "lines+markers", "Lines Only" = "lines", "Markers Only" = "markers"),
                    selected = cv$trace_mode %||% "lines+markers"),
        sliderInput("editor_trace_width", "Line Width", min = 0, max = 10,
                    value = cv$trace_width %||% 2, step = 0.5),
        selectInput("editor_trace_dash", "Line Style",
                    choices = c("Solid" = "solid", "Dashed" = "dash", "Dotted" = "dot", "Dash-Dot" = "dashdot"),
                    selected = cv$trace_dash %||% "solid"),
        sliderInput("editor_trace_markersize", "Marker Size", min = 1, max = 20,
                    value = cv$trace_markersize %||% 6, step = 1),
        colourpicker::colourInput("editor_trace_color", "Color",
                    value = if (length(cv$trace_color) > 0) cv$trace_color[[1]] else "#000000", showColour = "background")
      )
    }
    
    footer_ui <- div(
      class = "mt-4 pt-3 border-top",
      actionButton("editor_export_btn", "Export Plot", icon = icon("download"), class = "btn-primary w-100 mb-2"),
      actionButton("editor_reset_btn", "Reset to Defaults", icon = icon("undo"), class = "btn-outline-danger w-100")
    )
    
    tagList(header_ui, controls_ui, footer_ui)
  })
  
  # --- Apply Aesthetics Handlers ---
  # In a full implementation, each input change saves to rv_plot_aesthetics$styles
  # and sends a relayout/restyle message. For the pilot, we wire up a few core ones.
  
  # Axis Line Width
  observeEvent(input$editor_axis_width, {
    req(rv_plot_aesthetics$current_plot_id, rv_plot_aesthetics$current_axis_id)
    el <- rv_plot_aesthetics$current_element
    ax <- rv_plot_aesthetics$current_axis_id
    
    update <- list()
    if (grepl("grid", el)) {
      update[[paste0(ax, ".gridwidth")]] <- input$editor_axis_width
    } else {
      update[[paste0(ax, ".linewidth")]] <- input$editor_axis_width
    }
    
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = update
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$editor_yaxis_range, {
    req(rv_plot_aesthetics$current_plot_id)
    rng <- suppressWarnings(as.numeric(input$editor_yaxis_range))
    if (length(rng) < 2 || any(!is.finite(rng))) return()
    ax <- rv_plot_aesthetics$current_axis_id
    ax_full <- if (is.null(ax) || !nzchar(ax)) "yaxis" else if (startsWith(ax, "yaxis")) ax else sub("^y", "yaxis", ax)
    update <- list()
    update[[paste0(ax_full, ".range")]] <- c(rng[[1]], rng[[2]])
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = update
    ))
  }, ignoreInit = TRUE)

  # Axis Color
  observeEvent(input$editor_axis_color, {
    req(rv_plot_aesthetics$current_plot_id, rv_plot_aesthetics$current_axis_id)
    el <- rv_plot_aesthetics$current_element
    ax <- rv_plot_aesthetics$current_axis_id
    
    update <- list()
    if (grepl("grid", el)) {
      update[[paste0(ax, ".gridcolor")]] <- input$editor_axis_color
    } else {
      update[[paste0(ax, ".linecolor")]] <- input$editor_axis_color
    }
    
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = update
    ))
  }, ignoreInit = TRUE)
  
  # Axis Text & Bold
  observe({
    req(rv_plot_aesthetics$current_plot_id, rv_plot_aesthetics$current_axis_id)
    ax <- rv_plot_aesthetics$current_axis_id
    txt <- input$editor_axis_text
    if (is.null(txt)) return()
    
    if (isTRUE(input$editor_axis_fontbold)) txt <- paste0("<b>", txt, "</b>")
    
    update <- list()
    update[[paste0(ax, ".title.text")]] <- txt
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = update
    ))
  })
  
  # Axis Font Styling (Family, Size, Color)
  observe({
    req(rv_plot_aesthetics$current_plot_id, rv_plot_aesthetics$current_axis_id)
    ax <- rv_plot_aesthetics$current_axis_id
    update <- list()
    
    if (!is.null(input$editor_axis_fontfamily)) {
      update[[paste0(ax, ".title.font.family")]] <- input$editor_axis_fontfamily
      update[[paste0(ax, ".tickfont.family")]] <- input$editor_axis_fontfamily
    }
    if (!is.null(input$editor_axis_fontsize)) {
      update[[paste0(ax, ".title.font.size")]] <- input$editor_axis_fontsize
      update[[paste0(ax, ".tickfont.size")]] <- input$editor_axis_fontsize
    }
    if (!is.null(input$editor_axis_fontcolor)) {
      update[[paste0(ax, ".title.font.color")]] <- input$editor_axis_fontcolor
      update[[paste0(ax, ".tickfont.color")]] <- input$editor_axis_fontcolor
    }
    
    if (length(update) > 0) {
      session$sendCustomMessage("editor_relayout", list(
        plot_id = rv_plot_aesthetics$current_plot_id,
        update = update
      ))
    }
  })

  # Title Text & Bold
  observe({
    req(rv_plot_aesthetics$current_plot_id)
    txt <- input$editor_title_text
    if (is.null(txt)) return()
    
    if (isTRUE(input$editor_title_fontbold)) txt <- paste0("<b>", txt, "</b>")
    
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("title.text" = txt)
    ))
  })
  
  # Title Font Styling (Family, Size, Color)
  observe({
    req(rv_plot_aesthetics$current_plot_id)
    update <- list()
    
    if (!is.null(input$editor_title_fontfamily)) {
      update[["title.font.family"]] <- input$editor_title_fontfamily
    }
    if (!is.null(input$editor_title_fontsize)) {
      update[["title.font.size"]] <- input$editor_title_fontsize
    }
    if (!is.null(input$editor_title_color)) {
      update[["title.font.color"]] <- input$editor_title_color
    }
    
    if (length(update) > 0) {
      session$sendCustomMessage("editor_relayout", list(
        plot_id = rv_plot_aesthetics$current_plot_id,
        update = update
      ))
    }
  })
  
  # Background
  observeEvent(input$editor_bg_plot, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list(plot_bgcolor = input$editor_bg_plot)
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$editor_bg_paper, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list(paper_bgcolor = input$editor_bg_paper)
    ))
  }, ignoreInit = TRUE)

  # Legend Orientation
  observeEvent(input$editor_legend_orientation, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("legend.orientation" = input$editor_legend_orientation)
    ))
  }, ignoreInit = TRUE)

  # Legend X position
  observeEvent(input$editor_legend_x, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("legend.x" = input$editor_legend_x)
    ))
  }, ignoreInit = TRUE)

  # Legend Y position
  observeEvent(input$editor_legend_y, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("legend.y" = input$editor_legend_y)
    ))
  }, ignoreInit = TRUE)

  # Legend Background
  observeEvent(input$editor_legend_bg, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_relayout", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("legend.bgcolor" = input$editor_legend_bg)
    ))
  }, ignoreInit = TRUE)

  # Legend TITLE font (family / size / color)
  observe({
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    update <- list()
    if (!is.null(input$editor_legend_title_fontfamily)) update[["legend.title.font.family"]] <- input$editor_legend_title_fontfamily
    if (!is.null(input$editor_legend_title_fontsize))   update[["legend.title.font.size"]]   <- input$editor_legend_title_fontsize
    if (!is.null(input$editor_legend_title_fontcolor))  update[["legend.title.font.color"]]  <- input$editor_legend_title_fontcolor
    if (length(update) > 0) session$sendCustomMessage("editor_relayout",
      list(plot_id = rv_plot_aesthetics$current_plot_id, update = update))
  })

  # Legend ITEM font (family / size / color)
  observe({
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    update <- list()
    if (!is.null(input$editor_legend_item_fontfamily)) update[["legend.font.family"]] <- input$editor_legend_item_fontfamily
    if (!is.null(input$editor_legend_item_fontsize))   update[["legend.font.size"]]   <- input$editor_legend_item_fontsize
    if (!is.null(input$editor_legend_item_fontcolor))  update[["legend.font.color"]]  <- input$editor_legend_item_fontcolor
    if (length(update) > 0) session$sendCustomMessage("editor_relayout",
      list(plot_id = rv_plot_aesthetics$current_plot_id, update = update))
  })

  # Legend TITLE bold (HTML wrap of legend.title.text)
  observeEvent(input$editor_legend_title_bold, {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    session$sendCustomMessage("editor_legend_title_bold",
      list(plot_id = rv_plot_aesthetics$current_plot_id, bold = isTRUE(input$editor_legend_title_bold)))
  }, ignoreInit = TRUE)

  # Legend ITEM bold (HTML wrap of each trace name)
  observeEvent(input$editor_legend_item_bold, {
    req(rv_plot_aesthetics$current_plot_id, identical(rv_plot_aesthetics$current_element, "legend"))
    session$sendCustomMessage("editor_legend_items_bold",
      list(plot_id = rv_plot_aesthetics$current_plot_id, bold = isTRUE(input$editor_legend_item_bold)))
  }, ignoreInit = TRUE)

  # Sync sliders when the legend is dragged directly on the plot
  observeEvent(input$editor_legend_dragged, {
    d <- input$editor_legend_dragged
    if (!is.null(d$x)) updateSliderInput(session, "editor_legend_x", value = round(as.numeric(d$x), 2))
    if (!is.null(d$y)) updateSliderInput(session, "editor_legend_y", value = round(as.numeric(d$y), 2))
  }, ignoreInit = TRUE)
  
  # Trace Styling
  observeEvent(input$editor_trace_width, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    session$sendCustomMessage("editor_restyle", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("line.width" = input$editor_trace_width),
      trace_indices = list(rv_plot_aesthetics$current_trace_index)
    ))
    # Save semantic state (simplified for pilot)
    rv_plot_aesthetics$styles[[rv_plot_aesthetics$current_trace_name]] <- list(line_width = input$editor_trace_width)
  }, ignoreInit = TRUE)
  
  observeEvent(input$editor_trace_dash, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    session$sendCustomMessage("editor_restyle", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list("line.dash" = input$editor_trace_dash),
      trace_indices = list(rv_plot_aesthetics$current_trace_index)
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$editor_trace_color, {
    req(rv_plot_aesthetics$current_plot_id, !is.null(rv_plot_aesthetics$current_trace_index))
    session$sendCustomMessage("editor_restyle", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      update = list(
        "line.color" = input$editor_trace_color,
        "marker.color" = input$editor_trace_color
      ),
      trace_indices = list(rv_plot_aesthetics$current_trace_index)
    ))
  }, ignoreInit = TRUE)
  
  # Reset
  observeEvent(input$editor_reset_btn, {
    req(rv_plot_aesthetics$current_plot_id)
    session$sendCustomMessage("editor_reset", list(
      plot_id = rv_plot_aesthetics$current_plot_id
    ))
    # Clear R state
    rv_plot_aesthetics$styles <- list()
    shinyjs::hide("plot_aesthetic_editor")
  })
  
  # Export Modal
  observeEvent(input$editor_export_btn, {
    req(rv_plot_aesthetics$current_plot_id)
    showModal(modalDialog(
      title = "Export Customized Plot",
      selectInput("editor_export_fmt", "Format", choices = c("PNG" = "png", "SVG" = "svg")),
      numericInput("editor_export_w", "Width (px)", value = 1200),
      numericInput("editor_export_h", "Height (px)", value = 800),
      numericInput("editor_export_scale", "Scale (Resolution Multiplier)", value = 3, min = 1, max = 10, step = 0.5),
      helpText("A higher scale value produces a higher resolution image (e.g., scale=3 is roughly 300 DPI for PNGs)."),
      footer = tagList(
        actionButton("editor_export_confirm", "Download", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$editor_export_confirm, {
    req(rv_plot_aesthetics$current_plot_id)
    removeModal()
    session$sendCustomMessage("editor_export", list(
      plot_id = rv_plot_aesthetics$current_plot_id,
      format = input$editor_export_fmt,
      width = input$editor_export_w,
      height = input$editor_export_h,
      scale = input$editor_export_scale,
      filename = paste0("customized_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    ))
  })
  
  return(rv_plot_aesthetics)
}
