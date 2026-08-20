#R/13_upload_wizard.R
#"Describe your design" upload wizard (Phase 4), pure Shiny + DT, no JS build.
#studyDesignWizardUI/Server collect fields and assemble a `spec` list server-side;
#wizard_spec_to_design() turns that spec into a validated study_design (R/02b) and
#describe_study_design() summarizes it for the Review step.
#depends on make_study_design / validate_study_design (R/02b), auto-sourced before 13.

#the spec the wizard emits (only `align`/`members`/`condition_*` are required by
#make_study_design; the rest fall back to its defaults):
#  spec$id, $align, $members, $comparison_label,
#  $condition_column, $condition_type, $condition_levels, $condition_values,
#  $condition_reference, $condition_label,
#  $genome_col, $replicate, $samples_key, $paralog_policy, ...

#' Turn a wizard spec into a validated study_design
#'
#' Coerces the few fields the UI can only send as text (numeric spacing, member
#' lists), drops empties so make_study_design()'s defaults apply, then builds +
#' validates. Never stops: returns a tagged list the server can branch on.
#'
#' @param spec Named list emitted by the wizard component
#' @return list(ok = TRUE, design = <study_design>, error = NULL) on success,
#'         or list(ok = FALSE, design = NULL, error = <message>) on failure
wizard_spec_to_design <- function(spec) {
  spec <- as.list(spec)

  #Shiny delivers JSON arrays as R lists, so flatten the vector-valued fields
  #(each also tolerates a single "a,b,c" comma string from a text field).
  as_vec <- function(x) {
    if (is.null(x)) return(NULL)
    x <- unlist(x, use.names = FALSE)
    if (is.character(x) && length(x) == 1 && grepl(",", x)) trimws(strsplit(x, ",")[[1]]) else x
  }
  spec$condition_levels <- as_vec(spec$condition_levels)
  spec$members          <- as_vec(spec$members)
  if (!is.null(spec$condition_values)) {
    spec$condition_values <- suppressWarnings(as.numeric(as.character(as_vec(spec$condition_values))))
  }

  #drop NULLs and empty strings so make_study_design() uses its own defaults
  is_empty <- function(x) is.null(x) || (length(x) == 1 && is.character(x) && !nzchar(x))
  spec <- spec[!vapply(spec, is_empty, logical(1))]

  #pass only recognized args
  keep <- intersect(names(spec), names(formals(make_study_design)))
  spec <- spec[keep]

  #auto-default a dataset id (the user shouldn't have to invent one)
  if (is.null(spec$id)) spec$id <- "uploaded_design"

  #friendly report of the fields still to fill in, instead of a raw
  #"argument X is missing". validate_study_design handles the semantic checks
  #(e.g. interval-needs-values) once these are present.
  required <- c("align", "members", "condition_column", "condition_type", "condition_levels")
  missing  <- required[!(required %in% names(spec))]
  if (length(missing) > 0) {
    return(list(ok = FALSE, design = NULL,
                error = paste0("Still need: ", paste(missing, collapse = ", "))))
  }

  tryCatch(
    list(ok = TRUE, design = do.call(make_study_design, spec), error = NULL),
    error = function(e) list(ok = FALSE, design = NULL, error = conditionMessage(e))
  )
}

#' One-line-per-field human summary of a study_design, for the Review step
#'
#' @param sd A validated study_design
#' @return Character vector, one "Field: value" line per row
describe_study_design <- function(sd) {
  lv <- condition_levels(sd)
  ref <- condition_reference(sd)
  c(
    sprintf("Compare across: %s (%s)", comparison_label(sd), comparison_align(sd)),
    sprintf("Members: %s", paste(comparison_members(sd), collapse = ", ")),
    sprintf("Primary axis: %s [%s]", condition_label(sd), condition_type(sd)),
    sprintf("Levels (%d): %s", length(lv), paste(lv, collapse = " -> ")),
    sprintf("Reference: %s", if (is.null(ref)) "(none)" else ref),
    sprintf("Replicate col: %s", replicate_column(sd) %||% "(none)"),
    sprintf("Genome col: %s", genome_column(sd) %||% "(none)")
  )
}

#' Study-design wizard as a Shiny module (pure Shiny, no JS build)
#'
#' UI + server that turn an uploaded sample table into a study_design. The spec
#' is assembled server-side in reactiveValues; the level->value editor is an
#' editable, reorderable DT whose rows ARE level-value pairs, so a reorder can't
#' desync a level from its number. studyDesignWizardServer() returns a reactive
#' of wizard_spec_to_design()'s result: list(ok, design, error).
#'
#' @param id Module id
studyDesignWizardUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("1. Compare across"),
    radioButtons(ns("align"), NULL, c(
      "Same genome (one species / strain)"      = "identity",
      "Different species / strains (orthology)"  = "orthology")),
    conditionalPanel(
      sprintf("input['%s'] == 'orthology'", ns("align")),
      selectInput(ns("genome_col"), "Which column labels the species / strain?", choices = NULL),
      uiOutput(ns("members_ui"))),

    tags$h4("2. Primary axis"),
    selectInput(ns("axis_col"), "Which column is the primary axis?", choices = NULL),
    radioButtons(ns("axis_type"), "How are the levels related?", c(
      "Separate groups (unordered)"            = "nominal",
      "Ordered stages"                         = "ordinal",
      "Numeric progression (timecourse, dose)" = "interval")),
    fluidRow(
      column(7, DTOutput(ns("levels"))),
      column(5,
        tags$p(class = "text-muted", style = "font-size:13px;margin-top:6px",
               "Numeric axis: edit a value and the rows re-sort. Ordered or categorical: select a row, then move it."),
        actionButton(ns("up"), "▲ Move up"),
        actionButton(ns("down"), "▼ Move down"))),
    selectInput(ns("reference"), "Reference / baseline (optional)", choices = NULL),

    tags$h4("3. Samples"),
    selectInput(ns("replicate"), "Replicate column (optional)", choices = NULL))
}

#' @param id Module id (matches studyDesignWizardUI)
#' @param sample_data reactive() returning the parsed sample table (data frame)
#' @return reactive() -> list(ok, design, error)
studyDesignWizardServer <- function(id, sample_data) {
  moduleServer(id, function(input, output, session) {
    col_vals <- reactive({ req(sample_data()); wizard_column_values(sample_data()) })
    col_num  <- reactive({ req(sample_data()); wizard_column_numeric(sample_data()) })
    rv <- reactiveValues(rows = NULL)   # the level -> value table

    #new data -> refresh the column pickers (2nd col defaults to the axis)
    observeEvent(sample_data(), {
      cols <- names(sample_data())
      updateSelectInput(session, "axis_col",   choices = cols, selected = cols[[min(2, length(cols))]])
      updateSelectInput(session, "genome_col", choices = cols, selected = if ("Species" %in% cols) "Species" else cols[[1]])
      rep_guess <- cols[grepl("^rep", cols, ignore.case = TRUE)]
      updateSelectInput(session, "replicate",  choices = c("(none)" = "", cols),
                        selected = if (length(rep_guess)) rep_guess[[1]] else "")
    })

    #pick the axis column -> cascade-build levels + type + inferred numeric spacing
    observeEvent(input$axis_col, {
      req(input$axis_col, input$axis_col %in% names(sample_data()))
      lv  <- unlist(col_vals()[[input$axis_col]])
      num <- col_num()[[input$axis_col]]
      is_interval <- !is.null(num)
      rows <- data.frame(level = lv, value = if (is_interval) unlist(num) else NA_real_, stringsAsFactors = FALSE)
      if (is_interval) rows <- rows[order(rows$value), , drop = FALSE]
      rv$rows <- rows
      updateRadioButtons(session, "axis_type", selected = if (is_interval) "interval" else "nominal")
    })

    #reference choices track the current level order
    observeEvent(rv$rows, {
      lv  <- rv$rows$level; cur <- isolate(input$reference)
      updateSelectInput(session, "reference", choices = c("(none)" = "", lv),
                        selected = if (!is.null(cur) && cur %in% lv) cur else lv[[1]])
    })

    #members follow the genome column
    output$members_ui <- renderUI({
      req(input$align == "orthology", input$genome_col)
      m <- unlist(col_vals()[[input$genome_col]])
      checkboxGroupInput(session$ns("members"), "Members (which to include)",
                         choices = m, selected = m, inline = TRUE)
    })

    #level -> value editor: edit values (numeric axis re-sorts), reorder rows
    output$levels <- renderDT({
      req(rv$rows)
      interval <- input$axis_type == "interval"
      df <- if (interval) rv$rows else rv$rows["level"]
      datatable(df, rownames = FALSE, selection = "single", class = "compact cell-border",
        colnames = if (interval) c("Level", "Numeric value") else "Level",
        editable = if (interval) list(target = "cell", disable = list(columns = 0)) else FALSE,
        options = list(dom = "t", ordering = FALSE, pageLength = 100))
    }, server = TRUE)

    observeEvent(input$levels_cell_edit, {
      info <- input$levels_cell_edit; rows <- rv$rows
      rows$value[info$row] <- suppressWarnings(as.numeric(info$value))
      rv$rows <- rows[order(rows$value), , drop = FALSE]   # numeric axis re-sorts by value
    })

    move <- function(d) {
      sel <- input$levels_rows_selected; req(sel); j <- sel + d; rows <- rv$rows
      if (j >= 1 && j <= nrow(rows)) { rows[c(sel, j), ] <- rows[c(j, sel), ]; rv$rows <- rows }
    }
    observeEvent(input$up,   move(-1))
    observeEvent(input$down, move( 1))

    #assemble the spec (the shape wizard_spec_to_design expects)
    spec <- reactive({
      req(rv$rows)
      interval <- input$axis_type == "interval"; ortho <- input$align == "orthology"
      list(
        align               = input$align,
        members             = if (ortho) as.list(input$members) else list("genome"),
        comparison_label    = if (ortho) input$genome_col else "Sample",
        genome_col          = if (ortho) input$genome_col else NULL,
        condition_column    = input$axis_col,
        condition_type      = input$axis_type,
        condition_levels    = as.list(rv$rows$level),
        condition_values    = if (interval) as.list(rv$rows$value) else NULL,
        condition_reference = if (!is.null(input$reference) && nzchar(input$reference)) input$reference else NULL,
        replicate           = if (!is.null(input$replicate) && nzchar(input$replicate)) input$replicate else NULL)
    })

    reactive(wizard_spec_to_design(spec()))
  })
}

#' Infer numeric spacing from level labels (for interval axes)
#'
#' Parses timecourse-style labels to a common unit (minutes): "0min"->0,
#' "1.5h"->90, "8h"->480; also plain numbers and leading-number doses ("10uM"
#' ->10). All-or-nothing per column: returns NULL if any label can't be parsed,
#' so the wizard only pre-fills when the inference is trustworthy.
#'
#' @param labels Character vector of level labels
#' @return Numeric vector (same length/order as labels), or NULL if unparseable
infer_numeric_progression <- function(labels) {
  labels <- as.character(labels)
  one <- function(x) {
    x <- trimws(tolower(x))
    num <- function(s) suppressWarnings(as.numeric(gsub("[^0-9.]", "", s)))
    if (grepl("^[0-9.]+\\s*(s|sec|secs|second|seconds)$", x))       return(num(x) / 60)
    if (grepl("^[0-9.]+\\s*min(s|ute|utes)?$", x))                  return(num(x))
    if (grepl("^[0-9.]+\\s*(h|hr|hrs|hour|hours)$", x))             return(num(x) * 60)
    if (grepl("^[0-9.]+\\s*(d|day|days)$", x))                      return(num(x) * 1440)
    if (grepl("^[0-9.]", x)) return(suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", x))))
    NA_real_
  }
  vals <- vapply(labels, one, numeric(1), USE.NAMES = FALSE)
  if (anyNA(vals)) return(NULL)
  vals
}

#' Per-column inferred numeric spacing, for the wizard's interval defaults
#'
#' @param df A parsed sample table
#' @param max_unique Passed through to wizard_column_values()
#' @return Named list, column -> list of inferred numbers (JSON array) or NULL
wizard_column_numeric <- function(df, max_unique = 50) {
  lapply(wizard_column_values(df, max_unique), function(vals) {
    if (length(vals) == 0) return(NULL)
    num <- infer_numeric_progression(unlist(vals))
    if (is.null(num)) NULL else as.list(num)
  })
}

#' Per-column unique values for the wizard's level pickers
#'
#' High-cardinality columns (sample ids etc.) return empty, they aren't axes.
#'
#' @param df A parsed sample table (data frame)
#' @param max_unique Skip columns with more distinct values than this
#' @return Named list, column -> list of unique value strings (JSON arrays)
wizard_column_values <- function(df, max_unique = 50) {
  vals <- lapply(names(df), function(col) {
    u <- unique(as.character(df[[col]]))
    u <- u[!is.na(u)]
    if (length(u) > max_unique) list() else as.list(u)
  })
  stats::setNames(vals, names(df))
}

#null-coalescing helper (base R has none)
`%||%` <- function(a, b) if (is.null(a)) b else a
