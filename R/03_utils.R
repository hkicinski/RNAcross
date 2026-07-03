#rnacross utility functions module
#generic utility functions used across the application
#dependencies: 01_config

#' Create hidden input element
#'
#' Creates a hidden HTML input element for storing values in the UI.
#'
#' @param inputId Character ID for the input element
#' @param value Value to store in the hidden input
#' @return Shiny tags input element
hiddenInput <- function(inputId, value) {
  tags$input(
    id = inputId,
    type = "hidden",
    value = value
  )
}

apply_y_axis_range <- function(p, ps, base_rev = NULL) {
  if (is.null(ps) || !isTRUE(ps$y_axis_manual)) return(p)
  ymin <- suppressWarnings(as.numeric(ps$y_axis_min))
  ymax <- suppressWarnings(as.numeric(ps$y_axis_max))
  if (length(ymin) == 0 || length(ymax) == 0 || is.na(ymin) || is.na(ymax) || ymin >= ymax) return(p)
  p <- plotly::layout(p, yaxis = list(range = c(ymin, ymax), autorange = FALSE))
  if (!is.null(base_rev)) p <- plotly::layout(p, uirevision = paste0(base_rev, "_y_", ymin, "_", ymax))
  p
}

#' Create species panel content
#'
#' Creates the content for a species-specific analysis panel including
#' gene search, orthogroup selection, and plot display.
#' Returns content only - nav_panel wrapper handled by caller.
#'
#' @param species List containing species info (id, name)
#' @return Shiny fluidRow UI element (panel content)
create_species_panel <- function(species) {
  #returns content only - nav_panel wrapper handled by caller
  fluidRow(
    column(
      width = 3,
      div(
        class = "sidebar-panel",
        h4(class = "mb-4", "Analysis Controls"),
        textInput(
          paste0(species$id, "_genename"),
          "Gene name or ID:",
          placeholder = "e.g., PHO4"
        ),
        actionButton(
          paste0(species$id, "_search_button"),
          "Search Gene",
          icon = icon("search"),
          class = "custom-button"
        ),
        #orthogroup selection area
        div(
          id = paste0(species$id, "_orthogroup_container"),
          style = "display: none;",
          hr(),
          div(
            class = "orthogroup-info",
            h5("Orthogroup Members"),
            p("Select genes to analyze from the orthogroup:"),
            div(
              id = paste0(species$id, "_orthogroup_selection"),
              class = "orthogroup-selection",
              #pre-create the radio buttons with dummy choice
              radioButtons(
                paste0(species$id, "_", species$id, "_selection"),
                label = NULL,
                choices = c("Search for a gene..." = ""),
                selected = ""
              )
            )
          ),
          actionButton(
            paste0(species$id, "_plot_button"),
            "Generate Plot",
            icon = icon("chart-line"),
            class = "custom-button"
          )
        ),
        actionButton(
          paste0(species$id, "_export_btn"),
          "Download Plot",
          icon = icon("download"),
          class = "btn btn-secondary mt-2 w-100"
        ),
        hr(),
        div(class = "gene-info",
            verbatimTextOutput(paste0(species$id, "_gene_info"))
        )
      )
    ),
    column(
      width = 9,
      div(
        class = "results-panel",
        plotlyOutput(paste0(species$id, "_gene_plot"), height = "400px"),
        fluidRow(
          column(
            width = 6,
            div(
              class = "mt-4",
              h5(tags$em(species$name), " Search Results"),
              DTOutput(paste0(species$id, "_search_results"))
            )
          ),
          column(
            width = 6,
            div(
              class = "mt-4",
              h5("Orthogroup Members"),
              DTOutput(paste0(species$id, "_orthogroup_results"))
            )
          )
        )
      )
    )
  )
}

#' Create enhanced orthogroup selection UI
#'
#' Creates UI elements for selecting genes from orthogroups,
#' handling both single-copy and paralogous genes.
#'
#' @param query_result Query result containing genes_by_species
#' @param prefix Character prefix for input IDs (species code or "combined")
#' @param config Optional species configuration list
#' @return Shiny tagList UI element or NULL if no results
create_orthogroup_selection_ui_enhanced <- function(query_result, prefix, config = NULL) {
  #use default config if not provided
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }
  
  if (is.null(query_result) || query_result$source == "none") {
    return(NULL)
  }
  
  #determine which species to show
  current_species <- prefix
  if (prefix == "combined") {
    species_to_show <- names(query_result$genes_by_species)
  } else {
    species_to_show <- current_species
  }
  
  selection_ui <- tagList()
  
  #check if any species has paralogs
  has_paralogs <- FALSE
  for (sp in species_to_show) {
    if (sp %in% names(query_result$genes_by_species)) {
      if (nrow(query_result$genes_by_species[[sp]]) > 1) {
        has_paralogs <- TRUE
        break
      }
    }
  }
  
  #add paralog notice if applicable
  if (has_paralogs) {
    selection_ui <- tagList(
      selection_ui,
      div(
        class = "paralog-notice",
        icon("info-circle"),
        " Multiple gene copies (paralogs) detected. Select the appropriate gene for each species."
      )
    )
  }
  
  for (species_code in species_to_show) {
    if (species_code %in% names(query_result$genes_by_species)) {
      genes_df <- query_result$genes_by_species[[species_code]]
      
      if (nrow(genes_df) > 0) {
        radio_id <- paste0(prefix, "_", species_code, "_selection")
        
        #for single species view
        if (prefix != "combined") {
          if (nrow(genes_df) == 1) {
            #single gene - show as info with auto-selection
            selection_ui <- div(
              class = "paralog-selection",
              div(class = "paralog-header", "Gene found in this species:"),
              div(
                class = "single-gene-notice",
                genes_df$display[1],
                " (automatically selected)"
              ),
              #hidden radio button for consistency
              div(
                style = "display: none;",
                radioButtons(
                  radio_id,
                  label = NULL,
                  choices = setNames(genes_df$gene_id, genes_df$display),
                  selected = genes_df$gene_id[1]
                )
              )
            )
          } else {
            #multiple paralogs
            selection_ui <- div(
              class = "paralog-selection",
              div(
                class = "paralog-header",
                paste0(nrow(genes_df), " paralogs found - Select one:")
              ),
              radioButtons(
                radio_id,
                label = NULL,
                choices = setNames(genes_df$gene_id, genes_df$display),
                selected = genes_df$gene_id[1]
              ),
              div(
                style = "margin-top: 10px; font-size: 0.9em; color: #6c757d;",
                "Tip: Paralogs are gene duplicates that may have diverged in function."
              )
            )
          }
          
        } else {
          species_name <- if(species_code %in% names(config)) {
            config[[species_code]]$short
          } else {
            species_code
          }
          
          species_ui <- div(
            class = "orthogroup-species",
            h5(
              species_name,
              if(nrow(genes_df) > 1) {
                span(
                  class = "badge bg-warning text-dark ms-2",
                  paste(nrow(genes_df), "paralogs")
                )
              }
            ),
            if (nrow(genes_df) == 1) {
              div(
                class = "single-gene-notice mb-2",
                "Single copy gene: ", genes_df$display[1]
              )
            },
            radioButtons(
              radio_id,
              label = NULL,
              choices = setNames(genes_df$gene_id, genes_df$display),
              selected = genes_df$gene_id[1],
              width = "100%"
            )
          )
          
          selection_ui <- tagList(selection_ui, species_ui)
        }
      }
    }
  }
  
  #if no genes found for this species
  if (length(selection_ui) == 0 && prefix != "combined") {
    selection_ui <- div(
      class = "alert alert-info",
      icon("info-circle"),
      " No genes found in this species for the queried orthogroup."
    )
  }
  
  return(selection_ui)
}

#' Load phylogenetic tree for orthogroup
#'
#' Loads a phylogenetic tree from the data structure for a given orthogroup.
#'
#' @param og_id Orthogroup ID
#' @param current_data Optional data structure (defaults to all_species_data)
#' @return Phylo tree object or NULL if not found
load_gene_tree <- function(og_id, current_data = NULL) {
  #access trees from the loaded data structure
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  if (!is.null(current_data$phylo_trees$trees)) {
    tree_name <- paste0(og_id, "_tree.txt")
    
    #check if tree exists in the list
    if (tree_name %in% names(current_data$phylo_trees$trees)) {
      tree_data <- current_data$phylo_trees$trees[[tree_name]]
      
      #parse the newick string to create a tree object
      if (!is.null(tree_data$newick) && nchar(tree_data$newick) > 0) {
        tree <- read.tree(text = tree_data$newick)
        return(tree)
      }
    }
  }
  return(NULL)
}

#' Capture current session state
#'
#' Captures the current state of the Shiny session for saving/restoring.
#'
#' @param selected_genes Reactive values for selected genes
#' @param query_results Reactive values for query results
#' @param global_query_state Reactive values for global query state
#' @param combined_selections Reactive values for combined selections
#' @param ortholog_state Reactive values for ortholog state
#' @param input Shiny input object
#' @return List containing session state
capture_session_state <- function(selected_genes, query_results, global_query_state, combined_selections, ortholog_state, input) {
  list(
    version = "1.0",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    selected_genes = reactiveValuesToList(selected_genes),
    query_results = reactiveValuesToList(query_results),
    current_query = global_query_state$current_query,
    combined_selections = reactiveValuesToList(combined_selections),
    gene_list = input$gene_list,
    enable_ortholog_analysis = input$enable_ortholog_analysis,
    current_tab = input$nav,
    group_analysis_species = input$group_analysis_species,
    group_viz_type = input$group_viz_type,
    distance_method = input$distance_method,
    data_transform = input$data_transform,
    aggregation_level = input$aggregation_level,
    ortholog_selected = ortholog_state$selected_orthologs
  )
}

#' Restore session state
#'
#' Restores a previously captured session state to the Shiny session.
#'
#' @param state List containing session state to restore
#' @param selected_genes Reactive values for selected genes
#' @param query_results Reactive values for query results
#' @param global_query_state Reactive values for global query state
#' @param combined_selections Reactive values for combined selections
#' @param ortholog_state Reactive values for ortholog state
#' @param session Shiny session object
#' @return NULL (side effect: updates session state)
restore_session_state <- function(state, selected_genes, query_results, global_query_state, combined_selections, ortholog_state, session) {
  tryCatch({
    if (!is.null(state$selected_genes)) {
      for (sp in names(state$selected_genes)) {
        selected_genes[[sp]] <- state$selected_genes[[sp]]
      }
    }
    
    if (!is.null(state$query_results)) {
      for (sp in names(state$query_results)) {
        query_results[[sp]] <- state$query_results[[sp]]
      }
    }
    
    if (!is.null(state$current_query)) {
      global_query_state$current_query <- state$current_query
    }
    
    if (!is.null(state$combined_selections)) {
      for (sp in names(state$combined_selections)) {
        combined_selections[[sp]] <- state$combined_selections[[sp]]
      }
    }
    
    #restore gene group analysis inputs with delay to ensure UI is ready
    if (!is.null(state$gene_list)) {
      tryCatch({
        updateTextAreaInput(session, "gene_list", value = state$gene_list)
      }, error = function(e) {
        debug_print("could not restore gene_list: ", e$message)
      })
    }
    
    if (!is.null(state$enable_ortholog_analysis)) {
      tryCatch({
        updateCheckboxInput(session, "enable_ortholog_analysis", value = state$enable_ortholog_analysis)
      }, error = function(e) {
        debug_print("could not restore enable_ortholog_analysis: ", e$message)
      })
    }
    
    if (!is.null(state$group_analysis_species)) {
      tryCatch({
        updateSelectInput(session, "group_analysis_species", selected = state$group_analysis_species)
      }, error = function(e) {
        debug_print("could not restore group_analysis_species: ", e$message)
      })
    }
    
    if (!is.null(state$group_viz_type)) {
      tryCatch({
        updateSelectInput(session, "group_viz_type", selected = state$group_viz_type)
      }, error = function(e) {
        debug_print("could not restore group_viz_type: ", e$message)
      })
    }
    
    if (!is.null(state$distance_method)) {
      tryCatch({
        updateSelectInput(session, "distance_method", selected = state$distance_method)
      }, error = function(e) {
        debug_print("could not restore distance_method: ", e$message)
      })
    }
    
    if (!is.null(state$data_transform)) {
      tryCatch({
        updateSelectInput(session, "data_transform", selected = state$data_transform)
      }, error = function(e) {
        debug_print("could not restore data_transform: ", e$message)
      })
    }
    
    if (!is.null(state$aggregation_level)) {
      tryCatch({
        updateRadioButtons(session, "aggregation_level", selected = state$aggregation_level)
      }, error = function(e) {
        debug_print("could not restore aggregation_level: ", e$message)
      })
    }
    
    #do NOT restore ortholog_state$mapped
    
    if (!is.null(state$ortholog_selected)) {
      tryCatch({
        ortholog_state$selected_orthologs <- state$ortholog_selected
      }, error = function(e) {
        debug_print("could not restore ortholog_selected: ", e$message)
      })
    }
    
    #restore current tab
    if (!is.null(state$current_tab)) {
      tryCatch({
        updateNavbarPage(session, "nav", selected = state$current_tab)
      }, error = function(e) {
        debug_print("could not restore current_tab: ", e$message)
      })
    }
  }, error = function(e) {
    debug_print("error in restore_session_state: ", e$message)
  })
}

#' Format timestamp as "time ago" string
#'
#' Converts a timestamp to a human-readable "time ago" format.
#'
#' @param timestamp Character timestamp in ISO format
#' @return Character string like "5 minutes ago", "2 hours ago", etc.
format_time_ago <- function(timestamp) {
  if (is.null(timestamp)) return("just now")
  
  saved_time <- as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  diff_secs <- as.numeric(difftime(Sys.time(), saved_time, units = "secs"))
  
  if (diff_secs < 60) return("just now")
  if (diff_secs < 3600) return(paste(round(diff_secs / 60), "minutes ago"))
  if (diff_secs < 86400) return(paste(round(diff_secs / 3600), "hours ago"))
  return(paste(round(diff_secs / 86400), "days ago"))
}

