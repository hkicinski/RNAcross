#11_server.R
#RNAcross Server Logic
#
#Complete Shiny server function containing all reactive expressions,
#observers, render functions, and event handlers.
#
#Dependencies: 01_config, 02_constants_themes, 03_utils, 04_data_io,
#              05_orthology_query, 06_data_process, 07_visualization_core,
#              08_visualization_heatmaps, 09_visualization_outputs, 10_ui
server <- function(input, output, session) {
  # Theme state
  is_dark <- reactiveVal(FALSE)
  containers_update_needed <- reactiveVal(TRUE)
  
  #centralized container management
  manage_combined_containers <- function(config) {
    removeUI("#combined_orthogroup_selection_wrapper > *", multiple = TRUE, immediate = TRUE)
    
    for (sp_id in names(reactiveValuesToList(existing_containers))) {
      existing_containers[[sp_id]] <- NULL
    }
    
    for (sp_id in names(config)) {
      if (is.null(existing_containers[[sp_id]])) {
        insertUI(
          selector = "#combined_orthogroup_selection_wrapper",
          where = "beforeEnd",
          ui = div(
            id = paste0("combined_", sp_id, "_selection_ui"),
            class = "orthogroup-species mb-3"
          ),
          immediate = TRUE
        )
        existing_containers[[sp_id]] <- TRUE
      }
    }
  }
  
  observe({
    if (containers_update_needed()) {
      containers_update_needed(FALSE)
      config <- current_species_config()
      manage_combined_containers(config)
    }
  })
  #dynamic combined species 
  observeEvent(upload_state$processed, {
    if (upload_state$processed && data_source() == "custom") {
      containers_update_needed(TRUE)
    }
  })
  
  # New code (lines 2863-2867)
  observeEvent(data_source(), {
    config <- current_species_config()
    manage_combined_containers(config)
  }, ignoreInit = TRUE)
  
  
  #aggregation method info text
  output$hog_aggregation_info <- renderUI({
    req(input$hog_aggregation_method)
    
    info_text <- switch(input$hog_aggregation_method,
                        "single_only" = tagList(
                          icon("filter"), 
                          " Uses only genes with 1:1:1:1 orthology. Most conservative, excludes ~40-60% of HOGs."
                        ),
                        "mean" = tagList(
                          icon("calculator"),
                          " Averages expression across all paralogs. Simple and interpretable."
                        ),
                        "median" = tagList(
                          icon("chart-line"),
                          " Uses median expression. Robust to outlier paralogs (e.g., pseudogenes)."
                        ),
                        "eigengene" = tagList(
                          icon("project-diagram"),
                          strong(" Computes first PC to capture dominant pattern.")
                        ),
                        "max_expr" = tagList(
                          icon("arrow-up"),
                          " Selects most highly expressed paralog. Assumes dominant copy drives function."
                        ),
                        "max_var" = tagList(
                          icon("signal"),
                          " Selects most variable paralog. Focuses on dynamic genes, most informative for PCA."
                        ),
                        "var_weighted" = tagList(
                          icon("balance-scale"),
                          " Weights paralogs by expression variance. Compromise between mean and eigengene."
                        ),
                        ""
    )
    
    info_text
  })
  
  output$ridgeline_species_ui <- renderUI({
    config <- current_species_config()
    #build choices: "All Species" (not italic) + species names (italic via render)
    species_choices <- setNames(names(config), sapply(config, `[[`, "name"))
    all_choices <- c("All Species" = "all", species_choices)
    
    selectizeInput(
      "ridgeline_species",
      "Select Species:",
      choices = all_choices,
      selected = "all",
      options = list(
        render = I("{
          option: function(item, escape) {
            if (item.value === 'all') {
              return '<div>' + escape(item.label) + '</div>';
            }
            return '<div><em>' + escape(item.label) + '</em></div>';
          },
          item: function(item, escape) {
            if (item.value === 'all') {
              return '<div>' + escape(item.label) + '</div>';
            }
            return '<div><em>' + escape(item.label) + '</em></div>';
          }
        }")
      )
    )
  })
  
  output$pca_species_ui <- renderUI({
    config <- current_species_config()
    choices <- setNames(names(config), sapply(config, `[[`, "name"))
    
    selectizeInput(
      "pca_species",
      "Select Species:",
      choices = choices,
      selected = names(config)[1],
      options = list(
        render = I("{
          option: function(item, escape) {
            return '<div><em>' + escape(item.label) + '</em></div>';
          },
          item: function(item, escape) {
            return '<div><em>' + escape(item.label) + '</em></div>';
          }
        }")
      )
    )
  })
  
  # Generate dynamic species menu (hidden in cross-species mode)
  output$group_analysis_species_ui <- renderUI({
    #hide when cross-species ortholog analysis is enabled
    if (!is.null(input$enable_ortholog_analysis) && input$enable_ortholog_analysis) {
      return(NULL)
    }
    
    config <- current_species_config()
    choices <- setNames(names(config), sapply(config, `[[`, "name"))
    
    selectizeInput(
      "group_analysis_species",
      "Select Species:",
      choices = choices,
      selected = names(config)[1],
      options = list(
        render = I("{
          option: function(item, escape) {
            return '<div><em>' + escape(item.label) + '</em></div>';
          },
          item: function(item, escape) {
            return '<div><em>' + escape(item.label) + '</em></div>';
          }
        }")
      )
    )
  })
  output$dynamic_species_menu <- renderUI({
    config <- current_species_config()
    
    if (length(config) == 0) {
      return(p("No species configured. Please upload data."))
    }
    
    # Create nav_menu dynamically
    menu_items <- lapply(names(config), function(id) {
      species <- c(list(id = id), config[[id]])
      nav_panel(
        title = tags$em(species$name),
        value = paste0("species_", id),
        create_species_panel(species)
      )
    })
    
    #return as nav_menu
    do.call(nav_menu, c(list(title = "Single Species View"), menu_items))
  })
  
  # Generate species selection for combined view
  output$species_select_ui <- renderUI({
    config <- current_species_config()
    checkboxGroupInput(
      "species_select",
      "Select Species to Plot:",
      choiceNames = unname(lapply(config, function(x) tags$em(x$name))),
      choiceValues = unname(names(config)),
      selected = names(config)
    )
  })
  # Upload state management
  upload_state <- reactiveValues(
    validated = FALSE,
    processed = FALSE,
    validation_errors = list(),
    validation_warnings = list(),
    uploaded_data = list(),
    custom_all_species_data = NULL
  )
  
  #ortholog analysis state
  ortholog_state <- reactiveValues(
    mapped = FALSE,
    ortholog_data = NULL,
    coverage_stats = NULL,
    gene_mapping = NULL,
    multi_species_data = NULL,
    selected_orthologs = list()
  )
  # This manages the user-defined species list
  species_list <- reactiveValues(
    count = 1,
    entries = list(),
    config = list()
  )
  
  # Observer to add new species input fields
  observeEvent(input$add_species, {
    new_count <- species_list$count + 1
    species_list$count <- new_count
    
    insertUI(
      selector = "#species_input_area",
      where = "beforeEnd",
      ui = div(
        class = "species-entry mb-2",
        id = paste0("species_entry_", new_count),
        fluidRow(
          column(3, 
                 textInput(paste0("species_code_", new_count), "Code*", 
                           value = "", placeholder = "e.g., at")),
          column(5, 
                 textInput(paste0("species_name_", new_count), "Full Name*", 
                           value = "", placeholder = "e.g., Arabidopsis thaliana")),
          column(4, 
                 textInput(paste0("species_short_", new_count), "Display Name", 
                           value = "", placeholder = "e.g., Arabidopsis"))
        )
      )
    )
  })
  
  # Observer to remove species input fields
  observeEvent(input$remove_species, {
    if (species_list$count > 1) {
      removeUI(
        selector = paste0("#species_entry_", species_list$count),
        immediate = TRUE
      )
      species_list$count <- species_list$count - 1
    } else {
      showNotification("Must have at least one species", type = "warning")
    }
  })
  
  # Reactive to collect all defined species
  defined_species <- reactive({
    species_data <- list()
    
    for (i in 1:species_list$count) {
      code <- trimws(input[[paste0("species_code_", i)]])
      name <- trimws(input[[paste0("species_name_", i)]])
      short <- trimws(input[[paste0("species_short_", i)]])
      
      if (!is.null(code) && code != "") {
        # Auto-generate short name if not provided
        if (is.null(short) || short == "") {
          # Try to create abbreviated form (e.g., "H. sapiens" from "Homo sapiens")
          name_parts <- strsplit(name, " ")[[1]]
          if (length(name_parts) >= 2) {
            short <- paste0(substr(name_parts[1], 1, 1), ". ", 
                            paste(name_parts[-1], collapse = " "))
          } else {
            short <- name
          }
        }
        
        species_data[[code]] <- list(
          code = code,
          name = if(!is.null(name) && name != "") name else code,
          short = short
        )
      }
    }
    
    species_data
  })
  
  # Dynamic species configuration
  current_species_config <- reactive({
    if (data_source() == "custom" && length(defined_species()) > 0) {
      defined_species()
    } else {
      DEFAULT_SPECIES_CONFIG
    }
  })
  
  #initialize plot settings
  plot_settings <- reactiveValues(
    #species identity
    species_palette = "Dark2",
    species_colors = list(),
    species_shapes = list(),
    updating_colors_from_palette = FALSE,
    
    #multi-gene line/point encoding
    encoding_multigene_color = "species",
    encoding_multigene_secondary = "linetype",
    gene_palette = "Set2",
    
    #heatmap settings
    heatmap_palette = "viridis",
    heatmap_scale_type = "sequential",
    heatmap_midpoint = "auto",
    heatmap_show_row_dendro = TRUE,
    heatmap_show_col_dendro = TRUE,
    heatmap_row_annotation = TRUE,
    
    #ridgeline settings
    ridgeline_palette = "viridis",
    ridgeline_alpha = 0.8,
    
    #pca settings
    encoding_pca_color = "species",
    encoding_pca_shape = "species",
    pca_alpha = 0.8,
    pca_point_size = 3,
    pca_show_ellipses = TRUE,
    pca_show_loadings = FALSE,
    
    #export settings
    export_width = 8,
    export_height = 6,
    export_dpi = 300,
    export_format = "png",
    
    #saved presets
    presets = list(),
    
    #initialization flag
    initialized = FALSE
  )
  
  #plot state tracking for reactive re-rendering on settings changes
  plot_state <- reactiveValues(
    combined_ready = FALSE,
    combined_data = NULL,
    pca_ready = FALSE,
    pca_data = NULL,
    ridgeline_ready = FALSE,
    ridgeline_data = NULL,
    heatmap_ready = FALSE,
    heatmap_data = NULL,
    species_plots = list(),
    gene_group_ready = FALSE,
    gene_group_data = NULL,
    gene_group_mode = NULL
  )
  
  #initialize settings from species config
  observe({
    req(!plot_settings$initialized)
    config <- current_species_config()
    defaults <- generate_default_settings(config)
    for (key in names(defaults)) {
      plot_settings[[key]] <- defaults[[key]]
    }
    plot_settings$presets <- get_bundled_presets()
    plot_settings$initialized <- TRUE
  })
  
  
  #dynamic color assignment reads from plot_settings
  species_colors_dynamic <- reactive({
    if (plot_settings$initialized && length(plot_settings$species_colors) > 0) {
      return(plot_settings$species_colors)
    }
    #fallback: generate from current palette setting
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    derive_species_colors(species_list, plot_settings$species_palette, NULL, config)
  })
  
  # Display current species table
  output$current_species_table <- renderTable({
    species <- defined_species()
    if (length(species) > 0) {
      df <- data.frame(
        Code = sapply(species, `[[`, "code"),
        `Scientific Name` = sapply(species, `[[`, "name"),
        `Display Name` = sapply(species, `[[`, "short"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      df
    } else {
      data.frame(Note = "No species defined yet. Add at least one species above.")
    }
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  #expression upload UI based on defined species
  output$expression_upload_ui <- renderUI({
    species <- defined_species()
    
    if (length(species) == 0) {
      return(div(class = "text-muted", icon("arrow-up"), 
                 " Define species in Step 1 first"))
    }
    
    upload_inputs <- lapply(names(species), function(sp_code) {
      sp_info <- species[[sp_code]]
      div(
        class = "mb-2",
        fileInput(
          paste0("upload_", sp_code, "_expr"),
          paste0(sp_info$name, " (", sp_code, "):"),
          accept = c(".tsv", ".txt", ".csv"),
          width = "100%"
        )
      )
    })
    
    do.call(tagList, upload_inputs)
  })
  
  # Generate sample metadata upload UI
  output$sample_upload_ui <- renderUI({
    species <- defined_species()
    
    if (length(species) == 0) {
      return(div(class = "text-muted", icon("arrow-up"),
                 " Define species in Step 1 first"))
    }
    
    upload_inputs <- lapply(names(species), function(sp_code) {
      sp_info <- species[[sp_code]]
      div(
        class = "mb-2",
        fileInput(
          paste0("upload_", sp_code, "_samples"),
          paste0(sp_info$name, " (", sp_code, "):"),
          accept = c(".tsv", ".txt", ".csv"),
          width = "100%"
        )
      )
    })
    
    do.call(tagList, upload_inputs)
  })
  
  # Generate annotation upload UI
  output$annotation_upload_ui <- renderUI({
    species <- defined_species()
    
    if (length(species) == 0) {
      return(div(class = "text-muted", icon("arrow-up"),
                 " Define species in Step 1 first"))
    }
    
    upload_inputs <- lapply(names(species), function(sp_code) {
      sp_info <- species[[sp_code]]
      div(
        class = "mb-2",
        fileInput(
          paste0("upload_", sp_code, "_anno"),
          paste0(sp_info$name, " (", sp_code, "):"),
          accept = c(".tsv", ".txt", ".csv"),
          width = "100%"
        )
      )
    })
    
    do.call(tagList, upload_inputs)
  })

  #data source toggle
  data_source <- reactiveVal("default")
  
  #clear cache when data source changes
  observeEvent(data_source(), {
    clear_performance_cache()
    debug_print("Performance cache cleared")
  }, ignoreInit = TRUE)
  
  #get appropriate species data
  get_all_species_data <- reactive({
    if (data_source() == "custom" && !is.null(upload_state$custom_all_species_data)) {
      return(upload_state$custom_all_species_data)
    } else {
      return(all_species_data)
    }
  })
  
  # Reactive values for combined selections - dynamically initialized
  combined_selections <- reactiveValues()
  existing_containers <- reactiveValues()
  
  observe({
    config <- current_species_config()
    for (sp_id in names(config)) {
      if (is.null(combined_selections[[sp_id]])) {
        combined_selections[[sp_id]] <- character(0)
      }
    }
    
    existing_species <- names(reactiveValuesToList(combined_selections))
    for (sp_id in existing_species) {
      if (!sp_id %in% names(config)) {
        combined_selections[[sp_id]] <- NULL
      }
    }
  })
  
  # Reactive values for storing query results
  query_results <- reactiveValues()
  selected_genes <- reactiveValues()
  
  global_query_state <- reactiveValues(
    current_query = NULL,
    query_result = NULL,
    tree_data = NULL,
    last_search_time = NULL
  )
  
  #restore session on startup
  observeEvent(input$restore_session, {
    req(input$restore_session)
    
    debug_print("restore session triggered")
    
    state <- input$restore_session$state
    saved_at <- input$restore_session$saved_at
    time_ago <- format_time_ago(saved_at)
    
    debug_print(paste("restoring state from", time_ago))
    debug_print(paste("state contents:", paste(names(state), collapse = ", ")))
    
    #delay restoration to ensure UI elements are rendered
    shinyjs::delay(500, {
      restore_session_state(state, selected_genes, query_results, global_query_state, combined_selections, ortholog_state, session)
      
      debug_print("state restored, showing notification")
      
      showNotification(
        ui = tagList(
          icon("check-circle"),
          span(paste("Session restored (saved", time_ago, ")")),
          actionButton(
            "clear_session_btn",
            "Start Fresh",
            class = "btn-sm btn-warning",
            style = "margin-left: 10px;"
          )
        ),
        duration = 8,
        type = "message",
        id = "session_restored"
      )
    })
  }, once = TRUE)
  
  #track initialization state to prevent saving empty data
  session_initialized <- reactiveVal(FALSE)
  
  #mark session as ready after short delay
  observe({
    invalidateLater(2000)
    isolate({
      session_initialized(TRUE)
      debug_print("session initialized, auto-save enabled")
    })
  }) %>% bindEvent(TRUE, once = TRUE)
  
  #auto-save session after changes
  observe({
    req(session_initialized())
    
    #reactive dependencies that trigger save
    deps <- list(
      selected = reactiveValuesToList(selected_genes),
      queries = reactiveValuesToList(query_results),
      combined = reactiveValuesToList(combined_selections),
      current_query = global_query_state$current_query,
      gene_list = input$gene_list,
      ortholog_enabled = input$enable_ortholog_analysis,
      nav = input$nav,
      species = input$group_analysis_species,
      viz = input$group_viz_type,
      distance = input$distance_method,
      transform = input$data_transform,
      aggregation = input$aggregation_level
    )
    
    isolate({
      if (is.null(deps$nav)) return()
      
      current_state <- list(
        version = "1.0",
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        selected_genes = deps$selected,
        query_results = deps$queries,
        current_query = deps$current_query,
        combined_selections = deps$combined,
        gene_list = deps$gene_list,
        enable_ortholog_analysis = deps$ortholog_enabled,
        current_tab = deps$nav,
        group_analysis_species = deps$species,
        group_viz_type = deps$viz,
        distance_method = deps$distance,
        data_transform = deps$transform,
        aggregation_level = deps$aggregation,
        ortholog_selected = if (exists("ortholog_state")) ortholog_state$selected_orthologs else NULL
      )
      
      session$sendCustomMessage(type = "saveSession", message = current_state)
      debug_print("session state saved")
    })
  }) %>% debounce(2000)
  
  #handle start fresh button
  observeEvent(input$clear_session_btn, {
    session$sendCustomMessage(type = "clearSession", message = list())
  })
  
  
  #helper function to update combined species table
  update_combined_table <- function(query_results, combined_selections, is_dark) {
    if (is.null(query_results$combined)) return(NULL)
    
    result <- query_results$combined
    config <- current_species_config()
    
    #collect data in list first, then combine once
    genes_list <- lapply(names(result$genes_by_species), function(sp) {
      sp_data <- result$genes_by_species[[sp]]
      if (nrow(sp_data) == 0) return(NULL)
      
      sp_data$Species <- if(sp %in% names(config)) config[[sp]]$short else sp
      
      #add a column to indicate currently selected gene(s)
      selected_genes <- combined_selections[[sp]]
      if (is.null(selected_genes)) selected_genes <- character(0)
      sp_data$Selected <- sp_data$gene_id %in% selected_genes
      
      sp_data[, c("Species", "gene_id", "gene_name", "Selected")]
    })
    
    #combine all at once
    all_genes <- rbindlist(Filter(Negate(is.null), genes_list), fill = TRUE)
    
    if (nrow(all_genes) == 0) return(NULL)
    
    dt <- datatable(
      all_genes[, c("Species", "gene_id", "gene_name")],
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tp'
      ),
      colnames = c("Species", "Gene ID", "Gene Name"),
      rownames = FALSE
    ) %>%
      formatStyle("Species", fontStyle = "italic")
    
    # Highlight selected genes
    if (any(all_genes$Selected)) {
      dt <- dt %>% formatStyle(
        columns = 1:3,
        target = "row",
        backgroundColor = styleRow(which(all_genes$Selected), 
                                   if(is_dark()) "#3a4a5a" else "#e6f3ff")
      )
    }
    
    return(dt)
  }
  
  # Observe for gene group analysis (bar plot significance testing)
  observe({
    req(input$gene_list, input$group_viz_type == "bar")
    
    # Parse gene list from textarea
    gene_list <- strsplit(trimws(input$gene_list), "[\n\r]+")[[1]]
    gene_list <- gene_list[gene_list != ""]
    
    updateSelectizeInput(
      session,
      "sig_test_gene",
      choices = setNames(gene_list, gene_list),
      selected = NULL
    )
  })
  
  observe({
    req(input$sig_test_gene, input$group_viz_type == "bar")
    
    # Create all possible timepoint pairs
    timepoint_pairs <- combn(TIME_POINTS, 2, simplify = FALSE)
    comparisons <- sapply(timepoint_pairs, function(pair) {
      paste(pair[1], "vs.", pair[2])
    })
    
    updateSelectizeInput(
      session,
      "sig_test_timepoints",
      choices = setNames(comparisons, comparisons),
      selected = NULL
    )
  })
  
  observe({
    req(global_query_state$current_query)
    
    # Make the query result available to all tabs
    config <- current_species_config()
    for (species_id in names(config)) {
      if (!is.null(global_query_state$query_result)) {
        query_results[[species_id]] <- global_query_state$query_result
      }
    }
    
    # Also make it available for combined view
    if (!is.null(global_query_state$query_result)) {
      query_results$combined <- global_query_state$query_result
    }
  })
  
  # Modified extract_orthology_matrix for HOGs
  extract_orthology_matrix <- function() {
    # Get data for all species
    species_data_list <- list()
    config <- current_species_config()
    for (species_id in names(config)) {
      species_data_list[[species_id]] <- get_species_data(species_id)
    }
    
    # Get HOG data
    current_data <- get_all_species_data()
    og_data <- current_data$orthofinder$orthogroups
    
    # Find HOGs with all 4 species
    hog_summary <- og_data %>%
      mutate(species = case_when(
        grepl("^Y[A-P][LR]", gene_id) ~ "sc",
        grepl("^CAGL0", gene_id) ~ "cg",
        grepl("^orf19", gene_id) ~ "ca",
        grepl("^KLLA0", gene_id) ~ "kl"
      )) %>%
      group_by(hog_id) %>%
      summarise(
        n_species = n_distinct(species),
        .groups = 'drop'
      ) %>%
      filter(n_species == 4)
    
    common_hogs <- hog_summary$hog_id
    
    # Create sample metadata dynamically
    sample_metadata_list <- list()
    
    for (species_id in names(config)) {
      sp_data <- species_data_list[[species_id]]
      if (!is.null(sp_data)) {
        # Get the lcpm matrix (handle both naming conventions)
        lcpm_matrix <- if (!is.null(sp_data$lcpm)) sp_data$lcpm else sp_data[[paste0(species_id, "_lcpm")]]
        # Get sample info
        sample_info <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(species_id, "_sample_info")]]
        
        if (!is.null(lcpm_matrix) && !is.null(sample_info)) {
          sample_metadata_list[[species_id]] <- data.frame(
            Sample = colnames(lcpm_matrix),
            Species = config[[species_id]]$short,
            Timepoint = sample_info$Timepoint,
            Replicate = sample_info$Replicate,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    sample_metadata <- do.call(rbind, sample_metadata_list)
    
    # Create expression matrix
    sample_matrix <- matrix(NA, 
                           nrow = nrow(sample_metadata), 
                           ncol = length(common_hogs))
    rownames(sample_matrix) <- sample_metadata$Sample
    colnames(sample_matrix) <- common_hogs
    
    # Fill matrix with expression values
    for (i in 1:nrow(sample_metadata)) {
      sample_name <- sample_metadata$Sample[i]
      species <- sample_metadata$Species[i]
      config <- current_species_config()
      species_code <- NULL
      for (sp_id in names(config)) {
        if (config[[sp_id]]$short == species || config[[sp_id]]$name == species) {
          species_code <- sp_id
          break
        }
      }
      
      # Dynamic lcpm matrix retrieval
      sp_data <- species_data_list[[species_code]]
      lcpm_matrix <- if (!is.null(sp_data$lcpm)) {
        sp_data$lcpm
      } else if (!is.null(sp_data[[paste0(species_code, "_lcpm")]])) {
        sp_data[[paste0(species_code, "_lcpm")]]
      } else {
        NULL
      }
      
      for (j in 1:length(common_hogs)) {
        hog <- common_hogs[j]
        
        lookup_matches <- current_data$gene_lookup[species == species_code & hog_id == hog]
        hog_genes <- lookup_matches$gene_id
        
        if (length(hog_genes) > 0 && hog_genes[1] %in% rownames(lcpm_matrix)) {
          sample_matrix[i, j] <- lcpm_matrix[hog_genes[1], sample_name]
        }
      }
    }
    
    # Remove columns with NAs
    na_cols <- apply(sample_matrix, 2, function(x) any(is.na(x)))
    sample_matrix <- sample_matrix[, !na_cols]
    
    return(list(
      sample_matrix = sample_matrix,
      sample_metadata = sample_metadata,
      common_hogs = colnames(sample_matrix)
    ))
  }
  
  #pca observer - stores data for reactive rendering
  observeEvent(input$run_pca, {
    waiter_show(html = loading_screen)
    
    tryCatch({
      if (input$pca_type == "single") {
        species_data <- get_species_data(input$pca_species)
        lcpm_data <- get_expression_matrix(input$pca_species, input$global_transform, species_data)
        sample_info <- if(input$pca_species == "cg") species_data$sample_info else species_data[[paste0(input$pca_species, "_sample_info")]]
        
        if (is.null(lcpm_data) || is.null(sample_info)) {
          stop("Required data not found for selected species")
        }
        
        plot_state$pca_data <- list(
          type = "single",
          expression_matrix = lcpm_data,
          sample_info = sample_info,
          species = input$pca_species,
          n_genes = nrow(lcpm_data),
          n_samples = ncol(lcpm_data)
        )
        plot_state$pca_ready <- TRUE
        
      } else {
        aggregation_method <- if (!is.null(input$hog_aggregation_method)) input$hog_aggregation_method else "eigengene"
        
        plot_result <- create_multi_species_pca(
          get_species_data = get_species_data,
          is_dark_mode = is_dark(),
          aggregation_method = aggregation_method,
          species_config = current_species_config(),
          all_species_data_obj = get_all_species_data(),
          transform_type = input$global_transform,
          plot_settings = reactiveValuesToList(plot_settings)
        )
        
        if (!is.null(plot_result)) {
          pca_matrices_data <- attr(plot_result, "matrices_data")
          if (!is.null(pca_matrices_data)) {
            session$userData$pca_matrices <- pca_matrices_data
          }
          
          plot_state$pca_data <- list(
            type = "multi",
            plot_result = plot_result,
            aggregation_method = aggregation_method,
            matrices_data = pca_matrices_data
          )
          plot_state$pca_ready <- TRUE
        } else {
          plot_state$pca_ready <- FALSE
        }
      }
      
    }, error = function(e) {
      showNotification(paste("Error in PCA analysis:", e$message), type = "error", duration = NULL)
      plot_state$pca_ready <- FALSE
      plot_state$pca_data <- list(error = e$message)
    })
    
    waiter_hide()
  })
  
  #pca plot render - reactive to plot_state and plot_settings
  output$pca_plot <- renderPlotly({
    if (!isTRUE(plot_state$pca_ready) || is.null(plot_state$pca_data)) {
      return(plotly_empty() %>% add_annotations(text = "Click Run PCA to generate plot", showarrow = FALSE))
    }
    
    pca_data <- plot_state$pca_data
    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)
    
    if (!is.null(pca_data$error)) {
      return(plotly_empty() %>% add_annotations(text = paste("Error:", pca_data$error), showarrow = FALSE))
    }
    
    if (pca_data$type == "single") {
      create_pca_plot(
        expression_matrix = pca_data$expression_matrix,
        sample_info = pca_data$sample_info,
        is_dark_mode = dark_mode,
        plot_settings = current_settings
      )
    } else {
      create_multi_species_pca(
        get_species_data = get_species_data,
        is_dark_mode = dark_mode,
        aggregation_method = pca_data$aggregation_method,
        species_config = current_species_config(),
        all_species_data_obj = get_all_species_data(),
        transform_type = input$global_transform,
        plot_settings = current_settings
      )
    }
  })
  
  #pca debug output
  output$pca_debug_output <- renderPrint({
    if (!isTRUE(plot_state$pca_ready) || is.null(plot_state$pca_data)) return(invisible())
    
    pca_data <- plot_state$pca_data
    if (!is.null(pca_data$error)) {
      cat("ERROR:", pca_data$error, "\n")
      return()
    }
    
    if (pca_data$type == "single") {
      cat("Single species PCA completed for:", pca_data$species, "\n")
      cat("Number of genes analyzed:", pca_data$n_genes, "\n")
      cat("Number of samples:", pca_data$n_samples, "\n")
    } else {
      cat("Multi-species PCA completed\n")
      cat("Aggregation method:", pca_data$aggregation_method, "\n")
      
      method_desc <- switch(pca_data$aggregation_method,
                            "single_only" = "Using SINGLE-COPY GENES ONLY",
                            "mean" = "Averaging expression across paralogs",
                            "median" = "Using median expression",
                            "eigengene" = "Computing eigengene for each HOG",
                            "max_expr" = "Selecting highest expressed paralog",
                            "max_var" = "Selecting most variable paralog",
                            "var_weighted" = "Variance-weighted mean",
                            "Unknown method")
      cat(method_desc, "\n")
      
      if (!is.null(pca_data$matrices_data)) {
        cat("\nMatrix:", nrow(pca_data$matrices_data$sample_matrix), "x", ncol(pca_data$matrices_data$sample_matrix), "\n")
      }
    }
  })

  
  # Theme toggle
  observeEvent(input$theme_toggle, {
    current_dark <- is_dark()
    is_dark(!current_dark)
    
    if (!current_dark) {
      # Switching to dark mode
      addCssClass("html", "dark-mode")
      updateActionButton(session, "theme_toggle",
                        icon = icon("sun", verify_fa = FALSE))
      session$setCurrentTheme(dark_theme)
    } else {
      # Switching to light mode
      removeCssClass("html", "dark-mode")
      updateActionButton(session, "theme_toggle",
                        icon = icon("moon", verify_fa = FALSE))
      session$setCurrentTheme(light_theme)
    }
  })
  
  # Help modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Gene Expression Analysis Tutorial",
      
      tags$div(
        style = "font-size: 16px;",
        
        tags$h4("Basic Usage"),
        tags$ul(
          tags$li("Enter a gene name or ID in the search box (e.g., PHO4)"),
          tags$li("Click 'Search Gene' to find orthogroup members"),
          tags$li("Select genes from the orthogroup to analyze"),
          tags$li("Click 'Generate Plot' to visualize gene expression")
        ),
        
        tags$h4("New Features"),
        tags$ul(
          tags$li(strong("HOG-based Orthology:"), " Genes are now grouped by Hierarchical Orthologous Groups"),
          tags$li(strong("Gene Selection:"), " Choose specific genes from orthogroups for analysis"),
          tags$li(strong("Flexible Queries:"), " Captures paralogs and gene families, not just 1:1 orthologs"),
          tags$li(strong("Legacy Fallback:"), " Still searches YGOB/CGOB if genes aren't found in HOGs")
        ),
        
        tags$h4("Plot Interactions"),
        tags$ul(
          tags$li("Hover over points to see exact values"),
          tags$li("Use the toolbar to zoom, pan, or save the plot"),
          tags$li("Click legend items to show/hide species or replicates")
        ),
        
        tags$h4("Tips"),
        tags$ul(
          tags$li("Use either systematic names (e.g., CAGL0D05170g) or standard names (e.g., PHO4)"),
          tags$li("When multiple orthologs exist, select the most relevant for your analysis"),
          tags$li("Tables can be sorted by clicking column headers")
        )
      ),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  #settings modal
  observeEvent(input$show_settings, {
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    
    showModal(modalDialog(
      title = div(icon("gear"), " Plot Settings"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("settings_reset", "Reset to Defaults", class = "btn-secondary"),
        modalButton("Close")
      ),
      
      tabsetPanel(
        id = "settings_tabs",
        type = "tabs",
        
        #tab 1: species identity
        tabPanel(
          "Species",
          icon = icon("palette"),
          div(
            style = "padding: 15px;",
            h5("Species Color Palette"),
            selectInput("settings_species_palette", NULL,
                        choices = c("Dark2", "Set1", "Set2", "Paired", "Accent", "Okabe-Ito"),
                        selected = isolate(plot_settings$species_palette)),
            hr(),
            h5("Per-Species Colors"),
            p(class = "text-muted", "Click to customize individual species colors"),
            uiOutput("species_color_pickers"),
            hr(),
            h5("Per-Species Shapes"),
            uiOutput("species_shape_pickers")
          )
        ),
        
        #tab 2: multi-gene plots
        tabPanel(
          "Multi-Gene",
          icon = icon("chart-line"),
          div(
            style = "padding: 15px;",
            h5("Line/Point Plot Encoding"),
            radioButtons("settings_multigene_color", "Color represents:",
                         choices = c("Species" = "species", "Gene" = "gene"),
                         selected = isolate(plot_settings$encoding_multigene_color),
                         inline = TRUE),
            radioButtons("settings_multigene_secondary", "Secondary encoding:",
                         choices = c("Linetype" = "linetype", "Shape" = "shape", "None" = "none"),
                         selected = isolate(plot_settings$encoding_multigene_secondary),
                         inline = TRUE),
            conditionalPanel(
              condition = "input.settings_multigene_color == 'gene'",
              selectInput("settings_gene_palette", "Gene color palette:",
                          choices = c("Set1", "Set2", "Dark2", "Paired", "Accent", "Okabe-Ito"),
                          selected = isolate(plot_settings$gene_palette))
            ),
            hr(),
            h5("Heatmap Settings"),
            selectInput("settings_heatmap_palette", "Heatmap palette:",
                        choices = c("viridis", "plasma", "inferno", "magma", "cividis",
                                    "RdBu", "RdYlBu", "PiYG", "PRGn", "BrBG"),
                        selected = isolate(plot_settings$heatmap_palette)),
            radioButtons("settings_heatmap_scale", "Scale type:",
                         choices = c("Sequential" = "sequential", "Diverging" = "diverging"),
                         selected = isolate(plot_settings$heatmap_scale_type),
                         inline = TRUE),
            conditionalPanel(
              condition = "input.settings_heatmap_scale == 'diverging'",
              radioButtons("settings_heatmap_midpoint", "Center at:",
                           choices = c("Auto" = "auto", "Zero" = "zero", "Median" = "median"),
                           selected = isolate(plot_settings$heatmap_midpoint),
                           inline = TRUE)
            ),
            checkboxInput("settings_heatmap_row_dendro", "Show row dendrogram",
                          value = isolate(plot_settings$heatmap_show_row_dendro)),
            checkboxInput("settings_heatmap_col_dendro", "Show column dendrogram",
                          value = isolate(plot_settings$heatmap_show_col_dendro)),
            hr(),
            h5("Ridgeline Settings"),
            selectInput("settings_ridgeline_palette", "Ridgeline palette:",
                        choices = c("viridis", "plasma", "inferno", "magma", "cividis"),
                        selected = isolate(plot_settings$ridgeline_palette)),
            sliderInput("settings_ridgeline_alpha", "Opacity:",
                        min = 0.3, max = 1, value = isolate(plot_settings$ridgeline_alpha),
                        step = 0.1)
          )
        ),
        
        #tab 3: pca settings
        tabPanel(
          "PCA",
          icon = icon("project-diagram"),
          div(
            style = "padding: 15px;",
            h5("PCA Encoding"),
            selectInput("settings_pca_color", "Color represents:",
                        choices = c("Species" = "species", "Condition" = "condition",
                                    "Timepoint" = "timepoint"),
                        selected = isolate(plot_settings$encoding_pca_color)),
            selectInput("settings_pca_shape", "Shape represents:",
                        choices = c("Species" = "species", "Condition" = "condition",
                                    "None" = "none"),
                        selected = isolate(plot_settings$encoding_pca_shape)),
            hr(),
            h5("Appearance"),
            sliderInput("settings_pca_alpha", "Point opacity:",
                        min = 0.3, max = 1, value = isolate(plot_settings$pca_alpha),
                        step = 0.1),
            sliderInput("settings_pca_size", "Point size:",
                        min = 1, max = 6, value = isolate(plot_settings$pca_point_size),
                        step = 0.5),
            checkboxInput("settings_pca_ellipses", "Show confidence ellipses",
                          value = isolate(plot_settings$pca_show_ellipses)),
            checkboxInput("settings_pca_loadings", "Show gene loadings",
                          value = isolate(plot_settings$pca_show_loadings))
          )
        ),
        
        #tab 4: presets
        tabPanel(
          "Presets",
          icon = icon("save"),
          div(
            style = "padding: 15px;",
            h5("Load Preset"),
            selectInput("settings_load_preset", NULL,
                        choices = c("Select..." = "", names(isolate(plot_settings$presets)))),
            actionButton("settings_apply_preset", "Apply Preset", class = "btn-primary"),
            hr(),
            h5("Save Current Settings"),
            textInput("settings_preset_name", "Preset name:"),
            actionButton("settings_save_preset", "Save as Preset", class = "btn-success"),
            hr(),
            h5("Export Settings"),
            selectInput("settings_export_format", "Default export format:",
                        choices = c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"),
                        selected = isolate(plot_settings$export_format)),
            numericInput("settings_export_dpi", "DPI:", value = isolate(plot_settings$export_dpi),
                         min = 72, max = 600, step = 50)
          )
        )
      )
    ))
  })
  
  #render species color pickers
  output$species_color_pickers <- renderUI({
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    current_colors <- plot_settings$species_colors
    
    picker_list <- lapply(species_list, function(sp) {
      col <- if (sp %in% names(current_colors)) current_colors[[sp]] else "#808080"
      div(
        style = "display: inline-block; margin: 5px;",
        colourpicker::colourInput(
          paste0("species_color_", gsub("[^a-zA-Z0-9]", "_", sp)),
          label = sp,
          value = col,
          showColour = "background"
        )
      )
    })
    
    do.call(tagList, picker_list)
  })
  
  #render species shape pickers
  output$species_shape_pickers <- renderUI({
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    current_shapes <- plot_settings$species_shapes
    
    shape_choices <- c("Circle" = 16, "Triangle" = 17, "Square" = 15,
                       "Diamond" = 18, "Star" = 8, "Plus" = 3, "Cross" = 4)
    
    picker_list <- lapply(species_list, function(sp) {
      shp <- if (sp %in% names(current_shapes)) current_shapes[[sp]] else 16
      div(
        style = "display: inline-block; margin: 5px; min-width: 120px;",
        selectInput(
          paste0("species_shape_", gsub("[^a-zA-Z0-9]", "_", sp)),
          label = sp,
          choices = shape_choices,
          selected = shp
        )
      )
    })
    
    do.call(tagList, picker_list)
  })
  
  #update species colors when palette changes - regenerates all colors from palette
  observeEvent(input$settings_species_palette, {
    req(plot_settings$initialized)
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    plot_settings$species_palette <- input$settings_species_palette
    new_colors <- derive_species_colors(species_list, input$settings_species_palette, NULL, config)
    plot_settings$species_colors <- new_colors
    #flag to prevent individual picker observer from triggering during palette update
    plot_settings$updating_colors_from_palette <- TRUE
    #update colourInput UI elements to reflect new palette
    for (sp in species_list) {
      input_id <- paste0("species_color_", gsub("[^a-zA-Z0-9]", "_", sp))
      if (sp %in% names(new_colors)) {
        colourpicker::updateColourInput(session, input_id, value = new_colors[[sp]])
      }
    }
    #reset flag after client round-trip completes
    later::later(function() {
      plot_settings$updating_colors_from_palette <- FALSE
    }, delay = 0.3)
  })
  
  observeEvent(input$settings_multigene_color, {
    plot_settings$encoding_multigene_color <- input$settings_multigene_color
  })
  
  observeEvent(input$settings_multigene_secondary, {
    plot_settings$encoding_multigene_secondary <- input$settings_multigene_secondary
  })
  
  observeEvent(input$settings_gene_palette, {
    plot_settings$gene_palette <- input$settings_gene_palette
  })
  
  observeEvent(input$settings_heatmap_palette, {
    plot_settings$heatmap_palette <- input$settings_heatmap_palette
  })
  
  observeEvent(input$settings_heatmap_scale, {
    plot_settings$heatmap_scale_type <- input$settings_heatmap_scale
  })
  
  observeEvent(input$settings_heatmap_midpoint, {
    plot_settings$heatmap_midpoint <- input$settings_heatmap_midpoint
  })
  
  observeEvent(input$settings_heatmap_row_dendro, {
    plot_settings$heatmap_show_row_dendro <- input$settings_heatmap_row_dendro
  })
  
  observeEvent(input$settings_heatmap_col_dendro, {
    plot_settings$heatmap_show_col_dendro <- input$settings_heatmap_col_dendro
  })
  
  observeEvent(input$settings_ridgeline_palette, {
    plot_settings$ridgeline_palette <- input$settings_ridgeline_palette
  })
  
  observeEvent(input$settings_ridgeline_alpha, {
    plot_settings$ridgeline_alpha <- input$settings_ridgeline_alpha
  })
  
  observeEvent(input$settings_pca_color, {
    plot_settings$encoding_pca_color <- input$settings_pca_color
  })
  
  observeEvent(input$settings_pca_shape, {
    plot_settings$encoding_pca_shape <- input$settings_pca_shape
  })
  
  observeEvent(input$settings_pca_alpha, {
    plot_settings$pca_alpha <- input$settings_pca_alpha
  })
  
  observeEvent(input$settings_pca_size, {
    plot_settings$pca_point_size <- input$settings_pca_size
  })
  
  observeEvent(input$settings_pca_ellipses, {
    plot_settings$pca_show_ellipses <- input$settings_pca_ellipses
  })
  
  observeEvent(input$settings_pca_loadings, {
    plot_settings$pca_show_loadings <- input$settings_pca_loadings
  })
  
  observeEvent(input$settings_export_format, {
    plot_settings$export_format <- input$settings_export_format
  })
  
  observeEvent(input$settings_export_dpi, {
    plot_settings$export_dpi <- input$settings_export_dpi
  })
  
  #update species_colors directly when individual color pickers change
  observe({
    req(plot_settings$initialized)
    #skip if palette update is in progress to avoid reactive loop
    #use isolate to prevent flag changes from re-triggering this observer
    if (isTRUE(isolate(plot_settings$updating_colors_from_palette))) return()
    config <- isolate(current_species_config())
    species_list <- sapply(config, function(x) x$short)
    
    for (sp in species_list) {
      input_id <- paste0("species_color_", gsub("[^a-zA-Z0-9]", "_", sp))
      val <- input[[input_id]]
      #isolate stored color read so this observer only triggers on input changes
      stored_color <- isolate(plot_settings$species_colors[[sp]])
      if (!is.null(val) && !is.null(stored_color)) {
        if (val != stored_color) {
          plot_settings$species_colors[[sp]] <- val
          #also update full name key
          sp_full <- config[[names(config)[sapply(config, function(x) x$short == sp)]]]$name
          if (!is.null(sp_full)) {
            plot_settings$species_colors[[sp_full]] <- val
          }
        }
      }
    }
  })
  
  #update species_shapes directly when individual shape pickers change
  observe({
    req(plot_settings$initialized)
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    
    for (sp in species_list) {
      input_id <- paste0("species_shape_", gsub("[^a-zA-Z0-9]", "_", sp))
      val <- input[[input_id]]
      if (!is.null(val) && !is.null(plot_settings$species_shapes[[sp]])) {
        if (as.integer(val) != plot_settings$species_shapes[[sp]]) {
          plot_settings$species_shapes[[sp]] <- as.integer(val)
          #also update full name key
          sp_full <- config[[names(config)[sapply(config, function(x) x$short == sp)]]]$name
          if (!is.null(sp_full)) {
            plot_settings$species_shapes[[sp_full]] <- as.integer(val)
          }
        }
      }
    }
  })
  
  #apply preset
  observeEvent(input$settings_apply_preset, {
    req(input$settings_load_preset != "")
    preset <- plot_settings$presets[[input$settings_load_preset]]
    if (!is.null(preset)) {
      for (key in names(preset)) {
        if (key != "name" && key %in% names(reactiveValuesToList(plot_settings))) {
          plot_settings[[key]] <- preset[[key]]
        }
      }
      showNotification(paste("Applied preset:", preset$name), type = "message")
      removeModal()
    }
  })
  
  #save preset
  observeEvent(input$settings_save_preset, {
    req(input$settings_preset_name != "")
    preset_name <- input$settings_preset_name
    current <- reactiveValuesToList(plot_settings)
    current$name <- preset_name
    current$presets <- NULL
    current$initialized <- NULL
    plot_settings$presets[[preset_name]] <- current
    showNotification(paste("Saved preset:", preset_name), type = "success")
    updateTextInput(session, "settings_preset_name", value = "")
    updateSelectInput(session, "settings_load_preset",
                      choices = c("Select..." = "", names(plot_settings$presets)))
  })
  
  #reset to defaults
  observeEvent(input$settings_reset, {
    config <- current_species_config()
    defaults <- generate_default_settings(config)
    for (key in names(defaults)) {
      plot_settings[[key]] <- defaults[[key]]
    }
    showNotification("Settings reset to defaults", type = "message")
    removeModal()
  })
  
  #species data cache with composite key
  species_data_cache <- new.env()
  
  get_species_data <- function(species_id) {
    config <- current_species_config()
    current_source <- data_source()
    
    current_data <- if (current_source == "custom" && !is.null(upload_state$custom_all_species_data)) {
      upload_state$custom_all_species_data
    } else {
      all_species_data
    }
    
    #composite cache key includes data source to prevent stale data
    cache_key <- paste(species_id, current_source, sep = "_")
    
    if (!exists(cache_key, envir = species_data_cache)) {
      if (species_id %in% names(current_data)) {
        sp_config <- config[[species_id]]
        
        #strategy 1: direct naming (standard for uploaded data and cg)
        if (!is.null(current_data[[species_id]]$lcpm) || !is.null(current_data[[species_id]]$rlog)) {
          data <- current_data[[species_id]]
          
          if (is.null(data$species_name)) {
            data$species_name <- if(!is.null(sp_config$short)) sp_config$short else species_id
          }
          
          #add prefixed aliases for backward compatibility
          for (key in names(data)) {
            prefixed_key <- paste0(species_id, "_", key)
            if (is.null(data[[prefixed_key]])) {
              data[[prefixed_key]] <- data[[key]]
            }
          }
        } 
        #strategy 2: prefixed naming (for original sc, kl, ca data)
        else {
          data <- list(
            species_name = if(!is.null(sp_config$short)) sp_config$short else species_id
          )
          
          prefixed_keys <- names(current_data[[species_id]])
          prefix_pattern <- paste0("^", species_id, "_")
          
          for (key in prefixed_keys) {
            if (grepl(prefix_pattern, key)) {
              base_name <- sub(prefix_pattern, "", key)
              data[[key]] <- current_data[[species_id]][[key]]
              data[[base_name]] <- current_data[[species_id]][[key]]
            }
          }
        }
        
        #verify expression matrix exists
        has_expr_data <- any(sapply(names(data), function(nm) {
          obj <- data[[nm]]
          is.matrix(obj) || (is.data.frame(obj) && ncol(obj) > 5)
        }))
        
        if (!has_expr_data) {
          warning(paste("No expression data found for species:", species_id))
          return(NULL)
        }
        
        assign(cache_key, data, envir = species_data_cache)
      } else {
        return(NULL)
      }
    }
    get(cache_key, envir = species_data_cache)
  }
  
  # FIXED Combined view search handler
  observeEvent(input$combined_search_button, {
    waiter_show(html = loading_screen)
    
    gene <- trimws(input$combined_genename)
    
    if (gene == "") {
      showNotification("Please enter a gene name or ID", type = "warning")
      waiter_hide()
      return()
    }
    
    # Reset selections - store as character vectors to support multiple selections
    config <- current_species_config()
    for (sp_id in names(config)) {
      combined_selections[[sp_id]] <- character(0)
    }
    
    # Ensure containers exist for current configuration
    config <- current_species_config()
    
    # Check if containers need refresh
    if (length(names(existing_containers)) != length(names(config))) {
      manage_combined_containers(config)
    }
    
    # Search for the gene in all species
    found_in_species <- NULL
    config <- current_species_config()
    current_data <- get_all_species_data()
    for (species_id in names(config)) {
      species_data <- get_species_data(species_id)
      result <- query_gene_flexible(gene, species_data, current_data)
      if (!is.null(result) && result$source != "none") {
        found_in_species <- species_id
        query_results$combined <- result
        break
      }
    }
    
    if (!is.null(found_in_species)) {
      # Show orthogroup container
      shinyjs::show("combined_orthogroup_container")
      
      # Get current configuration
      config <- current_species_config()
      
      # Create selection UI for each species
      for (sp_id in names(config)) {
        local({
          species_id <- sp_id
          species_name <- config[[species_id]]$short
          
          # Check if this species has genes
          has_genes <- species_id %in% names(query_results$combined$genes_by_species) &&
                       !is.null(query_results$combined$genes_by_species[[species_id]]) &&
                       nrow(query_results$combined$genes_by_species[[species_id]]) > 0
          
          if (has_genes) {
            genes_df <- query_results$combined$genes_by_species[[species_id]]
            
            # Set default selection(s)
            combined_selections[[species_id]] <- genes_df$gene_id[1]
            
            # Create the UI content
            ui_content <- div(
              h6(
                tags$em(species_name),
                if(nrow(genes_df) > 1) {
                  span(
                    class = "badge bg-warning text-dark ms-2",
                    style = "font-size: 0.8em;",
                    paste(nrow(genes_df), "paralogs")
                  )
                }
              ),
              if (nrow(genes_df) == 1) {
                tagList(
                  div(
                    class = "alert alert-success py-2 px-3 mb-2",
                    style = "font-size: 0.9em;",
                    icon("check-circle"),
                    " Single copy: ", genes_df$display[1]
                  ),
                  # Hidden checkbox for consistency
                  div(
                    style = "display: none;",
                    checkboxGroupInput(
                      inputId = paste0("combined_", species_id, "_selection"),
                      label = NULL,
                      choices = setNames(genes_df$gene_id, genes_df$display),
                      selected = genes_df$gene_id[1]
                    )
                  )
                )
              } else {
                tagList(
                  div(
                    class = "alert alert-info py-2 px-3 mb-2",
                    style = "font-size: 0.9em;",
                    icon("info-circle"),
                    " Multiple paralogs found. Select one or more to compare:"
                  ),
                  checkboxGroupInput(
                    inputId = paste0("combined_", species_id, "_selection"),
                    label = NULL,
                    choices = setNames(genes_df$gene_id, genes_df$display),
                    selected = genes_df$gene_id[1]
                  ),
                  tags$small(
                    class = "text-muted",
                    "Select multiple paralogs to compare their expression patterns"
                  )
                )
              }
            )
            
            # Use the existing container
            container_id <- paste0("combined_", species_id, "_selection_ui")
            container_selector <- paste0("#", container_id)
            
            # Clear existing content in container only
            removeUI(
              selector = paste0(container_selector, " > *"),
              multiple = TRUE,
              immediate = TRUE
            )
            
            # Insert new content directly
            insertUI(
              selector = container_selector,
              where = "afterBegin",
              ui = ui_content,
              immediate = TRUE
            )
            
          } else {
            # No genes found for this species
            ui_content <- div(
              h6(tags$em(species_name)),
              p("No genes found in this species", style = "color: #999;")
            )
            
            container_selector <- paste0("#combined_", species_id, "_selection_ui")
            
            # Clear existing content first
            removeUI(
              selector = paste0(container_selector, " > *"),
              multiple = TRUE,
              immediate = TRUE
            )
            
            insertUI(
              selector = container_selector,
              where = "afterBegin",
              ui = ui_content,
              immediate = TRUE
            )
          }
        })
      }
      
      shinyjs::delay(100, {
        config <- current_species_config()
        for (sp_id in names(config)) {
          local({
            species_id <- sp_id
            
            # Only create observer if there are genes for this species
            if (species_id %in% names(query_results$combined$genes_by_species) &&
                nrow(query_results$combined$genes_by_species[[species_id]]) > 0) {
              
              observeEvent(input[[paste0("combined_", species_id, "_selection")]], {
                new_selection <- input[[paste0("combined_", species_id, "_selection")]]
                combined_selections[[species_id]] <- new_selection
                
                # Update the table
                output$combined_orthogroup_table <- renderDT({
                  update_combined_table(query_results, combined_selections, is_dark)
                })
                
                # Update selection summary
                output$combined_selection_summary <- renderUI({
                  config <- current_species_config()
                  selected_count <- sum(sapply(names(config), function(sp) {
                    length(combined_selections[[sp]])
                  }))
                  
                  if (selected_count > 0) {
                    div(
                      icon("check"),
                      paste(selected_count, "gene(s) selected for comparison")
                    )
                  } else {
                    div(
                      icon("info-circle"),
                      "No genes selected yet"
                    )
                  }
                })
              }, ignoreInit = TRUE, ignoreNULL = FALSE)
            }
          })
        }
      })
      
      # Initial table render
      output$combined_orthogroup_table <- renderDT({
        update_combined_table(query_results, combined_selections, is_dark)
      })
      
      # Initial selection summary
      output$combined_selection_summary <- renderUI({
        div(
          icon("info-circle"),
          "Genes selected with default options"
        )
      })
      
      # Auto-click plot button if all species have only single genes
      shinyjs::delay(500, {
        all_single <- TRUE
        config <- current_species_config()
        for (sp in names(config)) {
          if (sp %in% names(query_results$combined$genes_by_species)) {
            if (nrow(query_results$combined$genes_by_species[[sp]]) > 1) {
              all_single <- FALSE
              break
            }
          }
        }
        
        if (all_single) {
          shinyjs::click("combined_plot_button")
        }
      })
      
    } else {
      shinyjs::hide("combined_orthogroup_container")
      showNotification("Gene not found in any species", type = "error")
    }
    
    waiter_hide()
  })
  
  #track created observers for cleanup
  if (is.null(session$userData$created_species_observers)) {
    session$userData$created_species_observers <- character(0)
  }
  
  #observers dynamically based on current configuration
  observe({
    config <- current_species_config()
    current_species <- names(config)
    
    #cleanup observers for removed species
    existing_observers <- session$userData$created_species_observers
    removed_species <- setdiff(existing_observers, current_species)
    
    for (sp_id in removed_species) {
      #destroy observers for removed species
      obs_names <- c(
        paste0("obs_search_", sp_id),
        paste0("obs_plot_", sp_id),
        paste0("obs_download_", sp_id)
      )
      for (obs_name in obs_names) {
        if (exists(obs_name, envir = session$userData)) {
          obs <- session$userData[[obs_name]]
          if (inherits(obs, "Observer")) {
            obs$destroy()
          }
          rm(list = obs_name, envir = session$userData)
        }
      }
    }
    session$userData$created_species_observers <- current_species
    
    #create observers for each species
    lapply(current_species, function(species_id) {
      obs_search_id <- paste0("obs_search_", species_id)
      obs_plot_id <- paste0("obs_plot_", species_id)
      obs_download_id <- paste0("obs_download_", species_id)
      
      if (!exists(obs_search_id, envir = session$userData)) {
        #search button handler
        session$userData[[obs_search_id]] <- observeEvent(input[[paste0(species_id, "_search_button")]], {
          waiter_show(html = loading_screen)
          
          gene_query <- trimws(input[[paste0(species_id, "_genename")]])
          
          if (gene_query == "") {
            showNotification("Please enter a gene name or ID", type = "warning")
            waiter_hide()
            return()
          }
          
          # Store query results in reactive values
          query_results[[species_id]] <- query_orthogroups(
            gene_query, 
            get_all_species_data(), 
            current_species_config(),
            get_species_data
          )
          gene_result <- query_results[[species_id]]
          
          if (is.null(gene_result)) {
            showNotification(paste("Gene", gene_query, "not found"), type = "error")
            waiter_hide()
            return()
          }
          
          # Show orthogroup container
          shinyjs::show(paste0(species_id, "_orthogroup_container"))
          
          # Update the orthogroup selection UI with enhanced version
          output[[paste0(species_id, "_orthogroup_selection")]] <- renderUI({
            create_orthogroup_selection_ui_enhanced(gene_result, species_id, current_species_config())
          })
          
          # Get genes for this species
          current_species_genes <- gene_result$genes_by_species[[species_id]]
          
          if (!is.null(current_species_genes) && nrow(current_species_genes) > 0) {
            # Auto-click plot button if only one gene
            if (nrow(current_species_genes) == 1) {
              # Add a small delay to ensure UI is rendered
              shinyjs::delay(100, shinyjs::click(paste0(species_id, "_plot_button")))
            }
          }
          
          if (is.null(current_species_genes) || nrow(current_species_genes) == 0) {
            # Gene not found in this species
            updateRadioButtons(
              session,
              paste0(species_id, "_", species_id, "_selection"),
              label = NULL,
              choices = c("No genes found in this species" = ""),
              selected = ""
            )
            config <- current_species_config()
            showNotification(
              paste("Gene", gene_query, "not found in", config[[species_id]]$name,
                    "but found in other species"), 
              type = "warning",
              duration = 5
            )
          } else {
            # Update radio buttons with actual genes
            genes_df <- current_species_genes
            gene_choices <- setNames(genes_df$gene_id, genes_df$display)
            
            updateRadioButtons(
              session,
              paste0(species_id, "_", species_id, "_selection"),
              label = NULL,
              choices = gene_choices,
              selected = genes_df$gene_id[1]
            )
            
            # Auto-click plot button if only one gene
            if (nrow(genes_df) == 1) {
              shinyjs::click(paste0(species_id, "_plot_button"))
            }
          }
          
          # Update search results table
          output[[paste0(species_id, "_search_results")]] <- renderDT({
            if (!is.null(gene_result$genes_by_species[[species_id]]) && 
                nrow(gene_result$genes_by_species[[species_id]]) > 0) {
              species_genes <- gene_result$genes_by_species[[species_id]]
              datatable(
                species_genes[, c("gene_id", "gene_name")],
                options = list(
                  pageLength = 5,
                  dom = 'tp',
                  scrollX = TRUE
                ),
                colnames = c("Gene ID", "Gene Name"),
                rownames = FALSE
              )
            }
          })
          
          #update orthogroup results table
          output[[paste0(species_id, "_orthogroup_results")]] <- renderDT({
            config <- current_species_config()
            
            #collect data in list first, then combine once
            ortho_list <- lapply(names(gene_result$genes_by_species), function(sp) {
              sp_data <- gene_result$genes_by_species[[sp]]
              if (nrow(sp_data) == 0) return(NULL)
              sp_data$Species <- config[[sp]]$short
              sp_data$Current <- (sp == species_id)
              sp_data[, c("Species", "gene_id", "gene_name", "Current")]
            })
            
            ortho_data <- rbindlist(Filter(Negate(is.null), ortho_list), fill = TRUE)
            
            if (nrow(ortho_data) > 0) {
              ortho_data <- ortho_data[order(ortho_data$Current, decreasing = TRUE), ]
              
              dt <- datatable(
                ortho_data[, c("Species", "gene_id", "gene_name")],
                options = list(
                  pageLength = 10,
                  dom = 'tp',
                  scrollX = TRUE
                ),
                colnames = c("Species", "Gene ID", "Gene Name"),
                rownames = FALSE
              ) %>%
                formatStyle("Species", fontStyle = "italic")
              
              if (any(ortho_data$Current)) {
                dt <- dt %>% formatStyle(
                  columns = 1:3,
                  target = "row",
                  backgroundColor = styleRow(which(ortho_data$Current), 
                                             if(is_dark()) "#3a4a5a" else "#e6f3ff")
                )
              }
              
              return(dt)
            }
          })
          
          waiter_hide()
        })
      }
      
      #plot button handler - stores state for reactive rendering
      if (!exists(obs_plot_id, envir = session$userData)) {
        session$userData[[obs_plot_id]] <- observeEvent(input[[paste0(species_id, "_plot_button")]], {
          waiter_show(html = loading_screen)
          
          selected_gene <- input[[paste0(species_id, "_", species_id, "_selection")]]
          
          if (is.null(selected_gene) || selected_gene == "") {
            showNotification("Please select a gene", type = "warning")
            waiter_hide()
            return()
          }
          
          plot_state$species_plots[[species_id]] <- list(
            gene = selected_gene,
            ready = TRUE
          )
          
          waiter_hide()
        })
        
        #species gene plot render - reactive to plot_state and plot_settings
        local({
          sp_id <- species_id
          output[[paste0(sp_id, "_gene_plot")]] <- renderPlotly({
            sp_state <- plot_state$species_plots[[sp_id]]
            if (is.null(sp_state) || !isTRUE(sp_state$ready)) {
              return(plotly_empty() %>% add_annotations(text = "Search for a gene and click Generate Plot", showarrow = FALSE))
            }
            
            species_data <- get_species_data(sp_id)
            config <- current_species_config()
            current_settings <- reactiveValuesToList(plot_settings)
            
            create_gene_plot(
              lc = get_expression_matrix(sp_id, input$global_transform, species_data),
              gene = sp_state$gene,
              sample_info = species_data$sample_info,
              species_name = config[[sp_id]]$short,
              is_dark_mode = is_dark(),
              species_colors = species_colors_dynamic(),
              transform_type = input$global_transform,
              plot_settings = current_settings
            )
          })
          
          output[[paste0(sp_id, "_gene_info")]] <- renderText({
            sp_state <- plot_state$species_plots[[sp_id]]
            if (is.null(sp_state) || !isTRUE(sp_state$ready)) return("")
            
            species_data <- get_species_data(sp_id)
            gene_info <- species_data$anno[species_data$anno$GeneID == sp_state$gene, ]
            if (nrow(gene_info) > 0) {
              paste("Gene ID:", sp_state$gene, "\nGene Name:", gene_info$GeneName[1], "\nChromosome:", gene_info$Chr[1])
            } else {
              paste("Gene ID:", sp_state$gene)
            }
          })
        })
      }
      
      # Download handler
      if (!exists(obs_download_id, envir = session$userData)) {
        output[[paste0(species_id, "_download")]] <- downloadHandler(
          filename = function() {
            paste0(SPECIES_CONFIG[[species_id]]$short, "_gene_expression_", Sys.Date(), ".png")
          },
          content = function(file) {
            selected_gene <- input[[paste0(species_id, "_", species_id, "_selection")]]
            if (!is.null(selected_gene) && selected_gene != "") {
              species_data <- get_species_data(species_id)
              config <- current_species_config()
              p <- create_gene_plot(
                lc = get_expression_matrix(species_id, input$global_transform, species_data),
                gene = selected_gene,
                sample_info = species_data$sample_info,
                species_name = config[[species_id]]$short,
                is_dark_mode = is_dark(),
                species_colors = species_colors_dynamic(),
                transform_type = input$global_transform,
                plot_settings = reactiveValuesToList(plot_settings)
              )
              orca(p, file)
            }
          }
        )
        session$userData[[obs_download_id]] <- TRUE
      }
    })
  })
  
  #download handler for orthology matrix
  output$download_orthology_matrix <- downloadHandler(
    filename = function() {
      paste("HOG_expression_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      waiter_show(html = loading_screen)
      
      tryCatch({
        # Use stored data if available
        if (!is.null(session$userData$pca_matrices)) {
          result <- session$userData$pca_matrices
        } else {
          # Fall back to extraction
          result <- extract_orthology_matrix()
        }
        
        write.csv(result$sample_matrix, file)
        
        # Also save metadata
        dir_path <- dirname(file)
        base_name <- tools::file_path_sans_ext(basename(file))
        
        write.csv(
          result$sample_metadata,
          file.path(dir_path, paste0(base_name, "_metadata.csv")),
          row.names = FALSE
        )
        
        showNotification(
          "HOG matrix exported successfully",
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Error generating matrix:", e$message),
          type = "error",
          duration = NULL
        )
      })
      
      waiter_hide()
    }
  )
  
  #combined plot button observer - stores data for reactive rendering
  observeEvent(input$combined_plot_button, {
    req(input$species_select)
    waiter_show(html = loading_screen)
    
    selected_genes_list <- list()
    for (species_code in input$species_select) {
      gene_ids <- combined_selections[[species_code]]
      if (!is.null(gene_ids) && length(gene_ids) > 0) {
        selected_genes_list[[species_code]] <- gene_ids
      }
    }
    
    if (length(selected_genes_list) == 0) {
      showNotification("No genes selected for plotting. Please search for a gene first.", type = "warning")
      plot_state$combined_ready <- FALSE
      waiter_hide()
      return()
    }
    
    config <- current_species_config()
    plot_data_list <- list()
    
    for (species_code in names(selected_genes_list)) {
      species_data <- get_species_data(species_code)
      gene_ids <- selected_genes_list[[species_code]]
      expr_matrix <- get_expression_matrix(species_code, input$global_transform, species_data)
      
      for (gene_id in gene_ids) {
        gene_id_to_use <- gene_id
        if (data_source() == "default" && species_code == "kl" && !gene_id %in% rownames(expr_matrix)) {
          gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
          if (gene_id_alt %in% rownames(expr_matrix)) {
            gene_id_to_use <- gene_id_alt
          }
        }
        
        if (gene_id_to_use %in% rownames(expr_matrix)) {
          species_name <- config[[species_code]]$name
          gene_name <- ""
          anno_idx <- which(species_data$anno$GeneID == gene_id)
          if (length(anno_idx) > 0) {
            gene_name <- species_data$anno$GeneName[anno_idx[1]]
            if (is.na(gene_name)) gene_name <- ""
          }
          
          gene_display <- if (gene_name != "") paste0(gene_name, " (", gene_id, ")") else gene_id
          
          expr_data <- data.frame(
            Gene = gene_display,
            GeneID = gene_id,
            Species = species_name,
            SpeciesCode = species_code,
            Timepoint = factor(species_data$sample_info$Timepoint, levels = TIME_POINTS),
            Replicate = species_data$sample_info$Replicate,
            Expression = as.numeric(expr_matrix[gene_id_to_use, ])
          )
          plot_data_list[[length(plot_data_list) + 1]] <- expr_data
        }
      }
    }
    
    plot_data <- if (length(plot_data_list) > 0) {
      as.data.frame(rbindlist(plot_data_list, fill = TRUE))
    } else {
      data.frame()
    }
    
    if (nrow(plot_data) > 0) {
      plot_state$combined_data <- plot_data
      plot_state$combined_ready <- TRUE
    } else {
      plot_state$combined_ready <- FALSE
      showNotification("No valid expression data found for selected genes", type = "error")
    }
    
    waiter_hide()
  })
  
  #combined gene plot render - reactive to plot_state and plot_settings
  output$combined_gene_plot <- renderPlotly({
    if (!isTRUE(plot_state$combined_ready) || is.null(plot_state$combined_data)) {
      return(plotly_empty() %>% add_annotations(text = "Search for a gene and click Generate Plot", showarrow = FALSE))
    }
    
    plot_data <- plot_state$combined_data
    dark_mode <- is_dark()
    config <- current_species_config()
    settings_colors <- plot_settings$species_colors
    settings_shapes <- plot_settings$species_shapes
    encoding_color <- plot_settings$encoding_multigene_color
    encoding_secondary <- plot_settings$encoding_multigene_secondary
    normalize_baseline <- isTRUE(input$normalize_to_baseline)
    transform_type <- input$global_transform
    
    if (normalize_baseline) {
      plot_data <- plot_data %>%
        group_by(Gene, Species, Replicate) %>%
        mutate(
          Baseline = Expression[Timepoint == "0min"][1],
          Expression = Expression - Baseline
        ) %>%
        ungroup() %>%
        select(-Baseline)
    }
    
    #build color map using SpeciesCode for reliable lookup
    species_color_map <- list()
    species_shape_map <- list()
    for (sp_code in unique(plot_data$SpeciesCode)) {
      sp_short <- config[[sp_code]]$short
      species_color_map[[sp_code]] <- resolve_species_color(sp_short, settings_colors, "#808080")
      species_shape_map[[sp_code]] <- resolve_species_shape(sp_short, settings_shapes, 16L)
    }
    
    plot_data$GeneSpecies <- paste(plot_data$Gene, "-", plot_data$Species)
    plot_data$GeneSpeciesRep <- paste(plot_data$GeneSpecies, "Rep", plot_data$Replicate)
    plot_data$SpeciesColor <- sapply(plot_data$SpeciesCode, function(sc) species_color_map[[sc]])
    plot_data$SpeciesShape <- sapply(plot_data$SpeciesCode, function(sc) species_shape_map[[sc]])
    
    unique_combinations <- unique(plot_data$GeneSpecies)
    unique_genes <- unique(plot_data$Gene)
    unique_species <- unique(plot_data$SpeciesCode)
    n_genes <- length(unique_genes)
    n_species <- length(unique_species)
    
    #build color vector for gene-species-replicate combinations
    color_vector <- c()
    rep_labels <- c()
    for (combo in unique_combinations) {
      row_match <- plot_data[plot_data$GeneSpecies == combo, ][1, ]
      base_color <- species_color_map[[row_match$SpeciesCode]]
      color_vector <- c(color_vector, base_color, adjustcolor(base_color, alpha.f = 0.6))
      rep_labels <- c(rep_labels, paste(combo, "Rep 1"), paste(combo, "Rep 2"))
    }
    names(color_vector) <- rep_labels
    
    #build shape mapping based on encoding settings
    shape_aes <- NULL
    shape_scale <- NULL
    if (encoding_secondary == "shape" && n_genes <= 6) {
      plot_data$ShapeVar <- plot_data$Gene
      shape_aes <- aes(shape = ShapeVar)
      gene_shapes <- setNames(SHAPES_DEFAULT[1:n_genes], unique_genes)
      shape_scale <- scale_shape_manual(values = gene_shapes, name = "Gene")
    } else if (encoding_secondary == "shape" && n_species <= 6) {
      plot_data$ShapeVar <- plot_data$SpeciesCode
      shape_aes <- aes(shape = ShapeVar)
      species_shape_vec <- sapply(unique_species, function(sc) species_shape_map[[sc]])
      names(species_shape_vec) <- unique_species
      shape_scale <- scale_shape_manual(values = species_shape_vec, name = "Species")
    }
    
    #build linetype mapping based on encoding settings
    linetype_aes <- NULL
    linetype_scale <- NULL
    if (encoding_secondary == "linetype" && n_genes <= 6) {
      plot_data$LinetypeVar <- plot_data$Gene
      linetype_aes <- aes(linetype = LinetypeVar)
      gene_linetypes <- setNames(LINETYPES_DEFAULT[1:n_genes], unique_genes)
      linetype_scale <- scale_linetype_manual(values = gene_linetypes, name = "Gene")
    }
    
    expr_label <- if (normalize_baseline) "log2 FC" else "Expression"
    
    p <- ggplot(plot_data,
                aes(x = Timepoint, y = Expression, color = GeneSpeciesRep, group = GeneSpeciesRep,
                    text = paste("Gene:", Gene, "<br>Species:", Species, "<br>Replicate:", Replicate,
                                 "<br>Time:", Timepoint, paste0("<br>", expr_label, ": "), round(Expression, 2))))
    
    #add shape aesthetic if configured
    if (!is.null(shape_aes)) {
      p <- p + shape_aes
    }
    
    #add linetype aesthetic if configured
    if (!is.null(linetype_aes)) {
      p <- p + linetype_aes
    }
    
    p <- p +
      geom_point(size = 3, alpha = 0.8) +
      geom_line(linewidth = 1.2, alpha = 0.9) +
      scale_color_manual(values = color_vector, name = "Gene - Species", breaks = names(color_vector), labels = names(color_vector))
    
    #add shape scale if configured
    if (!is.null(shape_scale)) {
      p <- p + shape_scale
    }
    
    #add linetype scale if configured
    if (!is.null(linetype_scale)) {
      p <- p + linetype_scale
    }
    
    p <- p +
      labs(
        y = if (normalize_baseline) "log2 Fold-Change (vs. 0 min)" else get_expression_label(transform_type),
        title = "Cross-species Expression Comparison",
        subtitle = paste("Comparing", length(unique(plot_data$Gene)), "genes across", length(unique(plot_data$Species)), "species"),
        x = "Timepoint"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major = element_line(color = if(dark_mode) "gray30" else "gray90"),
        panel.grid.minor = element_line(color = if(dark_mode) "gray20" else "gray95"),
        plot.background = element_rect(fill = if(dark_mode) "#2c3034" else "white", color = NA),
        panel.background = element_rect(fill = if(dark_mode) "#2c3034" else "white", color = NA),
        text = element_text(color = if(dark_mode) "white" else "black"),
        axis.text = element_text(color = if(dark_mode) "white" else "black"),
        legend.text = element_text(color = if(dark_mode) "white" else "black", size = 9),
        legend.title = element_text(color = if(dark_mode) "white" else "black", size = 11),
        legend.background = element_rect(fill = if(dark_mode) "#2c3034" else "white"),
        legend.key = element_rect(fill = if(dark_mode) "#2c3034" else "white")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor = if(dark_mode) "#2c3034" else "white",
        paper_bgcolor = if(dark_mode) "#2c3034" else "white",
        font = list(color = if(dark_mode) "white" else "black"),
        hoverlabel = list(bgcolor = if(dark_mode) "#444" else "white", font = list(size = 12)),
        showlegend = TRUE,
        legend = list(x = 1.02, y = 0.5, bgcolor = if(dark_mode) "#2c3034" else "white",
                      bordercolor = if(dark_mode) "#444" else "#ddd", borderwidth = 1, font = list(size = 10), tracegroupgap = 5),
        margin = list(b = 100, r = 250, t = 80, l = 60),
        xaxis = list(tickfont = list(size = 11)),
        yaxis = list(tickfont = list(size = 11))
      ) %>%
      config(displayModeBar = TRUE, modeBarButtons = list(list("zoom2d", "pan2d", "resetScale2d", "toImage")))
  })
  
  # Download handler for combined plot
  output$download_combined_plot <- downloadHandler(
    filename = function() {
      paste0("combined_expression_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Get current plot
      p <- plotly::plotly_build(isolate(output$combined_gene_plot()))
      plotly::export(p, file = file)
    }
  )
  
  # Store heatmap data for download
  ortholog_result <- reactiveVal(NULL)

  # Process gene list from textarea or file
  observe({
    if (!is.null(input$ortholog_gene_file$datapath)) {
      file_ext <- tools::file_ext(input$ortholog_gene_file$name)
      
      if (file_ext == "csv") {
        genes_df <- read.csv(input$ortholog_gene_file$datapath, stringsAsFactors = FALSE)
        if (ncol(genes_df) == 1) {
          gene_list <- genes_df[[1]]
        } else {
          gene_list <- as.character(genes_df[,1])
        }
      } else {
        # txt file
        gene_list <- readLines(input$ortholog_gene_file$datapath)
      }
      
      # update the textarea with the file contents
      updateTextAreaInput(session, "ortholog_gene_list", value = paste(gene_list, collapse = "\n"))
    }
  })

  # Generate heatmap when button is clicked
  #heatmap observer - stores data for reactive rendering
  observeEvent(input$generate_ortholog_heatmap, {
    req(input$ortholog_gene_list)
    waiter_show(html = loading_screen)
    
    gene_list <- unlist(strsplit(input$ortholog_gene_list, "[\n\r,;]+"))
    gene_list <- trimws(gene_list)
    gene_list <- gene_list[gene_list != ""]
    
    if (length(gene_list) == 0) {
      showNotification("Please enter at least one gene", type = "error")
      plot_state$heatmap_ready <- FALSE
      waiter_hide()
      return()
    }
    
    config <- current_species_config()
    species_data_list <- list()
    for (species_id in names(config)) {
      species_data_list[[species_id]] <- get_species_data(species_id)
    }
    
    plot_state$heatmap_data <- list(
      gene_list = gene_list,
      species_data_list = species_data_list,
      normalization = input$heatmap_normalization,
      cluster_rows = input$cluster_rows,
      cluster_cols = input$cluster_cols,
      config = config,
      transform_type = input$global_transform
    )
    plot_state$heatmap_ready <- TRUE
    
    waiter_hide()
  })
  
  #heatmap plot render - reactive to plot_state and plot_settings
  output$ortholog_heatmap_plot <- renderPlotly({
    if (!isTRUE(plot_state$heatmap_ready) || is.null(plot_state$heatmap_data)) {
      return(plotly_empty() %>% add_annotations(text = "Enter genes and click Generate Heatmap", showarrow = FALSE))
    }
    
    hd <- plot_state$heatmap_data
    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)
    
    result <- tryCatch({
      generate_cross_species_heatmap(
        gene_list = hd$gene_list,
        species_data_list = hd$species_data_list,
        normalization = hd$normalization,
        is_dark_mode = dark_mode,
        cluster_rows = hd$cluster_rows,
        cluster_cols = hd$cluster_cols,
        config = hd$config,
        all_species_data = get_all_species_data(),
        transform_type = hd$transform_type,
        plot_settings = current_settings
      )
    }, error = function(e) {
      list(plot = NULL, error = e$message)
    })
    
    if (!is.null(result$error)) {
      return(plotly_empty() %>% add_annotations(text = paste("Error:", result$error), showarrow = FALSE))
    }
    
    ortholog_result(result)
    result$plot
  })
  
  #heatmap table render
  output$ortholog_mapping_table <- renderDT({
    req(ortholog_result())
    ortholog_result()$table
  })

  # Download handlers
  output$download_ortholog_heatmap <- downloadHandler(
    filename = function() {
      paste("cross_species_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      # make sure we have a result
      req(ortholog_result())
      result <- ortholog_result()
      
      # save plot to file (using plotly export)
      p <- result$plot
      plotly::export(p, file = file)
    }
  )

  output$download_ortholog_data <- downloadHandler(
    filename = function() {
      paste("cross_species_expression_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # make sure we have a result
      req(ortholog_result())
      result <- ortholog_result()
      
      # Use the stored matrix directly if available
      if (!is.null(result$matrix)) {
        write.csv(result$matrix, file)
      } else {
        # Fallback: extract the matrix from the correct plot element
        p <- result$plot
        
        # The subplot has multiple data elements - find the main heatmap
        mat_data <- NULL
        if (length(p$x$data) > 1) {
          # Look for the data element with more than 1 row (not the species bar)
          for (i in seq_along(p$x$data)) {
            if (!is.null(p$x$data[[i]]$z) && is.matrix(p$x$data[[i]]$z)) {
              if (nrow(p$x$data[[i]]$z) > 1) {
                mat_data <- p$x$data[[i]]
                break
              }
            }
          }
        }
        
        # If still not found, use the first element as fallback
        if (is.null(mat_data)) {
          mat_data <- p$x$data[[1]]
        }
        
        # recreate the matrix
        genes <- mat_data$y
        timepoints <- mat_data$x
        expression_values <- mat_data$z
        
        # create a data frame from the matrix
        matrix_df <- as.data.frame(expression_values)
        rownames(matrix_df) <- genes
        colnames(matrix_df) <- timepoints
        
        # write to CSV
        write.csv(matrix_df, file)
      }
    }
  )
  
  #observer for ortholog mapping when checkbox is toggled
  observeEvent(input$enable_ortholog_analysis, {
    if (input$enable_ortholog_analysis) {
      #get gene list from either source
      gene_list <- NULL
      
      #check pathway definitions first if in pathway mode
      if (!is.null(input$enable_pathway_comparison) && input$enable_pathway_comparison) {
        pathway_text <- input$pathway_definitions
        if (!is.null(pathway_text) && nchar(trimws(pathway_text)) > 0) {
          #extract all genes from pathway definitions
          normalized <- gsub("\r\n", "\n", pathway_text)
          normalized <- gsub("\r", "\n", normalized)
          lines <- strsplit(normalized, "\n")[[1]]
          
          all_genes <- c()
          is_gene_line <- FALSE
          
          for (line in lines) {
            line <- trimws(line)
            if (line == "") {
              is_gene_line <- FALSE
            } else if (!is_gene_line) {
              #pathway name line, next line is genes
              is_gene_line <- TRUE
            } else {
              #gene line
              genes <- strsplit(line, "[,;\\s]+")[[1]]
              genes <- trimws(genes)
              genes <- genes[genes != ""]
              all_genes <- c(all_genes, genes)
            }
          }
          
          gene_list <- unique(all_genes)
        }
      }
      
      #fallback to regular gene list
      if (is.null(gene_list) || length(gene_list) == 0) {
        if (!is.null(input$gene_list) && nchar(trimws(input$gene_list)) > 0) {
          genes <- strsplit(trimws(input$gene_list), "[\n\r]+")[[1]]
          gene_list <- genes[genes != ""]
        }
      }
      
      if (is.null(gene_list) || length(gene_list) == 0) {
        showNotification("Please enter a gene list or pathway definitions first", type = "warning", duration = 3)
        updateCheckboxInput(session, "enable_ortholog_analysis", value = FALSE)
        return()
      }
      
      #perform initial mapping
      perform_ortholog_mapping(gene_list)
      
    } else {
      #reset when unchecked
      ortholog_state$mapped <- FALSE
      ortholog_state$gene_mapping <- NULL
      ortholog_state$coverage_stats <- NULL
      shinyjs::hide("ortholog_mapping_results")
    }
  })
  
  #reusable function to perform ortholog mapping
  perform_ortholog_mapping <- function(gene_list) {
    if (is.null(gene_list) || length(gene_list) == 0) {
      showNotification("No gene list to map", type = "warning", duration = 3)
      return(FALSE)
    }
    
    waiter_show(html = loading_screen)
    
    config <- current_species_config()
    current_data <- get_all_species_data()
    
    tryCatch({
      gene_mapping <- extract_orthology_for_genes(gene_list, current_data, config)
      
      if (is.null(gene_mapping) || length(gene_mapping) == 0) {
        showNotification("No orthologs found for input genes", type = "warning", duration = 5)
        ortholog_state$mapped <- FALSE
        shinyjs::hide("ortholog_mapping_results")
        waiter_hide()
        return(FALSE)
      }
      
      coverage_stats <- calculate_ortholog_coverage(gene_mapping, config)
      
      ortholog_state$mapped <- TRUE
      ortholog_state$gene_mapping <- gene_mapping
      ortholog_state$coverage_stats <- coverage_stats
      
      shinyjs::show("ortholog_mapping_results")
      
      #update coverage display
      output$ortholog_coverage_summary <- renderUI({
        if (is.null(coverage_stats)) return(NULL)
        
        div(
          class = "ortholog-summary-box",
          h6(icon("chart-bar"), " Ortholog Coverage by Species"),
          lapply(names(coverage_stats), function(sp_code) {
            stats <- coverage_stats[[sp_code]]
            div(
              style = "margin: 5px 0;",
              span(
                class = paste("coverage-badge", stats$coverage_class),
                tags$em(stats$species_name), ": ", 
                paste0(stats$genes_found, "/", stats$total_genes, " (", stats$coverage_pct, "%)")
              ),
              tags$small(
                style = "margin-left: 8px; color: #666;",
                paste0(stats$total_orthologs, " total ortholog", 
                       if(stats$total_orthologs != 1) "s" else "",
                       if(stats$paralog_count > 0) 
                         paste0(", ", stats$paralog_count, " paralog", 
                                if(stats$paralog_count != 1) "s" else "")
                       else "")
              )
            )
          })
        )
      })
      #generate paralog selection UI
      output$paralog_selection_ui <- renderUI({
        if (is.null(gene_mapping)) return(NULL)
        
        selection_ui <- tagList()
        
        for (gene_map in gene_mapping) {
          input_gene <- gene_map$original
          
          #create section for this input gene
          gene_ui <- div(
            class = "mb-3",
            h6(strong(input_gene), style = "color: var(--bs-primary);"),
            
            #create checkboxes for each species
            lapply(names(config), function(sp_code) {
              orthologs <- gene_map[[sp_code]]
              if (is.null(orthologs) || length(orthologs) == 0) return(NULL)
              
              sp_name <- config[[sp_code]]$short
              checkbox_id <- paste0("select_", input_gene, "_", sp_code)
              
              #create choices with gene IDs
              choices <- setNames(orthologs, 
                                  if(length(orthologs) > 1) {
                                    sapply(1:length(orthologs), function(i) {
                                      paste0(orthologs[i], " [", i, "/", length(orthologs), "]")
                                    })
                                  } else {
                                    orthologs
                                  })
              
              div(
                class = "ms-3 mb-2",
                strong(tags$em(sp_name), ":"),
                checkboxGroupInput(
                  checkbox_id,
                  label = NULL,
                  choices = choices,
                  selected = orthologs[1],  #default: select first paralog only
                  inline = FALSE
                )
              )
            })
          )
          
          selection_ui <- tagList(selection_ui, gene_ui)
        }
        
        return(selection_ui)
      })
      
      #initialize selected_orthologs with first paralog from each species
      ortholog_state$selected_orthologs <- list()
      for (gene_map in gene_mapping) {
        input_gene <- gene_map$original
        for (sp_code in names(config)) {
          orthologs <- gene_map[[sp_code]]
          if (!is.null(orthologs) && length(orthologs) > 0) {
            checkbox_id <- paste0("select_", input_gene, "_", sp_code)
            ortholog_state$selected_orthologs[[checkbox_id]] <- orthologs[1]
          }
        }
      }
      showNotification(
        paste("Mapped", length(gene_mapping), "genes across", 
              length(config), "species"),
        type = "message",
        duration = 3
      )
      
      waiter_hide()
      return(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Error mapping orthologs:", e$message), 
                       type = "error", duration = 5)
      ortholog_state$mapped <- FALSE
      waiter_hide()
      return(FALSE)
    })
  }
  #observer for explicit ortholog remapping button
  observeEvent(input$remap_orthologs, {
    req(input$enable_ortholog_analysis)
    
    #get current gene list
    gene_list <- if (!is.null(input$gene_list) && nchar(trimws(input$gene_list)) > 0) {
      genes <- strsplit(trimws(input$gene_list), "[\n\r]+")[[1]]
      genes[genes != ""]
    } else {
      NULL
    }
    
    #perform remapping
    perform_ortholog_mapping(gene_list)
  })
  
  #observer for "Select All" button
  observeEvent(input$select_all_paralogs, {
    req(ortholog_state$gene_mapping)
    
    config <- current_species_config()
    
    for (gene_map in ortholog_state$gene_mapping) {
      input_gene <- gene_map$original
      for (sp_code in names(config)) {
        orthologs <- gene_map[[sp_code]]
        if (!is.null(orthologs) && length(orthologs) > 0) {
          checkbox_id <- paste0("select_", input_gene, "_", sp_code)
          updateCheckboxGroupInput(session, checkbox_id, selected = orthologs)
        }
      }
    }
    
    showNotification("Selected all orthologs", type = "message", duration = 2)
  })
  
  #observer for "Select First Only" button
  observeEvent(input$select_first_paralogs, {
    req(ortholog_state$gene_mapping)
    
    config <- current_species_config()
    
    for (gene_map in ortholog_state$gene_mapping) {
      input_gene <- gene_map$original
      for (sp_code in names(config)) {
        orthologs <- gene_map[[sp_code]]
        if (!is.null(orthologs) && length(orthologs) > 0) {
          checkbox_id <- paste0("select_", input_gene, "_", sp_code)
          updateCheckboxGroupInput(session, checkbox_id, selected = orthologs[1])
        }
      }
    }
    
    showNotification("Selected first ortholog from each species", type = "message", duration = 2)
  })
  
  # Observer to show detailed ortholog mapping
  observeEvent(input$view_ortholog_details, {
    req(ortholog_state$gene_mapping)
    
    config <- current_species_config()
    mapping_table <- create_ortholog_mapping_table(ortholog_state$gene_mapping, config)
    
    showModal(modalDialog(
      title = div(icon("sitemap"), " Detailed Ortholog Mapping"),
      size = "xl",
      DTOutput("ortholog_mapping_modal_table"),
      footer = tagList(
        downloadButton("download_ortholog_mapping", "Download CSV", 
                       class = "btn btn-primary"),
        modalButton("Close")
      )
    ))
    
    output$ortholog_mapping_modal_table <- renderDT({
      datatable(
        mapping_table,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      )
    })
  })
  
  # Download handler for ortholog mapping
  output$download_ortholog_mapping <- downloadHandler(
    filename = function() {
      paste0("ortholog_mapping_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(ortholog_state$gene_mapping)
      config <- current_species_config()
      mapping_table <- create_ortholog_mapping_table(ortholog_state$gene_mapping, config)
      write.csv(mapping_table, file, row.names = FALSE)
    }
  )
  #download handler for coverage statistics
  output$download_coverage_stats <- downloadHandler(
    filename = function() {
      paste0("ortholog_coverage_stats_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(ortholog_state$coverage_stats)
      config <- current_species_config()
      stats_list <- ortholog_state$coverage_stats
      stats_df <- data.frame(
        Species = sapply(names(stats_list), function(sp) config[[sp]]$short),
        Species_Code = names(stats_list),
        Genes_Found = sapply(stats_list, function(x) x$genes_found),
        Total_Input_Genes = sapply(stats_list, function(x) x$total_genes),
        Coverage_Percent = sapply(stats_list, function(x) x$coverage_pct),
        Total_Orthologs = sapply(stats_list, function(x) x$total_orthologs),
        Paralog_Count = sapply(stats_list, function(x) x$paralog_count),
        Coverage_Class = sapply(stats_list, function(x) x$coverage_class),
        stringsAsFactors = FALSE
      )
      write.csv(stats_df, file, row.names = FALSE)
    }
  )
  #parse pathway definitions from text input
  parse_pathway_definitions <- reactive({
    if (input$enable_pathway_comparison) {
      #check for file upload first
      pathway_text <- if (!is.null(input$pathway_file$datapath)) {
        readLines(input$pathway_file$datapath, warn = FALSE)
      } else if (!is.null(input$pathway_definitions) && nchar(trimws(input$pathway_definitions)) > 0) {
        #split on single newline to preserve blank lines
        #normalize line endings first (handle \r\n, \r, \n)
        normalized <- gsub("\r\n", "\n", input$pathway_definitions)
        normalized <- gsub("\r", "\n", normalized)
        strsplit(normalized, "\n")[[1]]
      } else {
        return(NULL)
      }
      
      if (is.null(pathway_text) || length(pathway_text) == 0) {
        return(NULL)
      }
      
      #parse pathway format
      pathway_list <- list()
      current_pathway <- NULL
      current_genes <- c()
      
      for (line in pathway_text) {
        line <- trimws(line)
        
        if (line == "") {
          #blank line indicates pathway boundary
          if (!is.null(current_pathway) && length(current_genes) > 0) {
            pathway_list[[current_pathway]] <- current_genes
            current_pathway <- NULL
            current_genes <- c()
          }
        } else if (is.null(current_pathway)) {
          #this is a pathway name
          current_pathway <- line
        } else {
          #this line contains genes (split on comma, semicolon, or whitespace)
          genes <- strsplit(line, "[,;\\s]+")[[1]]
          genes <- trimws(genes)
          genes <- genes[genes != ""]
          current_genes <- c(current_genes, genes)
        }
      }
      
      #add last pathway if exists (handles case with no trailing blank line)
      if (!is.null(current_pathway) && length(current_genes) > 0) {
        pathway_list[[current_pathway]] <- current_genes
      }
      
      if (length(pathway_list) == 0) {
        return(NULL)
      }
      
      return(pathway_list)
    }
    return(NULL)
  })
  
  observeEvent(input$analyze_gene_groups, {
    req(!is.null(input$gene_list) || !is.null(input$gene_group_file))
    if (is.null(input$gene_list) && is.null(input$gene_group_file)) {
      showNotification("Please provide either a gene list or upload a file", type = "error")
      return()
    }
    
    waiter_show(html = loading_screen)
    
    tryCatch({
      #check if pathway comparison mode is enabled
      if (input$enable_pathway_comparison) {
        #pathway comparison mode
        pathway_defs <- parse_pathway_definitions()
        
        if (is.null(pathway_defs) || length(pathway_defs) == 0) {
          showNotification("Please define at least one pathway", type = "error")
          waiter_hide()
          return()
        }
        
        config <- current_species_config()
        pathway_results <- NULL
        
        #branch based on cross-species ortholog analysis checkbox
        if (input$enable_ortholog_analysis) {
          #multi-species mode via orthology
          current_data <- get_all_species_data()
          
          species_data_list <- list()
          for (sp_code in names(config)) {
            species_data_list[[sp_code]] <- get_species_data(sp_code)
          }
          
          pathway_results <- process_pathway_comparison(
            pathway_defs,
            species_data_list,
            config,
            current_data
          )
        } else {
          #single-species mode (no orthology)
          selected_species <- input$group_analysis_species
          if (is.null(selected_species) || selected_species == "") {
            selected_species <- names(config)[1]
          }
          
          species_data <- get_species_data(selected_species)
          species_name <- config[[selected_species]]$short
          
          pathway_results <- process_single_species_pathway(
            pathway_defs,
            species_data,
            species_name,
            input$global_transform
          )
        }
        
        if (is.null(pathway_results) || nrow(pathway_results) == 0) {
          showNotification("No valid expression data found for pathways", 
                           type = "error", duration = 5)
          waiter_hide()
          return()
        }
        
        #generate pathway heatmap
        output$gene_group_plot <- renderPlotly({
          create_pathway_heatmap(
            pathway_data = pathway_results,
            value_type = input$pathway_value_type,
            is_dark_mode = is_dark(),
            cluster_pathways = input$cluster_pathways,
            timepoint_mode = input$timepoint_display_mode
          )
        })
        
        #generate summary table with gene details
        output$gene_group_table <- renderDT({
          #get gene details from attribute
          gene_details <- attr(pathway_results, "gene_details")
          
          summary_table <- pathway_results %>%
            group_by(Pathway, Species) %>%
            summarise(
              NGenes = first(NGenes),
              Mean_Baseline = MeanExpression[Timepoint == "0min"],
              Mean_Peak = max(MeanExpression, na.rm = TRUE),
              Mean_Final = MeanExpression[Timepoint == "8h"],
              .groups = 'drop'
            )
          
          #join gene details if available
          if (!is.null(gene_details) && nrow(gene_details) > 0) {
            summary_table <- left_join(summary_table, 
                                       gene_details[, c("Pathway", "Species", "Genes"), drop = FALSE],
                                       by = c("Pathway", "Species"))
          } else {
            summary_table$Genes <- NA_character_
          }
          
          if (input$pathway_value_type == "foldchange") {
            fc_summary <- calculate_pathway_foldchange(pathway_results) %>%
              group_by(Pathway, Species) %>%
              summarise(
                Max_FoldChange = max(abs(Log2FC), na.rm = TRUE),
                .groups = 'drop'
              )
            summary_table <- left_join(summary_table, fc_summary, by = c("Pathway", "Species"))
          }
          
          #create pathway color mapping using pastel palette
          unique_pathways <- unique(summary_table$Pathway)
          n_pathways <- length(unique_pathways)
          pastel_colors <- if (n_pathways <= 8) {
            RColorBrewer::brewer.pal(max(3, n_pathways), "Pastel1")[1:n_pathways]
          } else if (n_pathways <= 12) {
            RColorBrewer::brewer.pal(n_pathways, "Set3")
          } else {
            colorRampPalette(RColorBrewer::brewer.pal(8, "Pastel1"))(n_pathways)
          }
          pathway_colors <- setNames(pastel_colors, unique_pathways)
          
          #add color column for styling (will be hidden)
          summary_table$RowColor <- pathway_colors[summary_table$Pathway]
          
          #reorder columns to put Genes before Max_FoldChange if present
          col_order <- c("Pathway", "Species", "NGenes", "Mean_Baseline", "Mean_Peak", "Mean_Final", "Genes")
          if ("Max_FoldChange" %in% colnames(summary_table)) {
            col_order <- c(col_order, "Max_FoldChange")
          }
          col_order <- c(col_order, "RowColor")
          summary_table <- summary_table[, col_order]
          
          datatable(
            summary_table,
            options = list(
              pageLength = 20,
              scrollX = TRUE,
              dom = 'tip',
              columnDefs = list(
                list(visible = FALSE, targets = ncol(summary_table) - 1),
                list(width = '180px', targets = which(colnames(summary_table) == "Genes") - 1)
              )
            ),
            rownames = FALSE,
            caption = "Pathway Expression Summary"
          ) %>%
            formatRound(columns = which(sapply(summary_table[, -ncol(summary_table)], is.numeric)), digits = 2) %>%
            formatStyle("Species", fontStyle = "italic") %>%
            formatStyle(
              'Pathway',
              'RowColor',
              backgroundColor = styleEqual(
                unique(summary_table$RowColor),
                unique(summary_table$RowColor)
              )
            ) %>%
            formatStyle(
              columns = 1:(ncol(summary_table) - 1),
              'RowColor',
              backgroundColor = styleEqual(
                unique(summary_table$RowColor),
                unique(summary_table$RowColor)
              )
            )
        })
        
        #render pathway legend
        output$pathway_table_legend <- renderUI({
          unique_pathways <- unique(pathway_results$Pathway)
          n_pathways <- length(unique_pathways)
          
          pastel_colors <- if (n_pathways <= 8) {
            RColorBrewer::brewer.pal(max(3, n_pathways), "Pastel1")[1:n_pathways]
          } else if (n_pathways <= 12) {
            RColorBrewer::brewer.pal(n_pathways, "Set3")
          } else {
            colorRampPalette(RColorBrewer::brewer.pal(8, "Pastel1"))(n_pathways)
          }
          
          #create legend items
          legend_items <- lapply(seq_along(unique_pathways), function(i) {
            tags$span(
              style = "display: inline-flex; align-items: center; margin-right: 16px; margin-bottom: 4px;",
              tags$span(
                style = paste0(
                  "display: inline-block; width: 16px; height: 16px; ",
                  "background-color: ", pastel_colors[i], "; ",
                  "border: 1px solid #ccc; border-radius: 3px; margin-right: 6px;"
                )
              ),
              tags$span(unique_pathways[i], style = "font-size: 0.9em;")
            )
          })
          
          div(
            class = "mt-3 mb-2 p-2",
            style = "background-color: var(--bs-body-bg); border-radius: 4px; border: 1px solid var(--bs-border-color);",
            tags$strong("Pathway Legend:", style = "margin-right: 12px;"),
            div(
              style = "display: flex; flex-wrap: wrap; align-items: center; margin-top: 6px;",
              legend_items
            )
          )
        })
        
        waiter_hide()
        return()
      }
      
      #clear pathway legend when not in pathway mode
      output$pathway_table_legend <- renderUI({ NULL })
      
      # Process input data (ORIGINAL SINGLE/MULTI-GENE MODE)
      gene_groups <- if (!is.null(input$gene_group_file$datapath)) {
        read.csv(input$gene_group_file$datapath)
      } else {
        # Make sure gene_list is not empty and process it
        gene_list <- trimws(input$gene_list)
        if (nchar(gene_list) > 0) {
          genes <- strsplit(gene_list, "[\n\r]+")[[1]]
          genes <- genes[genes != ""] # Remove empty lines
          data.frame(
            group_name = "Custom Group",
            group_member = genes
          )
        } else {
          showNotification("Please enter at least one gene", type = "error")
          return()
        }
      }
      
      # Get species data
      species_data <- get_species_data(input$group_analysis_species)
      
      #process gene expression data with HOG support; dynamic!
      if (input$enable_ortholog_analysis && ortholog_state$mapped) {
        # Multi-species mode using ortholog mapping
        config <- current_species_config()
        current_data <- get_all_species_data()
        
        # Get species data for all species
        species_data_list <- list()
        for (sp_code in names(config)) {
          species_data_list[[sp_code]] <- get_species_data(sp_code)
        }
        
        #process multi-species data
        plot_data <- process_multi_species_gene_set(
          ortholog_state$gene_mapping,
          species_data_list,
          config
        )
        
        if (is.null(plot_data) || nrow(plot_data) == 0) {
          showNotification("No valid expression data found for orthologs", 
                           type = "error", duration = 5)
          waiter_hide()
          return()
        }
        
        #filter plot data based on user selections
        selected_gene_ids <- c()
        for (gene_map in ortholog_state$gene_mapping) {
          input_gene <- gene_map$original
          for (sp_code in names(config)) {
            checkbox_id <- paste0("select_", input_gene, "_", sp_code)
            selected <- input[[checkbox_id]]
            if (!is.null(selected) && length(selected) > 0) {
              selected_gene_ids <- c(selected_gene_ids, selected)
            }
          }
        }
        
        #filter to only selected orthologs
        if (length(selected_gene_ids) > 0) {
          plot_data <- plot_data[plot_data$GeneID %in% selected_gene_ids, ]
        }
        
        if (nrow(plot_data) == 0) {
          showNotification("No orthologs selected for plotting", 
                           type = "warning", duration = 5)
          waiter_hide()
          return()
        }
        
        #store for later use
        ortholog_state$multi_species_data <- plot_data
        
      } else {
        # Single species mode (existing behavior)
        config <- current_species_config()  
        current_data <- get_all_species_data()
        plot_data <- process_gene_group_data(
          gene_groups, 
          species_data, 
          current_data,
          config,  
          input$group_analysis_species  
        )
      }
      
      # check if we got any valid data
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        showNotification("No valid gene expression data found", type = "error")
        return()
      }
      
      # Create a reactive value to store the current significance test parameters
      sig_test_params <- reactiveValues(
        gene = NULL,
        comparisons = NULL
      )
      
      # Event handler for applying significance test
      observeEvent(input$apply_sig_test, {
        req(input$sig_test_gene, input$sig_test_timepoints)
        
        sig_test_params$gene <- input$sig_test_gene
        sig_test_params$comparisons <- input$sig_test_timepoints
        
        #regenerate the plot
        output$gene_group_plot <- renderPlotly({
          create_group_visualization(
            plot_data = plot_data, 
            viz_type = input$group_viz_type, 
            is_dark_mode = is_dark(), 
            distance_method = input$distance_method,
            data_transform = input$data_transform,
            show_significance = input$show_significance,
            alpha = input$significance_threshold,
            selected_gene = sig_test_params$gene,
            selected_comparisons = sig_test_params$comparisons,
            plot_settings = reactiveValuesToList(plot_settings)
          )
        })
      })
      
      #initialize the plot without significance testing
      # Store flag for multi-species mode
      is_multi_species <- input$enable_ortholog_analysis && 
        ortholog_state$mapped && 
        "Species" %in% colnames(plot_data)
      
      #initialize the plot without significance testing
      output$gene_group_plot <- renderPlotly({
        # Check if we have multi-species data
        if (is_multi_species) {
          #check if aggregation to species mean is requested
          if (!is.null(input$aggregation_level) && input$aggregation_level == "species_mean") {
            plot_data <- aggregate_to_species_mean(plot_data)
          }
          
          # Use multi-species visualization
          if (input$group_viz_type == "line") {
            #create unique identifier for each gene including paralog info
            plot_data$GeneLabel <- ifelse(
              duplicated(paste(plot_data$Gene, plot_data$Species)) | 
                duplicated(paste(plot_data$Gene, plot_data$Species), fromLast = TRUE),
              paste0(plot_data$Gene, " (", plot_data$GeneID, ")"),
              plot_data$Gene
            )
            
            #get species colors from plot_settings
            settings_colors <- plot_settings$species_colors
            unique_species <- unique(plot_data$Species)
            species_color_vec <- sapply(unique_species, function(sp) {
              if (sp %in% names(settings_colors)) settings_colors[[sp]] else "#808080"
            })
            names(species_color_vec) <- unique_species
            
            #check if aggregation to species mean is requested
            if (!is.null(input$aggregation_level) && input$aggregation_level == "species_mean") {
              #calculate mean and SE for error bars
              plot_summary <- plot_data %>%
                group_by(Species, Timepoint) %>%
                summarise(
                  Mean = mean(Expression, na.rm = TRUE),
                  SE = sd(Expression, na.rm = TRUE) / sqrt(n()),
                  .groups = 'drop'
                )
              p <- plot_ly(plot_summary, x = ~Timepoint, y = ~Mean, color = ~Species,
                           colors = species_color_vec,
                           type = 'scatter', mode = 'lines+markers',
                           error_y = list(
                             type = "data",
                             array = ~SE,
                             visible = TRUE
                           )) %>%
                layout(
                  title = "Multi-Species Mean Gene Expression",
                  xaxis = list(title = "Timepoint"),
                  yaxis = list(title = "Mean log2 CPM"),
                  hovermode = "closest",
                  plot_bgcolor = if(is_dark()) "#2c3034" else "white",
                  paper_bgcolor = if(is_dark()) "#2c3034" else "white",
                  font = list(color = if(is_dark()) "white" else "black")
                )
              return(p)
            }
            
            #build color map for gene-species combinations using species colors
            unique_combos <- unique(paste(plot_data$GeneLabel, plot_data$Species, sep = " | "))
            combo_colors <- sapply(unique_combos, function(combo) {
              sp <- sub("^.* \\| ", "", combo)
              if (sp %in% names(settings_colors)) settings_colors[[sp]] else "#808080"
            })
            names(combo_colors) <- unique_combos
            
            #create multi-species line plot with paralog distinction
            plot_data$ComboLabel <- paste(plot_data$GeneLabel, plot_data$Species, sep = " | ")
            
            p <- ggplot(plot_data, 
                        aes(x = Timepoint, y = Expression,
                            color = ComboLabel,
                            group = interaction(GeneID, Species, Replicate),
                            text = paste("Gene:", GeneLabel,
                                         "<br>Gene ID:", GeneID,
                                         "<br>Species:", Species,
                                         "<br>Replicate:", Replicate,
                                         "<br>Time:", Timepoint,
                                         "<br>Expression:", round(Expression, 2)))) +
              geom_point(size = 3, alpha = 0.8) +
              geom_line(linewidth = 1.2, alpha = 0.9) +
              scale_color_manual(values = combo_colors) +
              labs(
                y = "log2 count per million",
                title = "Multi-Species Gene Set Expression",
                subtitle = paste("Comparing", length(unique(plot_data$GeneID)), 
                                 "total orthologs across", length(unique(plot_data$Species)), "species"),
                x = "Timepoint",
                color = "Gene | Species"
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                axis.text.y = element_text(size = 11),
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                panel.grid.major = element_line(color = if(is_dark()) "gray30" else "gray90"),
                panel.grid.minor = element_line(color = if(is_dark()) "gray20" else "gray95"),
                plot.background = element_rect(fill = if(is_dark()) "#2c3034" else "white", color = NA),
                panel.background = element_rect(fill = if(is_dark()) "#2c3034" else "white", color = NA),
                text = element_text(color = if(is_dark()) "white" else "black"),
                axis.text = element_text(color = if(is_dark()) "white" else "black"),
                legend.text = element_text(color = if(is_dark()) "white" else "black"),
                legend.title = element_text(color = if(is_dark()) "white" else "black"),
                legend.background = element_rect(fill = if(is_dark()) "#2c3034" else "white")
              )
            
            ggplotly(p, tooltip = "text") %>%
              layout(
                plot_bgcolor = if(is_dark()) "#2c3034" else "white",
                paper_bgcolor = if(is_dark()) "#2c3034" else "white",
                font = list(color = if(is_dark()) "white" else "black")
              )
          } else if (input$group_viz_type == "heatmap") {
            #get heatmap palette from settings
            hm_palette <- plot_settings$heatmap_palette
            
            #prepare data: average replicates first
            heatmap_data <- plot_data %>%
              group_by(Gene, Species, Timepoint) %>%
              summarise(AvgExpression = mean(Expression, na.rm = TRUE), .groups = 'drop') %>%
              mutate(GeneSpecies = paste(Gene, Species, sep = "_"))
            
            #create wide format matrix
            hm_matrix <- heatmap_data %>%
              select(GeneSpecies, Timepoint, AvgExpression) %>%
              pivot_wider(names_from = Timepoint, values_from = AvgExpression) %>%
              column_to_rownames("GeneSpecies") %>%
              as.matrix()
            
            #apply transformation
            if (input$data_transform == "zscore") {
              hm_matrix <- t(scale(t(hm_matrix)))
              hm_matrix[!is.finite(hm_matrix)] <- 0
            } else if (input$data_transform == "centered") {
              hm_matrix <- t(scale(t(hm_matrix), center = TRUE, scale = FALSE))
            }
            
            #create hover text before NaN conversion
            hover_vals <- ifelse(is.na(hm_matrix), "No data", round(hm_matrix, 2))
            hm_hover <- matrix(
              paste(
                "Gene-Species:", rep(rownames(hm_matrix), each = ncol(hm_matrix)),
                "<br>Time:", rep(colnames(hm_matrix), times = nrow(hm_matrix)),
                "<br>Value:", as.vector(t(hover_vals))
              ),
              nrow = nrow(hm_matrix),
              ncol = ncol(hm_matrix),
              byrow = TRUE
            )
            
            #convert NA to NaN to prevent plotly nacolor warning
            hm_matrix[is.na(hm_matrix)] <- NaN
            
            plot_ly(
              z = hm_matrix,
              x = colnames(hm_matrix),
              y = rownames(hm_matrix),
              type = "heatmap",
              colorscale = hm_palette,
              hoverinfo = "text",
              text = hm_hover
            ) %>%
              layout(
                title = "Multi-Species Gene Expression Heatmap",
                xaxis = list(
                  title = "Timepoint",
                  tickangle = -45
                ),
                yaxis = list(
                  title = "Gene - Species",
                  autorange = "reversed"
                ),
                plot_bgcolor = if(is_dark()) "#2c3034" else "white",
                paper_bgcolor = if(is_dark()) "#2c3034" else "white",
                font = list(color = if(is_dark()) "white" else "black")
              )
          } else {
            #for bar chart in multi-species mode - use species colors
            settings_colors <- plot_settings$species_colors
            
            summary_data <- plot_data %>%
              group_by(Timepoint, Species, Gene) %>%
              summarise(Mean = mean(Expression, na.rm = TRUE), .groups = 'drop') %>%
              mutate(GeneSpecies = paste(Gene, Species, sep = " | "))
            
            #build color map for gene-species combinations using species colors
            unique_combos <- unique(summary_data$GeneSpecies)
            combo_colors <- sapply(unique_combos, function(combo) {
              sp <- sub("^.* \\| ", "", combo)
              if (sp %in% names(settings_colors)) settings_colors[[sp]] else "#808080"
            })
            names(combo_colors) <- unique_combos
            
            p <- ggplot(summary_data, aes(x = Timepoint, y = Mean, fill = GeneSpecies)) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_fill_manual(values = combo_colors) +
              labs(
                title = "Multi-Species Gene Set Expression",
                y = "Mean log2 CPM",
                x = "Timepoint",
                fill = "Gene | Species"
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.background = element_rect(fill = if(is_dark()) "#2c3034" else "white", color = NA),
                panel.background = element_rect(fill = if(is_dark()) "#2c3034" else "white", color = NA),
                text = element_text(color = if(is_dark()) "white" else "black"),
                legend.background = element_rect(fill = if(is_dark()) "#2c3034" else "white")
              )
            
            ggplotly(p) %>%
              layout(
                plot_bgcolor = if(is_dark()) "#2c3034" else "white",
                paper_bgcolor = if(is_dark()) "#2c3034" else "white"
              )
          }
        } else {
          #single species mode - use existing visualization
          create_group_visualization(
            plot_data = plot_data, 
            viz_type = input$group_viz_type, 
            is_dark_mode = is_dark(), 
            distance_method = input$distance_method,
            data_transform = input$data_transform,
            show_significance = input$show_significance,
            alpha = input$significance_threshold,
            selected_gene = NULL,
            selected_comparisons = NULL,
            plot_settings = reactiveValuesToList(plot_settings)
          )
        }
      })
      # Generate summary table based on mode
      output$gene_group_table <- renderDT({
        if (is_multi_species) {
          # Multi-species summary table
          summary_data <- plot_data %>%
            group_by(Gene, Species) %>%
            summarise(
              Mean_Expression = mean(Expression, na.rm = TRUE),
              Max_Expression = max(Expression, na.rm = TRUE),
              Min_Expression = min(Expression, na.rm = TRUE),
              SD_Expression = sd(Expression, na.rm = TRUE),
              .groups = 'drop'
            )
          
          datatable(
            summary_data,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'tp'
            ),
            rownames = FALSE,
            caption = "Multi-Species Expression Summary"
          ) %>%
            formatStyle("Species", fontStyle = "italic") %>%
            formatRound(columns = c('Mean_Expression', 'Max_Expression', 'Min_Expression', 'SD_Expression'), 
                        digits = 2)
        } else {
          # Single species mode - use existing function
          config <- current_species_config()
          species_name <- config[[input$group_analysis_species]]$name
          create_group_summary_table(plot_data, species_name)
        }
      })
      
    }, error = function(e) {
      showNotification(
        paste("Error processing gene groups:", e$message),
        type = "error"
      )
    })
    
    waiter_hide()
  })

  # Download handler for gene group plot
  output$download_group_plot <- downloadHandler(
    filename = function() {
      if (input$enable_pathway_comparison) {
        paste("pathway_comparison_", input$pathway_value_type, "_", Sys.Date(), ".png", sep = "")
      } else {
        paste("gene_group_", input$group_viz_type, "_", Sys.Date(), ".png", sep = "")
      }
    },
    content = function(file) {
      p <- plotly::plotly_build(isolate(output$gene_group_plot()))
      plotly::export(p, file = file)
    }
  )
  
  #download handler for pathway data matrix
  output$download_pathway_data <- downloadHandler(
    filename = function() {
      paste("pathway_data_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      if (input$enable_pathway_comparison) {
        pathway_defs <- parse_pathway_definitions()
        config <- current_species_config()
        current_data <- get_all_species_data()
        
        species_data_list <- list()
        for (sp_code in names(config)) {
          species_data_list[[sp_code]] <- get_species_data(sp_code)
        }
        
        pathway_results <- process_pathway_comparison(
          pathway_defs,
          species_data_list,
          config,
          current_data
        )
        
        if (input$pathway_value_type == "foldchange") {
          pathway_results <- calculate_pathway_foldchange(pathway_results)
        }
        
        write.csv(pathway_results, file, row.names = FALSE)
      }
    }
  )

  #ridgeline plot handlers - stores data for reactive rendering
  observeEvent(input$generate_ridgeline, {
    waiter_show(html = loading_screen)
    
    species_data_list <- list()
    
    if (input$ridgeline_species == "all") {
      config <- current_species_config()
      species_list <- names(config)
    } else {
      species_list <- c(input$ridgeline_species)
    }
    
    for (species_id in species_list) {
      species_data_list[[species_id]] <- get_species_data(species_id)
    }
    
    plot_state$ridgeline_data <- list(
      species_data_list = species_data_list,
      view_type = input$ridgeline_view,
      threshold = input$expression_threshold
    )
    plot_state$ridgeline_ready <- TRUE
    
    waiter_hide()
  })
  
  #ridgeline plot render - reactive to plot_state and plot_settings
  output$ridgeline_plot <- renderPlot({
    if (!isTRUE(plot_state$ridgeline_ready) || is.null(plot_state$ridgeline_data)) {
      plot.new()
      text(0.5, 0.5, "Click Generate Plot to create ridgeline visualization", col = "gray50", cex = 1.2)
      return()
    }
    
    rd <- plot_state$ridgeline_data
    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)
    
    if (rd$view_type == "distribution") {
      create_ridgeline_plot(
        species_data_list = rd$species_data_list,
        is_dark_mode = dark_mode,
        plot_settings = current_settings
      )
    } else {
      create_threshold_ridgeline(
        species_data_list = rd$species_data_list,
        threshold = rd$threshold,
        is_dark_mode = dark_mode,
        plot_settings = current_settings
      )
    }
  })
  
  #global search observer 
  observeEvent(input$global_search_button, {
    req(input$global_gene_query)
    waiter_show(html = loading_screen)
    
    query <- trimws(input$global_gene_query)
    
    if (query == "") {
      showNotification("Please enter a gene name or ID", type = "warning")
      waiter_hide()
      return()
    }
    
    # Store in global state
    global_query_state$current_query <- query
    global_query_state$last_search_time <- Sys.time()
    
    # Search across all species
    query_result <- NULL
    found_species <- NULL
    config <- current_species_config()
    current_data <- get_all_species_data()
    
    for (species_id in names(config)) {
      species_data <- get_species_data(species_id)
      result <- query_gene_flexible(query, species_data, current_data)
      
      if (!is.null(result) && result$source != "none") {
        query_result <- result
        found_species <- species_id
        break
      }
    }
    
    if (!is.null(query_result)) {
      # Store the result globally
      global_query_state$query_result <- query_result
      
      # Show the results containers
      shinyjs::show("gene_explorer_results")
      shinyjs::show("query_status_container")
      
      # Update query status
      output$query_status <- renderUI({
        div(
          class = "alert alert-success",
          icon("check-circle"),
          strong("Query successful!"),
          br(),
          paste("Found", query, "in orthogroup", query_result$orthogroup),
          br(),
          tags$small(
            "Species with orthologs: ",
            paste(sapply(names(query_result$genes_by_species), function(sp) {
              config[[sp]]$short  # Use local config variable instead of SPECIES_CONFIG
            }), collapse = ", ")
          )
        )
      })
      
      if (!is.null(query_result$og_id)) {
        tree <- load_gene_tree(query_result$og_id, current_data)  # Pass current_data
        global_query_state$tree_data <- tree
        
        # Dynamic UI for the tree plot
      output$phylo_tree_plot_ui <- renderUI({
        if (!is.null(tree)) {
          n_tips <- length(tree$tip.label)
          # Calculate height based on number of tips (minimum 400px, 30px per tip)
          plot_height <- max(400, min(900, n_tips * 30))
          plotOutput("phylo_tree_plot", height = paste0(plot_height, "px"))
        } else {
          plotOutput("phylo_tree_plot", height = "400px")
        }
      })
      
      output$phylo_tree_plot <- renderPlot({
        if (!is.null(tree)) {
          create_phylo_tree_plot(tree, query_result$genes_by_species, query_result$orthogroup, is_dark(), current_data, species_colors_dynamic(), config)
        } else {
          plot.new()
          text(0.5, 0.5, "Phylogenetic tree not available for this orthogroup", 
               cex = 1.2, col = if(is_dark()) "white" else "black")
        }
      }, bg = if(is_dark()) "#2c3034" else "white", res = 120)
      }
      
      #update orthogroup summary  
      output$orthogroup_summary <- renderUI({
        create_orthogroup_summary(query_result, config)
      })
      
      #update orthogroup table with enhanced formatting
      output$explorer_orthogroup_table <- renderDT({
        create_orthogroup_details_table(query_result, config)
      })
      
    } else {
      # Not found
      shinyjs::hide("gene_explorer_results")
      shinyjs::show("query_status_container")
      
      output$query_status <- renderUI({
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          strong("Gene not found!"),
          br(),
          paste("Could not find", query, "in any species."),
          br(),
          tags$small("Try using a different gene name or ID format.")
        )
      })
    }
    
    waiter_hide()
  })
  
  #quick action: view in Single Species View
  observeEvent(input$explore_species_view, {
    req(global_query_state$query_result)
    
    # Find which species has the gene
    result <- global_query_state$query_result
    config <- current_species_config()
    
    if (!is.null(result$genes_by_species)) {
      species_list <- names(result$genes_by_species)
      
     if(length(species_list) == 1) {
        # Only one species - navigate directly
        first_species <- species_list[1]
        config <- current_species_config()  # Get current config
        species_name <- config[[first_species]]$name
        
        #navigate to Single Species View and then to the specific species tab
        updateTabsetPanel(session, "nav", selected = "species_analysis_container")
        
        #small delay to ensure the main tab is loaded
        shinyjs::delay(100, {
          updateTabsetPanel(session, "species_tabs", selected = first_species)
          
          # Scroll to top of page
          shinyjs::runjs("window.scrollTo(0, 0);")
          
          # Pre-fill and trigger search
          updateTextInput(session, paste0(first_species, "_genename"), 
                          value = global_query_state$current_query)
          shinyjs::delay(100, {
            shinyjs::click(paste0(first_species, "_search_button"))
          })
        })
        
        #JavaScript to directly activate the tab panel WITHOUT opening the dropdown
        #navigate to Single Species View and then to the specific species tab
        updateTabsetPanel(session, "nav", selected = "species_analysis_container")
        
        # Small delay to ensure the main tab is loaded
        shinyjs::delay(100, {
          updateTabsetPanel(session, "species_tabs", selected = selected_species)
          
          # Scroll to top of page
          shinyjs::runjs("window.scrollTo(0, 0);")
          
          # Pre-fill and trigger search
          updateTextInput(session, paste0(selected_species, "_genename"), 
                          value = global_query_state$current_query)
          shinyjs::delay(100, {
            shinyjs::click(paste0(selected_species, "_search_button"))
          })
        })
        
        updateTextInput(session, paste0(first_species, "_genename"), 
                        value = global_query_state$current_query)
        shinyjs::delay(100, {
          shinyjs::click(paste0(first_species, "_search_button"))
        })
      } else {
        #multiple species; selection modal 
        config <- current_species_config()  
        species_choices <- setNames(
          species_list,
          sapply(species_list, function(sp) {
            sp_config <- config[[sp]]
            genes_count <- nrow(result$genes_by_species[[sp]])
            paste0(sp_config$name, " (", genes_count, " gene", 
                   if(genes_count > 1) "s" else "", ")")
          })
        )
        
        showModal(modalDialog(
          title = "Select Species for Analysis",
          tags$p("This gene was found in multiple species. Select which species to analyze:"),
          radioButtons(
            "species_selection_modal",
            label = NULL,
            choices = species_choices,
            selected = species_list[1]
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_species_selection", "Go to Species", 
                         class = "btn btn-primary")
          ),
          size = "m",
          easyClose = TRUE
        ))
      }
    }
  })
  
  #handler for species selection confirmation
  observeEvent(input$confirm_species_selection, {
    req(input$species_selection_modal)
    
    selected_species <- input$species_selection_modal
    config <- current_species_config()  
    species_name <- config[[selected_species]]$name
    
    # Close the modal
    removeModal()
    
    #navigate to Single Species View and then to the specific species tab
    updateTabsetPanel(session, "nav", selected = "species_analysis_container")
    
    # Small delay to ensure the main tab is loaded
    shinyjs::delay(100, {
      updateTabsetPanel(session, "species_tabs", selected = selected_species)
      
      # Pre-fill and trigger search
      updateTextInput(session, paste0(selected_species, "_genename"), 
                      value = global_query_state$current_query)
      shinyjs::delay(100, {
        shinyjs::click(paste0(selected_species, "_search_button"))
      })
    })
    
    # Pre-fill and trigger search
    updateTextInput(session, paste0(selected_species, "_genename"), 
                    value = global_query_state$current_query)
    shinyjs::delay(100, {
      shinyjs::click(paste0(selected_species, "_search_button"))
    })
  })
  
  #quick action: view in Comparative Analysis
  observeEvent(input$explore_combined_view, {
    req(global_query_state$query_result)
    
    #navigate to comparative view
    updateTabsetPanel(session, "nav", selected = "Comparative View")
    
    # Pre-fill the search box
    updateTextInput(session, "combined_genename", 
                    value = global_query_state$current_query)
    
    # Trigger the search
    shinyjs::delay(100, {
      shinyjs::click("combined_search_button")
    })
  })
  
  # Quick action: Generate Cross-Species Heatmap
  observeEvent(input$explore_heatmap, {
    req(global_query_state$query_result)
    
    # Navigate to heatmap tab
    updateTabsetPanel(session, "nav", selected = "Cross-Species Heatmap")
    
    # Get all genes from the orthogroup
    all_genes <- c()
    for (sp in names(global_query_state$query_result$genes_by_species)) {
      genes_df <- global_query_state$query_result$genes_by_species[[sp]]
      if (nrow(genes_df) > 0) {
        # Add the first gene from each species
        all_genes <- c(all_genes, genes_df$gene_id[1])
      }
    }
    
    # Pre-fill the gene list
    updateTextAreaInput(session, "ortholog_gene_list", 
                        value = paste(all_genes, collapse = "\n"))
    
    # Trigger heatmap generation
    shinyjs::delay(100, {
      shinyjs::click("generate_ortholog_heatmap")
    })
  })
  
  # Add download handler for ridgeline plots
  output$download_ridgeline <- downloadHandler(
    filename = function() {
      paste("ridgeline_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      species_data_list <- list()
      
      if (input$ridgeline_species == "all") {
        config <- current_species_config()
        species_list <- names(config)
      } else {
        species_list <- c(input$ridgeline_species)
      }
      
      for (species_id in species_list) {
        species_data_list[[species_id]] <- get_species_data(species_id)
      }
      
      p <- if (input$ridgeline_view == "distribution") {
        create_ridgeline_plot(
          species_data_list = species_data_list,
          is_dark_mode = is_dark(),
          plot_settings = reactiveValuesToList(plot_settings)
        )
      } else {
        create_threshold_ridgeline(
          species_data_list = species_data_list,
          threshold = input$expression_threshold,
          is_dark_mode = is_dark(),
          plot_settings = reactiveValuesToList(plot_settings)
        )
      }
      
      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  # File reading helper
  read_data_file <- function(file_path, file_name = "") {
    if (is.null(file_path)) return(NULL)
    
    ext <- tools::file_ext(file_path)
    
    tryCatch({
      if (ext %in% c("tsv", "txt")) {
        data <- read.table(file_path, header = TRUE, sep = "\t", 
                           stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "csv") {
        data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        stop("Unsupported file format")
      }
      return(data)
    }, error = function(e) {
      showNotification(paste("Error reading", file_name, ":", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  }
  
  # Validation function for expression matrix
  validate_expression_matrix <- function(expr_data, species_name) {
    errors <- c()
    warnings <- c()
    
    if (is.null(expr_data)) {
      errors <- c(errors, paste(species_name, ": No expression data provided"))
      return(list(valid = FALSE, errors = errors, warnings = warnings))
    }
    
    # Check if numeric
    if (!all(sapply(expr_data, is.numeric))) {
      errors <- c(errors, paste(species_name, ": Expression matrix must contain only numeric values"))
    }
    
    # Check for row names (gene IDs)
    if (is.null(rownames(expr_data)) || all(rownames(expr_data) == as.character(1:nrow(expr_data)))) {
      warnings <- c(warnings, paste(species_name, ": No gene IDs found in row names, using first column"))
    }
    
    # Check for reasonable values (log2 CPM typically between -5 and 20)
    if (any(expr_data < -10 | expr_data > 30, na.rm = TRUE)) {
      warnings <- c(warnings, paste(species_name, ": Some expression values outside typical log2 CPM range"))
    }
    
    return(list(valid = length(errors) == 0, errors = errors, warnings = warnings))
  }
  
  validate_sample_info <- function(sample_data, species_name, expr_data = NULL) {
    errors <- c()
    warnings <- c()
    
    # Auto-generate sample_info from column names if not provided
    if (is.null(sample_data) && !is.null(expr_data)) {
      # Try to parse column names like T0_R1, T15_R2, etc.
      col_names <- colnames(expr_data)
      if (all(grepl("^T\\d+_R\\d+$", col_names))) {
        # Parse timepoint and replicate from column names
        timepoint_minutes <- as.integer(gsub("^T(\\d+)_R\\d+$", "\\1", col_names))
        replicates <- as.integer(gsub("^T\\d+_R(\\d+)$", "\\1", col_names))
        
        # Convert minutes to appropriate format
        timepoints <- sapply(timepoint_minutes, function(mins) {
          if (mins < 60) {
            return(paste0(mins, "min"))
          } else {
            hours <- mins / 60
            if (hours == floor(hours)) {
              return(paste0(as.integer(hours), "h"))
            } else {
              return(paste0(hours, "h"))
            }
          }
        })
        
        sample_data <- data.frame(
          Sample = col_names,
          Timepoint = timepoints,
          Replicate = replicates,
          stringsAsFactors = FALSE
        )
        
        warnings <- c(warnings, paste(species_name, ": Auto-generated sample info from column names"))
        return(list(valid = TRUE, errors = errors, warnings = warnings, data = sample_data))
      }
    }
    
    if (is.null(sample_data)) {
      errors <- c(errors, paste(species_name, ": No sample info provided"))
      return(list(valid = FALSE, errors = errors, warnings = warnings))
    }
    
    # Check required columns
    required_cols <- c("Sample", "Timepoint", "Replicate")
    missing_cols <- setdiff(required_cols, colnames(sample_data))
    
    if (length(missing_cols) > 0) {
      errors <- c(errors, paste(species_name, ": Missing required columns:", 
                                paste(missing_cols, collapse = ", ")))
    }
    
    # Check sample names match expression matrix columns if provided
    if (!is.null(expr_data) && "Sample" %in% colnames(sample_data)) {
      expr_samples <- colnames(expr_data)
      info_samples <- sample_data$Sample
      
      if (!all(info_samples %in% expr_samples)) {
        errors <- c(errors, paste(species_name, 
                                  ": Sample names in metadata don't match expression matrix columns"))
      }
    }
    
    return(list(valid = length(errors) == 0, errors = errors, warnings = warnings))
  }
  
  # Validation function for annotations
  validate_annotations <- function(anno_data, species_name) {
    errors <- c()
    warnings <- c()
    
    if (is.null(anno_data)) {
      warnings <- c(warnings, paste(species_name, ": No annotation data provided, will use gene IDs only"))
      return(list(valid = TRUE, errors = errors, warnings = warnings))
    }
    
    # Check required columns
    required_cols <- c("GeneID", "GeneName", "Chr")
    missing_cols <- setdiff(required_cols, colnames(anno_data))
    
    if (length(missing_cols) > 0) {
      errors <- c(errors, paste(species_name, ": Missing required annotation columns:", 
                                paste(missing_cols, collapse = ", ")))
    }
    
    return(list(valid = length(errors) == 0, errors = errors, warnings = warnings))
  }
  
  build_gene_lookup <- function(species_data_list, ortho_data = NULL) {
    lookup_entries <- list()
    
    for (species_id in names(species_data_list)) {
      # Skip non-species entries
      if (species_id %in% c("orthofinder", "metadata", "gene_lookup", "phylo_trees")) {
        next
      }
      
      sp_data <- species_data_list[[species_id]]
      
      if (!is.null(sp_data$anno)) {
        gene_ids <- sp_data$anno$GeneID
        gene_names <- sp_data$anno$GeneName
      } else if (!is.null(sp_data$lcpm)) {
        gene_ids <- rownames(sp_data$lcpm)
        gene_names <- rep("", length(gene_ids))
      } else {
        next
      }
      
      for (i in seq_along(gene_ids)) {
        # Create single entry per gene with both ID and name
        entry <- data.frame(
          gene_id = gene_ids[i],
          species = species_id,
          expression_id = gene_ids[i],
          id_type = paste0(toupper(species_id), "GL0"),
          source_info = "original",
          gene_name = if(length(gene_names) >= i && !is.na(gene_names[i])) gene_names[i] else "",
          hog_id = "",
          og_id = "",
          stringsAsFactors = FALSE
        )
        lookup_entries[[length(lookup_entries) + 1]] <- entry
      }
    }
    
    lookup_table <- as.data.frame(rbindlist(lookup_entries, fill = TRUE))
    
    # Add orthology information if provided
    if (!is.null(ortho_data) && "gene_id" %in% colnames(ortho_data)) {
      for (i in 1:nrow(lookup_table)) {
        gene_match <- which(ortho_data$gene_id == lookup_table$gene_id[i])
        if (length(gene_match) > 0) {
          if ("hog_id" %in% colnames(ortho_data)) {
            lookup_table$hog_id[i] <- ortho_data$hog_id[gene_match[1]]
          }
          if ("og_id" %in% colnames(ortho_data)) {
            lookup_table$og_id[i] <- ortho_data$og_id[gene_match[1]]
          }
        }
      }
    }
    
    return(as.data.table(lookup_table))
  }
  
  # Process OrthoFinder output
  process_orthofinder_output <- function(orthogroups_file, hog_file = NULL) {
    ortho_data <- read_data_file(orthogroups_file, "Orthogroups")
    
    if (is.null(ortho_data)) return(NULL)
    
    # Parse OrthoFinder format (wide format with OG in first column)
    og_list <- list()
    
    for (i in 1:nrow(ortho_data)) {
      og_id <- ortho_data[i, 1]
      
      for (j in 2:ncol(ortho_data)) {
        genes <- ortho_data[i, j]
        if (!is.na(genes) && genes != "") {
          # Split multiple genes
          gene_list <- unlist(strsplit(genes, "[, ]+"))
          species <- colnames(ortho_data)[j]
          
          for (gene in gene_list) {
            og_list[[length(og_list) + 1]] <- data.frame(
              gene_id = gene,
              og_id = og_id,
              hog_id = og_id,  # Default HOG to OG if not provided
              species = species,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    orthogroups_df <- do.call(rbind, og_list)
    
    # Process HOG file if provided
    if (!is.null(hog_file)) {
      hog_data <- read_data_file(hog_file, "HOG")
      # Process HOG data and update orthogroups_df
      # Implementation depends on HOG file format
    }
    
    return(orthogroups_df)
  }
  
  # Validate uploads observer
  observeEvent(input$validate_uploads, {
    upload_state$validation_errors <- list()
    upload_state$validation_warnings <- list()
    upload_state$uploaded_data <- list()
    
    waiter_show(html = loading_screen)
    
    # Process each user-defined species
    species <- defined_species()
    
    if (length(species) == 0) {
      showNotification("Please define at least one species first", type = "error")
      waiter_hide()
      return()
    }
    
    for (species_id in names(species)) {
      species_info <- species[[species_id]]
      species_name <- species_info$name
      
      # Read files
      expr_file <- input[[paste0("upload_", species_id, "_expr")]]
      sample_file <- input[[paste0("upload_", species_id, "_samples")]]
      anno_file <- input[[paste0("upload_", species_id, "_anno")]]
      
      expr_data <- if (!is.null(expr_file)) read_data_file(expr_file$datapath, paste(species_name, "expression"))
      
      #Process gene IDs before validation
      if (!is.null(expr_data)) {
        # Check if first column is unnamed (empty string) - this happens with row.names CSV export
        if (names(expr_data)[1] == "") {
          # First column is unnamed and contains gene IDs
          rownames(expr_data) <- as.character(expr_data[,1])  # Convert to character first
          expr_data <- expr_data[,-1, drop=FALSE]  # Remove the first column, keep as data frame
        } else if ("GeneID" %in% colnames(expr_data)) {
          rownames(expr_data) <- as.character(expr_data$GeneID)
          expr_data$GeneID <- NULL
        } else if ("gene_id" %in% colnames(expr_data)) {
          rownames(expr_data) <- as.character(expr_data$gene_id)
          expr_data$gene_id <- NULL
        } else if ("Gene" %in% colnames(expr_data)) {
          rownames(expr_data) <- as.character(expr_data$Gene)
          expr_data$Gene <- NULL
        }
      }
      #debug output
      if (!is.null(expr_data) && DEBUG_MODE) {
        debug_print(paste("Species:", species_name))
        debug_print(paste("Columns:", paste(names(expr_data), collapse=", ")))
        debug_print(paste("Column types:", paste(sapply(expr_data, class), collapse=", ")))
        debug_print(paste("First rownames:", paste(head(rownames(expr_data), 3), collapse=", ")))
        debug_print(paste("Dimensions:", nrow(expr_data), "x", ncol(expr_data)))
        debug_print(paste("All numeric?", all(sapply(expr_data, is.numeric))))
      }
      
      # NEW: Process gene IDs before validation
      if (!is.null(expr_data)) {
        # Check for GeneID column and handle it
        if ("GeneID" %in% colnames(expr_data)) {
          rownames(expr_data) <- expr_data$GeneID
          expr_data$GeneID <- NULL
        } else if ("gene_id" %in% colnames(expr_data)) {
          rownames(expr_data) <- expr_data$gene_id
          expr_data$gene_id <- NULL
        } else if ("Gene" %in% colnames(expr_data)) {
          rownames(expr_data) <- expr_data$Gene
          expr_data$Gene <- NULL
        }
      }
      
      sample_data <- if (!is.null(sample_file)) {
        uploaded_sample <- read_data_file(sample_file$datapath, paste(species_name, "samples"))
        
        # If uploaded sample metadata exists, normalize timepoint format if needed
        if (!is.null(uploaded_sample) && "Timepoint" %in% colnames(uploaded_sample)) {
          # Handle various timepoint formats (T60 -> 1h, 60 -> 1h, etc.)
          uploaded_sample$Timepoint <- sapply(uploaded_sample$Timepoint, function(tp) {
            tp_str <- as.character(tp)
            
            # If it's already in correct format (0min, 1h, etc.), keep it
            if (grepl("^\\d+min$|^\\d+(\\.\\d+)?h$", tp_str)) {
              return(tp_str)
            }
            
            # Extract numeric value from T60, 60, 60min, etc.
            numeric_val <- as.numeric(gsub("^T?(\\d+).*", "\\1", tp_str))
            
            if (!is.na(numeric_val)) {
              if (numeric_val < 60) {
                return(paste0(numeric_val, "min"))
              } else {
                hours <- numeric_val / 60
                if (hours == floor(hours)) {
                  return(paste0(as.integer(hours), "h"))
                } else {
                  return(paste0(hours, "h"))
                }
              }
            }
            
            return(tp_str)  # Return as-is if can't parse
          })
        }
        
        uploaded_sample
      } else {
        NULL
      }
      anno_data <- if (!is.null(anno_file)) read_data_file(anno_file$datapath, paste(species_name, "annotations"))
      
      # Validate each component
      expr_valid <- validate_expression_matrix(expr_data, species_name)
      sample_valid <- validate_sample_info(sample_data, species_name, expr_data)
      anno_valid <- validate_annotations(anno_data, species_name)
      
      # Use auto-generated sample data if provided
      if (!is.null(sample_valid$data)) {
        sample_data <- sample_valid$data
      }
      
      # Collect errors and warnings
      upload_state$validation_errors <- c(upload_state$validation_errors, 
                                          expr_valid$errors, sample_valid$errors, anno_valid$errors)
      upload_state$validation_warnings <- c(upload_state$validation_warnings,
                                            expr_valid$warnings, sample_valid$warnings, anno_valid$warnings)
      
      #properly using auto-generated sample_info
      if (expr_valid$valid && sample_valid$valid) {
        # Use auto-generated sample data if validation provided it
        if (!is.null(sample_valid$data)) {
          sample_data <- sample_valid$data
        }
        
        # Ensure expression matrix is properly formatted
        if (!is.null(expr_data)) {
          expr_matrix <- as.matrix(expr_data)
          
          # Handle row names
          if (is.null(rownames(expr_matrix)) || all(rownames(expr_matrix) == as.character(1:nrow(expr_matrix)))) {
            if ("GeneID" %in% colnames(expr_data)) {
              rownames(expr_matrix) <- expr_data$GeneID
              expr_matrix <- expr_matrix[, colnames(expr_matrix) != "GeneID"]
            }
          }
          
          # Store with CONSISTENT naming (no prefixes in main structure)
          upload_state$uploaded_data[[species_id]] <- list(
            lcpm = expr_matrix,
            sample_info = sample_data,  # Now uses the auto-generated data with correct timepoints
            anno = anno_data
          )
          
          # Always add prefixed versions for compatibility (for ALL species)
          upload_state$uploaded_data[[species_id]][[paste0(species_id, "_lcpm")]] <- expr_matrix
          upload_state$uploaded_data[[species_id]][[paste0(species_id, "_sample_info")]] <- sample_data
          upload_state$uploaded_data[[species_id]][[paste0(species_id, "_anno")]] <- anno_data
        }
      }
    }
    
    # Process orthology data
    if (input$orthology_source == "orthofinder" && !is.null(input$upload_orthogroups)) {
      ortho_data <- process_orthofinder_output(
        input$upload_orthogroups$datapath,
        if (!is.null(input$upload_hog)) input$upload_hog$datapath else NULL
      )
      upload_state$uploaded_data$orthofinder <- list(orthogroups = ortho_data)
    } else if (input$orthology_source == "custom" && !is.null(input$upload_custom_ortho)) {
      ortho_data <- read_data_file(input$upload_custom_ortho$datapath, "Custom orthology")
      upload_state$uploaded_data$orthofinder <- list(orthogroups = ortho_data)
    }
    
    # Update validation status
    upload_state$validated <- length(upload_state$validation_errors) == 0
    
    # Enable process button if validated
    if (upload_state$validated) {
      shinyjs::enable("process_uploads")
      showNotification("Validation successful! You can now process the data.", 
                       type = "message", duration = 5)
    } else {
      shinyjs::disable("process_uploads")
      showNotification("Validation failed. Please fix the errors and try again.", 
                       type = "error", duration = 5)
    }
    
    waiter_hide()
  })
  
  # Process uploads observer
  observeEvent(input$process_uploads, {
    req(upload_state$validated)
    
    waiter_show(html = loading_screen)
    
    tryCatch({
      # Build gene lookup table
      gene_lookup <- build_gene_lookup(
        upload_state$uploaded_data,
        if (!is.null(upload_state$uploaded_data$orthofinder)) {
          upload_state$uploaded_data$orthofinder$orthogroups
        } else NULL
      )
      
      # Create custom all_species_data structure
      custom_data <- upload_state$uploaded_data
      custom_data$gene_lookup <- gene_lookup
      
      # Add metadata
      custom_data$metadata <- list(
        source_file = "User uploaded",
        date_parsed = Sys.Date(),
        hog_level = "N0",
        n_hogs = length(unique(gene_lookup$hog_id[gene_lookup$hog_id != ""])),
        n_ogs = length(unique(gene_lookup$og_id[gene_lookup$og_id != ""])),
        n_genes = nrow(gene_lookup),
        species_included = paste(input$upload_species_select, collapse = ", "),
        note = "Custom user data upload"
      )
      
      # Store in upload state
      upload_state$custom_all_species_data <- custom_data
      upload_state$processed <- TRUE
      
      # Switch data source to custom
      data_source("custom")
      
      # Clear species data cache to force reload
      rm(list = ls(species_data_cache), envir = species_data_cache)
      
      showNotification("Data processed successfully! The app is now using your uploaded data.", 
                       type = "success", duration = 5)
      
      # Show success banner
      shinyjs::show("upload_status_banner")
      output$upload_status_content <- renderUI({
        div(
          icon("check-circle"),
          strong("Custom data loaded successfully!"),
          br(),
          paste("Active species:", paste(input$upload_species_select, collapse = ", ")),
          actionButton("dismiss_upload_banner", "Dismiss", 
                       class = "btn btn-sm btn-light float-right")
        )
      })
      
      # Update class for success
      shinyjs::removeClass("upload_status_banner", "alert-warning")
      shinyjs::addClass("upload_status_banner", "alert-success")
      
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), 
                       type = "error", duration = NULL)
      upload_state$processed <- FALSE
    })
    
    waiter_hide()
  })
  
  # Reset to default data
  observeEvent(input$reset_to_default, {
    data_source("default")
    upload_state$custom_all_species_data <- NULL
    upload_state$processed <- FALSE
    upload_state$validated <- FALSE
    
    # Clear species data cache
    rm(list = ls(species_data_cache), envir = species_data_cache)
    
    shinyjs::hide("upload_status_banner")
    shinyjs::disable("process_uploads")
    
    showNotification("Reset to demo data successful!", type = "success", duration = 3)
  })
  
  # Dismiss banner
  observeEvent(input$dismiss_upload_banner, {
    shinyjs::hide("upload_status_banner")
  })
  
  # Validation results output
  output$validation_results <- renderUI({
    if (!upload_state$validated && length(upload_state$validation_errors) == 0) {
      return(div(
        class = "text-muted",
        icon("info-circle"),
        " Upload your data files and click 'Validate Data' to begin"
      ))
    }
    
    ui_elements <- list()
    
    # Show errors
    if (length(upload_state$validation_errors) > 0) {
      ui_elements[[length(ui_elements) + 1]] <- div(
        class = "alert alert-danger",
        h5(icon("times-circle"), " Validation Errors"),
        tags$ul(
          lapply(upload_state$validation_errors, function(err) tags$li(err))
        )
      )
    }
    
    # Show warnings
    if (length(upload_state$validation_warnings) > 0) {
      ui_elements[[length(ui_elements) + 1]] <- div(
        class = "alert alert-warning",
        h5(icon("exclamation-triangle"), " Warnings"),
        tags$ul(
          lapply(upload_state$validation_warnings, function(warn) tags$li(warn))
        )
      )
    }
    
    # Show success
    if (upload_state$validated) {
      ui_elements[[length(ui_elements) + 1]] <- div(
        class = "alert alert-success",
        h5(icon("check-circle"), " Validation Successful"),
        p("All required data components are valid. You can now process the data.")
      )
    }
    
    do.call(tagList, ui_elements)
  })
  
  # Preview outputs
  output$upload_expr_preview <- renderDT({
    if (length(upload_state$uploaded_data) > 0) {
      # Show first species with data
      for (sp_id in names(upload_state$uploaded_data)) {
        if (!is.null(upload_state$uploaded_data[[sp_id]]$lcpm)) {
          expr_data <- upload_state$uploaded_data[[sp_id]]$lcpm
          preview <- expr_data[1:min(10, nrow(expr_data)), 1:min(5, ncol(expr_data))]
          return(datatable(preview, options = list(dom = 't', pageLength = 10)))
        }
      }
    }
  })
  
  output$upload_sample_preview <- renderDT({
    if (length(upload_state$uploaded_data) > 0) {
      for (sp_id in names(upload_state$uploaded_data)) {
        if (!is.null(upload_state$uploaded_data[[sp_id]]$sample_info)) {
          return(datatable(upload_state$uploaded_data[[sp_id]]$sample_info, 
                           options = list(dom = 't', pageLength = 10)))
        }
      }
    }
  })
  
  output$upload_anno_preview <- renderDT({
    if (length(upload_state$uploaded_data) > 0) {
      for (sp_id in names(upload_state$uploaded_data)) {
        if (!is.null(upload_state$uploaded_data[[sp_id]]$anno)) {
          preview <- upload_state$uploaded_data[[sp_id]]$anno[1:min(10, nrow(upload_state$uploaded_data[[sp_id]]$anno)), ]
          return(datatable(preview, options = list(dom = 't', pageLength = 10)))
        }
      }
    }
  })
  
  output$upload_ortho_preview <- renderDT({
    if (!is.null(upload_state$uploaded_data$orthofinder)) {
      ortho_data <- upload_state$uploaded_data$orthofinder$orthogroups
      if (!is.null(ortho_data)) {
        preview <- ortho_data[1:min(20, nrow(ortho_data)), ]
        return(datatable(preview, options = list(dom = 't', pageLength = 20)))
      }
    }
  })
  
  # Make processed status available to UI
  output$data_processed <- reactive({
    upload_state$processed
  })
  outputOptions(output, "data_processed", suspendWhenHidden = FALSE)
  
  # Download handlers
  output$download_processed_rdata <- downloadHandler(
    filename = function() {
      paste0("RNAcross_custom_data_", format(Sys.Date(), "%Y%m%d"), ".RData")
    },
    content = function(file) {
      all_species_data <- upload_state$custom_all_species_data
      save(all_species_data, file = file)
    }
  )
  # Dynamic species panels based on current configuration
  output$dynamic_species_panels <- renderUI({
    config <- current_species_config()
    
    # Create tab panels for each species with italicized names
    tabs <- lapply(names(config), function(id) {
      species <- c(list(id = id), config[[id]])
      tabPanel(
        title = tags$em(species$name),
        value = id,
        create_species_panel(species)
      )
    })
    
    # Return tabsetPanel with all species
    do.call(tabsetPanel, c(tabs, list(id = "species_tabs")))
  })
  output$download_gene_lookup <- downloadHandler(
    filename = function() {
      paste0("gene_lookup_table_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      if (!is.null(upload_state$custom_all_species_data$gene_lookup)) {
        write.csv(upload_state$custom_all_species_data$gene_lookup, file, row.names = FALSE)
      }
    }
  )
  
}
