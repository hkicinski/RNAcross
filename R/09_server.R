server <- function(input, output, session) {
  # Theme state
  is_dark <- reactiveVal(FALSE)
  containers_update_needed <- reactiveVal(TRUE)
  
  # Centralized container management function
  manage_combined_containers <- function(config) {
    # Clear ALL existing containers first
    removeUI("#combined_orthogroup_selection_wrapper > *", multiple = TRUE, immediate = TRUE)
    
    # Clear tracking
    for (sp_id in names(reactiveValuesToList(existing_containers))) {
      existing_containers[[sp_id]] <- NULL
    }
    
    # Wait for DOM to clear
    Sys.sleep(0.1)
    
    # Add containers for current configuration
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
  
  
  output$ridgeline_species_ui <- renderUI({
    config <- current_species_config()
    choices <- c(
      "All Species" = "all",
      setNames(names(config), sapply(config, `[[`, "name"))
    )
    selectInput(
      "ridgeline_species",
      "Select Species:",
      choices = choices,
      selected = "all"
    )
  })
  
  output$pca_species_ui <- renderUI({
    config <- current_species_config()
    choices <- setNames(
      names(config),
      sapply(config, `[[`, "name")
    )
    selectInput(
      "pca_species",
      "Select Species:",
      choices = choices,
      selected = names(config)[1]
    )
  })
  # Generate dynamic species menu
  output$group_analysis_species_ui <- renderUI({
    config <- current_species_config()
    choices <- setNames(
      names(config),
      sapply(config, `[[`, "name")
    )
    selectInput(
      "group_analysis_species",
      "Select Species:",
      choices = choices,
      selected = names(config)[1]
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
        title = species$name,
        value = paste0("species_", id),
        create_species_panel(species)
      )
    })
    
    # Return as nav_menu
    do.call(nav_menu, c(list(title = "Species Analysis"), menu_items))
  })
  
  # Generate species selection for combined view
  output$species_select_ui <- renderUI({
    config <- current_species_config()
    checkboxGroupInput(
      "species_select",
      "Select Species to Plot:",
      choices = setNames(
        names(config),
        sapply(config, `[[`, "name")
      ),
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
  #dynamic color assignment to sps (will add more colors if needed) 
  species_colors_dynamic <- reactive({
    config <- current_species_config()
    colors <- list()
    
    # Use a diverse color palette for automatic assignment
    auto_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
                     "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F",
                     "#BB8FCE", "#85C1E2", "#F8B739", "#52BE80",
                     "#E74C3C", "#3498DB", "#9B59B6", "#1ABC9C")
    
    color_index <- 1
    
    for (sp_code in names(config)) {
      sp_short <- config[[sp_code]]$short
      
      # Check if this species has a predefined color
      if (sp_short %in% names(DEFAULT_SPECIES_COLORS)) {
        colors[[sp_short]] <- DEFAULT_SPECIES_COLORS[[sp_short]]
      } else {
        # Assign from auto_colors palette
        colors[[sp_short]] <- auto_colors[color_index]
        color_index <- ((color_index - 1) %% length(auto_colors)) + 1
      }
    }
    
    colors
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
  
  # ========== DYNAMIC FILE UPLOAD UI GENERATION ==========
  
  # Generate expression upload UI based on defined species
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

  # Data source toggle
  data_source <- reactiveVal("default")  # "default" or "custom"
  
  # Function to get the appropriate all_species_data
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
  
  # Helper function to update combined species table
  update_combined_table <- function(query_results, combined_selections, is_dark) {
    if (is.null(query_results$combined)) return(NULL)
    
    result <- query_results$combined
    all_genes <- data.frame()
    
    for (sp in names(result$genes_by_species)) {
      if (nrow(result$genes_by_species[[sp]]) > 0) {
        sp_data <- result$genes_by_species[[sp]]
        config <- current_species_config()
        sp_data$Species <- if(sp %in% names(config)) {
          config[[sp]]$short
        } else {
          sp  
        }
        
        # Add a column to indicate currently selected gene(s)
        selected_genes <- combined_selections[[sp]]
        if (is.null(selected_genes)) selected_genes <- character(0)
        sp_data$Selected <- sp_data$gene_id %in% selected_genes
        
        all_genes <- rbind(all_genes, sp_data[, c("Species", "gene_id", "gene_name", "Selected")])
      }
    }
    
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
    )
    
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
  
  # PCA observer - MODIFIED FOR SINGLE-COPY ONLY
  observeEvent(input$run_pca, {
    waiter_show(html = loading_screen)
    
    # Add explicit debugging for PCA type
    cat("Debug: PCA type selected:", input$pca_type, "\n")
    
    tryCatch({
      if (input$pca_type == "single") {
        cat("Debug: Entering single-species PCA branch\n")
        species_data <- get_species_data(input$pca_species)
        
        # Handle different naming conventions
        lcpm_data <- if(input$pca_species == "cg") {
          species_data$lcpm
        } else {
          species_data[[paste0(input$pca_species, "_lcpm")]]
        }
        
        sample_info <- if(input$pca_species == "cg") {
          species_data$sample_info
        } else {
          species_data[[paste0(input$pca_species, "_sample_info")]]
        }
        
        if (is.null(lcpm_data) || is.null(sample_info)) {
          stop("Required data not found for selected species")
        }
        
        output$pca_plot <- renderPlotly({
          create_pca_plot(
            expression_matrix = lcpm_data,
            sample_info = sample_info,
            is_dark_mode = is_dark()
          )
        })
        
        # Add debugging output
        output$pca_debug_output <- renderPrint({
          cat("Single species PCA completed for:", input$pca_species, "\n")
          cat("Number of genes analyzed:", nrow(lcpm_data), "\n")
          cat("Number of samples:", ncol(lcpm_data), "\n")
          cat("Data transformation: centered-scaled\n")  
        })
        
      } else {
        cat("Debug: Entering multi-species PCA branch\n")
        # Multi-species PCA - NOW USING SINGLE-COPY ONLY!
        plot_result <- create_multi_species_pca(
          get_species_data = get_species_data,
          is_dark_mode = is_dark()
        )
        
        # Check if plot was created successfully
        if (!is.null(plot_result)) {
          output$pca_plot <- renderPlotly({
            plot_result
          })
          
          # Store matrices data for download
          pca_matrices_data <- attr(plot_result, "matrices_data")
          if (!is.null(pca_matrices_data)) {
            session$userData$pca_matrices <- pca_matrices_data
          }
        }
        
        # Add debugging output
        output$pca_debug_output <- renderPrint({
          cat("Multi-species PCA completed\n")
          cat("Using SINGLE-COPY GENES ONLY\n")
          cat("Comparing gene expression across species based on single-copy HOGs\n")
          if (!is.null(session$userData$pca_matrices)) {
            cat("Matrix dimensions:", 
                nrow(session$userData$pca_matrices$sample_matrix), "x",
                ncol(session$userData$pca_matrices$sample_matrix), "\n")
            cat("Total single-copy HOGs used:", ncol(session$userData$pca_matrices$sample_matrix), "\n")
          }
        })
      }
      
    }, error = function(e) {
      cat("Debug: Error occurred:", e$message, "\n")
      showNotification(
        paste("Error in PCA analysis:", e$message),
        type = "error",
        duration = NULL
      )
      
      # Show error in the debug output
      output$pca_debug_output <- renderPrint({
        cat("ERROR in PCA analysis:\n")
        cat(e$message, "\n")
      })
    })
    
    waiter_hide()
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
  
  # Get species data with caching
  species_data_cache <- new.env()
  #make get_species_data handle dynamic species
  get_species_data <- function(species_id) {
    # Get current configuration
    config <- current_species_config()
    
    # Check data source
    current_data <- if (data_source() == "custom" && !is.null(upload_state$custom_all_species_data)) {
      upload_state$custom_all_species_data
    } else {
      all_species_data
    }
    
    if (!exists(species_id, envir = species_data_cache)) {
      # Check if this is a known species in the current data
      if (species_id %in% names(current_data)) {
        sp_config <- config[[species_id]]
        
        # Initialize variables
        lcpm_data <- NULL
        anno_data <- NULL
        sample_info_data <- NULL
        
        # Try multiple strategies to find the data
        # Strategy 1: Direct naming (standard for uploaded data and cg)
        if (!is.null(current_data[[species_id]]$lcpm)) {
          lcpm_data <- current_data[[species_id]]$lcpm
          anno_data <- current_data[[species_id]]$anno
          sample_info_data <- current_data[[species_id]]$sample_info
        } 
        # Strategy 2: Prefixed naming (for original sc, kl, ca data)
        else {
          lcpm_key <- paste0(species_id, "_lcpm")
          anno_key <- paste0(species_id, "_anno")
          sample_info_key <- paste0(species_id, "_sample_info")
          
          if (!is.null(current_data[[species_id]][[lcpm_key]])) {
            lcpm_data <- current_data[[species_id]][[lcpm_key]]
            anno_data <- current_data[[species_id]][[anno_key]]
            sample_info_data <- current_data[[species_id]][[sample_info_key]]
          }
        }
        
        # Verify we found the essential data
        if (is.null(lcpm_data)) {
          warning(paste("No expression data found for species:", species_id))
          return(NULL)
        }
        
        # Build data structure with both naming conventions for maximum compatibility
        data <- list(
          lcpm = lcpm_data,
          anno = anno_data,
          sample_info = sample_info_data,
          species_name = if(!is.null(sp_config$short)) sp_config$short else species_id
        )
        
        # Add alternative naming for compatibility
        data[[paste0(species_id, "_lcpm")]] <- lcpm_data
        data[[paste0(species_id, "_sample_info")]] <- sample_info_data
        data[[paste0(species_id, "_anno")]] <- anno_data
        
        assign(species_id, data, envir = species_data_cache)
      } else {
        return(NULL)
      }
    }
    get(species_id, envir = species_data_cache)
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
                species_name,
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
              h6(species_name),
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
  
  #observers dynamically based on current configuration
  observe({
    config <- current_species_config()
    
    # Create observers for each species
    lapply(names(config), function(species_id) {
      # Create unique observer ID to avoid duplicates
      obs_search_id <- paste0("obs_search_", species_id)
      obs_plot_id <- paste0("obs_plot_", species_id)
      obs_download_id <- paste0("obs_download_", species_id)
      
      # Only create if not already exists
      if (!exists(obs_search_id, envir = session$userData)) {
        # Search button handler
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
            create_orthogroup_selection_ui_enhanced(gene_result, species_id)
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
          
          # Update orthogroup results table
          output[[paste0(species_id, "_orthogroup_results")]] <- renderDT({
            ortho_data <- data.frame()
            
            for (sp in names(gene_result$genes_by_species)) {
              if (nrow(gene_result$genes_by_species[[sp]]) > 0) {
                sp_data <- gene_result$genes_by_species[[sp]]
                config <- current_species_config()
                sp_data$Species <- config[[sp]]$short
                sp_data$Current <- (sp == species_id)
                ortho_data <- rbind(ortho_data, sp_data[, c("Species", "gene_id", "gene_name", "Current")])
              }
            }
            
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
              )
              
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
      
      # Plot button handler - SIMPLIFIED
      if (!exists(obs_plot_id, envir = session$userData)) {
        session$userData[[obs_plot_id]] <- observeEvent(input[[paste0(species_id, "_plot_button")]], {
          waiter_show(html = loading_screen)
          
          selected_gene <- input[[paste0(species_id, "_", species_id, "_selection")]]
          
          if (is.null(selected_gene) || selected_gene == "") {
            showNotification("Please select a gene", type = "warning")
            waiter_hide()
            return()
          }
          
          species_data <- get_species_data(species_id)
          
          output[[paste0(species_id, "_gene_plot")]] <- renderPlotly({
            config <- current_species_config()
            create_gene_plot(
              lc = species_data$lcpm,
              gene = selected_gene,
              sample_info = species_data$sample_info,
              species_name = config[[species_id]]$name,
              is_dark_mode = is_dark(),
              species_colors = species_colors_dynamic()
            )
          })
          
          output[[paste0(species_id, "_gene_info")]] <- renderText({
            gene_info <- species_data$anno[species_data$anno$GeneID == selected_gene, ]
            if (nrow(gene_info) > 0) {
              paste("Gene ID:", selected_gene,
                    "\nGene Name:", gene_info$GeneName[1],
                    "\nChromosome:", gene_info$Chr[1])
            } else {
              paste("Gene ID:", selected_gene)
            }
          })
          
          waiter_hide()
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
                lc = species_data$lcpm,
                gene = selected_gene,
                sample_info = species_data$sample_info,
                species_name = config[[species_id]]$name,
                is_dark_mode = FALSE,
                species_colors = species_colors_dynamic()
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
  
  # Combined plot button observer
  observeEvent(input$combined_plot_button, {
    req(input$species_select)
    waiter_show(html = loading_screen)
    
    # Collect all selected genes
    selected_genes_list <- list()
    
    for (species_code in input$species_select) {
      # Get selections from combined_selections reactive values
      gene_ids <- combined_selections[[species_code]]
      
      if (!is.null(gene_ids) && length(gene_ids) > 0) {
        selected_genes_list[[species_code]] <- gene_ids
      }
    }
    
    # Check if we have any genes selected
    if (length(selected_genes_list) == 0) {
      showNotification("No genes selected for plotting. Please search for a gene first.", type = "warning")
      waiter_hide()
      return()
    }
    
    # Create combined plot data with support for multiple genes per species
    plot_data <- data.frame()

    config <- current_species_config()
    
    for (species_code in names(selected_genes_list)) {
      species_data <- get_species_data(species_code)
      gene_ids <- selected_genes_list[[species_code]]
      
      for (gene_id in gene_ids) {
        # Handle K. lactis underscore issue (only for original data)
        gene_id_to_use <- gene_id
        if (data_source() == "default" && species_code == "kl" && !gene_id %in% rownames(species_data$lcpm)) {
          gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
          if (gene_id_alt %in% rownames(species_data$lcpm)) {
            gene_id_to_use <- gene_id_alt
          }
        }
        
        if (gene_id_to_use %in% rownames(species_data$lcpm)) {
          species_name <- config[[species_code]]$name
          
          # Get gene name for display
          gene_name <- ""
          anno_idx <- which(species_data$anno$GeneID == gene_id)
          if (length(anno_idx) > 0) {
            gene_name <- species_data$anno$GeneName[anno_idx[1]]
            if (is.na(gene_name)) gene_name <- ""
          }
          
          gene_display <- if (gene_name != "") {
            paste0(gene_name, " (", gene_id, ")")
          } else {
            gene_id
          }
          
          expr_data <- data.frame(
            Gene = gene_display,
            GeneID = gene_id,
            Species = species_name,
            SpeciesCode = species_code,
            Timepoint = factor(species_data$sample_info$Timepoint, levels = TIME_POINTS),
            Replicate = species_data$sample_info$Replicate,
            Expression = as.numeric(species_data$lcpm[gene_id_to_use, ])
          )
          
          plot_data <- rbind(plot_data, expr_data)
        }
      }
    }
    
    if (nrow(plot_data) > 0) {
      output$combined_gene_plot <- renderPlotly({
        # Create a unique identifier for each gene-species combination
        plot_data$GeneSpecies <- paste(plot_data$Gene, "-", plot_data$Species)
        
        # Create color palette for all unique gene-species combinations
        unique_combinations <- unique(plot_data$GeneSpecies)
        n_combinations <- length(unique_combinations)
        
        # Use a color palette that can handle many colors
        if (n_combinations <= 8) {
          colors <- RColorBrewer::brewer.pal(max(3, n_combinations), "Set1")
        } else {
          colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_combinations)
        }
        
        color_map <- setNames(colors[1:n_combinations], unique_combinations)
        
        # Add replicate information
        plot_data$GeneSpeciesRep <- paste(plot_data$GeneSpecies, "Rep", plot_data$Replicate)
        
        # Create color vector with alpha for replicates
        color_vector <- c()
        for (combo in unique_combinations) {
          base_color <- color_map[combo]
          color_vector <- c(color_vector, 
                            base_color,  # Rep 1 - full opacity
                            adjustcolor(base_color, alpha.f = 0.6))  # Rep 2 - reduced opacity
        }
        
        # Create proper labels for the color vector
        rep_labels <- c()
        for (combo in unique_combinations) {
          rep_labels <- c(rep_labels,
                          paste(combo, "Rep 1"),
                          paste(combo, "Rep 2"))
        }
        names(color_vector) <- rep_labels
        
        # Create the plot
        p <- ggplot(plot_data,
                    aes(x = Timepoint, y = Expression,
                        color = GeneSpeciesRep, group = GeneSpeciesRep,
                        text = paste("Gene:", Gene,
                                     "<br>Species:", Species,
                                     "<br>Replicate:", Replicate,
                                     "<br>Time:", Timepoint,
                                     "<br>Expression:", round(Expression, 2)))) +
          geom_point(size = 3, alpha = 0.8) +
          geom_line(linewidth = 1.2, alpha = 0.9) +
          scale_color_manual(
            values = color_vector,
            name = "Gene - Species",
            breaks = names(color_vector),
            labels = names(color_vector)
          ) +
          labs(
            y = "log2 count per million",
            title = "Cross-species Expression Comparison",
            subtitle = paste("Comparing", length(unique(plot_data$Gene)), "genes across", 
                             length(unique(plot_data$Species)), "species"),
            x = "Timepoint"
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
            legend.text = element_text(color = if(is_dark()) "white" else "black", size = 9),
            legend.title = element_text(color = if(is_dark()) "white" else "black", size = 11),
            legend.background = element_rect(fill = if(is_dark()) "#2c3034" else "white"),
            legend.key = element_rect(fill = if(is_dark()) "#2c3034" else "white")
          )
        
        # Convert to plotly with custom hover text
        ggplotly(p, tooltip = "text") %>%
          layout(
            plot_bgcolor = if(is_dark()) "#2c3034" else "white",
            paper_bgcolor = if(is_dark()) "#2c3034" else "white",
            font = list(color = if(is_dark()) "white" else "black"),
            hoverlabel = list(
              bgcolor = if(is_dark()) "#444" else "white",
              font = list(size = 12)
            ),
            showlegend = TRUE,
            legend = list(
              x = 1.02,
              y = 0.5,
              bgcolor = if(is_dark()) "#2c3034" else "white",
              bordercolor = if(is_dark()) "#444" else "#ddd",
              borderwidth = 1,
              font = list(size = 10),
              tracegroupgap = 5
            ),
            margin = list(b = 100, r = 250, t = 80, l = 60),
            xaxis = list(
              tickfont = list(size = 11)
            ),
            yaxis = list(
              tickfont = list(size = 11)
            )
          ) %>%
          config(
            displayModeBar = TRUE,
            modeBarButtons = list(
              list("zoom2d", "pan2d", "resetScale2d", "toImage")
            )
          )
      })
    } else {
      output$combined_gene_plot <- renderPlotly({
        plotly_empty() %>%
          add_annotations(
            text = "No valid expression data found for selected genes",
            showarrow = FALSE
          )
      })
      showNotification("No valid expression data found for selected genes", type = "error")
    }
    
    waiter_hide()
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
  observeEvent(input$generate_ortholog_heatmap, {
    req(input$ortholog_gene_list)
    waiter_show(html = loading_screen)
    
    # split gene list by newlines, commas, or semicolons
    gene_list <- unlist(strsplit(input$ortholog_gene_list, "[\n\r,;]+"))
    gene_list <- trimws(gene_list)
    gene_list <- gene_list[gene_list != ""]
    
    # only proceed if we have genes
    if (length(gene_list) == 0) {
      showNotification("Please enter at least one gene", type = "error")
      waiter_hide()
      return()
    }
    
    config <- current_species_config()
    current_data <- get_all_species_data()
    species_data_list <- list()
    for (species_id in names(config)) {
      species_data_list[[species_id]] <- get_species_data(species_id)
    }
    
    # generate the cross-species heatmap
    result <- tryCatch({
      generate_cross_species_heatmap(
        gene_list = gene_list,
        species_data_list = species_data_list,
        normalization = input$ortholog_normalization,
        is_dark_mode = is_dark(),
        cluster_rows = input$ortholog_cluster_rows,
        cluster_cols = input$ortholog_cluster_cols,
        config = config,
        all_species_data = current_data
      )
    }, error = function(e) {
      list(
        plot = NULL,
        table = NULL,
        error = paste("Error generating heatmap:", e$message)
      )
    })
    
    # check for errors
    if (!is.null(result$error)) {
      showNotification(result$error, type = "error", duration = NULL)
      waiter_hide()
      return()
    }
    
    # store results for download
    ortholog_result(result)
    
    # update UI with results
    output$ortholog_heatmap_plot <- renderPlotly({
      result$plot
    })
    
    output$ortholog_mapping_table <- renderDT({
      result$table
    })
    
    waiter_hide()
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
  
  observeEvent(input$analyze_gene_groups, {
    req(!is.null(input$gene_list) || !is.null(input$gene_group_file))
    if (is.null(input$gene_list) && is.null(input$gene_group_file)) {
      showNotification("Please provide either a gene list or upload a file", type = "error")
      return()
    }
    
    waiter_show(html = loading_screen)
    
    tryCatch({
      # Process input data
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
      config <- current_species_config()  
      current_data <- get_all_species_data()  # Get the correct data source
      plot_data <- process_gene_group_data(
        gene_groups, 
        species_data, 
        current_data,  # Use current_data instead of global all_species_data
        config,  
        input$group_analysis_species  
      )
      
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
        
        # Regenerate the plot
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
            selected_comparisons = sig_test_params$comparisons
          )
        })
      })
      
      # initialize the plot without significance testing
      output$gene_group_plot <- renderPlotly({
        create_group_visualization(
          plot_data = plot_data, 
          viz_type = input$group_viz_type, 
          is_dark_mode = is_dark(), 
          distance_method = input$distance_method,
          data_transform = input$data_transform,
          show_significance = input$show_significance,
          alpha = input$significance_threshold,
          selected_gene = NULL,
          selected_comparisons = NULL
        )
      })
      config <- current_species_config()
      species_name <- config[[input$group_analysis_species]]$name
      output$gene_group_table <- renderDT({
        create_group_summary_table(plot_data, species_name)
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
      paste("gene_group_", input$group_viz_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      p <- plotly::plotly_build(isolate(output$gene_group_plot()))
      plotly::export(p, file = file)
    }
  )

  # Ridgeline Plot handlers
  observeEvent(input$generate_ridgeline, {
    waiter_show(html = loading_screen)
    
    # Get data for selected species or all species
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
    
    # Create the ridgeline plot
    output$ridgeline_plot <- renderPlot({
      if (input$ridgeline_view == "distribution") {
        create_ridgeline_plot(
          species_data_list = species_data_list,
          is_dark_mode = is_dark()
        )
      } else {
        create_threshold_ridgeline(
          species_data_list = species_data_list,
          threshold = input$expression_threshold,
          is_dark_mode = is_dark()
        )
      }
    })
    
    waiter_hide()
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
          create_phylo_tree_plot(tree, query_result$genes_by_species, query_result$orthogroup, is_dark(), current_data, species_colors_dynamic(), current_species_config())
        } else {
          plot.new()
          text(0.5, 0.5, "Phylogenetic tree not available for this orthogroup", 
               cex = 1.2, col = if(is_dark()) "white" else "black")
        }
      }, bg = if(is_dark()) "#2c3034" else "white", res = 120)
      }
      
      #update orthogroup summary  
      output$orthogroup_summary <- renderUI({
        create_orthogroup_summary(query_result, current_species_config())
      })
      
      # Update the orthogroup table with enhanced formatting
      output$explorer_orthogroup_table <- renderDT({
        create_orthogroup_details_table(query_result)
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
  
  #quick action: view in Species Analysis
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
        
        # Navigate to Species Analysis and then to the specific species tab
        updateTabsetPanel(session, "nav", selected = "species_analysis_container")
        
        # Small delay to ensure the main tab is loaded
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
        #navigate to Species Analysis and then to the specific species tab
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
    
    # Navigate to Species Analysis and then to the specific species tab
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
  
  # Quick action: View in Combined Analysis
  observeEvent(input$explore_combined_view, {
    req(global_query_state$query_result)
    
    # Navigate to combined view
    updateTabsetPanel(session, "nav", selected = "Combined View")
    
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
          is_dark_mode = is_dark()
        )
      } else {
        create_threshold_ridgeline(
          species_data_list = species_data_list,
          threshold = input$expression_threshold,
          is_dark_mode = is_dark()
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
    
    lookup_table <- do.call(rbind, lookup_entries)
    
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
      #debug
      if (!is.null(expr_data)) {
        print(paste("DEBUG - Species:", species_name))
        print(paste("DEBUG - Columns:", paste(names(expr_data), collapse=", ")))
        print(paste("DEBUG - Column types:", paste(sapply(expr_data, class), collapse=", ")))
        print(paste("DEBUG - First few row names (gene IDs):", paste(head(rownames(expr_data), 3), collapse=", ")))
        print(paste("DEBUG - Dimensions:", nrow(expr_data), "x", ncol(expr_data)))
        print(paste("DEBUG - All columns numeric?", all(sapply(expr_data, is.numeric))))
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
    
    # Create tab panels for each species
    tabs <- lapply(names(config), function(id) {
      species <- c(list(id = id), config[[id]])
      tabPanel(
        title = species$name,
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

# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))