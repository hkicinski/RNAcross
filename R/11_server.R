# 11_server.R
# RNAcross Server Logic
#
# Complete Shiny server function containing all reactive expressions,
# observers, render functions, and event handlers.
#
# Dependencies: 01_config, 02_constants_themes, 03_utils, 04_data_io,
#              05_orthology_query, 06_data_process, 07_visualization_core,
#              08_visualization_heatmaps, 09_visualization_outputs, 10_ui
server <- function(input, output, session) {
  # Theme state
  is_dark <- reactiveVal(FALSE)
  containers_update_needed <- reactiveVal(TRUE)
  
  # Initialize the interactive aesthetic editor
  rv_plot_aesthetics <- setup_interactive_editor(input, output, session)

  # centralized container management
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
  # dynamic combined species
  observeEvent(upload_state$processed, {
    if (upload_state$processed && data_source() == "custom") {
      containers_update_needed(TRUE)
    }
  })

  # New code (lines 2863-2867)
  observeEvent(data_source(),
    {
      config <- current_species_config()
      manage_combined_containers(config)
    },
    ignoreInit = TRUE
  )


  # aggregation method info text
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
    # build choices: "All Species" (not italic) + species names (italic via render)
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
    # hide when cross-species ortholog analysis is enabled
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

    # return as nav_menu
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
    custom_all_species_data = NULL,
    study_design = NULL, #wizard-built design; current_study_design() reads this
    design_inferred = FALSE #TRUE when we fell back to an inferred design
  )

  # ortholog analysis state
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
          column(
            3,
            textInput(paste0("species_code_", new_count), "Code*",
              value = "", placeholder = "e.g., at"
            )
          ),
          column(
            5,
            textInput(paste0("species_name_", new_count), "Full Name*",
              value = "", placeholder = "e.g., Arabidopsis thaliana"
            )
          ),
          column(
            4,
            textInput(paste0("species_short_", new_count), "Display Name",
              value = "", placeholder = "e.g., Arabidopsis"
            )
          )
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
      # Defensive get: input might be NULL during initialization/restore
      raw_code <- input[[paste0("species_code_", i)]]
      raw_name <- input[[paste0("species_name_", i)]]
      raw_short <- input[[paste0("species_short_", i)]]

      # Skip if input hasn't matched yet
      if (is.null(raw_code)) next

      code <- trimws(raw_code)
      name <- if (!is.null(raw_name)) trimws(raw_name) else ""
      short <- if (!is.null(raw_short)) trimws(raw_short) else ""

      if (length(code) > 0 && code != "") {
        # Auto-generate short name if not provided
        if (is.null(short) || short == "") {
          # Try to create abbreviated form (e.g., "H. sapiens" from "Homo sapiens")
          name_parts <- strsplit(name, " ")[[1]]
          if (length(name_parts) >= 2) {
            short <- paste0(
              substr(name_parts[1], 1, 1), ". ",
              paste(name_parts[-1], collapse = " ")
            )
          } else {
            short <- name
          }
        }

        species_data[[code]] <- list(
          code = code,
          name = if (!is.null(name) && name != "") name else code,
          short = short
        )
      }
    }

    species_data
  })

  # Dynamic species configuration
  current_species_config <- reactive({
    base_config <- if (data_source() == "custom" && length(defined_species()) > 0) {
      defined_species()
    } else {
      DEFAULT_SPECIES_CONFIG
    }
    
    # only relabel sc when it is actually configured
    if ("sc" %in% names(base_config)) {
      base_config[["sc"]]$name <- if (isTRUE(plot_settings$contrast_mode_enabled)) {
        "Saccharomyces cerevisiae (Contrast Mode)"
      } else {
        "Saccharomyces cerevisiae"
      }
    }

    base_config
  })

  # initialize plot settings
  plot_settings <- reactiveValues(
    # species identity
    species_palette = STANDARD_SPECIES_PALETTE,
    species_colors = list(),
    species_shapes = list(),
    updating_colors_from_palette = FALSE,

    # global data settings
    global_transform = NULL, # NULL = follow the data

    # global line aesthetics
    line_thickness = 1,
    line_type = "solid",
    y_axis_manual = FALSE,
    y_axis_min = 0,
    y_axis_max = 15,

    # multi-gene line/point encoding
    encoding_multigene_color = "species",
    encoding_multigene_secondary = "linetype",
    gene_palette = "Set2",

    # similarity search encoding
    encoding_similarity_color = "species",
    encoding_similarity_secondary = "linetype",
    similarity_palette = "Set2",
    similarity_gene_colors = list(),
    updating_similarity_colors_from_palette = FALSE,

    # heatmap settings
    heatmap_palette = "viridis",
    heatmap_scale_type = "sequential",
    heatmap_midpoint = "auto",
    heatmap_show_row_dendro = TRUE,
    heatmap_show_col_dendro = TRUE,
    heatmap_row_annotation = TRUE,

    # ridgeline settings
    ridgeline_palette = "viridis",
    ridgeline_alpha = 0.8,

    # pca settings
    encoding_pca_color = "species",
    encoding_pca_shape = "species",
    pca_alpha = 0.8,
    pca_point_size = 3,
    pca_show_ellipses = TRUE,
    pca_show_loadings = FALSE,

    # export settings
    export_width = 8,
    export_height = 6,
    export_dpi = 300,
    export_format = "png",

    # saved presets
    presets = list(),

    # initialization flag
    initialized = FALSE
  )

  # plot state tracking for reactive re-rendering on settings changes
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

  # initialize settings from species config
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

  # uploaded species join the config after init, so fill gaps; user picks stay
  observeEvent(current_species_config(), {
    req(plot_settings$initialized)
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    colors <- derive_species_colors(species_list, plot_settings$species_palette, NULL, config)
    shapes <- derive_species_shapes(species_list, NULL, config)
    for (nm in names(colors)) {
      if (is.null(plot_settings$species_colors[[nm]])) plot_settings$species_colors[[nm]] <- colors[[nm]]
    }
    for (nm in names(shapes)) {
      if (is.null(plot_settings$species_shapes[[nm]])) plot_settings$species_shapes[[nm]] <- shapes[[nm]]
    }
  })


  # dynamic color assignment reads from plot_settings
  species_colors_dynamic <- reactive({
    if (plot_settings$initialized && length(plot_settings$species_colors) > 0) {
      return(plot_settings$species_colors)
    }
    # fallback: generate from current palette setting
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    derive_species_colors(species_list, plot_settings$species_palette, NULL, config)
  })

  # Display current species table
  output$current_species_table <- renderTable(
    {
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
    },
    striped = TRUE,
    hover = TRUE,
    spacing = "xs"
  )

  # expression upload UI based on defined species
  output$expression_upload_ui <- renderUI({
    species <- defined_species()

    if (length(species) == 0) {
      return(div(
        class = "text-muted", icon("arrow-up"),
        " Define species in Step 1 first"
      ))
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
      return(div(
        class = "text-muted", icon("arrow-up"),
        " Define species in Step 1 first"
      ))
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
      return(div(
        class = "text-muted", icon("arrow-up"),
        " Define species in Step 1 first"
      ))
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

  # data source toggle
  data_source <- reactiveVal("default")

  # clear cache when data source changes
  observeEvent(data_source(),
    {
      clear_performance_cache()
      debug_print("Performance cache cleared")
    },
    ignoreInit = TRUE
  )

  #active study_design (R/02b): stock uses GRE design; uploaded design later
  current_study_design <- reactive({
    if (data_source() == "custom" && !is.null(upload_state$study_design)) {
      upload_state$study_design
    } else {
      GRE_study_design()
    }
  })

  # get appropriate species data
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
    last_search_time = NULL,
    #ok / orphan / synteny / not_found / cached, so the pill can say what happened
    last_status = NULL,
    #when the cached session was saved, for the resume card's relative time
    restored_at = NULL
  )

  # restore session on startup
  observeEvent(input$restore_session,
    {
      req(input$restore_session)

      debug_print("restore session triggered")

      state <- input$restore_session$state
      saved_at <- input$restore_session$saved_at
      time_ago <- format_time_ago(saved_at)

      debug_print(paste("restoring state from", time_ago))
      debug_print(paste("state contents:", paste(names(state), collapse = ", ")))

      # delay restoration to ensure UI elements are rendered
      shinyjs::delay(500, {
        restore_session_state(state, selected_genes, query_results, global_query_state, combined_selections, ortholog_state, session)
        
        if (!is.null(state$plot_settings_gene_colors)) {
          plot_settings$gene_colors <- state$plot_settings_gene_colors
        }
        if (!is.null(state$plot_settings_similarity_gene_colors)) {
          plot_settings$similarity_gene_colors <- state$plot_settings_similarity_gene_colors
        }
        if (!is.null(state$plot_settings_sc_dataset)) {
          plot_settings$sc_dataset <- state$plot_settings_sc_dataset
        }
        if (!is.null(state$plot_settings_contrast_transform)) {
          plot_settings$contrast_transform <- state$plot_settings_contrast_transform
        }

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
    },
    once = TRUE
  )

  # track initialization state to prevent saving empty data
  session_initialized <- reactiveVal(FALSE)

  # mark session as ready after short delay
  observe({
    invalidateLater(2000)
    isolate({
      session_initialized(TRUE)
      debug_print("session initialized, auto-save enabled")
    })
  }) %>% bindEvent(TRUE, once = TRUE)

  # auto-save session after changes
  observe({
    req(session_initialized())

    # reactive dependencies that trigger save
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
      aggregation = input$aggregation_level,
      gene_colors = plot_settings$gene_colors,
      sim_gene_colors = plot_settings$similarity_gene_colors,
      sc_dataset = plot_settings$sc_dataset,
      contrast_transform = plot_settings$contrast_transform
    )

    isolate({
      if (is.null(deps$nav)) {
        return()
      }

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
        ortholog_selected = if (exists("ortholog_state")) {
          ortholog_state$selected_orthologs
        } else {
          NULL
        },
        plot_settings_gene_colors = deps$gene_colors,
        plot_settings_similarity_gene_colors = deps$sim_gene_colors,
        plot_settings_sc_dataset = deps$sc_dataset,
        plot_settings_contrast_transform = deps$contrast_transform
      )

      session$sendCustomMessage(type = "saveSession", message = current_state)
      debug_print("session state saved")
    })
  }) %>% debounce(2000)

  # handle start fresh button
  observeEvent(input$clear_session_btn, {
    session$sendCustomMessage(type = "clearSession", message = list())
  })


  # helper function to update combined species table
  update_combined_table <- function(query_results, combined_selections, is_dark) {
    if (is.null(query_results$combined)) {
      return(NULL)
    }

    result <- query_results$combined
    config <- current_species_config()

    # collect data in list first, then combine once
    genes_list <- lapply(names(result$genes_by_species), function(sp) {
      sp_data <- result$genes_by_species[[sp]]
      if (nrow(sp_data) == 0) {
        return(NULL)
      }

      sp_data$Species <- if (sp %in% names(config)) config[[sp]]$short else sp

      # add a column to indicate currently selected gene(s)
      selected_genes <- combined_selections[[sp]]
      if (is.null(selected_genes)) selected_genes <- character(0)
      sp_data$Selected <- sp_data$gene_id %in% selected_genes

      sp_data[, c("Species", "gene_id", "gene_name", "Selected")]
    })

    # combine all at once
    all_genes <- rbindlist(Filter(Negate(is.null), genes_list), fill = TRUE)

    if (nrow(all_genes) == 0) {
      return(NULL)
    }

    dt <- datatable(
      all_genes[, c("Species", "gene_id", "gene_name")],
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tp"
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
        backgroundColor = styleRow(
          which(all_genes$Selected),
          if (is_dark()) "#3a4a5a" else "#e6f3ff"
        )
      )
    }

    return(dt)
  }

  # Observe for gene group analysis (bar plot significance testing)
  observe({
    req(input$gene_list, input$group_viz_type == "bar")

    gene_list <- strsplit(trimws(input$gene_list), "[,;[:space:]]+")[[1]]
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
    timepoint_pairs <- combn(condition_levels(current_study_design()), 2, simplify = FALSE)
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
      species_data_list[[species_id]] <- get_species_data(species_id, force_no_contrast = TRUE)
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
        .groups = "drop"
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
            Timepoint = condition_of(current_study_design(), sample_info),
            Replicate = replicate_of(current_study_design(), sample_info),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    sample_metadata <- do.call(rbind, sample_metadata_list)

    # Create expression matrix
    sample_matrix <- matrix(NA,
      nrow = nrow(sample_metadata),
      ncol = length(common_hogs)
    )
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

  # pca observer - stores data for reactive rendering
  observeEvent(input$run_pca, {
    waiter_show(html = loading_screen)

    tryCatch(
      {
        if (input$pca_type == "single") {
          resolved_species <- input$pca_species
          if (resolved_species == "sc" && isTRUE(plot_settings$contrast_mode_enabled)) {
            showNotification("Contrast mode active. Using base 2026 WT dataset for PCA.", type = "warning", duration = 5)
          }
          
          species_data <- get_species_data(resolved_species, force_no_contrast = TRUE)
          lcpm_data <- get_expression_matrix(resolved_species, active_transform(), species_data)
          sample_info <- if (resolved_species == "cg") species_data$sample_info else species_data[[paste0(resolved_species, "_sample_info")]]

          if (is.null(lcpm_data) || is.null(sample_info)) {
            stop("Required data not found for selected species")
          }

          plot_state$pca_data <- list(
            type = "single",
            expression_matrix = lcpm_data,
            sample_info = sample_info,
            species = resolved_species,
            n_genes = nrow(lcpm_data),
            n_samples = ncol(lcpm_data)
          )
          plot_state$pca_ready <- TRUE
        } else {
          aggregation_method <- if (!is.null(input$hog_aggregation_method)) input$hog_aggregation_method else "eigengene"

          plot_result <- create_multi_species_pca(
            get_species_data = function(sp) get_species_data(sp, force_no_contrast = TRUE),
            is_dark_mode = is_dark(),
            aggregation_method = aggregation_method,
            species_config = current_species_config(),
            all_species_data_obj = get_all_species_data(),
            transform_type = active_transform(),
            plot_settings = reactiveValuesToList(plot_settings),
            study_design = current_study_design()
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
      },
      error = function(e) {
        showNotification(paste("Error in PCA analysis:", e$message), type = "error", duration = NULL)
        plot_state$pca_ready <- FALSE
        plot_state$pca_data <- list(error = e$message)
      }
    )

    waiter_hide()
  })

  # pca plot dynamic container
  output$pca_plot_container <- renderUI({
    mode <- plot_settings$pca_viz_mode
    if (is.null(mode)) mode <- "interactive"

    if (mode == "publication") {
      height <- plot_settings$pca_export_height
      if (is.null(height)) height <- 8
      plotOutput("pca_plot_publication", height = paste0(height * 100, "px"))
    } else {
      plotlyOutput("pca_plot_interactive", height = "500px")
    }
  })

  # Helper function to generate PCA plots
  generate_pca_plot <- function() {
    if (!isTRUE(plot_state$pca_ready) || is.null(plot_state$pca_data)) {
      return(NULL)
    }

    pca_data <- plot_state$pca_data
    if (!is.null(pca_data$error)) {
      return(NULL)
    }

    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)

    if (pca_data$type == "single") {
      create_pca_plot(
        expression_matrix = pca_data$expression_matrix,
        sample_info = pca_data$sample_info,
        is_dark_mode = dark_mode,
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    } else {
      create_multi_species_pca(
        get_species_data = get_species_data,
        is_dark_mode = dark_mode,
        aggregation_method = pca_data$aggregation_method,
        species_config = current_species_config(),
        all_species_data_obj = get_all_species_data(),
        transform_type = active_transform(),
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    }
  }

  output$pca_plot_interactive <- renderPlotly({
    p <- generate_pca_plot()
    if (is.null(p)) {
      if (!is.null(plot_state$pca_data$error)) {
        return(plotly_empty() %>% add_annotations(text = paste("Error:", plot_state$pca_data$error), showarrow = FALSE))
      }
      return(plotly_empty() %>% add_annotations(text = "Click Run PCA to generate plot", showarrow = FALSE))
    }
    p
  })

  output$pca_plot_publication <- renderPlot({
    p <- generate_pca_plot()
    if (is.null(p)) {
      return(NULL)
    }
    p
  })

  # pca debug output
  output$pca_debug_output <- renderPrint({
    if (!isTRUE(plot_state$pca_ready) || is.null(plot_state$pca_data)) {
      return(invisible())
    }

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
        "Unknown method"
      )
      cat(method_desc, "\n")

      if (!is.null(pca_data$matrices_data)) {
        cat("\nMatrix:", nrow(pca_data$matrices_data$sample_matrix), "x", ncol(pca_data$matrices_data$sample_matrix), "\n")
      }
    }
  })

  # ----- PCA Export Modal -----
  observeEvent(input$show_pca_export_modal, {
    show_plot_export_modal("execute_pca_export", "Export PCA Plot")
  })

  observeEvent(input$execute_pca_export_confirm, {
    req(plot_state$pca_data)

    fmt <- input$execute_pca_export_format
    w <- input$execute_pca_export_width
    h <- input$execute_pca_export_height
    dpi_val <- input$execute_pca_export_dpi
    if (is.null(fmt)) fmt <- "png"
    if (is.null(w)) w <- 10
    if (is.null(h)) h <- 8
    if (is.null(dpi_val)) dpi_val <- 300

    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)
    current_settings$pca_viz_mode <- "publication"

    p <- if (plot_state$pca_data$type == "single") {
      create_pca_plot(
        expression_matrix = plot_state$pca_data$expression_matrix,
        sample_info = plot_state$pca_data$sample_info,
        is_dark_mode = dark_mode,
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    } else {
      create_multi_species_pca(
        get_species_data = get_species_data,
        is_dark_mode = dark_mode,
        aggregation_method = plot_state$pca_data$aggregation_method,
        species_config = current_species_config(),
        all_species_data_obj = get_all_species_data(),
        transform_type = active_transform(),
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    }

    req(p)
    if (inherits(p, "ggplot")) {
      tmp <- tempfile(fileext = paste0(".", fmt))
      ggplot2::ggsave(
        filename = tmp, plot = p, device = fmt,
        width = w, height = h, dpi = dpi_val,
        units = "in", bg = if (dark_mode) "#2c3034" else "white"
      )
      raw <- readBin(tmp, "raw", file.size(tmp))
      encoded <- jsonlite::base64_enc(raw)
      mime <- switch(fmt,
        png = "image/png", jpeg = "image/jpeg",
        pdf = "application/pdf", svg = "image/svg+xml",
        "application/octet-stream"
      )
      session$sendCustomMessage("download_base64", list(
        data = encoded,
        filename = paste0("RNAcross_PCA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt),
        mime = mime
      ))
      removeModal()
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
        icon = icon("sun", verify_fa = FALSE)
      )
      session$setCurrentTheme(dark_theme)
    } else {
      # Switching to light mode
      removeCssClass("html", "dark-mode")
      updateActionButton(session, "theme_toggle",
        icon = icon("moon", verify_fa = FALSE)
      )
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

  # settings modal
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
      div(
        class = "mb-3 p-3",
        style = "background-color: #f8f9fa; border-radius: 4px; border: 1px solid #dee2e6;",
        h5(icon("database"), " Expression Data Source", style = "margin-top: 0;"),
        uiOutput("settings_transform_ui"),
        h5(icon("dna"), " ", tags$em("Saccharomyces cerevisiae"), " Dataset", style = "margin-top: 15px;"),
        materialSwitch(
          inputId = "settings_contrast_mode_enabled",
          label = "Enable Contrast Mode (Overlay Datasets)",
          value = isolate(plot_settings$contrast_mode_enabled %||% FALSE),
          status = "success"
        ),
        conditionalPanel(
          condition = "input.settings_contrast_mode_enabled == false",
          radioButtons(
            "settings_sc_dataset",
            label = NULL,
            choices = c("2023 Data" = "2023", 
                        "WT S288C 2026 (yH545)" = "yH545", 
                        "\u0394ppx1 \u0394ppn1 S288C 2026 (yH1053)" = "yH1053"),
            selected = isolate(plot_settings$sc_dataset %||% "2023"),
            inline = FALSE,
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.settings_contrast_mode_enabled == true",
          radioButtons(
            "settings_contrast_type",
            "Contrast Selection:",
            choices = c("2026 WT vs \u0394ppx1 \u0394ppn1" = "contrast_2026",
                        "2023 WT vs 2026 WT" = "contrast_wt"),
            selected = isolate(plot_settings$contrast_type %||% "contrast_2026"),
            inline = FALSE,
            width = "100%"
          ),
          h5(icon("chart-line"), " Contrast Display Scaling", style = "margin-top: 15px;"),
          radioButtons(
            "settings_contrast_transform",
            label = NULL,
            choices = c("None (Raw values)" = "none", "Z-score (Row-scaled)" = "zscore", "Center (Row-centered)" = "center"),
            selected = isolate(plot_settings$contrast_transform %||% "none"),
            inline = TRUE,
            width = "100%"
          )
        )
      ),
      tabsetPanel(
        id = "settings_tabs",
        type = "tabs",

        # tab 1: species identity
        tabPanel(
          "Species",
          icon = icon("palette"),
          div(
            style = "padding: 15px;",
            h5("Species Color Palette"),
            selectInput("settings_species_palette", NULL,
              choices = c(STANDARD_SPECIES_PALETTE, "Dark2", "Set1", "Set2", "Paired", "Accent", "Okabe-Ito"),
              selected = isolate(plot_settings$species_palette)
            ),
            tags$small(class = "text-muted",
              sprintf("%s uses the fixed RNAcross hues: S. cerevisiae #377EB8, C. glabrata #E41A1C, C. albicans #4DAF4A, K. lactis #FF7F00.",
                      STANDARD_SPECIES_PALETTE)),
            hr(),
            h5("Per-Species Colors"),
            p(class = "text-muted", "Click to customize individual species colors"),
            uiOutput("species_color_pickers"),
            hr(),
            h5("Per-Species Shapes"),
            uiOutput("species_shape_pickers")
          )
        ),

        # tab 1b: line aesthetics
        tabPanel(
          "Line Plots",
          icon = icon("chart-line"),
          div(
            style = "padding: 15px;",
            h5("Global Line Aesthetics"),
            p(class = "text-muted", "These settings apply across modules where line type/thickness isn't actively used for encoding."),
            sliderInput("settings_line_thickness", "Line Thickness:", min = 0.5, max = 3.0, value = isolate(plot_settings$line_thickness %||% 1), step = 0.1),
            selectInput("settings_line_type", "Line Type:", choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dot-Dash" = "dotdash", "Long Dash" = "longdash", "Two Dash" = "twodash"), selected = isolate(plot_settings$line_type %||% "solid")),
            hr(),
            h5("Fixed Y-axis Range"),
            p(class = "text-muted", "Apply a consistent Y-axis (expression) range across the Gene Group, Single Species, and Comparative line plots, on screen and in exports."),
            checkboxInput("settings_y_axis_manual", "Use fixed Y-axis range", value = isolate(isTRUE(plot_settings$y_axis_manual))),
            conditionalPanel(
              condition = "input.settings_y_axis_manual == true",
              fluidRow(
                column(6, numericInput("settings_y_axis_min", "Y min:", value = isolate(plot_settings$y_axis_min %||% 0))),
                column(6, numericInput("settings_y_axis_max", "Y max:", value = isolate(plot_settings$y_axis_max %||% 15)))
              )
            )
          )
        ),

        # tab 2: similarity
        tabPanel(
          "Similarity",
          icon = icon("search"),
          div(
            style = "padding: 15px;",
            h5("Graph Color Encoding"),
            radioButtons("settings_similarity_color", "Color represents:",
              choices = c("Species" = "species", "Gene" = "gene"),
              selected = isolate(plot_settings$encoding_similarity_color %||% "species"),
              inline = TRUE
            ),
            radioButtons("settings_similarity_secondary", "Secondary encoding:",
              choices = c("Linetype" = "linetype", "Shape" = "shape", "Linetype + Shape" = "both", "None" = "none"),
              selected = isolate(plot_settings$encoding_similarity_secondary %||% "linetype"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.settings_similarity_color == 'gene'",
              selectInput("settings_similarity_palette", "Gene color palette:",
                choices = c("Set1", "Set2", "Dark2", "Paired", "Accent", "Okabe-Ito"),
                selected = isolate(plot_settings$similarity_palette %||% "Set2")
              ),
              hr(),
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                h6("Per-Gene Colors", style = "margin: 0;"),
                actionButton("reset_sim_gene_colors", "Reset to Defaults", icon = icon("undo"), class = "btn-sm btn-outline-secondary")
              ),
              p(class = "text-muted", "Click or type hex code to customize individual gene colors"),
              uiOutput("similarity_gene_color_pickers")
            ),
            hr(),
            h6("Appearance"),
            fluidRow(
              column(4, numericInput("settings_similarity_line_width", "Line Width:", value = isolate(plot_settings$similarity_line_width %||% 2), min = 1, max = 10, step = 1)),
              column(4, numericInput("settings_similarity_marker_size", "Point Size:", value = isolate(plot_settings$similarity_marker_size %||% 6), min = 1, max = 20, step = 1)),
              column(4, numericInput("settings_similarity_opacity", "Opacity:", value = isolate(plot_settings$similarity_opacity %||% 0.6), min = 0.1, max = 1.0, step = 0.1))
            )
          )
        ),

        # tab 3: multi-gene plots
        tabPanel(
          "Multi-Gene",
          icon = icon("chart-line"),
          div(
            style = "padding: 15px;",
            h5("Line/Point Plot Encoding"),
            conditionalPanel(
              condition = "input.settings_contrast_mode_enabled == false",
              radioButtons("settings_multigene_color", "Color represents:",
                choices = c("Species" = "species", "Gene" = "gene"),
                selected = isolate(plot_settings$encoding_multigene_color),
                inline = TRUE
              ),
              radioButtons("settings_multigene_secondary", "Secondary encoding:",
                choices = c("Linetype" = "linetype", "Shape" = "shape", "Linetype + Shape" = "both", "None" = "none"),
                selected = isolate(plot_settings$encoding_multigene_secondary),
                inline = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.settings_contrast_mode_enabled == true",
              radioButtons("settings_contrast_dataset_encoding", "Dataset (WT vs Mutant) encoded by:",
                choices = c("Color" = "color", "Linetype" = "linetype", "Shape" = "shape", "Color + Shape" = "color_shape", "Linetype + Shape" = "linetype_shape"),
                selected = isolate(plot_settings$contrast_dataset_encoding %||% "color"),
                inline = TRUE
              ),
              uiOutput("ui_settings_contrast_gene_encoding")
            ),
            conditionalPanel(
              condition = "(input.settings_contrast_mode_enabled == false && input.settings_multigene_color == 'gene') || (input.settings_contrast_mode_enabled == true && typeof input.settings_contrast_gene_encoding !== 'undefined' && input.settings_contrast_gene_encoding !== null && input.settings_contrast_gene_encoding.indexOf('color') > -1)",
              selectInput("settings_gene_palette", "Gene color palette:",
                choices = c("Set1", "Set2", "Dark2", "Paired", "Accent", "Okabe-Ito"),
                selected = isolate(plot_settings$gene_palette)
              ),
              hr(),
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                h6("Per-Gene Colors", style = "margin: 0;"),
                actionButton("reset_gene_colors", "Reset to Defaults", icon = icon("undo"), class = "btn-sm btn-outline-secondary")
              ),
              p(class = "text-muted", "Click or type hex code to customize individual gene colors"),
              uiOutput("gene_color_pickers")
            ),
            hr(),
            h5("Heatmap Settings"),
            selectInput("settings_heatmap_palette", "Heatmap palette:",
              choices = c(
                "viridis", "plasma", "inferno", "magma", "cividis",
                "RdBu", "RdYlBu", "PiYG", "PRGn", "BrBG"
              ),
              selected = isolate(plot_settings$heatmap_palette)
            ),
            radioButtons("settings_heatmap_scale", "Scale type:",
              choices = c("Sequential" = "sequential", "Diverging" = "diverging"),
              selected = isolate(plot_settings$heatmap_scale_type),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.settings_heatmap_scale == 'diverging'",
              radioButtons("settings_heatmap_midpoint", "Center at:",
                choices = c("Auto" = "auto", "Zero" = "zero", "Median" = "median"),
                selected = isolate(plot_settings$heatmap_midpoint),
                inline = TRUE
              )
            ),
            checkboxInput("settings_heatmap_col_dendro", "Show column dendrogram",
              value = isolate(plot_settings$heatmap_show_col_dendro)
            ),
            hr(),
            h5("Publication Settings (ComplexHeatmap)"),
            radioButtons("settings_viz_mode", "Visualization Mode:",
              choices = c(
                "Interactive (Plotly)" = "interactive",
                "Publication (Static)" = "publication"
              ),
              selected = isolate(plot_settings$viz_mode %||% "interactive"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.settings_viz_mode == 'publication'",
              wellPanel(
                style = "background-color: #f8f9fa; padding: 10px;",
                h6("Data Processing"),
                selectInput("settings_data_transform", "Transformation:",
                  choices = c(
                    "Log2FC from baseline" = "log2fc",
                    "Z-score (by gene)" = "zscore",
                    "Z-score (baseline subtracted)" = "zscore_minus_t0",
                    "Centered log2CPM" = "centered"
                  ),
                  selected = isolate(plot_settings$data_transform %||% "log2fc")
                ),
                checkboxInput("settings_show_t0", "Show t0 (baseline) timepoint",
                  value = isolate(plot_settings$show_t0 %||% TRUE)
                ),
                selectInput("settings_time_axis", "Time Axis:",
                  choices = c(
                    "Standardized (T01-T10)" = "standardized",
                    "Raw timepoints" = "raw",
                    "Intersection only" = "intersection"
                  ),
                  selected = isolate(plot_settings$time_axis %||% "standardized")
                ),
                selectInput("settings_row_ordering", "Row Ordering:",
                  choices = c(
                    "By functional category" = "functional",
                    "By Saccharomyces cerevisiae peak" = "peak_time",
                    "Alphabetical" = "alphabetical"
                  ),
                  selected = isolate(plot_settings$row_ordering %||% "functional")
                ),
                radioButtons("settings_missing_orthologs", "Missing Orthologs:",
                  choices = c("Show as grey" = "grey", "Exclude" = "exclude"),
                  selected = isolate(plot_settings$missing_orthologs %||% "grey"),
                  inline = TRUE
                ),
                h6("Appearance"),
                fluidRow(
                  column(6, numericInput("settings_color_min", "Min Scale:",
                    value = isolate(plot_settings$color_min %||% -6)
                  )),
                  column(6, numericInput("settings_color_max", "Max Scale:",
                    value = isolate(plot_settings$color_max %||% 6)
                  ))
                ),
                hr(),
                h6("Publication Export Settings"),
                p("Configure dimensions and appearance for high-resolution export."),

                # Display Mode (Added for Compact Mode)
                radioButtons(
                  "settings_pub_mode",
                  "Display Mode:",
                  choices = c("Full (with labels)" = "full", "Compact (no labels)" = "compact"),
                  selected = if (!is.null(plot_settings$pub_mode)) plot_settings$pub_mode else "full",
                  inline = TRUE
                ),

                # Download override
                checkboxInput(
                  "settings_download_labels",
                  "Include gene labels in download (even in Compact Mode)",
                  value = if (!is.null(plot_settings$download_labels)) plot_settings$download_labels else FALSE
                ),
                h6("Export Dimensions (inches)"),
                fluidRow(
                  column(6, numericInput("settings_export_width", "Width:",
                    value = isolate(plot_settings$export_width %||% 12)
                  )),
                  column(6, numericInput("settings_export_height", "Height:",
                    value = isolate(plot_settings$export_height %||% 10)
                  ))
                )
              )
            ),
            hr(),
            h5("Ridgeline Settings"),
            selectInput("settings_ridgeline_palette", "Ridgeline palette:",
              choices = c("viridis", "plasma", "inferno", "magma", "cividis"),
              selected = isolate(plot_settings$ridgeline_palette)
            ),
            sliderInput("settings_ridgeline_alpha", "Opacity:",
              min = 0.3, max = 1, value = isolate(plot_settings$ridgeline_alpha),
              step = 0.1
            )
          )
        ),

        # tab 3: pca settings
        tabPanel(
          "PCA",
          icon = icon("project-diagram"),
          div(
            style = "padding: 15px;",
            h5("PCA Encoding"),
            selectInput("settings_pca_color", "Color represents:",
              choices = c(
                "Species" = "species", "Condition" = "condition",
                "Timepoint" = "timepoint"
              ),
              selected = isolate(plot_settings$encoding_pca_color)
            ),
            selectInput("settings_pca_shape", "Shape represents:",
              choices = c(
                "Species" = "species", "Condition" = "condition", "Replicate" = "replicate",
                "None" = "none"
              ),
              selected = isolate(plot_settings$encoding_pca_shape)
            ),
            radioButtons("settings_pca_collapse_reps", "Collapse Biological Replicates:",
              choices = c("None" = "none", "Mean" = "mean", "Median" = "median"),
              selected = isolate(plot_settings$pca_collapse_reps %||% "none"),
              inline = TRUE
            ),
            hr(),
            h5("Appearance"),
            sliderInput("settings_pca_alpha", "Point opacity:",
              min = 0.3, max = 1, value = isolate(plot_settings$pca_alpha),
              step = 0.1
            ),
            sliderInput("settings_pca_size", "Point size:",
              min = 1, max = 6, value = isolate(plot_settings$pca_point_size),
              step = 0.5
            ),
            checkboxInput("settings_pca_ellipses", "Show confidence ellipses",
              value = isolate(plot_settings$pca_show_ellipses)
            ),
            checkboxInput("settings_pca_loadings", "Show gene loadings",
              value = isolate(plot_settings$pca_show_loadings)
            ),
            hr(),
            h5("Publication Settings"),
            radioButtons("settings_pca_viz_mode", "Visualization Mode:",
              choices = c(
                "Interactive (Plotly)" = "interactive",
                "Publication (Static)" = "publication"
              ),
              selected = isolate(plot_settings$pca_viz_mode %||% "interactive"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.settings_pca_viz_mode == 'publication'",
              wellPanel(
                style = "background-color: #f8f9fa; padding: 10px;",
                checkboxInput("settings_pca_trajectories", "Show temporal trajectories (arrows)",
                  value = isolate(plot_settings$pca_trajectories %||% FALSE)
                ),
                checkboxInput("settings_pca_labels", "Show timepoint labels",
                  value = isolate(plot_settings$pca_labels %||% FALSE)
                ),
                h6("Export Dimensions (inches)"),
                fluidRow(
                  column(6, numericInput("settings_pca_export_width", "Width:",
                    value = isolate(plot_settings$pca_export_width %||% 10)
                  )),
                  column(6, numericInput("settings_pca_export_height", "Height:",
                    value = isolate(plot_settings$pca_export_height %||% 8)
                  ))
                )
              )
            )
          )
        ),

        # tab 4: presets
        tabPanel(
          "Presets",
          icon = icon("save"),
          div(
            style = "padding: 15px;",
            h5("Load Preset"),
            selectInput("settings_load_preset", NULL,
              choices = c("Select..." = "", names(isolate(plot_settings$presets)))
            ),
            actionButton("settings_apply_preset", "Apply Preset", class = "btn-primary"),
            hr(),
            h5("Save Current Settings"),
            textInput("settings_preset_name", "Preset name:"),
            actionButton("settings_save_preset", "Save as Preset", class = "btn-success"),
            hr(),
            h5("Export Settings"),
            selectInput("settings_export_format", "Default export format:",
              choices = c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"),
              selected = isolate(plot_settings$export_format)
            ),
            numericInput("settings_export_dpi", "DPI:",
              value = isolate(plot_settings$export_dpi),
              min = 72, max = 600, step = 50
            )
          )
        )
      )
    ))
  })

  # render species color pickers
  output$species_color_pickers <- renderUI({
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    
    if (isTRUE(plot_settings$contrast_mode_enabled)) {
      species_list <- species_list[names(species_list) != "sc"]
      if (plot_settings$contrast_type == "contrast_2026") {
        species_list <- c(species_list, "WT 2026" = "WT 2026", "Mutant 2026" = "Mutant 2026")
      } else {
        species_list <- c(species_list, "WT 2023" = "WT 2023", "WT 2026" = "WT 2026")
      }
    }
    
    current_colors <- plot_settings$species_colors

    picker_list <- lapply(species_list, function(sp) {
      col <- if (sp %in% names(current_colors) && !is.null(current_colors[[sp]])) {
        current_colors[[sp]]
      } else if (sp == "WT 2026") {
        "#E69F00"
      } else if (sp == "Mutant 2026") {
        "#56B4E9"
      } else if (sp == "WT 2023") {
        "#009E73"
      } else {
        "#808080"
      }
      id_suffix <- gsub("[^a-zA-Z0-9]", "_", sp)
      div(
        style = "display: inline-block; margin: 5px; width: 150px; vertical-align: top;",
        # visual swatch picker (click to choose)
        colourpicker::colourInput(
          paste0("species_color_", id_suffix),
          label = sp,
          value = col,
          showColour = "background"
        ),
        # exact hex entry: plain text field so partial values are never
        # auto-expanded; applied only once a complete hex is typed
        div(
          style = "margin-top: -8px;",
          textInput(
            paste0("species_color_hex_", id_suffix),
            label = NULL,
            value = col,
            placeholder = "#RRGGBB"
          ),
          tags$small(
            class = "text-muted",
            style = "display: block; margin-top: -6px; font-size: 10px;",
            "Hex (applies on pause)"
          )
        )
      )
    })

    do.call(tagList, picker_list)
  })

  # render species shape pickers
  output$species_shape_pickers <- renderUI({
    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    
    if (isTRUE(plot_settings$contrast_mode_enabled)) {
      species_list <- species_list[names(species_list) != "sc"]
      if (plot_settings$contrast_type == "contrast_2026") {
        species_list <- c(species_list, "WT 2026" = "WT 2026", "Mutant 2026" = "Mutant 2026")
      } else {
        species_list <- c(species_list, "WT 2023" = "WT 2023", "WT 2026" = "WT 2026")
      }
    }
    
    current_shapes <- plot_settings$species_shapes

    shape_choices <- c(
      "Circle" = 16, "Triangle" = 17, "Square" = 15,
      "Diamond" = 18, "Star" = 8, "Plus" = 3, "Cross" = 4
    )

    picker_list <- lapply(species_list, function(sp) {
      shp <- if (sp %in% names(current_shapes) && !is.null(current_shapes[[sp]])) {
        current_shapes[[sp]]
      } else if (sp == "WT 2026") {
        16
      } else if (sp == "Mutant 2026") {
        17
      } else if (sp == "WT 2023") {
        15
      } else {
        16
      }
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

  # Contrast Mode Gene Encoding UI
  output$ui_settings_contrast_gene_encoding <- renderUI({
    ds_enc <- input$settings_contrast_dataset_encoding
    if (is.null(ds_enc)) return(NULL)
    
    # Available aesthetics
    avail_color <- !grepl("color", ds_enc)
    avail_line  <- !grepl("linetype", ds_enc)
    avail_shape <- !grepl("shape", ds_enc)
    
    choices <- c("None" = "none")
    if (avail_color) choices <- c(choices, "Color" = "color")
    if (avail_line) choices <- c(choices, "Linetype" = "linetype")
    if (avail_shape) choices <- c(choices, "Shape" = "shape")
    
    if (avail_color && avail_shape) choices <- c(choices, "Color + Shape" = "color_shape")
    if (avail_line && avail_shape) choices <- c(choices, "Linetype + Shape" = "linetype_shape")
    if (avail_color && avail_line) choices <- c(choices, "Color + Linetype" = "color_linetype")
    if (avail_color && avail_line && avail_shape) choices <- c(choices, "Color + Linetype + Shape" = "color_linetype_shape")
    
    # Keep current selection if possible
    curr_gene <- isolate(plot_settings$contrast_gene_encoding) %||% "linetype"
    if (!(curr_gene %in% choices)) {
      # Fallback
      if (avail_line) curr_gene <- "linetype"
      else if (avail_shape) curr_gene <- "shape"
      else if (avail_color) curr_gene <- "color"
      else curr_gene <- "none"
    }
    
    radioButtons("settings_contrast_gene_encoding", "Gene encoded by:",
                 choices = choices,
                 selected = curr_gene,
                 inline = TRUE)
  })

  # Gene Color Pickers Implementation
  output$gene_color_pickers <- renderUI({
    req(gene_group_state$ready, gene_group_state$data)

    if (isTRUE(gene_group_state$is_multi_species)) {
      genes <- unique(gene_group_state$data$GeneLabel)
    } else {
      genes <- unique(gene_group_state$data$Gene)
    }
    if (length(genes) == 0) return(NULL)
    genes <- stringr::str_sort(genes, numeric = TRUE)

    # Reactive read (mirrors species picker). No isolate() and no write-back here,
    # so the rendered swatches always reflect the current saved colours and there
    # is no stale frame to flicker on reopen. Defaults are filled elsewhere
    # (see the gene_group_state$data observer).
    current_colors <- plot_settings$gene_colors
    defaults <- get_palette_colors(plot_settings$gene_palette %||% "Set2", length(genes))
    names(defaults) <- genes

    picker_list <- lapply(genes, function(g) {
      col <- if (!is.null(current_colors[[g]])) current_colors[[g]] else defaults[[g]]
      div(
        style = "display: inline-block; margin: 5px; width: 150px;",
        colourpicker::colourInput(
          paste0("gene_color_", gsub("[^a-zA-Z0-9]", "_", g)),
          label = g, value = col, showColour = "both"
        )
      )
    })
    do.call(tagList, picker_list)
  })

  # Keep live while the modal/conditionalPanel is hidden, so the input$show_settings
  # bump recomputes it with the current plot_settings$gene_colors on reopen instead
  # of re-serving the stale first (default) render.
  outputOptions(output, "gene_color_pickers", suspendWhenHidden = FALSE)

  # Handle reset gene colors
  observeEvent(input$reset_gene_colors, {
    req(gene_group_state$ready, gene_group_state$data)
    if (isTRUE(gene_group_state$is_multi_species)) {
      genes <- unique(gene_group_state$data$GeneLabel)
    } else {
      genes <- unique(gene_group_state$data$Gene)
    }
    if (length(genes) > 0) {
      sorted_genes <- stringr::str_sort(genes, numeric = TRUE)
      defaults <- get_palette_colors(plot_settings$gene_palette %||% "Set2", length(sorted_genes))
      names(defaults) <- sorted_genes
      plot_settings$gene_colors <- as.list(defaults)
    }
  })

  # Similarity Gene Color Pickers Implementation
  output$similarity_gene_color_pickers <- renderUI({
    req(similarity_results())
    top_genes <- similarity_results()$table %>% group_by(Target) %>% slice_head(n = input$similarity_top_matches[1]) %>% pull(gene_id)
    matches <- similarity_results()$plot_data %>% filter(type == "match", gene_id %in% top_genes)
    unique_labels <- unique(paste0(matches$label, " (", matches$target_species, ")"))
    if (length(unique_labels) == 0) return(NULL)
    unique_labels <- stringr::str_sort(unique_labels, numeric = TRUE)

    current_colors <- plot_settings$similarity_gene_colors
    defaults <- get_palette_colors(plot_settings$similarity_palette %||% "Dark2", length(unique_labels))
    names(defaults) <- unique_labels

    picker_list <- lapply(unique_labels, function(lbl) {
      col <- if (!is.null(current_colors[[lbl]])) current_colors[[lbl]] else defaults[[lbl]]
      div(
        style = "display: block; margin: 5px; width: 100%;",
        colourpicker::colourInput(
          paste0("sim_gene_color_", gsub("[^a-zA-Z0-9]", "_", lbl)),
          label = lbl, value = col, showColour = "both"
        )
      )
    })
    do.call(tagList, picker_list)
  })
  outputOptions(output, "similarity_gene_color_pickers", suspendWhenHidden = FALSE)

  # Handle reset similarity gene colors
  observeEvent(input$reset_sim_gene_colors, {
    req(similarity_results())
    top_genes <- similarity_results()$table %>% group_by(Target) %>% slice_head(n = input$similarity_top_matches[1]) %>% pull(gene_id)
    matches <- similarity_results()$plot_data %>% filter(type == "match", gene_id %in% top_genes)
    unique_labels <- unique(paste0(matches$label, " (", matches$target_species, ")"))
    if (length(unique_labels) > 0) {
      sorted_labels <- stringr::str_sort(unique_labels, numeric = TRUE)
      defaults <- get_palette_colors(plot_settings$similarity_palette %||% "Dark2", length(sorted_labels))
      names(defaults) <- sorted_labels
      plot_settings$similarity_gene_colors <- as.list(defaults)
    }
  })

  # Update gene colors when palette changes
  observeEvent(input$settings_gene_palette, {
    req(gene_group_state$ready, gene_group_state$data)

    # The settings modal is rebuilt on every open, which re-initializes this
    # selectInput and re-fires this observer (NULL -> saved value). Without this
    # guard we would regenerate gene_colors from the palette on every reopen,
    # discarding the user's custom per-gene colors. Only regenerate when the
    # palette selection has genuinely changed.
    if (identical(input$settings_gene_palette, plot_settings$gene_palette)) {
      return()
    }

    plot_settings$gene_palette <- input$settings_gene_palette
    plot_settings$updating_colors_from_palette <- TRUE

    if (isTRUE(gene_group_state$is_multi_species)) {
      genes <- unique(gene_group_state$data$GeneLabel)
    } else {
      genes <- unique(gene_group_state$data$Gene)
    }
    if (length(genes) == 0) {
      return()
    }
    genes <- stringr::str_sort(genes, numeric = TRUE)

    new_colors <- get_palette_colors(input$settings_gene_palette, length(genes))
    names(new_colors) <- genes
    as_list <- as.list(new_colors)
    plot_settings$gene_colors <- as_list

    for (g in genes) {
      id_suffix <- gsub("[^a-zA-Z0-9]", "_", g)
      input_id <- paste0("gene_color_", id_suffix)
      colourpicker::updateColourInput(session, input_id, value = as_list[[g]])
    }

    later::later(function() {
      plot_settings$updating_colors_from_palette <- FALSE
    }, delay = 0.3)
  })


  # update species colors when palette changes - regenerates all colors from palette
  observeEvent(input$settings_species_palette, {
    req(plot_settings$initialized)

    # Only regenerate colors when the species palette selection has genuinely changed
    if (identical(input$settings_species_palette, plot_settings$species_palette)) {
      return()
    }

    config <- current_species_config()
    species_list <- sapply(config, function(x) x$short)
    plot_settings$species_palette <- input$settings_species_palette
    new_colors <- derive_species_colors(species_list, input$settings_species_palette, NULL, config)
    plot_settings$species_colors <- new_colors
    # flag to prevent individual picker observer from triggering during palette update
    plot_settings$updating_colors_from_palette <- TRUE
    # update colourInput + hex text UI elements to reflect new palette
    for (sp in species_list) {
      id_suffix <- gsub("[^a-zA-Z0-9]", "_", sp)
      if (sp %in% names(new_colors)) {
        colourpicker::updateColourInput(session, paste0("species_color_", id_suffix), value = new_colors[[sp]])
        updateTextInput(session, paste0("species_color_hex_", id_suffix), value = new_colors[[sp]])
      }
    }
    # reset flag after client round-trip completes
    later::later(function() {
      plot_settings$updating_colors_from_palette <- FALSE
    }, delay = 0.3)
  })

  observeEvent(input$settings_multigene_color, {
    plot_settings$encoding_multigene_color <- input$settings_multigene_color
  })

  observeEvent(input$settings_contrast_dataset_encoding, {
    plot_settings$contrast_dataset_encoding <- input$settings_contrast_dataset_encoding
  })

  observeEvent(input$settings_contrast_gene_encoding, {
    plot_settings$contrast_gene_encoding <- input$settings_contrast_gene_encoding
  })

  observeEvent(input$settings_similarity_color, {
    plot_settings$encoding_similarity_color <- input$settings_similarity_color
  })

  observeEvent(input$settings_similarity_secondary, {
    plot_settings$encoding_similarity_secondary <- input$settings_similarity_secondary
  })

  observeEvent(input$settings_similarity_palette, {
    # Only regenerate colors when the similarity palette selection has genuinely changed
    if (identical(input$settings_similarity_palette, plot_settings$similarity_palette)) {
      return()
    }

    plot_settings$similarity_palette <- input$settings_similarity_palette
    
    # Update individual pickers
    req(similarity_results())
    
    top_genes <- similarity_results()$table %>% group_by(Target) %>% slice_head(n = input$similarity_top_matches[1]) %>% pull(gene_id)
    matches <- similarity_results()$plot_data %>% filter(type == "match", gene_id %in% top_genes)
    unique_labels <- unique(paste0(matches$label, " (", matches$target_species, ")"))
    if (length(unique_labels) == 0) return()
    unique_labels <- stringr::str_sort(unique_labels, numeric = TRUE)
    
    new_colors <- get_palette_colors(input$settings_similarity_palette, length(unique_labels))
    names(new_colors) <- unique_labels
    
    plot_settings$similarity_gene_colors <- as.list(new_colors)
    plot_settings$updating_similarity_colors_from_palette <- TRUE
    
    for (lbl in unique_labels) {
      input_id <- paste0("sim_gene_color_", gsub("[^a-zA-Z0-9]", "_", lbl))
      colourpicker::updateColourInput(session, input_id, value = new_colors[[lbl]])
    }
    later::later(function() {
      plot_settings$updating_similarity_colors_from_palette <- FALSE
    }, delay = 0.3)
  })

  observeEvent(input$settings_similarity_line_width, {
    plot_settings$similarity_line_width <- input$settings_similarity_line_width
  })

  observeEvent(input$settings_similarity_marker_size, {
    plot_settings$similarity_marker_size <- input$settings_similarity_marker_size
  })

  observeEvent(input$settings_similarity_opacity, {
    plot_settings$similarity_opacity <- input$settings_similarity_opacity
  })

  observeEvent(input$settings_multigene_secondary, {
    plot_settings$encoding_multigene_secondary <- input$settings_multigene_secondary
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

  observeEvent(input$settings_pca_collapse_reps, {
    plot_settings$pca_collapse_reps <- input$settings_pca_collapse_reps
  })

  observeEvent(input$settings_min_scale, {
    plot_settings$min_scale <- input$settings_min_scale
  })
  observeEvent(input$settings_max_scale, {
    plot_settings$max_scale <- input$settings_max_scale
  })
  observeEvent(input$settings_pub_mode, {
    plot_settings$pub_mode <- input$settings_pub_mode
  })
  observeEvent(input$settings_download_labels, {
    plot_settings$download_labels <- input$settings_download_labels
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

  observeEvent(input$settings_pca_viz_mode, {
    plot_settings$pca_viz_mode <- input$settings_pca_viz_mode
  })
  
  observeEvent(input$settings_pca_trajectories, {
    plot_settings$pca_trajectories <- input$settings_pca_trajectories
  })
  
  observeEvent(input$settings_pca_labels, {
    plot_settings$pca_labels <- input$settings_pca_labels
  })
  
  observeEvent(input$settings_pca_export_width, {
    plot_settings$pca_export_width <- input$settings_pca_export_width
  })
  
  observeEvent(input$settings_pca_export_height, {
    plot_settings$pca_export_height <- input$settings_pca_export_height
  })

  observeEvent(input$settings_export_format, {
    plot_settings$export_format <- input$settings_export_format
  })

  observeEvent(input$settings_export_dpi, {
    plot_settings$export_dpi <- input$settings_export_dpi
  })

  # Global Data Settings
  # offer a choice only when the data holds more than one matrix
  output$settings_transform_ui <- renderUI({
    avail <- available_transforms_current()
    if (length(avail) == 0) {
      return(div(class = "text-muted small", "No expression matrix loaded."))
    }
    if (length(avail) == 1) {
      return(div(
        class = "text-muted small",
        paste0("Plotting ", expression_axis_label(), " (the only matrix in this dataset).")
      ))
    }
    tagList(
      radioButtons(
        "settings_global_transform",
        label = NULL,
        choices = setNames(avail, vapply(avail, transform_choice_label, character(1))),
        selected = active_transform(),
        inline = TRUE,
        width = "100%"
      ),
      div(class = "text-muted small", paste("Currently plotting", expression_axis_label()))
    )
  })

  observeEvent(input$settings_global_transform, {
    plot_settings$global_transform <- input$settings_global_transform
  })
  
  observeEvent(input$settings_sc_dataset, {
    plot_settings$sc_dataset <- input$settings_sc_dataset
  })
  observeEvent(input$settings_contrast_mode_enabled, {
    plot_settings$contrast_mode_enabled <- input$settings_contrast_mode_enabled
  })
  observeEvent(input$settings_contrast_type, {
    plot_settings$contrast_type <- input$settings_contrast_type
  })

  observeEvent(input$settings_contrast_transform, {
    plot_settings$contrast_transform <- input$settings_contrast_transform
  })

  # Line aesthetics observers
  observeEvent(input$settings_line_thickness, {
    plot_settings$line_thickness <- input$settings_line_thickness
  })

  observeEvent(input$settings_line_type, {
    plot_settings$line_type <- input$settings_line_type
  })

  observeEvent(input$settings_y_axis_manual, {
    plot_settings$y_axis_manual <- isTRUE(input$settings_y_axis_manual)
  })

  observeEvent(input$settings_y_axis_min, {
    plot_settings$y_axis_min <- input$settings_y_axis_min
  })

  observeEvent(input$settings_y_axis_max, {
    plot_settings$y_axis_max <- input$settings_y_axis_max
  })

  # Publication Settings Observers
  observeEvent(input$settings_viz_mode, {
    plot_settings$viz_mode <- input$settings_viz_mode
  })

  observeEvent(input$settings_data_transform, {
    plot_settings$data_transform <- input$settings_data_transform
  })

  observeEvent(input$settings_time_axis, {
    plot_settings$time_axis <- input$settings_time_axis
  })

  observeEvent(input$settings_row_ordering, {
    plot_settings$row_ordering <- input$settings_row_ordering
  })

  observeEvent(input$settings_missing_orthologs, {
    plot_settings$missing_orthologs <- input$settings_missing_orthologs
  })

  observeEvent(input$settings_color_min, {
    plot_settings$color_min <- input$settings_color_min
  })

  observeEvent(input$settings_color_max, {
    plot_settings$color_max <- input$settings_color_max
  })

  observeEvent(input$settings_export_width, {
    plot_settings$export_width <- input$settings_export_width
  })

  observeEvent(input$settings_export_height, {
    plot_settings$export_height <- input$settings_export_height
  })

  species_color_obs_manager <- reactiveValues(observers = list())
  species_shape_obs_manager <- reactiveValues(observers = list())

  # Efficiently manage species color and shape observers
  observe({
    req(plot_settings$initialized)
    
    # We depend on contrast settings which dictates the species list
    contrast_enabled <- plot_settings$contrast_mode_enabled
    contrast_type <- plot_settings$contrast_type
    config <- current_species_config()
    
    isolate({
      #vapply, not sapply: an entry with no $short would yield a list and break the loop
      species_list <- vapply(config, function(x) {
        if (is.null(x$short) || !nzchar(x$short[1])) NA_character_ else as.character(x$short)[1]
      }, character(1))
      species_list <- species_list[!is.na(species_list)]
      if (isTRUE(contrast_enabled)) {
        species_list <- species_list[names(species_list) != "sc"]
        if (contrast_type == "contrast_2026") {
          species_list <- c(species_list, "WT 2026" = "WT 2026", "Mutant 2026" = "Mutant 2026")
        } else {
          species_list <- c(species_list, "WT 2023" = "WT 2023", "WT 2026" = "WT 2026")
        }
      }

      # Clean up old observers
      lapply(species_color_obs_manager$observers, function(o) o$destroy())
      lapply(species_shape_obs_manager$observers, function(o) o$destroy())
      
      #NULL-safe short-name index; sapply() returns a list if any entry lacks $short
      config_shorts <- vapply(config, function(x) if (is.null(x$short)) NA_character_ else as.character(x$short)[1], character(1))
      species_color_obs_manager$observers <- do.call(c, lapply(species_list, function(sp) {
        id_suffix <- gsub("[^a-zA-Z0-9]", "_", sp)
        color_id <- paste0("species_color_", id_suffix)
        hex_id <- paste0("species_color_hex_", id_suffix)

        # resolve the full binomial key once for this species
        sp_full <- NULL
        if (sp %in% config_shorts) {
          sp_full <- config[[names(config)[match(sp, config_shorts)]]]$name
        }

        apply_species_color <- function(val) {
          plot_settings$species_colors[[sp]] <- val
          # also update full name key if it maps to a base species
          if (!is.null(sp_full)) {
            plot_settings$species_colors[[sp_full]] <- val
          }
        }

        # visual swatch picker -> settings, mirrored into the hex text field
        obs_color <- observeEvent(input[[color_id]], {
          val <- input[[color_id]]
          if (!is.null(val) && val != "" && !isTRUE(plot_settings$updating_colors_from_palette)) {
            apply_species_color(val)
            updateTextInput(session, hex_id, value = val)
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        # typed hex -> settings, debounced so it commits only after typing
        # settles, and only for a complete valid hex (never a half-typed value)
        hex_debounced <- debounce(reactive(input[[hex_id]]), 500)
        obs_hex <- observeEvent(hex_debounced(), {
          if (isTRUE(plot_settings$updating_colors_from_palette)) return()
          norm <- normalize_hex_color(hex_debounced())
          if (is.null(norm)) return()
          apply_species_color(norm)
          # sync the swatch, and canonicalize the field (e.g. #e41 -> #EE4411)
          colourpicker::updateColourInput(session, color_id, value = norm)
          if (!identical(norm, trimws(hex_debounced() %||% ""))) {
            updateTextInput(session, hex_id, value = norm)
          }
        }, ignoreInit = TRUE, ignoreNULL = FALSE)

        list(obs_color, obs_hex)
      }))

      species_shape_obs_manager$observers <- lapply(species_list, function(sp) {
        input_id <- paste0("species_shape_", gsub("[^a-zA-Z0-9]", "_", sp))
        observeEvent(input[[input_id]], {
          val <- input[[input_id]]
          if (!is.null(val)) {
            plot_settings$species_shapes[[sp]] <- as.integer(val)
            # also update full name key if it maps to a base species
            sp_full <- NULL
            if (sp %in% config_shorts) {
              sp_full <- config[[names(config)[match(sp, config_shorts)]]]$name
            }
            if (!is.null(sp_full)) {
              plot_settings$species_shapes[[sp_full]] <- as.integer(val)
            }
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
  })

  # apply preset
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

  # save preset
  observeEvent(input$settings_save_preset, {
    req(input$settings_preset_name != "")
    preset_name <- input$settings_preset_name
    current <- reactiveValuesToList(plot_settings)
    current$name <- preset_name
    current$presets <- NULL
    current$initialized <- NULL
    plot_settings$presets[[preset_name]] <- current
    showNotification(paste("Saved preset:", preset_name), type = "message")
    updateTextInput(session, "settings_preset_name", value = "")
    updateSelectInput(session, "settings_load_preset",
      choices = c("Select..." = "", names(plot_settings$presets))
    )
  })

  # reset to defaults
  observeEvent(input$settings_reset, {
    config <- current_species_config()
    defaults <- generate_default_settings(config)
    for (key in names(defaults)) {
      plot_settings[[key]] <- defaults[[key]]
    }
    showNotification("Settings reset to defaults", type = "message")
    removeModal()
  })

  # species data cache with composite key
  species_data_cache <- new.env()

  get_species_data <- function(species_id, force_no_contrast = FALSE) {
    if (is.null(species_id) || length(species_id) == 0) {
      return(NULL)
    }
    config <- current_species_config()
    current_source <- data_source()

    current_data <- if (current_source == "custom" && !is.null(upload_state$custom_all_species_data)) {
      upload_state$custom_all_species_data
    } else {
      all_species_data
    }

    # composite cache key includes data source to prevent stale data
    if (species_id == "sc") {
      if (isTRUE(plot_settings$contrast_mode_enabled)) {
        if (!force_no_contrast) {
          sc_dataset_setting <- paste0("contrast_", plot_settings$contrast_type %||% "contrast_2026")
        } else {
          sc_dataset_setting <- "yH545"
        }
      } else {
        sc_dataset_setting <- plot_settings$sc_dataset %||% "2023"
      }
    } else {
      sc_dataset_setting <- "NA"
    }
    
    cache_key <- paste(species_id, current_source, sc_dataset_setting, sep = "_")

    if (!exists(cache_key, envir = species_data_cache)) {
      if (species_id == "sc") {
        base_id <- "sc"
        if (!base_id %in% names(current_data)) return(NULL)
        
        sp_config <- config[[species_id]]
        
        data <- list(
          species_name = if (!is.null(sp_config$short)) sp_config$short else species_id
        )

        contrast_mode <- isTRUE(plot_settings$contrast_mode_enabled) && !force_no_contrast
        
        if (isTRUE(plot_settings$contrast_mode_enabled) && force_no_contrast) {
          sc_dataset_choice <- "yH545"
        } else {
          sc_dataset_choice <- plot_settings$sc_dataset %||% "2023"
        }
        
        contrast_type <- plot_settings$contrast_type %||% "contrast_2026"
        
        if (contrast_mode) {
          if (contrast_type == "contrast_2026") {
            # 2026 WT (yH545) vs ppx1d ppn1d KO (yH1053), each normalized on its own
            info_ko <- current_data$sc$sc_sample_info_KO
            info_ko <- info_ko[info_ko$Condition == "noPi", , drop = FALSE]
            info_ko$Contrast_Series <- "Mutant 2026"

            info_wt <- current_data$sc$sc_sample_info
            info_wt <- info_wt[info_wt$Condition == "noPi", , drop = FALSE]
            info_wt$Contrast_Series <- "WT 2026"

            data$sample_info <- as_tibble(rbind(info_ko, info_wt))

            lcpm_ko <- current_data$sc$sc_lcpm_KO[, info_ko$Sample, drop = FALSE]
            rlog_ko <- current_data$sc$sc_rlog_KO[, info_ko$Sample, drop = FALSE]
            lcpm_wt <- current_data$sc$sc_lcpm[, info_wt$Sample, drop = FALSE]
            rlog_wt <- current_data$sc$sc_rlog[, info_wt$Sample, drop = FALSE]

            common_lcpm <- intersect(rownames(lcpm_ko), rownames(lcpm_wt))
            common_rlog <- intersect(rownames(rlog_ko), rownames(rlog_wt))

            data$lcpm <- cbind(lcpm_ko[common_lcpm, ], lcpm_wt[common_lcpm, ])
            data$rlog <- cbind(rlog_ko[common_rlog, ], rlog_wt[common_rlog, ])
            data$anno <- current_data$sc$sc_anno

          } else if (contrast_type == "contrast_wt") {
            # 2023 WT vs 2026 WT
            info_2023 <- current_data$sc$sc_sample_info_2023
            info_2023$Contrast_Series <- "WT 2023"

            info_2026 <- current_data$sc$sc_sample_info
            info_2026 <- info_2026[info_2026$Condition == "noPi", , drop = FALSE]
            info_2026$Contrast_Series <- "WT 2026"

            # Align columns for rbind
            common_cols <- intersect(names(info_2023), names(info_2026))
            info_2023_sub <- info_2023[, common_cols, drop = FALSE]
            info_2026_sub <- info_2026[, common_cols, drop = FALSE]

            data$sample_info <- as_tibble(rbind(info_2023_sub, info_2026_sub))

            lcpm_2023 <- current_data$sc$sc_lcpm_2023
            rlog_2023 <- current_data$sc$sc_rlog_2023
            lcpm_2026 <- current_data$sc$sc_lcpm[, info_2026$Sample, drop = FALSE]
            rlog_2026 <- current_data$sc$sc_rlog[, info_2026$Sample, drop = FALSE]

            common_lcpm <- intersect(rownames(lcpm_2023), rownames(lcpm_2026))
            common_rlog <- intersect(rownames(rlog_2023), rownames(rlog_2026))

            data$lcpm <- cbind(lcpm_2023[common_lcpm, ], lcpm_2026[common_lcpm, ])
            data$rlog <- cbind(rlog_2023[common_rlog, ], rlog_2026[common_rlog, ])
            data$anno <- current_data$sc$sc_anno
          }
        } else {
          # Standard Single Dataset Mode
          if (sc_dataset_choice == "2023") {
            data$anno <- current_data$sc$sc_anno_2023
            data$sample_info <- as_tibble(current_data$sc$sc_sample_info_2023)
            data$lcpm <- current_data$sc$sc_lcpm_2023
            data$rlog <- current_data$sc$sc_rlog_2023
          } else {
            # yH545 (WT) is unsuffixed, yH1053 (ppx1d ppn1d KO) carries the _KO suffix
            suffix <- if (sc_dataset_choice == "yH1053") "_KO" else ""

            full_sample_info <- current_data$sc[[paste0("sc_sample_info", suffix)]]
            subset_info <- full_sample_info[full_sample_info$Condition == "noPi", , drop = FALSE]
            data$sample_info <- as_tibble(subset_info)

            keep_samples <- data$sample_info$Sample
            data$anno <- current_data$sc[[paste0("sc_anno", suffix)]]
            data$lcpm <- current_data$sc[[paste0("sc_lcpm", suffix)]][, keep_samples, drop = FALSE]
            data$rlog <- current_data$sc[[paste0("sc_rlog", suffix)]][, keep_samples, drop = FALSE]
          }
        }
        
        data$sc_anno <- data$anno
        data$sc_lcpm <- data$lcpm
        data$sc_sample_info <- data$sample_info
        data$sc_rlog <- data$rlog
        
      } else if (species_id %in% names(current_data)) {
        sp_config <- config[[species_id]]
        
        if (!is.null(current_data[[species_id]]$lcpm) || !is.null(current_data[[species_id]]$rlog)) {
          data <- current_data[[species_id]]
          
          if (is.null(data$species_name)) {
            data$species_name <- if (!is.null(sp_config$short)) sp_config$short else species_id
          }
          
          for (key in names(data)) {
            prefixed_key <- paste0(species_id, "_", key)
            if (is.null(data[[prefixed_key]])) {
              data[[prefixed_key]] <- data[[key]]
            }
          }
        } else {
          data <- list(
            species_name = if (!is.null(sp_config$short)) sp_config$short else species_id
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
      } else {
        return(NULL)
      }

      # verify expression matrix exists
      has_expr_data <- any(sapply(names(data), function(nm) {
        obj <- data[[nm]]
        is.matrix(obj) || (is.data.frame(obj) && ncol(obj) > 5)
      }))

      if (!has_expr_data) {
        warning(paste("No expression data found for species:", species_id))
        return(NULL)
      }

      assign(cache_key, data, envir = species_data_cache)
    }
    get(cache_key, envir = species_data_cache)
  }

  # transforms the loaded data actually carries, across the active species
  available_transforms_current <- reactive({
    config <- current_species_config()
    tts <- unlist(lapply(names(config), function(sp) {
      available_transforms(sp, get_species_data(sp))
    }))
    if (is.null(tts)) character(0) else KNOWN_TRANSFORMS[KNOWN_TRANSFORMS %in% tts]
  })

  # an explicit pick only when the data offers it, else whatever the data has
  active_transform <- reactive({
    avail <- available_transforms_current()
    chosen <- plot_settings$global_transform
    if (!is.null(chosen) && chosen %in% avail) chosen else if (length(avail)) avail[[1]] else NULL
  })

  # axis label follows the loaded matrices rather than a fixed pipeline name
  expression_axis_label <- reactive({
    tt <- active_transform()
    m <- NULL
    for (sp in names(current_species_config())) {
      m <- get_expression_matrix(sp, tt, get_species_data(sp))
      if (!is.null(m)) break
    }
    get_expression_label(tt, m)
  })

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
      species_data <- get_species_data(species_id, force_no_contrast = TRUE)
      result <- query_gene_flexible(gene, species_data, current_data)
      if (!is.null(result) && result$source != "none") {
        found_in_species <- species_id
        query_results$combined <- result
        break
      }
    }

    if (!is.null(found_in_species)) {
      combined_source <- query_results$combined$source

      if (!is.null(combined_source) && combined_source == "gene_lookup_no_orthogroup") {
        shinyjs::hide("combined_orthogroup_container")

        container_selector <- "#combined_orthogroup_selection_wrapper"
        removeUI(selector = paste0(container_selector, " > *"), multiple = TRUE, immediate = TRUE)
        insertUI(
          selector = container_selector,
          where = "afterBegin",
          ui = div(
            class = "alert alert-warning mt-3",
            icon("exclamation-circle"),
            strong(paste(gene, "is not assigned to an orthogroup.")),
            br(),
            "Cross-species comparison requires orthology data.",
            br(), br(),
            actionLink("combined_goto_single_species", "Go to Single Species View",
              class = "alert-link"
            )
          ),
          immediate = TRUE
        )

        shinyjs::show("combined_orthogroup_container")
        waiter_hide()
        return()
      }

      if (!is.null(combined_source) && combined_source == "synteny_aided") {
        container_selector <- "#combined_orthogroup_selection_wrapper"
        removeUI(selector = paste0(container_selector, " > *"), multiple = TRUE, immediate = TRUE)
        insertUI(
          selector = container_selector,
          where = "afterBegin",
          ui = div(
            class = "alert alert-info py-2 mb-3",
            icon("link"),
            " Orthology based on synteny (YGOB/CGOB), not OrthoFinder."
          ),
          immediate = TRUE
        )
      }

      shinyjs::show("combined_orthogroup_container")

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
                if (nrow(genes_df) > 1) {
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
              observeEvent(input[[paste0("combined_", species_id, "_selection")]],
                {
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
                },
                ignoreInit = TRUE,
                ignoreNULL = FALSE
              )
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

  observeEvent(input$combined_goto_single_species, {
    req(global_query_state$query_result)
    result <- global_query_state$query_result
    species_list <- names(result$genes_by_species)
    if (length(species_list) > 0) {
      first_species <- species_list[1]
      updateTabsetPanel(session, "nav", selected = "species_analysis_container")
      shinyjs::delay(100, {
        updateTabsetPanel(session, "species_tabs", selected = first_species)
        updateTextInput(session, paste0(first_species, "_genename"),
          value = global_query_state$current_query
        )
        shinyjs::delay(100, {
          shinyjs::click(paste0(first_species, "_search_button"))
        })
      })
    }
  })

  # track created observers for cleanup
  if (is.null(session$userData$created_species_observers)) {
    session$userData$created_species_observers <- character(0)
  }

  # observers dynamically based on current configuration
  observe({
    config <- current_species_config()
    current_species <- names(config)

    # cleanup observers for removed species
    existing_observers <- session$userData$created_species_observers
    removed_species <- setdiff(existing_observers, current_species)

    for (sp_id in removed_species) {
      # destroy observers for removed species
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

    # create observers for each species
    lapply(current_species, function(species_id) {
      obs_search_id <- paste0("obs_search_", species_id)
      obs_plot_id <- paste0("obs_plot_", species_id)
      obs_download_id <- paste0("obs_download_", species_id)

      if (!exists(obs_search_id, envir = session$userData)) {
        # search button handler
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
              paste(
                "Gene", gene_query, "not found in", config[[species_id]]$name,
                "but found in other species"
              ),
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
                  dom = "tp",
                  scrollX = TRUE
                ),
                colnames = c("Gene ID", "Gene Name"),
                rownames = FALSE
              )
            }
          })

          output[[paste0(species_id, "_orthogroup_results")]] <- renderDT({
            if (!is.null(gene_result$source) && gene_result$source == "gene_lookup_no_orthogroup") {
              return(NULL)
            }

            config <- current_species_config()

            ortho_list <- lapply(names(gene_result$genes_by_species), function(sp) {
              sp_data <- gene_result$genes_by_species[[sp]]
              if (nrow(sp_data) == 0) {
                return(NULL)
              }
              sp_data$Species <- config[[sp]]$short
              sp_data$Current <- (sp == species_id)
              sp_data[, c("Species", "gene_id", "gene_name", "Current")]
            })

            ortho_data <- rbindlist(Filter(Negate(is.null), ortho_list), fill = TRUE)

            if (nrow(ortho_data) > 0) {
              ortho_data <- ortho_data[order(ortho_data$Current, decreasing = TRUE), ]

              cap_text <- if (!is.null(gene_result$source) && gene_result$source == "synteny_aided") {
                "Synteny-aided orthologs (YGOB/CGOB)"
              } else {
                NULL
              }

              dt <- datatable(
                ortho_data[, c("Species", "gene_id", "gene_name")],
                options = list(
                  pageLength = 10,
                  dom = "tp",
                  scrollX = TRUE
                ),
                colnames = c("Species", "Gene ID", "Gene Name"),
                rownames = FALSE,
                caption = cap_text
              ) %>%
                formatStyle("Species", fontStyle = "italic")

              if (any(ortho_data$Current)) {
                dt <- dt %>% formatStyle(
                  columns = 1:3,
                  target = "row",
                  backgroundColor = styleRow(
                    which(ortho_data$Current),
                    if (is_dark()) "#3a4a5a" else "#e6f3ff"
                  )
                )
              }

              return(dt)
            }
          })

          waiter_hide()
        })
      }

      # plot button handler - stores state for reactive rendering
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

        # species gene plot render - reactive to plot_state and plot_settings
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

            p <- create_gene_plot(
              lc = get_expression_matrix(sp_id, active_transform(), species_data),
              gene = sp_state$gene,
              sample_info = species_data$sample_info,
              species_name = config[[sp_id]]$short,
              is_dark_mode = is_dark(),
              species_colors = species_colors_dynamic(),
              transform_type = active_transform(),
              plot_settings = current_settings,
              study_design = current_study_design()
            )
            apply_y_axis_range(p, plot_settings, base_rev = paste0(sp_id, "_gene_plot"))
          })

          output[[paste0(sp_id, "_gene_info")]] <- renderText({
            sp_state <- plot_state$species_plots[[sp_id]]
            if (is.null(sp_state) || !isTRUE(sp_state$ready)) {
              return("")
            }

            species_data <- get_species_data(sp_id)
            gene_info <- species_data$anno[species_data$anno$GeneID == sp_state$gene, ]
            if (nrow(gene_info) > 0) {
              info <- paste("Gene ID:", sp_state$gene, "\nGene Name:", gene_info$GeneName[1])
              #Chr is optional; skip the line rather than printing an empty one
              if (!is.null(gene_info$Chr)) info <- paste0(info, "\nChromosome: ", gene_info$Chr[1])
              info
            } else {
              paste("Gene ID:", sp_state$gene)
            }
          })
        })
      }

      # Export modal trigger
      obs_modal_id <- paste0("obs_modal_", species_id)
      if (!exists(obs_modal_id, envir = session$userData)) {
        local({
          local_sp <- species_id
          observeEvent(input[[paste0(local_sp, "_export_btn")]], {
            config <- current_species_config()
            sp_label <- if (local_sp %in% names(config)) config[[local_sp]]$short else local_sp
            show_plot_export_modal(
              paste0(local_sp, "_download"), paste0("Export ", sp_label, " Plot"),
              formats = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg")
            )
          })
        })
        session$userData[[obs_modal_id]] <- TRUE
      }

      # Export confirm handler (client-side plotly export)
      obs_confirm_id <- paste0("obs_confirm_", species_id)
      if (!exists(obs_confirm_id, envir = session$userData)) {
        local({
          local_sp <- species_id
          observeEvent(input[[paste0(local_sp, "_download_confirm")]], {
            fmt <- input[[paste0(local_sp, "_download_format")]]
            w <- input[[paste0(local_sp, "_download_width")]]
            h <- input[[paste0(local_sp, "_download_height")]]
            dpi_val <- input[[paste0(local_sp, "_download_dpi")]]
            if (is.null(fmt)) fmt <- "png"
            if (is.null(w)) w <- 10
            if (is.null(h)) h <- 8
            if (is.null(dpi_val)) dpi_val <- 300

            config <- current_species_config()
            sp_short <- if (local_sp %in% names(config)) config[[local_sp]]$short else local_sp

            session$sendCustomMessage("plotly_export", list(
              plotId = paste0(local_sp, "_gene_plot"),
              format = fmt,
              width = round(w * dpi_val),
              height = round(h * dpi_val),
              filename = paste0(sp_short, "_gene_expression_", Sys.Date())
            ))
            removeModal()
          })
        })
        session$userData[[obs_confirm_id]] <- TRUE
      }
    })
  })

  # download handler for orthology matrix
  output$download_orthology_matrix <- downloadHandler(
    filename = function() {
      paste("HOG_expression_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      waiter_show(html = loading_screen)

      tryCatch(
        {
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
        },
        error = function(e) {
          showNotification(
            paste("Error generating matrix:", e$message),
            type = "error",
            duration = NULL
          )
        }
      )

      waiter_hide()
    }
  )

  # combined plot button observer - stores data for reactive rendering
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
      species_data <- get_species_data(species_code, force_no_contrast = FALSE)
      gene_ids <- selected_genes_list[[species_code]]
      expr_matrix <- get_expression_matrix(species_code, active_transform(), species_data)

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
            Timepoint = factor(condition_of(current_study_design(), species_data$sample_info), levels = condition_levels(current_study_design())),
            Replicate = replicate_of(current_study_design(), species_data$sample_info),
            Expression = as.numeric(expr_matrix[gene_id_to_use, ])
          )
          
          if ("Contrast_Series" %in% names(species_data$sample_info)) {
            expr_data$Contrast_Series <- species_data$sample_info$Contrast_Series
          } else {
            expr_data$Contrast_Series <- NA
          }
          
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

  # combined gene plot render - reactive to plot_state and plot_settings
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
    transform_type <- active_transform()

    if (normalize_baseline) {
      plot_data <- plot_data %>%
        group_by(Gene, Species, Replicate) %>%
        mutate(
          Baseline = Expression[Timepoint == condition_reference(current_study_design())][1],
          Expression = Expression - Baseline
        ) %>%
        ungroup() %>%
        select(-Baseline)
    }

    # setup EntityID for Contrast Series mapping
    plot_data$EntityID <- plot_data$SpeciesCode
    if ("Contrast_Series" %in% names(plot_data)) {
      is_contrast <- !is.na(plot_data$Contrast_Series)
      plot_data$EntityID[is_contrast] <- plot_data$Contrast_Series[is_contrast]
      plot_data$Species[is_contrast] <- plot_data$Contrast_Series[is_contrast]
    }

    # build color map using EntityID for reliable lookup
    entity_color_map <- list()
    entity_shape_map <- list()
    for (ent in unique(plot_data$EntityID)) {
      if (ent %in% names(config)) {
        sp_short <- config[[ent]]$short
        entity_color_map[[ent]] <- resolve_species_color(sp_short, settings_colors, "#808080")
        entity_shape_map[[ent]] <- resolve_species_shape(sp_short, settings_shapes, 16L)
      } else {
        # Contrast series like "WT 2026"
        entity_color_map[[ent]] <- if (!is.null(settings_colors[[ent]])) settings_colors[[ent]] else if (ent == "WT 2026") "#E69F00" else if (ent == "Mutant 2026") "#56B4E9" else if (ent == "WT 2023") "#009E73" else "#808080"
        entity_shape_map[[ent]] <- if (!is.null(settings_shapes[[ent]])) as.integer(settings_shapes[[ent]]) else if (ent == "WT 2026") 16L else if (ent == "Mutant 2026") 17L else if (ent == "WT 2023") 15L else 16L
      }
    }

    plot_data$GeneSpecies <- paste(plot_data$Gene, "-", plot_data$Species)
    plot_data$GeneSpeciesRep <- paste(plot_data$GeneSpecies, "Rep", plot_data$Replicate)
    plot_data$SpeciesColor <- sapply(plot_data$EntityID, function(sc) entity_color_map[[sc]])
    plot_data$SpeciesShape <- sapply(plot_data$EntityID, function(sc) entity_shape_map[[sc]])

    unique_combinations <- unique(plot_data$GeneSpecies)
    unique_genes <- unique(plot_data$Gene)
    unique_species <- unique(plot_data$EntityID)
    n_genes <- length(unique_genes)
    n_species <- length(unique_species)

    # build color vector for gene-species-replicate combinations
    color_vector <- c()
    rep_labels <- c()
    for (combo in unique_combinations) {
      row_match <- plot_data[plot_data$GeneSpecies == combo, ][1, ]
      base_color <- entity_color_map[[row_match$EntityID]]
      
      combo_reps <- sort(unique(plot_data$Replicate[plot_data$GeneSpecies == combo]))
      n_reps <- length(combo_reps)
      
      if (n_reps == 1) {
        color_vector <- c(color_vector, base_color)
        rep_labels <- c(rep_labels, paste(combo, "Rep", combo_reps[1]))
      } else if (n_reps == 2) {
        color_vector <- c(color_vector, base_color, adjustcolor(base_color, alpha.f = 0.6))
        rep_labels <- c(rep_labels, paste(combo, "Rep", combo_reps[1]), paste(combo, "Rep", combo_reps[2]))
      } else {
        alpha_values <- seq(1, 0.4, length.out = n_reps)
        rep_colors <- sapply(alpha_values, function(a) adjustcolor(base_color, alpha.f = a))
        color_vector <- c(color_vector, rep_colors)
        rep_labels <- c(rep_labels, paste(combo, "Rep", combo_reps))
      }
    }
    names(color_vector) <- rep_labels

    # build shape mapping based on encoding settings
    shape_aes <- NULL
    shape_scale <- NULL
    if (encoding_secondary == "shape" && n_genes <= 6) {
      plot_data$ShapeVar <- plot_data$Gene
      shape_aes <- aes(shape = ShapeVar)
      gene_shapes <- setNames(SHAPES_DEFAULT[1:n_genes], unique_genes)
      shape_scale <- scale_shape_manual(values = gene_shapes, name = "Gene")
    } else if (encoding_secondary == "shape" && n_species <= 6) {
      plot_data$ShapeVar <- plot_data$EntityID
      shape_aes <- aes(shape = ShapeVar)
      species_shape_vec <- sapply(unique_species, function(sc) entity_shape_map[[sc]])
      names(species_shape_vec) <- unique_species
      shape_scale <- scale_shape_manual(values = species_shape_vec, name = "Species")
    }

    # build linetype mapping based on encoding settings
    linetype_aes <- NULL
    linetype_scale <- NULL
    if (encoding_secondary == "linetype" && n_genes <= 6) {
      plot_data$LinetypeVar <- plot_data$Gene
      linetype_aes <- aes(linetype = LinetypeVar)
      gene_linetypes <- setNames(LINETYPES_DEFAULT[1:n_genes], unique_genes)
      linetype_scale <- scale_linetype_manual(values = gene_linetypes, name = "Gene")
    }

    expr_label <- if (normalize_baseline) "log2 FC" else "Expression"

    p <- ggplot(
      plot_data,
      aes(
        x = Timepoint, y = Expression, color = GeneSpeciesRep, group = GeneSpeciesRep,
        text = paste(
          "Gene:", Gene, "<br>Species/Series:", Species, "<br>Replicate:", Replicate,
          "<br>Time:", Timepoint, paste0("<br>", expr_label, ": "), round(Expression, 2)
        )
      )
    )

    # add shape aesthetic if configured
    if (!is.null(shape_aes)) {
      p <- p + shape_aes
    }

    # global line aesthetics
    global_thickness <- plot_settings$line_thickness %||% 1

    # add linetype aesthetic if configured
    if (!is.null(linetype_aes)) {
      p <- p + linetype_aes
      p <- p + geom_line(linewidth = global_thickness, alpha = 0.9)
    } else {
      global_type <- plot_settings$line_type %||% "solid"
      p <- p + geom_line(linewidth = global_thickness, linetype = global_type, alpha = 0.9)
    }

    p <- p +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = color_vector, name = "Gene - Species/Series", breaks = names(color_vector), labels = names(color_vector))

    # add shape scale if configured
    if (!is.null(shape_scale)) {
      p <- p + shape_scale
    }

    # add linetype scale if configured
    if (!is.null(linetype_scale)) {
      p <- p + linetype_scale
    }

    p <- p +
      labs(
        y = if (normalize_baseline) "log2 Fold-Change (vs. 0 min)" else expression_axis_label(),
        title = "Cross-species Expression Comparison",
        subtitle = paste("Comparing", length(unique(plot_data$Gene)), "genes across", length(unique(plot_data$Species)), "species"),
        x = condition_label(current_study_design())
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major = element_line(color = if (dark_mode) "gray30" else "gray90"),
        panel.grid.minor = element_line(color = if (dark_mode) "gray20" else "gray95"),
        plot.background = element_rect(fill = if (dark_mode) "#2c3034" else "white", color = NA),
        panel.background = element_rect(fill = if (dark_mode) "#2c3034" else "white", color = NA),
        text = element_text(color = if (dark_mode) "white" else "black"),
        axis.text = element_text(color = if (dark_mode) "white" else "black"),
        legend.text = element_text(color = if (dark_mode) "white" else "black", size = 9),
        legend.title = element_text(color = if (dark_mode) "white" else "black", size = 11),
        legend.background = element_rect(fill = if (dark_mode) "#2c3034" else "white"),
        legend.key = element_rect(fill = if (dark_mode) "#2c3034" else "white")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor = if (dark_mode) "#2c3034" else "white",
        paper_bgcolor = if (dark_mode) "#2c3034" else "white",
        font = list(color = if (dark_mode) "white" else "black"),
        hoverlabel = list(bgcolor = if (dark_mode) "#444" else "white", font = list(size = 12)),
        showlegend = TRUE,
        legend = list(
          x = 1.02, y = 0.5, bgcolor = if (dark_mode) "#2c3034" else "white",
          bordercolor = if (dark_mode) "#444" else "#ddd", borderwidth = 1, font = list(size = 10), tracegroupgap = 5
        ),
        margin = list(b = 100, r = 250, t = 80, l = 60),
        xaxis = list(tickfont = list(size = 11)),
        yaxis = list(tickfont = list(size = 11))
      ) %>%
      apply_y_axis_range(plot_settings, base_rev = "combined_gene_plot") %>%
      config(displayModeBar = TRUE, modeBarButtons = list(list("zoom2d", "pan2d", "resetScale2d", "toImage"))) %>%
      htmlwidgets::onRender("
        function(el, x) {
          if (typeof window.initInteractiveEditor === 'function') {
            window.initInteractiveEditor(el);
          }
        }
      ")
  })

  observeEvent(input$export_combined_plot_btn, {
    show_plot_export_modal("download_combined_plot", "Export Combined Expression Plot",
                           formats = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg"))
  })

  observeEvent(input$download_combined_plot_confirm, {
    fmt <- input$download_combined_plot_format
    w <- input$download_combined_plot_width
    h <- input$download_combined_plot_height
    dpi_val <- input$download_combined_plot_dpi
    if (is.null(fmt)) fmt <- "png"
    if (is.null(w)) w <- 10
    if (is.null(h)) h <- 8
    if (is.null(dpi_val)) dpi_val <- 300

    session$sendCustomMessage("plotly_export", list(
      plotId = "combined_gene_plot",
      format = fmt,
      width = round(w * dpi_val),
      height = round(h * dpi_val),
      filename = paste0("combined_expression_", Sys.Date())
    ))
    removeModal()
  })

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
          gene_list <- as.character(genes_df[, 1])
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
  # heatmap observer - stores data for reactive rendering
  observeEvent(input$generate_ortholog_heatmap, {
    req(input$ortholog_gene_list)
    waiter_show(html = loading_screen)

    gene_list <- unlist(strsplit(input$ortholog_gene_list, "[,;[:space:]]+"))
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
      species_data_list[[species_id]] <- get_species_data(species_id, force_no_contrast = TRUE)
    }

    plot_state$heatmap_data <- list(
      gene_list = gene_list,
      species_data_list = species_data_list,
      normalization = input$heatmap_normalization,
      cluster_rows = input$cluster_rows,
      cluster_cols = input$cluster_cols,
      config = config,
      transform_type = active_transform()
    )
    plot_state$heatmap_ready <- TRUE

    waiter_hide()
  })

  # heatmap plot render - reactive to plot_state and plot_settings
  output$ortholog_heatmap_plot <- renderPlotly({
    if (!isTRUE(plot_state$heatmap_ready) || is.null(plot_state$heatmap_data)) {
      return(plotly_empty() %>% add_annotations(text = "Enter genes and click Generate Heatmap", showarrow = FALSE))
    }

    hd <- plot_state$heatmap_data
    dark_mode <- is_dark()
    current_settings <- reactiveValuesToList(plot_settings)

    result <- tryCatch(
      {
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
          plot_settings = current_settings,
          study_design = current_study_design()
        )
      },
      error = function(e) {
        list(plot = NULL, error = e$message)
      }
    )

    if (!is.null(result$error)) {
      return(plotly_empty() %>% add_annotations(text = paste("Error:", result$error), showarrow = FALSE))
    }

    ortholog_result(result)
    result$plot
  })

  # heatmap table render
  output$ortholog_mapping_table <- renderDT({
    req(ortholog_result())
    ortholog_result()$table
  })

  observeEvent(input$export_ortholog_heatmap_btn, {
    show_plot_export_modal("download_ortholog_heatmap", "Export Cross-Species Heatmap",
                           formats = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg"))
  })

  observeEvent(input$download_ortholog_heatmap_confirm, {
    fmt <- input$download_ortholog_heatmap_format
    w <- input$download_ortholog_heatmap_width
    h <- input$download_ortholog_heatmap_height
    dpi_val <- input$download_ortholog_heatmap_dpi
    if (is.null(fmt)) fmt <- "png"
    if (is.null(w)) w <- 10
    if (is.null(h)) h <- 8
    if (is.null(dpi_val)) dpi_val <- 300

    session$sendCustomMessage("plotly_export", list(
      plotId = "ortholog_heatmap_plot",
      format = fmt,
      width = round(w * dpi_val),
      height = round(h * dpi_val),
      filename = paste0("cross_species_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    ))
    removeModal()
  })

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

  # observer for ortholog mapping when checkbox is toggled
  observeEvent(input$enable_ortholog_analysis, {
    if (input$enable_ortholog_analysis) {
      # get gene list from either source
      gene_list <- NULL

      # check pathway definitions first if in pathway mode
      if (!is.null(input$enable_pathway_comparison) && input$enable_pathway_comparison) {
        pathway_text <- input$pathway_definitions
        if (!is.null(pathway_text) && nchar(trimws(pathway_text)) > 0) {
          # extract all genes from pathway definitions
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
              # pathway name line, next line is genes
              is_gene_line <- TRUE
            } else {
              # gene line
              genes <- strsplit(line, "[,;[:space:]]+")[[1]]
              genes <- trimws(genes)
              genes <- genes[genes != ""]
              all_genes <- c(all_genes, genes)
            }
          }

          gene_list <- unique(all_genes)
        }
      }

      # fallback to regular gene list
      if (is.null(gene_list) || length(gene_list) == 0) {
        if (!is.null(input$gene_list) && nchar(trimws(input$gene_list)) > 0) {
          genes <- strsplit(trimws(input$gene_list), "[,;[:space:]]+")[[1]]
          gene_list <- genes[genes != ""]
        }
      }

      if (is.null(gene_list) || length(gene_list) == 0) {
        showNotification("Please enter a gene list or pathway definitions first", type = "warning", duration = 3)
        updateCheckboxInput(session, "enable_ortholog_analysis", value = FALSE)
        return()
      }

      # perform initial mapping
      perform_ortholog_mapping(gene_list)
    } else {
      # reset when unchecked
      ortholog_state$mapped <- FALSE
      ortholog_state$gene_mapping <- NULL
      ortholog_state$coverage_stats <- NULL
      shinyjs::hide("ortholog_mapping_results")
    }
  })

  # reusable function to perform ortholog mapping
  perform_ortholog_mapping <- function(gene_list) {
    if (is.null(gene_list) || length(gene_list) == 0) {
      showNotification("No gene list to map", type = "warning", duration = 3)
      return(FALSE)
    }

    waiter_show(html = loading_screen)

    config <- current_species_config()
    current_data <- get_all_species_data()

    tryCatch(
      {
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

        # update coverage display
        output$ortholog_coverage_summary <- renderUI({
          if (is.null(coverage_stats)) {
            return(NULL)
          }

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
                  paste0(
                    stats$total_orthologs, " total ortholog",
                    if (stats$total_orthologs != 1) "s" else "",
                    if (stats$paralog_count > 0) {
                      paste0(
                        ", ", stats$paralog_count, " paralog",
                        if (stats$paralog_count != 1) "s" else ""
                      )
                    } else {
                      ""
                    }
                  )
                )
              )
            })
          )
        })
        # generate paralog selection UI
        output$paralog_selection_ui <- renderUI({
          if (is.null(gene_mapping)) {
            return(NULL)
          }

          selection_ui <- tagList()

          for (gene_map in gene_mapping) {
            input_gene <- gene_map$original

            # create section for this input gene
            gene_ui <- div(
              class = "mb-3",
              h6(strong(input_gene), style = "color: var(--bs-primary);"),

              # create checkboxes for each species
              lapply(names(config), function(sp_code) {
                orthologs <- gene_map[[sp_code]]
                if (is.null(orthologs) || length(orthologs) == 0) {
                  return(NULL)
                }

                sp_name <- config[[sp_code]]$short
                checkbox_id <- paste0("select_", input_gene, "_", sp_code)

                # create choices with gene IDs
                choices <- setNames(
                  orthologs,
                  if (length(orthologs) > 1) {
                    sapply(1:length(orthologs), function(i) {
                      paste0(orthologs[i], " [", i, "/", length(orthologs), "]")
                    })
                  } else {
                    orthologs
                  }
                )

                div(
                  class = "ms-3 mb-2",
                  strong(tags$em(sp_name), ":"),
                  checkboxGroupInput(
                    checkbox_id,
                    label = NULL,
                    choices = choices,
                    selected = orthologs[1], # default: select first paralog only
                    inline = FALSE
                  )
                )
              })
            )

            selection_ui <- tagList(selection_ui, gene_ui)
          }

          return(selection_ui)
        })

        # initialize selected_orthologs with first paralog from each species
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
          paste(
            "Mapped", length(gene_mapping), "genes across",
            length(config), "species"
          ),
          type = "message",
          duration = 3
        )

        waiter_hide()
        return(TRUE)
      },
      error = function(e) {
        showNotification(paste("Error mapping orthologs:", e$message),
          type = "error", duration = 5
        )
        ortholog_state$mapped <- FALSE
        waiter_hide()
        return(FALSE)
      }
    )
  }
  # observer for explicit ortholog remapping button
  observeEvent(input$remap_orthologs, {
    req(input$enable_ortholog_analysis)

    # get current gene list
    gene_list <- if (!is.null(input$gene_list) && nchar(trimws(input$gene_list)) > 0) {
      genes <- strsplit(trimws(input$gene_list), "[,;[:space:]]+")[[1]]
      genes[genes != ""]
    } else {
      NULL
    }

    # perform remapping
    perform_ortholog_mapping(gene_list)
  })

  # observer for "Select All" button
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

  # observer for "Select First Only" button
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
          class = "btn btn-primary"
        ),
        modalButton("Close")
      )
    ))

    output$ortholog_mapping_modal_table <- renderDT({
      display_colnames <- colnames(mapping_table)
      for (i in seq_along(display_colnames)) {
        if (grepl("_Ortholog$", display_colnames[i])) {
          short_name <- gsub("_Ortholog$", "", display_colnames[i])
          display_colnames[i] <- paste0("<i>", short_name, "</i> Ortholog")
        }
      }

      datatable(
        mapping_table,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = "Bfrtip"
        ),
        colnames = display_colnames,
        rownames = FALSE,
        escape = FALSE
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
  # download handler for coverage statistics
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
  # parse pathway definitions from text input
  parse_pathway_definitions <- reactive({
    if (input$enable_pathway_comparison) {
      # check for file upload first
      pathway_text <- if (!is.null(input$pathway_file$datapath)) {
        readLines(input$pathway_file$datapath, warn = FALSE)
      } else if (!is.null(input$pathway_definitions) && nchar(trimws(input$pathway_definitions)) > 0) {
        # split on single newline to preserve blank lines
        # normalize line endings first (handle \r\n, \r, \n)
        normalized <- gsub("\r\n", "\n", input$pathway_definitions)
        normalized <- gsub("\r", "\n", normalized)
        strsplit(normalized, "\n")[[1]]
      } else {
        return(NULL)
      }

      if (is.null(pathway_text) || length(pathway_text) == 0) {
        return(NULL)
      }

      # parse pathway format
      pathway_list <- list()
      current_pathway <- NULL
      current_genes <- c()

      for (line in pathway_text) {
        line <- trimws(line)

        if (line == "") {
          # blank line indicates pathway boundary
          if (!is.null(current_pathway) && length(current_genes) > 0) {
            pathway_list[[current_pathway]] <- current_genes
            current_pathway <- NULL
            current_genes <- c()
          }
        } else if (is.null(current_pathway)) {
          # this is a pathway name
          current_pathway <- line
        } else {
          # this line contains genes (split on comma, semicolon, or whitespace)
          genes <- strsplit(line, "[,;[:space:]]+")[[1]]
          genes <- trimws(genes)
          genes <- genes[genes != ""]
          current_genes <- c(current_genes, genes)
        }
      }

      # add last pathway if exists (handles case with no trailing blank line)
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

  # State for gene group analysis (Added for Refactor)
  gene_group_state <- reactiveValues(
    data = NULL,
    type = NULL, # "pathway", "single_species", "multi_species", "pathway_comparison"
    params = list(),
    is_multi_species = FALSE
  )

  # Observer manager for dynamic gene color inputs (prevents slowness/cascading)
  gene_color_obs_manager <- reactiveValues(observers = list())

  # Efficiently manage gene color observers
  observeEvent(gene_group_state$data, {
    # Clean up old observers
    lapply(gene_color_obs_manager$observers, function(o) o$destroy())
    gene_color_obs_manager$observers <- list()

    req(gene_group_state$ready)

    # Get unique genes
    if (isTRUE(gene_group_state$is_multi_species)) {
      genes <- unique(gene_group_state$data$GeneLabel)
    } else {
      genes <- unique(gene_group_state$data$Gene)
    }

    if (length(genes) == 0) {
      return()
    }

    # Ensure every current gene has a stored colour (palette default for any not
    # yet customised). Lives here, not in the picker renderUI, so the renderUI can
    # read gene_colors reactively without a read/write feedback loop. Reads via
    # isolate() so it doesn't re-trigger on its own writes.
    local({
      sorted_genes <- stringr::str_sort(genes, numeric = TRUE)
      current <- isolate(plot_settings$gene_colors)
      if (is.null(current)) current <- list()
      defaults <- get_palette_colors(isolate(plot_settings$gene_palette) %||% "Set2", length(sorted_genes))
      names(defaults) <- sorted_genes
      changed <- FALSE
      for (g in sorted_genes) if (is.null(current[[g]])) { current[[g]] <- defaults[[g]]; changed <- TRUE }
      if (changed) plot_settings$gene_colors <- current
    })

    # create new observers
    gene_color_obs_manager$observers <- lapply(genes, function(g) {
      id_suffix <- gsub("[^a-zA-Z0-9]", "_", g)
      input_id <- paste0("gene_color_", id_suffix)

      observeEvent(input[[input_id]],
        {
          val <- input[[input_id]]
          if (!is.null(val)) {
            plot_settings$gene_colors[[g]] <- val
          }
        },
        ignoreInit = TRUE
      )
    })
  })

  sim_gene_color_obs_manager <- reactiveValues(observers = list())
  
  observeEvent(similarity_results(), {
    lapply(sim_gene_color_obs_manager$observers, function(o) o$destroy())
    sim_gene_color_obs_manager$observers <- list()
    
    req(similarity_results())
    top_genes <- similarity_results()$table %>% group_by(Target) %>% slice_head(n = input$similarity_top_matches[1]) %>% pull(gene_id)
    matches <- similarity_results()$plot_data %>% filter(type == "match", gene_id %in% top_genes)
    unique_labels <- unique(paste0(matches$label, " (", matches$target_species, ")"))
    if (length(unique_labels) == 0) return()
    
    local({
      sorted_labels <- stringr::str_sort(unique_labels, numeric = TRUE)
      current <- isolate(plot_settings$similarity_gene_colors)
      if (is.null(current)) current <- list()
      defaults <- get_palette_colors(isolate(plot_settings$similarity_palette) %||% "Dark2", length(sorted_labels))
      names(defaults) <- sorted_labels
      changed <- FALSE
      for (lbl in sorted_labels) if (is.null(current[[lbl]])) { current[[lbl]] <- defaults[[lbl]]; changed <- TRUE }
      if (changed) plot_settings$similarity_gene_colors <- current
    })

    sim_gene_color_obs_manager$observers <- lapply(unique_labels, function(lbl) {
      input_id <- paste0("sim_gene_color_", gsub("[^a-zA-Z0-9]", "_", lbl))
      observeEvent(input[[input_id]], {
        val <- input[[input_id]]
        if (!is.null(val) && !isTRUE(plot_settings$updating_similarity_colors_from_palette)) {
          plot_settings$similarity_gene_colors[[lbl]] <- val
        }
      }, ignoreInit = TRUE)
    })
  })

  # Dynamic container for visualization (Publication Mode support)
  # Dynamic container for visualization (Publication Mode support)

  # AUTO-SWITCH LOGIC:
  # When user changes "Visualization Type" in sidebar (Line/Bar/Heatmap),
  # they expect to see that interactive plot.
  # If we are currently stuck in "Publication Mode", reset it to Interactive.
  observeEvent(input$group_viz_type,
    {
      req(input$group_viz_type)
      current_mode <- isolate(plot_settings$viz_mode)
      if (!is.null(current_mode) && current_mode == "publication") {
        # Show notification to explain why it switched
        showNotification("Switched to Interactive Mode", type = "message", duration = 2)

        # Reset state
        plot_settings$viz_mode <- "interactive"
        updateRadioButtons(session, "settings_viz_mode", selected = "interactive")
      }
    },
    ignoreInit = TRUE
  )

  # Sync settings_viz_mode from modal to persistent storage
  observeEvent(input$settings_viz_mode, {
    plot_settings$viz_mode <- input$settings_viz_mode
  })

  output$heatmap_container <- renderUI({
    # Use persistent settings instead of transient input
    mode <- plot_settings$viz_mode
    if (is.null(mode)) mode <- "interactive"

    if (mode == "publication") {
      # Check display mode
      pub_mode <- input$settings_pub_mode
      if (is.null(pub_mode)) pub_mode <- "full"

      if (pub_mode == "compact") {
        # Fixed height for compact mode - increased for 2x2 grid
        plotOutput("gene_group_publication_plot", height = "1000px")
      } else {
        # Dynamic height for full mode
        n_genes <- 10 # default fallback
        if (!is.null(gene_group_state$data)) {
          n_genes <- length(unique(gene_group_state$data$Gene))
        }

        # Calculate height:
        # 1 gene = 2 rows in 2x2 grid
        # 4mm (0.15in) per gene per row -> 8mm (~32px) total vertical space/gene
        # Plus buffers for titles, legend, margins (~300px)
        min_height <- n_genes * 32 + 300
        final_height <- max(600, min_height)

        plotOutput("gene_group_publication_plot", height = paste0(final_height, "px"))
      }
    } else {
      plotlyOutput("gene_group_plot", height = "500px")
    }
  })

  # Refactored analysis logic to support re-running from settings
  run_gene_group_analysis <- function() {
    req(!is.null(input$gene_list) || !is.null(input$gene_group_file))
    if (is.null(input$gene_list) && is.null(input$gene_group_file)) {
      showNotification("Please provide either a gene list or upload a file", type = "error")
      return()
    }

    waiter_show(html = loading_screen)

    tryCatch(
      {
        # check if pathway comparison mode is enabled
        if (input$enable_pathway_comparison) {
          # pathway comparison mode
          pathway_defs <- parse_pathway_definitions()

          if (is.null(pathway_defs) || length(pathway_defs) == 0) {
            showNotification("Please define at least one pathway", type = "error")
            waiter_hide()
            return()
          }

          config <- current_species_config()
          pathway_results <- NULL

          # branch based on cross-species ortholog analysis checkbox
          if (input$enable_ortholog_analysis) {
            # multi-species mode via orthology
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
            # single-species mode (no orthology)
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
              active_transform(),
              study_design = current_study_design()
            )
          }

          if (is.null(pathway_results) || nrow(pathway_results) == 0) {
            showNotification("No valid expression data found for pathways",
              type = "error", duration = 5
            )
            waiter_hide()
            return()
          }

          # generate pathway heatmap
          # Store pathway results in state
          gene_group_state$data <- pathway_results
          gene_group_state$type <- "pathway_comparison"
          gene_group_state$params <- list(
            value_type = input$pathway_value_type,
            cluster_pathways = input$cluster_pathways,
            timepoint_mode = input$timepoint_display_mode
          )
          gene_group_state$ready <- TRUE

          # generate summary table with gene details
          output$gene_group_table <- renderDT({
            # get gene details from attribute
            gene_details <- attr(pathway_results, "gene_details")

            summary_table <- pathway_results %>%
              group_by(Pathway, Species) %>%
              summarise(
                NGenes = first(NGenes),
                Mean_Baseline = MeanExpression[Timepoint == condition_reference(current_study_design())],
                Mean_Peak = max(MeanExpression, na.rm = TRUE),
                Mean_Final = MeanExpression[Timepoint == tail(condition_levels(current_study_design()), 1L)],
                .groups = "drop"
              )

            # join gene details if available
            if (!is.null(gene_details) && nrow(gene_details) > 0) {
              summary_table <- left_join(summary_table,
                gene_details[, c("Pathway", "Species", "Genes"), drop = FALSE],
                by = c("Pathway", "Species")
              )
            } else {
              summary_table$Genes <- NA_character_
            }

            if (input$pathway_value_type == "foldchange") {
              fc_summary <- calculate_pathway_foldchange(pathway_results, study_design = current_study_design()) %>%
                group_by(Pathway, Species) %>%
                summarise(
                  Max_FoldChange = max(abs(Log2FC), na.rm = TRUE),
                  .groups = "drop"
                )
              summary_table <- left_join(summary_table, fc_summary, by = c("Pathway", "Species"))
            }

            # create pathway color mapping using pastel palette
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

            # add color column for styling (will be hidden)
            summary_table$RowColor <- pathway_colors[summary_table$Pathway]

            # reorder columns to put Genes before Max_FoldChange if present
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
                dom = "tip",
                columnDefs = list(
                  list(visible = FALSE, targets = ncol(summary_table) - 1),
                  list(width = "180px", targets = which(colnames(summary_table) == "Genes") - 1)
                )
              ),
              rownames = FALSE,
              caption = "Pathway Expression Summary"
            ) %>%
              formatRound(columns = which(sapply(summary_table[, -ncol(summary_table)], is.numeric)), digits = 2) %>%
              formatStyle("Species", fontStyle = "italic") %>%
              formatStyle(
                "Pathway",
                "RowColor",
                backgroundColor = styleEqual(
                  unique(summary_table$RowColor),
                  unique(summary_table$RowColor)
                )
              ) %>%
              formatStyle(
                columns = 1:(ncol(summary_table) - 1),
                "RowColor",
                backgroundColor = styleEqual(
                  unique(summary_table$RowColor),
                  unique(summary_table$RowColor)
                )
              )
          })

          # render pathway legend
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

            # create legend items
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

        # clear pathway legend when not in pathway mode
        output$pathway_table_legend <- renderUI({
          NULL
        })

        # Process input data (ORIGINAL SINGLE/MULTI-GENE MODE)
        gene_groups <- if (!is.null(input$gene_group_file$datapath)) {
          read.csv(input$gene_group_file$datapath)
        } else {
          # Make sure gene_list is not empty and process it
          gene_list <- input$gene_list
          if (!is.null(gene_list) && length(gene_list) > 0 && nchar(trimws(gene_list)) > 0) {
            gene_list <- trimws(gene_list)
            genes <- strsplit(gene_list, "[,;[:space:]]+")[[1]]
            genes <- genes[genes != ""]
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

        # process gene expression data with HOG support; dynamic!
        if (input$enable_ortholog_analysis && ortholog_state$mapped) {
          # Multi-species mode using ortholog mapping
          config <- current_species_config()
          current_data <- get_all_species_data()

          # Get species data for all species
          species_data_list <- list()
          for (sp_code in names(config)) {
            species_data_list[[sp_code]] <- get_species_data(sp_code)
          }

          # process multi-species data
          plot_data <- process_multi_species_gene_set(
            ortholog_state$gene_mapping,
            species_data_list,
            config,
            active_transform(),
            study_design = current_study_design()
          )

          if (is.null(plot_data) || nrow(plot_data) == 0) {
            showNotification("No valid expression data found for orthologs",
              type = "error", duration = 5
            )
            waiter_hide()
            return()
          }

          # filter plot data based on user selections
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

          # filter to only selected orthologs
          if (length(selected_gene_ids) > 0) {
            plot_data <- plot_data[plot_data$GeneID %in% selected_gene_ids, ]
          }

          if (nrow(plot_data) == 0) {
            showNotification("No orthologs selected for plotting",
              type = "warning", duration = 5
            )
            waiter_hide()
            return()
          }

          # store for later use
          ortholog_state$multi_species_data <- plot_data
        } else {
          # Single species mode (existing behavior)
          config <- current_species_config()

          if (is.null(species_data)) {
            showNotification("Species data not available", type = "warning")
            waiter_hide()
            return()
          }

          current_data <- get_all_species_data()
          plot_data <- process_gene_group_data(
            gene_groups,
            species_data,
            current_data,
            config,
            input$group_analysis_species,
            active_transform(),
            study_design = current_study_design()
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

          # Update state with sig test params
          gene_group_state$params$selected_gene <- input$sig_test_gene
          gene_group_state$params$selected_comparisons <- input$sig_test_timepoints
          # Trigger update?
        })

        # initialize the plot without significance testing
        # Store flag for multi-species mode
        is_multi_species <- input$enable_ortholog_analysis &&
          ortholog_state$mapped &&
          "Species" %in% colnames(plot_data)

        # initialize the plot without significance testing
        # Store results in state
        gene_group_state$data <- plot_data
        gene_group_state$type <- if (is_multi_species) "multi_species" else "single_species"
        gene_group_state$is_multi_species <- is_multi_species
        gene_group_state$params <- list(
          viz_type = input$group_viz_type,
          distance_method = input$distance_method,
          data_transform = input$data_transform,
          show_significance = input$show_significance,
          alpha = 1, # default
          selected_gene = NULL,
          selected_comparisons = NULL
        )

        gene_group_state$ready <- TRUE

        # Generator summary table
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
                .groups = "drop"
              )

            datatable(
              summary_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = "tp"
              ),
              rownames = FALSE,
              caption = "Multi-Species Expression Summary"
            ) %>%
              formatStyle("Species", fontStyle = "italic") %>%
              formatRound(
                columns = c("Mean_Expression", "Max_Expression", "Min_Expression", "SD_Expression"),
                digits = 2
              )
          } else {
            # Single species mode - use existing function
            config <- current_species_config()
            species_name <- config[[input$group_analysis_species]]$name
            create_group_summary_table(plot_data, species_name)
          }
        })
      },
      error = function(e) {
        showNotification(
          paste("Error processing gene groups:", e$message),
          type = "error"
        )
      }
    )

    waiter_hide()
  }

  observeEvent(input$analyze_gene_groups, {
    run_gene_group_analysis()
  })

  # Trigger analysis when data source changes (fixes broken toggle)
  observeEvent(active_transform(), {
    if (isTRUE(gene_group_state$ready)) {
      run_gene_group_analysis()
    }
  })


  # Render Interactive Plotly Visualization
  output$gene_group_plot <- renderPlotly({
    req(gene_group_state$ready, gene_group_state$data)

    plot_data <- gene_group_state$data
    params <- gene_group_state$params
    is_multi <- gene_group_state$is_multi_species

    # Handle Pathway Mode
    if (gene_group_state$type == "pathway_comparison") {
      create_pathway_heatmap(
        pathway_data = plot_data,
        value_type = params$value_type,
        is_dark_mode = is_dark(),
        cluster_pathways = params$cluster_pathways,
        timepoint_mode = params$timepoint_mode,
        study_design = current_study_design()
      )
    }
    # Handle Gene Group Mode (Original Logic Refactored)
    else {
      # Check if we have multi-species data
      if (isTRUE(is_multi)) {
        # check if aggregation to species mean is requested
        if (!is.null(params$aggregation_level) && params$aggregation_level == "species_mean") {
          plot_data <- aggregate_to_species_mean(plot_data)
        }

        # Use multi-species visualization
        if (params$viz_type == "line") {
          # Handle Log2FC Transformation
          y_label <- "log2 count per million"
          if (params$data_transform == "log2fc") {
            # Calculate baseline (0min) per Gene and Species
            baseline_data <- plot_data %>%
              filter(Timepoint == condition_reference(current_study_design())) %>%
              group_by(Gene, Species) %>%
              summarise(Baseline = mean(Expression, na.rm = TRUE), .groups = "drop")

            plot_data <- plot_data %>%
              left_join(baseline_data, by = c("Gene", "Species")) %>%
              mutate(Expression = Expression - Baseline)

            y_label <- "log2 Fold-Change (vs. 0 min)"
          }

          # create unique identifier for each gene including paralog info
          plot_data$GeneLabel <- ifelse(
            duplicated(paste(plot_data$Gene, plot_data$Species)) |
              duplicated(paste(plot_data$Gene, plot_data$Species), fromLast = TRUE),
            paste0(plot_data$Gene, " (", plot_data$GeneID, ")"),
            plot_data$Gene
          )

          # get aesthetic mappings
          aes_config <- get_multigene_aesthetics(
            plot_settings,
            unique(plot_data$Species),
            unique(plot_data$GeneLabel)
          )

          # check if aggregation to species mean is requested
          if (!is.null(params$aggregation_level) && params$aggregation_level == "species_mean") {
            # ... existing aggregation logic (keep it but careful with reuse)
            # calculate mean and SE for error bars
            plot_summary <- plot_data %>%
              group_by(Species, Timepoint) %>%
              summarise(
                Mean = mean(Expression, na.rm = TRUE),
                SE = sd(Expression, na.rm = TRUE) / sqrt(n()),
                .groups = "drop"
              )

            # Use species colors for mean plot
            sp_colors <- unlist(plot_settings$species_colors)

            p <- plot_ly(plot_summary,
              x = ~Timepoint, y = ~Mean, color = ~Species,
              colors = sp_colors,
              type = "scatter", mode = "lines+markers",
              error_y = list(
                type = "data",
                array = ~SE,
                visible = TRUE
              )
            ) %>%
              layout(
                title = "Multi-Species Mean Gene Expression",
                xaxis = list(title = condition_label(current_study_design())),
                yaxis = list(title = y_label),
                hovermode = "closest",
                plot_bgcolor = if (is_dark()) "#2c3034" else "white",
                paper_bgcolor = if (is_dark()) "#2c3034" else "white",
                font = list(color = if (is_dark()) "white" else "black")
              )
            return(apply_y_axis_range(p, plot_settings, base_rev = "gene_group_plot"))
          }

          # Prepare aesthetics for ggplot
          # Map columns based on settings
          color_col <- if (aes_config$color_var == "species") "Species" else "GeneLabel"

          # Construct mapping
          # Initialize ggplot with base aesthetics
          p <- ggplot(plot_data, aes(
            x = Timepoint,
            y = Expression,
            color = .data[[color_col]],
            group = interaction(GeneID, Species, Replicate)
          ))

          # Add secondary aesthetics if defined
          if (!is.null(aes_config$linetype_var)) {
            linetype_col <- if (aes_config$linetype_var == "species") "Species" else "GeneLabel"
            p <- p + aes(linetype = .data[[linetype_col]])
            plot_data[[linetype_col]] <- factor(plot_data[[linetype_col]])
          }
          if (!is.null(aes_config$shape_var)) {
            shape_col <- if (aes_config$shape_var == "species") "Species" else "GeneLabel"
            p <- p + aes(shape = .data[[shape_col]])
            plot_data[[shape_col]] <- factor(plot_data[[shape_col]])
          }

          # Add hover text aesthetic
          plot_data$HoverText <- paste(
            "Gene:", plot_data$GeneLabel,
            "<br>Species:", plot_data$Species,
            "<br>Time:", plot_data$Timepoint,
            "<br>Value:", round(plot_data$Expression, 2)
          )
          p <- p + aes(text = HoverText)

          # Add geometries and update data
          # Note: We updated plot_data columns (factors/hover), so we need to ensure ggplot uses the updated data
          # ggplot(plot_data...) captures the data frame at initialization.
          # To ensure factor updates are respected, we should update the data in the plot object
          p$data <- plot_data

          p <- p +
            geom_line(linewidth = 1, alpha = 0.9) +
            geom_point(size = 3, alpha = 0.8) +
            labs(
              y = y_label,
              title = "Multi-Species Gene Set Expression",
              subtitle = paste(
                "Comparing", length(unique(plot_data$GeneID)),
                "total orthologs across", length(unique(plot_data$Species)), "species"
              ),
              x = condition_label(current_study_design())
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
              axis.text.y = element_text(size = 11),
              plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 12),
              panel.grid.major = element_line(color = if (is_dark()) "gray30" else "gray90"),
              panel.grid.minor = element_line(color = if (is_dark()) "gray20" else "gray95"),
              plot.background = element_rect(fill = if (is_dark()) "#2c3034" else "white", color = NA),
              panel.background = element_rect(fill = if (is_dark()) "#2c3034" else "white", color = NA),
              text = element_text(color = if (is_dark()) "white" else "black"),
              axis.text = element_text(color = if (is_dark()) "white" else "black"),
              legend.text = element_text(color = if (is_dark()) "white" else "black"),
              legend.title = element_text(color = if (is_dark()) "white" else "black"),
              legend.background = element_rect(fill = if (is_dark()) "#2c3034" else "white")
            )

          # Apply scales
          if (!is.null(aes_config$color_values)) {
            p <- p + scale_color_manual(values = aes_config$color_values)
          }
          if (!is.null(aes_config$linetype_values)) {
            p <- p + scale_linetype_manual(values = aes_config$linetype_values)
          }
          if (!is.null(aes_config$shape_values)) {
            p <- p + scale_shape_manual(values = aes_config$shape_values)
          }

          # Get current custom styles
          custom_styles <- isolate(rv_plot_aesthetics$styles)
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              uirevision = "gene_group_plot",
              plot_bgcolor = if (is_dark()) "#2c3034" else "white",
              paper_bgcolor = if (is_dark()) "#2c3034" else "white",
              font = list(color = if (is_dark()) "white" else "black")
            ) %>%
            apply_y_axis_range(plot_settings, base_rev = "gene_group_plot") %>%
            htmlwidgets::onRender(paste0("
              function(el, x) {
                if (window.initInteractiveEditor) {
                  window.initInteractiveEditor(el);
                  
                  // Re-apply saved semantic styles
                  var savedStyles = ", jsonlite::toJSON(custom_styles, auto_unbox = TRUE), ";
                  if (Object.keys(savedStyles).length > 0 && window.applySavedAesthetics) {
                      window.applySavedAesthetics(el, savedStyles);
                  }
                }
              }
            "))
        } else if (params$viz_type == "heatmap") {
          # get heatmap palette from settings
          hm_palette <- plot_settings$heatmap_palette

          # prepare data: average replicates first
          heatmap_data <- plot_data %>%
            group_by(Gene, Species, Timepoint) %>%
            summarise(AvgExpression = mean(Expression, na.rm = TRUE), .groups = "drop") %>%
            mutate(GeneSpecies = paste(Gene, Species, sep = "_"))

          # create wide format matrix
          hm_matrix <- heatmap_data %>%
            select(GeneSpecies, Timepoint, AvgExpression) %>%
            pivot_wider(names_from = Timepoint, values_from = AvgExpression) %>%
            column_to_rownames("GeneSpecies") %>%
            as.matrix()

          # apply transformation
          if (params$data_transform == "zscore") {
            hm_matrix <- t(scale(t(hm_matrix)))
            hm_matrix[!is.finite(hm_matrix)] <- 0
          } else if (params$data_transform == "centered") {
            hm_matrix <- t(scale(t(hm_matrix), center = TRUE, scale = FALSE))
          }

          # create hover text before NaN conversion
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

          # convert NA to NaN to prevent plotly nacolor warning
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
                title = condition_label(current_study_design()),
                tickangle = -45
              ),
              yaxis = list(
                title = "Gene - Species",
                autorange = "reversed"
              ),
              plot_bgcolor = if (is_dark()) "#2c3034" else "white",
              paper_bgcolor = if (is_dark()) "#2c3034" else "white",
              font = list(color = if (is_dark()) "white" else "black")
            )
        } else {
          # bar fill encodes species, the hatching through it encodes gene.
          # built with plot_ly directly: ggplotly cannot carry pattern fills.
          settings_colors <- plot_settings$species_colors

          summary_data <- plot_data %>%
            group_by(Timepoint, Species, Gene) %>%
            summarise(Mean = mean(Expression, na.rm = TRUE), .groups = "drop") %>%
            mutate(GeneSpecies = paste(Gene, Species, sep = " | "))

          axis_levels <- if (is.factor(summary_data$Timepoint)) {
            levels(droplevels(summary_data$Timepoint))
          } else {
            unique(as.character(summary_data$Timepoint))
          }

          genes <- sort(unique(as.character(summary_data$Gene)))
          gene_cols <- get_palette_colors(plot_settings$gene_palette %||% "Set2", max(length(genes), 3))
          gene_cols <- setNames(gene_cols[seq_along(genes)], genes)
          # solid marks the first gene, so a single-gene set carries no hatching
          hatch <- c("", "/", "x", "-", "\\", "|", "+", ".")
          gene_shapes <- setNames(hatch[((seq_along(genes) - 1) %% length(hatch)) + 1], genes)

          bar_border <- if (is_dark()) "#2c3034" else "white"
          p <- plot_ly()
          for (combo in sort(unique(summary_data$GeneSpecies))) {
            d <- summary_data[summary_data$GeneSpecies == combo, , drop = FALSE]
            d <- d[order(match(as.character(d$Timepoint), axis_levels)), , drop = FALSE]
            g <- sub(" \\| .*$", "", combo)
            sp <- sub("^.* \\| ", "", combo)
            sp_col <- if (sp %in% names(settings_colors)) settings_colors[[sp]] else "#808080"
            p <- add_trace(p,
              type = "bar", name = combo,
              x = as.character(d$Timepoint), y = d$Mean,
              marker = list(
                color = sp_col,
                line = list(color = bar_border, width = 0.5),
                pattern = list(
                  shape = unname(gene_shapes[[g]]), fgcolor = unname(gene_cols[[g]]),
                  bgcolor = sp_col, size = 7, solidity = 0.35
                )
              ),
              hovertemplate = paste0("<b>", combo, "</b><br>%{x}: %{y:.2f}<extra></extra>")
            )
          }

          p %>% layout(
            barmode = "group",
            bargap = 0.25,
            title = list(text = "Multi-Species Gene Set Expression"),
            xaxis = list(
              title = condition_label(current_study_design()),
              type = "category", categoryorder = "array", categoryarray = axis_levels,
              tickangle = -45
            ),
            yaxis = list(title = paste("Mean", expression_axis_label())),
            legend = list(title = list(text = "Gene | Species")),
            plot_bgcolor = if (is_dark()) "#2c3034" else "white",
            paper_bgcolor = if (is_dark()) "#2c3034" else "white",
            font = list(color = if (is_dark()) "white" else "black")
          )
        }
      } else {
        # Single Species Logic
        gv <- create_group_visualization(
          plot_data = plot_data,
          viz_type = params$viz_type,
          is_dark_mode = is_dark(),
          distance_method = params$distance_method,
          data_transform = params$data_transform,
          show_significance = params$show_significance,
          alpha = params$alpha,
          selected_gene = params$selected_gene,
          selected_comparisons = params$selected_comparisons,
          plot_settings = reactiveValuesToList(plot_settings),
          study_design = current_study_design()
        )
        if (isTRUE(params$viz_type == "line")) gv <- apply_y_axis_range(gv, plot_settings, base_rev = "gene_group_plot")
        gv
      }
    }
  })

  # Render Publication Static Plot (ComplexHeatmap)
  output$gene_group_publication_plot <- renderPlot({
    req(gene_group_state$ready, gene_group_state$data)
    req(input$settings_viz_mode == "publication")

    # 1. Prepare Data
    plot_data <- gene_group_state$data

    # If aggregation was done in state, undo or use raw?
    # Publication plot expects raw long format ideally.
    # If plot_data is already aggregated, we might have issues.
    # But gene_group_state$data stores 'plot_data' which was 'process_multi_species_gene_set' output,
    # which is long format (Gene, Species, Timepoint, Expression).

    # 2. Get Settings
    transform <- input$settings_data_transform
    time_axis <- input$settings_time_axis
    row_order <- input$settings_row_ordering
    show_t0 <- if (!is.null(input$settings_show_t0)) input$settings_show_t0 else TRUE
    # ...

    # 3. Create Heatmaps List
    ht_list <- list()
    config <- current_species_config()

    # Defined order: Sc, Cg, Ca, Kl needed for multi-species
    # For single species, just use the selected one
    species_order <- if (isTRUE(gene_group_state$is_multi_species)) {
      c("sc", "cg", "ca", "kl")
    } else {
      input$group_analysis_species
    }

    # Gene Order Logic
    all_genes <- unique(plot_data$Gene) # Or GeneID?

    # 4. Process Annotations
    row_annot <- NULL
    pal <- NULL
    if (!is.null(gene_group_state$annotations)) {
      df_anno <- gene_group_state$annotations

      # Filter to genes present in the plot
      # Note: gene_group_state$data$Gene contains gene IDs or names depending on mode.
      # Ideally parse_annotations returns ID or Name matching the data.
      # Assuming exact match for now.

      # Match genes
      df_anno <- df_anno[df_anno$Gene %in% all_genes, ]

      if (nrow(df_anno) > 0) {
        # Define colors for categories
        cats <- unique(df_anno$Category)
        n_cats <- length(cats)

        if (n_cats > 0) {
          # Generate colors
          if (n_cats <= 8) {
            pal <- RColorBrewer::brewer.pal(max(3, n_cats), "Set2")[1:n_cats]
          } else {
            pal <- rainbow(n_cats)
          }
          names(pal) <- cats

          # Create HeatmapAnnotation
          # We need to make sure the order aligns with the matrix rows
          # ComplexHeatmap handles alignment via name matching if we assume rows are named ?
          # Ideally, we construct the annotation object such that it can be subsetted.
          # But `make_publication_heatmap` expects `row_annotation` to be passed to `right_annotation`.
          # The Heatmap function aligns annotations if valid.
          # But usually we need to pass a data frame or HeatmapAnnotation.

          # We will create a named vector/list for 'df' argument of HeatmapAnnotation
          # But wait, rows of matrix are genes.
          # We need a data frame with row names = genes.

          anno_df_clean <- data.frame(Category = df_anno$Category)
          rownames(anno_df_clean) <- df_anno$Gene

          # Ensure we have entries for all genes (fill NA)
          missing_genes <- setdiff(all_genes, rownames(anno_df_clean))
          if (length(missing_genes) > 0) {
            missing_df <- data.frame(Category = rep(NA, length(missing_genes)))
            rownames(missing_df) <- missing_genes
            anno_df_clean <- rbind(anno_df_clean, missing_df)
          }

          # Reorder to match all_genes (which dictates row order in `prepare_heatmap_matrix_publication`)
          anno_df_clean <- anno_df_clean[all_genes, , drop = FALSE]

          row_annot <- ComplexHeatmap::HeatmapAnnotation(
            Category = anno_df_clean$Category,
            col = list(Category = pal),
            which = "row",
            show_legend = TRUE,
            annotation_name_side = "top"
          )
        }
      }
    }

    # Define colors and scale
    min_scale <- plot_settings$color_min
    max_scale <- plot_settings$color_max

    # Sanitized range (always straddles 0) drives BOTH the colors and the legend
    scale_rng <- heatmap_scale_range(min_scale, max_scale)
    color_fun <- heatmap_diverging_palette(scale_rng[1], scale_rng[2])

    # Calculate shared timepoints for intersection mode
    allowed_timepoints <- NULL
    if (time_axis == "intersection") {
      # Collect all timepoints per species
      tp_list <- list()
      for (sp in species_order) {
        # Check SpeciesCode column first (consistent with our data fix)
        sp_data <- if ("SpeciesCode" %in% names(plot_data)) {
          plot_data[plot_data$SpeciesCode == sp, ]
        } else {
          plot_data[plot_data$Species == sp, ]
        }

        if (nrow(sp_data) > 0) {
          tp_list[[sp]] <- unique(sp_data$Timepoint)
        }
      }

      # Find intersection
      if (length(tp_list) > 0) {
        allowed_timepoints <- Reduce(intersect, tp_list)
      }
    }

    # 5. Generate Heatmaps
    for (sp in species_order) {
      # Prepare Matrix
      mat <- prepare_heatmap_matrix_publication(
        expression_data = plot_data,
        species_code = sp,
        gene_order = all_genes,
        transform_type = transform,
        time_axis_type = time_axis,
        allowed_timepoints = allowed_timepoints,
        show_t0 = show_t0,
        study_design = current_study_design()
      )

      # Create Heatmap
      # Determine Compact Mode Settings
      pub_mode <- plot_settings$pub_mode
      if (is.null(pub_mode)) pub_mode <- "full"

      is_compact <- pub_mode == "compact"

      # For on-screen rendering, always respect the mode
      show_rows <- !is_compact
      use_raster <- is_compact

      ht <- make_publication_heatmap(
        mat = mat,
        species_prefix = paste0(config[[sp]]$short, "-"),
        species_name = config[[sp]]$name,
        color_fun = color_fun,
        row_annot = if (sp == tail(species_order, 1)) row_annot else NULL, # Only show annotation on the right-most heatmap
        show_legend = FALSE, # Shared legend used
        show_row_names = show_rows,
        category_colors = pal,
        use_raster = use_raster
      )
      ht_list[[length(ht_list) + 1]] <- ht
    }

    # 5. Draw Grid
    # Create shared legend
    min_val <- scale_rng[1]
    max_val <- scale_rng[2]

    legend <- make_shared_legend(
      color_fun = color_fun,
      category_colors = pal,
      min_val = min_val,
      max_val = max_val
    )

    draw_2x2_heatmap(ht_list, legend = legend)
  }, res = 130)

  observeEvent(input$export_group_plot_btn, {
    mode <- input$settings_viz_mode
    if (!is.null(mode) && mode == "publication") {
      show_plot_export_modal("download_group_plot", "Export Gene Group Plot")
    } else {
      show_plot_export_modal("download_group_plot", "Export Gene Group Plot",
                             formats = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg"))
    }
  })

  observeEvent(input$download_group_plot_confirm, {
    fmt <- input$download_group_plot_format
    modal_w <- input$download_group_plot_width
    modal_h <- input$download_group_plot_height
    dpi_val <- input$download_group_plot_dpi
    if (is.null(fmt)) fmt <- "png"
    if (is.null(modal_w)) modal_w <- 10
    if (is.null(modal_h)) modal_h <- 8
    if (is.null(dpi_val)) dpi_val <- 300

    mode <- input$settings_viz_mode
    if (is.null(mode)) mode <- "interactive"

    base <- if (input$enable_pathway_comparison) {
      paste0("pathway_comparison_", input$pathway_value_type)
    } else {
      paste0("gene_group_", input$group_viz_type)
    }
    if (mode == "publication") base <- paste0(base, "_publication")
    out_filename <- paste0(base, "_", Sys.Date())

    if (mode == "publication") {
      req(gene_group_state$ready, gene_group_state$data)
      plot_data <- gene_group_state$data

      pub_mode <- input$settings_pub_mode
      if (is.null(pub_mode)) pub_mode <- "full"
      force_labels <- input$settings_download_labels
      if (is.null(force_labels)) force_labels <- FALSE
      is_compact <- pub_mode == "compact"

      w <- modal_w
      h <- modal_h
      n_genes <- length(unique(plot_data$Gene))
      rec_height <- (n_genes * 0.35) + 2
      if (force_labels || !is_compact) h <- max(h, rec_height)

      tmp <- tempfile(fileext = paste0(".", fmt))
      if (fmt == "pdf") {
        pdf(tmp, width = w, height = h)
      } else if (fmt == "svg") {
        svg(tmp, width = w, height = h)
      } else if (fmt == "jpeg") {
        jpeg(tmp, width = w, height = h, units = "in", res = dpi_val, quality = 95)
      } else {
        png(tmp, width = w, height = h, units = "in", res = dpi_val)
      }

      # Rendering Logic params
      show_rows <- force_labels || !is_compact
      use_raster <- !show_rows

      transform <- input$settings_data_transform
      time_axis <- input$settings_time_axis
      show_t0 <- if (!is.null(input$settings_show_t0)) input$settings_show_t0 else TRUE
      ht_list <- list()
      config <- current_species_config()
      species_order <- c("sc", "cg", "ca", "kl")
      all_genes <- unique(plot_data$Gene)

      min_scale <- plot_settings$color_min
      max_scale <- plot_settings$color_max
      scale_rng <- heatmap_scale_range(min_scale, max_scale)
      color_fun <- heatmap_diverging_palette(scale_rng[1], scale_rng[2])

      allowed_timepoints <- NULL
      if (time_axis == "intersection") {
        tp_list <- list()
        for (sp in species_order) {
          sp_data <- if ("SpeciesCode" %in% names(plot_data)) {
            plot_data[plot_data$SpeciesCode == sp, ]
          } else {
            plot_data[plot_data$Species == sp, ]
          }
          if (nrow(sp_data) > 0) tp_list[[sp]] <- unique(sp_data$Timepoint)
        }
        if (length(tp_list) > 0) allowed_timepoints <- Reduce(intersect, tp_list)
      }

      row_annot <- NULL
      pal <- NULL
      if (!is.null(gene_group_state$annotations)) {
        df_anno <- gene_group_state$annotations
        df_anno <- df_anno[df_anno$Gene %in% all_genes, ]
        if (nrow(df_anno) > 0) {
          cats <- unique(df_anno$Category)
          n_cats <- length(cats)
          if (n_cats > 0) {
            if (n_cats <= 8) {
              pal <- RColorBrewer::brewer.pal(max(3, n_cats), "Set2")[1:n_cats]
            } else {
              pal <- rainbow(n_cats)
            }
            names(pal) <- cats
            anno_df_clean <- data.frame(Category = df_anno$Category)
            rownames(anno_df_clean) <- df_anno$Gene
            missing_genes <- setdiff(all_genes, rownames(anno_df_clean))
            if (length(missing_genes) > 0) {
              missing_df <- data.frame(Category = rep(NA, length(missing_genes)))
              rownames(missing_df) <- missing_genes
              anno_df_clean <- rbind(anno_df_clean, missing_df)
            }
            anno_df_clean <- anno_df_clean[all_genes, , drop = FALSE]
            row_annot <- ComplexHeatmap::HeatmapAnnotation(
              Category = anno_df_clean$Category,
              col = list(Category = pal),
              which = "row",
              show_legend = TRUE,
              annotation_name_side = "top"
            )
          }
        }
      }

      for (sp in species_order) {
        mat <- prepare_heatmap_matrix_publication(
          expression_data = plot_data,
          species_code = sp,
          gene_order = all_genes,
          transform_type = transform,
          time_axis_type = time_axis,
          allowed_timepoints = allowed_timepoints,
          show_t0 = show_t0,
          study_design = current_study_design()
        )

        ht <- make_publication_heatmap(
          mat = mat,
          species_prefix = paste0(config[[sp]]$short, "-"),
          species_name = config[[sp]]$name,
          color_fun = color_fun,
          row_annot = if (sp == tail(species_order, 1)) row_annot else NULL,
          show_legend = FALSE,
          show_row_names = show_rows,
          category_colors = pal,
          use_raster = use_raster
        )
        ht_list[[length(ht_list) + 1]] <- ht
      }

      min_val <- scale_rng[1]
      max_val <- scale_rng[2]
      legend <- make_shared_legend(
        color_fun = color_fun,
        category_colors = pal,
        min_val = min_val,
        max_val = max_val
      )

      draw_2x2_heatmap(ht_list, legend = legend)
      dev.off()

      # Send file via base64
      raw <- readBin(tmp, "raw", file.size(tmp))
      encoded <- jsonlite::base64_enc(raw)
      mime <- switch(fmt,
        png = "image/png", jpeg = "image/jpeg",
        pdf = "application/pdf", svg = "image/svg+xml",
        "application/octet-stream"
      )
      session$sendCustomMessage("download_base64", list(
        data = encoded,
        filename = paste0(out_filename, ".", fmt),
        mime = mime
      ))
    } else {
      # Interactive mode: client-side plotly export
      session$sendCustomMessage("plotly_export", list(
        plotId = "gene_group_plot",
        format = fmt,
        width = round(modal_w * dpi_val),
        height = round(modal_h * dpi_val),
        filename = out_filename
      ))
    }
    removeModal()
  })

  # download handler for pathway data matrix
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
          pathway_results <- calculate_pathway_foldchange(pathway_results, study_design = current_study_design())
        }

        write.csv(pathway_results, file, row.names = FALSE)
      }
    }
  )

  # ridgeline plot handlers - stores data for reactive rendering
  observeEvent(input$generate_ridgeline, {
    waiter_show(html = loading_screen)

    species_data_list <- list()

    if (input$ridgeline_species == "all") {
      config <- current_species_config()
      species_list <- names(config)
    } else {
      resolved_species <- input$ridgeline_species
      if (resolved_species == "sc" && isTRUE(plot_settings$contrast_mode_enabled)) {
        showNotification("Contrast mode active. Using base 2026 WT dataset for Ridgeline.", type = "warning", duration = 5)
      }
      species_list <- c(resolved_species)
    }

    for (species_id in species_list) {
      species_data_list[[species_id]] <- get_species_data(species_id, force_no_contrast = TRUE)
    }

    plot_state$ridgeline_data <- list(
      species_data_list = species_data_list,
      view_type = input$ridgeline_view,
      threshold = input$expression_threshold
    )
    plot_state$ridgeline_ready <- TRUE

    waiter_hide()
  })

  # ridgeline plot render - reactive to plot_state and plot_settings
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
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    } else {
      create_threshold_ridgeline(
        species_data_list = rd$species_data_list,
        threshold = rd$threshold,
        is_dark_mode = dark_mode,
        plot_settings = current_settings,
        study_design = current_study_design()
      )
    }
  })

  # ==========================================
  # Phylogenetic tree: aesthetic editor + export
  # ==========================================

  tree_aes <- do.call(reactiveValues, DEFAULT_TREE_AES)
  tree_editor_rev <- reactiveVal(0)

  # species shown in the current tree (falls back to the full config)
  tree_species <- reactive({
    config <- current_species_config()
    qr <- global_query_state$query_result
    codes <- if (!is.null(qr) && !is.null(qr$genes_by_species)) names(qr$genes_by_species) else names(config)
    codes <- codes[codes %in% names(config)]
    sh <- vapply(codes, function(cd) {
      v <- config[[cd]]$short
      if (is.null(v) || !nzchar(as.character(v)[1])) NA_character_ else as.character(v)[1]
    }, character(1))
    unname(sh[!is.na(sh)])
  })

  observeEvent(input$toggle_tree_editor, {
    shinyjs::toggle("tree_aesthetic_editor")
  })
  observeEvent(input$close_tree_editor, {
    shinyjs::hide("tree_aesthetic_editor")
  })

  output$tree_editor_ui <- renderUI({
    tree_editor_rev()
    species <- tree_species()
    dark <- is_dark()
    fg <- if (dark) "#FFFFFF" else "#000000"
    bgd <- if (dark) "#2c3034" else "#FFFFFF"
    a <- isolate(reactiveValuesToList(tree_aes))
    colors <- isolate(species_colors_dynamic())

    auto_title <- {
      og <- isolate(global_query_state$query_result)$orthogroup
      if (is.null(og)) "Phylogenetic Tree" else paste("Phylogenetic Tree for Orthogroup", og)
    }
    #seed label size with the size the tree is actually drawn at
    n_tips <- length(isolate(global_query_state$tree_data)$tip.label)
    dyn_tip_size <- if (n_tips > 20) 2.5 else if (n_tips > 10) 3.0 else 3.5

    tagList(
      tags$strong("Title"),
      checkboxInput("tree_title_show", "Show title", value = isTRUE(a$title_show)),
      textInput("tree_title_text", "Title text", value = a$title_text %||% auto_title),
      fluidRow(
        column(6, numericInput("tree_title_size", "Size", value = a$title_size, min = 6, max = 40)),
        column(6, colourpicker::colourInput("tree_title_color", "Color",
          value = a$title_color %||% fg, showColour = "background"))
      ),
      checkboxInput("tree_title_bold", "Bold", value = isTRUE(a$title_bold)),

      tags$hr(), tags$strong("Tip labels"),
      radioButtons("tree_tip_color_mode", "Color tips by:",
        choices = c("Species" = "species", "Single color" = "single"),
        selected = a$tip_color_mode, inline = TRUE
      ),
      conditionalPanel(
        condition = "input.tree_tip_color_mode == 'single'",
        colourpicker::colourInput("tree_tip_color", "Tip label color",
          value = a$tip_color %||% fg, showColour = "background")
      ),
      fluidRow(
        column(6, numericInput("tree_tip_size", "Label size",
          value = a$tip_size %||% dyn_tip_size, min = 1, max = 12, step = 0.5)),
        column(6, sliderInput("tree_label_space", "Label space",
          min = 0.6, max = 2.5, value = a$label_space %||% 1, step = 0.05))
      ),
      checkboxInput("tree_tip_align", "Align labels", value = isTRUE(a$tip_align)),

      conditionalPanel(
        condition = "input.tree_tip_color_mode == 'species'",
        tags$hr(), tags$strong("Species tip colors"),
        helpText("App-wide species colors, shared with every other plot."),
        div(lapply(species, function(sp) {
          id_suffix <- gsub("[^a-zA-Z0-9]", "_", sp)
          div(
            style = "display: inline-block; margin: 4px; width: 150px; vertical-align: top;",
            colourpicker::colourInput(
              paste0("tree_species_color_", id_suffix),
              label = sp,
              value = resolve_species_color(sp, colors, "#808080"),
              showColour = "background"
            )
          )
        }))
      ),

      tags$hr(), tags$strong("Branches & nodes"),
      fluidRow(
        column(6, colourpicker::colourInput("tree_branch_color", "Branch color",
          value = a$branch_color %||% fg, showColour = "background")),
        column(6, sliderInput("tree_branch_width", "Branch width",
          min = 0.1, max = 3, value = a$branch_width, step = 0.1))
      ),
      checkboxInput("tree_node_show", "Show node points", value = isTRUE(a$node_show)),
      fluidRow(
        column(6, colourpicker::colourInput("tree_node_color", "Node color",
          value = a$node_color %||% (if (dark) "#666666" else "#999999"), showColour = "background")),
        column(6, sliderInput("tree_node_size", "Node size",
          min = 0, max = 8, value = a$node_size, step = 0.5))
      ),

      tags$hr(), tags$strong("Legend"),
      checkboxInput("tree_legend_show", "Show legend", value = isTRUE(a$legend_show)),
      fluidRow(
        column(6, selectInput("tree_legend_position", "Position",
          choices = c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left"),
          selected = a$legend_position)),
        column(6, textInput("tree_legend_title", "Legend title", value = a$legend_title))
      ),
      fluidRow(
        column(4, numericInput("tree_legend_title_size", "Title size", value = a$legend_title_size, min = 4, max = 30)),
        column(4, numericInput("tree_legend_text_size", "Item size", value = a$legend_text_size, min = 4, max = 30)),
        column(4, colourpicker::colourInput("tree_legend_text_color", "Text color",
          value = a$legend_text_color %||% fg, showColour = "background"))
      ),

      tags$hr(), tags$strong("Background"),
      colourpicker::colourInput("tree_bg_color", "Plot background",
        value = a$bg_color %||% bgd, showColour = "background"),

      tags$hr(),
      actionButton("export_phylo_tree_btn2", "Export tree", icon = icon("download"),
        class = "btn-primary w-100 mb-2"),
      actionButton("tree_editor_reset", "Reset to defaults", icon = icon("undo"),
        class = "btn-outline-danger w-100")
    )
  })

  # control id -> tree_aes field
  tree_editor_bindings <- list(
    tree_title_show = "title_show", tree_title_text = "title_text",
    tree_title_size = "title_size", tree_title_color = "title_color",
    tree_title_bold = "title_bold",
    tree_tip_color_mode = "tip_color_mode", tree_tip_color = "tip_color",
    tree_tip_size = "tip_size", tree_tip_align = "tip_align",
    tree_label_space = "label_space",
    tree_branch_color = "branch_color", tree_branch_width = "branch_width",
    tree_node_show = "node_show", tree_node_color = "node_color",
    tree_node_size = "node_size",
    tree_legend_show = "legend_show", tree_legend_position = "legend_position",
    tree_legend_title = "legend_title", tree_legend_title_size = "legend_title_size",
    tree_legend_text_size = "legend_text_size", tree_legend_text_color = "legend_text_color",
    tree_bg_color = "bg_color"
  )

  for (ctrl_id in names(tree_editor_bindings)) {
    local({
      id <- ctrl_id
      field <- tree_editor_bindings[[ctrl_id]]
      observeEvent(input[[id]], {
        val <- input[[id]]
        if (is.null(val)) return()
        if (is.numeric(val) && any(is.na(val))) return()
        tree_aes[[field]] <- val
      }, ignoreInit = TRUE)
    })
  }

  #tip colors write to the shared species colors; plain env + observeEvent, never observe()
  tree_color_obs <- new.env(parent = emptyenv())
  tree_color_obs$observers <- list()
  observeEvent(list(current_species_config(), tree_species()), {
    config <- current_species_config()
    species <- tree_species()
    lapply(tree_color_obs$observers, function(o) o$destroy())
    tree_color_obs$observers <- lapply(species, function(sp) {
      id <- paste0("tree_species_color_", gsub("[^a-zA-Z0-9]", "_", sp))
      sp_full <- NULL
      hit <- Filter(function(cd) identical(config[[cd]]$short, sp), names(config))
      if (length(hit) > 0) sp_full <- config[[hit[[1]]]]$name
      observeEvent(input[[id]], {
        val <- input[[id]]
        if (is.null(val) || val == "" || isTRUE(plot_settings$updating_colors_from_palette)) return()
        plot_settings$species_colors[[sp]] <- val
        if (!is.null(sp_full)) plot_settings$species_colors[[sp_full]] <- val
        #sync the Plot Settings picker
        id_suffix <- gsub("[^a-zA-Z0-9]", "_", sp)
        colourpicker::updateColourInput(session, paste0("species_color_", id_suffix), value = val)
        updateTextInput(session, paste0("species_color_hex_", id_suffix), value = val)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })

  observeEvent(input$tree_editor_reset, {
    for (nm in names(DEFAULT_TREE_AES)) tree_aes[[nm]] <- DEFAULT_TREE_AES[[nm]]
    tree_editor_rev(tree_editor_rev() + 1)
  })

  # export the tree as currently styled
  observeEvent(input$export_phylo_tree_btn, {
    show_tree_export_modal()
  })
  observeEvent(input$export_phylo_tree_btn2, {
    show_tree_export_modal()
  })

  show_tree_export_modal <- function() {
    tree <- global_query_state$tree_data
    if (is.null(tree)) {
      showNotification("No phylogenetic tree to export. Search for a gene with an orthogroup first.",
                       type = "warning")
      return()
    }
    n_tips <- length(tree$tip.label)
    show_plot_export_modal("download_phylo_tree", "Export Phylogenetic Tree",
      default_width = 10, default_height = max(6, min(16, round(n_tips * 0.35))))
  }

  observeEvent(input$download_phylo_tree_confirm, {
    tree <- global_query_state$tree_data
    qr <- global_query_state$query_result
    req(tree, qr)

    num_or <- function(x, default) {
      x <- suppressWarnings(as.numeric(x))
      if (length(x) != 1 || !is.finite(x) || x <= 0) default else x
    }
    fmt <- input$download_phylo_tree_format %||% "png"
    w <- num_or(input$download_phylo_tree_width, 10)
    h <- num_or(input$download_phylo_tree_height, 8)
    dpi_val <- num_or(input$download_phylo_tree_dpi, 300)

    p <- create_phylo_tree_plot(
      tree, qr$genes_by_species, qr$orthogroup, is_dark(),
      get_all_species_data(), species_colors_dynamic(), current_species_config(),
      aes_opts = reactiveValuesToList(tree_aes)
    )
    req(!is.null(p))

    tmp <- tempfile(fileext = paste0(".", fmt))
    ggsave(tmp, p, device = fmt, width = w, height = h, dpi = dpi_val,
           bg = tree_aes$bg_color %||% (if (is_dark()) "#2c3034" else "white"))

    raw <- readBin(tmp, "raw", file.size(tmp))
    mime <- switch(fmt,
      png = "image/png", jpeg = "image/jpeg",
      pdf = "application/pdf", svg = "image/svg+xml",
      "application/octet-stream"
    )
    session$sendCustomMessage("download_base64", list(
      data = jsonlite::base64_enc(raw),
      filename = paste0("phylo_tree_", qr$orthogroup %||% "orthogroup", "_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt),
      mime = mime
    ))
    removeModal()
  })

  # global search observer
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
    #a custom title names an orthogroup, so drop it on a new search
    tree_aes$title_text <- NULL

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
      global_query_state$query_result <- query_result

      shinyjs::show("gene_explorer_results")
      shinyjs::show("query_status_container")

      is_orphan <- !is.null(query_result$source) && query_result$source == "gene_lookup_no_orthogroup"
      is_synteny <- !is.null(query_result$source) && query_result$source == "synteny_aided"

      global_query_state$last_status <- if (is_orphan) "orphan" else if (is_synteny) "synteny" else "ok"

      output$query_status <- renderUI({
        if (is_orphan) {
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            strong(paste(query, "found")),
            br(),
            "This gene is not assigned to any orthogroup. Single-species analysis tools are available.",
            br(),
            tags$small(
              "Species: ",
              paste(sapply(names(query_result$genes_by_species), function(sp) {
                config[[sp]]$short
              }), collapse = ", ")
            )
          )
        } else if (is_synteny) {
          div(
            class = "alert alert-info",
            icon("link"),
            strong("Query successful!"),
            br(),
            paste("Found", query, "via synteny-aided orthology (YGOB/CGOB)"),
            br(),
            tags$small(
              "Species with syntenic orthologs: ",
              paste(sapply(names(query_result$genes_by_species), function(sp) {
                config[[sp]]$short
              }), collapse = ", ")
            )
          )
        } else {
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
                config[[sp]]$short
              }), collapse = ", ")
            )
          )
        }
      })

      if (is_orphan) {
        global_query_state$tree_data <- NULL
        output$phylo_tree_plot_ui <- renderUI({
          plotOutput("phylo_tree_plot", height = "400px")
        })
        output$phylo_tree_plot <- renderPlot(
          {
            plot.new()
            text(0.5, 0.5, "Unassigned Gene\nNo Phylogenetic Tree Available",
              cex = 1.4, col = if (is_dark()) "#ffc107" else "#856404", font = 2
            )
          },
          bg = if (is_dark()) "#2c3034" else "white",
          res = 120
        )
      } else if (is_synteny) {
        global_query_state$tree_data <- NULL
        output$phylo_tree_plot_ui <- renderUI({
          plotOutput("phylo_tree_plot", height = "400px")
        })
        output$phylo_tree_plot <- renderPlot(
          {
            plot.new()
            text(0.5, 0.5, "No Phylogenetic Tree Available\nOrthology based on synteny (YGOB/CGOB)",
              cex = 1.2, col = if (is_dark()) "#6ea8fe" else "#0d6efd", font = 2
            )
          },
          bg = if (is_dark()) "#2c3034" else "white",
          res = 120
        )
      } else if (!is.null(query_result$og_id) && !is.na(query_result$og_id)) {
        tree <- load_gene_tree(query_result$og_id, current_data)
        global_query_state$tree_data <- tree

        output$phylo_tree_plot_ui <- renderUI({
          if (!is.null(tree)) {
            n_tips <- length(tree$tip.label)
            plot_height <- max(400, min(900, n_tips * 30))
            plotOutput("phylo_tree_plot", height = paste0(plot_height, "px"))
          } else {
            plotOutput("phylo_tree_plot", height = "400px")
          }
        })

        output$phylo_tree_plot <- renderPlot(
          {
            if (!is.null(tree)) {
              create_phylo_tree_plot(tree, query_result$genes_by_species, query_result$orthogroup,
                is_dark(), current_data, species_colors_dynamic(), config,
                aes_opts = reactiveValuesToList(tree_aes))
            } else {
              plot.new()
              text(0.5, 0.5, "Phylogenetic tree not available for this orthogroup",
                cex = 1.2, col = if (is_dark()) "white" else "black"
              )
            }
          },
          bg = tree_aes$bg_color %||% (if (is_dark()) "#2c3034" else "white"),
          res = 120
        )
      }

      output$orthogroup_summary <- renderUI({
        create_orthogroup_summary(query_result, config)
      })

      output$explorer_quick_actions <- renderUI({
        disabled_msg <- "This gene is not assigned to an orthogroup. Cross-species tools require orthology data."
        div(
          class = "mt-4",
          h5("Quick Actions"),
          actionButton(
            "explore_species_view",
            "View in Single Species View",
            icon = icon("chart-line"),
            class = "btn btn-secondary w-100 mb-2"
          ),
          if (is_orphan) {
            tagList(
              tags$button(
                type = "button",
                class = "btn btn-secondary w-100 mb-2",
                disabled = "disabled",
                style = "opacity: 0.45; cursor: not-allowed;",
                title = disabled_msg,
                icon("layer-group"), " View in Comparative Analysis"
              ),
              tags$button(
                type = "button",
                class = "btn btn-secondary w-100",
                disabled = "disabled",
                style = "opacity: 0.45; cursor: not-allowed;",
                title = disabled_msg,
                icon("th"), " Generate Cross-Species Heatmap"
              )
            )
          } else {
            tagList(
              actionButton(
                "explore_combined_view",
                "View in Comparative Analysis",
                icon = icon("layer-group"),
                class = "btn btn-secondary w-100 mb-2"
              ),
              actionButton(
                "explore_heatmap",
                "Generate Cross-Species Heatmap",
                icon = icon("th"),
                class = "btn btn-secondary w-100"
              )
            )
          }
        )
      })

      output$explorer_orthogroup_section <- renderUI({
        if (is_orphan) {
          NULL
        } else {
          div(
            class = "mt-4",
            h5(if (is_synteny) "Synteny-Aided Orthologs" else "Orthogroup Members"),
            DTOutput("explorer_orthogroup_table")
          )
        }
      })

      output$explorer_orthogroup_table <- renderDT({
        create_orthogroup_details_table(query_result, config)
      })
    } else {
      # Not found
      global_query_state$last_status <- "not_found"
      #drop the previous hit too, or the results row stays hidden with nothing
      #in its place and the tab reads as blank
      global_query_state$query_result <- NULL
      global_query_state$tree_data <- NULL
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

  # quick action: view in Single Species View
  observeEvent(input$explore_species_view, {
    req(global_query_state$query_result)

    # Find which species has the gene
    result <- global_query_state$query_result
    config <- current_species_config()

    if (!is.null(result$genes_by_species)) {
      species_list <- names(result$genes_by_species)

      if (length(species_list) == 1) {
        # Only one species - navigate directly
        first_species <- species_list[1]
        config <- current_species_config() # Get current config
        species_name <- config[[first_species]]$name

        # navigate to Single Species View and then to the specific species tab
        updateTabsetPanel(session, "nav", selected = "species_analysis_container")

        # small delay to ensure the main tab is loaded
        shinyjs::delay(100, {
          updateTabsetPanel(session, "species_tabs", selected = first_species)

          # Scroll to top of page
          shinyjs::runjs("window.scrollTo(0, 0);")

          # Pre-fill and trigger search
          updateTextInput(session, paste0(first_species, "_genename"),
            value = global_query_state$current_query
          )
          shinyjs::delay(100, {
            shinyjs::click(paste0(first_species, "_search_button"))
          })
        })

        # JavaScript to directly activate the tab panel WITHOUT opening the dropdown
        # navigate to Single Species View and then to the specific species tab
        updateTabsetPanel(session, "nav", selected = "species_analysis_container")

        # Small delay to ensure the main tab is loaded
        shinyjs::delay(100, {
          updateTabsetPanel(session, "species_tabs", selected = selected_species)

          # Scroll to top of page
          shinyjs::runjs("window.scrollTo(0, 0);")

          # Pre-fill and trigger search
          updateTextInput(session, paste0(selected_species, "_genename"),
            value = global_query_state$current_query
          )
          shinyjs::delay(100, {
            shinyjs::click(paste0(selected_species, "_search_button"))
          })
        })

        updateTextInput(session, paste0(first_species, "_genename"),
          value = global_query_state$current_query
        )
        shinyjs::delay(100, {
          shinyjs::click(paste0(first_species, "_search_button"))
        })
      } else {
        # multiple species; selection modal
        config <- current_species_config()
        species_choices <- setNames(
          species_list,
          sapply(species_list, function(sp) {
            sp_config <- config[[sp]]
            genes_count <- nrow(result$genes_by_species[[sp]])
            paste0(
              sp_config$name, " (", genes_count, " gene",
              if (genes_count > 1) "s" else "", ")"
            )
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
              class = "btn btn-primary"
            )
          ),
          size = "m",
          easyClose = TRUE
        ))
      }
    }
  })

  # handler for species selection confirmation
  observeEvent(input$confirm_species_selection, {
    req(input$species_selection_modal)

    selected_species <- input$species_selection_modal
    config <- current_species_config()
    species_name <- config[[selected_species]]$name

    # Close the modal
    removeModal()

    # navigate to Single Species View and then to the specific species tab
    updateTabsetPanel(session, "nav", selected = "species_analysis_container")

    # Small delay to ensure the main tab is loaded
    shinyjs::delay(100, {
      updateTabsetPanel(session, "species_tabs", selected = selected_species)

      # Pre-fill and trigger search
      updateTextInput(session, paste0(selected_species, "_genename"),
        value = global_query_state$current_query
      )
      shinyjs::delay(100, {
        shinyjs::click(paste0(selected_species, "_search_button"))
      })
    })

    # Pre-fill and trigger search
    updateTextInput(session, paste0(selected_species, "_genename"),
      value = global_query_state$current_query
    )
    shinyjs::delay(100, {
      shinyjs::click(paste0(selected_species, "_search_button"))
    })
  })

  # quick action: view in Comparative Analysis
  observeEvent(input$explore_combined_view, {
    req(global_query_state$query_result)

    # navigate to comparative view
    updateTabsetPanel(session, "nav", selected = "Comparative View")

    # Pre-fill the search box
    updateTextInput(session, "combined_genename",
      value = global_query_state$current_query
    )

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
      value = paste(all_genes, collapse = "\n")
    )

    # Trigger heatmap generation
    shinyjs::delay(100, {
      shinyjs::click("generate_ortholog_heatmap")
    })
  })

  observeEvent(input$export_ridgeline_btn, {
    show_plot_export_modal("download_ridgeline", "Export Ridgeline Plot")
  })

  observeEvent(input$download_ridgeline_confirm, {
    fmt <- input$download_ridgeline_format
    w <- input$download_ridgeline_width
    h <- input$download_ridgeline_height
    dpi_val <- input$download_ridgeline_dpi
    if (is.null(fmt)) fmt <- "png"
    if (is.null(w)) w <- 10
    if (is.null(h)) h <- 8
    if (is.null(dpi_val)) dpi_val <- 300

    species_data_list <- list()
    if (input$ridgeline_species == "all") {
      species_list <- names(current_species_config())
    } else {
      resolved_species <- input$ridgeline_species
      species_list <- c(resolved_species)
    }

    for (species_id in species_list) {
      species_data_list[[species_id]] <- get_species_data(species_id, force_no_contrast = TRUE)
    }

    p <- if (input$ridgeline_view == "distribution") {
      create_ridgeline_plot(
        species_data_list = species_data_list,
        is_dark_mode = is_dark(),
        plot_settings = reactiveValuesToList(plot_settings),
        study_design = current_study_design()
      )
    } else {
      create_threshold_ridgeline(
        species_data_list = species_data_list,
        threshold = input$expression_threshold,
        is_dark_mode = is_dark(),
        plot_settings = reactiveValuesToList(plot_settings),
        study_design = current_study_design()
      )
    }

    tmp <- tempfile(fileext = paste0(".", fmt))
    ggsave(tmp, p, device = fmt, width = w, height = h, dpi = dpi_val)

    raw <- readBin(tmp, "raw", file.size(tmp))
    encoded <- jsonlite::base64_enc(raw)
    mime <- switch(fmt,
      png = "image/png", jpeg = "image/jpeg",
      pdf = "application/pdf", svg = "image/svg+xml",
      "application/octet-stream"
    )
    session$sendCustomMessage("download_base64", list(
      data = encoded,
      filename = paste0("ridgeline_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt),
      mime = mime
    ))
    removeModal()
  })
  # File reading helper
  read_data_file <- function(file_path, file_name = "") {
    if (is.null(file_path)) {
      return(NULL)
    }

    ext <- tools::file_ext(file_path)

    tryCatch(
      {
        if (ext %in% c("tsv", "txt")) {
          data <- read.table(file_path,
            header = TRUE, sep = "\t",
            stringsAsFactors = FALSE, check.names = FALSE
          )
        } else if (ext == "csv") {
          data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          stop("Unsupported file format")
        }
        return(data)
      },
      error = function(e) {
        showNotification(paste("Error reading", file_name, ":", e$message),
          type = "error", duration = 5
        )
        return(NULL)
      }
    )
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

    # Sample is the only fixed column; the condition axis is whatever the user
    # named it and is declared later in the Design wizard
    missing_cols <- setdiff("Sample", colnames(sample_data))

    if (length(missing_cols) > 0) {
      errors <- c(errors, paste(
        species_name, ": Missing required columns:",
        paste(missing_cols, collapse = ", ")
      ))
    }

    candidate_cols <- setdiff(colnames(sample_data), c("Sample", "Replicate"))
    if (length(candidate_cols) == 0) {
      errors <- c(errors, paste(
        species_name,
        ": No condition column found. Add a column describing each sample (Timepoint, Dose, Genotype, ...)."
      ))
    }
    if (!"Replicate" %in% colnames(sample_data)) {
      warnings <- c(warnings, paste(
        species_name, ": No Replicate column; samples are treated as unreplicated."
      ))
    }
    if (length(candidate_cols) > 0) {
      warnings <- c(warnings, paste0(
        species_name, ": condition column candidates - ",
        paste(candidate_cols, collapse = ", "), ". Choose one in Step 6."
      ))
    }

    # Check sample names match expression matrix columns if provided
    if (!is.null(expr_data) && "Sample" %in% colnames(sample_data)) {
      expr_samples <- colnames(expr_data)
      info_samples <- sample_data$Sample

      if (!all(info_samples %in% expr_samples)) {
        errors <- c(errors, paste(
          species_name,
          ": Sample names in metadata don't match expression matrix columns"
        ))
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

    # GeneID is the only column the app needs; GeneName aids search
    if (!"GeneName" %in% colnames(anno_data)) {
      warnings <- c(warnings, paste0(
        species_name, ": annotation has no GeneName column; genes will only be searchable by ID."))
    }
    missing_cols <- setdiff("GeneID", colnames(anno_data))

    if (length(missing_cols) > 0) {
      errors <- c(errors, paste(
        species_name, ": Missing required annotation columns:",
        paste(missing_cols, collapse = ", ")
      ))
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

      # one frame per species, not one per gene: a real genome is ~10^4-10^5 rows
      gene_names <- as.character(gene_names)[seq_along(gene_ids)]
      gene_names[is.na(gene_names)] <- ""
      lookup_entries[[species_id]] <- data.frame(
        gene_id = gene_ids,
        species = species_id,
        expression_id = gene_ids,
        id_type = paste0(toupper(species_id), "GL0"),
        source_info = "original",
        gene_name = gene_names,
        hog_id = "",
        og_id = "",
        stringsAsFactors = FALSE
      )
    }

    lookup_table <- as.data.frame(rbindlist(lookup_entries, fill = TRUE))

    # Add orthology information if provided
    if (!is.null(ortho_data) && "gene_id" %in% colnames(ortho_data)) {
      # single vectorized match; the old per-gene which() was O(genes x ortho rows)
      idx <- match(lookup_table$gene_id, ortho_data$gene_id)
      hit <- !is.na(idx)
      if (any(hit)) {
        if ("hog_id" %in% colnames(ortho_data)) {
          lookup_table$hog_id[hit] <- as.character(ortho_data$hog_id[idx[hit]])
        }
        if ("og_id" %in% colnames(ortho_data)) {
          lookup_table$og_id[hit] <- as.character(ortho_data$og_id[idx[hit]])
        }
      }
    }

    return(as.data.table(lookup_table))
  }

  #Gene trees for the Gene Explorer. Accepts an OrthoFinder Gene_Trees zip, loose
  #newick files, or a two-column table of orthogroup + newick. Produces the same
  #shape load_gene_tree() reads: trees[["<OG>_tree.txt"]]$newick
  parse_gene_trees_upload <- function(files) {
    if (is.null(files) || nrow(files) == 0) return(NULL)
    trees <- list()

    add_tree <- function(og, nwk) {
      og <- trimws(as.character(og)); nwk <- trimws(as.character(nwk))
      if (!nzchar(og) || !nzchar(nwk) || !grepl(";", nwk, fixed = TRUE)) return(invisible(NULL))
      og <- sub("_tree(\\.txt)?$", "", og)
      n_tips <- tryCatch(length(ape::read.tree(text = nwk)$tip.label),
                         error = function(e) NA_integer_)
      if (is.na(n_tips)) return(invisible(NULL))
      trees[[paste0(og, "_tree.txt")]] <<- list(newick = nwk, n_tips = n_tips)
    }

    read_newick_file <- function(path, name) {
      txt <- paste(readLines(path, warn = FALSE), collapse = "")
      add_tree(tools::file_path_sans_ext(basename(name)), txt)
    }

    for (i in seq_len(nrow(files))) {
      path <- files$datapath[i]
      name <- files$name[i]
      ext <- tolower(tools::file_ext(name))

      if (ext == "zip") {
        exdir <- file.path(tempdir(), paste0("trees_", i))
        unlink(exdir, recursive = TRUE); dir.create(exdir, showWarnings = FALSE)
        inner <- tryCatch({ utils::unzip(path, exdir = exdir); list.files(exdir, recursive = TRUE, full.names = TRUE) },
                          error = function(e) character(0))
        for (f in inner) {
          if (tolower(tools::file_ext(f)) %in% c("txt", "nwk", "newick", "tree", "tre")) {
            read_newick_file(f, basename(f))
          }
        }
      } else if (ext %in% c("tsv", "csv")) {
        tbl <- tryCatch(read_data_file(path, "Gene trees"), error = function(e) NULL)
        if (!is.null(tbl) && ncol(tbl) >= 2) {
          for (r in seq_len(nrow(tbl))) add_tree(tbl[r, 1], tbl[r, 2])
        }
      } else {
        read_newick_file(path, name)
      }
    }

    if (length(trees) == 0) NULL else list(trees = trees)
  }

  # Process OrthoFinder output
  process_orthofinder_output <- function(orthogroups_file, hog_file = NULL) {
    ortho_data <- read_data_file(orthogroups_file, "Orthogroups")

    if (is.null(ortho_data)) {
      return(NULL)
    }

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
              hog_id = og_id, # Default HOG to OG if not provided
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

      # Process gene IDs before validation
      if (!is.null(expr_data)) {
        # Check if first column is unnamed (empty string) - this happens with row.names CSV export
        if (names(expr_data)[1] == "") {
          # First column is unnamed and contains gene IDs
          rownames(expr_data) <- as.character(expr_data[, 1]) # Convert to character first
          expr_data <- expr_data[, -1, drop = FALSE] # Remove the first column, keep as data frame
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
      # debug output
      if (!is.null(expr_data) && DEBUG_MODE) {
        debug_print(paste("Species:", species_name))
        debug_print(paste("Columns:", paste(names(expr_data), collapse = ", ")))
        debug_print(paste("Column types:", paste(sapply(expr_data, class), collapse = ", ")))
        debug_print(paste("First rownames:", paste(head(rownames(expr_data), 3), collapse = ", ")))
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

            return(tp_str) # Return as-is if can't parse
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
      upload_state$validation_errors <- c(
        upload_state$validation_errors,
        expr_valid$errors, sample_valid$errors, anno_valid$errors
      )
      upload_state$validation_warnings <- c(
        upload_state$validation_warnings,
        expr_valid$warnings, sample_valid$warnings, anno_valid$warnings
      )

      # properly using auto-generated sample_info
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

          # an upload has no provenance, so label it from its own values
          attr(expr_matrix, "expr_label") <- detected_expression_label(expr_matrix)

          # Store with CONSISTENT naming (no prefixes in main structure)
          upload_state$uploaded_data[[species_id]] <- list(
            lcpm = expr_matrix,
            sample_info = sample_data, # Now uses the auto-generated data with correct timepoints
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
      hog_path <- if (!is.null(input$upload_hog)) {
        input$upload_hog$datapath
      } else {
        NULL
      }
      ortho_data <- process_orthofinder_output(
        input$upload_orthogroups$datapath,
        hog_path
      )
      upload_state$uploaded_data$orthofinder <- list(orthogroups = ortho_data)
    } else if (input$orthology_source == "custom" && !is.null(input$upload_custom_ortho)) {
      ortho_data <- read_data_file(input$upload_custom_ortho$datapath, "Custom orthology")
      upload_state$uploaded_data$orthofinder <- list(orthogroups = ortho_data)
    }

    # Gene trees (optional): powers the Gene Explorer tree for uploaded data
    if (!is.null(input$upload_gene_trees)) {
      pt <- parse_gene_trees_upload(input$upload_gene_trees)
      upload_state$uploaded_data$phylo_trees <- pt
      if (is.null(pt)) {
        upload_state$validation_warnings <- c(
          upload_state$validation_warnings,
          "Gene trees: no readable newick strings found; the tree panel will stay empty."
        )
      } else {
        og_ids <- sub("_tree\\.txt$", "", names(pt$trees))
        known <- if (!is.null(upload_state$uploaded_data$orthofinder$orthogroups$og_id)) {
          unique(upload_state$uploaded_data$orthofinder$orthogroups$og_id)
        } else NULL
        matched <- if (is.null(known)) NA_integer_ else sum(og_ids %in% known)
        upload_state$validation_warnings <- c(
          upload_state$validation_warnings,
          sprintf("Gene trees: %d parsed%s.", length(og_ids),
                  if (is.na(matched)) "" else sprintf(", %d matching an uploaded orthogroup", matched))
        )
      }
    }

    # Update validation status
    upload_state$validated <- length(upload_state$validation_errors) == 0

    # Enable process button if validated
    if (upload_state$validated) {
      shinyjs::enable("process_uploads")
      #the design is the next thing they must do, so send them there
      if (is.null(upload_state$study_design)) {
        updateTabsetPanel(session, "upload_preview_tabs", selected = "design")
        showNotification("Validation successful. Next: describe your design (Step 6).",
          type = "message", duration = 8
        )
      } else {
        showNotification("Validation successful! You can now process the data.",
          type = "message", duration = 5
        )
      }
    } else {
      shinyjs::disable("process_uploads")
      showNotification("Validation failed. Please fix the errors and try again.",
        type = "error", duration = 5
      )
    }

    waiter_hide()
  })

  # Process uploads observer
  observeEvent(input$process_uploads, {
    req(upload_state$validated)

    waiter_show(html = loading_screen)

    tryCatch(
      {
        # fall back to an inferred design rather than the stock time course
        if (is.null(upload_state$study_design)) {
          inferred <- infer_study_design_from_uploads()
          if (!is.null(inferred)) {
            upload_state$study_design <- inferred
            upload_state$design_inferred <- TRUE
            showNotification(
              sprintf("No design applied, so one was inferred: %s (%s, %d levels).",
                      condition_label(inferred), condition_type(inferred),
                      length(condition_levels(inferred))),
              type = "warning", duration = 10)
          }
        }

        # Build gene lookup table
        ortho_data_for_lookup <- if (!is.null(upload_state$uploaded_data$orthofinder)) {
          upload_state$uploaded_data$orthofinder$orthogroups
        } else {
          NULL
        }
        gene_lookup <- build_gene_lookup(
          upload_state$uploaded_data,
          ortho_data_for_lookup
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
          type = "message", duration = 5
        )

        # Show success banner
        shinyjs::show("upload_status_banner")
        output$upload_status_content <- renderUI({
          div(
            icon("check-circle"),
            strong("Custom data loaded successfully!"),
            br(),
            paste("Active species:", paste(input$upload_species_select, collapse = ", ")),
            actionButton("dismiss_upload_banner", "Dismiss",
              class = "btn btn-sm btn-light float-right"
            )
          )
        })

        # Update class for success
        shinyjs::removeClass("upload_status_banner", "alert-warning")
        shinyjs::addClass("upload_status_banner", "alert-success")
      },
      error = function(e) {
        showNotification(paste("Error processing data:", e$message),
          type = "error", duration = NULL
        )
        upload_state$processed <- FALSE
      }
    )

    waiter_hide()
  })

  # Reset to default data
  observeEvent(input$reset_to_default, {
    data_source("default")
    upload_state$custom_all_species_data <- NULL
    upload_state$processed <- FALSE
    upload_state$validated <- FALSE
    upload_state$study_design <- NULL
    upload_state$design_inferred <- FALSE

    # Clear species data cache
    rm(list = ls(species_data_cache), envir = species_data_cache)

    shinyjs::hide("upload_status_banner")
    shinyjs::disable("process_uploads")

    showNotification("Reset to demo data successful!", type = "message", duration = 3)
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
          return(datatable(preview, options = list(dom = "t", pageLength = 10)))
        }
      }
    }
  })

  output$upload_sample_preview <- renderDT({
    if (length(upload_state$uploaded_data) > 0) {
      for (sp_id in names(upload_state$uploaded_data)) {
        if (!is.null(upload_state$uploaded_data[[sp_id]]$sample_info)) {
          return(datatable(upload_state$uploaded_data[[sp_id]]$sample_info,
            options = list(dom = "t", pageLength = 10)
          ))
        }
      }
    }
  })

  #study-design wizard: combine per-species sample_info into one table with a
  #Species column so orthology members come from the uploaded species.
  #safety net: without this, processing an upload with no design applied would
  #silently fall back to the stock GRE time levels and blank every plot
  infer_study_design_from_uploads <- function() {
    sp_ids <- setdiff(names(upload_state$uploaded_data),
                      c("orthofinder", "metadata", "gene_lookup", "phylo_trees"))
    frames <- Filter(Negate(is.null), lapply(sp_ids, function(sp) {
      upload_state$uploaded_data[[sp]]$sample_info
    }))
    if (length(frames) == 0) return(NULL)

    #the axis is any column every species carries besides Sample/Replicate
    cand <- Reduce(intersect, lapply(frames, function(f) {
      setdiff(names(f), c("Sample", "Replicate"))
    }))
    if (length(cand) == 0) return(NULL)
    axis <- cand[1]

    seen <- unique(unlist(lapply(frames, function(f) as.character(f[[axis]]))))
    seen <- seen[!is.na(seen) & nzchar(seen)]
    if (length(seen) == 0) return(NULL)

    vals <- infer_numeric_progression(seen)
    if (!is.null(vals)) {
      ord <- order(vals); lv <- seen[ord]; vv <- vals[ord]; ctype <- "interval"
    } else {
      lv <- seen; vv <- NULL; ctype <- "nominal"
    }
    multi <- length(sp_ids) > 1

    tryCatch(make_study_design(
      id = "uploaded_inferred",
      align = if (multi) "orthology" else "identity",
      members = sp_ids,
      comparison_label = if (multi) "Species" else "Group",
      condition_column = axis, condition_type = ctype,
      condition_levels = lv, condition_values = vv,
      condition_reference = lv[1], condition_label = axis,
      genome_col = "Species", replicate = "Replicate"
    ), error = function(e) NULL)
  }

  #what the app will actually use, shown next to the Process button
  output$design_status <- renderUI({
    sd <- upload_state$study_design
    if (is.null(sd)) {
      guess <- infer_study_design_from_uploads()
      if (is.null(guess)) {
        return(div(class = "alert alert-secondary py-2 mb-2",
                   icon("circle-info"), " Upload sample metadata, then describe your design."))
      }
      return(div(class = "alert alert-warning py-2 mb-2",
        icon("triangle-exclamation"), strong(" No design applied yet."), br(),
        sprintf("Processing now would assume: %s (%s, %d levels, baseline %s).",
                condition_label(guess), condition_type(guess),
                length(condition_levels(guess)), condition_reference(guess)),
        br(), tags$small("Open the Design tab to confirm or change it.")))
    }
    div(class = if (isTRUE(upload_state$design_inferred)) "alert alert-warning py-2 mb-2"
                else "alert alert-success py-2 mb-2",
      icon(if (isTRUE(upload_state$design_inferred)) "wand-magic-sparkles" else "check"),
      strong(if (isTRUE(upload_state$design_inferred)) " Design inferred." else " Design applied."),
      br(),
      sprintf("%s: %s, %d levels, baseline %s.",
              condition_label(sd), condition_type(sd),
              length(condition_levels(sd)), condition_reference(sd) %||% "none"))
  })

  wizard_sample_data <- reactive({
    sp_ids <- setdiff(names(upload_state$uploaded_data),
                      c("orthofinder", "metadata", "gene_lookup", "phylo_trees"))
    frames <- lapply(sp_ids, function(sp) {
      si <- upload_state$uploaded_data[[sp]]$sample_info
      if (is.null(si)) return(NULL)
      si$Species <- sp
      si
    })
    frames <- frames[!vapply(frames, is.null, logical(1))]
    req(length(frames) > 0)
    dplyr::bind_rows(frames)
  })

  observeEvent(input$goto_design_tab, {
    updateTabsetPanel(session, "upload_preview_tabs", selected = "design")
  })

  output$design_wizard_ui <- renderUI({ studyDesignWizardUI("design_wizard") })
  outputOptions(output, "design_wizard_ui", suspendWhenHidden = FALSE)
  wizard_design <- studyDesignWizardServer("design_wizard", sample_data = wizard_sample_data)

  observeEvent(input$apply_design, {
    res <- wizard_design()
    if (isTRUE(res$ok)) {
      upload_state$study_design <- res$design
      upload_state$design_inferred <- FALSE
      output$design_review <- renderUI({
        div(class = "alert alert-success mt-2",
          strong("Design applied."), br(),
          HTML(paste(describe_study_design(res$design), collapse = "<br>")))
      })
      showNotification("Study design applied.", type = "message", duration = 4)
    } else {
      output$design_review <- renderUI({
        div(class = "alert alert-warning mt-2", strong("Not ready: "), res$error)
      })
    }
  })

  output$upload_anno_preview <- renderDT({
    if (length(upload_state$uploaded_data) > 0) {
      for (sp_id in names(upload_state$uploaded_data)) {
        if (!is.null(upload_state$uploaded_data[[sp_id]]$anno)) {
          preview <- upload_state$uploaded_data[[sp_id]]$anno[1:min(10, nrow(upload_state$uploaded_data[[sp_id]]$anno)), ]
          return(datatable(preview, options = list(dom = "t", pageLength = 10)))
        }
      }
    }
  })

  output$upload_ortho_preview <- renderDT({
    if (!is.null(upload_state$uploaded_data$orthofinder)) {
      ortho_data <- upload_state$uploaded_data$orthofinder$orthogroups
      if (!is.null(ortho_data)) {
        preview <- ortho_data[1:min(20, nrow(ortho_data)), ]
        return(datatable(preview, options = list(dom = "t", pageLength = 20)))
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

  # ==========================================
  # Find Similar Profiles (Gene Shape-Search)
  # ==========================================
  
  observe({
    config <- current_species_config()
    req(config)
    
    choices <- setNames(names(config), sapply(names(config), function(id) {
      config[[id]]$name
    }))
    
    updateSelectizeInput(session, "similarity_ref_species", choices = choices)
    
    updateCheckboxGroupInput(
      session, 
      "similarity_tgt_species", 
      choiceNames = lapply(names(config), function(id) HTML(paste0("<i>", config[[id]]$name, "</i>"))),
      choiceValues = names(config)
    )
  })

  similarity_results <- reactiveVal(NULL)
  
  observeEvent(input$similarity_search_button, {
    req(input$similarity_gene_input, input$similarity_ref_species, input$similarity_tgt_species)
    
    query_gene <- trimws(input$similarity_gene_input)
    if (query_gene == "") return()
    
    withProgress(message = 'Running similarity search...', value = 0, {
      tryCatch({
        all_data <- get_all_species_data()
        all_results_table <- list()
        all_plot_data <- list()
        raw_by_target <- list() #per-target res (incl null_cors) for the ggprism export

        for (tgt in input$similarity_tgt_species) {
          incProgress(1/length(input$similarity_tgt_species), detail = paste("Processing", tgt))

          res <- run_similarity_search(
            query_gene_name = query_gene,
            ref_species = input$similarity_ref_species,
            tgt_species = tgt,
            top_x = input$similarity_top_matches[2],
            all_species_data = all_data,
            study_design = current_study_design()
          )

          res$table$Target <- tgt
          res$plot_data$target_species <- ifelse(res$plot_data$type == "match", tgt, input$similarity_ref_species)

          raw_by_target[[tgt]] <- res
          all_results_table[[tgt]] <- res$table
          all_plot_data[[tgt]] <- res$plot_data %>% dplyr::filter(type == "match")
          
          # Save the first target's query trajectory for the plot overlay
          if (tgt == input$similarity_tgt_species[1]) {
            query_plot_data <- res$plot_data %>% dplyr::filter(type == "query")
          }
        }

        combined_table <- dplyr::bind_rows(all_results_table) %>% dplyr::arrange(dplyr::desc(pearson_r))
        combined_plot_data <- dplyr::bind_rows(query_plot_data, dplyr::bind_rows(all_plot_data))
        
        # Ensure Timepoint is a factor with proper chronological order and sort the data so lines connect correctly
        combined_plot_data$Timepoint <- factor(combined_plot_data$Timepoint, levels = condition_levels(current_study_design()))
        combined_plot_data <- combined_plot_data %>% dplyr::arrange(gene_id, Timepoint)
        similarity_results(list(
          table = combined_table,
          plot_data = combined_plot_data,
          raw = raw_by_target,
          query_gene = query_gene,
          ref_species = input$similarity_ref_species,
          top_n = input$similarity_top_matches[1]
        ))
      }, error = function(e) {
        showNotification(paste("Search failed:", e$message), type = "error")
        similarity_results(NULL)
      })
    })
  })

  output$similarity_plot <- renderPlotly({
    req(similarity_results())
    
    current_settings <- reactiveValuesToList(plot_settings)
    
    plot_data <- similarity_results()$plot_data
    
    # Filter the matches for the graph based on the top N gene IDs PER SPECIES
    top_genes <- similarity_results()$table %>% dplyr::group_by(Target) %>% dplyr::slice_head(n = input$similarity_top_matches[1]) %>% dplyr::pull(gene_id)

    matches <- plot_data %>%
      dplyr::filter(type == "match", gene_id %in% top_genes) %>%
      dplyr::mutate(plot_label = label)
    
    p <- plot_ly()
    
    if (nrow(matches) > 0) {
      unique_labels <- unique(matches$plot_label)
      color_map <- character(length(unique_labels))
      names(color_map) <- unique_labels
      linetype_map <- character(length(unique_labels))
      names(linetype_map) <- unique_labels
      symbol_map <- character(length(unique_labels))
      names(symbol_map) <- unique_labels
      
      plotly_linetypes <- c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot")
      plotly_symbols <- c("circle", "square", "diamond", "cross", "x", "triangle-up", "star")
      unique_species <- unique(matches$target_species)
      config <- current_species_config()
      
      sec <- current_settings$encoding_similarity_secondary
      if (is.null(sec)) sec <- "linetype"
      
      for (i in seq_along(unique_labels)) {
        lbl <- unique_labels[i]
        sp_code <- matches$target_species[matches$plot_label == lbl][1]
        sp_short <- if(!is.null(config[[sp_code]])) config[[sp_code]]$short else sp_code
        sp_idx <- which(unique_species == sp_code)
        
        # Color resolution
        if (current_settings$encoding_similarity_color == "species") {
          color_map[lbl] <- resolve_species_color(sp_short, current_settings$species_colors)
          
          lbl_idx <- which(unique_labels[sapply(unique_labels, function(x) matches$target_species[matches$plot_label == x][1] == sp_code)] == lbl)
          linetype_map[lbl] <- if (sec %in% c("linetype", "both")) plotly_linetypes[((lbl_idx - 1) %% length(plotly_linetypes)) + 1] else "solid"
          symbol_map[lbl] <- if (sec %in% c("shape", "both")) plotly_symbols[((lbl_idx - 1) %% length(plotly_symbols)) + 1] else "circle"
          
        } else {
          if (!is.null(current_settings$similarity_gene_colors[[lbl]])) {
            color_map[lbl] <- current_settings$similarity_gene_colors[[lbl]]
          } else {
            gene_colors <- get_palette_colors(current_settings$similarity_palette, length(unique_labels))
            color_map[lbl] <- gene_colors[i]
          }
          
          linetype_map[lbl] <- if (sec %in% c("linetype", "both")) plotly_linetypes[((sp_idx - 1) %% length(plotly_linetypes)) + 1] else "solid"
          
          if (sec %in% c("shape", "both")) {
            pch_val <- resolve_species_shape(sp_short, current_settings$species_shapes)
            pch_to_plotly_symbol <- function(pch) {
              pch <- as.integer(pch); if (pch == 16) return("circle"); if (pch == 17) return("triangle-up"); if (pch == 15) return("square"); if (pch == 18) return("diamond"); if (pch == 8) return("star"); if (pch == 3) return("cross"); if (pch == 4) return("x"); return("circle")
            }
            symbol_map[lbl] <- pch_to_plotly_symbol(pch_val)
          } else {
            symbol_map[lbl] <- "circle"
          }
        }
      }

      p <- matches %>%
        dplyr::group_by(plot_label) %>%
        plot_ly(x = ~Timepoint, y = ~consensus_z,
                color = ~plot_label, colors = color_map,
                linetype = ~plot_label, linetypes = linetype_map,
                symbol = ~plot_label, symbols = symbol_map,
                type = 'scatter', mode = 'lines+markers',
                line = list(width = current_settings$similarity_line_width %||% 2),
                marker = list(size = current_settings$similarity_marker_size %||% 6),
                opacity = current_settings$similarity_opacity %||% 0.6)
    }
    
    query_data <- plot_data %>% dplyr::filter(type == "query")
    if (nrow(query_data) > 0) {
      p <- p %>% add_trace(data = query_data, x = ~Timepoint, y = ~consensus_z,
                     type = 'scatter', mode = 'lines+markers',
                     name = query_data$label[1],
                     inherit = FALSE,
                     line = list(color = "black", width = 4, dash = "dash"),
                     marker = list(color = "black", size = 8),
                     opacity = 1)
    }
                   
    #emulate ggprism theme_prism: thick black axis bars, outside ticks, no grid, bold
    prism_axis <- function(title_text, extra = list()) {
      c(list(
        title = list(text = title_text, font = list(size = 16, color = "black")),
        showline = TRUE, linecolor = "black", linewidth = 3,
        ticks = "outside", tickcolor = "black", tickwidth = 2, ticklen = 6,
        showgrid = FALSE, zeroline = FALSE,
        tickfont = list(size = 13, color = "black")
      ), extra)
    }
    p <- p %>% layout(
      title = list(text = "Temporal Profile Similarity Overlay", font = list(size = 18, color = "black")),
      xaxis = prism_axis(condition_label(current_study_design()), list(categoryorder = "array",
                         categoryarray = condition_levels(current_study_design()))),
      yaxis = prism_axis("Consensus Z-Score"),
      plot_bgcolor = "white", paper_bgcolor = "white",
      showlegend = TRUE,
      legend = list(title = list(text = "Gene", font = list(size = 13, color = "black")),
                    font = list(size = 12, color = "black")),
      margin = list(r = 220),
      hovermode = "closest",
      uirevision = "similarity_plot"
    )

    #replay prior edits; isolated so reading them cannot re-render this plot
    saved <- isolate(rv_plot_aesthetics$plot_styles[["similarity_plot"]])
    saved_json <- if (length(saved) > 0) jsonlite::toJSON(saved, auto_unbox = TRUE) else "{}"
    p %>% htmlwidgets::onRender(paste0("
      function(el, x) {
        if (typeof window.initInteractiveEditor === 'function') {
          window.initInteractiveEditor(el);
        }
        if (typeof window.applyEditorStyleState === 'function') {
          window.applyEditorStyleState(el, ", saved_json, ");
        }
      }
    "))
  })

  #aesthetic editor state for the interactive similarity plot
  similarity_editor_styles <- reactive({
    rv_plot_aesthetics$plot_styles[["similarity_plot"]]
  })

  #static ggprism two-panel for the first target species, carrying the editor's edits
  output$similarity_prism_plot <- renderPlot({
    req(similarity_results()$raw)
    top_n <- similarity_results()$top_n %||% 5
    similarity_twopanel_prism(similarity_results(), top_n = top_n,
                              styles = similarity_editor_styles())
  }, res = 96)

  #export the trajectory and null-distribution panels separately, each with
  #DPI/width/height + PNG/SVG (same modal convention as the other modules)
  observeEvent(input$export_similarity_trajectory_btn, {
    show_plot_export_modal("download_sim_trajectory", "Export Trajectory",
      default_width = 12, default_height = 6, formats = c("PNG" = "png", "SVG" = "svg"))
  })
  observeEvent(input$export_similarity_null_btn, {
    show_plot_export_modal("download_sim_null", "Export Null Distribution",
      default_width = 12, default_height = 5, formats = c("PNG" = "png", "SVG" = "svg"))
  })

  deliver_similarity_panel <- function(prefix, plot_obj, name) {
    fmt <- input[[paste0(prefix, "_format")]] %||% "png"
    w   <- input[[paste0(prefix, "_width")]]  %||% 12
    h   <- input[[paste0(prefix, "_height")]] %||% 6
    dpi <- input[[paste0(prefix, "_dpi")]]    %||% 300
    tmp <- tempfile(fileext = paste0(".", fmt))
    ggplot2::ggsave(tmp, plot_obj, device = fmt, width = w, height = h, dpi = dpi, bg = "white")
    raw <- readBin(tmp, "raw", file.size(tmp))
    mime <- switch(fmt, png = "image/png", svg = "image/svg+xml", "application/octet-stream")
    session$sendCustomMessage("download_base64", list(
      data = jsonlite::base64_enc(raw),
      filename = sprintf("%s_%s.%s", name, Sys.Date(), fmt),
      mime = mime))
    removeModal()
  }

  observeEvent(input$download_sim_trajectory_confirm, {
    sr <- similarity_results(); req(sr$raw)
    tgt <- names(sr$raw)[1]; top_n <- sr$top_n %||% 5
    p <- similarity_trajectory_prism(sr$raw[[tgt]], sr$query_gene, sr$ref_species, tgt, top_n,
                                     styles = similarity_editor_styles())
    deliver_similarity_panel("download_sim_trajectory", p,
      sprintf("trajectory_%s_%s_vs_%s", sr$query_gene, sr$ref_species, tgt))
  })
  observeEvent(input$download_sim_null_confirm, {
    sr <- similarity_results(); req(sr$raw)
    tgt <- names(sr$raw)[1]; top_n <- sr$top_n %||% 5
    p <- similarity_null_prism(sr$raw[[tgt]], tgt, top_n, styles = similarity_editor_styles())
    deliver_similarity_panel("download_sim_null", p,
      sprintf("nulldist_%s_%s_vs_%s", sr$query_gene, sr$ref_species, tgt))
  })
  
  output$similarity_table <- renderDT({
    req(similarity_results())
    
    dt <- datatable(
      similarity_results()$table %>% dplyr::select(Target, gene_id, gene_name, pearson_r, null_percentile, perm_p_value),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  var $th = $(thead).find('th');",
          "  $th.eq(3).attr('title', 'Pearson correlation coefficient measuring similarity of temporal expression profiles.');",
          "  $th.eq(4).attr('title', 'Percentile rank compared to 1000 randomly selected genes from the target species (e.g. 99 means top 1%).');",
          "  $th.eq(5).attr('title', 'Empirical P-Value calculated by randomly permuting the target gene\\'s timepoints 1000 times. Lower values indicate the shape similarity is less likely due to chance.');",
          "}"
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      colnames = c("Target", "Gene ID", "Gene Name", "Pearson R", "Null Pctl", "Perm P-Val")
    )
    
    config <- current_species_config()
    current_settings <- reactiveValuesToList(plot_settings)
    
    targets <- unique(similarity_results()$table$Target)
    bg_colors <- sapply(targets, function(sp_code) {
      sp_short <- if(!is.null(config[[sp_code]])) config[[sp_code]]$short else sp_code
      resolve_species_color(sp_short, current_settings$species_colors)
    })
    
    dt %>% formatStyle(
      'Target',
      backgroundColor = styleEqual(unname(targets), unname(bg_colors)),
      color = 'white',
      fontWeight = 'bold'
    )
  })

  ver_nav <- reactiveValues(keys = character(0), idx = 1L, missed = FALSE)

  # full history keys ordered oldest -> newest (independent of list order)
  ver_keys_sorted <- function() {
    vers <- vapply(app_version_history, function(e) e$version, character(1))
    names(app_version_history)[order(numeric_version(vers))]
  }

  open_version_modal <- function(keys, start_idx, missed) {
    ver_nav$keys <- keys; ver_nav$idx <- start_idx; ver_nav$missed <- missed
    showModal(modalDialog(
      title = tagList(icon("bullhorn"), " App Updates"),
      easyClose = TRUE, size = "m",
      uiOutput("version_modal_header"),
      uiOutput("version_modal_body"),
      footer = tagList(div(
        class = "d-flex w-100 justify-content-between align-items-center",
        div(
          actionButton("ver_prev", NULL, icon = icon("chevron-left"), class = "btn-sm"),
          tags$span(class = "mx-2 text-muted small", textOutput("version_modal_pos", inline = TRUE)),
          actionButton("ver_next", NULL, icon = icon("chevron-right"), class = "btn-sm")
        ),
        div(
          tags$a(href = app_version_info$changelog_url, target = "_blank",
                 class = "btn btn-outline-secondary btn-sm me-2",
                 icon("scroll"), " View Change Log"),
          modalButton("Got it!")
        )
      ))
    ))
  }

  output$version_modal_header <- renderUI({
    if (isTRUE(ver_nav$missed) && length(ver_nav$keys) > 1) {
      div(class = "alert alert-info py-2",
          strong("You haven't been around for a while!"), br(),
          "Use the arrows below to catch up on everything that changed.")
    }
  })

  output$version_modal_body <- renderUI({
    req(length(ver_nav$keys) >= 1)
    build_version_entry(app_version_history[[ ver_nav$keys[[ver_nav$idx]] ]])
  })

  output$version_modal_pos <- renderText({
    req(length(ver_nav$keys) >= 1)
    sprintf("%d of %d", ver_nav$idx, length(ver_nav$keys))
  })

  observeEvent(input$ver_prev, { ver_nav$idx <- max(1L, ver_nav$idx - 1L) })
  observeEvent(input$ver_next, { ver_nav$idx <- min(length(ver_nav$keys), ver_nav$idx + 1L) })

  # AUTO trigger: show only releases newer than what the user last saw
  observeEvent(input$trigger_version_modal, {
    seen <- input$trigger_version_modal$seen
    all_keys <- ver_keys_sorted()                       # oldest -> newest
    if (is.null(seen) || !nzchar(seen)) {
      open_version_modal(tail(all_keys, 1), 1L, missed = FALSE)   # first-time: newest only
    } else {
      newer <- vapply(all_keys, function(k)
        numeric_version(app_version_history[[k]]$version) > numeric_version(seen), logical(1))
      unseen <- all_keys[newer]
      if (length(unseen) == 0) unseen <- tail(all_keys, 1)
      open_version_modal(unseen, 1L, missed = length(unseen) > 1)  # start at OLDEST unseen
    }
    shinyjs::runjs(sprintf('localStorage.setItem("rnacross_seen_version", "%s");',
                           app_version_info$version))
  })

  # MANUAL trigger (bullhorn): full history, start at newest, browse backward
  observeEvent(input$show_version_info, {
    all_keys <- ver_keys_sorted()
    open_version_modal(all_keys, length(all_keys), missed = FALSE)
  })

  observeEvent(input$launch_tutorial_from_modal, {
    removeModal()
    shinyjs::delay(300, shinyjs::click("show_help"))
  })

  # ---------------------------------------------------------------
  # search-first navbar: command palette, search pill, Gene Explorer landing
  # ---------------------------------------------------------------

  #species per HOG, one pass over the lookup table instead of a query per keystroke
  hog_species_count <- reactive({
    empty <- setNames(integer(0), character(0))
    gl <- get_all_species_data()$gene_lookup
    if (is.null(gl) || !NROW(gl)) return(empty)
    hog <- as.character(gl$hog_id)
    sp <- as.character(gl$species)
    keep <- !is.na(hog) & nzchar(hog)
    if (!any(keep)) return(empty)
    counts <- tapply(sp[keep], hog[keep], function(x) length(unique(x)))
    setNames(as.integer(counts), names(counts))
  })

  observeEvent(input$rnx_query, {
    raw <- input$rnx_query %||% ""
    genes <- rnx_gene_suggestions(
      get_all_species_data()$gene_lookup, raw,
      hog_species_count(), length(current_species_config())
    )
    session$sendCustomMessage("rnx_results", list(query = raw, genes = genes))
  }, ignoreInit = TRUE)

  #the palette does not run its own query: it drives the Gene Explorer's
  observeEvent(input$rnx_open_gene, {
    gene <- input$rnx_open_gene$gene
    req(is.character(gene), nzchar(gene))
    updateTextInput(session, "global_gene_query", value = gene)
    nav_select("nav", "gene_explorer")
    shinyjs::delay(150, shinyjs::click("global_search_button"))
  })

  observeEvent(input$rnx_goto, {
    value <- input$rnx_goto$value
    req(is.character(value), nzchar(value))
    nav_select("nav", value)
  })

  observeEvent(input$rnx_clear_gene, {
    rnx_reset_query()
  })

  observeEvent(input$rnx_export, {
    show_tree_export_modal()
  })

  #keeps the active chip in sync with wherever the app actually is
  observeEvent(input$nav, {
    session$sendCustomMessage("rnx_active_tab", list(value = input$nav))
  })

  #the palette prefills with whatever gene is in scope, so it needs the name
  observe({
    sc <- rnx_scope()
    in_scope <- !is.null(sc) && !identical(sc$state, "not_found")
    session$sendCustomMessage("rnx_scope", list(gene = if (in_scope) sc$gene else NULL))
  })

  # ---- the gene in scope, for the pill and the resume card ----

  #one description of scope for every surface. A restored session has only the
  #gene name, so the HOG and coverage come from the lookup table instead.
  rnx_scope <- reactive({
    q <- global_query_state$current_query
    if (is.null(q) || !nzchar(q)) return(NULL)

    status <- global_query_state$last_status
    n_total <- max(1L, length(current_species_config()))

    if (identical(status, "not_found")) {
      return(list(gene = q, state = "not_found", hog = "", n_species = 0L,
                  n_total = n_total, level = "low", badge = "not found",
                  badge_icon = "triangle-exclamation"))
    }

    qr <- global_query_state$query_result
    if (!is.null(qr)) {
      n_sp <- if (!is.null(qr$genes_by_species)) {
        sum(vapply(qr$genes_by_species, function(d) NROW(d) > 0, logical(1)))
      } else {
        0L
      }
      hog <- qr$orthogroup %||% ""
      state <- status %||% "ok"
    } else {
      #restored from localStorage: the query has not been re-run yet
      info <- rnx_gene_scope_info(get_all_species_data()$gene_lookup, q,
                                  hog_species_count(), n_total)
      if (!isTRUE(info$found)) return(NULL)
      n_sp <- info$n_species
      hog <- info$hog_id
      state <- if (nzchar(hog)) "cached" else "orphan"
    }

    if (identical(state, "orphan")) {
      return(list(gene = q, state = state, hog = "", n_species = 0L, n_total = n_total,
                  level = "medium", badge = "no orthogroup",
                  badge_icon = "triangle-exclamation"))
    }

    ratio <- n_sp / n_total
    level <- if (ratio >= 0.999) "high" else if (ratio >= 0.5) "medium" else "low"
    list(
      gene = q, state = state, hog = hog, n_species = n_sp, n_total = n_total,
      level = level,
      badge = sprintf("%d / %d species", n_sp, n_total),
      badge_icon = if (identical(level, "high")) "circle-check" else "triangle-exclamation"
    )
  })

  output$rnx_pill_content <- renderUI({
    sc <- rnx_scope()
    if (is.null(sc)) {
      return(tagList(
        span(id = "rnx-trigger-label", class = "rnx-trigger-label",
             "Search a gene, or jump to a tool…"),
        span(class = "rnx-spacer"),
        span(class = "rnx-kbd", "⌘K")
      ))
    }
    tagList(
      span(id = "rnx-trigger-label", class = "rnx-scope-gene", sc$gene),
      if (nzchar(sc$hog)) span(class = "rnx-scope-hog", sc$hog),
      span(class = paste("rnx-scope-badge", sc$level),
           icon(sc$badge_icon), sc$badge),
      span(class = "rnx-spacer")
    )
  })

  output$rnx_pill_clear <- renderUI({
    if (is.null(rnx_scope())) return(NULL)
    tags$button(
      id = "rnx-scope-clear", type = "button", class = "rnx-scope-clear",
      `aria-label` = "Clear the gene in scope", title = "Clear the gene in scope",
      icon("xmark")
    )
  })

  output$rnx_export_slot <- renderUI({
    sc <- rnx_scope()
    if (is.null(sc) || identical(sc$state, "not_found")) return(NULL)
    tags$button(
      id = "rnx-export", type = "button", class = "rnx-export",
      title = "Export the phylogenetic tree",
      icon("download"), "Export"
    )
  })

  # ---- Gene Explorer landing ----

  #example chips validated against whatever data is actually loaded
  rnx_examples <- reactive({
    rnx_example_genes(get_all_species_data()$gene_lookup, EXAMPLE_GENES, 5L)
  })

  #shared by both landing states; scope per species is either its dataset count
  #or the number of levels on the study design's axis
  rnx_species_rows <- function() {
    config <- current_species_config()
    colors <- species_colors_dynamic()
    data_all <- get_all_species_data()
    design <- current_study_design()
    n_levels <- length(condition_levels(design))
    axis <- tolower(condition_label(design))

    lapply(names(config), function(sp_code) {
      sp <- config[[sp_code]]
      color <- resolve_species_color(sp$short, colors,
                                     resolve_species_color(sp$name, colors))
      sp_data <- data_all[[sp_code]]
      n_sets <- if (is.null(sp_data)) 0L else length(grep("sample_info", names(sp_data), value = TRUE))
      scope <- if (n_sets > 1L) {
        sprintf("%d datasets", n_sets)
      } else {
        sprintf("%d %ss", n_levels, axis)
      }
      tags$tr(
        tags$td(span(class = "rnx-dot", style = paste0("background:", color, ";"))),
        tags$td(tags$em(sp$name)),
        tags$td(scope)
      )
    })
  }

  rnx_dataset_footer <- function() {
    if (identical(data_source(), "custom")) {
      div(class = "rnx-card-foot",
          sprintf("Your uploaded dataset · %d species · ", length(current_species_config())),
          tags$a(`data-rnx-tool` = "data_upload", "manage"))
    } else {
      div(class = "rnx-card-foot",
          "Demo data · ",
          tags$a(`data-rnx-tool` = "data_upload", "upload your own"))
    }
  }

  #recent genes live in localStorage; command_palette.js pushes them up as JSON
  rnx_recent_rows <- function() {
    raw <- input$rnx_recent_genes
    if (is.null(raw) || !length(raw) || !nzchar(raw[1])) return(NULL)
    recents <- tryCatch(jsonlite::fromJSON(raw[1], simplifyDataFrame = FALSE),
                        error = function(e) NULL)
    if (is.null(recents) || !length(recents)) return(NULL)
    lapply(recents, function(r) {
      gene <- if (is.list(r)) r$gene else r
      at <- if (is.list(r)) r$at else NULL
      if (is.null(gene) || !nzchar(gene)) return(NULL)
      tags$button(
        type = "button", class = "rnx-recent", `data-rnx-gene` = gene,
        span(class = "rnx-recent-gene", gene),
        div(class = "rnx-spacer"),
        span(class = "rnx-recent-at", if (is.null(at)) "earlier" else format_time_ago(at))
      )
    })
  }

  rnx_launchpad <- function() {
    active <- input$nav %||% "gene_explorer"
    tiles <- lapply(RNX_TOOL_GROUPS, function(grp) {
      lapply(Filter(function(t) identical(t$group, grp), RNX_TOOLS), function(tool) {
        tags$button(
          type = "button",
          class = paste0("rnx-tile", if (identical(tool$value, active)) " rnx-tile-current" else ""),
          `data-rnx-tool` = tool$value,
          div(class = "rnx-tile-head",
              span(class = "rnx-tile-icon", tags$i(class = tool$icon)),
              span(class = "rnx-tile-name", tool$label)),
          div(class = "rnx-tile-desc", tool$desc)
        )
      })
    })

    recents <- rnx_recent_rows()

    div(
      class = "rnx-landing",
      div(
        class = "rnx-landing-row",
        div(
          class = "rnx-landing-main",
          tags$h2("No gene in scope yet"),
          div(class = "rnx-landing-sub",
              "Search above (", span(class = "rnx-inline-kbd", "⌘K"), ") or start from a tool."),
          div(class = "rnx-tiles", tiles)
        ),
        div(
          class = "rnx-side",
          if (length(recents)) {
            div(class = "rnx-card",
                div(class = "rnx-card-title", "Recent genes"),
                div(recents))
          },
          div(class = "rnx-card",
              div(class = "rnx-card-title", "Dataset in scope"),
              tags$table(class = "rnx-species", tags$tbody(rnx_species_rows())),
              rnx_dataset_footer())
        )
      )
    )
  }

  rnx_resume_card <- function(sc) {
    saved_at <- global_query_state$restored_at
    chips <- lapply(setdiff(rnx_examples(), sc$gene), function(g) {
      tags$button(type = "button", class = "rnx-gene-chip-btn", `data-rnx-gene` = g, g)
    })

    div(
      class = "rnx-landing rnx-landing-resume",
      div(
        class = "rnx-landing-row",
        div(
          class = "rnx-resume",
          div(class = "rnx-eyebrow", "Pick up where you left off"),
          div(
            class = "rnx-resume-head",
            span(class = "rnx-resume-gene", sc$gene),
            if (nzchar(sc$hog)) span(class = "rnx-resume-hog", sc$hog),
            span(class = paste("coverage-badge", sc$level), sc$badge),
            if (!is.null(saved_at)) {
              span(class = "rnx-resume-at", paste("queried", format_time_ago(saved_at)))
            }
          ),
          div(
            class = "rnx-actions",
            tags$button(type = "button", class = "rnx-btn rnx-btn-primary",
                        `data-rnx-gene` = sc$gene,
                        icon("dna"), "Resume in Gene Explorer"),
            tags$button(type = "button", class = "rnx-btn rnx-btn-secondary",
                        id = "rnx-resume-comparative",
                        icon("layer-group"), "Comparative View"),
            tags$button(type = "button", class = "rnx-btn rnx-btn-secondary",
                        id = "rnx-resume-similarity",
                        icon("chart-line"), "Similar profiles"),
            tags$button(type = "button", class = "rnx-btn rnx-btn-tertiary",
                        id = "rnx-start-fresh", "Start fresh")
          ),
          div(class = "rnx-resume-divider"),
          div(class = "rnx-try",
              div(class = "rnx-eyebrow", "Or try"),
              div(class = "rnx-try-chips", chips))
        ),
        div(
          class = "rnx-loaded",
          div(class = "rnx-card-title", "What's loaded"),
          tags$table(class = "rnx-species", tags$tbody(rnx_species_rows())),
          div(class = "rnx-loaded-note", rnx_loaded_note()),
          tags$button(type = "button", class = "rnx-btn rnx-btn-secondary rnx-btn-block",
                      `data-rnx-tool` = "data_upload",
                      icon("upload"), "Upload your own dataset")
        )
      )
    )
  }

  #the sc multi-dataset sentence only makes sense for the bundled GRE data
  rnx_loaded_note <- function() {
    design <- current_study_design()
    levels <- condition_levels(design)
    axis <- tolower(condition_label(design))
    span_range <- if (length(levels) > 1) {
      sprintf("%ss run %s → %s.", axis, levels[1], levels[length(levels)])
    } else {
      sprintf("One %s level.", axis)
    }
    multi <- names(Filter(function(sp_code) {
      sp_data <- get_all_species_data()[[sp_code]]
      !is.null(sp_data) && length(grep("sample_info", names(sp_data), value = TRUE)) > 1L
    }, setNames(names(current_species_config()), names(current_species_config()))))

    if (!length(multi)) return(span_range)
    config <- current_species_config()
    tagList(
      span_range, " ",
      tags$em(config[[multi[1]]]$short),
      " can switch between the 2023 set, WT S288C 2026 and Δppx1 Δppn1 2026 in Plot settings."
    )
  }

  output$rnx_explorer_landing <- renderUI({
    if (!is.null(global_query_state$query_result)) return(NULL)
    sc <- rnx_scope()
    if (!is.null(sc) && !identical(sc$state, "not_found")) rnx_resume_card(sc) else rnx_launchpad()
  })

  # ---- resume actions ----

  #narrower than the clearSession handler on purpose: that one drops
  #rnacross_seen_version and reloads, which pops the What's New modal again
  rnx_reset_query <- function() {
    global_query_state$current_query <- NULL
    global_query_state$query_result <- NULL
    global_query_state$tree_data <- NULL
    global_query_state$last_status <- NULL
    global_query_state$restored_at <- NULL
    updateTextInput(session, "global_gene_query", value = "")
    shinyjs::hide("gene_explorer_results")
    session$sendCustomMessage("rnx_forget_query", list(rand = runif(1)))
  }

  observeEvent(input$rnx_start_fresh, {
    rnx_reset_query()
  })

  observeEvent(input$rnx_resume_jump, {
    gene <- global_query_state$current_query
    target <- input$rnx_resume_jump$target
    req(is.character(gene), nzchar(gene), is.character(target))

    if (identical(target, "Comparative View")) {
      updateTextInput(session, "combined_genename", value = gene)
      nav_select("nav", "Comparative View")
      shinyjs::delay(150, shinyjs::click("combined_search_button"))
    } else {
      #similarity needs a reference species chosen first, so only prefill
      updateTextInput(session, "similarity_gene_input", value = gene)
      nav_select("nav", "similarity_search")
    }
  })

  #the resume card's relative time comes from the restored session payload
  observeEvent(input$restore_session, {
    global_query_state$restored_at <- input$restore_session$saved_at
  })

  #?tab=&gene= deep links, for the palette's Cmd+Enter. Only fires when the url
  #actually carries them, so a normal load leaves restore_session_state alone.
  observeEvent(session$clientData$url_search, {
    qs <- parseQueryString(session$clientData$url_search %||% "")
    tab <- qs$tab
    gene <- qs$gene
    if (is.null(tab) && is.null(gene)) return()
    shinyjs::delay(900, {
      if (!is.null(gene) && nzchar(gene)) {
        updateTextInput(session, "global_gene_query", value = gene)
        nav_select("nav", tab %||% "gene_explorer")
        shinyjs::delay(150, shinyjs::click("global_search_button"))
      } else if (nzchar(tab)) {
        nav_select("nav", tab)
      }
    })
  }, once = TRUE)
}
