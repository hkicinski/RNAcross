# rnacross visualization outputs module
# composite visualization outputs and orchestrator functions
# dependencies: 02_constants_themes, 05_orthology_query, 08_visualization_heatmaps

#' Create ortholog mapping table
#'
#' Creates a DT datatable showing the ortholog mapping across species
#' for a given set of genes.
#'
#' @param gene_mapping List of gene mappings from extract_orthology_for_genes()
#' @param config Optional species configuration list
#' @return DT datatable with gene IDs per species
create_ortholog_table <- function(gene_mapping, config = NULL) {
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }

  # start with the gene column
  ortholog_df <- data.frame(
    Gene = sapply(gene_mapping, function(x) x$original),
    stringsAsFactors = FALSE
  )

  # dynamically add columns for each species in the config
  col_names <- c("Input Gene" = "Gene")

  for (sp_code in names(config)) {
    # add column for this species
    col_name <- paste0(toupper(sp_code), "_ID")
    ortholog_df[[col_name]] <- sapply(gene_mapping, function(x) {
      ifelse(is.null(x[[sp_code]]), "-", x[[sp_code]])
    })

    # add to column names mapping
    col_names[[config[[sp_code]]$name]] <- col_name
  }

  # create a datatable
  dt <- datatable(
    ortholog_df,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "tip"
    ),
    colnames = col_names,
    rownames = FALSE
  )

  return(dt)
}

#' Generate cross-species heatmap with ortholog table
#'
#' Main orchestrator function that coordinates the creation of a cross-species
#' heatmap visualization along with an ortholog mapping table. This function
#' combines multiple visualization components into a single output.
#'
#' @param gene_list Character vector of gene IDs or names
#' @param species_data_list List of species data structures
#' @param normalization Normalization method: "zscore" or "none"
#' @param is_dark_mode Logical for dark mode styling
#' @param cluster_rows Logical to cluster genes
#' @param cluster_cols Logical to cluster columns
#' @param config Optional species configuration list
#' @param all_species_data Optional global data object
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @param plot_settings Optional plot settings list
#' @return List with plot (plotly), table (DT), matrix, and error components
generate_cross_species_heatmap <- function(gene_list, species_data_list,
                                           normalization = "zscore",
                                           is_dark_mode = FALSE,
                                           cluster_rows = TRUE,
                                           cluster_cols = FALSE,
                                           config = NULL,
                                           all_species_data = NULL,
                                           transform_type = "lcpm",
                                           plot_settings = NULL) {
  # clean up gene list - remove empty strings, trim whitespace
  gene_list <- gene_list[gene_list != ""]
  gene_list <- trimws(gene_list)

  # map genes across species using HOGs
  gene_mapping <- extract_orthology_for_genes(gene_list, all_species_data, config)

  # stop if no genes were mapped
  if (length(gene_mapping) == 0) {
    return(list(
      plot = NULL,
      table = NULL,
      error = "No genes could be mapped across species"
    ))
  }

  # extract expression data
  expression_data <- extract_ortholog_expression(gene_mapping, species_data_list, config, transform_type)

  # prepare heatmap matrix
  heatmap_matrix <- prepare_heatmap_matrix(expression_data, normalization)

  # use species colors from plot_settings, fallback to palette
  dynamic_colors <- NULL
  if (!is.null(plot_settings) && length(plot_settings$species_colors) > 0) {
    dynamic_colors <- plot_settings$species_colors
  } else if (!is.null(config)) {
    species_list <- sapply(config, function(x) x$short)
    palette_name <- if (!is.null(plot_settings$species_palette)) plot_settings$species_palette else "Dark2"
    dynamic_colors <- derive_species_colors(species_list, palette_name, NULL, config)
  }

  heatmap_plot <- create_cross_species_heatmap(
    heatmap_matrix,
    is_dark_mode = is_dark_mode,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    config = config,
    species_colors_dynamic = dynamic_colors,
    plot_settings = plot_settings
  )

  # create ortholog table
  ortholog_table <- create_ortholog_table(gene_mapping, config)

  # return both the plot and table
  return(list(
    plot = heatmap_plot,
    table = ortholog_table,
    matrix = heatmap_matrix,
    error = NULL
  ))
}


#' Create orthogroup summary HTML
#'
#' Creates a summary HTML block showing composition of the orthogroup.
#'
#' @param query_result Query result object
#' @param config Optional species configuration list
#' @return HTML tag list
create_orthogroup_summary <- function(query_result, config = NULL) {
  if (is.null(query_result)) {
    return(NULL)
  }

  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG # Use default if not provided
  }

  # Count genes per species
  gene_counts <- sapply(query_result$genes_by_species, nrow)
  total_genes <- sum(gene_counts)

  # Create summary HTML
  summary_html <- tags$div(
    h5("Orthogroup Summary"),
    tags$ul(
      tags$li(paste("HOG ID:", query_result$orthogroup)),
      tags$li(paste("Total genes:", total_genes)),
      tags$li(paste("Species represented:", sum(gene_counts > 0))),
      tags$li(
        "Gene distribution:",
        tags$ul(
          lapply(names(gene_counts)[gene_counts > 0], function(sp) {
            species_name <- if (sp %in% names(config)) config[[sp]]$name else sp
            count <- gene_counts[sp]
            tags$li(paste0(species_name, ": ", count, " gene", if (count > 1) "s" else ""))
          })
        )
      )
    )
  )

  return(summary_html)
}

#' Create orthogroup details table
#'
#' Creates a formatted DT datatable showing details of all genes in the orthogroup.
#'
#' @param query_result Query result object
#' @param config Optional species configuration list
#' @return DT datatable object
create_orthogroup_details_table <- function(query_result, config = NULL) {
  if (is.null(query_result) || is.null(query_result$genes_by_species)) {
    return(NULL)
  }

  # use default config if not provided
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }

  # collect gene info using list collection
  gene_list <- lapply(names(query_result$genes_by_species), function(sp) {
    genes_df <- query_result$genes_by_species[[sp]]
    if (nrow(genes_df) > 0) {
      species_name <- if (!is.null(config[[sp]])) config[[sp]]$name else sp

      genes_df$Species <- species_name
      genes_df$SpeciesCode <- sp

      genes_df[, c("Species", "gene_id", "gene_name", "display"), drop = FALSE]
    } else {
      NULL
    }
  })

  gene_list <- Filter(Negate(is.null), gene_list)
  if (length(gene_list) == 0) {
    return(NULL)
  }

  all_genes <- rbindlist(gene_list, fill = TRUE)
  if (nrow(all_genes) == 0) {
    return(NULL)
  }

  # Create a nicely formatted datatable
  dt <- datatable(
    all_genes[, c("Species", "gene_id", "gene_name")],
    options = list(
      pageLength = 10,
      dom = "tip",
      ordering = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    ),
    colnames = c("Species", "Gene ID", "Gene Name"),
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-weight: bold;",
      paste(
        "Orthogroup:", query_result$orthogroup,
        "| Total genes:", nrow(all_genes)
      )
    )
  )

  return(dt)
}
