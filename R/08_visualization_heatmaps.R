#rnacross visualization heatmaps module
#heatmap-specific visualization functions
#dependencies: 02_constants_themes, 04_data_io, 07_visualization_core

#' Calculate fold-change from baseline for pathway heatmap
#'
#' Computes log2 fold-change relative to the first timepoint (0min baseline).
#'
#' @param pathway_data data.frame with Pathway, Species, Timepoint, MeanExpression columns
#' @return data.frame with additional Log2FC column
calculate_pathway_foldchange <- function(pathway_data) {
  #ensure timepoint is a factor with correct order
  pathway_data$Timepoint <- factor(pathway_data$Timepoint, levels = TIME_POINTS)
  
  #calculate fold-change per pathway per species
  fc_data <- pathway_data %>%
    group_by(Pathway, Species) %>%
    arrange(Timepoint) %>%
    mutate(
      BaselineExpr = {
        baseline_vals <- MeanExpression[Timepoint == "0min"]
        if (length(baseline_vals) == 0 || all(is.na(baseline_vals))) {
          #fallback to first timepoint if 0min missing
          first(MeanExpression)
        } else {
          baseline_vals[1]
        }
      },
      FoldChange = MeanExpression - BaselineExpr,
      Log2FC = FoldChange
    ) %>%
    ungroup()
  
  return(fc_data)
}

#' Create pathway comparison heatmap
#'
#' Creates a multi-panel heatmap comparing pathway expression across species.
#'
#' @param pathway_data data.frame with Pathway, Species, Timepoint, MeanExpression columns
#' @param value_type Either "foldchange" or "absolute"
#' @param is_dark_mode Logical for dark mode styling
#' @param cluster_pathways Logical to cluster pathways by expression pattern
#' @param timepoint_mode Either "all" or "comparable" (intersection of timepoints)
#' @return Plotly subplot with heatmaps for each species
create_pathway_heatmap <- function(pathway_data, value_type = "foldchange", 
                                   is_dark_mode = FALSE, cluster_pathways = FALSE,
                                   timepoint_mode = "all") {
  #validate input
  if (is.null(pathway_data) || nrow(pathway_data) == 0) {
    return(plotly_empty() %>% 
             add_annotations(text = "No pathway data available", showarrow = FALSE))
  }
  
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  
  #prepare data based on value type
  if (value_type == "foldchange") {
    heatmap_data <- calculate_pathway_foldchange(pathway_data)
    value_col <- "Log2FC"
    colorbar_title <- "log2 Fold-Change"
    zmid_val <- 0
    zmin_val <- -3
    zmax_val <- 3
  } else {
    heatmap_data <- pathway_data
    value_col <- "MeanExpression"
    colorbar_title <- "Mean log2 CPM"
    zmid_val <- NULL
    zmin_val <- NULL
    zmax_val <- NULL
  }
  
  #validate value column exists
  if (!value_col %in% colnames(heatmap_data)) {
    return(plotly_empty() %>% 
             add_annotations(text = paste("Missing column:", value_col), showarrow = FALSE))
  }
  
  #get all unique pathways and species
  all_pathways <- unique(heatmap_data$Pathway)
  species_list <- unique(heatmap_data$Species)
  n_species <- length(species_list)
  n_pathways <- length(all_pathways)
  
  #validate we have data
  if (n_species == 0 || n_pathways == 0) {
    return(plotly_empty() %>% 
             add_annotations(text = "No pathways or species found in data", showarrow = FALSE))
  }
  
  #get timepoints present in data
  all_data_timepoints <- unique(as.character(heatmap_data$Timepoint))
  all_data_timepoints <- all_data_timepoints[all_data_timepoints %in% TIME_POINTS]
  all_data_timepoints <- TIME_POINTS[TIME_POINTS %in% all_data_timepoints]
  
  if (length(all_data_timepoints) == 0) {
    return(plotly_empty() %>% 
             add_annotations(text = "No valid timepoints found", showarrow = FALSE))
  }
  
  #determine which timepoints to display based on mode
  if (timepoint_mode == "comparable") {
    #find intersection of timepoints across all species
    timepoints_by_species <- lapply(species_list, function(sp) {
      sp_data <- heatmap_data[heatmap_data$Species == sp, ]
      unique(as.character(sp_data$Timepoint))
    })
    comparable_timepoints <- Reduce(intersect, timepoints_by_species)
    comparable_timepoints <- TIME_POINTS[TIME_POINTS %in% comparable_timepoints]
    
    if (length(comparable_timepoints) == 0) {
      return(plotly_empty() %>% 
               add_annotations(text = "No comparable timepoints across species", showarrow = FALSE))
    }
    
    data_timepoints <- comparable_timepoints
  } else {
    data_timepoints <- all_data_timepoints
  }
  
  #optional clustering of pathways (globally across all species)
  pathway_order <- all_pathways
  if (cluster_pathways && n_pathways > 1) {
    tryCatch({
      #aggregate to handle duplicates before pivot
      avg_data <- heatmap_data %>%
        group_by(Pathway, Timepoint) %>%
        summarise(AvgValue = mean(.data[[value_col]], na.rm = TRUE), .groups = 'drop')
      
      avg_matrix <- avg_data %>%
        pivot_wider(names_from = Timepoint, values_from = AvgValue, values_fn = mean) %>%
        as.data.frame()
      
      rownames(avg_matrix) <- avg_matrix$Pathway
      avg_matrix <- as.matrix(avg_matrix[, -1, drop = FALSE])
      
      if (nrow(avg_matrix) > 1 && ncol(avg_matrix) > 0) {
        avg_matrix[is.na(avg_matrix)] <- 0
        row_dist <- dist(avg_matrix)
        row_hc <- hclust(row_dist)
        pathway_order <- rownames(avg_matrix)[row_hc$order]
      }
    }, error = function(e) {
      #clustering failed, use original order
      pathway_order <- all_pathways
    })
  }
  
  #calculate dynamic grid layout
  grid_layout <- calculate_subplot_grid(n_species)
  
  #create subplot list
  subplot_list <- list()
  annotations <- list()
  
  for (i in seq_len(n_species)) {
    sp <- species_list[i]
    sp_data <- heatmap_data %>% filter(Species == sp)
    
    #aggregate duplicates before pivot (handle paralogs)
    sp_agg <- sp_data %>%
      group_by(Pathway, Timepoint) %>%
      summarise(Value = mean(.data[[value_col]], na.rm = TRUE), .groups = 'drop')
    
    #create matrix: pathways x timepoints
    heat_wide <- sp_agg %>%
      pivot_wider(names_from = Timepoint, values_from = Value, values_fn = mean)
    
    #ensure all pathways present
    missing_pathways <- setdiff(pathway_order, heat_wide$Pathway)
    if (length(missing_pathways) > 0) {
      missing_df <- data.table(Pathway = missing_pathways)
      missing_df[, (data_timepoints) := NA_real_]
      heat_wide <- rbindlist(list(as.data.table(heat_wide), missing_df), fill = TRUE)
    }
    
    #ensure all timepoints are present as columns
    for (tp in data_timepoints) {
      if (!tp %in% colnames(heat_wide)) {
        heat_wide[[tp]] <- NA_real_
      }
    }
    
    #convert to matrix with proper ordering
    heat_df <- as.data.frame(heat_wide)
    rownames(heat_df) <- heat_df$Pathway
    heat_matrix <- as.matrix(heat_df[, data_timepoints, drop = FALSE])
    
    #reorder rows to match pathway_order
    valid_pathways <- intersect(pathway_order, rownames(heat_matrix))
    if (length(valid_pathways) == 0) {
      heat_matrix <- matrix(NA, nrow = 1, ncol = length(data_timepoints))
      rownames(heat_matrix) <- "No Data"
      colnames(heat_matrix) <- data_timepoints
    } else {
      heat_matrix <- heat_matrix[valid_pathways, , drop = FALSE]
    }
    
    missing_mask <- is.na(heat_matrix)
    
    #validate matrix dimensions before creating hover text
    n_rows <- nrow(heat_matrix)
    n_cols <- ncol(heat_matrix)
    
    if (n_rows == 0 || n_cols == 0) {
      heat_matrix <- matrix(NA, nrow = 1, ncol = length(data_timepoints))
      rownames(heat_matrix) <- "No Data"
      colnames(heat_matrix) <- data_timepoints
      n_rows <- 1
      n_cols <- length(data_timepoints)
    }
    
    #create hover text matrix safely
    values_display <- ifelse(
      is.na(as.vector(heat_matrix)),
      "No data",
      round(as.vector(heat_matrix), 2)
    )
    
    hover_text <- matrix(
      paste0(
        "Species: ", sp,
        "<br>Pathway: ", rep(rownames(heat_matrix), times = n_cols),
        "<br>Timepoint: ", rep(colnames(heat_matrix), each = n_rows),
        "<br>Value: ", values_display
      ),
      nrow = n_rows,
      ncol = n_cols,
      byrow = FALSE
    )
    
    #determine y-axis label visibility based on grid position
    show_y_labels <- (i - 1) %% grid_layout$ncols == 0
    
    #convert NA to NaN to prevent plotly nacolor warning
    heat_matrix[is.na(heat_matrix)] <- NaN
    
    #create heatmap with shared colorscale
    p <- plot_ly(
      z = heat_matrix,
      x = colnames(heat_matrix),
      y = rownames(heat_matrix),
      type = "heatmap",
      colorscale = list(
        list(0, "#2166AC"),
        list(0.5, "#F7F7F7"),
        list(1, "#B2182B")
      ),
      zmid = zmid_val,
      zmin = zmin_val,
      zmax = zmax_val,
      showscale = (i == 1),
      hoverinfo = "text",
      text = hover_text,
      colorbar = if(i == 1) {
        list(
          title = list(text = colorbar_title, side = "right"),
          len = 0.7,
          thickness = 15,
          x = 1.02,
          y = 0.45,
          yanchor = "middle",
          xpad = 10
        )
      } else NULL
    )
    
    p <- p %>% layout(
      xaxis = list(
        title = "",
        tickangle = -45,
        tickfont = list(size = 9),
        showticklabels = TRUE,
        side = "bottom"
      ),
      yaxis = list(
        title = "",
        autorange = "reversed",
        tickfont = list(size = 10),
        showticklabels = show_y_labels
      ),
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = plot_bg_color,
      font = list(color = text_color),
      margin = list(l = if(show_y_labels) 120 else 20, r = 20, t = 50, b = 80)
    )
    
    subplot_list[[i]] <- p
    
    #use domain-relative coordinates for precise positioning
    axis_suffix <- if (i == 1) "" else as.character(i)
    
    annotations[[i]] <- list(
      text = paste0("<i>", sp, "</i>"),
      xref = paste0("x", axis_suffix, " domain"),
      yref = paste0("y", axis_suffix, " domain"),
      x = 0.5,
      y = 1.08,
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 14, color = text_color)
    )
  }
  
  #calculate row heights for subplot
  row_heights <- rep(1 / grid_layout$nrows, grid_layout$nrows)
  
  #combine into dynamic grid
  combined_plot <- subplot(
    subplot_list,
    nrows = grid_layout$nrows,
    shareX = FALSE,
    shareY = FALSE,
    margin = 0.12,
    heights = row_heights
  )
  
  combined_plot <- combined_plot %>% layout(
    title = list(
      text = paste("Pathway Comparison Across Species -", 
                   ifelse(value_type == "foldchange", "Fold-Change from Baseline", "Absolute Expression")),
      font = list(size = 16, color = text_color),
      y = 0.98
    ),
    annotations = annotations,
    showlegend = FALSE,
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color,
    font = list(color = text_color),
    margin = list(r = 100)
  )
  
  return(combined_plot)
}

#' Aggregate HOG expression across paralogs
#'
#' Aggregates expression values from multiple paralogs within a HOG
#' using various methods.
#'
#' @param expr_matrix Expression matrix (genes x samples) for paralogs
#' @param method Aggregation method: "mean", "median", "eigengene", 
#'   "max_expr", "max_var", or "var_weighted"
#' @return Numeric vector of aggregated expression per sample
aggregate_hog_expression <- function(expr_matrix, method = "eigengene") {
  #expr_matrix: genes (paralogs) x samples
  
  if (nrow(expr_matrix) == 1) {
    return(as.numeric(expr_matrix[1, ]))
  }
  
  if (nrow(expr_matrix) == 0) {
    return(NULL)
  }
  
  switch(method,
         "mean" = colMeans(expr_matrix, na.rm = TRUE),
         
         "median" = apply(expr_matrix, 2, median, na.rm = TRUE),
         
         "eigengene" = {
           #WGCNA-style: PC1 of expression matrix
           if (ncol(expr_matrix) < 2) {
             warning("eigengene requires multiple samples (received ", ncol(expr_matrix), 
                     " sample). falling back to mean.")
             return(colMeans(expr_matrix, na.rm = TRUE))
           }
           
           #center and scale each gene first
           expr_scaled <- t(scale(t(expr_matrix), center = TRUE, scale = TRUE))
           
           #remove any genes with zero variance
           gene_vars <- apply(expr_scaled, 1, var, na.rm = TRUE)
           if (sum(gene_vars > 0, na.rm = TRUE) == 0) {
             return(colMeans(expr_matrix, na.rm = TRUE))
           }
           expr_scaled <- expr_scaled[gene_vars > 0, , drop = FALSE]
           
           #check if we still have multiple genes after filtering
           if (nrow(expr_scaled) < 2) {
             warning("only one gene with variance. using that gene.")
             return(as.numeric(expr_scaled[1, ]))
           }
           
           #PCA on genes to get first PC as eigengene
           pca <- tryCatch({
             prcomp(t(expr_scaled), center = FALSE, scale. = FALSE)
           }, error = function(e) {
             warning("PCA failed: ", e$message, ". falling back to mean.")
             return(NULL)
           })
           
           if (is.null(pca)) {
             return(colMeans(expr_matrix, na.rm = TRUE))
           }
           
           eigengene <- pca$x[, 1]
           
           #orient eigengene to have positive correlation with mean expression
           mean_expr <- colMeans(expr_matrix, na.rm = TRUE)
           if (cor(eigengene, mean_expr, use = "complete.obs") < 0) {
             eigengene <- -eigengene
           }
           
           #store variance explained as attribute
           var_explained <- (pca$sdev[1]^2 / sum(pca$sdev^2)) * 100
           attr(eigengene, "var_explained") <- var_explained
           attr(eigengene, "n_paralogs") <- nrow(expr_matrix)
           
           eigengene
         },
         
         "max_expr" = {
           #paralog with highest mean expression
           mean_exprs <- rowMeans(expr_matrix, na.rm = TRUE)
           best_idx <- which.max(mean_exprs)
           as.numeric(expr_matrix[best_idx, ])
         },
         
         "max_var" = {
           #paralog with highest variance
           gene_vars <- apply(expr_matrix, 1, var, na.rm = TRUE)
           best_idx <- which.max(gene_vars)
           as.numeric(expr_matrix[best_idx, ])
         },
         
         "var_weighted" = {
           #variance-weighted mean
           gene_vars <- apply(expr_matrix, 1, var, na.rm = TRUE)
           if (sum(gene_vars, na.rm = TRUE) == 0) {
             return(colMeans(expr_matrix, na.rm = TRUE))
           }
           weights <- gene_vars / sum(gene_vars, na.rm = TRUE)
           colSums(expr_matrix * weights, na.rm = TRUE)
         },
         
         stop("Unknown aggregation method")
  )
}

#' Extract ortholog expression across species
#'
#' Extracts expression data for orthologous genes across multiple species
#' based on a gene mapping.
#'
#' @param gene_mapping List of gene mappings from extract_orthology_for_genes()
#' @param species_data_list List of species data structures
#' @param config Optional species configuration list
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @return data.frame with Gene, OriginalID, Species, Timepoint, Replicate, Expression
extract_ortholog_expression <- function(gene_mapping, species_data_list, config = NULL, transform_type = "lcpm") {
  #prepare data structure to hold all expression values
  expression_data <- list()
  
  #get species configuration if not provided
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }
  
  #for each mapped gene, extract expression from all species
  for (i in 1:length(gene_mapping)) {
    gene_map <- gene_mapping[[i]]
    gene_name <- gene_map$original
    
    #get expression for each species
    expression_by_species <- list()
    
    for (species_code in names(species_data_list)) {
      #skip non-species entries
      if (species_code %in% c("orthofinder", "metadata", "gene_lookup", "phylo_trees")) {
        next
      }
      
      #check if this species has the gene in the mapping
      if (!is.null(gene_map[[species_code]])) {
        gene_id <- gene_map[[species_code]]
        
        #get expression matrix based on transformation
        expr_matrix <- get_expression_matrix(species_code, transform_type, species_data_list[[species_code]])
        
        #get sample info - try direct access first, then prefixed
        sample_info <- species_data_list[[species_code]]$sample_info
        if (is.null(sample_info)) {
          sample_info <- species_data_list[[species_code]][[paste0(species_code, "_sample_info")]]
        }
        
        if (is.null(expr_matrix) || is.null(sample_info)) {
          next
        }
        
        #handle K. lactis underscore issue (only for default data)
        if (species_code == "kl" && !is.null(gene_id)) {
          gene_id_with_underscore <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
          if (gene_id_with_underscore %in% rownames(expr_matrix)) {
            gene_id <- gene_id_with_underscore
          }
        }
        
        if (gene_id %in% rownames(expr_matrix)) {
          #get species display name
          species_display <- if (species_code %in% names(config)) {
            config[[species_code]]$short
          } else if (species_code %in% names(DEFAULT_SPECIES_CONFIG)) {
            DEFAULT_SPECIES_CONFIG[[species_code]]$short
          } else {
            species_code  
          }
          
          #ensure timepoints are correctly matched with expression values
          if ("Sample" %in% colnames(sample_info)) {
            expression_values <- as.numeric(expr_matrix[gene_id, sample_info$Sample])
          } else {
            expression_values <- as.numeric(expr_matrix[gene_id, ])
          }
          
          expr_df <- data.frame(
            Gene = gene_name,
            OriginalID = gene_id,
            Species = species_display,
            Timepoint = sample_info$Timepoint,
            Replicate = sample_info$Replicate,
            Expression = expression_values,
            stringsAsFactors = FALSE
          )
          expression_by_species[[species_code]] <- expr_df
        }
      }
    }
    #combine all species data for this gene
    if (length(expression_by_species) > 0) {
      combined_expr <- rbindlist(expression_by_species, fill = TRUE)
      if (nrow(combined_expr) > 0) {
        expression_data[[gene_name]] <- combined_expr
      }
    }
  }
  
  #merge all gene data
  if (length(expression_data) > 0) {
    all_expression <- rbindlist(expression_data, fill = TRUE)
    return(as.data.frame(all_expression))
  } else {
    return(data.frame(
      Gene = character(0),
      OriginalID = character(0),
      Species = character(0),
      Timepoint = character(0),
      Replicate = integer(0),
      Expression = numeric(0)
    ))
  }
}

#' Prepare heatmap matrix from expression data
#'
#' Converts long-format expression data to a matrix suitable for heatmap
#' visualization, with optional z-score normalization.
#'
#' @param expression_data data.frame with Gene, Species, Timepoint, Expression columns
#' @param normalization Normalization method: "zscore" or "none"
#' @return Numeric matrix (genes x species-timepoints)
prepare_heatmap_matrix <- function(expression_data, normalization = "zscore") {
  #ensure timepoints are properly ordered
  expression_data$Timepoint <- factor(expression_data$Timepoint, levels = TIME_POINTS)
  
  #average replicates
  avg_data <- expression_data %>%
    group_by(Gene, Species, Timepoint) %>%
    summarise(AvgExpression = mean(Expression, na.rm = TRUE), .groups = 'drop')
  
  #create a unique column ID for each species-timepoint combination
  avg_data$SpeciesTimepoint <- paste(avg_data$Species, avg_data$Timepoint, sep = "_")
  
  #create a wide format matrix
  heatmap_matrix <- avg_data %>%
    select(Gene, SpeciesTimepoint, AvgExpression) %>%
    pivot_wider(names_from = SpeciesTimepoint, values_from = AvgExpression)
  
  #convert to matrix format
  genes <- heatmap_matrix$Gene
  hm_matrix <- as.matrix(heatmap_matrix[, -1])
  rownames(hm_matrix) <- genes
  
  #apply normalization if requested
  if (normalization == "zscore") {
    #check for genes with zero variance before normalization
    gene_vars <- apply(hm_matrix, 1, var)
    const_genes <- which(gene_vars == 0)
    
    if (length(const_genes) > 0) {
      showNotification(
        paste("Note:", length(const_genes), "genes have constant expression and will be shown at z-score = 0"),
        type = "info",
        duration = 5
      )
    }
    
    #apply z-score normalization to each gene (row) separately
    hm_matrix <- t(scale(t(hm_matrix)))
    
    #replace any NaN or Inf values with 0 (genes with no variance)
    hm_matrix[!is.finite(hm_matrix)] <- 0
  }
  
  return(hm_matrix)
}

#' Create cross-species heatmap visualization
#'
#' Creates a complex heatmap with species annotation bar and gene expression.
#'
#' @param heatmap_matrix Numeric matrix (genes x species-timepoints)
#' @param is_dark_mode Logical for dark mode styling
#' @param cluster_rows Logical to cluster genes
#' @param cluster_cols Logical to cluster columns
#' @param config Optional species configuration list
#' @param species_colors_dynamic Optional named list of species colors
#' @param plot_settings Optional plot settings list
#' @return Plotly combined plot with annotation bar and heatmap
create_cross_species_heatmap <- function(heatmap_matrix, is_dark_mode = FALSE, 
                                         cluster_rows = TRUE, cluster_cols = FALSE,
                                         config = NULL, species_colors_dynamic = NULL,
                                         plot_settings = NULL) {
  #set color scheme based on dark mode
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  
  #define column groups (for species)
  column_names <- colnames(heatmap_matrix)
  species_names <- sapply(strsplit(column_names, "_"), function(x) x[1])
  
  #get unique species for dynamic color assignment
  unique_species <- unique(species_names)
  
  #create species colors mapping using the actual species names from columns
  if (!is.null(species_colors_dynamic)) {
    species_colors <- list()
    
    for (sp_name in unique_species) {
      if (sp_name %in% names(species_colors_dynamic)) {
        species_colors[[sp_name]] <- species_colors_dynamic[[sp_name]]
      } else {
        found_color <- FALSE
        for (provided_name in names(species_colors_dynamic)) {
          if (grepl(sp_name, provided_name, ignore.case = TRUE) || 
              grepl(provided_name, sp_name, ignore.case = TRUE)) {
            species_colors[[sp_name]] <- species_colors_dynamic[[provided_name]]
            found_color <- TRUE
            break
          }
        }
        
        if (!found_color) {
          if (FALSE) {
          } else {
            default_palette <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
            idx <- (which(unique_species == sp_name) - 1) %% length(default_palette) + 1
            species_colors[[sp_name]] <- default_palette[idx]
          }
        }
      }
    }
  } else {
    palette_colors <- get_palette_colors("Dark2", length(unique_species))
    species_colors <- as.list(setNames(palette_colors, unique_species))
  }
  
  #build species mapping dynamically from config if provided
  if (!is.null(config)) {
    species_mapping <- list()
    for (sp in unique_species) {
      found <- FALSE
      for (sp_code in names(config)) {
        if (config[[sp_code]]$short == sp || config[[sp_code]]$name == sp) {
          species_mapping[[sp]] <- config[[sp_code]]$name
          found <- TRUE
          break
        }
      }
      if (!found) {
        species_mapping[[sp]] <- sp
      }
    }
  } else {
    species_mapping <- setNames(unique_species, unique_species)
  }
  
  #create a one-row matrix for the species annotation bar
  unique_species <- unique(species_names)
  species_to_num <- setNames(seq_along(unique_species), unique_species)
  species_matrix <- matrix(sapply(species_names, function(s) species_to_num[[s]]), nrow = 1)
  colnames(species_matrix) <- column_names
  rownames(species_matrix) <- "Species"
  
  #custom colorscale for species bar using settings colors
  colorscale <- lapply(seq_along(unique_species), function(i) {
    species <- unique_species[i]
    
    fallback_palette <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4",
                          "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F")
    species_color <- if (species %in% names(species_colors)) {
      species_colors[[species]]
    } else {
      fallback_palette[((i - 1) %% length(fallback_palette)) + 1]
    }
    
    return(list(
      (i-1)/(max(1, length(unique_species)-1)),
      species_color
    ))
  })
  
  colorscale_flat <- unlist(colorscale, recursive = FALSE)
  
  #create the species annotation bar
  discrete_colorscale <- list()
  for (i in seq_along(unique_species)) {
    val_start <- (i - 1) / length(unique_species)
    val_end <- i / length(unique_species)
    color <- colorscale[[i]][[2]]
    
    discrete_colorscale[[length(discrete_colorscale) + 1]] <- list(val_start, color)
    discrete_colorscale[[length(discrete_colorscale) + 1]] <- list(val_end, color)
  }
  
  p1 <- plot_ly() %>%
    add_heatmap(
      z = species_matrix,
      x = column_names,
      y = c(""),
      showscale = FALSE,
      colorscale = discrete_colorscale,
      zmin = 0.5,
      zmax = length(unique_species) + 0.5,
      hoverinfo = "text",
      text = matrix(paste("Species:", species_names), nrow = 1)
    ) %>%
    layout(
      xaxis = list(
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      margin = list(l = 100, r = 50, b = 0, t = 10),
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = plot_bg_color
    )
  
  #convert NA to NaN to prevent plotly nacolor warning
  heatmap_matrix[is.na(heatmap_matrix)] <- NaN
  
  #create hover text before NaN conversion affects display
  hover_values <- ifelse(is.nan(heatmap_matrix), "No data", round(heatmap_matrix, 2))
  hover_text_matrix <- matrix(
    paste(
      "Gene:", rep(rownames(heatmap_matrix), each = ncol(heatmap_matrix)),
      "<br>Species-Timepoint:", rep(column_names, times = nrow(heatmap_matrix)),
      "<br>Value:", as.vector(t(hover_values))
    ),
    nrow = nrow(heatmap_matrix),
    ncol = ncol(heatmap_matrix),
    byrow = TRUE
  )
  
  #get heatmap palette from settings or use default
  hm_colorscale <- if (!is.null(plot_settings) && !is.null(plot_settings$heatmap_palette)) {
    plot_settings$heatmap_palette
  } else {
    "viridis"
  }
  
  p2 <- plot_ly(
    z = heatmap_matrix,
    x = column_names,
    y = rownames(heatmap_matrix),
    type = "heatmap",
    colorscale = hm_colorscale,  
    zmin = -3, 
    zmax = 3,
    zmid = 0,
    hoverinfo = "text",
    text = hover_text_matrix,
    showscale = TRUE,
    colorbar = list(
      title = "Z-score",
      len = 0.6,
      thickness = 20,
      y = 1.1,
      x = 0.878,
      outlinewidth = 1
    )
  )
  
  
  #build legend data from actual unique species in the data
  legend_data <- data.frame(
    species_code = unique_species,
    species_name = unique_species,
    value = seq_along(unique_species),
    stringsAsFactors = FALSE
  )
  
  #create the legend heatmap
  legend_plot <- plot_ly(
    height = 100 * length(unique_species)
  )
  
  #extract colors from the colorscale for annotation bar
  annotation_colors <- list()
  for (i in seq_along(colorscale)) {
    species <- unique_species[i]
    color <- colorscale[[i]][[2]]
    annotation_colors[[species]] <- color
  }
  
  #add separate scatter traces for each species with custom markers
  for (i in 1:nrow(legend_data)) {
    species_code <- legend_data$species_code[i]
    species_name <- legend_data$species_name[i]
    
    species_index <- which(unique_species == species_code)
    if (length(species_index) > 0) {
      marker_color <- colorscale[[species_index[1]]][[2]]
    } else {
      marker_color <- "#808080"
    }
    
    legend_plot <- legend_plot %>% add_trace(
      x = c(0.5),
      y = c(nrow(legend_data) - i + 1),
      type = "scatter",
      mode = "markers",
      marker = list(
        symbol = "square",
        size = 15,
        color = marker_color,
        line = list(color = "black", width = 1)
      ),
      text = species_code,
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    legend_plot <- legend_plot %>% add_annotations(
      x = 0.9,
      y = nrow(legend_data) - i + 1,
      text = species_code, 
      showarrow = FALSE,
      xanchor = "left"
    )
  }
  
  #configure the legend plot layout
  legend_plot <- legend_plot %>% layout(
    xaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      range = c(0, 2)
    ),
    yaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      range = c(0, nrow(legend_data) + 1)
    ),
    margin = list(l = 0, r = 0, t = 30, b = 0),
    title = list(text = "Species", x = 0.5),
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color
  )
  
  #main heatmap layout
  p2 <- p2 %>% layout(
    xaxis = list(
      title = "Species-Timepoint",
      tickangle = -45,
      tickfont = list(size = 10),
      showticklabels = TRUE
    ),
    yaxis = list(
      title = "Gene",
      autorange = "reversed",
      tickmode = "array", 
      tickvals = 0:(length(rownames(heatmap_matrix))-1),
      ticktext = rownames(heatmap_matrix),
      tickfont = list(size = 10)
    ),
    margin = list(l = 120, r = 100, b = 200, t = 0),
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color,
    coloraxis = list(colorscale = "viridis")  
  )
  
  #annotation bar and main heatmap combined
  main_plot <- subplot(
    style(p1, showscale = FALSE), p2, 
    nrows = 2,
    heights = c(0.05, 0.95),
    shareX = TRUE, 
    titleY = FALSE
  )
  
  #create a dummy plot to act as a spacer
  spacer_plot <- plot_ly() %>%
    layout(
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
  
  #stack the spacer and legend vertically
  legend_column <- subplot(
    spacer_plot, legend_plot,
    nrows = 2,
    heights = c(0.45, 0.55)
  )
  
  combined_plot <- subplot(
    main_plot, legend_column,
    widths = c(0.85, 0.15),
    titleX = TRUE,
    titleY = TRUE
  ) %>% layout(
    title = list(
      text = "Cross-Species Gene Expression Heatmap",
      x = 0.45  
    ),
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color,
    font = list(color = text_color),
    margin = list(l = 100, r = 100, b = 50, t = 50),
    showlegend = FALSE
  )
  
  return(combined_plot)
}

