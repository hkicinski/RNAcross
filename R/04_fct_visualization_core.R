#visualization functions for plots and graphics

#helper function for hidden input
hiddenInput <- function(inputId, value) {
  tags$input(
    id = inputId,
    type = "hidden",
    value = value
  )
}
#01. Gene Plots 
#modified plot function to handle selected genes
create_gene_plot <- function(lc, gene, sample_info, species_name, is_dark_mode = FALSE, species_colors = NULL) {
  tryCatch({
    #try to find the gene with different formats (for K. lactis)
    gene_found <- FALSE
    gene_to_use <- gene
    
    #for K. lactis, try multiple formats
    if (grepl("^KLLA0", gene)) {
      possible_genes <- c(gene, gsub("_", "", gene), gsub("^(KLLA0)(.*)", "\\1_\\2", gene))
      for (pg in possible_genes) {
        if (pg %in% rownames(lc)) {
          gene_to_use <- pg
          gene_found <- TRUE
          break
        }
      }
    } else {
      gene_found <- gene %in% rownames(lc)
      gene_to_use <- gene
    }
    
    if(!gene_found) {
      #more informative error message
      available_example <- head(rownames(lc)[grepl(substr(gene, 1, 5), rownames(lc))], 3)
      error_msg <- paste("Gene ID", gene, "not found in expression data.")
      if (length(available_example) > 0) {
        error_msg <- paste(error_msg, "\nSimilar genes:", paste(available_example, collapse = ", "))
      }
      
      return(plotly_empty() %>% 
             add_annotations(text = error_msg,
                           showarrow = FALSE))
    }
    
    #get the appropriate color for this species
    species_color <- if(!is.null(species_colors) && species_name %in% names(species_colors)) {
      species_colors[[species_name]]
    } else {
      "#440154"  #default color
    }
    
    #normalize timepoint format (handle both "0min" and "0" formats)
    normalized_timepoints <- sample_info$Timepoint
    normalized_timepoints <- gsub("^(\\d+)$", "\\1min", normalized_timepoints)  #add "min" if missing
    normalized_timepoints <- gsub("^(\\d+)min$", "\\1min", normalized_timepoints)  #ensure format
    
    dt <- data.table(
      Sample = colnames(lc),
      Timepoint = factor(normalized_timepoints, levels = TIME_POINTS),
      Replicate = sample_info$Replicate,
      exn = as.numeric(lc[gene_to_use,])
    )
    
    plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
    text_color <- if(is_dark_mode) "white" else "black"
    grid_color <- if(is_dark_mode) "gray30" else "gray90"
    
    #custom color scale with two shades of the species color for any rep number
    n_replicates <- length(unique(dt$Replicate))
    if (n_replicates == 1) {
      replicate_colors <- species_color
    } else if (n_replicates == 2) {
      replicate_colors <- c(species_color, adjustcolor(species_color, alpha.f = 0.7))
    } else {
      #generate a gradient for more replicates
      alpha_values <- seq(1, 0.4, length.out = n_replicates)
      replicate_colors <- sapply(alpha_values, function(a) adjustcolor(species_color, alpha.f = a))
    }
    
    p <- ggplot(dt, aes(x = Timepoint, y = exn, color = factor(Replicate), group = Replicate)) +
      geom_point(size = 3) + 
      geom_line(linewidth = 1) +
      scale_color_manual(values = replicate_colors) +
      labs(
        y = "log2 count per million",
        title = paste("Expression of", gene, "in", species_name),
        color = "Replicate"
      ) +
      theme_minimal() +
      theme(
        text = element_text(color = text_color),
        axis.text.x = element_text(angle = 45, hjust = 1, color = text_color),
        axis.text.y = element_text(color = text_color),
        plot.title = element_text(size = 14, face = "bold", color = text_color),
        panel.grid.major = element_line(color = grid_color),
        panel.grid.minor = element_line(color = ifelse(is_dark_mode, "gray20", "gray95")),
        plot.background = element_rect(fill = plot_bg_color, color = NA),
        panel.background = element_rect(fill = plot_bg_color, color = NA),
        legend.background = element_rect(fill = plot_bg_color),
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color),
        axis.title = element_text(color = text_color)
      )
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color),
        hoverlabel = list(bgcolor = if(is_dark_mode) "#444" else "white"),
        showlegend = TRUE,
        margin = list(b = 100)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtons = list(
          list("zoom2d", "pan2d", "resetScale2d", "toImage")
        )
      )
  }, error = function(e) {
    plotly_empty() %>% 
      add_annotations(
        text = paste("Error creating plot:", e$message),
        showarrow = FALSE
      )
  })
}
#02. PCA Plot for Single Sps
#for the single-species PCA functionality
create_pca_plot <- function(expression_matrix, sample_info, is_dark_mode = FALSE) {
  #some debugs
  print("Debug: Starting PCA plot creation")
  print(paste("Expression matrix dimensions:", nrow(expression_matrix), "x", ncol(expression_matrix)))
  print(paste("Sample info rows:", nrow(sample_info)))
  
  #makes sure the user has a loaded exp matrix 
  if (!is.matrix(expression_matrix) && !is.data.frame(expression_matrix)) {
    stop("Expression matrix must be a matrix or data frame")
  }
  
  #converts to matrix if you fed a data frame
  if (is.data.frame(expression_matrix)) {
    expression_matrix <- as.matrix(expression_matrix)
  }
  
  #data filtering step
  print("Debug: Calculating gene variances")
  var_genes <- apply(expression_matrix, 1, var)
  expression_matrix <- expression_matrix[var_genes > 0, ]
  print(paste("Genes after variance filtering:", nrow(expression_matrix)))
  
  #transposes the matrix for PCA (samples as rows here)
  print("Debug: Transposing matrix")
  mat <- t(expression_matrix)
  print(paste("Transposed matrix dimensions:", nrow(mat), "x", ncol(mat)))
  
  #runs a PCA with centered transformation (exp-mu)
  print("Debug: Running PCA")
  pca_res <- prcomp(mat, scale. = FALSE, center = TRUE)
  
  #calculates variance
  var_explained <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
  print(paste("Variance explained PC1:", round(var_explained[1], 2), "%"))
  print(paste("Variance explained PC2:", round(var_explained[2], 2), "%"))
  
  #creates data frame for plotting
  print("Debug: Creating plot data frame")
  pca_data <- data.frame(
    PC1 = pca_res$x[, 1],
    PC2 = pca_res$x[, 2],
    Sample = rownames(pca_res$x)
  )
  
  #merge with sample information dataframe
  print("Debug: Merging with sample info")
  pca_data <- merge(pca_data, sample_info, by = "Sample", all.x = TRUE)
  print(paste("Final data frame rows:", nrow(pca_data)))
  
  #timepoints ordered along the legend
  pca_data$Timepoint <- factor(pca_data$Timepoint, levels = TIME_POINTS)
  
  #sets general plot styling
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  grid_color <- if(is_dark_mode) "gray30" else "gray90"
  
  #creates plots with viridis color scale for natural progression
  print("Debug: Creating ggplot with viridis colors")
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Timepoint, text = Sample)) +
    geom_point(size = 3) +
    #this is where viridis is
    scale_color_viridis(discrete = TRUE, option = "viridis") +
    labs(
      x = sprintf("PC1 (%.1f%%)", var_explained[1]),
      y = sprintf("PC2 (%.1f%%)", var_explained[2]),
      title = "PCA of Gene Expression Data"
    ) +
    theme_minimal() +
    theme(
      text = element_text(color = text_color),
      axis.text = element_text(color = text_color),
      plot.title = element_text(size = 14, face = "bold", color = text_color),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = if(is_dark_mode) "gray20" else "gray95"),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )
  
  #converts to plotly output
  print("Debug: Converting to plotly")
  plot <- plotly::ggplotly(p, tooltip = c("Sample", "Timepoint")) %>%
    layout(
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = plot_bg_color,
      font = list(color = text_color),
      showlegend = TRUE,
      legend = list(bgcolor = plot_bg_color),
      margin = list(t = 50, r = 20, b = 50, l = 50)
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtons = list(
        list("zoom2d", "pan2d", "resetScale2d", "toImage")
      )
    )
  
  print("Debug: PCA plot creation completed")
  return(plot)
}
#03. Multi-SPS PCA (UPDATE REQUIRED)
create_multi_species_pca <- function(get_species_data, is_dark_mode = FALSE) {
  
  print("Debug: Starting HOG-based species PCA with SINGLE-COPY GENES ONLY")
  
  #get current configuration and data
  config <- current_species_config()
  current_data <- get_all_species_data()
  
  #get data for all species
  species_data_list <- list()
  for (species_id in names(config)) {
    species_data_list[[species_id]] <- get_species_data(species_id)
  }
  
  #get HOG data
  og_data <- current_data$orthofinder$orthogroups
  
  #pre-compute HOG-to-gene lookup table
  print("Debug: Pre-computing HOG lookup table")
  lookup_table <- current_data$gene_lookup
  hog_gene_map <- og_data %>%
    left_join(lookup_table[, .(gene_id, species)], by = "gene_id") %>%
    filter(!is.na(species)) %>%
    mutate(species = as.character(species)) %>%
    filter(species != "unknown") %>%
    split(.$hog_id) %>%
    lapply(function(hog_df) {
      split(hog_df$gene_id, hog_df$species)
    })
  print(paste("Debug: Lookup table created with", length(hog_gene_map), "HOGs"))
  
  #filter for single-copy HOGs only
  print("Debug: Filtering for single-copy genes only")
  
  #find HOGs that have exactly one gene in each species that has representation
  single_copy_hogs <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
    #check if all species present have exactly 1 gene
    gene_counts <- sapply(hog, length)
    #must have at least 2 species and all must be single-copy
    length(gene_counts) >= 2 && all(gene_counts == 1)
  })]
  
  print(paste("Found", length(single_copy_hogs), "single-copy HOGs (with at least 2 species)"))
  
  #further filter to get stats on how many have all 4 species
  four_species_single <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
    length(hog) == 4 && all(sapply(hog, length) == 1)
  })]
  
  print(paste("  -", length(four_species_single), "HOGs have single-copy genes in all 4 species"))
  
  #get stats for different species combinations
  three_species_single <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
    length(hog) == 3 && all(sapply(hog, length) == 1)
  })]
  
  two_species_single <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
    length(hog) == 2 && all(sapply(hog, length) == 1)
  })]
  
  print(paste("  -", length(three_species_single), "HOGs have single-copy genes in exactly 3 species"))
  print(paste("  -", length(two_species_single), "HOGs have single-copy genes in exactly 2 species"))
  
  #use all single-copy HOGs
  common_hogs <- single_copy_hogs
  
  #check if we have any HOGs
  if (length(common_hogs) == 0) {
    error_msg <- "No single-copy HOGs found across species. Cannot perform PCA."
    showNotification(error_msg, type = "error")
    return(plotly_empty() %>% 
             add_annotations(text = error_msg, showarrow = FALSE))
  }
  
  #create sample metadata dynamically
  sample_metadata_list <- list()
  
  for (species_id in names(config)) {
    sp_data <- species_data_list[[species_id]]
    if (!is.null(sp_data)) {
      lcpm_matrix <- if (!is.null(sp_data$lcpm)) sp_data$lcpm else sp_data[[paste0(species_id, "_lcpm")]]
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
  
  print(paste("Total samples:", nrow(sample_metadata)))
  
  #create expression matrix based on single-copy HOGs
  sample_matrix <- matrix(NA, 
                          nrow = nrow(sample_metadata), 
                          ncol = length(common_hogs))
  rownames(sample_matrix) <- sample_metadata$Sample
  colnames(sample_matrix) <- common_hogs
  
  #track successful matches by species
  matches_by_species <- list(cg = 0, sc = 0, kl = 0, ca = 0)
  
  #fill the matrix with single-copy genes only
  print("Debug: Starting to fill expression matrix with single-copy genes")
  
  for (i in 1:nrow(sample_metadata)) {
    if (i %% 10 == 0) print(paste("Processing sample", i, "of", nrow(sample_metadata)))
    
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
    
    #dynamic lcpm matrix retrieval
    sp_data <- species_data_list[[species_code]]
    lcpm_matrix <- if (!is.null(sp_data$lcpm)) {
      sp_data$lcpm
    } else if (!is.null(sp_data[[paste0(species_code, "_lcpm")]])) {
      sp_data[[paste0(species_code, "_lcpm")]]
    } else {
      NULL
    }
    
    #debug: check matrix dimensions
    if (i == 1) {
      print(paste(species_code, "expression matrix has", nrow(lcpm_matrix), "genes"))
    }
    
    for (j in 1:length(common_hogs)) {
      hog <- common_hogs[j]
      
      #direct lookup
      if (!is.null(hog_gene_map[[hog]]) && !is.null(hog_gene_map[[hog]][[species_code]])) {
        gene_ids <- hog_gene_map[[hog]][[species_code]]
        
        #for single-copy analysis, we know there's exactly 1 gene if it exists
        if (length(gene_ids) == 1) {
          gene_id <- gene_ids[1]
          
          #handle K. lactis underscore format
          if (species_code == "kl" && !gene_id %in% rownames(lcpm_matrix)) {
            gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
            if (gene_id_alt %in% rownames(lcpm_matrix)) {
              gene_id <- gene_id_alt
            }
          }
          
          #check if gene exists in expression matrix
          if (gene_id %in% rownames(lcpm_matrix)) {
            sample_matrix[i, j] <- lcpm_matrix[gene_id, sample_name]
            matches_by_species[[species_code]] <- matches_by_species[[species_code]] + 1
          }
        }
      }
    }
  }
  
  #print match statistics
  print("Debug: Match statistics by species (single-copy genes only):")
  for (sp in names(matches_by_species)) {
    print(paste(sp, ":", matches_by_species[[sp]], "single-copy values filled"))
  }
  
  #check how many NAs we have
  total_nas <- sum(is.na(sample_matrix))
  total_cells <- nrow(sample_matrix) * ncol(sample_matrix)
  print(paste("Total NAs:", total_nas, "out of", total_cells, "cells (", 
              round(100 * total_nas / total_cells, 1), "%)"))
  
  #remove columns (HOGs) with too many NAs (more than 50% missing)
  na_proportion <- apply(sample_matrix, 2, function(x) sum(is.na(x)) / length(x))
  keep_cols <- na_proportion < 0.5
  
  print(paste("HOGs with >50% missing data:", sum(!keep_cols)))
  print(paste("HOGs with data in all samples:", sum(na_proportion == 0)))
  
  if (sum(keep_cols) < 2) {  #need at least 2 HOGs for PCA
    error_msg <- "Too few single-copy HOGs with sufficient data. Cannot perform PCA."
    showNotification(error_msg, type = "error")
    return(plotly_empty() %>% 
             add_annotations(text = error_msg, showarrow = FALSE))
  }
  
  sample_matrix <- sample_matrix[, keep_cols]
  print(paste("Using", sum(keep_cols), "single-copy HOGs for PCA"))
  
  #remove rows (samples) that have all NAs
  row_has_data <- apply(sample_matrix, 1, function(x) !all(is.na(x)))
  if (sum(row_has_data) < nrow(sample_matrix)) {
    print(paste("Removing", sum(!row_has_data), "samples with no data"))
    sample_matrix <- sample_matrix[row_has_data, ]
    sample_metadata <- sample_metadata[row_has_data, ]
  }
  
  #impute remaining NAs with column means
  print("Debug: Imputing missing values")
  imputed_count <- 0
  for (j in 1:ncol(sample_matrix)) {
    col_data <- sample_matrix[, j]
    if (any(is.na(col_data))) {
      col_mean <- mean(col_data, na.rm = TRUE)
      if (!is.na(col_mean)) {
        na_count <- sum(is.na(col_data))
        sample_matrix[is.na(col_data), j] <- col_mean
        imputed_count <- imputed_count + na_count
      }
    }
  }
  print(paste("Imputed", imputed_count, "missing values with column means"))
  
  #remove any columns that are all NA after imputation
  valid_cols <- apply(sample_matrix, 2, function(x) !all(is.na(x)))
  sample_matrix <- sample_matrix[, valid_cols]
  
  print(paste("Final matrix dimensions:", nrow(sample_matrix), "x", ncol(sample_matrix)))
  
  #final check before PCA
  if (ncol(sample_matrix) < 2 || nrow(sample_matrix) < 3) {
    showNotification("Insufficient valid data for PCA", type = "error")
    return(plotly_empty() %>% 
             add_annotations(text = "Not enough valid single-copy orthogroups for analysis",
                             showarrow = FALSE))
  }
  
  #run PCA
  print("Debug: Running PCA on single-copy HOG matrix")
  
  tryCatch({
    #run PCA with scaling and centering
    pca_result <- prcomp(sample_matrix, scale. = TRUE, center = TRUE)
    var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
    
    print(paste("PCA successful. PC1 explains", round(var_explained[1], 1), "% variance"))
    print(paste("PC2 explains", round(var_explained[2], 1), "% variance"))
    print(paste("First 5 PCs explain", round(sum(var_explained[1:5]), 1), "% total variance"))
    
    #create plotting data
    plot_data <- data.frame(
      PC1 = pca_result$x[, 1],
      PC2 = pca_result$x[, 2],
      Sample = rownames(pca_result$x)
    )
    
    #add metadata
    plot_data <- merge(plot_data, sample_metadata, by = "Sample")
    plot_data$Timepoint <- factor(plot_data$Timepoint, levels = TIME_POINTS)
    
    #set plot styling
    plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
    text_color <- if(is_dark_mode) "white" else "black"
    grid_color <- if(is_dark_mode) "gray30" else "gray90"
    
    #create plot
    p <- ggplot(plot_data, aes(x = PC1, y = PC2, 
                               color = Timepoint,
                               shape = Species,
                               text = paste0("Sample: ", Sample, 
                                             "<br>Species: ", Species, 
                                             "<br>Timepoint: ", Timepoint))) +
      geom_point(size = 4, alpha = 0.8) +
      scale_color_viridis(discrete = TRUE, option = "viridis") +
      scale_shape_manual(values = c(16, 17, 15, 18)) +
      labs(
        x = sprintf("PC1 (%.1f%%)", var_explained[1]),
        y = sprintf("PC2 (%.1f%%)", var_explained[2]),
        title = "PCA of HOG Expression Across Species (Single-Copy Genes Only)",
        subtitle = paste(ncol(sample_matrix), "single-copy HOGs compared across", 
                         length(unique(plot_data$Species)), "species")
      ) +
      theme_minimal() +
      theme(
        text = element_text(color = text_color),
        axis.text = element_text(color = text_color),
        plot.title = element_text(size = 14, face = "bold", color = text_color),
        plot.subtitle = element_text(size = 12, color = text_color),
        panel.grid.major = element_line(color = grid_color),
        panel.grid.minor = element_line(color = if(is_dark_mode) "gray20" else "gray95"),
        plot.background = element_rect(fill = plot_bg_color, color = NA),
        panel.background = element_rect(fill = plot_bg_color, color = NA),
        legend.background = element_rect(fill = plot_bg_color),
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color)
      )
    
    #store matrices data for download
    matrices_data <- list(
      sample_matrix = sample_matrix,
      sample_metadata = sample_metadata,
      common_hogs = colnames(sample_matrix),
      method = "single_copy_only",
      pca_result = pca_result,
      var_explained = var_explained
    )
    
    attr(p, "matrices_data") <- matrices_data
    
    #convert to plotly
    plot <- plotly::ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color),
        hoverlabel = list(
          bgcolor = if(is_dark_mode) "#444" else "white",
          bordercolor = "white"
        )
      ) %>%
      config(displayModeBar = TRUE)
    
    attr(plot, "matrices_data") <- matrices_data
    
    print("Debug: PCA plot created successfully")
    return(plot)
    
  }, error = function(e) {
    print(paste("Error in PCA:", e$message))
    print("Debug info:")
    print(paste("Matrix has", sum(!is.na(sample_matrix)), "non-NA values"))
    print(paste("Matrix variance:", var(as.vector(sample_matrix), na.rm = TRUE)))
    
    showNotification(paste("PCA failed:", e$message), type = "error")
    return(plotly_empty() %>% 
             add_annotations(text = paste("PCA Error:", e$message),
                             showarrow = FALSE))
  })
}
#04. Ridgeline Plots
#ridgeline plot functions remain unchanged
create_ridgeline_plot <- function(species_data_list, is_dark_mode = FALSE) {
  #prepare data frame for plotting
  all_density_data <- data.frame()
  
  for (species_id in names(species_data_list)) {
    species_data <- species_data_list[[species_id]]
    species_name <- switch(species_id,
                          "cg" = "C. glabrata",
                          "sc" = "S. cerevisiae",
                          "kl" = "K. lactis",
                          "ca" = "C. albicans")
    
    #extract expression data and sample info - handle different naming conventions
    expr_matrix <- if(species_id == "cg") species_data$lcpm else species_data[[paste0(species_id, "_lcpm")]]
    sample_info <- if(species_id == "cg") species_data$sample_info else species_data[[paste0(species_id, "_sample_info")]]
    
    #for each sample, extract expression values
    for (i in 1:nrow(sample_info)) {
      sample_name <- sample_info$Sample[i]
      timepoint <- sample_info$Timepoint[i]
      replicate <- sample_info$Replicate[i]
      
      #get expression values for this sample
      expr_values <- expr_matrix[, sample_name]
      
      #add to data frame
      sample_df <- data.frame(
        Species = species_name,
        Timepoint = timepoint,
        Replicate = replicate,
        Expression = expr_values
      )
      
      all_density_data <- rbind(all_density_data, sample_df)
    }
  }
  
  #convert timepoint to factor to preserve order
  all_density_data$Timepoint <- factor(all_density_data$Timepoint, 
                                      levels = TIME_POINTS)
  
  #set plot styling based on dark mode
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  grid_color <- if(is_dark_mode) "gray30" else "gray90"
  
  #create ridgeline plot with ggplot2 and ggridges
  p <- ggplot(all_density_data, 
             aes(x = Expression, y = Timepoint, fill = Timepoint)) +
    geom_density_ridges(scale = 2, alpha = 0.7, quantile_lines = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~ Species, scales = "free_y") +
    labs(
      title = "Gene Expression Distribution Across Timepoints",
      x = "log2 CPM",
      y = "Timepoint"
    ) +
    theme_minimal() +
    theme(
      text = element_text(color = text_color),
      axis.text = element_text(color = text_color),
      plot.title = element_text(size = 14, face = "bold", color = text_color),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = if(is_dark_mode) "gray20" else "gray95"),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )
  
  return(p)
}

#function to create gene count threshold plots
create_threshold_ridgeline <- function(species_data_list, threshold = 2, is_dark_mode = FALSE) {
  #prepare data frame for gene counts
  count_data <- data.frame()
  
  for (species_id in names(species_data_list)) {
    species_data <- species_data_list[[species_id]]
    species_name <- switch(species_id,
                          "cg" = "C. glabrata",
                          "sc" = "S. cerevisiae",
                          "kl" = "K. lactis",
                          "ca" = "C. albicans")
    
    #extract expression data and sample info - handle different naming conventions
    expr_matrix <- if(species_id == "cg") species_data$lcpm else species_data[[paste0(species_id, "_lcpm")]]
    sample_info <- if(species_id == "cg") species_data$sample_info else species_data[[paste0(species_id, "_sample_info")]]
    
    #for each sample, count genes above threshold
    for (i in 1:nrow(sample_info)) {
      sample_name <- sample_info$Sample[i]
      timepoint <- sample_info$Timepoint[i]
      replicate <- sample_info$Replicate[i]
      
      #get expression values and count those above threshold
      expr_values <- expr_matrix[, sample_name]
      genes_above <- sum(expr_values > threshold)
      percent_above <- (genes_above / length(expr_values)) * 100
      
      #add to data frame
      count_df <- data.frame(
        Species = species_name,
        Timepoint = timepoint,
        Replicate = replicate,
        GenesAbove = genes_above,
        PercentAbove = percent_above
      )
      
      count_data <- rbind(count_data, count_df)
    }
  }
  
  #convert timepoint to factor to preserve order
  count_data$Timepoint <- factor(count_data$Timepoint, levels = TIME_POINTS)
  
  #set plot styling based on dark mode
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  grid_color <- if(is_dark_mode) "gray30" else "gray90"
  
  #create bar plot
  p <- ggplot(count_data, 
             aes(x = Timepoint, y = PercentAbove, fill = Timepoint)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Species) +
    scale_fill_viridis(discrete = TRUE) +
    labs(
      title = paste("Percentage of Genes with Expression Above", threshold, "log2 CPM"),
      x = "Timepoint",
      y = "Percent of Genes (%)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(color = text_color),
      axis.text.x = element_text(angle = 45, hjust = 1, color = text_color),
      axis.text.y = element_text(color = text_color),
      plot.title = element_text(size = 14, face = "bold", color = text_color),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = if(is_dark_mode) "gray20" else "gray95"),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )
  
  return(p)
}
#05. Phylogenetic Tree visualization 
#ggtree module for phylo tree (react fixed)
create_phylo_tree_plot <- function(tree, orthogroup_genes, orthogroup_id, is_dark_mode = FALSE, current_data = NULL, species_colors = NULL, species_config = NULL) {
  if (is.null(tree)) {
    return(NULL)
  }
  
  #create a mapping of gene IDs to display names and species
  gene_mapping <- data.frame()
  for (sp in names(orthogroup_genes)) {
    if (nrow(orthogroup_genes[[sp]]) > 0) {
      genes_df <- orthogroup_genes[[sp]]
      genes_df$species_code <- sp
      gene_mapping <- rbind(gene_mapping, genes_df[, c("gene_id", "display", "species_code")])
    }
  }
  
  #create metadata dataframe for tips only
  tip_metadata <- data.frame(
    label = tree$tip.label,
    stringsAsFactors = FALSE
  )
  
  #add display_label and species columns
  tip_metadata$display_label <- NA_character_
  tip_metadata$species <- NA_character_
  
  for (i in 1:nrow(tip_metadata)) {
    current_gene_id <- tip_metadata$label[i]  #renamed to avoid confusion
    
    #use provided current_data or fall back to global
    if (is.null(current_data)) {
      current_data <- all_species_data  
    }
    
    #extract lookup table from current_data
    lookup_table <- current_data$gene_lookup
    
    #ensure it's a data.table for consistent subsetting
    if (!is.data.table(lookup_table)) {
      lookup_table <- as.data.table(lookup_table)
    }
    
    lookup_match <- lookup_table[gene_id == current_gene_id, ]
    
    if (nrow(lookup_match) > 0) {
      #found in lookup table
      sp_code <- as.character(lookup_match$species[1])
      
      #use passed config or fall back to DEFAULT_SPECIES_CONFIG
      config <- if(!is.null(species_config)) species_config else DEFAULT_SPECIES_CONFIG
      
      if (sp_code %in% names(config)) {
        #use SHORT name for consistency with dynamic species colors
        tip_metadata$species[i] <- config[[sp_code]]$short
        
        #build display label
        if (!is.na(lookup_match$gene_name[1]) && lookup_match$gene_name[1] != "") {
          tip_metadata$display_label[i] <- paste0(lookup_match$gene_name[1], " (", current_gene_id, ")")
        } else {
          tip_metadata$display_label[i] <- current_gene_id
        }
      } else {
        #species code not recognized
        tip_metadata$species[i] <- "Unknown"
        tip_metadata$display_label[i] <- current_gene_id
      }
    } else {
      #not in gene_lookup - mark as unknown
      tip_metadata$species[i] <- "Unknown"
      tip_metadata$display_label[i] <- current_gene_id
    }
  }
  
  #add colors for any species not in SPECIES_COLORS but present in the data
  unique_species <- unique(tip_metadata$species[!is.na(tip_metadata$species)])
  additional_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
                        "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F")
  color_index <- 1
  for (sp in unique_species) {
    if (!sp %in% names(species_colors)) {
      species_colors[[sp]] <- additional_colors[color_index]
      color_index <- (color_index %% length(additional_colors)) + 1
    }
  }
  
  #calculate dynamic parameters based on tree size
  n_tips <- length(tree$tip.label)
  max_label_length <- max(nchar(tip_metadata$display_label))
  
  #dynamic text size - smaller for larger trees
  text_size <- if(n_tips > 20) 2.5 else if(n_tips > 10) 3.0 else 3.5
  
  #dynamic x-limit to accommodate long labels
  x_limit <- 8 + (max_label_length / 10)
  
  #create the tree plot and add metadata
  p <- ggtree(tree, layout = "rectangular", branch.length = "none") %<+% tip_metadata +
    geom_tiplab(aes(label = display_label, color = species), 
                size = text_size, align = TRUE, linesize = 0.5) +
    geom_nodepoint(color = if(is_dark_mode) "#666" else "#999", 
                   alpha = 0.5, size = 2) +
    scale_color_manual(values = species_colors, name = "Species") +
    guides(color = guide_legend(override.aes = list(label = "●", size = 6, linetype = 0))) +
    theme_tree2() +
    xlim_tree(x_limit) + 
    theme(
      legend.position = "bottom",
      legend.box.background = element_rect(
        fill = if(is_dark_mode) "#2c3034" else "white",
        color = if(is_dark_mode) "#444" else "#ddd"
      ),
      plot.background = element_rect(
        fill = if(is_dark_mode) "#2c3034" else "white", 
        color = NA
      ),
      panel.background = element_rect(
        fill = if(is_dark_mode) "#2c3034" else "white", 
        color = NA
      ),
      legend.background = element_rect(
        fill = if(is_dark_mode) "#2c3034" else "white"
      ),
      legend.text = element_text(
        color = if(is_dark_mode) "white" else "black",
        size = 10
      ),
      legend.title = element_text(
        color = if(is_dark_mode) "white" else "black",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(
        color = if(is_dark_mode) "white" else "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      )
    ) +
    ggtitle(paste("Phylogenetic Tree for Orthogroup", orthogroup_id))
  
  return(p)
}

#function to load tree file
load_gene_tree <- function(og_id, current_data = NULL) {
  #access trees from the loaded data structure
  if (is.null(current_data)) {
    current_data <- all_species_data  #use global all_species_data as fallback
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