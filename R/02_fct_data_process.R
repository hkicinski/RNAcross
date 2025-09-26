#data processing and manipulation functions

#function to process gene group data for visualization
process_gene_group_data <- function(gene_groups, species_data, all_species_data, config, species_code) {
  plot_data <- data.frame()
  
  for (group_name in names(gene_groups)) {
    genes <- gene_groups[[group_name]]
    
    for (gene in genes) {
      #handle k. lactis underscore format
      if (species_code == "kl" && !gene %in% rownames(species_data$lcpm)) {
        gene_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene)
        if (gene_alt %in% rownames(species_data$lcpm)) {
          gene <- gene_alt
        }
      }
      
      if (gene %in% rownames(species_data$lcpm)) {
        expr_values <- species_data$lcpm[gene, ]
        
        #create data frame for this gene
        gene_data <- data.frame(
          Gene = gene,
          Group = group_name,
          Sample = colnames(species_data$lcpm),
          Timepoint = species_data$sample_info$Timepoint,
          Replicate = species_data$sample_info$Replicate,
          Expression = as.numeric(expr_values)
        )
        
        plot_data <- rbind(plot_data, gene_data)
      }
    }
  }
  
  #convert timepoint to factor
  plot_data$Timepoint <- factor(plot_data$Timepoint, levels = TIME_POINTS)
  
  return(plot_data)
}

#function to get species configuration
current_species_config <- function() {
  return(DEFAULT_SPECIES_CONFIG)
}

#function to generate dynamic colors for species
generate_species_colors <- function(species_list, base_colors = DEFAULT_SPECIES_COLORS) {
  colors <- base_colors
  color_palette <- DYNAMIC_COLOR_PALETTE
  color_index <- 1
  
  for (species in species_list) {
    if (!species %in% names(colors)) {
      colors[[species]] <- color_palette[color_index]
      color_index <- ((color_index - 1) %% length(color_palette)) + 1
    }
  }
  
  return(colors)
}

#function to validate uploaded data format
validate_upload_data <- function(data, data_type) {
  errors <- character()
  warnings <- character()
  
  if (data_type == "expression") {
    #check if numeric
    if (!all(sapply(data, is.numeric))) {
      errors <- c(errors, "Expression data must contain only numeric values")
    }
    
    #check for negative values
    if (any(data < 0, na.rm = TRUE)) {
      warnings <- c(warnings, "Expression data contains negative values")
    }
    
    #check dimensions
    if (nrow(data) < 100) {
      warnings <- c(warnings, "Expression data has fewer than 100 genes")
    }
    
    if (ncol(data) < 3) {
      errors <- c(errors, "Expression data must have at least 3 samples")
    }
  }
  
  if (data_type == "metadata") {
    #check required columns
    required_cols <- c("Sample", "Timepoint", "Replicate")
    missing_cols <- setdiff(required_cols, colnames(data))
    
    if (length(missing_cols) > 0) {
      errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#function to normalize expression data
normalize_expression <- function(expr_matrix, method = "none") {
  if (method == "none") {
    return(expr_matrix)
  }
  
  if (method == "log2") {
    #add pseudocount and log transform
    return(log2(expr_matrix + 1))
  }
  
  if (method == "zscore") {
    #z-score normalize each gene
    return(t(scale(t(expr_matrix))))
  }
  
  if (method == "quantile") {
    #quantile normalization would go here
    #requires preprocesscore or similar
    warning("Quantile normalization not implemented")
    return(expr_matrix)
  }
  
  return(expr_matrix)
}

#function to filter low expression genes
filter_low_expression <- function(expr_matrix, min_cpm = 1, min_samples = 2) {
  #calculate CPM if not already
  if (max(expr_matrix, na.rm = TRUE) > 100) {
    #assume raw counts, convert to CPM
    cpm_matrix <- sweep(expr_matrix, 2, colSums(expr_matrix), "/") * 1e6
  } else {
    cpm_matrix <- expr_matrix
  }
  
  #count samples above threshold
  above_threshold <- cpm_matrix >= min_cpm
  genes_to_keep <- rowSums(above_threshold) >= min_samples
  
  return(expr_matrix[genes_to_keep, ])
}

#function to merge orthogroup data with expression
merge_orthogroup_expression <- function(orthogroup_genes, species_data_list, config) {
  merged_data <- list()
  
  for (species_code in names(orthogroup_genes)) {
    if (species_code %in% names(species_data_list)) {
      species_data <- species_data_list[[species_code]]
      genes_df <- orthogroup_genes[[species_code]]
      
      if (nrow(genes_df) > 0) {
        #get expression matrix
        lcpm_matrix <- if(!is.null(species_data$lcpm)) {
          species_data$lcpm
        } else if(!is.null(species_data[[paste0(species_code, "_lcpm")]])) {
          species_data[[paste0(species_code, "_lcpm")]]
        } else {
          NULL
        }
        
        if (!is.null(lcpm_matrix)) {
          #extract expression for these genes
          gene_ids <- genes_df$gene_id
          available_genes <- gene_ids[gene_ids %in% rownames(lcpm_matrix)]
          
          if (length(available_genes) > 0) {
            expr_subset <- lcpm_matrix[available_genes, , drop = FALSE]
            merged_data[[species_code]] <- list(
              genes = genes_df,
              expression = expr_subset,
              sample_info = species_data$sample_info
            )
          }
        }
      }
    }
  }
  
  return(merged_data)
}

#function to calculate expression statistics
calculate_expression_stats <- function(expr_matrix, sample_info = NULL) {
  stats <- list(
    mean_expression = rowMeans(expr_matrix),
    median_expression = apply(expr_matrix, 1, median),
    sd_expression = apply(expr_matrix, 1, sd),
    cv_expression = apply(expr_matrix, 1, sd) / rowMeans(expr_matrix),
    min_expression = apply(expr_matrix, 1, min),
    max_expression = apply(expr_matrix, 1, max)
  )
  
  #if sample info provided, calculate per-condition stats
  if (!is.null(sample_info) && "Timepoint" %in% colnames(sample_info)) {
    timepoints <- unique(sample_info$Timepoint)
    
    for (tp in timepoints) {
      tp_samples <- sample_info$Sample[sample_info$Timepoint == tp]
      tp_expr <- expr_matrix[, tp_samples, drop = FALSE]
      
      stats[[paste0("mean_", tp)]] <- rowMeans(tp_expr)
      stats[[paste0("sd_", tp)]] <- apply(tp_expr, 1, sd)
    }
  }
  
  return(as.data.frame(stats))
}