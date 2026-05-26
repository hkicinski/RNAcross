# rnacross data processing module
# data processing and transformation functions
# dependencies: 04_data_io, 05_orthology_query

#' Process gene group data with HOG support
#'
#' Extracts expression data for a group of genes, using flexible gene queries
#' to find genes across species with HOG support.
#'
#' @param gene_groups data.frame with group_name and group_member columns
#' @param species_data Species-specific data (lcpm, anno, sample_info)
#' @param all_species_data Full data structure
#' @param config Species configuration list
#' @param species_code Character species code
#' @return data.table of expression data or NULL if no data found
process_gene_group_data <- function(gene_groups, species_data, all_species_data, config, species_code, transform_type = "lcpm") {
  # remove existing UI elements for combined selections
  for (sp_id in names(config)) {
    removeUI(
      selector = paste0("#combined_", sp_id, "_selection_ui > *"),
      multiple = TRUE,
      immediate = TRUE
    )
  }

  # get expression matrix based on transform type
  expr_matrix <- get_expression_matrix(species_code, transform_type, species_data)
  if (is.null(expr_matrix)) {
    # fallback to lcpm if requested transform not found
    expr_matrix <- species_data$lcpm
  }

  # extract expression data for each gene
  expression_data <- lapply(unique(gene_groups$group_member), function(gene) {
    # use the flexible query to find the gene
    gene_result <- query_gene_flexible(gene, species_data, all_species_data)

    if (!is.null(gene_result) && !is.null(gene_result$gene_id)) {
      # check if the gene exists in the current species
      if (species_code %in% names(gene_result$genes_by_species)) {
        species_genes <- gene_result$genes_by_species[[species_code]]
        if (nrow(species_genes) > 0) {
          # try to find the exact match first
          gene_id <- NULL
          gene_upper <- toupper(trimws(gene))

          # check if the queried gene itself is in the species genes
          for (i in 1:nrow(species_genes)) {
            # check both gene_id and gene_name for exact match
            if (toupper(species_genes$gene_id[i]) == gene_upper ||
              toupper(species_genes$gene_name[i]) == gene_upper) {
              gene_id <- species_genes$gene_id[i]
              break
            }
          }

          # no exact match found, fall back to first gene
          if (is.null(gene_id)) {
            gene_id <- species_genes$gene_id[1]
          }

          # handle K. lactis underscore format
          if (species_code == "kl" && !gene_id %in% rownames(expr_matrix)) {
            gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
            if (gene_id_alt %in% rownames(expr_matrix)) {
              gene_id <- gene_id_alt
            }
          }

          if (gene_id %in% rownames(expr_matrix)) {
            data.frame(
              Gene = gene,
              Group = gene_groups$group_name[gene_groups$group_member == gene],
              Species = config[[species_code]]$short,
              SpeciesCode = species_code,
              Timepoint = factor(species_data$sample_info$Timepoint, levels = TIME_POINTS),
              Replicate = species_data$sample_info$Replicate,
              Expression = as.numeric(expr_matrix[gene_id, ]),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  })

  # combine all data
  filtered_data <- Filter(Negate(is.null), expression_data)
  if (length(filtered_data) > 0) {
    rbindlist(filtered_data, fill = TRUE)
  } else {
    NULL
  }
}

#' Aggregate gene expression to species means
#'
#' Calculates mean expression per species per timepoint.
#'
#' @param plot_data data.frame with Species, Timepoint, Replicate, Expression columns
#' @return data.frame with aggregated expression values
aggregate_to_species_mean <- function(plot_data) {
  # calculate mean per species per timepoint per replicate
  aggregated <- plot_data %>%
    group_by(Species, Timepoint, Replicate) %>%
    summarise(
      Expression = mean(Expression, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Gene = paste0(Species, "_mean"))

  return(aggregated)
}

#' Process pathway comparison across multiple species
#'
#' Maps genes to orthologs and extracts expression data for pathway analysis.
#'
#' @param pathway_definitions Named list of pathway gene lists
#' @param species_data_list List of species data structures
#' @param config Species configuration list
#' @param all_species_data Full data structure
#' @return data.frame with pathway expression summaries, gene details as attribute
process_pathway_comparison <- function(pathway_definitions, species_data_list, config, all_species_data) {
  pathway_results <- list()
  gene_details_list <- list()
  detail_idx <- 1

  for (pathway_name in names(pathway_definitions)) {
    gene_list <- pathway_definitions[[pathway_name]]

    # map genes to orthologs
    gene_mapping <- extract_orthology_for_genes(gene_list, all_species_data, config)

    if (is.null(gene_mapping) || length(gene_mapping) == 0) {
      next
    }

    # extract expression data
    pathway_expr <- process_multi_species_gene_set(gene_mapping, species_data_list, config)

    if (is.null(pathway_expr) || nrow(pathway_expr) == 0) {
      next
    }

    # store gene details per species for this pathway
    species_in_data <- unique(pathway_expr$Species)
    for (sp in species_in_data) {
      sp_expr <- pathway_expr[pathway_expr$Species == sp, ]
      gene_details_list[[detail_idx]] <- data.frame(
        Pathway = pathway_name,
        Species = sp,
        Genes = paste(sort(unique(sp_expr$Gene)), collapse = ", "),
        GeneIDs = paste(sort(unique(sp_expr$GeneID)), collapse = ", "),
        stringsAsFactors = FALSE
      )
      detail_idx <- detail_idx + 1
    }

    # calculate mean expression per species per timepoint
    pathway_summary <- pathway_expr %>%
      group_by(Species, Timepoint) %>%
      summarise(
        MeanExpression = mean(Expression, na.rm = TRUE),
        SEExpression = sd(Expression, na.rm = TRUE) / sqrt(n()),
        NGenes = n_distinct(Gene),
        .groups = "drop"
      ) %>%
      mutate(Pathway = pathway_name)

    pathway_results[[pathway_name]] <- pathway_summary
  }

  if (length(pathway_results) == 0) {
    return(NULL)
  }

  # combine all pathway data
  combined_data <- rbindlist(pathway_results, fill = TRUE)
  combined_genes <- if (length(gene_details_list) > 0) {
    rbindlist(gene_details_list, fill = TRUE)
  } else {
    data.frame(
      Pathway = character(), Species = character(),
      Genes = character(), GeneIDs = character()
    )
  }

  # attach gene details as attribute
  result <- as.data.frame(combined_data)
  attr(result, "gene_details") <- as.data.frame(combined_genes)
  return(result)
}

#' Process single species pathway data
#'
#' Extracts pathway expression data for a single species.
#'
#' @param pathway_definitions Named list of pathway gene lists
#' @param species_data Species-specific data structure
#' @param species_name Display name for species
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @return data.frame with pathway expression summaries
process_single_species_pathway <- function(pathway_definitions, species_data, species_name, transform_type = "lcpm") {
  pathway_results <- list()
  gene_details <- list()

  # get expression matrix
  lcpm_matrix <- if (!is.null(species_data$lcpm)) {
    species_data$lcpm
  } else {
    # try prefixed version
    prefix_names <- names(species_data)[grepl("_lcpm$", names(species_data))]
    if (length(prefix_names) > 0) {
      species_data[[prefix_names[1]]]
    } else {
      NULL
    }
  }

  sample_info <- if (!is.null(species_data$sample_info)) {
    species_data$sample_info
  } else {
    prefix_names <- names(species_data)[grepl("_sample_info$", names(species_data))]
    if (length(prefix_names) > 0) {
      species_data[[prefix_names[1]]]
    } else {
      NULL
    }
  }

  anno <- if (!is.null(species_data$anno)) {
    species_data$anno
  } else {
    prefix_names <- names(species_data)[grepl("_anno$", names(species_data))]
    if (length(prefix_names) > 0) {
      species_data[[prefix_names[1]]]
    } else {
      NULL
    }
  }

  if (is.null(lcpm_matrix) || is.null(sample_info)) {
    return(NULL)
  }

  for (pathway_name in names(pathway_definitions)) {
    gene_list <- pathway_definitions[[pathway_name]]
    expression_data <- list()

    for (gene in gene_list) {
      gene_upper <- toupper(trimws(gene))

      # find gene in matrix (by ID or name via anno)
      gene_id <- NULL

      # direct match in rownames
      if (gene_upper %in% toupper(rownames(lcpm_matrix))) {
        idx <- which(toupper(rownames(lcpm_matrix)) == gene_upper)
        gene_id <- rownames(lcpm_matrix)[idx[1]]
      }

      # match via annotation
      if (is.null(gene_id) && !is.null(anno)) {
        if ("GeneName" %in% colnames(anno)) {
          anno_match <- which(toupper(anno$GeneName) == gene_upper)
          if (length(anno_match) > 0 && "GeneID" %in% colnames(anno)) {
            potential_id <- anno$GeneID[anno_match[1]]
            if (potential_id %in% rownames(lcpm_matrix)) {
              gene_id <- potential_id
            }
          }
        }
        if (is.null(gene_id) && "GeneID" %in% colnames(anno)) {
          anno_match <- which(toupper(anno$GeneID) == gene_upper)
          if (length(anno_match) > 0) {
            potential_id <- anno$GeneID[anno_match[1]]
            if (potential_id %in% rownames(lcpm_matrix)) {
              gene_id <- potential_id
            }
          }
        }
      }

      if (!is.null(gene_id) && gene_id %in% rownames(lcpm_matrix)) {
        expr_df <- data.frame(
          Gene = gene,
          GeneID = gene_id,
          Species = species_name,
          Timepoint = factor(sample_info$Timepoint, levels = TIME_POINTS),
          Replicate = sample_info$Replicate,
          Expression = as.numeric(lcpm_matrix[gene_id, ]),
          stringsAsFactors = FALSE
        )
        expression_data[[length(expression_data) + 1]] <- expr_df
      }
    }

    if (length(expression_data) == 0) next

    pathway_expr <- rbindlist(expression_data, fill = TRUE)

    # calculate mean expression per timepoint
    pathway_summary <- pathway_expr %>%
      group_by(Species, Timepoint) %>%
      summarise(
        MeanExpression = mean(Expression, na.rm = TRUE),
        SEExpression = sd(Expression, na.rm = TRUE) / sqrt(n()),
        NGenes = n_distinct(Gene),
        .groups = "drop"
      ) %>%
      mutate(Pathway = pathway_name)

    pathway_results[[pathway_name]] <- pathway_summary
  }

  if (length(pathway_results) == 0) {
    return(NULL)
  }

  combined_data <- rbindlist(pathway_results, fill = TRUE)
  combined_genes <- rbindlist(gene_details, fill = TRUE)

  result <- as.data.frame(combined_data)
  attr(result, "gene_details") <- as.data.frame(combined_genes)
  return(result)
}

#' Calculate fold-change from baseline for pathway heatmap
#'
#' Calculates fold-change relative to t=0 for each pathway and species.
#'
#' @param pathway_data data.frame with Pathway, Species, Timepoint, MeanExpression columns
#' @return data.frame with FoldChange and Log2FC columns added
calculate_pathway_foldchange <- function(pathway_data) {
  # ensure timepoint is a factor with correct order
  pathway_data$Timepoint <- factor(pathway_data$Timepoint, levels = TIME_POINTS)

  # calculate fold-change per pathway per species
  fc_data <- pathway_data %>%
    group_by(Pathway, Species) %>%
    arrange(Timepoint) %>%
    mutate(
      BaselineExpr = {
        baseline_vals <- MeanExpression[Timepoint == "0min"]
        if (length(baseline_vals) == 0 || all(is.na(baseline_vals))) {
          # fallback to first timepoint if 0min missing
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

#' Aggregate HOG expression across paralogs
#'
#' Aggregates expression values from multiple paralogs within a HOG
#' using various methods (mean, median, eigengene, etc.).
#'
#' @param expr_matrix Matrix with genes (paralogs) as rows, samples as columns
#' @param method Aggregation method: "mean", "median", "eigengene", "max_expr", "max_var", "var_weighted"
#' @return Numeric vector of aggregated expression values (one per sample)
aggregate_hog_expression <- function(expr_matrix, method = "eigengene") {
  # single gene case
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
      # WGCNA-style: PC1 of expression matrix
      # validate we have multiple samples for eigengene computation
      if (ncol(expr_matrix) < 2) {
        warning(
          "eigengene requires multiple samples (received ", ncol(expr_matrix),
          " sample). falling back to mean."
        )
        return(colMeans(expr_matrix, na.rm = TRUE))
      }

      # center and scale each gene first
      expr_scaled <- t(scale(t(expr_matrix), center = TRUE, scale = TRUE))

      # remove any genes with zero variance
      gene_vars <- apply(expr_scaled, 1, var, na.rm = TRUE)
      if (sum(gene_vars > 0, na.rm = TRUE) == 0) {
        return(colMeans(expr_matrix, na.rm = TRUE))
      }
      expr_scaled <- expr_scaled[gene_vars > 0, , drop = FALSE]

      # check if we still have multiple genes after filtering
      if (nrow(expr_scaled) < 2) {
        warning("only one gene with variance. using that gene.")
        return(as.numeric(expr_scaled[1, ]))
      }

      # PCA on genes to get first PC as eigengene
      pca <- tryCatch(
        {
          prcomp(t(expr_scaled), center = FALSE, scale. = FALSE)
        },
        error = function(e) {
          warning("PCA failed: ", e$message, ". falling back to mean.")
          return(NULL)
        }
      )

      if (is.null(pca)) {
        return(colMeans(expr_matrix, na.rm = TRUE))
      }

      eigengene <- pca$x[, 1]

      # orient eigengene to have positive correlation with mean expression
      mean_expr <- colMeans(expr_matrix, na.rm = TRUE)
      if (cor(eigengene, mean_expr, use = "complete.obs") < 0) {
        eigengene <- -eigengene
      }

      # store variance explained as attribute
      var_explained <- (pca$sdev[1]^2 / sum(pca$sdev^2)) * 100
      attr(eigengene, "var_explained") <- var_explained
      attr(eigengene, "n_paralogs") <- nrow(expr_matrix)

      eigengene
    },
    "max_expr" = {
      # paralog with highest mean expression
      mean_exprs <- rowMeans(expr_matrix, na.rm = TRUE)
      best_idx <- which.max(mean_exprs)
      as.numeric(expr_matrix[best_idx, ])
    },
    "max_var" = {
      # paralog with highest variance
      gene_vars <- apply(expr_matrix, 1, var, na.rm = TRUE)
      best_idx <- which.max(gene_vars)
      as.numeric(expr_matrix[best_idx, ])
    },
    "var_weighted" = {
      # variance-weighted mean
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

#' Pre-aggregate HOG expression for a species
#'
#' Pre-computes aggregated expression values for all HOGs in a species
#' across all samples.
#'
#' @param lcpm_matrix Expression matrix (genes x samples)
#' @param species_hog_map Named list mapping HOG IDs to gene IDs
#' @param common_hogs Character vector of HOG IDs to process
#' @param aggregation_method Aggregation method for paralogs
#' @param species_code Optional species code for K. lactis handling
#' @return Named list with aggregated expression vectors per HOG
preaggregate_species_hogs <- function(lcpm_matrix, species_hog_map, common_hogs,
                                      aggregation_method = "eigengene", species_code = NULL) {
  # returns named list: hog_expr[[hog_id]] = vector of expression values (one per sample)

  hog_expr <- list()

  for (hog in common_hogs) {
    gene_ids <- species_hog_map[[hog]]

    if (is.null(gene_ids) || length(gene_ids) == 0) {
      hog_expr[[hog]] <- NULL
      next
    }

    # try alternative gene ID formats
    gene_ids_to_use <- sapply(gene_ids, function(gene_id) {
      if (gene_id %in% rownames(lcpm_matrix)) {
        return(gene_id)
      }
      # try removing transcript suffix
      gene_id_alt <- sub("(\\.[0-9]+)$", "", gene_id)
      if (gene_id_alt %in% rownames(lcpm_matrix)) {
        return(gene_id_alt)
      }
      # try K. lactis underscore pattern
      if (!is.null(species_code) && species_code == "kl") {
        gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
        if (gene_id_alt %in% rownames(lcpm_matrix)) {
          return(gene_id_alt)
        }
      }
      return(gene_id)
    })

    # get valid genes
    valid_genes <- gene_ids_to_use[gene_ids_to_use %in% rownames(lcpm_matrix)]

    if (length(valid_genes) == 0) {
      hog_expr[[hog]] <- NULL
    } else if (length(valid_genes) == 1) {
      # single-copy: extract directly
      hog_expr[[hog]] <- lcpm_matrix[valid_genes[1], ]
    } else {
      # multi-copy: aggregate across ALL samples at once
      paralog_expr <- lcpm_matrix[valid_genes, , drop = FALSE]
      aggregated <- aggregate_hog_expression(paralog_expr, method = aggregation_method)
      hog_expr[[hog]] <- aggregated
    }
  }

  return(hog_expr)
}

#' Extract ortholog expression data
#'
#' Extracts expression data for mapped orthologs across multiple species.
#'
#' @param gene_mapping List of gene mappings from extract_orthology_for_genes
#' @param species_data_list List of species data structures
#' @param config Species configuration list
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @return data.frame with expression data for all genes across species
extract_ortholog_expression <- function(gene_mapping, species_data_list, config = NULL, transform_type = "lcpm") {
  # prepare data structure to hold all expression values
  expression_data <- list()

  # get species configuration if not provided
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }

  # for each mapped gene, extract expression from all species
  for (i in 1:length(gene_mapping)) {
    gene_map <- gene_mapping[[i]]
    gene_name <- gene_map$original

    # get expression for each species
    expression_by_species <- list()

    for (species_code in names(species_data_list)) {
      # skip non-species entries
      if (species_code %in% c("orthofinder", "metadata", "gene_lookup", "phylo_trees")) {
        next
      }

      # check if this species has the gene in the mapping
      if (!is.null(gene_map[[species_code]])) {
        gene_id <- gene_map[[species_code]]

        # get expression matrix based on transformation
        expr_matrix <- get_expression_matrix(species_code, transform_type, species_data_list[[species_code]])

        # get sample info - try direct access first, then prefixed
        sample_info <- species_data_list[[species_code]]$sample_info
        if (is.null(sample_info)) {
          sample_info <- species_data_list[[species_code]][[paste0(species_code, "_sample_info")]]
        }

        if (is.null(expr_matrix) || is.null(sample_info)) {
          next
        }

        # handle K. lactis underscore issue
        if (species_code == "kl" && !is.null(gene_id)) {
          gene_id_with_underscore <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
          if (gene_id_with_underscore %in% rownames(expr_matrix)) {
            gene_id <- gene_id_with_underscore
          }
        }

        if (gene_id %in% rownames(expr_matrix)) {
          # get species display name
          species_display <- if (species_code %in% names(config)) {
            config[[species_code]]$short
          } else if (species_code %in% names(DEFAULT_SPECIES_CONFIG)) {
            DEFAULT_SPECIES_CONFIG[[species_code]]$short
          } else {
            species_code
          }

          # ensure timepoints are correctly matched with expression values
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
    # combine all species data for this gene
    if (length(expression_by_species) > 0) {
      combined_expr <- rbindlist(expression_by_species, fill = TRUE)
      if (nrow(combined_expr) > 0) {
        expression_data[[gene_name]] <- combined_expr
      }
    }
  }

  # merge all gene data
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
#' Creates a matrix suitable for heatmap visualization, with optional z-score normalization.
#'
#' @param expression_data data.frame with Gene, Species, Timepoint, Expression columns
#' @param normalization Normalization method ("zscore" or "none")
#' @return Matrix with genes as rows, species-timepoint combinations as columns
prepare_heatmap_matrix <- function(expression_data, normalization = "zscore") {
  # ensure timepoints are properly ordered
  expression_data$Timepoint <- factor(expression_data$Timepoint, levels = TIME_POINTS)

  # average replicates
  avg_data <- expression_data %>%
    group_by(Gene, Species, Timepoint) %>%
    summarise(AvgExpression = mean(Expression, na.rm = TRUE), .groups = "drop")

  # create a unique column ID for each species-timepoint combination
  avg_data$SpeciesTimepoint <- paste(avg_data$Species, avg_data$Timepoint, sep = "_")

  # create a wide format matrix
  heatmap_matrix <- avg_data %>%
    select(Gene, SpeciesTimepoint, AvgExpression) %>%
    pivot_wider(names_from = SpeciesTimepoint, values_from = AvgExpression)

  # convert to matrix format
  genes <- heatmap_matrix$Gene
  hm_matrix <- as.matrix(heatmap_matrix[, -1])
  rownames(hm_matrix) <- genes

  # apply normalization if requested
  if (normalization == "zscore") {
    # check for genes with zero variance before normalization
    gene_vars <- apply(hm_matrix, 1, var)
    const_genes <- which(gene_vars == 0)

    if (length(const_genes) > 0) {
      showNotification(
        paste("Note:", length(const_genes), "genes have constant expression and will be shown at z-score = 0"),
        type = "info",
        duration = 5
      )
    }

    # apply z-score normalization to each gene (row) separately
    hm_matrix <- t(scale(t(hm_matrix)))

    # replace any NaN or Inf values with 0 (genes with no variance)
    hm_matrix[!is.finite(hm_matrix)] <- 0
  }

  return(hm_matrix)
}

#' Process gene set for multi-species ortholog analysis
#'
#' Extracts expression data for a mapped gene set across multiple species.
#'
#' @param gene_mapping List of gene mappings from extract_orthology_for_genes
#' @param species_data_list List of species data structures
#' @param config Species configuration list
#' @return data.frame with expression data or NULL if no data found
process_multi_species_gene_set <- function(gene_mapping, species_data_list, config, transform_type = "lcpm") {
  all_expression_data <- list()

  for (gene_map in gene_mapping) {
    input_gene <- gene_map$original

    for (sp_code in names(config)) {
      if (!is.null(gene_map[[sp_code]]) && length(gene_map[[sp_code]]) > 0) {
        # get species data
        sp_data <- species_data_list[[sp_code]]
        if (is.null(sp_data)) next

        # get expression matrix using helper
        lcpm_matrix <- get_expression_matrix(sp_code, transform_type, sp_data)
        if (is.null(lcpm_matrix)) {
          # fallback to lcpm direct access
          lcpm_matrix <- if (!is.null(sp_data$lcpm)) {
            sp_data$lcpm
          } else {
            sp_data[[paste0(sp_code, "_lcpm")]]
          }
        }

        # get sample info
        sample_info <- if (!is.null(sp_data$sample_info)) {
          sp_data$sample_info
        } else {
          sp_data[[paste0(sp_code, "_sample_info")]]
        }

        if (is.null(lcpm_matrix) || is.null(sample_info)) next

        # for each ortholog in this species
        for (gene_id in gene_map[[sp_code]]) {
          # handle K. lactis underscore format
          gene_id_to_use <- gene_id
          if (sp_code == "kl" && !gene_id %in% rownames(lcpm_matrix)) {
            gene_id_alt <- gsub("^(KLLA0)(.*)", "\\1_\\2", gene_id)
            if (gene_id_alt %in% rownames(lcpm_matrix)) {
              gene_id_to_use <- gene_id_alt
            }
          }

          if (gene_id_to_use %in% rownames(lcpm_matrix)) {
            expr_df <- data.frame(
              Gene = input_gene,
              GeneID = gene_id,
              Species = config[[sp_code]]$short,
              SpeciesCode = sp_code,
              Timepoint = factor(sample_info$Timepoint, levels = TIME_POINTS),
              Replicate = sample_info$Replicate,
              Expression = as.numeric(lcpm_matrix[gene_id_to_use, ]),
              stringsAsFactors = FALSE
            )

            all_expression_data[[length(all_expression_data) + 1]] <- expr_df
          }
        }
      }
    }
  }

  if (length(all_expression_data) > 0) {
    return(as.data.frame(rbindlist(all_expression_data, fill = TRUE)))
  } else {
    return(NULL)
  }
}

# --- Gene Similarity Shape-Search Service Functions ---

#' Convert character timepoints to numeric minutes
#' @param tp Character vector of timepoints (e.g. '15min', '1.5h')
#' @return Numeric vector of minutes
tp_to_minutes <- function(tp) {
  sapply(tp, function(x) {
    if (grepl("min$", x)) return(as.numeric(gsub("min$", "", x)))
    if (grepl("h$", x)) return(as.numeric(gsub("h$", "", x)) * 60)
    NA_real_
  }, USE.NAMES = FALSE)
}

#' Build wide z-scored consensus matrix
#' @param mat Expression matrix
#' @param sample_info Sample info dataframe
#' @return Data frame with gene_id as rownames and timepoints as columns
build_consensus_wide <- function(mat, sample_info) {
  tp_order <- unique(sample_info$Timepoint)
  mat_long <- mat %>%
    as.data.frame() %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(-gene_id, names_to = "Sample", values_to = "expression") %>%
    left_join(sample_info, by = "Sample")
  z_rep <- mat_long %>%
    group_by(gene_id, Replicate) %>%
    mutate(
      expr_sd = sd(expression, na.rm = TRUE),
      z_score = if_else(is.na(expr_sd) | expr_sd == 0, 0,
                        (expression - mean(expression, na.rm = TRUE)) / expr_sd)
    ) %>%
    ungroup()
  consensus <- z_rep %>%
    group_by(gene_id, Timepoint) %>%
    summarise(consensus_z = mean(z_score, na.rm = TRUE), .groups = "drop")
  consensus$Timepoint <- factor(consensus$Timepoint, levels = tp_order)
  consensus %>%
    pivot_wider(names_from = Timepoint, values_from = consensus_z) %>%
    column_to_rownames("gene_id")
}

#' Filter invariant genes
#' @param consensus_wide Wide matrix
#' @return Matrix with invariant rows removed
filter_invariant <- function(consensus_wide) {
  row_sds <- apply(consensus_wide, 1, sd, na.rm = TRUE)
  consensus_wide[!is.na(row_sds) & row_sds > 0, , drop = FALSE]
}

#' Run unified gene similarity search
#' Handles both single-species and cross-species cases
#' @param query_gene_name User's query gene string
#' @param ref_species Reference species ID (e.g. 'sc')
#' @param tgt_species Target species ID (e.g. 'cg')
#' @param top_x Number of top matches to return
#' @param all_species_data Global RNAcross data object
#' @return List containing table, plot_data, query_id, and interpolation_gaps (if any)
run_similarity_search <- function(query_gene_name, ref_species, tgt_species, top_x, all_species_data) {
  n_null <- 1000
  n_perm <- 1000
  set.seed(2026)
  
  get_sp_data <- function(sp) {
    sp_data <- all_species_data[[sp]]
    mat <- if (!is.null(sp_data[["rlog"]])) sp_data[["rlog"]] else sp_data[[paste0(sp, "_rlog")]]
    info <- if (!is.null(sp_data[["sample_info"]])) sp_data[["sample_info"]] else sp_data[[paste0(sp, "_sample_info")]]
    list(mat = mat, sample_info = info)
  }
  
  ref_data <- get_sp_data(ref_species)
  ref_consensus_wide <- build_consensus_wide(ref_data$mat, ref_data$sample_info)
  
  gene_lookup <- as.data.table(all_species_data$gene_lookup)
  lookup_match <- gene_lookup[species == ref_species & toupper(gene_name) == toupper(query_gene_name)]
  
  if (nrow(lookup_match) == 0) {
    if (toupper(query_gene_name) %in% toupper(rownames(ref_data$mat))) {
      query_gene_id <- rownames(ref_data$mat)[toupper(rownames(ref_data$mat)) == toupper(query_gene_name)][1]
    } else {
      stop(paste("Query gene", query_gene_name, "not found in reference species", ref_species))
    }
  } else {
    query_gene_id <- lookup_match$gene_id[1]
    if (!query_gene_id %in% rownames(ref_data$mat)) {
      stop(paste("Query gene mapped to", query_gene_id, "but not found in expression matrix"))
    }
  }
  
  ref_tp_labels <- colnames(ref_consensus_wide)
  ref_tp_mins <- tp_to_minutes(ref_tp_labels)
  ref_query_profile <- as.numeric(ref_consensus_wide[query_gene_id, ])
  
  tgt_data <- get_sp_data(tgt_species)
  tgt_consensus_wide <- build_consensus_wide(tgt_data$mat, tgt_data$sample_info)
  tgt_tp_labels <- colnames(tgt_consensus_wide)
  tgt_tp_mins <- tp_to_minutes(tgt_tp_labels)
  
  grids_match <- identical(sort(ref_tp_mins), sort(tgt_tp_mins))
  interp_gaps <- NULL
  
  if (grids_match) {
    aligned_query <- ref_query_profile[match(tgt_tp_mins, ref_tp_mins)]
  } else {
    aligned <- approx(x = ref_tp_mins, y = ref_query_profile, xout = tgt_tp_mins, method = "linear")
    aligned_query <- aligned$y
    interp_gaps <- sapply(tgt_tp_mins, function(t) {
      if (t %in% ref_tp_mins) return(0)
      lower <- max(ref_tp_mins[ref_tp_mins < t])
      upper <- min(ref_tp_mins[ref_tp_mins > t])
      upper - lower
    })
  }
  
  tgt_filtered <- filter_invariant(tgt_consensus_wide)
  
  cors <- apply(tgt_filtered, 1, function(x) {
    cor(x, aligned_query, method = "pearson", use = "pairwise.complete.obs")
  })
  
  cor_df <- tibble(gene_id = names(cors), pearson_r = cors) %>%
    left_join(
      gene_lookup %>% filter(species == tgt_species) %>% select(gene_id, gene_name) %>% distinct(gene_id, .keep_all = TRUE),
      by = "gene_id"
    ) %>%
    filter(!is.na(pearson_r)) %>%
    filter(!(ref_species == tgt_species & gene_id == query_gene_id)) %>%
    arrange(desc(pearson_r))
    
  null_sample_idx <- sample(seq_len(nrow(cor_df)), min(n_null, nrow(cor_df)))
  null_cors <- cor_df$pearson_r[null_sample_idx]
  
  top_matches <- head(cor_df, top_x)
  top_matches$null_percentile <- sapply(top_matches$pearson_r, function(r) {
    round(mean(null_cors <= r) * 100, 2)
  })
  
  top_matches$perm_p_value <- NA_real_
  for (j in seq_len(nrow(top_matches))) {
    cand_vec <- as.numeric(tgt_filtered[top_matches$gene_id[j], ])
    obs_cor <- top_matches$pearson_r[j]
    perm_cors <- replicate(n_perm, {
      cor(sample(cand_vec), aligned_query, method = "pearson", use = "pairwise.complete.obs")
    })
    top_matches$perm_p_value[j] <- mean(perm_cors >= obs_cor)
  }
  
  top_matches <- top_matches %>%
    select(gene_id, gene_name, pearson_r, null_percentile, perm_p_value) %>%
    mutate(
      pearson_r = round(pearson_r, 4),
      perm_p_value = round(perm_p_value, 4)
    )
  
  top_plot_ids <- top_matches$gene_id
  tgt_long <- tgt_filtered[top_plot_ids, , drop = FALSE] %>%
    as.data.frame() %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(-gene_id, names_to = "Timepoint", values_to = "consensus_z")
  tgt_long$label <- tgt_long$gene_id
  for (i in seq_len(nrow(tgt_long))) {
    gn <- cor_df$gene_name[cor_df$gene_id == tgt_long$gene_id[i]]
    if (length(gn) > 0 && !is.na(gn[1]) && gn[1] != "") {
      tgt_long$label[i] <- paste0(gn[1], " (", tgt_long$gene_id[i], ")")
    }
  }
  
  ref_plot <- data.frame(
    Timepoint = tgt_tp_labels,
    consensus_z = aligned_query,
    gene_id = "query",
    label = paste0(query_gene_name, " (query)")
  )
  
  plot_data <- bind_rows(tgt_long, ref_plot)
  chrono_order <- tgt_tp_labels[order(tgt_tp_mins)]
  plot_data$Timepoint <- factor(plot_data$Timepoint, levels = chrono_order)
  plot_data$type <- ifelse(plot_data$gene_id == "query", "query", "match")
  
  list(
    table = top_matches,
    plot_data = plot_data,
    query_id = query_gene_id,
    interp_gaps = interp_gaps
  )
}
