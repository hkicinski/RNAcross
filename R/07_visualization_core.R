# rnacross core visualization module
# core visualization functions (line plots, PCA, group visualizations)
# dependencies: 02_constants_themes, 06_data_process

#' Create gene expression plot
#'
#' Creates a line plot showing gene expression across timepoints with
#' replicates differentiated by color shading.
#'
#' @param lc Expression matrix (genes x samples)
#' @param gene Gene ID to plot
#' @param sample_info Sample information data.frame
#' @param species_name Species display name
#' @param is_dark_mode Logical for dark mode styling
#' @param species_colors Named list of species colors
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @param plot_settings Optional plot settings list
#' @return Plotly interactive plot
create_gene_plot <- function(lc, gene, sample_info, species_name, is_dark_mode = FALSE,
                             species_colors = NULL, transform_type = "lcpm", plot_settings = NULL) {
  tryCatch(
    {
      # try to find the gene with different formats (for K. lactis)
      gene_found <- FALSE
      gene_to_use <- gene

      # for K. lactis, try multiple formats
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

      if (!gene_found) {
        available_example <- head(rownames(lc)[grepl(substr(gene, 1, 5), rownames(lc))], 3)
        error_msg <- paste("Gene ID", gene, "not found in expression data.")
        if (length(available_example) > 0) {
          error_msg <- paste(error_msg, "\nSimilar genes:", paste(available_example, collapse = ", "))
        }

        return(plotly_empty() %>%
          add_annotations(
            text = error_msg,
            showarrow = FALSE
          ))
      }

      # get species color from settings or fallback
      species_color <- if (!is.null(plot_settings) && species_name %in% names(plot_settings$species_colors)) {
        plot_settings$species_colors[[species_name]]
      } else if (!is.null(species_colors) && species_name %in% names(species_colors)) {
        species_colors[[species_name]]
      } else {
        "#440154"
      }

      # get species shape from settings or fallback
      species_shape <- if (!is.null(plot_settings) && species_name %in% names(plot_settings$species_shapes)) {
        as.integer(plot_settings$species_shapes[[species_name]])
      } else {
        16L
      }

      # normalize timepoint format (handle both "0min" and "0" formats)
      normalized_timepoints <- sample_info$Timepoint
      normalized_timepoints <- gsub("^(\\d+)$", "\\1min", normalized_timepoints)
      normalized_timepoints <- gsub("^(\\d+)min$", "\\1min", normalized_timepoints)

      dt <- data.table(
        Sample = colnames(lc),
        Timepoint = factor(normalized_timepoints, levels = TIME_POINTS),
        Replicate = sample_info$Replicate,
        exn = as.numeric(lc[gene_to_use, ])
      )

      plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
      text_color <- if (is_dark_mode) "white" else "black"
      grid_color <- if (is_dark_mode) "gray30" else "gray90"

      # custom color scale with shades of the species color for replicates
      n_replicates <- length(unique(dt$Replicate))
      if (n_replicates == 1) {
        replicate_colors <- species_color
      } else if (n_replicates == 2) {
        replicate_colors <- c(species_color, adjustcolor(species_color, alpha.f = 0.7))
      } else {
        # generate a gradient for more replicates
        alpha_values <- seq(1, 0.4, length.out = n_replicates)
        replicate_colors <- sapply(alpha_values, function(a) adjustcolor(species_color, alpha.f = a))
      }

      plot_title <- paste0("Expression of ", gene, " in <i>", species_name, "</i>")

      p <- ggplot(dt, aes(x = Timepoint, y = exn, color = factor(Replicate), group = Replicate)) +
        geom_point(size = 3, shape = species_shape) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = replicate_colors) +
        labs(
          y = get_expression_label(transform_type),
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
          title = list(text = plot_title, font = list(size = 14, color = text_color)),
          plot_bgcolor = plot_bg_color,
          paper_bgcolor = plot_bg_color,
          font = list(color = text_color),
          hoverlabel = list(bgcolor = if (is_dark_mode) "#444" else "white"),
          showlegend = TRUE,
          margin = list(b = 100, t = 50)
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtons = list(
            list("zoom2d", "pan2d", "resetScale2d", "toImage")
          )
        )
    },
    error = function(e) {
      plotly_empty() %>%
        add_annotations(
          text = paste("Error creating plot:", e$message),
          showarrow = FALSE
        )
    }
  )
}

#' Search for gene in expression data
#'
#' Searches for a gene by ID or name in the expression matrix.
#'
#' @param query Gene query string
#' @param species_data Species data structure
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @return List with id and type, or NULL if not found
search_gene <- function(query, species_data, transform_type = "lcpm") {
  query <- toupper(trimws(query))
  if (nchar(query) == 0) {
    return(NULL)
  }

  # get appropriate expression matrix based on transformation
  expr_matrix <- if (transform_type == "rlog") {
    rlog_name <- names(species_data)[grepl("rlog$", names(species_data))][1]
    if (is.null(rlog_name)) species_data$lcpm else species_data[[rlog_name]]
  } else {
    lcpm_name <- names(species_data)[grepl("lcpm$", names(species_data))][1]
    if (is.null(lcpm_name)) species_data$lcpm else species_data[[lcpm_name]]
  }

  # direct match to expression matrix
  if (query %in% rownames(expr_matrix)) {
    return(list(id = query, type = "direct_match"))
  }

  # matches via annotation
  if (!is.null(species_data$anno) && query %in% species_data$anno$GeneName) {
    gene_id <- species_data$anno$GeneID[species_data$anno$GeneName == query]
    if (length(gene_id) == 1 && gene_id[1] %in% rownames(expr_matrix)) {
      return(list(id = gene_id[1], type = "annotation_match"))
    }
  }

  return(NULL)
}

#' Calculate optimal subplot grid layout
#'
#' Determines the optimal number of rows and columns for a subplot grid.
#'
#' @param n_species Number of species/panels to display
#' @return List with nrows and ncols
calculate_subplot_grid <- function(n_species) {
  if (n_species <= 1) {
    return(list(nrows = 1, ncols = 1))
  } else if (n_species == 2) {
    return(list(nrows = 1, ncols = 2))
  } else if (n_species <= 4) {
    return(list(nrows = 2, ncols = 2))
  } else if (n_species <= 6) {
    return(list(nrows = 2, ncols = 3))
  } else if (n_species <= 9) {
    return(list(nrows = 3, ncols = 3))
  } else {
    ncols <- ceiling(sqrt(n_species))
    nrows <- ceiling(n_species / ncols)
    return(list(nrows = nrows, ncols = ncols))
  }
}

#' Create group visualization
#'
#' Creates visualizations for gene groups: line plots, bar plots, or heatmaps.
#'
#' @param plot_data data.frame with Gene, Group, Timepoint, Expression columns
#' @param viz_type Visualization type ("line", "bar", "heatmap")
#' @param is_dark_mode Logical for dark mode styling
#' @param distance_method Distance method for clustering ("pearson", "euclidean", "none")
#' @param linkage_method Linkage method for clustering
#' @param data_transform Data transformation ("raw", "centered", "zscore")
#' @param show_significance Logical to show significance markers
#' @param alpha Significance threshold
#' @param selected_gene Selected gene for significance testing
#' @param selected_comparisons Selected timepoint comparisons
#' @param plot_settings Optional plot settings list
#' @return Plotly interactive plot
create_group_visualization <- function(plot_data, viz_type, is_dark_mode = FALSE,
                                       distance_method = "pearson",
                                       linkage_method = "average",
                                       data_transform = "raw",
                                       show_significance = TRUE,
                                       alpha = 0.05,
                                       selected_gene = NULL,
                                       selected_comparisons = NULL,
                                       plot_settings = NULL) {
  plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
  text_color <- if (is_dark_mode) "white" else "black"
  grid_color <- if (is_dark_mode) "gray30" else "gray90"


  if (viz_type == "line") {
    # Handle Log2FC Transformation
    y_label <- "log2 count per million"
    if (data_transform == "log2fc") {
      # Calculate baseline (0min) per Group and Gene
      baseline_data <- plot_data %>%
        filter(Timepoint %in% c("0min", "0")) %>%
        group_by(Group, Gene) %>%
        summarise(Baseline = mean(Expression, na.rm = TRUE), .groups = "drop")

      plot_data <- plot_data %>%
        left_join(baseline_data, by = c("Group", "Gene")) %>%
        mutate(Expression = Expression - Baseline)

      y_label <- "log2 Fold-Change (vs. 0 min)"
    } else if (data_transform == "rlog") {
      y_label <- "rlog expression"
    }

    n_genes <- length(unique(plot_data$Gene))
    n_groups <- length(unique(plot_data$Group))

    p <- ggplot(plot_data) +
      aes(
        x = Timepoint, y = Expression,
        color = Gene, group = interaction(Gene, Replicate)
      ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3)

    # Apply settings-based aesthetics if available
    if (!is.null(plot_settings)) {
      # Gene Colors
      if (!is.null(plot_settings$gene_colors)) {
        # flatten list to named vector
        gene_cols <- unlist(plot_settings$gene_colors)
        if (length(gene_cols) > 0) {
          p <- p + scale_color_manual(values = gene_cols)
        }
      } else if (!is.null(plot_settings$gene_palette)) {
         p <- p + scale_color_manual(values = get_palette_colors(plot_settings$gene_palette, n_genes))
      }

      # Secondary Encoding
      if (!is.null(plot_settings$encoding_multigene_secondary)) {
        sec <- plot_settings$encoding_multigene_secondary
        if (sec %in% c("linetype", "both")) {
          p <- p + aes(linetype = Gene)
        }
        if (sec %in% c("shape", "both")) {
          p <- p + aes(shape = Gene)
        }
      }
    }

    p <- p + labs(
        title = "Gene Set Analysis: Trajectory Plot",
        subtitle = paste(
          "Analyzing", n_genes, "genes across", n_groups,
          if (n_groups == 1) "group" else "groups"
        ),
        x = "Timepoint",
        y = y_label
      ) +
      theme_minimal()

    p <- p + theme(
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
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color)
    )

    return(ggplotly(p) %>%
      layout(
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color),
        hoverlabel = list(bgcolor = if (is_dark_mode) "#444" else "white"),
        showlegend = TRUE
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var debugMsg = document.createElement('div');
          debugMsg.className = 'interactive-debug-msg';
          debugMsg.style.padding = '5px';
          debugMsg.style.fontWeight = 'bold';
          
          if (typeof window.initInteractiveEditor === 'function') {
            debugMsg.style.color = 'green';
            debugMsg.innerText = '[System: Interactive Editor Successfully Initialized]';
            window.initInteractiveEditor(el);
          } else {
            debugMsg.style.color = 'red';
            debugMsg.innerText = '[System Error: Interactive Editor JS failed to load or is undefined]';
          }
          
          if (el && el.parentNode) {
            el.parentNode.insertBefore(debugMsg, el);
          }
        }
      "))
  } else if (viz_type == "bar") {
    summary_data <- plot_data %>%
      group_by(Timepoint, Group, Gene) %>%
      summarise(Mean = mean(Expression), .groups = "drop")

    p <- ggplot(summary_data, aes(x = Timepoint, y = Mean, fill = Gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Group) +
      theme_minimal()

    p <- p + theme(
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
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color)
    )

    # add significance markers if requested
    if (show_significance && !is.null(selected_gene) && !is.null(selected_comparisons) && length(selected_comparisons) > 0) {
      sig_results <- calculate_significance(
        plot_data,
        alpha,
        selected_gene,
        selected_comparisons
      )

      if (length(sig_results) > 0) {
        sig_data <- data.frame(
          Gene = sapply(sig_results, function(x) x$gene),
          Group = sapply(sig_results, function(x) unique(plot_data$Group[plot_data$Gene == x$gene])[1]),
          Timepoint1 = sapply(sig_results, function(x) x$timepoint1),
          Timepoint2 = sapply(sig_results, function(x) x$timepoint2),
          YPosition = sapply(sig_results, function(x) x$y_position),
          Significance = sapply(sig_results, function(x) x$significance),
          stringsAsFactors = FALSE
        )

        for (g in unique(sig_data$Group)) {
          for (gene in unique(sig_data$Gene[sig_data$Group == g])) {
            gene_sig <- sig_data[sig_data$Group == g & sig_data$Gene == gene, ]

            for (i in 1:nrow(gene_sig)) {
              pos1 <- which(levels(summary_data$Timepoint) == gene_sig$Timepoint1[i])
              pos2 <- which(levels(summary_data$Timepoint) == gene_sig$Timepoint2[i])

              dodge_width <- 0.9
              gene_pos <- which(unique(summary_data$Gene) == gene) - 1
              num_genes <- length(unique(summary_data$Gene))
              bar_width <- dodge_width / num_genes

              x1 <- pos1 - dodge_width / 2 + bar_width / 2 + gene_pos * bar_width
              x2 <- pos2 - dodge_width / 2 + bar_width / 2 + gene_pos * bar_width

              p <- p +
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x1,
                    xend = x2,
                    y = gene_sig$YPosition[i],
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x1,
                    xend = x1,
                    y = gene_sig$YPosition[i] - 0.2,
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x2,
                    xend = x2,
                    y = gene_sig$YPosition[i] - 0.2,
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                geom_text(
                  data = data.frame(
                    Group = g,
                    x = (x1 + x2) / 2,
                    y = gene_sig$YPosition[i] + 0.2,
                    label = gene_sig$Significance[i]
                  ),
                  aes(x = x, y = y, label = label),
                  inherit.aes = FALSE,
                  size = 3
                )
            }
          }
        }
      }
    }

    return(ggplotly(p) %>%
      layout(
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color),
        hoverlabel = list(bgcolor = if (is_dark_mode) "#444" else "white"),
        showlegend = TRUE
      ))
  } else if (viz_type == "heatmap") {
    heatmap_data <- plot_data %>%
      group_by(Gene, Timepoint) %>%
      summarise(Expression = mean(Expression), .groups = "drop") %>%
      pivot_wider(names_from = Timepoint, values_from = Expression)

    gene_names <- heatmap_data$Gene
    expression_matrix <- as.matrix(heatmap_data[, -1])
    rownames(expression_matrix) <- gene_names

    transformed_matrix <- switch(data_transform,
      "raw" = expression_matrix,
      "centered" = t(scale(t(expression_matrix), center = TRUE, scale = FALSE)),
      "zscore" = t(scale(t(expression_matrix), center = TRUE, scale = TRUE)),
      expression_matrix
    )

    # apply clustering if there's more than one gene
    if (nrow(transformed_matrix) > 1) {
      if (distance_method == "euclidean") {
        dist_matrix <- dist(transformed_matrix, method = "euclidean")
        hclust_result <- hclust(dist_matrix, method = linkage_method)
        row_order <- hclust_result$order
      } else if (distance_method == "pearson") {
        cor_matrix <- cor(t(transformed_matrix))
        dist_matrix <- as.dist(1 - cor_matrix)
        hclust_result <- hclust(dist_matrix, method = linkage_method)
        row_order <- hclust_result$order
      } else {
        row_order <- 1:nrow(transformed_matrix)
      }

      transformed_matrix <- transformed_matrix[row_order, ]
    }

    plot_params <- switch(data_transform,
      "raw" = list(
        zmid = mean(range(transformed_matrix)),
        zmin = min(transformed_matrix),
        zmax = max(transformed_matrix),
        title = "Raw log2CPM"
      ),
      "centered" = list(
        zmid = 0,
        zmin = -2,
        zmax = 2,
        title = "Centered log2CPM"
      ),
      "zscore" = list(
        zmid = 0,
        zmin = -2,
        zmax = 2,
        title = "Z-score"
      ),
      list(
        zmid = mean(range(transformed_matrix)),
        zmin = min(transformed_matrix),
        zmax = max(transformed_matrix),
        title = "Expression Values"
      )
    )

    # create hover text before NaN conversion
    hm_hover_vals <- ifelse(is.na(transformed_matrix), "No data", round(transformed_matrix, 2))
    hm_hover_text <- matrix(
      paste(
        "Gene:", rep(rownames(transformed_matrix), each = ncol(transformed_matrix)),
        "<br>Time:", rep(colnames(transformed_matrix), times = nrow(transformed_matrix)),
        "<br>Value:", as.vector(t(hm_hover_vals))
      ),
      nrow = nrow(transformed_matrix),
      ncol = ncol(transformed_matrix),
      byrow = TRUE
    )

    # convert NA to NaN to prevent plotly nacolor warning
    transformed_matrix[is.na(transformed_matrix)] <- NaN

    # get heatmap palette from settings or use default
    hm_colorscale <- if (!is.null(plot_settings) && !is.null(plot_settings$heatmap_palette)) {
      plot_settings$heatmap_palette
    } else {
      list(c(0, "#0000FF"), c(0.5, "#000000"), c(1, "#FFFF00"))
    }

    p <- plot_ly(
      z = transformed_matrix,
      x = colnames(transformed_matrix),
      y = rownames(transformed_matrix),
      type = "heatmap",
      colorscale = hm_colorscale,
      zmid = plot_params$zmid,
      zmin = plot_params$zmin,
      zmax = plot_params$zmax,
      hoverinfo = "text",
      text = hm_hover_text
    ) %>%
      layout(
        title = paste(
          "Gene Expression Heatmap -", plot_params$title,
          "<br><sub>Clustering:",
          if (nrow(transformed_matrix) > 1) paste(distance_method, "distance") else "None",
          "</sub>"
        ),
        xaxis = list(
          title = "Timepoint",
          tickangle = -45
        ),
        yaxis = list(
          title = "Gene",
          autorange = "reversed"
        ),
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color)
      )

    return(p)
  }
}

#' Create group summary table
#'
#' Creates a summary datatable for gene group expression data.
#'
#' @param plot_data data.frame with Group, Gene, Expression columns
#' @param species_name Optional species name to replace "Custom Group"
#' @return DT datatable
create_group_summary_table <- function(plot_data, species_name = NULL) {
  summary_data <- plot_data %>%
    group_by(Group, Gene) %>%
    summarise(
      Mean_Expression = mean(Expression),
      Max_Expression = max(Expression),
      Min_Expression = min(Expression),
      .groups = "drop"
    )

  # replace "Custom Group" with species-specific name
  if (!is.null(species_name) && "Custom Group" %in% summary_data$Group) {
    summary_data$Group[summary_data$Group == "Custom Group"] <- paste0("<i>", species_name, "</i> Gene Set")
  }

  datatable(
    summary_data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "tp"
    ),
    rownames = FALSE,
    escape = FALSE
  )
}

#' Create single-species PCA plot
#'
#' Creates a PCA plot for a single species showing sample clustering.
#'
#' @param expression_matrix Expression matrix (genes x samples)
#' @param sample_info Sample information data.frame
#' @param is_dark_mode Logical for dark mode styling
#' @param plot_settings Optional plot settings list
#' @return Plotly interactive plot
create_pca_plot <- function(expression_matrix, sample_info, is_dark_mode = FALSE, plot_settings = NULL) {
  if (!is.matrix(expression_matrix) && !is.data.frame(expression_matrix)) {
    stop("Expression matrix must be a matrix or data frame")
  }

  if (is.data.frame(expression_matrix)) {
    expression_matrix <- as.matrix(expression_matrix)
  }

  # filter genes with zero variance
  var_genes <- apply(expression_matrix, 1, var)
  expression_matrix <- expression_matrix[var_genes > 0, , drop = FALSE]

  mat <- t(expression_matrix)
  pca_res <- prcomp(mat, scale. = FALSE, center = TRUE)
  var_explained <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100

  pca_data <- data.frame(
    PC1 = pca_res$x[, 1],
    PC2 = pca_res$x[, 2],
    Sample = rownames(pca_res$x)
  )

  pca_data <- merge(pca_data, sample_info, by = "Sample", all.x = TRUE)
  pca_data$Timepoint <- factor(pca_data$Timepoint, levels = TIME_POINTS)

  pca_collapse <- if (!is.null(plot_settings) && length(plot_settings$pca_collapse_reps) > 0) plot_settings$pca_collapse_reps else "none"
  if (pca_collapse %in% c("mean", "median") && "Replicate" %in% names(pca_data)) {
    agg_func <- if (pca_collapse == "mean") mean else median
    group_cols <- intersect(names(pca_data), c("Timepoint", "Condition"))
    pca_data <- pca_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        PC1 = agg_func(PC1),
        PC2 = agg_func(PC2),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Sample = paste(Timepoint, "Collapsed", sep="_"),
        Replicate = "Collapsed"
      )
  }

  # get settings with defaults
  point_size <- if (length(plot_settings$pca_point_size) > 0) plot_settings$pca_point_size else 3
  point_alpha <- if (length(plot_settings$pca_alpha) > 0) plot_settings$pca_alpha else 0.8

  plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
  text_color <- if (is_dark_mode) "white" else "black"
  grid_color <- if (is_dark_mode) "gray30" else "gray90"
  minor_grid_color <- if (is_dark_mode) "gray20" else "gray95"

  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Timepoint, text = Sample)) +
    geom_point(size = point_size, alpha = point_alpha) +
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
      panel.grid.minor = element_line(color = minor_grid_color),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )

  if (!is.null(plot_settings) && isTRUE(plot_settings$pca_viz_mode == "publication")) {
    pca_data <- pca_data[order(as.numeric(pca_data$Timepoint)), ]
    
    if (isTRUE(plot_settings$pca_trajectories)) {
      path_data <- pca_data %>%
        dplyr::group_by(Timepoint) %>%
        dplyr::summarize(
          PC1 = mean(PC1, na.rm = TRUE),
          PC2 = mean(PC2, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(as.numeric(Timepoint))
        
      p <- p + geom_path(
        data = path_data,
        aes(x = PC1, y = PC2, group = 1),
        arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
        alpha = 0.7,
        linewidth = 0.8
      )
    }

    if (isTRUE(plot_settings$pca_labels)) {
      pca_data$pub_label <- ifelse(is.na(as.numeric(pca_data$Timepoint)), 
                                   as.character(pca_data$Timepoint), 
                                   paste0("T", as.numeric(pca_data$Timepoint)))
      
      label_data <- pca_data %>%
        dplyr::group_by(Timepoint, pub_label) %>%
        dplyr::summarize(
          PC1 = mean(PC1, na.rm = TRUE),
          PC2 = mean(PC2, na.rm = TRUE),
          .groups = "drop"
        )
        
      p <- p + ggrepel::geom_text_repel(
        data = label_data,
        aes(x = PC1, y = PC2, label = pub_label),
        size = 3.5,
        bg.color = "white",
        bg.r = 0.15,
        show.legend = FALSE
      )
    }

    p <- p +
      ggprism::theme_prism(base_size = 14) +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold")
      )
    return(p)
  }

  plotly::ggplotly(p, tooltip = c("Sample", "Timepoint")) %>%
    layout(
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = plot_bg_color,
      font = list(color = text_color),
      showlegend = TRUE,
      legend = list(bgcolor = plot_bg_color),
      margin = list(t = 50, r = 20, b = 50, l = 50)
    ) %>%
    config(displayModeBar = TRUE, modeBarButtons = list(list("zoom2d", "pan2d", "resetScale2d", "toImage")))
}

#' Create multi-species PCA plot
#'
#' Creates a PCA plot comparing multiple species using HOG-based expression.
#'
#' @param get_species_data Function to retrieve species data
#' @param is_dark_mode Logical for dark mode styling
#' @param aggregation_method Method for aggregating multi-copy genes
#' @param species_config Species configuration list
#' @param all_species_data_obj Optional pre-loaded data object
#' @param transform_type Transformation type ("lcpm" or "rlog")
#' @param debug Logical for debug output
#' @param plot_settings Optional plot settings list
#' @return Plotly interactive plot with matrices_data attribute
create_multi_species_pca <- function(get_species_data, is_dark_mode = FALSE, aggregation_method = "eigengene",
                                     species_config = DEFAULT_SPECIES_CONFIG, all_species_data_obj = NULL,
                                     transform_type = "lcpm", debug = FALSE, plot_settings = NULL) {
  if (length(aggregation_method) == 0) aggregation_method <- "eigengene"
  if (length(transform_type) == 0) transform_type <- "lcpm"
  
  method_label <- switch(aggregation_method,
    "single_only" = "SINGLE-COPY GENES ONLY",
    "mean" = "MEAN AGGREGATION",
    "median" = "MEDIAN AGGREGATION",
    "eigengene" = "EIGENGENE AGGREGATION",
    "max_expr" = "HIGHEST EXPRESSION SELECTION",
    "max_var" = "HIGHEST VARIANCE SELECTION",
    "var_weighted" = "VARIANCE-WEIGHTED MEAN",
    "UNKNOWN METHOD"
  )

  if (debug) message(paste("PCA: Starting with", method_label))

  config <- species_config
  current_data <- if (!is.null(all_species_data_obj)) all_species_data_obj else all_species_data

  # get data and cache expression matrices upfront
  species_data_list <- list()
  expression_matrix_cache <- list()
  sample_info_cache <- list()

  for (species_id in names(config)) {
    sp_data <- get_species_data(species_id)
    species_data_list[[species_id]] <- sp_data

    if (!is.null(sp_data)) {
      expression_matrix_cache[[species_id]] <- get_expression_matrix(species_id, transform_type, sp_data)
      sample_info_cache[[species_id]] <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(species_id, "_sample_info")]]
    }
  }

  # get HOG data
  og_data <- current_data$orthofinder$orthogroups

  # pre-compute HOG-to-gene lookup table
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
  if (debug) message(paste("PCA: Lookup table created with", length(hog_gene_map), "HOGs"))

  # filter or aggregate based on method
  if (aggregation_method == "single_only") {
    # find HOGs that have exactly one gene in each species that has representation
    single_copy_hogs <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
      gene_counts <- sapply(hog, length)
      length(gene_counts) >= 2 && all(gene_counts == 1)
    })]

    if (debug) {
      message(paste("PCA: Found", length(single_copy_hogs), "single-copy HOGs"))
    }

    common_hogs <- single_copy_hogs
  } else {
    # use all HOGs with at least 2 species represented
    common_hogs <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
      length(hog) >= 2
    })]

    if (debug) {
      message(paste("PCA: Found", length(common_hogs), "HOGs with at least 2 species"))
    }
  }

  # check if we have any HOGs
  if (length(common_hogs) == 0) {
    error_msg <- "No single-copy HOGs found across species. Cannot perform PCA."
    showNotification(error_msg, type = "error")
    return(plotly_empty() %>%
      add_annotations(text = error_msg, showarrow = FALSE))
  }

  # create sample metadata using cached data
  sample_metadata_list <- list()

  for (species_id in names(config)) {
    lcpm_matrix <- expression_matrix_cache[[species_id]]
    sample_info <- sample_info_cache[[species_id]]

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

  sample_metadata <- rbindlist(sample_metadata_list, fill = TRUE)
  if (debug) message(paste("PCA: Total samples:", nrow(sample_metadata)))

  # create expression matrix based on single-copy HOGs
  sample_matrix <- matrix(NA,
    nrow = nrow(sample_metadata),
    ncol = length(common_hogs)
  )
  rownames(sample_matrix) <- sample_metadata$Sample
  colnames(sample_matrix) <- common_hogs

  # track successful matches by species
  matches_by_species <- list(cg = 0, sc = 0, kl = 0, ca = 0)

  # track aggregation statistics
  aggregation_stats <- list()
  for (sp_code in names(config)) {
    aggregation_stats[[sp_code]] <- list(
      single_copy = 0,
      multi_copy = 0,
      total_hogs = 0
    )
  }

  # count single vs multi-copy for each species across all HOGs
  for (hog in common_hogs) {
    if (!is.null(hog_gene_map[[hog]])) {
      for (sp_code in names(config)) {
        if (!is.null(hog_gene_map[[hog]][[sp_code]])) {
          gene_count <- length(hog_gene_map[[hog]][[sp_code]])
          aggregation_stats[[sp_code]]$total_hogs <- aggregation_stats[[sp_code]]$total_hogs + 1

          if (gene_count == 1) {
            aggregation_stats[[sp_code]]$single_copy <- aggregation_stats[[sp_code]]$single_copy + 1
          } else if (gene_count > 1) {
            aggregation_stats[[sp_code]]$multi_copy <- aggregation_stats[[sp_code]]$multi_copy + 1
          }
        }
      }
    }
  }

  if (debug) {
    message("PCA: HOG composition by species:")
    for (sp_code in names(aggregation_stats)) {
      stats <- aggregation_stats[[sp_code]]
      message(paste("  ", sp_code, ": ", stats$total_hogs, " HOGs (",
        stats$single_copy, " single, ", stats$multi_copy, " multi)",
        sep = ""
      ))
    }
  }

  # pre-aggregate HOGs per species using cached matrices
  species_hog_expr <- list()

  for (species_id in names(config)) {
    lcpm_matrix <- expression_matrix_cache[[species_id]]
    if (is.null(lcpm_matrix)) next

    if (debug) message(paste("PCA: pre-aggregating", species_id, "-", nrow(lcpm_matrix), "genes"))

    # build species HOG map
    species_hog_map <- lapply(hog_gene_map[common_hogs], function(hog) {
      if (!is.null(hog[[species_id]])) {
        hog[[species_id]]
      } else {
        NULL
      }
    })
    names(species_hog_map) <- common_hogs

    # aggregate HOGs using cached matrix
    species_hog_expr[[species_id]] <- preaggregate_species_hogs(
      lcpm_matrix = lcpm_matrix,
      species_hog_map = species_hog_map,
      common_hogs = common_hogs,
      aggregation_method = aggregation_method,
      species_code = species_id
    )
  }

  # prebuild species code lookup
  species_short_to_code <- setNames(names(config), sapply(config, `[[`, "short"))
  species_name_to_code <- setNames(names(config), sapply(config, `[[`, "name"))

  # fill matrix using pre-aggregated values
  for (i in 1:nrow(sample_metadata)) {
    sample_name <- sample_metadata$Sample[i]
    species <- sample_metadata$Species[i]

    species_code <- if (species %in% names(species_short_to_code)) {
      species_short_to_code[[species]]
    } else if (species %in% names(species_name_to_code)) {
      species_name_to_code[[species]]
    } else {
      NULL
    }

    if (is.null(species_code)) next
    if (is.null(species_hog_expr[[species_code]])) next

    lcpm_matrix <- expression_matrix_cache[[species_code]]

    if (is.null(lcpm_matrix)) next
    if (!sample_name %in% colnames(lcpm_matrix)) next

    sample_idx <- which(colnames(lcpm_matrix) == sample_name)

    # fill HOG values from pre-aggregated data
    for (j in seq_along(common_hogs)) {
      hog <- common_hogs[j]

      if (!is.null(species_hog_expr[[species_code]][[hog]])) {
        sample_matrix[i, j] <- species_hog_expr[[species_code]][[hog]][sample_idx]
        matches_by_species[[species_code]] <- matches_by_species[[species_code]] + 1
      }
    }
  }

  if (debug) {
    message("PCA: Match statistics by species:")
    for (sp in names(matches_by_species)) {
      message(paste(sp, ":", matches_by_species[[sp]], "HOG values filled"))
    }
  }

  # remove columns (HOGs) with too many NAs (more than 50% missing)
  na_proportion <- apply(sample_matrix, 2, function(x) sum(is.na(x)) / length(x))
  keep_cols <- na_proportion < 0.5

  if (sum(keep_cols) < 2) {
    error_msg <- "Too few single-copy HOGs with sufficient data. Cannot perform PCA."
    showNotification(error_msg, type = "error")
    return(plotly_empty() %>%
      add_annotations(text = error_msg, showarrow = FALSE))
  }

  sample_matrix <- sample_matrix[, keep_cols, drop = FALSE]
  debug_print(paste("Using", sum(keep_cols), "single-copy HOGs for PCA"))

  # remove samples with all NAs
  row_has_data <- apply(sample_matrix, 1, function(x) !all(is.na(x)))
  if (sum(row_has_data) < nrow(sample_matrix)) {
    debug_print(paste("Removing", sum(!row_has_data), "samples with no data"))
    sample_matrix <- sample_matrix[row_has_data, , drop = FALSE]
    sample_metadata <- sample_metadata[row_has_data, , drop = FALSE]
  }

  # impute NAs with column means
  debug_print("Imputing missing values")
  col_means <- colMeans(sample_matrix, na.rm = TRUE)
  na_matrix <- is.na(sample_matrix)
  imputed_count <- sum(na_matrix)

  for (j in which(colSums(na_matrix) > 0)) {
    sample_matrix[na_matrix[, j], j] <- col_means[j]
  }
  debug_print(paste("Imputed", imputed_count, "missing values with column means"))

  # remove all-NA columns
  valid_cols <- !is.na(col_means)
  sample_matrix <- sample_matrix[, valid_cols, drop = FALSE]

  debug_print(paste("Final matrix dimensions:", nrow(sample_matrix), "x", ncol(sample_matrix)))

  # final check before PCA
  if (ncol(sample_matrix) < 2 || nrow(sample_matrix) < 3) {
    showNotification("Insufficient valid data for PCA", type = "error")
    return(plotly_empty() %>%
      add_annotations(
        text = "Not enough valid single-copy orthogroups for analysis",
        showarrow = FALSE
      ))
  }

  # run PCA
  debug_print("Running PCA on single-copy HOG matrix")

  tryCatch(
    {
      pca_result <- prcomp(sample_matrix, scale. = TRUE, center = TRUE)
      var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

      debug_print(paste("PCA successful. PC1 explains", round(var_explained[1], 1), "% variance"))

      plot_data <- data.frame(
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Sample = rownames(pca_result$x)
      )

      plot_data <- merge(plot_data, sample_metadata, by = "Sample")
      plot_data$Timepoint <- factor(plot_data$Timepoint, levels = TIME_POINTS)

      pca_collapse <- if (!is.null(plot_settings) && length(plot_settings$pca_collapse_reps) > 0) plot_settings$pca_collapse_reps else "none"
      if (pca_collapse %in% c("mean", "median") && "Replicate" %in% names(plot_data)) {
        agg_func <- if (pca_collapse == "mean") mean else median
        group_cols <- intersect(names(plot_data), c("Species", "Timepoint", "Condition"))
        plot_data <- plot_data %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
          dplyr::summarize(
            PC1 = agg_func(PC1),
            PC2 = agg_func(PC2),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Sample = paste(Species, Timepoint, sep = "_"),
            Replicate = "Collapsed"
          )
      }

      plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
      text_color <- if (is_dark_mode) "white" else "black"
      grid_color <- if (is_dark_mode) "gray30" else "gray90"

      # get settings with defaults
      point_size <- if (length(plot_settings$pca_point_size) > 0) plot_settings$pca_point_size else 4
      point_alpha <- if (length(plot_settings$pca_alpha) > 0) plot_settings$pca_alpha else 0.8
      encoding_color <- if (length(plot_settings$encoding_pca_color) > 0) plot_settings$encoding_pca_color else "timepoint"
      encoding_shape <- if (length(plot_settings$encoding_pca_shape) > 0) plot_settings$encoding_pca_shape else "species"
      species_colors <- if (!is.null(plot_settings$species_colors)) plot_settings$species_colors else list()
      species_shapes <- if (!is.null(plot_settings$species_shapes)) plot_settings$species_shapes else list()

      # build color and shape vectors for species
      unique_species <- unique(plot_data$Species)
      shape_vec <- sapply(unique_species, function(sp) {
        resolve_species_shape(sp, species_shapes, 16L)
      })
      names(shape_vec) <- unique_species

      color_vec <- sapply(unique_species, function(sp) {
        resolve_species_color(sp, species_colors, "#808080")
      })
      names(color_vec) <- unique_species

      # create plot with encoding settings
      hover_text <- paste0(
        "Sample: ", plot_data$Sample,
        "<br>Species: ", plot_data$Species,
        "<br>Timepoint: ", plot_data$Timepoint
      )
      plot_data$hover_text <- hover_text

      if (encoding_color == "species" && encoding_shape == "species") {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Species, shape = Species, text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_manual(values = color_vec) +
          scale_shape_manual(values = shape_vec)
      } else if (encoding_color == "species" && encoding_shape == "replicate") {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Species, shape = factor(Replicate), text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_manual(values = color_vec) +
          labs(shape = "Replicate")
      } else if (encoding_color == "species" && encoding_shape == "none") {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Species, text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_manual(values = color_vec)
      } else if (encoding_color == "timepoint" && encoding_shape == "species") {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Timepoint, shape = Species, text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_viridis(discrete = TRUE, option = "viridis") +
          scale_shape_manual(values = shape_vec)
      } else if (encoding_color == "timepoint" && encoding_shape == "replicate") {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Timepoint, shape = factor(Replicate), text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_viridis(discrete = TRUE, option = "viridis") +
          labs(shape = "Replicate")
      } else {
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Timepoint, text = hover_text)) +
          geom_point(size = point_size, alpha = point_alpha) +
          scale_color_viridis(discrete = TRUE, option = "viridis")
      }

      p <- p +
        labs(
          x = sprintf("PC1 (%.1f%%)", var_explained[1]),
          y = sprintf("PC2 (%.1f%%)", var_explained[2]),
          title = paste("PCA of HOG Expression Across Species -", method_label),
          subtitle = paste(
            ncol(sample_matrix), "HOGs compared across",
            length(unique(plot_data$Species)), "species",
            if (aggregation_method != "single_only") {
              paste(
                "(", sum(sapply(aggregation_stats, function(x) x$multi_copy)),
                "multi-copy HOGs aggregated)"
              )
            } else {
              ""
            }
          )
        ) +
        theme_minimal() +
        theme(
          text = element_text(color = text_color),
          axis.text = element_text(color = text_color),
          plot.title = element_text(size = 14, face = "bold", color = text_color),
          plot.subtitle = element_text(size = 12, color = text_color),
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_line(color = if (is_dark_mode) "gray20" else "gray95"),
          plot.background = element_rect(fill = plot_bg_color, color = NA),
          panel.background = element_rect(fill = plot_bg_color, color = NA),
          legend.background = element_rect(fill = plot_bg_color),
          legend.text = element_text(color = text_color, face = "italic"),
          legend.title = element_text(color = text_color)
        )

      # store matrices data for download
      matrices_data <- list(
        sample_matrix = sample_matrix,
        sample_metadata = sample_metadata,
        common_hogs = colnames(sample_matrix),
        method = aggregation_method,
        aggregation_stats = if (aggregation_method != "single_only") {
          aggregation_stats
        } else {
          NULL
        },
        pca_result = pca_result,
        var_explained = var_explained
      )

      attr(p, "matrices_data") <- matrices_data

      if (!is.null(plot_settings) && isTRUE(plot_settings$pca_viz_mode == "publication")) {
        plot_data <- plot_data[order(plot_data$Species, as.numeric(plot_data$Timepoint)), ]
        
        # Hardcode natural species colors per user request, fallback for unknown species
        base_colors <- c(
          "S. cerevisiae" = "blue",
          "C. glabrata" = "red",
          "C. albicans" = "#74AC4C",
          "K. lactis" = "#F8C434"
        )
        for (sp in unique(plot_data$Species)) {
          if (!(sp %in% names(base_colors))) base_colors[sp] <- color_vec[sp]
        }
        
        # Parse text labels explicitly mapping 'T1', 'T2'
        mapped_num <- as.numeric(plot_data$Timepoint)
        plot_data$pub_label <- ifelse(is.na(mapped_num), as.character(plot_data$Timepoint), paste0("T", mapped_num))
        
        # Build interaction colors for progression (HCL/Lab Linear mapping equivalent)
        plot_data$SpeciesTime <- paste(plot_data$Species, plot_data$Timepoint, sep = "_")
        interaction_colors <- c()
        for (sp in unique(plot_data$Species)) {
           sp_data <- plot_data[plot_data$Species == sp, ]
           t_levels_sp <- sort(unique(as.numeric(sp_data$Timepoint)))
           num_t <- length(t_levels_sp)
           if (num_t < 1) num_t <- 1
           
           # Generate color progression ignoring early ultra-light tones for visibility
           pal <- colorRampPalette(c("white", base_colors[sp]), space = "Lab")(num_t + 2)
           sp_colors <- pal[3:(num_t + 2)]
           
           for (i in seq_along(t_levels_sp)) {
             t_str <- as.character(unique(sp_data$Timepoint[as.numeric(sp_data$Timepoint) == t_levels_sp[i]]))
             interaction_colors[paste(sp, t_str, sep = "_")] <- sp_colors[i]
           }
        }
        
        # Render Publication Base Figure
        p <- ggplot(plot_data, aes(x = PC1, y = PC2))
        
        if ("Replicate" %in% names(plot_data) && pca_collapse == "none") {
           p <- p + geom_point(aes(color = SpeciesTime, shape = factor(Replicate)), size = point_size) +
                scale_shape_manual(values = c(1, 16, 2, 17, 0, 15, 3, 4, 8)) +
                labs(shape = "Replicate")
        } else {
           p <- p + geom_point(aes(color = SpeciesTime), shape = 16, size = point_size)
        }
        
        # Turn off native massive interaction legend mappings
        p <- p + scale_color_manual(values = interaction_colors, guide = "none") 
        
        # Apply dummy mapping cleanly establishing a Species Legend tied to raw Natural Colors
        p <- p + geom_point(data = plot_data, aes(x = NA, y = NA, fill = Species), shape = 21, size = 3, color = "transparent", na.rm = TRUE) +
             scale_fill_manual(values = base_colors, name = "Species")
             
        if (isTRUE(plot_settings$pca_trajectories)) {
          path_data <- plot_data %>%
            dplyr::group_by(Species, Timepoint, SpeciesTime) %>%
            dplyr::summarize(
              PC1 = mean(PC1, na.rm = TRUE),
              PC2 = mean(PC2, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::arrange(Species, as.numeric(Timepoint))
            
          p <- p + geom_path(
            data = path_data,
            aes(x = PC1, y = PC2, group = Species, color = SpeciesTime),
            arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
            alpha = 0.7,
            linewidth = 0.8,
            show.legend = FALSE
          )
        }

        if (isTRUE(plot_settings$pca_labels)) {
          label_data <- plot_data %>%
            dplyr::group_by(Species, SpeciesTime, pub_label) %>%
            dplyr::summarize(
              PC1 = mean(PC1, na.rm = TRUE),
              PC2 = mean(PC2, na.rm = TRUE),
              .groups = "drop"
            )
            
          p <- p + ggrepel::geom_text_repel(
            data = label_data,
            aes(x = PC1, y = PC2, label = pub_label, color = SpeciesTime),
            size = 3.5,
            bg.color = "white",
            bg.r = 0.15,
            show.legend = FALSE
          )
        }

        p <- p +
          labs(
            x = sprintf("PC1 (Variance Explained: %.1f%%)", var_explained[1]),
            y = sprintf("PC2 (Variance Explained: %.1f%%)", var_explained[2]),
            title = paste("Multi-Species PCA: Temporal Trajectories -", method_label),
            subtitle = paste("Showing progression over timepoints for", length(unique(plot_data$Species)), "species")
          ) +
          ggprism::theme_prism(base_size = 14) +
          theme(
            legend.position = "right",
            legend.title = element_text(face = "bold")
          ) +
          (if (encoding_color == "species" || encoding_shape == "species") theme(legend.text = element_text(face = "italic")) else NULL)

        attr(p, "matrices_data") <- matrices_data
        debug_print("PCA static plot created successfully")
        return(p)
      }

      plot <- plotly::ggplotly(p, tooltip = "text") %>%
        layout(
          plot_bgcolor = plot_bg_color,
          paper_bgcolor = plot_bg_color,
          font = list(color = text_color),
          hoverlabel = list(
            bgcolor = if (is_dark_mode) "#444" else "white",
            bordercolor = "white"
          )
        ) %>%
        config(displayModeBar = TRUE)

      attr(plot, "matrices_data") <- matrices_data

      debug_print("PCA plot created successfully")
      return(plot)
    },
    error = function(e) {
      debug_print(paste("Error in PCA:", e$message))

      showNotification(paste("PCA failed:", e$message), type = "error")
      return(plotly_empty() %>%
        add_annotations(
          text = paste("PCA Error:", e$message),
          showarrow = FALSE
        ))
    }
  )
}

#' Calculate significance for timepoint comparisons
#'
#' Performs t-tests between timepoints for bar graph visualization.
#'
#' @param plot_data data.frame with Gene, Timepoint, Expression columns
#' @param alpha Significance threshold
#' @param selected_gene Gene to test
#' @param selected_comparisons Timepoint pairs to compare
#' @return List of significance results
calculate_significance <- function(plot_data, alpha = 0.05, selected_gene = NULL, selected_comparisons = NULL) {
  if (is.null(selected_gene) || is.null(selected_comparisons) || length(selected_comparisons) == 0) {
    return(list())
  }

  gene_data <- plot_data[plot_data$Gene == selected_gene, ]
  if (nrow(gene_data) == 0) {
    return(list())
  }

  timepoints <- levels(plot_data$Timepoint)
  sig_results <- list()

  for (comparison in selected_comparisons) {
    t_pair <- strsplit(comparison, " vs. ")[[1]]
    t1 <- t_pair[1]
    t2 <- t_pair[2]

    t1_data <- gene_data$Expression[gene_data$Timepoint == t1]
    t2_data <- gene_data$Expression[gene_data$Timepoint == t2]

    # only test if we have enough data points (at least 2 replicates)
    if (length(t1_data) >= 2 && length(t2_data) >= 2) {
      test_result <- t.test(t1_data, t2_data)
      p_value <- test_result$p.value

      significance <- "ns"
      if (p_value < 0.001) {
        significance <- "***"
      } else if (p_value < 0.01) {
        significance <- "**"
      } else if (p_value < 0.05) significance <- "*"

      sig_results[[length(sig_results) + 1]] <- list(
        gene = selected_gene,
        timepoint1 = t1,
        timepoint2 = t2,
        p_value = p_value,
        significance = significance,
        mean1 = mean(t1_data),
        mean2 = mean(t2_data),
        y_position = max(mean(t1_data), mean(t2_data)) * 1.1
      )
    }
  }

  return(sig_results)
}

#' Create ridgeline plot
#'
#' Creates a ridgeline plot showing expression distribution across timepoints.
#'
#' @param species_data_list List of species data structures
#' @param is_dark_mode Logical for dark mode styling
#' @param plot_settings Optional plot settings list
#' @return ggplot object
create_ridgeline_plot <- function(species_data_list, is_dark_mode = FALSE, plot_settings = NULL) {
  # preallocate list for faster data collection
  data_list <- vector("list", length = sum(sapply(species_data_list, function(x) {
    sample_info <- if ("sample_info" %in% names(x)) x$sample_info else x[[paste0(names(species_data_list)[1], "_sample_info")]]
    nrow(sample_info)
  })))
  list_idx <- 1

  for (species_id in names(species_data_list)) {
    species_data <- species_data_list[[species_id]]
    species_name <- switch(species_id,
      "cg" = "C. glabrata",
      "sc" = "S. cerevisiae",
      "kl" = "K. lactis",
      "ca" = "C. albicans"
    )

    expr_matrix <- if (species_id == "cg") species_data$lcpm else species_data[[paste0(species_id, "_lcpm")]]
    sample_info <- if (species_id == "cg") species_data$sample_info else species_data[[paste0(species_id, "_sample_info")]]

    for (i in 1:nrow(sample_info)) {
      sample_name <- sample_info$Sample[i]
      timepoint <- sample_info$Timepoint[i]
      replicate <- sample_info$Replicate[i]

      expr_values <- expr_matrix[, sample_name]

      data_list[[list_idx]] <- data.table(
        Species = species_name,
        Timepoint = timepoint,
        Replicate = replicate,
        Expression = expr_values
      )
      list_idx <- list_idx + 1
    }
  }

  all_density_data <- rbindlist(data_list[1:(list_idx - 1)])
  all_density_data$Timepoint <- factor(all_density_data$Timepoint, levels = TIME_POINTS)

  plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
  text_color <- if (is_dark_mode) "white" else "black"
  grid_color <- if (is_dark_mode) "gray30" else "gray90"

  ridgeline_pal <- if (!is.null(plot_settings) && !is.null(plot_settings$ridgeline_palette)) {
    plot_settings$ridgeline_palette
  } else {
    "viridis"
  }
  ridgeline_alpha <- if (!is.null(plot_settings) && !is.null(plot_settings$ridgeline_alpha)) {
    plot_settings$ridgeline_alpha
  } else {
    0.7
  }

  p <- ggplot(
    all_density_data,
    aes(x = Expression, y = Timepoint, fill = Timepoint)
  ) +
    geom_density_ridges(scale = 2, alpha = ridgeline_alpha, quantile_lines = TRUE) +
    scale_fill_viridis(discrete = TRUE, option = ridgeline_pal) +
    facet_wrap(~Species, scales = "free_y") +
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
      panel.grid.minor = element_line(color = if (is_dark_mode) "gray20" else "gray95"),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color, face = "italic"),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )

  return(p)
}

#' Create threshold ridgeline plot
#'
#' Creates a bar plot showing percentage of genes above expression threshold.
#'
#' @param species_data_list List of species data structures
#' @param threshold Expression threshold value
#' @param is_dark_mode Logical for dark mode styling
#' @param plot_settings Optional plot settings list
#' @return ggplot object
create_threshold_ridgeline <- function(species_data_list, threshold = 2, is_dark_mode = FALSE, plot_settings = NULL) {
  count_list <- list()
  list_idx <- 1

  for (species_id in names(species_data_list)) {
    species_data <- species_data_list[[species_id]]
    species_name <- switch(species_id,
      "cg" = "C. glabrata",
      "sc" = "S. cerevisiae",
      "kl" = "K. lactis",
      "ca" = "C. albicans"
    )

    expr_matrix <- if (species_id == "cg") species_data$lcpm else species_data[[paste0(species_id, "_lcpm")]]
    sample_info <- if (species_id == "cg") species_data$sample_info else species_data[[paste0(species_id, "_sample_info")]]

    for (i in 1:nrow(sample_info)) {
      sample_name <- sample_info$Sample[i]
      timepoint <- sample_info$Timepoint[i]
      replicate <- sample_info$Replicate[i]

      expr_values <- expr_matrix[, sample_name]
      genes_above <- sum(expr_values > threshold)
      percent_above <- (genes_above / length(expr_values)) * 100

      count_list[[list_idx]] <- data.table(
        Species = species_name,
        Timepoint = timepoint,
        Replicate = replicate,
        GenesAbove = genes_above,
        PercentAbove = percent_above
      )
      list_idx <- list_idx + 1
    }
  }

  count_data <- rbindlist(count_list)
  count_data$Timepoint <- factor(count_data$Timepoint, levels = TIME_POINTS)

  plot_bg_color <- if (is_dark_mode) "#2c3034" else "white"
  text_color <- if (is_dark_mode) "white" else "black"
  grid_color <- if (is_dark_mode) "gray30" else "gray90"

  ridgeline_pal <- if (!is.null(plot_settings) && !is.null(plot_settings$ridgeline_palette)) {
    plot_settings$ridgeline_palette
  } else {
    "viridis"
  }

  p <- ggplot(
    count_data,
    aes(x = Timepoint, y = PercentAbove, fill = Timepoint)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Species) +
    scale_fill_viridis(discrete = TRUE, option = ridgeline_pal) +
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
      panel.grid.minor = element_line(color = if (is_dark_mode) "gray20" else "gray95"),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color, face = "italic"),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color)
    )

  return(p)
}


#' Create phylogenetic tree plot
#'
#' Visualizes the gene tree for an orthogroup with species coloring.
#'
#' @param tree Phylo tree object
#' @param orthogroup_genes List of gene data frames by species
#' @param orthogroup_id Orthogroup ID
#' @param is_dark_mode Logical for dark mode styling
#' @param current_data Current species data object
#' @param species_colors Named list of species colors
#' @param species_config Species configuration list
#' @return ggtree plot object
create_phylo_tree_plot <- function(tree, orthogroup_genes, orthogroup_id, is_dark_mode = FALSE, current_data = NULL, species_colors = NULL, species_config = NULL) {
  if (is.null(tree)) {
    return(NULL)
  }

  # create gene ID mapping using list collection
  mapping_list <- lapply(names(orthogroup_genes), function(sp) {
    if (nrow(orthogroup_genes[[sp]]) > 0) {
      genes_df <- orthogroup_genes[[sp]]
      genes_df$species_code <- sp
      genes_df[, c("gene_id", "display", "species_code"), drop = FALSE]
    } else {
      NULL
    }
  })
  gene_mapping <- rbindlist(Filter(Negate(is.null), mapping_list), fill = TRUE)

  # create metadata dataframe for tips
  tip_metadata <- data.frame(
    label = tree$tip.label,
    stringsAsFactors = FALSE
  )

  tip_metadata$display_label <- NA_character_
  tip_metadata$species <- NA_character_

  # extract lookup table once outside loop
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup
  config <- if (!is.null(species_config)) species_config else DEFAULT_SPECIES_CONFIG

  for (i in 1:nrow(tip_metadata)) {
    current_gene_id <- tip_metadata$label[i]

    lookup_match <- lookup_table[gene_id == current_gene_id, ]

    if (nrow(lookup_match) > 0) {
      sp_code <- as.character(lookup_match$species[1])

      if (sp_code %in% names(config)) {
        tip_metadata$species[i] <- config[[sp_code]]$short

        if (!is.na(lookup_match$gene_name[1]) && lookup_match$gene_name[1] != "") {
          tip_metadata$display_label[i] <- paste0(lookup_match$gene_name[1], " (", current_gene_id, ")")
        } else {
          tip_metadata$display_label[i] <- current_gene_id
        }
      } else {
        tip_metadata$species[i] <- "Unknown"
        tip_metadata$display_label[i] <- current_gene_id
      }
    } else {
      tip_metadata$species[i] <- "Unknown"
      tip_metadata$display_label[i] <- current_gene_id
    }
  }

  # Add colors for any species not in SPECIES_COLORS but present in the data
  unique_species <- unique(tip_metadata$species[!is.na(tip_metadata$species)])
  additional_colors <- c(
    "#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4",
    "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F"
  )
  color_index <- 1
  for (sp in unique_species) {
    if (!sp %in% names(species_colors)) {
      species_colors[[sp]] <- additional_colors[color_index]
      color_index <- (color_index %% length(additional_colors)) + 1
    }
  }

  # Calculate dynamic parameters based on tree size
  n_tips <- length(tree$tip.label)
  max_label_length <- max(nchar(tip_metadata$display_label))

  # Dynamic text size - smaller for larger trees
  text_size <- if (n_tips > 20) 2.5 else if (n_tips > 10) 3.0 else 3.5

  # Dynamic x-limit to accommodate long labels
  x_limit <- 8 + (max_label_length / 10)

  # create the tree plot and add metadata
  p <- ggtree(tree, layout = "rectangular", branch.length = "none") %<+% tip_metadata +
    geom_tiplab(aes(label = display_label, color = species),
      size = text_size, align = TRUE, linesize = 0.5
    ) +
    geom_nodepoint(
      color = if (is_dark_mode) "#666" else "#999",
      alpha = 0.5, size = 2
    ) +
    scale_color_manual(values = species_colors, name = "Species") +
    guides(color = guide_legend(override.aes = list(label = "●", size = 6, linetype = 0))) +
    theme_tree2() +
    xlim_tree(x_limit) +
    theme(
      legend.position = "bottom",
      legend.box.background = element_rect(
        fill = if (is_dark_mode) "#2c3034" else "white",
        color = if (is_dark_mode) "#444" else "#ddd"
      ),
      plot.background = element_rect(
        fill = if (is_dark_mode) "#2c3034" else "white",
        color = NA
      ),
      panel.background = element_rect(
        fill = if (is_dark_mode) "#2c3034" else "white",
        color = NA
      ),
      legend.background = element_rect(
        fill = if (is_dark_mode) "#2c3034" else "white"
      ),
      legend.text = element_text(
        color = if (is_dark_mode) "white" else "black",
        size = 10
      ),
      legend.title = element_text(
        color = if (is_dark_mode) "white" else "black",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(
        color = if (is_dark_mode) "white" else "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      )
    ) +
    ggtitle(paste("Phylogenetic Tree for Orthogroup", orthogroup_id))

  return(p)
}
