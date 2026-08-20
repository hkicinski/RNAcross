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
    col_names[[paste0("<i>", config[[sp_code]]$name, "</i>")]] <- col_name
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
    rownames = FALSE,
    escape = FALSE
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
                                           plot_settings = NULL,
                                           study_design = GRE_study_design()) {
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
  expression_data <- extract_ortholog_expression(gene_mapping, species_data_list, config, transform_type,
                                                 study_design = study_design)

  # prepare heatmap matrix
  heatmap_matrix <- prepare_heatmap_matrix(expression_data, normalization, study_design = study_design)

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
    config <- DEFAULT_SPECIES_CONFIG
  }

  if (!is.null(query_result$source) && query_result$source == "gene_lookup_no_orthogroup") {
    return(tags$div(
      h5("Gene Information"),
      div(
        class = "alert alert-warning py-2",
        icon("exclamation-triangle"),
        " This gene is not assigned to any orthogroup."
      ),
      tags$ul(
        tags$li(paste("Gene ID:", query_result$gene_id)),
        tags$li(paste("Species:", {
          sp <- names(query_result$genes_by_species)[1]
          if (!is.null(sp) && sp %in% names(config)) config[[sp]]$name else sp
        }))
      )
    ))
  }

  if (!is.null(query_result$source) && query_result$source == "synteny_aided") {
    gene_counts <- sapply(query_result$genes_by_species, nrow)
    total_genes <- sum(gene_counts)

    return(tags$div(
      h5("Synteny-Aided Orthology"),
      div(
        class = "alert alert-info py-2",
        icon("link"),
        " Based on YGOB/CGOB synteny data. Not an OrthoFinder orthogroup."
      ),
      tags$ul(
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
    ))
  }

  gene_counts <- sapply(query_result$genes_by_species, nrow)
  total_genes <- sum(gene_counts)

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

  if (!is.null(query_result$source) && query_result$source == "gene_lookup_no_orthogroup") {
    return(NULL)
  }

  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }

  gene_list <- lapply(names(query_result$genes_by_species), function(sp) {
    genes_df <- query_result$genes_by_species[[sp]]
    if (nrow(genes_df) > 0) {
      species_name <- if (!is.null(config[[sp]])) config[[sp]]$name else sp

      genes_df$Species <- paste0("<i>", species_name, "</i>")
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

  caption_text <- if (!is.null(query_result$source) && query_result$source == "synteny_aided") {
    paste("Synteny-aided orthologs (YGOB/CGOB) | Total genes:", nrow(all_genes))
  } else {
    paste("Orthogroup:", query_result$orthogroup, "| Total genes:", nrow(all_genes))
  }

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
    escape = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-weight: bold;",
      caption_text
    )
  )

  return(dt)
}

#' Create detailed ortholog mapping table
#'
#' Creates a detailed data frame showing all paralogs for a gene set.
#'
#' @param gene_mapping List of gene mappings
#' @param config Species configuration list
#' @return data.frame with ortholog mapping details
create_ortholog_mapping_table <- function(gene_mapping, config) {
  if (is.null(gene_mapping) || length(gene_mapping) == 0) {
    return(NULL)
  }

  # build expanded data frame showing ALL paralogs
  rows_list <- list()

  for (gene_map in gene_mapping) {
    input_gene <- gene_map$original

    # get max number of paralogs across species for this gene
    max_paralogs <- max(sapply(names(config), function(sp_code) {
      orthologs <- gene_map[[sp_code]]
      if (is.null(orthologs)) 0 else length(orthologs)
    }), na.rm = TRUE)

    # create one row per paralog position
    for (i in 1:max_paralogs) {
      row_data <- list(InputGene = if (i == 1) input_gene else "")

      for (sp_code in names(config)) {
        col_name <- paste0(config[[sp_code]]$short, "_Ortholog")
        orthologs <- gene_map[[sp_code]]

        if (is.null(orthologs) || length(orthologs) == 0) {
          row_data[[col_name]] <- if (i == 1) "-" else ""
        } else if (i <= length(orthologs)) {
          # show paralog with indicator if multiple exist
          if (length(orthologs) == 1) {
            row_data[[col_name]] <- orthologs[i]
          } else {
            row_data[[col_name]] <- paste0(orthologs[i], " [", i, "/", length(orthologs), "]")
          }
        } else {
          row_data[[col_name]] <- ""
        }
      }

      rows_list[[length(rows_list) + 1]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
    }
  }

  if (length(rows_list) > 0) {
    mapping_df <- rbindlist(rows_list, fill = TRUE)
    return(as.data.frame(mapping_df))
  } else {
    return(data.frame())
  }
}

# ---------------------------------------------------------------------------
# Aesthetic editor -> ggprism translation
# ---------------------------------------------------------------------------

#' px (plotly) -> pt (ggplot theme text)
px_to_pt <- function(px) as.numeric(px) * 0.75

#' px (plotly) -> ggplot linewidth
px_to_linewidth <- function(px) as.numeric(px) / 2

#' px (plotly) -> ggplot point size
px_to_pointsize <- function(px) as.numeric(px) / 3

#' CSS colour from plotly -> something ggplot2 accepts
css_to_r_color <- function(x, default = NULL) {
  if (is.null(x)) return(default)
  x <- as.character(x)[1]
  if (!nzchar(x)) return(default)
  m <- regmatches(x, regexec("^rgba?\\(([^)]*)\\)$", x))[[1]]
  if (length(m) == 2) {
    parts <- suppressWarnings(as.numeric(strsplit(m[2], ",")[[1]]))
    if (length(parts) < 3 || any(!is.finite(parts[1:3]))) return(default)
    alpha <- if (length(parts) >= 4 && is.finite(parts[4])) parts[4] else 1
    if (alpha <= 0) return("transparent")
    return(grDevices::rgb(parts[1], parts[2], parts[3], alpha = alpha * 255, maxColorValue = 255))
  }
  x
}

#' Plotly dash pattern -> ggplot2 linetype
plotly_dash_to_linetype <- function(dash) {
  switch(as.character(dash),
    "solid" = "solid", "dash" = "dashed", "dot" = "dotted",
    "dashdot" = "dotdash", "longdash" = "longdash", "longdashdot" = "twodash",
    "solid")
}

#' element_text from edited properties only; NULL entries inherit
editor_element_text <- function(family = NULL, size = NULL, color = NULL, bold = NULL, ...) {
  ggplot2::element_text(
    family = family,
    size = if (!is.null(size)) px_to_pt(size) else NULL,
    colour = css_to_r_color(color),
    face = if (is.null(bold)) NULL else if (isTRUE(bold)) "bold" else "plain",
    ...
  )
}

#' Match a series label to a recorded trace name: exact, then leading symbol
editor_trace_style <- function(traces, label) {
  if (is.null(traces) || length(traces) == 0) return(NULL)
  if (!is.null(traces[[label]])) return(traces[[label]])
  norm <- function(x) toupper(trimws(sub("\\s*\\(.*$", "", gsub("</?b>", "", x))))
  hit <- which(vapply(names(traces), function(n) identical(norm(n), norm(label)), logical(1)))
  if (length(hit) == 0) return(NULL)
  traces[[hit[1]]]
}

#' Apply recorded editor state to a ggprism figure
#' @param st semantic style list for one plot id
#' @param scope "full" also rewrites titles/labels/ranges; "theme" is fonts + frame only
apply_prism_editor_styles <- function(p, st, scope = c("full", "theme")) {
  scope <- match.arg(scope)
  if (is.null(st) || length(st) == 0) return(p)
  strip_bold <- function(x) gsub("</?b>", "", x)
  th <- list()

  # plot title
  if (!is.null(st$title)) {
    if (scope == "full" && !is.null(st$title$text)) {
      p <- p + ggplot2::labs(title = strip_bold(st$title$text))
    }
    th$plot.title <- editor_element_text(st$title$family, st$title$size, st$title$color, st$title$bold)
  }

  # axes
  for (ax in c("xaxis", "yaxis")) {
    a <- st[[ax]]
    if (is.null(a)) next
    side <- substr(ax, 1, 1)
    if (scope == "full" && !is.null(a$title)) {
      p <- p + if (side == "x") ggplot2::labs(x = strip_bold(a$title)) else ggplot2::labs(y = strip_bold(a$title))
    }
    if (!is.null(a$title_family) || !is.null(a$title_size) || !is.null(a$title_color) || !is.null(a$title_bold)) {
      th[[paste0("axis.title.", side)]] <-
        editor_element_text(a$title_family, a$title_size, a$title_color, a$title_bold)
    }
    if (!is.null(a$tick_family) || !is.null(a$tick_size) || !is.null(a$tick_color)) {
      #keep the trajectory panel's angled timepoint labels
      extra <- if (side == "x" && scope == "full") list(angle = 45, hjust = 1) else list()
      th[[paste0("axis.text.", side)]] <- do.call(
        editor_element_text,
        c(list(a$tick_family, a$tick_size, a$tick_color), extra)
      )
    }
    if (!is.null(a$line_width) || !is.null(a$line_color)) {
      th[[paste0("axis.line.", side)]] <- ggplot2::element_line(
        colour = css_to_r_color(a$line_color),
        linewidth = if (!is.null(a$line_width)) px_to_linewidth(a$line_width) else NULL
      )
    }
    if (!is.null(a$grid_width) || !is.null(a$grid_color)) {
      lw <- if (!is.null(a$grid_width)) px_to_linewidth(a$grid_width) else 0.5
      th[[paste0("panel.grid.major.", side)]] <- if (lw <= 0) ggplot2::element_blank() else
        ggplot2::element_line(colour = css_to_r_color(a$grid_color, "grey85"), linewidth = lw)
    }
    if (scope == "full" && side == "y" && length(a$range) == 2) {
      rng <- suppressWarnings(as.numeric(unlist(a$range)))
      if (all(is.finite(rng))) p <- p + ggplot2::coord_cartesian(ylim = rng)
    }
  }

  # background
  if (!is.null(st$background)) {
    if (!is.null(st$background$plot)) {
      th$panel.background <- ggplot2::element_rect(fill = css_to_r_color(st$background$plot), colour = NA)
    }
    if (!is.null(st$background$paper)) {
      th$plot.background <- ggplot2::element_rect(fill = css_to_r_color(st$background$paper), colour = NA)
    }
  }

  # legend
  lg <- st$legend
  if (!is.null(lg)) {
    if (identical(lg$show, FALSE)) {
      th$legend.position <- "none"
    } else if (!is.null(lg$x) || !is.null(lg$y)) {
      x <- as.numeric(lg$x %||% 1.02); y <- as.numeric(lg$y %||% 1)
      if (is.finite(x) && is.finite(y) && x >= 0 && x <= 1 && y >= 0 && y <= 1) {
        if (utils::packageVersion("ggplot2") >= "3.5.0") {
          th$legend.position <- "inside"
          th$legend.position.inside <- c(x, y)
        } else {
          th$legend.position <- c(x, y)
        }
      } else {
        th$legend.position <- if (x > 1) "right" else if (x < 0) "left" else if (y > 1) "top" else "bottom"
      }
    }
    if (!is.null(lg$orientation)) {
      th$legend.direction <- if (identical(lg$orientation, "h")) "horizontal" else "vertical"
    }
    if (!is.null(lg$bgcolor)) {
      th$legend.background <- ggplot2::element_rect(fill = css_to_r_color(lg$bgcolor), colour = NA)
    }
    if (!is.null(lg$item_family) || !is.null(lg$item_size) || !is.null(lg$item_color) || !is.null(lg$item_bold)) {
      th$legend.text <- editor_element_text(lg$item_family, lg$item_size, lg$item_color, lg$item_bold)
    }
    #theme_prism blanks the legend title, so a title edit must re-enable it
    titled <- !is.null(lg$title_text) && nzchar(strip_bold(lg$title_text))
    if (titled || !is.null(lg$title_family) || !is.null(lg$title_size) ||
        !is.null(lg$title_color) || !is.null(lg$title_bold)) {
      th$legend.title <- editor_element_text(lg$title_family, lg$title_size, lg$title_color, lg$title_bold)
    } else if (!is.null(lg$title_text)) {
      th$legend.title <- ggplot2::element_blank()
    }
    if (scope == "full" && !is.null(lg$title_text)) {
      p <- p + ggplot2::labs(color = strip_bold(lg$title_text))
    }
  }

  if (length(th) > 0) p <- p + do.call(ggplot2::theme, th)
  p
}

# ---------------------------------------------------------------------------
# Similarity search: two-panel ggprism output (trajectory + null density)
# Mirrors 05162026-geneexp-sim/script/02_gene_similarity_crossspecies_test.R
# ---------------------------------------------------------------------------

#' Top panel: query vs top-N cross-species trajectory, ggprism styled
#' @param styles optional aesthetic-editor state for the interactive plot
similarity_trajectory_prism <- function(res, query_gene_name, ref_species, tgt_species,
                                        top_n = 5, styles = NULL) {
  top_ids <- head(res$table$gene_id, top_n)
  pd <- res$plot_data %>% dplyr::filter(type == "query" | gene_id %in% top_ids)
  #richer query label for the poster (matches the reference figure)
  pd$label[pd$type == "query"] <- paste0(query_gene_name, " (", ref_species, ", query)")

  #per-series defaults, overridden by trace name from the editor
  lv <- sort(unique(pd$label))
  is_query <- vapply(lv, function(l) any(pd$type[pd$label == l] == "query"), logical(1))
  col_v <- stats::setNames(viridis::viridis(length(lv)), lv)
  lw_v  <- stats::setNames(ifelse(is_query, 2, 1), lv)
  lt_v  <- stats::setNames(ifelse(is_query, "dashed", "solid"), lv)
  sz_v  <- stats::setNames(rep(2, length(lv)), lv)

  traces <- if (!is.null(styles)) styles$traces else NULL
  for (l in lv) {
    s <- editor_trace_style(traces, l)
    if (is.null(s)) next
    if (!is.null(s$color))       col_v[[l]] <- css_to_r_color(s$color, col_v[[l]])
    if (!is.null(s$width))       lw_v[[l]]  <- px_to_linewidth(s$width)
    if (!is.null(s$dash))        lt_v[[l]]  <- plotly_dash_to_linetype(s$dash)
    if (!is.null(s$marker_size)) sz_v[[l]]  <- px_to_pointsize(s$marker_size)
    if (!is.null(s$mode)) {
      if (!grepl("lines", s$mode))   lw_v[[l]] <- 0
      if (!grepl("markers", s$mode)) sz_v[[l]] <- 0
    }
  }

  p <- ggplot2::ggplot(pd, ggplot2::aes(x = Timepoint, y = consensus_z, group = label, color = label)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = label, linetype = label)) +
    ggplot2::geom_point(ggplot2::aes(size = label)) +
    ggplot2::scale_linewidth_manual(values = lw_v, guide = "none") +
    ggplot2::scale_linetype_manual(values = lt_v, guide = "none") +
    ggplot2::scale_size_manual(values = sz_v, guide = "none") +
    ggprism::theme_prism() +
    ggplot2::scale_color_manual(values = col_v) +
    ggplot2::labs(
      title = paste0("Cross-Species: ", query_gene_name, " ( ", ref_species, " ) vs Top ", top_n, " in ", tgt_species),
      y = "Z-Scored Consensus Expression", color = "Gene") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  apply_prism_editor_styles(p, styles, scope = "full")
}

#' Bottom panel: background null density with dashed top-N markers, ggprism styled
#' @param styles optional aesthetic-editor state (fonts/frame only)
similarity_null_prism <- function(res, tgt_species, top_n = 5, styles = NULL) {
  null_df <- data.frame(pearson_r = res$null_cors)
  top_r <- head(res$table$pearson_r, top_n)
  p <- ggplot2::ggplot(null_df, ggplot2::aes(x = pearson_r)) +
    ggplot2::geom_density(fill = "grey80", color = "black") +
    ggplot2::geom_vline(xintercept = top_r, color = viridis::viridis(length(top_r)),
                        linetype = "dashed", linewidth = 1) +
    ggprism::theme_prism() +
    ggplot2::labs(
      title = paste0("Background Null ( ", tgt_species, " )"),
      x = "Pearson Correlation", y = "Density",
      subtitle = paste0("N = ", length(res$null_cors), " genes. Dashed = Top ", top_n))

  apply_prism_editor_styles(p, styles, scope = "theme")
}

#' Combined two-panel ggprism figure for one target species (static export)
similarity_twopanel_prism <- function(sim_res, target = NULL, top_n = 5, styles = NULL) {
  if (is.null(target)) target <- names(sim_res$raw)[1]
  res <- sim_res$raw[[target]]
  p1 <- similarity_trajectory_prism(res, sim_res$query_gene, sim_res$ref_species, target, top_n, styles)
  p2 <- similarity_null_prism(res, target, top_n, styles)
  cowplot::plot_grid(p1, p2, ncol = 1, align = "v")
}
