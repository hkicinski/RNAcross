library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(ggprism)

setwd("C:/Users/huber/OneDrive/Documents/RNAcross")

source("R/01_config.R")
source("R/02_constants_themes.R")
source("R/03_utils.R")
source("R/04_data_io.R")
source("R/05_orthology_query.R")
source("R/06_data_process.R")

load("data/RData_perSpecies_HOG_clean_11182025_rlog.RData")

inputs <- c("lcpm", "rlog")
pre_process <- c("none", "center_only", "z_score")
pca_args <- list(
  X = list(center = TRUE, scale. = TRUE),
  Y = list(center = TRUE, scale. = FALSE),
  Z = list(center = FALSE, scale. = FALSE)
)

config <- DEFAULT_SPECIES_CONFIG
og_data <- all_species_data$orthofinder$orthogroups
lookup_table <- all_species_data$gene_lookup

hog_gene_map <- og_data %>%
  left_join(lookup_table[, .(gene_id, species)], by = "gene_id") %>%
  filter(!is.na(species)) %>%
  mutate(species = as.character(species)) %>%
  filter(species != "unknown") %>%
  split(.$hog_id) %>%
  lapply(function(hog_df) split(hog_df$gene_id, hog_df$species))

common_hogs <- names(hog_gene_map)[sapply(hog_gene_map, function(hog) {
  length(hog) == length(config) && all(lengths(hog) == 1)
})]

pdf("0409-debug/multispecies_pca_debug_variations.pdf", width = 12, height = 8)

for (input in inputs) {
  for (prep in pre_process) {
    for (arg_name in names(pca_args)) {
      arg <- pca_args[[arg_name]]
      
      if (prep != "none" && (arg$center || arg$scale.)) {
        next
      }
      
      sample_metadata_list <- list()
      expression_matrix_cache <- list()
      
      for (sp_id in names(config)) {
        sp_data <- all_species_data[[sp_id]]
        if (is.null(sp_data)) next
        
        expr_mat <- get_expression_matrix(sp_id, input, sp_data)
        if (is.null(expr_mat)) next
        
        if (prep == "center_only") {
          expr_mat <- t(scale(t(expr_mat), center = TRUE, scale = FALSE))
        } else if (prep == "z_score") {
          expr_mat <- t(scale(t(expr_mat), center = TRUE, scale = TRUE))
          expr_mat[is.na(expr_mat) | is.nan(expr_mat)] <- 0
        }
        
        expression_matrix_cache[[sp_id]] <- expr_mat
        
        s_info <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(sp_id, "_sample_info")]]
        
        sample_metadata_list[[sp_id]] <- data.frame(
          Sample = colnames(expr_mat),
          Species = config[[sp_id]]$short,
          Timepoint = s_info$Timepoint,
          Replicate = s_info$Replicate,
          stringsAsFactors = FALSE
        )
      }
      
      sample_metadata <- rbindlist(sample_metadata_list, fill = TRUE)
      
      sample_matrix <- matrix(NA, nrow = nrow(sample_metadata), ncol = length(common_hogs))
      rownames(sample_matrix) <- sample_metadata$Sample
      colnames(sample_matrix) <- common_hogs
      
      species_hog_expr <- list()
      for (sp_id in names(config)) {
        if (is.null(expression_matrix_cache[[sp_id]])) next
        sp_hog_map <- lapply(hog_gene_map[common_hogs], function(h) if(!is.null(h[[sp_id]])) h[[sp_id]] else NULL)
        names(sp_hog_map) <- common_hogs
        
        species_hog_expr[[sp_id]] <- preaggregate_species_hogs(
          lcpm_matrix = expression_matrix_cache[[sp_id]],
          species_hog_map = sp_hog_map,
          common_hogs = common_hogs,
          aggregation_method = "eigengene",
          species_code = sp_id
        )
      }
      
      sp_short_to_code <- setNames(names(config), sapply(config, `[[`, "short"))
      
      for (i in seq_len(nrow(sample_metadata))) {
        s_name <- sample_metadata$Sample[i]
        sp <- sample_metadata$Species[i]
        sp_code <- sp_short_to_code[[sp]]
        
        if (is.null(sp_code) || is.null(species_hog_expr[[sp_code]])) next
        
        expr_mat <- expression_matrix_cache[[sp_code]]
        if (is.null(expr_mat) || !s_name %in% colnames(expr_mat)) next
        s_idx <- which(colnames(expr_mat) == s_name)
        
        for (j in seq_along(common_hogs)) {
          hog <- common_hogs[j]
          if (!is.null(species_hog_expr[[sp_code]][[hog]])) {
            sample_matrix[i, j] <- species_hog_expr[[sp_code]][[hog]][s_idx]
          }
        }
      }
      
      na_cols <- apply(sample_matrix, 2, function(x) any(is.na(x)))
      sample_matrix <- sample_matrix[, !na_cols, drop = FALSE]
      
      row_has_data <- apply(sample_matrix, 1, function(x) !all(is.na(x)))
      sample_matrix <- sample_matrix[row_has_data, , drop = FALSE]
      sample_metadata <- sample_metadata[row_has_data, , drop = FALSE]
      
      tryCatch({
        pca_res <- prcomp(sample_matrix, scale. = arg$scale., center = arg$center)
        var_exp <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
        
        plot_data <- data.frame(
          PC1 = pca_res$x[, 1],
          PC2 = pca_res$x[, 2],
          Sample = rownames(pca_res$x)
        )
        
        plot_data <- merge(plot_data, sample_metadata, by = "Sample")
        plot_data$Timepoint <- factor(plot_data$Timepoint, levels = TIME_POINTS)
        plot_data <- plot_data[order(as.numeric(plot_data$Timepoint)), ]
        
        path_data <- plot_data %>%
          group_by(Species, Timepoint) %>%
          summarize(PC1 = mean(PC1, na.rm = TRUE), PC2 = mean(PC2, na.rm = TRUE), .groups = "drop") %>%
          arrange(as.numeric(Timepoint))
          
        plot_data$pub_label <- ifelse(is.na(as.numeric(plot_data$Timepoint)), 
                                      as.character(plot_data$Timepoint), 
                                      paste0("T", as.numeric(plot_data$Timepoint)))
        
        label_data <- plot_data %>%
          group_by(Species, Timepoint, pub_label) %>%
          summarize(PC1 = mean(PC1, na.rm = TRUE), PC2 = mean(PC2, na.rm = TRUE), .groups = "drop")
        
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Species)) +
          geom_point(aes(shape = Species), size = 3, alpha = 0.8) +
          geom_path(data = path_data, aes(group = Species), arrow = arrow(length = unit(0.15, "inches"), type = "closed"), alpha = 0.7, linewidth = 0.8) +
          geom_text_repel(data = label_data, aes(label = pub_label), size = 3.5, bg.color = "white", bg.r = 0.15, show.legend = FALSE) +
          labs(
            x = sprintf("PC1 (%.1f%%)", var_exp[1]),
            y = sprintf("PC2 (%.1f%%)", var_exp[2]),
            title = sprintf("In: %s | Pre: %s | PRCOMP: (C=%s, S=%s)", input, prep, arg$center, arg$scale.)
          ) +
          theme_prism(base_size = 14) +
          theme(legend.position = "right", legend.title = element_text(face = "bold"))
          
        print(p)
        
      }, error = function(e) {
        warning(paste("Failed combination:", input, prep, arg_name, ":", e$message))
      })
    }
  }
}

dev.off()
