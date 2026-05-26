library(tidyverse)
library(data.table)
library(here)
library(ggprism)
library(cowplot)

query_gene_name <- "PHO84"
reference_species <- "cg"
top_x <- 20
n_null <- 1000
n_perm <- 1000
set.seed(2026)

all_species <- c("sc", "cg", "ca", "kl")
target_species_list <- setdiff(all_species, reference_species)

out_dir <- here("05162026-geneexp-sim", "output")
fig_dir <- here("05162026-geneexp-sim", "figure")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

load(here("data", "RData_perSpecies_HOG_clean_11182025_rlog.RData"))
gene_lookup <- as.data.table(all_species_data$gene_lookup)

tp_to_minutes <- function(tp) {
  sapply(tp, function(x) {
    if (grepl("min$", x)) return(as.numeric(gsub("min$", "", x)))
    if (grepl("h$", x)) return(as.numeric(gsub("h$", "", x)) * 60)
    NA_real_
  }, USE.NAMES = FALSE)
}

get_species_data <- function(sp) {
  sp_data <- all_species_data[[sp]]
  mat <- if (!is.null(sp_data[["rlog"]])) sp_data[["rlog"]] else sp_data[[paste0(sp, "_rlog")]]
  info <- if (!is.null(sp_data[["sample_info"]])) sp_data[["sample_info"]] else sp_data[[paste0(sp, "_sample_info")]]
  list(mat = mat, sample_info = info)
}

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

filter_invariant <- function(consensus_wide) {
  row_sds <- apply(consensus_wide, 1, sd, na.rm = TRUE)
  consensus_wide[!is.na(row_sds) & row_sds > 0, , drop = FALSE]
}

print("=== Timepoint Grids (all species) ===")
for (sp in all_species) {
  sp_d <- get_species_data(sp)
  print(paste("Species:", sp))
  print(table(sp_d$sample_info$Timepoint, sp_d$sample_info$Replicate))
}

ref_data <- get_species_data(reference_species)
ref_consensus_wide <- build_consensus_wide(ref_data$mat, ref_data$sample_info)

lookup_match <- gene_lookup[species == reference_species & toupper(gene_name) == toupper(query_gene_name)]
if (nrow(lookup_match) == 0) {
  if (toupper(query_gene_name) %in% toupper(rownames(ref_data$mat))) {
    query_gene_id <- rownames(ref_data$mat)[toupper(rownames(ref_data$mat)) == toupper(query_gene_name)][1]
  } else {
    stop(paste("query gene", query_gene_name,
               "not found in lookup table or expression matrix for species", reference_species))
  }
} else {
  query_gene_id <- lookup_match$gene_id[1]
  if (!query_gene_id %in% rownames(ref_data$mat)) {
    stop(paste("query gene", query_gene_name, "mapped to", query_gene_id,
               "in lookup table but absent from expression matrix"))
  }
}

print(paste("Query Gene:", query_gene_name, "-> ID:", query_gene_id, "in", reference_species))

ref_tp_labels <- colnames(ref_consensus_wide)
ref_tp_mins <- tp_to_minutes(ref_tp_labels)
ref_query_profile <- as.numeric(ref_consensus_wide[query_gene_id, ])

print("Reference query profile:")
print(data.frame(Timepoint = ref_tp_labels, Minutes = ref_tp_mins,
                 Z_Score = round(ref_query_profile, 4)))

for (tgt_sp in target_species_list) {
  print(paste("=========================================="))
  print(paste("Target species:", tgt_sp))
  print(paste("=========================================="))

  tgt_data <- get_species_data(tgt_sp)
  tgt_consensus_wide <- build_consensus_wide(tgt_data$mat, tgt_data$sample_info)

  tgt_tp_labels <- colnames(tgt_consensus_wide)
  tgt_tp_mins <- tp_to_minutes(tgt_tp_labels)

  grids_match <- identical(sort(ref_tp_mins), sort(tgt_tp_mins))
  print(paste("Grids identical:", grids_match))

  if (grids_match) {
    aligned_query <- ref_query_profile[match(tgt_tp_mins, ref_tp_mins)]
    interp_gaps <- rep(0, length(tgt_tp_mins))
  } else {
    aligned <- approx(x = ref_tp_mins, y = ref_query_profile,
                      xout = tgt_tp_mins, method = "linear")
    aligned_query <- aligned$y
    interp_gaps <- sapply(tgt_tp_mins, function(t) {
      if (t %in% ref_tp_mins) return(0)
      lower <- max(ref_tp_mins[ref_tp_mins < t])
      upper <- min(ref_tp_mins[ref_tp_mins > t])
      upper - lower
    })
  }

  print(data.frame(
    Target_TP = tgt_tp_labels,
    Minutes = tgt_tp_mins,
    Aligned_Query_Z = round(aligned_query, 4),
    Interp_Gap_Min = interp_gaps
  ))
  print(paste("Maximum interpolation gap:", max(interp_gaps), "minutes"))

  n_before <- nrow(tgt_consensus_wide)
  tgt_filtered <- filter_invariant(tgt_consensus_wide)
  n_removed <- n_before - nrow(tgt_filtered)
  print(paste("Filtered", n_removed, "invariant genes. Remaining:", nrow(tgt_filtered)))

  if (any(is.na(aligned_query))) {
    print("WARNING: NA values in aligned query (target timepoints outside reference range)")
  }

  cors <- apply(tgt_filtered, 1, function(x) {
    cor(x, aligned_query, method = "pearson", use = "pairwise.complete.obs")
  })

  cor_df <- tibble(gene_id = names(cors), pearson_r = cors) %>%
    left_join(
      gene_lookup %>% filter(species == tgt_sp) %>%
        select(gene_id, gene_name) %>% distinct(gene_id, .keep_all = TRUE),
      by = "gene_id"
    ) %>%
    filter(!is.na(pearson_r)) %>%
    arrange(desc(pearson_r)) %>%
    mutate(rank = row_number())

  print("Correlation distribution summary:")
  print(paste("  Min:", round(min(cor_df$pearson_r), 4)))
  print(paste("  Median:", round(median(cor_df$pearson_r), 4)))
  print(paste("  95th pctl:", round(quantile(cor_df$pearson_r, 0.95), 4)))
  print(paste("  Max:", round(max(cor_df$pearson_r), 4)))
  for (r in c(100, 500, 1000)) {
    if (nrow(cor_df) >= r) {
      print(paste("  Correlation at rank", r, ":", round(cor_df$pearson_r[r], 4)))
    }
  }

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

  print(paste("Top 5 hits for", tgt_sp, ":"))
  print(head(top_matches %>% select(gene_id, gene_name, pearson_r, null_percentile, perm_p_value, rank), 5))

  out_file <- file.path(out_dir, paste0(
    "crosssp_", query_gene_name, "_", reference_species, "_vs_", tgt_sp,
    "_top", top_x, "_", format(Sys.Date(), "%Y%m%d"), ".csv"
  ))
  write_csv(top_matches, out_file)
  print(paste("Saved to", out_file))

  top5_ids <- top_matches$gene_id[1:5]
  tgt_long <- tgt_filtered[top5_ids, , drop = FALSE] %>%
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
    label = paste0(query_gene_name, " (", reference_species, ", query)")
  )

  plot_data <- bind_rows(tgt_long, ref_plot)
  chrono_order <- tgt_tp_labels[order(tgt_tp_mins)]
  plot_data$Timepoint <- factor(plot_data$Timepoint, levels = chrono_order)
  plot_data$type <- ifelse(plot_data$gene_id == "query", "query", "match")

  p1 <- ggplot(plot_data, aes(x = Timepoint, y = consensus_z,
                               group = label, color = label)) +
    geom_line(aes(linewidth = type, linetype = type)) +
    geom_point(size = 2) +
    scale_linewidth_manual(values = c("query" = 2, "match" = 1), guide = "none") +
    scale_linetype_manual(values = c("query" = "dashed", "match" = "solid"), guide = "none") +
    theme_prism() +
    scale_color_viridis_d() +
    labs(
      title = paste("Cross-Species:", query_gene_name, "(", reference_species, ") vs Top 5 in", tgt_sp),
      y = "Z-Scored Consensus Expression"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  null_df <- data.frame(pearson_r = null_cors)
  p2 <- ggplot(null_df, aes(x = pearson_r)) +
    geom_density(fill = "grey80", color = "black") +
    geom_vline(xintercept = top_matches$pearson_r[1:5],
               color = viridis::viridis(5), linetype = "dashed", linewidth = 1) +
    theme_prism() +
    labs(
      title = paste("Background Null (", tgt_sp, ")"),
      x = "Pearson Correlation", y = "Density",
      subtitle = paste("N =", min(n_null, nrow(cor_df)), "genes. Dashed = Top 5")
    )

  final_plot <- plot_grid(p1, p2, ncol = 1, align = "v")

  fig_file <- file.path(fig_dir, paste0(
    "crosssp_", query_gene_name, "_", reference_species, "_vs_", tgt_sp,
    "_", format(Sys.Date(), "%Y%m%d"), ".png"
  ))
  ggsave(fig_file, final_plot, width = 12, height = 10, dpi = 300, bg = "white")
  print(paste("Saved figure to", fig_file))
}
