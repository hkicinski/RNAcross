library(tidyverse)
library(data.table)
library(here)
library(ggprism)
library(cowplot)

query_gene_name <- "PHO4"
query_species <- "ca"
top_x <- 20
null_n <- 1000
set.seed(2026)

out_dir <- here("05162026-geneexp-sim", "output")
fig_dir <- here("05162026-geneexp-sim", "figure")

load(here("data", "RData_perSpecies_HOG_clean_11182025_rlog.RData"))

sp_data <- all_species_data[[query_species]]
mat <- if (!is.null(sp_data$rlog)) sp_data$rlog else sp_data[[paste0(query_species, "_rlog")]]
sample_info <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(query_species, "_sample_info")]]

gene_lookup <- as.data.table(all_species_data$gene_lookup)
lookup_match <- gene_lookup[species == query_species & toupper(gene_name) == toupper(query_gene_name)]

if (nrow(lookup_match) == 0) {
  if (toupper(query_gene_name) %in% toupper(rownames(mat))) {
    query_gene_id <- rownames(mat)[toupper(rownames(mat)) == toupper(query_gene_name)][1]
  } else {
    stop(paste("query gene", query_gene_name, "not found in species", query_species))
  }
} else {
  query_gene_id <- lookup_match$gene_id[1]
  if (!query_gene_id %in% rownames(mat)) {
    stop(paste("query gene mapped to", query_gene_id, "but not found in expression matrix"))
  }
}

print(paste("Query Gene:", query_gene_name, "-> ID:", query_gene_id))
print("Timepoints & Replicates:")
print(table(sample_info$Timepoint, sample_info$Replicate))

mat_long <- mat %>% 
  as.data.frame() %>% 
  rownames_to_column("gene_id") %>%
  pivot_longer(-gene_id, names_to = "Sample", values_to = "expression") %>%
  left_join(sample_info, by = "Sample")
#z score across all TPs for SD and mean remoal
z_replicate <- mat_long %>%
  group_by(gene_id, Replicate) %>%
  mutate(
    expr_sd = sd(expression, na.rm = TRUE),
    z_score = if_else(is.na(expr_sd) | expr_sd == 0, 0, (expression - mean(expression, na.rm = TRUE)) / expr_sd)
  ) %>%
  ungroup()

consensus <- z_replicate %>%
  group_by(gene_id, Timepoint) %>%
  summarise(consensus_z = mean(z_score, na.rm = TRUE), .groups = "drop")

print(paste("Analyzed", length(unique(consensus$gene_id)), "genes after building consensus profiles."))

tp_order <- unique(sample_info$Timepoint)
consensus$Timepoint <- factor(consensus$Timepoint, levels = tp_order)

consensus_wide <- consensus %>%
  pivot_wider(names_from = Timepoint, values_from = consensus_z) %>%
  column_to_rownames("gene_id")

q_profile <- as.numeric(consensus_wide[query_gene_id, ])

cor_results <- apply(consensus_wide, 1, function(x) {
  c(
    spearman = cor(x, q_profile, method = "spearman", use = "pairwise.complete.obs"),
    pearson = cor(x, q_profile, method = "pearson", use = "pairwise.complete.obs")
  )
})

cor_df <- t(cor_results) %>% 
  as.data.frame() %>% 
  rownames_to_column("gene_id") %>%
  left_join(gene_lookup %>% filter(species == query_species) %>% select(gene_id, gene_name) %>% distinct(gene_id, .keep_all = TRUE), by = "gene_id") %>%
  arrange(desc(pearson), desc(spearman)) %>%
  mutate(
    spearman_rank = row_number(),
    pearson_rank = rank(-pearson, ties.method = "min")
  )

top_matches <- head(cor_df, top_x)

out_file <- file.path(out_dir, paste0("gene_similarity_", query_gene_name, "_top", top_x, "_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(top_matches, out_file)
print(paste("Saved top matches to", out_file))
print("Top 5 Hits:")
print(head(top_matches, 5))

query_rank <- top_matches %>% filter(gene_id == query_gene_id)
if(nrow(query_rank) > 0) {
  print(paste("Sanity check passed: Query gene spearman =", query_rank$spearman, "at rank", query_rank$spearman_rank))
} else {
  print("WARNING: Query gene not found in top hits!")
}

all_genes <- cor_df$gene_id
null_genes <- sample(all_genes, min(null_n, length(all_genes)))
null_dist <- cor_df %>% filter(gene_id %in% null_genes)

plot_data <- consensus %>% filter(gene_id %in% top_matches$gene_id[1:5])
plot_data$label <- plot_data$gene_id

for (i in 1:nrow(plot_data)) {
  g_name <- cor_df$gene_name[cor_df$gene_id == plot_data$gene_id[i]]
  if (!is.na(g_name) && g_name != "") {
    plot_data$label[i] <- paste0(g_name, " (", plot_data$gene_id[i], ")")
  }
}

p1 <- ggplot(plot_data, aes(x = Timepoint, y = consensus_z, group = label, color = label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_prism() +
  scale_color_viridis_d() +
  labs(title = paste("Consensus Trajectories (Top 5 matches to", query_gene_name, ")"),
       y = "Z-Scored Consensus Expression") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(null_dist, aes(x = spearman)) +
  geom_density(fill = "grey80", color = "black") +
  geom_vline(xintercept = top_matches$spearman[1:5], color = viridis::viridis(5), linetype = "dashed", linewidth = 1) +
  theme_prism() +
  labs(title = "Background Null Distribution",
       x = "Spearman Correlation", y = "Density",
       subtitle = paste("Sampled N =", null_n, "genes. Dashed lines = Top 5"))

final_plot <- plot_grid(p1, p2, ncol = 1, align = "v")

fig_file <- file.path(fig_dir, paste0("gene_similarity_", query_gene_name, "_plot_", format(Sys.Date(), "%Y%m%d"), ".png"))
ggsave(fig_file, final_plot, width = 10, height = 10, dpi = 300, bg = "white")
print(paste("Saved figure to", fig_file))
