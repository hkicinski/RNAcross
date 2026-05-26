library(dplyr)
library(data.table)

setwd("C:/Users/huber/OneDrive/Documents/RNAcross")
source("R/01_config.R")
source("R/02_constants_themes.R")
source("R/03_utils.R")
source("R/04_data_io.R")
source("R/05_orthology_query.R")
source("R/06_data_process.R")

load("data/RData_perSpecies_HOG_clean_11182025_rlog.RData")

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

expression_matrix_cache <- list()
sample_metadata_list <- list()

for (sp_id in names(config)) {
  sp_data <- all_species_data[[sp_id]]
  if (is.null(sp_data)) next
  expr_mat <- get_expression_matrix(sp_id, "lcpm", sp_data)
  if (is.null(expr_mat)) next
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

for (sp_id in names(config)) {
  expr_mat <- expression_matrix_cache[[sp_id]]
  if (is.null(expr_mat)) next
  sp_samples <- which(sample_metadata$Species == config[[sp_id]]$short)
  for (j in seq_along(common_hogs)) {
    hog <- common_hogs[j]
    gene_id <- hog_gene_map[[hog]][[sp_id]]
    if (is.null(gene_id) || !gene_id %in% rownames(expr_mat)) next
    sample_matrix[sp_samples, j] <- expr_mat[gene_id, ]
  }
}

na_cols <- apply(sample_matrix, 2, function(x) any(is.na(x)))
sample_matrix <- sample_matrix[, !na_cols, drop = FALSE]

cat("=== SAMPLE MATRIX DIMENSIONS ===\n")
cat(sprintf("Rows (samples): %d\n", nrow(sample_matrix)))
cat(sprintf("Cols (1:1:1:1 HOGs): %d\n", ncol(sample_matrix)))
cat(sprintf("HOGs dropped due to NA: %d\n", sum(na_cols)))
cat(sprintf("NAs remaining: %d\n\n", sum(is.na(sample_matrix))))

cat("=== SAMPLE METADATA ===\n")
print(as.data.frame(sample_metadata))

cat("\n=== FIRST 5 HOGs x ALL SAMPLES (raw log2CPM) ===\n")
subset_mat <- sample_matrix[, 1:min(5, ncol(sample_matrix))]
gene_labels <- sapply(colnames(subset_mat), function(hog) {
  genes <- sapply(names(config), function(sp) {
    gid <- hog_gene_map[[hog]][[sp]]
    if (is.null(gid)) "NA" else gid
  })
  paste(paste0(names(config), "=", genes), collapse = " | ")
})
cat("\nGene mapping for displayed HOGs:\n")
for (i in seq_along(gene_labels)) {
  cat(sprintf("  %s : %s\n", colnames(subset_mat)[i], gene_labels[i]))
}
cat("\n")
print(round(subset_mat, 3))

cat("\n=== VALUE RANGES PER SPECIES ===\n")
for (sp_short in unique(sample_metadata$Species)) {
  sp_rows <- which(sample_metadata$Species == sp_short)
  sp_vals <- sample_matrix[sp_rows, ]
  cat(sprintf("  %s : min=%.2f  mean=%.2f  max=%.2f  sd=%.2f\n",
              sp_short, min(sp_vals, na.rm=TRUE), mean(sp_vals, na.rm=TRUE),
              max(sp_vals, na.rm=TRUE), sd(as.vector(sp_vals), na.rm=TRUE)))
}

write.csv(
  data.frame(Sample = rownames(sample_matrix), sample_metadata, sample_matrix, check.names = FALSE),
  "0409-debug/sample_matrix_1to1_lcpm.csv",
  row.names = FALSE
)
cat("\nFull matrix saved to: 0409-debug/sample_matrix_1to1_lcpm.csv\n")
