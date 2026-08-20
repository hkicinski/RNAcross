suppressPackageStartupMessages({
  library(edgeR); library(DESeq2); library(dplyr)
  library(tibble); library(stringr); library(readr)
})

parse_tp   <- function(x) as.integer(str_extract(x, "\\d+(?=m\\.rep)"))
parse_cond <- function(x) str_extract(x, "mock|noPi")

read_counts <- function(f) {
  raw <- read_tsv(f, show_col_types = FALSE)
  ids <- sub("^gene-", "", raw[[1]])
  m <- apply(as.matrix(raw[, -(1:2)]), 2, function(x) as.integer(round(x)))
  rownames(m) <- ids
  list(counts = m, gene_name = setNames(raw[[2]], ids))
}

make_lcpm <- function(m) {
  m <- m[rowSums(m >= 10) >= 2, ]
  edgeR::cpm(calcNormFactors(DGEList(counts = m), method = "TMM"), log = TRUE)
}

make_rlog <- function(m) {
  cd <- data.frame(condition = factor(parse_cond(colnames(m))),
                   timepoint = factor(parse_tp(colnames(m))),
                   row.names = colnames(m))
  design <- if (nlevels(cd$condition) > 1) ~ condition + timepoint else ~ timepoint
  dds <- DESeq(DESeqDataSetFromMatrix(m, cd, design), quiet = TRUE)
  assay(rlog(dds, blind = FALSE))[rowSums(m) >= 10, ]
}

build_sample_info <- function(samples) {
  tibble(Sample = samples,
         Condition = parse_cond(samples),
         tp_min = parse_tp(samples),
         Replicate = as.integer(str_extract(samples, "(?<=rep)\\d+"))) |>
    arrange(factor(Condition, levels = c("noPi", "mock")), tp_min, Replicate) |>
    group_by(Condition, tp_min) |>
    mutate(StandardizedReplicate = dense_rank(Replicate)) |>
    ungroup() |>
    transmute(Sample, Condition,
              Timepoint = ifelse(tp_min < 60, paste0(tp_min, "min"), paste0(tp_min / 60, "h")),
              Replicate, StandardizedReplicate) |>
    as.data.frame()
}

process <- function(f) {
  cm <- read_counts(f)
  si <- build_sample_info(colnames(cm$counts))
  list(lcpm = make_lcpm(cm$counts)[, si$Sample, drop = FALSE],
       rlog = make_rlog(cm$counts)[, si$Sample, drop = FALSE],
       sample_info = si, gene_name = cm$gene_name)
}

load(file.path("data", "06092026-updated.RData"))

wt <- process(file.path("data", "yH545_salmon.merged.gene_counts.tsv"))
ko <- process(file.path("data", "yH1053_salmon.merged.gene_counts.tsv"))

# extend the 2023 S288C annotation to cover 2026-only gene IDs, same schema
anno_2023 <- as.data.frame(all_species_data$sc$sc_anno_2023)
new_ids <- setdiff(union(rownames(wt$rlog), rownames(ko$rlog)), anno_2023$GeneID)
gn <- c(wt$gene_name, ko$gene_name); gn <- gn[!duplicated(names(gn))][new_ids]

sc_anno <- as_tibble(rbind(anno_2023, data.frame(
  Chr = NA_character_, GeneID = new_ids,
  GeneName = ifelse(is.na(gn) | !nzchar(gn), new_ids, unname(gn)),
  Type.of.Gene = NA_character_, FungiDB_ID = new_ids, AllScIDs = new_ids,
  InScToSc = FALSE)[, names(anno_2023)]))

sc <- all_species_data$sc
all_species_data$sc <- list(
  sc_anno_2023 = sc$sc_anno_2023, sc_lcpm_2023 = sc$sc_lcpm_2023,
  sc_sample_info_2023 = sc$sc_sample_info_2023, sc_rlog_2023 = sc$sc_rlog_2023,
  sc_anno = sc_anno, sc_lcpm = wt$lcpm,
  sc_sample_info = wt$sample_info, sc_rlog = wt$rlog,
  sc_anno_KO = sc_anno, sc_lcpm_KO = ko$lcpm,
  sc_sample_info_KO = ko$sample_info, sc_rlog_KO = ko$rlog
)

all_species_data$metadata$version <- "per_species_HOG_v2_scer_split"
all_species_data$metadata$date_created <- as.Date("2026-07-31")
all_species_data$metadata$description <- paste(
  "Per-species structure with HOG orthogroups, all ToSc moved to legacy_synteny.",
  "S. cerevisiae 2026 reprocessed per strain: WT yH545 unsuffixed, ppx1d ppn1d KO",
  "yH1053 suffixed _KO. 2023 data remains year-suffixed.")

save(all_species_data, file = file.path("data", "07312026-updated.RData"), compress = "xz")

cat(dim(wt$lcpm), dim(wt$rlog), "\n")
cat(dim(ko$lcpm), dim(ko$rlog), "\n")
cat(nrow(sc_anno), length(new_ids), "\n")
