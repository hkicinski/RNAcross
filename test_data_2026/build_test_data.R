#Reshapes Expression Atlas E-MTAB-3716..3719 (6 tissues x 4 species) into an RNAcross upload set with Ensembl orthogroups

suppressPackageStartupMessages({library(data.table)})
OUT <- "test_data_2026"; RAW <- file.path(OUT, "raw")

SP <- list(
  hs  = list(acc = "E-MTAB-3716", name = "Homo sapiens",          short = "Human",   mart = "hsapiens"),
  mml = list(acc = "E-MTAB-3717", name = "Macaca mulatta",        short = "Macaque", mart = "mmulatta"),
  mm  = list(acc = "E-MTAB-3718", name = "Mus musculus",          short = "Mouse",   mart = "mmusculus"),
  md  = list(acc = "E-MTAB-3719", name = "Monodelphis domestica", short = "Opossum", mart = "mdomestica")
)

gene_names <- function(code) {
  f <- file.path(RAW, paste0("names_", SP[[code]]$mart, ".tsv"))
  if (!file.exists(f)) return(NULL)
  d <- fread(f, header = FALSE, col.names = c("id", "symbol"), fill = TRUE)
  d[!is.na(symbol) & symbol != ""]
}

TISSUE_MAP <- c("frontal lobe" = "brain", "prefrontal cortex" = "brain",
                "temporal lobe" = "brain")

read_tissues <- function(path) {
  parts <- strsplit(readLines(path), "\t", fixed = TRUE)
  ok <- vapply(parts, function(p) length(p) >= 6 && p[5] == "organism part", logical(1))
  unique(data.table(Sample = vapply(parts[ok], `[`, "", 3),
                    Tissue = vapply(parts[ok], `[`, "", 6)))
}

for (code in names(SP)) {
  acc <- SP[[code]]$acc
  cnt <- fread(file.path(RAW, paste0(acc, "-counts.tsv")))
  setnames(cnt, 1, "GeneID")

  tis <- read_tissues(file.path(RAW, paste0(acc, "-sdrf.tsv")))
  tis[, Tissue := fifelse(Tissue %chin% names(TISSUE_MAP), TISSUE_MAP[Tissue], Tissue)]
  keep <- intersect(tis$Sample, names(cnt))
  stopifnot(length(keep) > 0)
  tis <- tis[Sample %chin% keep][order(Tissue, Sample)]

  m <- as.matrix(cnt[, ..keep]); rownames(m) <- cnt$GeneID
  m <- m[rowSums(m) >= 10, tis$Sample, drop = FALSE]
  lcpm <- round(log2(t(t(m) / pmax(colSums(m), 1)) * 1e6 + 1), 4)

  tis[, Replicate := seq_len(.N), by = Tissue]
  fwrite(data.table(Gene = rownames(lcpm), lcpm), file.path(OUT, paste0(code, "_expr.csv")))
  fwrite(tis[, .(Sample, Tissue, Replicate)], file.path(OUT, paste0(code, "_samples.csv")))
  nm <- gene_names(code)
  sym <- if (is.null(nm)) rep("", nrow(lcpm)) else nm$symbol[match(rownames(lcpm), nm$id)]
  sym[is.na(sym)] <- ""
  fwrite(data.table(GeneID = rownames(lcpm), GeneName = sym), file.path(OUT, paste0(code, "_anno.csv")))
  cat(sprintf("%-4s %-22s %5d genes x %2d samples | %5d named | %s\n", code, SP[[code]]$name,
              nrow(lcpm), ncol(lcpm), sum(nzchar(sym)), paste(sort(unique(tis$Tissue)), collapse = ", ")))
}

hom <- list(mml = "mmulatta", mm = "mmusculus", md = "mdomestica")
maps <- lapply(hom, function(f) {
  d <- fread(file.path(RAW, paste0("homologs_", f, ".tsv")), header = FALSE,
             col.names = c("hs", "target", "type"), fill = TRUE)
  d[!is.na(target) & target != ""]
})
expressed <- setNames(lapply(names(SP), function(c)
  fread(file.path(OUT, paste0(c, "_expr.csv")), select = 1)[[1]]), names(SP))

hs_genes <- intersect(unique(unlist(lapply(maps, `[[`, "hs"))), expressed$hs)
og <- data.table(Orthogroup = sprintf("OG%06d", seq_along(hs_genes)), hs = hs_genes)
for (code in names(maps)) {
  d <- maps[[code]][target %chin% expressed[[code]]]
  agg <- d[, .(ids = paste(unique(target), collapse = ", ")), by = hs]
  v <- agg$ids[match(og$hs, agg$hs)]; v[is.na(v)] <- ""
  og[[code]] <- v
}
og <- og[(mml != "") | (mm != "") | (md != "")]
setcolorder(og, c("Orthogroup", "hs", "mml", "mm", "md"))
fwrite(og, file.path(OUT, "orthogroups.tsv"), sep = "\t")

cat(sprintf("\northogroups: %d\n", nrow(og)))
cat(sprintf("  1:many into mouse: %d\n",
            sum(vapply(strsplit(og$mm, ", "), length, integer(1)) > 1 & og$mm != "")))
cat(sprintf("  complete 4-species: %d\n", sum(og$mml != "" & og$mm != "" & og$md != "")))
