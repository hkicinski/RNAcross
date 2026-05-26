suppressMessages({library(data.table)})
load("data/RData_perSpecies_HOG_clean_11182025_rlog.RData")

config <- list(cg="C. glabrata", sc="S. cerevisiae", kl="K. lactis", ca="C. albicans")

for (sp_id in names(config)) {
  sp_data <- all_species_data[[sp_id]]
  s_info <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(sp_id, "_sample_info")]]
  tps <- sort(unique(s_info$Timepoint))
  cat(sprintf("%-15s [%d timepoints]: %s\n", config[[sp_id]], length(tps), paste(tps, collapse=", ")))
}

tp_lists <- lapply(names(config), function(sp_id) {
  sp_data <- all_species_data[[sp_id]]
  s_info <- if (!is.null(sp_data$sample_info)) sp_data$sample_info else sp_data[[paste0(sp_id, "_sample_info")]]
  unique(as.character(s_info$Timepoint))
})

shared <- Reduce(intersect, tp_lists)
TIME_POINTS <- c("0min", "15min", "30min", "45min", "1h", "1.5h", "2h", "2.5h", "3h", "3.5h", "4h", "6h", "8h")
shared_ordered <- TIME_POINTS[TIME_POINTS %in% shared]

cat(sprintf("\nIntersection   [%d timepoints]: %s\n", length(shared_ordered), paste(shared_ordered, collapse=", ")))
