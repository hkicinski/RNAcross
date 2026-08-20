# rnacross configuration module
# global settings, debug functions, library loading, data initialization
# dependencies: none

# cran repository setup
r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org"
options(repos = r)

MAX_UPLOAD_MB <- 2048  # 2 GB
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

# debug mode configuration
# set to TRUE to enable debug messages throughout the application
DEBUG_MODE <- FALSE

#' Conditional debug message printing
#'
#' Prints debug messages to console only when DEBUG_MODE is TRUE.
#' Uses message() for clean output that can be suppressed.
#'
#' @param ... Arguments to concatenate and print
#' @return NULL (invisible, side effect: prints if DEBUG_MODE = TRUE)
debug_print <- function(...) {
  if (DEBUG_MODE) {
    message(paste0("[DEBUG] ", ...))
  }
}

#' Conditional debug cat output
#'
#' Outputs text via cat() only when DEBUG_MODE is TRUE.
#' Useful for formatted debug output.
#'
#' @param ... Arguments to pass to cat()
#' @return NULL (invisible, side effect: cat output if DEBUG_MODE = TRUE)
debug_cat <- function(...) {
  if (DEBUG_MODE) {
    cat(...)
  }
}

# performance cache
# environment for caching computed results to avoid redundant calculations
.performance_cache <- new.env(hash = TRUE)

#' Clear the performance cache
#'
#' Removes all cached objects from the performance cache environment.
#' Call this when data sources change to ensure fresh computations.
#'
#' @return NULL (invisible, side effect: clears cache)
clear_performance_cache <- function() {
  rm(list = ls(envir = .performance_cache), envir = .performance_cache)
}

# library loading
.cran_pkgs <- c(
  "tidyverse", "data.table", "cowplot", "tidyr",
  "shiny", "bslib", "waiter", "shinyjs", "plotly", "DT",
  "fontawesome", "shinyBS", "shinyWidgets",
  "viridis", "ggridges", "ape", "RColorBrewer", "colourpicker",
  "ggprism", "ggrepel", "circlize", "svglite", "here"
)
.bioc_pkgs <- c("ComplexHeatmap", "ggtree", "treeio")

.missing_cran <- .cran_pkgs[!vapply(.cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing_cran) > 0) {
  message("Installing missing CRAN packages: ", paste(.missing_cran, collapse = ", "))
  install.packages(.missing_cran, repos = "https://cloud.r-project.org")
}

.missing_bioc <- .bioc_pkgs[!vapply(.bioc_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing_bioc) > 0) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  }
  message("Installing missing Bioconductor packages: ", paste(.missing_bioc, collapse = ", "))
  BiocManager::install(.missing_bioc, update = FALSE, ask = FALSE)
}

suppressMessages({
  # core data manipulation
  library(tidyverse)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
  library(forcats)
  library(data.table)
  library(cowplot)
  library(tidyr)

  # shiny framework and ui components
  library(shiny)
  library(bslib)
  library(waiter)
  library(shinyjs)
  library(plotly)
  library(DT)
  library(fontawesome)
  library(shinyBS)

  # visualization
  library(viridis)
  library(ggridges)
  library(ggtree)
  library(ape)
  library(treeio)
  library(RColorBrewer)
  library(colourpicker)
  library(ggprism)
  library(ggrepel)

  # complex visualization
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
  library(grDevices)

  # file paths
  library(here)
})

#ggplot2 4.0 dropped is.waive(), which ggtree still calls for aligned tip labels
if (!exists("is.waive")) {
  assign("is.waive", function(x) inherits(x, "waiver"), envir = globalenv())
}

# data loading
load(file.path("data", "07312026-updated.RData"))

# gene lookup table preprocessing
# preprocess gene lookup table at startup for faster queries
if (!is.null(all_species_data$gene_lookup)) {
  if (!is.data.table(all_species_data$gene_lookup)) {
    all_species_data$gene_lookup <- as.data.table(all_species_data$gene_lookup)
  }

  # precompute uppercase columns for case-insensitive matching
  all_species_data$gene_lookup[, gene_id_upper := toupper(gene_id)]
  all_species_data$gene_lookup[, gene_name_upper := toupper(gene_name)]

  # create indices on query columns for fast lookups
  setindex(all_species_data$gene_lookup, gene_id_upper)
  setindex(all_species_data$gene_lookup, gene_name_upper)
  setindex(all_species_data$gene_lookup, hog_id)
  setindex(all_species_data$gene_lookup, species)
  setindex(all_species_data$gene_lookup, gene_id)
}

if (!requireNamespace('shinyWidgets', quietly = TRUE)) {
  install.packages('shinyWidgets', repos = 'https://cloud.r-project.org')
}
library(shinyWidgets)

# default list GRE species
ALL_SPECIES <- list(
  sc = list(
    name = "S. cerevisiae",
    short = "S.c.",
    color = "#440154",
    shape = 16
  ),
  cg = list(
    name = "C. glabrata",
    short = "C.g.",
    color = "#3b528b",
    shape = 17
  ),
  kl = list(
    name = "K. lactis",
    short = "K.l.",
    color = "#21918c",
    shape = 15
  ),
  ca = list(
    name = "C. albicans",
    short = "C.a.",
    color = "#5ec962",
    shape = 18
  )
)
