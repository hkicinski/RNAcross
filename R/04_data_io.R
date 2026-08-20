# rnacross data i/o module
# data input/output operations, expression matrix retrieval
# dependencies: 01_config

# preference order; only breaks ties when a dataset carries both
KNOWN_TRANSFORMS <- c("rlog", "lcpm")

# provenance of the bundled GRE dataset; uploads get labelled from their values
TRANSFORM_LABELS <- list(
  lcpm = list(choice = "TMM + log2CPM", axis = "log2(CPM)"),
  rlog = list(choice = "DESeq2 rlog", axis = "rlog expression")
)

EXPRESSION_SCALE_LABELS <- c(
  counts   = "counts (uploaded)",
  log      = "log-scale expression (uploaded)",
  centered = "centered expression (uploaded)",
  unknown  = "expression (uploaded)"
)

#' Resolve the key holding a species' expression matrix
#'
#' Tries "<code>_<transform>" then bare "<transform>", exact match only.
#'
#' @param species_code Character species code
#' @param transform_type Character transform slot ("lcpm", "rlog")
#' @param species_data Species record (list)
#' @return The matching key, or NULL if this record has no such matrix
expression_matrix_key <- function(species_code, transform_type, species_data) {
  if (is.null(species_data) || is.null(transform_type) || length(transform_type) == 0) {
    return(NULL)
  }
  for (k in c(paste0(species_code, "_", transform_type), transform_type)) {
    if (!is.null(species_data[[k]])) return(k)
  }
  NULL
}

#' Which transforms a species record actually carries
#'
#' @param species_code Character species code
#' @param species_data Species record; defaults to the bundled dataset
#' @return Character vector of available transform slots, possibly empty
available_transforms <- function(species_code, species_data = NULL) {
  if (is.null(species_data)) species_data <- all_species_data[[species_code]]
  if (is.null(species_data)) return(character(0))
  KNOWN_TRANSFORMS[!vapply(
    KNOWN_TRANSFORMS,
    function(tt) is.null(expression_matrix_key(species_code, tt, species_data)),
    logical(1)
  )]
}

#' Get expression matrix based on transformation type
#'
#' NULL transform_type, or one the record does not carry, falls back to
#' whatever matrix the record does have.
#'
#' @param species_code Character species code (e.g., "cg", "sc", "kl", "ca")
#' @param transform_type Character transformation type, or NULL to auto-detect
#' @param species_data Optional pre-loaded species data (defaults to all_species_data[[species_code]])
#' @return Expression matrix with genes as rows and samples as columns, or NULL if not found
get_expression_matrix <- function(species_code, transform_type = NULL, species_data = NULL) {
  if (is.null(species_data)) {
    species_data <- all_species_data[[species_code]]
  }
  if (is.null(species_data)) {
    return(NULL)
  }

  matrix_name <- expression_matrix_key(species_code, transform_type, species_data)

  if (is.null(matrix_name)) {
    fallback <- available_transforms(species_code, species_data)
    if (length(fallback) == 0) {
      warning(paste("No expression matrix found for species:", species_code))
      return(NULL)
    }
    matrix_name <- expression_matrix_key(species_code, fallback[[1]], species_data)
  }

  expr_matrix <- species_data[[matrix_name]]

  # debug output using conditional debug_cat
  if (DEBUG_MODE) {
    if (!is.null(expr_matrix)) {
      debug_cat("\n=== get_expression_matrix DEBUG ===\n")
      debug_cat("Species:", species_code, "\n")
      debug_cat("Transform requested:", transform_type %||% "(auto)", "\n")
      debug_cat("Matrix name:", matrix_name, "\n")
      debug_cat("Rows:", nrow(expr_matrix), "Cols:", ncol(expr_matrix), "\n")
      debug_cat("First 5 rownames:", paste(head(rownames(expr_matrix), 5), collapse = ", "), "\n")
    } else {
      debug_cat("\n!!! expr_matrix is NULL for species:", species_code, "transform:", transform_type, "\n")
    }
  }

  return(expr_matrix)
}

#' Describe what an expression matrix actually holds
#'
#' Reports the scale it can see, never a normalization method it cannot.
#'
#' @param expr_matrix Numeric matrix or data frame
#' @return One of "counts", "log", "centered", "unknown"
detect_expression_scale <- function(expr_matrix) {
  if (is.null(expr_matrix) || length(expr_matrix) == 0) return("unknown")

  # a head slice is enough to characterise the scale and keeps this cheap
  probe <- if (is.null(nrow(expr_matrix))) expr_matrix else {
    expr_matrix[seq_len(min(nrow(expr_matrix), 500L)), , drop = FALSE]
  }
  v <- suppressWarnings(as.numeric(probe))
  v <- v[is.finite(v)]
  if (length(v) == 0) return("unknown")

  rng <- range(v)
  if (all(v >= 0) && all(abs(v - round(v)) < 1e-8) && rng[[2]] > 100) return("counts")
  if (rng[[1]] < -0.5 && abs(mean(v)) < 0.5) return("centered")
  if (rng[[1]] >= -0.5 && rng[[2]] <= 60) return("log")
  "unknown"
}

#' Label for an expression matrix, stamped at upload
#'
#' @param expr_matrix Numeric matrix
#' @return Character label describing the values
detected_expression_label <- function(expr_matrix) {
  unname(EXPRESSION_SCALE_LABELS[[detect_expression_scale(expr_matrix)]])
}

#' Human-readable name for a transform in the settings picker
#'
#' @param transform_type Character transform slot
#' @return Character label
transform_choice_label <- function(transform_type) {
  lbl <- TRANSFORM_LABELS[[transform_type]]
  if (is.null(lbl)) transform_type else lbl$choice
}

#' Get axis label for the expression values being plotted
#'
#' Resolution order: stamp on the matrix, known bundled transform, then values.
#'
#' @param transform_type Character transformation type, or NULL
#' @param expr_matrix Optional matrix being plotted, used for its stamp/values
#' @return Character string for axis label
get_expression_label <- function(transform_type, expr_matrix = NULL) {
  if (!is.null(expr_matrix)) {
    stamped <- attr(expr_matrix, "expr_label", exact = TRUE)
    if (!is.null(stamped) && nzchar(stamped)) return(stamped)
  }
  if (!is.null(transform_type) && length(transform_type) > 0 &&
      transform_type %in% names(TRANSFORM_LABELS)) {
    return(TRANSFORM_LABELS[[transform_type]]$axis)
  }
  if (!is.null(expr_matrix)) return(detected_expression_label(expr_matrix))
  "Expression"
}
