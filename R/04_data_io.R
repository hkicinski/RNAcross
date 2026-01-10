# rnacross data i/o module
# data input/output operations, expression matrix retrieval
# dependencies: 01_config

#' Get expression matrix based on transformation type
#'
#' Retrieves the appropriate expression matrix (lcpm or rlog) for a given
#' species and transformation type. Handles species-specific naming conventions.
#'
#' @param species_code Character species code (e.g., "cg", "sc", "kl", "ca")
#' @param transform_type Character transformation type ("lcpm" or "rlog")
#' @param species_data Optional pre-loaded species data (defaults to all_species_data[[species_code]])
#' @return Expression matrix with genes as rows and samples as columns, or NULL if not found
get_expression_matrix <- function(species_code, transform_type, species_data = NULL) {
  if (is.null(species_data)) {
    species_data <- all_species_data[[species_code]]
  }

  # precompute matrix name based on species code
  # cg uses unprefixed names, others use prefixed names
  is_cg <- species_code == "cg"

  if (is.null(transform_type) || length(transform_type) == 0) {
    warning(paste("transform_type is NULL/empty for species:", species_code))
    return(NULL)
  }

  if (transform_type == "rlog") {
    matrix_name <- if (is_cg) "rlog" else paste0(species_code, "_rlog")
    expr_matrix <- species_data[[matrix_name]]
  } else {
    matrix_name <- if (is_cg) "lcpm" else paste0(species_code, "_lcpm")
    expr_matrix <- species_data[[matrix_name]]
  }

  # debug output using conditional debug_cat
  if (DEBUG_MODE) {
    if (!is.null(expr_matrix)) {
      debug_cat("\n=== get_expression_matrix DEBUG ===\n")
      debug_cat("Species:", species_code, "\n")
      debug_cat("Transform:", transform_type, "\n")
      debug_cat("Matrix name:", matrix_name, "\n")
      debug_cat("Rows:", nrow(expr_matrix), "Cols:", ncol(expr_matrix), "\n")
      debug_cat("First 5 rownames:", paste(head(rownames(expr_matrix), 5), collapse = ", "), "\n")
    } else {
      debug_cat("\n!!! expr_matrix is NULL for species:", species_code, "transform:", transform_type, "\n")
    }
  }

  return(expr_matrix)
}

#' Get axis label based on transformation type
#'
#' Returns the appropriate y-axis label for expression plots based on
#' the transformation type used.
#'
#' @param transform_type Character transformation type ("lcpm" or "rlog")
#' @return Character string for axis label
get_expression_label <- function(transform_type) {
  if (is.null(transform_type) || length(transform_type) == 0) {
    return("Expression")
  }
  if (transform_type == "rlog") {
    return("rlog expression")
  } else {
    return("log2(CPM)")
  }
}
