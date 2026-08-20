# rnacross study-design module
# Defines the per-dataset `study_design` record, its constructor, the fixed
# GRE-lab stock design, and the accessor functions that the rest of the app
# reads instead of the hardcoded TIME_POINTS / "0min" globals.
# dependencies: TIME_POINTS (02_constants_themes) is referenced only at call
# time inside GRE_study_design(), so source order is not load-bearing.
#
# ---------------------------------------------------------------------------
# The `study_design` record (a named list). Its drawers:
#
#   study_design$data_upload_version              # structure version stamp
#   study_design$id                               # dataset id
#   study_design$comparison$align                 # "orthology" | "identity"
#   study_design$comparison$members               # entities compared across (species)
#   study_design$comparison$label                 # display word ("Species")
#   study_design$condition$column                 # primary-axis column name
#   study_design$condition$type                   # "nominal" | "ordinal" | "interval"
#   study_design$condition$levels                 # ordered vocabulary (was TIME_POINTS)
#   study_design$condition$values                 # numeric spacing (interval only)
#   study_design$condition$reference              # baseline level (may be NULL)
#   study_design$condition$label                  # display text
#   study_design$assay$matrix                     # which assay matrix ("lcpm")
#   study_design$assay$normalization$method       # inert: provenance label
#   study_design$assay$normalization$space        # inert: "log2" etc.
#   study_design$samples$key                      # sample id column
#   study_design$samples$genome_col               # column giving each sample's genome
#   study_design$replicate                        # replicate column
#   study_design$features$paralog_policy          # paralog aggregation method
#   study_design$features$coverage_filter         # list(present_in=, of=)
#   study_design$factors                          # secondary factors (list; Phase 4)
#   study_design$confounders                      # nuisance column names (stored only)
# ---------------------------------------------------------------------------


#' Validate a study_design record
#'
#' Refuse-loudly at the spec level: stops with a clear message when the record
#' is malformed (unknown scale type, empty levels, interval without numeric
#' values, etc.). Warns (does not stop) when a declared reference is not among
#' the levels, since the accessor falls back to the first level.
#'
#' @param study_design A study_design named list
#' @return TRUE (invisibly) if valid; otherwise stops
#' @export
validate_study_design <- function(study_design) {
  sd <- study_design

  # comparison
  if (is.null(sd$comparison$align) ||
      !sd$comparison$align %in% c("orthology", "identity")) {
    stop("study_design: comparison$align must be 'orthology' or 'identity'.")
  }
  if (length(sd$comparison$members) < 1) {
    stop("study_design: comparison$members must list at least one member.")
  }

  # condition (the primary axis)
  ct <- sd$condition$type
  if (is.null(ct) || !ct %in% c("nominal", "ordinal", "interval")) {
    stop("study_design: condition$type must be 'nominal', 'ordinal', or 'interval'.")
  }
  lv <- sd$condition$levels
  if (length(lv) < 1) {
    stop("study_design: condition$levels must be non-empty.")
  }
  if (anyDuplicated(lv)) {
    stop("study_design: condition$levels must be unique.")
  }
  if (ct == "interval") {
    vv <- sd$condition$values
    if (is.null(vv) || !is.numeric(vv) || length(vv) != length(lv)) {
      stop("study_design: an 'interval' condition needs numeric $values the same length as $levels.")
    }
  }
  ref <- sd$condition$reference
  if (!is.null(ref) && !ref %in% lv) {
    warning(sprintf(
      "study_design: condition$reference '%s' is not among the levels; it will fall back to the first level.",
      ref))
  }

  invisible(TRUE)
}


#' Construct a study_design from declared parts
#'
#' The builder used for uploaded data (the wizard fills these in) and, via
#' GRE_study_design(), for the stock data. Assembles the record and validates it.
#'
#' @return A validated study_design named list
#' @export
make_study_design <- function(
    id,
    align, members, comparison_label = "Group",
    condition_column, condition_type, condition_levels,
    condition_values = NULL, condition_reference = NULL, condition_label = NULL,
    assay_matrix = "expression",
    normalization_method = NA_character_, normalization_space = NA_character_,
    samples_key = "sample_id", genome_col = NULL,
    replicate = NULL,
    paralog_policy = "mean", coverage_filter = NULL,
    factors = list(), confounders = character(0),
    data_upload_version = "1.0") {

  sd <- list(
    data_upload_version = data_upload_version,
    id = id,
    comparison = list(align = align, members = members, label = comparison_label),
    condition = list(
      column    = condition_column,
      type      = condition_type,
      levels    = condition_levels,
      values    = condition_values,
      reference = condition_reference,
      label     = if (is.null(condition_label)) condition_column else condition_label
    ),
    assay = list(
      matrix = assay_matrix,
      normalization = list(method = normalization_method, space = normalization_space)
    ),
    samples  = list(key = samples_key, genome_col = genome_col),
    replicate = replicate,
    features = list(paralog_policy = paralog_policy, coverage_filter = coverage_filter),
    factors  = factors,
    confounders = confounders
  )

  validate_study_design(sd)
  sd
}


#' The fixed study_design for the GRE-lab stock data
#'
#' Reproduces today's hardcoded behavior as one configuration of the general
#' model: cross-species (orthology), interval time axis, 0min reference.
#'
#' @return A validated study_design named list
#' @export
GRE_study_design <- function() {
  make_study_design(
    id                 = "GRE_pho_timecourse",
    align              = "orthology",
    members            = c("sc", "cg", "kl", "ca"),
    comparison_label   = "Species",
    condition_column   = "Timepoint",
    condition_type     = "interval",
    condition_levels   = TIME_POINTS,
    condition_values   = c(0, 15, 30, 45, 60, 90, 120, 150, 180, 210, 240, 360, 480),
    condition_reference = "0min",
    condition_label    = "Timepoint",
    assay_matrix       = "lcpm",
    normalization_method = "TMM_log2CPM",
    normalization_space  = "log2",
    samples_key        = "sample_id",
    genome_col         = "species",
    replicate          = "Replicate",
    paralog_policy     = "eigengene",
    coverage_filter    = list(present_in = 4, of = 4)
  )
}


# ---------------------------------------------------------------------------
# Accessors: condition family (the primary axis; was TIME_POINTS / "0min")
# ---------------------------------------------------------------------------

#' @export
condition_column <- function(study_design) study_design$condition$column

#' @export
condition_type <- function(study_design) study_design$condition$type

#' @export
condition_levels <- function(study_design) study_design$condition$levels

#' Display label for the primary axis (falls back to the column name)
#' @export
condition_label <- function(study_design) {
  lbl <- study_design$condition$label
  if (is.null(lbl) || !nzchar(lbl)) condition_column(study_design) else lbl
}

#' Numeric position of each level.
#' Interval: the stored values. Ordinal: equal ranks. Nominal: NULL.
#' @export
condition_values <- function(study_design) {
  type <- condition_type(study_design)
  if (type == "interval") {
    study_design$condition$values
  } else if (type == "ordinal") {
    seq_along(condition_levels(study_design))
  } else {
    NULL
  }
}

#' Baseline level for fold-change.
#' Returns the declared reference; NULL if none is intended; the first level if
#' a reference was declared but is not present among the levels.
#' @export
condition_reference <- function(study_design) {
  ref <- study_design$condition$reference
  if (is.null(ref)) return(NULL)
  lv <- condition_levels(study_design)
  if (!ref %in% lv) return(lv[1])
  ref
}

#' TRUE when the axis has a meaningful order (ordinal or interval)
#' @export
condition_is_ordered <- function(study_design) {
  condition_type(study_design) != "nominal"
}

#' Which column of a table actually holds the primary axis
#'
#' Never renames the user's column: looks up condition_column(), falling back to
#' "Timepoint" for stock/legacy tables.
#' @export
condition_column_in <- function(study_design, df) {
  col <- condition_column(study_design)
  if (!is.null(col) && col %in% names(df)) return(col)
  if ("Timepoint" %in% names(df)) return("Timepoint")
  stop(sprintf("condition_column_in: no '%s' column in the table.",
               if (is.null(col)) "condition" else col))
}

#' Read the primary-axis vector out of a table by its real column name
#' @export
condition_of <- function(study_design, samples) {
  samples[[condition_column_in(study_design, samples)]]
}

#' TRUE when a samples table carries replicate information
#' @export
has_replicates <- function(study_design, samples) {
  col <- replicate_column(study_design)
  (!is.null(col) && col %in% names(samples)) || "Replicate" %in% names(samples)
}

#' Replicate vector for a samples table, synthesizing one level when unreplicated
#' @export
replicate_of <- function(study_design, samples) {
  col <- replicate_column(study_design)
  if (!is.null(col) && col %in% names(samples)) return(samples[[col]])
  if ("Replicate" %in% names(samples)) return(samples[["Replicate"]])
  rep(1L, nrow(samples))
}

#' Numeric position of each label along the primary axis
#'
#' Interval: declared values. Ordinal/nominal: level rank. Replaces the
#' time-only parsers, so dose/genotype axes order correctly.
#' @export
condition_positions <- function(study_design, labels) {
  lv <- as.character(condition_levels(study_design))
  vals <- condition_values(study_design)
  if (is.null(vals)) vals <- seq_along(lv)
  labels <- as.character(labels)
  out <- as.numeric(vals[match(labels, lv)])
  #labels outside the declared vocabulary sort last, in order of appearance
  if (anyNA(out)) {
    extra <- unique(labels[is.na(out)])
    out[is.na(out)] <- max(c(vals, 0), na.rm = TRUE) + match(labels[is.na(out)], extra)
  }
  out
}

#' Order a vector of labels along the primary axis
#' @export
condition_order <- function(study_design, labels) {
  order(condition_positions(study_design, labels))
}

#' Short codes (T01, T02, ...) ranked by the design's axis order
#'
#' Same result as the old time parser for the stock time course, but correct for
#' any axis, since ranking comes from the design rather than parsed time units.
#' @export
condition_codes <- function(study_design, labels, prefix = "T") {
  pos <- condition_positions(study_design, labels)
  sprintf("%s%02d", prefix, match(pos, sort(unique(pos))))
}


# ---------------------------------------------------------------------------
# Accessors: comparison family (entities compared across; the species)
# ---------------------------------------------------------------------------

#' @export
comparison_members <- function(study_design) study_design$comparison$members

#' @export
comparison_align <- function(study_design) study_design$comparison$align

#' @export
comparison_label <- function(study_design) study_design$comparison$label


# ---------------------------------------------------------------------------
# Accessors: samples / genome, assay, replicate, features, confounders
# ---------------------------------------------------------------------------

#' Column name that gives each sample's genome
#' @export
genome_column <- function(study_design) study_design$samples$genome_col

#' Genome of each row of a samples table, read via genome_column()
#' @param samples A sample-metadata (colData) data frame
#' @export
sample_genome <- function(study_design, samples) {
  gc <- genome_column(study_design)
  if (is.null(gc) || !gc %in% names(samples)) {
    stop("sample_genome: genome column not found in the samples table.")
  }
  samples[[gc]]
}

#' @export
assay_matrix <- function(study_design) study_design$assay$matrix

#' @export
normalization_method <- function(study_design) study_design$assay$normalization$method

#' @export
normalization_space <- function(study_design) study_design$assay$normalization$space

#' @export
replicate_column <- function(study_design) study_design$replicate

#' @export
paralog_policy <- function(study_design) study_design$features$paralog_policy

#' @export
coverage_filter <- function(study_design) study_design$features$coverage_filter

#' @export
secondary_factors <- function(study_design) study_design$factors

#' @export
confounders <- function(study_design) study_design$confounders

#' @export
data_upload_version <- function(study_design) study_design$data_upload_version


# ---------------------------------------------------------------------------
# Scale-dispatch gates (what the plots check before assuming order/spacing)
# ---------------------------------------------------------------------------

#' TRUE only for an interval axis (numeric interpolation is meaningful)
#' @export
can_interpolate <- function(study_design) {
  condition_type(study_design) == "interval"
}

#' TRUE for an ordered axis (trajectory arrows / connecting paths make sense)
#' @export
can_draw_trajectory <- function(study_design) {
  condition_is_ordered(study_design)
}

#' TRUE when a baseline reference exists (vs-reference normalization is possible)
#' @export
has_reference <- function(study_design) {
  !is.null(condition_reference(study_design))
}
