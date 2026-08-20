# rnacross orthology query module
# gene lookup, orthogroup queries, and cross-species gene mapping
# dependencies: 01_config, 04_data_io

#' Query genes using lookup table
#'
#' Searches the gene lookup table for matching gene IDs or names.
#' Uses precomputed uppercase columns for faster case-insensitive matching.
#'
#' @param query Character gene query (ID or name)
#' @param species_filter Optional character vector of species codes to filter by
#' @param current_data Optional data structure (defaults to all_species_data)
#' @return data.table of matching genes
query_gene_lookup <- function(query, species_filter = NULL, current_data = NULL) {
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup
  query <- toupper(trimws(query))

  # use precomputed uppercase columns if available
  if ("gene_id_upper" %in% names(lookup_table)) {
    matches <- lookup_table[gene_id_upper == query | gene_name_upper == query, ]
  } else {
    matches <- lookup_table[toupper(gene_id) == query | toupper(gene_name) == query, ]
  }

  if (!is.null(species_filter) && length(species_filter) > 0) {
    matches <- matches[species %in% species_filter]
  }
  return(matches)
}

#' Get expression matrix ID for a gene
#'
#' Retrieves the expression ID for a gene in a specific species.
#' The expression ID may differ from the gene ID in some cases.
#'
#' @param query_gene_id Character gene ID to look up
#' @param species_code Character species code
#' @return Character expression ID or NULL if not found
get_expression_id <- function(query_gene_id, species_code) {
  lookup_table <- all_species_data$gene_lookup
  match <- lookup_table[gene_id == query_gene_id & species == species_code]

  if (nrow(match) > 0) {
    return(match$expression_id[1])
  }
  return(NULL)
}

#' Get all genes in an orthogroup
#'
#' Retrieves all genes belonging to a given orthogroup (HOG or OG).
#' Falls back to OG-based search if HOG search doesn't find genes in all species.
#'
#' @param group_id Character orthogroup ID (HOG ID)
#' @param use_og Logical whether to use OG-based search (currently unused)
#' @param current_data Optional data structure (defaults to all_species_data)
#' @return data.table of genes in the orthogroup
get_orthogroup_genes <- function(group_id, use_og = FALSE, current_data = NULL) {
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup

  # HOG-based search
  result <- lookup_table[hog_id == group_id, ]

  # fallback to OG if not all species found
  if (length(unique(result$species)) < 4 && nrow(result) > 0) {
    og_ids <- unique(result$og_id)
    og_ids <- og_ids[!is.na(og_ids)]

    if (length(og_ids) > 0) {
      og_result <- lookup_table[og_id %in% og_ids, ]
      if (nrow(og_result) > nrow(result)) {
        result <- og_result
      }
    }
  }

  return(result)
}

#' Identify species from gene ID
#'
#' Determines which species a gene belongs to using the lookup table.
#'
#' @param query_gene_id Character gene ID
#' @param current_data Optional data structure (defaults to all_species_data)
#' @return Character species code or NULL if not found
identify_species <- function(query_gene_id, current_data = NULL) {
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup
  match <- lookup_table[gene_id == query_gene_id]

  if (nrow(match) > 0) {
    return(as.character(match$species[1]))
  }
  return(NULL)
}

#' Query orthogroups across all species
#'
#' Searches for a gene across all species and returns orthogroup information.
#' Iterates through species until a match is found.
#'
#' @param gene_query Character gene query
#' @param current_data Data structure containing gene data
#' @param config Species configuration list
#' @param get_species_data_fn Function to retrieve species data
#' @return Query result list or NULL if not found
query_orthogroups <- function(gene_query, current_data, config, get_species_data_fn) {
  # searches in all species to find the gene
  for (species_id in names(config)) {
    species_data <- get_species_data_fn(species_id)

    if (!is.null(species_data)) {
      result <- query_gene_flexible(gene_query, species_data, current_data)

      if (!is.null(result) && result$source != "none") {
        return(result)
      }
    }
  }

  return(NULL)
}

build_synteny_group <- function(gene_id, species_code, all_species_data) {
  syn <- all_species_data$legacy_synteny
  if (is.null(syn)) return(NULL)

  sc_gene <- NULL

  if (species_code == "sc") {
    sc_gene <- gene_id
  } else {
    table_name <- paste0(species_code, "ToSc")
    if (!is.null(syn[[table_name]])) {
      id_col <- paste0(species_code, "id")
      syn_match <- syn[[table_name]][syn[[table_name]][[id_col]] == gene_id, ]
      if (nrow(syn_match) > 0) {
        sc_gene <- as.character(syn_match$scid[1])
      }
    }
  }

  if (is.null(sc_gene)) return(NULL)

  genes_by_species <- list()
  genes_by_species[["sc"]] <- sc_gene

  if (!is.null(syn$cgToSc)) {
    cg_matches <- syn$cgToSc[syn$cgToSc$scid == sc_gene, ]
    if (nrow(cg_matches) > 0) genes_by_species[["cg"]] <- as.character(cg_matches$cgid)
  }

  if (!is.null(syn$klToSc)) {
    kl_matches <- syn$klToSc[syn$klToSc$scid == sc_gene, ]
    if (nrow(kl_matches) > 0) genes_by_species[["kl"]] <- as.character(kl_matches$klid)
  }

  if (!is.null(syn$caToSc)) {
    ca_matches <- syn$caToSc[syn$caToSc$scid == sc_gene, ]
    if (nrow(ca_matches) > 0) genes_by_species[["ca"]] <- as.character(ca_matches$caid)
  }

  if (!is.null(syn$scToSc)) {
    sc_paralogs <- syn$scToSc[syn$scToSc$scid == sc_gene, ]
    if (nrow(sc_paralogs) > 0 && !is.na(sc_paralogs$scid_alt[1])) {
      sc_all <- unique(c(sc_gene, as.character(sc_paralogs$scid_alt)))
      sc_all <- sc_all[!is.na(sc_all) & sc_all != ""]
      if (length(sc_all) > 0) genes_by_species[["sc"]] <- sc_all
    }
  }

  return(genes_by_species)
}

#' Flexible gene query with HOG support
#'
#' Main query function that searches for a gene using the lookup table
#' and returns orthogroup information with genes grouped by species.
#'
#' @param gene_query Character gene query (ID or name)
#' @param species_data Species-specific data (lcpm, anno, sample_info)
#' @param all_species_data Full data structure with lookup table
#' @return List with query results including genes_by_species, or NULL if not found
query_gene_flexible <- function(gene_query, species_data, all_species_data) {
  gene_query <- toupper(trimws(gene_query))
  if (nchar(gene_query) == 0) {
    return(NULL)
  }

  debug_cat("\n=== query_gene_flexible ===\n")
  debug_cat("Query:", gene_query, "\n")

  # use the lookup table to find the gene
  lookup_matches <- query_gene_lookup(gene_query, NULL, all_species_data)

  debug_cat("Total lookup matches:", nrow(lookup_matches), "\n")

  if (nrow(lookup_matches) == 0) {
    debug_cat("No matches found in lookup table\n")
    return(NULL)
  }

  # get the first match
  match_info <- lookup_matches[1, ]
  gene_id <- match_info$gene_id
  expression_id <- match_info$expression_id
  hog_id <- match_info$hog_id
  og_id <- match_info$og_id

  # use expression_id for retrieving actual expression data
  actual_gene_id <- if (!is.na(expression_id) && expression_id != "") expression_id else gene_id

  debug_cat("First match - Gene ID:", gene_id, "HOG:", hog_id, "OG:", og_id, "\n")

  # build result
  result <- list(
    query = gene_query,
    gene_id = gene_id,
    expression_id = expression_id,
    match_type = ifelse(gene_query == toupper(gene_id), "direct", "gene_name"),
    source = "gene_lookup",
    orthogroup = hog_id,
    og_id = og_id,
    genes_by_species = list()
  )

  # get all genes in this HOG (try both HOG and OG)
  if (!is.na(hog_id) && nchar(hog_id) > 0) {
    hog_genes <- get_orthogroup_genes(hog_id, FALSE, all_species_data)

    # if we didn't find enough genes, try using the OG ID
    if (nrow(hog_genes) < 4 && !is.na(og_id)) {
      debug_cat("Trying OG-based search as HOG gave limited results\n")
      og_id_value <- og_id
      og_genes <- all_species_data$gene_lookup[og_id == og_id_value]
      if (nrow(og_genes) > nrow(hog_genes)) {
        debug_cat("OG search found more genes:", nrow(og_genes), "vs", nrow(hog_genes), "\n")
        hog_genes <- og_genes
      }
    }

    debug_cat("Total orthogroup genes found:", nrow(hog_genes), "\n")

    # group by species
    for (sp in unique(hog_genes$species)) {
      sp_genes <- hog_genes[species == sp]

      debug_cat("  Species", sp, ":", nrow(sp_genes), "genes\n")

      # create display format
      genes_df <- data.frame(
        gene_id = sp_genes$gene_id,
        gene_name = sp_genes$gene_name,
        display = ifelse(
          !is.na(sp_genes$gene_name) & sp_genes$gene_name != "",
          paste0(sp_genes$gene_name, " (", sp_genes$gene_id, ")"),
          sp_genes$gene_id
        ),
        expression_id = sp_genes$expression_id,
        stringsAsFactors = FALSE
      )

      result$genes_by_species[[sp]] <- genes_df
    }
  } else {
    match_species <- as.character(match_info$species)
    synteny_group <- build_synteny_group(gene_id, match_species, all_species_data)

    if (!is.null(synteny_group) && length(synteny_group) > 0) {
      result$source <- "synteny_aided"
      result$orthogroup <- "Synteny-aided (YGOB/CGOB)"

      for (sp in names(synteny_group)) {
        sp_gene_ids <- synteny_group[[sp]]
        sp_info <- all_species_data$gene_lookup[
          gene_id %in% sp_gene_ids & species == sp
        ]
        if (nrow(sp_info) > 0) {
          genes_df <- data.frame(
            gene_id = sp_info$gene_id,
            gene_name = sp_info$gene_name,
            display = ifelse(
              !is.na(sp_info$gene_name) & sp_info$gene_name != "",
              paste0(sp_info$gene_name, " (", sp_info$gene_id, ")"),
              sp_info$gene_id
            ),
            expression_id = sp_info$expression_id,
            stringsAsFactors = FALSE
          )
          result$genes_by_species[[sp]] <- genes_df
        }
      }
    } else {
      result$source <- "gene_lookup_no_orthogroup"
      genes_df <- data.frame(
        gene_id = gene_id,
        gene_name = if (!is.na(match_info$gene_name)) as.character(match_info$gene_name) else "",
        display = if (!is.na(match_info$gene_name) && match_info$gene_name != "")
          paste0(match_info$gene_name, " (", gene_id, ")")
        else gene_id,
        expression_id = actual_gene_id,
        stringsAsFactors = FALSE
      )
      result$genes_by_species[[match_species]] <- genes_df
    }
  }

  return(result)
}

#' Add gene names to gene IDs
#'
#' Looks up gene names for a vector of gene IDs in a specific species.
#'
#' @param gene_ids Character vector of gene IDs
#' @param species_id Character species code
#' @return data.frame with gene_id, gene_name, and display columns
add_gene_names <- function(gene_ids, species_id) {
  if (length(gene_ids) == 0) {
    return(data.frame(
      gene_id = character(),
      gene_name = character(),
      display = character(),
      stringsAsFactors = FALSE
    ))
  }

  # use lookup table
  lookup <- all_species_data$gene_lookup
  gene_info <- lookup[gene_id %in% gene_ids & species == species_id]

  # create result dataframe
  result <- data.frame(
    gene_id = gene_info$gene_id,
    gene_name = gene_info$gene_name,
    display = ifelse(
      !is.na(gene_info$gene_name) & gene_info$gene_name != "",
      paste0(gene_info$gene_name, " (", gene_info$gene_id, ")"),
      gene_info$gene_id
    ),
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Extract orthology mapping for gene list
#'
#' Maps a list of genes to their orthologs across all species.
#' Uses query_gene_flexible to find orthogroups for each gene.
#'
#' @param gene_list Character vector of gene queries
#' @param all_species_data Optional data structure (defaults to global)
#' @param config Optional species configuration (defaults to DEFAULT_SPECIES_CONFIG)
#' @return List of gene mappings with original gene and ortholog IDs per species
extract_orthology_for_genes <- function(gene_list, all_species_data = NULL, config = NULL) {
  if (is.null(all_species_data)) {
    all_species_data <- get_all_species_data()
  }
  if (is.null(config)) {
    config <- DEFAULT_SPECIES_CONFIG
  }
  ortholog_gene_map <- list()

  for (gene in gene_list) {
    gene_found <- FALSE

    # use existing query_gene_flexible like Comparative View does
    query_result <- NULL
    for (sp_id in names(config)) {
      sp_data <- list(
        lcpm = if (!is.null(all_species_data[[sp_id]]$lcpm)) all_species_data[[sp_id]]$lcpm else all_species_data[[sp_id]][[paste0(sp_id, "_lcpm")]],
        anno = if (!is.null(all_species_data[[sp_id]]$anno)) all_species_data[[sp_id]]$anno else all_species_data[[sp_id]][[paste0(sp_id, "_anno")]],
        sample_info = if (!is.null(all_species_data[[sp_id]]$sample_info)) all_species_data[[sp_id]]$sample_info else all_species_data[[sp_id]][[paste0(sp_id, "_sample_info")]]
      )

      result <- query_gene_flexible(gene, sp_data, all_species_data)
      if (!is.null(result) && result$source != "none") {
        query_result <- result
        gene_found <- TRUE
        break
      }
    }

    if (!is.null(query_result) && !is.null(query_result$genes_by_species)) {
      # build entry with all genes from orthogroup
      gene_entry <- list(original = gene)

      for (sp_code in names(config)) {
        base_code <- sp_code
        if (base_code %in% names(query_result$genes_by_species)) {
          sp_genes_df <- query_result$genes_by_species[[base_code]]
          if (!is.null(sp_genes_df) && nrow(sp_genes_df) > 0) {
            gene_entry[[sp_code]] <- sp_genes_df$gene_id
          } else {
            gene_entry[[sp_code]] <- NULL
          }
        } else {
          gene_entry[[sp_code]] <- NULL
        }
      }

      ortholog_gene_map[[length(ortholog_gene_map) + 1]] <- gene_entry
    }
  }

  return(ortholog_gene_map)
}


#' Calculate ortholog coverage statistics
#'
#' Calculates ortholog coverage statistics for gene set analysis.
#'
#' @param gene_mapping List of gene mappings
#' @param config Species configuration list
#' @return List of coverage statistics per species
calculate_ortholog_coverage <- function(gene_mapping, config) {
  if (is.null(gene_mapping) || length(gene_mapping) == 0) {
    return(NULL)
  }

  total_genes <- length(gene_mapping)
  coverage_stats <- list()

  for (sp_code in names(config)) {
    # count how many input genes have orthologs in this species
    genes_with_orthologs <- sum(sapply(gene_mapping, function(gene_map) {
      !is.null(gene_map[[sp_code]]) && length(gene_map[[sp_code]]) > 0
    }))

    # get unique ortholog genes (de-duplicate across orthogroups)
    all_orthologs <- unique(unlist(sapply(gene_mapping, function(gene_map) {
      gene_map[[sp_code]]
    })))

    total_orthologs <- length(all_orthologs)

    # count paralogs (genes beyond 1 per orthogroup)
    paralog_count <- sum(sapply(gene_mapping, function(gene_map) {
      max(0, length(gene_map[[sp_code]]) - 1)
    }))

    coverage_pct <- round((genes_with_orthologs / total_genes) * 100, 1)

    coverage_stats[[sp_code]] <- list(
      species_name = config[[sp_code]]$short,
      genes_found = genes_with_orthologs,
      total_orthologs = total_orthologs,
      paralog_count = paralog_count,
      total_genes = total_genes,
      coverage_pct = coverage_pct,
      coverage_class = if (coverage_pct >= 80) "high" else if (coverage_pct >= 50) "medium" else "low"
    )
  }

  return(coverage_stats)
}

#' Command palette gene suggestions
#'
#' Prefix matches first, topped up with substring matches, collapsed to one row
#' per orthogroup. Orphan genes key on their own id since they have no HOG.
#'
#' @param lookup_table gene_lookup data.table (or data.frame)
#' @param query Raw query string from the palette input
#' @param hog_counts Named integer vector of species-per-HOG
#' @param n_total Number of species in the current config
#' @param limit Maximum rows to return
#' @return List of suggestion lists (gene, label, detail, gene_id, hog_id)
rnx_gene_suggestions <- function(lookup_table, query, hog_counts = integer(0),
                                 n_total = 4L, limit = 5L) {
  q <- toupper(trimws(query %||% ""))
  if (!nzchar(q) || is.null(lookup_table) || !NROW(lookup_table)) return(list())

  gl <- lookup_table
  nm <- if ("gene_name_upper" %in% names(gl)) gl$gene_name_upper else toupper(as.character(gl$gene_name))
  id <- if ("gene_id_upper" %in% names(gl)) gl$gene_id_upper else toupper(as.character(gl$gene_id))
  nm[is.na(nm)] <- ""
  id[is.na(id)] <- ""

  ord <- unique(c(which(startsWith(nm, q)), which(startsWith(id, q))))
  if (length(ord) < 40L) {
    ord <- unique(c(ord, which(grepl(q, nm, fixed = TRUE)), which(grepl(q, id, fixed = TRUE))))
  }
  if (!length(ord)) return(list())
  ord <- head(ord, 400L)

  seen <- character(0)
  out <- list()
  for (i in ord) {
    if (length(out) >= limit) break
    hog <- as.character(gl$hog_id[i])
    gene_id <- as.character(gl$gene_id[i])
    gene_name <- as.character(gl$gene_name[i])
    if (is.na(gene_name)) gene_name <- ""
    has_hog <- !is.na(hog) && nzchar(hog)

    key <- if (has_hog) hog else paste0("gene:", gene_id)
    if (key %in% seen) next
    seen <- c(seen, key)

    label <- if (nzchar(gene_name)) gene_name else gene_id
    coverage <- if (has_hog && !is.na(hog_counts[hog])) {
      sprintf("%d / %d species", hog_counts[[hog]], n_total)
    } else {
      "unassigned"
    }

    out[[length(out) + 1L]] <- list(
      gene = label,
      label = label,
      detail = if (identical(label, gene_id)) coverage else paste0(gene_id, " · ", coverage),
      gene_id = gene_id,
      hog_id = if (has_hog) hog else ""
    )
  }

  #two hits sharing a name would run the same query, so fall back to the id
  labels <- vapply(out, function(g) g$label, character(1))
  dupes <- labels[duplicated(labels)]
  if (length(dupes)) {
    out <- lapply(out, function(g) {
      if (g$label %in% dupes) g$gene <- g$gene_id
      g
    })
  }
  out
}

#' Orthogroup and coverage for a gene, without running a full query
#'
#' Cheap enough for the search pill and the resume card, which need the HOG and
#' the species count before (or instead of) a real query_gene_flexible() run.
#'
#' @param lookup_table gene_lookup data.table (or data.frame)
#' @param query Gene name or id
#' @param hog_counts Named integer vector of species-per-HOG
#' @param n_total Number of species in the current config
#' @return List with found, gene_id, hog_id, n_species, n_total
rnx_gene_scope_info <- function(lookup_table, query, hog_counts = integer(0), n_total = 4L) {
  miss <- list(found = FALSE, gene_id = "", hog_id = "", n_species = 0L, n_total = n_total)
  q <- toupper(trimws(query %||% ""))
  if (!nzchar(q) || is.null(lookup_table) || !NROW(lookup_table)) return(miss)

  gl <- lookup_table
  nm <- if ("gene_name_upper" %in% names(gl)) gl$gene_name_upper else toupper(as.character(gl$gene_name))
  id <- if ("gene_id_upper" %in% names(gl)) gl$gene_id_upper else toupper(as.character(gl$gene_id))
  nm[is.na(nm)] <- ""
  id[is.na(id)] <- ""

  hit <- which(nm == q | id == q)
  if (!length(hit)) return(miss)

  i <- hit[1]
  hog <- as.character(gl$hog_id[i])
  has_hog <- !is.na(hog) && nzchar(hog)
  n_sp <- if (has_hog && !is.na(hog_counts[hog])) as.integer(hog_counts[[hog]]) else 0L

  list(
    found = TRUE,
    gene_id = as.character(gl$gene_id[i]),
    hog_id = if (has_hog) hog else "",
    n_species = n_sp,
    n_total = n_total
  )
}

#' Example genes that actually exist in the loaded data
#'
#' Keeps the launchpad and resume chips honest after a custom upload: anything
#' the lookup table does not know is dropped, and the shortfall is topped up
#' from the table itself.
#'
#' @param lookup_table gene_lookup data.table (or data.frame)
#' @param candidates Character vector of preferred gene names
#' @param n How many chips to return
#' @return Character vector of gene labels
rnx_example_genes <- function(lookup_table, candidates = EXAMPLE_GENES, n = 5L) {
  if (is.null(lookup_table) || !NROW(lookup_table)) return(character(0))

  gl <- lookup_table
  nm <- if ("gene_name_upper" %in% names(gl)) gl$gene_name_upper else toupper(as.character(gl$gene_name))
  id <- if ("gene_id_upper" %in% names(gl)) gl$gene_id_upper else toupper(as.character(gl$gene_id))
  nm[is.na(nm)] <- ""
  id[is.na(id)] <- ""

  known <- unique(c(nm, id))
  out <- candidates[toupper(candidates) %in% known]

  #fall back to named genes from the loaded table, then to bare ids
  if (length(out) < n) {
    named <- unique(as.character(gl$gene_name[nzchar(nm)]))
    out <- unique(c(out, named))
  }
  if (length(out) < n) {
    out <- unique(c(out, as.character(gl$gene_id)))
  }
  head(out[nzchar(out)], n)
}
