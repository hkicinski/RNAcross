#query and lookup functions for gene and orthogroup searches

#function to query genes using the lookup table
query_gene_lookup <- function(query, species_filter = NULL, current_data = NULL) {
  #access the gene lookup table from current data
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup
  
  #ensure it's a data.table
  if (!is.data.table(lookup_table)) {
    lookup_table <- as.data.table(lookup_table)
  }
  
  #normalize query
  query <- toupper(trimws(query))
  
  #debug print
  cat("Searching for:", query, "\n")
  
  #search in both gene_id and gene_name
  matches <- lookup_table[toupper(gene_id) == query | toupper(gene_name) == query, ]
  
  #debug print
  cat("Found", nrow(matches), "matches before species filter\n")
  
  #apply species filter if provided
  if (!is.null(species_filter) && length(species_filter) > 0) {
    matches <- matches[species %in% species_filter]
    cat("Found", nrow(matches), "matches after species filter for:", species_filter, "\n")
  }
  
  return(matches)
}

#function to get expression matrix ID for a gene
get_expression_id <- function(gene_id, species_code) {
  lookup_table <- all_species_data$gene_lookup
  match <- lookup_table[gene_id == gene_id & species == species_code]
  
  if (nrow(match) > 0) {
    return(match$expression_id[1])
  }
  return(NULL)
}

#function to get all genes in an orthogroup
get_orthogroup_genes <- function(group_id, use_og = FALSE, current_data = NULL) {
  if (is.null(current_data)) {
    current_data <- all_species_data  
  }
  lookup_table <- current_data$gene_lookup
  
  if (!is.data.table(lookup_table)) {
    lookup_table <- as.data.table(lookup_table)
  }
  
  #standard HOG-based search
  result <- lookup_table[hog_id == group_id, ]
  
  #automatic fallback: if we don't get all 4 species, try OG
  if (length(unique(result$species)) < 4 && nrow(result) > 0) {
    #get the og_id(s) for this HOG
    og_ids <- unique(result$og_id)
    og_ids <- og_ids[!is.na(og_ids)]  #remove NAs
    
    if (length(og_ids) > 0) {
      og_result <- lookup_table[og_id %in% og_ids, ]
      if (nrow(og_result) > nrow(result)) {
        cat("Automatic OG fallback: found", nrow(og_result), "genes vs", nrow(result), "in HOG\n")
        result <- og_result
      }
    }
  }
  
  return(result)
}

#function to identify species from gene ID using lookup table
identify_species <- function(gene_id, current_data = NULL) {
  if (is.null(current_data)) {
    current_data <- all_species_data
  }
  lookup_table <- current_data$gene_lookup
  match <- lookup_table[gene_id == gene_id]
  
  if (nrow(match) > 0) {
    return(as.character(match$species[1]))
  }
  return(NULL)
}

#function for orthogroup queries that works across all species
query_orthogroups <- function(gene_query, current_data, config, get_species_data_fn) {
  #searches in all species to find the gene
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

#modified query function for HOG + Gene Lookup
query_gene_flexible <- function(gene_query, species_data, all_species_data) {
  #normalize query
  gene_query <- toupper(trimws(gene_query))
  if (nchar(gene_query) == 0) return(NULL)
  
  cat("\n=== query_gene_flexible ===\n")
  cat("Query:", gene_query, "\n")
  
  #use the lookup table to find the gene
  lookup_matches <- query_gene_lookup(gene_query, NULL, all_species_data)
  
  cat("Total lookup matches:", nrow(lookup_matches), "\n")
  
  if (nrow(lookup_matches) == 0) {
    cat("No matches found in lookup table\n")
    return(NULL)
  }
  
  #get the first match
  match_info <- lookup_matches[1,]
  gene_id <- match_info$gene_id
  expression_id <- match_info$expression_id
  hog_id <- match_info$hog_id
  og_id <- match_info$og_id
  
  #use expression_id for retrieving actual expression data
  actual_gene_id <- if(!is.na(expression_id) && expression_id != "") expression_id else gene_id
  
  cat("First match - Gene ID:", gene_id, "HOG:", hog_id, "OG:", og_id, "\n")
  
  #build result
  result <- list(
    query = gene_query,
    gene_id = gene_id,
    expression_id = expression_id,
    match_type = ifelse(gene_query == toupper(gene_id), "direct", "gene_name"),
    source = "gene_lookup",
    orthogroup = hog_id,
    og_id = og_id,  #add og_id to result
    genes_by_species = list()
  )
  
  #get all genes in this HOG (try both HOG and OG)
  if (!is.na(hog_id) && nchar(hog_id) > 0) {
    hog_genes <- get_orthogroup_genes(hog_id, FALSE, all_species_data)
    
    #if we didn't find enough genes, try using the OG ID
    if (nrow(hog_genes) < 4 && !is.na(og_id)) {
      cat("Trying OG-based search as HOG gave limited results\n")
      og_id_value <- og_id
      og_genes <- all_species_data$gene_lookup[og_id == og_id_value]
      if (nrow(og_genes) > nrow(hog_genes)) {
        cat("OG search found more genes:", nrow(og_genes), "vs", nrow(hog_genes), "\n")
        hog_genes <- og_genes
      }
    }
    
    cat("Total orthogroup genes found:", nrow(hog_genes), "\n")
    
    #group by species
    for (sp in unique(hog_genes$species)) {
      sp_genes <- hog_genes[species == sp]
      
      cat("  Species", sp, ":", nrow(sp_genes), "genes\n")
      
      #create display format
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
  }
  
  return(result)
}

#simplified add_gene_names function
add_gene_names <- function(gene_ids, species_id) {
  if (length(gene_ids) == 0) return(data.frame(
    gene_id = character(),
    gene_name = character(),
    display = character(),
    stringsAsFactors = FALSE
  ))
  
  #use lookup table
  lookup_table <- all_species_data$gene_lookup
  
  #get genes for this species
  gene_info <- lookup_table[gene_id %in% gene_ids & species == species_id]
  
  #create result dataframe
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

#legacy search function for basic compatibility
search_gene <- function(query, species_data) {
  query <- toupper(trimws(query))
  if (nchar(query) == 0) return(NULL)
  
  #direct match to LCPM 
  if (query %in% rownames(species_data$lcpm)) {
    return(list(id = query, type = "direct_match"))
  }
  
  #matches via annotation 
  if (query %in% species_data$anno$GeneName) {
    gene_id <- species_data$anno$GeneID[species_data$anno$GeneName == query]
    if (length(gene_id) == 1 && gene_id[1] %in% rownames(species_data$lcpm)) {
      return(list(id = gene_id[1], type = "annotation_match"))
    }
  }
  
  #legacy S. cerevisiae match (if available)
  if (!is.null(all_species_data$legacy_synteny)) {
  }
  
  return(NULL)
}