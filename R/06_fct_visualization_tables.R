#table and UI element creation functions

#create ortholog table for display
create_ortholog_table <- function(gene_mapping, config = NULL) {
  #prepare data for table
  table_data <- data.frame()
  
  for (og_id in names(gene_mapping)) {
    og_data <- gene_mapping[[og_id]]
    
    #create a row for each gene in the orthogroup
    for (species in names(og_data$genes)) {
      if (length(og_data$genes[[species]]) > 0) {
        for (gene in og_data$genes[[species]]) {
          #use config to get species name if available
          species_name <- if(!is.null(config) && species %in% names(config)) {
            config[[species]]$name
          } else {
            species
          }
          
          table_data <- rbind(table_data, data.frame(
            Orthogroup = og_id,
            Species = species_name,
            Gene = gene,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  #create a DT datatable
  dt <- datatable(
    table_data,
    options = list(
      pageLength = 10,
      searching = TRUE,
      ordering = TRUE
    ),
    rownames = FALSE
  )
  
  return(dt)
}

#create summary table for gene groups
create_group_summary_table <- function(gene_groups_list, species_data) {
  #prepare summary data
  summary_data <- data.frame()
  
  for (group_name in names(gene_groups_list)) {
    genes <- gene_groups_list[[group_name]]
    
    #calculate summary statistics for this group
    n_genes <- length(genes)
    
    #check how many exist in expression data
    if (!is.null(species_data$lcpm)) {
      genes_found <- sum(genes %in% rownames(species_data$lcpm))
    } else {
      genes_found <- 0
    }
    
    summary_data <- rbind(summary_data, data.frame(
      Group = group_name,
      `Total Genes` = n_genes,
      `Genes Found` = genes_found,
      `Missing` = n_genes - genes_found,
      `Coverage (%)` = round((genes_found / n_genes) * 100, 1),
      stringsAsFactors = FALSE
    ))
  }
  
  #create datatable
  dt <- datatable(
    summary_data,
    options = list(
      pageLength = 10,
      dom = 't',  #table only, no search/pagination for small tables
      ordering = TRUE
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      'Coverage (%)',
      backgroundColor = styleInterval(c(50, 80), c('#ffcccc', '#ffffcc', '#ccffcc'))
    )
  
  return(dt)
}

#create HTML summary for orthogroup
create_orthogroup_summary <- function(orthogroup_genes, orthogroup_id) {
  total_genes <- sum(sapply(orthogroup_genes, nrow))
  n_species <- length(orthogroup_genes)
  
  html_content <- tags$div(
    tags$h4(paste("Orthogroup:", orthogroup_id)),
    tags$p(
      tags$strong("Total genes:"), total_genes, tags$br(),
      tags$strong("Species represented:"), n_species, tags$br()
    ),
    tags$hr(),
    tags$h5("Genes by species:"),
    tags$ul(
      lapply(names(orthogroup_genes), function(sp) {
        genes_df <- orthogroup_genes[[sp]]
        if (nrow(genes_df) > 0) {
          tags$li(
            tags$strong(paste0(sp, ":")),
            paste(genes_df$display, collapse = ", ")
          )
        }
      })
    )
  )
  
  return(html_content)
}

#create detailed orthogroup member table
create_orthogroup_details_table <- function(orthogroup_genes, config = NULL) {
  #flatten the orthogroup data for table display
  table_data <- data.frame()
  
  for (species_code in names(orthogroup_genes)) {
    genes_df <- orthogroup_genes[[species_code]]
    
    if (nrow(genes_df) > 0) {
      #get species name from config if available
      species_name <- if(!is.null(config) && species_code %in% names(config)) {
        config[[species_code]]$name
      } else {
        species_code
      }
      
      for (i in 1:nrow(genes_df)) {
        table_data <- rbind(table_data, data.frame(
          Species = species_name,
          `Gene ID` = genes_df$gene_id[i],
          `Gene Name` = ifelse(is.na(genes_df$gene_name[i]) || genes_df$gene_name[i] == "",
                               "-", genes_df$gene_name[i]),
          `Display` = genes_df$display[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  #create formatted datatable
  dt <- datatable(
    table_data,
    options = list(
      pageLength = 15,
      searching = TRUE,
      ordering = TRUE,
      columnDefs = list(
        list(width = '25%', targets = 0),  #species
        list(width = '25%', targets = 1),  #gene ID
        list(width = '25%', targets = 2),  #gene name
        list(width = '25%', targets = 3)   #display
      )
    ),
    rownames = FALSE,
    caption = "Orthogroup Members"
  )
  
  return(dt)
}

#create enhanced UI for orthogroup selection
create_orthogroup_selection_ui_enhanced <- function(orthogroup_genes, species_id, input_prefix = "gene") {
  ui_elements <- list()
  
  for (sp_code in names(orthogroup_genes)) {
    genes_df <- orthogroup_genes[[sp_code]]
    
    if (nrow(genes_df) > 0) {
      #get species display name
      sp_name <- switch(sp_code,
                       "cg" = "C. glabrata",
                       "sc" = "S. cerevisiae", 
                       "kl" = "K. lactis",
                       "ca" = "C. albicans",
                       sp_code)
      
      #create choices for selectInput
      choices <- setNames(genes_df$gene_id, genes_df$display)
      
      #add species-specific selectInput
      ui_elements[[sp_code]] <- column(
        width = 3,
        selectInput(
          inputId = paste0(input_prefix, "_", sp_code),
          label = sp_name,
          choices = choices,
          selected = if(sp_code == species_id && length(choices) > 0) choices[1] else NULL,
          width = "100%"
        )
      )
    }
  }
  
  #wrap in fluidRow
  if (length(ui_elements) > 0) {
    return(fluidRow(ui_elements))
  } else {
    return(tags$p("No orthologs found"))
  }
}

#create species analysis panel
create_species_panel <- function(species_id, species_name, config = NULL) {
  tabPanel(
    title = species_name,
    value = paste0("tab_", species_id),
    
    fluidRow(
      column(12,
        tags$h3(paste("Analysis for", species_name)),
        tags$hr()
      )
    ),
    
    #gene selection section
    fluidRow(
      column(12,
        wellPanel(
          tags$h4("Gene Selection"),
          fluidRow(
            column(8,
              textInput(
                inputId = paste0("gene_query_", species_id),
                label = "Enter gene ID or name:",
                placeholder = "e.g., YAL001C or TFC3",
                width = "100%"
              )
            ),
            column(4,
              actionButton(
                inputId = paste0("search_gene_", species_id),
                label = "Search",
                class = "btn-primary",
                style = "margin-top: 25px;"
              )
            )
          ),
          
          #search results area
          uiOutput(paste0("search_results_", species_id)),
          
          #orthogroup information
          conditionalPanel(
            condition = paste0("output.show_orthogroup_", species_id),
            tags$hr(),
            tags$h5("Orthogroup Information"),
            uiOutput(paste0("orthogroup_info_", species_id))
          )
        )
      )
    ),
    
    #visualization section
    fluidRow(
      column(12,
        tabsetPanel(
          id = paste0("viz_tabs_", species_id),
          
          #expression plot tab
          tabPanel(
            "Expression Plot",
            plotlyOutput(paste0("gene_plot_", species_id), height = "500px"),
            tags$br(),
            downloadButton(paste0("download_plot_", species_id), "Download Plot")
          ),
          
          #data table tab
          tabPanel(
            "Expression Data",
            DTOutput(paste0("expression_table_", species_id)),
            tags$br(),
            downloadButton(paste0("download_data_", species_id), "Download Data")
          ),
          
          #pca tab
          tabPanel(
            "PCA Analysis",
            plotlyOutput(paste0("pca_plot_", species_id), height = "500px"),
            tags$br(),
            downloadButton(paste0("download_pca_", species_id), "Download PCA")
          )
        )
      )
    ),
    
    #footer with metadata
    fluidRow(
      column(12,
        tags$hr(),
        tags$div(
          class = "text-muted",
          tags$small(
            paste("Dataset:", species_id, "|",
                  "Last updated:", Sys.Date())
          )
        )
      )
    )
  )
}

#create expression data table
create_expression_data_table <- function(lcpm_matrix, gene_id, sample_info) {
  #extract expression values for the gene
  if (!gene_id %in% rownames(lcpm_matrix)) {
    return(NULL)
  }
  
  expr_values <- lcpm_matrix[gene_id, ]
  
  #combine with sample info
  table_data <- data.frame(
    Sample = names(expr_values),
    Timepoint = sample_info$Timepoint,
    Replicate = sample_info$Replicate,
    Expression = round(expr_values, 3),
    stringsAsFactors = FALSE
  )
  
  #create datatable
  dt <- datatable(
    table_data,
    options = list(
      pageLength = 15,
      searching = FALSE,
      ordering = TRUE
    ),
    rownames = FALSE,
    caption = paste("Expression values for", gene_id)
  ) %>%
    formatStyle(
      'Expression',
      background = styleColorBar(range(table_data$Expression), 'lightblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
  
  return(dt)
}

#helper function to extract ortholog expression data
extract_ortholog_expression <- function(gene_mapping, species_data_list, config = NULL) {
  expression_data <- data.frame()
  
  for (og_id in names(gene_mapping)) {
    og_data <- gene_mapping[[og_id]]
    
    for (species_code in names(og_data$genes)) {
      if (length(og_data$genes[[species_code]]) > 0) {
        #get species data
        species_data <- species_data_list[[species_code]]
        if (is.null(species_data)) next
        
        #get expression matrix and sample info
        lcpm_matrix <- if(!is.null(species_data$lcpm)) {
          species_data$lcpm
        } else if(!is.null(species_data[[paste0(species_code, "_lcpm")]])) {
          species_data[[paste0(species_code, "_lcpm")]]
        } else {
          NULL
        }
        
        sample_info <- if(!is.null(species_data$sample_info)) {
          species_data$sample_info
        } else if(!is.null(species_data[[paste0(species_code, "_sample_info")]])) {
          species_data[[paste0(species_code, "_sample_info")]]
        } else {
          NULL
        }
        
        if (is.null(lcpm_matrix) || is.null(sample_info)) next
        
        #get species name for display
        species_name <- if(!is.null(config) && species_code %in% names(config)) {
          config[[species_code]]$short
        } else {
          species_code
        }
        
        #extract expression for each gene
        for (gene_id in og_data$genes[[species_code]]) {
          if (gene_id %in% rownames(lcpm_matrix)) {
            expr_values <- lcpm_matrix[gene_id, ]
            
            #create data frame for this gene
            gene_expr <- data.frame(
              Gene = og_data$display_name,
              Species = species_name,
              Sample = names(expr_values),
              Timepoint = sample_info$Timepoint,
              Replicate = sample_info$Replicate,
              Expression = as.numeric(expr_values),
              stringsAsFactors = FALSE
            )
            
            expression_data <- rbind(expression_data, gene_expr)
          }
        }
      }
    }
  }
  
  return(expression_data)
}

#extract orthology information for gene list
extract_orthology_for_genes <- function(gene_list, all_species_data = NULL, config = NULL) {
  if (is.null(all_species_data)) {
    all_species_data <- all_species_data  #use global
  }
  
  lookup_table <- all_species_data$gene_lookup
  gene_mapping <- list()
  
  for (query_gene in gene_list) {
    #find the gene in lookup table
    matches <- query_gene_lookup(query_gene, NULL, all_species_data)
    
    if (nrow(matches) > 0) {
      #get HOG/OG for this gene
      hog_id <- matches$hog_id[1]
      og_id <- matches$og_id[1]
      
      #use HOG if available, otherwise OG
      group_id <- if(!is.na(hog_id) && hog_id != "") hog_id else og_id
      
      if (!is.na(group_id) && group_id != "") {
        #get all genes in this orthogroup
        og_genes <- get_orthogroup_genes(group_id, FALSE, all_species_data)
        
        #organize by species
        genes_by_species <- list()
        for (sp in unique(og_genes$species)) {
          sp_genes <- og_genes[species == sp]$gene_id
          genes_by_species[[sp]] <- sp_genes
        }
        
        gene_mapping[[group_id]] <- list(
          query = query_gene,
          display_name = query_gene,  #or use gene name if available
          genes = genes_by_species
        )
      }
    }
  }
  
  return(gene_mapping)
}