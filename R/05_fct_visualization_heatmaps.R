#heatmap and group visualization functions

#01. Gene Group Visualization 
#group visualization
create_group_visualization <- function(plot_data, viz_type, is_dark_mode = FALSE,
                                     distance_method = "pearson", 
                                     linkage_method = "average",
                                     data_transform = "raw",
                                     show_significance = TRUE,
                                     alpha = 0.05,
                                     selected_gene = NULL,
                                     selected_comparisons = NULL) {
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  grid_color <- if(is_dark_mode) "gray30" else "gray90"
  
  
  if (viz_type == "line") {
    #get unique gene count for subtitle
    n_genes <- length(unique(plot_data$Gene))
    n_groups <- length(unique(plot_data$Group))
    
    p <- ggplot(plot_data, 
                aes(x = Timepoint, y = Expression, 
                    color = Gene, group = interaction(Gene, Replicate))) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      #remove the facet_wrap line entirely
      labs(
        title = "Gene Set Analysis: Trajectory Plot",
        subtitle = paste("Analyzing", n_genes, "genes across", n_groups, 
                         if(n_groups == 1) "group" else "groups"),
        x = "Timepoint",
        y = "log2 count per million"
      ) +
      theme_minimal()
    
    #apply theme
    p <- p + theme(
      text = element_text(color = text_color),
      axis.text.x = element_text(angle = 45, hjust = 1, color = text_color),
      axis.text.y = element_text(color = text_color),
      plot.title = element_text(size = 14, face = "bold", color = text_color),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = ifelse(is_dark_mode, "gray20", "gray95")),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color)
    )
    
    return(ggplotly(p) %>%
             layout(
               plot_bgcolor = plot_bg_color,
               paper_bgcolor = plot_bg_color,
               font = list(color = text_color),
               hoverlabel = list(bgcolor = if(is_dark_mode) "#444" else "white"),
               showlegend = TRUE
             ))
    
  } else if (viz_type == "bar") {
    summary_data <- plot_data %>%
      group_by(Timepoint, Group, Gene) %>%
      summarise(Mean = mean(Expression), .groups = 'drop')
    
    p <- ggplot(summary_data, aes(x = Timepoint, y = Mean, fill = Gene)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Group) +
      theme_minimal()
    
    #apply theme
    p <- p + theme(
      text = element_text(color = text_color),
      axis.text.x = element_text(angle = 45, hjust = 1, color = text_color),
      axis.text.y = element_text(color = text_color),
      plot.title = element_text(size = 14, face = "bold", color = text_color),
      panel.grid.major = element_line(color = grid_color),
      panel.grid.minor = element_line(color = ifelse(is_dark_mode, "gray20", "gray95")),
      plot.background = element_rect(fill = plot_bg_color, color = NA),
      panel.background = element_rect(fill = plot_bg_color, color = NA),
      legend.background = element_rect(fill = plot_bg_color),
      legend.text = element_text(color = text_color),
      legend.title = element_text(color = text_color),
      strip.background = element_rect(fill = plot_bg_color),
      strip.text = element_text(color = text_color)
    )
    
    #add significance markers if requested
    if (show_significance && !is.null(selected_gene) && !is.null(selected_comparisons) && length(selected_comparisons) > 0) {
      #calculate significance
      sig_results <- calculate_significance(
        plot_data, 
        alpha, 
        selected_gene, 
        selected_comparisons
      )
      
      #add significance annotations if we have results
      if (length(sig_results) > 0) {
        #convert significance results to a data frame for ggplot
        sig_data <- data.frame(
          Gene = sapply(sig_results, function(x) x$gene),
          Group = sapply(sig_results, function(x) unique(plot_data$Group[plot_data$Gene == x$gene])[1]),
          Timepoint1 = sapply(sig_results, function(x) x$timepoint1),
          Timepoint2 = sapply(sig_results, function(x) x$timepoint2),
          YPosition = sapply(sig_results, function(x) x$y_position),
          Significance = sapply(sig_results, function(x) x$significance),
          stringsAsFactors = FALSE
        )
        
        #for each group-gene combination, add significance markers
        for (g in unique(sig_data$Group)) {
          for (gene in unique(sig_data$Gene[sig_data$Group == g])) {
            gene_sig <- sig_data[sig_data$Group == g & sig_data$Gene == gene, ]
            
            for (i in 1:nrow(gene_sig)) {
              #get positions for timepoints
              pos1 <- which(levels(summary_data$Timepoint) == gene_sig$Timepoint1[i])
              pos2 <- which(levels(summary_data$Timepoint) == gene_sig$Timepoint2[i])
              
              #calculate bar positions accounting for dodge position
              dodge_width <- 0.9  #default dodge width
              gene_pos <- which(unique(summary_data$Gene) == gene) - 1
              num_genes <- length(unique(summary_data$Gene))
              bar_width <- dodge_width / num_genes
              
              #adjusted positions
              x1 <- pos1 - dodge_width/2 + bar_width/2 + gene_pos * bar_width
              x2 <- pos2 - dodge_width/2 + bar_width/2 + gene_pos * bar_width
              
              #draw bracket and significance marker
              p <- p + 
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x1,
                    xend = x2,
                    y = gene_sig$YPosition[i],
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                #vertical segments at each end
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x1,
                    xend = x1,
                    y = gene_sig$YPosition[i] - 0.2,
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                geom_segment(
                  data = data.frame(
                    Group = g,
                    x = x2,
                    xend = x2,
                    y = gene_sig$YPosition[i] - 0.2,
                    yend = gene_sig$YPosition[i]
                  ),
                  aes(x = x, y = y, xend = xend, yend = yend),
                  inherit.aes = FALSE
                ) +
                #significance text
                geom_text(
                  data = data.frame(
                    Group = g,
                    x = (x1 + x2) / 2,
                    y = gene_sig$YPosition[i] + 0.2,
                    label = gene_sig$Significance[i]
                  ),
                  aes(x = x, y = y, label = label),
                  inherit.aes = FALSE,
                  size = 3
                )
            }
          }
        }
      }
    }
    
    #convert to plotly with adjusted layout
    return(ggplotly(p) %>%
             layout(
               plot_bgcolor = plot_bg_color,
               paper_bgcolor = plot_bg_color,
               font = list(color = text_color),
               hoverlabel = list(bgcolor = if(is_dark_mode) "#444" else "white"),
               showlegend = TRUE
             ))
    
  } else if (viz_type == "heatmap") {
    #prepare data for heatmap
    heatmap_data <- plot_data %>%
      group_by(Gene, Timepoint) %>%
      summarise(Expression = mean(Expression), .groups = 'drop') %>%
      pivot_wider(names_from = Timepoint, values_from = Expression)
    
    #extract gene names and create expression matrix
    gene_names <- heatmap_data$Gene
    expression_matrix <- as.matrix(heatmap_data[, -1])
    rownames(expression_matrix) <- gene_names
    
    #apply data transformation
    transformed_matrix <- switch(data_transform,
                                 "raw" = expression_matrix,
                                 "centered" = t(scale(t(expression_matrix), center = TRUE, scale = FALSE)),
                                 "zscore" = t(scale(t(expression_matrix), center = TRUE, scale = TRUE)),
                                 expression_matrix
    )
    
    #apply clustering if there's more than one gene
    if (nrow(transformed_matrix) > 1) {
      if (distance_method == "euclidean") {
        #euclidean distance clustering
        dist_matrix <- dist(transformed_matrix, method = "euclidean")
        hclust_result <- hclust(dist_matrix, method = linkage_method)
        row_order <- hclust_result$order
      } else if (distance_method == "pearson") {
        #pearson correlation distance (1 - correlation)
        cor_matrix <- cor(t(transformed_matrix))
        dist_matrix <- as.dist(1 - cor_matrix)
        hclust_result <- hclust(dist_matrix, method = linkage_method)
        row_order <- hclust_result$order
      } else {
        #no clustering
        row_order <- 1:nrow(transformed_matrix)
      }
      
      #reorder matrix according to clustering
      transformed_matrix <- transformed_matrix[row_order, ]
    }
    
    #set plot parameters based on transformation
    plot_params <- switch(data_transform,
                          "raw" = list(
                            zmid = mean(range(transformed_matrix)),
                            zmin = min(transformed_matrix),
                            zmax = max(transformed_matrix),
                            title = "Raw log2CPM"
                          ),
                          "centered" = list(
                            zmid = 0,
                            zmin = -2,
                            zmax = 2,
                            title = "Centered log2CPM"
                          ),
                          "zscore" = list(
                            zmid = 0,
                            zmin = -2,
                            zmax = 2,
                            title = "Z-score"
                          ),
                          list(
                            zmid = mean(range(transformed_matrix)),
                            zmin = min(transformed_matrix),
                            zmax = max(transformed_matrix),
                            title = "Expression Values"
                          )
    )
    
    #create heatmap with clustering info in title
    p <- plot_ly(
      z = transformed_matrix,
      x = colnames(transformed_matrix),
      y = rownames(transformed_matrix),
      type = "heatmap",
      colorscale = list(
        c(0, "#0000FF"),
        c(0.5, "#000000"),
        c(1, "#FFFF00")
      ),
      zmid = plot_params$zmid,
      zmin = plot_params$zmin,
      zmax = plot_params$zmax,
      hoverinfo = "text",
      text = matrix(
        paste(
          "Gene:", rep(rownames(transformed_matrix), each = ncol(transformed_matrix)),
          "<br>Time:", rep(colnames(transformed_matrix), times = nrow(transformed_matrix)),
          "<br>Value:", round(as.vector(t(transformed_matrix)), 2)
        ),
        nrow = nrow(transformed_matrix),
        ncol = ncol(transformed_matrix),
        byrow = TRUE
      )
    ) %>%
      layout(
        title = paste("Gene Expression Heatmap -", plot_params$title, 
                      "<br><sub>Clustering:", 
                      if(nrow(transformed_matrix) > 1) paste(distance_method, "distance") else "None",
                      "</sub>"),
        xaxis = list(
          title = "Timepoint",
          tickangle = -45
        ),
        yaxis = list(
          title = "Gene",
          autorange = "reversed"
        ),
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = plot_bg_color,
        font = list(color = text_color)
      )
    
    return(p)
  }
}

#used for the bar-graph visualization in gene groups 
calculate_significance <- function(plot_data, alpha = 0.05, selected_gene = NULL, selected_comparisons = NULL) {
  #if no gene or comparisons selected, return empty list
  if (is.null(selected_gene) || is.null(selected_comparisons) || length(selected_comparisons) == 0) {
    return(list())
  }
  
  #filter data for the selected gene
  gene_data <- plot_data[plot_data$Gene == selected_gene, ]
  if (nrow(gene_data) == 0) {
    return(list())
  }
  
  timepoints <- levels(plot_data$Timepoint)
  sig_results <- list()
  
  #process each selected comparison
  for (comparison in selected_comparisons) {
    #parse the comparison string
    t_pair <- strsplit(comparison, " vs. ")[[1]]
    t1 <- t_pair[1]
    t2 <- t_pair[2]
    
    #extract data for these timepoints
    t1_data <- gene_data$Expression[gene_data$Timepoint == t1]
    t2_data <- gene_data$Expression[gene_data$Timepoint == t2]
    
    #only test if we have enough data points (at least 2 replicates)
    if (length(t1_data) >= 2 && length(t2_data) >= 2) {
      #perform t-test
      test_result <- t.test(t1_data, t2_data)
      p_value <- test_result$p.value
      
      #determine significance level
      significance <- "ns"
      if (p_value < 0.001) significance <- "***"
      else if (p_value < 0.01) significance <- "**" 
      else if (p_value < 0.05) significance <- "*"
      
      #add result regardless of significance (to show "ns" when not significant)
      sig_results[[length(sig_results) + 1]] <- list(
        gene = selected_gene,
        timepoint1 = t1,
        timepoint2 = t2,
        p_value = p_value,
        significance = significance,
        mean1 = mean(t1_data),
        mean2 = mean(t2_data),
        y_position = max(mean(t1_data), mean(t2_data)) * 1.1  #position above the bars
      )
    }
  }
  
  return(sig_results)
}
#02. Cross-sps heatmap 
#create a cross-species heatmap visualization
create_cross_species_heatmap <- function(heatmap_matrix, is_dark_mode = FALSE, 
                                         cluster_rows = TRUE, cluster_cols = FALSE,
                                         config = NULL, species_colors_dynamic = NULL) {
  #set color scheme based on dark mode
  plot_bg_color <- if(is_dark_mode) "#2c3034" else "white"
  text_color <- if(is_dark_mode) "white" else "black"
  
  #define column groups (for species)
  column_names <- colnames(heatmap_matrix)
  species_names <- sapply(strsplit(column_names, "_"), function(x) x[1])
  
  #get unique species for dynamic color assignment
  unique_species <- unique(species_names)
  
  #create species colors mapping using the actual species names from columns
  if (!is.null(species_colors_dynamic)) {
    #map the provided colors to the actual species names in the data
    species_colors <- list()
    
    #the dynamic colors are keyed by short names from config, but our column names might be full
    for (sp_name in unique_species) {
      #check if this exact name exists in dynamic colors
      if (sp_name %in% names(species_colors_dynamic)) {
        species_colors[[sp_name]] <- species_colors_dynamic[[sp_name]]
      } else {
        #try to find a matching species by checking all provided colors
        found_color <- FALSE
        for (provided_name in names(species_colors_dynamic)) {
          #check if this is a match (could be short vs full name)
          if (grepl(sp_name, provided_name, ignore.case = TRUE) || 
              grepl(provided_name, sp_name, ignore.case = TRUE)) {
            species_colors[[sp_name]] <- species_colors_dynamic[[provided_name]]
            found_color <- TRUE
            break
          }
        }
        
        #if still no match, assign a default color
        if (!found_color) {
          if (FALSE) {
            } else {
            #fallback color
            default_palette <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
            idx <- (which(unique_species == sp_name) - 1) %% length(default_palette) + 1
            species_colors[[sp_name]] <- default_palette[idx]
          }
        }
      }
    }
  } else {
    #no dynamic colors provided, use defaults based on species names
    species_colors <- list()
    for (sp_name in unique_species) {
      if (grepl("albicans", sp_name, ignore.case = TRUE)) {
        species_colors[[sp_name]] <- "#74ac4c"  #green
      } else if (grepl("glabrata", sp_name, ignore.case = TRUE)) {
        species_colors[[sp_name]] <- "red"  #red  
      } else if (grepl("lactis", sp_name, ignore.case = TRUE)) {
        species_colors[[sp_name]] <- "#f8c434"  #yellow
      } else if (grepl("cerevisiae", sp_name, ignore.case = TRUE)) {
        species_colors[[sp_name]] <- "blue"  #blue
      } else {
        #default colors for unknown species
        default_palette <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4",
                            "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F")
        idx <- (which(unique_species == sp_name) - 1) %% length(default_palette) + 1
        species_colors[[sp_name]] <- default_palette[idx]
      }
    }
  }
  
  #build species mapping dynamically from config if provided
  if (!is.null(config)) {
    species_mapping <- list()
    for (sp in unique_species) {
      #try to find this species in the config
      found <- FALSE
      for (sp_code in names(config)) {
        if (config[[sp_code]]$short == sp || config[[sp_code]]$name == sp) {
          species_mapping[[sp]] <- config[[sp_code]]$name
          found <- TRUE
          break
        }
      }
      if (!found) {
        #fallback to using the species name as-is
        species_mapping[[sp]] <- sp
      }
    }
  } else {
    #fallback mapping
    species_mapping <- setNames(unique_species, unique_species)
  }
  
  #create a one-row matrix for the species annotation bar
  unique_species <- unique(species_names)
  species_to_num <- setNames(seq_along(unique_species), unique_species)
  species_matrix <- matrix(sapply(species_names, function(s) species_to_num[[s]]), nrow = 1)
  colnames(species_matrix) <- column_names
  rownames(species_matrix) <- "Species"
  
  #custom colorscale for species bar - ALWAYS use position-based colors
  colorscale <- lapply(seq_along(unique_species), function(i) {
    species <- unique_species[i]
    
    #IGNORE species_colors parameter completely - always use dynamic palette
    dynamic_palette <- c("#808080", "#FFB6C1", "#8B4513", "#A52A2A", 
                        "#4682B4", "#32CD32", "#FF6347", "#9370DB",
                        "#20B2AA", "#FF69B4", "#CD853F", "#00CED1")
    species_color <- dynamic_palette[((i - 1) %% length(dynamic_palette)) + 1]
    
    return(list(
      (i-1)/(max(1, length(unique_species)-1)),
      species_color
    ))
  })
  
  #convert the nested list to the format plotly expects
  colorscale_flat <- unlist(colorscale, recursive = FALSE)
  
  #create the species annotation bar
  discrete_colorscale <- list()
  for (i in seq_along(unique_species)) {
    #add two entries per color to create discrete blocks
    val_start <- (i - 1) / length(unique_species)
    val_end <- i / length(unique_species)
    color <- colorscale[[i]][[2]]
    
    discrete_colorscale[[length(discrete_colorscale) + 1]] <- list(val_start, color)
    discrete_colorscale[[length(discrete_colorscale) + 1]] <- list(val_end, color)
  }
  
  p1 <- plot_ly() %>%
    add_heatmap(
      z = species_matrix,
      x = column_names,
      y = c(""),  #no y labels
      showscale = FALSE,
      colorscale = discrete_colorscale,
      zmin = 0.5,
      zmax = length(unique_species) + 0.5,
      hoverinfo = "text",
      text = matrix(paste("Species:", species_names), nrow = 1)
    ) %>%
    layout(
      xaxis = list(
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      margin = list(l = 100, r = 50, b = 0, t = 10),
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = plot_bg_color
    )
  
  p2 <- plot_ly(
    z = heatmap_matrix,
    x = column_names,
    y = rownames(heatmap_matrix),
    type = "heatmap",
    colorscale = "viridis",  
    zmin = -3, 
    zmax = 3,
    zmid = 0,
    hoverinfo = "text",
    text = matrix(
      paste(
        "Gene:", rep(rownames(heatmap_matrix), each = ncol(heatmap_matrix)),
        "<br>Species-Timepoint:", rep(column_names, times = nrow(heatmap_matrix)),
        "<br>Value:", round(as.vector(t(heatmap_matrix)), 2)
      ),
      nrow = nrow(heatmap_matrix),
      ncol = ncol(heatmap_matrix),
      byrow = TRUE
    ),
    showscale = TRUE,  #explicitly show the colorbar scale
    colorbar = list(
      title = "Z-score",
      len = 0.6,       #longer colorbar (was 0.4)
      thickness = 20,  #thicker colorbar (was 15)
      y = 1.1,        #higher position (was 0.77)
      x = 0.878,        #more to the right (was 0.85)
      outlinewidth = 1
    )
  )
  
  
  #build legend data from actual unique species in the data
  legend_data <- data.frame(
    species_code = unique_species,  #keep exact same as unique_species
    species_name = unique_species,  #display exactly what's in unique_species
    value = seq_along(unique_species),
    stringsAsFactors = FALSE
  )
  
  #create the legend heatmap
  legend_plot <- plot_ly(
    height = 100 * length(unique_species)  #adjust height based on number of species
  )
  
  #extract colors from the colorscale we created for the annotation bar
  annotation_colors <- list()
  for (i in seq_along(colorscale)) {
    #each element of colorscale is a list with [position, color]
    species <- unique_species[i]
    color <- colorscale[[i]][[2]]  #get the color (second element)
    annotation_colors[[species]] <- color
  }
  
  #debug: print what we have
  cat("DEBUG: annotation_colors =", paste(names(annotation_colors), annotation_colors, sep=":", collapse=", "), "\n")
  cat("DEBUG: legend_data$species_code =", paste(legend_data$species_code, collapse=", "), "\n")
  
  #add separate scatter traces for each species with custom markers
  for (i in 1:nrow(legend_data)) {
    species_code <- legend_data$species_code[i]
    species_name <- legend_data$species_name[i]
    
    #use the EXACT index to get color from colorscale to ensure match
    species_index <- which(unique_species == species_code)
    if (length(species_index) > 0) {
      marker_color <- colorscale[[species_index[1]]][[2]]
    } else {
      marker_color <- "#808080"  #fallback gray
    }
    
    cat("DEBUG: Species", species_code, "gets color", marker_color, "\n")
    
    #add colored square marker for the species  
    legend_plot <- legend_plot %>% add_trace(
      x = c(0.5),  #position in center
      y = c(nrow(legend_data) - i + 1),  #position from top to bottom
      type = "scatter",
      mode = "markers",
      marker = list(
        symbol = "square",
        size = 15,
        color = marker_color,  #this now uses annotation_colors which matches the bar
        line = list(color = "black", width = 1)
      ),
      text = species_code,  #use species_code for consistency
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    #add text label for the species
    legend_plot <- legend_plot %>% add_annotations(
      x = 0.9,  #position text to the right of square
      y = nrow(legend_data) - i + 1,
      text = species_code, 
      showarrow = FALSE,
      xanchor = "left"
    )
  }
  
  #configure the legend plot layout
  legend_plot <- legend_plot %>% layout(
    xaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      range = c(0, 2)  #set fixed range for consistent positioning
    ),
    yaxis = list(
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      range = c(0, nrow(legend_data) + 1)  #add some padding
    ),
    margin = list(l = 0, r = 0, t = 30, b = 0),
    title = list(text = "Species", x = 0.5),
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color
  )
  
  #main heatmap layout
  p2 <- p2 %>% layout(
  xaxis = list(
    title = "Species-Timepoint",
    tickangle = -45,
    tickfont = list(size = 10),
    showticklabels = TRUE
  ),
  yaxis = list(
    title = "Gene",
    autorange = "reversed",
    tickmode = "array", 
    tickvals = 0:(length(rownames(heatmap_matrix))-1),  #start from 0
    ticktext = rownames(heatmap_matrix),
    tickfont = list(size = 10)
  ),
  margin = list(l = 120, r = 100, b = 200, t = 0),  #increased bottom margin for labels
  plot_bgcolor = plot_bg_color,
  paper_bgcolor = plot_bg_color,
  coloraxis = list(colorscale = "viridis")  
)
  
  #annotation bar and main heatmap combined
  main_plot <- subplot(
    style(p1, showscale = FALSE), p2, 
    nrows = 2,
    heights = c(0.05, 0.95),
    shareX = TRUE, 
    titleY = FALSE
  )
  
  #create a dummy plot to act as a spacer
  spacer_plot <- plot_ly() %>%
    layout(
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
  
  #first stack the spacer and legend vertically
  legend_column <- subplot(
    spacer_plot, legend_plot,
    nrows = 2,
    heights = c(0.45, 0.55)  #adjust this ratio to position the legend lower
  )
  
  combined_plot <- subplot(
    main_plot, legend_column,
    widths = c(0.85, 0.15),
    titleX = TRUE,
    titleY = TRUE
  ) %>% layout(
    title = list(
      text = "Cross-Species Gene Expression Heatmap",
      x = 0.45  
    ),
    plot_bgcolor = plot_bg_color,
    paper_bgcolor = plot_bg_color,
    font = list(color = text_color),
    margin = list(l = 100, r = 100, b = 50, t = 50),
    showlegend = FALSE
  )
  
  return(combined_plot)
}

#prepare heatmap matrix from expression data
prepare_heatmap_matrix <- function(expression_data, normalization = "zscore") {
  #ensure timepoints are properly ordered
  expression_data$Timepoint <- factor(expression_data$Timepoint, levels = TIME_POINTS)
  
  #average replicates
  avg_data <- expression_data %>%
    group_by(Gene, Species, Timepoint) %>%
    summarise(AvgExpression = mean(Expression, na.rm = TRUE), .groups = 'drop')
  
  #create a unique column ID for each species-timepoint combination
  avg_data$SpeciesTimepoint <- paste(avg_data$Species, avg_data$Timepoint, sep = "_")
  
  #create a wide format matrix
  heatmap_matrix <- avg_data %>%
    select(Gene, SpeciesTimepoint, AvgExpression) %>%
    pivot_wider(names_from = SpeciesTimepoint, values_from = AvgExpression)
  
  #convert to matrix format
  genes <- heatmap_matrix$Gene
  hm_matrix <- as.matrix(heatmap_matrix[, -1])
  rownames(hm_matrix) <- genes
  
  #apply normalization if requested
  if (normalization == "zscore") {
    #check for genes with zero variance before normalization
    gene_vars <- apply(hm_matrix, 1, var)
    const_genes <- which(gene_vars == 0)
    
    if (length(const_genes) > 0) {
      showNotification(
        paste("Note:", length(const_genes), "genes have constant expression and will be shown at z-score = 0"),
        type = "info",
        duration = 5
      )
    }
    
    #apply z-score normalization to each gene (row) separately
    hm_matrix <- t(scale(t(hm_matrix)))
    
    #replace any NaN or Inf values with 0 (genes with no variance)
    hm_matrix[!is.finite(hm_matrix)] <- 0
  }
  
  return(hm_matrix)
}

#main function to orchestrate the cross-species heatmap creation
generate_cross_species_heatmap <- function(gene_list, species_data_list, 
                                           normalization = "zscore", 
                                           is_dark_mode = FALSE,
                                           cluster_rows = TRUE,
                                           cluster_cols = FALSE,
                                           config = NULL,
                                           all_species_data = NULL) {
  #clean up gene list - remove empty strings, trim whitespace
  gene_list <- gene_list[gene_list != ""]
  gene_list <- trimws(gene_list)
  
  #map genes across species using HOGs
  gene_mapping <- extract_orthology_for_genes(gene_list, all_species_data, config)
  
  #stop if no genes were mapped
  if (length(gene_mapping) == 0) {
    return(list(
      plot = NULL,
      table = NULL,
      error = "No genes could be mapped across species"
    ))
  }
  
  #extract expression data
  expression_data <- extract_ortholog_expression(gene_mapping, species_data_list, config)
  
  #prepare heatmap matrix
  heatmap_matrix <- prepare_heatmap_matrix(expression_data, normalization)
  
  #create heatmap
  #get dynamic colors for current species configuration
  dynamic_colors <- NULL
  if (!is.null(config)) {
    auto_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
                     "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F",
                     "#BB8FCE", "#85C1E2", "#F8B739", "#52BE80")
    color_index <- 1
    dynamic_colors <- list()
    
    for (sp_code in names(config)) {
      sp_short <- config[[sp_code]]$short
      #check if this species has a predefined color in DEFAULT_SPECIES_COLORS
      if (sp_short %in% names(DEFAULT_SPECIES_COLORS)) {
        dynamic_colors[[sp_short]] <- DEFAULT_SPECIES_COLORS[[sp_short]]
      } else {
        #assign from auto_colors palette
        dynamic_colors[[sp_short]] <- auto_colors[color_index]
        color_index <- ((color_index - 1) %% length(auto_colors)) + 1
      }
    }
  }
  
  heatmap_plot <- create_cross_species_heatmap(
    heatmap_matrix, 
    is_dark_mode = is_dark_mode,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    config = config,
    species_colors_dynamic = dynamic_colors
  )
  
  #create ortholog table
  ortholog_table <- create_ortholog_table(gene_mapping, config)
  
  #return both the plot and table
  return(list(
    plot = heatmap_plot,
    table = ortholog_table,
    matrix = heatmap_matrix,  
    error = NULL
  ))
}