#rnacross constants and themes module
#css themes, color palettes, species configuration constants
#dependencies: 01_config

#timepoint constants
TIME_POINTS <- c("0min", "15min", "30min", "45min", "1h", "1.5h", "2h", "2.5h", "3h", "3.5h", "4h", "6h", "8h")

#default species configuration - will be overridden by user uploads
#html fields contain italicized versions for UI display
DEFAULT_SPECIES_CONFIG <- list(
  cg = list(name = "Candida glabrata", short = "C. glabrata", html = "<em>C. glabrata</em>"),
  sc = list(name = "Saccharomyces cerevisiae", short = "S. cerevisiae", html = "<em>S. cerevisiae</em>"),
  kl = list(name = "Kluyveromyces lactis", short = "K. lactis", html = "<em>K. lactis</em>"),
  ca = list(name = "Candida albicans", short = "C. albicans", html = "<em>C. albicans</em>")
)

#' Format species name with italics for HTML contexts
#' @param species_name Species name string
#' @return HTML-formatted italic species name
format_species_italic <- function(species_name) {
  tags$em(species_name)
}

#' Format species name with italics for ggplot titles
#' @param prefix Text before species name
#' @param species_name Species name string
#' @param suffix Text after species name (optional
#' @return Expression for ggplot title
format_species_title <- function(prefix, species_name, suffix = NULL) {
  if (is.null(suffix)) {
    bquote(.(prefix) ~ italic(.(species_name)))
  } else {
    bquote(.(prefix) ~ italic(.(species_name)) ~ .(suffix))
  }
}

#backward compatibility alias
SPECIES_CONFIG <- DEFAULT_SPECIES_CONFIG

#standard species colors (Set1 hues), default for the four-species set
STANDARD_SPECIES_PALETTE <- "RNAcross Standard"

STANDARD_SPECIES_COLORS <- list(
  "S. cerevisiae" = "#377EB8",  #blue
  "C. glabrata"   = "#E41A1C",  #red
  "C. albicans"   = "#4DAF4A",  #green
  "K. lactis"     = "#FF7F00",  #orange
  "Saccharomyces cerevisiae" = "#377EB8",
  "Candida glabrata"         = "#E41A1C",
  "Candida albicans"         = "#4DAF4A",
  "Kluyveromyces lactis"     = "#FF7F00"
)

#default species colors
DEFAULT_SPECIES_COLORS <- STANDARD_SPECIES_COLORS

SPECIES_COLORS <- DEFAULT_SPECIES_COLORS

#tree aesthetics; NULL means follow the light/dark theme default
DEFAULT_TREE_AES <- list(
  title_show = TRUE,
  title_text = NULL,
  title_size = 14,
  title_color = NULL,
  title_bold = TRUE,
  tip_size = NULL,
  tip_align = TRUE,
  tip_color_mode = "species",   # "species" or "single"
  tip_color = "#000000",
  label_space = 1,
  branch_color = NULL,
  branch_width = 0.5,
  node_show = TRUE,
  node_color = NULL,
  node_size = 2,
  legend_show = TRUE,
  legend_position = "bottom",
  legend_title = "Species",
  legend_text_size = 10,
  legend_title_size = 11,
  legend_text_color = NULL,
  bg_color = NULL
)

#additional colors for dynamic species assignment
DYNAMIC_COLOR_PALETTE <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
                           "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F",
                           "#BB8FCE", "#85C1E2", "#F8B739", "#52BE80")

#qualitative palettes for categorical data
PALETTES_QUALITATIVE <- c("Dark2", "Set1", "Set2", "Paired", "Accent", "Pastel1")

#sequential palettes for continuous data
PALETTES_SEQUENTIAL <- c("viridis", "plasma", "inferno", "magma", "cividis", "Blues", "Greens", "YlOrRd")

#diverging palettes for centered data
PALETTES_DIVERGING <- c("RdBu", "RdYlBu", "PiYG", "PRGn", "BrBG", "Spectral")

#default shape cycle (pch values)
SHAPES_DEFAULT <- c(16L, 17L, 15L, 18L, 8L, 3L)

#default linetype cycle
LINETYPES_DEFAULT <- c("solid", "dashed", "dotted", "longdash", "twodash", "dotdash")

#colorblind-safe palette (okabe-ito)
PALETTE_OKABE_ITO <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' Get n colors from a named palette
#'
#' Returns colors from various palette sources including Okabe-Ito,
#' viridis variants, and RColorBrewer palettes.
#'
#' @param palette_name Character name of the palette
#' @param n Number of colors needed
#' @return Character vector of hex color codes
get_palette_colors <- function(palette_name, n) {
  if (palette_name == "Okabe-Ito") {
    cols <- PALETTE_OKABE_ITO
    if (n > length(cols)) {
      return(colorRampPalette(cols)(n))
    }
    return(cols[1:n])
  }
  if (palette_name %in% c("viridis", "plasma", "inferno", "magma", "cividis")) {
    return(viridis::viridis(n, option = palette_name))
  }
  if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    if (n <= max_colors) {
      return(RColorBrewer::brewer.pal(max(n, 3), palette_name)[1:n]) # min 3 for brewer.pal
    } else {
      # Interpolate if we need more colors than valid for this palette
      base_colors <- RColorBrewer::brewer.pal(max_colors, palette_name)
      return(colorRampPalette(base_colors)(n))
    }
  }
  # Fallback dynamic palette
  cols <- DYNAMIC_COLOR_PALETTE
  if (n > length(cols)) {
    return(colorRampPalette(cols)(n))
  }
  return(cols[1:n])
}

#' Derive species colors from palette
#'
#' Stores colors keyed by short name AND full name for flexible lookup.
#'
#' @param species_list Character vector of species short names
#' @param palette_name Name of color palette to use
#' @param unused Unused parameter (kept for API compatibility)
#' @param config Optional species config for name mapping
#' @return Named list of colors
derive_species_colors <- function(species_list, palette_name, unused = NULL, config = NULL) {
  n <- length(species_list)
  if (identical(palette_name, STANDARD_SPECIES_PALETTE)) {
    #canonical hue per known species; unknown ones take unclaimed palette colors
    spare <- get_palette_colors("Dark2", max(n, 3))
    spare <- setdiff(spare, unlist(STANDARD_SPECIES_COLORS))
    base_colors <- character(n)
    for (i in seq_len(n)) {
      sp <- species_list[[i]]
      std <- STANDARD_SPECIES_COLORS[[sp]]
      if (is.null(std) && !is.null(config)) {
        #try the other naming form (short vs full binomial)
        for (sp_code in names(config)) {
          if (identical(config[[sp_code]]$short, sp) || identical(config[[sp_code]]$name, sp)) {
            std <- STANDARD_SPECIES_COLORS[[config[[sp_code]]$short]] %||%
                   STANDARD_SPECIES_COLORS[[config[[sp_code]]$name]]
            break
          }
        }
      }
      if (!is.null(std)) {
        base_colors[i] <- std
      } else {
        base_colors[i] <- if (length(spare) > 0) spare[[1]] else "#808080"
        if (length(spare) > 0) spare <- spare[-1]
      }
    }
  } else {
    base_colors <- get_palette_colors(palette_name, n)
  }
  result <- as.list(setNames(base_colors, species_list))
  if (!is.null(config)) {
    for (sp_code in names(config)) {
      sp_short <- config[[sp_code]]$short
      sp_full <- config[[sp_code]]$name
      if (sp_short %in% names(result)) {
        result[[sp_full]] <- result[[sp_short]]
      }
    }
  }
  result
}

#' Derive species shapes from defaults
#'
#' Stores shapes keyed by short name AND full name for flexible lookup.
#'
#' @param species_list Character vector of species short names
#' @param unused Unused parameter (kept for API compatibility)
#' @param config Optional species config for name mapping
#' @return Named list of integer shape values
derive_species_shapes <- function(species_list, unused = NULL, config = NULL) {
  n <- length(species_list)
  base_shapes <- SHAPES_DEFAULT[((seq_len(n) - 1) %% length(SHAPES_DEFAULT)) + 1]
  result <- as.list(setNames(as.integer(base_shapes), species_list))
  if (!is.null(config)) {
    for (sp_code in names(config)) {
      sp_short <- config[[sp_code]]$short
      sp_full <- config[[sp_code]]$name
      if (sp_short %in% names(result)) {
        result[[sp_full]] <- result[[sp_short]]
      }
    }
  }
  result
}

#' Resolve species color from any name format
#'
#' @param species_name Species name (short or full)
#' @param settings_colors Named list of species colors
#' @param fallback Default color if not found
#' @return Hex color string
resolve_species_color <- function(species_name, settings_colors, fallback = "#808080") {
  if (species_name %in% names(settings_colors)) {
    return(settings_colors[[species_name]])
  }
  fallback
}

#' Resolve species shape from any name format
#'
#' @param species_name Species name (short or full)
#' @param settings_shapes Named list of species shapes
#' @param fallback Default shape if not found
#' @return Integer pch value
resolve_species_shape <- function(species_name, settings_shapes, fallback = 16L) {
  if (species_name %in% names(settings_shapes)) {
    return(as.integer(settings_shapes[[species_name]]))
  }
  as.integer(fallback)
}

#' Get aesthetic mappings for multi-gene plots
#'
#' Determines color, shape, and linetype mappings based on encoding settings.
#'
#' @param settings Plot settings list
#' @param species_list Vector of species names
#' @param gene_list Vector of gene names
#' @return List with color_values, color_var, linetype_values, linetype_var, shape_values, shape_var
get_multigene_aesthetics <- function(settings, species_list, gene_list) {
  n_species <- length(species_list)
  n_genes <- length(gene_list)
  
  shape_values <- NULL
  shape_var <- NULL
  linetype_values <- NULL
  linetype_var <- NULL
  
  if (settings$encoding_multigene_color == "species") {
    color_values <- unlist(settings$species_colors[species_list])
    color_var <- "species"
    
    # Secondary encoding logic for species color mode
    if (n_genes <= 12) { # increased limit slightly
      if (settings$encoding_multigene_secondary %in% c("linetype", "both")) {
        linetype_values <- setNames(LINETYPES_DEFAULT[((seq_along(gene_list)-1) %% length(LINETYPES_DEFAULT)) + 1], gene_list)
        linetype_var <- "gene"
      }
      if (settings$encoding_multigene_secondary %in% c("shape", "both")) {
        shape_values <- setNames(SHAPES_DEFAULT[((seq_along(gene_list)-1) %% length(SHAPES_DEFAULT)) + 1], gene_list)
        shape_var <- "gene"
      }
    }
  } else {
    # Color by Gene
    # Check if custom gene colors exist, otherwise generate from palette
    if (!is.null(settings$gene_colors)) {
      # use custom colors if available, falling back to palette-generated
      color_values <- sapply(gene_list, function(g) {
        if (g %in% names(settings$gene_colors)) settings$gene_colors[[g]] else "#000000"
      })
      # if any missing (black), maybe fallback to palette
      if (any(color_values == "#000000")) {
        defaults <- get_palette_colors(settings$gene_palette, n_genes)
        names(defaults) <- gene_list
        missing <- color_values == "#000000"
        color_values[missing] <- defaults[missing]
      }
    } else {
      color_values <- get_palette_colors(settings$gene_palette, n_genes)
      names(color_values) <- gene_list
    }
    color_var <- "gene"
    
    # Secondary encoding logic for gene color mode
    if (n_species <= 12) {
      if (settings$encoding_multigene_secondary %in% c("linetype", "both")) {
        linetype_values <- setNames(LINETYPES_DEFAULT[((seq_along(species_list)-1) %% length(LINETYPES_DEFAULT)) + 1], species_list)
        linetype_var <- "species"
      }
      # Shape is usually not ideal for lines, but useful for points
      if (settings$encoding_multigene_secondary %in% c("shape", "both")) {
        # Shape by species
        # Use species_shapes from settings if available
        if (!is.null(settings$species_shapes)) {
          shape_values <- unlist(settings$species_shapes[species_list])
        } else {
           shape_values <- setNames(SHAPES_DEFAULT[((seq_along(species_list)-1) %% length(SHAPES_DEFAULT)) + 1], species_list)
        }
        shape_var <- "species"
      }
    }
  }
  
  list(
    color_values = color_values,
    color_var = color_var,
    linetype_values = linetype_values,
    linetype_var = linetype_var,
    shape_values = shape_values,
    shape_var = shape_var
  )
}

#' Merge global settings with local overrides
#'
#' @param global Global settings list
#' @param local Local override settings list
#' @return Merged settings list
merge_plot_settings <- function(global, local) {
  if (is.null(local) || isTRUE(local$use_global)) {
    return(global)
  }
  merged <- global
  for (key in names(local)) {
    if (!is.null(local[[key]]) && key != "use_global") {
      merged[[key]] <- local[[key]]
    }
  }
  merged
}

#' Generate default settings for a species configuration
#'
#' Creates a complete settings list with defaults for all plot types.
#'
#' @param species_config Species configuration list
#' @return List of default settings
generate_default_settings <- function(species_config) {
  species_list <- sapply(species_config, function(x) x$short)
  list(
    species_palette = STANDARD_SPECIES_PALETTE,
    species_colors = derive_species_colors(species_list, STANDARD_SPECIES_PALETTE, NULL, species_config),
    species_shapes = derive_species_shapes(species_list, NULL, species_config),
    encoding_multigene_color = "species",
    encoding_multigene_secondary = "linetype",
    gene_palette = "Set2",
    encoding_similarity_color = "species",
    encoding_similarity_secondary = "linetype",
    similarity_palette = "Dark2",
    heatmap_palette = "viridis",
    heatmap_scale_type = "sequential",
    heatmap_midpoint = "auto",
    heatmap_show_row_dendro = TRUE,
    heatmap_show_col_dendro = TRUE,
    heatmap_row_annotation = TRUE,
    ridgeline_palette = "viridis",
    ridgeline_alpha = 0.8,
    encoding_pca_color = "species",
    encoding_pca_shape = "species",
    pca_alpha = 0.8,
    pca_point_size = 3,
    pca_show_ellipses = TRUE,
    pca_show_loadings = FALSE,
    export_width = 8,
    export_height = 6,
    export_dpi = 300,
    export_format = "png",
    sc_dataset = "2023"
  )
}

#' Get bundled presets
#'
#' Returns predefined settings presets for common use cases.
#'
#' @return Named list of preset configurations
get_bundled_presets <- function() {
  list(
    default = list(
      name = "Default",
      species_palette = STANDARD_SPECIES_PALETTE,
      encoding_multigene_color = "species",
      gene_palette = "Set2",
      encoding_similarity_color = "species",
      encoding_similarity_secondary = "linetype",
      similarity_palette = "Dark2",
      heatmap_palette = "viridis",
      ridgeline_palette = "viridis"
    ),
    publication = list(
      name = "Publication",
      species_palette = "Set1",
      encoding_multigene_color = "species",
      gene_palette = "Set1",
      encoding_similarity_color = "species",
      encoding_similarity_secondary = "linetype",
      similarity_palette = "Set1",
      heatmap_palette = "RdBu",
      heatmap_scale_type = "diverging",
      ridgeline_palette = "plasma",
      export_dpi = 300,
      export_format = "pdf"
    ),
    colorblind = list(
      name = "Colorblind Safe",
      species_palette = "Okabe-Ito",
      encoding_multigene_color = "species",
      gene_palette = "Okabe-Ito",
      encoding_similarity_color = "species",
      encoding_similarity_secondary = "linetype",
      similarity_palette = "Okabe-Ito",
      heatmap_palette = "cividis",
      ridgeline_palette = "cividis"
    ),
    gene_focused = list(
      name = "Gene Focused",
      species_palette = "Dark2",
      encoding_multigene_color = "gene",
      encoding_multigene_secondary = "linetype",
      gene_palette = "Set1",
      heatmap_palette = "viridis",
      ridgeline_palette = "viridis"
    )
  )
}

#the nine tools. value must match the nav_panel value in R/10 or nav_select does nothing.
#desc is copy from the 2b prototype, used verbatim on the launchpad tiles.
RNX_TOOLS <- list(
  list(label = "Gene Explorer", chip = "Gene Explorer", value = "gene_explorer",
       group = "Explore", icon = "fas fa-dna",
       desc = "Orthogroup, phylogeny, species coverage",
       syn = "gene search query hub orthogroup ortholog tree phylogeny hog"),
  list(label = "Find similar profiles", chip = "Similar profiles", value = "similarity_search",
       group = "Explore", icon = "fas fa-chart-line",
       desc = "Matching temporal shapes, Pearson R + perm p",
       syn = "similarity correlation trajectory profile match neighbours"),
  list(label = "Ridgeline Plots", chip = "Ridgelines", value = "Ridgeline Plots",
       group = "Explore", icon = "fas fa-water",
       desc = "Distributions per timepoint, or counts over a threshold",
       syn = "ridge joyplot density distribution threshold"),
  list(label = "Single Species View", chip = "Single species", value = "species_analysis_container",
       group = "Compare", icon = "fas fa-chart-column",
       desc = "One species at a time, paralogs selectable",
       syn = "one species per species individual expression plot"),
  list(label = "Comparative View", chip = "Comparative", value = "Comparative View",
       group = "Compare", icon = "fas fa-layer-group",
       desc = "Overlay orthologs, optionally normalised to 0 min",
       syn = "combined overlay cross species multi species paralog"),
  list(label = "Cross-Species Heatmap", chip = "Heatmap", value = "Cross-Species Heatmap",
       group = "Compare", icon = "fas fa-table-cells",
       desc = "Gene set \u00d7 timepoints, z-scored by gene",
       syn = "heatmap matrix ortholog grid zscore"),
  list(label = "Gene Group Analysis", chip = "Gene groups", value = "Gene Group Analysis",
       group = "Analyze", icon = "fas fa-list-check",
       desc = "Lists or pathways \u00b7 line, bar, heatmap",
       syn = "pathway gene set cluster group list bar line"),
  list(label = "PCA", chip = "PCA", value = "PCA",
       group = "Analyze", icon = "fas fa-braille",
       desc = "Single or multi-species, HOG aggregation choice",
       syn = "principal component ordination dimension reduction variance"),
  list(label = "Data Upload", chip = "Upload", value = "data_upload",
       group = "Data", icon = "fas fa-upload",
       desc = "Your own species, matrices, metadata, orthology",
       syn = "import upload your own data design wizard orthofinder validate")
)

#launchpad/resume chips. checked against gene_lookup at startup so an upload
#never shows chips that go nowhere; see rnx_example_genes() in R/05.
EXAMPLE_GENES <- c("PHO4", "PHO5", "PHO80", "PHO81", "PHO84", "PHO85", "PHO86", "INO1", "SOD1")

#order the palette lists groups in when nothing is typed
RNX_TOOL_GROUPS <- c("Explore", "Compare", "Analyze", "Data")

#' Nav value to tool name, for the search pill and the launchpad
#' @param value Character nav_panel value
#' @return Character tool label, or the value itself when unknown
rnx_tool_label <- function(value) {
  if (is.null(value) || !nzchar(value)) return("")
  for (tool in RNX_TOOLS) {
    if (identical(tool$value, value)) return(tool$label)
  }
  value
}

#bslib theme configurations
light_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",
  secondary = "#95a5a6",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  bg = "#ffffff",
  fg = "#333333"
)

dark_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#375a7f",
  secondary = "#444444",
  success = "#00bc8c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  bg = "#222222",
  fg = "#ffffff"
)

#loading screen for waiter
loading_screen <- tagList(
  spin_flower(),
  h3("Processing data...", style = "color: #2C3E50; margin-top: 15px;")
)

#custom css styles
custom_css <- tags$style(HTML("
  /* Base styles */
  .nav-tabs .nav-link.active {
    background-color: var(--bs-primary) !important;
    color: white !important;
    border-color: var(--bs-primary) !important;
  }
  
  .sidebar-panel {
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .results-panel {
    border-radius: 8px;
    padding: 20px;
    padding-bottom: 100px; 
    margin-bottofm: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    overflow: visible;
  }
    /* Make the main flex container */
  .bslib-page-navbar {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0; 
  }
  .custom-button {
    width: 100%;
    padding: 10px;
    margin-top: 10px;
    background-color: var(--bs-primary);
    border: none;
    color: white;
    border-radius: 4px;
    transition: all 0.3s ease;
  }
  
  .custom-button:hover {
    background-color: #34495E;
    transform: translateY(-2px);
  }
  
  .gene-info {
    padding: 15px;
    border-radius: 4px;
    margin-top: 15px;
    font-family: monospace;
  }
  
    html {
    height: 100%;
    overflow-y: scroll;  
  }
  
  body {
    min-height: 100%;
    margin-bottom: 0 !important;
    padding-bottom: 0 !important;
  }
  
  /* Adjust the main container to fill space */
  .bslib-page-navbar {
    min-height: 100vh;
    margin-bottom: 0 !important;
  }
  
  /* footer flows after the content; it is not fixed, so nothing has to reserve space for it */
  .footer {
    background-color: var(--bs-primary);
    color: white;
    padding: 20px 0;
    width: 100%;
    flex-shrink: 0;
    position: relative;
    margin-top: 30px;
    left: 0;
    z-index: 100;
  }

  .bslib-page-navbar {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }

  /* Make main content flexible */
  .main-content {
    flex: 1;
    display: flex;
    flex-direction: column;
  }
  
  /* Ensure tab content takes available space */
  .tab-content {
    flex: 1;
    display: flex;
    flex-direction: column;
  }

  /* 60px = the bar, which is now the whole chrome above a tab */
  .tab-pane {
    flex: 1;
    padding-bottom: 24px;
    min-height: calc(100vh - 60px);
  }

.modal-header {
  background-color: var(--bs-primary);
  color: white;
  border-radius: 8px 8px 0 0;
}

.modal-body {
  padding: 20px;
}

.modal-body h4 {
  color: var(--bs-primary);
  margin-top: 20px;
  margin-bottom: 10px;
}

.modal-body ul {
  margin-bottom: 15px;
}

.modal-body li {
  margin-bottom: 8px;
}

/* Dark mode support */
[data-bs-theme='dark'] .modal-content {
  background-color: #2c3034;
  color: #ffffff;
}

[data-bs-theme='dark'] .modal-header {
  border-bottom-color: #444;
}

[data-bs-theme='dark'] .modal-footer {
  border-top-color: #444;
}
  
  /* Dark mode theme */
  [data-bs-theme='dark'] {
    --bs-body-bg: #222222;
    --bs-body-color: #ffffff;
  }

  [data-bs-theme='light'] {
    --bs-body-bg: #ffffff;
    --bs-body-color: #212529;
  }
  
  /* Dark mode panels and containers */
  .dark-mode {
    background-color: #222222 !important;
    color: #ffffff !important;
  }
  
  .dark-mode .sidebar-panel,
  .dark-mode .results-panel {
    background-color: #2c3034 !important;
    color: #ffffff !important;
    border: 1px solid #444;
  }
  
  .dark-mode .gene-info {
    background-color: #2c3034 !important;
    color: #ffffff !important;
  }

  .dark-mode .gene-info pre {
    background-color: #2c3034 !important;
    color: #ffffff !important;
    border: none !important;
  }
  
  /* Dark mode plot styles */
  .dark-mode .plotly .main-svg {
    background-color: #2c3034 !important;
  }
  
  .dark-mode .plotly .bg {
    fill: #2c3034 !important;
  }
  
  .dark-mode .js-plotly-plot .plotly .modebar {
    background: #2c3034 !important;
    color: #ffffff !important;
  }
  
  /* Dark mode table styles */
  .dark-mode .dataTables_wrapper {
    color: #ffffff !important;
    background-color: #2c3034 !important;
  }
  
  .dark-mode .dataTable {
    background-color: #2c3034 !important;
    color: #ffffff !important;
  }
  
  .dark-mode .dataTable th,
  .dark-mode .dataTable td {
    background-color: #2c3034 !important;
    color: #ffffff !important;
    border-color: #444 !important;
  }
  
  .dark-mode .dataTables_info,
  .dark-mode .dataTables_length,
  .dark-mode .dataTables_filter,
  .dark-mode .dataTables_paginate {
    color: #ffffff !important;
  }
  
  /* Dark mode form controls */
  .dark-mode input,
  .dark-mode select,
  .dark-mode textarea {
    background-color: #2c3034 !important;
    color: #ffffff !important;
    border-color: #444 !important;
  }
  
  .dark-mode input::placeholder {
    color: #888888 !important;
  }
  
  .dark-mode .checkbox label {
    color: #ffffff !important;
  }
  
  /* Dark mode navigation */
  .dark-mode .nav-tabs {
    border-color: #444 !important;
  }
  
  .dark-mode .nav-tabs .nav-link {
    color: #ffffff !important;
  }
  
  .dark-mode .nav-tabs .nav-link.active {
    background-color: #2c3034 !important;
    border-color: #444 !important;
    color: #ffffff !important;
  }
  .title, .author, .date {
  display: none !important;
  }
  /* Fix for scroll bars and layout issues */
  html, body {
    margin: 0;
    padding: 0;
    height: 100%;
    overflow-x: hidden;
  }
  
  .container-fluid {
    padding: 0;
    margin: 0;
    width: 100%;
  }
  
  /* Fix for panel heights */
  .sidebar-panel {
    height: auto;
    overflow-y: auto;
    max-height: calc(100vh - 200px);
    padding-bottom: 20px;
  }
  
  /* Ensure the results panel expands properly */
  .results-panel {
    min-height: auto;
    overflow: visible;
    margin-bottom: 20px;
  } 
  
  /* Prevent horizontal scrolling */
  .row {
    margin-left: 0;
    margin-right: 0;
    width: 100%;
  }
  
  /* Combined view selection improvements */
  #combined_orthogroup_container .orthogroup-species {
    background-color: #f8f9fa;
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 15px;
    border: 1px solid #dee2e6;
  }
  
  [data-bs-theme='dark'] #combined_orthogroup_container .orthogroup-species {
    background-color: #2c3034;
    border-color: #444;
  }
  
  #combined_orthogroup_container .radio label {
    margin-left: 5px;
    font-size: 14px;
  }
  
  #combined_orthogroup_container h6 {
    margin-bottom: 10px;
    font-weight: 600;
    color: var(--bs-primary);
  }
  
  #combined_orthogroup_container .alert {
    font-size: 13px;
    padding: 8px 12px;
  }
  
  /* Improve radio button spacing */
  #combined_orthogroup_container .radio {
    margin-bottom: 8px;
  }
  
  #combined_orthogroup_container .radio input[type='radio'] {
    margin-right: 5px;
  }
  
  /* the .navbar-scoped overflow fixes went with the tab strip; these are the generic ones */
  .dropdown-menu {
    position: absolute !important;
    z-index: 1050 !important;
    background-color: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
  }

  /* Ensure dropdown items are visible */
  .dropdown-item {
    color: var(--bs-body-color);
  }
  
  .dropdown-item:hover {
    background-color: var(--bs-primary);
    color: white;
  }
  /* Gene Explorer Styles */
  .gene-explorer-container {
    padding: 20px;
  }
  
  .query-panel {
    background-color: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .tree-panel {
    background-color: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
    border-radius: 8px;
    padding: 20px;
    min-height: 400px;
  }
  
  .orthogroup-summary {
    background-color: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
    border-radius: 8px;
    padding: 20px;
  }
  
  .query-status {
    margin-top: 15px;
    padding: 10px;
    border-radius: 4px;
    background-color: var(--bs-light);
  }
  
  [data-bs-theme='dark'] .query-status {
    background-color: #2c3034;
  }
  
  .tree-legend {
    margin-top: 20px;
    padding: 15px;
    background-color: #f8f9fa;
    border-radius: 4px;
  }
  
  [data-bs-theme='dark'] .tree-legend {
    background-color: #2c3034;
  }
  
    /* Animated DNA to Chart Icon */
  .icon-morph-container {
    display: inline-block;
    position: relative;
    width: 20px;
    height: 20px;
    margin-right: 5px;
  }
  
  .icon-morph-container .fa-dna,
  .icon-morph-container .fa-chart-line {
    position: absolute;
    top: 0;
    left: 0;
    transition: all 0.8s ease-in-out;
  }
  
  .icon-morph-container .fa-dna {
    animation: dna-morph 4s infinite;
  }
  
  .icon-morph-container .fa-chart-line {
    animation: chart-morph 4s infinite;
  }
  
  @keyframes dna-morph {
    0%, 20% {
      opacity: 1;
      transform: rotate(0deg) scale(1);
    }
    40%, 60% {
      opacity: 0;
      transform: rotate(180deg) scale(0.5);
    }
    80%, 100% {
      opacity: 1;
      transform: rotate(360deg) scale(1);
    }
  }
  
  @keyframes chart-morph {
    0%, 20% {
      opacity: 0;
      transform: translateY(10px) scale(0.5);
    }
    40%, 60% {
      opacity: 1;
      transform: translateY(0) scale(1);
    }
    80%, 100% {
      opacity: 0;
      transform: translateY(-10px) scale(0.5);
    }
  }
  
  /* Hover effect to pause animation */
  .icon-morph-container:hover .fa-dna,
  .icon-morph-container:hover .fa-chart-line {
    animation-play-state: paused;
  }
  
    /* Gene Explorer specific styles */
   .gene-explorer-container {
    padding: 20px;
    padding-bottom: 40px !important;
    max-width: 1400px;
    margin: 0 auto;
    min-height: calc(100vh - 60px);
  }
  
  /* Cross-Species Ortholog Analysis styles */
  .ortholog-analysis-panel {
    background-color: var(--bs-body-bg);
    border: 2px solid var(--bs-primary);
    border-radius: 8px;
    padding: 15px;
    margin: 15px 0;
  }
  
  [data-bs-theme='dark'] .ortholog-analysis-panel {
    background-color: #2c3034;
    border-color: #375a7f;
  }
  
  .coverage-badge {
    display: inline-block;
    padding: 4px 8px;
    margin: 2px;
    border-radius: 4px;
    font-size: 0.85em;
    font-weight: 500;
  }
  
  .coverage-badge.high {
    background-color: #d4edda;
    color: #155724;
  }
  
  .coverage-badge.medium {
    background-color: #fff3cd;
    color: #856404;
  }
  
  .coverage-badge.low {
    background-color: #f8d7da;
    color: #721c24;
  }
  
  [data-bs-theme='dark'] .coverage-badge.high {
    background-color: #1e4620;
    color: #a3d9a5;
  }
  
  [data-bs-theme='dark'] .coverage-badge.medium {
    background-color: #664d03;
    color: #ffc107;
  }
  
  [data-bs-theme='dark'] .coverage-badge.low {
    background-color: #58151c;
    color: #f1aeb5;
  }
  
  .ortholog-summary-box {
    background-color: #f8f9fa;
    border-left: 4px solid var(--bs-primary);
    padding: 10px;
    margin: 10px 0;
    border-radius: 4px;
  }
  
  [data-bs-theme='dark'] .ortholog-summary-box {
    background-color: #2c3034;
  }
  
  .query-panel {
    background: linear-gradient(135deg, var(--bs-primary) 0%, var(--bs-info) 100%);
    color: white;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  
  .query-panel h3 {
    color: white;
    margin-bottom: 20px;
  }
  
  .query-panel p {
    color: rgba(255,255,255,0.9);
    margin-bottom: 20px;
  }
  
  .query-panel input {
    font-size: 16px;
    padding: 10px;
  }
  
  .tree-panel {
    overflow: hidden;
  }
  
  .orthogroup-summary h5 {
    color: var(--bs-primary);
    margin-bottom: 15px;
    font-weight: 600;
  }
  
  .orthogroup-summary ul {
    padding-left: 20px;
  }
  
  .orthogroup-summary li {
    margin-bottom: 8px;
  }
  
  /* Dark mode adjustments */
  [data-bs-theme='dark'] .query-panel {
    background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
  }
  
  [data-bs-theme='dark'] .tree-panel,
  [data-bs-theme='dark'] .orthogroup-summary {
    background-color: #2c3034;
    border-color: #444;
  }
  /* Data Upload Panel Styles */
  .upload-step {
    background-color: var(--bs-body-bg);
    padding: 15px;
    border-radius: 8px;
    margin-bottom: 15px;
    border: 1px solid var(--bs-border-color);
  }
  
  .upload-step h5 {
    color: var(--bs-primary);
    margin-bottom: 10px;
    font-weight: 600;
  }
  
  .upload-step p {
    font-size: 0.9em;
    color: var(--bs-secondary);
    margin-bottom: 10px;
  }
  
  [data-bs-theme='dark'] .upload-step {
    background-color: #2c3034;
    border-color: #444;
  }
  
  #upload_status_banner {
    position: sticky;
    top: 0;
    z-index: 1000;
    margin-bottom: 20px;
  }
  
  /* Species name italicization - select dropdown options only */
  #ridgeline_species option:not([value='all']),
  #pca_species option,
  #group_analysis_species option {
    font-style: italic;
  }
  
  /* Utility class for italic species names */
  .species-name-italic {
    font-style: italic;
  }
"))

#design 1c + revision 2: the one-row bar and the Gene Explorer landing states.
#kept apart from custom_css so the whole redesign is one block to read or revert.
rnx_nav_css <- tags$style(HTML("
  /* the nav_panels all still exist, only the rendered tab strip is gone.
     bslib puts .bslib-page-navbar on <body>, so the nav is a body child. */
  .bslib-page-navbar > nav.navbar,
  body > nav.navbar.navbar-static-top { display: none !important; }

  /* design tokens. everything below reads these so dark mode is one block.
     .rnx-dark is set on <html> by command_palette.js: the theme_toggle observer
     swaps the bootswatch but never marks the document, and the .dark-mode class
     it tries to add goes to a #html that does not exist. */
  :root {
    --rnx-surface: #FFFFFF;
    --rnx-surface-2: #FAFBFC;
    --rnx-canvas: #F6F7F8;
    --rnx-hairline: #EDF0F2;
    --rnx-hairline-2: #F0F2F4;
    --rnx-panel-border: #E3E7EA;
    --rnx-border-strong: #DCE1E5;
    --rnx-text: #16212B;
    --rnx-text-muted: #6B7A87;
    --rnx-text-faint: #8C9AA6;
    --rnx-chip-bg: #16212B;
    --rnx-accent-wash: #F2FBF8;
    --rnx-badge-bg: #F2F4F6;
    --rnx-tile-icon-bg: #EDF0F2;
    --rnx-tile-icon-fg: #2C3E50;
  }

  html.rnx-dark,
  .dark-mode,
  [data-bs-theme='dark'] {
    --rnx-surface: #2c3034;
    --rnx-surface-2: #26292d;
    --rnx-canvas: #1e2226;
    --rnx-hairline: #444;
    --rnx-hairline-2: #3a3f44;
    --rnx-panel-border: #444;
    --rnx-border-strong: #444;
    --rnx-text: #FFFFFF;
    --rnx-text-muted: #A4AEB7;
    --rnx-text-faint: #94A3AE;
    --rnx-chip-bg: #0E1720;
    --rnx-accent-wash: #1C3B35;
    --rnx-badge-bg: #3A3F44;
    --rnx-tile-icon-bg: #3A3F44;
    --rnx-tile-icon-fg: #cfd6dc;
  }

  /* ---------- the bar: one 60px row, context folded in ---------- */
  .rnx-bar {
    display: flex;
    align-items: center;
    gap: 14px;
    height: 60px;
    padding: 0 20px;
    background: #16212B;
    color: #fff;
  }

  .rnx-brand {
    display: flex;
    align-items: center;
    gap: 9px;
    flex: none;
  }

  .rnx-brand i { font-size: 17px; color: #18BC9C; }

  .rnx-brand span {
    font-size: 16.5px;
    font-weight: 900;
    letter-spacing: -.2px;
    color: #fff;
  }

  /* flex:1 with no max-width is what closes the void on wide screens */
  .rnx-pill {
    display: flex;
    align-items: center;
    gap: 10px;
    flex: 1;
    min-width: 0;
    height: 36px;
    padding: 0 14px;
    border: 1px solid rgba(255,255,255,.16);
    border-radius: 8px;
    background: rgba(255,255,255,.1);
  }

  .rnx-pill:hover { background: rgba(255,255,255,.15); }
  .rnx-pill:focus-within { outline: 2px solid #18BC9C; outline-offset: 1px; }

  /* the search half of the pill; the clear button sits beside it, not inside,
     so we never nest a button in a button */
  .rnx-trigger {
    display: flex;
    align-items: center;
    gap: 10px;
    flex: 1;
    min-width: 0;
    height: 100%;
    padding: 0;
    border: none;
    background: none;
    color: #fff;
    font-family: inherit;
    text-align: left;
    cursor: pointer;
  }

  .rnx-trigger:focus { outline: none; }
  .rnx-trigger > i { font-size: 12px; opacity: .6; flex: none; }

  /* uiOutput wrappers must not break the pill's flex row */
  .rnx-slot { display: contents; }

  .rnx-trigger-label {
    font-size: 13.5px;
    color: rgba(255,255,255,.55);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .rnx-scope-gene {
    font: 700 13.5px ui-monospace, Menlo, monospace;
    color: #fff;
    white-space: nowrap;
  }

  .rnx-scope-hog {
    font: 12px ui-monospace, Menlo, monospace;
    color: rgba(255,255,255,.45);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  /* coverage on navy; the light .coverage-badge palette is unreadable here */
  .rnx-scope-badge {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    padding: 2px 7px;
    border-radius: 5px;
    font-size: 11px;
    font-weight: 700;
    white-space: nowrap;
    flex: none;
  }

  .rnx-scope-badge i { font-size: 9px; }
  .rnx-scope-badge.high { color: #a3d9a5; background: rgba(24,188,156,.16); }
  .rnx-scope-badge.medium { color: #FFE08A; background: rgba(255,224,138,.14); }
  .rnx-scope-badge.low { color: #f1aeb5; background: rgba(231,41,138,.14); }

  .rnx-kbd {
    font: 700 10px ui-monospace, Menlo, monospace;
    padding: 3px 6px;
    border-radius: 4px;
    background: rgba(255,255,255,.14);
    color: rgba(255,255,255,.7);
    flex: none;
  }

  .rnx-scope-clear {
    flex: none;
    padding: 0;
    border: none;
    background: none;
    color: #fff;
    font-size: 11px;
    line-height: 1;
    opacity: .5;
    cursor: pointer;
  }

  .rnx-scope-clear:hover { opacity: 1; }

  .rnx-divider {
    width: 1px;
    height: 26px;
    background: rgba(255,255,255,.14);
    flex: none;
  }

  .rnx-chips { display: flex; align-items: center; gap: 3px; flex: none; }

  .rnx-chip {
    display: flex;
    align-items: center;
    gap: 7px;
    height: 34px;
    padding: 0 12px;
    border: none;
    border-radius: 7px;
    background: transparent;
    color: rgba(255,255,255,.8);
    font-family: inherit;
    font-size: 13px;
    font-weight: 700;
    white-space: nowrap;
    cursor: pointer;
  }

  .rnx-chip i { font-size: 12px; }
  .rnx-chip:hover { background: rgba(255,255,255,.1); color: #fff; }
  .rnx-chip:focus-visible { outline: 2px solid #18BC9C; outline-offset: 1px; }

  .rnx-chip.rnx-chip-active {
    background: rgba(24,188,156,.16);
    color: #18BC9C;
  }

  .rnx-chip.rnx-chip-active:hover { background: rgba(24,188,156,.24); color: #18BC9C; }
  .rnx-chip-all { color: rgba(255,255,255,.6); }

  .rnx-utils { display: flex; align-items: center; gap: 2px; flex: none; }

  /* the four utility buttons keep their old ids, so only the skin changes */
  .rnx-utils .rnx-util-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 34px;
    height: 34px;
    padding: 0;
    border: none;
    border-radius: 7px;
    background: transparent !important;
    color: rgba(255,255,255,.72) !important;
    font-size: 14px;
    line-height: 1;
    box-shadow: none !important;
    text-decoration: none !important;
  }

  .rnx-utils .rnx-util-btn:hover,
  .rnx-utils .rnx-util-btn:focus {
    background: rgba(255,255,255,.1) !important;
    color: #fff !important;
  }

  .rnx-utils .rnx-util-btn:focus-visible { outline: 2px solid #18BC9C; outline-offset: 1px; }

  .rnx-export {
    display: flex;
    align-items: center;
    gap: 7px;
    flex: none;
    height: 34px;
    padding: 0 12px;
    border: 1px solid rgba(255,255,255,.2);
    border-radius: 7px;
    background: transparent;
    color: #fff;
    font-family: inherit;
    font-size: 12.5px;
    font-weight: 700;
    white-space: nowrap;
    cursor: pointer;
  }

  .rnx-export i { font-size: 11px; }
  .rnx-export:hover { border-color: #18BC9C; color: #18BC9C; }

  /* ---------- Gene Explorer landing ---------- */
  .rnx-landing {
    background: var(--rnx-canvas);
    margin: -20px -20px 0;
    padding: 22px 18px 26px;
  }

  .rnx-landing-row { display: flex; gap: 16px; align-items: flex-start; }
  .rnx-landing-main { flex: 1; min-width: 0; }

  .rnx-landing h2 {
    font-size: 20px;
    font-weight: 900;
    letter-spacing: -.3px;
    margin: 0 0 3px;
    color: var(--rnx-text);
  }

  .rnx-landing-sub {
    font-size: 13px;
    color: var(--rnx-text-muted);
    margin-bottom: 16px;
  }

  .rnx-inline-kbd {
    font: 700 11px ui-monospace, Menlo, monospace;
    background: #E9EDF0;
    color: var(--rnx-text);
    padding: 2px 5px;
    border-radius: 4px;
  }

  html.rnx-dark .rnx-inline-kbd { background: #3A3F44; color: #fff; }

  .rnx-tiles {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 10px;
  }

  .rnx-tile {
    display: block;
    background: var(--rnx-surface);
    border: 1px solid var(--rnx-panel-border);
    border-radius: 8px;
    padding: 13px 14px;
    text-decoration: none;
    cursor: pointer;
    text-align: left;
    font-family: inherit;
    width: 100%;
  }

  .rnx-tile:hover {
    border-color: #18BC9C;
    box-shadow: 0 2px 8px -4px rgba(22,33,43,.2);
  }

  .rnx-tile:focus-visible { outline: 2px solid #18BC9C; outline-offset: 1px; }

  .rnx-tile-head {
    display: flex;
    align-items: center;
    gap: 9px;
    margin-bottom: 5px;
  }

  .rnx-tile-icon {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 26px;
    height: 26px;
    border-radius: 7px;
    background: var(--rnx-tile-icon-bg);
    color: var(--rnx-tile-icon-fg);
    font-size: 12px;
    flex: none;
  }

  .rnx-tile.rnx-tile-current .rnx-tile-icon { background: #18BC9C; color: #fff; }

  .rnx-tile-name { font-size: 13px; font-weight: 700; color: var(--rnx-text); }

  .rnx-tile-desc {
    font-size: 11.5px;
    color: var(--rnx-text-muted);
    line-height: 1.45;
  }

  .rnx-side {
    display: flex;
    flex-direction: column;
    gap: 10px;
    flex: none;
    width: 392px;
  }

  .rnx-card {
    background: var(--rnx-surface);
    border: 1px solid var(--rnx-panel-border);
    border-radius: 8px;
    padding: 14px 16px;
  }

  .rnx-card-title {
    font-size: 13px;
    font-weight: 700;
    margin-bottom: 10px;
    color: var(--rnx-text);
  }

  .rnx-recent {
    display: flex;
    align-items: center;
    gap: 9px;
    padding: 7px 0;
    border-bottom: 1px solid var(--rnx-hairline-2);
    text-decoration: none;
    cursor: pointer;
    background: none;
    border-left: none;
    border-right: none;
    border-top: none;
    width: 100%;
    font-family: inherit;
    text-align: left;
  }

  .rnx-recent:last-child { border-bottom: none; }
  .rnx-recent:hover { background: var(--rnx-surface-2); }
  .rnx-recent-gene { font: 700 12.5px ui-monospace, Menlo, monospace; color: var(--rnx-text); }
  .rnx-recent-at { font-size: 11px; color: var(--rnx-text-faint); }

  .rnx-species { width: 100%; border-collapse: collapse; font-size: 12px; }
  .rnx-species td { padding: 4px 0; color: var(--rnx-text); }
  .rnx-species td:first-child { width: 16px; }
  .rnx-species td:last-child { text-align: right; color: var(--rnx-text-muted); }

  .rnx-dot {
    display: inline-block;
    width: 8px;
    height: 8px;
    border-radius: 50%;
  }

  .rnx-card-foot {
    font-size: 11.5px;
    color: var(--rnx-text-faint);
    margin-top: 9px;
    padding-top: 9px;
    border-top: 1px solid var(--rnx-hairline-2);
  }

  .rnx-card-foot a { font-weight: 700; color: #2C3E50; cursor: pointer; }
  html.rnx-dark .rnx-card-foot a { color: #9fd8ff; }

  /* ---------- landing state B: resume ---------- */
  .rnx-landing-resume { padding: 26px 20px 30px; }

  .rnx-resume {
    flex: 1;
    min-width: 0;
    background: var(--rnx-surface);
    border: 1px solid var(--rnx-panel-border);
    border-radius: 10px;
    padding: 20px 22px;
  }

  .rnx-eyebrow {
    font: 700 10px/1 Lato, sans-serif;
    letter-spacing: .13em;
    text-transform: uppercase;
    color: var(--rnx-text-faint);
    margin-bottom: 11px;
  }

  .rnx-resume-head {
    display: flex;
    align-items: center;
    gap: 12px;
    flex-wrap: wrap;
    margin-bottom: 16px;
  }

  .rnx-resume-gene { font: 700 20px ui-monospace, Menlo, monospace; color: var(--rnx-text); }
  .rnx-resume-hog { font: 12.5px ui-monospace, Menlo, monospace; color: var(--rnx-text-muted); }
  .rnx-resume-at { font-size: 11.5px; color: var(--rnx-text-faint); }

  .rnx-actions { display: flex; gap: 8px; flex-wrap: wrap; }

  .rnx-btn {
    display: flex;
    align-items: center;
    gap: 8px;
    height: 36px;
    padding: 0 14px;
    border-radius: 7px;
    font-family: inherit;
    font-size: 13px;
    font-weight: 700;
    cursor: pointer;
  }

  .rnx-btn i { font-size: 12px; }

  .rnx-btn-primary {
    padding: 0 16px;
    border: none;
    background: #16212B;
    color: #fff;
  }

  .rnx-btn-primary:hover { background: #22303C; }

  .rnx-btn-secondary {
    border: 1px solid var(--rnx-border-strong);
    background: var(--rnx-surface);
    color: #2C3E50;
  }

  html.rnx-dark .rnx-btn-secondary { color: #fff; }
  .rnx-btn-secondary:hover { border-color: #18BC9C; color: #18BC9C; }

  .rnx-btn-tertiary {
    border: none;
    background: transparent;
    color: var(--rnx-text-faint);
  }

  .rnx-btn-tertiary:hover { color: var(--rnx-text); }

  .rnx-resume-divider {
    height: 1px;
    background: var(--rnx-hairline-2);
    margin: 18px 0 15px;
  }

  .rnx-try { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
  .rnx-try .rnx-eyebrow { margin-bottom: 0; }
  .rnx-try-chips { display: flex; gap: 6px; flex-wrap: wrap; }

  .rnx-gene-chip-btn {
    font: 700 12.5px ui-monospace, Menlo, monospace;
    color: var(--rnx-text);
    background: var(--rnx-canvas);
    border: 1px solid var(--rnx-panel-border);
    padding: 4px 9px;
    border-radius: 6px;
    text-decoration: none;
    cursor: pointer;
  }

  .rnx-gene-chip-btn:hover { border-color: #18BC9C; color: #18BC9C; }

  .rnx-loaded {
    flex: none;
    width: 420px;
    background: var(--rnx-surface);
    border: 1px solid var(--rnx-panel-border);
    border-radius: 10px;
    padding: 18px 20px;
  }

  .rnx-loaded .rnx-species { font-size: 12.5px; }
  .rnx-loaded .rnx-species td { padding: 5px 0; }

  .rnx-loaded-note {
    font-size: 12px;
    color: var(--rnx-text-muted);
    margin-top: 12px;
    padding-top: 12px;
    border-top: 1px solid var(--rnx-hairline-2);
    line-height: 1.5;
  }

  .rnx-btn-block {
    margin-top: 12px;
    width: 100%;
    height: 34px;
    justify-content: center;
    font-size: 12.5px;
  }

  /* ---------- command palette ---------- */
  .rnx-scrim {
    position: fixed;
    inset: 0;
    z-index: 1090;
    display: flex;
    justify-content: center;
    padding-top: 74px;
    background: rgba(22,33,43,.34);
    opacity: 0;
    visibility: hidden;
    transition: opacity 120ms ease-out, visibility 0s linear 120ms;
  }

  .rnx-scrim.rnx-open {
    opacity: 1;
    visibility: visible;
    transition: opacity 120ms ease-out;
  }

  .rnx-palette-card {
    display: flex;
    flex-direction: column;
    width: 620px;
    max-width: calc(100vw - 32px);
    max-height: calc(100vh - 110px);
    border-radius: 12px;
    background: var(--rnx-surface);
    box-shadow: 0 24px 60px -12px rgba(22,33,43,.5);
    overflow: hidden;
    opacity: 0;
    transform: translateY(-6px) scale(.985);
    transition: opacity 120ms ease-out, transform 140ms cubic-bezier(.2,.8,.2,1);
  }

  .rnx-scrim.rnx-open .rnx-palette-card { opacity: 1; transform: none; }

  .rnx-card-input {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 13px 15px;
    border-bottom: 1px solid var(--rnx-hairline);
    flex: none;
  }

  .rnx-card-input > i { font-size: 12px; color: var(--rnx-text-faint); }

  /* !important beats the blanket .dark-mode input rule in custom_css */
  .rnx-card-input input {
    flex: 1;
    min-width: 0;
    border: none !important;
    outline: none;
    background: none !important;
    padding: 0;
    font-size: 15px;
    font-weight: 700;
    color: var(--rnx-text) !important;
  }

  .rnx-card-input input::placeholder { color: var(--rnx-text-faint); font-weight: 400; }

  .rnx-esc {
    font: 700 9.5px ui-monospace, Menlo, monospace;
    padding: 2px 6px;
    border-radius: 4px;
    background: var(--rnx-badge-bg);
    color: var(--rnx-text-muted);
  }

  .rnx-results {
    padding: 8px 6px 6px;
    overflow-y: auto;
    flex: 1;
  }

  .rnx-section {
    font: 700 9.5px/1 Lato, sans-serif;
    letter-spacing: .13em;
    text-transform: uppercase;
    color: var(--rnx-text-faint);
    padding: 6px 10px 7px;
  }

  .rnx-hairline { height: 1px; margin: 7px 10px; background: var(--rnx-hairline); }

  .rnx-row {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 7px 10px;
    border-radius: 7px;
    cursor: pointer;
  }

  .rnx-row > i { width: 14px; font-size: 12px; color: var(--rnx-text-faint); flex: none; }
  .rnx-row-gene { font: 700 13px ui-monospace, Menlo, monospace; color: var(--rnx-text); }
  .rnx-row-detail { font-size: 12px; color: var(--rnx-text-muted); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .rnx-row-tool { font-size: 13px; font-weight: 700; color: var(--rnx-text); }
  .rnx-row-group { font-size: 11px; color: var(--rnx-text-faint); }
  .rnx-row-hint { font-size: 11px; color: var(--rnx-text-faint); display: none; }

  .rnx-row-enter {
    font: 700 9.5px ui-monospace, Menlo, monospace;
    padding: 2px 6px;
    border-radius: 4px;
    background: var(--rnx-chip-bg);
    color: #fff;
    display: none;
  }

  .rnx-row.rnx-sel { background: var(--rnx-accent-wash); }
  .rnx-row.rnx-sel > i { color: #18BC9C; }
  .rnx-row.rnx-sel .rnx-row-hint,
  .rnx-row.rnx-sel .rnx-row-enter { display: inline; }

  .rnx-empty { padding: 10px; font-size: 13px; color: var(--rnx-text-muted); }

  .rnx-card-footer {
    display: flex;
    align-items: center;
    gap: 14px;
    padding: 9px 15px;
    border-top: 1px solid var(--rnx-hairline);
    background: var(--rnx-surface-2);
    font-size: 11px;
    color: var(--rnx-text-muted);
    flex: none;
  }

  .rnx-card-footer .rnx-fkey {
    font: 700 9.5px ui-monospace, Menlo, monospace;
    padding: 2px 5px;
    margin-right: 5px;
    border-radius: 4px;
    background: var(--rnx-badge-bg);
  }

  .rnx-spacer { flex: 1; }

  /* the coverage badges in the resume card only had light values wired up */
  html.rnx-dark .coverage-badge.high { background-color: #1e4620; color: #a3d9a5; }
  html.rnx-dark .coverage-badge.medium { background-color: #664d03; color: #ffc107; }
  html.rnx-dark .coverage-badge.low { background-color: #58151c; color: #f1aeb5; }

  @media (prefers-reduced-motion: reduce) {
    .rnx-palette-card { transform: none; transition: opacity 120ms ease-out; }
    .rnx-scrim.rnx-open .rnx-palette-card { transform: none; }
  }

  /* below ~1400px only the active chip stays, so the pill keeps its room */
  @media (max-width: 1400px) {
    .rnx-chips .rnx-chip:not(.rnx-chip-active):not(.rnx-chip-all) { display: none; }
  }

  @media (max-width: 1100px) {
    .rnx-landing-row { flex-wrap: wrap; }
    .rnx-side, .rnx-loaded { width: 100%; }
    .rnx-tiles { grid-template-columns: repeat(2, 1fr); }
  }

  @media (max-width: 900px) {
    .rnx-chips .rnx-chip span { display: none; }
    .rnx-chip { padding: 0 9px; }
    .rnx-scope-hog { display: none; }
  }
"))

