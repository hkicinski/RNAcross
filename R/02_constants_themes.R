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

#default species colors
DEFAULT_SPECIES_COLORS <- list(
  "C. glabrata" = "red",    
  "S. cerevisiae" = "blue",  
  "K. lactis" = "#f8c434",      
  "C. albicans" = "#74ac4c"     
)

SPECIES_COLORS <- DEFAULT_SPECIES_COLORS

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
    return(PALETTE_OKABE_ITO[1:min(n, length(PALETTE_OKABE_ITO))])
  }
  if (palette_name %in% c("viridis", "plasma", "inferno", "magma", "cividis")) {
    return(viridis::viridis(n, option = palette_name))
  }
  if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    return(RColorBrewer::brewer.pal(min(n, max_colors), palette_name))
  }
  return(DYNAMIC_COLOR_PALETTE[1:min(n, length(DYNAMIC_COLOR_PALETTE))])
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
  base_colors <- get_palette_colors(palette_name, n)
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
    if (settings$encoding_multigene_secondary == "linetype" && n_genes <= 6) {
      linetype_values <- setNames(LINETYPES_DEFAULT[1:n_genes], gene_list)
      linetype_var <- "gene"
    } else if (settings$encoding_multigene_secondary == "shape" && n_genes <= 6) {
      shape_values <- setNames(SHAPES_DEFAULT[1:n_genes], gene_list)
      shape_var <- "gene"
    }
  } else {
    color_values <- get_palette_colors(settings$gene_palette, n_genes)
    names(color_values) <- gene_list
    color_var <- "gene"
    if (settings$encoding_multigene_secondary == "linetype") {
      linetype_values <- setNames(LINETYPES_DEFAULT[1:min(n_species, 6)], species_list[1:min(n_species, 6)])
      linetype_var <- "species"
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
    species_palette = "Dark2",
    species_colors = derive_species_colors(species_list, "Dark2", NULL, species_config),
    species_shapes = derive_species_shapes(species_list, NULL, species_config),
    encoding_multigene_color = "species",
    encoding_multigene_secondary = "linetype",
    gene_palette = "Set2",
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
    export_format = "png"
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
      species_palette = "Dark2",
      encoding_multigene_color = "species",
      gene_palette = "Set2",
      heatmap_palette = "viridis",
      ridgeline_palette = "viridis"
    ),
    publication = list(
      name = "Publication",
      species_palette = "Set1",
      encoding_multigene_color = "species",
      gene_palette = "Set1",
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
  
  .footer {
    background-color: var(--bs-primary);
    color: white;
    padding: 20px 0;
    width: 100%;
    flex-shrink: 0;
    margin-top: auto;
    position: fixed;
    bottom: -1px;
    left: 0;
    z-index: 100;
  }

/* Ensure page structure supports footer positioning */
  .bslib-page-navbar {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
    padding-bottom: 80px; /* Add space for fixed footer */
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
    padding-bottom: 100px !important; 
  }
  
  .tab-pane {
    flex: 1;
    padding-bottom: 100px !important; 
    min-height: calc(100vh - 200px);
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
  
  /* Make sure the footer doesn't overlap content */
  .footer {
    position: relative;
    margin-top: 30px;
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
  
  /* Fix for Single Species View dropdown menu */
  .navbar {
    overflow: visible !important;
  }
  
  .navbar-nav {
    overflow: visible !important;
  }
  
  .dropdown-menu {
    position: absolute !important;
    z-index: 1050 !important;
    background-color: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
  }
  
  .navbar .dropdown {
    position: static;
  }
  
  .navbar .dropdown-menu {
    position: absolute !important;
    top: 100%;
    left: auto;
    right: auto;
  }
  
  /* Ensure the page navbar container allows overflow for dropdowns */
  .bslib-page-navbar > .navbar {
    overflow: visible !important;
  }
  
  /* Fix the nav container to allow dropdown visibility */
  .nav-underline {
    overflow: visible !important;
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
    padding-bottom: 120px !important;
    max-width: 1400px;
    margin: 0 auto;
    min-height: calc(100vh - 200px);
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

