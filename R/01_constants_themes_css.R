#constants
TIME_POINTS <- c("0min", "15min", "30min", "45min", "1h", "1.5h", "2h", "2.5h", "3h", "3.5h", "4h", "6h", "8h")

#default species for demo data - will be overridden by user uploads
DEFAULT_SPECIES_CONFIG <- list(
  cg = list(name = "Candida glabrata", short = "C. glabrata"),
  sc = list(name = "Saccharomyces cerevisiae", short = "S. cerevisiae"),
  kl = list(name = "Kluyveromyces lactis", short = "K. lactis"),
  ca = list(name = "Candida albicans", short = "C. albicans")
)

#keep SPECIES_CONFIG for backward compatibility 
SPECIES_CONFIG <- DEFAULT_SPECIES_CONFIG

#default colors - will be extended dynamically for new species
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

#theme configs
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

#sets a loading screen
loading_screen <- tagList(
  spin_flower(),
  h3("Processing data...", style = "color: #2C3E50; margin-top: 15px;")
)

#custom CSS
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
  
  [data-bs-theme='dark'] #combined_orthogroup_container 
  .orthogroup-species {
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
  
  /* Fix for Species Analysis dropdown menu */
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
  [data-bs-theme='dark'] 
  .query-panel {
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
  
  /* Gene Explorer specific styles */
   .gene-explorer-container {
  
"))