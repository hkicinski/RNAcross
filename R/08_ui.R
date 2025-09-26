ui <- page_navbar(
  theme = light_theme,  # Start with light theme
  title = NULL,
  id = "nav",
  fillable = FALSE,
  
  # Add all the header elements before the nav panels
  header = tagList(
    useWaiter(),
    useShinyjs(),
    custom_css,
    
    # Add splash screen CSS
    tags$style(HTML("
      /* Splash Screen Styles */
      #splash-screen {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        /* Use safe viewport units */
        height: 100svh;
        height: 100dvh;
        background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
        z-index: 9999;
        overflow: hidden;
        display: grid;
        place-items: center;
        padding: clamp(12px, 3vw, 48px);
        box-sizing: border-box;
      }
      
      #splash-screen.fade-out {
        opacity: 0;
        transform: scale(1.1);
        transition: all 0.8s ease-out;
      }
      
      /* SVG container with safe viewport handling */
      #splash-logo {
        width: 100%;
        height: 100%;
        position: relative;
        display: grid;
        place-items: center;
        padding: 0;
        margin: 0;
      }
      
      /* SVG wrapper and direct SVG styling */
      #svg-wrapper {
        width: min(96vw, 1600px);
        height: auto;
        max-height: min(88svh, 760px);
        position: relative;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
    #svg-wrapper svg, #splash-logo svg {
      width: 100vw !important;
      height: 100vh !important;
      object-fit: cover !important;
      position: fixed !important;
      top: 0 !important;
      left: 0 !important;
      }
      
      /* Mobile-specific adjustments */
      @media (max-width: 480px) {
        #svg-wrapper {
          width: 92vw;
          max-height: 80svh;
        }
        
        #svg-wrapper svg, #splash-logo svg {
          max-height: 80svh !important;
        }
      }
      
      /* Very tall narrow screens */
      @media (max-aspect-ratio: 3/4) {
        #svg-wrapper svg, #splash-logo svg {
          max-height: 82svh !important;
        }
      }
      
      /* Prevent any overflow from the splash screen */
      body.splash-active {
        overflow: hidden;
      }
      
      /* Ensure proper scaling on all devices */
      #splash-screen * {
        max-width: 100%;
      }
            
      /* Glowing trail effect */
      .glow-trail {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 10000;
        overflow: hidden;
      }
      
      .trail-particle {
        position: absolute;
        width: 12px;
        height: 12px;
        border-radius: 50%;
        filter: blur(4px);
        animation: fadeOut 1.5s ease-out forwards;
        pointer-events: none;
      }
      
      @keyframes fadeOut {
        0% { 
          opacity: 1;
          transform: scale(1) translate(0, 0);
        }
        100% { 
          opacity: 0;
          transform: scale(0.3) translate(var(--tx, 0), var(--ty, 0));
        }
      }
      
      /* Yeast cell base styles */
      #splash-logo svg #cells > g[id^=\"cell-\"] {
        cursor: pointer;
        transition: all 0.3s ease;
        transform-origin: center;
      }
      
      /* Spinning animation for cells wrapper */
      @keyframes spinCells {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(720deg); }
      }
      
      .spinning-animation {
        animation: spinCells 0.8s cubic-bezier(0.4, 0, 0.2, 1) forwards;
        transform-origin: center;
      }
      
      /* Dispersal animations */
      @keyframes disperseTopLeft {
        to { transform: translate(-200%, -200%) scale(0.1); opacity: 0; }
      }
      @keyframes disperseTopRight {
        to { transform: translate(200%, -200%) scale(0.1); opacity: 0; }
      }
      @keyframes disperseBottomLeft {
        to { transform: translate(-200%, 200%) scale(0.1); opacity: 0; }
      }
      @keyframes disperseBottomRight {
        to { transform: translate(200%, 200%) scale(0.1); opacity: 0; }
      }
      
      /*hint text animation hidden for now */
      #interaction-hint {
        display: none !important; 
      }

      
      @keyframes fadeInHint {
        to { opacity: 1; }
      }
      
      /* Skip link styling */
      #splash-skip {
        position: absolute;
        top: 20px;
        right: 20px;
        color: rgba(255, 255, 255, 0.7);
        text-decoration: none;
        font-size: 14px;
        cursor: pointer;
        transition: color 0.3s ease;
        z-index: 10001;
      }
      
      #splash-skip:hover {
        color: white;
      }
    ")),
    
    #div for the splash screen overlay
    div(
      id = "splash-screen",
      
      # Skip link
      tags$a(
        id = "splash-skip",
        href = "#",
        onclick = "skipSplash(); return false;",
        "Skip intro →"
      ),
      
      # Logo SVG with proper wrapper for scaling
      tags$div(
        id = "splash-logo",
        tags$div(
          id = "svg-wrapper",
          HTML(paste(readLines("../www/rnacross-prominent-rna-logo-interactive.svg", warn = FALSE), collapse = "\n"))
        )
      ),
      
      # Interaction hint (hidden by CSS)
      div(id = "interaction-hint", "Click the cells to begin"),
      
      # Hidden audio element for sound effect
      tags$audio(
        id = "whoosh-sound",
        src = "data:audio/wav;base64,UklGRnoGAABXQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQoGAACBhYqFbF1fdJivrJBhNjVgodDbq2EcBj+a2/LDciUFLIHO8tiJNwgZaLvt559NEAxQp+PwtmMcBjiR1/LMeSwFJHfH8N2QQAoUXrTp66hVFApGn+DyvmwhBTGH0fPTgjMGHm7A7+OZURE",
        type = "audio/wav",
        preload = "auto"
      )
    ),
    
    # Glow trail container (outside splash screen for z-index)
    div(class = "glow-trail", id = "glow-trail-container"),
    
    # Header
    div(
      class = "navbar-container",
      div(
        class = "d-flex justify-content-between align-items-center p-3 bg-primary text-white",
        span(
          div(
            class = "icon-morph-container",
            style = "margin-right: 3px; position: relative; top: 1px;", 
            icon("dna", class = "fas"),
            icon("chart-line", class = "fas")
          ),
          span(style = "margin-left: 2px;", "Comparative Transcriptomic Analysis")
        ),
        div(
          actionButton("show_help", "Tutorial", icon = icon("question-circle"), 
                       class = "btn-link text-white me-2"),
          actionButton("theme_toggle", label = NULL,
                       icon = icon("moon", verify_fa = FALSE),
                       class = "btn-link text-white",
                       title = "Toggle dark/light mode")
        )
      )
    ),
    
    # JavaScript with SVG scaling fixes
    tags$script(HTML('
    // Auto-hide timer
    let splashTimer;
    let hasInteracted = false;
    
    // Start the auto-hide timer
    function startSplashTimer() {
      splashTimer = setTimeout(function() {
        if (!hasInteracted) {
          hideSplash();
        }
      }, 60000); // 60 seconds
    }
    
    // Initialize event listeners when DOM is ready
    function initializeSplash() {
      // Find yeast cells - try multiple selectors
      const svg = document.querySelector("#splash-logo svg, #svg-wrapper svg");
      if (!svg) {
        console.error("SVG not found");
        return;
      }
      
      // Ensure SVG scales properly without cropping
      svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
      svg.removeAttribute("width");
      svg.removeAttribute("height");
      svg.style.width = "100%";
      svg.style.height = "100%";
      
      const cells = svg.querySelectorAll(\'#cells > g[id^="cell-"], #cells-wrapper > g[id^="cell-"], g[id*="yeast"]\');
      const cellsWrapper = svg.querySelector("#cells-wrapper, #cells");
      
      console.log("Found cells:", cells.length);
      console.log("Found wrapper:", cellsWrapper);
      
      // Add click handlers with improved hover effects
      if (cells.length > 0) {
        cells.forEach(cell => {
          cell.style.cursor = "pointer";
          cell.addEventListener("click", handleYeastClick);
          
          // Improved hover effect
          cell.addEventListener("mouseenter", () => {
            if (!hasInteracted) {
              cell.style.filter = "brightness(1.3) saturate(1.2)";
            }
          });
          cell.addEventListener("mouseleave", () => {
            if (!hasInteracted) {
              cell.style.filter = "";
            }
          });
        });
      }
      // Also add handler to cells wrapper if it exists
      if (cellsWrapper) {
        cellsWrapper.style.cursor = "pointer";
        cellsWrapper.addEventListener("click", handleYeastClick);
      }
      
      // Fallback: make entire SVG clickable
      if (cells.length === 0 && !cellsWrapper) {
        svg.style.cursor = "pointer";
        svg.addEventListener("click", handleYeastClick);
      }
    }
    
    // Handle yeast click
    function handleYeastClick() {
      if (hasInteracted) return; // Prevent multiple clicks
      hasInteracted = true;
      
      // Clear the auto-hide timer
      clearTimeout(splashTimer);
      
      // Play sound effect
      const audio = document.getElementById("whoosh-sound");
      if (audio) {
        audio.play().catch(e => console.log("Audio play failed:", e));
      }
      
      // Get SVG elements
      const svgElement = document.querySelector("#splash-logo svg, #svg-wrapper svg");
      const cellsWrapper = svgElement.querySelector("#cells-wrapper, #cells");
      const cells = svgElement.querySelectorAll(\'#cells > g[id^="cell-"], #cells-wrapper > g[id^="cell-"]\');
      
      // Add spinning animation
      if (cellsWrapper) {
        cellsWrapper.classList.add("spinning-animation");
      }
      
      // Create glowing trails
      createGlowTrails();
      
      // After spin, disperse cells
      setTimeout(() => {
        disperseSVGCells(cells);
        
        // Hide splash after dispersal
        setTimeout(() => {
          hideSplash();
        }, 1000);
      }, 800);
    }
    
    // Create glowing trail particles
    function createGlowTrails() {
      const trailContainer = document.getElementById("glow-trail-container");
      const colors = ["#00F5FF", "#FF006E", "#8338EC", "#FFBE0B"];
      const rect = document.getElementById("splash-logo").getBoundingClientRect();
      const centerX = rect.left + rect.width / 2;
      const centerY = rect.top + rect.height / 2;
      
      // Create burst of particles
      for (let i = 0; i < 40; i++) {
        setTimeout(() => {
          const particle = document.createElement("div");
          particle.className = "trail-particle";
          particle.style.backgroundColor = colors[Math.floor(Math.random() * colors.length)];
          
          // Random position around center
          const angle = (Math.PI * 2 * i) / 40;
          const radius = 20 + Math.random() * 60;
          const startX = centerX + Math.cos(angle) * radius;
          const startY = centerY + Math.sin(angle) * radius;
          
          particle.style.left = startX + "px";
          particle.style.top = startY + "px";
          
          // Set custom properties for animation
          const tx = Math.cos(angle) * (100 + Math.random() * 200);
          const ty = Math.sin(angle) * (100 + Math.random() * 200);
          particle.style.setProperty("--tx", tx + "px");
          particle.style.setProperty("--ty", ty + "px");
          
          trailContainer.appendChild(particle);
          
          // Remove particle after animation
          setTimeout(() => particle.remove(), 1500);
        }, i * 15);
      }
    }
    
    // Disperse SVG cells
    function disperseSVGCells(cells) {
      const animations = [
        "disperseTopLeft",
        "disperseTopRight", 
        "disperseBottomLeft",
        "disperseBottomRight"
      ];
      
      cells.forEach((cell, index) => {
        if (index < 4) {
          cell.style.animation = `${animations[index]} 1s ease-in forwards`;
        }
      });
    }
    
    // Hide splash screen
    function hideSplash() {
      const splash = document.getElementById("splash-screen");
      splash.classList.add("fade-out");
      
      // Remove body class when hiding splash
      document.body.classList.remove("splash-active");
      
      setTimeout(() => {
        splash.style.display = "none";
        // Clean up glow trail container
        const glowContainer = document.getElementById("glow-trail-container");
        if (glowContainer) glowContainer.remove();
      }, 800);
    }
    
    // Skip splash function
    function skipSplash() {
      hasInteracted = true;
      clearTimeout(splashTimer);
      hideSplash();
      localStorage.setItem("skipSplash", "true");
    }
    
    // Handle yeast click from SVG (if SVG has onclick)
    window.handleYeastClickFromSVG = handleYeastClick;
    
    // Initialize on page load
    document.addEventListener("DOMContentLoaded", function() {
      // Check if should skip
      const skipStored = localStorage.getItem("skipSplash");
      if (skipStored === "true") {
        document.getElementById("splash-screen").style.display = "none";
        document.getElementById("glow-trail-container").style.display = "none";
      } else {
        // Add body class when showing splash
        document.body.classList.add("splash-active");
        startSplashTimer();
        // Initialize event listeners after a small delay to ensure SVG is loaded
        setTimeout(initializeSplash, 100);
      }
    });
    '))
  ),
  
  # Footer
  footer = tags$footer(
    class = "footer",
    div(
      class = "container text-center",
      p(
        "Created by Hubert Kicinski",
        br(),
        "Contact: ",
        a(href = "mailto:hkicinski@uiowa.edu", "hkicinski@uiowa.edu", class = "text-white")
      ),
      p(
        class = "mt-2 mb-0",
        "Version 2.9 - Updated August 2025"
      )
    )
  ),
  
  # gene explorer panel:
  nav_panel(
    title = span(icon("dna"), " Gene Explorer"),
    value = "gene_explorer",
    
    div(
      class = "gene-explorer-container",
      style = "padding-bottom: 120px !important;",
      
      # Query Panel
      div(
        class = "query-panel",
        h3("Gene Query Hub", class = "mb-4"),
        p("Search for a gene to explore its orthogroup across all species. Your query will be available throughout all analysis tabs."),
        
        fluidRow(
          column(
            width = 8,
            textInput(
              "global_gene_query",
              label = NULL,
              placeholder = "Enter gene name or ID (e.g., PHO4, YFR034C, CAGL0D05170g)",
              width = "100%"
            )
          ),
          column(
            width = 4,
            actionButton(
              "global_search_button",
              "Search",
              icon = icon("search"),
              class = "btn btn-primary w-100"
            )
          )
        ),
        
        # Query status
        div(
          id = "query_status_container",
          style = "display: none;",
          class = "query-status",
          uiOutput("query_status")
        )
      ),
      
      # Results container
      div(
        id = "gene_explorer_results",
        style = "display: none; magin-bottom: 100px;",
        
        fluidRow(
          # Left column - Tree
          column(
            width = 7,
            div(
              class = "tree-panel",
              h4("Phylogenetic Tree"),
              uiOutput("phylo_tree_plot_ui"),
              div(
                class = "tree-legend",
                h5("Tree Information"),
                p("This tree shows the evolutionary relationships between orthogroup members."),
                p("Tips are colored by species. Branch lengths represent evolutionary distance.")
              )
            )
          ),
          
          #right columns is Orthogroup info
          column(
            width = 5,
            div(
              class = "orthogroup-summary",
              uiOutput("orthogroup_summary")
            ),
            
            #quick action buttons
            div(
              class = "mt-4",
              h5("Quick Actions"),
              actionButton(
                "explore_species_view",
                "View in Species Analysis",
                icon = icon("chart-line"),
                class = "btn btn-secondary w-100 mb-2"
              ),
              actionButton(
                "explore_combined_view",
                "View in Combined Analysis",
                icon = icon("layer-group"),
                class = "btn btn-secondary w-100 mb-2"
              ),
              actionButton(
                "explore_heatmap",
                "Generate Cross-Species Heatmap",
                icon = icon("th"),
                class = "btn btn-secondary w-100"
              )
            ),
            
            # Orthogroup table
            div(
              class = "mt-4",
              h5("Orthogroup Members"),
              DTOutput("explorer_orthogroup_table")
            )
          )
        )
      )
    )
  ),
  # Data Upload Panel
  nav_panel(
    title = span(icon("upload"), " Data Upload"),
    value = "data_upload",
    
    div(
      class = "container-fluid",
      style = "padding: 20px; padding-bottom: 120px;",
      
      # Upload Status Banner
      div(
        id = "upload_status_banner",
        style = "display: none;",
        class = "alert",
        uiOutput("upload_status_content")
      ),
      
      # Main upload interface
      fluidRow(
        # Left panel - Species Definition and Upload
        column(
          width = 4,
          div(
            class = "sidebar-panel",
            h4("Define Your Dataset", class = "mb-4"),
            
            # Step 1: Define Species/Groups
            div(
              class = "upload-step",
              h5("Step 1: Define Your Species/Groups", icon("dna")),
              p("Add each species or experimental group. You can use any organisms."),
              
              # Dynamic species input area
              div(id = "species_input_area",
                  div(class = "species-entry mb-2", id = "species_entry_1",
                      fluidRow(
                        column(3, 
                               textInput("species_code_1", "Code*", value = "", 
                                         placeholder = "e.g., hs, mm, dm")),
                        column(5, 
                               textInput("species_name_1", "Full Name*", value = "",
                                         placeholder = "e.g., Homo sapiens")),
                        column(4, 
                               textInput("species_short_1", "Display Name", value = "",
                                         placeholder = "e.g., Human"))
                      ),
                      tags$small(class = "text-muted", 
                                 "Code: short identifier for files. Full Name: scientific name. Display: for plots.")
                  )
              ),
              
              div(class = "mt-2 mb-3",
                  actionButton("add_species", "Add Species", 
                               icon = icon("plus"), class = "btn btn-sm btn-success"),
                  actionButton("remove_species", "Remove Last", 
                               icon = icon("minus"), class = "btn btn-sm btn-warning ml-2")
              ),
              
              # Current species display
              div(
                class = "mt-3 p-2 bg-light rounded",
                h6("Defined Species:", class = "mb-2"),
                tableOutput("current_species_table")
              ),
              hr()
            ),
            
            # Step 2: Upload Expression Data
            div(
              class = "upload-step",
              h5("Step 2: Expression Matrices", icon("table")),
              p("Upload normalized expression data (genes × samples)"),
              uiOutput("expression_upload_ui"),
              hr()
            ),
            
            # Step 3: Upload Sample Info
            div(
              class = "upload-step",
              h5("Step 3: Sample Metadata", icon("clipboard")),
              p("Required columns: Sample, Timepoint, Replicate"),
              uiOutput("sample_upload_ui"),
              hr()
            ),
            
            # Step 4: Upload Annotations (Optional)
            div(
              class = "upload-step",
              h5("Step 4: Gene Annotations", icon("book"), 
                 span("(Optional)", class = "badge badge-secondary ml-2")),
              p("Columns: GeneID, GeneName, Chr"),
              uiOutput("annotation_upload_ui")
            )
          )
        ),
        
        # Middle panel - Orthology
        column(
          width = 4,
          div(
            class = "sidebar-panel",
            h4("Orthology Mapping", class = "mb-4"),
            
            div(
              class = "upload-step",
              h5("Step 5: Define Orthology", icon("sitemap")),
              p("How genes relate across your species"),
              
              radioButtons(
                "orthology_source",
                "Orthology data source:",
                choices = list(
                  "Upload OrthoFinder output" = "orthofinder",
                  "Upload custom orthology table" = "custom",
                  "No orthology (single species)" = "none",
                  "Use demo data" = "default"
                ),
                selected = "none"
              ),
              
              conditionalPanel(
                condition = "input.orthology_source == 'orthofinder'",
                fileInput("upload_orthogroups", "Orthogroups.tsv:",
                          accept = c(".tsv", ".txt")),
                fileInput("upload_hog", "N0.tsv (HOGs, optional):",
                          accept = c(".tsv", ".txt"))
              ),
              
              conditionalPanel(
                condition = "input.orthology_source == 'custom'",
                fileInput("upload_custom_ortho", "Custom orthology:",
                          accept = c(".tsv", ".txt", ".csv")),
                p(class = "text-muted small mt-2",
                  "Required: gene_id, species_code, orthogroup_id")
              ),
              
              conditionalPanel(
                condition = "input.orthology_source == 'none'",
                div(class = "alert alert-info",
                    icon("info-circle"),
                    " Single-species mode. Cross-species features will be disabled.")
              ),
              hr()
            ),
            
            # Action buttons
            div(
              class = "mt-4",
              actionButton("validate_uploads", "Validate Data",
                           icon = icon("check-circle"),
                           class = "btn btn-info w-100 mb-2"),
              actionButton("process_uploads", "Process & Load",
                           icon = icon("cogs"),
                           class = "btn btn-success w-100 mb-2",
                           disabled = TRUE),
              actionButton("reset_to_default", "Use Demo Data",
                           icon = icon("undo"),
                           class = "btn btn-warning w-100")
            )
          )
        ),
        
        # Right panel - Validation
        column(
          width = 4,
          div(
            class = "results-panel",
            h4("Validation & Preview", class = "mb-4"),
            
            div(
              id = "validation_summary",
              style = "min-height: 200px;",
              uiOutput("validation_results")
            ),
            
            # Preview tabs
            div(
              class = "mt-4",
              tabsetPanel(
                id = "upload_preview_tabs",
                tabPanel("Expression", DTOutput("upload_expr_preview")),
                tabPanel("Samples", DTOutput("upload_sample_preview")),
                tabPanel("Annotations", DTOutput("upload_anno_preview")),
                tabPanel("Orthology", DTOutput("upload_ortho_preview"))
              )
            ),
            
            # Download section
            conditionalPanel(
              condition = "output.data_processed == true",
              div(
                class = "mt-4",
                h5("Export Configuration"),
                downloadButton("download_config", "Download Config",
                               class = "btn btn-primary w-100 mb-2"),
                downloadButton("download_processed_rdata", "Download RData",
                               class = "btn btn-secondary w-100")
              )
            )
          )
        )
      )
    )
  ),
  #dynamic species analysis tabs
  nav_panel(
    title = "Species Analysis",
    value = "species_analysis_container",
    uiOutput("dynamic_species_panels")
  ),
  
  
  
  #combined view tab
  nav_panel(
    "Combined View",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Combined Analysis"),
          textInput(
            "combined_genename",
            "Gene name or ID:",
            placeholder = "e.g., PHO4, MSN2"
          ),
          actionButton(
            "combined_search_button",
            "Search Gene",
            icon = icon("search"),
            class = "custom-button"
          ),
          
          # Enhanced orthogroup selection container
          div(
            id = "combined_orthogroup_container",
            style = "display: none;",
            hr(),
            div(
              class = "orthogroup-info",
              h5("Select Genes from Orthogroup"),
              p("Choose one or more genes per species to compare:"),
              
              # Information about paralog selection
              div(
                id = "paralog_info",
                class = "alert alert-info mb-3",
                style = "font-size: 0.9em;",
                icon("info-circle"),
                " When multiple paralogs exist, you can select multiple genes to compare their expression patterns."
              ),
              #dynamic div
              div(
                id = "combined_orthogroup_selection_wrapper"
                #containers will be added dynamically by observers
              )
            ),
            
            # Add a summary of selections
            div(
              id = "selection_summary",
              class = "mt-3 p-2 bg-light rounded",
              style = "font-size: 0.9em;",
              uiOutput("combined_selection_summary")
            )
          ),
          
          uiOutput("species_select_ui"),
          actionButton(
            "combined_plot_button",
            "Generate Combined Plot",
            icon = icon("chart-line"),
            class = "custom-button"
          ),
          
          # Download button for the plot
          downloadButton(
            "download_combined_plot",
            "Download Plot",
            class = "btn btn-secondary mt-2 w-100"
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          plotlyOutput("combined_gene_plot", height = "600px"),
          div(
            class = "mt-4",
            h5("Orthogroup Information"),
            DTOutput("combined_orthogroup_table")
          )
        )
      )
    )
  ),
  
  # gene group Analysis tab
  nav_panel(
    "Gene Group Analysis",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Gene Group Analysis Controls"),
          fileInput(
            "gene_group_file",
            "Upload Gene Groups (CSV)",
            accept = c("text/csv", ".csv")
          ),
          textAreaInput(
            "gene_list",
            "Or paste gene list:",
            rows = 5,
            placeholder = "Enter genes, one per line"
          ),
          uiOutput("group_analysis_species_ui"),
          selectInput(
            "distance_method",
            "Clustering Method:",
            choices = list(
              "Pearson Correlation" = "pearson",
              "Euclidean" = "euclidean"
            ),
            selected = "pearson"
          ),
          selectInput(
            "data_transform",
            "Data Transformation:",
            choices = list(
              "Raw log2CPM" = "raw",
              "Centered log2CPM" = "centered",
              "Z-score" = "zscore"
            ),
            selected = "centered"
          ),
          selectInput(
            "group_viz_type",
            "Visualization Type:",
            choices = list(
              "Line Plot" = "line",
              "Bar Chart" = "bar",
              "Heatmap" = "heatmap"
            )
          ),
          
          # significance testing control
          div(
            id = "significance_controls",
            style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #ccc;",
            h5("Significance Testing"),
            
            # Toggle significance markers
            checkboxInput(
              "show_significance",
              "Show Statistical Significance",
              value = TRUE
            ),
            
            # Only show other controls if significance is enabled
            conditionalPanel(
              condition = "input.show_significance == true && input.group_viz_type == 'bar'",
              
              # P-value threshold slider
              sliderInput(
                "significance_threshold",
                "P-value Threshold:",
                min = 0.001,
                max = 0.05,
                value = 0.05,
                step = 0.001
              ),
              
              # Gene selector for significance testing
              selectizeInput(
                "sig_test_gene",
                "Select Gene to Test:",
                choices = NULL,  # Will be populated dynamically
                options = list(
                  placeholder = "Select a gene"
                )
              ),
              
              # Timepoint pair selector
              selectizeInput(
                "sig_test_timepoints",
                "Select Timepoint Comparison:",
                choices = NULL,  # Will be populated dynamically
                multiple = TRUE,
                options = list(
                  placeholder = "Select timepoint pairs to compare",
                  maxItems = 3  # Limit to 3 comparisons at a time
                )
              ),
              
              # Add button to apply the selection
              actionButton(
                "apply_sig_test",
                "Apply Significance Test",
                class = "btn btn-primary btn-sm w-100 mt-2"
              )
            )
          ),
          
          actionButton(
            "analyze_gene_groups",
            "Generate Visualization",
            icon = icon("chart-line"),
            class = "custom-button"
          ),
          downloadButton(
            "download_group_plot",
            "Download Plot",
            class = "btn btn-secondary mt-2 w-100"
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          plotlyOutput("gene_group_plot", height = "500px"),
          div(
            class = "mt-4",
            DTOutput("gene_group_table")
          ),
          
          # Significance legend
          conditionalPanel(
            condition = "input.show_significance == true && input.group_viz_type == 'bar'",
            div(
              class = "mt-3 p-3",
              style = "background-color: var(--bs-body-bg); border-radius: 4px; border: 1px solid var(--bs-border-color);",
              h5("Significance Legend", style = "margin-top: 0;"),
              tags$ul(
                style = "padding-left: 20px; margin-bottom: 5px;",
                tags$li("* : p < 0.05"),
                tags$li("** : p < 0.01"),
                tags$li("*** : p < 0.001")
              ),
              p("Statistical analysis based on unpaired t-test between timepoints.", 
                style = "font-size: 12px; margin-bottom: 0;")
            )
          )
        )
      )
    )
  ),
  
  # Updated PCA tab with removed color by and data transformation options
  nav_panel(
    "PCA",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("PCA Controls"),
          radioButtons(
            "pca_type",
            "Analysis Type:",
            choices = list(
              "Single Species" = "single",
              "Multi-Species Comparison" = "multi"
            ),
            selected = "single"
          ),
          
          # Single species specific controls
          conditionalPanel(
            condition = "input.pca_type == 'single'",
            uiOutput("pca_species_ui")
          ),
          
          actionButton(
            "run_pca",
            "Run PCA",
            icon = icon("play"),
            class = "custom-button"
          ),
          
          # Add download button for the orthology matrix
          conditionalPanel(
            condition = "input.pca_type == 'multi'",
            downloadButton(
              "download_orthology_matrix",
              "Export Expression Matrix",
              class = "btn btn-secondary mt-3 w-100"
            )
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          plotlyOutput("pca_plot", height = "500px"),
          verbatimTextOutput("pca_debug_output")
        )
      )
    )
  ),
  
  # Cross-Species Heatmap tab 
  nav_panel(
    "Cross-Species Heatmap",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Gene Set Controls"),
          textAreaInput(
            "ortholog_gene_list",
            "Enter gene list:",
            rows = 5,
            placeholder = "Enter genes, one per line (e.g., PHO4, PHO81, PHO84)"
          ),
          fileInput(
            "ortholog_gene_file",
            "Or upload gene list (text file):",
            accept = c("text/plain", ".txt", ".csv")
          ),
          selectInput(
            "ortholog_normalization",
            "Data Normalization:",
            choices = list(
              "Z-score (by gene)" = "zscore",
              "Centered (by gene)" = "centered",
              "Raw log2CPM values" = "raw"
            ),
            selected = "zscore"
          ),
          checkboxInput(
            "ortholog_cluster_rows",
            "Cluster genes (rows)",
            value = TRUE
          ),
          checkboxInput(
            "ortholog_cluster_cols",
            "Cluster timepoints (columns)",
            value = FALSE
          ),
          actionButton(
            "generate_ortholog_heatmap",
            "Generate Heatmap",
            icon = icon("chart-line"),
            class = "custom-button"
          ),
          downloadButton(
            "download_ortholog_heatmap",
            "Download Heatmap",
            class = "btn btn-secondary mt-2 w-100"
          ),
          downloadButton(
            "download_ortholog_data",
            "Download Data Matrix",
            class = "btn btn-secondary mt-2 w-100"
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          plotlyOutput("ortholog_heatmap_plot", height = "500px"),
          div(
            class = "mt-4",
            h5("Ortholog Mapping Table"),
            DTOutput("ortholog_mapping_table")
          )
        )
      )
    )
  ),
  
  # Ridgeline Plots tab
  nav_panel(
    "Ridgeline Plots",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Ridgeline Plot Controls"),
          uiOutput("ridgeline_species_ui"),
          selectInput(
            "ridgeline_view",
            "View Type:",
            choices = list(
              "Expression Distribution" = "distribution",
              "Gene Count Above Threshold" = "count"
            ),
            selected = "distribution"
          ),
          sliderInput(
            "expression_threshold",
            "Expression Threshold:",
            min = 0,
            max = 10,
            value = 2,
            step = 0.5
          ),
          actionButton(
            "generate_ridgeline",
            "Generate Plot",
            icon = icon("chart-line"),
            class = "custom-button"
          ),
          downloadButton(
            "download_ridgeline",
            "Download Plot",
            class = "btn btn-secondary mt-2 w-100"
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          plotOutput("ridgeline_plot", height = "600px")
        )
      )
    )
  )
)
