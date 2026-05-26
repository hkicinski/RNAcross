# rnacross ui module
# shiny user interface definition
# dependencies: 02_constants_themes

ui <- page_navbar(
  theme = light_theme,
  title = NULL,
  id = "nav",
  fillable = FALSE,

  # header elements before nav panels
  header = tagList(
    useWaiter(),
    useShinyjs(),
    tags$head(includeScript("www/interactive_editor.js")),
    tags$head(
      tags$meta(name = "theme-color", content = "#000814"),
      tags$meta(name = "color-scheme", content = "dark"),
      tags$style(HTML("html { color-scheme: dark; }"))
    ),
    absolutePanel(
      id = "plot_aesthetic_editor",
      top = 100, right = 20, width = 320,
      fixed = TRUE,
      draggable = FALSE, # Manual JS drag
      style = "z-index: 9999; background: var(--bs-body-bg); border: 1px solid var(--bs-border-color); border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); display: none;",
      div(
        id = "plot_editor_header",
        style = "padding: 10px 15px; background: var(--bs-primary); color: white; border-top-left-radius: 5px; border-top-right-radius: 5px; cursor: move; display: flex; justify-content: space-between; align-items: center;",
        strong("Aesthetic Editor"),
        actionButton("close_editor_btn", "", icon = icon("times"), class = "btn-sm btn-link text-white p-0 text-decoration-none border-0", style = "line-height: 1;")
      ),
      div(
        style = "padding: 15px; max-height: 70vh; overflow-y: auto;",
        uiOutput("plot_editor_ui")
      )
    ),
    tags$script(HTML(sprintf(
      'const RNACROSS_VERSION = "%s";',
      app_version_info$version
    ))),
    tags$script(HTML("
      // --- Plot Export Handlers ---
      Shiny.addCustomMessageHandler('plotly_export', function(msg) {
        var container = document.getElementById(msg.plotId);
        if (!container) { alert('Plot not found. Please generate the plot first.'); return; }
        var gd = container.querySelector('.js-plotly-plot') ||
                 container.querySelector('.plotly.html-widget') ||
                 container;
        Plotly.downloadImage(gd, {
          format: msg.format,
          width: msg.width,
          height: msg.height,
          filename: msg.filename,
          scale: 1
        }).catch(function(err) {
          alert('Export failed: ' + err.message);
        });
      });

      Shiny.addCustomMessageHandler('download_base64', function(msg) {
        var raw = atob(msg.data);
        var arr = new Uint8Array(raw.length);
        for (var i = 0; i < raw.length; i++) arr[i] = raw.charCodeAt(i);
        var blob = new Blob([arr], {type: msg.mime});
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 100);
      });

      // --- Enter Key Submit Handlers ---
      $(document).on('keyup', function(e) {
        if (e.which == 13) {
          var tag = document.activeElement ? document.activeElement.tagName : '';
          if (tag === 'TEXTAREA' || tag === 'BUTTON' || tag === 'A' || tag === 'SELECT') return;
          
          if (document.activeElement && document.activeElement.closest && document.activeElement.closest('.selectize-control')) return;

          var possibleButtons = [
            'global_search_button',
            'combined_search_button',
            'analyze_gene_groups',
            'run_pca',
            'generate_heatmap',
            'generate_ridgeline'
          ];
          
          var clicked = false;
          
          for (var i = 0; i < possibleButtons.length; i++) {
            var btn = document.getElementById(possibleButtons[i]);
            if (btn && btn.offsetParent !== null && !btn.disabled) {
              btn.click();
              clicked = true;
              break;
            }
          }
          
          if (!clicked) {
            var allBtns = document.querySelectorAll('button[id$=\"_search_button\"]');
            for (var i = 0; i < allBtns.length; i++) {
              if (allBtns[i].offsetParent !== null && !allBtns[i].disabled) {
                allBtns[i].click();
                break;
              }
            }
          }
        }
      });
    ")),
    custom_css,

    # Fix Plotly pointer events and splash screen css
    tags$style(HTML("
      /* FORCE PLOTLY ELEMENTS TO BE CLICKABLE */
      .js-plotly-plot text, 
      .js-plotly-plot tspan,
      .js-plotly-plot path,
      .js-plotly-plot circle,
      .js-plotly-plot rect.bg,
      .js-plotly-plot rect.nsewdrag {
          pointer-events: all !important;
      }

      /* Force backgrounds to match SVG logo ONLY during splash */
      body.splash-active,
      body.splash-active .bslib-page-fill,
      body.splash-active .bslib-page,
      body.splash-active .container-fluid,
      body.splash-active > div,
      body.splash-active > nav {
        background-color: #000814 !important;
      }
      #splash-screen {
        position: fixed;
        inset: 0;
        background: #000814;
        z-index: 9999;
        overflow: hidden;
        margin: 0;
        padding: 0;
      }

      #splash-screen.fade-out {
        opacity: 0;
        transform: scale(1.1);
        transition: all 0.8s ease-out;
      }

      #splash-logo {
        position: absolute;
        inset: 0;
        margin: 0;
        padding: 0;
      }

      #svg-wrapper {
        position: absolute;
        inset: 0;
        margin: 0;
        padding: 0;
      }

      #svg-wrapper svg, #splash-logo svg {
        position: absolute;
        inset: 0;
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        display: block;
      }

      /* Prevent any overflow from the splash screen */
      body.splash-active {
        overflow: hidden;
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

    # splash screen overlay div
    div(
      id = "splash-screen",

      # skip link
      tags$a(
        id = "splash-skip",
        href = "#",
        onclick = "skipSplash(); return false;",
        "Skip intro →"
      ),

      # logo SVG with proper wrapper for scaling
      tags$div(
        id = "splash-logo",
        tags$div(
          id = "svg-wrapper",
          HTML(paste(readLines(file.path("www", "rnacross-prominent-rna-logo-interactive.svg"), warn = FALSE), collapse = "\n"))
        )
      ),

      # interaction hint (hidden by CSS)
      div(id = "interaction-hint", "Click the cells to begin"),

      # hidden audio element for sound effect
      tags$audio(
        id = "whoosh-sound",
        src = "data:audio/wav;base64,UklGRnoGAABXQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQoGAACBhYqFbF1fdJivrJBhNjVgodDbq2EcBj+a2/LDciUFLIHO8tiJNwgZaLvt559NEAxQp+PwtmMcBjiR1/LMeSwFJHfH8N2QQAoUXrTp66hVFApGn+DyvmwhBTGH0fPTgjMGHm7A7+OZURE",
        type = "audio/wav",
        preload = "auto"
      )
    ),

    # glow trail container (outside splash screen for z-index)
    div(class = "glow-trail", id = "glow-trail-container"),

    # header bar
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
          class = "d-flex align-items-center",
          actionButton(
            "show_version_info",
            label = NULL,
            icon = icon("bullhorn"),
            class = "btn-link text-white me-1",
            title = "What's New"
          ),
          actionButton("show_help", "Tutorial",
            icon = icon("question-circle"),
            class = "btn-link text-white me-2"
          ),
          actionButton("show_settings",
            label = NULL,
            icon = icon("gear"),
            class = "btn-link text-white me-2",
            title = "Plot settings"
          ),
          actionButton("theme_toggle",
            label = NULL,
            icon = icon("moon", verify_fa = FALSE),
            class = "btn-link text-white",
            title = "Toggle dark/light mode"
          )
        )
      )
    ),

    # javascript with SVG scaling fixes
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
      svg.setAttribute("preserveAspectRatio", "xMidYMid slice");
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
        setTimeout(function() {
          var seenVersion = localStorage.getItem("rnacross_seen_version");
          if (seenVersion !== RNACROSS_VERSION) {
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue("trigger_version_modal", Math.random());
            }
          }
        }, 500);
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
        setTimeout(function() {
          var seenVersion = localStorage.getItem("rnacross_seen_version");
          if (seenVersion !== RNACROSS_VERSION) {
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue("trigger_version_modal", Math.random());
            }
          }
        }, 1000);
      } else {
        // Add body class when showing splash
        document.body.classList.add("splash-active");
        startSplashTimer();
        // Initialize event listeners after a small delay to ensure SVG is loaded
        setTimeout(initializeSplash, 100);
      }
    });
    ')),

    # localstorage for session persistence
    tags$script(HTML("
    //save session to browser localStorage
    Shiny.addCustomMessageHandler('saveSession', function(session_state) {
      try {
        console.log('Saving session state:', session_state);
        localStorage.setItem('rnacross_session', JSON.stringify(session_state));
        localStorage.setItem('rnacross_session_timestamp', new Date().toISOString());
        console.log('Session saved successfully');
      } catch(e) {
        console.error('localStorage save failed:', e);
      }
    });

    //load session on app startup
    $(document).on('shiny:connected', function() {
      console.log('Shiny connected, checking for saved session');
      var session_state = localStorage.getItem('rnacross_session');
      var timestamp = localStorage.getItem('rnacross_session_timestamp');

      if (session_state) {
        console.log('Found saved session from:', timestamp);
        try {
          Shiny.setInputValue('restore_session', {
            state: JSON.parse(session_state),
            saved_at: timestamp
          }, {priority: 'event'});
          console.log('Restore signal sent to R');
        } catch(e) {
          console.error('Failed to restore session:', e);
        }
      } else {
        console.log('No saved session found');
      }
    });

    //clear session
    Shiny.addCustomMessageHandler('clearSession', function(msg) {
      console.log('Clearing session');
      localStorage.removeItem('rnacross_session');
      localStorage.removeItem('rnacross_session_timestamp');
      localStorage.removeItem('rnacross_seen_version');
      location.reload();
    });
    ")),

    tags$script(HTML("
      $(document).on('shiny:disconnected', function(event) {
        if (document.getElementById('shiny-disconnected-overlay')) return;

        var overlay = document.createElement('div');
        overlay.id = 'shiny-disconnected-overlay';
        overlay.style.cssText = 'position:fixed;top:0;left:0;right:0;bottom:0;background:#1a1a2e;z-index:99999;display:flex;align-items:center;justify-content:center;text-align:center;color:#e0e0e0;font-family:Inter,Segoe UI,sans-serif;';
        overlay.innerHTML =
          '<div style=\"max-width:500px;padding:40px;background:#16213e;border-radius:16px;border:1px solid #1a1a4e;\">' +
            '<h2 style=\"color:#ff6b6b;margin-bottom:16px;font-size:1.6rem;\">Something went wrong</h2>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">The app has encountered an error or lost connection to the server.</p>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">Please try reloading the page. If the issue persists, report it:</p>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">' +
              '<a href=\"https://github.com/hkicinski/RNAcross/issues\" target=\"_blank\" style=\"color:#6ea8fe;text-decoration:underline;\">GitHub Issues</a>' +
              ' &nbsp;|&nbsp; ' +
              '<a href=\"mailto:hkicinski@uiowa.edu\" style=\"color:#6ea8fe;text-decoration:underline;\">hkicinski@uiowa.edu</a>' +
            '</p>' +
            '<button onclick=\"location.reload()\" style=\"display:inline-block;margin-top:20px;padding:10px 28px;background:#0d6efd;color:white;border:none;border-radius:8px;font-size:1rem;cursor:pointer;\">Reload App</button>' +
          '</div>';
        document.body.appendChild(overlay);

        var waiter = document.querySelector('.waiter-overlay');
        if (waiter) waiter.style.display = 'none';

        var splash = document.getElementById('splash-screen');
        if (splash) splash.style.display = 'none';
      });

      $(document).on('shiny:error', function(event) {
        var waiter = document.querySelector('.waiter-overlay');
        if (waiter) waiter.style.display = 'none';
      });

      var _waiterTimeout = null;
      $(document).on('shiny:busy', function() {
        clearTimeout(_waiterTimeout);
        _waiterTimeout = setTimeout(function() {
          var waiter = document.querySelector('.waiter-overlay');
          if (waiter && waiter.style.display !== 'none') {
            waiter.style.display = 'none';
          }
        }, 45000);
      });
      $(document).on('shiny:idle', function() {
        clearTimeout(_waiterTimeout);
        setTimeout(function() {
          var waiter = document.querySelector('.waiter-overlay');
          if (waiter && waiter.style.display !== 'none') {
            waiter.style.display = 'none';
          }
        }, 500);
      });
    ")),

    tags$style(HTML("
      #shiny-disconnected-screen { display: none !important; }
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before { visibility: hidden; }
    "))
  ),

  # footer
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
        sprintf("Version %s - Updated %s",
          app_version_info$version,
          format(as.Date(app_version_info$release_date), "%B %Y")),
        br(),
        a(href = "https://github.com/hkicinski/RNAcross/issues",
          target = "_blank",
          icon("github"), " Report a Bug or Suggest a Feature",
          class = "text-white")
      )
    )
  ),

  # gene explorer panel
  nav_panel(
    title = span(icon("dna"), " Gene Explorer"),
    value = "gene_explorer",
    div(
      class = "gene-explorer-container",
      style = "padding-bottom: 120px !important;",

      # query panel
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

        # query status
        div(
          id = "query_status_container",
          style = "display: none;",
          class = "query-status",
          uiOutput("query_status")
        )
      ),

      # results container
      div(
        id = "gene_explorer_results",
        style = "display: none; magin-bottom: 100px;",
        fluidRow(
          # left column - tree
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

          # right column - orthogroup info
          column(
            width = 5,
            div(
              class = "orthogroup-summary",
              uiOutput("orthogroup_summary")
            ),

            uiOutput("explorer_quick_actions"),

            uiOutput("explorer_orthogroup_section")
          )
        )
      )
    )
  ),
  # gene similarity panel
  nav_panel(
    title = span(icon("chart-line"), " Find similar profiles"),
    value = "similarity_search",
    div(
      class = "container-fluid",
      style = "padding: 20px; padding-bottom: 120px;",
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            h4(class = "mb-4", "Similarity Search"),
            textInput(
              "similarity_gene_input",
              "Query Gene:",
              placeholder = "e.g., PHO84"
            ),
            selectizeInput(
              "similarity_ref_species",
              "Reference Species:",
              choices = NULL,
              options = list(
                render = I("{
                  option: function(item, escape) { return '<div><i>' + escape(item.label) + '</i></div>'; },
                  item: function(item, escape) { return '<div><i>' + escape(item.label) + '</i></div>'; }
                }")
              )
            ),
            checkboxGroupInput(
              "similarity_tgt_species",
              "Target Species (Overlay):",
              choices = NULL
            ),
            sliderInput(
              "similarity_top_matches",
              "Top Matches (Graph - Table):",
              min = 1,
              max = 100,
              value = c(5, 20),
              step = 1
            ),
            actionButton(
              "similarity_search_button",
              "Search Profiles",
              icon = icon("search"),
              class = "btn-primary mt-3 w-100"
            )
          )
        ),
        column(
          width = 9,
          div(
            class = "main-plot-area",
            plotlyOutput("similarity_plot", height = "500px")
          ),
          div(
            class = "mt-4",
            DTOutput("similarity_table")
          )
        )
      )
    )
  ),
  # data upload panel
  nav_panel(
    title = span(icon("upload"), " Data Upload"),
    value = "data_upload",
    div(
      class = "container-fluid",
      style = "padding: 20px; padding-bottom: 120px;",

      # upload status banner
      div(
        id = "upload_status_banner",
        style = "display: none;",
        class = "alert",
        uiOutput("upload_status_content")
      ),

      # main upload interface
      fluidRow(
        # left panel - species definition and upload
        column(
          width = 4,
          div(
            class = "sidebar-panel",
            h4("Define Your Dataset", class = "mb-4"),

            # step 1: define species/groups
            div(
              class = "upload-step",
              h5("Step 1: Define Your Species/Groups", icon("dna")),
              p("Add each species or experimental group. You can use any organisms."),

              # dynamic species input area
              div(
                id = "species_input_area",
                div(
                  class = "species-entry mb-2", id = "species_entry_1",
                  fluidRow(
                    column(
                      3,
                      textInput("species_code_1", "Code*",
                        value = "",
                        placeholder = "e.g., hs, mm, dm"
                      )
                    ),
                    column(
                      5,
                      textInput("species_name_1", "Full Name*",
                        value = "",
                        placeholder = "e.g., Homo sapiens"
                      )
                    ),
                    column(
                      4,
                      textInput("species_short_1", "Display Name",
                        value = "",
                        placeholder = "e.g., Human"
                      )
                    )
                  ),
                  tags$small(
                    class = "text-muted",
                    "Code: short identifier for files. Full Name: scientific name. Display: for plots."
                  )
                )
              ),
              div(
                class = "mt-2 mb-3",
                actionButton("add_species", "Add Species",
                  icon = icon("plus"), class = "btn btn-sm btn-success"
                ),
                actionButton("remove_species", "Remove Last",
                  icon = icon("minus"), class = "btn btn-sm btn-warning ml-2"
                )
              ),

              # current species display
              div(
                class = "mt-3 p-2 bg-light rounded",
                h6("Defined Species:", class = "mb-2"),
                tableOutput("current_species_table")
              ),
              hr()
            ),

            # step 2: upload expression data
            div(
              class = "upload-step",
              h5("Step 2: Expression Matrices", icon("table")),
              p("Upload normalized expression data (genes × samples)"),
              uiOutput("expression_upload_ui"),
              hr()
            ),

            # step 3: upload sample info
            div(
              class = "upload-step",
              h5("Step 3: Sample Metadata", icon("clipboard")),
              p("Required columns: Sample, Timepoint, Replicate"),
              uiOutput("sample_upload_ui"),
              hr()
            ),

            # step 4: upload annotations (optional)
            div(
              class = "upload-step",
              h5(
                "Step 4: Gene Annotations", icon("book"),
                span("(Optional)", class = "badge badge-secondary ml-2")
              ),
              p("Columns: GeneID, GeneName, Chr"),
              uiOutput("annotation_upload_ui")
            )
          )
        ),

        # middle panel - orthology
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
                  accept = c(".tsv", ".txt")
                ),
                fileInput("upload_hog", "N0.tsv (HOGs, optional):",
                  accept = c(".tsv", ".txt")
                )
              ),
              conditionalPanel(
                condition = "input.orthology_source == 'custom'",
                fileInput("upload_custom_ortho", "Custom orthology:",
                  accept = c(".tsv", ".txt", ".csv")
                ),
                p(
                  class = "text-muted small mt-2",
                  "Required: gene_id, species_code, orthogroup_id"
                )
              ),
              conditionalPanel(
                condition = "input.orthology_source == 'none'",
                div(
                  class = "alert alert-info",
                  icon("info-circle"),
                  " Single-species mode. Cross-species features will be disabled."
                )
              ),
              hr()
            ),

            # action buttons
            div(
              class = "mt-4",
              actionButton("validate_uploads", "Validate Data",
                icon = icon("check-circle"),
                class = "btn btn-info w-100 mb-2"
              ),
              actionButton("process_uploads", "Process & Load",
                icon = icon("cogs"),
                class = "btn btn-success w-100 mb-2",
                disabled = TRUE
              ),
              actionButton("reset_to_default", "Use Demo Data",
                icon = icon("undo"),
                class = "btn btn-warning w-100"
              )
            )
          )
        ),

        # right panel - validation
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

            # preview tabs
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

            # download section
            conditionalPanel(
              condition = "output.data_processed == true",
              div(
                class = "mt-4",
                h5("Export Configuration"),
                downloadButton("download_config", "Download Config",
                  class = "btn btn-primary w-100 mb-2"
                ),
                downloadButton("download_processed_rdata", "Download RData",
                  class = "btn btn-secondary w-100"
                )
              )
            )
          )
        )
      )
    )
  ),
  # single species view tabs
  nav_panel(
    title = "Single Species View",
    value = "species_analysis_container",
    uiOutput("dynamic_species_panels")
  ),


  # comparative view tab
  nav_panel(
    "Comparative View",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Comparative Analysis"),
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

          # enhanced orthogroup selection container
          div(
            id = "combined_orthogroup_container",
            style = "display: none;",
            hr(),
            div(
              class = "orthogroup-info",
              h5("Select Genes from Orthogroup"),
              p("Choose one or more genes per species to compare:"),

              # information about paralog selection
              div(
                id = "paralog_info",
                class = "alert alert-info mb-3",
                style = "font-size: 0.9em;",
                icon("info-circle"),
                " When multiple paralogs exist, you can select multiple genes to compare their expression patterns."
              ),
              # dynamic div
              div(
                id = "combined_orthogroup_selection_wrapper"
                # containers will be added dynamically by observers
              )
            ),

            # summary of selections
            div(
              id = "selection_summary",
              class = "mt-3 p-2 bg-light rounded",
              style = "font-size: 0.9em;",
              uiOutput("combined_selection_summary")
            )
          ),
          uiOutput("species_select_ui"),

          # normalization options
          div(
            class = "mt-3 mb-3",
            checkboxInput(
              "normalize_to_baseline",
              "Normalize to baseline (0 min)",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.normalize_to_baseline == true",
              div(
                class = "small text-muted",
                icon("info-circle"),
                " Y-axis will show log2 fold-change relative to 0 min"
              )
            )
          ),
          actionButton(
            "combined_plot_button",
            "Generate Combined Plot",
            icon = icon("chart-line"),
            class = "custom-button"
          ),

          actionButton(
            "export_combined_plot_btn",
            "Download Plot",
            icon = icon("download"),
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

  # gene group analysis tab
  nav_panel(
    "Gene Group Analysis",
    fluidRow(
      column(
        width = 3,
        div(
          class = "sidebar-panel",
          h4("Gene Group Analysis Controls"),

          # pathway comparison mode toggle
          checkboxInput(
            "enable_pathway_comparison",
            label = div(
              icon("th"),
              strong(" Multi-Pathway Comparison Mode"),
              style = "display: inline;"
            ),
            value = FALSE
          ),

          # conditional UI for single vs multi-pathway mode
          conditionalPanel(
            condition = "input.enable_pathway_comparison == false",
            fileInput(
              "gene_group_file",
              "Upload Gene Groups (CSV)",
              accept = c("text/csv", ".csv")
            ),

            # functional annotation upload
            fileInput(
              "functional_annotations",
              "Upload Gene Annotations (Optional CSV)",
              accept = c("text/csv", ".csv"),
              placeholder = "Gene,Category,Order"
            ),
            textAreaInput(
              "gene_list",
              "Or paste gene list:",
              rows = 5,
              placeholder = "Enter genes separated by newlines, commas, or spaces"
            )
          ),
          conditionalPanel(
            condition = "input.enable_pathway_comparison == true",
            div(
              class = "alert alert-info py-2 px-3 mb-3",
              style = "font-size: 0.9em;",
              icon("info-circle"),
              strong(" Pathway Format:"),
              br(),
              "Pathway Name",
              br(),
              "GENE1, GENE2, GENE3",
              br(),
              tags$small("Separate pathways with blank line")
            ),
            textAreaInput(
              "pathway_definitions",
              "Define Pathways:",
              rows = 15,
              placeholder = "Ribophagy\nATG11, ATG19, UBI4, RPN4\n\nMitophagy\nATG11, ATG32, ATG33\n\nPexophagy\nATG11, ATG30, ATG36, PEX3"
            ),
            fileInput(
              "pathway_file",
              "Or upload pathway file:",
              accept = c("text/plain", ".txt")
            )
          ),

          # cross-species ortholog analysis checkbox
          div(
            class = "ortholog-analysis-panel",
            checkboxInput(
              "enable_ortholog_analysis",
              label = div(
                icon("sitemap"),
                strong(" Cross-Species Ortholog Analysis"),
                style = "display: inline;"
              ),
              value = FALSE
            ),
            div(
              style = "margin-left: 25px; font-size: 0.9em; color: #6c757d;",
              "When checked:",
              tags$ul(
                style = "margin-top: 5px; margin-bottom: 5px;",
                tags$li("Maps genes to orthologs across species"),
                tags$li("Shows coverage badges for each species"),
                tags$li("Enables multi-species expression comparison")
              )
            ),
            # remap button (only shows when ortholog analysis is enabled)
            conditionalPanel(
              condition = "input.enable_ortholog_analysis == true",
              div(
                class = "mt-2 mb-2",
                actionButton(
                  "remap_orthologs",
                  "Update Ortholog Mapping",
                  icon = icon("sync-alt"),
                  class = "btn btn-warning btn-sm w-100"
                ),
                downloadButton(
                  "download_coverage_stats",
                  "Download Coverage Stats",
                  class = "btn btn-secondary btn-sm w-100 mt-2"
                ),
                tags$small(
                  class = "text-muted d-block mt-1",
                  "Click after changing gene list to refresh mappings"
                )
              )
            ),
            # ortholog mapping results (appears after mapping)
            div(
              id = "ortholog_mapping_results",
              style = "display: none; margin-top: 10px;",
              uiOutput("ortholog_coverage_summary"),

              # paralog selection interface
              div(
                class = "mt-3 mb-3 p-3",
                style = "border: 1px solid #ddd; border-radius: 4px; background-color: #f8f9fa;",
                h6(icon("check-square"), " Select Orthologs to Plot"),
                tags$small(
                  class = "text-muted d-block mb-2",
                  "Choose which orthologs to include in visualization"
                ),
                uiOutput("paralog_selection_ui"),
                actionButton(
                  "select_all_paralogs",
                  "Select All",
                  icon = icon("check-double"),
                  class = "btn btn-sm btn-secondary mt-2 me-2"
                ),
                actionButton(
                  "select_first_paralogs",
                  "Select First Only",
                  icon = icon("check"),
                  class = "btn btn-sm btn-secondary mt-2"
                )
              ),
              actionButton(
                "view_ortholog_details",
                "View Detailed Mapping",
                icon = icon("table"),
                class = "btn btn-sm btn-info w-100 mt-2"
              )
            )
          ),
          uiOutput("group_analysis_species_ui"),

          # conditional controls based on mode
          conditionalPanel(
            condition = "input.enable_pathway_comparison == false",
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
                "Z-score" = "zscore",
                "Log2 Fold-Change (vs 0 min)" = "log2fc"
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
            )
          ),

          # pathway comparison specific controls
          conditionalPanel(
            condition = "input.enable_pathway_comparison == true",
            selectInput(
              "pathway_value_type",
              "Heatmap Values:",
              choices = list(
                "Fold-Change from Baseline (t=0)" = "foldchange",
                "Absolute Expression" = "absolute"
              ),
              selected = "foldchange"
            ),
            checkboxInput(
              "cluster_pathways",
              "Cluster Pathways",
              value = FALSE
            ),
            selectInput(
              "timepoint_display_mode",
              "Timepoint Display:",
              choices = list(
                "All Timepoints" = "all",
                "Comparable Only" = "comparable"
              ),
              selected = "all"
            )
          ),

          # significance testing control
          div(
            id = "significance_controls",
            style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #ccc;",
            h5("Significance Testing"),

            # toggle significance markers
            checkboxInput(
              "show_significance",
              "Show Statistical Significance",
              value = TRUE
            ),

            # only show other controls if significance is enabled
            conditionalPanel(
              condition = "input.show_significance == true && input.group_viz_type == 'bar'",

              # p-value threshold slider
              sliderInput(
                "significance_threshold",
                "P-value Threshold:",
                min = 0.001,
                max = 0.05,
                value = 0.05,
                step = 0.001
              ),

              # gene selector for significance testing
              selectizeInput(
                "sig_test_gene",
                "Select Gene to Test:",
                choices = NULL,
                options = list(
                  placeholder = "Select a gene"
                )
              ),

              # timepoint pair selector
              selectizeInput(
                "sig_test_timepoints",
                "Select Timepoint Comparison:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select timepoint pairs to compare",
                  maxItems = 3
                )
              ),

              # button to apply the selection
              actionButton(
                "apply_sig_test",
                "Apply Significance Test",
                class = "btn btn-primary btn-sm w-100 mt-2"
              )
            )
          ),

          # aggregation level control (only shown when ortholog analysis is enabled)
          conditionalPanel(
            condition = "input.enable_ortholog_analysis == true",
            radioButtons(
              "aggregation_level",
              "Aggregation Level:",
              choices = list(
                "Individual genes" = "genes",
                "Mean per species" = "species_mean"
              ),
              selected = "genes"
            )
          ),
          actionButton(
            "analyze_gene_groups",
            "Generate Visualization",
            icon = icon("chart-line"),
            class = "custom-button"
          ),
          actionButton(
            "export_group_plot_btn",
            "Download Plot",
            icon = icon("download"),
            class = "btn btn-secondary mt-2 w-100"
          ),
          conditionalPanel(
            condition = "input.enable_pathway_comparison == true",
            downloadButton(
              "download_pathway_data",
              "Download Pathway Data",
              class = "btn btn-secondary mt-2 w-100"
            )
          )
        )
      ),
      column(
        width = 9,
        div(
          class = "results-panel",
          # dynamic container to support either plotly (interactive) or static plot (publication)
          uiOutput("heatmap_container", style = "min-height: 500px;"),
          uiOutput("pathway_table_legend"),
          div(
            class = "mt-4",
            DTOutput("gene_group_table")
          ),

          # significance legend
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
                style = "font-size: 12px; margin-bottom: 0;"
              )
            )
          )
        )
      )
    )
  ),

  # pca tab
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

          # single species specific controls
          conditionalPanel(
            condition = "input.pca_type == 'single'",
            uiOutput("pca_species_ui")
          ),

          # multi-species aggregation method
          conditionalPanel(
            condition = "input.pca_type == 'multi'",
            selectInput(
              "hog_aggregation_method",
              "Multi-copy gene handling:",
              choices = list(
                "Eigengene" = "eigengene",
                "Mean expression" = "mean",
                "Median expression" = "median",
                "Single-copy genes only" = "single_only",
                "Highest expressed paralog" = "max_expr",
                "Most variable paralog" = "max_var",
                "Variance-weighted mean" = "var_weighted"
              ),
              selected = "eigengene"
            ),

            # info about the method
            div(
              class = "alert alert-info mt-2",
              style = "font-size: 0.85em; padding: 8px 12px;",
              uiOutput("hog_aggregation_info")
            )
          ),
          actionButton(
            "run_pca",
            "Run PCA",
            icon = icon("play"),
            class = "custom-button"
          ),

          # download button for the orthology matrix
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
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            h4("PCA Visualization", style = "margin: 0;"),
            actionButton("show_pca_export_modal", "Export Plot", icon = icon("download"), class = "btn btn-outline-primary btn-sm")
          ),
          uiOutput("pca_plot_container", style = "min-height: 500px;"),
          verbatimTextOutput("pca_debug_output")
        )
      )
    )
  ),

  # cross-species heatmap tab
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
            placeholder = "Enter genes separated by newlines, commas, or spaces (e.g., PHO4, PHO81, PHO84)"
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
          actionButton(
            "export_ortholog_heatmap_btn",
            "Download Heatmap",
            icon = icon("download"),
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

  # ridgeline plots tab
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
          actionButton(
            "export_ridgeline_btn",
            "Download Plot",
            icon = icon("download"),
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
