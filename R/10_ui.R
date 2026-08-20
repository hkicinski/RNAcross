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
      tags$script(HTML(sprintf(
        "window.RNX_TOOLS = %s; window.RNX_TOOL_GROUPS = %s;",
        jsonlite::toJSON(RNX_TOOLS, auto_unbox = TRUE),
        jsonlite::toJSON(RNX_TOOL_GROUPS)
      ))),
      includeScript("www/command_palette.js")
    ),
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
      # 1080 keeps it above content and modals but under the palette (1090)
      style = "z-index: 1080; background: var(--bs-body-bg); border: 1px solid var(--bs-border-color); border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); display: none;",
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
          // the palette input is an INPUT, so it would otherwise fall through to the search buttons
          if (document.body.classList.contains('rnx-palette-open')) return;
          if (document.activeElement && document.activeElement.id === 'rnx-palette-input') return;

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
    rnx_nav_css,

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

      /* Phylogenetic cladogram reveal (grows upward, click upon click) */
      .clado-branch {
        transition: stroke-dashoffset 0.6s ease-out;
      }
      .clado-leaf {
        opacity: 0;
        transform: scale(0.4);
        transform-box: fill-box;
        transform-origin: center;
        transition: opacity 0.5s ease, transform 0.5s ease;
      }
      .clado-leaf.show {
        opacity: 1;
        transform: scale(1);
      }
      .clado-gem {
        opacity: 0;
        transform: scale(0.2);
        transform-box: fill-box;
        transform-origin: center;
        transition: opacity 0.45s ease, transform 0.45s ease;
      }
      .clado-gem.show {
        opacity: 1;
        transform: scale(1);
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
        src = "47313572-sci-fi-sfx16-350847.mp3",
        type = "audio/mpeg",
        preload = "auto"
      )
    ),

    # glow trail container (outside splash screen for z-index)
    div(class = "glow-trail", id = "glow-trail-container"),

    # top bar: replaces both the tab strip and the old bg-primary title row
    div(
      class = "rnx-bar",
      div(
        class = "rnx-brand",
        icon("dna", class = "fas"),
        span("RNAcross")
      ),
      # the pill carries the gene in scope, so there is no second row
      div(
        class = "rnx-pill",
        tags$button(
          id = "rnx-trigger",
          type = "button",
          class = "rnx-trigger",
          role = "combobox",
          `aria-haspopup` = "dialog",
          `aria-controls` = "rnx-palette",
          title = "Search a gene, or jump to a tool (Ctrl+K)",
          icon("magnifying-glass"),
          uiOutput("rnx_pill_content", class = "rnx-slot")
        ),
        uiOutput("rnx_pill_clear", class = "rnx-slot")
      ),
      # chips are drawn by command_palette.js from localStorage
      div(id = "rnx-chips", class = "rnx-chips"),
      div(class = "rnx-divider"),
      div(
        class = "rnx-utils",
        actionButton(
          "show_version_info",
          label = NULL,
          icon = icon("bullhorn"),
          class = "rnx-util-btn",
          title = "What's New"
        ),
        actionButton("show_help",
          label = NULL,
          icon = icon("circle-question", class = "far"),
          class = "rnx-util-btn",
          title = "Tutorial"
        ),
        actionButton("show_settings",
          label = NULL,
          icon = icon("sliders"),
          class = "rnx-util-btn",
          title = "Plot settings"
        ),
        actionButton("theme_toggle",
          label = NULL,
          icon = icon("moon", verify_fa = FALSE),
          class = "rnx-util-btn",
          title = "Toggle dark/light mode"
        )
      ),
      uiOutput("rnx_export_slot", class = "rnx-slot")
    ),

    # command palette overlay; kept out of #splash-screen so it layers independently
    div(
      id = "rnx-palette",
      class = "rnx-scrim",
      `aria-hidden` = "true",
      div(
        id = "rnx-palette-card",
        class = "rnx-palette-card",
        role = "dialog",
        `aria-modal` = "true",
        `aria-label` = "Search genes and tools",
        div(
          class = "rnx-card-input",
          icon("magnifying-glass"),
          tags$input(
            id = "rnx-palette-input",
            type = "text",
            autocomplete = "off",
            spellcheck = "false",
            placeholder = "Search a gene, or jump to a tool…",
            role = "combobox",
            `aria-expanded` = "false",
            `aria-autocomplete` = "list",
            `aria-controls` = "rnx-palette-results",
            `aria-label` = "Search a gene, or jump to a tool"
          ),
          span(class = "rnx-esc", "esc")
        ),
        div(
          id = "rnx-palette-results",
          class = "rnx-results",
          role = "listbox",
          `aria-label` = "Results"
        ),
        div(
          class = "rnx-card-footer",
          span(span(class = "rnx-fkey", "↑↓"), "navigate"),
          span(span(class = "rnx-fkey", "↵"), "open"),
          span(span(class = "rnx-fkey", "⌘↵"), "open in new tab"),
          div(class = "rnx-spacer"),
          span(sprintf("%d tools · full gene index", length(RNX_TOOLS)))
        )
      )
    ),

    # javascript with SVG scaling fixes
    tags$script(HTML('
    // Auto-hide timer
    let splashTimer;
    let hasInteracted = false;

    // Cladogram (phylogenetic reveal) state
    var CLADO_MAXGEN = 4, cladoCurGen = 0, cladoWantGen = 0;
    var cladoBuilt = false, cladoGens = [], cladoLeaves = [], cladoGem = null;

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

      // Pre-build the (hidden) phylogenetic cladogram so clicks can grow it
      buildCladogram(svg);

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

    // Play the click sound (restart so rapid clicks retrigger it)
    function playClickSound() {
      var audio = document.getElementById("whoosh-sound");
      if (audio) {
        try { audio.currentTime = 0; } catch (e) {}
        var p = audio.play();
        if (p && p.catch) p.catch(function () {});
      }
    }

    // Small SVG element factory
    function mkSVG(tag, attrs) {
      var el = document.createElementNS("http://www.w3.org/2000/svg", tag);
      for (var k in attrs) el.setAttribute(k, attrs[k]);
      return el;
    }

    // Build the hidden phylogenetic cladogram in the logo SVG coordinate space:
    // rooted at the yeast hub (~421,540), climbing upward, and kept left of the
    // RNACROSS wordmark (x>=735) so it never overlaps the actual logo.
    function buildCladogram(svg) {
      if (cladoBuilt || !svg) return;
      cladoBuilt = true;

      // Ensure a soft-glow filter exists for the halo layer
      var defs = svg.querySelector("defs");
      if (defs && !svg.querySelector("#rnaSoft")) {
        var f = mkSVG("filter", { id: "rnaSoft", x: "-60%", y: "-60%", width: "220%", height: "220%" });
        f.appendChild(mkSVG("feGaussianBlur", { stdDeviation: 2.4 }));
        defs.appendChild(f);
      }

      var rootX = 421, rootY = 540, topY = 95, D = CLADO_MAXGEN;
      var leftX = 160, rightX = 680;
      var pal = ["#00F5FF", "#FF006E", "#8338EC", "#FFBE0B"];
      var gradId = ["neonBlue", "neonPink", "neonPurple", "neonOrange"];
      var nleaf = Math.pow(2, D);

      function gy(g) { return rootY - (rootY - topY) * g / D; }
      function cladeOf(g, i) { return g >= 2 ? Math.ceil((i + 1) / Math.pow(2, g - 2)) : 0; }

      // node x-positions per generation (bottom-up midpoints)
      var nodes = [];
      nodes[D] = [];
      for (var i0 = 0; i0 < nleaf; i0++) nodes[D][i0] = leftX + (rightX - leftX) * i0 / (nleaf - 1);
      for (var g0 = D - 1; g0 >= 0; g0--) {
        var pn = Math.pow(2, g0);
        nodes[g0] = [];
        for (var j = 0; j < pn; j++) nodes[g0][j] = (nodes[g0 + 1][2 * j] + nodes[g0 + 1][2 * j + 1]) / 2;
      }

      var grp = mkSVG("g", { id: "cladogram" });
      for (var gi = 1; gi <= D; gi++) cladoGens[gi] = [];

      for (var g = 1; g <= D; g++) {
        var nc = Math.pow(2, g);
        for (var i = 0; i < nc; i++) {
          var p = Math.floor(i / 2);
          var x1 = nodes[g - 1][p], y1 = gy(g - 1), x2 = nodes[g][i], y2 = gy(g);
          var cl = cladeOf(g, i);
          var col = cl === 0 ? "#e6f7ff" : pal[cl - 1];
          var halo = cl === 0 ? "#79d8ff" : pal[cl - 1];
          var d = "M" + x1 + "," + y1 + " L" + x2 + "," + y2;
          var hp = mkSVG("path", { d: d, pathLength: 100, "class": "clado-branch", stroke: halo, "stroke-width": 7, fill: "none", opacity: 0.22, filter: "url(#rnaSoft)", "stroke-linecap": "round" });
          var cp = mkSVG("path", { d: d, pathLength: 100, "class": "clado-branch", stroke: col, "stroke-width": 3.4, fill: "none", opacity: 0.95, "stroke-linecap": "round" });
          hp.style.strokeDasharray = 100; hp.style.strokeDashoffset = 100;
          cp.style.strokeDasharray = 100; cp.style.strokeDashoffset = 100;
          grp.appendChild(hp); grp.appendChild(cp);
          cladoGens[g].push(hp); cladoGens[g].push(cp);

          if (g === D) {
            var cl2 = cladeOf(D, i);
            var lg = mkSVG("g", { "class": "clado-leaf" });
            if (i % 4 === 2) {
              var inner = mkSVG("g", { transform: "translate(" + x2 + "," + y2 + ")" });
              inner.appendChild(mkSVG("ellipse", { rx: 15, ry: 17, fill: "#000814", stroke: "url(#" + gradId[cl2 - 1] + ")", "stroke-width": 4 }));
              inner.appendChild(mkSVG("circle", { r: 7, fill: pal[cl2 - 1], opacity: 0.5, filter: "url(#rnaSoft)" }));
              inner.appendChild(mkSVG("circle", { r: 4, fill: pal[cl2 - 1], opacity: 0.95 }));
              lg.appendChild(inner);
            } else {
              lg.appendChild(mkSVG("circle", { cx: x2, cy: y2, r: 4.5, fill: pal[cl2 - 1], opacity: 0.9 }));
            }
            grp.appendChild(lg);
            cladoLeaves.push(lg);
          }
        }
      }

      // Root gem (common ancestor / orthogroup mark; echoes the brand diamond)
      cladoGem = mkSVG("g", { "class": "clado-gem" });
      cladoGem.appendChild(mkSVG("polygon", { points: rootX + "," + (rootY - 20) + " " + (rootX + 20) + "," + rootY + " " + rootX + "," + (rootY + 20) + " " + (rootX - 20) + "," + rootY, fill: "#00141c", stroke: "url(#neonGrad)", "stroke-width": 5 }));
      cladoGem.appendChild(mkSVG("circle", { cx: rootX, cy: rootY, r: 5, fill: "#FFFFFF" }));
      grp.appendChild(cladoGem);

      // Insert behind the wordmark so the actual logo always stays on top
      var typo = svg.querySelector("#modern-typography");
      if (typo) svg.insertBefore(grp, typo); else svg.appendChild(grp);
    }

    // Draw one generation of branches (and pop the leaves on the final one)
    function revealGen(k) {
      var arr = cladoGens[k];
      if (arr) arr.forEach(function (el) { el.style.strokeDashoffset = 0; });
      if (k >= CLADO_MAXGEN) cladoLeaves.forEach(function (l) { l.classList.add("show"); });
    }
    function growTo(n) {
      while (cladoCurGen < n) { cladoCurGen++; revealGen(cladoCurGen); }
    }
    function bloomGem() { if (cladoGem) cladoGem.classList.add("show"); }

    // Subsequent clicks grow the tree one generation each
    function growthClick(e) {
      if (e && e.target && e.target.closest && e.target.closest("#splash-skip")) return;
      playClickSound();
      if (cladoWantGen < CLADO_MAXGEN) { cladoWantGen++; growTo(cladoWantGen); }
    }

    // Handle the first yeast click: scatter the cells, then start the cladogram
    function handleYeastClick() {
      if (hasInteracted) return; // first interaction only
      hasInteracted = true;
      clearTimeout(splashTimer);
      playClickSound();

      const svgElement = document.querySelector("#splash-logo svg, #svg-wrapper svg");
      const cellsWrapper = svgElement.querySelector("#cells-wrapper, #cells");
      const cells = svgElement.querySelectorAll(\'#cells > g[id^="cell-"], #cells-wrapper > g[id^="cell-"]\');

      if (cellsWrapper) cellsWrapper.classList.add("spinning-animation");
      createGlowTrails();

      // Immediately remove the old RNA "spaghetti", its flow particles, and the
      // central lens so none of it flashes while the cells spin and fly away.
      var oldRna = [];
      ["#rna-helixes", "#rna-flow-particles"].forEach(function (sel) {
        var el = svgElement.querySelector(sel); if (el) oldRna.push(el);
      });
      // central convergence lens = the non-cell <g> inside #cells
      svgElement.querySelectorAll("#cells > g:not([id])").forEach(function (el) { oldRna.push(el); });
      oldRna.forEach(function (el) { el.style.transition = "opacity 0.2s ease"; el.style.opacity = 0; });

      // The cells fly off, then the cladogram roots where they were (gem + first split)
      setTimeout(function () { disperseSVGCells(cells); }, 800);
      setTimeout(function () { bloomGem(); growTo(1); }, 850);
      cladoWantGen = 1;

      // After the bloom, further clicks grow it (delayed so this first click does not count)
      var splash = document.getElementById("splash-screen");
      if (splash) setTimeout(function () { splash.addEventListener("click", growthClick); }, 900);

      // The app initializes after a few seconds regardless: gently finish the tree, then dismiss
      setTimeout(function () {
        (function step() {
          if (cladoCurGen < CLADO_MAXGEN) { cladoWantGen = cladoCurGen + 1; growTo(cladoWantGen); setTimeout(step, 280); }
          else setTimeout(hideSplash, 900);
        })();
      }, 3000);
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
              Shiny.setInputValue("trigger_version_modal", { seen: (seenVersion || ""), rand: Math.random() });
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
              Shiny.setInputValue("trigger_version_modal", { seen: (seenVersion || ""), rand: Math.random() });
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
      function showRNAcrossFatalOverlay() {
        if (document.getElementById('rnacross-fatal-overlay')) return;

        var shinyDimmer = document.getElementById('shiny-disconnected-overlay');
        if (shinyDimmer && shinyDimmer.parentNode) shinyDimmer.parentNode.removeChild(shinyDimmer);

        var overlay = document.createElement('div');
        overlay.id = 'rnacross-fatal-overlay';
        overlay.style.cssText = 'position:fixed;top:0;left:0;right:0;bottom:0;background:#1a1a2e;z-index:2147483647;display:flex;align-items:center;justify-content:center;text-align:center;color:#e0e0e0;font-family:Inter,Segoe UI,sans-serif;pointer-events:auto;';
        overlay.innerHTML =
          '<div style=\"max-width:500px;padding:40px;background:#16213e;border-radius:16px;border:1px solid #1a1a4e;pointer-events:auto;\">' +
            '<h2 style=\"color:#ff6b6b;margin-bottom:16px;font-size:1.6rem;\">Something went wrong</h2>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">The app has encountered an error or lost connection to the server.</p>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">Please try reloading the page. If the issue persists, report it:</p>' +
            '<p style=\"margin-bottom:12px;line-height:1.6;font-size:0.95rem;\">' +
              '<a href=\"https://github.com/hkicinski/RNAcross/issues\" target=\"_blank\" style=\"color:#6ea8fe;text-decoration:underline;pointer-events:auto;\">GitHub Issues</a>' +
              ' &nbsp;|&nbsp; ' +
              '<a href=\"mailto:hkicinski@uiowa.edu\" style=\"color:#6ea8fe;text-decoration:underline;pointer-events:auto;\">hkicinski@uiowa.edu</a>' +
            '</p>' +
            '<button id=\"rnacross-fatal-reload\" style=\"display:inline-block;margin-top:20px;padding:10px 28px;background:#0d6efd;color:white;border:none;border-radius:8px;font-size:1rem;cursor:pointer;pointer-events:auto;\">Reload App</button>' +
          '</div>';
        document.body.appendChild(overlay);

        var reloadBtn = document.getElementById('rnacross-fatal-reload');
        if (reloadBtn) reloadBtn.addEventListener('click', function() { location.reload(); });

        var waiter = document.querySelector('.waiter-overlay');
        if (waiter) waiter.style.display = 'none';

        var splash = document.getElementById('splash-screen');
        if (splash) splash.style.display = 'none';
      }

      $(document).on('shiny:disconnected', function(event) {
        showRNAcrossFatalOverlay();
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
      #shiny-disconnected-overlay { display: none !important; }
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

      # the Gene Query Hub moved into the top bar, but its inputs stay in the DOM:
      # the whole ~240-line search observer still drives itself off these two ids.
      div(
        id = "rnx_legacy_query",
        style = "display: none;",
        textInput(
          "global_gene_query",
          label = NULL,
          placeholder = "Enter gene name or ID (e.g., PHO4, YFR034C, CAGL0D05170g)",
          width = "100%"
        ),
        actionButton(
          "global_search_button",
          "Search",
          icon = icon("search"),
          class = "btn btn-primary"
        ),
        div(
          id = "query_status_container",
          style = "display: none;",
          class = "query-status",
          uiOutput("query_status")
        )
      ),

      # launchpad or resume card, whenever there is no query result to show
      uiOutput("rnx_explorer_landing"),

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
              div(
                class = "mb-2",
                actionButton("toggle_tree_editor", "Edit appearance",
                  icon = icon("wand-magic-sparkles"),
                  class = "btn btn-sm btn-outline-primary"
                ),
                actionButton("export_phylo_tree_btn", "Export tree",
                  icon = icon("download"),
                  class = "btn btn-sm btn-outline-primary ms-2"
                )
              ),
              shinyjs::hidden(div(
                id = "tree_aesthetic_editor",
                class = "card mb-3",
                div(
                  class = "card-header py-2 d-flex justify-content-between align-items-center",
                  strong("Tree Aesthetic Editor"),
                  actionButton("close_tree_editor", "", icon = icon("times"),
                    class = "btn-sm btn-link p-0 text-decoration-none border-0"
                  )
                ),
                div(
                  class = "card-body",
                  style = "max-height: 460px; overflow-y: auto;",
                  uiOutput("tree_editor_ui")
                )
              )),
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
            tabsetPanel(
              id = "similarity_view_tabs",
              tabPanel("Interactive",
                div(class = "mt-2 text-muted", style = "font-size: 0.85em;",
                  icon("wand-magic-sparkles"),
                  " Click any element (title, axes, legend, a trace) to edit it. Edits carry over to the Publication tab."),
                plotlyOutput("similarity_plot", height = "500px")),
              tabPanel("Publication (ggprism)",
                div(class = "mt-2 mb-2",
                  actionButton("export_similarity_trajectory_btn", "Export trajectory",
                    icon = icon("download"), class = "btn btn-sm btn-outline-primary"),
                  actionButton("export_similarity_null_btn", "Export null distribution",
                    icon = icon("download"), class = "btn btn-sm btn-outline-primary ms-2"),
                  span(class = "text-muted ms-2", style = "font-size: 0.85em;",
                    "Reflects the edits made on the Interactive tab.")),
                plotOutput("similarity_prism_plot", height = "760px"))
            )
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
              p("Required: ", tags$code("Sample"), ", plus one column describing each sample."),
              tags$small(
                class = "text-muted d-block mb-2",
                "That column can be named anything (", tags$code("Timepoint"), ", ",
                tags$code("Dose"), ", ", tags$code("Genotype"), ", ",
                tags$code("Phase"), "). You say which one it is in Step 6. ",
                tags$code("Replicate"), " is optional."
              ),
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

            # step 5b: gene trees, what the Gene Explorer draws
            div(
              class = "upload-step",
              h5(
                "Step 5b: Gene Trees", icon("sitemap"),
                span("(Optional)", class = "badge badge-secondary ml-2")
              ),
              p("Newick trees, one per orthogroup. Without these the Gene Explorer has no tree to draw."),
              fileInput("upload_gene_trees", "Gene trees:",
                accept = c(".zip", ".tsv", ".csv", ".txt", ".nwk", ".newick", ".tree", ".tre"),
                multiple = TRUE
              ),
              tags$small(
                class = "text-muted d-block",
                "Accepts an OrthoFinder ", tags$code("Gene_Trees/"), " folder zipped, ",
                "loose ", tags$code(".nwk"), " files named after their orthogroup, ",
                "or a two-column table of orthogroup and newick."
              ),
              hr()
            ),

            # step 6: describe the design (the wizard itself lives in the Design tab)
            div(
              class = "upload-step",
              h5("Step 6: Describe Your Design", icon("compass-drafting")),
              p("What the axis of your experiment is, and what it is measured against."),
              actionButton("goto_design_tab", "Open the Design step",
                icon = icon("arrow-right"), class = "btn btn-sm btn-outline-primary mb-2"
              ),
              uiOutput("design_status"),
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
                tabPanel("Orthology", DTOutput("upload_ortho_preview")),
                tabPanel(
                  "Design",
                  value = "design",
                  div(
                    class = "mt-2 mb-3",
                    h5("Step 6: Describe Your Design"),
                    tags$small(
                      class = "text-muted",
                      "Pick the column that is your experimental axis and say how its levels relate: ",
                      "spaced numbers (a dose or time series), ordered but unspaced (growth phases), ",
                      "or unordered (genotypes). Everything downstream reads this."
                    )
                  ),
                  uiOutput("design_wizard_ui"), #rendered server-side (R/13 sourced after R/10)
                  div(
                    class = "mt-3",
                    actionButton("apply_design", "Apply design", class = "btn btn-primary"),
                    uiOutput("design_review")
                  )
                )
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
  # the five titles below double as their nav value; spelled out so the palette
  # table and the existing updateTabsetPanel calls cannot drift apart
  nav_panel(
    "Comparative View",
    value = "Comparative View",
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
    value = "Gene Group Analysis",
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
    value = "PCA",
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
    value = "Cross-Species Heatmap",
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
    value = "Ridgeline Plots",
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
