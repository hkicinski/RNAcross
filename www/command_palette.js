//command palette for the search-first navbar (design 1c)
//genes come from the server (rnx_query -> rnx_results); tools are filtered here.
//nothing navigates directly: everything goes back to R as rnx_open_gene / rnx_goto.
(function () {
  "use strict";

  var LS_GENES = "rnacross_recent_genes";
  var LS_TOOLS = "rnacross_recent_tools";
  var MAX_RECENT_GENES = 8;
  var MAX_CHIPS = 3;
  var MAX_ROWS = 5;
  var DEBOUNCE_MS = 150;
  var DEFAULT_CHIPS = ["gene_explorer", "Comparative View", "Gene Group Analysis"];

  var tools = [];
  var groups = ["Explore", "Compare", "Analyze", "Data"];

  var state = {
    open: false,
    query: "",
    genes: [],
    genesFor: null,
    rows: [],
    sel: 0,
    lastFocused: null,
    timer: null,
    activeTab: null,
    gene: null
  };

  //the app-wide keyup handler in R/10 clicks the first visible search button on Enter.
  //we swallow the keyup that follows our own Enter so it does not fire a second search.
  var swallowEnterUp = false;
  var swallowTimer = null;

  var el = {};

  function byId(id) { return document.getElementById(id); }

  function lsGet(key, fallback) {
    try {
      var raw = localStorage.getItem(key);
      if (!raw) return fallback;
      var val = JSON.parse(raw);
      return Array.isArray(val) ? val : fallback;
    } catch (e) { return fallback; }
  }

  function lsSet(key, val) {
    try { localStorage.setItem(key, JSON.stringify(val)); } catch (e) {}
  }

  function toolByValue(value) {
    for (var i = 0; i < tools.length; i++) {
      if (tools[i].value === value) return tools[i];
    }
    return null;
  }

  // ---------- chips ----------

  function chipTools() {
    var stored = lsGet(LS_TOOLS, []);
    var out = [];
    var i;
    for (i = 0; i < stored.length && out.length < MAX_CHIPS; i++) {
      var t = toolByValue(stored[i]);
      if (t && out.indexOf(t) === -1) out.push(t);
    }
    for (i = 0; i < DEFAULT_CHIPS.length && out.length < MAX_CHIPS; i++) {
      var d = toolByValue(DEFAULT_CHIPS[i]);
      if (d && out.indexOf(d) === -1) out.push(d);
    }
    return out;
  }

  //recent, but position-stable: a tool already on the bar stays where it is so the
  //chips do not reshuffle under the cursor every time you switch tabs.
  function rememberTool(value) {
    if (!toolByValue(value)) return;
    var stored = lsGet(LS_TOOLS, []);
    if (stored.indexOf(value) === -1) {
      stored.unshift(value);
      stored = stored.slice(0, MAX_CHIPS);
      lsSet(LS_TOOLS, stored);
    }
  }

  function renderChips() {
    if (!el.chips) return;
    var list = chipTools();
    var html = "";
    for (var i = 0; i < list.length; i++) {
      var t = list[i];
      var active = t.value === state.activeTab ? " rnx-chip-active" : "";
      html += '<button type="button" class="rnx-chip' + active + '" data-rnx-tool="' +
        esc(t.value) + '" title="' + esc(t.label) + '"' +
        (active ? ' aria-current="page"' : "") + '>' +
        '<i class="' + esc(t.icon) + '" aria-hidden="true"></i><span>' + esc(t.chip) + "</span></button>";
    }
    html += '<button type="button" class="rnx-chip rnx-chip-all" id="rnx-all-tools" title="All tools">' +
      '<i class="fas fa-grip" aria-hidden="true"></i><span>All tools</span></button>';
    el.chips.innerHTML = html;
  }

  // ---------- recent genes ----------

  //stored as [{gene, at}] MRU; older builds wrote bare strings, so read both
  function recentGenes() {
    return lsGet(LS_GENES, []).map(function (r) {
      return typeof r === "string" ? { gene: r, at: null } : r;
    }).filter(function (r) { return r && r.gene; });
  }

  function rememberGene(gene) {
    var recent = recentGenes().filter(function (r) {
      return r.gene.toUpperCase() !== gene.toUpperCase();
    });
    recent.unshift({ gene: gene, at: new Date().toISOString() });
    lsSet(LS_GENES, recent.slice(0, MAX_RECENT_GENES));
    pushRecents();
  }

  //the launchpad card is rendered by R, so the list has to go up to the server.
  //sent as a JSON string: an array of objects comes through to R flattened.
  function pushRecents() {
    shinySet("rnx_recent_genes", JSON.stringify(recentGenes()));
  }

  // ---------- rows ----------

  function esc(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;").replace(/'/g, "&#39;");
  }

  function filterTools(q) {
    var needle = q.toLowerCase();
    var out = [];
    for (var i = 0; i < tools.length && out.length < MAX_ROWS; i++) {
      var t = tools[i];
      var hay = (t.label + " " + t.group + " " + (t.syn || "")).toLowerCase();
      if (hay.indexOf(needle) !== -1) out.push(t);
    }
    return out;
  }

  //flat row model; the sections are only captions drawn between the rows
  function buildRows() {
    var rows = [];
    var i;

    if (!state.query) {
      var recent = recentGenes().slice(0, MAX_ROWS);
      for (i = 0; i < recent.length; i++) {
        rows.push({
          kind: "gene", gene: recent[i].gene, label: recent[i].gene, detail: "recent",
          section: i === 0 ? "Recent genes" : null
        });
      }
      var seen = {};
      for (var g = 0; g < groups.length; g++) {
        var first = true;
        for (i = 0; i < tools.length; i++) {
          if (tools[i].group !== groups[g]) continue;
          rows.push({
            kind: "tool", tool: tools[i],
            section: first ? groups[g] : null,
            divider: first && rows.length > 0
          });
          seen[tools[i].value] = true;
          first = false;
        }
      }
      for (i = 0; i < tools.length; i++) {
        if (!seen[tools[i].value]) rows.push({ kind: "tool", tool: tools[i], section: null });
      }
      return rows;
    }

    var genes = state.genesFor === state.query.toUpperCase() ? state.genes : [];
    var matched = filterTools(state.query);

    //a query that IS a tool name puts tools first, otherwise "PCA" would open
    //the gene PCA1 and the tool would be unreachable by typing its own name
    var needle = state.query.trim().toLowerCase();
    var toolsFirst = matched.some(function (t) { return t.label.toLowerCase() === needle; });

    var pushGenes = function () {
      for (var j = 0; j < genes.length && j < MAX_ROWS; j++) {
        rows.push({
          kind: "gene", gene: genes[j].gene, label: genes[j].label,
          detail: genes[j].detail,
          section: j === 0 ? "Genes" : null,
          divider: j === 0 && rows.length > 0
        });
      }
    };
    var pushTools = function () {
      for (var j = 0; j < matched.length; j++) {
        rows.push({
          kind: "tool", tool: matched[j],
          section: j === 0 ? "Tools" : null,
          divider: j === 0 && rows.length > 0
        });
      }
    };

    if (toolsFirst) { pushTools(); pushGenes(); } else { pushGenes(); pushTools(); }
    return rows;
  }

  function render() {
    state.rows = buildRows();
    if (state.sel >= state.rows.length) state.sel = state.rows.length ? state.rows.length - 1 : 0;
    if (state.sel < 0) state.sel = 0;

    if (!state.rows.length) {
      el.results.innerHTML = '<div class="rnx-empty">No gene or tool matches "' + esc(state.query) + '"</div>';
      el.input.removeAttribute("aria-activedescendant");
      return;
    }

    var html = "";
    for (var i = 0; i < state.rows.length; i++) {
      var r = state.rows[i];
      if (r.divider) html += '<div class="rnx-hairline"></div>';
      if (r.section) html += '<div class="rnx-section">' + esc(r.section) + "</div>";
      var sel = i === state.sel;
      html += '<div class="rnx-row' + (sel ? " rnx-sel" : "") + '" role="option" id="rnx-row-' + i +
        '" aria-selected="' + (sel ? "true" : "false") + '" data-rnx-index="' + i + '">';
      if (r.kind === "gene") {
        html += '<i class="fas fa-dna" aria-hidden="true"></i>' +
          '<span class="rnx-row-gene">' + esc(r.label) + "</span>" +
          '<span class="rnx-row-detail">' + esc(r.detail || "") + "</span>" +
          '<span class="rnx-spacer"></span>' +
          '<span class="rnx-row-hint">open in Gene Explorer</span>' +
          '<span class="rnx-row-enter">&#8629;</span>';
      } else {
        html += '<i class="' + esc(r.tool.icon) + '" aria-hidden="true"></i>' +
          '<span class="rnx-row-tool">' + esc(r.tool.label) + "</span>" +
          '<span class="rnx-spacer"></span>' +
          '<span class="rnx-row-hint">open tool</span>' +
          '<span class="rnx-row-group">' + esc(r.tool.group) + "</span>" +
          '<span class="rnx-row-enter">&#8629;</span>';
      }
      html += "</div>";
    }
    el.results.innerHTML = html;
    el.input.setAttribute("aria-activedescendant", "rnx-row-" + state.sel);
    scrollSelIntoView();
  }

  function scrollSelIntoView() {
    var node = el.results.querySelector('[data-rnx-index="' + state.sel + '"]');
    if (node && node.scrollIntoView) node.scrollIntoView({ block: "nearest" });
  }

  function move(delta) {
    if (!state.rows.length) return;
    state.sel = (state.sel + delta + state.rows.length) % state.rows.length;
    render();
  }

  // ---------- open / close ----------

  function open(initialQuery) {
    //the splash owns the screen until it is dismissed, and a modal (settings,
    //version history, export) would end up underneath us at z-index 1055
    if (document.body.classList.contains("splash-active")) return;
    if (document.body.classList.contains("modal-open") ||
        document.querySelector(".modal.show, .modal.in, #shiny-modal-wrapper .modal")) return;
    if (!state.open) {
      state.lastFocused = document.activeElement;
      state.open = true;
      document.body.classList.add("rnx-palette-open");
      el.scrim.classList.add("rnx-open");
      el.scrim.setAttribute("aria-hidden", "false");
      el.input.setAttribute("aria-expanded", "true");
    }
    if (typeof initialQuery === "string") {
      el.input.value = initialQuery;
      state.query = initialQuery;
    }
    state.sel = 0;
    render();
    el.input.focus();
    el.input.select();
    if (state.query) requestResults(state.query);
  }

  function close() {
    if (!state.open) return;
    state.open = false;
    document.body.classList.remove("rnx-palette-open");
    el.scrim.classList.remove("rnx-open");
    el.scrim.setAttribute("aria-hidden", "true");
    el.input.setAttribute("aria-expanded", "false");
    el.input.value = "";
    state.query = "";
    state.genes = [];
    state.genesFor = null;
    state.sel = 0;
    clearTimeout(state.timer);
    //never hand focus back into the card we just hid (reopening leaves it there)
    var back = state.lastFocused;
    if (back && back.focus && !el.scrim.contains(back)) {
      try { back.focus(); } catch (e) {}
    } else {
      el.input.blur();
    }
    state.lastFocused = null;
  }

  // ---------- shiny bridges ----------

  function shinySet(name, value) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  function requestResults(q) {
    shinySet("rnx_query", q);
  }

  function onType() {
    state.query = el.input.value;
    state.sel = 0;
    clearTimeout(state.timer);
    if (!state.query.trim()) {
      state.genes = [];
      state.genesFor = null;
      render();
      return;
    }
    render();
    state.timer = setTimeout(function () { requestResults(state.query); }, DEBOUNCE_MS);
  }

  function activate(index, newTab) {
    var r = state.rows[index];
    if (!r) {
      //nothing matched but there is text: still run it as a gene query
      var raw = state.query.trim();
      if (raw) { runGene(raw, newTab); }
      return;
    }
    if (r.kind === "gene") runGene(r.gene, newTab);
    else goTool(r.tool.value, newTab);
  }

  function runGene(gene, newTab) {
    rememberGene(gene);
    if (newTab) {
      window.open(deepLink("gene_explorer", gene), "_blank");
      close();
      return;
    }
    close();
    shinySet("rnx_open_gene", { gene: gene, rand: Math.random() });
  }

  function goTool(value, newTab) {
    if (newTab) {
      window.open(deepLink(value, null), "_blank");
      close();
      return;
    }
    close();
    shinySet("rnx_goto", { value: value, rand: Math.random() });
  }

  function deepLink(tab, gene) {
    var url = window.location.pathname + "?tab=" + encodeURIComponent(tab);
    if (gene) url += "&gene=" + encodeURIComponent(gene);
    return url;
  }

  // ---------- top bar state ----------

  function setActiveTab(value) {
    state.activeTab = value;
    rememberTool(value);
    renderChips();
  }

  //the pill's contents are rendered server-side; we only keep the name so
  //Cmd+K can preselect it
  function setGene(gene) {
    state.gene = gene || null;
  }

  // ---------- wiring ----------

  function bind() {
    el.scrim = byId("rnx-palette");
    el.card = byId("rnx-palette-card");
    el.input = byId("rnx-palette-input");
    el.results = byId("rnx-palette-results");
    el.trigger = byId("rnx-trigger");
    el.chips = byId("rnx-chips");
    if (!el.scrim || !el.input) return false;

    tools = window.RNX_TOOLS || [];
    if (window.RNX_TOOL_GROUPS) groups = window.RNX_TOOL_GROUPS;
    renderChips();

    //opening with the scoped gene preselected: type to replace it, Enter to re-run it
    el.trigger.addEventListener("click", function () { open(state.gene || ""); });

    el.chips.addEventListener("click", function (e) {
      if (e.target.closest && e.target.closest("#rnx-all-tools")) { open(""); }
    });

    el.scrim.addEventListener("mousedown", function (e) {
      if (e.target === el.scrim) close();
    });

    el.results.addEventListener("mousemove", function (e) {
      var row = e.target.closest ? e.target.closest("[data-rnx-index]") : null;
      if (!row) return;
      var idx = parseInt(row.getAttribute("data-rnx-index"), 10);
      if (idx !== state.sel) { state.sel = idx; render(); }
    });

    el.results.addEventListener("click", function (e) {
      var row = e.target.closest ? e.target.closest("[data-rnx-index]") : null;
      if (!row) return;
      activate(parseInt(row.getAttribute("data-rnx-index"), 10), e.metaKey || e.ctrlKey);
    });

    el.input.addEventListener("input", onType);

    el.input.addEventListener("keydown", function (e) {
      if (!state.open) return;
      if (e.key === "ArrowDown") { e.preventDefault(); move(1); }
      else if (e.key === "ArrowUp") { e.preventDefault(); move(-1); }
      else if (e.key === "Enter") {
        e.preventDefault();
        armEnterSwallow();
        activate(state.sel, e.metaKey || e.ctrlKey);
      } else if (e.key === "Escape") { e.preventDefault(); close(); }
      else if (e.key === "Tab") { e.preventDefault(); }
    });

    //the pill, the export button and the whole landing are re-rendered by Shiny,
    //so everything they contain is delegated from the document instead of bound
    document.addEventListener("click", function (e) {
      if (!e.target.closest) return;

      var tool = e.target.closest("[data-rnx-tool]");
      if (tool) { e.preventDefault(); goTool(tool.getAttribute("data-rnx-tool"), false); return; }

      var gene = e.target.closest("[data-rnx-gene]");
      if (gene) { e.preventDefault(); runGene(gene.getAttribute("data-rnx-gene"), false); return; }

      if (e.target.closest("#rnx-scope-clear")) {
        e.preventDefault();
        shinySet("rnx_clear_gene", Math.random());
      } else if (e.target.closest("#rnx-export")) {
        e.preventDefault();
        shinySet("rnx_export", Math.random());
      } else if (e.target.closest("#rnx-start-fresh")) {
        e.preventDefault();
        shinySet("rnx_start_fresh", Math.random());
      } else if (e.target.closest("#rnx-resume-comparative")) {
        e.preventDefault();
        shinySet("rnx_resume_jump", { target: "Comparative View", rand: Math.random() });
      } else if (e.target.closest("#rnx-resume-similarity")) {
        e.preventDefault();
        shinySet("rnx_resume_jump", { target: "similarity_search", rand: Math.random() });
      }
    });

    document.addEventListener("keydown", function (e) {
      if ((e.metaKey || e.ctrlKey) && (e.key === "k" || e.key === "K")) {
        e.preventDefault();
        if (state.open) close(); else open(state.gene || "");
        return;
      }
      if (e.key === "Escape" && state.open) { e.preventDefault(); close(); }
    });

    //capture phase, so it beats the bubble-phase jQuery handler on document
    document.addEventListener("keyup", function (e) {
      if (swallowEnterUp && (e.key === "Enter" || e.which === 13)) {
        swallowEnterUp = false;
        clearTimeout(swallowTimer);
        e.stopPropagation();
        if (e.stopImmediatePropagation) e.stopImmediatePropagation();
      }
    }, true);

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("rnx_results", function (msg) {
        state.genes = msg.genes || [];
        state.genesFor = (msg.query || "").toUpperCase();
        if (state.open) render();
      });
      Shiny.addCustomMessageHandler("rnx_active_tab", function (msg) {
        setActiveTab(msg.value);
      });
      Shiny.addCustomMessageHandler("rnx_scope", function (msg) {
        setGene(msg && typeof msg.gene === "string" && msg.gene ? msg.gene : null);
      });
      //"Start fresh": drop the cached gene only. NOT the clearSession handler,
      //which also wipes rnacross_seen_version and reloads into the What's New modal.
      //Shiny rejects a zero-argument handler, so keep the unused parameter
      Shiny.addCustomMessageHandler("rnx_forget_query", function (msg) {
        try {
          var raw = localStorage.getItem("rnacross_session");
          if (!raw) return;
          var saved = JSON.parse(raw);
          delete saved.current_query;
          localStorage.setItem("rnacross_session", JSON.stringify(saved));
        } catch (e) {}
      });
    }
    //setInputValue before the socket is up is dropped, so wait for the session
    if (window.jQuery) jQuery(document).on("shiny:connected", pushRecents);
    else pushRecents();
    return true;
  }

  // ---------- dark mode ----------

  //theme_toggle swaps the bootswatch stylesheet and nothing else: its addCssClass
  //call targets #html, which does not exist, so no class ever lands. Read the
  //bootstrap variable instead and mark <html> ourselves.
  function isDarkTheme() {
    var bg = getComputedStyle(document.documentElement)
      .getPropertyValue("--bs-body-bg").trim();
    if (!bg) return false;
    var r, g, b, m;
    if (bg.charAt(0) === "#") {
      var hex = bg.slice(1);
      if (hex.length === 3) hex = hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2];
      if (hex.length < 6) return false;
      r = parseInt(hex.slice(0, 2), 16);
      g = parseInt(hex.slice(2, 4), 16);
      b = parseInt(hex.slice(4, 6), 16);
    } else {
      m = bg.match(/(\d+(?:\.\d+)?)/g);
      if (!m || m.length < 3) return false;
      r = +m[0]; g = +m[1]; b = +m[2];
    }
    return (0.299 * r + 0.587 * g + 0.114 * b) < 128;
  }

  function syncTheme() {
    document.documentElement.classList.toggle("rnx-dark", isDarkTheme());
  }

  //the swapped stylesheet loads and reparses on its own schedule, so a couple of
  //fixed timeouts miss it. Watch the variable itself for a few seconds instead.
  var themeBurst = null, themeBurstEnd = 0;
  function scheduleThemeSync() {
    syncTheme();
    themeBurstEnd = (window.performance ? performance.now() : 0) + 6000;
    if (themeBurst) return;
    themeBurst = setInterval(function () {
      syncTheme();
      if ((window.performance ? performance.now() : themeBurstEnd + 1) >= themeBurstEnd) {
        clearInterval(themeBurst);
        themeBurst = null;
      }
    }, 250);
  }

  function watchTheme() {
    scheduleThemeSync();
    if (window.MutationObserver) {
      new MutationObserver(scheduleThemeSync).observe(document.head, {
        childList: true, subtree: true, attributes: true, attributeFilter: ["href"]
      });
    }
    if (window.jQuery) jQuery(document).on("shiny:idle", scheduleThemeSync);
    document.addEventListener("click", function (e) {
      if (e.target.closest && e.target.closest("#theme_toggle")) scheduleThemeSync();
    });
  }

  function armEnterSwallow() {
    swallowEnterUp = true;
    clearTimeout(swallowTimer);
    swallowTimer = setTimeout(function () { swallowEnterUp = false; }, 500);
  }

  function init() {
    watchTheme();
    if (!bind()) setTimeout(bind, 300);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
