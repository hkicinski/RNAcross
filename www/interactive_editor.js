(function() {
  // Global registry for our customized plots
  window.interactivePlots = window.interactivePlots || {};

  window.initInteractiveEditor = function(plotEl) {
    if (!plotEl || !plotEl.id) return;
    
    // Avoid double initialization
    if (window.interactivePlots[plotEl.id]) {
        // If re-initialized due to full render, we need to re-bind listeners
        // but we preserve the originalState if it exists
    } else {
        window.interactivePlots[plotEl.id] = {
            originalState: null
        };
    }
    
    const state = window.interactivePlots[plotEl.id];
    
    // Function to safely clone objects avoiding circular references
    const clonePlotlyState = function(obj) {
        try {
            return JSON.parse(JSON.stringify(obj));
        } catch (e) {
            console.error("Could not clone state", e);
            return {};
        }
    };

    // Function to attach DOM listeners
    // Function to attach DOM listeners
    const bindListeners = function() {
        if (plotEl._hasEditorListener) return;
        
        plotEl.addEventListener('click', function(e) {
            let target = e.target;
            if (!target) return;
            
            // Helpers for RStudio Viewer compatibility (SVG classList and closest are often missing)
            const matchesFn = Element.prototype.matches || Element.prototype.msMatchesSelector || Element.prototype.webkitMatchesSelector;
            const findAncestor = function(el, sel) {
                let curr = el;
                while (curr && curr.nodeType === 1) {
                    if (matchesFn && matchesFn.call(curr, sel)) return curr;
                    curr = curr.parentNode;
                }
                return null;
            };
            const getClassStr = function(el) {
                if (!el || !el.className) return "";
                return typeof el.className === 'string' ? el.className : (el.className.baseVal || "");
            };
            const hasClass = function(el, cls) {
                const c = getClassStr(el);
                return c && c.split(/\s+/).indexOf(cls) > -1;
            };

            // 1. Ignore Modebar
            if (findAncestor(target, '.modebar') || findAncestor(target, '.modebar-container')) return;
            
            // 2. Pierce specific invisible interaction overlays
            let maxDepth = 3;
            let hiddenElements = [];
            
            while (maxDepth > 0 && target && target.tagName === 'rect' && (hasClass(target, 'nsewdrag') || hasClass(target, 'drag'))) {
                hiddenElements.push({ el: target, original: target.style.pointerEvents });
                target.style.pointerEvents = 'none';
                
                const elementUnder = document.elementFromPoint(e.clientX, e.clientY);
                if (!elementUnder || elementUnder === target) break;
                
                target = elementUnder;
                maxDepth--;
            }
            
            // Restore pointer events
            for (let i = 0; i < hiddenElements.length; i++) {
                hiddenElements[i].el.style.pointerEvents = hiddenElements[i].original || 'all';
            }
            
            // 3. Identify Element
            let elementType = 'none';
            let axisId = '';
            let traceName = '';
            let traceIndex = -1;
            
            if (findAncestor(target, '.g-gtitle')) {
                elementType = 'title';
            } else if (findAncestor(target, '.legend')) {
                const legendItem = findAncestor(target, '.traces');
                if (legendItem) {
                    const textNode = legendItem.querySelector('.legendtext');
                    if (textNode) {
                        elementType = 'trace';
                        traceName = textNode.textContent || "";
                    }
                } else {
                    elementType = 'legend';
                }
            } else if (findAncestor(target, '.annotation')) {
                const ann = findAncestor(target, '.annotation');
                const idx = ann.getAttribute('data-index');
                if (idx !== null) {
                    elementType = 'annotation';
                    axisId = idx;
                }
            } else if (findAncestor(target, '.g-x-title')) {
                elementType = 'xaxis_title';
                const parent = findAncestor(target, 'g[class*="-title"]');
                if (parent) {
                    const match = getClassStr(parent).match(/g-(x\d*)-title/);
                    if (match) axisId = match[1];
                }
            } else if (findAncestor(target, '.g-y-title')) {
                elementType = 'yaxis_title';
                const parent = findAncestor(target, 'g[class*="-title"]');
                if (parent) {
                    const match = getClassStr(parent).match(/g-(y\d*)-title/);
                    if (match) axisId = match[1];
                }
            } else if (findAncestor(target, '.xtick')) {
                elementType = 'xaxis_tick';
                const parent = findAncestor(target, 'g[class^="x"]');
                if (parent) {
                   const match = getClassStr(parent).match(/(x\d*)axislayer/);
                   if (match) axisId = match[1];
                }
            } else if (findAncestor(target, '.ytick')) {
                elementType = 'yaxis_tick';
                const parent = findAncestor(target, 'g[class^="y"]');
                if (parent) {
                   const match = getClassStr(parent).match(/(y\d*)axislayer/);
                   if (match) axisId = match[1];
                }
            } else if (findAncestor(target, '.gridlayer .x')) {
                elementType = 'xaxis_grid';
            } else if (findAncestor(target, '.gridlayer .y')) {
                elementType = 'yaxis_grid';
            } else if (findAncestor(target, '.trace') || findAncestor(target, '.js-line') || findAncestor(target, '.point')) {
                elementType = 'trace';
                let traceNode = findAncestor(target, '.trace');
                if (traceNode) {
                    const match = getClassStr(traceNode).match(/trace-(\d+)/);
                    if (match) traceIndex = parseInt(match[1], 10);
                }
                if (traceIndex >= 0 && plotEl.data && plotEl.data[traceIndex]) {
                    traceName = plotEl.data[traceIndex].name || "";
                }
            } else if (hasClass(target, 'nsewdrag') || hasClass(target, 'bg')) {
                elementType = 'background';
            }
            
            // --- ROBUST FALLBACK FOR GGPLOTLY SVG TAGS ---
            if (elementType === 'none') {
                const tag = (target.tagName || "").toLowerCase();
                if (tag === 'text' || tag === 'tspan') {
                    // It's some text element. Try to guess based on position or parents.
                    const parentClass = target.parentNode ? getClassStr(target.parentNode) : "";
                    if (parentClass.indexOf('x') > -1 && parentClass.indexOf('title') > -1) elementType = 'xaxis_title';
                    else if (parentClass.indexOf('y') > -1 && parentClass.indexOf('title') > -1) elementType = 'yaxis_title';
                    else if (findAncestor(target, '.xaxislayer-above')) elementType = 'xaxis_tick';
                    else if (findAncestor(target, '.yaxislayer-above')) elementType = 'yaxis_tick';
                    else {
                        elementType = 'text_element'; // generic text
                        traceName = target.textContent;
                    }
                } else if (tag === 'path' || tag === 'circle' || tag === 'rect') {
                    // Could be a trace line, marker, or bar
                    if (findAncestor(target, '.trace') || findAncestor(target, '.scatterlayer') || findAncestor(target, '.barlayer')) {
                        elementType = 'trace';
                    }
                }
            }
            // Clean up missing axisId
            if ((elementType.startsWith('x') || elementType.startsWith('y')) && !axisId) {
                 axisId = elementType.startsWith('x') ? 'xaxis' : 'yaxis';
            }
            
            // Extract text content if it's a text element so Shiny doesn't erase it
            if (target && (target.tagName === 'text' || target.tagName === 'tspan')) {
                const txtNode = target.tagName === 'tspan' ? target.parentNode : target;
                if (!traceName) traceName = txtNode.textContent.trim();
            }

            // Immediately show UI for visual feedback
            const editorPanel = document.getElementById('plot_aesthetic_editor');
            if (editorPanel) {
                if (elementType !== 'none') {
                    editorPanel.style.display = 'block';
                    editorPanel.style.top = Math.max(20, e.clientY - 100) + "px";
                    editorPanel.style.left = Math.min(window.innerWidth - 340, e.clientX + 50) + "px";
                } else {
                    editorPanel.style.display = 'none';
                }
            }

            // Update debug text so we can see what was clicked
            const debugMsg = plotEl.parentNode.querySelector('.interactive-debug-msg');
            if (debugMsg) {
                const targetTag = target.tagName ? target.tagName.toLowerCase() : 'unknown';
                const targetClass = getClassStr(target);
                debugMsg.innerText = '[System: Clicked element = ' + elementType + 
                                     ', tag = ' + targetTag + 
                                     ', class = ' + targetClass + ']';
                debugMsg.style.color = elementType === 'none' ? 'orange' : 'blue';
            }

            // Capture the element's CURRENT values so the editor controls can be
            // seeded with them (otherwise reopening pushes hardcoded defaults).
            let current = {};
            try {
                const FL = gd._fullLayout || {};
                const L  = gd.layout || {};
                if (elementType === 'legend') {
                    const lg   = FL.legend || L.legend || {};
                    const lgf  = lg.font || {};
                    const lgt  = lg.title || {};
                    const lgtf = lgt.font || {};
                    const boldRe = /^<b>[\s\S]*<\/b>$/;
                    const firstName = (gd.data && gd.data[0] && gd.data[0].name) || "";
                    current = {
                        legend_x: lg.x, legend_y: lg.y,
                        legend_orientation: lg.orientation || 'v',
                        legend_bgcolor: lg.bgcolor,
                        legend_item_fontfamily: lgf.family,
                        legend_item_fontsize: lgf.size,
                        legend_item_fontcolor: lgf.color,
                        legend_item_bold: boldRe.test(firstName),
                        legend_title_fontfamily: lgtf.family,
                        legend_title_fontsize: lgtf.size,
                        legend_title_fontcolor: lgtf.color,
                        legend_title_bold: boldRe.test(lgt.text || "")
                    };
                } else if (elementType === 'trace' && traceIndex >= 0 && gd.data && gd.data[traceIndex]) {
                    const tr = gd.data[traceIndex];
                    current = {
                        trace_color: (tr.line && tr.line.color) || (tr.marker && tr.marker.color),
                        trace_width: tr.line && tr.line.width,
                        trace_dash: (tr.line && tr.line.dash) || 'solid',
                        trace_markersize: tr.marker && tr.marker.size,
                        trace_mode: tr.mode
                    };
                } else if (elementType === 'title' || elementType === 'text_element') {
                    const t = L.title || FL.title || {};
                    current = {
                        title_text: (typeof t === 'string') ? t : (t.text),
                        title_size: t.font && t.font.size,
                        title_color: t.font && t.font.color
                    };
                } else if (elementType === 'background') {
                    current = {
                        bg_plot:  FL.plot_bgcolor  || L.plot_bgcolor,
                        bg_paper: FL.paper_bgcolor || L.paper_bgcolor
                    };
                } else if (elementType.indexOf('yaxis') === 0) {
                    let axKey = 'yaxis';
                    if (axisId && /^y\d+$/.test(axisId)) axKey = axisId.replace(/^y/, 'yaxis');
                    const yaObj = (FL[axKey] || L[axKey] || {});
                    if (yaObj.range) current = { yaxis_range: yaObj.range.slice() };
                }
            } catch (e) { current = {}; }

            // Send to Shiny
            Shiny.setInputValue('plot_element_clicked', {
                element: elementType,
                axis_id: axisId,
                trace_name: traceName,
                trace_index: traceIndex,
                current: current,
                plot_id: plotEl.id,
                rand: Math.random() 
            });
        }, true);
        
        plotEl._hasEditorListener = true;
    };

    // Get the actual Plotly graph div
    const gd = plotEl.classList && plotEl.classList.contains('js-plotly-plot') ? plotEl : 
               (plotEl.querySelector('.js-plotly-plot') || plotEl);
    
    // Cache initial state exactly once per session/plot
    if (!state.originalState && gd.layout && gd.data) {
        state.originalState = {
            layout: clonePlotlyState(gd.layout),
            data: clonePlotlyState(gd.data)
        };
    }

    // Initial binding
    bindListeners();

    // Rebind on Plotly redraw
    if (typeof gd.on === 'function') {
        gd.on('plotly_afterplot', function() {
            bindListeners();
        });

        gd.on('plotly_relayout', function(ed) {
            if (!ed) return;
            if (ed['legend.x'] !== undefined || ed['legend.y'] !== undefined) {
                Shiny.setInputValue('editor_legend_dragged', {
                    x: ed['legend.x'],
                    y: ed['legend.y'],
                    plot_id: plotEl.id,
                    rand: Math.random()
                });
            }
        });

        // Handle trace clicks natively through Plotly
        if (!gd._hasTraceListener) {
            gd.on('plotly_click', function(data) {
                if (data && data.points && data.points.length > 0) {
                    const point = data.points[0];
                    let traceName = point.data.name || "";
                    
                    Shiny.setInputValue('plot_element_clicked', {
                        element: 'trace',
                        trace_name: traceName,
                        trace_index: point.curveNumber,
                        current: {},
                        plot_id: plotEl.id || gd.id,
                        rand: Math.random()
                    });
                }
            });
            gd._hasTraceListener = true;
        }
    }
  };

  // --- Shiny Custom Message Handlers ---

  // Handle Plotly.relayout (Axes, Background, Legend, Title)
  Shiny.addCustomMessageHandler('editor_relayout', function(msg) {
      if (!msg.plot_id || !msg.update) return;
      const plotEl = document.getElementById(msg.plot_id);
      if (plotEl) {
          Plotly.relayout(plotEl, msg.update);
      }
  });

  // Handle Plotly.restyle (Traces)
  Shiny.addCustomMessageHandler('editor_restyle', function(msg) {
      if (!msg.plot_id || !msg.update) return;
      const plotEl = document.getElementById(msg.plot_id);
      if (plotEl) {
          // If trace_indices is provided, target specific traces
          if (msg.trace_indices && msg.trace_indices.length > 0) {
              Plotly.restyle(plotEl, msg.update, msg.trace_indices);
          } else {
              Plotly.restyle(plotEl, msg.update);
          }
      }
  });

  Shiny.addCustomMessageHandler('editor_legend_title_bold', function(msg) {
      if (!msg.plot_id) return;
      const el = document.getElementById(msg.plot_id);
      if (!el) return;
      const FL = el._fullLayout || {}, L = el.layout || {};
      let t = (FL.legend && FL.legend.title && FL.legend.title.text) ||
              (L.legend && L.legend.title && L.legend.title.text) || "";
      t = t.replace(/^<b>([\s\S]*)<\/b>$/, "$1");
      Plotly.relayout(el, { 'legend.title.text': msg.bold ? ('<b>' + t + '</b>') : t });
  });

  Shiny.addCustomMessageHandler('editor_legend_items_bold', function(msg) {
      if (!msg.plot_id) return;
      const el = document.getElementById(msg.plot_id);
      if (!el || !el.data) return;
      const names = el.data.map(function(tr) {
          let n = (tr.name || "").replace(/^<b>([\s\S]*)<\/b>$/, "$1");
          return msg.bold ? ('<b>' + n + '</b>') : n;
      });
      Plotly.restyle(el, { name: names });
  });

  // Handle Reset to Defaults
  Shiny.addCustomMessageHandler('editor_reset', function(msg) {
      if (!msg.plot_id) return;
      const plotEl = document.getElementById(msg.plot_id);
      const state = window.interactivePlots[msg.plot_id];
      
      if (plotEl && state && state.originalState) {
          // Reactivate original Plotly state instantly
          // Using react is safer to perform a full diff replacement
          Plotly.react(plotEl, clonePlotlyState(state.originalState.data), clonePlotlyState(state.originalState.layout));
      }
  });

  // Handle High-Res Export
  Shiny.addCustomMessageHandler('editor_export', function(msg) {
      if (!msg.plot_id) return;
      const plotEl = document.getElementById(msg.plot_id);
      if (plotEl) {
          Plotly.downloadImage(plotEl, {
              format: msg.format || 'png',
              width: msg.width || 800,
              height: msg.height || 600,
              filename: msg.filename || 'customized_plot',
              scale: msg.scale || 1
          });
      }
  });

  // Handle Draggable Panel (Vanilla JS)
  // Invoked from R when the UI is rendered
  window.makePanelDraggable = function(panelId, headerId) {
      const panel = document.getElementById(panelId);
      const header = document.getElementById(headerId);
      if (!panel || !header) return;

      let pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
      
      header.onmousedown = dragMouseDown;
      header.style.cursor = 'move';

      function dragMouseDown(e) {
          e = e || window.event;
          // Don't drag if clicking a button inside header
          if(e.target.tagName === 'BUTTON') return;
          e.preventDefault();
          pos3 = e.clientX;
          pos4 = e.clientY;
          document.onmouseup = closeDragElement;
          document.onmousemove = elementDrag;
      }

      function elementDrag(e) {
          e = e || window.event;
          e.preventDefault();
          pos1 = pos3 - e.clientX;
          pos2 = pos4 - e.clientY;
          pos3 = e.clientX;
          pos4 = e.clientY;
          // Set new position
          panel.style.top = (panel.offsetTop - pos2) + "px";
          panel.style.left = (panel.offsetLeft - pos1) + "px";
          // Remove bottom/right constraints so it doesn't fight top/left
          panel.style.bottom = "auto";
          panel.style.right = "auto";
      }

      function closeDragElement() {
          document.onmouseup = null;
          document.onmousemove = null;
      }
  };

  // Re-apply saved styles passed from R on render
  window.applySavedAesthetics = function(plotEl, savedStyles) {
      if (!plotEl || !plotEl.data) return;
      
      // Iterate through current traces and see if they match a saved semantic key
      const updates = {};
      const targetIndices = [];
      
      for (let i = 0; i < plotEl.data.length; i++) {
          const traceName = plotEl.data[i].name || "";
          // Check against all saved keys
          for (const key in savedStyles) {
              if (traceName.includes(key)) {
                  // Apply saved style to this trace index
                  const style = savedStyles[key];
                  if (style.line_width !== undefined) {
                      updates['line.width'] = updates['line.width'] || [];
                      updates['line.width'].push(style.line_width);
                      targetIndices.push(i);
                  }
                  // More style properties would be mapped here
              }
          }
      }
      
      if (targetIndices.length > 0) {
          Plotly.restyle(plotEl, updates, targetIndices);
      }
  };

})();
