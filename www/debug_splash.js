// Debug: log all computed dimensions of splash elements
document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    var splash = document.getElementById('splash-screen');
    var logo = document.getElementById('splash-logo');
    var svg = logo ? logo.querySelector('svg') : null;
    
    var info = {
      viewport: { w: window.innerWidth, h: window.innerHeight },
      body: null,
      splash: null,
      logo: null,
      svg: null,
      ancestors: []
    };
    
    var body = document.body;
    var bodyCS = getComputedStyle(body);
    info.body = {
      margin: bodyCS.margin,
      padding: bodyCS.padding,
      width: body.offsetWidth,
      clientWidth: body.clientWidth
    };
    
    if (splash) {
      var cs = getComputedStyle(splash);
      info.splash = {
        offsetWidth: splash.offsetWidth,
        offsetHeight: splash.offsetHeight,
        clientWidth: splash.clientWidth,
        position: cs.position,
        left: cs.left,
        top: cs.top,
        width: cs.width,
        height: cs.height,
        overflow: cs.overflow,
        padding: cs.padding,
        margin: cs.margin,
        boxSizing: cs.boxSizing
      };
      
      // Walk up ancestors to find constraints
      var el = splash.parentElement;
      while (el && el !== document.documentElement) {
        var ecs = getComputedStyle(el);
        info.ancestors.push({
          tag: el.tagName,
          id: el.id,
          class: el.className.substring(0, 100),
          width: ecs.width,
          maxWidth: ecs.maxWidth,
          padding: ecs.padding,
          margin: ecs.margin,
          overflow: ecs.overflow,
          position: ecs.position,
          offsetWidth: el.offsetWidth
        });
        el = el.parentElement;
      }
    }
    
    if (logo) {
      var lcs = getComputedStyle(logo);
      info.logo = {
        offsetWidth: logo.offsetWidth,
        offsetHeight: logo.offsetHeight,
        width: lcs.width,
        height: lcs.height,
        position: lcs.position
      };
    }
    
    if (svg) {
      var scs = getComputedStyle(svg);
      info.svg = {
        offsetWidth: svg.offsetWidth || svg.getBoundingClientRect().width,
        offsetHeight: svg.offsetHeight || svg.getBoundingClientRect().height,
        width: scs.width,
        height: scs.height,
        position: scs.position,
        maxWidth: scs.maxWidth,
        viewBox: svg.getAttribute('viewBox'),
        preserveAspectRatio: svg.getAttribute('preserveAspectRatio'),
        svgWidth: svg.getAttribute('width'),
        svgHeight: svg.getAttribute('height'),
        bbox: svg.getBoundingClientRect()
      };
    }
    
    console.log('=== SPLASH DEBUG ===');
    console.log(JSON.stringify(info, null, 2));
    
    // Also display as alert for RStudio viewer where console may not be accessible
    var summary = 'Viewport: ' + info.viewport.w + 'x' + info.viewport.h + '\n';
    if (info.splash) summary += 'Splash: ' + info.splash.offsetWidth + 'x' + info.splash.offsetHeight + ' pos:' + info.splash.position + '\n';
    if (info.logo) summary += 'Logo: ' + info.logo.offsetWidth + 'x' + info.logo.offsetHeight + '\n';
    if (info.svg) summary += 'SVG: ' + Math.round(info.svg.bbox.width) + 'x' + Math.round(info.svg.bbox.height) + ' at(' + Math.round(info.svg.bbox.left) + ',' + Math.round(info.svg.bbox.top) + ')\n';
    summary += 'Body margin: ' + (info.body ? info.body.margin : 'N/A') + '\n';
    if (info.ancestors.length > 0) {
      summary += '\nAncestors:\n';
      info.ancestors.forEach(function(a) {
        summary += '  ' + a.tag + '#' + a.id + ' w:' + a.offsetWidth + ' maxW:' + a.maxWidth + ' pad:' + a.padding + '\n';
      });
    }
    
    // Send to Shiny as a notification
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('splash_debug_info', summary);
    }
    
    alert(summary);
  }, 1500);
});
