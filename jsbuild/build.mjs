// build the study-design wizard into an IIFE the shiny app serves.
// react / react-dom / reactR are NOT bundled: reactR::createReactShinyInput()
// already loads them as window.React / window.ReactDOM / window.reactR, so we
// map the imports to those globals (same idea as reactR's webpack externals).
// nothing here runs on shinyapps.io; it only produces ../www/study_design_wizard.js.
import * as esbuild from 'esbuild';
import externalGlobalPkg from 'esbuild-plugin-external-global';
const { externalGlobalPlugin } = externalGlobalPkg;

const ctx = {
  entryPoints: ['src/study_design_wizard.jsx'],
  bundle: true,
  format: 'iife',
  outfile: '../www/study_design_wizard.js',
  jsx: 'transform',
  jsxFactory: 'React.createElement',
  jsxFragment: 'React.Fragment',
  minify: true,
  logLevel: 'info',
  plugins: [
    externalGlobalPlugin({
      react: 'window.React',
      'react-dom': 'window.ReactDOM',
      reactR: 'window.reactR',
    }),
  ],
};

if (process.argv.includes('--watch')) {
  const c = await esbuild.context(ctx);
  await c.watch();
  console.log('watching src/ -> ../www/study_design_wizard.js');
} else {
  await esbuild.build(ctx);
  console.log('built -> ../www/study_design_wizard.js');
}
