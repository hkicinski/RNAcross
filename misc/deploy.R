#deploy.R
#RNAcross Deployment Script for shinyapps.io
library(rsconnect)
if (!file.exists("app.R")) {
  stop("Error: app.R not found. Run this script from the RNAcross project root directory.")
}
if (!file.exists("data/07312026-updated.RData")) {
  stop("Error: Data file not found. Ensure data/ directory contains the RData file.")
}
if (!file.exists("www/rnacross-prominent-rna-logo-interactive.svg")) {
  stop("Error: Logo SVG not found. Ensure www/ directory contains the logo.")
}
if (!file.exists("www/47313572-sci-fi-sfx16-350847.mp3")) {
  stop("Error: Intro click sound not found. Ensure www/ directory contains the sound effect.")
}
if (!file.exists("www/command_palette.js")) {
  stop("Error: Command palette JS not found. Ensure www/ directory contains it.")
}
cat("Deploying RNAcross to shinyapps.io...\n")
cat("App name: gene-plot-Pi-4sps\n")
cat("Account: hkicinski\n\n")
#deploy the app
rsconnect::deployApp(
  appDir = ".",
  appName = "gene-plot-Pi-4sps",
  appFiles = c(
    "app.R",
    "R/00_version_init.R",
    "R/01_config.R",
    "R/02_constants_themes.R",
    "R/02b_study_design.R",
    "R/03_utils.R",
    "R/04_data_io.R",
    "R/05_orthology_query.R",
    "R/06_data_process.R",
    "R/07_visualization_core.R",
    "R/08_visualization_heatmaps.R",
    "R/09_visualization_outputs.R",
    "R/10_ui.R",
    "R/11_server.R",
    "R/12_interactive_editor.R",
    "R/13_upload_wizard.R",
    "data/07312026-updated.RData",
    "www/rnacross-prominent-rna-logo-interactive.svg",
    "www/interactive_editor.js",
    "www/command_palette.js",
    "www/47313572-sci-fi-sfx16-350847.mp3"
  ),
  account = "hkicinski",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
cat("\nDeployment complete!\n")
cat("App URL: https://hkicinski.shinyapps.io/gene-plot-Pi-4sps/\n")
