#deploy.R
#RNAcross Deployment Script for shinyapps.io
#
#This script deploys the RNAcross app to shinyapps.io
#Run from the project root directory (RNAcross/)

#prerequisites: rsconnect package configured with shinyapps.io credentials
#if not configured, run:
#  rsconnect::setAccountInfo(name = "hkicinski",
#                            token = "YOUR_TOKEN",
#                            secret = "YOUR_SECRET")

library(rsconnect)

#verify we're in the correct directory
if (!file.exists("app.R")) {
  stop("Error: app.R not found. Run this script from the RNAcross project root directory.")
}

if (!file.exists("data/RData_perSpecies_HOG_clean_11182025_rlog.RData")) {
  stop("Error: Data file not found. Ensure data/ directory contains the RData file.")
}

if (!file.exists("www/rnacross-prominent-rna-logo-interactive.svg")) {
  stop("Error: Logo SVG not found. Ensure www/ directory contains the logo.")
}

cat("Deploying RNAcross to shinyapps.io...\n")
cat("App name: gene-plot-Pi-4sps\n")
cat("Account: hkicinski\n\n")

#deploy the app
#using appDir approach - deploys entire directory structure
rsconnect::deployApp(
  appDir = ".",
  appName = "gene-plot-Pi-4sps",
  appFiles = c(
    "app.R",
    "R/01_config.R",
    "R/02_constants_themes.R",
    "R/03_utils.R",
    "R/04_data_io.R",
    "R/05_orthology_query.R",
    "R/06_data_process.R",
    "R/07_visualization_core.R",
    "R/08_visualization_heatmaps.R",
    "R/09_visualization_outputs.R",
    "R/10_ui.R",
    "R/11_server.R",
    "data/RData_perSpecies_HOG_clean_11182025_rlog.RData",
    "www/rnacross-prominent-rna-logo-interactive.svg"
  ),
  account = "hkicinski",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\nDeployment complete!\n")
cat("App URL: https://hkicinski.shinyapps.io/gene-plot-Pi-4sps/\n")

