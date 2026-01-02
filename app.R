#app.R
#RNAcross Application Entry Point (Deployment Version)
#
#This file is located at the project root for shinyapps.io compatibility.
#Sources all modular components in dependency order and launches the Shiny app.

#source all modular components (paths relative to project root)
source("R/01_config.R")
source("R/02_constants_themes.R")
source("R/03_utils.R")
source("R/04_data_io.R")
source("R/05_orthology_query.R")
source("R/06_data_process.R")
source("R/07_visualization_core.R")
source("R/08_visualization_heatmaps.R")
source("R/09_visualization_outputs.R")
source("R/10_ui.R")
source("R/11_server.R")

#run the app
shinyApp(ui = ui, server = server)

