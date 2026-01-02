#app/app.R
#DEPRECATED: This file is for local development only
#For deployment, use the root-level app.R instead
#
#This version uses ../ paths which work locally but NOT on shinyapps.io
#
#To run locally from this directory:
#  setwd("app")
#  source("app.R")
#
#To deploy, use:
#  setwd("..")  # go to project root
#  source("deploy.R")

#source all modular components (paths relative to app/ directory)
source("../R/01_config.R")
source("../R/02_constants_themes.R")
source("../R/03_utils.R")
source("../R/04_data_io.R")
source("../R/05_orthology_query.R")
source("../R/06_data_process.R")
source("../R/07_visualization_core.R")
source("../R/08_visualization_heatmaps.R")
source("../R/09_visualization_outputs.R")
source("../R/10_ui.R")
source("../R/11_server.R")

#run the app
shinyApp(ui = ui, server = server)
