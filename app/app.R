#source all modular components
source("../R/00_config.R")
source("../R/01_constants_themes_css.R")
source("../R/02_fct_data_process.R")
source("../R/03_fct_query.R")
source("../R/04_fct_visualization_core.R")
source("../R/05_fct_visualization_heatmaps.R")
source("../R/06_fct_visualization_tables.R")
source("../R/07_fct_utils.R")
source("../R/08_ui.R")
source("../R/09_server.R")

#run the app
shinyApp(ui = ui, server = server)