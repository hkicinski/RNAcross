#app.R
#RNAcross Application Entry Point (Deployment Version)
#
#This file is located at the project root for shinyapps.io compatibility.
#Sources all modular components in dependency order and launches the Shiny app.

# Components are automatically sourced from R/ directory by Shiny (since V. 1.5.0)

#run the app
shinyApp(ui = ui, server = server)

