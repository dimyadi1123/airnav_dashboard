library(shiny)
library(shinydashboard)

# Load UI and Server
source("ui.R")
source("server.R")

shinyApp(ui=ui, server = server)
