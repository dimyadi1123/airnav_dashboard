library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(DT)
library(fontawesome)
library(grid)
library(plotly)
library(tidyr)

# Load UI and Server
source("ui.R")
source("server.R")

shinyApp(ui=ui, server = server)
