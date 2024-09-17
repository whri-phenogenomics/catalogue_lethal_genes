library(shiny)
library(shinydashboard)
library(fst)
library(reactable)
library(plotly)
library(dplyr)
library(stringr)

source("mod_home.R")
source("mod_content.R")
source("mod_curation.R")
source("mod_catalogue.R")
source("mod_mouse_evidence.R")
source("mod_score.R")


tab_defs <- data.frame(
  text = c(
    "Home",
    "Database Content",
    "Data Curation",
    "OMIM Catalogue",
    "Mouse Evidence",
    "LOEUF Score",
    "Shet Score",
    "Gene Effect Score"
  ),
  name = c(
    "HomePage",
    "DatabaseContent",
    "DataCuration",
    "OMIM",
    "MouseEvidence",
    "LOEUF",
    "Shet",
    "DepMap"
  ),
  tab_icon = c(
    "home",
    "database",
    "book-reader",
    "book",
    "project-diagram",
    "chart-bar",
    "chart-bar",
    "chart-bar"
    
  )
)

tab_builder <- function(text, name, tab_icon) {
  menuItem(text,
           tabName = name,
           icon = icon(tab_icon, lib = 'font-awesome'))
}

all_tabs = list()
for (tab in 1:nrow(tab_defs)) {
  all_tabs[[tab]] <-  do.call(tab_builder, tab_defs[tab, ])
}

sidebar <- dashboardSidebar(sidebarMenu(id = 'tabs', .list = all_tabs))

app_ui <- function(request) {
  tagList(dashboardPage(
    dashboardHeader(title = "Lethal Phenotypes"),
    sidebar,
    dashboardBody(
      includeCSS("www/style.css"),
      tabItems(
        tabItem("HomePage", mod_home_ui("HomePage")),
        tabItem("DatabaseContent", mod_content_ui("DatabaseContent")),
        tabItem("DataCuration", mod_curation_ui("DataCuration")),
        tabItem("OMIM", mod_catalogue_ui("OMIM")),
        tabItem("MouseEvidence", mod_mouse_evidence_ui("MouseEvidence")),
        tabItem("LOEUF", mod_score_ui("LOEUF")),
        tabItem("Shet", mod_score_ui("Shet")),
        tabItem("DepMap", mod_score_ui("DepMap"))
        
      )
    )
  ))
  
}

app_server <- function(input, output, session) {
  observe({
    tab <- input$tabs
    
    cat_tabs <- c("literature", "OMIM")
    hist_tabs <- c("LOEUF", "Shet", "DepMap")
    
    if (tab == "HomePage") {
      server <- mod_home_server
    } else if (tab == "DatabaseContent") {
      server <- mod_content_server
    } else if (tab == "DataCuration") {
      server <- mod_curation_server
    } else if (is.element(tab, cat_tabs)) {
      server <- mod_catalogue_server
    } else if (tab == "MouseEvidence") {
      server <- mod_mouse_evidence_server
    } else if (is.element(tab, hist_tabs)) {
      server <- mod_score_server
    }
    
    server(tab)
    
  })
  
}

shinyApp(app_ui, app_server)
