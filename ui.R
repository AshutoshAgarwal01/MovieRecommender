## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(shinyjs)
library(lubridate)
library(data.table)

source('functions/helpers.R')

# https://rstudio.github.io/shinydashboard/get_started.html
shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("Recommendations", tabName = "reco", icon = icon("thumbs-up")),
              menuItem("Most Popular", tabName = "popular", icon = icon("fire"))
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
            tabItems(
              # First tab content
              tabItem(tabName = "reco",
                      fluidRow(
                        box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            div(class = "rateitems", uiOutput('ratings'))
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                        )
                      )
              ),
              
              # Second tab content
              tabItem(tabName = "popular",
                      fluidRow(
                        box(width = 12, title = "Step 1: What is your favorite genre?", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            div(class = "fav_genre", uiOutput('genre'))
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Most popular movies",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btngenre", "Click here to get most popular movies.", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("genreresults")
                        )
                      )
              )
            )
          )
    )
) 