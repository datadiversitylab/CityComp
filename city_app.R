require(shiny)
require(ggplot2)
library(DT)
library(data.table)
library(leaflet)
library(feather)
library(shinydashboard)
library(shinybusy)
library(dashboardthemes)
library(reshape2)

## Global
#m <- read_csv("data/pca_dists2.csv.gz")
m <- fread("data/pca_dists2.csv.gz")

### City characteristics
City_chars <- read.csv("data/cities_stabilities_k6.csv")
colnames(City_chars)[2] <- "Continent"

coords <- read.csv("data/cities_coords.csv")
City_chars <- cbind.data.frame(City_chars, coords)

### UI
header <- dashboardHeader(title = "City comparison",
                          titleWidth = 200)

body <- dashboardBody(
  add_busy_spinner(spin = "dots",
                   timeout = 10,
                   height = "25px",
                   width = "25px"),
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  fluidRow(
    column(width = 4,
           box(width = NULL,title = tagList(shiny::icon("info-circle",class = 'fa-lg'), "About the app"), solidHeader = T, collapsible = T, status = 'primary',
               strong("CityComp"),"is an interactive shiny app which allows you to compare cities across the globe."
           ),
           box(width = NULL, title = tagList(shiny::icon("filter",class = 'fa-lg'), "Filters") ,
               solidHeader = T, collapsible = T, status = 'primary',
               uiOutput("cities"),
               uiOutput("clusters")
           ),
           box(width = NULL, title = tagList(shiny::icon("laptop-code",class = 'fa-lg'), "Code") ,
               solidHeader = T, collapsible = T, status = 'primary',
               a(icon("github",class = 'fa-lg'), ' GitHub repository', href = 'https://github.com/cromanpa94/city_app', target = "_blank"))
    ),
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               DT::dataTableOutput("table")
           )
    )
  )
)

ui <- dashboardPage(skin = 'black',
                    header,
                    dashboardSidebar(disable = T),
                    body
)


## Server

server <- function(input, output) {
  
  # Get filters
  output$cities <- renderUI({
    selectInput(
      inputId = "cities",
      label = "Select a city",
      choices = c(Choose = unique(as.vector(m[,1]))),
      selectize = TRUE
    )})
  
  output$clusters <- renderUI({
    selectizeInput(
      inputId = "clusters",
      label = "Select a cluster",
      choices = c(as.character(unique(City_chars$Cluster))),
      selected = 1,
      options = list(maxOptions = 100)
    )})
  
  # Filter the table
  table <- reactive({
    x = input$cities 
    if(length(x) != 0){
      if(x != "Aalborg"){
        d1 <- m[, ..x]
        d1 <- cbind(cities = m$c1, d1)
        colnames(d1)[2] <- "distance"
        d2 <- data.frame(distance = t(m[c1 == x])[-1,])
        d2 <- cbind(cities = row.names(d2), distance = d2)
        row.names(d2) <- NULL
        dtot <- rbind.data.frame(d1, d2)
        dtot <- na.omit(dtot)
      }else{
        d2 <- data.frame(distance = t(m[c1 == x])[-1,])
        d2 <- cbind(cities = row.names(d2), distance = d2)
        row.names(d2) <- NULL  
        dtot <- na.omit(d2)
      }
      dtot <- dtot[order(dtot$distance),] 
      dtot
    }
  })
  
  # Plot table
  output$table <- DT::renderDataTable({
    datatable(table(), filter = 'top')
  })
  
}

# Run shiny.
shinyApp(ui = ui, server = server)