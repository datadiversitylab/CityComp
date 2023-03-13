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
m <- na.omit(melt(m))

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
    selectizeInput(
      inputId = "cities",
      label = "Select a city",
      choices = unique(m[,1]),
      selected = 1,
      options = list(maxOptions = 100)
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
    d1 <- m[m$c1 == input$cities,]
    d1
  })
  
  # Plot table
  output$table <- DT::renderDataTable({
    datatable(table(), filter = 'top')
  })
  
}

# Run shiny.
shinyApp(ui = ui, server = server)