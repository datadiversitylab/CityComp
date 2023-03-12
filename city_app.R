require(shiny)
require(ggplot2)
library(DT)
library(leaflet)
library(feather)
library(shinydashboard)
library(shinybusy)
library(dashboardthemes)

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
           box(width = NULL, title = tagList(shiny::icon("gear",class = 'fa-lg'), "Run") ,
               solidHeader = T, collapsible = T, status = 'primary',
               "Once you have selected the city and clusters, please make sure you run the filter:",
               br(),
               actionButton("method", "Run")
           ),
           box(width = NULL, title = tagList(shiny::icon("laptop-code",class = 'fa-lg'), "Code") ,
               solidHeader = T, collapsible = T, status = 'primary',
               a(icon("github",class = 'fa-lg'), ' GitHub repository', href = 'https://github.com/cromanpa94/vessels.app', target = "_blank"))
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
  # Read data.
  df <- m
  
  # Get distanceTab
  output$cities <- renderUI({
    selectizeInput(
      inputId = "cities",
      label = "Select a city",
      choices = c("", as.character(unique(colnames(m)[-1], m$c1 ))),
      selected = 2,
      options = list(maxOptions = 100)
    )})
  
  
  output$clusters <- renderUI({
    selectizeInput(
      inputId = "clusters",
      label = "Select a cluster",
      choices = c("", as.character(unique(City_chars$Cluster))),
      selected = 1,
      options = list(maxOptions = 100)
    )})
  
  # Subset data
  df2 <-
    reactive({
      df2 <- droplevels(m[m$c1 == input$city | m$c2 == input$city,])
      })
  
  # Filter data.
  df3 <-
    eventReactive({
      input$clusterFilter
      df2()
    },
    {
        req(input$clusterFilter) #input$clusterFilter
        
        sub <- City_chars[City_chars$Cluster %in% input$clusterFilter,]
        
        df3a <- df2()[df2()$c1 %in% sub$City | df2()$c2 %in% sub$City,]

      df3a
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE)
  
  # Plot table.
  output$table <- DT::renderDataTable({
    datatable(df3(), filter = 'top')
  })
  
  output$ClusterFilter <- renderUI({
    req(input$city)
    checkboxGroupInput(
      inputId = "clusterFilter",
      label = "Filter by cluster",
      choices = as.character(unique(City_chars$Cluster)),
      selected = as.character(unique(City_chars$Cluster))
    )
  })
  
  filtered_table <- reactive({
    req(input$table_rows_all)
    m[input$table_rows_all, ]  
  })
  
  filtered_table2 <- reactive({
    req(input$table_rows_all)
    subsetCities <- filtered_table()
    tCities <- unique(unlist(subsetCities[,c(1,2)]))
    tCities <- tCities[which( tCities !=  input$city)]
    City_chars[City_chars$City %in% tCities,]
  })

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = filtered_table2(), lng = ~x, lat = ~y)
  })
  
  
}

# Run shiny.
shinyApp(ui = ui, server = server)