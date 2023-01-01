require(shiny)
require(ggplot2)
library(DT)
library(leaflet)
library(feather)

## Global

### Distance matrix
#x <- feather::read_feather("data/pca_dists2.feather")
#x <- data.frame(x)
#row.names(x) <- colnames(x)
#xy <- t(combn(colnames(x), 2))
#test <- data.frame(xy, dist=x[xy])
#names(test) <- c("c1", "c2", "distance")
#write_feather(test, "data/pca_dists2.long.feather")
m <- feather::read_feather("data/pca_dists2.long.feather")

### City characteristics
City_chars <- read.csv("data/cities_stabilities_k6.csv")
colnames(City_chars)[2] <- "Continent"

coords <- read.csv("data/cities_coords.csv")
City_chars <- cbind.data.frame(City_chars, coords)


### UI

ui <- fluidPage(
  titlePanel("Compare cities"),
  br(),
  uiOutput(outputId = "distanceTab"),
  sidebarLayout(
    mainPanel(
      DT::dataTableOutput("table"),
      width = 9
    ),
    sidebarPanel(
      uiOutput(outputId = "ClusterFilter"),
      width = 3
    )
  ),
  leafletOutput("mymap")
)


## Server

server <- function(input, output) {
  # Read data.
  df <- m
  
  # Get distanceTab
  output$distanceTab <- renderUI({
    selectizeInput(
      inputId = "city",
      label = "Select a city",
      choices = c("", as.character(unique(City_chars$City))),
      selected = 1,
      options = list(maxOptions = 100)
    )})
  
  # Subset data by city.
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