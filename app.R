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
library(shinyWidgets)
library(leaflet.extras)
library(plotly)
library(R.utils)
library(here)

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
               strong("CityComp"),"is an interactive shiny app that allows you compare the distances between cities across the globe. This app is based upon analyses presented in Arechiga et al. (in preparation)."
           ),
           box(width = NULL, title = tagList(shiny::icon("filter",class = 'fa-lg'), "Filters") ,
               solidHeader = T, collapsible = T, status = 'primary',
               uiOutput("dataset"),
               uiOutput("cities"),
               uiOutput("continent"),
               uiOutput("clusters")
           ),
           box(width = NULL, title = tagList(shiny::icon("map",class = 'fa-lg'), "Map") ,
               solidHeader = T, collapsible = FALSE, status = 'primary',
               leafletOutput('Map',height = 200)),
           box(width = NULL, title = tagList(shiny::icon("laptop-code",class = 'fa-lg'), "Code") ,
               solidHeader = T, collapsible = FALSE, status = 'primary',
               a(icon("at",class = 'fa-lg'), ' Author 1: Cristian Román Palacios', href = 'cromanpa94.github.io', target = "_blank"),
               br(),
               a(icon("at",class = 'fa-lg'), ' Author 2: Kyle Arechiga', href = 'https://www.linkedin.com/in/kyle-arechiga-007a92132', target = "_blank"),
               br(),
               a(icon("github",class = 'fa-lg'), ' GitHub repository', href = 'https://github.com/cromanpa94/city_app', target = "_blank")
               )
    ),
    column(width = 8,
           box(width = NULL, title = tagList(shiny::icon("check", class = 'fa-lg'), "PCA") ,
               solidHeader = TRUE, collapsible = TRUE, status = 'primary',
               h5("Let's get started comparing some cities! This app is designed such that you can
                  quantify differences between cities across the globe. To begin comparing cities,
                  please select your reference city using the filters on the left. Next, if you
                  are interested in comparing that particular city to others in a particular region,
                  please update the (1) continent-ish like filter or (2) the cluster-based one.
                  Distances between cities can be visualized on the PCA plot in this app. Clusters are 
                  based on KNN analyses. The app also includes a map that gets updated in accordance with
                  the target city and the rows selected by clicking in the PCA-based distance table. Note that the
                  visualization for the human-only dataset is limited to c(-15, 15) for illustration purposes."),
               ),
           box(width = NULL, title = tagList(shiny::icon("tachometer-alt", class = 'fa-lg'), "PCA") ,
               solidHeader = TRUE, collapsible = TRUE, status = 'primary',
               plotlyOutput("plotPCA")),
           box(width = NULL, title = tagList(shiny::icon("table",class = 'fa-lg'), "PCA-based distances") ,
               solidHeader = T, collapsible = FALSE, status = 'primary',
               DT::dataTableOutput("table"))
    )
  )
)

ui <- dashboardPage(skin = 'black',
                    header,
                    dashboardSidebar(disable = T),
                    body
)


## Server

server <- function(input, output, session) {
  
  # Dataset selection UI
  output$dataset <- renderUI({
    pickerInput(
      inputId = "dataset",
      label = "Select Dataset",
      choices = c("Climate Only" = "climate", 
                  "Combined" = "combined",
                  "Human Only" = "human"),
      selected = "combined",
      options = list(`actions-box` = FALSE)
    )
  })
  
  # Reactive expression to load data based on selected dataset
  selectedData <- reactive({
    req(input$dataset)
    
    if (input$dataset == "climate") {
      # Climate Only
      m <- data.table::fread(file = here("data", "clim_only_30s_cluster_data", "dists.csv.gz"))
      clim_30s <- read.csv(here("data", "clim_only_30s_cluster_data", "cities_5.csv"))
      City_chars <- clim_30s[, c(1:5, 26)]
      City_pcs <- clim_30s[, c(27, 28)]
      
    } else if (input$dataset == "combined") {
      # Combined
      m <- data.table::fread(file = here("data", "combined_30s_cluster_data", "dists.csv.gz"))
      comb_30s <- read.csv(here("data", "combined_30s_cluster_data", "cities_6.csv"))
      City_chars <- comb_30s[, c(1:5, 36)]
      City_pcs <- comb_30s[, c(37, 38)]
      
    } else {  # Human Only
      # Human Only
      dataScaled <- read.csv(here("data", "cities_scaled.csv"), row.names = 1)
      datacoords <- read.csv(here("data", "cities_coords.csv"), row.names = 1)
      pc <- princomp(dataScaled[,c(3:12)])
      km.res <- kmeans(dataScaled[,c(3:12)], 6)
      PCAdata <- data.frame(pc$scores, "cluster" = factor(km.res$cluster))
      City_chars <- cbind.data.frame(City = row.names(dataScaled), 
                                     Region =  dataScaled$Region, 
                                     Cluster = km.res$cluster, 
                                     Longitude =  datacoords$x,
                                     Latitude = datacoords$y)
      City_pcs <- PCAdata[,c(1,2)]
      colnames(City_pcs) <- c("PC1", "PC2")
      m <- as.matrix(dist(City_pcs, method = "euclidean", 
                          diag = FALSE, upper = FALSE))
      City_chars$Cluster <- sapply(City_chars$Cluster, function(x) LETTERS[x])
      m <- round(m, 3)
    }
    

    
    list(m = data.table(m), City_chars = City_chars, City_pcs = City_pcs)
  })
  
  
  
  output$cities <- renderUI({
    req(selectedData()) 
    pickerInput(
      inputId = "cities",
      label = "Select target city",
      choices = unique(selectedData()$City_chars$City),
      options = list(`live-search` = TRUE)
    )
  })
  
  output$clusters <- renderUI({
    req(selectedData()) 
    pickerInput(
      inputId = "clusters",
      label = "Select target cluster(s)",
      choices = as.character(unique(selectedData()$City_chars$Cluster)),
      selected = as.character(unique(selectedData()$City_chars$Cluster)),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$continent <- renderUI({
    req(selectedData())
    pickerInput(
      inputId = "continent",
      label = "Select target continent(s)",
      choices = as.character(unique(selectedData()$City_chars$Region)),
      selected = as.character(unique(selectedData()$City_chars$Region)),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  # Base map
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>% 
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addFullscreenControl()
  })
  
  # Filter the table using reactive data
  table <- reactive({
    data <- selectedData()
    m <- data$m
    City_chars <- data$City_chars
    
    x <- input$cities
    y <- input$clusters
    z <- input$continent
    
    if(length(x) != 0 & length(y) != 0 & length(z) != 0){
      d1 <- data.frame(m[, ..x])
      d1 <- cbind(cities = colnames(m), d1)
      colnames(d1)[2] <- "distance"
      dtot <- na.omit(d1)
      dtot <- dtot[order(dtot$distance),] 
      
      # Filter cluster and continent
      tcities <- City_chars[City_chars$Cluster %in% y & 
                              City_chars$Region %in% z, "City"]
      dtot <- dtot[-1, ]  # Remove the first row if it's the target city
      dtot <- dtot[dtot$cities %in% tcities, ]
      row.names(dtot) <- NULL
      
      # Plot map
      tcity <- City_chars[City_chars$City %in% x, ]
      leafletProxy("Map", data = tcity) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addAwesomeMarkers(
          data = tcity,
          lng = ~ Longitude, lat = ~ Latitude,
          label = ~City,
          icon = makeAwesomeIcon(
            markerColor = "red",
            library = "fa",
            iconColor = "#FFFFFF"
          )
        ) %>%
        flyToBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
      
      dtot
    }
  })
  
  # Render the filtered table
  output$table <- DT::renderDataTable({
    datatable(table(), options = list(lengthChange = FALSE, searching = FALSE))
  })
  
  # Observe table row selections to update the map
  observe({
    data <- selectedData()
    City_chars <- data$City_chars
    
    tdt <- table()[input$table_rows_selected, ]
    tcity <- City_chars[City_chars$City %in% c(tdt$cities, input$cities), ]
    
    if(nrow(tcity)){
      leafletProxy("Map", data = tcity) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addAwesomeMarkers(
          data = tcity,
          lng = ~ Longitude, lat = ~ Latitude,
          label = ~City,
          icon = makeAwesomeIcon(
            markerColor = "red",
            library = "fa",
            iconColor = "#FFFFFF"
          )
        ) %>%
        flyToBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    }
  })
  
  # PCA Plot using reactive data
  output$plotPCA <- renderPlotly({
    data <- selectedData()
    plot_p <- plot_ly(
      data = data$City_pcs,
      x = ~PC1, y = ~PC2,
      text = ~data$City_chars$City,
      mode = "markers",
      color = ~data$City_chars$Cluster,
      type = 'scatter',
      marker = list(size = 10)
    ) %>%
      layout(
        title = "PCA Clusters",
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2")
      )
    
    if(input$dataset == "human"){
      plot_p  %>% layout(
        xaxis = list(
          range=c(-15,15)
        )
      )
    } else{
      plot_p
    }
    
  })
  
}

# Run shiny.
shinyApp(ui = ui, server = server)