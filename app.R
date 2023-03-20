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

## Global
#m <- read_csv("data/pca_dists2.csv.gz")
m <- fread("data/pca_dists2.csv.gz")
namesds <- unlist(c(m[1,1], colnames(m)[-1]))

### City characteristics
City_chars <- read.csv("data/cities_stabilities_k6.csv")
colnames(City_chars)[2] <- "Continent"

coords <- read.csv("data/cities_coords.csv")
City_chars <- cbind.data.frame(namesds,City_chars, coords)

#PCA
dataScaled <- read.csv("data/cities_scaled.csv")
pc <- princomp(dataScaled[,-c(1:2)])
PCAdata <- data.frame(pc$scores,"cluster"=factor(City_chars$Cluster))

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
                  the target city and the rows selected by clicking in the PCA-based distance table."),
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

server <- function(input, output) {
  
  # Get filters
  output$cities <- renderUI({
    pickerInput(
      inputId = "cities",
      label = "Select target city",
      choices = unique(City_chars$namesds),
      options = list(`live-search`=TRUE)
    )})
  
  output$clusters <- renderUI({
    pickerInput(
      inputId = "clusters",
      label = "Select target cluster(s)",
      choices = c(as.character(unique(City_chars$Cluster))),
      selected = c(as.character(unique(City_chars$Cluster))),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )})
  
  
  output$continent <- renderUI({
    pickerInput(
      inputId = "continent",
      label = "Select target continent(s)",
      choices = c(as.character(unique(City_chars$Continent))),
      selected = c(as.character(unique(City_chars$Continent))),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )})
  
  # Base map
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(
                      attributionControl = FALSE)) %>% 
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addFullscreenControl()
  })
  
  # Filter the table
  table <- reactive({
    x <<- input$cities
    y = input$clusters
    z = input$continent
    
    if(length(x) != 0 & length(y) != 0 & length(z) != 0){
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
      # Filter cluster and continent
      tcities <- City_chars[City_chars$Cluster %in% y & 
                                      City_chars$Continent %in% z,1]
      dtot <- data.frame(dtot)
      dtot <- dtot[dtot$cities %in% tcities,]
      row.names(dtot) <- NULL
      
      #Plot map
      tcity <- City_chars[City_chars$namesds %in% x,]
      leafletProxy("Map", data = tcity) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addAwesomeMarkers(data = tcity,
                          lng = ~ x, lat = ~ y,
                          label = lapply(tcity$City, htmltools::HTML),
                          icon = makeAwesomeIcon(
                            markerColor = "red",
                            library = "fa",
                            iconColor = "#FFFFFF"
                          )) %>%
        flyToBounds(~min(x), ~min(y), ~max(x), ~max(y))
      dtot
    }
  })
  
  # Plot table
  output$table <- DT::renderDataTable({
    datatable(table(), options = list(lengthChange = FALSE, searching = FALSE) )
  })
  
  
  observe({
    tdt <- table()[input$table_rows_selected,]
    tcity <- City_chars[City_chars$namesds %in% c(tdt$cities, x),]

    if(nrow(tcity)){
     leafletProxy("Map", data = tcity) %>%
        clearMarkers() %>%
        clearShapes() %>%
       addAwesomeMarkers(data = tcity,
                         lng = ~ x, lat = ~ y,
                         label = lapply(tcity$City, htmltools::HTML),
                         icon = makeAwesomeIcon(
                           markerColor = "red",
                           library = "fa",
                           iconColor = "#FFFFFF"
                         )) %>%
       flyToBounds(~min(x), ~min(y), ~max(x), ~max(y))
     }
  })
  
  # PCA
  
  output$plotPCA <- renderPlotly({
  p <- plot_ly(PCAdata,x=~Comp.1,y=~Comp.2,text=~City_chars$City,
               mode="markers",color = ~cluster)
  p <- layout(p,title="PCA Clusters",
              xaxis=list(title="PC1"),
              yaxis=list(title="PC2"))
  p
  })
  
  

  
}

# Run shiny.
shinyApp(ui = ui, server = server)