require(shiny)
require(ggplot2)
library(DT)


##Global

### Distance matrix
n <- 20
x <- data.frame(rnorm(n))
m <- data.frame(t(combn(row.names(x),2)), as.numeric(dist(x)))
names(m) <- c("c1", "c2", "distance")

### City characteristics

City_chars <-    cbind.data.frame(City = 1:n, 
                                  Cluster = sample(LETTERS[1:6], n, replace = TRUE),
                                  Continent = sample(LETTERS[1:7], n, replace = TRUE)
)


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
      uiOutput(outputId = "modelFilter"),
      uiOutput(outputId = "ClusterFilter"),
      width = 3
    )
  ),
  plotOutput('plot1')
)

server <- function(input, output) {
  # Read data.
  df <- m
  
  # Get distanceTab
  output$distanceTab <- renderUI({
    selectInput(
      inputId = "city",
      label = "Select a city",
      choices = c("", as.character(unique(City_chars$City)))
    )})
  
  # Subset data by city.
  df2 <-
    reactive({
      df2 <- droplevels(m[m$c1 == input$city | m$c2 == input$city,])})
  # Filter data.
  df3 <-
    eventReactive({
      input$modelFilter
      input$clusterFilter
      df2()
    },
    {
        req(input$modelFilter) #input$clusterFilter
        
        sub <- City_chars[City_chars$Continent %in% input$modelFilter,]
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
  
  # Filter by continent / cluster
  output$modelFilter <- renderUI({
    req(input$city)
    checkboxGroupInput(
      inputId = "modelFilter",
      label = "Filter by continent",
      choices = as.character(unique(City_chars$Continent)),
      selected = as.character(unique(City_chars$Continent))
    )
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
  
  output$plot1 <- renderPlot({
    req(input$table_rows_all)
    if(nrow(filtered_table() > 0 & !is.null(input$clusterFilter)
            & !is.null(input$modelFilter) )){
    plot(filtered_table()$distance, filtered_table()$distance, col = "red", lwd = 10)
    }
    })
  
}

# Run shiny.
shinyApp(ui = ui, server = server)