#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)

library(rsconnect)
library(lubridate)
library(rlang)
library(cli)
library(flexdashboard)

library(shiny)
library(plotly)
library(lubridate) # For month() function

data <- read.csv("Bollywood.csv")
data$Release.Date <- as.Date(data$Release.Date,format = "%d-%b-%y")
data_sorted <- data[order(data$Release.Date), ]
data_sorted$CollectiontoBudget_ROI <- data_sorted$Box.Office.Collection / data_sorted$Budget
data_sorted$RelMonth <- month(data_sorted$Release.Date, abbr = TRUE, label = TRUE)
data_sorted$RelYear <- format(data_sorted$Release.Date, "%Y")

# Define the UI
ui <- fluidPage(
  fluidRow(
    column(width = 3, offset = 9,
           selectInput("variable", "Select a Measure:", choices = c("Budget", "Box.Office.Collection", "Youtube.Views", "Youtube.Likes", "Youtube.Dislikes", "CollectiontoBudget_ROI")),
           selectInput("genre", "Select Genre:", 
                              choices = c("All Genres" = "All", unique(data_sorted$Genre)),
                              selected = "All",multiple =TRUE) # Default to "All Genres"
    ),
    column(width = 7,
           plotlyOutput("plot1"),
           plotlyOutput("plot2"),
           plotlyOutput("plot3"),
           plotlyOutput("plot4"),
           plotlyOutput("plot5"),
           plotlyOutput("plot6"),
           plotlyOutput("plot7")
    )
  )
)
#server logic:
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$variable)
    
    # Handle multiple genre selections
    if ("All" %in% input$genre) {
      genre_filtered_data <- data_sorted
    } else {
      genre_filtered_data <- data_sorted[data_sorted$Genre %in% input$genre, ]
    }
    
    genre_filtered_data
  })
  
  output$plot1 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p1 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~Genre, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          title = list(text = "Genre"),
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p1
  })
  
  output$plot2 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p2 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~RelMonth, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          title = list(text = "Rel Month"),
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p2
  })
  
  output$plot3 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p3 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~Release.Date..N...LW...Festive., type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          title = list(text = "Season"),
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p3
  })
  
  output$plot4 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p4 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~RelYear, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          title = list(text = "Release Year"),
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p4
  })
  
  output$plot5 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p5 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~Youtube.Likes, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p5
  })
  
  output$plot6 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p6 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~Youtube.Dislikes, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p6
  })
  
  output$plot7 <- renderPlotly({
    req(input$variable)
    var <- input$variable
    p7 <- plot_ly(filtered_data(), x = ~Movie.Name, y = ~get(var), color = ~Youtube.Views, type = 'bar') %>%
      layout(
        title = list(
          text = paste(var),
          font = list(size = 15)),
        xaxis = list(title = "Movie Name"),
        yaxis = list(title = var),
        autosize = TRUE,
        width = 800,
        height = 500,
        legend = list(
          x = 1,
          y = .5,
          orientation = 'v'
        )
      )
    p7
  })
}
# Run the application
shinyApp(ui = ui, server = server)