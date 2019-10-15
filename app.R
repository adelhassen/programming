#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Work in Progress, add slider input, add opponents, add more games
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(rsconnect)

#Import dataset bball
data <- bball
data1 <- bball %>% select(PTS, AST, TRB, X3P.,FG.)

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("darkly"),
   
   # Application title
   titlePanel("Basketball Players"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("stat", label = "Choose the statistic to analyze: ", names(data1)),
         radioButtons("location", label = "Show games at home or away: ",
                     choices= c("H" = "H", "A" = "A"), selected= "H"),
         radioButtons("result", label = "Show games that the player won or lost: ",
                     choices= c("W" = "W", "L" = "L"), selected= "W")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
   output$plot <- renderPlotly({
     
     data = as_tibble(data)
     plot_stat <- data %>% filter(Location == input$location & Result == input$result)
     
     if (input$stat == "PTS")
       x <- ggplot(plot_stat, (aes(Game, PTS, color = Player))) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(0,11,1))
     if (input$stat == "AST")
       x <- ggplot(plot_stat, (aes(Game, AST, color = Player))) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(0,11,1))
     if (input$stat == "TRB")
       x <- ggplot(plot_stat, (aes(Game, TRB, color = Player))) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(0,11,1))
     if (input$stat == "X3P.")
       x <- ggplot(plot_stat, (aes(Game, X3P., color = Player))) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(0,11,1))
     if (input$stat == "FG.")
       x <- ggplot(plot_stat, (aes(Game, FG., color = Player))) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(0,11,1))
     
     ggplotly(x)
    
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

