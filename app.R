library(shiny)
library(ggplot2)
library(lab5)
munNames <- read.csv("municipalities.csv", encoding = "UTF-8")
ui <- fluidPage(
  titlePanel("Kolada Data"),
  
  sidebarLayout(
    sidebarPanel(
      h3('Do the search'),
      #textInput('municipality',label = 'Type',value='Linköping'),
      selectInput('municipality',label = 'Select:',choices = as.vector(munNames$x), selected = 'Linköping'),
      # c('Stockholm','Linköping','Västerås')
      radioButtons("stats", h3("Please select statistics"),
                   choices = list("Death number" = 1, "Moving net" = 2,
                                  "Born child per 1000 residents" = 3,"Born child" =4),selected = 2)
      
    ),
    
    mainPanel(plotOutput("out"))
  )
)

# Define server logic ----
server <- function(input, output) {

  output$out <- renderPlot({
    ggplot(data=kolada()$get_stats(input$municipality,as.integer(input$stats)), aes(x=period, y=values)) +
      geom_line(stat="identity", color="blue") + geom_point(size=3, color="blue")
    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)