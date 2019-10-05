library(shiny)
library(ggplot2)
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
  kolada <- setRefClass("kolada",
                        field = list(),
                        methods = list(
                          get_municipality_list = function(){
                            'Get the complete municipality list'
                            path_get_mu <- "http://api.kolada.se/v2/municipality"
                            api_raw_ret <- httr::GET(url = path_get_mu)
                            api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                            municipality_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                            municipality_list <- municipality_list_raw[,c(2,3)]
                            colnames(municipality_list) = c('id','municipality')
                            return(municipality_list)
                          },
                          get_id = function(Mname){
                            'Get the id of a municipality'
                            mlist <- get_municipality_list()
                            id <- mlist$id[mlist$municipality==Mname]
                            return(id)
                          },
                          get_numb = function(Mname, op){
                            'Get the statistics'
                            #1 dead
                            #2 moving net
                            #3 born / 1000
                            #4 born numb
                            try(if (!(op %in% c(1:4))) stop("not one of the provided options"))
                            
                            start_path <- "http://api.kolada.se/v2/data/kpi"
                            if (op==1){
                              n<-"N01805"
                              uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                              api_raw_ret <- httr::GET(url = uurl)
                              api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                              polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                              
                              vvec <- rep(0, length(polulation_list_raw$values.values))
                              for (i in 1:length(polulation_list_raw$values.values)){
                                vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                              }
                              
                              df <- data.frame("period"=polulation_list_raw$values.period)
                              df$values <- vvec 
                              
                            }else if (op==2){
                              n<-"N01800"
                              uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                              api_raw_ret <- httr::GET(url = uurl)
                              api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                              polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                              
                              vvec <- rep(0, length(polulation_list_raw$values.values))
                              for (i in 1:length(polulation_list_raw$values.values)){
                                vvec[i] <- polulation_list_raw$values.values[[i]][[4]][[3]]
                              }
                              df <- data.frame("period"=polulation_list_raw$values.period)
                              df$values <- vvec 
                              
                              
                            }else if (op==3){
                              n<-"N02951"
                              uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                              api_raw_ret <- httr::GET(url = uurl)
                              api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                              polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                              
                              vvec <- rep(0, length(polulation_list_raw$values.values))
                              for (i in 1:length(polulation_list_raw$values.values)){
                                vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                              }
                              df <- data.frame("period"=polulation_list_raw$values.period)
                              df$values <- vvec 
                              
                            }else {
                              n<-"N01804"
                              uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                              api_raw_ret <- httr::GET(url = uurl)
                              api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                              polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                              
                              vvec <- rep(0, length(polulation_list_raw$values.values))
                              for (i in 1:length(polulation_list_raw$values.values)){
                                vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                              }
                              df <- data.frame("period"=polulation_list_raw$values.period)
                              df$values <- vvec 
                            }
                            
                            return(df)
                          }
                          
                          
                        )
  )
  
  output$out <- renderPlot({
    ggplot(data=kolada()$get_numb(input$municipality,as.integer(input$stats)), aes(x=period, y=values)) +
      geom_bar(stat="identity", color="blue", fill="white")
    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)