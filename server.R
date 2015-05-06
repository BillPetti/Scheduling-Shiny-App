library(shiny)

shinyServer(function(input, output) {
  
  output$table1 <- renderDataTable({
    Pitchers <- read.csv("data/Pitchers_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Pitchers <- Pitchers[Pitchers$Year == input$Year,]
    } 
    subset(Pitchers, Pitchers$'# of total pitches'>= input$pitch_total) 
  }, options = list(lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'All')),
  pageLength = 15
  ))
  
  output$table2 <- renderDataTable({
    Batters <- read.csv("data/Batters_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Batters <- Batters[Batters$Year == input$Year,]
    } 
    subset(Batters, Batters$'# of total pitches'>= input$pitch_total)
  }, options = list(lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'All')),
                    pageLength = 15
  ))
  
  output$table3 <- renderDataTable({
    Team_P <- read.csv("data/Team_P_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Team_P <- Team_P[Team_P$Year == input$Year,]
    } 
    subset(Team_P, Team_P$'# of total pitches'>= input$pitch_total)
  }, options = list(lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'All')),
                    pageLength = 15
  ))
  
  output$table4 <- renderDataTable({
    Team_B <- read.csv("data/Team_B_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Team_B <- Team_B[Team_B$Year == input$Year,]
    } 
    subset(Team_B, Team_B$'# of total pitches'>= input$pitch_total)
  }, options = list(lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'All')),
                    pageLength = 15
  ))
  
})