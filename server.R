library(shiny)

shinyServer(function(input, output) {
  
  Pitchers <- reactive({read.csv("data/Pitchers_Edge.csv", header=TRUE, check.names = FALSE)})
  Batters <- reactive({read.csv("data/Batters_Edge.csv", header=TRUE, check.names = FALSE)})
  
  output$table1 <- renderDataTable({
    Pitchers <- Pitchers()
    if (input$Year != "All"){
      Pitchers <- Pitchers[Pitchers$Year == input$Year,]
    } 
    subset(Pitchers, Pitchers$'# of total pitches'>= input$pitch_total) 
  })
  
  output$table2 <- renderDataTable({
    Batters <- Batters()
    if (input$Year != "All"){
      Batters <- Batters[Batters$Year == input$Year,]
    } 
    subset(Batters, Batters$'# of total pitches'>= input$pitch_total)
  })
  
})