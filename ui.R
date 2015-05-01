library(shiny)

shinyUI(fluidPage(
  titlePanel('Edge%: 2010-Present'),
  fluidRow(
    column(12,
           p("Provides current and historical data on the percentage of pitches thrown to different parts of the strike zone by pitchers and to batters"),
           p("Created and maintained by Bill Petti",
             a("(@BillPetti)", href = "https://twitter.com/billpetti")),
           p("Data last updated",Sys.time()))
  ), 
    sidebarLayout(
    sidebarPanel(selectInput("Year", 
                                    "Year:", 
                                    c("All", 
                                      unique(as.character(Pitchers$Year)))),
                 numericInput("pitch_total", 
                             label = "Minimum # of Pitches:",
                             value = 300)
  )
  ,
  mainPanel(
    tabsetPanel(
      tabPanel("Pitchers", dataTableOutput(outputId = 'table1')),
      tabPanel("Batters", dataTableOutput(outputId = 'table2')),
      tabPanel("About", 
               br(), h1("About Edge%"), 
               br(), p("A few years ago, Jeff Zimmerman and I created a metric to represent how often a pitcher threw to the edges of the strike zone compared to the heart of the strike zone. The result was Edge%. The metric has evolved to include separate metrics for different edges (upper, lower, etc.). In the image below, the brown shaded areas represent the horizontal edges of the strike zone, the blue the top, and the green the bottom edges. You will notice the horizontal edges differ by batter handedness, given how umpires actually call balls and strikes."), 
               br(), img( src = "edge_image.png", height = 350, width = 700), 
      br(), p("Edge% is useful in a number of contexts. For example, we know that as pitchers age they lose velocity and therefore need to avoid throwing to the heart of the plate to be successful. Edge% provides a quick look at who is adjusting to lower velocity and who isn't. It can also be used to see how pitchers are adjusting to hitters as they age (i.e. as hitters improve, pitchers may avoid the heart of the plate more, or as hitters decline they may begin challenge them more."), 
      br(), p("For more information on Edge%, check out these articles:"), 
              br(), a("Introduction to Edge%", href = "http://www.fangraphs.com/blogs/the-difference-pitching-on-the-edge-makes/"), 
              br(), br(), a("Collection of Articles Using and Expanding on Edge%", href = "http://www.fangraphs.com/blogs/category/edge/"),
              br(), br(), a("Most Recent Version", href = "http://www.hardballtimes.com/expanding-the-edges-of-the-strike-zone/") 
      )
    )))))