# Building, Updating, and Deploying a Shiny App to shinyapps.io
A few years back I [co-developed a metric](http://www.fangraphs.com/blogs/category/edge/) for measuring how often a pitcher threw to the edges of the strike zone versus in the heart of the zone. We called it Edge%. The metric [has evolved over the years](http://www.hardballtimes.com/expanding-the-
edges-of-the-strike-zone/), but the metric is not updated and hosted on any major baseball site. Since there is interest in updated Edge% throughout the season I decided to automate the process for pulling updated Edge% data and created a shiny app to host and display the data:

[![alt text](https://raw.githubusercontent.com/BillPetti/Scheduling-Shiny-App/master/images/Shiny_App_Screenshot.JPG)](https://billpetti.shinyapps.io/edge_shiny/)

The nice thing about shiny apps is that with only a few short steps you can create very functional, interactive visuals and share them widely.

Here's how I built and maintain this one.

R script for pulling Edge% data daily
------------------
I wrote a script in R that allows me to connet to a PITCHf/x database; in this case, the writers' database at FanGraphs. 

``` r
require(RMySQL)

con<-dbConnect(RMySQL::MySQL(), dbname="...", username = "...", password = "...", host = "...", port = ...)
```

Creat the shiny app
------------------
With the historical data in hand the next step is to build the shiny app. There are lots of tutorials on creating apps in shiny--[including from RStudio](http://shiny.rstudio.com/tutorial/)--so I won't get too deep here. 

The basics are you need both a server.R script and a ui.R script to create the app. The server.R script basically contains the instructions for how to build your app and how the data will be rendered (i.e. a table, plot, etc.), and the ui.R determines how your app will look and how users can interact with it.

For this app, I wanted to display the information as an interactive table, allowing users to sort by columns and filter results by season (or year) as well as restrict results based on some minimum number of pitches thrown or seen in a given season.

Here is the ui.R script for the app:	

``` r
library(shiny)

#load data that the selectInput below refers to
Pitchers <- read.csv("data/Pitchers_Edge.csv", header=TRUE, check.names = FALSE)

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
```

Because I have essentially two sets of data--pitchers and hitters--I decided on a tab-style layout. The app is rendering two seperate tables based on two different data sets. The data can be filtered by year through the `selectInput` function. The data can also be filtered using a `numericInput` by inputting a number that will restrict records based on a minimum number of pitches. I also included an About tab with references and a quick summary of what Edge% is.

Now for the server.R file:
``` r
library(shiny)

shinyServer(function(input, output) {

  output$table1 <- renderDataTable({
    Pitchers <- read.csv("data/Pitchers_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Pitchers <- Pitchers[Pitchers$Year == input$Year,]
    } 
    subset(Pitchers, Pitchers$'# of total pitches'>= input$pitch_total) 
  })
  
  output$table2 <- renderDataTable({
    Batters <- read.csv("data/Batters_Edge.csv", header=TRUE, check.names = FALSE)
    if (input$Year != "All"){
      Batters <- Batters[Batters$Year == input$Year,]
    } 
    subset(Batters, Batters$'# of total pitches'>= input$pitch_total)
  })
  
})

```

The server.R essentially tells the app to render the two tables based on the different datasets. Within each table render you will see two subsetting options for the data displayed--the Year filter as well as the minimum number of pitches. If you look back at the ui.R script, you can see that the first tab contains a call for table 1, which is the table that was rendered based on the pitcher data file. 

Now, both files should be saved in the same directory. To run the app locally on your computer you could simply run `runApp('directory_with_your_shiny_files')`. However, if you want to deploy the app and all the supporting files to the web you need to run the following:

``` r
deployApp('directory_with_your_shiny_files'')
```

And with that, you should an interactive shiny app!

But what about updating the data throughout the season?


Updating the data
---------------

First, you need to update the original Rscript so that it strips out all records for the current year:

``` r
require(dplyr)
Pitchers_old<-filter(Pitchers_old, Year < 2015)
```
reruns Edge% for the current year, merges the new current year data and the main data file:

``` r
Pitchers<-rbind(Pitchers_old, Pitchers_new)
```
overwrites the existing data files in the shiny app directory with the new files, and then redeploys the shiny app:

``` r
deployApp('directory_with_your_shiny_files'')
```

The database query is essentially unchanged, except that in the `WHERE` clause you restrict the `gamedate` value to the current year:

``` r
require(RMySQL)

res<-dbGetQuery(con, "SELECT ... WHERE substr(gamedate, 1,4) = 2015")
```

After that, you just need to redeploy the app exactly as before using `deployApp`.

Scheduling the shiny app to automatically update
-----------------------------------------------------------------
The entire process doesnt take long to run, but you don't want to have to manually run all the scripts every day. It's much easier to set the scripts up to run automatically at a schedule time everyday. There are lots of ways to do this--[here's just one example](https://github.com/BillPetti/Spray-Chart-Tool).

My deployed app can be viewed [here](https://billpetti.shinyapps.io/edge_shiny/).
