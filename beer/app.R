library(plyr)
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beer IBU and ABV data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons(
                        "plotType", 
                        "Plot type",
                         c("Histogram"="hist", "Boxplot"="box")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ibuPlot"),
           plotOutput("abvPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    beer = read.csv("./data/Beers.csv",header = TRUE)
    breweries = read.csv("./data/Breweries.csv",header = TRUE, strip.white = TRUE)
    
    attach(beer)
    beer[order(Brewery_id),] # sort the data to determine column for merge
    # merge on Brewery ID
    breweries_named <- plyr::rename(breweries, c("Brew_ID"="Brewery_id"))
    brewing_beer <- merge(breweries_named,beer,by="Brewery_id", all=TRUE) # outter join
    brewed_beer <- plyr::rename(brewing_beer, c("Name.x"="Brewery", "Name.y"="Beer")) # rename breweries and beer

    output$ibuPlot <- renderPlot({
        
        if(input$plotType=='hist')
        {
            hist(
                brewed_beer$IBU, 
                breaks = input$bins, 
                col = 'darkgray', 
                border = 'white', 
                xlab='IBU',
                main='Historam of IBU'
                )
        } else {
            boxplot(
                brewed_beer$IBU,
                col = 'darkgray', 
                xlab='IBU',
                main='Boxplot of IBU',
                horizontal = TRUE
            )
        }
    })
    
    output$abvPlot <- renderPlot({
        if(input$plotType=='hist')
        {
            hist(
                brewed_beer$ABV, 
                breaks = input$bins, 
                col = 'darkgray', 
                border = 'white', 
                xlab='ABV',
                main='Historam of ABV'
            )
        } else {
            boxplot(
                brewed_beer$ABV,
                col = 'darkgray', 
                xlab='ABV',
                main='Boxplot of ABV',
                horizontal = TRUE
            )
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
