library(gapminder)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(RColorBrewer)
library(firatheme)
library(shiny)
source("data_cleaning.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application theme settings
    shinythemes::themeSelector(),

    # Application title
    titlePanel(""),

    # Sidebar with a slider and select input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # choosing the years for the analysis
            sliderInput("year",
                        label = h3("Choose a year:"),
                        min = 1950,
                        max = 2021,
                        value = 1996),
          # Select a region for display
            selectInput("region",
                        label = h3("Filter by region:"),
                        choices = unique(tfr_women_edu_df$four_regions),
                        selected = "asia")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
