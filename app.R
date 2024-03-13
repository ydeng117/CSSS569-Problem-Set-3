library(gapminder)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(RColorBrewer)
library(shiny)

source("data_cleaning.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application theme settings
    shinythemes::themeSelector(),
    
    # Application title
    titlePanel(
        "Relation between Total Fertility Rate and Women's Mean Education in Productive Age from 1970 to 2009"
    ),
    
    # Sidebar with a slider and select input for number of bins
    sidebarLayout(
        sidebarPanel(
            # choosing the years for the analysis
            sliderInput(
                "year",
                label = h3("Choose a year:"),
                min = 1970,
                max = 2009,
                value = 1996
            ),
            # Select a group of region for display
            checkboxGroupInput(
                "region_group",
                label = h3("Filter by region group:"),
                choices = unique(tfr_women_edu_df$four_regions),
                selected = "asia"
            ),
            hr(),
            # Add a horizontal rule
            checkboxInput("smooth", "Add smoother"),
            checkboxInput("facet", "Small multiples")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    subset_data <- reactive({
        req(input$region_group)
        tfr_women_edu_df %>%
            filter(four_regions %in% input$region_group) %>%
            filter(Year == input$year)
    })
    
    output$distPlot <- renderPlot({
        p <- ggplot(
            subset_data(),
            aes(
                x = Mean_edu_year_women,
                y = Total_Fertility_Rate,
                color = four_regions
            )
        ) +
            
            list(
                theme_minimal(),
                geom_point(alpha = 0.55, size = 3),
                if (input$smooth)
                    geom_smooth(
                        inherit.aes = FALSE,
                        aes(x = Mean_edu_year_women,
                            y = Total_Fertility_Rate),
                        method = "lm",
                        se = TRUE,
                        color = "black"
                    ),
                stat_cor(
                    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                    label.x = 0.5,
                    label.y = 0.9,
                    size = 5,
                    color = "black"
                ),
                geom_text_repel(
                    aes(label = country),
                    size = 2.5,
                    alpha = 0.60,
                    segment.size = 0.2
                ),
                scale_color_brewer(palette = "Set1"),
                labs(
                    title = "Relation between Total Fertility Rate and Women's Mean Education in Productive Age",
                    x = "Women's Mean Education in Productive Age (15 to 44)",
                    y = "Total Fertility Rate (babies per women)",
                    color = "Region"
                ),
                scale_x_continuous(breaks = seq(0, 16, by = 2)),
                scale_y_continuous(
                    breaks = seq(0, 10, by = 2),
                    limits = c(-.1, 9)
                ),
                
                if (input$facet)
                    facet_wrap(~ four_regions)
                
            )
        
        p
        
    }, height = 600, width = 900)
}

# Run the application
shinyApp(ui = ui, server = server)
