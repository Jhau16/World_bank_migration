# Make sure to install necessary packages first.
# You can do this by running: install.packages(c("shiny", "ggplot2", "dplyr", "readxl"))

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(hrbrthemes)

# Load the data
#data <- read_excel("path/to/your/dataset.xlsx")

migration_data <- Totalemigrantstock

# Define the user interface
ui <- fluidPage(
  
  # App title
  titlePanel("Emigrant stock in cross-section per 1,000,000 People vs GDP per Capita"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select year
      selectInput("year", "Select year:", choices = unique(migration_data$year)),
      
      # Input: Select smoothing parameter
      sliderInput("span", "Select smoothing parameter:", min = 0.01, max = 1, value = 0.7),
      
      # Input: Select region
      checkboxGroupInput("region", "Select region:", choices = unique(migration_data$region), selected = unique(migration_data$region))
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Plot
      plotlyOutput("plot")
      
    )
  )
  
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on selected year
  filteredData <- reactive({
    Totalemigrantstock %>% 
      filter(year == input$year, region %in% input$region)
  })

  
  # Render plot based on selected year and smoothing parameter
  output$plot <- renderPlotly({
    
      p <- ggplot(filteredData(), aes(x = gdpcap, y = migpop, color = region)) +
      geom_smooth(method = "loess", se = FALSE, span = input$span, level = 0.9, color = "Dark Grey") +
      geom_point(aes(text = country), alpha = 0.7) +
      scale_x_continuous(trans = "log", breaks = c(0, 500, 5000, 50000), limits = c(500, 50000)) +
      scale_color_wsj() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_hc() +
      labs(color = "\   Region:" ,x = "GDP per Capita (Log Scale)", y = "Number of Migrants per 1,000,000 People",
           title = paste("Stock per 1,000,000 People vs GDP/Capita for the Year", input$year))

      ggplotly(p, tooltip = "text")
  })
  
}


# Create Shiny app
shinyApp(ui = ui, server = server)
