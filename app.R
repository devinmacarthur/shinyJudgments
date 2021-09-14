library(dplyr)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(leaflet)
library(DT)
source("loadGlobals.R")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Judgments at a Glance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                inputId = "counties", 
                label = "Select Counties", 
                choices = unique(flat_file$location), 
                selected = unique(flat_file$location),
                multiple = TRUE),
            selectizeInput(
                inputId = "monthWeek", 
                label = "Select month or week", 
                choices = c("month", "week"), 
                selected = "month",
                multiple = FALSE)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    output$distPlot3 <- renderPlotly({
        
        county <- flat_file %>% filter(location %in% input$counties) %>% pull(case_code)
        
        judgments %>%
            filter(case_code %in% county) %>%
            group_by(MW = lubridate::floor_date(as.Date(date, "%m/%d/%Y"), input$monthWeek), 
                     case_type) %>% 
            summarise(count = n()) %>% 
            plot_ly(x = ~MW, y = ~count, color = ~case_type, type = "scatter", mode = 'line',
                    hovertemplate=paste("%{yaxis.title.text}: %{y}<br>",
                                        "%{x}")) %>% 
            layout(showlegend=FALSE,
                   height = 700,
                   xaxis = list(title = ""),
                   yaxis = list(title = "Judgments"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
