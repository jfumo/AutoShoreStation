#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rerddap)
library(xts)
library(dplyr)       
library(dygraphs)
library(shinythemes)
library(lubridate)

pwd <- setwd(getwd())

filtered_data <- read_csv(paste0(pwd,"/../TRIScorrectedData.csv"))

#Variables you want to read 
variables = c('time', 'temperature', 'pH')

endDate = as.Date(max(ymd_hms(filtered_data$time)))
startDate = endDate - 7
minDate = as.Date(min(filtered_data$time))

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("cerulean"),

    # Application title
    titlePanel("SIO Observations"),

        # Show a plot of the generated distribution
        mainPanel(
            sliderInput(inputId = "Date",
                        label = "Time Interval",
                        width = '100%',
                        min = minDate,
                        max = endDate,
                        value = c(startDate, endDate)),
            dygraphOutput("temp", width = "100%", height = "200px"),
            br(),
            br(),
            dygraphOutput("ph", width = "100%", height = "200px")
        )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    require(dygraphs)

    sio_data <- filtered_data

#    temp_ts = xts(x = sio_data$temperature, order.by =  as_datetime(sio_data$numtime))
    temp_ts = xts(x = sio_data$temperature, order.by =  sio_data$time)
    ph_ts = xts(x = sio_data$pH, order.by = sio_data$time)

    output$temp <- renderDygraph({
        dygraph(temp_ts,
                ylab = "Temp_C", 
                group="sioObs") %>%
        dyRangeSelector(height = 2, dateWindow = c(input$Date[1], input$Date[2]))
    })
    output$ph <- renderDygraph({
                dygraph(ph_ts,
                ylab = "pH", 
                group="sioObs") %>%
            dyRangeSelector(height = 2, dateWindow = c(input$Date[1], input$Date[2]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
