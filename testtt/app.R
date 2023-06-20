#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#setting working directory 
#setwd("C:/Users/gwiggins/Documents/hanover-DSPG-2023/data/soil")

#reading in packages
library(readxl)
library(ggplot2)
library(viridis)
library(sf)
library(shinydashboard)
library(DT)
library(bslib)
#assigning the data file path to soil excel
soil_excel <- "soillabels.xlsx"

#reading in soil excel and assigning it to soil data
soil_data <- read_excel(soil_excel)

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_data <- soil_data[, c("Rating", "Acres in AOI")]

#re-naming Rating and Acres in Aoi columns to soil rating and acres
colnames(rateacre_data) <- c("soil_rating", "acres")
rateacre_data_clean <- rateacre_data[-c(119,120), ]

#turning soil rating column to a factor to use viridis 
rateacre_data_clean$soil_rating <- factor(rateacre_data_clean$soil_rating) 



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Soils In Hanover County"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    
    theme = bs_theme(
      
      bootswatch = "darkly"
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      #creating a sideways bar plot showing the acres of each rating in the county with viridis
      ggplot(rateacre_data_clean, aes(x = soil_rating, 
                                      y = acres, 
                                      fill = soil_rating)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_viridis_d(option = "viridis") +
        theme(legend.position = "none") +
        labs(x = "Soil Rating", 
             y = "Acreage", 
             title = "USDA Soil Rating by Acerage in Hanover County", 
             caption = "Data Source: USDA, NRCC Web Soil Survey, 2019")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
