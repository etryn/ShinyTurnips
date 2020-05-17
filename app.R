#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(base)

# List of Ceylon Participants
ceylonMembers <- c("Ashley", "Veronica", "Janessa", "Judy", "Andre", "WS", "Dani", "KGB", "Rachel", "Julie")

# Designate Mandatory Fields
fieldsMandatory <- c("name", "date", "ampm", "price", "firstweek")

# Define UI for application that draws a histogram
ui = fluidPage(
    
    shinyjs::useShinyjs(),
    
    #Define Form
    titlePanel("Ceylon Stalk Market"),
    div(
        id = "form",
        selectInput("name", "Name", ceylonMembers),
        dateInput("date", "Date",
                  value = NULL,
                  min = "2020-03-19",
                  max = NULL,
                  format = "D mm/dd/yy"),
        radioButtons("ampm", "AM or PM Price",
                     choiceNames = c("AM", "PM"),
                     choiceValues = c("am", "pm")),
        numericInput("price", "Current Price",
                     value = NULL,
                     min = 20,
                     max = 800),
        checkboxInput("firstweek", "This is my first week buying turnips in my own town.", value = FALSE),
        actionButton("submit", "Submit", class = "btn-primary")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    


}

# Run the application 
shinyApp(ui = ui, server = server)
