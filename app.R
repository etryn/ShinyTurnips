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
fieldsMandatory <- c("name", "price", "firstweek")

# Designate which fields to save
fieldsAll <- c("name", "date", "ampm", "price", "firstweek")
responsesDir <- file.path("responses")

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
    
    # Enable the Submit button only when all mandatory fields are filled out
    observe({
        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })

    # Transpose Data Into a One-Row Entry
    formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- t(data)
        data
    })
    
    # Save CSV File of Form Responses
    saveData <- function(data) {
        fileName <- paste(paste(data[1,1], data[1,2], data[1,3], sep = "_"), ".csv", sep = "")
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
    }
    
    # Save Data
    observeEvent(input$submit, {
        saveData(formData())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
