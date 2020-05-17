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
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# List of Ceylon Participants
ceylonMembers <- c("Ashley", "Veronica", "Janessa", "Judy", "Andre", "WS", "Dani", "KGB", "Rachel", "Julie")

# Designate Mandatory Fields
fieldsMandatory <- c("name", "price", "firstweek")

# Designate which fields to save
fieldsAll <- c("name", "date", "ampm", "price", "firstweek")
responsesDir <- file.path("responses")

# Define data to display live in app
loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    data <- dplyr::bind_rows(data)
}

tableData <- function(data) {
    data <- data %>%
        pivot_wider(names_from = name, values_from = price) %>%
        arrange(date, ampm) %>%
        mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        mutate(
               the.year = year(date),
               the.week = epiweek(date)) %>%
        filter(the.year == max(the.year) & the.week == max(the.week)) %>%
        mutate(date = format(date, "%a %b %d")) %>%
        select(-firstweek, -the.year, -the.week, Date = date, Window = ampm)
    data
}

plotData <- function(data) {
    data <- data %>%
        arrange(date, ampm) %>%
        mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        mutate(
            the.year = year(date),
            the.week = epiweek(date)) %>%
        filter(the.year == max(the.year) & the.week == max(the.week)) %>%
        mutate(date = format(date, "%a")) %>%
        mutate(datetime = paste(date, ampm, sep = " "))
               
    ggplot(data = data, aes(x = datetime, y = price, group = name, color = name)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        theme(
            axis.title.x=element_blank(),
            legend.title=element_blank(),
            axis.title.y=element_text(size=14),
            axis.text=element_text(size=11, color = "black"),
            legend.text=element_text(size=12)
            ) +
        ylab(expression("Price"))
        
}

# UI
ui = fluidPage(
    
    shinyjs::useShinyjs(),
    
    #Define Form
    titlePanel("Ceylon Stalk Market"),
    
    sidebarLayout(
        sidebarPanel(
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
                numericInput("price", "Purchase or Sell Price",
                             value = NULL,
                             min = 20,
                             max = 800),
                checkboxInput("firstweek", "This is my first week buying turnips in my own town.", value = FALSE),
                actionButton("submit", "Submit", class = "btn-primary")
            ),
            
            #Submission Successful Message
            shinyjs::hidden(
                div(
                    id = "submitted_msg",
                    h3("Your turnip price has been logged!"),
                    actionLink("submit_another", "Submit another price")
                )
            ),
                
            #Error Message
            shinyjs::hidden(
                span(id = "loading_msg", "Logging your price..."),
                div(id = "error",
                    div(br(), tags$b("Error: Something went wrong"), span(id = "error_msg"))
                )
            )
            
        ),
    
        mainPanel(
            tabsetPanel(
                
                #View Current Week's Prices
                tabPanel("Table", DT::dataTableOutput("responsesTable")),
                
                #View Current Week's Graph
                tabPanel("Graph", plotOutput("plot"))
            )
        )
    )
)


# Server
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
    
    # When submit button is pressed, acknowledge and hide/reset form
    observeEvent(input$submit, {
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("submitted_msg")
    })
    
    # Display new form when "submit another price" is chosen
    observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("submitted_msg")
    })    
    
    # Add loading and error messages
    observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::show("loading_msg")
        shinyjs::hide("error")
        
        tryCatch({
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("submitted_msg")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
            shinyjs::hide("loading_msg")
        })
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
    
    #Output Raw Data
    data <- loadData()
    
    #Display Data as Table
    output$responsesTable <- DT::renderDataTable(
        tableData(data),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
    ) 
    
    #Display Data as Plot
    output$plot <- renderPlot(
        plotData(data)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
