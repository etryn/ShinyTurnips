# ShinyTurnips
# This is a Shiny App to log and visualize current turnip prices for Ceylon & friends
# Author: Veronica Davé, 16may2020
# https://github.com/etryn/ShinyTurnips

library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Load List of Friends and Favorite Colors
friends <- read.table("friends.txt", header = TRUE)
friendNames <- friends$name
friendColors <- friends$color
names(friendColors) <- friendNames

# Designate Mandatory Fields
fieldsMandatory <- c("name", "date", "ampm", "price") #Date is mandatory as well but will be independently evaluated for length > 0

# Designate which fields to save
fieldsAll <- c("name", "date", "ampm", "price")
responsesDir <- file.path("responses")

# Define data to use for table and graph
loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    data <- dplyr::bind_rows(data)
}

# Manipulate data into viewer-friendly table for current week
currentData <- function(data) {
    data <- data %>%
        pivot_wider(names_from = name, values_from = price) %>%
        arrange(date, ampm) %>%
        mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        mutate(
               the.year = year(date),
               the.week = epiweek(date)) %>%
        filter(the.year == max(the.year) & the.week == max(the.week)) %>%
        mutate(date = format(date, "%a %b %d")) %>%
        select(-the.year, -the.week, Date = date, Window = ampm)
    data
}

# Manipulate data into viewer-friendly table for all weeks
allData <- function(data) {
    data <- data %>%
        pivot_wider(names_from = name, values_from = price) %>%
        arrange(desc(date), desc(ampm)) %>%
        mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        select(Date = date, Window = ampm, everything())
    data
}

# Manipulate data for plotting and create ggplot
plotData <- function(data) {
    data <- data %>%
        arrange(date, ampm) %>%
        mutate(date = as.Date(date, origin = "1970-01-01")) %>%
        mutate(
            the.year = year(date),
            the.week = epiweek(date)) %>%
        filter(the.year == max(the.year) & the.week == max(the.week)) %>%
        mutate(date = format(date, "%a")) %>%
        mutate(datetime = paste(date, ampm, sep = " ")) %>%
        mutate(datetime = case_when(
            datetime == "Sun am" ~ "Purchase",
            datetime == "Sun pm" ~ "Purchase",
            TRUE ~ datetime)
        ) %>%
        mutate(name = factor(name, levels = friendNames)) %>%
        mutate(datetime = factor(datetime, levels = c(
            "Purchase",
            "Mon am",
            "Mon pm",
            "Tue am",
            "Tue pm",
            "Wed am",
            "Wed pm",
            "Thu am",
            "Thu pm",
            "Fri am",
            "Fri pm",
            "Sat am",
            "Sat pm")))
               
    ggplot(data = data, aes(x = datetime, y = price, group = name, color = name)) +
        geom_point(aes(size = price)) +
        geom_line() +
        ylim(20,max(200, ceiling(max(data$price)/100)*100)) +
        theme_bw() +
        scale_color_manual(values = friendColors) +
        scale_size(
            limits = c(0,700),
            breaks = c(100, 200, 300, 400, 500, 600, 700)) +
        theme(
            axis.title.x=element_blank(),
            legend.title=element_blank(),
            axis.title.y=element_text(size=14),
            axis.text=element_text(size=11, color = "black"),
            axis.text.x=element_text(angle = 90, hjust = 1, vjust = .5),
            legend.text=element_text(size=12)
            ) +
        ylab(expression("Price"))
        
}

# UI
ui = fluidPage(
    
    #Set Meta Tags and Header Info
    shinyjs::useShinyjs(),
    tags$head(
        shiny::tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
        shiny::tags$meta(name = "mobile-web-app-capable", content = "yes"),
        shiny::tags$link(rel = "manifest", href = "manifest.json")
    ),
    
    #Define Form
    titlePanel("Ceylon Stalk Market"),
    
    #Sidebar for all data entry fields and buttons
    sidebarLayout(
        sidebarPanel(
            
            #Main Entry Form
            div(
                id = "form",
                radioButtons("name", "Name",
                             choiceNames = friendNames,
                             choiceValues = friendNames,
                             selected = character(0)),
                dateInput("date", "Date",
                          value = NULL,
                          min = Sys.Date() - 3,
                          max = Sys.Date() + 3,
                          format = "D mm/dd/yy"),
                radioButtons("ampm", "AM or PM Price",
                             choiceNames = c("AM", "PM"),
                             choiceValues = c("am", "pm"),
                             selected = character(0)),
                numericInput("price", "Purchase or Sell Price",
                             value = NULL,
                             min = 20,
                             max = 800),
                actionButton("submit", "Submit", class = "btn-primary"),
                
                #Add Refresh Button
                shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }", functions = "refresh"),
                actionButton("refresh", "Refresh", class = "btn-info"),
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
    
        #Main Panel for Table and Graph Views
        mainPanel(
            tabsetPanel(
                
                #View Current Week's Prices
                tabPanel("Table", DT::dataTableOutput("responsesTable")),
                
                #View Current Week's Graph
                tabPanel("Graph", plotOutput("plot")),
                
                #View Historic Prices
                tabPanel("Archive", DT::dataTableOutput("archiveTable"))
            )
        )
    ),
    hr(),
    print(tags$a(href="https://github.com/etryn/ShinyTurnips", "ShinyTurnips")),
    print("by etryn")
)


# Server
server <- function(input, output, session) {
    
    # Enable the Submit button only when all mandatory fields are filled out
    observe({
        mandatoryFilled <- # Check if string and numeric fields are filled out
            vapply(fieldsMandatory,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                   },
                   logical(1))
        mandatoryFilled <- c(mandatoryFilled, length(input$date) > 0) # Check if date field is filled out
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
    
    # Refresh Button
    observeEvent(input$refresh, {
        shinyjs::js$refresh()
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
    
    #Display Data as Table - Current Week
    output$responsesTable <- DT::renderDataTable(
        currentData(data),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE, dom = 't', ordering = FALSE)
    ) 
    
    #Display Data as Table - All Historic Data
    output$archiveTable <- DT::renderDataTable(
        allData(data),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE, pageLength = 10)
    ) 
    
    #Display Data as Plot
    output$plot <- renderPlot(
        plotData(data)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
