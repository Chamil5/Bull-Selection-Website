# Load necessary packages
library(shiny)
library(dplyr)
library(pdftools)
library(shinyjs)  # Load shinyjs for additional features

# Define UI for application
ui <- fluidPage(
    useShinyjs(),  # Enable shinyjs functionality
    
    # Include custom CSS for styling
    tags$head(
        tags$style(HTML("
            body {
                background-color: #f5f5dc;  /* Light beige background */
                font-family: 'Georgia', serif; /* Stylish font */
            }
            .btn {
                background-color: #8b5a2b; /* Saddle brown button */
                color: white;
            }
            .sidebar {
                background-color: #d2b48c; /* Tan sidebar */
                border: 2px solid #8b5a2b; /* Saddle brown border */
                padding: 15px;
            }
            h1 {
                color: #8b4513; /* Saddle brown for headings */
            }
            th {
                background-color: #deb887; /* Burlywood header */
            }
            td {
                background-color: #fff8dc; /* Cornsilk body */
            }
        "))
    ),
    
    titlePanel("Bull EPD Selection"),
    
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"),
                      multiple = FALSE),  # Change for multiple files if needed
            actionButton("extractButton", "Extract EPDs"),
            uiOutput("slides"),
            sliderInput("weightInput", "Weight EPD:", 
                        min = 0, max = 100, value = c(0, 100)),
            sliderInput("milkInput", "Milk EPD:", 
                        min = 0, max = 100, value = c(0, 100)),
            sliderInput("qualityInput", "Quality EPD:", 
                        min = 0, max = 10, value = c(0, 10)),
            actionButton("filterButton", "Filter Bulls"),
            htmlOutput("progress")  # Place to show progress messages
        ),
        
        mainPanel(
            tableOutput("bullTable")
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Placeholder for bull data
    bulls <- reactiveVal(data.frame())
    
    observeEvent(input$extractButton, {
        req(input$pdfInput)  # Ensure a file is uploaded
        
        # Display progress message
        output$progress <- renderText("Extracting EPDs, please wait...")
        shinyjs::disable("extractButton")  # Disable button to prevent multiple clicks
        
        # Read PDF and extract text
        text <- pdf_text(input$pdfInput$datapath)
        extracted_data <- unlist(strsplit(text, "\n"))
        
        # Regex to extract EPDs
        epd_pattern <- "ID: (\\d+), Name: ([A-Za-z\\s]+), Weight: (\\d+), Milk: (\\d+), Quality: (\\d+\\.\\d+)"
        bulls_data <- data.frame(matrix(NA, ncol=5, nrow=0))
        colnames(bulls_data) <- c("ID", "Name", "EPD_Weight", "EPD_Milk", "EPD_Quality")
        
        for (line in extracted_data) {
            if (grepl(epd_pattern, line)) {
                match <- regmatches(line, regexec(epd_pattern, line))
                bulls_data <- rbind(bulls_data, 
                                    data.frame(ID = as.integer(match[[1]][2]),
                                               Name = as.character(match[[1]][3]),
                                               EPD_Weight = as.numeric(match[[1]][4]),
                                               EPD_Milk = as.numeric(match[[1]][5]),
                                               EPD_Quality = as.numeric(match[[1]][6]),
                                               stringsAsFactors = FALSE))
            }
        }
        
        # Update the bulls data frame
        bulls(bulls_data)
        
        # Dynamically update slider inputs based on the new data
        output$slides <- renderUI({
            if (nrow(bulls()) > 0) {
                tagList(
                    sliderInput("weightInput", "Weight EPD:", 
                                min = min(bulls()$EPD_Weight), 
                                max = max(bulls()$EPD_Weight), 
                                value = c(min(bulls()$EPD_Weight), max(bulls()$EPD_Weight))),
                    sliderInput("milkInput", "Milk EPD:", 
                                min = min(bulls()$EPD_Milk), 
                                max = max(bulls()$EPD_Milk), 
                                value = c(min(bulls()$EPD_Milk), max(bulls()$EPD_Milk))),
                    sliderInput("qualityInput", "Quality EPD:", 
                                min = min(bulls()$EPD_Quality), 
                                max = max(bulls()$EPD_Quality), 
                                value = c(min(bulls()$EPD_Quality), max(bulls()$EPD_Quality)))
                )
            }
        })
        
        # Clear progress message
        output$progress <- renderText("EPDs extracted successfully!")
        shinyjs::enable("extractButton")  # Re-enable button
    })
    
    filteredBulls <- reactiveVal(data.frame())
    
    observeEvent(input$filterButton, {
        filteredBulls(bulls() %>%
                          filter(EPD_Weight >= input$weightInput[1],
                                 EPD_Weight <= input$weightInput[2],
                                 EPD_Milk >= input$milkInput[1],
                                 EPD_Milk <= input$milkInput[2],
                                 EPD_Quality >= input$qualityInput[1],
                                 EPD_Quality <= input$qualityInput[2]))
    })
    
    output$bullTable <- renderTable({
        filteredBulls()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
