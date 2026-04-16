library(shiny)
library(dplyr)
library(pdftools)
library(shinyjs)
library(rmarkdown)

# Set maximum request size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

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
            .info-box {
                background-color: #fffacd;
                border-left: 4px solid #8b5a2b;
                padding: 10px;
                margin: 10px 0;
            }
        "))
    ),
    
    titlePanel("Bull EPD Selection"),
    
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"),
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs"),
            br(), br(),
            
            # Filter criteria section
            h4("Filter Criteria"),
            uiOutput("slides"),
            actionButton("filterButton", "Filter Bulls", class = "btn btn-primary"),
            br(), br(),
            
            # Generate report section
            h4("Generate Report"),
            textInput("reportTitle", "Report Title", 
                      value = "Bull Selection Report"),
            downloadButton("downloadReport", "Download Report as PDF", 
                           class = "btn btn-success"),
            br(), br(),
            
            htmlOutput("progress")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Filtered Bulls",
                         tableOutput("bullTable")
                ),
                tabPanel("Summary Statistics",
                         verbatimTextOutput("summaryStats")
                )
            )
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
        output$progress <- renderText(paste("EPDs extracted successfully! Total bulls found:",
                                            nrow(bulls_data)))
        shinyjs::enable("extractButton")  # Re-enable button
    })
    
    filteredBulls <- reactiveVal(data.frame())
    
    observeEvent(input$filterButton, {
        req(nrow(bulls()) > 0)  # Ensure bulls data exists
        
        # Filter bulls based on the selected EPD criteria
        filtered_data <- bulls() %>%
            filter(EPD_Weight >= input$weightInput[1],
                   EPD_Weight <= input$weightInput[2],
                   EPD_Milk >= input$milkInput[1],
                   EPD_Milk <= input$milkInput[2],
                   EPD_Quality >= input$qualityInput[1],
                   EPD_Quality <= input$qualityInput[2])
        
        # Update the reactive value with filtered data
        filteredBulls(filtered_data)
        
        # Show a message if no bulls are found
        if (nrow(filtered_data) == 0) {
            output$progress <- renderText("No bulls match the selected criteria.")
        } else {
            output$progress <- renderText(paste("Bulls filtered successfully! Found:",
                                                nrow(filtered_data), "bulls"))
        }
    })
    
    output$bullTable <- renderTable({
        filteredBulls()
    })
    
    # Summary statistics
    output$summaryStats <- renderPrint({
        if (nrow(filteredBulls()) > 0) {
            cat("Summary Statistics for Filtered Bulls\n")
            cat("=====================================\n\n")
            
            cat("Weight EPD:\n")
            print(summary(filteredBulls()$EPD_Weight))
            cat("\nMilk EPD:\n")
            print(summary(filteredBulls()$EPD_Milk))
            cat("\nQuality EPD:\n")
            print(summary(filteredBulls()$EPD_Quality))
            
            cat("\n\nTotal bulls selected:", nrow(filteredBulls()), "\n")
        } else {
            cat("No bulls selected. Please apply filters first.\n")
        }
    })
    
    # Download report
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste0(gsub(" ", "_", input$reportTitle), "_", 
                   format(Sys.Date(), "%Y%m%d"), ".pdf")
        },
        content = function(file) {
            # Create temporary R Markdown file
            rmd_file <- tempfile(fileext = ".Rmd")
            
            # Write R Markdown content
            rmd_content <- paste0("
---
title: '", input$reportTitle, "'
date: '", format(Sys.Date(), '%B %d, %Y'), "'
output: pdf_document
---

# Bull Selection Report

## Report Details
- **Generated**: ", format(Sys.Date(), '%B %d, %Y'), "
- **Total Bulls Selected**: ", nrow(filteredBulls()), "

## Selected Bulls

```{r, echo=FALSE}
library(knitr)
kable(filteredBulls(), caption = 'Selected Bulls Based on EPD Criteria