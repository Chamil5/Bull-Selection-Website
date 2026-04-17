# Load necessary packages
library(shiny)
library(dplyr)
library(pdftools)
library(shinyjs)
library(rmarkdown)
library(stringr)

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
            textInput("pdfPath", "Or paste PDF file path:", 
                      placeholder = "/Users/your-name/Downloads/filename.pdf"),
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

# Function to extract EPDs from PDF text
extract_epds_from_text <- function(text) {
    # Split text into lines
    lines <- unlist(strsplit(text, "\n"))
    
    bulls_data <- data.frame(matrix(NA, ncol=5, nrow=0))
    colnames(bulls_data) <- c("ID", "Name", "EPD_Weight", "EPD_Milk", "EPD_Quality")
    
    i <- 1
    while (i <= length(lines)) {
        line <- lines[i]
        
        # Try multiple patterns to find bull information
        
        # Pattern 1: Standard format with ID, Name, Weight, Milk, Quality
        epd_pattern <- "ID:\\s*(\\S+).*?Name:\\s*([^,]+).*?Weight:\\s*([\\d.-]+).*?Milk:\\s*([\\d.-]+).*?Quality:\\s*([\\d.-]+)"
        
        if (grepl(epd_pattern, line, ignore.case = TRUE)) {
            match <- regmatches(line, regexec(epd_pattern, line, ignore.case = TRUE))
            if (length(match[[1]]) > 0) {
                bulls_data <- rbind(bulls_data, 
                                    data.frame(ID = as.character(trimws(match[[1]][2])),
                                               Name = as.character(trimws(match[[1]][3])),
                                               EPD_Weight = as.numeric(trimws(match[[1]][4])),
                                               EPD_Milk = as.numeric(trimws(match[[1]][5])),
                                               EPD_Quality = as.numeric(trimws(match[[1]][6])),
                                               stringsAsFactors = FALSE))
                i <- i + 1
                next
            }
        }
        
        # Pattern 2: Bull number at start of line, followed by name and EPDs
        bull_pattern <- "^\\s*(\\d+)\\s+([A-Z][^0-9]*?)\\s+([-\\d.]+)\\s+([-\\d.]+)\\s+([-\\d.]+)"
        
        if (grepl(bull_pattern, line)) {
            match <- regmatches(line, regexec(bull_pattern, line))
            if (length(match[[1]]) > 0) {
                bulls_data <- rbind(bulls_data, 
                                    data.frame(ID = as.character(trimws(match[[1]][2])),
                                               Name = as.character(trimws(match[[1]][3])),
                                               EPD_Weight = as.numeric(trimws(match[[1]][4])),
                                               EPD_Milk = as.numeric(trimws(match[[1]][5])),
                                               EPD_Quality = as.numeric(trimws(match[[1]][6])),
                                               stringsAsFactors = FALSE))
                i <- i + 1
                next
            }
        }
        
        # Pattern 3: Look for lines with bull name and check next lines for EPD values
        if (grepl("^[A-Z].*[A-Z]|^\\d+", line) && !grepl("^\\s*$", line)) {
            # Try to find EPD pattern in next few lines
            for (j in 0:2) {
                if ((i + j) <= length(lines)) {
                    check_line <- lines[i + j]
                    epd_check <- "Weight.*?([-\\d.]+).*?Milk.*?([-\\d.]+).*?Quality.*?([-\\d.]+)"
                    if (grepl(epd_check, check_line, ignore.case = TRUE)) {
                        match <- regmatches(check_line, regexec(epd_check, check_line, ignore.case = TRUE))
                        if (length(match[[1]]) > 0) {
                            bulls_data <- rbind(bulls_data, 
                                                data.frame(ID = as.character(nrow(bulls_data) + 1),
                                                           Name = as.character(trimws(line)),
                                                           EPD_Weight = as.numeric(trimws(match[[1]][2])),
                                                           EPD_Milk = as.numeric(trimws(match[[1]][3])),
                                                           EPD_Quality = as.numeric(trimws(match[[1]][4])),
                                                           stringsAsFactors = FALSE))
                            i <- i + j + 1
                            next
                        }
                    }
                }
            }
        }
        
        i <- i + 1
    }
    
    return(bulls_data)
}

# Define server logic
server <- function(input, output) {
    # Placeholder for bull data
    bulls <- reactiveVal(data.frame())
    
    observeEvent(input$extractButton, {
        # Check if file is uploaded or path is provided
        if (is.null(input$pdfInput) && input$pdfPath == "") {
            output$progress <- renderText("Please upload a PDF or provide a file path.")
            return()
        }
        
        # Display progress message
        output$progress <- renderText("Extracting EPDs, please wait...")
        shinyjs::disable("extractButton")  # Disable button to prevent multiple clicks
        
        tryCatch({
            # Determine PDF path
            pdf_path <- if (!is.null(input$pdfInput)) {
                input$pdfInput$datapath
            } else {
                # Clean up the path (remove file:// prefix if present)
                path <- input$pdfPath
                if (grepl("^file://", path)) {
                    path <- gsub("^file://", "", path)
                }
                path <- URLdecode(path)
                path
            }
            
            # Check if file exists
            if (!file.exists(pdf_path)) {
                stop("File not found. Please check the file path.")
            }
            
            # Read PDF and extract text
            text <- pdf_text(pdf_path)
            full_text <- paste(text, collapse = "\n")
            
            # Extract EPDs using the custom function
            bulls_data <- extract_epds_from_text(full_text)
            
            if (nrow(bulls_data) == 0) {
                output$progress <- renderText("No EPD data found in the PDF. Please check the file format.")
                shinyjs::enable("extractButton")
                return()
            }
            
            # Update the bulls data frame
            bulls(bulls_data)
            
            # Dynamically update slider inputs based on the new data
            output$slides <- renderUI({
                if (nrow(bulls()) > 0) {
                    tagList(
                        sliderInput("weightInput", "Weight EPD:", 
                                    min = floor(min(bulls()$EPD_Weight, na.rm = TRUE)), 
                                    max = ceiling(max(bulls()$EPD_Weight, na.rm = TRUE)), 
                                    value = c(floor(min(bulls()$EPD_Weight, na.rm = TRUE)), 
                                              ceiling(max(bulls()$EPD_Weight, na.rm = TRUE)))),
                        sliderInput("milkInput", "Milk EPD:", 
                                    min = floor(min(bulls()$EPD_Milk, na.rm = TRUE)), 
                                    max = ceiling(max(bulls()$EPD_Milk, na.rm = TRUE)), 
                                    value = c(floor(min(bulls()$EPD_Milk, na.rm = TRUE)), 
                                              ceiling(max(bulls()$EPD_Milk, na.rm = TRUE)))),
                        sliderInput("qualityInput", "Quality EPD:", 
                                    min = floor(min(bulls()$EPD_Quality, na.rm = TRUE)), 
                                    max = ceiling(max(bulls()$EPD_Quality, na.rm = TRUE)), 
                                    value = c(floor(min(bulls()$EPD_Quality, na.rm = TRUE)), 
                                              ceiling(max(bulls()$EPD_Quality, na.rm = TRUE))))
                    )
                }
            })
            
            # Success message
            output$progress <- renderText(paste("✓ EPDs extracted successfully! Total bulls found:",
                                                nrow(bulls_data)))
            
        }, error = function(e) {
            output$progress <- renderText(paste("Error:", e$message))
        })
        
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
            output$progress <- renderText(paste("✓ Bulls filtered successfully! Found:",
                                                nrow(filtered_data), "bulls"))
        }
    })
    
    output$bullTable <- renderTable({
        if (nrow(filteredBulls()) > 0) {
            filteredBulls()
        } else {
            data.frame(Message = "No bulls to display. Apply filters first.")
        }
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
kable(filteredBulls(), caption = 'Selected Bulls Based on EPD Criteria')
