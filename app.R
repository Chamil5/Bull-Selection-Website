library(shiny)
library(pdftools)
library(shinyjs)
library(dplyr)

# Set the maximum request size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

# Define UI for the application
ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("Bull EPD Selection"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"), 
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs"),
            htmlOutput("progress"),
            hr(),
            h4("Filter EPDs"),
            checkboxGroupInput("selectedTraits", "Select EPD Traits to Filter By", 
                               choices = c("Weight",
                                           "Milk",
                                           "Quality",
                                           "REA",
                                           "MARB",
                                           "FAT",
                                           "YLD",
                                           "CW")),
            sliderInput("weightRange", "Weight (lbs)", min = 0, max = 2000, value = c(0, 2000)),
            sliderInput("milkRange", "Milk", min = 0, max = 100, value = c(0, 100)),
            sliderInput("qualityRange", "Quality", min = 0, max = 100, value = c(0, 100)),
            sliderInput("reaRange", "REA", min = 0, max = 100, value = c(0, 100)),
            sliderInput("marbRange", "MARB", min = 0, max = 100, value = c(0, 100)),
            sliderInput("fatRange", "FAT", min = 0, max = 10, value = c(0, 10)),
            sliderInput("yldRange", "YLD", min = 0, max = 100, value = c(0, 100)),
            sliderInput("cwRange", "CW", min = 0, max = 1000, value = c(0, 1000))
        ),
        
        mainPanel(
            tableOutput("bullTable")
        )
    )
)

# Define server logic
server <- function(input, output) {
    bulls <- reactiveVal(data.frame())
    
    observeEvent(input$extractButton, {
        req(input$pdfInput)
        
        output$progress <- renderText("Extracting EPDs, please wait...")
        shinyjs::disable("extractButton")  # Disable button during processing
        
        # Try to read and extract EPDs from the uploaded PDF
        tryCatch({
            # Read PDF content
            pdf_text_content <- pdf_text(input$pdfInput$datapath)
            full_text <- paste(pdf_text_content, collapse = " ")
            
            # Debug: check the first few hundred characters of the text
            cat("Extracted PDF content (first 500 chars):\n")
            cat(substr(full_text, 1, 500), "\n")
            
            # Initialize the data frame to store extracted EPDs
            bulls_data <- data.frame(ID = integer(),
                                     Name = character(),
                                     Weight = numeric(),
                                     Milk = numeric(),
                                     Quality = numeric(),
                                     REA = numeric(),
                                     MARB = numeric(),
                                     FAT = numeric(),
                                     YLD = numeric(),
                                     CW = numeric(),
                                     stringsAsFactors = FALSE)
            
            # Use regex to match EPD data.
            # Adjust the pattern to match your specific data format.
            pattern <- "ID:\\s*(\\d+)\\s*Name:\\s*([^\\n]*)\\s*Weight:\\s*(\\d+)\\s*Milk:\\s*(\\d+)\\s*Quality:\\s*(\\d+)\\s*REA:\\s*(\\d+)\\s*MARB:\\s*(\\d+)\\s*FAT:\\s*(\\d+)\\s*YLD:\\s*(\\d+)\\s*CW:\\s*(\\d+)"
            
            matches <- gregexpr(pattern, full_text)
            found_bulls <- regmatches(full_text, matches)
            
            # Process all found bulls
            for (bull in found_bulls[[1]]) {
                if (nchar(bull) > 0) {
                    values <- unlist(regmatches(bull, gregexpr("\\d+", bull)))
                    
                    if (length(values) >= 10) {  # Ensure we have at least 10 values
                        bulls_data <- rbind(bulls_data, data.frame(
                            ID = as.integer(values[1]),
                            Name = trimws(sub("ID:\\s*\\d+\\s*Name:\\s*", "", bull)),
                            Weight = as.numeric(values[2]),
                            Milk = as.numeric(values[3]),
                            Quality = as.numeric(values[4]),
                            REA = as.numeric(values[5]),
                            MARB = as.numeric(values[6]),
                            FAT = as.numeric(values[7]),
                            YLD = as.numeric(values[8]),
                            CW = as.numeric(values[9]),
                            stringsAsFactors = FALSE
                        ))
                        # Debug: Print extracted values
                        cat(sprintf("Extracted: ID=%s, Name=%s, Weight=%s, Milk=%s, Quality=%s, REA=%s, MARB=%s, FAT=%s, YLD=%s, CW=%s\n",
                                    values[1], values[2], values[3], values[4], values[5], values[6], values[7], values[8], values[9]))
                    }
                }
            }
            
            # Update the reactive variable with the extracted data
            bulls(bulls_data)
            
            if (nrow(bulls_data) == 0) {
                output$progress <- renderText("No EPDs found in the provided PDF.")
            } else {
                output$progress <- renderText("EPDs extracted successfully!")
            }
        }, error = function(e) {
            output$progress <- renderText(paste("Error:", conditionMessage(e)))
        })
        
        shinyjs::enable("extractButton")  # Re-enable button after processing
    })
    
    output$bullTable <- renderTable({
        req(bulls())
        
        filtered_bulls <- bulls()
        
        # Apply filtering based on selected traits and their ranges
        if ("Weight" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(Weight),
                                     Weight >= input$weightRange[1],
                                     Weight <= input$weightRange[2])
        }
        
        if ("Milk" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(Milk),
                                     Milk >= input$milkRange[1],
                                     Milk <= input$milkRange[2])
        }
        
        if ("Quality" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(Quality),
                                     Quality >= input$qualityRange[1],
                                     Quality <= input$qualityRange[2])
        }
        
        if ("REA" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(REA),
                                     REA >= input$reaRange[1],
                                     REA <= input$reaRange[2])
        }
        
        if ("MARB" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(MARB),
                                     MARB >= input$marbRange[1],
                                     MARB <= input$marbRange[2])
        }
        
        if ("FAT" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(FAT),
                                     FAT >= input$fatRange[1],
                                     FAT <= input$fatRange[2])
        }
        
        if ("YLD" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(YLD),
                                     YLD >= input$yldRange[1],
                                     YLD <= input$yldRange[2])
        }
        
        if ("CW" %in% input$selectedTraits) {
            filtered_bulls <- filter(filtered_bulls,
                                     !is.na(CW),
                                     CW >= input$cwRange[1],
                                     CW <= input$cwRange[2])
        }
        
        filtered_bulls
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
