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
                               choices = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW")),
            sliderInput("weightRange", "Weight (lbs)", 0, 2000, c(0, 2000)),
            sliderInput("milkRange", "Milk", 0, 100, c(0, 100)),
            sliderInput("qualityRange", "Quality", 0, 100, c(0, 100)),
            sliderInput("reaRange", "REA", 0, 100, c(0, 100)),
            sliderInput("marbRange", "MARB", 0, 100, c(0, 100)),
            sliderInput("fatRange", "FAT", 0, 10, c(0, 10)),
            sliderInput("yldRange", "YLD", 0, 100, c(0, 100)),
            sliderInput("cwRange", "CW", 0, 1000, c(0, 1000))
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
        
        tryCatch({
            # Read entire PDF content from all pages
            pdf_text_content <- pdf_text(input$pdfInput$datapath)
            full_text <- paste(pdf_text_content, collapse = " ")  # Combine all pages' text
            
            # Debug: Print the extracted text to console for inspection
            # Uncomment the next line for debugging
            # cat(full_text)
            
            # Improved regex pattern to capture EPD fields more flexibly
            pattern <- "ID:\\s*(\\d+)\\s*Name:\\s*([^\\n]*)\\s*Weight:\\s*(\\d+)\\s*Milk:\\s*(\\d+)\\s*Quality:\\s*(\\d+)\\s*REA:\\s*(\\d+)\\s*MARB:\\s*(\\d+)\\s*FAT:\\s*(\\d+)\\s*YLD:\\s*(\\d+)\\s*CW:\\s*(\\d+)"
            
            matches <- gregexpr(pattern, full_text, perl = TRUE)
            found_bulls <- regmatches(full_text, matches)
            
            # Create an empty data frame for the bull data
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
            
            # Populate the data frame with extracted data
            for (bull in found_bulls[[1]]) {
                if (nchar(bull) > 0) {
                    values <- unlist(regmatches(bull, gregexpr("\\d+", bull)))
                    
                    # Ensure we have valid data
                    if (length(values) >= 9) {  # Ensure we capture ID and at least 8 traits
                        name_match <- sub("ID:\\s*\\d+\\s*Name:\\s*", "", bull)
                        bulls_data <- rbind(bulls_data, data.frame(
                            ID = as.integer(values[1]),
                            Name = trimws(name_match),
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
                    }
                }
            }
            
            # Store extracted data
            bulls(bulls_data)
            output$progress <- renderText(ifelse(nrow(bulls_data) == 0, "No EPDs found in the provided PDF.", "EPDs extracted successfully!"))
            
        }, error = function(e) {
            output$progress <- renderText(paste("Error:", conditionMessage(e)))
        })
        
        shinyjs::enable("extractButton")  # Re-enable button after processing
    })
    
    output$bullTable <- renderTable({
        req(bulls())
        
        # Get the current bulls data
        filtered_bulls <- bulls()
        
        # Apply filtering based on selected traits
        if (!is.null(input$selectedTraits) && length(input$selectedTraits) > 0) {
            for (trait in input$selectedTraits) {
                if (trait %in% names(filtered_bulls)) {
                    range_input <- switch(trait,
                                          "Weight" = input$weightRange,
                                          "Milk" = input$milkRange,
                                          "Quality" = input$qualityRange,
                                          "REA" = input$reaRange,
                                          "MARB" = input$marbRange,
                                          "FAT" = input$fatRange,
                                          "YLD" = input$yldRange,
                                          "CW" = input$cwRange)
                    
                    # Check if the range inputs are valid
                    if (!is.null(range_input)) {
                        filtered_bulls <- filtered_bulls %>%
                            filter(get(trait) >= range_input[1] & get(trait) <= range_input[2])
                    }
                }
            }
        }
        
        # Display a message if no bulls are found matching the criteria
        if (nrow(filtered_bulls) == 0) {
            return(data.frame(Message = "No bulls found matching these criteria."))
        }
        
        filtered_bulls
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
