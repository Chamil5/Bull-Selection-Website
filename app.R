library(shiny)
library(shinyjs)
library(dplyr)
library(tesseract)
library(magick)

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
        
        output$progress <- renderText("Converting PDF to Text, please wait...")
        shinyjs::disable("extractButton")  # Disable button during processing
        
        # Create temporary directory for images
        img_dir <- tempdir()  
        pdf_file <- input$pdfInput$datapath
        
        # Convert PDF to images using pdftocairo
        command <- sprintf("pdftocairo -png '%s' '%s/page'", pdf_file, img_dir)
        system(command, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        # Use Tesseract to read the images with preprocessing
        image_files <- list.files(img_dir, pattern = "\\.png$", full.names = TRUE)
        
        full_text <- ""
        for (image_file in image_files) {
            # Load the image
            img <- image_read(image_file)
            
            # Preprocess the image (optional: adjust quality, thresholding, etc.)
            img <- image_flatten(image_background(img, "white"))
            
            # Perform OCR
            text <- tesseract::ocr(img)
            full_text <- paste(full_text, text, sep = " ")
        }
        
        # Check if any readable text has been extracted
        if (nchar(full_text) == 0) {
            output$progress <- renderText("No readable text found after OCR extraction.")
            shinyjs::enable("extractButton")  # Re-enable button after processing
            return()
        }
        
        # Regex pattern to extract EPD fields
        pattern <- "ID:\\s*(\\d+)\\s*Name:\\s*([^\\n]+?)\\s*Weight:\\s*(\\d+)\\s*Milk:\\s*(\\d+)\\s*Quality:\\s*(\\d+)\\s*REA:\\s*(\\d+)\\s*MARB:\\s*(\\d+)\\s*FAT:\\s*(\\d+)\\s*YLD:\\s*(\\d+)\\s*CW:\\s*(\\d+)"
        
        matches <- gregexpr(pattern, full_text, perl = TRUE)
        found_bulls <- regmatches(full_text, matches)
        found_bulls <- unlist(found_bulls)
        
        # Create an empty data frame to collect bull data
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
        for (bull in found_bulls) {
            if (nchar(bull) > 0) {
                values <- unlist(regmatches(bull, gregexpr("\\d+", bull)))
                if (length(values) >= 9) {
                    name_match <- gsub("ID:\\s*\\d+\\s*Name:\\s*|\\s*Weight:.*", "", bull)
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
                    
                    if (!is.null(range_input)) {
                        filtered_bulls <- filtered_bulls %>%
                            filter(get(trait) >= range_input[1] & get(trait) <= range_input[2])
                    }
                }
            }
        }
        
        # Display a message if no bulls are found matching the criteria
        if (nrow(filtered_bulls) == 0) {
            return(data.frame(Message = "No bulls found matching these criteria."));
        }
        
        filtered_bulls
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
