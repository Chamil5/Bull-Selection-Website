library(shiny)
library(shinyjs)
library(dplyr)
library(pdftools)

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
        
        output$progress <- renderText("Extracting EPDs from PDF, please wait...")
        shinyjs::disable("extractButton")  # Disable button during processing
        
        tryCatch({
            # Extract text from PDF using pdftools
            pdf_text <- pdf_text(input$pdfInput$datapath)
            full_text <- paste(pdf_text, collapse = " ")
            
            # Clean up the text
            full_text <- gsub("\n", " ", full_text)
            full_text <- gsub("\r", " ", full_text)
            
            # Create an empty data frame to collect bull data
            bulls_data <- data.frame(
                ID = character(),
                Name = character(),
                Weight = numeric(),
                Milk = numeric(),
                Quality = numeric(),
                REA = numeric(),
                MARB = numeric(),
                FAT = numeric(),
                YLD = numeric(),
                CW = numeric(),
                stringsAsFactors = FALSE
            )
            
            # Pattern to match bull entries - adjust based on your PDF format
            # This pattern looks for ID followed by various EPD values
            pattern <- "(?:ID|Lot)\\s*[#:]?\\s*(\\S+).*?(?:Name|Sire)\\s*[#:]?\\s*([^,]+?)\\s+Weight\\s*[#:]?\\s*([-+]?\\d+).*?Milk\\s*[#:]?\\s*([-+]?\\d+).*?Quality\\s*[#:]?\\s*([-+]?\\d+).*?REA\\s*[#:]?\\s*([-+]?\\d+).*?MARB\\s*[#:]?\\s*([-+]?\\d+).*?FAT\\s*[#:]?\\s*([-+]?\\d+).*?YLD\\s*[#:]?\\s*([-+]?\\d+).*?CW\\s*[#:]?\\s*([-+]?\\d+)"
            
            matches <- gregexpr(pattern, full_text, perl = TRUE, ignore.case = TRUE)
            
            if (length(matches[[1]]) > 0 && matches[[1]][1] > 0) {
                # Extract matched strings
                matched_strings <- regmatches(full_text, matches)[[1]]
                
                # Use regex to extract individual fields from each match
                for (match_str in matched_strings) {
                    tryCatch({
                        # Extract ID
                        id_match <- regmatches(match_str, 
                                               gregexpr("(?:ID|Lot)\\s*[#:]?\\s*(\\S+)", 
                                                        match_str, perl = TRUE, ignore.case = TRUE))
                        id <- if (length(id_match[[1]]) > 0) trimws(gsub("(?:ID|Lot)\\s*[#:]?\\s*", "", id_match[[1]][1], ignore.case = TRUE)) else NA
                        
                        # Extract Name
                        name_match <- regmatches(match_str, 
                                                 gregexpr("(?:Name|Sire)\\s*[#:]?\\s*([^,]+?)\\s+(?=Weight)", 
                                                          match_str, perl = TRUE, ignore.case = TRUE))
                        name <- if (length(name_match[[1]]) > 0) trimws(gsub("(?:Name|Sire)\\s*[#:]?\\s*", "", name_match[[1]][1], ignore.case = TRUE)) else NA
                        
                        # Extract numeric values
                        weight <- extract_numeric(match_str, "Weight")
                        milk <- extract_numeric(match_str, "Milk")
                        quality <- extract_numeric(match_str, "Quality")
                        rea <- extract_numeric(match_str, "REA")
                        marb <- extract_numeric(match_str, "MARB")
                        fat <- extract_numeric(match_str, "FAT")
                        yld <- extract_numeric(match_str, "YLD")
                        cw <- extract_numeric(match_str, "CW")
                        
                        # Only add if we have valid data
                        if (!is.na(id) && !is.na(name) && !is.na(weight)) {
                            bulls_data <- rbind(bulls_data, data.frame(
                                ID = id,
                                Name = name,
                                Weight = weight,
                                Milk = milk,
                                Quality = quality,
                                REA = rea,
                                MARB = marb,
                                FAT = fat,
                                YLD = yld,
                                CW = cw,
                                stringsAsFactors = FALSE
                            ))
                        }
                    }, error = function(e) {
                        # Skip problematic entries
                    })
                }
            } else {
                # Alternative pattern if the main pattern fails
                bulls_data <- extract_bulls_alternative(full_text)
            }
            
            # Store extracted data
            bulls(bulls_data)
            
            message_text <- if (nrow(bulls_data) == 0) {
                "No EPDs found in the provided PDF. Please check the PDF format."
            } else {
                paste("Successfully extracted", nrow(bulls_data), "bulls!")
            }
            
            output$progress <- renderText(message_text)
            
        }, error = function(e) {
            output$progress <- renderText(paste("Error:", conditionMessage(e)))
        }, finally = {
            shinyjs::enable("extractButton")  # Re-enable button after processing
        })
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
            return(data.frame(Message = "No bulls found matching these criteria."))
        }
        
        filtered_bulls
    })
}

# Helper function to extract numeric values
extract_numeric <- function(text, field_name) {
    pattern <- paste0(field_name, "\\s*[#:]?\\s*([-+]?\\d+)")
    match <- regmatches(text, gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE))
    
    if (length(match[[1]]) > 0) {
        value_str <- gsub(paste0(field_name, "\\s*[#:]?\\s*"), "", match[[1]][1], ignore.case = TRUE)
        as.numeric(trimws(value_str))
    } else {
        NA
    }
}

# Alternative extraction method for different PDF formats
extract_bulls_alternative <- function(text) {
    bulls_df <- data.frame(
        ID = character(),
        Name = character(),
        Weight = numeric(),
        Milk = numeric(),
        Quality = numeric(),
        REA = numeric(),
        MARB = numeric(),
        FAT = numeric(),
        YLD = numeric(),
        CW = numeric(),
        stringsAsFactors = FALSE
    )
    
    # Try a more flexible pattern
    lines <- strsplit(text, "\\s{2,}")[[1]]
    
    for (i in seq_along(lines)) {
        line <- lines[i]
        if (grepl("\\d+", line) && nchar(line) > 10) {
            # Try to extract data from this line
            # This is a fallback and may need customization
        }
    }
    
    return(bulls_df)
}

# Run the application 
shinyApp(ui = ui, server = server)
