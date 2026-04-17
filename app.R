library(shiny)
library(shinyjs)
library(dplyr)
library(pdftools)

# Set the maximum request size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

# Define breed averages (you can modify these based on your breed)
BREED_AVERAGES <- list(
    Weight = 0,
    Milk = 0,
    Quality = 0,
    REA = 0,
    MARB = 0,
    FAT = 0,
    YLD = 0,
    CW = 0
)

# Define UI for the application
ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("Bull EPD Selection (Compared to Breed Averages)"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"), 
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs"),
            htmlOutput("progress"),
            hr(),
            h4("Breed Averages"),
            fluidRow(
                column(6, numericInput("breedWeight", "Weight", value = 0)),
                column(6, numericInput("breedMilk", "Milk", value = 0))
            ),
            fluidRow(
                column(6, numericInput("breedQuality", "Quality", value = 0)),
                column(6, numericInput("breedREA", "REA", value = 0))
            ),
            fluidRow(
                column(6, numericInput("breedMARB", "MARB", value = 0)),
                column(6, numericInput("breedFAT", "FAT", value = 0))
            ),
            fluidRow(
                column(6, numericInput("breedYLD", "YLD", value = 0)),
                column(6, numericInput("breedCW", "CW", value = 0))
            ),
            hr(),
            h4("Filter by Difference from Breed Average"),
            checkboxGroupInput("selectedTraits", "Select EPD Traits to Filter By", 
                               choices = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW")),
            sliderInput("weightDiff", "Weight (difference from avg)", -500, 500, c(-500, 500)),
            sliderInput("milkDiff", "Milk (difference from avg)", -50, 50, c(-50, 50)),
            sliderInput("qualityDiff", "Quality (difference from avg)", -50, 50, c(-50, 50)),
            sliderInput("reaDiff", "REA (difference from avg)", -50, 50, c(-50, 50)),
            sliderInput("marbDiff", "MARB (difference from avg)", -50, 50, c(-50, 50)),
            sliderInput("fatDiff", "FAT (difference from avg)", -10, 10, c(-10, 10)),
            sliderInput("yldDiff", "YLD (difference from avg)", -50, 50, c(-50, 50)),
            sliderInput("cwDiff", "CW (difference from avg)", -500, 500, c(-500, 500))
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
            
            # Print first 2000 characters for debugging
            cat("\n=== PDF TEXT SAMPLE ===\n")
            cat(substr(full_text, 1, 2000))
            cat("\n=== END SAMPLE ===\n")
            
            # Clean up the text
            full_text <- gsub("\n", " ", full_text)
            full_text <- gsub("\r", " ", full_text)
            full_text <- gsub("\t", " ", full_text)
            
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
            
            # Try multiple extraction patterns
            bulls_data <- extract_bulls_flexible(full_text)
            
            # If no bulls found, try additional parsing methods
            if (nrow(bulls_data) == 0) {
                bulls_data <- extract_bulls_by_lines(full_text)
            }
            
            # Store extracted data
            bulls(bulls_data)
            
            message_text <- if (nrow(bulls_data) == 0) {
                "No EPDs found in the provided PDF. Please check the PDF format and ensure it contains bull data."
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
        
        # Collect breed averages
        breed_avg <- data.frame(
            Weight = input$breedWeight,
            Milk = input$breedMilk,
            Quality = input$breedQuality,
            REA = input$breedREA,
            MARB = input$breedMARB,
            FAT = input$breedFAT,
            YLD = input$breedYLD,
            CW = input$breedCW,
            stringsAsFactors = FALSE
        )
        
        # Apply filtering based on selected traits (comparing to breed average)
        if (!is.null(input$selectedTraits) && length(input$selectedTraits) > 0) {
            for (trait in input$selectedTraits) {
                if (trait %in% names(filtered_bulls)) {
                    range_input <- switch(trait,
                                          "Weight" = input$weightDiff,
                                          "Milk" = input$milkDiff,
                                          "Quality" = input$qualityDiff,
                                          "REA" = input$reaDiff,
                                          "MARB" = input$marbDiff,
                                          "FAT" = input$fatDiff,
                                          "YLD" = input$yldDiff,
                                          "CW" = input$cwDiff)
                    
                    if (!is.null(range_input)) {
                        breed_avg_value <- breed_avg[[trait]][1]
                        
                        filtered_bulls <- filtered_bulls %>%
                            filter((get(trait) - breed_avg_value) >= range_input[1] & 
                                       (get(trait) - breed_avg_value) <= range_input[2])
                    }
                }
            }
        }
        
        # Add columns showing difference from breed average
        if (nrow(filtered_bulls) > 0) {
            filtered_bulls$Weight_Diff <- filtered_bulls$Weight - input$breedWeight
            filtered_bulls$Milk_Diff <- filtered_bulls$Milk - input$breedMilk
            filtered_bulls$Quality_Diff <- filtered_bulls$Quality - input$breedQuality
            filtered_bulls$REA_Diff <- filtered_bulls$REA - input$breedREA
            filtered_bulls$MARB_Diff <- filtered_bulls$MARB - input$breedMARB
            filtered_bulls$FAT_Diff <- filtered_bulls$FAT - input$breedFAT
            filtered_bulls$YLD_Diff <- filtered_bulls$YLD - input$breedYLD
            filtered_bulls$CW_Diff <- filtered_bulls$CW - input$breedCW
        }
        
        # Display a message if no bulls are found matching the criteria
        if (nrow(filtered_bulls) == 0) {
            return(data.frame(Message = "No bulls found matching these criteria."))
        }
        
        filtered_bulls
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Flexible extraction function - tries multiple patterns
extract_bulls_flexible <- function(text) {
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
    
    # Pattern 1: Try to find EPD values with labels
    patterns <- list(
        # Pattern for "EPD: value" format
        "EPD format" = list(
            id = "(?:ID|Lot|#)\\s*[:#]?\\s*(\\S+)",
            name = "(?:Name|Bull)\\s*[:#]?\\s*([A-Za-z0-9\\s-]+?)(?=Weight|WEIGHT|EPD)",
            weight = "Weight\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            milk = "Milk\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            quality = "Quality\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            rea = "REA\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            marb = "MARB\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            fat = "FAT\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            yld = "YLD\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            cw = "CW\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)"
        )
    )
    
    # Split by common delimiters that might separate bulls
    potential_sections <- strsplit(text, "(?:Lot|ID|Bull)\\s*[:#]?\\s*(?=[A-Za-z0-9])", perl = TRUE)[[1]]
    
    for (section in potential_sections) {
        if (nchar(section) > 20) {
            tryCatch({
                # Extract each field
                id <- extract_value(section, "(?:ID|Lot|#)\\s*[:#]?\\s*(\\S+)")
                name <- extract_value(section, "(?:Name|Bull)\\s*[:#]?\\s*([A-Za-z0-9\\s-]+?)(?=Weight|WEIGHT|EPD|Sire)")
                weight <- extract_numeric_value(section, "Weight")
                milk <- extract_numeric_value(section, "Milk")
                quality <- extract_numeric_value(section, "Quality")
                rea <- extract_numeric_value(section, "REA")
                marb <- extract_numeric_value(section, "MARB")
                fat <- extract_numeric_value(section, "FAT")
                yld <- extract_numeric_value(section, "YLD")
                cw <- extract_numeric_value(section, "CW")
                
                # Only add if we have valid data
                if (!is.na(id) && !is.na(name) && length(c(weight, milk, quality, rea, marb, fat, yld, cw)) > 0) {
                    if (sum(!is.na(c(weight, milk, quality, rea, marb, fat, yld, cw))) >= 3) {
                        bulls_df <- rbind(bulls_df, data.frame(
                            ID = as.character(id),
                            Name = as.character(name),
                            Weight = as.numeric(weight),
                            Milk = as.numeric(milk),
                            Quality = as.numeric(quality),
                            REA = as.numeric(rea),
                            MARB = as.numeric(marb),
                            FAT = as.numeric(fat),
                            YLD = as.numeric(yld),
                            CW = as.numeric(cw),
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }, error = function(e) {
                # Skip problematic entries silently
            })
        }
    }
    
    return(bulls_df)
}

# Alternative extraction method - line by line
extract_bulls_by_lines <- function(text) {
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
    
    # Split into lines and look for numeric patterns
    lines <- strsplit(text, "\\s{2,}")[[1]]
    
    i <- 1
    while (i <= length(lines)) {
        line <- lines[i]
        
        tryCatch({
            # Look for lines with lots of numbers (likely EPD lines)
            if (grepl("\\d+", line) && nchar(line) > 30) {
                # Try to extract numbers
                numbers <- as.numeric(unlist(strsplit(gsub("[^0-9.-]", " ", line), "\\s+")))
                numbers <- numbers[!is.na(numbers)]
                
                if (length(numbers) >= 8) {
                    # Assume first number is ID, rest are EPDs
                    bulls_df <- rbind(bulls_df, data.frame(
                        ID = as.character(numbers[1]),
                        Name = paste("Bull", numbers[1]),
                        Weight = numbers[2],
                        Milk = numbers[3],
                        Quality = numbers[4],
                        REA = numbers[5],
                        MARB = numbers[6],
                        FAT = numbers[7],
                        YLD = numbers[8],
                        CW = if(length(numbers) > 8) numbers[9] else NA,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }, error = function(e) {
            # Skip
        })
        
        i <- i + 1
    }
    
    return(bulls_df)
}

# Helper function to extract string values
extract_value <- function(text, pattern) {
    match <- regmatches(text, regexpr(pattern, text, perl = TRUE, ignore.case = TRUE))
    if (length(match) > 0 && nchar(match[1]) > 0) {
        trimws(gsub(pattern, "\\1", match[1], perl = TRUE, ignore.case = TRUE))
    } else {
        NA
    }
}

# Helper function to extract numeric values
extract_numeric_value <- function(text, field_name) {
    pattern <- paste0(field_name, "\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)")
    match <- regmatches(text, regexpr(pattern, text, perl = TRUE, ignore.case = TRUE))
    
    if (length(match) > 0 && nchar(match[1]) > 0) {
        value_str <- gsub(paste0(field_name, "\\s*[:#]?\\s*"), "", match[1], ignore.case = TRUE)
        as.numeric(trimws(value_str))
    } else {
        NA
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
