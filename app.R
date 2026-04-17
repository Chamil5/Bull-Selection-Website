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
            h4("Desired EPD Ranges"),
            fluidRow(
                column(6, numericInput("minWeight", "Min Weight", value = -50)),
                column(6, numericInput("maxWeight", "Max Weight", value = 150))
            ),
            fluidRow(
                column(6, numericInput("minMilk", "Min Milk", value = -10)),
                column(6, numericInput("maxMilk", "Max Milk", value = 100))
            ),
            fluidRow(
                column(6, numericInput("minQuality", "Min Quality", value = -5)),
                column(6, numericInput("maxQuality", "Max Quality", value = 5))
            ),
            fluidRow(
                column(6, numericInput("minREA", "Min REA", value = -0.5)),
                column(6, numericInput("maxREA", "Max REA", value = 2))
            ),
            fluidRow(
                column(6, numericInput("minMARB", "Min MARB", value = -1)),
                column(6, numericInput("maxMARB", "Max MARB", value = 2))
            ),
            fluidRow(
                column(6, numericInput("minFAT", "Min FAT", value = -0.5)),
                column(6, numericInput("maxFAT", "Max FAT", value = 1))
            ),
            fluidRow(
                column(6, numericInput("minYLD", "Min YLD", value = -3)),
                column(6, numericInput("maxYLD", "Max YLD", value = 3))
            ),
            fluidRow(
                column(6, numericInput("minCW", "Min CW", value = -50)),
                column(6, numericInput("maxCW", "Max CW", value = 150))
            ),
            hr(),
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"), 
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs", class = "btn-primary btn-lg"),
            htmlOutput("progress"),
            width = 3
        ),
        
        mainPanel(
            h3("Filtered Bulls"),
            tableOutput("bullTable"),
            hr(),
            h4("Summary Statistics"),
            tableOutput("summaryTable"),
            width = 9
        )
    )
)

# Define server logic
server <- function(input, output) {
    filtered_bulls <- reactiveVal(data.frame())
    
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
            
            # Extract bull data
            bulls_data <- extract_bulls_flexible(full_text)
            
            # If no bulls found, try additional parsing methods
            if (nrow(bulls_data) == 0) {
                bulls_data <- extract_bulls_by_lines(full_text)
            }
            
            # Now filter the extracted bulls based on user inputs
            if (nrow(bulls_data) > 0) {
                bulls_filtered <- apply_user_filters(bulls_data, input)
                filtered_bulls(bulls_filtered)
                
                message_text <- if (nrow(bulls_filtered) == 0) {
                    paste("Extracted", nrow(bulls_data), "bulls total, but 0 match your specified ranges.")
                } else {
                    paste("Successfully extracted and filtered", nrow(bulls_filtered), "bulls matching your criteria!")
                }
                
                output$progress <- renderText(message_text)
            } else {
                output$progress <- renderText("No EPDs found in the provided PDF. Please check the PDF format and ensure it contains bull data.")
            }
            
        }, error = function(e) {
            output$progress <- renderText(paste("Error:", conditionMessage(e)))
        }, finally = {
            shinyjs::enable("extractButton")  # Re-enable button after processing
        })
    })
    
    # Function to apply filters based on user inputs
    apply_user_filters <- function(bulls_data, input) {
        result <- bulls_data
        
        # Apply Weight filter
        result <- result %>%
            filter((is.na(Weight) | (Weight >= input$minWeight & Weight <= input$maxWeight)))
        
        # Apply Milk filter
        result <- result %>%
            filter((is.na(Milk) | (Milk >= input$minMilk & Milk <= input$maxMilk)))
        
        # Apply Quality filter
        result <- result %>%
            filter((is.na(Quality) | (Quality >= input$minQuality & Quality <= input$maxQuality)))
        
        # Apply REA filter
        result <- result %>%
            filter((is.na(REA) | (REA >= input$minREA & REA <= input$maxREA)))
        
        # Apply MARB filter
        result <- result %>%
            filter((is.na(MARB) | (MARB >= input$minMARB & MARB <= input$maxMARB)))
        
        # Apply FAT filter
        result <- result %>%
            filter((is.na(FAT) | (FAT >= input$minFAT & FAT <= input$maxFAT)))
        
        # Apply YLD filter
        result <- result %>%
            filter((is.na(YLD) | (YLD >= input$minYLD & YLD <= input$maxYLD)))
        
        # Apply CW filter
        result <- result %>%
            filter((is.na(CW) | (CW >= input$minCW & CW <= input$maxCW)))
        
        return(result)
    }
    
    output$bullTable <- renderTable({
        req(filtered_bulls())
        
        displayed_bulls <- filtered_bulls()
        
        if (nrow(displayed_bulls) == 0) {
            return(data.frame(Message = "No bulls found matching these criteria."))
        }
        
        # Format the output
        displayed_bulls %>%
            select(ID, Name, Weight, Milk, Quality, REA, MARB, FAT, YLD, CW) %>%
            arrange(ID)
        
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$summaryTable <- renderTable({
        req(filtered_bulls())
        
        bulls_data <- filtered_bulls()
        
        if (nrow(bulls_data) == 0) {
            return(data.frame(Trait = character(), Mean = numeric(), Min = numeric(), Max = numeric()))
        }
        
        summary_stats <- data.frame(
            Trait = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW"),
            Mean = c(
                mean(bulls_data$Weight, na.rm = TRUE),
                mean(bulls_data$Milk, na.rm = TRUE),
                mean(bulls_data$Quality, na.rm = TRUE),
                mean(bulls_data$REA, na.rm = TRUE),
                mean(bulls_data$MARB, na.rm = TRUE),
                mean(bulls_data$FAT, na.rm = TRUE),
                mean(bulls_data$YLD, na.rm = TRUE),
                mean(bulls_data$CW, na.rm = TRUE)
            ),
            Min = c(
                min(bulls_data$Weight, na.rm = TRUE),
                min(bulls_data$Milk, na.rm = TRUE),
                min(bulls_data$Quality, na.rm = TRUE),
                min(bulls_data$REA, na.rm = TRUE),
                min(bulls_data$MARB, na.rm = TRUE),
                min(bulls_data$FAT, na.rm = TRUE),
                min(bulls_data$YLD, na.rm = TRUE),
                min(bulls_data$CW, na.rm = TRUE)
            ),
            Max = c(
                max(bulls_data$Weight, na.rm = TRUE),
                max(bulls_data$Milk, na.rm = TRUE),
                max(bulls_data$Quality, na.rm = TRUE),
                max(bulls_data$REA, na.rm = TRUE),
                max(bulls_data$MARB, na.rm = TRUE),
                max(bulls_data$FAT, na.rm = TRUE),
                max(bulls_data$YLD, na.rm = TRUE),
                max(bulls_data$CW, na.rm = TRUE)
            ),
            stringsAsFactors = FALSE
        )
        
        # Format to 2 decimal places
        summary_stats[] <- lapply(summary_stats, function(x) {
            if (is.numeric(x)) round(x, 2) else x
        })
        
        summary_stats
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
