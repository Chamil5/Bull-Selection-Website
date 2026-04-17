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
    
    titlePanel("Bull EPD Selection"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"), 
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs"),
            htmlOutput("progress"),
            hr(),
            h4("Minimum EPD Values (Desired)"),
            fluidRow(
                column(6, numericInput("minWeight", "Weight", value = 0)),
                column(6, numericInput("minMilk", "Milk", value = 0))
            ),
            fluidRow(
                column(6, numericInput("minQuality", "Quality", value = 0)),
                column(6, numericInput("minREA", "REA", value = 0))
            ),
            fluidRow(
                column(6, numericInput("minMARB", "MARB", value = 0)),
                column(6, numericInput("minFAT", "FAT", value = 0))
            ),
            fluidRow(
                column(6, numericInput("minYLD", "YLD", value = 0)),
                column(6, numericInput("minCW", "CW", value = 0))
            ),
            hr(),
            h4("Maximum EPD Values (Optional)"),
            fluidRow(
                column(6, numericInput("maxWeight", "Weight", value = NA)),
                column(6, numericInput("maxMilk", "Milk", value = NA))
            ),
            fluidRow(
                column(6, numericInput("maxQuality", "Quality", value = NA)),
                column(6, numericInput("maxREA", "REA", value = NA))
            ),
            fluidRow(
                column(6, numericInput("maxMARB", "MARB", value = NA)),
                column(6, numericInput("maxFAT", "FAT", value = NA))
            ),
            fluidRow(
                column(6, numericInput("maxYLD", "YLD", value = NA)),
                column(6, numericInput("maxCW", "CW", value = NA))
            ),
            hr(),
            actionButton("filterButton", "Apply Filters", class = "btn-primary btn-lg"),
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
    bulls <- reactiveVal(data.frame())
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
            
            # Store extracted data
            bulls(bulls_data)
            filtered_bulls(bulls_data)  # Initialize filtered data with all bulls
            
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
    
    # Apply filters when button is clicked
    observeEvent(input$filterButton, {
        req(bulls())
        
        bulls_to_filter <- bulls()
        
        # Apply minimum filters
        if (!is.na(input$minWeight)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Weight >= input$minWeight | is.na(Weight))
        }
        
        if (!is.na(input$minMilk)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Milk >= input$minMilk | is.na(Milk))
        }
        
        if (!is.na(input$minQuality)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Quality >= input$minQuality | is.na(Quality))
        }
        
        if (!is.na(input$minREA)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(REA >= input$minREA | is.na(REA))
        }
        
        if (!is.na(input$minMARB)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(MARB >= input$minMARB | is.na(MARB))
        }
        
        if (!is.na(input$minFAT)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(FAT >= input$minFAT | is.na(FAT))
        }
        
        if (!is.na(input$minYLD)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(YLD >= input$minYLD | is.na(YLD))
        }
        
        if (!is.na(input$minCW)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(CW >= input$minCW | is.na(CW))
        }
        
        # Apply maximum filters
        if (!is.na(input$maxWeight)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Weight <= input$maxWeight | is.na(Weight))
        }
        
        if (!is.na(input$maxMilk)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Milk <= input$maxMilk | is.na(Milk))
        }
        
        if (!is.na(input$maxQuality)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(Quality <= input$maxQuality | is.na(Quality))
        }
        
        if (!is.na(input$maxREA)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(REA <= input$maxREA | is.na(REA))
        }
        
        if (!is.na(input$maxMARB)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(MARB <= input$maxMARB | is.na(MARB))
        }
        
        if (!is.na(input$maxFAT)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(FAT <= input$maxFAT | is.na(FAT))
        }
        
        if (!is.na(input$maxYLD)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(YLD <= input$maxYLD | is.na(YLD))
        }
        
        if (!is.na(input$maxCW)) {
            bulls_to_filter <- bulls_to_filter %>%
                filter(CW <= input$maxCW | is.na(CW))
        }
        
        filtered_bulls(bulls_to_filter)
    })
    
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
