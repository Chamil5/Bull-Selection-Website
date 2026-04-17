library(shiny)
library(pdftools)
library(shinyjs)
library(dplyr)

# UI Definition
ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
        tags$style(HTML("
      body {
        background: linear-gradient(135deg, #8B4513 0%, #D2691E 100%);
        font-family: 'Georgia', serif;
        color: #333;
      }
      
      .navbar {
        background-color: #654321 !important;
        border-bottom: 3px solid #8B4513;
      }
      
      .navbar-brand {
        font-size: 24px !important;
        font-weight: bold;
        color: #FFD700 !important;
      }
      
      .panel {
        background-color: rgba(255, 248, 240, 0.95);
        border: 2px solid #8B4513;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
      }
      
      .panel-heading {
        background-color: #8B4513 !important;
        color: #FFD700 !important;
        font-weight: bold;
        border: none !important;
      }
      
      .btn-primary {
        background-color: #CD853F !important;
        border-color: #8B4513 !important;
      }
      
      .btn-primary:hover {
        background-color: #8B4513 !important;
        border-color: #654321 !important;
      }
      
      .btn-success {
        background-color: #228B22 !important;
        border-color: #1a6b1a !important;
      }
      
      .btn-success:hover {
        background-color: #1a6b1a !important;
        border-color: #0d3d0d !important;
      }
      
      .btn-danger {
        background-color: #DC143C !important;
        border-color: #8B0000 !important;
      }
      
      .btn-danger:hover {
        background-color: #8B0000 !important;
        border-color: #660000 !important;
      }
      
      h1, h2, h3, h4 {
        color: #654321;
        font-weight: bold;
      }
      
      .well {
        background-color: rgba(255, 250, 240, 0.9);
        border: 2px solid #CD853F;
      }
      
      .progress {
        background-color: #ddd;
      }
      
      .progress-bar {
        background-color: #CD853F !important;
      }
      
      table {
        background-color: white;
        border-collapse: collapse;
      }
      
      table th {
        background-color: #8B4513;
        color: #FFD700;
        font-weight: bold;
        border: 1px solid #654321;
      }
      
      table td {
        border: 1px solid #D2691E;
        padding: 8px;
      }
      
      table tr:nth-child(even) {
        background-color: #FFF8DC;
      }
      
      table tr:hover {
        background-color: #FFEFD5;
      }
      
      .info-box {
        background-color: #FFFACD;
        border-left: 4px solid #8B4513;
        padding: 10px;
        margin: 10px 0;
        border-radius: 4px;
      }
      
      .error-message {
        color: #8B0000;
        font-weight: bold;
      }
      
      .success-message {
        color: #228B22;
        font-weight: bold;
      }
    "))
    ),
    
    # Navigation Bar
    navbarPage(
        "🤠 Angus Bull EPD Analyzer 🤠",
        id = "navbar",
        
        # Tab 1: Upload and Extract
        tabPanel(
            "📄 Upload & Extract",
            fluidRow(
                column(
                    12,
                    div(
                        class = "well",
                        h2("Step 1: Upload Your Angus Sale PDF"),
                        p("Choose a PDF file or provide a direct file path to analyze bull EPDs."),
                        fileInput(
                            "pdfInput",
                            "Choose PDF file:",
                            accept = c("application/pdf"),
                            multiple = FALSE
                        ),
                        p("OR", style = "text-align: center; font-weight: bold;"),
                        textInput(
                            "pdfPath",
                            "Provide file path directly:",
                            placeholder = "e.g., /Users/cadehamil/Downloads/your-file.pdf"
                        ),
                        p(
                            "Note: You can use either the file upload or provide a direct path. The file size limit is 200MB.",
                            class = "info-box"
                        ),
                        actionButton(
                            "extractButton",
                            "📊 Extract EPDs from PDF",
                            class = "btn-success",
                            style = "width: 100%; padding: 10px; font-size: 16px; font-weight: bold;"
                        ),
                        br(),
                        br(),
                        textOutput("progress"),
                        br()
                    )
                )
            )
        ),
        
        # Tab 2: Filter and Analyze
        tabPanel(
            "🔍 Filter & Analyze",
            fluidRow(
                column(
                    3,
                    div(
                        class = "panel panel-primary",
                        div(
                            class = "panel-heading",
                            "Filter Specifications"
                        ),
                        div(
                            class = "panel-body",
                            h4("Weight EPD"),
                            sliderInput(
                                "weightRange",
                                "Range:",
                                min = -200,
                                max = 200,
                                value = c(-200, 200),
                                step = 1
                            ),
                            h4("Milk EPD"),
                            sliderInput(
                                "milkRange",
                                "Range:",
                                min = -50,
                                max = 100,
                                value = c(-50, 100),
                                step = 1
                            ),
                            h4("Quality Grade EPD"),
                            sliderInput(
                                "qualityRange",
                                "Range:",
                                min = -3,
                                max = 3,
                                value = c(-3, 3),
                                step = 0.1
                            ),
                            h4("REA (Ribeye Area) EPD"),
                            sliderInput(
                                "reaRange",
                                "Range:",
                                min = -3,
                                max = 3,
                                value = c(-3, 3),
                                step = 0.1
                            ),
                            h4("MARB (Marbling) EPD"),
                            sliderInput(
                                "marbRange",
                                "Range:",
                                min = -3,
                                max = 3,
                                value = c(-3, 3),
                                step = 0.1
                            ),
                            h4("FAT EPD"),
                            sliderInput(
                                "fatRange",
                                "Range:",
                                min = -1,
                                max = 1,
                                value = c(-1, 1),
                                step = 0.05
                            ),
                            h4("Yield EPD"),
                            sliderInput(
                                "yldRange",
                                "Range:",
                                min = -3,
                                max = 3,
                                value = c(-3, 3),
                                step = 0.1
                            ),
                            h4("Carcass Weight EPD"),
                            sliderInput(
                                "cwRange",
                                "Range:",
                                min = -200,
                                max = 200,
                                value = c(-200, 200),
                                step = 1
                            ),
                            actionButton(
                                "resetFilters",
                                "Reset to Defaults",
                                class = "btn-warning",
                                style = "width: 100%;"
                            )
                        )
                    )
                ),
                
                column(
                    9,
                    div(
                        class = "panel panel-primary",
                        div(
                            class = "panel-heading",
                            "Bulls Matching Your Criteria"
                        ),
                        div(
                            class = "panel-body",
                            fluidRow(
                                column(
                                    6,
                                    h4(textOutput("bullCount"))
                                ),
                                column(
                                    6,
                                    downloadButton(
                                        "downloadData",
                                        "📥 Download Results as CSV",
                                        class = "btn-primary",
                                        style = "float: right;"
                                    )
                                )
                            ),
                            br(),
                            dataTableOutput("bullsTable")
                        )
                    )
                )
            ),
            
            # Summary Statistics
            fluidRow(
                column(
                    12,
                    div(
                        class = "panel panel-info",
                        div(
                            class = "panel-heading",
                            "Summary Statistics"
                        ),
                        div(
                            class = "panel-body",
                            fluidRow(
                                column(
                                    2,
                                    h4("Statistic"),
                                    p("Mean Weight"),
                                    p("Mean Milk"),
                                    p("Mean Quality"),
                                    p("Mean REA"),
                                    p("Mean MARB"),
                                    p("Mean FAT"),
                                    p("Mean Yield"),
                                    p("Mean CW")
                                ),
                                column(
                                    2,
                                    h4("Your Results"),
                                    textOutput("stat_weight"),
                                    textOutput("stat_milk"),
                                    textOutput("stat_quality"),
                                    textOutput("stat_rea"),
                                    textOutput("stat_marb"),
                                    textOutput("stat_fat"),
                                    textOutput("stat_yld"),
                                    textOutput("stat_cw")
                                ),
                                column(
                                    2,
                                    h4("Sale Average"),
                                    textOutput("stat_weight_all"),
                                    textOutput("stat_milk_all"),
                                    textOutput("stat_quality_all"),
                                    textOutput("stat_rea_all"),
                                    textOutput("stat_marb_all"),
                                    textOutput("stat_fat_all"),
                                    textOutput("stat_yld_all"),
                                    textOutput("stat_cw_all")
                                ),
                                column(
                                    2,
                                    h4("Difference"),
                                    textOutput("stat_weight_diff"),
                                    textOutput("stat_milk_diff"),
                                    textOutput("stat_quality_diff"),
                                    textOutput("stat_rea_diff"),
                                    textOutput("stat_marb_diff"),
                                    textOutput("stat_fat_diff"),
                                    textOutput("stat_yld_diff"),
                                    textOutput("stat_cw_diff")
                                ),
                                column(
                                    4,
                                    h4("Insights"),
                                    textOutput("insights")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

# Helper Functions
create_empty_bulls_df <- function() {
    data.frame(
        Lot = character(),
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
        DOC = numeric(),
        CONC = numeric(),
        MAINT = numeric(),
        FDAM = numeric(),
        MRATE = numeric(),
        PELVIC = numeric(),
        HEIGHT = numeric(),
        HYBRID = numeric(),
        stringsAsFactors = FALSE
    )
}

# Extract bulls using flexible labeled regex
extract_bulls_flexible <- function(text) {
    bulls_df <- create_empty_bulls_df()
    
    # Pattern: Look for "Weight: X" type patterns
    weight_pattern <- "Weight[:\\s]+(-?\\d+\\.?\\d*)"
    milk_pattern <- "Milk[:\\s]+(-?\\d+\\.?\\d*)"
    quality_pattern <- "Quality[:\\s]+(-?\\d+\\.?\\d*)"
    rea_pattern <- "REA[:\\s]+(-?\\d+\\.?\\d*)"
    marb_pattern <- "Marb[a-z]*[:\\s]+(-?\\d+\\.?\\d*)"
    
    # Try to identify bull entries (typically marked by lot or ID)
    lot_pattern <- "Lot\\s+(\\d+)"
    
    lots <- gregexpr(lot_pattern, text)
    if (lots[[1]][1] != -1) {
        lot_matches <- regmatches(text, lots)[[1]]
        
        for (lot_match in lot_matches) {
            tryCatch({
                lot_num <- gsub("Lot\\s+", "", lot_match)
                
                # Extract surrounding EPD values
                lot_pos <- gregexpr(gsub("\\\\", "", lot_match), text)[[1]][1]
                text_around <- substr(text, lot_pos, min(lot_pos + 500, nchar(text)))
                
                weight_val <- as.numeric(regmatches(text_around, gregexpr(weight_pattern, text_around))[[1]][1])
                milk_val <- as.numeric(regmatches(text_around, gregexpr(milk_pattern, text_around))[[1]][1])
                quality_val <- as.numeric(regmatches(text_around, gregexpr(quality_pattern, text_around))[[1]][1])
                rea_val <- as.numeric(regmatches(text_around, gregexpr(rea_pattern, text_around))[[1]][1])
                marb_val <- as.numeric(regmatches(text_around, gregexpr(marb_pattern, text_around))[[1]][1])
                
                if (!is.na(weight_val) && !is.na(milk_val)) {
                    new_row <- data.frame(
                        Lot = lot_num,
                        ID = lot_num,
                        Name = paste("Bull", lot_num),
                        Weight = weight_val,
                        Milk = milk_val,
                        Quality = quality_val,
                        REA = rea_val,
                        MARB = marb_val,
                        FAT = NA,
                        YLD = NA,
                        CW = NA,
                        DOC = NA,
                        CONC = NA,
                        MAINT = NA,
                        FDAM = NA,
                        MRATE = NA,
                        PELVIC = NA,
                        HEIGHT = NA,
                        HYBRID = NA,
                        stringsAsFactors = FALSE
                    )
                    bulls_df <- rbind(bulls_df, new_row)
                }
            }, error = function(e) {})
        }
    }
    
    return(bulls_df)
}

# Extract bulls by line-based positional numeric density
extract_bulls_by_lines <- function(text) {
    bulls_df <- create_empty_bulls_df()
    
    lines <- unlist(strsplit(text, "(?<=[.!?]\\s)(?=[A-Z])", perl = TRUE))
    
    for (line in lines) {
        line <- trimws(line)
        if (nchar(line) < 15) next
        
        tokens <- unlist(strsplit(line, "\\s+"))
        numeric_count <- sum(grepl("^-?\\d+\\.?\\d*$", tokens))
        
        if (numeric_count >= 3) {
            tryCatch({
                lot <- tokens[1]
                name <- tokens[2]
                
                epd_vals <- as.numeric(tokens[grep("^-?\\d+\\.?\\d*$", tokens)])
                
                if (length(epd_vals) >= 3 && grepl("\\d", lot)) {
                    new_row <- data.frame(
                        Lot = lot,
                        ID = lot,
                        Name = name,
                        Weight = if(length(epd_vals) > 0) epd_vals[1] else NA,
                        Milk = if(length(epd_vals) > 1) epd_vals[2] else NA,
                        Quality = if(length(epd_vals) > 2) epd_vals[3] else NA,
                        REA = if(length(epd_vals) > 3) epd_vals[4] else NA,
                        MARB = if(length(epd_vals) > 4) epd_vals[5] else NA,
                        FAT = if(length(epd_vals) > 5) epd_vals[6] else NA,
                        YLD = if(length(epd_vals) > 6) epd_vals[7] else NA,
                        CW = if(length(epd_vals) > 7) epd_vals[8] else NA,
                        DOC = if(length(epd_vals) > 8) epd_vals[9] else NA,
                        CONC = if(length(epd_vals) > 9) epd_vals[10] else NA,
                        MAINT = if(length(epd_vals) > 10) epd_vals[11] else NA,
                        FDAM = if(length(epd_vals) > 11) epd_vals[12] else NA,
                        MRATE = if(length(epd_vals) > 12) epd_vals[13] else NA,
                        PELVIC = if(length(epd_vals) > 13) epd_vals[14] else NA,
                        HEIGHT = if(length(epd_vals) > 14) epd_vals[15] else NA,
                        HYBRID = if(length(epd_vals) > 15) epd_vals[16] else NA,
                        stringsAsFactors = FALSE
                    )
                    bulls_df <- rbind(bulls_df, new_row)
                }
            }, error = function(e) {})
        }
    }
    
    if (nrow(bulls_df) > 0) {
        bulls_df <- bulls_df[!duplicated(bulls_df$ID), ]
    }
    
    return(bulls_df)
}

# Advanced extraction with pattern matching
extract_bulls_advanced <- function(text) {
    bulls_df <- create_empty_bulls_df()
    
    # Split by lot/ID patterns
    text_chunks <- unlist(strsplit(text, "(?=^\\s*\\d+\\s+)", perl = TRUE))
    
    for (chunk in text_chunks) {
        chunk <- trimws(chunk)
        if (nchar(chunk) < 10) next
        
        tryCatch({
            lines <- unlist(strsplit(chunk, "\n"))
            if (length(lines) == 0) next
            
            first_line <- trimws(lines[1])
            tokens <- unlist(strsplit(first_line, "\\s+"))
            
            if (length(tokens) < 2) next
            if (!grepl("^\\d", tokens[1])) next
            
            lot <- tokens[1]
            
            # Find name (capital letter sequences)
            name_tokens <- c()
            for (i in 2:min(4, length(tokens))) {
                if (grepl("^[A-Z][a-z]*$", tokens[i])) {
                    name_tokens <- c(name_tokens, tokens[i])
                } else if (grepl("^-?\\d", tokens[i])) {
                    break
                }
            }
            
            name <- paste(name_tokens, collapse = " ")
            if (name == "") name <- paste("Bull", lot)
            
            # Extract numeric EPD values
            epd_values <- c()
            for (i in 2:length(tokens)) {
                val <- suppressWarnings(as.numeric(tokens[i]))
                if (!is.na(val) && abs(val) < 1000) {
                    epd_values <- c(epd_values, val)
                }
            }
            
            if (length(epd_values) >= 3) {
                new_row <- data.frame(
                    Lot = lot,
                    ID = lot,
                    Name = name,
                    Weight = if(length(epd_values) > 0) epd_values[1] else NA,
                    Milk = if(length(epd_values) > 1) epd_values[2] else NA,
                    Quality = if(length(epd_values) > 2) epd_values[3] else NA,
                    REA = if(length(epd_values) > 3) epd_values[4] else NA,
                    MARB = if(length(epd_values) > 4) epd_values[5] else NA,
                    FAT = if(length(epd_values) > 5) epd_values[6] else NA,
                    YLD = if(length(epd_values) > 6) epd_values[7] else NA,
                    CW = if(length(epd_values) > 7) epd_values[8] else NA,
                    DOC = if(length(epd_values) > 8) epd_values[9] else NA,
                    CONC = if(length(epd_values) > 9) epd_values[10] else NA,
                    MAINT = if(length(epd_values) > 10) epd_values[11] else NA,
                    FDAM = if(length(epd_values) > 11) epd_values[12] else NA,
                    MRATE = if(length(epd_values) > 12) epd_values[13] else NA,
                    PELVIC = if(length(epd_values) > 13) epd_values[14] else NA,
                    HEIGHT = if(length(epd_values) > 14) epd_values[15] else NA,
                    HYBRID = if(length(epd_values) > 15) epd_values[16] else NA,
                    stringsAsFactors = FALSE
                )
                bulls_df <- rbind(bulls_df, new_row)
            }
        }, error = function(e) {})
    }
    
    if (nrow(bulls_df) > 0) {
        bulls_df <- bulls_df[!duplicated(bulls_df$ID), ]
    }
    
    return(bulls_df)
}

# Specialized extraction for table-formatted PDFs
extract_bulls_from_tables <- function(text_clean, text_raw) {
    bulls_df <- create_empty_bulls_df()
    
    # Split by common table separators and patterns
    lines_raw <- unlist(strsplit(paste(text_raw, collapse = "\n"), "\n"))
    lines <- unlist(strsplit(text_clean, "\\s{3,}"))
    
    cat("\nDEBUG: Total lines extracted:", length(lines), "\n")
    
    # Pattern 1: Look for lines that start with numbers (lot numbers)
    for (i in seq_along(lines)) {
        line <- trimws(lines[i])
        
        # Skip empty lines and headers
        if (nchar(line) < 10) next
        
        # Look for a line that starts with a number
        if (grepl("^\\d+", line)) {
            tryCatch({
                # Extract all tokens from the line
                tokens <- trimws(unlist(strsplit(line, "\\s+")))
                
                cat("\nDEBUG: Line", i, "tokens:", length(tokens), "\n")
                cat("First 10 tokens:", paste(head(tokens, 10), collapse=" | "), "\n")
                
                # Extract lot (first number)
                lot <- NA
                id <- NA
                name <- NA
                epd_start_idx <- 1
                
                # Try to identify lot, ID, and name
                if (length(tokens) > 0 && grepl("^\\d+$", tokens[1])) {
                    lot <- tokens[1]
                    epd_start_idx <- 2
                }
                
                if (length(tokens) > 1 && grepl("^\\d+", tokens[2])) {
                    id <- tokens[2]
                    epd_start_idx <- 3
                }
                
                # Get name (next token or two if they're alphabetic)
                if (length(tokens) > 2 && grepl("^[A-Za-z]", tokens[3])) {
                    if (length(tokens) > 3 && grepl("^[A-Za-z]", tokens[4]) && !grepl("^-?\\d+", tokens[4])) {
                        name <- paste(tokens[3], tokens[4])
                        epd_start_idx <- 5
                    } else {
                        name <- tokens[3]
                        epd_start_idx <- 4
                    }
                }
                
                # Extract EPD values (numbers after name)
                epd_values <- c()
                for (j in epd_start_idx:length(tokens)) {
                    val <- suppressWarnings(as.numeric(tokens[j]))
                    if (!is.na(val) && abs(val) < 1000) {
                        epd_values <- c(epd_values, val)
                    }
                }
                
                cat("Extracted - Lot:", lot, "ID:", id, "Name:", name, "EPDs:", length(epd_values), "\n")
                
                # Only add if we have ID and at least 3 EPD values
                if (!is.na(id) && !is.na(name) && length(epd_values) >= 3) {
                    new_row <- data.frame(
                        Lot = as.character(if(is.na(lot)) "" else lot),
                        ID = as.character(id),
                        Name = as.character(name),
                        Weight = if(length(epd_values) > 0) epd_values[1] else NA,
                        Milk = if(length(epd_values) > 1) epd_values[2] else NA,
                        Quality = if(length(epd_values) > 2) epd_values[3] else NA,
                        REA = if(length(epd_values) > 3) epd_values[4] else NA,
                        MARB = if(length(epd_values) > 4) epd_values[5] else NA,
                        FAT = if(length(epd_values) > 5) epd_values[6] else NA,
                        YLD = if(length(epd_values) > 6) epd_values[7] else NA,
                        CW = if(length(epd_values) > 7) epd_values[8] else NA,
                        DOC = if(length(epd_values) > 8) epd_values[9] else NA,
                        CONC = if(length(epd_values) > 9) epd_values[10] else NA,
                        MAINT = if(length(epd_values) > 10) epd_values[11] else NA,
                        FDAM = if(length(epd_values) > 11) epd_values[12] else NA,
                        MRATE = if(length(epd_values) > 12) epd_values[13] else NA,
                        PELVIC = if(length(epd_values) > 13) epd_values[14] else NA,
                        HEIGHT = if(length(epd_values) > 14) epd_values[15] else NA,
                        HYBRID = if(length(epd_values) > 15) epd_values[16] else NA,
                        stringsAsFactors = FALSE
                    )
                    
                    bulls_df <- rbind(bulls_df, new_row)
                }
            }, error = function(e) {
                cat("Error processing line:", conditionMessage(e), "\n")
            })
        }
    }
    
    # Remove duplicates
    if (nrow(bulls_df) > 0) {
        bulls_df <- bulls_df[!duplicated(bulls_df$ID), ]
    }
    
    return(bulls_df)
}

# Apply user filters
apply_user_filters <- function(bulls_df, input) {
    filtered <- bulls_df
    
    if (!is.null(input$weightRange)) {
        filtered <- filtered[
            (is.na(filtered$Weight) | filtered$Weight >= input$weightRange[1]) &
                (is.na(filtered$Weight) | filtered$Weight <= input$weightRange[2]),
        ]
    }
    
    if (!is.null(input$milkRange)) {
        filtered <- filtered[
            (is.na(filtered$Milk) | filtered$Milk >= input$milkRange[1]) &
                (is.na(filtered$Milk) | filtered$Milk <= input$milkRange[2]),
        ]
    }
    
    if (!is.null(input$qualityRange)) {
        filtered <- filtered[
            (is.na(filtered$Quality) | filtered$Quality >= input$qualityRange[1]) &
                (is.na(filtered$Quality) | filtered$Quality <= input$qualityRange[2]),
        ]
    }
    
    if (!is.null(input$reaRange)) {
        filtered <- filtered[
            (is.na(filtered$REA) | filtered$REA >= input$reaRange[1]) &
                (is.na(filtered$REA) | filtered$REA <= input$reaRange[2]),
        ]
    }
    
    if (!is.null(input$marbRange)) {
        filtered <- filtered[
            (is.na(filtered$MARB) | filtered$MARB >= input$marbRange[1]) &
                (is.na(filtered$MARB) | filtered$MARB <= input$marbRange[2]),
        ]
    }
    
    if (!is.null(input$fatRange)) {
        filtered <- filtered[
            (is.na(filtered$FAT) | filtered$FAT >= input$fatRange[1]) &
                (is.na(filtered$FAT) | filtered$FAT <= input$fatRange[2]),
        ]
    }
    
    if (!is.null(input$yldRange)) {
        filtered <- filtered[
            (is.na(filtered$YLD) | filtered$YLD >= input$yldRange[1]) &
                (is.na(filtered$YLD) | filtered$YLD <= input$yldRange[2]),
        ]
    }
    
    if (!is.null(input$cwRange)) {
        filtered <- filtered[
            (is.na(filtered$CW) | filtered$CW >= input$cwRange[1]) &
                (is.na(filtered$CW) | filtered$CW <= input$cwRange[2]),
        ]
    }
    
    return(filtered)
}

# Server
server <- function(input, output, session) {
    all_bulls <- reactiveVal(create_empty_bulls_df())
    filtered_bulls <- reactiveVal(create_empty_bulls_df())
    
    # Extract button event
    observeEvent(input$extractButton, {
        # Check if file is uploaded or path is provided
        if (is.null(input$pdfInput) && input$pdfPath == "") {
            output$progress <- renderText("Please upload a PDF or provide a file path.")
            return()
        }
        
        output$progress <- renderText("Extracting EPDs from PDF, please wait...")
        shinyjs::disable("extractButton")  # Disable button during processing
        
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
            
            # Extract text from PDF using pdftools
            pdf_text <- pdf_text(pdf_path)
            full_text <- paste(pdf_text, collapse = " ")
            
            # Print first 5000 characters for better debugging
            cat("\n=== PDF TEXT SAMPLE (First 5000 chars) ===\n")
            cat(substr(full_text, 1, 5000))
            cat("\n=== END SAMPLE ===\n")
            
            # Also print a sample of what the text looks like with line breaks preserved
            cat("\n=== PDF TEXT SAMPLE (With line structure) ===\n")
            pdf_text_sample <- head(pdf_text, 3)  # First 3 pages
            for (i in seq_along(pdf_text_sample)) {
                cat(sprintf("\n--- PAGE %d ---\n", i))
                cat(substr(pdf_text_sample[i], 1, 2000))
            }
            cat("\n=== END PAGE SAMPLE ===\n")
            
            # Clean up the text
            full_text_clean <- gsub("\n", " ", full_text)
            full_text_clean <- gsub("\r", " ", full_text_clean)
            full_text_clean <- gsub("\t", " ", full_text_clean)
            
            # Extract bull data
            bulls_data <- extract_bulls_flexible(full_text_clean)
            cat("\nFlexible extraction found:", nrow(bulls_data), "bulls\n")
            if (nrow(bulls_data) > 0) {
                cat("Sample bull data:\n")
                print(head(bulls_data, 3))
            }
            
            # If no bulls found, try additional parsing methods
            if (nrow(bulls_data) == 0) {
                cat("Trying line-based extraction...\n")
                bulls_data <- extract_bulls_by_lines(full_text_clean)
                cat("Line-based extraction found:", nrow(bulls_data), "bulls\n")
                if (nrow(bulls_data) > 0) {
                    cat("Sample bull data:\n")
                    print(head(bulls_data, 3))
                }
            }
            
            # If still no bulls, try the advanced pattern matching
            if (nrow(bulls_data) == 0) {
                cat("Trying advanced extraction...\n")
                bulls_data <- extract_bulls_advanced(full_text_clean)
                cat("Advanced extraction found:", nrow(bulls_data), "bulls\n")
                if (nrow(bulls_data) > 0) {
                    cat("Sample bull data:\n")
                    print(head(bulls_data, 3))
                }
            }
            
            # Try a completely new method for this specific PDF format
            if (nrow(bulls_data) == 0) {
                cat("Trying specialized extraction for table format...\n")
                bulls_data <- extract_bulls_from_tables(full_text_clean, pdf_text)
                cat("Table extraction found:", nrow(bulls_data), "bulls\n")
                if (nrow(bulls_data) > 0) {
                    cat("Sample bull data:\n")
                    print(head(bulls_data, 3))
                }
            }
            
            # Store all bulls
            all_bulls(bulls_data)
            
            # Now filter the extracted bulls based on user inputs
            if (nrow(bulls_data) > 0) {
                bulls_filtered <- apply_user_filters(bulls_data, input)
                filtered_bulls(bulls_filtered)
                
                message_text <- if (nrow(bulls_filtered) == 0) {
                    paste("Extracted", nrow(bulls_data), "bulls total, but 0 match your specified ranges.")
                } else {
                    paste("Successfully extracted", nrow(bulls_data), "bulls total.", nrow(bulls_filtered), "bulls match your selected criteria!")
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
    
    # Reset filters button
    observeEvent(input$resetFilters, {
        updateSliderInput(session, "weightRange", value = c(-200, 200))
        updateSliderInput(session, "milkRange", value = c(-50, 100))
        updateSliderInput(session, "qualityRange", value = c(-3, 3))
        updateSliderInput(session, "reaRange", value = c(-3, 3))
        updateSliderInput(session, "marbRange", value = c(-3, 3))
        updateSliderInput(session, "fatRange", value = c(-1, 1))
        updateSliderInput(session, "yldRange", value = c(-3, 3))
        updateSliderInput(session, "cwRange", value = c(-200, 200))
    })
    
    # Update filtered bulls based on slider changes
    observeEvent(
        list(
            input$weightRange,
            input$milkRange,
            input$qualityRange,
            input$reaRange,
            input$marbRange,
            input$fatRange,
            input$yldRange,
            input$cwRange
        ),
        {
            if (nrow(all_bulls()) > 0) {
                filtered_bulls(apply_user_filters(all_bulls(), input))
            }
        }
    )
    
    # Display bulls table
    output$bullsTable <- renderDataTable({
        if (nrow(filtered_bulls()) == 0) {
            return(data.frame())
        }
        
        filtered_bulls()[, c("Lot", "ID", "Name", "Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW")]
    }, options = list(pageLength = 10))
    
    # Display bull count
    output$bullCount <- renderText({
        paste("Total Bulls Matching Criteria:", nrow(filtered_bulls()), "of", nrow(all_bulls()))
    })
    
    # Download data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("angus-bulls-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtered_bulls(), file, row.names = FALSE)
        }
    )
    
    # Summary statistics
    output$stat_weight <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$Weight, na.rm = TRUE), 2)
    })
    
    output$stat_milk <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$Milk, na.rm = TRUE), 2)
    })
    
    output$stat_quality <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$Quality, na.rm = TRUE), 2)
    })
    
    output$stat_rea <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$REA, na.rm = TRUE), 2)
    })
    
    output$stat_marb <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$MARB, na.rm = TRUE), 2)
    })
    
    output$stat_fat <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$FAT, na.rm = TRUE), 2)
    })
    
    output$stat_yld <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$YLD, na.rm = TRUE), 2)
    })
    
    output$stat_cw <- renderText({
        if (nrow(filtered_bulls()) == 0) return("N/A")
        round(mean(filtered_bulls()$CW, na.rm = TRUE), 2)
    })
    
    # All bulls statistics
    output$stat_weight_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$Weight, na.rm = TRUE), 2)
    })
    
    output$stat_milk_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$Milk, na.rm = TRUE), 2)
    })
    
    output$stat_quality_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$Quality, na.rm = TRUE), 2)
    })
    
    output$stat_rea_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$REA, na.rm = TRUE), 2)
    })
    
    output$stat_marb_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$MARB, na.rm = TRUE), 2)
    })
    
    output$stat_fat_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$FAT, na.rm = TRUE), 2)
    })
    
    output$stat_yld_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$YLD, na.rm = TRUE), 2)
    })
    
    output$stat_cw_all <- renderText({
        if (nrow(all_bulls()) == 0) return("N/A")
        round(mean(all_bulls()$CW, na.rm = TRUE), 2)
    })
    
    # Differences
    output$stat_weight_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$Weight, na.rm = TRUE) - mean(all_bulls()$Weight, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_milk_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$Milk, na.rm = TRUE) - mean(all_bulls()$Milk, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_quality_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$Quality, na.rm = TRUE) - mean(all_bulls()$Quality, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_rea_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$REA, na.rm = TRUE) - mean(all_bulls()$REA, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_marb_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$MARB, na.rm = TRUE) - mean(all_bulls()$MARB, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_fat_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$FAT, na.rm = TRUE) - mean(all_bulls()$FAT, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_yld_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$YLD, na.rm = TRUE) - mean(all_bulls()$YLD, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    output$stat_cw_diff <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("N/A")
        diff <- round(mean(filtered_bulls()$CW, na.rm = TRUE) - mean(all_bulls()$CW, na.rm = TRUE), 2)
        paste(if(diff > 0) "+" else "", diff)
    })
    
    # Insights
    output$insights <- renderText({
        if (nrow(filtered_bulls()) == 0 || nrow(all_bulls()) == 0) return("No data to analyze.")
        
        filtered_weight <- mean(filtered_bulls()$Weight, na.rm = TRUE)
        all_weight <- mean(all_bulls()$Weight, na.rm = TRUE)
        
        filtered_milk <- mean(filtered_bulls()$Milk, na.rm = TRUE)
        all_milk <- mean(all_bulls()$Milk, na.rm = TRUE)
        
        insights <- c()
        
        if (filtered_weight > all_weight + 5) {
            insights <- c(insights, paste("✓ Your selection averages", round(filtered_weight - all_weight, 1), "lbs heavier than the sale average."))
        } else if (filtered_weight < all_weight - 5) {
            insights <- c(insights, paste("✓ Your selection averages", round(all_weight - filtered_weight, 1), "lbs lighter than the sale average."))
        }
        
        if (filtered_milk > all_milk + 2) {
            insights <- c(insights, paste("✓ Strong milking genetics selected (+", round(filtered_milk - all_milk, 1), " Milk EPD)."))
        } else if (filtered_milk < all_milk - 2) {
            insights <- c(insights, paste("✓ More moderate milk production selected (", round(filtered_milk - all_milk, 1), " Milk EPD)."))
        }
        
        if (length(insights) == 0) {
            insights <- "Your selection closely matches the sale average genetics."
        }
        
        paste(insights, collapse = "\n")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
