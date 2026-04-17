library(shiny)
library(shinyjs)
library(dplyr)
library(pdftools)

# Set the maximum request size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

# Define UI for the application
ui <- fluidPage(
    useShinyjs(),
    
    # Western Theme CSS
    tags$head(
        tags$style(HTML("
            /* Main background and text */
            body {
                background-color: #1a1a1a;
                color: #fff;
                font-family: 'Georgia', serif;
            }
            
            /* Title styling */
            .navbar-default {
                background-color: #2d2d2d;
                border-color: #ff8c00;
                border-bottom: 4px solid #ff8c00;
            }
            
            .page-header {
                border-bottom: 3px solid #ff8c00;
                padding-bottom: 20px;
                margin-bottom: 30px;
            }
            
            .page-header h1 {
                color: #ff8c00;
                text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
                font-weight: bold;
                letter-spacing: 2px;
            }
            
            /* Sidebar styling */
            .well {
                background-color: #2d2d2d;
                border: 2px solid #ff8c00;
                border-radius: 8px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.4);
            }
            
            /* Section headers */
            h4 {
                color: #ff8c00;
                font-weight: bold;
                border-bottom: 2px solid #ff8c00;
                padding-bottom: 8px;
                margin-top: 20px;
                text-transform: uppercase;
                letter-spacing: 1px;
            }
            
            h3 {
                color: #ff8c00;
                font-weight: bold;
                text-transform: uppercase;
                letter-spacing: 1px;
            }
            
            /* Button styling */
            .btn-primary {
                background-color: #ff8c00;
                border-color: #1a1a1a;
                color: #000;
                font-weight: bold;
                text-transform: uppercase;
                letter-spacing: 1px;
                border: 2px solid #000;
                transition: all 0.3s ease;
            }
            
            .btn-primary:hover {
                background-color: #e67e00;
                border-color: #ff8c00;
                color: #fff;
                box-shadow: 0 0 15px rgba(255, 140, 0, 0.6);
            }
            
            .btn-primary:focus {
                background-color: #ff8c00;
                border-color: #1a1a1a;
                box-shadow: 0 0 10px rgba(255, 140, 0, 0.4);
            }
            
            .btn-lg {
                padding: 15px 30px;
                font-size: 16px;
                width: 100%;
                margin-bottom: 20px;
            }
            
            /* Input fields */
            input[type='text'],
            input[type='number'],
            select {
                background-color: #1a1a1a;
                color: #ff8c00;
                border: 2px solid #ff8c00;
                border-radius: 4px;
                padding: 8px 12px;
            }
            
            input[type='text']:focus,
            input[type='number']:focus,
            select:focus {
                background-color: #2d2d2d;
                color: #fff;
                border-color: #ff8c00;
                box-shadow: 0 0 10px rgba(255, 140, 0, 0.3);
            }
            
            /* Checkbox styling */
            .checkbox {
                color: #ff8c00;
            }
            
            .checkbox input[type='checkbox'] {
                accent-color: #ff8c00;
            }
            
            .checkbox label {
                color: #fff;
                margin-left: 8px;
            }
            
            /* Horizontal rule */
            hr {
                border-color: #ff8c00;
                margin: 20px 0;
            }
            
            /* Table styling */
            table {
                background-color: #2d2d2d;
                color: #000;
                border: 2px solid #ff8c00;
                border-radius: 4px;
                overflow: hidden;
                width: 100%;
            }
            
            thead {
                background-color: #ff8c00;
                color: #000;
                font-weight: bold;
                text-transform: uppercase;
                letter-spacing: 1px;
            }
            
            tbody {
                color: #000;
            }
            
            tbody tr:nth-child(odd) {
                background-color: #e8e8e8;
            }
            
            tbody tr:nth-child(even) {
                background-color: #f5f5f5;
            }
            
            tbody tr:hover {
                background-color: #d0d0d0;
                box-shadow: inset 0 0 10px rgba(255, 140, 0, 0.2);
            }
            
            /* Progress message */
            #progress {
                color: #ff8c00;
                font-weight: bold;
                padding: 12px;
                background-color: #2d2d2d;
                border-left: 4px solid #ff8c00;
                margin: 15px 0;
                border-radius: 4px;
            }
            
            /* File input */
            .form-group {
                margin-bottom: 20px;
            }
            
            label {
                color: #ff8c00;
                font-weight: bold;
                display: block;
                margin-bottom: 8px;
                text-transform: uppercase;
                letter-spacing: 0.5px;
            }
            
            /* Main panel */
            .form-control {
                background-color: #1a1a1a;
                border: 2px solid #ff8c00;
                color: #ff8c00;
            }
            
            /* Western decoration elements */
            .container-fluid {
                background: linear-gradient(135deg, #1a1a1a 0%, #2d2d2d 100%);
            }
            
            /* Table wrapper for scrolling */
            .table-wrapper {
                overflow-x: auto;
                max-height: 800px;
                overflow-y: auto;
            }
            
            /* Responsive adjustments */
            @media (max-width: 768px) {
                .page-header h1 {
                    font-size: 24px;
                }
                
                .btn-lg {
                    padding: 10px 20px;
                    font-size: 14px;
                }
            }
        "))
    ),
    
    titlePanel("Bull EPD Selection"),
    
    sidebarLayout(
        sidebarPanel(
            # PDF Upload at the top
            fileInput("pdfInput", "Upload Bull Sale Magazine (PDF)", 
                      accept = c("application/pdf"), 
                      multiple = FALSE),
            actionButton("extractButton", "Extract EPDs", class = "btn-primary btn-lg"),
            htmlOutput("progress"),
            hr(),
            
            h4("Select Traits to Extract"),
            checkboxGroupInput("selectedTraits", "Traits:",
                               choices = c(
                                   "Weight" = "Weight",
                                   "Milk" = "Milk",
                                   "Quality" = "Quality",
                                   "REA" = "REA",
                                   "MARB" = "MARB",
                                   "FAT" = "FAT",
                                   "YLD" = "YLD",
                                   "CW" = "CW",
                                   "DOC" = "DOC",
                                   "CONC" = "CONC",
                                   "MAINT" = "MAINT",
                                   "FDAM" = "FDAM",
                                   "MRATE" = "MRATE",
                                   "PELVIC" = "PELVIC",
                                   "HEIGHT" = "HEIGHT",
                                   "HYBRID" = "HYBRID"
                               ),
                               selected = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW"),
                               inline = FALSE),
            hr(),
            
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
            fluidRow(
                column(6, numericInput("minDoc", "Min DOC", value = -5)),
                column(6, numericInput("maxDoc", "Max DOC", value = 5))
            ),
            fluidRow(
                column(6, numericInput("minConc", "Min CONC", value = -2)),
                column(6, numericInput("maxConc", "Max CONC", value = 2))
            ),
            fluidRow(
                column(6, numericInput("minMaint", "Min MAINT", value = -10)),
                column(6, numericInput("maxMaint", "Max MAINT", value = 10))
            ),
            fluidRow(
                column(6, numericInput("minFdam", "Min FDAM", value = -5)),
                column(6, numericInput("maxFdam", "Max FDAM", value = 5))
            ),
            fluidRow(
                column(6, numericInput("minMrate", "Min MRATE", value = -2)),
                column(6, numericInput("maxMrate", "Max MRATE", value = 2))
            ),
            fluidRow(
                column(6, numericInput("minPelvic", "Min PELVIC", value = 0)),
                column(6, numericInput("maxPelvic", "Max PELVIC", value = 5))
            ),
            fluidRow(
                column(6, numericInput("minHeight", "Min HEIGHT", value = 45)),
                column(6, numericInput("maxHeight", "Max HEIGHT", value = 55))
            ),
            fluidRow(
                column(6, numericInput("minHybrid", "Min HYBRID", value = -5)),
                column(6, numericInput("maxHybrid", "Max HYBRID", value = 5))
            ),
            width = 3
        ),
        
        mainPanel(
            h3("Filtered Bulls"),
            div(class = "table-wrapper",
                tableOutput("bullTable")
            ),
            hr(),
            h4("Summary Statistics"),
            tableOutput("summaryTable"),
            width = 9
        )
    )
)

# Define server logic
server <- function(input, output) {
    all_bulls <- reactiveVal(data.frame())
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
    
    # Update filtered bulls when filters change
    observeEvent({
        input$minWeight
        input$maxWeight
        input$minMilk
        input$maxMilk
        input$minQuality
        input$maxQuality
        input$minREA
        input$maxREA
        input$minMARB
        input$maxMARB
        input$minFAT
        input$maxFAT
        input$minYLD
        input$maxYLD
        input$minCW
        input$maxCW
        input$minDoc
        input$maxDoc
        input$minConc
        input$maxConc
        input$minMaint
        input$maxMaint
        input$minFdam
        input$maxFdam
        input$minMrate
        input$maxMrate
        input$minPelvic
        input$maxPelvic
        input$minHeight
        input$maxHeight
        input$minHybrid
        input$maxHybrid
        input$selectedTraits
    }, {
        if (nrow(all_bulls()) > 0) {
            bulls_filtered <- apply_user_filters(all_bulls(), input)
            filtered_bulls(bulls_filtered)
            
            if (nrow(bulls_filtered) > 0) {
                message_text <- paste(nrow(bulls_filtered), "bull(s) match your selected criteria!")
            } else {
                message_text <- "No bulls match your selected criteria. Try adjusting your ranges."
            }
            
            output$progress <- renderText(message_text)
        }
    })
    
    # Function to apply filters based on user inputs
    apply_user_filters <- function(bulls_data, input) {
        result <- bulls_data
        
        # Apply Weight filter
        if ("Weight" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(Weight) | (Weight >= input$minWeight & Weight <= input$maxWeight))
        }
        
        # Apply Milk filter
        if ("Milk" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(Milk) | (Milk >= input$minMilk & Milk <= input$maxMilk))
        }
        
        # Apply Quality filter
        if ("Quality" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(Quality) | (Quality >= input$minQuality & Quality <= input$maxQuality))
        }
        
        # Apply REA filter
        if ("REA" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(REA) | (REA >= input$minREA & REA <= input$maxREA))
        }
        
        # Apply MARB filter
        if ("MARB" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(MARB) | (MARB >= input$minMARB & MARB <= input$maxMARB))
        }
        
        # Apply FAT filter
        if ("FAT" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(FAT) | (FAT >= input$minFAT & FAT <= input$maxFAT))
        }
        
        # Apply YLD filter
        if ("YLD" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(YLD) | (YLD >= input$minYLD & YLD <= input$maxYLD))
        }
        
        # Apply CW filter
        if ("CW" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(CW) | (CW >= input$minCW & CW <= input$maxCW))
        }
        
        # Apply DOC filter
        if ("DOC" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(DOC) | (DOC >= input$minDoc & DOC <= input$maxDoc))
        }
        
        # Apply CONC filter
        if ("CONC" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(CONC) | (CONC >= input$minConc & CONC <= input$maxConc))
        }
        
        # Apply MAINT filter
        if ("MAINT" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(MAINT) | (MAINT >= input$minMaint & MAINT <= input$maxMaint))
        }
        
        # Apply FDAM filter
        if ("FDAM" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(FDAM) | (FDAM >= input$minFdam & FDAM <= input$maxFdam))
        }
        
        # Apply MRATE filter
        if ("MRATE" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(MRATE) | (MRATE >= input$minMrate & MRATE <= input$maxMrate))
        }
        
        # Apply PELVIC filter
        if ("PELVIC" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(PELVIC) | (PELVIC >= input$minPelvic & PELVIC <= input$maxPelvic))
        }
        
        # Apply HEIGHT filter
        if ("HEIGHT" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(HEIGHT) | (HEIGHT >= input$minHeight & HEIGHT <= input$maxHeight))
        }
        
        # Apply HYBRID filter
        if ("HYBRID" %in% input$selectedTraits) {
            result <- result %>%
                filter(is.na(HYBRID) | (HYBRID >= input$minHybrid & HYBRID <= input$maxHybrid))
        }
        
        return(result)
    }
    
    output$bullTable <- renderTable({
        req(filtered_bulls())
        req(input$selectedTraits)
        
        displayed_bulls <- filtered_bulls()
        
        if (nrow(displayed_bulls) == 0) {
            return(data.frame(Message = "No bulls found matching these criteria."))
        }
        
        # Start with Lot, ID and Name
        cols_to_select <- c("Lot", "ID", "Name")
        
        # Add selected traits in order
        trait_order <- c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW", 
                         "DOC", "CONC", "MAINT", "FDAM", "MRATE", "PELVIC", "HEIGHT", "HYBRID")
        
        for (trait in trait_order) {
            if (trait %in% input$selectedTraits && trait %in% names(displayed_bulls)) {
                cols_to_select <- c(cols_to_select, trait)
            }
        }
        
        # Format the output
        displayed_bulls %>%
            select(all_of(cols_to_select)) %>%
            arrange(as.numeric(Lot), as.numeric(ID)) %>%
            mutate(across(where(is.numeric), ~round(., 2)))
        
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na.rm = TRUE)
    
    output$summaryTable <- renderTable({
        req(filtered_bulls())
        req(input$selectedTraits)
        
        bulls_data <- filtered_bulls()
        
        if (nrow(bulls_data) == 0) {
            return(data.frame(Trait = character(), Mean = numeric(), Min = numeric(), Max = numeric()))
        }
        
        # Build summary stats only for selected traits
        trait_data <- list()
        trait_names <- c()
        
        trait_info <- list(
            Weight = "Weight",
            Milk = "Milk",
            Quality = "Quality",
            REA = "REA",
            MARB = "MARB",
            FAT = "FAT",
            YLD = "YLD",
            CW = "CW",
            DOC = "DOC",
            CONC = "CONC",
            MAINT = "MAINT",
            FDAM = "FDAM",
            MRATE = "MRATE",
            PELVIC = "PELVIC",
            HEIGHT = "HEIGHT",
            HYBRID = "HYBRID"
        )
        
        for (trait in input$selectedTraits) {
            if (trait %in% names(bulls_data)) {
                trait_data[[trait]] <- list(
                    Mean = mean(bulls_data[[trait]], na.rm = TRUE),
                    Min = min(bulls_data[[trait]], na.rm = TRUE),
                    Max = max(bulls_data[[trait]], na.rm = TRUE)
                )
                trait_names <- c(trait_names, trait)
            }
        }
        
        summary_stats <- data.frame(
            Trait = trait_names,
            Mean = sapply(trait_names, function(t) trait_data[[t]]$Mean),
            Min = sapply(trait_names, function(t) trait_data[[t]]$Min),
            Max = sapply(trait_names, function(t) trait_data[[t]]$Max),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        
        # Format to 2 decimal places
        summary_stats$Mean <- round(summary_stats$Mean, 2)
        summary_stats$Min <- round(summary_stats$Min, 2)
        summary_stats$Max <- round(summary_stats$Max, 2)
        
        summary_stats
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Flexible extraction function - tries multiple patterns
extract_bulls_flexible <- function(text) {
    bulls_df <- data.frame(
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
    
    # Pattern 1: Try to find EPD values with labels
    patterns <- list(
        # Pattern for "EPD: value" format
        "EPD format" = list(
            lot = "(?:Lot|LOT)\\s*[:#]?\\s*(\\S+)",
            id = "(?:ID|Lot|#)\\s*[:#]?\\s*(\\S+)",
            name = "(?:Name|Bull)\\s*[:#]?\\s*([A-Za-z0-9\\s-]+?)(?=Weight|WEIGHT|EPD)",
            weight = "Weight\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            milk = "Milk\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            quality = "Quality\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            rea = "REA\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            marb = "MARB\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            fat = "FAT\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            yld = "YLD\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            cw = "CW\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            doc = "DOC\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            conc = "CONC\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            maint = "MAINT\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            fdam = "FDAM\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            mrate = "MRATE\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            pelvic = "PELVIC\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            height = "HEIGHT\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)",
            hybrid = "HYBRID\\s*[:#]?\\s*([-+]?\\d+(?:\\.\\d+)?)"
        )
    )
    
    # Split by common delimiters that might separate bulls
    potential_sections <- strsplit(text, "(?:Lot|ID|Bull)\\s*[:#]?\\s*(?=[A-Za-z0-9])", perl = TRUE)[[1]]
    
    for (section in potential_sections) {
        if (nchar(section) > 20) {
            tryCatch({
                # Extract each field
                lot <- extract_value(section, "(?:Lot|LOT)\\s*[:#]?\\s*(\\S+)")
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
                doc <- extract_numeric_value(section, "DOC")
                conc <- extract_numeric_value(section, "CONC")
                maint <- extract_numeric_value(section, "MAINT")
                fdam <- extract_numeric_value(section, "FDAM")
                mrate <- extract_numeric_value(section, "MRATE")
                pelvic <- extract_numeric_value(section, "PELVIC")
                height <- extract_numeric_value(section, "HEIGHT")
                hybrid <- extract_numeric_value(section, "HYBRID")
                
                # Only add if we have valid data
                if (!is.na(id) && !is.na(name) && length(c(weight, milk, quality, rea, marb, fat, yld, cw, doc, conc, maint, fdam, mrate, pelvic, height, hybrid)) > 0) {
                    if (sum(!is.na(c(weight, milk, quality, rea, marb, fat, yld, cw, doc, conc, maint, fdam, mrate, pelvic, height, hybrid))) >= 3) {
                        bulls_df <- rbind(bulls_df, data.frame(
                            Lot = as.character(if(is.na(lot)) "" else lot),
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
                            DOC = as.numeric(doc),
                            CONC = as.numeric(conc),
                            MAINT = as.numeric(maint),
                            FDAM = as.numeric(fdam),
                            MRATE = as.numeric(mrate),
                            PELVIC = as.numeric(pelvic),
                            HEIGHT = as.numeric(height),
                            HYBRID = as.numeric(hybrid),
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
                    # Assume first number is Lot, second is ID, rest are EPDs
                    bulls_df <- rbind(bulls_df, data.frame(
                        Lot = as.character(numbers[1]),
                        ID = as.character(numbers[2]),
                        Name = paste("Bull", numbers[2]),
                        Weight = numbers[3],
                        Milk = numbers[4],
                        Quality = numbers[5],
                        REA = numbers[6],
                        MARB = numbers[7],
                        FAT = numbers[8],
                        YLD = if(length(numbers) > 8) numbers[9] else NA,
                        CW = if(length(numbers) > 9) numbers[10] else NA,
                        DOC = if(length(numbers) > 10) numbers[11] else NA,
                        CONC = if(length(numbers) > 11) numbers[12] else NA,
                        MAINT = if(length(numbers) > 12) numbers[13] else NA,
                        FDAM = if(length(numbers) > 13) numbers[14] else NA,
                        MRATE = if(length(numbers) > 14) numbers[15] else NA,
                        PELVIC = if(length(numbers) > 15) numbers[16] else NA,
                        HEIGHT = if(length(numbers) > 16) numbers[17] else NA,
                        HYBRID = if(length(numbers) > 17) numbers[18] else NA,
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
