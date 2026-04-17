library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(pdftools)
library(stringr)
library(DT)

# OSU Color Palette
osu_orange <- "#FF6600"
osu_black <- "#003366"
osu_light_orange <- "#FFB84D"
osu_light_gray <- "#F5F5F5"

# ==================== UI ====================
ui <- dashboardPage(
    # Custom CSS for OSU branding
    tags$head(
        tags$style(HTML("
      /* OSU Custom Branding */
      .main-header {
        background: linear-gradient(135deg, #003366 0%, #FF6600 100%);
        box-shadow: 0 2px 4px rgba(0,51,102,0.3);
      }
      
      .main-header .logo {
        background: #003366;
        color: #FF6600;
        font-weight: bold;
        letter-spacing: 1px;
      }
      
      .main-header .navbar {
        background: #003366;
      }
      
      .sidebar {
        background: #F5F5F5;
        border-right: 4px solid #FF6600;
      }
      
      .content-wrapper {
        background: #FFFFFF;
      }
      
      .box {
        border-top: 4px solid #FF6600;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      .box.box-primary {
        border-top-color: #FF6600;
      }
      
      .btn-primary {
        background-color: #FF6600;
        border-color: #FF6600;
      }
      
      .btn-primary:hover {
        background-color: #E55A00;
        border-color: #E55A00;
      }
      
      .btn-success {
        background-color: #FF6600;
        border-color: #FF6600;
      }
      
      .btn-success:hover {
        background-color: #E55A00;
        border-color: #E55A00;
      }
      
      /* OSU Header Styling */
      .osu-header {
        background: linear-gradient(135deg, #003366 0%, #1a4d7a 50%, #FF6600 100%);
        color: white;
        padding: 20px;
        text-align: center;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.2);
      }
      
      .osu-header h1 {
        margin: 0;
        font-size: 28px;
        font-weight: bold;
        letter-spacing: 1px;
      }
      
      .osu-header p {
        margin: 5px 0 0 0;
        font-size: 14px;
        opacity: 0.9;
      }
      
      /* Sidebar Title */
      .sidebar-title {
        color: #003366;
        font-weight: bold;
        font-size: 14px;
        margin-top: 15px;
        margin-bottom: 10px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        border-bottom: 2px solid #FF6600;
        padding-bottom: 8px;
      }
      
      /* Trait Selection Box */
      .trait-box {
        background: #F5F5F5;
        border-left: 4px solid #FF6600;
        padding: 10px;
        margin: 8px 0;
        border-radius: 3px;
      }
      
      /* Data Table Styling */
      .dataTables_wrapper {
        font-size: 13px;
      }
      
      .table-header {
        background: linear-gradient(135deg, #003366 0%, #FF6600 100%);
        color: white;
      }
      
      /* Stats Cards */
      .stat-card {
        background: white;
        border: 2px solid #FF6600;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        text-align: center;
      }
      
      .stat-card h3 {
        color: #003366;
        margin: 0 0 10px 0;
        font-size: 16px;
      }
      
      .stat-card .stat-value {
        color: #FF6600;
        font-size: 24px;
        font-weight: bold;
      }
      
      /* Info Box Custom */
      .info-box {
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      .info-box-icon {
        background: #FF6600;
      }
      
      /* Footer */
      .osu-footer {
        background: #003366;
        color: white;
        text-align: center;
        padding: 15px;
        margin-top: 20px;
        font-size: 12px;
      }
      
      .osu-footer a {
        color: #FFB84D;
        text-decoration: none;
      }
      
      .osu-footer a:hover {
        text-decoration: underline;
      }
      
      /* Checkbox Styling */
      .checkbox {
        margin-bottom: 8px;
      }
      
      .checkbox label {
        font-size: 13px;
        color: #003366;
      }
      
      /* Console Output */
      .console-output {
        background: #1a1a1a;
        color: #00FF00;
        font-family: 'Courier New', monospace;
        padding: 10px;
        border-radius: 3px;
        font-size: 12px;
        max-height: 200px;
        overflow-y: auto;
      }
    "))
    ),
    
    # Dashboard Header
    dashboardHeader(
        title = "OSU EPD Extractor",
        titleWidth = 300,
        tags$li(class = "dropdown",
                tags$a(href = "https://www.okstate.edu", 
                       target = "_blank",
                       tags$img(height = "30px", 
                                alt = "OSU Logo",
                                src = "https://brand.okstate.edu/sites/default/files/inline-images/osu-logo-horizontal-color-rgb.png"),
                       style = "padding: 10px;"))
    ),
    
    # Sidebar
    dashboardSidebar(
        width = 300,
        
        # OSU Branding
        div(class = "osu-header",
            h1("🐂 EPD Extraction Tool"),
            p("Oklahoma State University")
        ),
        
        # File Upload
        div(class = "sidebar-title", "📁 Step 1: Upload PDF"),
        fileInput("pdfFile", 
                  "Choose PDF File",
                  accept = c(".pdf"),
                  buttonLabel = "Browse...",
                  placeholder = "Max 200MB"),
        
        hr(style = "border-color: #FF6600;"),
        
        # Trait Selection
        div(class = "sidebar-title", "🎯 Step 2: Select Traits"),
        p("Choose which EPD traits to extract:", style = "font-size: 12px; color: #666;"),
        
        checkboxGroupInput(
            "selectedTraits",
            label = NULL,
            choices = list(
                "Weight Traits" = c(
                    "Weight (WW)" = "Weight",
                    "Calving Ease Direct (CED)" = "CED",
                    "Height" = "HEIGHT"
                ),
                "Production Traits" = c(
                    "Milk" = "Milk",
                    "Maternal Milk (MM)" = "MAINT",
                    "Maintenance Energy (ME)" = "MAINT"
                ),
                "Carcass Traits" = c(
                    "Marbling (MARB)" = "MARB",
                    "Rib Eye Area (REA)" = "REA",
                    "Fat Thickness" = "FAT",
                    "Carcass Weight (CW)" = "CW",
                    "Yield Grade (YLD)" = "YLD"
                ),
                "Quality & Other" = c(
                    "Meat Quality" = "Quality",
                    "Docility" = "DOC",
                    "Conception Rate" = "CONC",
                    "Feed Intake" = "FDAM",
                    "Maternal Rate" = "MRATE",
                    "Pelvic Area" = "PELVIC",
                    "Hybrid Vigor" = "HYBRID"
                )
            ),
            selected = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW"),
            inline = FALSE
        ),
        
        hr(style = "border-color: #FF6600;"),
        
        # Filter Ranges
        div(class = "sidebar-title", "📊 Step 3: Set Filter Ranges"),
        p("(Optional - leave blank for no filter)", style = "font-size: 11px; color: #999;"),
        
        # Dynamic range inputs (will be created by server)
        uiOutput("rangeInputs"),
        
        hr(style = "border-color: #FF6600;"),
        
        # Action Button
        actionButton("extractButton", 
                     "🚀 Extract EPDs",
                     class = "btn-block",
                     style = "background-color: #FF6600; color: white; border: none; font-weight: bold; padding: 12px;"),
        
        # Info Box
        box(
            title = "ℹ️ About",
            status = "info",
            solidHeader = FALSE,
            width = 12,
            p("This tool extracts EPD (Expected Progeny Difference) traits from PDF documents using advanced text extraction and pattern matching.",
              style = "font-size: 12px;"),
            p("For more info: ", 
              tags$a("OSU Beef Genetics", 
                     href = "https://www.ansi.okstate.edu/", 
                     target = "_blank",
                     style = "color: #FF6600;"),
              style = "font-size: 11px;"),
            br(),
            p("© 2024 Oklahoma State University", style = "font-size: 10px; color: #999;")
        )
    ),
    
    # Main Body
    dashboardBody(
        # Results Tabs
        tabsetPanel(
            # Tab 1: Extracted Data
            tabPanel(
                title = "📋 Filtered Bulls",
                icon = icon("table"),
                br(),
                
                fluidRow(
                    column(12,
                           box(
                               title = "Extracted EPD Data",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 12,
                               collapsible = TRUE,
                               collapsed = FALSE,
                               DTOutput("bullsTable")
                           )
                    )
                )
            ),
            
            # Tab 2: Summary Statistics
            tabPanel(
                title = "📈 Summary Statistics",
                icon = icon("chart-bar"),
                br(),
                
                fluidRow(
                    column(12,
                           infoBoxOutput("bullCountBox"),
                           infoBoxOutput("traitsCountBox"),
                           infoBoxOutput("successRateBox")
                    )
                ),
                
                fluidRow(
                    column(12,
                           box(
                               title = "Statistical Summary",
                               status = "success",
                               solidHeader = TRUE,
                               width = 12,
                               collapsible = TRUE,
                               collapsed = FALSE,
                               DTOutput("statsTable")
                           )
                    )
                )
            ),
            
            # Tab 3: Debug Console
            tabPanel(
                title = "🔍 Debug Console",
                icon = icon("bug"),
                br(),
                
                fluidRow(
                    column(12,
                           box(
                               title = "Extraction Debug Output (first 2000 chars)",
                               status = "warning",
                               solidHeader = TRUE,
                               width = 12,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               div(class = "console-output",
                                   verbatimTextOutput("debugConsole"))
                           )
                    )
                ),
                
                fluidRow(
                    column(12,
                           box(
                               title = "Extraction Details",
                               status = "info",
                               solidHeader = TRUE,
                               width = 12,
                               verbatimTextOutput("extractionDetails")
                           )
                    )
                )
            )
        )
    ),
    
    # Custom Footer
    tags$footer(
        div(class = "osu-footer",
            p("Oklahoma State University EPD Extraction Tool",
              br(),
              "Department of Animal and Food Sciences",
              br(),
              tags$a("Visit OSU", href = "https://www.okstate.edu", target = "_blank"),
              " | ",
              tags$a("Contact ANSI", href = "https://www.ansi.okstate.edu/", target = "_blank")
            ))
    )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
        extracted_data = NULL,
        all_traits = c("Weight", "Milk", "Quality", "REA", "MARB", "FAT", "YLD", "CW", 
                       "DOC", "CONC", "MAINT", "FDAM", "MRATE", "PELVIC", "HEIGHT", "HYBRID"),
        debug_text = ""
    )
    
    # Generate range inputs dynamically
    output$rangeInputs <- renderUI({
        req(input$selectedTraits)
        
        ranges <- list(
            Weight = c(-100, 100),
            Milk = c(-50, 50),
            Quality = c(-10, 10),
            REA = c(-3, 3),
            MARB = c(-100, 100),
            FAT = c(-0.5, 0.5),
            YLD = c(-3, 3),
            CW = c(-100, 100),
            DOC = c(-20, 20),
            CONC = c(-5, 5),
            MAINT = c(-50, 50),
            FDAM = c(-10, 10),
            MRATE = c(-5, 5),
            PELVIC = c(-2, 2),
            HEIGHT = c(-5, 5),
            HYBRID = c(-10, 10)
        )
        
        lapply(input$selectedTraits, function(trait) {
            range_vals <- ranges[[trait]]
            tagList(
                tags$div(
                    class = "trait-box",
                    strong(trait, style = "color: #FF6600;"),
                    br(),
                    sliderInput(
                        inputId = paste0("range_", trait),
                        label = "Range:",
                        min = range_vals[1],
                        max = range_vals[2],
                        value = range_vals,
                        step = 0.1,
                        ticks = FALSE
                    )
                )
            )
        })
    })
    
    # Extract EPDs
    observeEvent(input$extractButton, {
        req(input$pdfFile)
        
        tryCatch({
            pdf_path <- input$pdfFile$datapath
            pdf_text <- pdf_text(pdf_path)
            full_text <- paste(pdf_text, collapse = "\n")
            rv$debug_text <- substr(full_text, 1, 2000)
            
            # Extract bulls
            extracted <- extract_bulls_flexible(full_text, rv$all_traits)
            
            # Apply user filters
            filtered <- apply_user_filters(extracted, input$selectedTraits, input)
            rv$extracted_data <- filtered
            
            showNotification(
                paste0("✅ Successfully extracted ", nrow(filtered), " bulls!"),
                type = "message",
                duration = 5
            )
        }, error = function(e) {
            showNotification(
                paste0("❌ Error: ", e$message),
                type = "error",
                duration = 5
            )
        })
    })
    
    # Render Bulls Table
    output$bullsTable <- renderDT({
        req(rv$extracted_data, input$selectedTraits)
        
        data_cols <- c("ID", "Name", input$selectedTraits)
        display_data <- rv$extracted_data[, data_cols, drop = FALSE]
        
        datatable(
            display_data,
            options = list(
                pageLength = 10,
                dom = 'lftip',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            ),
            rownames = FALSE,
            filter = 'top'
        ) %>%
            formatStyle(
                columns = names(display_data),
                backgroundColor = '#F9F9F9',
                borderColor = '#FF6600'
            )
    })
    
    # Info Boxes
    output$bullCountBox <- renderInfoBox({
        count <- if (is.null(rv$extracted_data)) 0 else nrow(rv$extracted_data)
        infoBox(
            title = "Bulls Extracted",
            value = count,
            icon = icon("cow"),
            color = "orange",
            fill = TRUE
        )
    })
    
    output$traitsCountBox <- renderInfoBox({
        count <- length(input$selectedTraits)
        infoBox(
            title = "Traits Selected",
            value = count,
            icon = icon("list-check"),
            color = "blue",
            fill = TRUE
        )
    })
    
    output$successRateBox <- renderInfoBox({
        if (is.null(rv$extracted_data)) {
            rate <- "N/A"
        } else {
            na_count <- sum(is.na(rv$extracted_data[[input$selectedTraits[1]]]))
            rate <- paste0(round((1 - na_count / nrow(rv$extracted_data)) * 100), "%")
        }
        infoBox(
            title = "Data Completeness",
            value = rate,
            icon = icon("check-circle"),
            color = "green",
            fill = TRUE
        )
    })
    
    # Stats Table
    output$statsTable <- renderDT({
        req(rv$extracted_data, input$selectedTraits)
        
        stats <- data.frame(
            Trait = input$selectedTraits,
            Mean = sapply(input$selectedTraits, function(t) {
                round(mean(as.numeric(rv$extracted_data[[t]]), na.rm = TRUE), 2)
            }),
            Min = sapply(input$selectedTraits, function(t) {
                round(min(as.numeric(rv$extracted_data[[t]]), na.rm = TRUE), 2)
            }),
            Max = sapply(input$selectedTraits, function(t) {
                round(max(as.numeric(rv$extracted_data[[t]]), na.rm = TRUE), 2)
            }),
            StdDev = sapply(input$selectedTraits, function(t) {
                round(sd(as.numeric(rv$extracted_data[[t]]), na.rm = TRUE), 2)
            }),
            NAs = sapply(input$selectedTraits, function(t) {
                sum(is.na(rv$extracted_data[[t]]))
            })
        )
        
        datatable(
            stats,
            options = list(
                pageLength = 16,
                dom = 'lftip'
            ),
            rownames = FALSE
        ) %>%
            formatStyle(
                'Trait',
                backgroundColor = '#FF6600',
                color = 'white',
                fontWeight = 'bold'
            )
    })
    
    # Debug Console
    output$debugConsole <- renderText({
        if (nchar(rv$debug_text) > 0) {
            rv$debug_text
        } else {
            "No PDF extracted yet. Upload a PDF and click 'Extract EPDs' to see debug output."
        }
    })
    
    output$extractionDetails <- renderText({
        if (is.null(rv$extracted_data)) {
            "No extraction performed yet."
        } else {
            paste0(
                "Total Bulls Extracted: ", nrow(rv$extracted_data), "\n",
                "Selected Traits: ", paste(input$selectedTraits, collapse = ", "), "\n",
                "Data Dimensions: ", nrow(rv$extracted_data), " rows × ", ncol(rv$extracted_data), " columns\n",
                "Extraction Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            )
        }
    })
}

# ==================== HELPER FUNCTIONS ====================

extract_bulls_flexible <- function(text, all_traits) {
    # Pattern to find bull ID and Name lines
    bull_pattern <- "^\\s*([A-Z0-9]+)\\s+([A-Za-z\\s]+?)\\s+(-?\\d+)?\\s*$"
    
    # Split text into lines
    lines <- str_split(text, "\n")[[1]]
    
    # Initialize data frame
    bulls <- data.frame(ID = character(), Name = character(), stringsAsFactors = FALSE)
    
    # Add all trait columns
    for (trait in all_traits) {
        bulls[[trait]] <- NA_real_
    }
    
    # Extract bulls
    for (i in seq_along(lines)) {
        line <- lines[i]
        
        if (str_detect(line, bull_pattern)) {
            matches <- str_match(line, bull_pattern)
            id <- matches[1, 2]
            name <- str_trim(matches[1, 3])
            
            # Extract traits from nearby lines
            bull_row <- data.frame(ID = id, Name = name, stringsAsFactors = FALSE)
            
            for (trait in all_traits) {
                bull_row[[trait]] <- NA_real_
            }
            
            # Look for trait values in surrounding lines
            for (j in max(1, i-5):min(length(lines), i+5)) {
                for (trait in all_traits) {
                    pattern <- paste0("\\b", trait, "\\s+(-?\\d+\\.?\\d*)")
                    if (str_detect(lines[j], pattern)) {
                        value <- as.numeric(str_extract(lines[j], pattern))
                        bull_row[[trait]] <- value
                    }
                }
            }
            
            bulls <- rbind(bulls, bull_row)
        }
    }
    
    return(bulls)
}

apply_user_filters <- function(data, selected_traits, input) {
    filtered <- data
    
    for (trait in selected_traits) {
        range_id <- paste0("range_", trait)
        
        if (!is.null(input[[range_id]])) {
            min_val <- input[[range_id]][1]
            max_val <- input[[range_id]][2]
            
            # Filter by range
            filtered <- filtered[
                is.na(filtered[[trait]]) | 
                    (as.numeric(filtered[[trait]]) >= min_val & as.numeric(filtered[[trait]]) <= max_val),
            ]
        }
    }
    
    return(filtered)
}

# Run the app
shinyApp(ui, server)
