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
            
            # Check for empty PDF or extraction errors
            if (length(pdf_text_content) == 0) {
                stop("PDF file is empty or could not be read.")
            }
            
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
            
            # Process each page of the PDF
            for (page_text in pdf_text_content) {
                lines <- strsplit(page_text, "\n")[[1]]  # Split page text into lines
                
                for (line in lines) {
                    # Extract values for each trait using regex
                    id_match <- regmatches(line, regexpr("ID:\\s*(\\d+)", line))
                    name_match <- regmatches(line, regexpr("Name:\\s*([^,]*)", line))
                    weight_match <- regmatches(line, regexpr("Weight:\\s*(\\d+)", line))
                    milk_match <- regmatches(line, regexpr("Milk:\\s*(\\d+)", line))
                    quality_match <- regmatches(line, regexpr("Quality:\\s*(\\d+)", line))
                    rea_match <- regmatches(line, regexpr("REA:\\s*(\\d+)", line))
                    marb_match <- regmatches(line, regexpr("MARB:\\s*(\\d+)", line))
                    fat_match <- regmatches(line, regexpr("FAT:\\s*(\\d+)", line))
                    yld_match <- regmatches(line, regexpr("YLD:\\s*(\\d+)", line))
                    cw_match <- regmatches(line, regexpr("CW:\\s*(\\d+)", line))
                    
                    # Only proceed if ID is found and is valid
                    if (length(id_match) > 0 && nchar(id_match) > 0) {
                        id <- as.integer(sub("ID:\\s*", "", id_match))
                        name <- sub("Name:\\s*", "", name_match)
                        weight <- as.numeric(sub("Weight:\\s*", "", weight_match))
                        milk <- as.numeric(sub("Milk:\\s*", "", milk_match))
                        quality <- as.numeric(sub("Quality:\\s*", "", quality_match))
                        rea <- as.numeric(sub("REA:\\s*", "", rea_match))
                        marb <- as.numeric(sub("MARB:\\s*", "", marb_match))
                        fat <- as.numeric(sub("FAT:\\s*", "", fat_match))
                        yld <- as.numeric(sub("YLD:\\s*", "", yld_match))
                        cw <- as.numeric(sub("CW:\\s*", "", cw_match))
                        
                        # Add extracted data to bulls_data
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
