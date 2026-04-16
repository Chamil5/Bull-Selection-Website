library(shiny)
library(dplyr)
library(pdftools)
library(shinyjs)

# Set maximum request size to 200 MB
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
            htmlOutput("progress")  # To show progress messages
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
            
            if (length(pdf_text_content) == 0) {
                stop("PDF file is empty or could not be read.")
            }
            
            # Data frame to store extracted EPDs
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
                    # Use a regex pattern to capture EPD details
                    epd_pattern <- "ID: (\\d+), Name: ([A-Za-z\\s]+), Weight: (\\d+), Milk: (\\d+), Quality: (\\d+\\.?\\d*), REA: (\\d+\\.?\\d*), MARB: (\\d+\\.?\\d*), FAT: (\\d+\\.?\\d*), YLD: (\\d+\\.?\\d*), CW: (\\d+)"
                    if (grepl(epd_pattern, line)) {
                        match <- regexec(epd_pattern, line)
                        groups <- regmatches(line, match)
                        if (length(groups[[1]]) > 0) {
                            bulls_data <- rbind(bulls_data,
                                                data.frame(ID = as.integer(groups[[1]][2]),
                                                           Name = as.character(groups[[1]][3]),
                                                           Weight = as.numeric(groups[[1]][4]),
                                                           Milk = as.numeric(groups[[1]][5]),
                                                           Quality = as.numeric(groups[[1]][6]),
                                                           REA = as.numeric(groups[[1]][7]),
                                                           MARB = as.numeric(groups[[1]][8]),
                                                           FAT = as.numeric(groups[[1]][9]),
                                                           YLD = as.numeric(groups[[1]][10]),
                                                           CW = as.numeric(groups[[1]][11]),
                                                           stringsAsFactors = FALSE))
                        }
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
        bulls()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
