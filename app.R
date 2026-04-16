library(shiny)
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
            
            # Initialize data frame to store extracted EPDs
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
                
                # Debug: Check content of the first few lines
                print(head(lines))
                
                for (line in lines) {
                    # Check if this line looks like it contains EPD information
                    if (grepl("ID:|Name:|Weight:|Milk:|Quality:|REA:|MARB:|FAT:|YLD:|CW:", line)) {
                        # Attempt to extract values using string manipulation
                        id <- as.integer(sub(".*ID:\\s*(\\d+).*", "\\1", line))
                        name <- sub(".*Name:\\s*([^,]*).*", "\\1", line)
                        weight <- as.numeric(sub(".*Weight:\\s*(\\d+).*", "\\1", line))
                        milk <- as.numeric(sub(".*Milk:\\s*(\\d+).*", "\\1", line))
                        quality <- as.numeric(sub(".*Quality:\\s*(\\d+).*", "\\1", line))
                        rea <- as.numeric(sub(".*REA:\\s*(\\d+).*", "\\1", line))
                        marb <- as.numeric(sub(".*MARB:\\s*(\\d+).*", "\\1", line))
                        fat <- as.numeric(sub(".*FAT:\\s*(\\d+).*", "\\1", line))
                        yld <- as.numeric(sub(".*YLD:\\s*(\\d+).*", "\\1", line))
                        cw <- as.numeric(sub(".*CW:\\s*(\\d+).*", "\\1", line))
                        
                        # Add to data frame if ID is not NA
                        if (!is.na(id)) {
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
