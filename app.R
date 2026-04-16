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
            htmlOutput("progress"),
            hr(),
            h4("Filter EPDs"),
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
                    if (grepl("ID:|Name:|Weight:|Milk:|Quality:|REA:|MARB:|FAT:|YLD:|CW:", line)) {
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
        req(bulls())
        
        # Filtering based on slider inputs
        filtered_bulls <- bulls() %>%
            filter(
                Weight >= input$weightRange[1], Weight <= input$weightRange[2],
                Milk >= input$milkRange[1], Milk <= input$milkRange[2],
                Quality >= input$qualityRange[1], Quality <= input$qualityRange[2],
                REA >= input$reaRange[1], REA <= input$reaRange[2],
                MARB >= input$marbRange[1], MARB <= input$marbRange[2],
                FAT >= input$fatRange[1], FAT <= input$fatRange[2],
                YLD >= input$yldRange[1], YLD <= input$yldRange[2],
                CW >= input$cwRange[1], CW <= input$cwRange[2]
            )
        
        filtered_bulls
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
