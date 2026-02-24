library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(glue)

source('ac_wrangling.R')

# # ---- Updated analysis function: writes Excel into Results/ ----
# generate_notifications_list <- function(feedback_file, feedback_sheet, penji_file, start_date, outfile) {
#   # Placeholder logic — replace with your real analysis
#   df <- data.frame(
#     feedback_file = feedback_file,
#     Sheet = feedback_sheet,
#     penji_file = penji_file,
#     StartDate = as.character(start_date),
#     Status = "Analysis complete (dummy)"
#   )
#   
#   wb <- createWorkbook()
#   addWorksheet(wb, "Notifications")
#   writeData(wb, "Notifications", df)
#   saveWorkbook(wb, outfile, overwrite = TRUE)
#   
#   return(outfile)
# }

ui <- fluidPage(
  titlePanel("Academic Success Data Merging File Loader"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("feedback_file", "Choose Faculty Feedback file (Excel)", 
                accept = c(".xlsx", ".xls")),
      
      uiOutput("sheet_ui"),
      
      fileInput("penji_file", "Choose Penji File (CSV)", 
                accept = c(".csv")),
      
      dateInput("start_date", "Start date:", value = Sys.Date()),
      
      actionButton("run_btn", "Generate notifications"),
      
      uiOutput("download_ui")   # appears only after file is generated
    ),
    
    mainPanel(
      verbatimTextOutput("sheet_selected"),
      verbatimTextOutput("date_selected"),
      verbatimTextOutput("analysis_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive: list sheet names after File 1 is uploaded
  sheets_feedback_file <- reactive({
    req(input$feedback_file)
    excel_sheets(input$feedback_file$datapath)
  })
  
  output$sheet_ui <- renderUI({
    req(sheets_feedback_file())
    selectInput("feedback_sheet", "Select a sheet from File 1:",
                choices = sheets_feedback_file())
  })
  
  output$sheet_selected <- renderPrint({
    req(input$feedback_sheet)
    paste("You selected sheet:", input$feedback_sheet)
  })
  
  output$date_selected <- renderPrint({
    req(input$start_date)
    paste("Start date:", input$start_date)
  })
  
  # Store generated Excel file path
  generated_file <- reactiveVal(NULL)
  
  observeEvent(input$run_btn, {
    req(input$feedback_file, input$feedback_sheet, input$penji_file, input$start_date)
    
    # Ensure Results/ folder exists
    if (!dir.exists("Results")) {
      dir.create("Results")
    }
    
    # Timestamped filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    outfile <- file.path("Results", paste0("Notifications_", timestamp, ".xlsx"))
    
    result_file <- generate_notifications_list(
      feedback_file = input$feedback_file$datapath,
      feedback_sheet = input$feedback_sheet,
      penji_file = input$penji_file$datapath,
      start_date = input$start_date,
      outfile = outfile
    )
    
    generated_file(result_file)
    
    output$analysis_output <- renderPrint({
      paste("Notifications list generated at:", result_file)
    })
  })
  
  # Show download button only after file exists
  output$download_ui <- renderUI({
    req(generated_file())
    downloadButton("download_notifications", "Download Notifications Excel")
  })
  
  # Download handler
  output$download_notifications <- downloadHandler(
    filename = function() {
      basename(generated_file())
    },
    content = function(file) {
      file.copy(generated_file(), file)
    }
  )
}

shinyApp(ui, server)
