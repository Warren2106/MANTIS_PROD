# Header ----
# Eli Lilly and Company (required)-   SDnA
# CODE NAME (required)              : ODM Parser From VDV (https://github.com/EliLillyCo/LRL_ODMParserFromVDV_App/blob/main/app.R)
# PROJECT NAME (required)           : ODM Parser From VDV
# DESCRIPTION (required)            : Reads VDV Codelist Items Compare Tool output coversheet and processes corresponding archived DED xml file. Tool to aid STATS with DED standards from VDV Codelist Items Compare Tool Output run by Study Build Team. 
# SPECIFICATIONS(required)          : N/A
# VALIDATION TYPE (required)        : N/A
# INDEPENDENT REPLICATION (required): N/A
# ORIGINAL CODE (required)          : N/A, this is the original code
# COMPONENT CODE MODULES            : N/A
# SOFTWARE/VERSION# (required)      : R version 4.0.3
# INFRASTRUCTURE                    : Development Platform: R Studio Server (https://rstudio.am.lilly.com/)
#                                   : Running under: R Shiny Prod Server
# DATA INPUT                        : Custom spreadsheet (optional)
# OUTPUT                            : Tables of parsed DED fields
# SPECIAL INSTRUCTIONS              : Instructions generated within Shiny UI
# _______________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________
#   DOCUMENTATION AND REVISION HISTORY SECTION (required):
#   
#   Author &
#   Ver# Validator          Code History Description
# ____ ________________     _____________________________________________________________________________________________________
# 1.0  Sam Parmar   (Author)     
#      Ben Nealy (Validators) 
#      Mudit (Publish to R connect)

#Load libraries ----
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(XML)
library(data.table)
library(readxl)
library(DT)
library(shinyjs)
library(openxlsx)
library(glue)
library(tidyr)
library(datamods)
library(shinyBS)
library(shinyalert)
library(praise)
# Expand memory size limit ----
options(shiny.maxRequestSize=1000*1024^2, shiny.trace = TRUE)

# Define UI for app ---
ui <- fluidPage(
  # Team title ----
  h3("Study Build Team"),
  # App title ----
  titlePanel("VDV ODM Parser"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      #ensure using shinyjs for shiny app ---
      shinyjs::useShinyjs(),
      # Set up shinyalert ----
      useShinyalert(),  
      tags$b("Step 1. Import VDV Codelists Coversheet"),
      tags$br(), 
      shinyBS::bsButton("launch_modal", 
                        "Upload here",
                        icon("upload"), 
                        size = "default"),
      shinyjs::hidden(p(id = "upload_text1", "*Coversheet uploaded.")),
      tags$p("",style = "margin-bottom: 15px;"),
      tags$b("Step 2. Run ODM Parser"),
      tags$br(), 
      shinyBS::bsButton("runParser", 
                        "Click here", 
                        icon("running"),
                        size = "default"),
      shinyjs::hidden(p(id = "text1", "*Processing... Please wait.")),
      tags$p("", style = "margin-bottom: 15px;"),  
      tags$b("Step 3. Download parsed data from DED ODM xml reference file (all tabs)"),
      tags$br(), 
      downloadButton("downloadButton1", 
                     "Download here"),
      tags$p("", style = "margin-bottom: 15px;"), 
      tags$b("Step 4 (Optional). Select which data sheet you would like to view."),
      selectInput("dropdown_sheet1", "", selected = "Cover_Page", choices = c("Cover_Page", "ODM_FORM_IG", "ODM_IG_Item", "Codelist_ODM", 
                                                      "Codelist_SVA", "Codelist_Item_DT", "ODM_Item_Details")),
      tags$b("Note: Wait a few minutes for processing during Step 2. 
             This app is based on output from VDV Codelist Items Compare Tool Macro. 
             App designed for use with Microsoft Edge browser.
             Results automatically archived here: 
             /lrlhps/data/study_build_team/custom-odm-parsing/
             "),
      tags$div(h5("Developed by Statistics, Data & Analytics (SDnA)"), 
               style="text-align: center")
      ),
  mainPanel(
    # verbatimTextOutput(outputId = "name"),
    DT::dataTableOutput(outputId = "data")
    )
  )
)

# Define server logic for app ---
server <- function(input, output, session) {
  
  #welcome alert ---- 
  shinyalert("Welcome to the VDV ODM Parser App", glue("This app is intended to process the ODM DED files 
                                                       based on the coversheet obtained from the ODM Codelist Items Compare Shiny App. 
                                                       
                                                        Here's a compliment. {praise()}"), 
             type = "info")
  
  shinyjs::disable("downloadButton1")
  
  # create df with imported coversheet
  coversheet_df <- reactiveVal(NULL)
  
  observeEvent(input$launch_modal, {
    datamods::import_modal(
      id = "myid",
      from = c("file", "copypaste"),
      title = "Import data for VDV ODM Parser"
    )
    coversheet_df(datamods::import_server("myid", return_class = "tbl_df"))
  })
  
  # show uploaded text ----
  observe({
    req( !is.null(coversheet_df()) )
    req( coversheet_df()$data(), coversheet_df()$name())
    shinyjs::show("upload_text1")
    print(coversheet_df()$data())
    })
  
  # wait for user to indicate run process ----
  list_of_datasets_react <- reactive({
    req(input$runParser)
    #validate/need message to ensure uploaded proper format reference file ----
    validate(
      need(input$launch_modal, 
           "Please ensure that VDV Codelists Coversheet is uploaded in Step 1.")
    )
    req(coversheet_df())
    validate(
      need( !is.null(coversheet_df()) & nrow(coversheet_df()$data()) > 1, 
           "Please ensure that VDV Codelists Coversheet is uploaded successfully in Step 1.")
    )
    validate(
      need( coversheet_df()$data() %>% colnames() %>% 
              str_detect("Value") %>% any(), 
            "Please ensure that the VDV Codelists Coversheet is coversheet is uploaded that has Measure and Value columns.")
    )
    
    # define odm ded import path ----
    odm_ded_name <- coversheet_df()$data() %>% 
      filter(Value == "DED_File_Name") %>% 
      select(Measure) %>%
      pull() %>% 
      #str_replace("\\.xml$", "") %>%  # Remove any existing .xml extensions if present
      str_c(".xlsx")                 # Append .xlsx
    
    odm_ded_path <- str_c("/lrlhps/data/study_build_team/odm_ded_files/", 
                          odm_ded_name)
    print(odm_ded_name)
    print(odm_ded_path)
    validate(
      need(file.exists(odm_ded_path) , 
           glue::glue("Ensure that the ODM DED xlsx file is here: {odm_ded_path}")
             )
    )
    
    # running alert ---- 
    shinyalert("Running now", "The application is running now. Wait a few min.", type = "success")
    
    # disable run button after first click
    shinyjs::disable("runParser")
    shinyjs::show("text1")
    
    # run parsing script on data  ---- 
    source("parser_Xlsx.R", local = TRUE)
    
    # confirm getting StudyName 
    print(StudyName)
    
    # reenable run button after run completion 
    shinyjs::enable("runParser")
    shinyjs::hide("text1")
    
    shinyjs::enable("downloadButton1")
    
    # success alert ---- 
    shinyalert("Congrats!", glue("{praise()} The app just completed running. Inspect the output and download it as needed. "), type = "success")
    
    #return list of datasets generated via parser.R
    list_of_datasets

  })
  
  # Reactive for Study Name from uploaded input coversheet ----
  StudyName_react <- reactive({
      coversheet_df()$data() %>% 
      filter(Value == "Study_Name") %>% 
      select(Measure) %>%
      pull() 
    })
  
  # Output DT table ----
  output$data <- DT::renderDT({
    req(list_of_datasets_react())
    DT::datatable( list_of_datasets_react()[[input$dropdown_sheet1]] ) }, 
    server = FALSE )

    # specify download handler for output ----  
  output$downloadButton1 <- downloadHandler(
    filename = function(){
      paste0("ODM-DED-PROC-", StudyName_react(), "_", today(), ".xlsx")
    },
    content = function(con){
      openxlsx::write.xlsx(list_of_datasets_react(), file = con)
    }
  )
  
}

# Run the app ----
shinyApp(ui, server)