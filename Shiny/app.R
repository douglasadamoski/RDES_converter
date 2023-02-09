# Libraries
library(shiny)
library(readxl)

# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Upload your qPCR equipment full export"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select equipment ----
      selectInput(inputId = "qpcr_equipment",
                  label = "Select your qPCR equipment export:",
                  choices = c(
                    "",
                    "Applied Biosystems QuantStudio™ 5 System" = "applied_quantstudio5_v1",
                    "To be defined" = "other"
                  )
      ),
      # Horizontal line ----
      tags$hr(),
      # Input: Select a file ----
      fileInput("file1", "Choose exported file File",
                multiple = FALSE,
                accept = c(".xlsx",
                           ".xls",
                           ".csv")),
      # Horizontal line ----
      tags$hr()
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Text with equipment name ----
      # Button
      uiOutput("downloadData"),
      # Output: Data file ----
      tableOutput("contents")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  # Reactive collect file
  reactive_df <- reactive({
  # Require selections
  req(input$file1)
  req(input$qpcr_equipment)

  ##### Applied Biosystems QuantStudio™ 5 System #####
  ##### applied_quantstudio5_v1 
  if(input$qpcr_equipment == "applied_quantstudio5_v1"){
    
    #
    AmplificationData <- read_excel(#path="2022-09-23_120054.xls",
                     path=input$file1$datapath,
                     sheet = "Amplification Data",
                     range = NULL, col_names = TRUE, col_types = NULL,
                     na = "", trim_ws = TRUE,
                     skip = 0, n_max = Inf,
                     #guess_max = min(1000, n_max),
                     #progress = readxl_progress(),
                     .name_repair = "unique" 
    )
    
    #
    AmplificationData <- AmplificationData[grep("Well", AmplificationData$`Block Type`):dim(AmplificationData)[1],]
    colnames(AmplificationData) <- AmplificationData[1,]
    AmplificationData <- AmplificationData[-1,]

    # Create a dataframe
    x <- c("Well", "Sample", "Sample Type", "Target", "Target Type", "Dye", unique(AmplificationData$Cycle) )
    df <- data.frame(matrix(ncol = length(x), nrow = 0))
    colnames(df) <- x
    
    #
    i <- 0
    for(targetNow in unique( AmplificationData$`Target Name`) ){
     for(wellNow in unique(AmplificationData$`Well Position`) ){
      # select lines
       possibleLines <- which(
         AmplificationData$`Target Name` == targetNow &
           AmplificationData$`Well Position` == wellNow
       )
       #
       if(length(possibleLines) > 0){
         i <- i+1
         df[i, "Well"] <- wellNow
         df[i, "Target"] <- targetNow
         df[i, unique(AmplificationData$Cycle)] <- as.numeric( ( as.data.frame(AmplificationData)[possibleLines, "Rn"])   )
       }
     }
   }
    

    # Add other column information from Sample Setup
    #
    SampleSetupData <- read_excel(#path="2022-09-23_120054.xls",
                                    path=input$file1$datapath,
                                    sheet = "Sample Setup",
                                    range = NULL, col_names = TRUE, col_types = NULL,
                                    na = "", trim_ws = TRUE,
                                    skip = 0, n_max = Inf,
                                    #guess_max = min(1000, n_max),
                                    #progress = readxl_progress(),
                                    .name_repair = "unique" 
    )
    
    #
    as.data.frame(SampleSetupData) -> SampleSetupData
    SampleSetupData <- SampleSetupData[grep("Well", SampleSetupData$`Block Type`):dim(SampleSetupData)[1],]
    colnames(SampleSetupData) <- SampleSetupData[1,]
    SampleSetupData <- SampleSetupData[-1,]
    
    # Loop to fill
    for(i in 1:dim(df)[1]){
      
      #
      df[i, "Sample"] <-       SampleSetupData[which(SampleSetupData$`Well Position` == df[i, "Well"] &
                                                       SampleSetupData$`Target Name` == df[i, "Target"]), "Sample Name"]
      
      df[i, "Sample Type"]    <-  SampleSetupData[which(SampleSetupData$`Well Position` == df[i, "Well"] &
                                                         SampleSetupData$`Target Name` == df[i, "Target"]), "Task"]
      
      df[i, "Target Type"]   <-  ""
      
      df[i, "Dye"]   <-  SampleSetupData[which(SampleSetupData$`Well Position` == df[i, "Well"] &
                                                 SampleSetupData$`Target Name` == df[i, "Target"]), "Reporter"]

    }

    #
    remove(SampleSetupData)
    remove(AmplificationData)
    
    # return
    return(df)
  }
})
  
  
  # Create download button
  output$downloadData <- renderUI({
    req(reactive_df())
    downloadButton("downloadData01")
  })
  # Create donwload data
  output$downloadData01 <- downloadHandler(
    filename = function() {
      paste("RDES_v1_0_", input$file1, ".tsv", sep = "") #input$file1$datapath
    },
    content = function(file) {
      write.table(reactive_df(),
                  file,
                  row.names = FALSE,
                  quote=FALSE,
                  sep="\t")
    }
  )
  
  #
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    req(input$qpcr_equipment)
    #
    return( reactive_df() )

  })
}

# Create Shiny app ----
shinyApp(ui, server)



