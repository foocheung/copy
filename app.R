library(shiny)


server <- function(input, output, session) {
  # Reactive values to store available columns and breakdown of the first entered path
  available_columns <- reactiveValues(columns = NULL)
  first_path_folders <- reactiveValues(folders = NULL)
  
  # Observe event to update available columns and breakdown when file paths are entered
  observeEvent(input$enterPaths, {
    if (!is.null(input$paths)) {
      # Get the columns from the first entered file path
      file_structure <- unlist(strsplit(input$paths, "/"))
      available_columns$columns <- seq_along(file_structure)
      
      # Store the breakdown of the first entered path
      first_path_folders$folders <- file_structure
    }
  })
  
  # Dynamically update choices for selectInput based on available columns
  observe({
    updateSelectInput(session, "selectedColumn", choices = available_columns$columns)
  })
  
  # Read the list of file paths from the textarea
  paths <- reactive({
    req(input$copyButton)
    req(input$paths)
    return(strsplit(input$paths, "\n")[[1]])
  })
  
  # Get the substitution folders
  substitute_folder1 <- reactive({
    input$substituteFolder1
  })
  substitute_folder2 <- reactive({
    input$substituteFolder2
  })
  
  # Get the output folder name
  output_folder <- reactive({
    req(input$copyButton)
    input$outputFolder
  })
  
  # Create the output folder if it doesn't exist
  observe({
    dir.create(output_folder(), showWarnings = FALSE, recursive = TRUE)
  })
  
  # Copy and append files
  status_messages <- reactive({
    req(input$copyButton)
    req(paths())
    
    messages <- character(length(paths()))
    for (i in seq_along(paths())) {
      file <- trimws(paths()[i])
      if (file != "") {
        # Extract the current structure of the file path
        file_structure <- unlist(strsplit(file, "/"))
        
        # Substitute the top 2 folders in the file path
        if (length(file_structure) > 2) {
          file_structure[2] <- substitute_folder1()
          file_structure[3] <- substitute_folder2()
        }
        
        # Reconstruct the file path
        file <- paste(file_structure, collapse = "/")
        
        # Get the highest folder name
        highest_folder <- dirname(dirname(file))
        
        # Create a unique folder name based on the selected input column index for each sample
        selected_column_index <- as.numeric(input$selectedColumn)
        unique_folder_name <- file_structure[selected_column_index]
        
        # Create the new file name by appending the highest folder name and unique folder name
        new_file <- file.path(output_folder(), paste0(unique_folder_name, "_", basename(file)))
        
        # Copy the file to the output folder with the new name
        file.copy(file, new_file)
        
        # Create status message
        messages[i] <- paste("Copied:", file, "to", new_file)
      }
    }
    
    # Return status messages
    messages
  })
  
  output$statusOutput <- renderText({
    status_messages()
  })
  
  # Render UI for displaying the breakdown of each folder for the first entered path
  output$folderBreakdown <- renderUI({
    if (!is.null(first_path_folders$folders)) {
      fluidRow(
        column(4, h4("Folder Breakdown:")),
        column(8, renderTable({
          data.frame(Folder = seq_along(first_path_folders$folders), Value = first_path_folders$folders, stringsAsFactors = FALSE)
        }))
      )
    }
  })
}

# Define the Shiny app with a textarea for entering file paths
ui <- fluidPage(
  titlePanel("Copy and Append Files"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("substituteFolder1", "Substitute Folder 1:", "Volumes"),
      textInput("substituteFolder2", "Substitute Folder 2:", "CHI"),
      textInput("outputFolder", "Output Folder Name:", ""),
      selectInput("selectedColumn", "Select Column:", ""),
      textAreaInput("paths", "Enter file paths:", "", width = "100%", height = "300px"),
      actionButton("enterPaths", "Enter Paths"),
      actionButton("copyButton", "Copy and Append")
    ),
    
    mainPanel(
      uiOutput("folderBreakdown"),
      verbatimTextOutput("statusOutput")
    )
  )
)

# Run the Shiny app
shinyApp(ui, server)
