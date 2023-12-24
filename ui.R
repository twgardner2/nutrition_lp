ui <- shiny::fluidPage(
  sidebarLayout(
    sidebarPanel(
      width=1,
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      actionButton("Splitcolumn", "SplitColumn"),
      uiOutput("selectUI"),
      actionButton("deleteRows", "Delete Rows"),
      textInput("textbox", label="Input the value to replace:"),
      actionButton("replacevalues", label = 'Replace values'),
      actionButton("removecolumn", "Remove Column"),
      actionButton("Undo", 'Undo'),
      actionButton('runLp', 'Run LP'),
    ),
     mainPanel(
      DTOutput("tableInputs"),
      fluidRow(
        column(6, DTOutput("tableSolution")),
        column(6, DTOutput("tableSolutionNutrition")),
      )
    )
  )
)