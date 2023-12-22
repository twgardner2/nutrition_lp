server <- function(input, output, session) {
  
  rv <- reactiveValues(data = initialDf, orig=NULL, lpSolution=NULL, outputDf=NULL)
  
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    validate(need(ext == "csv", "Please upload a csv file"))
    
    rv$orig <- read.csv(file$datapath, header = input$header, )
    rv$data <- rv$orig
  })
  
  output$selectUI<-renderUI({
    req(rv$data)
    selectInput(inputId='selectcolumn', label='select column', choices = names(rv$data))
  })
  
  
  observeEvent(input$Splitcolumn, {
    rv$data <- splitColumn(rv$data, input$selectcolumn)
  })
  
  observeEvent(input$deleteRows,{
    if (!is.null(input$table1_rows_selected)) {
      rv$data <- rv$data[-as.numeric(input$table1_rows_selected),]
    }
  })
  
  output$tableInputs <- renderDT({
    datatable(rv$data, editable=TRUE)
  })
  
  output$tableSolution <- renderDT({
    #@if(rv$outputDf) {
      
      datatable(rv$outputDf, editable=FALSE, options=list(paging=FALSE))
    #}
  })
  
  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })
  
  observeEvent(input$removecolumn, {
    rv$data <- removecolumn(rv$data,input$selectcolumn)
  })
  
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })
  
  observeEvent(input$tableInputs_cell_edit, {
    row <- input$tableInputs_cell_edit$row
    col <- input$tableInputs_cell_edit$col
    rv$data[row,col] <- input$tableInputs_cell_edit$value
  })
  
  observeEvent(input$runLp, {
    
    f.obj <- as.numeric(rv$data$calories[-(1:4)])
    
    parsedInput <- parseInput(rv$data)
    
    # browser()
    results <-lp(
      direction = "max",
      objective.in = f.obj,
      const.mat = parsedInput$f.con,
      const.dir = parsedInput$f.dir,
      const.rhs = parsedInput$f.rhs,
      int.vec = parsedInput$int.vec
    )
    
    rv$lpSolution <- results
    
    rv$outputDf <- data.frame(
      item = rv$data$item[-(1:4)],
      servings = rv$lpSolution$solution
    )
    
  })
  
}