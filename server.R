server <- function(input, output, session) {
  
  # Reactive Values ------------------------------------------------------------
  rv <- ok.comma(reactiveValues)(
    data = initialDf,
    orig=NULL,
    lpSolution=NULL,
    solutionDf=NULL,
    solutionNutrDf=NULL,
    )
  
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
  
  # Inputs datatable -----------------------------------------------------------
  output$tableInputs <- renderDT({
  
    capAndRemoveUnderscore <- function(x) {
      x %>% str_replace_all("_", " ")  %>% str_to_title()
    }
    
    colnames <- names(rv$data)
    names(colnames) <- capAndRemoveUnderscore(colnames)
    
    # datatable(rv$data, editable=TRUE, colnames=colnames, rownames=FALSE)
    datatable(rv$data, editable=TRUE, colnames=colnames)
  })
  
  # Solution datatable -----------------------------------------------------------
  output$tableSolution <- renderDT({
    
    if( !is.null(rv$solutionDf) ) {
      df <- rv$solutionDf %>% filter(servings>0)
      # datatable(df, editable=FALSE, rownames=FALSE, options=list(paging=FALSE))
      datatable(
        df,
        editable=FALSE, 
        options=ok.comma(list)(
          paging=FALSE,
          autoWidth=TRUE,
          )
        )
    }
  })
  # Solution Nutrition datatable -----------------------------------------------
  output$tableSolutionNutrition <- renderDT({
    
    if( !is.null(rv$solutionNutrDf) ) {
      df <- rv$solutionNutrDf
      # datatable(df, editable=FALSE, rownames=FALSE, options=list(paging=FALSE))
      datatable(
        df,
        editable=FALSE, 
        options=ok.comma(list)(
          paging=FALSE,
          autoWidth=TRUE,
          )
        )
    }
  })
  
  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })
  
  observeEvent(input$removecolumn, {
    rv$data <- removecolumn(rv$data,input$selectcolumn)
  })
  
  # Undo -----------------------------------------------------------------------
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })
  
  # Cell edit ------------------------------------------------------------------
  observeEvent(input$tableInputs_cell_edit, {
    row <- input$tableInputs_cell_edit$row
    col <- input$tableInputs_cell_edit$col
    print(glue::glue('row: {row}, col: {col}, value: {input$tableInputs_cell_edit$value}'))
    
    rv$data[row,col] <- input$tableInputs_cell_edit$value
  })
  
  # Run LP ---------------------------------------------------------------------
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
    
    rv$solutionDf <- data.frame(
      item = rv$data$item[-(1:4)],
      servings = round(rv$lpSolution$solution,1)
    )
    
    rv$solutionNutrDf <- data.frame(
      category = names(rv$data)[-(1:5)]
    ) %>%
    mutate(val = map_dbl(category, getNutrTotal, rv$data, rv$solutionDf$servings))
    
  })
  
}