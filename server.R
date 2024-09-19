#' @import shiny
#' @import DT
#' @import dplyr
#' @import stringr
#' @include lib.R

server <- function(input, output, session) {
  # Reactive Values ------------------------------------------------------------
  rv <- ok_comma(reactiveValues)(
    data = initialDf,
    orig = NULL,
    lpSolution = NULL,
    solutionDf = NULL,
    solutionNutrDf = NULL,
  )

  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)

    validate(need(ext == "csv", "Please upload a csv file"))

    rv$orig <- read.csv(file$datapath, header = input$header, )
    rv$data <- rv$orig
  })

  output$selectUI <- renderUI({
    req(rv$data)
    selectInput(inputId = "selectcolumn", label = "select column", choices = names(rv$data))
  })


  observeEvent(input$Splitcolumn, {
    rv$data <- splitColumn(rv$data, input$selectcolumn)
  })

  observeEvent(input$deleteRows, {
    if (!is.null(input$table1_rows_selected)) {
      rv$data <- rv$data[-as.numeric(input$table1_rows_selected), ]
    }
  })

  # Inputs datatable -----------------------------------------------------------
  output$tableInputs <- renderDT({
    capAndRemoveUnderscore <- function(x) {
      x %>%
        str_replace_all("_", " ") %>%
        str_to_title()
    }

    colnames <- names(rv$data)
    names(colnames) <- capAndRemoveUnderscore(colnames)

    # Convert all columns to character type
    data_as_char <- rv$data %>% mutate(across(everything(), as.character))

    datatable(
      data_as_char,
      editable = TRUE,
      colnames = colnames,
      options = list(
        paging = FALSE,
        scrollY = "60vh", # 0.6 of the viewport height
        scrollX = "80vw", # 0.8 of the viewport width
        scrollCollapse = TRUE
      )
    )
  })

  # Solution datatable -----------------------------------------------------------
  output$tableSolution <- renderDT({
    if (!is.null(rv$solutionDf)) {
      df <- rv$solutionDf %>% filter(servings > 0)
      # datatable(df, editable=FALSE, rownames=FALSE, options=list(paging=FALSE))
      datatable(
        df,
        editable = FALSE,
        options = ok_comma(list)(
          paging = FALSE,
          autoWidth = TRUE,
        )
      )
    }
  })
  # Solution Nutrition datatable -----------------------------------------------
  output$tableSolutionNutrition <- renderDT({
    if (!is.null(rv$solutionNutrDf)) {
      df <- rv$solutionNutrDf
      # datatable(df, editable=FALSE, rownames=FALSE, options=list(paging=FALSE))
      datatable(
        df,
        editable = FALSE,
        options = ok_comma(list)(
          paging = FALSE,
          autoWidth = TRUE,
          pageLength = nrow(df)
        )
      )
    }
  })

  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })

  observeEvent(input$removecolumn, {
    rv$data <- removecolumn(rv$data, input$selectcolumn)
  })

  # Undo -----------------------------------------------------------------------
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })

  # Cell edit ------------------------------------------------------------------
  observeEvent(input$tableInputs_cell_edit, {
    row <- input$tableInputs_cell_edit$row
    col <- input$tableInputs_cell_edit$col
    print(glue::glue("row: {row}, col: {col}, value: {input$tableInputs_cell_edit$value}"))

    rv$data[row, col] <- input$tableInputs_cell_edit$value
  })

  # Run LP ---------------------------------------------------------------------
  observeEvent(input$runLp, {
    # browser()
    f.obj <- as.numeric(rv$data$calories[-(1:4)])

    parsedInput <- parseInput(rv$data)

    # browser()
    results <- lp(
      direction = "max",
      objective.in = f.obj,
      const.mat = parsedInput$con,
      const.dir = parsedInput$dir,
      const.rhs = parsedInput$rhs,
      int.vec = parsedInput$int_vec
    )

    rv$lpSolution <- results

    rv$solutionDf <- data.frame(
      item = rv$data$item[-(1:4)],
      servings = round(rv$lpSolution$solution, 1)
    )

    rv$solutionNutrDf <- data.frame(
      category = names(rv$data)[-(1:5)]
    ) %>%
      mutate(val = map_dbl(category, getNutrTotal, rv$data, rv$solutionDf$servings))
  })
}
