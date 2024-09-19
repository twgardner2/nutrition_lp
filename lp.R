library(tidyverse)
library(readxl)
library(lpSolve)


# df <- readxl::read_excel('data/nutrition_lp.xlsx')
df <- readr::read_csv("data/nutrition_lp.csv")
# df <- read.csv('data/nutrition_lp.csv')
# dput(df)

parseInput <- function(data) {
  browser()
  con <- matrix(NA, nrow = 0, ncol = nrow(data) - 4)
  rhs <- numeric()
  dir <- character()
  int_vec <- integer()

  int_vec <- data[-(1:4), "in_integer_amounts"]
  int_vec <- which(int_vec == 1)

  # Build min constraints
  mins <- unlist(data[-(1:4), "min"])
  min.cons <- diag(length(mins))
  con <- rbind(con, min.cons)
  rhs <- c(rhs, mins)
  dir <- c(dir, rep(">=", length(mins)))

  # Build max constraints
  maxes <- unlist(data[-(1:4), "max"])
  max.cons <- diag(length(maxes))
  con <- rbind(con, max.cons)
  rhs <- c(rhs, maxes)
  dir <- c(dir, rep("<=", length(mins)))


  for (col in names(data)[-(1:5)]) {
    for (row in c(1, 3)) {
      if (!is.na(data[row, col])) {
        # Get coefficients for constraint
        cons <- t(as.matrix(data[-(1:4), col]))
        class(cons) <- "numeric"
        con <- rbind(con, cons)

        # Get RHS
        rhs <- c(rhs, data[row, col])

        # Get direction
        dir <- c(dir, data[row + 1, col])
      }
    }
  }

  return(list(con = con, rhs = rhs, dir = dir, int_vec = int_vec))
}


f.obj <- as.numeric(df$calories[-(1:4)])


parsedInput <- parseInput(df)


results <- lp(
  direction = "max",
  objective.in = f.obj,
  const.mat = parsedInput$con,
  const.dir = parsedInput$dir,
  const.rhs = parsedInput$rhs,
  int.vec = parsedInput$int_vec
)

results$solution



displaySolution <- function(df, results) {
  items <- df$item[-(1:4)]
  output <- ""

  for (i in 1:length(items)) {
    print(glue::glue("{items[i]}: {results$solution[i]}\n"))
  }
}
displaySolution(df, results)
