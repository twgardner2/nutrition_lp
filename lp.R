library(tidyverse)
library(readxl)
library(lpSolve)


# df <- readxl::read_excel('data/nutrition_lp.xlsx')
df <- readr::read_csv('data/nutrition_lp.csv')
# df <- read.csv('data/nutrition_lp.csv')
# dput(df)

parseInput  <- function(data) {
  
  f.con   <- matrix(NA, nrow=0, ncol=nrow(data)-4)
  f.rhs   <- numeric()
  f.dir   <- character()
  int.vec <- integer()
  
  int.vec <- data[-(1:4), 'in_integer_amounts']
  int.vec <- which(int.vec==1)
  
  # Build min constraints
  mins  <- unlist(data[-(1:4),'min'])
  min.cons <- diag(length(mins))
  f.con <- rbind(f.con, min.cons)
  f.rhs <- c(f.rhs, mins)
  f.dir <- c(f.dir, rep('>=', length(mins)))
  
  # Build max constraints
  maxes <-unlist(data[-(1:4),'max'])
  max.cons <- diag(length(maxes))
  f.con <- rbind(f.con, max.cons)
  f.rhs <- c(f.rhs, maxes)
  f.dir <- c(f.dir, rep('<=', length(mins)))

  
  for(col in names(data)[-(1:5)]) {
    
    for(row in c(1,3)) {
      
      if(!is.na(data[row, col])) {
        
        # Get coefficients for constraint
        cons    <- t(as.matrix(data[-(1:4), col]))
        class(cons)    <- 'numeric'
        f.con <- rbind(f.con, cons)
        
        # Get RHS
        f.rhs <- c(f.rhs, data[row, col])
        
        # Get direction
        f.dir <- c(f.dir, data[row+1, col])
        
      }
    }
  }
  
  return(list(f.con=f.con, f.rhs=f.rhs, f.dir=f.dir, int.vec=int.vec))
}


f.obj <- as.numeric(df$calories[-(1:4)])


parsedInput <- parseInput(df)


results <-lp(
  direction = "max",
  objective.in = f.obj,
  const.mat = parsedInput$f.con,
  const.dir = parsedInput$f.dir,
  const.rhs = parsedInput$f.rhs,
  int.vec = parsedInput$int.vec
)

results$solution



displaySolution <- function(df, results) {
  items <- df$item[-(1:4)]
  output <- ''
  
  for (i in 1:length(items)) {
    print(glue::glue('{items[i]}: {results$solution[i]}\n'))
  }
  
}
displaySolution(df, results)
