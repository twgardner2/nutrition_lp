parseInput <- function(data) {
  con <- matrix(NA, nrow = 0, ncol = nrow(data) - 4)
  rhs <- numeric()
  dir <- character()
  int_vec <- integer()

  int_vec <- which(data[-(1:4), "in_integer_amounts"]==1)

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

  # browser()
  
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
  # browser()
  return(list(con = con, rhs = rhs, dir = dir, int_vec = int_vec))
}

# ok_comma allows for a hanging comma in the arguments of a function
ok_comma <- function(FUN) {
  function(...) {
    arg.list <- as.list(sys.call())[-1L]
    len <- length(arg.list)
    if (len > 1L) {
      last <- arg.list[[len]]
      if (missing(last)) {
        arg.list <- arg.list[-len]
      }
    }
    do.call(FUN, arg.list)
  }
}

getNutrTotal <- function(col, input, amounts) {
  input <- input[-(1:4), -(1:5)]
  coeff <- input[[col]]
  coeff[is.na(coeff)] <- 0
  # browser(expr={col=='potassium'})
  return(sum(as.double(coeff) * amounts))
}

initialDf <- structure(list(item = c(
  "constraint1", "direction1", "constraint2",
  "direction2", "Grape Nuts", "Kind Breakfast Protein Bars - Dark Chocolate Cocoa",
  "Protein Shake", "Chipolte Chicken", "Chili", "Egg", "Dahi",
  "Blueberries", "SlowMg", "Sambucus Immune Gummy", "KS Roasted Pistachios",
  "Apple", "Carrots", "Belvita", "Peanuts", "Peanut Butter", "Everything Bagel Cod Fillet",
  "Chicken Sausages", "Salmon Burger", "Dave’s Killer Bread",
  "Just Bare Lightly Breaded Chicken Strips", "Broccoli", "Spinach",
  "Pure Protein Bar"
), min = c(
  NA, NA, NA, NA, 0L, 0L, 0L, 0L,
  0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
  0L, 0L, 0L, 0L
), max = c(
  NA, NA, NA, NA, 2L, 1L, 1L, 1L, 1L,
  10L, 2L, 5L, 2L, 2L, 2L, 2L, 3L, 4L, 2L, 2L, 4L, 6L, 3L, 2L,
  2L, 5L, 5L, 1L
), in_integer_amounts = c(
  NA, NA, NA, NA, 0L, 0L,
  1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L,
  1L, 1L, 0L, 0L, 0L, 1L
), serving_size = c(
  "", "", "", "", "58",
  "50", "1", "1", "1", "1", "227", "148", "1", "1", "28", "1 medium",
  "85 g", "1 cracker", "30", "2 Tbsp", "1 piece", "1 link", "1 patty",
  "1 slice", "84 g", "85 g", "85 g", "1 bar"
), calories = c(
  "2400",
  "<=", "", "", "200", "210", "170", "377", "290", "70", "170",
  "80", "0", "10", "170", "80", "35", "57.5", "170", "190", "160",
  "30", "170", "110", "160", "25", "20", "200"
), total_fat = c(
  "200",
  "<=", "", "", "1", "8", "3", "14", "10", "5", "8", "0.5", "0",
  "0", "14", "0", "0", "2", "15", "16", "7", "1.5", "9", "1.5",
  "6", "0", "0", "7"
), sat_fat = c(
  NA, NA, NA, NA, 0, 1.5, 1, 3,
  3, 2, 4.5, 0, 0, 0, 1.5, 0, 0, 0.125, 2, 3.5, 0.5, 0.5, 1, 0,
  1, 0, 0, 3
), trans_fat = c(
  NA, NA, NA, NA, 0L, 0L, 0L, 0L, NA,
  0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
  0L, 0L, 0L
), punsat_fat = c(
  NA, NA, NA, NA, 0.5, 1.5, 1, 0, NA,
  1, NA, NA, 0, 0, NA, 0, 0, 0, NA, NA, NA, NA, NA, 1, 3, 0, 0,
  NA
), munsat_fat = c(
  NA, NA, NA, NA, 0, 5, 1, 0, NA, 2, NA, NA,
  0, 0, NA, 0, 0, 0, NA, NA, NA, NA, NA, 0, 1.5, 0, 0, NA
), chol = c(
  NA,
  NA, NA, NA, 0L, 0L, 30L, 69L, 43L, 186L, 35L, 0L, 0L, 0L, 0L,
  0L, 0L, 0L, 0L, 0L, 10L, 20L, 60L, 0L, 50L, 0L, 0L, 10L
), sodium = c(
  "2300",
  "<=", "", "", "280", "75", "300", "65", "200", "71", "135", "0",
  "0", "7.5", "160", "0", "65", "50", "120", "80", "330", "120",
  "330", "170", "540", "25", "65", "200"
), total_carb = c(
  "400",
  "<=", "", "", "47", "28", "", "44", "25", "0.5", "18", "", "0",
  "1.5", "8", "22", "8", "9", "6", "8", "15", "0", "2", "22", "9",
  "4", "3", "17"
), total_fiber = c(
  "35", ">=", "", "", "7", "3",
  "3", "1", "5", "0", "0", "4", "0", "0", "3", "3", "2", "0.5",
  "3", "3", "1", "0", "0", "5", "0", "3", "2", "1"
), sol_fib = c(
  NA,
  NA, NA, NA, 1, 1.5, NA, NA, NA, 0, 0, NA, 0, 0, NA, NA, NA, 0,
  NA, NA, NA, 0, 0, NA, 0, NA, NA, NA
), unsol_fib = c(
  NA, NA, NA,
  NA, 6, 1.5, NA, NA, NA, 0, 0, NA, 0, 0, NA, NA, NA, 0, NA, NA,
  NA, 0, 0, NA, 0, NA, NA, NA
), sugar = c(
  NA, NA, NA, NA, 5, 10,
  1.5, 0, 10, 0, 17, 15, 0, 1.5, 2, 16, 5, 3, 0, 4, 1, 0, 0, 5,
  2, 1, 0, 2
), added_sugar = c(
  NA, NA, NA, NA, 0, 9, 0, 0, 0, 0,
  0, 0, 0, 1.5, 0, 0, 0, 2.75, 0, 2, 1, 0, 0, 5, 1, 0, 0, 1
), protein = c(
  "180",
  ">=", "300", "<=", "6", "8", "30", "20", "26", "6", "11", "1",
  "0", "0", "6", "0", "1", "0.75", "8", "7", "9", "3", "20", "5",
  "16", "3", "2", "20"
), potassium = c(
  NA, NA, NA, NA, 260L, 140L,
  300L, 127L, 374L, 69L, 550L, 110L, 0L, 0L, 285L, 186L, NA, 0L,
  192L, 192L, 140L, NA, 390L, 100L, 300L, 282L, 474L, 150L
), magnesium = c(
  NA,
  NA, NA, NA, 42, NA, NA, NA, NA, NA, NA, NA, 71.5, 0, 31, 9, NA,
  0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA
), calcium = c(
  NA, NA,
  NA, NA, 20L, NA, NA, NA, NA, NA, NA, NA, 119L, 0L, 30L, 0L, 20L,
  0L, 27L, 18L, 20L, NA, 20L, 0L, 10L, 52L, 84L, 110L
), chloride = c(
  NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 208L, 0L, NA, 0L,
  NA, 0L, 0L, 0L, NA, NA, NA, NA, NA, NA, NA, NA
), zinc = c(
  NA,
  NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, NA, 0, 3.75, 1, 0, NA,
  0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA
), iron = c(
  NA, NA, NA,
  NA, NA, NA, 4, NA, NA, NA, NA, NA, 0, 0, 1, 0, 0.36, 0.45, 0.3,
  0.5, 0.5, 0.72, 0.7, 1, NA, 0.7, 2, 0.7
)), class = "data.frame", row.names = c(
  NA,
  -28L
))
initialDf[initialDf == ""] <- NA
