
library(data.table)
library(fairml)

# compas data -----------------------------------------------------------
data_compas <- function(data, job) {
  data(compas)
  dt <- data.table(compas)
  dt[, race := as.numeric(race)]

  # Protected attribute (sex)
  dt[, protected  := 1*(sex  == "Female")]
  dt[, sex := NULL]

  # Outcome
  y <- 1*(dt$two_year_recid  == "Yes")
  dt[, two_year_recid := NULL]

  # Predictors
  x <- as.matrix(dt)

  # Temp
  x <- x[, c(1:4, 15)]

  list(x = x,
       y = y)
}

# credit data -----------------------------------------------------------
data_credit <- function(data, job) {
  data(german.credit)
  dt <- as.data.table(lapply(german.credit, as.numeric))

  # Protected attribute (Gender)
  dt[, protected := Gender - 1]
  dt[, Gender := NULL]

  # Outcome
  y <- dt$Credit_risk - 1
  dt[, Credit_risk := NULL]

  # Predictors
  x <- as.matrix(dt)

  # Temp
  x <- x[, c(1:4, 20)]

  list(x = x,
       y = y)
}

# adult data -----------------------------------------------------------
data_adult <- function(data, job) {
  data(adult)
  dt <- as.data.table(lapply(adult, as.numeric))

  # Protected attribute (sex)
  dt[, protected := sex - 1]
  dt[, sex := NULL]

  # Outcome
  y <- dt$income - 1
  dt[, income := NULL]

  # Predictors
  x <- as.matrix(dt)

  # Temp
  x <- x[, c(1:4, 13)]

  list(x = x,
       y = y)
}
