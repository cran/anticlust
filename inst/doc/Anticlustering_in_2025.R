## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(1)


## ----setup--------------------------------------------------------------------
library(anticlust)

## -----------------------------------------------------------------------------
library(MASS)
data(survey)       # load data set
nrow(survey)       # number of students
head(survey, n=10) # look at the data
sapply(survey, anyNA) # most variables contain missing values

## -----------------------------------------------------------------------------

features <- c("Sex", "W.Hnd", "Exer", "Smoke", "Pulse", "Height", "Age")

survey$House <- anticlustering(
  survey[, features], 
  K = 3
)


## -----------------------------------------------------------------------------
library(tableone)

CreateTableOne(features, strata = "House", data = survey)


## -----------------------------------------------------------------------------
survey$Rnd_House <- sample(survey$House)

CreateTableOne(features, strata = "Rnd_House", data = survey)


## -----------------------------------------------------------------------------
survey$House2 <- anticlustering(
  survey[, features], 
  K = 3,
  standardize = TRUE
)

CreateTableOne(features, strata = "House2", data = survey)


## -----------------------------------------------------------------------------
survey$House3 <- anticlustering(
  survey[, features], 
  K = 3,
  standardize = TRUE,
  method = "3phase"
)
CreateTableOne(features, strata = "House3", data = survey)

## -----------------------------------------------------------------------------
survey$House4 <- anticlustering(
  survey[, features], 
  K = 3,
  standardize = TRUE,
  method = "3phase",
  objective = "variance"
)
CreateTableOne(features, strata = "House4", data = survey)


## -----------------------------------------------------------------------------

colors <- c("#a9a9a9", "#df536b", "#61d04f")

ord <- order(survey$Pulse)
# Plot the data while visualizing the different clusters
plot(
  survey$Pulse[ord], 
  col = colors[survey$House4[ord]], 
  pch = 19, 
  ylab = "Pulse", 
  xlab = "Students (ordered by pulse)"
)
legend("bottomright", legend = paste("Group", 1:3), col = colors, pch = 19)


## -----------------------------------------------------------------------------
survey$House5 <- anticlustering(
  survey[, features], 
  K = 3,
  method = "3phase",
  objective = "kplus",
  standardize = TRUE
)
CreateTableOne(features, strata = "House5", data = survey)


## -----------------------------------------------------------------------------
survey$House6 <- anticlustering(
  survey[, features], 
  K = c(137, 50, 50),
  standardize = TRUE,
  method = "3phase"
)
CreateTableOne(features, strata = "House6", data = survey)

## -----------------------------------------------------------------------------
survey$House7 <- anticlustering(
  survey[, features], 
  K = c(137, 50, 50),
  standardize = TRUE,
  method = "local-maximum",
  repetitions = 10, # increasing repetitions may be helpful with method = "local-maximum"
  objective = "average-diversity"
)
CreateTableOne(features, strata = "House7", data = survey)

## -----------------------------------------------------------------------------
survey$House8 <- anticlustering(
  survey[, features], 
  K = c(137, 50, 50),
  standardize = TRUE,
  method = "local-maximum",
  repetitions = 10, 
  objective = "kplus"
)
CreateTableOne(features, strata = "House8", data = survey)

## -----------------------------------------------------------------------------
hist(survey$Age)
sort(survey$Age, decreasing = TRUE)[1:10]

## -----------------------------------------------------------------------------
survey$is_age_outlier <- factor(survey$Age > 70)
survey$House9 <- anticlustering(
  survey[, features], 
  K = c(137, 50, 50),
  standardize = TRUE,
  method = "local-maximum",
  repetitions = 10, 
  objective = "kplus",
  categories = survey$is_age_outlier
)
CreateTableOne(features, strata = "House9", data = survey)

## -----------------------------------------------------------------------------
table(survey$is_age_outlier, survey$House9) # new assignment using `categories` argument
table(survey$is_age_outlier, survey$House8) # old assignment not using `categories` argument

