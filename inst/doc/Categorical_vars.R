## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(123)

## ----setup--------------------------------------------------------------------
library(anticlust)

## -----------------------------------------------------------------------------
data(penguins)
# First exclude cases with missing values
df <- na.omit(penguins)
head(df)
nrow(df)

## -----------------------------------------------------------------------------
numeric_vars <- df[, c("bill_len", "bill_dep", "flipper_len", "body_mass")]
groups <- anticlustering(
  numeric_vars, 
  K = 3,
  categories = df$sex
)

## -----------------------------------------------------------------------------
table(groups, df$sex)

## -----------------------------------------------------------------------------
groups <- anticlustering(
  numeric_vars, 
  K = 3,
  categories = df$species
)

table(groups, df$species)

## -----------------------------------------------------------------------------
groups <- anticlustering(
  numeric_vars, 
  K = 3,
  categories = df[, c("species", "sex")]
)

table(groups, df$sex)
table(groups, df$species) 

## -----------------------------------------------------------------------------
all_features <- data.frame(numeric_vars, df[, c("species", "sex")])

## -----------------------------------------------------------------------------
groups <- anticlustering(
  all_features,
  K = 3,
  method = "local-maximum", 
  standardize = TRUE
)
table(groups, df$sex)
table(groups, df$species)

## -----------------------------------------------------------------------------
binary_categories <- categories_to_binary(df[, c("species", "year")], use_combinations = FALSE)
data_input <- data.frame(binary_categories, numeric_vars)
groups <- anticlustering(
  data_input,
  K = 3,
  method = "local-maximum",
  standardize = TRUE
)
table(groups, df$year, df$species)

## -----------------------------------------------------------------------------
binary_categories <- categories_to_binary(df[, c("species", "year")], use_combinations = TRUE)
data_input <- data.frame(binary_categories, numeric_vars)
groups <- anticlustering(
  data_input,
  K = 3,
  method = "local-maximum",
  standardize = TRUE
)
table(groups, df$year, df$species)


