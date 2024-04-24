## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(123)

## ----setup--------------------------------------------------------------------
library(anticlust)

## -----------------------------------------------------------------------------
library(palmerpenguins)
# First exclude cases with missing values
df <- na.omit(penguins)
head(df)
nrow(df)

## -----------------------------------------------------------------------------
numeric_vars <- df[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
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
binary_categories <- categories_to_binary(df[, c("species", "sex")])
# see ?categories_to_binary
head(binary_categories)

## -----------------------------------------------------------------------------
groups <- anticlustering(
  binary_categories,
  K = 3,
  method = "local-maximum", 
  objective = "variance",
  repetitions = 10,
  standardize = TRUE
)
table(groups, df$sex)
table(groups, df$species)

## -----------------------------------------------------------------------------
binary_categories <- categories_to_binary(df[, c("species", "sex")], use_combinations = TRUE)
groups <- anticlustering(
  binary_categories,
  K = 3,
  method = "local-maximum", 
  objective = "variance",
  repetitions = 10,
  standardize = TRUE
)
table(groups, df$sex)
table(groups, df$species) 
table(groups, df$sex, df$species)

## -----------------------------------------------------------------------------
final_groups <- anticlustering(
  numeric_vars,
  K = groups,
  standardize = TRUE,
  method = "local-maximum",
  categories = df[, c("species", "sex")]
)

table(groups, df$sex)
table(groups, df$species)
mean_sd_tab(numeric_vars, final_groups)

## -----------------------------------------------------------------------------
final_groups <- anticlustering(
  cbind(numeric_vars, binary_categories),
  K = 3,
  standardize = TRUE,
  method = "local-maximum", 
  objective = "variance",
  repetitions = 10
)

table(groups, df$sex)
table(groups, df$species)
mean_sd_tab(numeric_vars, final_groups)

## -----------------------------------------------------------------------------
final_groups <- anticlustering(
  cbind(kplus_moment_variables(numeric_vars, T = 2), binary_categories),
  K = 3,
  method = "local-maximum", 
  objective = "variance", 
  repetitions = 10
)

table(groups, df$sex)
table(groups, df$species)
mean_sd_tab(numeric_vars, final_groups)

