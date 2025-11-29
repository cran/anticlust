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
df <- na.omit(palmerpenguins::penguins)
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


## -----------------------------------------------------------------------------
groups1 <- anticlustering(
  numeric_vars, 
  K = 3,
  categories = df$sex, 
  standardize = TRUE,
  objective = "kplus", 
  method = "local-maximum"
)
groups2 <- anticlustering(
  numeric_vars, 
  K = 3,
  blocks = df$sex, 
  standardize = TRUE,
  objective = "kplus", 
  method = "local-maximum"
)

table(df$sex, groups1)
table(df$sex, groups2)

## -----------------------------------------------------------------------------
knitr::kable(mean_sd_tab(numeric_vars[df$sex == "female", ], groups1[df$sex == "female"]), row.names = TRUE) # categories argument
knitr::kable(mean_sd_tab(numeric_vars[df$sex == "female", ], groups2[df$sex == "female"]), row.names = TRUE) # blocks argument

knitr::kable(mean_sd_tab(numeric_vars[df$sex == "male", ], groups1[df$sex == "male"]), row.names = TRUE) # categories argument
knitr::kable(mean_sd_tab(numeric_vars[df$sex == "male", ], groups2[df$sex == "male"]), row.names = TRUE) # blocks argument

