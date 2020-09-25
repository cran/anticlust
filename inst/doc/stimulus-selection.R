## ---- include = FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# define a method for objects of the class data.frame
knit_print.matrix = function(x, ...) {
    res = paste(c("", "", kable(x, row.names = TRUE)), collapse = "\n")
    asis_output(res)
}
# register the method
registerS3method("knit_print", "matrix", knit_print.matrix)


## -----------------------------------------------------------------------------
library(anticlust)

## -----------------------------------------------------------------------------
data("schaper2019")
# look at the data
head(schaper2019)

## ---- echo = FALSE------------------------------------------------------------
cols <- toString(paste0("\`", names(schaper2019)[3:6], "\`"))

## -----------------------------------------------------------------------------
schaper2019 <- within(schaper2019, {
  freq <- ifelse(frequency < 18, "high", NA)
  freq <- ifelse(frequency > 19, "low", freq)
})

## -----------------------------------------------------------------------------
schaper2019$freq

## -----------------------------------------------------------------------------
selected <- subset(schaper2019, !is.na(freq))
# see how many cases were selected:
table(selected$freq)

## -----------------------------------------------------------------------------
# Match the conditions based on covariates
covariates <- scale(selected[, 3:5])
selected$matches <- matching(
  covariates, 
  match_between = selected$freq,
  match_within = selected$room,
  match_extreme_first = FALSE
)

## -----------------------------------------------------------------------------
subset(selected, matches == 1)
subset(selected, matches == 2)

## -----------------------------------------------------------------------------
# Select the 8 best matches:
final_selection <- subset(selected, matches <= 8)

## -----------------------------------------------------------------------------
# Check quality of the selection:
mean_sd_tab(
  final_selection[, 3:6], 
  final_selection$freq
)

## ---- similarity-plot---------------------------------------------------------
plot_similarity(
  covariates, 
  groups = selected$matches
)

## -----------------------------------------------------------------------------
# Reload the data for next example
data("schaper2019")

## -----------------------------------------------------------------------------
schaper2019 <- within(schaper2019, {
  incon <- ifelse(rating_inconsistent < median(rating_inconsistent), "low incon", NA)
  incon <- ifelse(rating_inconsistent >= median(rating_inconsistent), "high incon", incon)
  con <- ifelse(rating_consistent <= median(rating_consistent), "low con", NA)
  con <- ifelse(rating_consistent > median(rating_consistent), "high con", con)
})

## -----------------------------------------------------------------------------
table(schaper2019$con, schaper2019$incon)

## -----------------------------------------------------------------------------
# Match the conditions based on covariates
covariates <- scale(schaper2019[, c("frequency", "syllables")])
schaper2019$matches <- matching(
  covariates, 
  match_between = schaper2019[, c("con", "incon")],
  match_extreme_first = FALSE
)

## -----------------------------------------------------------------------------
subset(schaper2019, matches == 1)

## -----------------------------------------------------------------------------
# Plot covariate similarity by match:
plot_similarity(covariates, schaper2019$matches)

## -----------------------------------------------------------------------------
# Select the 5 best matches:
final_selection <- subset(schaper2019, matches <= 10)

## -----------------------------------------------------------------------------
mean_sd_tab(
  subset(final_selection, select = 3:6), 
  paste(final_selection$con, final_selection$incon)
)

## -----------------------------------------------------------------------------
# Reload the data for next example
data("schaper2019")

## -----------------------------------------------------------------------------
## Conduct anticlustering (assign all items to three similar groups)
schaper2019$anticluster <- anticlustering(
  schaper2019[, 3:6], 
  K = 3,
  objective = "variance"
)

## check out quality of the solution
mean_sd_tab(
  subset(schaper2019, select = 3:6), 
  schaper2019$anticluster
)

## -----------------------------------------------------------------------------
# Reload the data for next example
data("schaper2019")

## -----------------------------------------------------------------------------
# First, identify triplets of similar word, within room
covariates <- scale(schaper2019[, 3:6])
schaper2019$triplet <- matching(
  covariates,
  p = 3,
  match_within = schaper2019$room
)

# check out the two most similar triplets:
subset(schaper2019, triplet == 1)
subset(schaper2019, triplet == 2)

# Select the 10 best triplets
best <- subset(schaper2019, triplet <= 10)

## -----------------------------------------------------------------------------
best$anticluster <- anticlustering(
  best[, 3:6], 
  K = 3,
  categories = best$triplet,
  objective = "variance"
)

## -----------------------------------------------------------------------------
table(best$triplet, best$anticluster)

## -----------------------------------------------------------------------------
table(best$room, best$anticluster)

## -----------------------------------------------------------------------------
## check out quality of the solution
mean_sd_tab(
  subset(best, select = 3:6), 
  best$anticluster
)

