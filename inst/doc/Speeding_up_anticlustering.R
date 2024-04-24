## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

set.seed(1234)

## ----setup--------------------------------------------------------------------
library(anticlust)

## -----------------------------------------------------------------------------

K <- 3
system.time(anticlustering(iris[, -5], K = K, method = "exchange"))
system.time(anticlustering(iris[, -5], K = K, method = "local-maximum"))


## -----------------------------------------------------------------------------

K <- 3
system.time(anticlustering(iris[, -5], K = K, method = "exchange"))
system.time(anticlustering(iris[, -5], K = K, method = "local-maximum"))
system.time(anticlustering(iris[, -5], K = K, method = "local-maximum", repetitions = 10))


## -----------------------------------------------------------------------------
N <- 5000
M <- 2
data <- matrix(rnorm(N * M), ncol = M)
start <- Sys.time()
groups1 <- fast_anticlustering(data, K = 2)  # default uses all exchange partners
Sys.time() - start 

## -----------------------------------------------------------------------------
start <- Sys.time()
groups2 <- fast_anticlustering(data, K = 2, k_neighbours = 20)
Sys.time() - start 

## -----------------------------------------------------------------------------
variance_objective(data, groups1)
variance_objective(data, groups2)

## -----------------------------------------------------------------------------
N <- 1000
M <- 5
K <- 3
data <- matrix(rnorm(N*M), ncol = M)
system.time(anticlustering(data, K = K))
system.time(anticlustering(data, K = K, preclustering = TRUE))

## -----------------------------------------------------------------------------
N <- nrow(iris)
K <- 3
initial_clusters <- sample(rep_len(1:K, N))
initial_clusters
table(initial_clusters)

## -----------------------------------------------------------------------------
exchange_partners <- sample(rep_len(1:(N/10), N)) #somewhat ugly but works
exchange_partners
table(exchange_partners)

## -----------------------------------------------------------------------------
system.time(anticlustering(iris[, -5], K = initial_clusters))
system.time(anticlustering(iris[, -5], K = initial_clusters, categories = exchange_partners))

## -----------------------------------------------------------------------------
N <- 1000
M <- 2
K <- 5
data <- matrix(rnorm(M*N), ncol = M)

initial_clusters <- sample(rep_len(1:K, N))
exchange_partners <- sample(rep_len(1:(N/10), N))

system.time(anticlustering(data, K = initial_clusters))
system.time(anticlustering(data, K = initial_clusters, categories = exchange_partners))

## -----------------------------------------------------------------------------
groups <- anticlustering(iris[, -5], K = 5, categories = iris$Species)
table(groups, iris$Species)

## -----------------------------------------------------------------------------
initial_groups <- categorical_sampling(iris$Species, K = 5)
table(initial_groups, iris$Species) # even!

## -----------------------------------------------------------------------------
N <- nrow(iris)
exchange_partners <- sample(rep_len(1:(N/10), N))

## -----------------------------------------------------------------------------
groups <- anticlustering(
  iris[, -5],
  K = initial_groups, 
  categories = cbind(iris$Species, exchange_partners)
)

## -----------------------------------------------------------------------------
table(groups, iris$Species)

## -----------------------------------------------------------------------------
N <- 100000
M <- 3
K <- 5
data <- matrix(rnorm(M*N), ncol = M)

start <- Sys.time()
groups <- fast_anticlustering(
  kplus_moment_variables(data, T = 2), 
  K = K, 
  exchange_partners = generate_exchange_partners(10, N = N)
)
Sys.time() - start
mean_sd_tab(data, groups) # means and standard deviations are similar

