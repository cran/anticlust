## ---- include = FALSE---------------------------------------------------------
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
N <- 1000
M <- 5
K <- 3
data <- matrix(rnorm(N*M), ncol = M)
system.time(anticlustering(data, K = K))
system.time(anticlustering(data, K = K, preclustering = TRUE))

## -----------------------------------------------------------------------------
N <- nrow(iris)
initial_clusters <- sample(rep_len(1:K, N))
initial_clusters

## -----------------------------------------------------------------------------
n_exchange_partners <- 10
exchange_partners <- sample(rep_len(1:(N/n_exchange_partners), N))
exchange_partners

## -----------------------------------------------------------------------------
system.time(anticlustering(iris[, -5], K = initial_clusters))
system.time(anticlustering(iris[, -5], K = initial_clusters, categories = exchange_partners))

## -----------------------------------------------------------------------------
N <- 1000
M <- 2
K <- 5
data <- matrix(rnorm(M*N), ncol = M)

initial_clusters <- sample(rep_len(1:K, N))
n_exchange_partners <- 10
exchange_partners <- sample(rep_len(1:(N/n_exchange_partners), N))

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
n_exchange_partners <- 10
exchange_partners <- sample(rep_len(1:(N/n_exchange_partners), N))

## -----------------------------------------------------------------------------
groups <- anticlustering(
  iris[, -5],
  K = initial_groups, 
  categories = cbind(iris$Species, exchange_partners)
)

## -----------------------------------------------------------------------------
table(groups, iris$Species)

## -----------------------------------------------------------------------------
N <- 1000
M <- 2
K <- 5
data <- matrix(rnorm(M*N), ncol = M)

system.time(anticlustering(data, K = K, objective = "diversity"))
system.time(anticlustering(data, K = K, objective = "variance")) # k-means anticlustering


## -----------------------------------------------------------------------------
N <- 1000
M <- 20
K <- 50
data <- matrix(rnorm(M*N), ncol = M)

system.time(anticlustering(data, K = K, objective = "diversity"))
system.time(anticlustering(data, K = K, objective = "variance"))

## ---- eval = FALSE------------------------------------------------------------
#  
#  N <- 50000
#  M <- 2
#  K <- 5
#  data <- matrix(rnorm(M*N), ncol = M)
#  
#  initial_clusters <- sample(rep_len(1:K, N))
#  n_exchange_partners <- 2
#  exchange_partners <- sample(rep_len(1:(N/n_exchange_partners), N))
#  
#  kplus_anticlustering(data, K = initial_clusters, categories = exchange_partners)

## -----------------------------------------------------------------------------
N <- 1000
M <- 2
K <- 5
data <- matrix(rnorm(M*N), ncol = M)

initial_clusters <- sample(rep_len(1:K, N))
n_exchange_partners <- 10
exchange_partners <- sample(rep_len(1:(N/n_exchange_partners), N))

# fast_anticlustering() always optimizes the k-means criterion (i.e., objective = "variance")
system.time(anticlustering(data, K = initial_clusters, categories = exchange_partners, objective = "variance"))
system.time(fast_anticlustering(data, K = initial_clusters, k_neighbours = n_exchange_partners))


