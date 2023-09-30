library(parallel)
library(MASS)
library(abind)

set.seed(135)

detectCores()

n_replications <- 10
n_samples <- 10
n_clusters <- 3
n_covariates <- 3


mu1 <- c(1, 1, 1)
mu2 <- c(50, 2, 2)
mu3 <- c(100, 3, 3)
mu <- cbind(mu1, mu2, mu3)

sigma <- matrix(1:9, 3, 3)
# Sigma size: covariates x covariates x clusters
sigma <- abind(sigma, sigma, sigma, along = 3)





generate_sample <- function(n_clusters, n_samples, mu, sigma) {
  # sample is of size: samples x ([cluster_id] + covariates)
  sample <- do.call(
    rbind,
    lapply(
      seq_len(n_clusters),
      function(i_cluster) {
        cluster_data <- mvrnorm(
          n = n_samples, mu[, i_cluster], sigma[, , i_cluster]
        )
        cluster_data_with_id <- cbind(i_cluster, cluster_data)
        return(cluster_data_with_id)
      }
    )
  )
  return(sample)
}

results_list <- mclapply(1:2, function(x) {
  generate_sample(n_clusters, n_samples, mu, sigma)
}, mc.cores = detectCores() - 2)

example_sample <- generate_sample(n_clusters, n_samples, mu, sigma)
