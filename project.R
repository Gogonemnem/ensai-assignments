if (!require("parallel")) install.packages("parallel")
if (!require("MASS")) install.packages("MASS")
if (!require("abind")) install.packages("abind")
if (!require("fpc")) install.packages("fpc") # Manjaro Distro needs "gcc-fortran"
# if (!require("cluster")) install.packages("cluster")
if (!require("flexclust")) install.packages("flexclust")
if (!require("doParallel")) install.packages("doParallel")


library(parallel)
library(MASS)
library(abind)
library(fpc)
library(cluster)
library(flexclust)
library(doParallel)

set.seed(135)

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


dbscan_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  eps <- ifelse("eps" %in% names(hyperparameters), hyperparameters["eps"], 1)
  min_pts <- ifelse("MinPts" %in% names(hyperparameters), hyperparameters["MinPts"], 5)

  distance_matrix <- dist(data, method = "minkowski", p = p)
  db <- fpc::dbscan(distance_matrix, eps = eps, MinPts = min_pts, method = "dist")
  return(db$cluster)
}

hier_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters["k"], 2)
  method <- ifelse("method" %in% names(hyperparameters), as.character(hyperparameters["method"]), "average")

  distance_matrix <- dist(data, method = "minkowski", p = p)
  cluster <- cutree(hclust(distance_matrix, method = method), k = k)
  return(cluster)
}

k_centroid_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters["k"], 2)

  dist_func <- function(x, centers) distMinkowski(x, centers, p = p)
  fam <- kccaFamily(dist = dist_func)
  kc <- kcca(data, k, family = fam)
  return(kc@cluster)
}


silhouette_score <- function(distance_matrix, cluster) {
  stats <- cluster.stats(distance_matrix, clustering = cluster, silhouette = TRUE)
  sil_score <- stats$avg.silwidth
  # sil_score <- stats$dunn
  return(sil_score)
}


hyper_scores <- function(data, hyper_set, method, detect_outliers = TRUE) {
  results <- apply(hyper_set, 1, function(current_hyperparameters) {    
    # Perform clustering
    cluster <- method(data, current_hyperparameters)

    # Check if any clusters were found
    if (max(cluster) == 0) {
      return(NULL)  # Skip this set of hyperparameters
    }

    # Filter outliers
    if (detect_outliers) {
      non_zero_cluster_indices <- which(cluster != 0)
      cluster <- cluster[non_zero_cluster_indices]
      filtered_data <- data[non_zero_cluster_indices, ]
      p <- ifelse("p" %in% names(current_hyperparameters), current_hyperparameters["p"], 2)
      dist <- dist(filtered_data[, -1], method = "minkowski", p = p)
    }

    # Ensure there are at least two clusters for silhouette calculation
    if (max(cluster) > 1) {
      score <- silhouette_score(dist, cluster)
      n_clusters <- max(cluster)

      # Return the result for this set of hyperparameters
      return(list(
        hyperparameters = current_hyperparameters,
        score = score,
        n_clusters = n_clusters
      ))
    } else {
      return(NULL)  # Skip this set of hyperparameters
    }
  })

  # Filter out NULL results and return the list of results
  return(results[!sapply(results, is.null)])
}

best_hyperparameters <- function(results) {
  best_result <- NULL
  best_score <- -Inf

  for (result in results) {
    if (result$score > best_score) {
      best_score <- result$score
      best_result <- result
    }
  }

  return(best_result)
}

simulate <- function(seed, n_clusters, n_samples, mu, sigma, method, hyper_set) {
  set.seed(seed)
  sample <- generate_sample(n_clusters, n_samples, mu, sigma)
  result <- hyper_scores(data = sample[, -1], hyper_set = hyper_set, method, TRUE)
  best <- best_hyperparameters(result)
  return(best$n_clusters)
}

# Define the range of seed values with the maximum possible integer as the upper bound
start_seed <- 1
end_seed <- .Machine$integer.max

n_replications <- 10
n_samples <- 200
n_clusters <- 3
n_covariates <- 3

mu1 <- c(0, 0, 0)
mu2 <- c(2, 2, 2)
mu3 <- c(4, 4, 4)
mu <- cbind(mu1, mu2, mu3)

sigma1 <- matrix(c(
  1.0, 0.5, 0.3,
  0.5, 1.0, 0.2,
  0.3, 0.2, 0.8
), nrow = 3, ncol = 3)

sigma2 <- matrix(c(
  0.9, -0.2, 0.4,
  -0.2, 1.2, -0.3,
  0.4, -0.3, 1.0
), nrow = 3, ncol = 3)

sigma3 <- matrix(c(
  1.0, 0.0, 0.0,
  0.0, 2.0, 0.0,
  0.0, 0.0, 0.5
), nrow = 3, ncol = 3)

# Sigma size: covariates x covariates x clusters
sigma <- abind(sigma1, sigma2, sigma3, along = 3)

dbscan_hyper_set <- expand.grid(
  p = seq(1, 3),
  eps = seq(0.5, 5, by = 0.1),
  MinPts = seq(3, 10)
)

hier_clust_hyper_set <- expand.grid(
  p = seq(1, 3),
  method = c("average", "complete", "ward.D"),
  k = seq(2, 10)
)

k_centroid_hyper_set <- expand.grid(
  p = seq(1, 3),
  k = seq(2, 10)
)

# Generate a vector of random seed values within the specified range
random_seeds <- sample(start_seed:end_seed, n_replications)

models <- list(
  model1 = list(method = hier_clusters, hyper_set = hier_clust_hyper_set),
  model2 = list(method = dbscan_clusters, hyper_set = dbscan_hyper_set),
  model3 = list(method = k_centroid_clusters, hyper_set = k_centroid_hyper_set)
)

registerDoParallel(2/3*detectCores())

acc <- foreach(model = models) %dopar% {
  predicted_clusters <- foreach(seed = random_seeds) %dopar% {
    n_cl <- simulate(seed, n_clusters, n_samples, mu, sigma, model$method, model$hyper_set)
    print(n_cl)
    return(n_cl)
  }
  accuracy <- mean(predicted_clusters == n_clusters)
  return(accuracy)
}

stopImplicitCluster()

# # Example usage:

method <- hier_clusters

example_sample <- generate_sample(n_clusters, n_samples, mu, sigma)
result <- hyper_scores(data = example_sample, hyper_set = hier_clust_hyper_set, method, TRUE)
best <- best_hyperparameters(result)
best

clusters <- method(example_sample[, -1], result$best_hyperparameters)
example_sample[, 1]
clusters