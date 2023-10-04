if (!require("parallel")) install.packages("parallel")
if (!require("MASS")) install.packages("MASS")
if (!require("abind")) install.packages("abind")
if (!require("fpc")) install.packages("fpc") # Manjaro Distro needs "gcc-fortran"
# if (!require("cluster")) install.packages("cluster")
if (!require("flexclust")) install.packages("flexclust")


library(parallel)
library(MASS)
library(abind)
library(fpc)
library(cluster)
library(flexclust)

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


# hclust(dist(example_sample[, 1], method = "minkowski", p = 4), method = "average")

dbscan_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters$p, 2)
  eps <- ifelse("eps" %in% names(hyperparameters), hyperparameters$eps, 1)
  min_pts <- ifelse("MinPts" %in% names(hyperparameters), hyperparameters$MinPts, 5)

  distance_matrix <- dist(data, method = "minkowski", p = p)
  db <- fpc::dbscan(distance_matrix, eps = eps, MinPts = min_pts, method = "dist")
  return(db$cluster)
}

hier_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters$p, 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters$k, 2)
  method <- ifelse("method" %in% names(hyperparameters), as.character(hyperparameters$method), "average")

  distance_matrix <- dist(data, method = "minkowski", p = p)
  cluster <- cutree(hclust(distance_matrix, method = method), k = k)
  return(cluster)
}

k_centroid_clusters <- function(data, hyperparameters) {
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters$p, 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters$k, 2)

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




optim <- function(data, hyper_set, method, detect_outliers = TRUE) {
  best_score <- -Inf
  best_hyperparameters <- NULL
  n_clusters <- NULL

  for (i in seq_len(nrow(hyper_set))) {
    current_hyperparameters <- hyper_set[i, ]
    print(current_hyperparameters)

    # Perform DBSCAN clustering
    # dist <- dist(data, method = "minkowski", p = 2)
    cluster <- method(data, current_hyperparameters)

    # Check if any clusters were found
    if (max(cluster) == 0) {
      next
    }

    # Filter outliers
    if (detect_outliers) {
      non_zero_cluster_indices <- which(cluster != 0)
      cluster <- cluster[non_zero_cluster_indices]
      filtered_data <- data[non_zero_cluster_indices, ]
      p <- ifelse("p" %in% names(current_hyperparameters), current_hyperparameters$p, 2)
      print(p)
      dist <- dist(filtered_data[, -1], method = "minkowski", p = p)
    }

    # Ensure there are at least two clusters for silhouette calculation
    if (max(cluster) > 1) {
      score <- silhouette_score(dist, cluster)
      print(cbind(score, n_clusters = max(cluster), current_hyperparameters))

      # Check if this combination is better than the current best
      if (score > best_score) {
        best_score <- score
        best_hyperparameters <- current_hyperparameters
        n_clusters <- max(cluster)
      }
    }
  }

  return(list(best_hyperparameters = best_hyperparameters, best_score = best_score, n_clusters = n_clusters))
}

# Example usage:
hyper_set <- expand.grid(
  p = seq(1, 3),
  # eps = seq(0.5, 5, by = 0.1),
  # MinPts = seq(3, 10)
  # method = c("average", "complete", "ward.D"),
  k = seq(3, 5)
)

method <- k_centroid_clusters

result <- optim(data = example_sample, hyper_set = hyper_set, method, TRUE)
result

clusters <- method(example_sample[, -1], result$best_hyperparameters)
example_sample[, 1]
clusters
