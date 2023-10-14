required_packages <- c("parallel", "MASS", "abind", "fpc", "cluster", "flexclust", "doParallel", "profvis", "dplyr")
install_missing <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(install_missing) > 0) {
  install.packages(install_missing)
}

library(flexclust)
library(parallel)
library(MASS)
library(fpc)
library(cluster)
library(doParallel)
library(dplyr)


set.seed(135)

# Function to generate synthetic data
generate_sample <- function(distribution_functions, n_samples, seed = 0) {
  set.seed(seed)

  n_clusters <- length(distribution_functions)

  sample <- lapply(seq_len(n_clusters), function(i_cluster) {
    cluster_data <- as.data.frame(distribution_functions[[i_cluster]](n_samples))
    cluster_data <- data.frame(cluster_id = i_cluster, cluster_data)
    return(cluster_data)
  }) %>%
    bind_rows()

  return(sample)
}

# Function to run DBSCAN clustering
dbscan_clusters <- function(data, hyperparameters) {
  # Unpack hyperparameters with defaults
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  eps <- ifelse("eps" %in% names(hyperparameters), hyperparameters["eps"], 1)
  min_pts <- ifelse("MinPts" %in% names(hyperparameters), hyperparameters["MinPts"], 5)

  distance_matrix <- dist(data, method = "minkowski", p = p)
  db <- fpc::dbscan(distance_matrix, eps = eps, MinPts = min_pts, method = "dist")
  return(db$cluster)
}

# Function to run hierarchical clustering
hier_clusters <- function(data, hyperparameters) {
  # Unpack hyperparameters with defaults
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters["k"], 2)
  method <- ifelse("method" %in% names(hyperparameters), as.character(hyperparameters["method"]), "average")

  distance_matrix <- dist(data, method = "minkowski", p = p)
  cluster <- cutree(hclust(distance_matrix, method = method), k = k)
  return(cluster)
}

# Function to run k-centroid clustering 
k_centroid_clusters <- function(data, hyperparameters) {
  # Unpack hyperparameters with defaults
  p <- ifelse("p" %in% names(hyperparameters), hyperparameters["p"], 2)
  k <- ifelse("k" %in% names(hyperparameters), hyperparameters["k"], 2)

  dist_func <- function(x, centers) flexclust::distMinkowski(x, centers, p = p)
  fam <- flexclust::kccaFamily(dist = dist_func)
  kc <- flexclust::kcca(data, k, family = fam)
  return(kc@cluster)
}

# Function to calculate silhouette score
silhouette_score <- function(distance_matrix, cluster) {
  stats <- cluster.stats(distance_matrix, clustering = cluster, silhouette = TRUE)
  sil_score <- stats$avg.silwidth
  # sil_score <- stats$dunn
  return(sil_score)
}


# Function to evaluate clustering hyperparameters
hyper_scores <- function(data, hyper_set, method, detect_outliers = TRUE) {
  results <- apply(hyper_set, 1, function(current_hyperparameters) {
    # Unpack hyperparameters
    p <- ifelse("p" %in% names(hyper_set), hyper_set["p"], 2)

    distance_matrix <- dist(data, method = "minkowski", p = p)

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
      distance_matrix <- dist(filtered_data, method = "minkowski", p = p)
    }

    # Ensure there are at least two clusters for silhouette calculation
    if (max(cluster) > 1) {
      score <- silhouette_score(distance_matrix, cluster)
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

# Function to find the best hyperparameters
best_hyperparameters <- function(results) {
  best_result <- results[which.max(sapply(results, function(result) result$score))]
  return(best_result)
}

n_replications <- 10
n_samples <- 200
n_clusters <- 3


start_seed <- 0
end_seed <- .Machine$integer.max
random_seeds <- sample(start_seed:end_seed, n_replications)

# 3 covariates
mu1 <- c(0, 0, 0)
mu2 <- c(4, 2, 4)
mu3 <- c(4, 8, 8)

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

distr1 <- function(n) mvrnorm(n, mu1, sigma1)
distr2 <- function(n) mvrnorm(n, mu2, sigma2)
distr3 <- function(n) mvrnorm(n, mu3, sigma3)

distrs <- list(
  distr1,
  distr2,
  distr3
)

# Define hyperparameter sets
dbscan_hyper_set <- expand.grid(
  # p = seq(1, 3),
  eps = seq(0.5, 5, by = 0.5),
  MinPts = seq(3, 10)
)

hier_clust_hyper_set <- expand.grid(
  # p = seq(1, 3),
  method = c("average", "complete", "ward.D"),
  k = seq(2, 10)
)

k_centroid_hyper_set <- expand.grid(
  # p = seq(1, 3),
  k = seq(2, 10)
)

# Define models
models <- list(
  model1 = list(method = hier_clusters, hyper_set = hier_clust_hyper_set),
  model2 = list(method = dbscan_clusters, hyper_set = dbscan_hyper_set),
  model3 = list(method = k_centroid_clusters, hyper_set = k_centroid_hyper_set)
)


run <- function() {
  registerDoParallel(2 / 3 * detectCores())

  p <- foreach(seed = random_seeds, .combine = "cbind") %dopar% {
    sample <- generate_sample(distrs, 100, seed)

    pred <- foreach(model = models, .combine = "c") %do% {
      model_scores <- hyper_scores(data = sample[, -1], hyper_set = model$hyper_set, model$method, TRUE)
      best <- best_hyperparameters(model_scores)
      return(best$n_clusters)
    }

    return(pred)
  }

  stopImplicitCluster()

  acc <- rowSums(p == n_clusters) / ncol(p)
  return(acc)
}

# Run the code to be profiled
run_profiler <- function() {
  p <- sapply(random_seeds, function(seed) {
    sample <- generate_sample(distrs, 100, seed)
    
    pred <- sapply(models, function(model) {
      model_scores <- hyper_scores(data = sample[, -1], hyper_set = model$hyper_set, model$method, TRUE)
      best <- best_hyperparameters(model_scores)
      return(best$n_clusters)
    })
    
    return(pred)
  })
  acc <- rowSums(p == n_clusters) / ncol(p)
  return(acc)
}


# Profile the code
Rprof()
run_profiler()
Rprof(NULL)
summaryRprof()


# Example single model usage
method <- k_centroid_clusters
example_sample <- generate_sample(distrs, n_samples, 0)
result <- hyper_scores(data = example_sample[, -1], hyper_set = k_centroid_hyper_set, method, TRUE)
best <- best_hyperparameters(result)

clusters <- method(example_sample[, -1], best$hyperparameters)
clusters
