# -----------------------
# Package Installation and Loading
# -----------------------
required_packages <- c("parallel", "MASS", "fpc", "cluster", "flexclust", "doParallel", "profvis", "dplyr", "scatterplot3d", "viridis", "fields", "webshot")
install_missing <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(install_missing) > 0) {
  install.packages(install_missing)
}
lapply(required_packages, library, character.only = TRUE)

# -----------------------
# Utility Functions
# -----------------------
# Utility function to retrieve hyperparameters with defaults
get_hyperparameter <- function(hyperparameters, param_name, default) {
  if (param_name %in% names(hyperparameters)) {
    return(hyperparameters[[param_name]])
  }
  return(default)
}

# -----------------------
# Data Generation and Clustering Functions
# -----------------------
# Function to generate synthetic data
generate_sample <- function(distribution_functions, n_samples, seed = 0) {
  set.seed(seed)
  n_clusters <- length(distribution_functions)

  sample <- lapply(seq_len(n_clusters), function(i_cluster) {
    cluster_data <- as.data.frame(distribution_functions[[i_cluster]](n_samples))
    cbind(cluster_id = i_cluster, cluster_data)
  }) %>%
    bind_rows()

  return(sample)
}

# Function to run DBSCAN clustering
dbscan_clusters <- function(data, hyperparameters) {
  dist_mat <- dist(data, method = "minkowski", p = get_hyperparameter(hyperparameters, "p", 2))
  fpc::dbscan(dist_mat, eps = get_hyperparameter(hyperparameters, "eps", 1), 
              MinPts = get_hyperparameter(hyperparameters, "MinPts", 5), method = "dist")$cluster
}

# Function to run hierarchical clustering
hier_clusters <- function(data, hyperparameters) {
  dist_mat <- dist(data, method = "minkowski", p = get_hyperparameter(hyperparameters, "p", 2))
  cutree(hclust(dist_mat, method = get_hyperparameter(hyperparameters, "method", "average")), 
         k = get_hyperparameter(hyperparameters, "k", 2))
}

# Function to run k-centroid clustering
k_centroid_clusters <- function(data, hyperparameters) {
  dist_func <- function(x, centers) flexclust::distMinkowski(x, centers, p = get_hyperparameter(hyperparameters, "p", 2))
  fam <- flexclust::kccaFamily(dist = dist_func)
  flexclust::kcca(data, k = get_hyperparameter(hyperparameters, "k", 2), family = fam)@cluster
}

# Function to calculate silhouette score
silhouette_score <- function(distance_matrix, cluster) {
  stats <- cluster.stats(distance_matrix, clustering = cluster, silhouette = TRUE)
  return(stats$avg.silwidth)
}

# Function to evaluate clustering hyperparameters
hyper_scores <- function(data, hyper_set, method, detect_outliers = TRUE) {
  results <- apply(hyper_set, 1, function(current_hyperparameters) {
    hyper_str <- paste(names(current_hyperparameters), current_hyperparameters, sep = ":", collapse = ", ")

    p <- get_hyperparameter(current_hyperparameters, "p", 2)
    distance_matrix <- dist(data, method = "minkowski", p = p)

    cluster <- method(data, current_hyperparameters)

    if (detect_outliers) {
      non_zero_cluster_indices <- which(cluster != 0)
      cluster <- cluster[non_zero_cluster_indices]
      filtered_data <- data[non_zero_cluster_indices, ]
      distance_matrix <- dist(filtered_data, method = "minkowski", p = p)
    }

    n_clusters <- max(cluster)
    score <- if (n_clusters > 1) {
      score <- silhouette_score(distance_matrix, cluster)
      data.frame(hyperparameters = hyper_str, score = score, n_clusters = n_clusters)
    }
  })

  results_df <- do.call(rbind, results)
  return(results_df)
}

# Find best hyperparameters based on score
best_hyperparameters <- function(results_df) {
  results_df[which.max(results_df$score), ]
}

set.seed(2023)
n_replications <- 5
n_samples <- 100
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
  p = seq(1, 5),
  eps = seq(0.5, 5, by = 0.5),
  MinPts = seq(3, 10)
)


hier_clust_hyper_set <- expand.grid(
  p = seq(1, 5),
  method = c("average", "complete", "ward.D"),
  k = seq(2, 10)
)

k_centroid_hyper_set <- expand.grid(
  p = seq(1, 5),
  k = seq(2, 10)
)

# Define models
models <- list(
  model1 = list(name = "hier", method = hier_clusters, hyper_set = hier_clust_hyper_set),
  model2 = list(name = "dbscan", method = dbscan_clusters, hyper_set = dbscan_hyper_set),
  model3 = list(name = "k-centroid", method = k_centroid_clusters, hyper_set = k_centroid_hyper_set)
)

# -----------------------
# Model Execution and Profiling
# -----------------------
run_model <- function(distrs, n_samples, seeds, models) {
  registerDoParallel(2 / 3 * detectCores())
  
  results <- foreach(seed = seeds, .combine = "rbind") %dopar% {
    sample <- generate_sample(distrs, n_samples, seed)
    foreach(model = models, .combine = "rbind") %do% {
      model_scores <- hyper_scores(sample[, -1], model$hyper_set, model$method, TRUE)
      cbind(model = model$name, model_scores, seed=seed)
    }
  }

  stopImplicitCluster()
  return(results)
}


# Run the code to be profiled
run_serialized <- function(distrs, n_samples, seeds, models) {
  results <- list()

  for (seed in seeds) {
    sample <- generate_sample(distrs, n_samples, seed)
    preds <- list()

    for (model in models) {
      model_scores <- hyper_scores(data = sample[, -1], hyper_set = model$hyper_set, model$method, TRUE)
      best <- best_hyperparameters(model_scores)
      preds[[model$name]] <- best$n_clusters
    }
    
    results[[as.character(seed)]] <- preds
  }

  p <- sapply(results, function(pred) unlist(pred))
  acc <- rowSums(p == n_clusters) / ncol(p)
  return(acc)
}

prof <- profvis({
  run_serialized(distrs, n_samples, random_seeds, models)
})
print(prof)

results <- run_model(distrs, n_samples, random_seeds, models)

results$correct <- results$n_clusters == n_clusters
results

best <- results %>%
    group_by(seed, model) %>%
    slice(which.max(score)) %>% # This selects the row with the highest "correct" value for each group
    ungroup() %>% # Ungroup before grouping again by another variable
    group_by(model) %>%
    summarize(
      acc = mean(correct, na.rm = TRUE)
    )

best

# webshot::install_phantomjs()

# Save the profvis output to an HTML file]
htmlwidgets::saveWidget(prof, "profile.html", selfcontained = FALSE)
# Capture the HTML file as an image
webshot::webshot("profile.html", "profile.png", delay = 5)


# -----------------------
# Hyperparameter Extraction Functions
# -----------------------
extract_hyperparameters_hier <- function(hyperparam_string) {
  list(
    p = as.numeric(gsub(".*p: *([0-9]+).*", "\\1", hyperparam_string)),
    method = gsub(".*method: *([^,]+),.*", "\\1", hyperparam_string),
    k = as.numeric(gsub(".*k: *([0-9]+).*", "\\1", hyperparam_string))
  )
}

extract_hyperparameters_dbscan <- function(hyperparam_string) {
  list(
    p = as.numeric(gsub(".*p: *([0-9]+).*", "\\1", hyperparam_string)),
    eps = as.numeric(gsub(".*eps: *([0-9.-]+).*", "\\1", hyperparam_string)), # Accounted for potential negative values
    MinPts = as.numeric(gsub(".*MinPts: *([0-9]+).*", "\\1", hyperparam_string))
  )
}

extract_hyperparameters_kcentroid <- function(hyperparam_string) {
  list(
    p = as.numeric(gsub(".*p: *([0-9]+).*", "\\1", hyperparam_string)),
    k = as.numeric(gsub(".*k: *([0-9]+).*", "\\1", hyperparam_string))
  )
}

# -----------------------
# Data Preparation Function
# -----------------------
prepare_data_for_model <- function(full_data, model_name, extract_func) {
  # Calculate average scores and sort by model and average score
  data_avg <- full_data %>%
    group_by(model, hyperparameters) %>%
    summarize(
      avg_score = mean(score, na.rm = TRUE),
      acc = mean(correct, na.rm = TRUE),
      n = n()
    ) %>%
    arrange(model, avg_score)

  data <- subset(data_avg, model == model_name)
  sorted_data <- data[order(data$acc, data$avg_score, decreasing = TRUE), ]
  best_record <- sorted_data[1, ]
  best_hyperparams <- extract_func(best_record$hyperparameters)
  
  all_data <- subset(full_data, model == model_name)
  all_data_hyperparams <- lapply(all_data$hyperparameters, extract_func)
  for (param in names(best_hyperparams)) {
    all_data[[param]] <- sapply(all_data_hyperparams, `[[`, param)
  }
  
  list(best_record = best_record, best_hyperparams = best_hyperparams, all_data = all_data)
}

# -----------------------
# Plotting Function
# -----------------------
plot_boxplots_for_model <- function(data, model_name, hyperparam_names, save_directory = ".") {
  
  for (param in hyperparam_names) {
    
    # Identify the conditions to filter the data
    conditions <- setdiff(hyperparam_names, param)
    subset_conditions <- sapply(conditions, function(cond) {
      if(is.character(data$best_hyperparams[[cond]])) {
        return(data$all_data[[cond]] == data$best_hyperparams[[cond]])
      } else {
        return(data$all_data[[cond]] == data$best_hyperparams[[cond]])
      }
    })
    filtered_data <- data$all_data[apply(subset_conditions, 1, all), ]
    
    # Define the filename for the plot
    file_name <- paste0(
      model_name, "_", 
      paste(conditions, sapply(conditions, function(cond) data$best_hyperparams[[cond]]), sep="=", collapse="_"), 
      ".png"
    )
    file_path <- file.path(save_directory, file_name)
    
    # Save the plot to PNG file
    png(filename = file_path)
    boxplot(score ~ filtered_data[[param]], data = filtered_data,
            main = paste(model_name, "with", paste(conditions, sapply(conditions, function(cond) data$best_hyperparams[[cond]]), sep=":", collapse=", ")),
            xlab = param, ylab = "Scores")
    dev.off()
  }
}

# -----------------------
# Main Execution
# -----------------------
hier_data <- prepare_data_for_model(results, "hier", extract_hyperparameters_hier)
plot_boxplots_for_model(hier_data, "Hier method", c("p", "method", "k"))

dbscan_data <- prepare_data_for_model(results, "dbscan", extract_hyperparameters_dbscan)
plot_boxplots_for_model(dbscan_data, "DBSCAN", c("p", "eps", "MinPts"))

kcentroid_data <- prepare_data_for_model(results, "k-centroid", extract_hyperparameters_kcentroid)
plot_boxplots_for_model(kcentroid_data, "k-centroid", c("p", "k"))


# -----------------------
# Color Generation Function
# -----------------------
generate_colors <- function(data, color_palette) {
  cut_values <- cut(data$avg_score, breaks = 100, labels = FALSE)
  color_palette[cut_values]
}

# -----------------------
# Data Preparation Function
# -----------------------
prepare_data_for_3d_plot <- function(full_data, model_name, extract_func) {
  data_avg <- full_data %>%
    group_by(model, hyperparameters) %>%
    summarize(
      avg_score = mean(score, na.rm = TRUE),
      acc = mean(correct, na.rm = TRUE),
      n = n()
    ) %>%
    arrange(model, avg_score)

  data <- subset(data_avg, model == model_name)

  hyperparams <- lapply(data$hyperparameters, extract_func)
  for (param in names(hyperparams[[1]])) {
    data[[param]] <- sapply(hyperparams, `[[`, param)
  }

  return(data)
}

# -----------------------
# Plotting Functions
# -----------------------
save_plot_to_png <- function(plot_func, data, filename) {
  png(filename, width = 800, height = 600)
  plot_func(data)
  dev.off()
}

plot_dbscan <- function(data) {
  color_palette <- viridis(100)
  colors <- generate_colors(data, color_palette)

  scatterplot3d(data$p, data$eps, data$MinPts, pch = 16, xlab = "p", ylab = "eps", zlab = "MinPts", color = colors, main = "DBSCAN", cex.symbols = 5)

  # Continuous color legend
  image.plot(legend.only = TRUE, zlim = c(min(data$avg_score, na.rm = TRUE), max(data$avg_score, na.rm = TRUE)), col = color_palette, horizontal = FALSE, legend.width = 1, legend.shrink = 0.75, legend.mar = 4.5, smallplot = c(.92, .95, .3, .8))
}

plot_hier <- function(data) {
  color_palette <- viridis(100)
  colors <- generate_colors(data, color_palette)

  data$coded_method <- as.numeric(as.factor(data$method))
  
  scatterplot3d(data$p, data$coded_method, data$k, pch = 16, xlab = "p", ylab = "Method", zlab = "k", color = colors, main = "Hierarchical Clustering", cex.symbols = 5)

  axis(2, at = seq_along(unique(data$method)), labels = unique(data$method))
  
  # Add a legend for method codes. Adjusted the location and size.
  legend("bottomright", legend = rev(paste(unique(data$coded_method), unique(data$method), sep = ": ")), bty = "n", cex = 1.2)

  # Continuous color legend
  image.plot(legend.only = TRUE, zlim = c(min(data$avg_score, na.rm = TRUE), max(data$avg_score, na.rm = TRUE)), col = color_palette, horizontal = FALSE, legend.width = 1, legend.shrink = 0.75, legend.mar = 4.5, smallplot = c(.92, .95, .3, .8))
}

plot_kcentroid <- function(data) {
  color_palette <- viridis(100)
  colors <- generate_colors(data, color_palette)

  plot(data$p, data$k, pch = 16, cex = 5, col = colors, xlab = "p", ylab = "k", main = "k-centroid Clustering")

  # Continuous color legend
  image.plot(legend.only = TRUE, zlim = c(min(data$avg_score, na.rm = TRUE), max(data$avg_score, na.rm = TRUE)), col = color_palette, horizontal = FALSE, legend.width = 1, legend.shrink = 0.75, legend.mar = 4.5, smallplot = c(.92, .95, .3, .8))
}



# -----------------------
# Main Execution
# -----------------------
dbscan_data <- prepare_data_for_3d_plot(results, "dbscan", extract_hyperparameters_dbscan)
save_plot_to_png(plot_dbscan, dbscan_data, "dbscan_plot.png")

hier_data <- prepare_data_for_3d_plot(results, "hier", extract_hyperparameters_hier)
save_plot_to_png(plot_hier, hier_data, "hier_plot.png")

kcentroid_data <- prepare_data_for_3d_plot(results, "k-centroid", extract_hyperparameters_kcentroid)
save_plot_to_png(plot_kcentroid, kcentroid_data, "kcentroid_plot.png")


