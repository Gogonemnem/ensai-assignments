J_BASES <- 100
N_SAMPLES <- 300
K_TIMES <- 250
MC_SAMPLES <- 500
R_REPLICATIONS <- 500
set.seed(2023)



### Utility and Mathematical Functions
zero_mean_fn <- function(time) {
  return(0)
}

sinusoidal_mean_fn <- function(time, amplitude = 1, frequency = 1, phase = 0) {
  amplitude * sin(2 * pi * frequency * time + phase)
}

error_fn <- function(time, sigma = 1) {
  rnorm(1, mean = 0, sd = sigma)
}


# Function to calculate the ith eigenvalue for Brownian motion
eigen_value_bm <- function(index) {
  nom <- 1
  denom <- pi^2 * (index - 1 / 2)^2
  return(nom / denom)
}

eigen_values_bm <- function(num_bases) {
  sapply(seq_len(num_bases), eigen_value_bm)
}

# Function to generate the ith eigenfunction for Brownian motion
eigen_function_bm <- function(index) {
  function(time) {
    sqrt(2) * sin(pi * (index - 1 / 2) * time)
  }
}

eigen_functions_bm <- function(num_bases) {
  sapply(seq_len(num_bases), eigen_function_bm)
}



### Data Generation Functions
# Function to simulate design points based on Poisson or fixed K
simulate_design_points <- function(num_functions, num_times, is_poisson = FALSE) {
  if (is_poisson) {
    sapply(seq_len(num_functions), function(i) sort(runif(rpois(1, lambda = num_times), 0, 1)))
  } else {
    sapply(seq_len(num_functions), function(i) sort(runif(num_times, 0, 1)))
  }
}


library(MASS)
# Function to generate scores based on eigenvalues
score_generator <- function(num_samples, eigen_values) {
  num_bases <- length(eigen_values)
  mean <- rep(0, num_bases)
  cov <- diag(num_bases) * eigen_values
  # Normal for convenience
  sampled_scores <- MASS::mvrnorm(n = num_samples, mu = mean, Sigma = cov)
  return(sampled_scores)
}

# Function to generate observations with noise at design points
generate_observations <- function(observation_functions, num_times, is_poisson = FALSE) {
  design_points <- simulate_design_points(length(observation_functions), num_times, is_poisson)

  observations <- lapply(seq_along(observation_functions), function(i) {
    sapply(design_points[[i]], observation_functions[[i]])
  })

  list(observations = observations, design_points = design_points)
}




### Functional Transformation and Analysis Functions
# Function to generate a time-specific function based on scores and eigenfunctions
function_generator_bm <- function(mean_fn, scores, eigen_fns) {
  function(time) {
    specific <- apply(scores, 1, function(row) sum(mapply(FUN = function(eigen_fn, elem) eigen_fn(time) * elem, eigen_fns, row)))
    common <- mean_fn(time)
    return(common + specific)
  }
}

function_generator_bm <- function(mean_fn, scores, eigen_fns) {
  apply(scores, 1, function(score_row) {
    function(time) {
      specific <- sum(mapply(FUN = function(eigen_fn, score) eigen_fn(time) * score, eigen_fns, score_row))
      common <- mean_fn(time)
      return(common + specific)
    }
  })
}


# Function to compute the empirical mean of a set of functions
empirical_mean_function <- function(functions) {
  function(time) {
    values_at_time <- sapply(functions, function(f) f(time))
    return(mean(values_at_time))
  }
}


observation_function_generator <- function(functions, error_fn) {
  lapply(functions, function(f) {
    function(time) {
      function_value <- f(time)
      error <- error_fn(time)
      return(function_value + error)
    }
  })
}


### Monte Carlo and Statistical Functions
# Function to approximate the norm of the squared difference between empirical and true mean functions
mc_norm <- function(mc_samples, empirical_mean_fn, mean_fn) {
  sampled_norms <- sapply(mc_samples, function(time) {(empirical_mean_fn(time) - mean_fn(time))^2})
  return(mean(sampled_norms))
}


# Function to test the norm of the difference between empirical and true means
test_norm <- function(num_functions, mc_samples, empirical_mean_fn, mean_fn) {
  return(num_functions * mc_norm(mc_samples, empirical_mean_fn, mean_fn))
}

# Function to generate a realization of the null hypothesis mean
mean_null_realization <- function(eigen_values) {
  random_variables <- rchisq(length(eigen_values), df = 1)
  sum(eigen_values * random_variables)
}

# Function to compute quantiles using Monte Carlo simulations
mc_quantiles <- function(mc_samples, eigen_values, probabilities) {
  simulated_sums <- replicate(mc_samples, mean_null_realization(eigen_values))
  quantile(simulated_sums, probabilities)
}


### Sampling and Replication Functions
# Function to sample random times uniformly within a specified range
sample_times <- function(n_samples, min_time, max_time) {
  random_samples <- runif(n_samples, min = min_time, max = max_time)
  return(sort(random_samples))
}

grid_times <- function(n_samples, min_time, max_time) {
  seq(min_time, max_time, length.out = n_samples)
}

# Function to replicate the test norm, generating new samples and functions each time
replicate_test_norm <- function(num_times, time_sampler, num_samples, eigen_values, eigen_functions, mean_fn, test_mean_fn = NULL) {
  if (is.null(test_mean_fn)) {
    test_mean_fn <- mean_fn
  }

  # Generate new sampled times for each replication
  sampled_times <- time_sampler(num_times, 0, 1)

  # Generate new scores and functions for each empirical mean function
  scores <- score_generator(num_samples, eigen_values)
  functions <- function_generator_bm(mean_fn, scores, eigen_functions)
  observation_functions <- observation_function_generator(functions, error_fn)
  
  empirical_mean_fn <- empirical_mean_function(functions)

  # Compute test norm
  # test_norm(num_samples, sampled_times, empirical_mean_fn, test_mean_fn)
}

replicate_test_norm(K_TIMES, grid_times, N_SAMPLES, eigen_values, eigen_functions, zero_mean_fn)

### Main Execution
# Brownian Motion simulation setup
eigen_values <- eigen_values_bm(J_BASES)
eigen_functions <- eigen_functions_bm(J_BASES)

# Parallel processing setup
library(parallel)
num_cores <- detectCores() * 2 / 3


zero_test_norm_results <- mclapply(seq_len(R_REPLICATIONS), function(i) replicate_test_norm(K_TIMES, grid_times, N_SAMPLES, eigen_values, eigen_functions, zero_mean_fn), mc.cores = num_cores)
sinusoidal_test_norm_results <- mclapply(seq_len(R_REPLICATIONS), function(i) replicate_test_norm(K_TIMES, grid_times, N_SAMPLES, eigen_values, eigen_functions, sinusoidal_mean_fn), mc.cores = num_cores)
sinusoidal_on_zero_test_norm_results <- mclapply(seq_len(R_REPLICATIONS), function(i) replicate_test_norm(K_TIMES, grid_times, N_SAMPLES, eigen_values, eigen_functions, sinusoidal_mean_fn, zero_mean_fn), mc.cores = num_cores)


# Generate a sequence of probabilities
probs_seq <- seq(0, 1, by = 0.01)

# Calculating and comparing quantiles
quantiles <- mc_quantiles(MC_SAMPLES, eigen_values, probs_seq)


zero_results <- sapply(quantiles, function(quantile) mean(zero_test_norm_results <= quantile))
zero_results

sinusoidal_results <- sapply(quantiles, function(quantile) mean(sinusoidal_test_norm_results <= quantile))
sinusoidal_results

sinusoidal_on_zero_results <- sapply(quantiles, function(quantile) mean(sinusoidal_on_zero_test_norm_results <= quantile))
sinusoidal_on_zero_results


# Calculate theoretical & empirical quantiles of test_norm_results
zero_empirical_quantiles <- quantile(unlist(zero_test_norm_results), probs = probs_seq)
sinusoidal_empirical_quantiles <- quantile(unlist(sinusoidal_test_norm_results), probs = probs_seq)
sinusoidal_on_zero_empirical_quantiles <- quantile(unlist(sinusoidal_on_zero_test_norm_results), probs = probs_seq)
theoretical_quantiles <- mc_quantiles(MC_SAMPLES, eigen_values, probs_seq)

# Creating the QQ plots
plot(theoretical_quantiles, zero_empirical_quantiles, main = "QQ Plot for Zero Mean", xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", pch = 19)
abline(0, 1, col = "red")

plot(theoretical_quantiles, sinusoidal_empirical_quantiles, main = "QQ Plot for Sinusoidal Mean", xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", pch = 19)
abline(0, 1, col = "red")

plot(theoretical_quantiles, sinusoidal_on_zero_empirical_quantiles, main = "QQ Plot for Sinusoidal Mean Testing Zero Mean", xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", pch = 19)
abline(0, 1, col = "red")
