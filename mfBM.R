D <- function(x, y) {
  nom <- gamma(2 * x + 1) * sin(pi * x) * gamma(2 * y + 1) * sin(pi * y)
  nom <- sqrt(nom)
  denom <- 2 * gamma(x + y + 1) * sin(pi * (x + y) / 2)

  res <- nom / denom
  return(res)
}

cov <- function(s, t, hurst_s, hurst_t) {
  Hs <- hurst_s(s)
  Ht <- hurst_t(t)
  H <- Hs + Ht
  term1 <- D(Hs, Ht)
  term2 <- abs(s)^H + abs(t)^H - abs(t - s)^H
  res <- term1 * term2
  return(res)
}

sample_times <- function(n_samples, min_time, max_time) {
  # Generate n_samples random numbers uniformly between min_time and max_time
  random_samples <- runif(n_samples, min = min_time, max = max_time)
  return(sort(random_samples))
}


vectorized_mu <- function(sample_times, mu_fn) {
  sapply(sample_times, mu_fn)
}


covariance_matrix <- function(sample_times, hurst_s, hurst_t) {
  n <- length(sample_times)
  cov_matrix <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      cov_matrix[i, j] <- cov(sample_times[i], sample_times[j], hurst_s, hurst_t)
      cov_matrix[j, i] <- cov_matrix[i, j]  # Exploit symmetry
    }
  }

  return(cov_matrix)
}

plot_curves <- function(times, curves) {
  # Check if curves is a matrix or a vector
  if (is.matrix(curves)) {
    # Determine the range for the y-axis
    y_min <- min(sampled_curves)
    y_max <- max(sampled_curves)


    # If it's a matrix, plot the first curve and then add the others
    plot(times, curves[1, ], type = "l", ylim = c(y_min, y_max),
         xlab = "Time", ylab = "Value", main = "Sampled Curves")
    if (n_curves > 1) {
      for (i in 2:n_curves) {
        lines(times, curves[i, ], col = i)
      }
    }
  } else {
    # If it's a vector (only one curve), plot it directly
    plot(times, curves, type = "l",
         xlab = "Time", ylab = "Value", main = "Sampled Curve")
  }
}

mu0 <- function(t) {
  0
}


hurst1 <- function(t) {
  # Adjust these parameters to change the behavior of the function
  amplitude <- 0.1
  frequency <- 0.1

  # Compute Hurst exponent
  H <- 0.5 + amplitude * sin(frequency * t)

  # Ensure H stays within reasonable bounds (0 to 1)
  return(min(max(H, 0), 1))
}

library(MASS)

sampled_times <- sample_times(100, 0, 20)

mu_vector <- vectorized_mu(sampled_times, mu0)
cov_matrix <- covariance_matrix(sampled_times, hurst1, hurst1)

n_curves <- 6
sampled_curves <- MASS::mvrnorm(n_curves, mu_vector, cov_matrix)
plot_curves(sampled_times, sampled_curves)
