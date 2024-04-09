####################
# QUESTION 1       #
####################

##### QUESTION a
data(iris)

subset_iris <- subset(iris, Species %in% c("setosa", "versicolor"))

l <- subset_iris$Sepal.Length
w <- subset_iris$Sepal.Width
species <- subset_iris$Species

plot(l, w, pch = 19, col = species)


##### Question b
covariates <- cbind(l, w)
target <- as.numeric(species) - 1

perceptron.train <- function(covariates, target, alpha, initial_weights, initial_bias, max_epochs) {
  alpha <- ifelse(missing(alpha), 0.01, alpha)
  weights <- ifelse(missing(initial_weights), numeric(ncol(covariates)), initial_weights)
  bias <- ifelse(missing(initial_bias), 0, initial_bias)
  max_epochs <- ifelse(missing(max_epochs), 10, max_epochs)

  for (epoch in 1:max_epochs) {
    for (i in seq_len(nrow(covariates))) {
      x <- covariates[i, ]
      v <- sum(x * weights) + bias
      prediction <- as.numeric(v > 0)
      delta <- target[i] - prediction
      weights <- weights + alpha * delta * x
      bias <- bias + alpha * delta
    }
  }

  return(list(weights = weights, bias = bias))
}

perceptron.classify <- function(covariates, weights, bias) {
  output <- rowSums(sweep(covariates, 2, weights, `*`)) + bias
  prediction <- as.numeric(output > 0)
  return(prediction)
}


set.seed(2023)
random_indices <- sample(nrow(covariates))

shuffled_covariates <- covariates[random_indices, ]
shuffled_target <- target[random_indices]

model <- perceptron.train(shuffled_covariates, shuffled_target, alpha = 0.01, max_epochs = 10)
pred <- perceptron.classify(covariates, model$weights, model$bias)

expand.grid()
filled.contour2()


##### Question c
x_range <- seq(min(l) - .5, max(l) + .5, length.out = 200)
y_range <- seq(min(w) - .5, max(w) + .5, length.out = 200)

grid <- expand.grid(x = x_range, y = y_range)
grid_predictions <- matrix(
  perceptron.classify(as.matrix(grid), model$weights, model$bias),
  nrow = length(x_range), ncol = length(y_range)
)

transparent_palette <- # Red and blue with transparency

  filled.contour(x_range, y_range, grid_predictions,
    color.palette = colorRampPalette(c("red", "blue")),
    xlab = "l", ylab = "w",
    plot.title = title(main = "Perceptron Decision Boundary", xlab = "l", ylab = "w"),
    key.title = title(main = "Predictions")
  )
points(l, w, col = ifelse(target == 1, "green", "yellow"), pch = 20, cex = 5)


##### Question d
library(MASS)
n <- 100
cov <- matrix(c(0.01, 0, 0, 0.01), ncol = 2)

group1 <- MASS::mvrnorm(n / 4, mu = c(0, 0), Sigma = cov)
group2 <- MASS::mvrnorm(n / 4, mu = c(1, 1), Sigma = cov)
group3 <- MASS::mvrnorm(n / 4, mu = c(0, 1), Sigma = cov)
group4 <- MASS::mvrnorm(n / 4, mu = c(1, 0), Sigma = cov)

labels <- c(rep(0, n / 2), rep(1, n / 2))

data <- rbind(group1, group2, group3, group4)
data_with_labels <- cbind(data, labels)

plot(x1, x2, pch = 19, col = as.factor(labels), cex = 5)

random_indices <- sample(nrow(data_with_labels))
shuffled_data <- data_with_labels[random_indices, ]

random_covariates <- shuffled_data[, -ncol(shuffled_data)]
random_targets <- shuffled_data[, ncol(shuffled_data)]


##### Question e
poly.covariates <- function(covariates) {
  print(ncol)
  poly_covariates <- as.data.frame(covariates)
  ncols <- ncol(poly_covariates)

  # Add interaction terms
  for (i in 1:(ncols - 1)) {
    for (j in (i + 1):ncols) {
      new_col_name <- paste0(names(poly_covariates)[i], "_x_", names(poly_covariates)[j])
      poly_covariates[[new_col_name]] <- poly_covariates[[i]] * poly_covariates[[j]]
    }
  }

  # Add squared terms
  for (i in 1:ncols) {
    poly_covariates[[paste0("sq_", names(poly_covariates)[i])]] <- poly_covariates[[i]]^2
  }
  return(as.matrix(poly_covariates))
}


perceptron2.train <- function(covariates, target, alpha, initial_weights, initial_bias, max_epochs) {
  poly_covariates <- poly.covariates(covariates)
  model <- perceptron.train(poly_covariates, target, alpha, initial_weights, initial_bias, max_epochs)
  return(model)
}

perceptron2.classify <- function(covariates, weights, bias) {
  poly_covariates <- poly.covariates(covariates)
  prediction <- perceptron.classify(poly_covariates, weights, bias)
  return(prediction)
}


##### Question f
model <- perceptron2.train(random_covariates, random_targets, alpha = 0.01, max_epochs = 100)
perceptron2.classify(random_covariates, model$weights, model$bias)


x_range <- seq(min(x1) - .5, max(x1) + .5, length.out = 200)
y_range <- seq(min(x2) - .5, max(x2) + .5, length.out = 200)

grid <- expand.grid(x = x_range, y = y_range)
grid_predictions <- matrix(
  perceptron2.classify(as.matrix(grid), model$weights, model$bias),
  nrow = length(x_range), ncol = length(y_range)
)

filled.contour(x_range, y_range, grid_predictions,
  color.palette = colorRampPalette(c("red", "blue")),
  plot.title = title(main = "Perceptron Decision Boundary", xlab = "x1", ylab = "x2"),
  key.title = title(main = "Pred")
)
points(x1, x2, col = ifelse(labels == 1, "green", "yellow"), pch = 20, cex = 5)


####################
# QUESTION 2       #
####################

# Question a
library(neuralnet)
library(NeuralNetTools)
n <- 100
cov <- matrix(c(1, 1, 1, 4), ncol = 2)

set.seed(2023)
group1 <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = cov)
group2 <- MASS::mvrnorm(n, mu = c(1, 1), Sigma = cov)
labels <- c(rep(0, n), rep(1, n))

data <- rbind(group1, group2)
colnames(data) <- c("X1", "X2")
data_with_labels <- cbind(data, labels)
data_with_labels_df <- as.data.frame(data_with_labels)
formula <- labels ~ X1 + X2

set.seed(2023)
nn <- neuralnet::neuralnet(formula, data = data_with_labels_df, hidden = 0, linear.output = FALSE, algorithm='backprop', learningrate = 0.001)
plotnet(nn)


x1_range <- seq(min(data_with_labels_df$X1), max(data_with_labels_df$X1), length.out = 200)
x2_range <- seq(min(data_with_labels_df$X2), max(data_with_labels_df$X2), length.out = 200)

grid <- expand.grid(X1 = x1_range, X2 = x2_range)
grid_predictions_nn <- predict(nn, newdata = grid)
prediction_matrix_nn <- matrix(grid_predictions_nn, nrow = length(x1_range), ncol = length(x2_range))


filled.contour(x1_range, x2_range, prediction_matrix_nn,
               color.palette = colorRampPalette(c("blue", "red")),
               xlab = "X1", ylab = "X2",
               plot.title = title(main = "NN Decision Boundary"))
points(data_with_labels_df$X1, data_with_labels_df$X2, pch = 19, col = as.factor(labels), cex = 5)


##### Question b
lda <- MASS::lda(formula, data = data_with_labels_df)
grid_predictions_lda <- predict(lda, newdata = grid)
prediction_matrix_lda <- matrix(grid_predictions_lda$posterior[,2], nrow = length(x1_range), ncol = length(x2_range))

filled.contour(x1_range, x2_range, prediction_matrix_lda, 
               color.palette = colorRampPalette(c("blue", "red")),
               xlab = "X1", ylab = "X2",
               plot.title = title(main = "LDA Decision Boundary"))
points(data_with_labels_df$X1, data_with_labels_df$X2, pch = 19, col = as.factor(labels), cex = 5)


nn_normal_vector <- nn$weights[[1]][[1]][2:3, 1]
lda_normal_vector <- lda$scaling[1:2]

angle_rad <- acos(sum(nn_normal_vector * lda_normal_vector) / 
                  (sqrt(sum(nn_normal_vector^2)) * sqrt(sum(lda_normal_vector^2))))
angle_deg <- angle_rad * (180 / pi)
angle_deg


##### Question c
n <- 100
cov1 <- matrix(c(1, 1, 1, 4), ncol = 2)
cov2 <- matrix(c(4, 4, 4, 16), ncol = 2)

set.seed(2023)
group1 <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = cov1)
group2 <- MASS::mvrnorm(n, mu = c(1, 1), Sigma = cov2)
labels <- c(rep(0, n), rep(1, n))

data <- rbind(group1, group2)
colnames(data) <- c("X1", "X2")
data_with_labels <- cbind(data, labels)
data_with_labels_df <- as.data.frame(data_with_labels)
formula <- labels ~ X1 + X2

set.seed(2023)
nn1 <- neuralnet::neuralnet(formula, data = data_with_labels_df, hidden = 3, linear.output = FALSE,
                            algorithm='backprop', learningrate = 0.01, stepmax = 1e+05)
plotnet(nn1)


x1_range <- seq(min(data_with_labels_df$X1), max(data_with_labels_df$X1), length.out = 200)
x2_range <- seq(min(data_with_labels_df$X2), max(data_with_labels_df$X2), length.out = 200)

grid <- expand.grid(X1 = x1_range, X2 = x2_range)
grid_predictions_nn1 <- predict(nn1, newdata = grid)
prediction_matrix_nn1 <- matrix(grid_predictions_nn1, nrow = length(x1_range), ncol = length(x2_range))

filled.contour(x1_range, x2_range, prediction_matrix_nn1,
               color.palette = colorRampPalette(c("blue", "red")),
               xlab = "X1", ylab = "X2",
               plot.title = title(main = "NN1 Decision Boundary"))
points(data_with_labels_df$X1, data_with_labels_df$X2, pch = 19, col = as.factor(labels), cex = 5)


##### Question d
qda <- MASS::qda(formula, data = data_with_labels_df)
grid_predictions_qda <- predict(qda, newdata = grid)
prediction_matrix_qda <- matrix(grid_predictions_qda$posterior[,2], nrow = length(x1_range), ncol = length(x2_range))

filled.contour(x1_range, x2_range, prediction_matrix_qda,
               color.palette = colorRampPalette(c("blue", "red")),
               xlab = "X1", ylab = "X2",
               plot.title = title(main = "QDA Decision Boundary"))
points(data_with_labels_df$X1, data_with_labels_df$X2, pch = 19, col = as.factor(labels), cex = 5)


####################
# QUESTION 3       #
####################

# Question a
library(neuralnet)

set.seed(2023)
n <- 100
x <- runif(n, 0, 2 * pi)
epsilon <- rnorm(n, mean = 0, sd = 0.1)
y <- sin(x) + epsilon
data <- data.frame(x = x, y = y)
formula <- y ~ x

rbf <- function(x) exp(-x^2)

set.seed(2023)
nn <- neuralnet(formula, data = data, hidden = c(25, 25, 1), stepmax = 1e+06,
                learningrate = 0.0001, algorithm = "backprop", act.fct = rbf)

# Generate a sequence of x-values
x_vals <- seq(0, 6 * pi, length.out = 200)
predict_data <- data.frame(x = x_vals)

# Predict y-values using the neural network
predictions <- compute(nn, predict_data)
predicted_y <- predictions$net.result

plot(data$x, data$y, xlim = c(0, 6 * pi), ylim = c(-2, 2), xlab = "x", ylab = "y",
     main = "Data points and Sine Function", pch = 19, col = "blue")
curve(sin, add = TRUE, col = "red")
lines(x_vals, predicted_y, col = "green", lwd = 2)
plotnet(nn)

# Question b
library(lattice)

set.seed(2023)
n <- 100
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
epsilon <- rnorm(n, mean = 0, sd = 0.05)
y <- cos(sqrt(x1^2 + x2^2) / sqrt(2) * pi) + epsilon
data <- data.frame(x1, x2, y)

formula <- y ~ x1 + x2

lattice::cloud(formula, data = data, pch = 19, drape = TRUE, colorkey = TRUE, screen = list(x = -90, x = 10))

nn <- neuralnet(formula, data = data, hidden = c(25, 25, 25), threshold = 0.01,
                stepmax = 1e+05, algorithm = "rprop+", act.fct = rbf)


grid <- expand.grid(x1 = seq(0, 1, length.out = 100), x2 = seq(0, 1, length.out = 100))
grid$y <- compute(nn, grid)$net.result

lattice::wireframe(formula, data = grid, drape = TRUE, colorkey = TRUE, screen = list(x = -90, x = 10))
