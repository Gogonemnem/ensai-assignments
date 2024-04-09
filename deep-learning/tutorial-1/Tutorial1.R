# Tutorial for the lecture on Rosenblatt's Perceptron and back-propagation
# Author: Pavlo Mozharovskyi
# Date: 09/11/2023

# 1. Rosenblatt's Perceptron ###################################################

# 1.a. Plot iris data ##########################################################
# Load iris data
data(iris)
iris.setosa = iris[iris[,5] == "setosa",]
iris.versicolor = iris[iris[,5] == "versicolor",]
iris.virginica = iris[iris[,5] == "virginica",]
# Plot pure iris data
plot(rbind(iris.setosa, iris.versicolor)[,1:2], 
     xlab = "Sepal length", ylab = "Sepal width", 
     main = "Iris data")
points(iris.setosa[,1:2], col = "blue", pch = 19)
points(iris.versicolor[,1:2], col = "red", pch = 19)

# 1.b. Program the perceptron ##################################################
# See file "perceptron.R"
source("perceptron.R")

# 1.c. Separate iris data ######################################################
source("filledContour2.R")
# Prepare data for perceptron
X.iris <- as.matrix(rbind(iris.setosa[,1:2], iris.versicolor[,1:2]))
y.iris <- c(rep(0, nrow(iris.setosa)), rep(1, nrow(iris.versicolor)))
# Train the perceptron
w.init <- c(3, -0.5)
b.init <- -19
machine.perceptron <- perceptron.train(X.iris, y.iris, num.epochs = 1000, 
                                       learning.rate = 0.01,
                                       w.init = w.init, b.init = b.init)
cat(nrow(machine.perceptron$hist$weights) - 1, "weight correctoins made\n")
cat("Last correction during epoch", max(machine.perceptron$hist$epoch), "\n")
# Determine plot borders
mins <- c(min(X.iris[,1]), min(X.iris[,2]))
maxs <- c(max(X.iris[,1]), max(X.iris[,2]))
perStretch <- 0.04
mins[1] <- mins[1] - perStretch * (maxs[1] - mins[1])
mins[2] <- mins[2] - perStretch * (maxs[2] - mins[2])
maxs[1] <- maxs[1] + perStretch * (maxs[1] - mins[1])
maxs[2] <- maxs[2] + perStretch * (maxs[2] - mins[2])
# Create input grid
frequency <- 250
gx <- seq(mins[1], maxs[1], length=frequency)
gy <- seq(mins[2], maxs[2], length=frequency)
X.grid <- matrix(as.numeric(as.matrix(expand.grid(gx, gy))), ncol = 2)
# Predic the output for each point of the grid
prediction.perceptron <- perceptron.classify(X.grid, machine.perceptron$w, 
                                             machine.perceptron$b)
# Fill the class-colored areas and add points
filled.contour2(gx, gy, 
                matrix(prediction.perceptron, nrow=length(gx), ncol=length(gy), 
                       byrow=FALSE), 
                levels=c(-0.2,-1:10/10,1.2),
                xlab = "Sepal length", ylab = "Sepal width", 
                xlim = c(mins[1], maxs[1]), ylim = c(mins[2], maxs[2]), 
                main = "Iris data: discriminating rule of the perceptron")
# Add points to the plot
points(iris.setosa[,1:2], col = "blue", pch = 19)
points(iris.versicolor[,1:2], col = "red", pch = 19)

# 1.d Plot the noixy XOR data ##################################################
library(MASS)
# Generate noisy XOR data
n <- 100
set.seed(1)
X.xor <- rbind(mvrnorm(n / 4, mu = c(0, 0), Sigma = diag(2) * 0.01), 
                mvrnorm(n / 4, mu = c(0, 1), Sigma = diag(2) * 0.01), 
                mvrnorm(n / 4, mu = c(1, 0), Sigma = diag(2) * 0.01), 
                mvrnorm(n / 4, mu = c(1, 1), Sigma = diag(2) * 0.01))
y.xor <- c(rep(0, n / 4), rep(1, n / 4), rep(1, n / 4), rep(0, n / 4))
# Plot pure noisy XOR data
plot(X.xor, 
     xlab = "x1", ylab = "x2", 
     main = "Noisy XOR data")
points(X.xor[y.xor == 0,1:2], col = "blue", pch = 19)
points(X.xor[y.xor == 1,1:2], col = "red", pch = 19)

# 1.e. Program the generalized perceptron ######################################
# See file "perceptron2.R"
library(gtools)
source("perceptron2.R")

# 1.f. Plot quadratic separating rule ##########################################
# Train the perceptron
machine.perceptron2 <- perceptron2.train(X.xor, y.xor, r = 2, 
                                         num.epochs = 1000, 
                                         learning.rate = 0.1)
cat(nrow(machine.perceptron2$hist$weights) - 1, "weight correctoins made\n")
cat("Last correction during epoch", max(machine.perceptron2$hist$epoch), "\n")
# Determine plot borders
mins <- c(min(X.xor[,1]), min(X.xor[,2]))
maxs <- c(max(X.xor[,1]), max(X.xor[,2]))
perStretch <- 0.04
mins[1] <- mins[1] - perStretch * (maxs[1] - mins[1])
mins[2] <- mins[2] - perStretch * (maxs[2] - mins[2])
maxs[1] <- maxs[1] + perStretch * (maxs[1] - mins[1])
maxs[2] <- maxs[2] + perStretch * (maxs[2] - mins[2])
# Create input grid
frequency <- 250
gx <- seq(mins[1], maxs[1], length=frequency)
gy <- seq(mins[2], maxs[2], length=frequency)
X.grid <- matrix(as.numeric(as.matrix(expand.grid(gx, gy))), ncol = 2)
# Predic the output for each point of the grid
prediction.perceptron2 <- perceptron2.classify(X.grid, r = 2, 
                                               machine.perceptron2$w, 
                                               machine.perceptron2$b)
# Fill the class-colored areas and add points
filled.contour2(gx, gy, 
                matrix(prediction.perceptron2, nrow=length(gx), ncol=length(gy),
                       byrow=FALSE), 
                levels=c(-0.2,-1:10/10,1.2),
                xlab = "x1", ylab = "x2", 
                xlim = c(mins[1], maxs[1]), ylim = c(mins[2], maxs[2]), 
                main = paste("Noisy XOR data: the generalized perceptron"))
# Add points to the plot
points(X.xor[y.xor == 0,1:2], col = "blue", pch = 19)
points(X.xor[y.xor == 1,1:2], col = "red", pch = 19)

# 2. Classification with a multilayer perceptron ###############################

# 2.a. A single neuron for location-shift model ################################
# (AF: shift the data further)
# (AF: scikit learn, Pythorch)
library(neuralnet)
library(NeuralNetTools)
source("distributionsDDP.r")
# Generate data
set.seed(1)
data1 <- Normal1(100, 500)
dataTrain1 <- data.frame(data1$learn)
names(dataTrain1) <- c("X1", "X2", "Y")
# Train a single-neuron neural network
machine.neuralnet1 <- neuralnet(Y ~ X1 + X2, dataTrain1, hidden = 0, 
                                algorithm = "backprop", learningrate = 0.001, 
                                err.fct = "sse", act.fct = "logistic")
# Plot its architecture
plotnet(machine.neuralnet1, main = "s")
# Determine plot borders
mins <- c(min(dataTrain1[,1]), min(dataTrain1[,2]))
maxs <- c(max(dataTrain1[,1]), max(dataTrain1[,2]))
perStretch <- 0.04
mins[1] <- mins[1] - perStretch * (maxs[1] - mins[1])
mins[2] <- mins[2] - perStretch * (maxs[2] - mins[2])
maxs[1] <- maxs[1] + perStretch * (maxs[1] - mins[1])
maxs[2] <- maxs[2] + perStretch * (maxs[2] - mins[2])
# Create input grid
frequency <- 250
gx <- seq(mins[1], maxs[1], length=frequency)
gy <- seq(mins[2], maxs[2], length=frequency)
X.grid <- matrix(as.numeric(as.matrix(expand.grid(gx, gy))), ncol = 2)
# Predic the output for each point of the grid
prediction.neuralnet1 <- round(compute(machine.neuralnet1, 
                                       X.grid)$net.result > 0.5)
# Fill the class-colored areas and add points
filled.contour2(gx, gy, 
                matrix(prediction.neuralnet1, nrow=length(gx), ncol=length(gy),
                       byrow=FALSE), 
                levels=c(-0.2,-1:10/10,1.2),
                xlab = "x1", ylab = "x2", 
                xlim = c(mins[1], maxs[1]), ylim = c(mins[2], maxs[2]), 
                main = paste("Normal1 data: discriminating rule",
                             "of a single neuron"))
# Add points to the plot
points(dataTrain1[dataTrain1[,3] == 0,1:2], col = "blue", pch = 19)
points(dataTrain1[dataTrain1[,3] == 1,1:2], col = "red", pch = 19)

# 2.b. Linear discriminant analysis for location-shift model ###################
# (AF: optional exercise)

# Discriminating direction of the LDA
machine.lda <- lda(Y ~ ., dataTrain1, method = "moment")
normal.lda <- c(machine.lda$scaling["X1",], machine.lda$scaling["X2",]) / 
  sqrt(machine.lda$scaling["X1",]^2 + machine.lda$scaling["X2",]^2)
normal.neuralnet1 <- c(machine.neuralnet1$result.matrix["X1.to.Y",], 
                       machine.neuralnet1$result.matrix["X2.to.Y",]) / 
  sqrt(machine.neuralnet1$result.matrix["X1.to.Y",]^2 + 
         machine.neuralnet1$result.matrix["X2.to.Y",]^2)
cat("For the LDA normal vector is:\n", round(normal.lda, 6), "\n")
cat("For the neuron normal vector is:\n", round(normal.neuralnet1, 6), "\n")
cat("The angle between the two normals is:", 
    round(acos(sum(normal.lda * normal.neuralnet1)) * 180 / pi, 6), "degree\n")

# 2.c. Two-layer neural network for location-scale alternative #################
# Generate data
set.seed(1)
data2 <- Normal2(100, 500)
dataTrain2 <- data.frame(data2$learn)
names(dataTrain2) <- c("X1", "X2", "Y")
# Train a two-layer neural network
machine.neuralnet2 <- neuralnet(Y ~ X1 + X2, dataTrain2, hidden = 5, rep = 1,
                                algorithm = "backprop", learningrate = 0.005, 
                                err.fct = "sse", act.fct = "logistic", 
                                lifesign = "minimal")
# Plot its architecture
plotnet(machine.neuralnet2)
# Determine plot borders
mins <- c(min(dataTrain2[,1]), min(dataTrain2[,2]))
maxs <- c(max(dataTrain2[,1]), max(dataTrain2[,2]))
perStretch <- 0.04
mins[1] <- mins[1] - perStretch * (maxs[1] - mins[1])
mins[2] <- mins[2] - perStretch * (maxs[2] - mins[2])
maxs[1] <- maxs[1] + perStretch * (maxs[1] - mins[1])
maxs[2] <- maxs[2] + perStretch * (maxs[2] - mins[2])
# Create input grid
frequency <- 250
gx <- seq(mins[1], maxs[1], length=frequency)
gy <- seq(mins[2], maxs[2], length=frequency)
X.grid <- matrix(as.numeric(as.matrix(expand.grid(gx, gy))), ncol = 2)
# Predic the output for each point of the grid
prediction.neuralnet2 <- round(compute(machine.neuralnet2, 
                                       X.grid)$net.result > 0.5)
# Fill the class-colored areas and add points
filled.contour2(gx, gy, 
                matrix(prediction.neuralnet2, nrow=length(gx), ncol=length(gy),
                       byrow=FALSE), 
                levels=c(-0.2,-1:10/10,1.2),
                xlab = "x1", ylab = "x2", 
                xlim = c(mins[1], maxs[1]), ylim = c(mins[2], maxs[2]), 
                main = paste("Normal2 data: discriminating rule",
                             "of a two-layer perceptron"))
# Add points to the plot
points(dataTrain2[dataTrain2[,3] == 0,1:2], col = "blue", pch = 19)
points(dataTrain2[dataTrain2[,3] == 1,1:2], col = "red", pch = 19)

# 2.d. QDA for location-scale alternative ######################################
machine.qda <- qda(Y ~ ., dataTrain2, method = "moment")
prediction.qda <- predict(machine.qda, data.frame(X.grid))$class
gz.qda <- as.numeric(levels(prediction.qda))[prediction.qda]
contour(gx, gy, 
        matrix(gz.qda, nrow=length(gx), ncol=length(gy),
               byrow=FALSE), drawlabels = FALSE, 
        levels=c(-0.2,-1:10/10,1.2), add = TRUE)

# 3. Prediction of continuous outputs ##########################################

# 3.a. Prediction of the sine function #########################################
set.seed(1)
# Generate training data
n <- 100
X1 <- runif(n, 0, 2 * pi)
y1 <- sin(X1) + rnorm(n, 0, 0.1)
plot(X1, y1, xlim = c(0, 6 * pi), xlab = "x", ylab = "y")
grid()
# Train the network
funcTrain1 <- data.frame(cbind(X1, y1))
names(funcTrain1) <- c("X1", "Y")
# Train a three-layer neural network
machine.neuralnet3 <- neuralnet(Y ~ X1, funcTrain1, hidden = c(25, 25, 1), 
                                algorithm = "backprop", learningrate = 0.0001, 
                                err.fct = "sse", 
                                act.fct = function(x){exp(-x^2)}, 
                                lifesign = "full")
# Plot its architecture
plotnet(machine.neuralnet3)
# Predict on the new interval
plot(X1, y1, xlim = c(0, 6 * pi), xlab = "x", ylab = "y")
grid()
X1.pred <- -100:round(7 * pi * 100)/100
funcTest1 <- data.frame(X1 = X1.pred)
lines(X1.pred, sin(X1.pred), col = "red", lty = 2)
y1.pred <- compute(machine.neuralnet3, funcTest1)$net.result
lines(X1.pred, y1.pred)
legend(15, 1.1, c("true", "predicted"), lty = c(2, 1), col = c("red", "black"))

# 3.b. Prediction of a 3D-surface ##############################################
library(lattice)
screen.setting = list(z = -55, x = -75, y = 0)
set.seed(1)
# Generate a sample of points
n <- 100
X2 <- cbind(runif(n), runif(n))
Y2 <- cos(sqrt(rowSums(X2^2)) * pi / sqrt(2)) + rnorm(n, sd = 0.05)
funcTrain2 <- data.frame(cbind(X2, Y2))
colnames(funcTrain2) <- c("X1", "X2", "Y")
# Plot generated points
cloud(Y ~ X1 * X2, funcTrain2,
      scales = list(arrows = FALSE, col = "black", font = 10),
      col = "black",
      drape = FALSE,
      xlab = list(expression(x[1]), rot = 0),
      ylab = list(expression(x[2]), rot = 0),
      zlab = list("y", rot = 90),
      shades = FALSE,
      colorkey = FALSE,
      screen = screen.setting,
      main = "Sample points",
      xlim = c(0, 1), ylim = c(0, 1), zlim = c(-1, 1))
# Train a three-layer neural network
machine.neuralnet4 <- neuralnet(Y ~ X1 + X2, funcTrain2, hidden = c(5, 1), 
                                algorithm = "backprop", learningrate = 0.0001, 
                                stepmax = 1e+6, 
                                err.fct = "sse", 
                                act.fct = function(x){exp(-x^2)}, 
                                lifesign = "full")
# Plot its architecture
plotnet(machine.neuralnet4)
# Determine limits and grid frequencies
x1lim <- c(0, 1)
x2lim <- c(0, 1)
x1num <- 50
x2num <- 50
# Generate grid
x1 <- seq(x1lim[1], x1lim[2], length.out = x1num)
x2 <- seq(x2lim[1], x2lim[2], length.out = x2num)
X2.pred <- as.matrix(expand.grid(x1, x2))
# Compute predicted function values for grid's points
y2.pred <- compute(machine.neuralnet4, X2.pred)$net.result
surfaceData <- data.frame(cbind(X2.pred, y2.pred))
colnames(surfaceData) <- c("X1", "X2", "Y")
# With wireframe
wireframe(Y ~ X1 + X2, surfaceData,
          scales = list(arrows = FALSE, col = "black", font = 10),
          col = "black",
          drape = FALSE,
          xlab = list(expression(x[1]), rot = 0),
          ylab = list(expression(x[2]), rot = 0),
          zlab = list("y", rot = 90),
          shades = FALSE,
          colorkey = FALSE,
          screen = screen.setting,
          main = "Predicted surface",
          xlim = c(0, 1), ylim = c(0, 1), zlim = c(-1, 1))
