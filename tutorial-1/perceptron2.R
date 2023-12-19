# Functionality of 'perceptron2'
# For tutorial for the lecture on Rosenblatt's Perceptron and back-propagation
# Author: Pavlo Mozharovskyi
# Date: 09/11/2023

# Function for learning the perceptron
perceptron2.train <- function(X, y, r = 1, learning.rate = 1, num.epochs = 1, 
                              w.init = rep(0, choose(ncol(X) + r, r)), 
                              b.init = 0, 
                              ret.hist = TRUE){
  # Choose combinations for (cross)products
  d <- ncol(X)
  cmbslist <- list()
  new.data <- X
  # Transform data
  if (r > 1){
    for (i in 2:r){
      tmp.perm <- permutations(d, r, repeats.allowed = TRUE)
      tmp.ids <- unique(t(apply(tmp.perm, 1, sort)))
      for (j in 1:nrow(tmp.ids)){
        new.data <- cbind(new.data, apply(X[,tmp.ids[j,]], 1, prod))
      }
    }
  }
  # Call the usual perceptron learning function
  return(perceptron.train(new.data, y, learning.rate, num.epochs, 
                          ret.hist = TRUE))
}

# Function for classification using perceptron rule
perceptron2.classify <- function(x, r = 1, w, b){
  if (is.vector(x)){
    x <- matrix(x, nrow = 1)
  }
  # Choose combinations for (cross)products
  d <- ncol(x)
  cmbslist <- list()
  new.data <- x
  # Transform data
  if (r > 1){
    for (i in 2:r){
      tmp.perm <- permutations(d, r, repeats.allowed = TRUE)
      tmp.ids <- unique(t(apply(tmp.perm, 1, sort)))
      for (j in 1:nrow(tmp.ids)){
        new.data <- cbind(new.data, apply(x[,tmp.ids[j,]], 1, prod))
      }
    }
  }
  # Call the usual perceptron classification function
  return(round(as.vector(cbind(1, new.data) %*% t(t(c(b, w)))) > 0))
}
