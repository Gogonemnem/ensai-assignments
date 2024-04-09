# Functionality of 'perceptron'
# For tutorial for the lecture on Rosenblatt's Perceptron and back-propagation
# Author: Pavlo Mozharovskyi
# Date: 09/11/2023

# Function for learning the perceptron
perceptron.train <- function(X, y, learning.rate = 1, num.epochs = 1, 
                             w.init = rep(0, ncol(X)), b.init = 0, 
                             ret.hist = TRUE){
  # Obtain data statistics
  n = nrow(X)
  d = ncol(X)
  w = w.init # initialize w
  b = b.init # initialize b
  if (ret.hist){ # prepare historical structure
    hist = list(weights = matrix(w, nrow = 1, ncol = d), 
                bs = b, epoch = 0, iobs = 0)
  }
  # Give the data repeatedly
  for (i in 1:num.epochs){ # for each epoch
    for (j in 1:n){ # for each observatoin
      delta <- round(y[j] - perceptron.classify(X[j,], w, b))
      if (delta != 0){
        w <- w + learning.rate * delta * as.vector(X[j,])
        b <- b + learning.rate * delta
        if (ret.hist){ # save historical record
          hist$weights <- rbind(hist$weights, w)
          hist$bs <- c(hist$bs, b)
          hist$epoch <- c(hist$epoch, i)
          hist$iobs <- c(hist$iobs, j)
        } 
      }
    }
  }
  if (ret.hist){
    return(list(w = w, b = b, hist = hist))
  }else{
    return(list(w = w, b = b))
  }
}

# Function for classification using perceptron rule
perceptron.classify <- function(x, w, b){
  if (is.vector(x)){
    x <- matrix(x, nrow = 1)
  }
  return(round(as.vector(cbind(1, x) %*% t(t(c(b, w)))) > 0))
}
