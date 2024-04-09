# 10 distributions from the article:
# Li, J., Cuesta-Albertos, J. A., and Liu, R. Y. (2012). 
# DD-classifier: Nonparametric clas- sification procedure based on DD-plot. 
# Journal of the American Statistical Association, 107(498), 737-753.
# To be used in my cours 'Deep Learning'
# Author: Pavlo Mozharovskyi
# Date: 09/11/2023

Normal1 <- function(numLearn, numTest){
  library("MASS")
  l1 <- mvrnorm(numLearn, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  l2 <- mvrnorm(numLearn, c(1,1), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  t1 <- mvrnorm(numTest, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  t2 <- mvrnorm(numTest, c(1,1), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Normal2 <- function(numLearn, numTest){
  library("MASS")
  l1 <- mvrnorm(numLearn, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE))
  l2 <- mvrnorm(numLearn, c(1,1), matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow = TRUE))
  t1 <- mvrnorm(numTest, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE))
  t2 <- mvrnorm(numTest, c(1,1), matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow = TRUE))
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Cauchy1 <- function(numLearn, numTest){
  library("bfp")
  library("MASS")
  l1 <- bfp:::rmvt(n=numLearn, mu=c(0,0), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  l2 <- bfp:::rmvt(n=numLearn, mu=c(1,1), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  t1 <- bfp:::rmvt(n=numTest, mu=c(0,0), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  t2 <- bfp:::rmvt(n=numTest, mu=c(1,1), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Cauchy2 <- function(numLearn, numTest){
  library("bfp")
  library("MASS")
  l1 <- bfp:::rmvt(n=numLearn, mu=c(0,0), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  l2 <- bfp:::rmvt(n=numLearn, mu=c(1,1), sigma=matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  t1 <- bfp:::rmvt(n=numTest, mu=c(0,0), sigma=matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  t2 <- bfp:::rmvt(n=numTest, mu=c(1,1), sigma=matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow=TRUE), df=1)
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

RobNormal1 <- function(numLearn, numTest){
  library("MASS")
  l1c <- mvrnorm(round(numLearn*0.9), c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  l1d <- mvrnorm(numLearn - round(numLearn*0.9), c(10,10), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  l1 <- rbind(l1c, l1d)
  l1 <- l1[sample(1:numLearn, numLearn),]
  l2 <- mvrnorm(numLearn, c(1,1), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  t1 <- mvrnorm(numTest, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  t2 <- mvrnorm(numTest, c(1,1), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

RobNormal2 <- function(numLearn, numTest){
  library("MASS")
  l1c <- mvrnorm(round(numLearn*0.9), c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  l1d <- mvrnorm(numLearn - round(numLearn*0.9), c(10,10), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow = TRUE))
  l1 <- rbind(l1c, l1d)
  l1 <- l1[sample(1:numLearn, numLearn),]
  l2 <- mvrnorm(numLearn, c(1,1), matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow = TRUE))
  t1 <- mvrnorm(numTest, c(0,0), matrix(c(1,1,1,4), nrow = 2, ncol = 2, byrow=TRUE))
  t2 <- mvrnorm(numTest, c(1,1), matrix(c(4,4,4,16), nrow = 2, ncol = 2, byrow = TRUE))
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Exponential1 <- function(numLearn, numTest){
  library("MASS")
  l1 <- cbind(rexp(numLearn, 1), rexp(numLearn, 1))
  l2 <- cbind(rexp(numLearn, 1) + 1, rexp(numLearn, 1) + 1)
  t1 <- cbind(rexp(numTest, 1), rexp(numTest, 1))
  t2 <- cbind(rexp(numTest, 1) + 1, rexp(numTest, 1) + 1)
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Exponential2 <- function(numLearn, numTest){
  library("MASS")
  l1 <- cbind(rexp(numLearn, 1), rexp(numLearn, 0.5))
  l2 <- cbind(rexp(numLearn, 0.5) + 1, rexp(numLearn, 1) + 1)
  t1 <- cbind(rexp(numTest, 1), rexp(numTest, 0.5))
  t2 <- cbind(rexp(numTest, 0.5) + 1, rexp(numTest, 1) + 1)
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Mix1 <- function(numLearn, numTest){
  library("MASS")
  l1 <- matrix(1:(numLearn*2), nrow = numLearn, ncol = 2)
  for(i in 1:numLearn){
    rnd <- round(runif(1))
    if (rnd < 0.5){
      l1[i,1] <- (-1)*abs(rnorm(1)) + 0
    }else{
      l1[i,1] <- 2*abs(rnorm(1)) + 0
    }
    rnd <- round(runif(1))
    if (rnd < 0.5){
      l1[i,2] <- (-1)*abs(rnorm(1)) + 0
    }else{
      l1[i,2] <- 4*abs(rnorm(1)) + 0
    }
  }
  l2 <- matrix(1:(numLearn*2), nrow = numLearn, ncol = 2)
  for(i in 1:numLearn){
    rnd <- round(runif(1))
    if (rnd < 0.5){
      l2[i,1] <- (-1)*abs(rnorm(1)) + 1
    }else{
      l2[i,1] <- 2*abs(rnorm(1)) + 1
    }
    rnd <- round(runif(1))
    if (rnd < 0.5){
      l2[i,2] <- (-1)*abs(rnorm(1)) + 1
    }else{
      l2[i,2] <- 4*abs(rnorm(1)) + 1
    }
  }
  t1 <- matrix(1:(numTest*2), nrow = numTest, ncol = 2)
  for(i in 1:numTest){
    rnd <- round(runif(1))
    if (rnd < 0.5){
      t1[i,1] <- (-1)*abs(rnorm(1)) + 0
    }else{
      t1[i,1] <- 2*abs(rnorm(1)) + 0
    }
    rnd <- round(runif(1))
    if (rnd < 0.5){
      t1[i,2] <- (-1)*abs(rnorm(1)) + 0
    }else{
      t1[i,2] <- 4*abs(rnorm(1)) + 0
    }
  }
  t2 <- matrix(1:(numTest*2), nrow = numTest, ncol = 2)
  for(i in 1:numTest){
    rnd <- round(runif(1))
    if (rnd < 0.5){
      t2[i,1] <- (-1)*abs(rnorm(1)) + 1
    }else{
      t2[i,1] <- 2*abs(rnorm(1)) + 1
    }
    rnd <- round(runif(1))
    if (rnd < 0.5){
      t2[i,2] <- (-1)*abs(rnorm(1)) + 1
    }else{
      t2[i,2] <- 4*abs(rnorm(1)) + 1
    }
  }
  learnData <- rbind(cbind(l1, rep(0, numLearn)),cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)),cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}

Mix2 <- function(numLearn, numTest){
  library("MASS")
  l1 <- mvrnorm(numLearn, c(0,0), matrix(c(1,0,0,1), nrow = 2, ncol = 2, byrow = TRUE))
  l2 <- cbind(rexp(numLearn, 1), rexp(numLearn, 1))
  t1 <- mvrnorm(numTest, c(0,0), matrix(c(1,0,0,1), nrow = 2, ncol = 2, byrow = TRUE))
  t2 <- cbind(rexp(numTest, 1), rexp(numTest, 1))
  learnData <- rbind(cbind(l1, rep(0, numLearn)), cbind(l2, rep(1, numLearn)))
  testData <- rbind(cbind(t1, rep(0, numTest)), cbind(t2, rep(1, numTest)))
  rez <- list(learn = learnData, test = testData)
  return(rez)
}
