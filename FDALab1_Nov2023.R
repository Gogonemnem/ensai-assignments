###########################
#Lab tutorial 1
#First steps in FDA
# 2023-2024
########################################


#######################################
#simulation  of the Wiener process (Brownian motion)
#######################################

setwd("C:/Users/patilea/Labs_FDA_Smarties")
set.seed(12345)
library(fda)
T <- 365
sigma <- 1  # std of the Bm
K <- 100 # nb. elements in the B-splines basis 

#### DGP
Wiener=cumsum(rnorm(T))*sigma     #random walk on [0,T]  
plot.ts(Wiener,xlab="",ylab="")

B.basis=create.bspline.basis(rangeval=c(0,T),nbasis=K)

#smoothing one curve with no penalty
Wiener.fd=smooth.basis(y=Wiener,fdParobj=B.basis)
lines(Wiener.fd,lwd=3,col='blue')
lines(Wiener.fd,lwd=3,col='red')

##################
# Exemples of Bases 
nbasis=5
basis_spline<-create.bspline.basis(c(0,1),nbasis=nbasis,norder=4)
basis_fourier<-create.fourier.basis(c(0,1),nbasis=nbasis)
#dev.new(height=6,width=10)
par(mfrow=c(1,2))
plot(basis_spline)
plot(basis_fourier)

##################
set.seed(1234)
# Simulate several Bm paths (random functions) on [0,1]
N<-100
N_times<-500
Z<-matrix(rnorm(N*N_times),nrow=N,ncol=N_times)
BM<-t(apply(Z,1,cumsum)/sqrt(N_times))

# Fix B-splines basis
K <- 100  #nb of elements in the basis
M <- 4    # degree of the piecewise polynomial -1 
basis<-create.bspline.basis(c(0,1),nbasis=K,norder=M)

# Expand Bm random functions using B-splines basis
Times<-(1:N_times)/N_times
BM_f<-Data2fd(Times,t(BM),basisobj=basis)

#Visualize sample paths and the basis approximation 
#dev.new(height=6,width=10)
plot(BM_f[53])
lines(Times,BM[53,],col="red")

# coefficients are saved in BM_f$coefs
dim(BM_f$coefs)  # the information carried by the N curves is summarized by the matrix of coefficients


################### Mean and variance estimation
# Examine Mean and SD of the process generating the sample paths
Xbar<-mean.fd(BM_f)
SDX<-sd.fd(BM_f)
#dev.new(height=6,width=10)
par(mfrow=c(1,2))
plot(Xbar)
plot(SDX)

# We can add point-wise CI to the mean function as well
SE_low<-fd(coef=Xbar$coef - 2*SDX$coef/sqrt(N),basisobj=basis) 
SE_high<-fd(coef=Xbar$coef + 2*SDX$coef/sqrt(N),basisobj=basis) 
plot(Xbar,ylim=c(-1,1))
lines(SE_low,lty=2,col="red")
lines(SE_high,lty=2,col="red")

# We now visualize the covariance function, a bivariate function 
Chat<-var.fd(BM_f)
eval_times<-seq(0,1,length=50)
Chat_matrix<-eval.bifd(eval_times,eval_times,Chat)

# Perspective Plot
persp(eval_times,eval_times,Chat_matrix)

# Contour Plot
contour(eval_times,eval_times,Chat_matrix)


###################### FPCA
# Estimated FPCA

BM_pca<-pca.fd(BM_f,nharm = 6)
BM_pca$varprop;
 cumsum(BM_pca$varprop)

par(mfrow=c(1,2))
plot(BM_pca$harmonics[1:4])

# Comparison with the theoretical PCs of Brownian Motion
t<-seq(0,1,length=100)
e1<-sqrt(2)*sin(.5*pi*t)
e2<-sqrt(2)*sin(1.5*pi*t)
e3<-sqrt(2)*sin(2.5*pi*t)
e4<-sqrt(2)*sin(3.5*pi*t)
plot(t,e1,type="l",ylim=c(-1.5,1.5),xlab="",ylab="")
points(t,e2,type="l",col=2)
points(t,e3,type="l",col=3)
points(t,e4,type="l",col=4)




##########################################
#Real data application -- BOA (Bank of America) Cumulative Returns
############################################
# See RK2017, section 1.4
# Consider stock values of BOA, recorded every minute, from April 9th, 1997 to April 2nd, 2007.
#Each trading day begins at 9:30 AM (EST) and ends at 4:00 PM
#Thus there are 6.5 hours of trading time. Thus we can take the interval [0,T] = [0,6.5].
#This results in 2511 days worth of data, with each day consisting of 390 measurements. 
#Thefunctional observation we consider is the cumulative log-return.


### Analysis of BOA Data ###
### Cummulative Log Returns ###
BOA<-read.table("BOA.txt",header=TRUE)
Dates<-dimnames(BOA)[[1]]
Times<-dimnames(BOA)[[2]]

N<-dim(BOA)[1]
M<-dim(BOA)[2]
T<-seq(0,6.5,length=M)

BOA<-data.matrix(BOA)
log_BOA<-log(BOA) - matrix(log(BOA)[,1],nrow=N,ncol=M)

bspline_basis<-create.bspline.basis(rangeval=c(0,6.5),norder=4,nbasis=200)
log_BOA_f<-Data2fd(T,t(log_BOA),basisobj = bspline_basis)
plot(log_BOA_f[1:10],xlab="",ylab="",lwd=1.5)

# We notice an outlier #
plot(log_BOA_f)
outlier<-which(abs(log_BOA)==max(abs(log_BOA)),arr.ind=TRUE)[[1]]
log_BOA<-log_BOA[-outlier,]
N<-dim(log_BOA)[1]
M<-dim(log_BOA)[2]

Y_f<-Data2fd(T,t(log_BOA),basisobj = bspline_basis)
plot(Y_f,xlab="",ylab="",lwd=1.5)

# Estimate the mean function #
muhat<-mean.fd(Y_f)
plot(muhat)
#the mean function is positive

#estimate the sd function
sdhat=sd.fd(Y_f)
plot(sdhat)


# We now visualize the covariance function/surface
Chat<-var.fd(Y_f)
eval_times<-seq(0,6.5,length=100)
Chat_matrix<-eval.bifd(eval_times,eval_times,Chat)


# Perspective Plot
persp(eval_times,eval_times,Chat_matrix)

# Contour Plot
contour(eval_times,eval_times,Chat_matrix)


#plot the pointwise confidence intervals
# Add some standard errors to mean #
SE_hat_U<-fd(basisobj=bspline_basis) # create upper SE bound
SE_hat_L<-fd(basisobj=bspline_basis) # create lower SE bound
SE_hat_U$coefs<-2*sdhat$coefs/sqrt(N) + muhat$coefs
SE_hat_L$coefs<- -2*sdhat$coefs/sqrt(N) + muhat$coefs
plot(muhat,ylim=c(-0.001,0.003),col='red',lty=2,xlab="",ylab="")
lines(SE_hat_L,col='blue',lty=1)
lines(SE_hat_U, col="blue",lty=1)



# Now lets examine the PCs
Y_pca<-pca.fd(Y_f,nharm = 6)
Y_pca$varprop; cumsum(Y_pca$varprop)
par(mfrow=c(2,3))
for(i in 1:4){
	plot(Y_pca$harmonics[i],xlab="",ylab="",main=paste("PC",i,sep=""), col=i)
}
#the cumulative returns show behavior similar to Brownian motion

plot(Y_pca$harmonics[1:4])









