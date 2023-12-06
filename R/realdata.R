#' @title Preprocessing of trading volume and price data 
#' @description Preprocessing of trading volume and price data 
#' @param p0 is a matrix with two columns,the first column is the trading volume of the stock,
#' while the second is price of the stock
#' @import zoo
#' @import TSA
#' @import forecast
#' @import DAAG
#' @import Rcpp
#'@import boot
#'@import bootstrap
#'@import coda
#'@import ggplot2
#'@import lmtest
#'@import microbenchmark 
#' @importFrom  zoo plot.zoo
#' @useDynLib SA23204198
#' @examples
#' \dontrun{
#' a=rep(5,times=10)
#' b=abs(runif(10))
#' c=cbind(a,b)
#' dataanalysis(c)
#' }
dataanalysis=function(p0)
{
  price=p0[,2]
realtrade=p0[,1]
num=1
nrow1=as.numeric(nrow(p0))
realtrade1=vector(length=(nrow1-2))
for (i in 1:(nrow1-2))
{
  realtrade1[i]=(realtrade[i])
}


trade=vector(length=(nrow1-2))
for (i in 1:(nrow1-2))
{
  trade[i]=(realtrade[i+2]-realtrade[i+1])}
plot.zoo(realtrade,xlab='second',ylab=' trading volume')
plot.zoo(trade,xlab='second',ylab='change of trading volume')
for (i in 1:nrow1)
{
  price[i]=log(price[i])
}#取对数
plot(price,type='l',xlab = 'second',ylab = 'logprice')#对数时序图

logyieldprice=vector(length=nrow1-2)
for (i in 1:(nrow1-2))
{
  logyieldprice[i]=(price[i+1]-price[i])
}
cbind(trade,logyieldprice)
}

#density of normal distribution for the Vacisek model
dcOU <- function (x, t, x0 , theta , log = FALSE ){
  Ex <- theta [1] / theta [2]+( x0 - theta [1] / theta [2]) * exp (- theta [2] *t)
  Vx <- (theta[3]^2+1)*(1- exp (-2* theta [2] *t))/(2* theta [2])
  dnorm (x, mean =Ex , sd = sqrt (Vx), log = log )
}
#' @title likelihood of parametric estimation for the stochastic diffenrential equation
#' @description Parametric estimation for the Vasicek model using maximum likelihood ofEuler-Maruyama method .
#' The parametric form of the Vasicek model used here is given by
#' \deqn{dX_t = (\thata1 - \theta2 X_t)dt + \theta3 dW_t.}
#' @param theta1 is a parameter
#' @param theta2 is a parameter
#' @param theta3 is a parameter
#' @param factor is a vector of time series
#' @import stats4
#' @importFrom stats dnorm
#' @importFrom stats ts
#' @importFrom stats deltat
#' @examples
#' \dontrun{
#'   z=ts(abs(rnorm(100))
#'   OU.lik(0.5,-0.5,0.3,z)
#' }

  OU.lik <- function ( theta1,theta2,theta3,factor){
    z=ts(factor,deltat=1,start=0)
    n <- length (z)
    dt <- deltat (z)
    -sum ( dcOU (z [2: n], dt , z [1:(n -1)] , c( theta1,theta2,theta3 ), log = TRUE ))
  }
  
  #' @title A dataset used for illustration.
  #' @name p
  #' @description This dataset is used to provide the reallife data 
  #' @examples
  #' \dontrun{
  #' data(p)
  #' attach(p)
  #' }
  NULL
 