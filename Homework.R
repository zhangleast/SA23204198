## -----------------------------------------------------------------------------
mysample=function(n1,size1,probabilty1)
{set.seed(2000)
  sample1=runif(size1)
  sdf1=cumsum(probabilty1)
  sdf2=c(0,sdf1)
   length1=length(n1)
  output=rep(1/length1, times=length1)
for(k in 1:size1)
{
  for(i in 1:length1)
  {
    if(sample1[k]<=sdf2[i+1]&&sample1[k]>sdf2[i])
    {
      output[k]=n1[i]
    }
  }
} 
  ct = as.vector(table(output))
  print(ct/sum(ct)/probabilty1)
  return(output)
}

result1=mysample(1:4,60,c(.2,.2,.3,.3))
print(result1[1:15])
result2=sample(1:4,60,replace=T,prob=c(.2,.2,.3,.3))
print(result2[1:15])
result1=mysample(1:4,1e5,c(.2,.2,.3,.3))



## -----------------------------------------------------------------------------
n=1000
output=seq(from=0,to=0,by=n)
sample2=runif(n)
for(i in 1:n)
{
  if(sample2[i]<1/2)
  {
    output[i]=log(sample2[i]*2)
  }
  if(sample2[i]>=1/2)
  {
    output[i]=-log(2-sample2[i]*2)
  }
}
print(output[1:15])
hist(output, prob = TRUE, main = expression(f(x)==0.5*e^({-abs(x)})))
y <- seq(-5, 5, .01)
lines(y, 0.5*exp({-abs(y)}))


## -----------------------------------------------------------------------------
mybeta=function(a,b,n){
k=0
j=0
y =numeric(n)
while (k < n) {
u1 =runif(1)
j = j + 1
x = runif(1) 
if (dbeta(x,a,b)/2 > u1) {
k = k + 1
y[k]= x
}
}
return(y)
}
set.seed(10000)
beta1=mybeta(3,2,10000)
hist(beta1, prob = TRUE, main = expression(f(x)-Beta(3,2)))
y <- seq(-5, 5, .001)
lines(y, dbeta(y,3,2))

## -----------------------------------------------------------------------------
mykernel=function(n){
k=0
y =numeric(n)
while (k < n) {
u1 =runif(1)
x = runif(1,-1,1) 
if (1/4*(1-x^2) > u1) {
k = k + 1
y[k]= x
}
}
return(y)
}
set.seed(1000)
z=mykernel(1e5)
hist(z, prob = TRUE, main = expression(f(x)==fe(x)))
y <- seq(-5, 5, .0001)
lines(y,3/4*(1-y^2) )

## -----------------------------------------------------------------------------
algkernel=function(n){
k=0
y =numeric(n)
while (k < n) {
x1 =runif(1,-1,1)
x2 = runif(1,-1,1) 
x3=runif(1,-1,1)
if (abs(x3) > abs(x2)&&abs(x3)>abs(x1)) {
k = k + 1
y[k]= x2
}
else
{k = k + 1
y[k]= x3}
}
return(y)
}
l=algkernel(1e5)
hist(l, prob = TRUE, main = expression(f(x)==fe(x)))
y <- seq(-5, 5, .0001)
lines(y,3/4*(1-y^2) )

## -----------------------------------------------------------------------------
set.seed(10000)
pihat=seq(from=1e-4,to=1,by=1e-4)
data1=runif(3)
result=numeric(length=3)
for (i in 1:100)
{i
for(j in 1:3)
{l=data1[j]
d=1
for(k in 1:1e4)
{X =runif(100,0,d/2)
Y =runif(100,0,pi/2)
  pihat[k]= 2*l/d/mean(l/2*sin(Y)>X)}
  result[j]=var(pihat)
  m=which(result==min(result),arr.ind =F)
min1=data1[m]
data1=c(min1*0.5,min1,min(c(2*min1,1)))
}

}
print(data1)


## -----------------------------------------------------------------------------
set.seed(2000)
num=1e5
n1=runif(num)
result1=exp(n1)
mean1=mean(result1)
var1=var(result1)
print(c(mean1,var1))
n2=runif(num/2)
result2=c(0.5*exp(n2)+0.5*exp({1-n2}))
mean2=mean(result2)
var2=var(result2)
print(c(mean2,var2))
stand=exp(1)-1
A=(var1-var2)/var1
print(A)

## -----------------------------------------------------------------------------
set.seed(2000)
m=10^5
sample1=numeric(m)
for (i in 1:m)
{
  sample1[i]=rnorm(1)
  while(sample1[i]<=1)
  {
    sample1[i]=rnorm(1)
  }
}
sample2=rexp(m,rate=1)+1
#两个函数的形式
hist(sample1,prob=T)
y <- seq(1, 100, .001)
lines(y, dnorm(y)/(1-pnorm(1)))
hist(sample2,prob=T)
y <- seq(1, 100, .001)#验证分布服从指定分布的结果
lines(y, dexp(y,rate=1)*exp(1))
result1<-result2<-numeric(m)

for (i in 1:m)
{result1[i]=sample1[i]^2*(1-pnorm(1))
result2[i]=sample2[i]^2/sqrt(2*pi)*exp(-sample2[i]^2/2+sample2[i]-1)
}


print(c(mean(result1),mean(result2)))
print(c(var(result1),var(result2)))

## -----------------------------------------------------------------------------
set.seed(1000)
m=10^4
n=5
N=50
sample1=sample2=numeric(m)
est1=matrix(0,nrow=N,ncol=2)
est2=matrix(0,nrow=N,ncol=2)
T=numeric(n)

for(i in 1:N)
{g <- function(x) {
exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
  sample1=runif(m)#逆变换法
x = -log(1 - sample1* (1 - exp(-1)))
fg <- g(x) / (exp(-x) /(1 - exp(-1)))
est1[i,1] <- mean(fg)
#Example6.10结果


for (j in 1:n)
  {g <- function(x) {
exp(-x - log(1+x^2)) * (x >(j-1)/5) * (x < ((j)/5))
}
  sample2=runif(m/n,0,1)
  x=-log(exp(-(j-1)/5)-sample2 * (exp(-(j-1)/5) - exp(-(j)/5)))
fg1 <- g(x) / (exp(-x) / (exp(-(j-1)/5) - exp(-(j)/5)))
T[j]=mean(fg1)
#新方法分层抽样
}
est1[i,2] <- sum(T)
#分层重要性抽样结果
}
print(round(apply(est1,2,mean),4))
print(round(apply(est1,2,sd),4))

## -----------------------------------------------------------------------------
set.seed(200)
n=20
m=10^5
num1=num2=0
for(i in 1:m){
x <- rchisq(n,df=2)
samplesd=sd(x)
meanx=mean(x)
leftpoint=2-(qt(0.975,df=19)*sd(x)/sqrt(20))
rightpoint=2+(qt(0.975,df=19)*sd(x)/sqrt(20))#t分布
leftpoint1=2-(qnorm(0.975)*2/sqrt(20))
rightpoint1=2+(qnorm(0.975)*2/sqrt(20))#正态
if(leftpoint<meanx && meanx<rightpoint)
{
  num1=num1+1
}
if(leftpoint1<meanx && meanx<rightpoint1)
{
  num2=num2+1
}

}
result1=num1/m
result2=num2/m
print(c(result1,result2))


## -----------------------------------------------------------------------------
set.seed(200)
n=100
m=10^5
num1=num2=num3=0
for(i in 1:m){
x <- rchisq(n,df=1)
y=runif(n,0,2)
z=rexp(n,rate=1)
meanx=mean(x)
meany=mean(y)
meanz=mean(z)
leftpoint=1-(qt(0.975,df=n-1)*sd(x)/sqrt(n))
rightpoint=1+(qt(0.975,df=n-1)*sd(x)/sqrt(n))#t分布
leftpoint1=1-(qt(0.975,df=n-1)*sd(y)/sqrt(n))
rightpoint1=1+(qt(0.975,df=n-1)*sd(y)/sqrt(n))
leftpoint2=1-(qt(0.975,df=n-1)*sd(z)/sqrt(n))
rightpoint2=1+(qt(0.975,df=n-1)*sd(z)/sqrt(n))
if(leftpoint>meanx ||meanx>rightpoint)
{
  num1=num1+1
}
if(leftpoint1>meany || meany>rightpoint1)
{
  num2=num2+1
}
if(leftpoint2>meanz || meanz>rightpoint2)
{
  num3=num3+1
}

}
print(c(num1/m,num2/m,num3/m))

## -----------------------------------------------------------------------------
set.seed(1000)
N=1000
alpha=0.1
num1=num2=num3=num4=numeric(N)
for(k in 1:1000)
{
sample1=runif(950)
sample2=rbeta(50,shape1=0.1,shape2=1)
position1=rep(0,times=950)
position2=rep(1,times=50)
position=c(position1,position2)#位置保留原假设与备择假设的分组信息
sample0=c(sample1,sample2)
sampleall=(cbind(sample0,position))#样本生成
n=1000
bonferroni1=BH1=numeric(n)
sample0=sort(sample0)
sampleall=sampleall[order(sampleall[,1]),]#样本与位置信息一起排序
for (i in 1:n)
{bonferroni1[i]=n*sample0[i]#bonferroni方法修正
  if( bonferroni1[i]>1)
  {
    bonferroni1[i]=1
  }
}
BH1=sample0*n/(1:n)#BH方法修正
for (i in 1:(n-1))
{
  if( BH1[i]>BH1[i+1])
  {
    BH1[i]=BH1[i+1]
  }
 if( BH1[i]<0.1&&BH1[i+1]>0.1)
  {
    num2[k]=i
    num4[k]=sum(sampleall[1:i,2])#BH方法拒绝域中备择假设个数
 }
   if( bonferroni1[i]<0.1&&bonferroni1[i+1]>0.1)
  {
    num1[k]=i#所有落入拒绝域的个数
    num3[k]=sum(sampleall[1:i,2])#bonferroni方法拒绝域中备择假设个数
  }
}

}
size1=size2=0
for(j in 1:N)
{
  if(num3[j]/num1[j]<1)
  {size1=size1+1}
   if(num4[j]/num2[j]<1)
  {size2=size2+1}
}
mean1=size1/N
mean2=size2/N
mean3=mean(1-num3/num1)
mean4=mean(1-num4/num2)
mean5=mean(num3/50)
mean6=mean(num4/50)
print(c(mean1,mean2,mean3,mean4,mean5,mean6))#输出三个指标在两种方法下的估计值

## -----------------------------------------------------------------------------
set.seed(2000)
length1=5
length2=10
length3=20
m=B=1000
lambda=2
result1=result2=result3=numeric(B)
mean1=mean2=mean3=numeric(m)
sd1=sd2=sd3=numeric(m)#三种样本容量的数据
for(k in 1:m)
{sample1=rexp(length1,rate=lambda)
  sample2=rexp(length2,rate=lambda)
  sample3=rexp(length3,rate=lambda)
for(i in 1:(B))
{ 
  sample1star=sample(sample1,length1,replace = TRUE)
  result1[i]=1/mean(sample1star)
 sample2star=sample(sample2,length2,replace = TRUE)
   result2[i]=1/mean(sample2star)
 sample3star=sample(sample3,length3,replace = TRUE)
   result3[i]=1/mean(sample3star)
}
mean1[k]=mean(result1)-1/mean(sample1)
mean2[k]=mean(result2)-1/mean(sample2)
mean3[k]=mean(result3)-1/mean(sample3)
sd1[k]=sd(result1)
sd2[k]=sd(result2)
sd3[k]=sd(result3)
}
meansd1=mean(sd1)
meansd2=mean(sd2)
meansd3=mean(sd3)
meanbias1=mean(mean1)
meanbias2=mean(mean2)
meanbias3=mean(mean3)#1000次模拟的均值与偏差结果
print(c(meanbias1,meanbias2,meanbias3))
print(c(meansd1,meansd2,meansd3))

## -----------------------------------------------------------------------------
set.seed(1000)
library(boot)
attach(aircondit)
m=1e3
data1=c(3, 5, 7, 18, 43, 85, 91, 98,100, 130, 230, 487)
boot.mean <- function(x,i) {mean(x[i])}
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
de <- boot(data=data1,statistic=boot.mean ,R = 999)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
ci.perc[i,]<-ci$percent[4:5];ci.bca[i,]<-ci$bca[4:5]
}
cat('norm :',c(mean(ci.norm[,1]) ,mean( ci.norm[,2])))
cat('basic :',c(mean(ci.basic[,1]) ,mean( ci.basic[,2])))
cat('perc :',c(mean(ci.perc[,1]) ,mean( ci.perc[,2])))
cat('BCa :',c(mean(ci.bca[,1]) ,mean( ci.bca[,2])))

## -----------------------------------------------------------------------------
set.seed(1000)
library(bootstrap)
attach(scor)
sample1=scor
realcov=cov(sample1)
realeigresult=eigen(realcov)
realtheta=realeigresult$values[1]/sum(realeigresult$values)
realtheta
num=nrow(sample1)
hattheta=numeric(num)
for(i in 1:num)
{
  hatsigma=cov(sample1[-i,])
  eigresult=eigen(hatsigma)
  hattheta[i]=eigresult$values[1]/sum(eigresult$values)
}
bias=(num-1)*(mean(hattheta)-realtheta)
se=sqrt((num-1)/num*sum((hattheta-(rep(1,times=num)*mean(hattheta)))^2))

## -----------------------------------------------------------------------------
set.seed(1000)
library(DAAG)
attach(ironslag)
n =length(magnetic) #in DAAG ironslag
e1 = e2 = e3 = e4=numeric(1000)
num=0
# for n-fold cross validation
# fit models on leave-two-out samples

for(i in 1:(1000)){
  sample1=sample(1:n,size=2,replace=FALSE)
  sample1=sort(sample1)
  j=sample1[1]
  k=sample1[2]
    y=magnetic[-j]
    y=y[-k]
    x = chemical[-j]
    x=x[-k]
    num=num+1
    J1 =lm(y ~ x)
    yhat1 = J1$coef[1] + J1$coef[2] * c(chemical[k],chemical[j])
    e1[i]=(c(magnetic[k],magnetic[j]) - yhat1)[1]^2
    +(c(magnetic[k],magnetic[j]) - yhat1)[2]^2
    J2 = lm(y ~ x + I(x^2))
    yhat2 = c(J2$coef[1] + J2$coef[2] * chemical[k] 
              +J2$coef[3] * chemical[k]^2,
   J2$coef[1] + J2$coef[2] * chemical[j] 
   +J2$coef[3] * chemical[j]^2)
    e2[i] = (c(magnetic[k],magnetic[j]) - yhat2)[1]^2
    +(c(magnetic[k],magnetic[j]) - yhat2)[2]^2
    J3 =lm(log(y) ~ x)
    logyhat3 = J3$coef[1] + J3$coef[2] * chemical[k]
    otherlogyhat3 = J3$coef[1] + J3$coef[2] * chemical[j]
    yhat3 = c(exp(logyhat3),exp(otherlogyhat3))
    e3[i] = (c(magnetic[k],magnetic[j]) - yhat3)[1]^2
    +(c(magnetic[k],magnetic[j]) - yhat3)[2]^2
    J4 = lm(log(y) ~ log(x))
    logyhat4 =J4$coef[1] + J4$coef[2] * log(chemical[k])
    otherlogyhat4 =J4$coef[1] + J4$coef[2] * log(chemical[j])
    yhat4 = c(exp(logyhat3),exp(otherlogyhat4))
    e4[i] =(c(magnetic[k],magnetic[j]) - yhat4)[1]^2
    +(c(magnetic[k],magnetic[j]) - yhat4)[2]^2
  }
print(c( c(mean(e1), mean(e2), mean(e3), mean(e4))))

## -----------------------------------------------------------------------------
set.seed(1000)
library(bootstrap)
myfunction=function(x,y)#建立函数
{
  numx=length(x)
  numy=length(y)
  num=numx+numy#计算样本的维数
  z=c(x,y)
  R=10^4
  result=numeric(R)
  power=0
  for(i in 1:R)
  {
    sample1=sample(z,num,replace=FALSE)
    pro1=sample1[1:numx]
    pro2=sample1[numx+1:num]
   fun1=ecdf(pro1)
    fun2=ecdf(pro2)#获取经验分布函数
    result1=fun1(z)
    result2=fun2(z)
    result[i]=numx*numy/((numx+numy)^2)*sum((result1-result2)^2)
    
  }
 funreal1=ecdf(x)
 funreal2=ecdf(y)
 result0=numx*numy/((numx+numy)^2)*sum((funreal1(z)-funreal2(z))^2)
 return(mean(c(result,result0)>result0))
}
attach(chickwts)
x0 <- sort(as.vector(weight[feed == "soybean"]))
y0<- sort(as.vector(weight[feed == "linseed"]))

detach(chickwts)#选取数据并代入函数
resultall=myfunction(x0,y0)
print(resultall)

## -----------------------------------------------------------------------------
set.seed(10000)
maxout <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
return(max(c(outx, outy)))
}
count5test <- function(x, y,N) {
x=x-mean(x)
y=y-mean(y)#中心化
z=c(x,y)
result0=numeric(N)
for(i in 1:1000){
z=sample(z,length(z),replace=FALSE)
X=z[1:length(x)]
Y=z[(length(x)+1):length(z)]
X=X-mean(X)
Y=Y-mean(Y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
result0[i]=(as.integer(max(c(outx, outy)) > 5))}
return(mean(result0))
}
n1 <-10
n2 <- 10
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 1000
tests <- replicate(m, expr = {
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
count5test(x, y,1000)
} )
alphahat <- mean(tests)
print(alphahat)
stat <- replicate(m, expr={
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
maxout(x, y)
})
print(cumsum(table(stat)) / m)#同样本容量检验
n1 <-10
n2 <- 8
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
tests <- replicate(m, expr = {
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
count5test(x, y,1000)
} )
alphahat <- mean(tests)
print(alphahat)
stat <- replicate(m, expr={
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
maxout(x, y)
})
print(cumsum(table(stat)) / m)#异样本容量检验

## -----------------------------------------------------------------------------
set.seed(1000)
N <- 1e6; b1 <- 0; b2 <- 1;b3=-1; f1 <- c(0.1,0.01,0.001,0.0001)
x1=rpois(N,lambda = 1)
x2 <- rexp(N,rate=1); x3 <- sample(0:1,N,replace=TRUE)
g <- function(alpha,f0){
tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
p <- 1/(1+tmp)
mean(p) - f0#逻辑回归部分
}
result=numeric(4)
solution1 <- uniroot(g,c(-10,0),f0=f1[1])
result[1]=round(unlist(solution1),5)[1]
solution2 <- uniroot(g,c(-10,0),f0=f1[2])
result[2]=round(unlist(solution2),5)[1]
solution3 <- uniroot(g,c(-100,0),f0=f1[3])
result[3]=round(unlist(solution3),5)[1]
solution4 <- uniroot(g,c(-100,0),f0=f1[4])
result[4]=round(unlist(solution4),5)[1]
print(result)
plot(-log(f1),result,type='l')

## -----------------------------------------------------------------------------
set.seed(1000)
library(ggplot2)
dlaplace=function(x)
{
  return(1/2*exp(-abs(x)))#Laplace分布对应的密度函数
}
rw.Metropolis <- function(n, sigma, x0, N) {
x <- numeric(N)
x[1] <- x0

u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <= (dlaplace(y) / dlaplace(x[i-1])))
x[i] <- y else {
x[i] <- x[i-1]
k <- k + 1
}#Metropolis方法采样
}
return(list(x=x, k=k))
}
N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(n, sigma[1], x0, N)
rw2 <- rw.Metropolis(n, sigma[2], x0, N)
rw3 <- rw.Metropolis(n, sigma[3], x0, N)
rw4 <- rw.Metropolis(n,sigma[4],x0,N)#四条马氏链结果
 print(c(rw1$k, rw2$k, rw3$k, rw4$k))#四种方差下的拒绝次数
 dat=cbind(rw1$x[1001:2000],rw2$x[1001:2000],rw3$x[1001:2000],rw4$x[1001:2000])
 p_line=ggplot(as.data.frame(dat))+geom_line(aes(x=1001:2000,y=dat[,1]),color='blue')+  geom_line(aes(x=1001:2000,y=dat[,2]),color='red')+  geom_line(aes(x=1001:2000,y=dat[,3]),color='yellow')+ geom_line(aes(x=1001:2000,y=dat[,4]),color='green')+labs(x= 'times',y='value')
 #方差由小到大依次是蓝红黄绿四条折线图
 

p_line

## -----------------------------------------------------------------------------
set.seed(10000)
library(lmtest)
library(ggplot2)
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- -.8 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
###### generate the chain #####
X[1, ] <- c(mu1, mu2) #initialize
for (i in 2:N) {
x2 <- X[i-1, 2]
m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
X[i, 1] <- rnorm(1, m1, s1)
x1 <- X[i, 1]
m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1
x0 <- X[b:N, ]#生成二元正态的样本
lmmodel=lm(x0[,2]~x0[,1])
summary(lmmodel)#回归
res=lmmodel$residuals
qqnorm(res)#qq图
ks.test(res,'pnorm')#KS正态性检验
gqtest(lmmodel)#Goldfeld-Quandt检验

## -----------------------------------------------------------------------------
set.seed(1000)
library(coda)
f <- function(x, sigma) {
if (any(x < 0)) return (0)
stopifnot(sigma > 0)
return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}

m <- 10000
sigma <- 4
x <- numeric(m)
x[1] <- rchisq(1, df=1)
k <- 0
u <- runif(m)
for (i in 2:m) {
xt <- x[i-1]
y <- rchisq(1, df = xt)
num <- f(y, sigma) * dchisq(xt, df = y)
den <- f(xt, sigma) * dchisq(y, df = xt)
if (u[i] <= num/den) x[i] <- y else {
x[i] <- xt
k <- k+1 #y is rejected
}
}
set.seed(2000)
x1=numeric(m)
x1[1]=rchisq(1,df=1)
u1 <- runif(m)
for (i in 2:m) {
xt <- x1[i-1]
y <- rchisq(1, df = xt)
num <- f(y, sigma) * dchisq(xt, df = y)
den <- f(xt, sigma) * dchisq(y, df = xt)
if(u1[i] <= num/den) x1[i] <- y else {
x1[i] <- xt
k <- k+1 #y is rejected
}
}
gelman.diag(mcmc.list(as.mcmc(x),as.mcmc(x1)),confidence = 0.95,transform = TRUE)

## -----------------------------------------------------------------------------
set.seed(1000)

A=matrix(c(11,12,
            8,9,
           27,28,
           13,14,
           16,17,
           0,1,
           23,24,
           10,11,
           24,25,
           2,3),2,10)
A=t(A)
fx=function(x)#LogML函数
{f0=0
  for(i in 1:10)
{
  f0=f0+(-A[i,1]*exp({-x*A[i,1]})+A[i,2]*
           exp({-x*A[i,2]}))/(exp({-x*A[i,1]})-exp({-x*A[i,2]}))
}
  f0
}
out1 <- uniroot(fx,
lower = 0, upper =10)
round(c(out1$root,out1$f.root),4)#MLE估计结果展示

#E-step
E_step <- function(x, lambda) {
  p_z_given_x <- lambda * exp(-lambda * x) / (lambda * exp(-lambda * x) + lambda)
  return(p_z_given_x)
}

# M-step
M_step <- function(x, p_z_given_x) {
  lambda_new <- sum(p_z_given_x) / sum(x)
  return(lambda_new)
}

EM_algorithm <- function(x, lambda_initial, tol = 1e-6, max_iter = 1000) {
  lambda <- lambda_initial
  for (iter in 1:max_iter) {
    # E-step
    p_z_given_x <- E_step(x, lambda)
    
    # M-step
    lambda_new <- M_step(x, p_z_given_x)
    
    # Check for convergence
    if (abs(lambda_new - lambda) < tol) {
      break
    }
    
    lambda <- lambda_new
  }
  
  return(lambda)
}
result=apply(A,1,mean)
x=result
lambda_initial <- 0.05
estimated_lambda <- EM_algorithm(x, lambda_initial)
estimated_lambda 

## -----------------------------------------------------------------------------
set.seed(1000)
solve.game <- function(A) {
#solve the two player zero-sum game by simplex method
#optimize for player 1, then player 2
#maximize v subject to ...
#let x strategies 1:m, and put v as extra variable
#A1, the <= constraints
#
min.A <- min(A)
A <- A - min.A #so that v >= 0
max.A <- max(A)
A <- A / max(A)
m <- nrow(A)
n <- ncol(A)
it <- n^3
a <- c(rep(0, m), 1) #objective function
A1 <- -cbind(t(A), rep(-1, n)) #constraints <=
b1 <- rep(0, n)
A3 <- t(as.matrix(c(rep(1, m), 0))) #constraints sum(x)=1
b3 <- 1
sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=TRUE, n.iter=it)
#the ’solution’ is [x1,x2,...,xm | value of game]
#
#minimize v subject to ...
#let y strategies 1:n, with v as extra variable
a <- c(rep(0, n), 1) #objective function
A1 <- cbind(A, rep(-1, m)) #constraints <=
b1 <- rep(0, m)
A3 <- t(as.matrix(c(rep(1, n), 0))) #constraints sum(y)=1
b3 <- 1
sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=FALSE, n.iter=it)
soln <- list("A" = A * max.A + min.A,
"x" = sx$soln[1:m],
"y" = sy$soln[1:n],
"v" = sx$soln[m+1] * max.A + min.A)
soln
}
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
2,0,0,0,-3,-3,4,0,0,
2,0,0,3,0,0,0,-4,-4,
-3,0,-3,0,4,0,0,5,0,
0,3,0,-4,0,-4,0,5,0,
0,3,0,0,4,0,-5,0,-5,
-4,-4,0,0,0,5,0,0,6,
0,0,4,-5,-5,0,0,0,6,
0,0,4,0,0,5,-6,-6,0), 9, 9)
A=A+matrix(rep(2,81),9,9)#A=A+2
library(boot) #needed for simplex function
s <- solve.game(A)
round(cbind(gameA=s$x, gameB=s$y), 7)#value of gameA and gameB
round(c(0, 0, 25/61, 0, 20/61, 0, 16/61, 0,0),7)

## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
my_list <- list(a = 1, b = list(c = 2, d = 3), e = 4,f=list(g=5))

# 使用unlist()
result_unlist <- unlist(my_list)
print(result_unlist)


# 使用as.vector()
result_as_vector <- as.vector(my_list)
print(result_as_vector)


## -----------------------------------------------------------------------------
a=c(1,1,2,3)
b=vector(length=5L)
print(dim(a))
print(dim(b))

## -----------------------------------------------------------------------------
a=matrix(2,3,3)
is.matrix(a)
is.array(a)

## -----------------------------------------------------------------------------

a<- data.frame(matrix(ncol = 5, nrow = 0))
a


## -----------------------------------------------------------------------------
a=data.frame(a=1,b=list(c=3,d=4))
print(as.matrix(a))

## -----------------------------------------------------------------------------
df1 <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 7)
)
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
# 使用apply()函数

result<- apply(df1,  2,scale01)
print(result)


## -----------------------------------------------------------------------------
df1 <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C=c("article","subject","lesson")
)
columns <- sapply(df1, is.numeric)
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# 使用apply()函数
result<- apply(df1[,columns], 2, scale01)
print(result)


## -----------------------------------------------------------------------------
df1 <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 7)
)


# 使用vapply()函数
result<- vapply(df1, sd,FUN.VALUE=c(sd=0))
print(result)


## -----------------------------------------------------------------------------
df1 <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 7),
  C=c("article","subject","lesson")
)
columns <- vapply(df1, is.numeric,FUN.VALUE=c(sd=0))


# 使用apply()函数
result<- vapply(df1[,columns],  sd,FUN.VALUE=c(sd=0))
print(as.vector(result))

## -----------------------------------------------------------------------------
library(microbenchmark)
set.seed(1000)
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
funa=function(a,b,n){


x=0
y=0.5
X[1, ] <- c(x, y)
for (i in 1:N) {
 
  x <- rbinom(1, n, y)
  
  y <- rbeta(1, x + a, n - x + b)
  
  # 存储采样结果
 X[i, ] <- c(x, y)
}
X=X[(burn+1):5000,]
return(X)
}
b=funa(1,1,10)
print(b[1:20,])


## -----------------------------------------------------------------------------
library(microbenchmark)
result <- microbenchmark(funa(1,1,10), gibbsSampler(5000,1000, 1,1, 10), times = 100)
print(result)

