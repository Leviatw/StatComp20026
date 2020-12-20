## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(StatComp20026)

## ----pressure, echo=FALSE, fig.cap="A Network", out.width = '30%'-------------
knitr::include_graphics("plot.png")

## ----table--------------------------------------------------------------------
knitr::kable(matrix(rnorm(25, 0, 1), ncol = 5), col.names = paste("Col",1:5,sep = " "),  caption = "A Random Matrix")

## -----------------------------------------------------------------------------
#Use the inverse transform method to simulate a random sample from the Pareto(2, 2) distribution

#Generate U ~ U(0, 1)
u = runif(1)
#Return x = F^{-1}(u)
x = round(2/sqrt(1-u), 3)
print(paste("The random sample wanted is ", x, ".", sep = ""))

## -----------------------------------------------------------------------------
library(ggplot2)
#generate data with amount == 5000
u = runif(5000)
x = round(2/sqrt(1-u), 3)
x = x[x<=15]

#gather the data into a dataframe
df = data.frame(sample = x)

#histogram
ggplot(df, aes(x = sample)) + geom_histogram(binwidth = 0.5, aes(y=..density..), boundary = 2) + labs(title="Histogram of random samples")

## -----------------------------------------------------------------------------
#density
funpareto = function(x){8/(x^3)}

#plot
ggplot(df, aes(x = sample)) + geom_histogram(binwidth = 0.5, aes(y=..density..), boundary = 2) + stat_function(fun=funpareto,geom="line") + labs(title="Histogram superimposed of theoretical density function")

## ---- eval=FALSE--------------------------------------------------------------
#  Generate i.i.d U1, U2, U3 ~ Uniform(-1,1)
#  if(|U3|>=|U2| and |U3| >= |U1|){
#    return(U2)
#  }
#  else{
#    return(U3)
#  }

## -----------------------------------------------------------------------------
library(ggplot2)
#generate samples
n = 10000
u = matrix(runif(3*n, min = -1, max = 1), nrow = 3)
sample = apply(u, MARGIN = 2, FUN = function(x){ifelse(abs(x[3])>abs(x[2]) & abs(x[3])>abs(x[1]), x[2], x[3])})
df = data.frame(sample = sample)

#construct the histogram density estimate
ggplot(df, aes(x = sample)) + geom_histogram(binwidth = 0.05, aes(y=..density..), boundary = -1) + geom_freqpoly(binwidth = 0.05, aes(y=..density..)) + labs(title="Histogram with density estimated")

## ---- eval=FALSE--------------------------------------------------------------
#  Generate i.i.d U1, U2, U3 ~ Uniform(-1,1)
#  if(|U3|>=|U2| and |U3| >= |U1|){
#    return(U2)
#  }
#  else{
#    return(U3)
#  }

## -----------------------------------------------------------------------------
library(ggplot2)
#generate data
r = 4
beta = 2
u = runif(1000)
x = beta * (1/(1-u)^(1/r) - 1)
x = x[x<=3]

#density function
beta_r_pareto = function(x, beta, r){r*beta^r/(beta+x)^(r+1)}

#gather the data into a dataframe
df = data.frame(sample = x)

#histogram
ggplot(df, aes(x = sample)) + geom_histogram(binwidth = 0.05, aes(y=..density..), boundary = 0) +  stat_function(fun=function(x){beta_r_pareto(x, beta, r)},geom="line") + labs(title="Histogram superimposed of theoretical density function")


## -----------------------------------------------------------------------------
n = 10^6
x = runif(n, min = 0, max = pi/3)
result = round(pi/3 * mean(sin(x)), 4)
print(paste("The approximated integation is ", result, ".", sep = ""))

## -----------------------------------------------------------------------------
#Simple Monte Carlo Method 
n = 10^6
u = runif(n)
x1 = exp(u)
m1 = mean(x1)
print(paste("The estimation of the integration under Simple Monte Carlo Method is ", round(m1, 3), ".", sep = ""))

#Antithetic Variate Monte Carlo Method 
u = runif(n/2)
x2 = 0.5*(exp(u)+exp(1-u))
m2 = mean(x2)
print(paste("The estimation of the integration under Antithetic Variate Monte Carlo Method is ", round(m2, 3), ".", sep = ""))

## -----------------------------------------------------------------------------
mc1 = mc2 = c()
n = 10^4
for (i in 1:1000) {
#Simple Monte Carlo Method 
u = runif(n)
x1 = exp(u)
m1 = mean(x1)
mc1 = c(mc1,m1)

#Antithetic Variate Monte Carlo Method 
u = runif(n/2)
x2 = 0.5*(exp(u)+exp(1-u))
m2 = mean(x2)
mc2 = c(mc2,m2)
}

#Empirical Variance Reduction
vr = 100*(var(mc1)-var(mc2))/var(mc1)
print(paste("The empirical variance reduction is ", round(vr,2), "%.", sep = ""))

## -----------------------------------------------------------------------------
library(ggplot2)
g_sc_hw3 = function(x){
  x^2 * dnorm(x)
}

base <- ggplot() + xlim(1,5)
base +
  geom_function(aes(colour = "g(x)"), fun = g_sc_hw3)

## -----------------------------------------------------------------------------
base +
  geom_function(aes(colour = "g(x)"), fun = g_sc_hw3) + 
  geom_function(aes(colour = "density of normal"), fun = function(x){dnorm(x, mean = 1.5)}) + 
  geom_function(aes(colour = "density of exponential"), fun = dexp)

## -----------------------------------------------------------------------------
m = 10^5
x = rexp(m)
x = x[x>1]
y = g_sc_hw3(x)/exp(1-x)
res = mean(y)
print(paste("The integration using truncated exponential function as the important function is approximated as ", round(res,3), ".", sep = ""))

## -----------------------------------------------------------------------------
m = 10^5
x = rnorm(m, mean = 1.5)
x = x[x>1]
y = g_sc_hw3(x)*(1-pnorm(-0.5))/dnorm(x-1.5)
res = mean(y)
print(paste("The integration using truncated normal function as the important function is approximated as ", round(res,3), ".", sep = ""))

## -----------------------------------------------------------------------------
est = est.sd = numeric(2)

m = 10^5
x = rexp(m)
x = x[x>1]
y1 = g_sc_hw3(x)/exp(1-x)


x = rnorm(m, mean = 1.5)
x = x[x>1]
y2 = g_sc_hw3(x)*(1-pnorm(-0.5))/dnorm(x-1.5)

n = min(length(y1), length(y2))
y1 = y1[1:n]
y2 = y2[1:n]

est[1] = mean(y1)
est.sd[1] = sd(y1)
est[2] = mean(y2)
est.sd[2] = sd(y2)

res <- rbind(est=round(est,3), sd=round(est.sd,3))
  colnames(res) <- paste0('f',1:2)
  knitr::kable(res,align='c')
  


## -----------------------------------------------------------------------------
#g(x)/f_j(z)
g_divide_fj = function(x, j){
  (exp(-j/5)-exp(-(j+1)/5))/(1+x^2)
}

#Total amount of samples
m = 10^4
#Amount of samples in each interval
mj = m/5
#Data Matrix
data.mat = matrix(0, nrow = mj, ncol = 5)
for (j in 0:4) {
  u = runif(mj)
  x = -log(exp(-j/5) - u*(exp(-j/5) - exp(-(j+1)/5)))
  data.mat[,(j+1)] = g_divide_fj(x, j)
}

result = sum(colMeans(data.mat))
print(paste("The estimation of the wanted integration using straitified method is ", round(result, 3), ".", sep = ""))

## -----------------------------------------------------------------------------
n = 10^3
result.vec = numeric(n)
for (i in 1:n) {
  data.mat = matrix(0, nrow = mj, ncol = 5)
  for (j in 0:4) {
    u = runif(mj)
    x = -log(exp(-j/5) - u*(exp(-j/5) - exp(-(j+1)/5)))
    data.mat[,(j+1)] = g_divide_fj(x, j)
  }

  result.vec[i] = sum(colMeans(data.mat))
}

res <- rbind(est=round(c(0.5257801, result),3), sd = round(c(0.0970314, sd(result.vec)),4))
  colnames(res) <- c("Best Importance Sampling in Example 5.10", "Stratified Importance Sampleing")
  knitr::kable(res,align='c')

## -----------------------------------------------------------------------------
skew.ness = function(x) {
#computes the sample skewness coeff.
m3 = mean((x - mean(x))^3)
m2 = mean((x - mean(x))^2)
return( m3 / m2^1.5 )
}

#different sample sizes
n = c(10, 20, 30, 50, 100, 500) 
#critical values for each n 
cv = qnorm(0.975, 0, sqrt(6/n)) 


#n is a vector of sample sizes
#we are doing length(n) different simulations
ht.power = numeric(length(n)) 
m = 1000
for (i in 1:length(n)) {
    sktests = numeric(m) #test decisions 
    for (j in 1:m) {
        x = rbeta(n[i],1,1)
        #test decision is 1 (reject) or 0
        sktests[j] = as.integer(abs(skew.ness(x)) >= cv[i] )
    }
    ht.power[i] = mean(sktests) #proportion rejected
}


knitr::kable(data.frame(t(ht.power), row.names = "Power"), col.names = paste("n =", n))

## -----------------------------------------------------------------------------
#fixed sample size
n = 100
#different parameter alpha
alpha = c(1, 2, 5, 10, 50, 100) 
#critical values for each n 
cv = qnorm(0.975, 0, sqrt(6/n)) 


#n is a vector of sample sizes
#we are doing length(n) different simulations
ht.power = numeric(length(n)) 
m = 1000
for (i in 1:length(alpha)) {
    sktests = numeric(m) #test decisions 
    for (j in 1:m) {
        x = rbeta(n,alpha[i], alpha[i])
        #test decision is 1 (reject) or 0
        sktests[j] = as.integer(abs(skew.ness(x)) >= cv )
    }
    ht.power[i] = mean(sktests) #proportion rejected
}


knitr::kable(data.frame(t(ht.power), row.names = "Power"), col.names = paste("$\\alpha$ =", alpha))

## -----------------------------------------------------------------------------
#n is a vector of sample sizes
n = c(10, 20, 30, 50, 100, 500)
#critical values for each n 
cv = qnorm(0.975, 0, sqrt(6/n)) 

#we are doing length(n) different simulations
ht.power = numeric(length(n)) 
m = 1000
for (i in 1:length(n)) {
    sktests = numeric(m) #test decisions
    for (j in 1:m) {
        x = rt(n[i],1)
        #test decision is 1 (reject) or 0
        sktests[j] = as.integer(abs(skew.ness(x)) >= cv[i] )
    }
    ht.power[i] = mean(sktests) #proportion rejected
}

knitr::kable(data.frame(t(ht.power), row.names = "Power"), col.names = paste("n =", n))

## -----------------------------------------------------------------------------
#fixed sample size
n = 100
#different parameter nu
nu = c(1, 2, 5, 10, 50, 100) 
#critical values for each n 
cv = qnorm(0.975, 0, sqrt(6/n)) 


#n is a vector of sample sizes
#we are doing length(n) different simulations
ht.power = numeric(length(n)) 
m = 1000
for (i in 1:length(nu)) {
    sktests = numeric(m) #test decisions 
    for (j in 1:m) {
        x = rt(n,nu[i])
        #test decision is 1 (reject) or 0
        sktests[j] = as.integer(abs(skew.ness(x)) >= cv )
    }
    ht.power[i] = mean(sktests) #proportion rejected
}


knitr::kable(data.frame(t(ht.power), row.names = "Power"), col.names = paste("$\\nu$ =", nu))

## -----------------------------------------------------------------------------
#count five test for one pair of samples
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}

alpha = 0.055
Ftest = function(x, y, alpha){
  #two sides F test
  F.sta = var(x)/var(y)
  df1 = length(x)
  df2 = length(y)
  return(as.integer((F.sta>qf(1-alpha/2, df1, df2))|(F.sta<qf(alpha/2, df1, df2))))
}

#sample sizes
n = c(5, 30, 100)
#repeated times
m = 1000

#calculate the power
sigma1 <- 1
sigma2 <- 1.5

power = matrix(0, nrow = 2, ncol = length(n))

for (i in 1:length(n)) {
  power[,i] = apply(replicate(m, expr={
x <- rnorm(n[i], 0, sigma1)
y <- rnorm(n[i], 0, sigma2)
c(count5test(x, y), Ftest(x, y, alpha))
}), MARGIN = 1, mean)
}



## -----------------------------------------------------------------------------
knitr::kable(data.frame(power, row.names = c("Count Five Test", "F test with condifence level 0.055")), col.names = paste("n =", n))

## -----------------------------------------------------------------------------
mul.sk = function(X){
  #X is a n*d matrix containing one group of samples
  n = nrow(X)
  d = ncol(X)
  barX = apply(X, 2, mean)
  Sigma.inverse = solve(var(X))
  
  stat = 0
  for (i in 1:n) {
    for (j in 1:n) {
      stat = stat + (t(X[i,]-barX) %*% Sigma.inverse %*% (X[j,]-barX))^3
    }
  }
  
  stat/(n^2)
}

#different sample sizes
n = c(10, 20, 30, 50, 100)

#dimension d
d = 2
#repeated times
m = 1000

#critical values for each n 
cv = 6*qchisq(0.95, df = d*(d+1)*(d+2)/6)/n 


p.rej = numeric(length(n))
for (i in 1:length(n)) {
  p.rej[i] =mean(replicate(m, expr={
  X = matrix(rnorm(d*n[i]), ncol = d)
  as.integer(mul.sk(X)>cv[i])
}))
}


knitr::kable(data.frame(t(p.rej), row.names = "Type one Error"), col.names = paste("n =", n))

## -----------------------------------------------------------------------------
#different sample sizes
n = 30
#dimension d
d = 2
#repeated times
m = 1000
#confidence level
alpha = 0.1

#critical values for each n 
cv = 6*qchisq(1-alpha, df = d*(d+1)*(d+2)/6)/n 

#epsilon
epsilon = c(seq(0, 0.14, 0.01), seq(0.15, 1, 0.05))

pwr = numeric(length(epsilon))
for (i in 1:length(epsilon)) {
  e = epsilon[i]
  pwr[i] =mean(replicate(m, expr={
  sigma = sample(c(1, 10), replace = TRUE,
size = n, prob = c(1-e, e))

  X = matrix(rnorm(d*n, sd = sigma), ncol = d)
  as.integer(mul.sk(X)>cv)
}))
}

knitr::kable(data.frame(t(pwr), row.names = "Power"), col.names = paste("$\\epsilon$ =", epsilon))

#plot power vs epsilon
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
#the statistic of interest
theta.hat = cor(law$LSAT, law$GPA)
#sample size
n = nrow(law)

#compute the jackknife replicates, leave-one-out estimates
theta.jack = numeric(n)
for (i in 1:n){
 theta.jack[i] = cor(law$LSAT[-i], law$GPA[-i])
}

bias.jack = (n-1) * (mean(theta.jack) - theta.hat)
bias.jack

## -----------------------------------------------------------------------------
#compute the jackknife estimate of se
se.jack = sqrt(sd(theta.jack)^2*(n-1)^2/n)
se.jack

#The results. 
knitr::kable(data.frame(bias.jack, se.jack), col.names =c("Jackknife Estimate of Bias", "Jackknife Estimate of SE"))

## -----------------------------------------------------------------------------
set.seed(5)
#maximum of extreme points 
max.ext.pts = function(x, y) {
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y))
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(max(c(outx, outy)))
}

#sample sizes
n1 = 20
n2 = 30

#repeated times
m = 10000

#calculate the power
sigma1 = 1
sigma2 = 1

x = rnorm(n1, 0, sigma1)
y = rnorm(n2, 0, sigma2)
z = c(x, y)
t0 = max.ext.pts(x, y)

  hat.p = (sum(replicate((m-1), expr={
    idx = sample(1:(n1+n2), size = n1, replace = FALSE)
    as.integer(max.ext.pts(z[idx], z[-idx])>=t0)
}))+1)/m

print(paste("The estimate of p value is ", round(hat.p, 4), ".", sep = ""))

## -----------------------------------------------------------------------------
set.seed(5)
#maximum of extreme points 
max.ext.pts = function(x, y) {
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y))
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(max(c(outx, outy)))
}

#sample sizes
n1 = 20
n2 = 50

#repeated times
m = 10000

#calculate the power
sigma1 = 1
sigma2 = 1

x = rnorm(n1, 0, sigma1)
y = rnorm(n2, 0, sigma2)
z = c(x, y)
t0 = max.ext.pts(x, y)

  hat.p = (sum(replicate((m-1), expr={
    idx = sample(1:(n1+n2), size = n1, replace = FALSE)
    as.integer(max.ext.pts(z[idx], z[-idx])>=t0)
}))+1)/m

print(paste("The estimate of p value is ", round(hat.p, 4), ".", sep = ""))

## -----------------------------------------------------------------------------
library(RANN) 
library(boot)
library(energy)
library(Ball)

Tn = function(z, ix, sizes,k) {
n1 = sizes[1]; n2 = sizes[2]; n = n1 + n2
if(is.vector(z)) z = data.frame(z,0);
z = z[ix, ];
NN = nn2(data=z, k=k+1) # what's the first column?
block1 = NN$nn.idx[1:n1,-1]
block2 = NN$nn.idx[(n1+1):n,-1]
i1 = sum(block1 < n1 + .5); i2 = sum(block2 > n1 + .5)
(i1 + i2) / (k * n)
}


m = 1e2; k=3; p=2;  
n1 = n2 = 50; R=999; n = n1+n2; N = c(n1,n2)

eqdist.nn = function(z,sizes,k){
boot.obj = boot(data=z,statistic=Tn,R=R,
sim = "permutation", sizes = sizes,k=k)
ts = c(boot.obj$t0,boot.obj$t)
p.value = mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)
}

p.values = matrix(0,m,3)

for(i in 1:m){
x = matrix(rnorm(n1*p, 0, 1.5), ncol = p)
y = cbind(rnorm(n2),rnorm(n2))
z = rbind(x,y)


p.values[i,1] = eqdist.nn(z,N,k)$p.value
p.values[i,2] =eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] = bd.test(x=x,y=y,num.permutations=R, seed = i)$p.value
}

alpha = 0.1;
pow = colMeans(p.values<alpha)
knitr::kable(data.frame(t(pow), row.names = "Power"), col.names = c("NN", "Energy", "Ball"))

## -----------------------------------------------------------------------------
mu = 0.3
p.values = matrix(0,m,3)

for(i in 1:m){
x = matrix(rnorm(n1*p, 0, 1.5), ncol = p)
y = cbind(rnorm(n2),rnorm(n2,mean=mu))
z = rbind(x,y)

p.values[i,1] = eqdist.nn(z,N,k)$p.value
p.values[i,2] =eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] = bd.test(x=x,y=y,num.permutations=R, seed = i)$p.value
}

alpha = 0.1;
pow = colMeans(p.values<alpha)
knitr::kable(data.frame(t(pow), row.names = "Power"), col.names = c("NN", "Energy", "Ball"))

## -----------------------------------------------------------------------------
for(i in 1:m){
x = matrix(0.5*rnorm(n1*p, -0.1, 3)+0.5*rnorm(n1*p, 0.1, 3), ncol = p)
y = cbind(rt(n2, df = 1), rt(n2, df = 1))
z = rbind(x,y)


p.values[i,1] = eqdist.nn(z,N,k)$p.value
p.values[i,2] =eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] = bd.test(x=x,y=y,num.permutations=R, seed = i)$p.value
}

alpha = 0.1;
pow = colMeans(p.values<alpha)
knitr::kable(data.frame(t(pow), row.names = "Power"), col.names = c("NN", "Energy", "Ball"))

## -----------------------------------------------------------------------------
n1 = 5
n2 = 50
n = n1+n2; N = c(n1,n2)
p.values = matrix(0,m,3)

for(i in 1:m){
x = matrix(rnorm(n1*p, 0, 1.5), ncol = p)
y = cbind(rnorm(n2),rnorm(n2,mean=mu))
z = rbind(x,y)

p.values[i,1] = eqdist.nn(z,N,k)$p.value
p.values[i,2] =eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] = bd.test(x=x,y=y,num.permutations=R, seed = i)$p.value
}

alpha = 0.1;
pow = colMeans(p.values<alpha)
knitr::kable(data.frame(t(pow), row.names = "Power"), col.names = c("NN", "Energy", "Ball"))

## -----------------------------------------------------------------------------
dlaplace <- function(x){
  0.5*exp(-abs(x))
}

rw.Metropolis <- function(sigma, x0, N) {
x <- numeric(N)
x[1] <- x0
u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <= (dlaplace(y) / dlaplace(x[i-1])))
x[i] <- y 
else {
x[i] <- x[i-1]
k <- k + 1
}
}

return(list(x=x, k=k))
}

N <- 4000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
knitr::kable(data.frame(t(paste(round(1 - c(rw1$k, rw2$k, rw3$k, rw4$k)/N, 4)*100, "%", sep = "")), row.names = "Accpectance Rate"), col.names = paste("sigma:", sigma))

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) #row means
B <- n * var(psi.means) #between variance est.
psi.w <- apply(psi, 1, "var") #within variances
W <- mean(psi.w) #within est.
v.hat <- W*(n-1)/n + (B/n) #upper variance est.
r.hat <- v.hat / W #G-R statistic
return(r.hat)
}


k <- 4
b <- 1000

N <- 4000
sigma <- 2
x0 <- c(-10, -5, 5, 10)
rw1 <- rw.Metropolis(sigma, x0[1], N)
rw2 <- rw.Metropolis(sigma, x0[2], N)
rw3 <- rw.Metropolis(sigma, x0[3], N)
rw4 <- rw.Metropolis(sigma, x0[4], N)

rw <- list(rw1, rw2, rw3, rw4)
X <- rbind(rw1$x, rw2$x, rw3$x, rw4$x)

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))
#plot psi for the four chains

#par(mfrow=c(2,2))
for (i in 1:k)
plot(psi[i, (b+1):N], type="l",
xlab=i, ylab=bquote(psi))
#par(mfrow=c(1,1)) #restore default

#plot the sequence of R-hat statistics
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
gka = function(k, a){
  pt(sqrt(a^2*(k-1)/(k-a^2)), df = k-1) - pt(sqrt(a^2*(k)/(k+1-a^2)), df = k)
}

plot.gka = function(k, plot.dim){
  #plot.dim is a 2-dim vector for plots
  #par(mfrow=plot.dim)
  n = length(k)
  
  for (i in 1:n) {
    a = seq(from = 0, to = sqrt(k[i]), length.out = 50)[-50]
    plot(x = a, y = gka(k[i], a), type="l",
    xlab=i, ylab=bquote(g))
    abline(h=0, lty=2)
  }
  
  #par(mfrow=c(1,1)) #restore default
  
}

plot.gka(c(4, 8, 12, 24), c(2, 2))
plot.gka(c(25, 100, 500, 1000), c(2, 2))

## -----------------------------------------------------------------------------
library("kableExtra")
k = c(4:25, 100, 500, 1000)
n = length(k)
res = numeric(n)

for (i in 1:n) {
  res[i] <- uniroot(function(a) {
gka(k[i], a) },
lower = 0.1, upper = min(sqrt(k[i])-0.1, 5))$root
}

dt = t(data.frame("k" = as.character(k), "root" = res))

kbl(dt) %>%  kable_paper() %>%
  scroll_box(width = "900px", height = "100%")

## -----------------------------------------------------------------------------
#Known Parameters
na = 444
nb = 132
noo = 361
nab = 63

#EM Algorithm

para.old = c(0.4, 0.4)
para.ma = matrix(para.old, nrow = 2)
para = c(0.1, 0.1)
thres = 1e-6

while (sum((para-para.old)^2)>thres) {
  para.old = para
  r0 = 1 - sum(para.old)
  #Update p, q
  A1 = na+nab+na*para.old[1]/(para.old[1]+2*r0)
  A2 = nb+nab+nb*para.old[2]/(para.old[2]+2*r0)
  B = 2*noo+2*na*r0/(para.old[1]+2*r0)+2*nb*r0/(para.old[2]+2*r0)
  para[1] = A1/(A1+A2+B)
  para[2] = A2/(A1+A2+B)
  para.ma = cbind(para.ma, para)
}

knitr::kable(data.frame(t(round(para, 3)), row.names = "Estimate"), col.names = c("p", "q"))

## -----------------------------------------------------------------------------
n = ncol(para.ma)
l = numeric(n)
for (i in 1:n) {
  p = para.ma[1, i]
  q = para.ma[2, i]
  r = 1-p-q
  l[i] = na*log(p^2+2*p*r)+nb*log(q^2+2*q*r)+2*noo*log(r)+nab*log(2*p*q)
}

knitr::kable(data.frame(t(round(l, 3)), row.names = "Log Likelihood"), col.names = 1:n)

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

## -----------------------------------------------------------------------------
n = length(formulas)
model = list(NULL)
for (i in 1:n) {
  model[[i]] = lm(formulas[[i]], mtcars)
}
model

## -----------------------------------------------------------------------------
model = lapply(formulas, function(fm) lm(fm, mtcars))

model

## -----------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)

## -----------------------------------------------------------------------------
library("kableExtra")
pvalues = sapply(trials, function(test) test$p.value)

dt = data.frame(t(pvalues))
row.names(dt) = "p values"
kbl(dt, col.names = 1:100, row.names = TRUE) %>%  kable_paper() %>%
  scroll_box(width = "900px", height = "100%")

## -----------------------------------------------------------------------------
pvalues = sapply(trials, "[[", 3)

dt = data.frame(t(pvalues))
row.names(dt) = "p values"
kbl(dt, col.names = 1:100, row.names = TRUE) %>%  kable_paper() %>%
  scroll_box(width = "900px", height = "100%")

## -----------------------------------------------------------------------------
lapply.para = function(x, f, ...){
  #parallel computing on x by col
  Map(f = function(x){vapply(x, FUN = f, FUN.VALUE = numeric(1))}, x)
}

eg = list(mtcars, cars)

ptm <- proc.time()
lapply(eg, function(x) vapply(x, mean, numeric(1)))
proc.time()-ptm

ptm <- proc.time()
lapply.para(eg, mean)
proc.time()-ptm

## -----------------------------------------------------------------------------
dlaplace = function(x){
  0.5*exp(-abs(x))
}

rw.Metropolis = function(sigma, x0, N) {
x = numeric(N)
x[1] = x0
u = runif(N)
k = 0
for (i in 2:N) {
y = rnorm(1, x[i-1], sigma)
if (u[i] <= (dlaplace(y) / dlaplace(x[i-1])))
x[i] = y 
else {
x[i] = x[i-1]
k = k + 1
}
}

return(list(x=x, k=k))
}

N = 4000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1 = rw.Metropolis(sigma[1], x0, N)
rw2 = rw.Metropolis(sigma[2], x0, N)
rw3 = rw.Metropolis(sigma[3], x0, N)
rw4 = rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
knitr::kable(data.frame(t(paste(round(1 - c(rw1$k, rw2$k, rw3$k, rw4$k)/N, 4)*100, "%", sep = "")), row.names = "Accpectance Rate"), col.names = paste("sigma:", sigma))

## ---- message=FALSE-----------------------------------------------------------
library(StatComp20026)
x = runif(1e4); mean2 = function(x)sum(x)/length(x)
N = 4000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1 = rwM(sigma[1], x0, N)
rw2 = rwM(sigma[2], x0, N)
rw3 = rwM(sigma[3], x0, N)
rw4 = rwM(sigma[4], x0, N)

#plot psi for the four chains
#par(mfrow=c(2,2))

plot(y = rw1[[1]], x = seq_along(rw1[[1]]),  type="l",
xlab= "index", ylab="random walk samples", main = paste0("sigma = ", sigma[1]))
plot(y = rw2[[1]], x = seq_along(rw2[[1]]),  type="l",
xlab= "index", ylab="random walk samples", main = paste0("sigma = ", sigma[2]))
plot(y = rw3[[1]], x = seq_along(rw3[[1]]),  type="l",
xlab= "index", ylab="random walk samples", main = paste0("sigma = ", sigma[3]))
plot(y = rw4[[1]], x = seq_along(rw4[[1]]),  type="l",
xlab= "index", ylab="random walk samples", main = paste0("sigma = ", sigma[4]))

#par(mfrow=c(1,1)) #restore default

#number of candidate points rejected
knitr::kable(data.frame(t(paste(round(1 - c(rw1[[2]], rw2[[2]], rw3[[2]], rw4[[2]])/N, 4)*100, "%", sep = "")), row.names = "Accpectance Rate"), col.names = paste("sigma:", sigma))

## -----------------------------------------------------------------------------
sample.R = rw.Metropolis(sigma[3], x0, N)
sample.C = rwM(sigma[3], x0, N)

qqplot(x = sort(sample.R[[1]]), y = sort(sample.C[[1]]), xlab = "Samples from R", ylab = "Samples from Cpp", main = "QQ plot between R and Cpp")
abline(0,1,col = "red")

## ---- message=FALSE-----------------------------------------------------------
library('microbenchmark')
ts = microbenchmark(sampleR=rw.Metropolis(sigma[3], x0, N), sampleC=rwM(sigma[3], x0, N))
summary(ts)[,c(1,3,5,6)]

