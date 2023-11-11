#=================2. TLC ===========

# ------- 1 ----------
tclbernoulli <- function(N,p){
  nb.echant <- 1000 
  res <- rep(0,nb.echant) 
  for (i in 1:nb.echant){
    data <- rbinom(N,1,p) 
    moy.emp <- mean(data) 
    res[i] <- sqrt(N/(p*(1-p)))*(moy.emp-p) 
  }
  return(res)
}

# ------- 2 ----------

s<-tclbernoulli(10,0.2)
mean(abs(s)<1.96)      
# Do the same for N=100 and N=1000
#...

# ------- 3 ----------
par(mfrow=c(1,3))

res <- tclbernoulli(10,.2) 
fdr10 <- ecdf(res)
plot.ecdf(res,main="FdR, N=10",cex=0.5)
lines(sort(res),pnorm(sort(res)),col='red')

# Do the same for N=100 and N=1000

#=================3. LLN ===========

#---------------1----------
par(mfrow=c(1,1))
Nfin <- 5000
X <- rexp(Nfin,2)       
Y <- cumsum(X)/1:Nfin   
plot(1:Nfin, Y, type='l', ylim=c(0,1), xlab='n', ylab='moyenne empirique')
for (i in 2:50){
  X <- rexp(Nfin,2)
  Y <- cumsum(X)/1:Nfin
  lines(1:Nfin, Y, col=i)   
}

#---------------- 2 ---------------

lgnexpo <- function(N){
  moy=rep(0,100) 
  for (i in 1:100){ 
    moy[i]=mean(rexp(N,2)) 
  }
  return(moy) 
}


boxplot(lgnexpo(100),lgnexpo(1000),lgnexpo(10000), names=c("100","1000","10000"),xlab='N: taille de l echantillon')


#---------------- 3 ---------------

Nfin <- 5000
X <- rcauchy(Nfin)
Y <- cumsum(X)/1:Nfin
plot(1:Nfin, Y, ylim=c(-50,50),type='l', xlab='n', ylab='moyenne empirique')
for (i in 2:10){
  X <- rcauchy(Nfin)
  Y <- cumsum(X)/1:Nfin
  lines(1:Nfin, Y, col=i)
}

lgncauchy <- function(N){
  moy <- rep(0,100) 
  for (i in 1:100){ 
    moy[i] <- mean(rcauchy(N))
  }
  return(moy) 
}

boxplot(lgncauchy(100),lgncauchy(1000),lgncauchy(10000),outline=T,names=c("100","1000","10000"),xlab='N: taille de l echantillon')

boxplot(lgncauchy(100),lgncauchy(1000),lgncauchy(10000),outline=F,names=c("100","1000","10000"),xlab='N: taille de l echantillon')


#=================3. Poisson law toward binomial law ===========

#----1-----
lambda=8
Bin10=rbinom(1000,10,lambda/10)
# Do the same for N=10,20, 30 and 100

#----2-----

M=max(Bin10,Bin20,Bin30,Bin100)
k=seq(0,M,by=1)
dpois(k,lambda)

#----3-----

par(mfrow=c(2,2))

plot(table(Bin10)/1000,main='N=10')
points(k,dpois(k,lambda),col='red')
# Do the same for N=10,20, 30 and 100

#----4-----

plot(ecdf(Bin10),main='N=10')
lines(k,ppois(k,lambda),type='s',col='red')
# Do the same for N=10,20, 30 and 100