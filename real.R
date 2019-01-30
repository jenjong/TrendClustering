# Introduction to the use of library for real data analysis
rm(list=ls()) ; gc()
setwd("C:/Users/Jeon/Documents/GitHub/TrendClustering")
source('./library/fusedQR.r')
Sys.setlocale("LC_ALL", "korean")
library(lpSolve)
library(evd)
library(quantreg)

# load AMPs 
load("./data/preci.rdata")
conv.data = gen.y(preci, m = 15)
# define the network N by locational information
N = dist.fun(site)
N = N[N[,3]<=150 ,-3]

## fitting for the fused lasso
betaVec = fused.QR.fun (conv.data, N,lambda1=3,lambda2= 30)
betaVec

# refitting
rcvbetaVec = refit.fun(conv.data, betaVec)
numActive = c()
for (i in 1:nrow(rbeta.mat))  numActive[i] = length(unique(rbeta.mat[i,]))
BIC.vec = c()
y = unlist(conv.data$diffy)
x = unlist(conv.data$diffx)
z = unlist(lapply(conv.data$diffx,length))
p = 60
nj = 43
for (i in 1:ncol(beta.mat))
{
  tmp = rbeta.mat[i,]
  eff.n = p*nj
  BIC.vec[i] = log(sum(abs(y-rep(tmp,z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
}
par(mfrow = c(1,1))
tmp = log(sum(abs(y-rep(rep(0,60),z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
BIC.vec1 = c(tmp, BIC.vec)
plot(c(51,lambda.vec), BIC.vec1, type= 's', ylab = 'BIC', main = 'BIC plot', xlab = 'tuning parameter')
write.csv(rbeta.mat, file = '..\\result\\rbetaMat.csv',row.names = F)

rbeta.mat[6,]

# (8), (27,28)
site[27:28,]
site[8,]
write.csv(rbeta.mat, file = '..\\result\\rbetaMat.csv',row.names = F)
write.csv(site,, file = '..\\result\\site.csv',row.names = F)



tmp = log(sum(abs(y-rep(rep(0,60),z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
BIC.vec = c(tmp,BIC.vec)
plot(BIC.vec, type ='s', ylab = 'BIC', ylab = )


numActive = c()
for (i in 1:ncol(betaMat))
  numActive[i] = length(unique(rbetaMat[,i]))

BIC.vec = c()
for (i in 1:ncol(betaMat))
{
  tmp = betaMat[,i]
  BIC.vec[i] = sum(abs(y-tmp[siteIdx]))/2 +  log(length(y))*numActive[i]
}
plot(log(lambda.vec),BIC.vec, type = 's', main = 'BIC of estimated models', lwd = 2,
     xlab = 'log(lambda)', ylab = 'BIC', bg='lightblue')
which(BIC.vec==min(BIC.vec))
numActive[23]
write.csv(betaMat, file = '..\\result\\betaMat.csv',row.names = F)
write.csv(site,, file = '..\\result\\site.csv',row.names = F)


