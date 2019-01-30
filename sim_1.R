# Simulation 1
rm(list=ls()) ; gc()
setwd("C:/Users/Jeon/Documents/GitHub/TrendClustering")
source('./library/fusedQR.r')
library(lpSolve)
library(evd)
library(quantreg)
# simulation Setting
sce = 2
nj = 30
p = 30
k = 1
iter = 200
beta.list = vector(mode='list', length = iter)
rbeta.list = vector(mode='list', length = iter)
for (k in 1:iter)
{
  set.seed(1105+k)
  source('./library/simulation.r')
  # conv.data and adh.mat are obtained by simulation.r
  lambda.vec = seq(300, 100, by = - 10)
  beta.mat = NULL
  for ( i in 1:length(lambda.vec))
  {
    cat(k,'th iter:',i,'\n')  
    lambda = lambda.vec[i]
    beta.vec= fused.QR.fun (conv.data, adj.mat,lambda1=0,lambda2=lambda)
    beta.vec= round(beta.vec,3)
    cat("### num of u Coef is ",length(unique(beta.vec)),'\n')
    beta.mat = rbind(beta.mat,beta.vec)
  }
  
  beta.list[[k]] = beta.mat
  
  ## BIC (refit)
  rbeta.mat = NULL
  for ( i in 1:nrow(beta.mat))
  {
    
    rbeta.vec = beta.vec = beta.mat[i,]
    u.beta = unique(beta.vec)
    g.idx = 1:length(u.beta)
    j = 1
    
    for ( j in 1:length(g.idx))
    {
      sel = which(beta.vec == u.beta[g.idx[j]])
      y = unlist( conv.data$diffy[sel] )
      x = unlist(conv.data$diffx[sel])
      fit = rq.fit(x,y)
      rbeta.vec[sel] =  fit$coefficients    
    }
    rbeta.mat = rbind(rbeta.mat,rbeta.vec)
  }
  numActive = c()
  for (i in 1:nrow(rbeta.mat))  numActive[i] = length(unique(rbeta.mat[i,]))
  BIC.vec = c()
  y = unlist(conv.data$diffy)
  x = unlist(conv.data$diffx)
  z = unlist(lapply(conv.data$diffx,length))
  for (i in 1:nrow(beta.mat))
  {
    tmp = rbeta.mat[i,]
    eff.n = p*nj
    BIC.vec[i] = log(sum(abs(y-rep(tmp,z)*x))) +  log(eff.n)*numActive[i]/eff.n/2
  }
  rbeta.list[[k]]= rbeta.mat[which.min(BIC.vec),]
  
}


plot(BIC.vec, type = 's', col = 'red')


