# Quantile regression with fused penalty
# arguments 
#   conv.data :
#   adj.mat   :

fused.QR.fun = function(conv.data, adj.mat,lambda1,lambda2)
{
  #### Const [eta+][eta-][beta+][beta-][beta_jk+][beta_jk-]
  site.length =unlist(lapply(conv.data$diffy,length))
  n = sum(site.length)
  p = length(conv.data$diffy)
  
  # const1 
  #eta
  etaconstP = etaconstN = matrix(0,n,3)
  etaconstP[,1] = etaconstP[,2] = etaconstN[,1] = etaconstN[,2] = 1:n
  etaconstP[,3] = 1
  etaconstN[,3] = -1
  # betaP
  betaconstP=matrix(0,n,3)
  betaconstP[,1] = 1:n
  betaconstP[,2] = rep(1:p,unlist(lapply(conv.data$diffx,length)))
  betaconstP[,3] = unlist(conv.data$diffx)
  # betaN
  betaconstN=matrix(0,n,3)
  betaconstN[,1] = 1:n
  betaconstN[,2] = rep(1:p,unlist(lapply(conv.data$diffx,length)))
  betaconstN[,3] = -unlist(conv.data$diffx)
  # rbind
  etaconstN[,2] = etaconstN[,2] + n
  betaconstP[,2] = betaconstP[,2] + 2*n
  betaconstN[,2] = betaconstN[,2] + 2*n + p
  const1.mat = rbind(etaconstP,etaconstN, betaconstP, betaconstN)
  rm(list =c('etaconstP','etaconstN','betaconstP','betaconstN' )); gc()
  
  
  # const2
  #adj.mat
  n3 = nrow(adj.mat)
  betaconstP = matrix(0,2*n3,3)
  betaconstN = matrix(0,2*n3,3)
  beta2constP = beta2constN = matrix(0,n3,3)
  betaconstP[,1] = rep(1:n3,2);   betaconstP[(1:n3),2] = adj.mat[,1]; betaconstP[(1:n3),3] = -1
  betaconstP[(1:n3)+n3,2] = adj.mat[,2]; betaconstP[(1:n3)+n3,3] = 1
  betaconstN[,1] = rep(1:n3,2);   betaconstN[(1:n3),2] = adj.mat[,1]; betaconstN[(1:n3),3] = 1
  betaconstN[(1:n3)+n3,2] = adj.mat[,2]; betaconstN[(1:n3)+n3,3] = -1
  
  beta2constP[,1] = beta2constP[,2] = 1:n3; beta2constP[,3] = 1
  beta2constN[,1] = beta2constN[,2] = 1:n3; beta2constN[,3] = -1
  # rbind
  betaconstP[,2] = betaconstP[,2] + 2*n
  betaconstN[,2] = betaconstN[,2] + 2*n + p
  beta2constP[,2] = beta2constP[,2] + 2*n+ 2*p
  beta2constN[,2] = beta2constN[,2] + 2*n+ 2*p + n3
  const2.mat = rbind(betaconstP, betaconstN, beta2constP,beta2constN) 
  rm(list = c('betaconstP','betaconstN','beta2constP', 'beta2constN'))
  
  # const aggregation
  const2.mat[,1]= const2.mat[,1]+n
  const.mat = rbind(const1.mat, const2.mat)
  rm(list  = c('const1.mat', 'const2.mat')) ; gc()
  # RHS AND DIRECTION OF CONSTRAINT   
  const.rhs = c(unlist(conv.data$diffy), rep(0, n3))
  const.dir = rep('==',length(const.rhs))   
  
  # cost.vec
  costVec = c(rep(1,n), rep(1,n), rep(lambda1,p), rep(lambda1,p), rep(lambda2,n3), rep(lambda2,n3))
  
  
  # FITTING OS LINEAR PROGRAMING  
  fit2 = lp (direction= "min", objective.in = costVec, 
             const.dir = const.dir, const.rhs = const.rhs, 
             dense.const = const.mat )
  solVec = fit2$solution
  # RETURN SOLUTIONS           
  betaVec = solVec[(2*n+1):(2*n+p)] - solVec[(2*n+p+1):(2*n+2*p)]
    return(betaVec) 
}


# Difference 
#     Arguments
#       preci:[site.idx][y1][y2]...[yn]
#       m    : size of lag

gen.y = function(preci,m=1){
  
  y = preci[,-1, drop = F]
  id = preci[,1]
  diffy = diffx = vector(mode='list', length = nrow(y))
  for ( i in 1:nrow(y))
  {
    tmp1 = tmp2 = c()
    for (j in 1:m)
    { 
      z = diff(y[i,],lag = j)
      tmp1 = c(tmp1,z)
      tmp2 = c(tmp2,rep(j,length(z)) )
    }
    idx = !is.na(tmp1)
    diffy[[i]] = tmp1[idx]
    diffx[[i]] = tmp2[idx]
  }
  return(list (diffy = diffy, diffx = diffx )) 
}


# Distance function
#   Argument
#     site: [site$L][site$A]

dist.fun = function(site)
{
  x = as.numeric(substring(site$L,1,2))*111 + as.numeric(substring(site$L,4,5))*1.85
  y = as.numeric(substring(site$A,1,3))*88.8 + as.numeric(substring(site$A,5,6))*1.48
  n = length(x)
  dist.mat = matrix(0,n*(n-1)/2,3)
  idx = 1
  for ( i in 1:(n-1))
  {
    for ( j in (i+1):n)
    {
      z = sqrt((x[i]-x[j])^2 + (y[i]-y[j])^2)
      dist.mat[idx,] = c(i,j, z)
      idx = idx + 1
    }    
  }
  return(dist.mat)  
}


refit.fun = function(conv.data, betaVec)
{
  betaVec = round(betaVec,5)
  rbeta.vec = rep(0, length(betaVec))
  u.beta = unique(betaVec)
  g.idx = 1:length(u.beta)
  for ( j in 1:length(g.idx))
  {
    sel = which(betaVec == u.beta[g.idx[j]])
    y = unlist( conv.data$diffy[sel] )
    x = unlist(conv.data$diffx[sel])
    fit = rq.fit(x,y)
    rbeta.vec[sel] =  fit$coefficients    
  }
  return(rbeta.vec)
}