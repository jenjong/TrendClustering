# simulation 1
  if (sce == 1)
  {
    trend.vec = c(rep(-5,10),rep(0,10), rep(5,10))
    set.seed(1105+k)
    eta.vec = rnorm(p, 120, 20)
    alpha.vec = rnorm(p, 40,5)
    kappa.vec = runif(p,-0.3,0.3)
    
    preci = matrix(0,p,nj+1) ; preci[,1] = 1:p
    for ( i in 1:p)
    {
      preci[i,-1] = rgev(nj, eta.vec[i], alpha.vec[i], kappa.vec[i]) + trend.vec[i]*(1:nj) - mean(trend.vec[i]*(1:nj))
    }
    conv.data = gen.y(preci, m = 30)
    adj.mat = NULL
    for ( i in 1:(p-1))
    {
      for ( j in (i+1):min(i+15,p))
      {
        tmp = c(i,j)
        adj.mat = rbind(tmp,adj.mat)
      }
    }    
  }
  
  # simulation 1
  if (sce == 2)
  {
    trend.vec = c(rep(-5,5),rep(-2.5,5),rep(0,10), rep(2.5,5),  rep(5,5))
    set.seed(1105+k)
    eta.vec = rnorm(p, 120, 20)
    alpha.vec = rnorm(p, 40,5)
    kappa.vec = runif(p,-0.3,0.3)
    
    preci = matrix(0,p,nj+1) ; preci[,1] = 1:p
    for ( i in 1:p)
    {
      preci[i,-1] = rgev(nj, eta.vec[i], alpha.vec[i], kappa.vec[i]) + trend.vec[i]*(1:nj) - mean(trend.vec[i]*(1:nj))
    }
    conv.data = gen.y(preci, m = 30)
    adj.mat = NULL
    for ( i in 1:(p-1))
    {
      for ( j in (i+1):min(i+15,p))
      {
        tmp = c(i,j)
        adj.mat = rbind(tmp,adj.mat)
      }
    }    
  }
  
  