rs <- function(f, nsample, envelope="n", lower.bound=-4, upper.bound=4)
{

    ## envelop = "n" : normal pdf, "e" : exponential pdf
    
if(envelope=="n") {env.exp = function(x){dnorm(x)}; env.ran = function(x){rnorm(x)}}
    else if(envelope=="e") {env.exp = function(x){dexp(x)} ; env.ran = function(x){rexp(x)}} else 
      {stop("Please enter an appropriate option for envelope.")}
    
    w = runif(1e4, lower.bound, upper.bound) ## 1e4 : large number
    alphaInverse = 1
    check=F
    
    while(!all(check))
    {
      alphaInverse = alphaInverse+1
      outer.exp = function(x){env.exp(x) * alphaInverse}
      check = (exp(f(w)) <= outer.exp(w))
      if(alphaInverse>=1e3)
      {
        stop("There seems no appropriate envelope.")
        break;
      }
    }
      
    
    ## iteration 
    
    accept_size   = 0
    res = c()
    iter = 0
    
    while(accept_size < nsample) 
    {
      y = env.ran(1)
      u = runif(1)
      if (u <= exp(f(y)) / outer.exp(y)) ##
      {
        accept_size = accept_size + 1
        res[accept_size] = y
      }
      iter = iter + 1
    }
    
    result = list(sample=res, iteration=iter)
    
    return(result)
    
  }
