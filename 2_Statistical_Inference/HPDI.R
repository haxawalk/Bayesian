# Define PDFs

PDF = function(x)
{
  return(dgamma(x, shape = 5, scale = 1))
}


PDF2 = function(x)
{
  ## support : [-2 * pi , pi]
  norm.const = 11.42478
  return((sin(x) + 1)/norm.const)
}



# HPDI

HPDI = function(fun, credible=.95, delta=.1, epsilon=1e-3, lb=0, ub=20)
{
  
  ## partitioning
  partition = seq(lb,ub, by=epsilon)
  xstar     = head(partition, -1) + epsilon/2
  
  
  ## realms
  sortcum   = cumsum(sort(fun(xstar) * epsilon, decreasing=T))
  realm_TF  = sortcum < sortcum[length(sortcum)] * credible
  realm_idx = order(fun(xstar) * epsilon, decreasing=T)[realm_TF]
  realm     = sort(xstar[realm_idx])
  
  
  ## finding the intervals
  intervals_finder = function(realm_)
  {
    size  = length(realm_)
    
    main  = realm_[-size]
    sub   = realm_[-1]
    
    criticals = (1:size)[abs(main-sub) >= delta]
    breaks    = realm_[sort(c(1, criticals, criticals+1, size))]
    
    for(i in 1:(length(breaks)/2))
    {
      cat("Interval : [", breaks[2*i - 1], breaks[2*i], "]\n")
    }
  }
  
  return(intervals_finder(realm))
}



# Validation

HPDI(PDF)
integrate(PDF, 1.2075, 9.4285)

HPDI(PDF2, lb=-2*pi, ub=pi)
integrate(PDF2, -6.2826, -2.7986)$value + integrate(PDF2, -0.3427, 3.1403)$value


# Plot


# Matlab Optimization
