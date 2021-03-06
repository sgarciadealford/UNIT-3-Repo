
SampDistSampMean = function(n = 100, simulations = 1000, Dist = "Norm", mean = 0, sd = 1)
{
  xbar_holder = numeric(n)
  TheSample = numeric(n)
  for(i in 1:simulation)     # for loop to make "simulation" number of xbars
  {
    if(Dist == "Norm") TheSample = rnorm(n,mean,sd)
    if(Dist == "Uni")  TheSample = runif(n)
    if(Dist == "Exp") TheSample = rexp(n)
    
    sampMean = mean(TheSample)  
    
    xbar_holder[i] = sampMean
    
  }
  return(TheSample)   # we want to return the "simulation" number of xbars. 
}

hist(SampDistSampMean())

