rm(list = ls())
one_day = function(X_curr)
  {
    p = runif(1)
    if (X_curr == 0)
      {
        if (p <= 0.01) #beta
          {
          X = 1
          }else{
          X = X_curr
          }
        }else if(X_curr == 1){
        if (p<= 0.10) #gamma
          {
          X = 2
          }else{
          X = X_curr 
          }
        }else if(X_curr == 2){
        if ( p<= 0.005) #alpha
          {
          X = 0
          }else{
          X = X_curr
          }
        }
    X
  }

one_year = function(X0)
  {
    X_curr = X0
    counts = array(0, dim = 3) #respectively for state 0,1,2 (indices 1,2,3)
    counts[X0+1] = counts[X0+1] + 1
    num_days = 365
    for (i in 1:365)
      {
        X = one_day(X_curr)
        counts[X+1] = counts[X+1] + 1
        X_curr = X
      }
  list(counts,X)
  }

#1) simulate for 20 years
twenty_yrs = function(X0)
  {
  X_start = X0
  counts_last_ten_years = array(0,dim=3)
  for(i in 1:20)
    {
      results = one_year(X_start)
      counts = results[[1]]
      X = results[[2]]
      if (i > 10)
        {
          #summing up number of days in each states for the last 10 of these 20 years
          counts_last_ten_years = counts_last_ten_years + counts #x0 = previous year final X, first element returned from one_year are the numebr of days in each state
        }
      X_start = X
    }
  #returning mean number of days
  mean_count = counts_last_ten_years/10
  mean_count
  }

#run simulation 30 times
counts_each_year = array(dim=c(30,3))
for (i in 1:30)
  {
    mean_counts = twenty_yrs(0) #assumed X0 = 0
    counts_each_year[i, ] = mean_counts
  }
cat("Mean number of days spent in state S: ", mean(counts_each_year[,1]), "\n") #pi0
qqnorm(counts_each_year[,1], main = "Normal Q-Q Plot for long-run average days spent in state S")
qqline(counts_each_year[,1])

cat("Mean number of days spent in state I: ", mean(counts_each_year[,2]), "\n") #pi1
qqnorm(counts_each_year[,2], main = "Normal Q-Q Plot for long-run average days spent in state I")
qqline(counts_each_year[,2])

cat("Mean number of days spent in state R: ", mean(counts_each_year[,3])) #pi2
qqnorm(counts_each_year[,3], main = "Normal Q-Q Plot for long-run average days spent in state R")
qqline(counts_each_year[,3])

#can approximate as normally distributed with unknown mu, sigma -> approximate with S and X_average -> student t quantiles -> confidence interval
#for each state:
q = 2.045 #t quantile, alpha/2 = 0.025, n-1 = 29
n = 30
for (i in 1:3)
  {
  X_bar = mean(counts_each_year[,i]) #empirical mean
  S = 0
  for (j in 1:n)
    {
      X_j = counts_each_year[j,i]
      S = S + (X_j - X_bar)^2
    }
  S = S/(n-1)
  S = sqrt(S) #empirical standard deviation
  #confidence interval
  lower = X_bar - q*S/sqrt(n)
  upper = X_bar + q*S/sqrt(n)
  cat("95% confidence interval for state ", i-1, " : [ ", lower, ", ", upper, "]\n")
  }

#checking if true value in interval 95% of the time:
N = 1000
pi = c(117.74, 11.774, 235.48) #pi0,pi1,pi2 analytical values
num_times_within_CI = array(0, dim=3) #states 0,1,2 respectively
pb <- txtProgressBar(min = 1, max = N, style = 3) #progress bar
for (i in 1:1000)
  {
  setTxtProgressBar(pb, i)
  counts_each_year = array(dim=c(30,3))
  for (i in 1:30)
    {
      mean_counts = twenty_yrs(0) #assumed X0 = 0
      counts_each_year[i, ] = mean_counts
    }
  for (i in 1:3)
    {
      X_bar = mean(counts_each_year[,i]) #empirical mean
      S = 0
      for (j in 1:n)
        {
          X_j = counts_each_year[j,i]
          S = S + (X_j - X_bar)^2
        }
      S = S/(n-1)
      S = sqrt(S) #empirical standard deviation
      #confidence interval
      lower = X_bar - q*S/sqrt(n)
      upper = X_bar + q*S/sqrt(n)
      if (pi[i] >= lower && pi[i] <= upper )
      {  
        num_times_within_CI[i] = num_times_within_CI[i] + 1
      }
    }
  fraction_within_CI = num_times_within_CI/N
  }

cat("Number of times true value for state S was within CI:\n", fraction_within_CI[1])
cat("Number of times true value for state I was within CI:\n", fraction_within_CI[2])
cat("Number of times true value for state R was within CI:\n", fraction_within_CI[3])

# -------------------------------------------------------------------------

