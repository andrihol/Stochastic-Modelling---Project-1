rm(list = ls())

simulate_n_times = function(n, Y_curr, N,v) #Y_curr = (Sn, In, Rn), N size of population, v = num of vaccinations
{
  S_n = Y_curr[1] - v #subtract v vaccinated individuals from susceptible population, but keep N total individuals in populatopn
  I_n = Y_curr[2]
  R_n = Y_curr[3]
  
  realizations = array(dim = c(n, 3)) #to store steps
  
  alpha = 0.005
  gamma = 0.10
  
  for (i in 1:n)
  {
    beta = 0.5*I_n/N
    S_new = rbinom(1, R_n, alpha) #S_new is based on the number of candidates who can move to S which is the current number of individuals in R
    R_new = rbinom(1, I_n, gamma)
    I_new = rbinom(1, S_n, beta)
    
    #at next time step
    S_n = S_n + S_new - I_new
    I_n = I_n + I_new - R_new
    R_n = R_n + R_new - S_new #evt N - S_next - I_next
    
    realizations[i, ] = c(S_n, I_n, R_n)
    
    #check sum = 1000
    if (S_n + I_n + R_n + v != N)
    {
      stop("N NOT CONSERVED!") #debug
    }
    
  }
  
  realizations
}


Y0 = c(950, 50, 0)
x = c(1:300)
N = 1000
for (v in c(0, 100, 600, 800))
{
  Y_vals = simulate_n_times(300, Y0, N, v)
  #par(mfrow = c(3,1))
  plot(x, Y_vals[ ,1], main = c("Temporal evolution of Sn with v = ", v))
  plot(x, Y_vals[ ,2], main = c("Temporal evolution of In with v = ", v))
  plot(x, Y_vals[ ,3], main = c("Temporal evolution of Rn with v = ", v))
  
  #1f)
  #run simulation 1000 times
  num = 1000
  In_maxvals = array(dim = num)
  times_to_first_max = array(dim = num)
  for (i in 1:num)
  {
    Y0 = c(950, 50, 0)
    Y_arr = simulate_n_times(300, Y0, N, v)
    In_arr = Y_arr[, 2]
    In_maxvals[i] = max(In_arr)
    times_to_first_max[i] = which.max(In_arr)
  }
  
  expected_max_In = mean(In_maxvals)
  expected_time_to_first_max = mean(times_to_first_max)
  cat("Expected value of maximum number of infected individuals with v =", v, " :", expected_max_In, "\n")
  cat("Expected value of time until first maximum value of the number of infected individuals with v =", v, " :", expected_time_to_first_max, "\n")
}
