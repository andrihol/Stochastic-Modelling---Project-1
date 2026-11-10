rm(list = ls())

simulate_n_times = function(n, Y_curr, N) #Y_curr = (Sn, In, Rn), N size of population
{
  S_n = Y_curr[1]
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
    if (S_n + I_n + R_n != N)
    {
      stop("N NOT CONSERVED!") #debug
    }
    
  }
  
  realizations
}

Y0 = c(950, 50, 0)
Y_vals = simulate_n_times(300, Y0, 1000)
#par(mfrow = c(3,1))
x = c(1:300)
plot(x, Y_vals[ ,1], main = "Temporal evolution of Sn")
plot(x, Y_vals[ ,2], main = "Temporal evolution of In")
plot(x, Y_vals[ ,3], main = "Temporal evolution of Rn")


#1f)
#run simulation 1000 times
num = 1000
N = 1000 #population size
In_maxvals = array(dim = num)
times_to_first_max = array(dim = num)
for (i in 1:num)
{
  Y0 = c(950, 50, 0)
  Y_arr = simulate_n_times(300, Y0, N)
  In_arr = Y_arr[, 2]
  In_maxvals[i] = max(In_arr)
  times_to_first_max[i] = which.max(In_arr)
}

expected_max_In = mean(In_maxvals)
expected_time_to_first_max = mean(times_to_first_max)
cat("Expected value of maximum number of infected individuals:", expected_max_In)
cat("Expected value of time until first maximum value of the number of infected individuals:", expected_time_to_first_max)


#approximate standard deviation for both In_max and time until first max
S_I_max = 0
S_time = 0
for (i in 1:num)
{
  S_I_max = S_I_max + (In_maxvals[i] - expected_max_In)^2
  S_time = S_time + (times_to_first_max[i] - expected_time_to_first_max)^2
}
S_I_max = S_I_max/(num-1)
S_time = S_time/(num-1)

S_I_max = sqrt(S_I_max)
S_time = sqrt(S_time)

upper_I_max = expected_max_In + 1.960*S_I_max/sqrt(num)
upper_time = expected_time_to_first_max + 1.960*S_time/sqrt(num)

lower_I_max = expected_max_In - 1.960*S_I_max/sqrt(num)
lower_time = expected_time_to_first_max - 1.960*S_time/sqrt(num)

cat("CI for expected value of maximum number of infected individuals:\n", "[", lower_I_max, ", ", upper_I_max, "]")
cat("CI for expected value of time until first maximum of I_n:\n", "[", lower_time, ", ", upper_time, "]")
