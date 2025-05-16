# our seed number for the whole project
set.seed(4444)
#parameters
S0 <- 100 #initial price
K <- 110 #strike price
T <- 183/365  # Time to maturity in years
r <- 0.03 # risk free rate/ compound rate 
sigma <- 0.2 #also volatility
mu <- 0.10 #expected retun 
n_sim <- 10000 

dt <- 1/365       # Daily steps
n_steps <- round(T / dt) # ?chat says using roundd improves calculations???
# amount of steps being taking in the stock price path simulation

# simulating stock prices
Z <- rnorm(n_sim)
ST <- S0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)

#exercise a
# probability of the option ending in the money
in_the_money <- ST>K # for every case where the stock price is higher than K
average_ITM <- mean(in_the_money)
cat('Average amount of options ending in the money:',round(average_ITM,2),"\n")
#exercise b

#Simulate stock paths 

epsilon <- matrix(rnorm(n_sim * n_steps), nrow = n_sim) # standard normally distibuted n_sim times
paths <- matrix(NA, nrow = n_sim, ncol = n_steps + 1) # initialize a matrix to store simulated price paths
paths[, 1] <- S0

for (t in 1:n_steps) {
  paths[, t+1] <- paths[, t] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * epsilon[, t])
} # Loop to update every day the stock price

#Trader A quantile function
profit_quantile <- function(P0) {
  #strategy of a
  payoff <- pmax(ST - K, 0) * 100
  value_at_maturity <- P0 * exp(r * T)
  profit <- value_at_maturity - payoff
  return(quantile(profit, 0.01))  # We want this to be 0
}

P0_result1 <- uniroot(function(P0) profit_quantile(P0), lower = 0, upper = 50000) # uniroot is a funciton that looks for a price to make it zero
P01 <- P0_result1$root
cat("Trader A's price for 99% no-loss: ", round(P02, 2), "\n")

#Trader B Stategy, 

# Function to compute 1% profit quantile for Trader B
profit_quantile_B <- function(P0) {
  N <- nrow(paths)
  cash <- rep(P0, N)
  stock <- rep(0, N) # Defining variables N times
  
  for (t in 1:(ncol(paths) - 1)) { # for every day update the the stocks that are held by the Trader B
    ST_t <- paths[, t]
    above_K <- ST_t > K # true of false
    
    stock_target <- as.numeric(above_K) * 100 # make the the true 1 or false 0
    stock_change <- stock_target - stock 
    cost <- stock_change * ST_t # stock price times the variable that keeps track if we buy or not
    cash <- cash - cost # update cash position
    stock <- stock_target  # update stock
    
    cash <- cash * exp(r * (1 / 365)) # invest cash into the risk free bonds and update cash.
  }
  
  ST <- paths[, ncol(paths)]
  payoff <- pmax(ST - K, 0) * 100
  portfolio_value <- cash + stock * ST 
  profit <- portfolio_value - payoff # calculate the end profit
  
  return(quantile(profit, 0.01))
}

P0_result2 <- uniroot(profit_quantile_B, lower = 0, upper = 5000, tol = 0.01)
P02 <- P0_result2$root
cat("Trader B's price for 99% no-loss:", round(P02, 2), "\n")

#trader c 

## --- Function to compute 1% quantile of profit for Trader C ---
profit_quantile_trader_C <- function(P0) {
  profits <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    cash <- P0
    stock <- 0
    
    for (t in 1:n_steps) {
      St <- paths[i, t]
      time <- (t - 1) * dt
      tau <- T - time # time left
      if (tau <= 0) {
        delta <- 0 # expired 
      } else {
        d1 <- (log(St / K) + (r + 0.5 * sigma^2) * tau) / (sigma * sqrt(tau))
        delta <- pnorm(d1) # formula for calulating change in our stock position
      }
      
      desired_stock <- 100 * delta
      delta_stock <- desired_stock - stock
      
      cash <- cash - delta_stock * St
      stock <- desired_stock
      
      cash <- cash * exp(r * dt)
    }
    
    ST <- paths[i, ncol(paths)]
    payoff <- pmax(ST - K, 0) * 100
    final_value <- cash + stock * ST - payoff
    profits[i] <- final_value
  }
  
  
  return(quantile(profits, 0.01)) # this needs to be zero
}

P0_result3 <- uniroot(function(P0) profit_quantile_trader_C(P0),
                     lower = 0, 
                     upper = 50000, 
                     tol = 0.01)
cat("Trader C should charge:.", P0_result3$root,"\n")




#exercise c


# --- Trader A: Alice ---
P0_alice <- 3351.32  # use actual P0 from uniroot for part (b)
ST <- paths[, ncol(paths)]
payoff_A <- pmax(ST - K, 0) * 100
value_final_A <- P0_alice * exp(r * T)
profits_A <- value_final_A - payoff_A
hist(profits_A, breaks = 10, col = "skyblue", main = paste("Profit Distribution -", 'Trader A'),
     xlab = "Profit (???)", xlim = c(min(profits_A), max(profits_A)))



# --- Trader B: Bradley ---
P0_bradley <- 1615.12  # use actual P0 from uniroot for part (b)
profits_B <- numeric(n_sim)

for (i in 1:n_sim) {
  cash <- P0_bradley
  stock <- 0
  
  for (t in 1:(ncol(paths) - 1)) {
    St <- paths[i, t]
    
    above_K <- St > K
    stock_target <- as.numeric(above_K) * 100
    stock_change <- stock_target - stock
    cost <- stock_change * St
    
    cash <- cash - cost
    stock <- stock_target
    cash <- cash * exp(r * (1 / 365))
  }
  
  ST <- paths[i, ncol(paths)]
  payoff <- pmax(ST - K, 0) * 100
  portfolio_value <- cash + stock * ST
  profits_B[i] <- portfolio_value - payoff
}

hist(profits_B, breaks = 10, col = "skyblue", main = paste("Profit Distribution -", 'Trader B'),
     xlab = "Profit (???)", xlim = c(min(profits_B), max(profits_B)))

# --- Trader C: Claire ---
P0_claire <- 365.30  # use actual P0 from uniroot for part (b)
profits_C <- numeric(n_sim)

for (i in 1:n_sim) {
  cash <- P0_claire 
  stock <- 0 
  for (t in 1:n_steps) {
    St <- paths[i, t] 
    time <- (t - 1) * dt
    tau <- T - time  
    if (tau <= 0) {
      delta <- 0
    } else {
      d1 <- (log(St / K) + (r + 0.5 * sigma^2) * tau) / (sigma * sqrt(tau))
      delta <- pnorm(d1)
    }
    desired_stock <- 100 * delta
    delta_stock <- desired_stock - stock
    cash <- cash - delta_stock * St
    stock <- desired_stock # update stock according to delta
    cash <- cash * exp(r * dt)
  }
  ST <- paths[i, ncol(paths)]
  payoff <- pmax(ST - K, 0) * 100
  profits_C[i] <- cash + stock * ST - payoff # here we calculating the final profit instead of quantile.
}

hist(profits_C, breaks = 100, col = "skyblue", main = paste("Profit Distribution -", 'Trader C'),
     xlab = "Profit (???)", xlim = c(min(profits_C), max(profits_C)))

#exercise d

#--- Trader A: Alice ---

a <- 0.001 # variable for risk aversion a ??? 0 for indifference towards risk
indifference_eq_A <- function(P0_star) {
  BT <- exp(r * T) * P0_star # risk free rate investment times the indifference price
  utility <- (1 - exp(-a * (BT - payoff))) / a  
  mean(utility)           
}

# solve for indifference price
root_A <- uniroot(indifference_eq_A, lower = 0, upper = 50000, tol = 0.01)
P0_star_A <- root_A$root

cat("Trader A's indifference price P0*:", round(P0_star_A, 2), "\n")

#--- Trader B ---

# function to simulate Trader B's final profit given an initial price
simulate_traderB_profit <- function(P0) {
  N <- nrow(paths)
  cash <- rep(P0, N)
  stock <- rep(0, N)
  
  for (t in 1:(ncol(paths) - 1)) {
    ST_t <- paths[, t]
    above_K <- ST_t > K
    stock_target <- as.numeric(above_K) * 100
    stock_change <- stock_target - stock
    cost <- stock_change * ST_t
    cash <- cash - cost
    stock <- stock_target
    cash <- cash * exp(r * (1 / 365))
  }
  
  ST <- paths[, ncol(paths)]
  payoff <- pmax(ST - K, 0) * 100
  portfolio_value <- cash + stock * ST
  profit <- portfolio_value - payoff
  return(profit)
}

# Function to compute the expected utility at a given P0
indifference_eq_B <- function(P0_star) {
  profits <- simulate_traderB_profit(P0_star)
  BT <- exp(r * T) * P0_star
  utility <- (1 - exp(-a * (BT + profits))) / a
  mean(utility)
}

# Solve for indifference price P0*
root_B <- uniroot(indifference_eq_B, lower = 0, upper = 50000, tol = 0.01)
P0_star_B <- root_B$root

# Output
cat("Trader B's indifference price P0*:", round(P0_star_B, 2), "\n")


#--- Trader C ---

bs_delta <- function(S, t) {
  tau <- T - t
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tau) / (sigma * sqrt(tau))
  return(pnorm(d1))
} #define the Black-Scholes equation for given Stock price and Time

# Simulate Trader C profit for given P0
simulate_traderC_profit <- function(P0) {
  N <- nrow(paths)
  cash <- rep(P0, N)
  stock <- rep(0, N)
  
  for (t in 1:(n_steps - 1)) {
    S_t <- paths[, t] #current stock price
    t_years <- time_grid[t]
    delta <- bs_delta(S_t, t_years) # use the Black Scholes equation but also use t-years as parameter
    delta[is.nan(delta)] <- 0  # handle NaNs at if T-t = 0
    stock_target <- delta * 100 # how much exposure to stocks
    stock_change <- stock_target - stock
    cost <- stock_change * S_t
    cash <- cash - cost 
    stock <- stock_target
    cash <- cash * exp(r * dt)
  }
  
  ST <- paths[, n_steps]
  payoff <- pmax(ST - K, 0) * 100
  portfolio_value <- cash + stock * ST
  profit <- portfolio_value - payoff
  return(profit)
}

# Indifference equation for Trader C
indifference_eq_C <- function(P0_star) {
  profits <- simulate_traderC_profit(P0_star)
  BT <- exp(r * T) * P0_star
  utility <- (1 - exp(-a * (BT + profits))) / a
  mean(utility)
}

# Solve for indifference price P0*
root_C <- uniroot(indifference_eq_C, lower = 0, upper = 50000, tol = 0.01)
P0_star_C <- root_C$root

# Output
cat("Trader C's indifference price P0*:", round(P0_star_C, 2), "\n")


#exercise e 

barrier <- 90

min_prices <- apply(paths, 1, min)
knocked_out <- min_prices < barrier

# Calculate conditional probability (Bayes-Theorem)
num_ITM_and_knocked_out <- sum(in_the_money & knocked_out)
num_ITM <- sum(in_the_money)

prob <- num_ITM_and_knocked_out / num_ITM
cat("Change of down-and-out call option ends worhtless if the regular call option has ended in the money:", round(prob, 2), "\n")


