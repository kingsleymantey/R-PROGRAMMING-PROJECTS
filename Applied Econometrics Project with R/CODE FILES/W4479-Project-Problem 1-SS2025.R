###############################################################################
#                                                                             #
#   W4479 - Applied Econometrics project with R, summer semester 2025         #
#                                                                             #
#                        Problem 1: Monte Carlo Study                         #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 1 of the applied 
# project.
# Remove '1234567' in line 20 and instead, replace it with the matriculation 
# number of one of your group members. Run lines 20 to 87 first to make 
# your group's simulated data available in R before continuing with your own 
# code.

# Start at line 91 with your own code.

MatrNr <- 4051498
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

simP1 <- function(seed = MatrNr, b1 = 2.75, b2 = 0.85, M = 1000) {
  n <- c(20, 80, 320) # sample size
  N <- 10000          # population size
  # M - Monte-Carlo simulations (i.e. the random sampling)
  
  rchisq_dm <- function(n, df, mean = 0, sd = 1) {
    (rchisq(n = n, df = df) - df) / sqrt(2 * df) * sd + mean
  }
  
  set.seed(seed)
  Xmin <- 10
  Xmax <- 150
  X_pop = runif(N, Xmin, Xmax)
  u_norm = rnorm(N, sd = 5)
  ui_rchisq = rchisq_dm(N, df = 5, sd = 5)
  Y_norm = b1 + b2 * X_pop + u_norm
  Y_rchisq = b1 + b2 * X_pop + ui_rchisq
  est_all = list()
  for (i in 1:3) {
    est = matrix(0, M, 4)
    for (j in 1:M) {
      ind_spl = sort(sample(1:N, n[i], replace = FALSE))
      est[j, 1] = lm(Y_norm[ind_spl] ~ X_pop[ind_spl])$coefficients[1]
      est[j, 2] = lm(Y_norm[ind_spl] ~ X_pop[ind_spl])$coefficients[2]
      est[j, 3] = lm(Y_rchisq[ind_spl] ~ X_pop[ind_spl])$coefficients[1]
      est[j, 4] = lm(Y_rchisq[ind_spl] ~ X_pop[ind_spl])$coefficients[2]
    }
    colnames(est) = c("Intercept", "Slope", "Intercept", "Slope")
    est_all[[i]] = est
  } 
  names(est_all) = c("n=20", "n=80", "n=320")
  return(est_all)
} 
est_all = simP1(MatrNr)

M = 1000
est20n <- data.frame(cbind("Sample No." = rep(1,M), 
                           "Sample Size" = rep(20, M), "Error Distribution" = "Normal",
                           est_all$`n=20`[,c(1,2)]))
est20chi <- data.frame(cbind("Sample No." = rep(1,M), 
                             "Sample Size" = rep(20, M), "Error Distribution" = "Chi-squared",
                             est_all$`n=20`[,c(3,4)]))
est20 <- rbind(est20n, est20chi)


est80n <- data.frame(cbind("Sample No." = rep(2,M), 
                            "Sample Size" = rep(80, M), "Error Distribution" = "Normal",
                            est_all$`n=80`[,c(1,2)]))
est80chi <- data.frame(cbind("Sample No." = rep(2,M), 
                              "Sample Size" = rep(80, M), "Error Distribution" = "Chi-squared",
                              est_all$`n=80`[,c(3,4)]))
est80 <- rbind(est80n, est80chi)


est320n <- data.frame(cbind("Sample No." = rep(3,M), 
                            "Sample Size" = rep(320, M), "Error Distribution" = "Normal",
                            est_all$`n=320`[,c(1,2)]))
est320chi <- data.frame(cbind("Sample No." = rep(3,M), 
                              "Sample Size" = rep(320, M), "Error Distribution" = "Chi-squared",
                              est_all$`n=320`[,c(3,4)]))
est320 <- rbind(est320n, est320chi)

sim_df <- rbind(est20, est80, est320)
sim_df$Intercept <- as.numeric(sim_df$Intercept)
sim_df$Slope <- as.numeric(sim_df$Slope)
head(sim_df)

# <------- Start here with your own code --------> # 
# Solution a
results <- stats::aggregate(cbind(Intercept, Slope) ~ Sample.Size + Error.Distribution, 
                     data = sim_df, FUN = function(x) c(mean = mean(x), var = var(x)))

results


# Solution b
# Unique combinations of sample size and error distribution
library(dplyr)
combinations <- unique(sim_df[, c("Sample.Size", "Error.Distribution")])
rownames(combinations) <- NULL
combinations <- combinations %>% 
  arrange(Error.Distribution)

comb_chi <- combinations[combinations$Error.Distribution == "Chi-squared", ]

#  Q-Q plots for Intercept
par(mfcol = c(3, 2))  

for (i in 1:nrow(comb_chi)) {
  size <- comb_chi[i, "Sample.Size"]
  distribution <- comb_chi[i, "Error.Distribution"]
  
  # Subset data for current combination
  sub_data <- sim_df[sim_df$`Sample.Size` == size & sim_df$`Error.Distribution` == distribution, ]
  
  # Q-Q plot for Intercept
  qqnorm(sub_data$Intercept, main = paste("Q-Q Plot: Intercept (n=", size, ", ", distribution, ")", sep = ""))
  qqline(sub_data$Intercept, col = "red")

}

for (i in 1:nrow(comb_chi)) {
  size <- comb_chi[i, "Sample.Size"]
  distribution <- comb_chi[i, "Error.Distribution"]
  
  # Subset data for current combination
  sub_data <- sim_df[sim_df$`Sample.Size` == size & sim_df$`Error.Distribution` == distribution, ]
  
  # Q-Q plot for Slope
  qqnorm(sub_data$Slope, main = paste("Q-Q Plot: Slope (n=", size, ", ", distribution, ")", sep = ""))
  qqline(sub_data$Slope, col = "red")

}

comb_normal <- combinations[combinations$Error.Distribution == "Normal", ]

#  Q-Q plots for Intercept
par(mfcol = c(3, 2))  

for (i in 1:nrow(comb_normal)) {
  size <- comb_normal[i, "Sample.Size"]
  distribution <- comb_normal[i, "Error.Distribution"]
  
  # Subset data for current combination
  sub_data <- sim_df[sim_df$`Sample.Size` == size & sim_df$`Error.Distribution` == distribution, ]
  
  # Q-Q plot for Intercept
  qqnorm(sub_data$Intercept, main = paste("Q-Q Plot: Intercept (n=", size, ", ", distribution, ")", sep = ""))
  qqline(sub_data$Intercept, col = "red")

}

for (i in 1:nrow(comb_normal)) {
  size <- comb_normal[i, "Sample.Size"]
  distribution <- comb_normal[i, "Error.Distribution"]
  
  # Subset data for current combination
  sub_data <- sim_df[sim_df$`Sample.Size` == size & sim_df$`Error.Distribution` == distribution, ]
  
  # Q-Q plot for Slope
  qqnorm(sub_data$Slope, main = paste("Q-Q Plot: Slope (n=", size, ", ", distribution, ")", sep = ""))
  qqline(sub_data$Slope, col = "red")

}


# Solution c
library(tseries)

# Initialize a data frame to store results
results <- data.frame(
  "Sample.Size" = integer(),
  "Error.Distribution" = character(),
  Estimate_Type = character(),
  Test_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Perform Jarque-Bera test for each combination and estimate type
for (i in 1:nrow(combinations)) {
  size <- combinations[i, "Sample.Size"]
  distribution <- combinations[i, "Error.Distribution"]
  
  # Subset data for current combination
  sub_data <- sim_df[sim_df$`Sample.Size` == size & sim_df$`Error.Distribution` == distribution, ]
  
  # Test for Intercept
  jb_intercept <- jarque.bera.test(sub_data$Intercept)
  results <- rbind(results, data.frame(
    "Sample.Size" = size,
    "Error.Distribution" = distribution,
    Estimate_Type = "Intercept",
    Test_Statistic = jb_intercept$statistic,
    P_Value = round(jb_intercept$p.value, 4)
  ))
  
  # Test for Slope
  jb_slope <- jarque.bera.test(sub_data$Slope)
  results <- rbind(results, data.frame(
    "Sample.Size" = size,
    "Error.Distribution" = distribution,
    Estimate_Type = "Slope",
    Test_Statistic = jb_slope$statistic,
    P_Value = round(jb_slope$p.value, 4)
  ))
  
}

rownames(results) <- NULL
results
