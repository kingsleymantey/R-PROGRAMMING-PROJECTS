###############################################################################
#                                                                             #
#     W4479 - Applied Econometrics project with R, summer semester 2025       #
#                                                                             #
#                Problem 3: Modelling Nonlinear Relationships                 #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 3 of the applied 
# project.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# <------- Start here with your own code --------> # 

# solution a 
# import dataset
economics <- read.csv("DATA SETS/economics.csv") 


summary_stats <- data.frame(
  Variable = c("psavert", "pop"),
  Observations = c(length(economics$psavert), length(economics$pop)),
  Minimum = c(min(economics$psavert), min(economics$pop)),
  Maximum = c(max(economics$psavert), max(economics$pop)),
  Mean = c(mean(economics$psavert), mean(economics$pop)),
  Variance = c(var(economics$psavert), var(economics$pop))
)

summary_stats

# solution b
library(dplyr)
library(ggplot2)
economics %>% 
  ggplot(aes(x = pop, y = psavert/100)) +
  geom_point(col = "#122451FF", alpha = .8) +
  geom_smooth(method = "lm", se = F, color = "indianred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Population",
       y = "Savings to DPI",
       title = "Savings ~ Population") +
  theme_bw()

# solution c
economics_df <- economics %>%
  mutate(
    log_pop = log(pop),
    inv_pop = 1 / pop,
    log_psavert = log(psavert)) 

p1 <- ggplot(economics_df, aes(x = log_pop, y = psavert)) + geom_point(col = "#122451FF") + 
  labs(x = "log.Population",
       y = "Savings to DPI",
       title = "log(Population), Savings") +
  theme_bw()

p2 <- ggplot(economics_df, aes(x = inv_pop, y = psavert)) + geom_point(col = "#122451FF") + 
  labs(x = "1/Population",
       y = "Savings",
       title = "1/Population, Savings") +
  theme_bw()

p3 <- ggplot(economics_df, aes(x = pop, y = log_psavert)) + geom_point(col = "#122451FF") +
  ggtitle("Savings vs log(Population)") +
  labs(x = "Population",
       y = "log.Savings",
       title = "Population, log(Savings)") +
  theme_bw()

p4 <- ggplot(economics_df, aes(x = log_pop, y = log_psavert)) + geom_point(col = "#122451FF") + 
  ggtitle("log(Savings) vs log(Population)") +
  labs(x = "log.Population",
       y = "log.Savings to DPI",
       title = "log(Population), log(Savings)") +
  theme_bw()

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

# solution d
# Fit the five models
model1 <- lm(psavert ~ pop, data = economics_df)
model2 <- lm(psavert ~ log_pop, data = economics_df)
model3 <- lm(psavert ~ inv_pop, data = economics_df)
model4 <- lm(log_psavert ~ pop, data = economics_df)
model5 <- lm(log_psavert ~ log_pop, data = economics_df)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# solution f
# Compute residual sum of squares (RSS)
rss1 <- sum(residuals(model1)^2)
rss2 <- sum(residuals(model2)^2)
rss3 <- sum(residuals(model3)^2)
rss4 <- sum(residuals(model4)^2)
rss5 <- sum(residuals(model5)^2)

# Summarize in a table
rss_table <- data.frame(
  Model = c(
    "Savings ~ pop", 
    "Savings ~ log(pop)", 
    "Savings ~ 1/pop", 
    "log(Savings) ~ pop", 
    "log(Savings) ~ log(pop)"
  ),
  RSS = c(rss1, rss2, rss3, rss4, rss5)
)

# Sort by lowest RSS
rss_table <- rss_table %>% arrange(RSS)


# solution g
p0 <- ggplot(economics_df, aes(x = pop, y = psavert)) + 
  geom_point(col = "#122451FF", size = .5) + 
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +
  labs(x = "Population",
       y = "Savings",
       title = "Population, Savings") +
  theme_bw()

p1 <- ggplot(economics_df, aes(x = log_pop, y = psavert)) + 
  geom_point(col = "#122451FF", size = .5) + 
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +
  labs(x = "log.Population",
       y = "Savings",
       title = "log(Population), Savings") +
  theme_bw()

p2 <- ggplot(economics_df, aes(x = inv_pop, y = psavert)) + 
  geom_point(col = "#122451FF", size = .5) + 
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +
  labs(x = "1/Population",
       y = "Savings",
       title = "1/Population, Savings") +
  theme_bw()

p3 <- ggplot(economics_df, aes(x = pop, y = log_psavert)) + 
  geom_point(col = "#122451FF", size = .5) + 
  ggtitle("Savings vs log(Population)") +
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +
  labs(x = "Population",
       y = "log.Savings",
       title = "Population, log(Savings)") +
  theme_bw()

p4 <- ggplot(economics_df, aes(x = log_pop, y = log_psavert)) + 
  geom_point(col = "#122451FF", size = .5) + 
  ggtitle("log(Savings) vs log(Population)") +
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +
  labs(x = "log.Population",
       y = "log.Savings",
       title = "log(Population), log(Savings)") +
  theme_bw()

gridExtra::grid.arrange(p0, p1, p2, p3, p4, ncol = 2)

# solution h
# Get prediction intervals from the model
pred_df <- predict(model5, interval = "prediction", level = 0.95)
plot_data <- cbind(economics_df, pred_df)

# plot log transformed data with prediction interval
ggplot(plot_data, aes(x = log_pop, y = log_psavert)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "pink", alpha = 0.2,
              col = "gray90") +
  geom_point(color = "darkblue", size = 1.5) +
  geom_line(aes(y = fit), color = "red", linewidth = 1) +
  labs(title = "95% Prediction Interval for log(Savings) ~ log(Population)",
       x = "log(Population)",
       y = "log(Savings)") +
  theme_bw()


