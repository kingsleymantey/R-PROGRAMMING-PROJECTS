###############################################################################
#                                                                             #
#     W4479 - Applied Econometrics project with R, summer semester 2025       #
#                                                                             #
#                     Problem 4: Multiple Linear Regression                   #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 4 of the applied 
# project.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# <------- Start here with your own code --------> # 

# Load your dataset
economics <- read.csv("DATA SETS/economics_multi.csv")

summary_stats <- data.frame(
  Variable = c("psavert", "pop", "pce", "unemploy"),
  Observations = c(length(economics$psavert), length(economics$pop), length(economics$pce), length(economics$unemploy)),
  Minimum = c(min(economics$psavert), min(economics$pop), min(economics$pce), min(economics$unemploy)),
  Maximum = c(max(economics$psavert), max(economics$pop), max(economics$pce), max(economics$unemploy)),
  Mean = c(mean(economics$psavert), mean(economics$pop), mean(economics$pce), mean(economics$unemploy)),
  Variance = c(var(economics$psavert), var(economics$pop), var(economics$pce), var(economics$unemploy))
)

# solution c

model1 = lm(psavert ~ pce, data = economics)
model2 = lm(psavert ~ pop, data = economics)
model3 = lm(psavert ~ unemploy, data = economics)
model4 = lm(psavert ~ pce + pop, data = economics)
model5 = lm(psavert ~ pce + unemploy, data = economics)
model6 = lm(psavert ~ pop + unemploy, data = economics)
model7 = lm(psavert ~ pce + pop + unemploy, data = economics)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)

# solution e
summary(model1)$r.squared
summary(model1)$adj.r.squared

summary(model2)$r.squared
summary(model2)$adj.r.squared

summary(model3)$r.squared
summary(model3)$adj.r.squared

summary(model4)$r.squared
summary(model4)$adj.r.squared

summary(model5)$r.squared
summary(model5)$adj.r.squared

summary(model6)$r.squared
summary(model6)$adj.r.squared

summary(model7)$r.squared
summary(model7)$adj.r.squared

# solution f
# 95% Confidence Intervals
confint(model7, level = 0.95)

# 99% Confidence Intervals
confint(model7, level = 0.99)

# solution g
# Create a new data frame with mean values of the predictors
new_data <- data.frame(
  pce = mean(economics$pce, na.rm = TRUE),
  pop = mean(economics$pop, na.rm = TRUE),
  unemploy = mean(economics$unemploy, na.rm = TRUE)
)

# Predict psavert with 99% prediction interval
prediction <- predict(model7, newdata = new_data, interval = "prediction", level = 0.99)

# View the result
print(prediction)

