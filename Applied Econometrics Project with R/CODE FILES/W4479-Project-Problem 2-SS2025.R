###############################################################################
#                                                                             #
#       W4479 - Applied Econometrics project with R, summer semester 2025     #
#                                                                             #
#                     Problem 2: Simple Linear Regression                     #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 2 of the applied 
# project.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# <------- Start here with your own code --------> # 
# solution a
# library(wooldridge)
# wage_df <- wage1[, 1:2]

wage_df <- read.csv("DATA SETS/wage_df.csv")

# Summary statistics
summary_stats <- data.frame(
  Variable = c("wage", "educ"),
  Observations = c(length(wage_df$wage), length(wage_df$educ)),
  Minimum = c(min(wage_df$wage), min(wage_df$educ)),
  Maximum = c(max(wage_df$wage), max(wage_df$educ)),
  Mean = c(mean(wage_df$wage), mean(wage_df$educ)),
  Variance = c(var(wage_df$wage), var(wage_df$educ))
)

summary_stats

model<-lm(wage ~ educ, data=wage_df)

summary(model)

# solution b)
# Create the scatter plot with regression line
library(ggplot2)
ggplot(wage_df, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.7, col = "#14607A") +  # Plot data points
  geom_smooth(method = "lm", se = FALSE, color = "#871A5B", linewidth = .6) +  # Overlay regression line
  labs(title = "How does years of Education influence Wage", x = "Years of Education",
       y = "Average hourly earnings") +
  theme_minimal() +
  # scale_y_continuous(limits = c(0, 16000)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(),
        text = element_text(family = "Frank Ruhl Libre", face = "bold",
                            colour =  "gray40", size = 12),
        plot.background = element_rect(colour = "black"),
        panel.background = element_rect(colour = "black"))

# solution d
confint(model, level = 0.99)

# solution d
# compute the t score 
qt(0.95, df = 524)



