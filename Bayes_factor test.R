install.packages("BayesFactor")
library(BayesFactor)

# Example dataset
data(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose) # Convert dose to a factor

# Perform Bayesian ANOVA
bf_result <- anovaBF(len ~ supp + dose, data = ToothGrowth)

# Display the Bayes factor results
print(bf_result)
