#install.packages("psych")

library(psych)
# Load the dataset
data <- read.csv("Ultimate MS Regression.csv")

# Run factor analysis to extract underlying factor
fa <- psych::fa(data[, 1:5], nfactors = 1, rotate = "oblimin")
fa
fa.diagram(fa)

# Extract factor scores for each individual
factor_scores <- fa$scores

factor_scores

# Run multiple linear regression to estimate weights of each question
reg_model <- lm(factor_scores ~ Q1 + Q2 + Q3 + Q4 + Q5, data = data)

#chronbach's alpha
alpha_result_fa <- alpha(data[, c("Q1", "Q2", "Q3","Q4","Q5")])
print(alpha_result_fa)

# View the regression coefficients (weights)
coefficients <- summary(reg_model)$coefficients
coefficients
