#install.packages("psych")
#install.packages("GPArotation")
library(GPArotation)
library(psych)
# Load the dataset
data <- read.csv("Ultimate Oppressive Measures.csv")

# Perform Bartlett's test
#bartlett.test(data[, c("Q10", "Q11", "Q12","Q13")], data$group)

# Calculate Cronbach's alpha for a set of variables
alpha_result <- alpha(data[, c("Q1", "Q2", "Q3", "Q4", "Q5")])
print(alpha_result)
alpha_result <- alpha(data[, c("Q6", "Q7", "Q8", "Q9")])
print(alpha_result)

alpha_result <- alpha(data[, c("Q10", "Q11")])
print(alpha_result)
alpha_result <- alpha(data[, c("Q12", "Q13")])
print(alpha_result)

fa <- psych::fa(data[, c("Q12", "Q13")], nfactors = 1, rotate = "oblimin")

# View the factor loadings or weights of each question
factor_loadings <- fa$loadings

factor_loadings

# Run factor analysis to extract underlying factor
#fa <- psych::fa(data[, 10:13], nfactors = 2, rotate = "varimax")

# View factor loadings
print(fa$loadings, cutoff = 0.3, sort = TRUE)

fa
fa.diagram(fa)

# Extract factor scores for each individual
factor_scores <- fa$scores

factor_scores
# Create new variables for the sums of scores for each factor
#data$factor1_sum <- rowSums(data[,c("Q1", "Q2", "Q3","Q4","Q5")])
#data$factor2_sum <- rowSums(data[,c("Q6", "Q7", "Q8", "Q9")])
#data$factor3_sum <- rowSums(data[,c("Q10", "Q11")])
data$factor4_sum <- rowSums(data[,c("Q12","Q13")])

# Run the regression analysis with factor scores as the dependent variable and sum of scores for each factor as the independent variables
#regression_model <- lm(factor_scores ~ factor3_sum, data=data)
regression_model <- lm(factor_scores ~ factor4_sum, data=data)
#regression_model <- lm(factor_scores ~ factor1_sum + factor2_sum, data=data)
summary(regression_model)

#MR1 <- factor_scores[,"MR1"]
#MR2 <- factor_scores[,"MR2"]
#MR3 <- factor_scores[,"MR1"]
MR4 <- factor_scores[,"MR1"]

#model_MR1 <- lm(MR1 ~ Q1 + Q2 + Q3 + Q4 + Q5, data = data)
#summary(model_MR1)

#model_MR2 <- lm(MR2 ~ Q6 + Q7 + Q8 + Q9, data = data)
#summary(model_MR2)

#model_MR3 <- lm(MR1 ~ Q10 + Q11, data = data)
#summary(model_MR3)

model_MR4 <- lm(MR4 ~ Q12 + Q13, data = data)
summary(model_MR4)

