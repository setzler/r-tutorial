
# Name: John Bonney
# Date: 7.3.18
# Quiz 2 - Data Analysis and Visualization

#setwd("C:/Users/johnf/Desktop/r-tutorial/quiz-results")

library(data.table)
library(AER)
library(lfe)

set.seed(101)
NI <- 1000
NT <- 5
aalpha <- 8
bbeta <- 1
kkappa <- 1

ddata <- data.table(expand.grid(id = 1:NI, time = 1:NT))
ddata[, mu := rnorm(n = 1), by = "id"]
ddata[, Z:= rnorm(n = nrow(ddata), mean = 2, sd = .75) ] # draw Z independent of epsilon
ddata[, epsilon := rnorm(n=nrow(ddata), mean = 2, sd = 0.5)]
ddata[, X:= Z*2 - mu - time * .5 + epsilon * 3] # let X depend on epsilon and Z
ddata[, Y := aalpha + bbeta * X + time * kkappa + mu + epsilon]
ddata[c(1:3)]

# Note: this may not be the most efficient (or appropriate) way to generate
# Z and epsilon such that both are correlated with X, but I was unsure of
# a better way.

# check correlation coefficients of X and Z, X and epsilon
cor(ddata[,list(X, Z, epsilon)])

obsdata <- ddata[, .(id, time, Y, X, Z)] # data actually observed

# OLS model with time trend and fixed effects
OLS_formula <- as.formula("Y ~ X + time + as.factor(id)")
OLS_result <- lm(formula = OLS_formula, data = obsdata)
OLS_coef <- coef(summary(OLS_result))
OLS_coef[1:3,]

# IV model using ivreg
IV_formula <- as.formula("Y ~ X + time + as.factor(id) | Z + time + as.factor(id)")
IV_result <- ivreg(IV_formula, data = obsdata)
IV_coef <- coef(summary(IV_result))
IV_coef[1:3,]

# IV model using felm
felm_formula <- as.formula("Y ~ time | id | (X ~ Z)")
felm_result <- felm(formula = felm_formula, data = obsdata)
felm_coef <- coef(summary(felm_result))
felm_coef

# Finally, export the matrix of regression coefficients, standard errors, and
# p-values from ivreg and felm as CSV file. [Note that ivreg and felm
# organize their results somewhat differently.]

ivreg_results <- IV_coef[2,]
felm_results <- felm_coef[2,]
IV_methods <- rbind(ivreg_results, felm_results)
IV_methods # show table
write.csv(IV_methods, file = "IV_methods.csv", row.names = T, na = " ")
