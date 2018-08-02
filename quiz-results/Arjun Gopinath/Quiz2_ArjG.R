# Arjun Gopinath - Aug 2 2018
# R Tutorial Quiz 2

library(data.table)
library(ggplot2)
library(AER)
library(lfe)

# Ensure reproducibility of random numbers generated.
set.seed(101)

num_ind <- 1000 # Number of individuals
num_time <- 5 # Number of time periods

# Provide parameters in a list
param <- list(
  "alp" = 8, "bet" = 1, "kap" = 1 # Providing parameter values for alpha, beta and kappa.
)

# Creating the data table
ddata_panel <- data.table(expand.grid(id = 1:num_ind, time = 1:num_time))

# Adding fixed effect error term for each individual
ddata_panel[, mu := rnorm(n = 1), by = "id"]

# Generating shock for each id, time
ddata_panel[, eps := rnorm(n = nrow(ddata_panel))]

# Generating Z independent of eps
ddata_panel[, Z := time * 0.75 + rnorm(n = nrow(ddata_panel))]

# Generating X based on mu, eps, Z and time for each individual in each time period
ddata_panel[, X := -mu + 0.25 * eps - time * 0.5 + 2.5 * Z + rnorm(n = nrow(ddata_panel))]

# Computing Y based on the DGP
ddata_panel[, Y := param$alp + param$bet * X + time * param$kap + mu + eps]

# Obtaining observed variables and store in a new dataset
observed_data <- ddata_panel [, .(id, time, Y, X, Z)]

# Running an OLS regression, ignoring Z

OLS_formula <- as.formula("Y ~ X + time + as.factor(id)")
OLS_result <- lm(formula = OLS_formula, data = observed_data) # regression
OLS_coef <- coef(summary(OLS_result))


# Running an IV regression
IV_formula <- as.formula("Y ~ X + time + as.factor(id) | time + as.factor(id) + Z")
IV_result <- ivreg(formula = IV_formula, data = observed_data)
IV_coef <- coef(summary(IV_result))

# Running a FE-LM regression accounting for Z
FELM_formula <- as.formula("Y ~ time | id | (X ~ Z)")
FELM_result <- felm(FELM_formula, data = observed_data)
FELM_coef <- coef(summary(FELM_result))

# Copying the results into a .CSV file
OLS_beta <- OLS_coef[2, ]
IV_beta <- IV_coef[2, ]
FELM_beta <- FELM_coef[2, ]

OLS_kappa <- OLS_coef[3, ]
IV_kappa <- IV_coef[3, ]
FELM_kappa <- FELM_coef[1, ]

results_beta <- rbind(OLS_beta, IV_beta, FELM_beta)
results_kappa <- rbind(OLS_kappa, IV_kappa, FELM_kappa)


write.csv(rbind(results_beta, results_kappa), 
          file = "IVresults.csv", row.names = T, na = " ")
