## Package

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

## CRCR case

set.seed(1)
n <- 30000

X  <- rbinom(n, 1, 0.5)
X0 <- 0
X1 <- 1

R  <- rbinom(n, 1, 0.5)
R0 <- 0
R1 <- 1

## PO of R_hat - related to R
R_hat0 <- rbinom(n, 1, 0.2)
R_hat1 <- rbinom(n, 1, 0.7)

R_hat <- ifelse(R == 0, R_hat0, R_hat1)

## PO of Y - affected by X
Y0 <- rnorm(n, 0, 1)
Y1 <- rnorm(n, 1, 1)

Y <- ifelse(X == 0, Y0, Y1)

## CR pattern
CR <- runif(n, -1, 1)

## Y_trueCR - Y variable under CR (CRCR case)
Y_obs <- ifelse(R == 1, Y, CR)


TE_true <- mean(Y1 - Y0)

# -----------------------
# Estimation helpers
# -----------------------
get_slope <- function(y, x) {
  unname(coef(lm(y ~ x))["x"])
}

# (A) "Ignore CR" estimator (uses contaminated Y_obs, all data)
est_all <- get_slope(Y_obs, X)

# (B) Oracle deletion using TRUE R (not available in practice)
est_R1  <- get_slope(Y_obs[R == 1], X[R == 1])

# (C) Practical deletion using R_hat (what you'd do with a detector)
est_Rhat1 <- get_slope(Y_obs[R_hat == 1], X[R_hat == 1])

# -----------------------
# Bias decomposition (2-part)
# -----------------------
total_bias_practical <- est_Rhat1 - TE_true

bias_oracle_deletion <- est_R1    - TE_true # effect of deleting (R known)
bias_misclass_R      <- est_Rhat1 - est_R1  # extra bias from using R_hat

# exact add-up check
bias_decomp_check <- total_bias_practical -
  (bias_contamination + bias_oracle_deletion + bias_misclass_R)

data.frame(
  TE_true = TE_true,
  est_all = est_all,
  est_R1_oracle = est_R1,
  est_Rhat1_practical = est_Rhat1,
  total_bias_practical = total_bias_practical,
  bias_oracle_deletion = bias_oracle_deletion,
  bias_misclass_R = bias_misclass_R,
  bias_decomp_check = bias_decomp_check
)

# ------------------------------------------------------------
# Monte Carlo wrapper to "verify" the decomposition in expectation
# (i.e., see how components behave across repeated samples)
# ------------------------------------------------------------
simulate_once <- function(n = 30000,
                          pR1 = 0.5,
                          pRhat1_R0 = 0.2,
                          pRhat1_R1 = 0.7) {
  
  X <- rbinom(n, 1, 0.5)
  R <- rbinom(n, 1, pR1)
  
  R_hat0 <- rbinom(n, 1, pRhat1_R0)
  R_hat1 <- rbinom(n, 1, pRhat1_R1)
  R_hat  <- ifelse(R == 0, R_hat0, R_hat1)
  
  Y0 <- rnorm(n, 0, 1)
  Y1 <- rnorm(n, 1, 1)
  Y  <- ifelse(X == 0, Y0, Y1)
  
  CR <- runif(n, -1, 1)
  Y_obs <- ifelse(R == 1, Y, CR)
  
  TE_true <- mean(Y1 - Y0)
  
  get_slope <- function(y, x) unname(coef(lm(y ~ x))["x"])
  
  est_all   <- get_slope(Y_obs, X)
  est_R1    <- get_slope(Y_obs[R == 1], X[R == 1])
  est_Rhat1 <- get_slope(Y_obs[R_hat == 1], X[R_hat == 1])
  
  total_bias_practical <- est_Rhat1 - TE_true
  bias_oracle_deletion <- est_R1    - TE_true
  bias_misclass_R      <- est_Rhat1 - est_R1
  
  bias_decomp_check <- total_bias_practical -
    (bias_oracle_deletion + bias_misclass_R)
  
  c(
    TE_true = TE_true,
    est_all = est_all,
    est_R1_oracle = est_R1,
    est_Rhat1_practical = est_Rhat1,
    total_bias_practical = total_bias_practical,
    bias_oracle_deletion = bias_oracle_deletion,
    bias_misclass_R = bias_misclass_R,
    bias_decomp_check = bias_decomp_check
  )
}

set.seed(1)
M <- 1000
out <- replicate(M, simulate_once(), simplify = "matrix")
out <- t(out)
summary_df <- as.data.frame(out)

colMeans(summary_df)
library(dplyr)
library(tidyr)

sum_table <- summary_df |>
  select(
    total_bias_practical,
    bias_oracle_deletion,
    bias_misclass_R,
    bias_decomp_check
  ) |>
  pivot_longer(everything(), names_to = "quantity", values_to = "value") |>
  group_by(quantity) |>
  summarise(
    mean = mean(value),
    sd   = sd(value),
    mc_se = sd / sqrt(n()),
    ci_lo = mean - 1.96 * mc_se,
    ci_hi = mean + 1.96 * mc_se,
    q025 = quantile(value, 0.025),
    q50  = quantile(value, 0.50),
    q975 = quantile(value, 0.975),
    .groups = "drop"
  )

sum_table
kable(
  sum_table |> mutate(across(where(is.numeric), ~ round(.x, 4))),
  caption = "Monte Carlo summary of bias decomposition components (Simulation 3, CRCR)."
)



## CRNAR case
set.seed(1)
n <- 30000

X  <- rbinom(n, 1, 0.5)
X0 <- 0
X1 <- 1

## PO of Y - affected by X
Y0 <- rnorm(n, 0, 1)
Y1 <- rnorm(n, 1, 1)

Y <- ifelse(X == 0, Y0, Y1)

R0 <- rbinom(n, 1, 0.1)
R1 <- rbinom(n, 1, 0.7)
R <- ifelse(Y > 0, R1, R0) ## R is affected by Y

## PO of R_hat - related to R
R_hat0 <- rbinom(n, 1, 0.2)
R_hat1 <- rbinom(n, 1, 0.7)

R_hat <- ifelse(R == 0, R_hat0, R_hat1)

## CR pattern
CR <- runif(n, -1, 1)

## Y_trueCR - Y variable under CR
Y_obs <- ifelse(R == 1, Y, CR)


TE_true <- mean(Y1 - Y0)

# -----------------------
# Estimation helpers
# -----------------------
get_slope <- function(y, x) {
  unname(coef(lm(y ~ x))["x"])
}

# (A) "Ignore CR" estimator (uses contaminated Y_obs, all data)
est_all <- get_slope(Y_obs, X)

# (B) Oracle deletion using TRUE R (not available in practice)
est_R1  <- get_slope(Y_obs[R == 1], X[R == 1])

# (C) Practical deletion using R_hat (what you'd do with a detector)
est_Rhat1 <- get_slope(Y_obs[R_hat == 1], X[R_hat == 1])

# -----------------------
# Bias decomposition (2-part)
# -----------------------
total_bias_practical <- est_Rhat1 - TE_true

bias_oracle_deletion <- est_R1    - TE_true # effect of deleting (R known)
bias_misclass_R      <- est_Rhat1 - est_R1  # extra bias from using R_hat

# exact add-up check
bias_decomp_check <- total_bias_practical -
  ( bias_oracle_deletion + bias_misclass_R)

data.frame(
  TE_true = TE_true,
  est_all = est_all,
  est_R1_oracle = est_R1,
  est_Rhat1_practical = est_Rhat1,
  total_bias_practical = total_bias_practical,
  bias_oracle_deletion = bias_oracle_deletion,
  bias_misclass_R = bias_misclass_R,
  bias_decomp_check = bias_decomp_check
)

# ------------------------------------------------------------
# Monte Carlo wrapper to "verify" the decomposition in expectation
# (i.e., see how components behave across repeated samples)
# ------------------------------------------------------------
simulate_once <- function(n = 30000,
                          pRhat1_R0 = 0.2,
                          pRhat1_R1 = 0.7) {
  
  X <- rbinom(n, 1, 0.5)
  
  Y0 <- rnorm(n, 0, 1)
  Y1 <- rnorm(n, 1, 1)
  Y  <- ifelse(X == 0, Y0, Y1)
  
  R0 <- rbinom(n, 1, 0.1)
  R1 <- rbinom(n, 1, 0.7)
  R <- ifelse(Y > 0, R1, R0)
  
  R_hat0 <- rbinom(n, 1, pRhat1_R0)
  R_hat1 <- rbinom(n, 1, pRhat1_R1)
  R_hat  <- ifelse(R == 0, R_hat0, R_hat1)
  
  
  CR <- runif(n, -1, 1)
  Y_obs <- ifelse(R == 1, Y, CR)
  
  TE_true <- mean(Y1 - Y0)
  
  get_slope <- function(y, x) unname(coef(lm(y ~ x))["x"])
  
  est_all   <- get_slope(Y_obs, X)
  est_R1    <- get_slope(Y_obs[R == 1], X[R == 1])
  est_Rhat1 <- get_slope(Y_obs[R_hat == 1], X[R_hat == 1])
  
  total_bias_practical <- est_Rhat1 - TE_true
  bias_oracle_deletion <- est_R1    - TE_true
  bias_misclass_R      <- est_Rhat1 - est_R1
  
  bias_decomp_check <- total_bias_practical -
    (bias_oracle_deletion + bias_misclass_R)
  
  c(
    TE_true = TE_true,
    est_all = est_all,
    est_R1_oracle = est_R1,
    est_Rhat1_practical = est_Rhat1,
    total_bias_practical = total_bias_practical,
    bias_oracle_deletion = bias_oracle_deletion,
    bias_misclass_R = bias_misclass_R,
    bias_decomp_check = bias_decomp_check
  )
}

set.seed(1)
M <- 1000
out <- replicate(M, simulate_once(), simplify = "matrix")
out <- t(out)
summary_df <- as.data.frame(out)

colMeans(summary_df)
library(dplyr)
library(tidyr)

sum_table <- summary_df |>
  select(
    total_bias_practical,
    bias_oracle_deletion,
    bias_misclass_R,
    bias_decomp_check
  ) |>
  pivot_longer(everything(), names_to = "quantity", values_to = "value") |>
  group_by(quantity) |>
  summarise(
    mean = mean(value),
    sd   = sd(value),
    mc_se = sd / sqrt(n()),
    ci_lo = mean - 1.96 * mc_se,
    ci_hi = mean + 1.96 * mc_se,
    q025 = quantile(value, 0.025),
    q50  = quantile(value, 0.50),
    q975 = quantile(value, 0.975),
    .groups = "drop"
  )

sum_table
kable(
  sum_table |> mutate(across(where(is.numeric), ~ round(.x, 4))),
  caption = "Monte Carlo summary of bias decomposition components (Simulation 3, CRNAR)."
)

