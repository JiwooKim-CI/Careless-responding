## Simulation 3: Bias from handling and imperfect detection
## Produces comprehensive Table S3 and the two decomposition tables.

library(dplyr)
library(tidyr)
library(knitr)

set.seed(1)

n <- 30000
B <- 1000

get_slope <- function(y, x) {
  unname(coef(lm(y ~ x))["x"])
}

simulate_once <- function(
    n = 30000,
    mechanism = c("CRCR", "CRNAR"),
    p_r1 = 0.5,
    p_r1_y_nonpositive = 0.1,
    p_r1_y_positive = 0.7,
    p_d1_r0 = 0.2,
    p_d1_r1 = 0.7) {
  
  mechanism <- match.arg(mechanism)
  
  # Treatment and uncontaminated target outcome.
  x <- rbinom(n, size = 1, prob = 0.5)
  y <- x + rnorm(n, mean = 0, sd = 1)
  
  # True response state: r = 1 denotes attentive responding.
  if (mechanism == "CRCR") {
    r <- rbinom(n, size = 1, prob = p_r1)
  } else {
    prob_r1 <- ifelse(y > 0, p_r1_y_positive, p_r1_y_nonpositive)
    r <- rbinom(n, size = 1, prob = prob_r1)
  }
  
  # Imperfect detector: d = 1 denotes classification as attentive.
  prob_d1 <- ifelse(r == 0, p_d1_r0, p_d1_r1)
  d <- rbinom(n, size = 1, prob = prob_d1)
  
  # Carelessly generated and observed outcomes.
  u <- runif(n, min = -1, max = 1)
  y_obs <- ifelse(r == 1, y, u)
  
  # Use the coefficient from the uncontaminated outcome in the same replication
  # so that the decomposition isolates handling and detection error.
  true_effect <- get_slope(y, x)
  est_all <- get_slope(y_obs, x)
  est_oracle <- get_slope(y_obs[r == 1], x[r == 1])
  est_practical <- get_slope(y_obs[d == 1], x[d == 1])
  
  total_bias <- est_practical - true_effect
  handling_bias <- est_oracle - true_effect
  misclassification_bias <- est_practical - est_oracle
  
  tibble(
    true_effect = true_effect,
    naive = est_all,
    oracle_deletion = est_oracle,
    practical_deletion = est_practical,
    total_bias = total_bias,
    handling_bias = handling_bias,
    misclassification_bias = misclassification_bias,
    decomposition_check = total_bias -
      (handling_bias + misclassification_bias),
    prop_attentive = mean(r),
    prop_classified_attentive = mean(d)
  )
}

run_condition <- function(mechanism, n, B) {
  bind_rows(lapply(seq_len(B), function(b) {
    simulate_once(n = n, mechanism = mechanism) |>
      mutate(replication = b)
  })) |>
    mutate(
      mechanism = mechanism,
      n = n
    )
}

# Keep both mechanisms instead of overwriting the first result.
simulation3_wide <- bind_rows(
  run_condition("CRCR", n = n, B = B),
  run_condition("CRNAR", n = n, B = B)
)

# -----------------------------------------------------------------------------
# Comprehensive estimator-level results: Supplemental Table S3
# -----------------------------------------------------------------------------

simulation3_long <- simulation3_wide |>
  pivot_longer(
    cols = c(naive, oracle_deletion, practical_deletion),
    names_to = "estimator",
    values_to = "estimate"
  ) |>
  mutate(
    estimator = recode(
      estimator,
      naive = "Naive (all responses)",
      oracle_deletion = "Oracle deletion",
      practical_deletion = "Practical deletion"
    ),
    estimation_error = estimate - true_effect
  )

table_s3 <- simulation3_long |>
  group_by(mechanism, estimator, n) |>
  summarise(
    replications = n(),
    true_value = 1,
    mean_true_effect = mean(true_effect),
    mean_estimate = mean(estimate),
    bias = mean(estimation_error),
    empirical_sd = sd(estimate),
    rmse = sqrt(mean(estimation_error^2)),
    mc_se_bias = sd(estimation_error) / sqrt(replications),
    mean_prop_attentive = mean(prop_attentive),
    mean_prop_classified_attentive = mean(prop_classified_attentive),
    .groups = "drop"
  ) |>
  arrange(mechanism, estimator)

print(table_s3)

write.csv(
  table_s3,
  file = "table_s3_simulation3.csv",
  row.names = FALSE
)

table_s3_latex <- table_s3 |>
  select(
    Mechanism = mechanism,
    Estimator = estimator,
    `True value` = true_value,
    `Mean estimate` = mean_estimate,
    Bias = bias,
    `Empirical SD` = empirical_sd,
    RMSE = rmse,
    `MC SE` = mc_se_bias
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 4,
    caption = "Complete Monte Carlo Results for Simulation 3",
    label = "tab:simulation3-complete",
    escape = TRUE
  )

writeLines(table_s3_latex, "table_s3_simulation3.tex")

# -----------------------------------------------------------------------------
# Bias-decomposition tables retained for the main manuscript
# -----------------------------------------------------------------------------

decomposition_long <- simulation3_wide |>
  select(
    replication,
    mechanism,
    total_bias,
    handling_bias,
    misclassification_bias,
    decomposition_check
  ) |>
  pivot_longer(
    cols = c(
      decomposition_check,
      misclassification_bias,
      handling_bias,
      total_bias
    ),
    names_to = "quantity",
    values_to = "value"
  ) |>
  mutate(
    quantity = factor(
      quantity,
      levels = c(
        "decomposition_check",
        "misclassification_bias",
        "handling_bias",
        "total_bias"
      ),
      labels = c(
        "Decomposition check",
        "Misclassification bias",
        "Handling-strategy bias",
        "Total practical bias"
      )
    )
  )

decomposition_summary <- decomposition_long |>
  group_by(mechanism, quantity) |>
  summarise(
    mean = mean(value),
    sd = sd(value),
    mc_se = sd(value) / sqrt(n()),
    ci_lo = mean - 1.96 * mc_se,
    ci_hi = mean + 1.96 * mc_se,
    q025 = quantile(value, 0.025),
    q50 = quantile(value, 0.50),
    q975 = quantile(value, 0.975),
    .groups = "drop"
  ) |>
  arrange(mechanism, quantity)

print(decomposition_summary)

write.csv(
  decomposition_summary,
  file = "simulation3_bias_decomposition.csv",
  row.names = FALSE
)

write_decomposition_table <- function(mechanism_name) {
  table_data <- decomposition_summary |>
    filter(mechanism == mechanism_name) |>
    select(
      Quantity = quantity,
      Mean = mean,
      SD = sd,
      `MC SE` = mc_se,
      `CI-L` = ci_lo,
      `CI-U` = ci_hi
    )
  
  latex_table <- kable(
    table_data,
    format = "latex",
    booktabs = TRUE,
    digits = 4,
    caption = paste0(
      "Monte Carlo Summary of Bias Decomposition Components in ",
      "Simulation 3 Under ", mechanism_name
    ),
    label = paste0("tab:mc-bias-decomp-", tolower(mechanism_name)),
    escape = TRUE
  )
  
  writeLines(
    latex_table,
    paste0("table_simulation3_decomposition_", tolower(mechanism_name), ".tex")
  )
}

write_decomposition_table("CRCR")
write_decomposition_table("CRNAR")
