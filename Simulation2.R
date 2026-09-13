## Simulation 2: Collider-type careless responding

library(dplyr)
library(tidyr)
library(ggplot2)
library(tikzDevice)
library(knitr)

set.seed(1)

n <- 30000
B <- 1000

simulate_once <- function(n = 30000) {
  x <- rnorm(n, mean = 0, sd = 1)
  u <- rnorm(n, mean = 1, sd = 1)
  y <- 0.4 * x + rnorm(n, mean = 0, sd = 1)

 
  # Here r = 1 denotes an attentive response.
  
  p <- plogis(0.5 * y + 0.4 * x)
  r <- rbinom(n, size = 1, prob = p)
 

  y_obs <- ifelse(r == 1, y, u)

  true_path <- unname(coef(lm(y ~ x))["x"])
  path_all <- unname(coef(lm(y_obs ~ x))["x"])
  path_oracle <- unname(coef(lm(y_obs[r == 1] ~ x[r == 1]))[2])

  tibble(
    true_path = true_path,
    naive = path_all,
    oracle_deletion = path_oracle,
    prop_attentive = mean(r)
  )
}

# Preserve the result from every replication instead of keeping only the
# final data set.
simulation2_wide <- bind_rows(lapply(seq_len(B), function(b) {
  simulate_once(n = n) |>
    mutate(replication = b)
})) |>
  mutate(n = n)

# Estimator-level results for the comprehensive supplemental table.
simulation2_long <- simulation2_wide |>
  pivot_longer(
    cols = c(true_path, naive, oracle_deletion),
    names_to = "estimator",
    values_to = "estimate"
  ) |>
  mutate(
    estimator = recode(
      estimator,
      true_path = "True-outcome estimator",
      naive = "Naive (all responses)",
      oracle_deletion = "Oracle deletion"
    )
  )

# The true-outcome estimator is included descriptively, but the two practical
# strategies are evaluated relative to the realized true-outcome coefficient
# from the same replication. This isolates contamination and selection effects
# from ordinary sampling variation in the true-outcome regression.
table_s2 <- simulation2_wide |>
  pivot_longer(
    cols = c(naive, oracle_deletion),
    names_to = "estimator",
    values_to = "estimate"
  ) |>
  mutate(
    estimator = recode(
      estimator,
      naive = "Naive (all responses)",
      oracle_deletion = "Oracle deletion"
    ),
    estimation_error = estimate - true_path
  ) |>
  group_by(estimator, n) |>
  summarise(
    replications = n(),
    true_value = 0.4,
    mean_true_path = mean(true_path),
    mean_estimate = mean(estimate),
    bias = mean(estimation_error),
    empirical_sd = sd(estimate),
    rmse = sqrt(mean(estimation_error^2)),
    mc_se_bias = sd(estimation_error) / sqrt(replications),
    mean_prop_attentive = mean(prop_attentive),
    .groups = "drop"
  )

print(table_s2)

write.csv(
  table_s2,
  file = "table_s2_simulation2.csv",
  row.names = FALSE
)

table_s2_latex <- table_s2 |>
  select(
    Estimator = estimator,
    `True value` = true_value,
    `Mean true path` = mean_true_path,
    `Mean estimate` = mean_estimate,
    Bias = bias,
    `Empirical SD` = empirical_sd,
    RMSE = rmse,
    `MC SE` = mc_se_bias,
    `Attentive proportion` = mean_prop_attentive
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 4,
    caption = "Complete Monte Carlo Results for Simulation 2",
    label = "tab:simulation2-complete",
    escape = TRUE
  )

writeLines(table_s2_latex, "table_s2_simulation2.tex")

# Reproduce the original density plot.
plot_data <- simulation2_long |>
  mutate(
    estimator = factor(
      estimator,
      levels = c(
        "True-outcome estimator",
        "Naive (all responses)",
        "Oracle deletion"
      ),
      labels = c(
        "True path",
        "Naive (All)",
        "Condition on R=1"
      )
    )
  )

p_collider <- ggplot(plot_data, aes(x = estimate, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0.4, linetype = "dashed") +
  labs(
    x = "Estimated path coefficient",
    y = "Density",
    fill = "Estimator"
  ) +
  scale_fill_manual(
    values = c("steelblue", "seagreen3", "salmon")
  ) +
  theme_minimal(base_size = 12)

tikz("figure_Collider.tex", standAlone = TRUE, width = 7.5, height = 3.6)
print(p_collider)
dev.off()
