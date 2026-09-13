## Library


library(dplyr)
library(tidyverse)
library(ggplot2)
library(tikzDevice)
library(patchwork)
library(knitr)

## Unsystematic answered CR

### CRCR
set.seed(1)

simulate_once <- function(n = 30000) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  p <- rep(0.5, n)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crcr_unbiased <- res
summary(res)




## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p1_1 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Unbiased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)


p1_1


### CRAR

set.seed(1)

simulate_once <- function(n = 30000) {
  c <- rbinom(n, 1, 0.5)
  x <- 0.5 * (c - 0.5) + rnorm(n, 0, sqrt(0.9375))
  u <- rnorm(n, 0, 1)
  p <- plogis(0.6 * c)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  mean_C0 <- mean(x_CR[c == 0&r == 1], na.rm = TRUE)
  mean_C1 <- mean(x_CR[c == 1&r == 1], na.rm = TRUE)
  
  w_C0 <- mean(c == 0)
  w_C1 <- mean(c == 1)
  
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  mean_obs_C1  <- mean_C0 * w_C0 + mean_C1 * w_C1
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    mean_obs_C1  = mean_obs_C1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crar_unbiased <- res
summary(res)



## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,mean_obs_C1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "mean_obs_C1"  ~ "Condition on C and R",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p2_1 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1",
                          "Condition on C and R")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Unbiased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)
p2_1




### CRNAR


set.seed(1)

simulate_once <- function(n = 30000) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  p <- plogis(0.5 * x)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crnar_unbiased <- res
summary(res)



## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p3_1 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Unbiased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)
p3_1



## Systematic answered CR

### CRCR

set.seed(1)

simulate_once <- function(n = 30000) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 1, 1)
  p <- rep(0.5, n)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crcr_biased <- res
summary(res)

## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p1_2 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Biased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)
p1_2
tikz("figure_CRCR.tex", standAlone = TRUE, width = 7.5, height = 3.6)

(p1_1 | p1_2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

dev.off()

### CRAR


set.seed(1)

simulate_once <- function(n = 30000) {
  c <- rbinom(n, 1, 0.5)
  x <- 0.5 * (c - 0.5) + rnorm(n, 0, sqrt(0.9375))
  u <- rnorm(n, 1, 1)
  p <- plogis(0.6 * c)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  mean_C0 <- mean(x_CR[c == 0&r == 1], na.rm = TRUE)
  mean_C1 <- mean(x_CR[c == 1&r == 1], na.rm = TRUE)
  
  w_C0 <- mean(c == 0)
  w_C1 <- mean(c == 1)
  
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  mean_obs_C1  <- mean_C0 * w_C0 + mean_C1 * w_C1
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    mean_obs_C1  = mean_obs_C1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crar_biased <- res
summary(res)



## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,mean_obs_C1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "mean_obs_C1"  ~ "Condition on C and R",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p2_2 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1",
                          "Condition on C and R")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Biased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)
p2_2

tikz("figure_CRAR.tex", standAlone = TRUE, width = 7.5, height = 3.6)

(p2_1 | p2_2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

dev.off()

### CRNAR

set.seed(1)

simulate_once <- function(n = 30000) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 1, 1)
  p <- plogis(0.5 * x)
  r <- rbinom(n, 1, p)
  x_CR <- ifelse(r == 0, u, x)
  
  true_mean    <- mean(x)
  mean_obs_all <- mean(x_CR, na.rm = TRUE)
  mean_obs_R1  <- mean(x_CR[r == 1], na.rm = TRUE)
  prop_R1      <- mean(r)
  
  data.frame(
    true_mean    = true_mean,
    mean_obs_all = mean_obs_all,
    mean_obs_R1  = mean_obs_R1,
    prop_R1      = prop_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)
res_crnar_biased <- res
summary(res)




## 1) long-format
res_long <- res %>%
  mutate(bias_all = mean_obs_all - true_mean,
         bias_R1  = mean_obs_R1  - true_mean) %>%
  pivot_longer(
    cols = c(true_mean, mean_obs_all, mean_obs_R1,
             bias_all, bias_R1),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(estimator = case_when(
    metric == "true_mean"    ~ "True mean",
    metric == "mean_obs_all" ~ "Naive (All)",
    metric == "mean_obs_R1"  ~ "Condition on R=1",
    metric == "bias_all"     ~ "Bias: Naive (All)",
    metric == "bias_R1"      ~ "Bias: Cond. on R=1",
    TRUE ~ metric
  ))

## 2) True/Naive/R=1 Compare
p3_2 <- res_long %>%
  filter(estimator %in% c("True mean","Naive (All)","Condition on R=1")) %>%
  ggplot(aes(x = value, fill = estimator)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Biased U",
       x = "Estimate", y = "Density", fill = "Estimator") +
  theme_minimal(base_size = 12)
p3_2


tikz("figure_CRNAR.tex", standAlone = TRUE, width = 7.5, height = 3.6)

(p3_1 | p3_2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

dev.off()
# -----------------------------------------------------------------------------
# Complete Monte Carlo results: Supplemental Table S1
# -----------------------------------------------------------------------------

prepare_condition <- function(data, mechanism, u_distribution) {
  data |>
    mutate(
      replication = seq_len(n()),
      mechanism = mechanism,
      u_distribution = u_distribution
    )
}

simulation1_wide <- bind_rows(
  prepare_condition(
    res_crcr_unbiased,
    mechanism = "CRCR",
    u_distribution = "Unshifted U"
  ),
  prepare_condition(
    res_crar_unbiased,
    mechanism = "CRAR",
    u_distribution = "Unshifted U"
  ),
  prepare_condition(
    res_crnar_unbiased,
    mechanism = "CRNAR",
    u_distribution = "Unshifted U"
  ),
  prepare_condition(
    res_crcr_biased,
    mechanism = "CRCR",
    u_distribution = "Shifted U"
  ),
  prepare_condition(
    res_crar_biased,
    mechanism = "CRAR",
    u_distribution = "Shifted U"
  ),
  prepare_condition(
    res_crnar_biased,
    mechanism = "CRNAR",
    u_distribution = "Shifted U"
  )
)

# Convert the estimator columns to long format.
# mean_obs_C1 exists only under CRAR and represents the C-standardized estimator.

simulation1_long <- simulation1_wide |>
  pivot_longer(
    cols = any_of(
      c(
        "mean_obs_all",
        "mean_obs_R1",
        "mean_obs_C1"
      )
    ),
    names_to = "estimator",
    values_to = "estimate"
  ) |>
  filter(!is.na(estimate)) |>
  mutate(
    estimator = recode(
      estimator,
      mean_obs_all = "Naive (all responses)",
      mean_obs_R1 = "Oracle deletion",
      mean_obs_C1 = "Covariate adjusted"
    ),
    estimation_error = estimate - true_mean
  )

# Summarize performance across Monte Carlo replications.
# Bias and RMSE are calculated relative to the realized true mean
# in the corresponding replication.

table_s1 <- simulation1_long |>
  group_by(
    mechanism,
    u_distribution,
    estimator
  ) |>
  summarise(
    replications = n(),
    true_value = 0,
    mean_true_mean = mean(true_mean),
    mean_estimate = mean(estimate),
    bias = mean(estimation_error),
    empirical_sd = sd(estimate),
    rmse = sqrt(mean(estimation_error^2)),
    mc_se_bias = sd(estimation_error) / sqrt(replications),
    mean_prop_attentive = mean(prop_R1),
    .groups = "drop"
  ) |>
  arrange(
    mechanism,
    u_distribution,
    estimator
  )

print(table_s1)

# Save the complete numerical results as a CSV file.

write.csv(
  table_s1,
  file = "table_s1_simulation1.csv",
  row.names = FALSE
)

# Create the LaTeX version used in the supplemental materials.

table_s1_latex <- table_s1 |>
  select(
    Mechanism = mechanism,
    `U distribution` = u_distribution,
    Estimator = estimator,
    `True value` = true_value,
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
    caption = "Complete Monte Carlo Results for Simulation 1",
    label = "tab:simulation1-complete",
    escape = TRUE
  )

writeLines(
  table_s1_latex,
  "table_s1_simulation1.tex"
)
