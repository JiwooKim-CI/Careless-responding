## Library


library(dplyr)
library(tidyverse)
library(ggplot2)
library(tikzDevice)
library(patchwork)


## Unsystematic answered CR

### CRCR
set.seed(1)

simulate_once <- function(n = 30000) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  p <- rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
  c <- rnorm(n, 0, 1)
  x_lat <- .5*c + rnorm(n, 0, 1)
  x <- ifelse(x_lat > 0, 1, 0)
  u <- rnorm(n, 0, 1)
  p <- 0.6 * c + rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
  p <- 0.5 * x + rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
  p <- rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
  x <- rnorm(n, 0, 1)
  c_lat <- .6*x + rnorm(n, 0, 1)
  c <- ifelse(c_lat > 0, 1, 0)
  u <- rnorm(n, 1, 1)
  p <- .7 * c + rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
  p <- 0.5 * x + rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
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
