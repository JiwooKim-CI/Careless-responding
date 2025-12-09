set.seed(1)

simulate_once <- function(n = 30000, prop_mis = 0.7) {
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 1, 1)
  y <- .4*x + rnorm(n, 0, 1)
  p <- 0.5 * y + .4*x + rnorm(n, 0, 1)
  r <- ifelse(p > 0, 1, 0)
  y_CR <- ifelse(r == 0, u, y)
  
  true_path    <- coef(lm(y~x))[2]
  path_all <- coef(lm(y_CR~x))[2]
  path_obs_R1  <- coef(lm(y_CR[r==1]~x[r==1]))[2]
  
  
  data.frame(
    true_path,path_all,path_obs_R1
  )
}

B <- 1000
res_list <- vector("list", B)
for (b in seq_len(B)) {
  res_list[[b]] <- simulate_once(n = 30000)
}
res <- do.call(rbind, res_list)

summary(res)
res_long <- res %>%
  pivot_longer(
    cols = everything(),
    names_to = "Estimator",
    values_to = "Estimate"
  ) %>%
  mutate(
    Estimator = factor(
      Estimator,
      levels = c("true_path", "path_all", "path_obs_R1"),
      labels = c("True path", "Naive (All)", "Condition on R=1")
    )
  )

p_collider <- ggplot(res_long, aes(x = Estimate, fill = Estimator)) +
  geom_density(alpha = 0.35) +
  labs(
    x = "Estimated path coefficient",
    y = "Density"
  ) +
  scale_fill_manual(
    values = c("steelblue", "seagreen3", "salmon")
  ) +
  theme_minimal(base_size = 12) 

tikz("figure_Collider.tex", standAlone = TRUE, width = 7.5, height = 3.6)

p_collider

dev.off()