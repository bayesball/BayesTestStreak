## ----message = FALSE--------------------------------------
library(BayesTestStreak)


## ---------------------------------------------------------
(ja_id <- find_id("Jose Abreu"))


## ---------------------------------------------------------
ja_data <- streak_data(ja_id, pbp2016, "H", AB = TRUE)
head(ja_data)


## ---------------------------------------------------------
plot_streak_data(ja_data)


## ----message = FALSE--------------------------------------
ja_ma <- moving_average(ja_data, width = 20)
head(ja_ma) 


## ---------------------------------------------------------
moving_average_plot(ja_ma) +
  ggtitle("Jose Abreu Moving AVG")


## ---------------------------------------------------------
ja_sp <- find_spacings(ja_data)
head(ja_sp)


## ---------------------------------------------------------
geometric_plot(ja_sp)


## ---------------------------------------------------------
permutation_test(ja_data)


## ---------------------------------------------------------
out <- bayes_factor_logK(ja_data)
head(out)


## ---------------------------------------------------------
ggplot(out, aes(log_K, log_BF)) +
  geom_line()

