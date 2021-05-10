# Upper Confidence Bound

# Importing the dataset

library(tidyverse)

get_most_frequent_bandit <- function(bandit_selected) {
  return(
    as.numeric(
      names(
        sort(
          table(bandit_selected), decreasing = T)
        )[1]
      )
  )
}

dataset <- 
  tibble(
    Ad.1 = rbernoulli(10000, p = 0.3),
    Ad.2 = rbernoulli(10000, p = 0.2),
    Ad.3 = rbernoulli(10000, p = 0.15),
    Ad.4 = rbernoulli(10000, p = 0.35),
    Ad.5 = rbernoulli(10000, p = 0.3),
    Ad.6 = rbernoulli(10000, p = 0.4),
    Ad.7 = rbernoulli(10000, p = 0.5),
    Ad.8 = rbernoulli(10000, p = 0.55),
    Ad.9 = rbernoulli(10000, p = 0.6),
    Ad.10 = rbernoulli(10000, p = 0.45),
  ) %>%
  mutate_if(is.logical, as.numeric)

# Implementing Random selection

rnd_selection <- function(data = dataset, seed = 1024) {
  set.seed(seed)
  N <- nrow(data)
  d <- ncol(data)
  ads_selected <- integer(0)
  total_reward = 0
  for (i in 1:N) {
    ad <- sample(1:d, 1)
    ads_selected <- append(ads_selected, ad)
    reward <- data[[i, ad]]
    total_reward <- total_reward + reward
  }
  cat("Total Reward: ", total_reward)
}

# UCB Random selection

ucb_selection <- function(data = dataset, seed = 1024) {
  
  # ==================================================================== #
  # ------------------------ Initialize -------------------------------- #
  # ==================================================================== #
  set.seed(seed)
  N <- nrow(dataset)    # N: total number of steps
  d <- ncol(dataset)    # d: total number of bandits
  
  ads_selected <- integer(0)           # max ucb selection for each step
  
  numbers_of_selections <- rep(0, d)   # n of selection for each bandits
  sums_of_rewards <- rep(0, d)         # sum of rewards for each bandits
  
  total_reward <- 0                    # total rewards
  
  for (i in 1 : d) {
    ads_selected <- c(ads_selected, i)
    numbers_of_selections[i] <- numbers_of_selections[i] + 1
    sums_of_rewards[i] <- sums_of_rewards[i] + data[[i, i]]
    total_reward <- total_reward + data[[i, i]]
  }
  
  # ==================================================================== #
  # ------------------------- Iteration -------------------------------- #
  # ==================================================================== #
  
  for (i in 1:(N-d)) {
    
    
    max_ucb_ad <- 0
    max_upper_bound <- 0
    
    # Find the bandit with the max upper bound mean of rewards
    for (j in 1:d) {
      avg_reward <- sums_of_rewards[j] / numbers_of_selections[j]
      delta <- sqrt(3/2 * log(i) / numbers_of_selections[j])
      upper_bound <- avg_reward + delta
      
      if (upper_bound > max_upper_bound) {
        max_ucb_ad <- j
        max_upper_bound <- upper_bound
      }
    }
    
  
    ads_selected <- c(ads_selected, max_ucb_ad)
    numbers_of_selections[max_ucb_ad] <- numbers_of_selections[max_ucb_ad] + 1
    sums_of_rewards[max_ucb_ad] <- sums_of_rewards[max_ucb_ad] + data[[i, max_ucb_ad]]
    total_reward <- total_reward + data[[i, max_ucb_ad]]
    
  }
  
  cat("Total Reward", total_reward, "\n")
  cat("Best Bandit Discovered: ", get_most_frequent_bandit(ads_selected))
}


# Thomphson Sampling Strategy
# return: param logs
tps_sampling <- function(data = dataset) {
  
  N <- nrow(data)
  d <- ncol(data)
  
  ads_selected <- integer(0)
  
  numbers_of_reward_1 <- 
    rep(0, d) %>% set_names(paste0("alpha_", c(1:10)))
    
  numbers_of_reward_0 <- 
    rep(0, d) %>% set_names(paste0("beta_", c(1:10)))
  
  alpha_logs <- tibble(
    bandit = paste0("alpha_", c(1:d)),
    init = 0
  ) %>%
    pivot_wider(
      bandit, names_from = bandit, 
      values_from = init
    )
  
  beta_logs <- tibble(
    bandit = paste0("beta_", c(1:d)),
    init = 0
  ) %>%
    pivot_wider(
      bandit, names_from = bandit, 
      values_from = init
    )
  
  total_reward  <- 0
  
  # Prior of theta: theta_i ~ Beta(1, 1) / Uniform(0, 1)
  # Likelihood: y|theta_i ~ Bernoulli(theta_i)
  # Posterior: theta_i|Y = y ~ Beta(number of success + 1, number of failures + 1)
  
  for (i in 1:N) {
    
    max_beta_ad <- 0
    max_random_beta <- 0
    
    for (j in 1:d) {
      random_beta <- rbeta(1, 
                           shape1 = numbers_of_reward_1[j] + 1, # alpha
                           shape2 = numbers_of_reward_0[j] + 1  # beta
      )
      if (random_beta > max_random_beta) {
        max_random_beta <- random_beta
        max_beta_ad <- j
      }
    }
    
    ads_selected <- c(ads_selected, max_beta_ad)
    reward <- data[[i, max_beta_ad]]
    
    if (reward == 1) {
      numbers_of_reward_1[max_beta_ad] <- numbers_of_reward_1[max_beta_ad] + 1
    } else {
      numbers_of_reward_0[max_beta_ad] <- numbers_of_reward_0[max_beta_ad] + 1
    }
    
    alpha_logs <- alpha_logs %>%
      bind_rows(
        numbers_of_reward_1
      )
    
    beta_logs <- beta_logs %>%
      bind_rows(
        numbers_of_reward_0
      )
    
    total_reward <- total_reward + reward
  }
  
  cat("Total Reward", total_reward, "\n")
  cat("Best Bandit Discovered: ", get_most_frequent_bandit(ads_selected))
  param_logs <- list(
    alpha = alpha_logs,
    beta = beta_logs
  )
  return(param_logs)
}

plot_param_trace <- function(param_logs, bandit) {
  
  trace_data <- tibble(
    alpha = param_logs$alpha %>% pull(bandit),
    beta = param_logs$beta %>% pull(bandit)
  ) %>%
    mutate(
      theta = alpha/(alpha + beta),
      step = row_number()
    )
  
  trace_plot <- trace_data %>%
    ggplot(aes(x = step, y = theta)) +
    geom_line()
  return(trace_plot)
}





