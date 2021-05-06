# param_logs
# alpha
# beta
N_BANDIT <- 10
init_param_logs <- function(n_bandit = N_BANDIT) {
  param_logs <- list(
    alpha = tibble(
      bandit = paste0("alpha_", c(1:n_bandit)),
      init = 1
    ) %>%
      pivot_wider(
        bandit, names_from = bandit, 
        values_from = init
      ),
    beta = tibble(
      bandit = paste0("beta_", c(1:n_bandit)),
      init = 1
    ) %>%
      pivot_wider(
        bandit, names_from = bandit, 
        values_from = init
      )
  )
  return(param_logs)
}

init_site_selection_logs <- function(n_bandit = N_BANDIT) {
  return(
    alpha = tibble(
      bandit = paste0("banidt_", c(1:n_bandit)),
      init = 0
    ) %>%
      pivot_wider(
        bandit, names_from = bandit, 
        values_from = init
      )
  )
}

param_logs <- init_param_logs(n_bandit = N_BANDIT)
site_selection_logs <- init_site_selection_logs(n_bandit = N_BANDIT)

tps_sampling_recommendation <- function(param_logs) {
  # Extract the most recent posterior ------------------------------
  alpha_post <- param_logs$alpha %>% tail(1)
  beta_post <- param_logs$beta %>% tail(1)
  n_bandit <- ncol(alpha_post)
  
  aggregate_distribution <- 
    tibble(
      bandit = paste0("banit_", c(1:n_bandit))
    ) %>%
    bind_cols(
      alpha_post %>%
        pivot_longer(everything(), 
                     names_to = "param", values_to = "alpha") %>%
        select(alpha),
      beta_post %>%
        pivot_longer(everything(), 
                     names_to = "param", values_to = "beta") %>%
        select(beta)
    )
  
  # Pull Random Sample from posterior distribution ----------------
  aggregate_distribution <- aggregate_distribution %>%
    mutate(
      bandit_id = row_number(),
      p = aggregate_distribution %>%
        select(
          shape1 = alpha, shape2 = beta
        ) %>% pmap_dbl(rbeta, n = 1)
    )
  return(aggregate_distribution)
}

peek_top_recommendation <- function(aggregate_distribution, top_n = 5) {
  
  return(
    aggregate_distribution %>%
    arrange(-p) %>%
    head(top_n)
  )
}


# Now it is the teams choice to 
# visit certain locations
# and provide testing data from the sites

# data format
# location_name, positive, total
# finalized to
# bandit, alpha (positive), beta (negative)

tidy_testing_data <- function(testing_data) {}

update_posterior <- function(testing_data, param_logs, site_selection_logs) {
   
   # Update Alpha
   alpha_names <- names(param_logs$alpha)
   
   updated_alpha <- 
     param_logs$alpha %>%
     tail(1) %>%
     pivot_longer(everything(), 
                  values_to = "alpha",
                  names_to = "name") %>%
     transmute(
       bandit = parse_number(name),
       alpha
     ) %>%
     bind_rows(
       testing_data %>%
         select(bandit, alpha)
     ) %>%
     group_by(bandit) %>%
     summarise_at(vars(alpha), .funs = list(sum)) %>%
     arrange(bandit) %>% 
     pivot_wider(values_from = alpha, 
                 names_from = bandit) %>%
     set_names(alpha_names)
    param_logs$alpha <- param_logs$alpha %>%
      bind_rows(
        updated_alpha
      )
    assign("param_logs", param_logs, envir = .GlobalEnv)
   # Update Beta
   
   beta_names <- names(param_logs$beta)
   
   updated_beta <- 
     param_logs$beta %>%
     tail(1) %>%
     pivot_longer(everything(), 
                  values_to = "beta",
                  names_to = "name") %>%
     transmute(
       bandit = parse_number(name),
       beta
     ) %>%
     bind_rows(
       testing_data %>%
         select(bandit, beta)
     ) %>%
     group_by(bandit) %>%
     summarise_at(vars(beta), .funs = list(sum)) %>%
     arrange(bandit) %>% 
     pivot_wider(values_from = beta, 
                 names_from = bandit) %>%
     set_names(beta_names)
   param_logs$beta <- param_logs$beta %>%
     bind_rows(
       updated_beta
     )
   assign("param_logs", param_logs, envir = .GlobalEnv)
}



























