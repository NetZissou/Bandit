# param_logs
# alpha
# beta
N_BANDIT <- 10
init_param_logs <- function(n_bandit = N_BANDIT) {
  param_logs <- list(
    alpha = tibble(
      step = 1,
      param = list(
        tibble(
          bandit = paste0("alpha_", c(1:n_bandit)),
          value = 1
        )
      )
    ),
    beta = tibble(
      step = 1,
      param = list(
        tibble(
          bandit = paste0("beta_", c(1:n_bandit)),
          value = 1
        )
      )
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
  alpha_post <- param_logs$alpha %>%
    arrange(step) %>%
    tail(1) %>%
    select(param) %>%
    unnest(cols = c(param))
  beta_post <- param_logs$beta %>%
    arrange(step) %>%
    tail(1) %>%
    select(param) %>%
    unnest(cols = c(param))
  
  n_bandit <- nrow(beta_post)
  
  posterior_distribution <- 
    tibble(
      bandit = paste0("banit_", c(1:n_bandit))
    ) %>%
    bind_cols(
      alpha_post %>%
        select(alpha = value),
      beta_post %>%
        select(beta = value)
    )
  
  # Pull Random Sample from posterior distribution ----------------
  posterior_distribution <- posterior_distribution %>%
    mutate(
      bandit_id = row_number(),
      p = posterior_distribution %>%
        select(
          shape1 = alpha, shape2 = beta
        ) %>% pmap_dbl(rbeta, n = 1)
    )
  return(posterior_distribution)
}

peek_top_recommendation <- function(posterior_distribution, top_n = 5) {
  
  return(
    posterior_distribution %>%
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

update_posterior <- function(testing_data, param_logs) {
   
   # Latest Parameters
   current_step <- param_logs$alpha %>%
     arrange(step) %>%
     tail(1) %>%
     pull(step)
   latest_alpha <- param_logs$alpha %>%
     arrange(step) %>%
     tail(1) %>%
     select(param) %>%
     unnest(cols = param)
   
   latest_beta <- param_logs$beta %>%
     arrange(step) %>%
     tail(1) %>%
     select(param) %>%
     unnest(cols = param)
   
   # Update Alpha
   updated_alpha <- 
     latest_alpha %>%
     bind_rows(
       testing_data %>%
         transmute(
           bandit = paste0("alpha_", bandit),
           value = alpha
         )
     ) %>%
     group_by(bandit) %>%
     summarise_at(vars(value), .funs = list(sum)) %>%
     mutate(
       bandit_id = parse_number(bandit)
     ) %>%
     arrange(bandit_id) %>%
     select(-bandit_id)
   
   param_logs$alpha <- param_logs$alpha %>%
     bind_rows(
       tibble(
         step = current_step + 1,
         param = list(updated_alpha)
       )
     )
   # Update Beta 
   updated_beta <- 
     latest_beta %>%
     bind_rows(
       testing_data %>%
         transmute(
           bandit = paste0("beta_", bandit),
           value = beta
         )
     ) %>%
     group_by(bandit) %>%
     summarise_at(vars(value), .funs = list(sum)) %>%
     mutate(
       bandit_id = parse_number(bandit)
     ) %>%
     arrange(bandit_id) %>%
     select(-bandit_id)
   
   param_logs$beta <- param_logs$beta %>%
     bind_rows(
       tibble(
         step = current_step + 1,
         param = list(updated_beta)
        )
      )
    assign("param_logs", param_logs, envir = .GlobalEnv)
}

trace_plot <- function(param_logs, bandit = 1) {
  
  get_param_value <- function(tbl, param = "alpha", id = 1) {
    tbl %>%
      filter(bandit == paste0(param, "_", id)) %>%
      pull(value)
  }
  
  plot_title <- paste0(
    "Parameter Tracing Plot of Location: ",
    bandit
  )
  
  result <- param_logs$alpha %>%
    rename(alpha = param) %>%
    left_join(
      param_logs$beta %>%
        rename(beta = param),
      by = "step"
    ) %>%
    mutate(
      alpha = map_dbl(alpha, get_param_value, 
                      param = "alpha", id = bandit),
      beta = map_dbl(beta, get_param_value, 
                      param = "beta", id = bandit),
      theta = alpha/(alpha + beta)
    ) %>%
    ggplot(aes(x = step, y = theta)) +
    geom_line(color = "darkred") + 
    labs(y = "Positivity Rate",
         title = plot_title) +
    theme_minimal()
  
  return(result)
}

























