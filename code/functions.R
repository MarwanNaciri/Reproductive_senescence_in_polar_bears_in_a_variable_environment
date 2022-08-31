# This script contains a few functions used in the other scripts.

check_convergence_several_predictors <- function(params.plot, nimble_output) {
  # Process Nimble output into dataframe
  chain1 <- data.frame(nimble_output[["samples"]][["chain1"]]) %>%
    dplyr::select(all_of(params.plot)) %>%
    mutate(chain = "1",
           iteration = seq(1, dim(nimble_output[["samples"]][["chain1"]])[1], by = 1))
  chain2 <- data.frame(nimble_output[["samples"]][["chain2"]]) %>%
    dplyr::select(all_of(params.plot)) %>%
    mutate(chain = "2",
           iteration = seq(1, dim(nimble_output[["samples"]][["chain2"]])[1], by = 1))
  chains <- rbind(chain1, chain2) 
  
  chains_l <- pivot_longer(chains, cols = all_of(params.plot), names_to = "parameter") %>%
    mutate(parameter = factor(parameter, levels = params.plot)) 
  
  param.mean <- chains_l %>%
    group_by(parameter, chain) %>%
    summarise(m = mean(value))
  
  param.running.mean <- chains_l %>%
    arrange(parameter, iteration) %>%
    group_by(parameter, chain) %>%
    mutate(rm = cumsum(value)/iteration)
  
  trace.plots <- ggplot(data = chains_l, 
                        aes(x = iteration, y = value, color = chain)) +
    geom_line(alpha = 0.5) +
    labs(y = "trace") +
    theme(legend.position = "none") +
    facet_wrap( ~ parameter,
                scales = "free",
                ncol = 1)
  
  density.plots <- ggplot(data = chains_l, 
                          aes(x = value, color = chain, fill = chain)) +
    geom_vline(xintercept = 0) +
    geom_density(alpha = 0.25) +
    
    labs(x = "density") +
    theme(legend.position = "none") +
    facet_wrap( ~ parameter,
                scales = "free_y",
                ncol = 1)
  
  running.mean.plot <- ggplot(param.running.mean, 
                              aes(x = iteration, y = rm, color = chain)) + 
    geom_line() + 
    geom_hline(aes(yintercept = m), param.mean,
               colour = "black", alpha = 0.5) + 
    ylab("running Mean") +
    facet_grid(parameter ~ chain, scales = "free")
  
  # Plot all the plots together
  diagnostic_plot <- plot_grid(trace.plots,
                               density.plots, 
                               running.mean.plot,
                               ncol = 3, nrow = 1)
  return(diagnostic_plot)
}


get_mean_and_CI <- function(x, lower, upper) {
  output <- data.frame(y = mean(x), 
                       ymin = quantile(x, probs = lower),
                       ymax = quantile(x, probs = upper))
  return(output)
}



scale_ext <- function(x, unscaled_variable) {
  y <- (x - mean(unscaled_variable))/sd(unscaled_variable) 
  return(y)
}

unscale <- function(x, unscaled_variable) {
  y <- x * sd(unscaled_variable) + mean(unscaled_variable)
  return(y)
}
