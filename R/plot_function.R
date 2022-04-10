bargraph_choices <- function(data, choicevar, ylab, sep_by_treatment){
  if(!sep_by_treatment){
    data <- data %>% 
      mutate(decision_nr_plotting = as.character(decision_nr)) 
  } else{
    data <- data %>% 
      mutate(decision_nr_plotting = paste(decision_nr, treatment, sep = "_"))
  }
  p <- data %>%
    group_by(decision_nr_plotting) %>% 
    summarise(ratio_hire  = paste0(sum(!!as.symbol(choicevar)), "/", n()),
              mean_hire   = mean(!!as.symbol(choicevar)),
              ci_lower    = binom.confint(sum(!!as.symbol(choicevar)), n(), methods = "wilson")$lower,
              ci_upper    = binom.confint(sum(!!as.symbol(choicevar)), n(), methods = "wilson")$upper) %>% 
    ggplot() + 
    geom_bar(aes(x = decision_nr_plotting, y = mean_hire),
             stat = "identity", col = "black", fill = "grey85") +
    geom_errorbar(aes(x = decision_nr_plotting, ymin = ci_lower, ymax = ci_upper),
                  width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    geom_text(aes(x = decision_nr_plotting, y = 0.03, label = ratio_hire), 
              size = 4, family = "Segoe UI Semilight") +
    theme_minimal(14) +
    scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(family = "Segoe UI Semilight")) +
    labs(y = ylab,
         x = "Decision")
  
  if(!sep_by_treatment){
    p <- p + scale_x_discrete(labels = function(x) parse(text=paste0("D[", x, "]")))
  } else{
    p <- p + scale_x_discrete(labels = function(x) parse(text=paste0("D[",
                                                                     str_remove(x, "_.*"),
                                                                     "]~(T", 
                                                                     str_remove(x, ".*_"),
                                                                     ")")))
  }
  return(p)
}
