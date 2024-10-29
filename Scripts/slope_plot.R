## this file shows to the referees why our estimation yields the same results for OLS, RE, FE
##
##



##  1. computing the effects

  glance <- df %>% 
    group_by(treatment, subject, income2, caddy, female, age, education) %>% 
    summarise(scoreFSA = (sum(FSAKcal))/sum(actual_Kcal),
              expenditure = 2000*sum(actual_observedprice)/sum(actual_Kcal)) %>% 
    pivot_longer(scoreFSA:expenditure, names_to = "indicator", values_to = "value") %>% 
    pivot_wider(names_from = caddy, values_from = value) %>% 
    mutate(diff = (`2`-`1`)) 
  
  ## renaming the treatments for plotting (possibly: move this up so that it applies everywhere)
  glance <- glance %>% 
    mutate(treatment = as.factor(treatment),
           treatment = fct_recode(treatment, "NutriScore" = "NS",
                                  "NS + large price" = "NS_pc_exp",
                                  "NS + small price" = "NS_ct_exp",
                                  "Implicit price" = "pc_imp",
                                  "Explicit price" = "pc_exp")) %>% 
    mutate(treatment = fct_relevel(treatment, "NutriScore", "NS + small price", "NS + large price"),
           treatment = fct_rev(treatment))
  
  ## creating variables for faceting and layout
  glance <- glance %>% 
    mutate(policy = case_when(treatment == "NutriScore" ~ "Labeling",
                              treatment %in% c("NS + large price","NS + small price") ~ "Policy mix",
                              treatment %in% c("Implicit price", "Explicit price") ~ "Price policy"),
           policy = as.factor(policy)) %>% 
    mutate(indicator = case_when(indicator == "scoreFSA" ~ "Δ scoreFSA",
                                 indicator == "expenditure" ~ "Δ expenditure"),
           indicator = fct_relevel(indicator, "Δ scoreFSA"))
  
  
## restircting attention to scoreFSA
  glance <- glance %>% 
    filter(indicator == "Δ scoreFSA") %>% 
    pivot_longer(cols = `1` | `2`, names_to = "caddy", values_to = "value") %>% 
    mutate(caddy = as.numeric(caddy)) %>% 
    mutate(treatment = as.character(treatment)) %>% 
    mutate(treatment = case_when(treatment == "Neutre2016" ~ "Benchmark", 
                                 treatment == "NS2016"     ~ "NutriScore 2016", 
                                 treatment == "NutriScore" ~ "NutriScore 2019", 
                                 TRUE ~ treatment)) 
  
## means by caddy and treatment  
  est <- glance %>% 
    group_by(caddy, treatment) %>% 
    summarise(value = mean(value))
  
  
## check that the difference in means over caddies by treatment and with respect to the benchmark category
## yields **exactly** the coefficients estimated by OLS, FE and RE.  
  
  # reshape and compute difference
  check <- est %>% 
    pivot_wider(names_from = caddy, values_from = value, names_prefix = "mean_caddy_") %>% 
    mutate(diff = mean_caddy_2 - mean_caddy_1)
  
  # extract the reference category, Benchmark
  reference_effect <- check$diff[check$treatment=="Benchmark"]
  
  # generate the check table
  check <- check %>% 
    mutate(diff_benchmark = diff - reference_effect)
  
  # table
  sink("Extra_tables/reg_coef_as_means.tex")
  check %>% 
    kbl(booktabs = T, 
        col.names = c("", "Mean Cart 1", "Mean Cart 2", "Raw Δ", "Δ w.r.t. Benchmark"), 
        format = "latex", 
        caption = "Regression coefficients computed as differences in means w.r.t. Benchmark", 
        label = "RR3_tab"
    ) %>% 
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  position = "center")
  sink()
            
### plot for reviewers
### all data points and estimated lines
  glance %>% 
    ggplot()+
    ## main plot: all data
    aes(x = caddy, y = value, group = subject, color = value)+
    geom_point()+
    geom_line(alpha = .1, color = "grey30")+
    scale_color_gradient(name = "Score FSA", high = "red", low = "green")+
    ## ancillary: means
    geom_line(data = est, aes(x = caddy, y = value, group = 1), inherit.aes = F, color = "darkred", linewidth = 1.3)+
    geom_point(data = est, aes(x = caddy, y = value), inherit.aes = F, color = "darkred", size = 3)+
    scale_x_continuous(breaks = c(1,2))+
    facet_wrap(~treatment, nrow = 2)+
    labs(y = "Score FSA", x = "Shopping cart")+
    theme_ipsum()+
    theme(panel.grid.minor.y = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank(), 
          plot.background = element_rect(fill = "white", color = "white"))
  
## saving plot  
  ggsave("Figures/Estimation_plot_for_reviewers.png", 
         width = 12/1.2, 
         height = 9/1.2, 
         dpi = 500)
            