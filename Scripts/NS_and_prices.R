 
## This file analyses the addition of price policies to NutriScore
##
## It compares the results of the treatment NutriScore 2019 vs NS + cent and NS + perc
## It does so over two dimensions: score FSA and expenditure
## It does so by comparing means, creating a table, and running a regression (MAYBE NOT NEEDED)



# getting the data
cp <- df %>% 
  filter(treatment %in% c("NS", "NS_pc_exp", "NS_ct_exp"))

# getting the indicators
cp <- cp %>% 
  group_by(treatment, subject, caddy) %>% 
  summarise(scoreFSA = (sum(FSAKcal))/sum(actual_Kcal),
            expenditure = 2000*sum(actual_observedprice)/sum(actual_Kcal)) %>% 
  pivot_longer(scoreFSA:expenditure, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = caddy, values_from = value) %>% 
  mutate(diff = (`2`-`1`))

# renaming treatments
cp <- cp %>% 
  mutate(treatment = as.factor(treatment),
         treatment = fct_recode(treatment, "NutriScore" = "NS",
                                                   "NS + large price" = "NS_pc_exp",
                                                   "NS + small price" = "NS_ct_exp"))
tests <- cp %>% 
  select(-diff) %>% 
  group_by(treatment, indicator) %>% 
  pivot_longer(`1`:`2`, names_to = "caddy", values_to = "value") %>% 
  group_modify(~tidy(wilcox.test(.$value~.$caddy, paired = T))) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  select(treatment, indicator, p.value)         

table <- cp %>% 
  group_by(treatment, indicator) %>% 
  rename("caddy1" = `1`, "caddy2" = `2`, "caddydiff" = "diff") %>% 
  summarise(across(starts_with("caddy"), list(mean = mean, sd = sd))) %>% 
  left_join(tests, by = c("treatment", "indicator")) 


## Table 8 -- policy mix

nice_table <- table %>% 
  mutate(across(starts_with("caddy"), ~round(.x, digits = 2))) %>% 
  mutate(caddy1 = paste(caddy1_mean, " (", caddy1_sd, ")", sep = ""),
         caddy2 = paste(caddy2_mean, " (", caddy2_sd, ")", sep = ""),
         "Δ" = paste(caddydiff_mean, " (", caddydiff_sd, ")", sep = "")) %>% 
  select(treatment, indicator, caddy1, caddy2, "Δ", p.value) %>% 
  pivot_wider(names_from = indicator, values_from = c(caddy1, caddy2, "Δ", p.value)) %>% 
  select(treatment, ends_with("FSA"), ends_with("ure"))

sink(file = "Tables/tab_NS_and_prices.tex")
nice_table %>% 
  mutate(treatment = fct_relevel(treatment, "NutriScore", "NS + small price", "NS + large price")) %>% 
  mutate("p.value_expenditure" = case_when(`p.value_expenditure` == 0.000 ~ "< 0.001",
                                             TRUE ~ as.character(`p.value_expenditure`)),
         "p.value_scoreFSA" = case_when(`p.value_scoreFSA` == 0.000 ~ "< 0.001",
                                          TRUE ~ as.character(`p.value_scoreFSA`))) %>% 
  arrange(treatment) %>% 
  kbl(booktabs = T, 
      col.names = c("", rep(c("cart 1", "cart 2", "difference", "p-value"),2)), 
      format = "latex", 
      caption = "Overview of nutritional and economic results", 
      label = "ataglance"
  ) %>% 
  add_header_above(c(" ", "ScoreFSA" = 4, "Expediture" = 4)) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  footnote(general = "Means (standard deviations) for each variable. P-values from Wilcoxon signed-rank test of the difference between carts 1 and 2.", 
           general_title = "")
sink()

## extra plot for presentation purposes
cp %>% 
  mutate(indicator = case_when(indicator == "scoreFSA" ~ "Δ scoreFSA",
                               indicator == "expenditure" ~ "Δ expenditure"),
         indicator = fct_relevel(indicator, "Δ scoreFSA")) %>% 
  ggplot(aes(treatment, diff, fill = indicator))+
  geom_hline(yintercept = 0, color = 'indianred', linetype = 'dashed', size = 0.2)+
  geom_half_violin(side = "r", position = position_nudge(x = +0.07), trim = F, alpha = 0.5, size = 0.2)+
  geom_quasirandom(width = 0.05, alpha = 0.2, pch = 21, fill= "grey20", color = "black")+
  stat_summary(position = position_nudge(x = -0.1), fun.data = "mean_cl_boot")+
  facet_grid(.~indicator, scales = "free")+
  scale_fill_scico_d(palette = "bamako")+
  labs(x = "", y = "Policy-induced change")+
  coord_flip()+
  theme_ipsum_ps()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")
ggsave("Figures/extra_figure_NS_prices.png", width = 12, height = 5, units = "in", dpi = 300)

## Extra-tests: testing one policy against each other
a <- cp %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(wilcox.test(.$diff[.$treatment == "NutriScore"], .$diff[.$treatment == "NS + large price"])))

b <- cp %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(wilcox.test(.$diff[.$treatment == "NutriScore"], .$diff[.$treatment == "NS + small price"])))

c <- cp %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(wilcox.test(.$diff[.$treatment == "NS + small price"], .$diff[.$treatment == "NS + large price"])))

tests <- bind_rows(a,b,c) %>% 
  bind_cols(comparison = rep(c("NS vs large", "NS vs small", "large vs small"), each = 2)) %>% 
  select(indicator, comparison, p.value) %>% 
  arrange(indicator, comparison, p.value) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  pivot_wider(names_from = indicator, values_from = p.value)

sink("Tables/extra_tests_additivity.tex")
tests %>% 
  kbl(booktabs = T, 
      #col.names = c("", rep(c("cart 1", "cart 2", "difference", "p-value"),2)), 
      format = "latex", 
  ) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center")
sink()

## cleaning up
rm(a, b, c)
rm(cp)
rm(table, nice_table, tests)
