
## This file analyses the addition of price policies to NutriScore
##
## It compares the results of the treatment NutriScore 2019 vs NS + cent and NS + perc
## It does so over two dimensions: score FSA and expenditure
## It does so by comparing means, creating a table, and running a regression (MAYBE NOT NEEDED)



# getting the data
cp <- df %>% 
  filter(treatment %in% c("pc_imp", "pc_exp"))

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
         treatment = fct_recode(treatment, "Implicit price" = "pc_imp",
                                "Explicit price" = "pc_exp"))
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

nice_table <- table %>% 
  mutate(across(starts_with("caddy"), ~round(.x, digits = 2))) %>% 
  mutate(caddy1 = paste(caddy1_mean, " (", caddy1_sd, ")", sep = ""),
         caddy2 = paste(caddy2_mean, " (", caddy2_sd, ")", sep = ""),
         "Δ" = paste(caddydiff_mean, " (", caddydiff_sd, ")", sep = "")) %>% 
  select(treatment, indicator, caddy1, caddy2, "Δ", p.value) %>% 
  pivot_wider(names_from = indicator, values_from = c(caddy1, caddy2, "Δ", p.value)) %>% 
  select(treatment, ends_with("FSA"), ends_with("ure"))

sink(file = "Tables/imp_exp_prices.tex")
nice_table %>% 
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
ggsave("Figures/imp_exp_presentation.png", width = 12, height = 5, units = "in", dpi = 300)

## simpler plot with errorbars
cp %>% 
  mutate(indicator = case_when(indicator == "scoreFSA" ~ "Δ scoreFSA",
                               indicator == "expenditure" ~ "Δ expenditure"),
         indicator = fct_relevel(indicator, "Δ scoreFSA")) %>% 
  ggplot(aes(treatment, diff, fill = indicator, color = treatment))+
  geom_hline(yintercept = 0, color = 'indianred', linetype = 'dashed', size = 0.2)+
  stat_summary(size = 1.2)+
  facet_grid(.~indicator, scales = "free")+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")+
  theme_ipsum_ps()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
  labs(x = "", y = "")
ggsave("Figures/tests_salience.png", width = 16/1.4, height = 8/1.4, units = "in", dpi = 300)

## testing one against the other
tests <- cp %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(wilcox.test(.$diff[.$treatment == "Implicit price"], .$diff[.$treatment == "Explicit price"]))) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  select(indicator, p.value)

sink("Tables/tests_salience.tex")
tests %>% 
  kbl(booktabs = T, 
      #col.names = c("", rep(c("cart 1", "cart 2", "difference", "p-value"),2)), 
      format = "latex", 
  ) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center")
sink()

## cleaning up
rm(cp)
rm(nice_table, table, tests)
