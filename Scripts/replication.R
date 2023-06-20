## This file checks that the results of NUtriScore 2016 (Crosetto et al, ERAE 2019) replicate
##
## It compares the results of the treatment NutriScore of 2016 (N = 117) with those of 2019 (N = 71)
## It does so over two dimensions: score FSA and expenditure
## It does so by comparing means, creating a table, and running a regression (MAYBE NOT NEEDED)


# getting the data
cp <- df %>% 
  filter(treatment %in% c("NS", "NS2016"))

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
         treatment = fct_recode(treatment, "NutriScore 2016" = "NS2016", "NutriScore 2019" = "NS"))

# 1. tests
tests <- cp %>% 
  select(treatment, subject, indicator, diff) %>% 
  mutate(year = if_else(treatment == "NutriScore 2019", "2019", "2016")) %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(t.test(.$diff~.$year))) %>% 
  select(indicator, statistic, p.value)

# 2. table
table <- cp %>% 
  group_by(treatment, indicator) %>% 
  rename("caddy1" = `1`, "caddy2" = `2`, "caddydiff" = "diff") %>% 
  summarise(across(starts_with("caddy"), list(mean = mean, sd = sd))) %>% 
  left_join(tests, by = c("indicator")) 

# 3. table formatting
nice_table <- table %>% 
  mutate(across(starts_with("caddy"), ~round(.x, digits = 2))) %>% 
  mutate(caddy1 = paste(caddy1_mean, " (", caddy1_sd, ")", sep = ""),
         caddy2 = paste(caddy2_mean, " (", caddy2_sd, ")", sep = ""),
         "Δ" = paste(caddydiff_mean, " (", caddydiff_sd, ")", sep = "")) %>% 
  select(treatment, indicator, caddy1, caddy2, "Δ", p.value) %>% 
  pivot_wider(names_from = indicator, values_from = c(caddy1, caddy2, "Δ", p.value))  %>% 
  select(treatment, ends_with("FSA"), ends_with("ure"))

# 4. latex output: Table 5
sink(file = "Tables/tab_replication.tex")
nice_table %>% 
  mutate(across(starts_with("p.val"), ~round(.x, 3))) %>% 
  kbl(booktabs = T, 
      col.names = c("", rep(c("cart 1", "cart 2", "difference", "p-value"),2)), 
      format = "latex", 
      caption = "Overview of nutritional and economic results", 
      label = "replication"
  ) %>% 
  add_header_above(c(" ", "ScoreFSA" = 4, "Expediture" = 4)) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  footnote(general = "Means (standard deviations) for each variable. P-values from Wilcoxon rank-sum tests of the differece in differences", 
           general_title = "")
sink()

# 5. test of differences in caddy 1 in FSA and expenditure
cp %>% 
  group_by(indicator) %>% 
  group_modify(~tidy(t.test(`1`~treatment, data = .)))

# plot for presentation 
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
ggsave("Figures/extra_figure_replication.png", width = 12, height = 5, units = "in", dpi = 300)
  

## cleaning up
rm(cp)
rm(nice_table)
rm(table)
rm(tests)