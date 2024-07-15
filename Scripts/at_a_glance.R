## This file generates the main results of the paper
## Relative to two indicators:
##   1. difference in score FSA across shopping carts by treatment
##   2. difference in expenditure across shopping carts by treatment
##
## The results are then displayed in: 
##  - a summary Table (mean, sd, p-value of a test)
##  - a comprehensive raincloud plot (distribution, jitter, boxplot)

## Computing the difference in the target variables by subject and treatment
glance <- df %>% 
  # filter(treatment != "Neutre2016" & treatment != "NS2016") %>% 
  group_by(treatment, subject, caddy) %>% 
  summarise(scoreFSA = (sum(FSAKcal))/sum(actual_Kcal),
            expenditure = 2000*sum(actual_observedprice)/sum(actual_Kcal)) %>% 
  pivot_longer(scoreFSA:expenditure, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = caddy, values_from = value) %>% 
  mutate(diff = (`2`-`1`)) 

## renaming the treatments for plotting (possibly: move this up so that it applies everywhere)
glance <- glance %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_recode(treatment, 
                                "NutriScore 2016" = "NS2016",
                                "Benchmark 2016" = "Neutre2016",
                                "NutriScore" = "NS",
                                "NS + large price" = "NS_pc_exp",
                                "NS + small price" = "NS_ct_exp",
                                "Implicit price" = "pc_imp",
                                "Explicit price" = "pc_exp")) %>% 
  mutate(treatment = fct_relevel(treatment, "Benchmark 2016","NutriScore 2016", "NutriScore", "NS + small price", "NS + large price"),
         treatment = fct_rev(treatment))

## creating variables for faceting and layout
glance <- glance %>% 
  mutate(policy = case_when(treatment == "NutriScore" ~ "Label",
                            treatment %in% c("NS + large price","NS + small price") ~ "Policy mix",
                            treatment %in% c("Implicit price", "Explicit price") ~ "Price",
                            treatment %in% c("NutriScore 2016","Benchmark 2016") ~ "Benchmarks")) %>% 
  mutate(policy = as.factor(policy), 
         policy = fct_relevel(policy, "Policy mix", "Price", "Label")) %>% 
  mutate(indicator = case_when(indicator == "scoreFSA" ~ "Δ scoreFSA",
                               indicator == "expenditure" ~ "Δ expenditure"),
         indicator = as.factor(indicator),
         indicator = fct_relevel(indicator, "Δ scoreFSA"))

  
## Table

# 1. computing wilcoxon signed-rank tests 
tests <- glance %>% 
  select(-diff, -policy) %>% 
  group_by(treatment, indicator) %>% 
  pivot_longer(`1`:`2`, names_to = "caddy", values_to = "value") %>% 
  group_modify(~tidy(wilcox.test(.$value~.$caddy, paired = T))) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  select(treatment, indicator, p.value)

# 2. computing means, st.devs, and pasting the p.values from tests
table <- glance %>% 
  group_by(treatment, indicator) %>% 
  rename("caddy1" = `1`, "caddy2" = `2`, "caddydiff" = "diff") %>% 
  summarise(across(starts_with("caddy"), list(mean = mean, sd = sd))) %>% 
  left_join(tests, by = c("treatment", "indicator")) 

# 3. table formatting 
nice_table <- table %>% 
  mutate(across(starts_with("caddy"), ~round(.x, digits = 2))) %>% 
  mutate(caddy1 = paste(caddy1_mean, " (", caddy1_sd, ")", sep = ""),
         caddy2 = paste(caddy2_mean, " (", caddy2_sd, ")", sep = ""),
         "Δ" = paste(caddydiff_mean, " (", caddydiff_sd, ")", sep = "")) %>% 
  select(treatment, indicator, caddy1, caddy2, "Δ", p.value) %>% 
  pivot_wider(names_from = indicator, values_from = c(caddy1, caddy2, "Δ", p.value)) %>% 
  select(treatment, ends_with("FSA"), ends_with("ure"))

# 3b. exporting table as csv
nice_table %>% 
  write_csv("Tables/Table_B1_descriptive_stat_changes.csv")
  
# 4. latex and pdf final formatting and export

export_table <- nice_table %>% 
  mutate(treatment = fct_relevel(treatment, "Benchmark 2016", "NutriScore 2016", "NutriScore", "NS + small price", "NS + large price")) %>% 
  mutate("p.value_Δ expenditure" = case_when(`p.value_Δ expenditure` == 0.000 ~ "< 0.001",
                                             TRUE ~ as.character(`p.value_Δ expenditure`)),
         "p.value_Δ scoreFSA" = case_when(`p.value_Δ scoreFSA` == 0.000 ~ "< 0.001",
                                             TRUE ~ as.character(`p.value_Δ scoreFSA`))) %>% 
  arrange(treatment) %>% 
  kbl(booktabs = T, 
      col.names = c("", rep(c("cart 1", "cart 2", "difference", "p-value"),2)), 
      format = "latex", 
      caption = "Overview of nutritional and economic results", 
      label = "ataglance"
      ) %>% 
  add_header_above(c(" ", "ScoreFSA" = 4, "Expenditure" = 4)) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  pack_rows("Benchmarks", 1, 2) %>% 
  pack_rows("Labeling", 3, 3) %>%
  pack_rows("Policy mix", 4, 5) %>%
  pack_rows("Price policy", 6, 7) %>% 
  footnote(general = "Means (standard deviations) for each variable. P-values from Wilcoxon signed-rank test of the difference between carts 1 and 2.", 
           general_title = "")

export_table %>% 
  save_kable("Tables/Table_B1_descriptive_stat_changes.pdf")

sink(file = "Tables/Table_B1_descriptive_stat_changes.tex")
export_table
sink()
  


### PLOT

## Figure 4: simple plot of the coefficients and conf int from the regression 
## export for plot
summary_FSA <- reg_FSA %>% 
  tidy(conf.int = T) %>% 
  mutate(indicator = "ScoreFSA") %>% 
  filter(str_detect(term, "caddy2")) 

summary_exp <- reg_exp %>% 
  tidy(conf.int = T) %>% 
  mutate(indicator = "Expenditure (€/2000kcal)") %>% 
  filter(str_detect(term, "caddy2"))

plotme <- summary_FSA %>% 
  bind_rows(summary_exp) %>% 
  mutate(term = as.factor(term)) %>% 
  mutate(term = fct_recode(term, 
                                "NutriScore 2016" = "caddy2:treatmentNS2016",
                                "Benchmark 2016" = "caddy2",
                                "NutriScore 2019" = "caddy2:treatmentNutriScore",
                                "NS + large price" = "caddy2:treatmentNS + large price",
                                "NS + small price" = "caddy2:treatmentNS + small price",
                                "Implicit price" = "caddy2:treatmentImplicit price",
                                "Explicit price" = "caddy2:treatmentExplicit price")) %>% 
  mutate(term = fct_relevel(term, "Benchmark 2016","NutriScore 2016", "NutriScore", "NS + small price", "NS + large price"),
         term = fct_rev(term)) %>% 
  mutate(indicator = as.factor(indicator), 
         indicator = fct_relevel(indicator, "ScoreFSA")) %>% 
  mutate(policy = case_when(term == "NutriScore 2019" ~ "Label",
                            term %in% c("NS + large price","NS + small price") ~ "Policy mix",
                            term %in% c("Implicit price", "Explicit price") ~ "Price",
                            term %in% c("NutriScore 2016","Benchmark 2016") ~ "Benchmarks")) %>% 
  mutate(policy = as.factor(policy), 
         policy = fct_relevel(policy, "Policy mix", "Price", "Label"),
         policy = fct_rev(policy))


## plot for V1 of the paper
plotme %>% 
  ggplot() + 
  geom_errorbar(aes(x = reorder(term, estimate), ymin = conf.low, ymax = conf.high,
                    group= reorder(term,estimate)), size=0.6, width = 0.1, 
                position = position_dodge(width = 0.07), color = "grey30")+
  geom_point(aes(reorder(term, estimate), estimate, fill = indicator), size=5, pch=21)+
  coord_flip()+
  geom_hline(yintercept = 0, color="indianred", linetype = "dashed")+
  labs(x = "", y = "Policy-induced change -- mean and 95% c.i.")+
  theme_ipsum_ps()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey70"),
        plot.background = element_rect(color = "white", fill = "white"),
        strip.text.x = element_text(face = "bold", size = 20),
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 14))+
  facet_grid(policy~indicator, scales = "free", space = "free_y")

## saving
ggsave("Figures/at_a_glance_means_CI_working_paper.png",
       width = 13/1.1, height = 8/1.1, units = "in", dpi = 300)

#" version for revised paper for RR @JEBo
ScoreFSA <- plotme %>% 
  filter(indicator == "ScoreFSA") %>% 
  ggplot() + 
  geom_errorbar(aes(x = reorder(term, estimate), ymin = conf.low, ymax = conf.high,
                    group= reorder(term,estimate)), size=0.6, width = 0.1, 
                position = position_dodge(width = 0.07), color = "grey30")+
  geom_point(aes(reorder(term, estimate), estimate), fill="#f8766d", size=5, pch=21)+
  coord_flip()+
  geom_hline(yintercept = 0, color="indianred", linetype = "dashed")+
  labs(x = "", y = "ΔScoreFSA -- cart 2 vs cart 1")+
  theme_ipsum_ps()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey70"),
        plot.background = element_rect(color = "white", fill = "white"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 10, hjust = 0.5))+
  facet_grid(policy~indicator, scales = "free", space = "free_y")

Expenditure <- plotme %>% 
  filter(indicator == "Expenditure (€/2000kcal)") %>% 
  ggplot() + 
  geom_errorbar(aes(x = reorder(term, estimate), ymin = conf.low, ymax = conf.high,
                    group= reorder(term,estimate)), size=0.6, width = 0.1, 
                position = position_dodge(width = 0.07), color = "grey30")+
  geom_point(aes(reorder(term, estimate), estimate), fill = "#00bec3", size=5, pch=21)+
  coord_flip()+
  geom_hline(yintercept = 0, color="indianred", linetype = "dashed")+
  labs(x = "", y = "ΔExpenditure/2000Kcal, € -- cart 2 vs cart 1")+
  theme_ipsum_ps()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey70"),
        plot.background = element_rect(color = "white", fill = "white"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, hjust = 0.5))+
  facet_grid(policy~indicator, scales = "free", space = "free_y")

ScoreFSA + Expenditure

## saving
ggsave("Figures/at_a_glance_means_CI_revision.png",
       width = 13/1.1, height = 8/1.1, units = "in", dpi = 300)


## Appendix: raincloud plot with all observations

glance %>% 
  mutate(policy=fct_rev(policy)) %>% 
  ggplot(aes(treatment, diff, fill = treatment)) +
  geom_boxplot(alpha = 0.01, width = 0.3, outlier.alpha = 0, color = "grey40")+
  geom_quasirandom(width = 0.3, alpha = 0.7, size = 2.5, color = "white", shape = 21)+
  geom_hline(yintercept = 0, color = 'indianred', linetype = 'dashed')+
  #geom_half_violin(side = "r", position = position_nudge(x = 0.15), color = "grey30", size = 0.1)+
  labs(x = "", y = "")+
  coord_flip()+
  scale_fill_manual(values = c(rev(RColorBrewer::brewer.pal(6, "Set1")), "grey"))+
  facet_grid(policy~indicator, scales = "free", space = "free")+
  theme_ipsum_ps()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 15, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("Figures/at_a_glance_raincloud.png",
       width = 12/1.1, height = 9/1.1, units = "in", dpi = 300)


## cleaning up
rm(glance)
rm(nice_table)
rm(summary)
rm(table)
rm(tests)