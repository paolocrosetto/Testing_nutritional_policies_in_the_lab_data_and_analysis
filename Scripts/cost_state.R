## computes the cost for the state of the policies
## also by revenue category
## and makes some plots

dfcost <- df %>% 
  mutate(nettax = case_when(treatment == "NS" ~ 0,
                            treatment == "pc_imp" ~ price_percent - price,
                            treatment == "pc_exp" ~ price_percent - price,
                            treatment == "NS_ct_exp" ~ price_cent - price,
                            treatment == "NS_pc_exp" ~ price_percent - price)) %>% 
  filter(treatment != "NS2016" & treatment != "Neutre2016") %>% 
  mutate(nettax = -nettax) %>% 
  filter(caddy == 2) %>% 
  group_by(treatment, subject) %>% 
  select(subject, treatment, caddy, product, quantity, price, price_cent, price_percent, nettax) %>% 
  summarise(netindtax = round(sum(nettax),2)*365/2, 
            spent = sum(price)) %>% 
  group_by(treatment) %>% 
  summarise(m = round(mean(netindtax, na.rm = TRUE),1), 
            sd = sd(netindtax, na.rm = TRUE),
            sem = sd(netindtax, na.rm = TRUE)/sqrt(n())) %>% 
  mutate(cil = m-2*sem, cih = m + 2*sem) %>% 
  arrange(-m)

## extra figure used for presentation: annual cost per household
dfcost %>% 
  mutate(treatment = fct_recode(treatment, "NutriScore" = "NS",
                                "NS + large price" = "NS_pc_exp",
                                "NS + small price" = "NS_ct_exp",
                                "Implicit price" = "pc_imp",
                                "Explicit price" = "pc_exp")) %>% 
  mutate(treatment = fct_relevel(treatment, "NutriScore", "NS + small price", "NS + large price"),
         treatment = fct_rev(treatment)) %>% 
  ggplot(aes(reorder(treatment, m), m, fill = treatment))+
  geom_errorbar(aes(ymin = cil, ymax = cih), size=0.8, width = 0.05, position = position_dodge(width = 0.07), color = "grey60")+
  geom_point(pch = 21, size = 4)+
  geom_hline(aes(yintercept = 0), color = 'red', lty = "dashed")+
  # theme(axis.text.x = element_text(hjust = 1, angle = 90))+
  coord_flip()+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Annual cost per household", x = "", y = "Euros")+
  theme_ipsum_ps()+
  theme(panel.grid.minor = element_blank(), legend.position = "none")
ggsave("Figures/cost_per_household_color_presentation.png", width = 13/1.4, height = 8/1.4, units = "in", dpi = 300)


## cost and benefit

library(ggrepel)

## nut benefit
dfnut <- df %>% 
  filter(treatment != "Neutre2016" & treatment != "NS2016") %>% 
  group_by(treatment, subject, caddy) %>% 
  summarise(scoreFSA = (sum(FSAKcal))/sum(actual_Kcal)) %>% 
  pivot_longer(scoreFSA, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = caddy, values_from = value) %>% 
  mutate(diff = (`2`-`1`)) %>% 
  group_by(treatment) %>%
  summarise(sdnut = sd(diff, na.rm = TRUE),
            diffFSA = round(mean(diff, na.rm = TRUE),2))

## delta expenditure
dfexp <- df %>% 
  filter(treatment != "Neutre2016" & treatment != "NS2016") %>% 
  group_by(treatment, subject, caddy) %>% 
  summarise(expenditure = 2000*sum(actual_observedprice)/sum(actual_Kcal)) %>% 
  pivot_longer(expenditure, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = caddy, values_from = value) %>% 
  mutate(diff = (`2`-`1`)) %>% 
  mutate(diff = round(sum(diff),2)*365/2) %>% 
  group_by(treatment) %>%
  summarise(familygainsd = round(sd(diff, na.rm = TRUE),2),
            familygain = round(mean(diff, na.rm = TRUE),1)) %>% 
  mutate(fg = paste0(familygain, "€", " (", familygainsd, ")")) %>% 
  select(treatment, fg)


plotme <- dfnut %>% 
  left_join(dfcost) %>% 
  mutate(cost = m) %>% 
  left_join(dfexp)


## Table 5 -- household benefits and gov't cost of different policies

plotme %>% 
  mutate(treatment = fct_recode(treatment, "NutriScore" = "NS",
                                "NS + large price" = "NS_pc_exp",
                                "NS + small price" = "NS_ct_exp",
                                "Implicit price" = "pc_imp",
                                "Explicit price" = "pc_exp"), 
         treatment = fct_relevel(treatment, "NutriScore", "Implicit price", "Explicit price", "NS + large price", "NS + small price")) %>% 
  arrange(treatment) %>% 
  mutate(nutrition = paste0(diffFSA, " (", round(sdnut,2), ")"),
         cost = paste0(m, '€', " (", round(sd,2), ")")) %>% 
  select(treatment, nutrition, cost) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Label only", 1, 1) %>% 
  pack_rows("Price only", 2, 3) %>% 
  pack_rows("Policy mix", 4, 5) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, 
                     "Nutritional change" = 1,
                     "Yearly cost per household" = 1
                     )) %>% 
  add_header_above(c(" " = 1, 
                     "Household" = 1, 
                     "Government" = 1)) %>% 
  save_kable("Tables/Table_cost_benefit_with_HH.tex") 

## cleaning up
rm(dfcost, dfexp, dfnut, plotme)
