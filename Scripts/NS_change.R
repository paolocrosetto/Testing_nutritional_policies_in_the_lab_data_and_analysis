## looking at changes in number of NS-category products by treatment

## data we need
NSstats <- df %>% 
  mutate(realNS = if_else(NS_shown == 0, "None", NS),
         realNS = if_else(NS_shown == "N", "None", realNS)) %>% 
  select(subject, treatment, caddy, NS = realNS)

## counting and making averages by subject
table <- NSstats %>% 
  group_by(subject, treatment, caddy, NS) %>% 
  tally() %>% 
  pivot_wider(names_from = caddy, values_from = n, names_prefix = "caddy", values_fill = 0) %>% 
  mutate(diff = caddy2 - caddy1) %>% 
  group_by(treatment, NS) %>% 
  summarise(mean_diff = mean(diff)) 

## putting results in a nice-looking table
nice_table <- table %>% 
  mutate(mean_diff = round(mean_diff, 2)) %>% 
  pivot_wider(names_from = NS, values_from = mean_diff) %>% 
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
         treatment = fct_rev(treatment)) %>% 
  arrange(desc(treatment))
  

## exporting into a clean latex and pdf tables

export_table <- nice_table %>% 
  kbl(booktabs = T, 
      col.names = c("", "A", "B", "C", "D", "E", "None"), 
      align = "lrrrrrr",
      format = "latex", 
      caption = "Mean difference in  number of products, caddy 2 vs caddy 1, by treatment and NutriScore category", 
      label = "change_n_prod_NS") %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  add_header_above(c( " " = 1, "NutriScore category" = 6)) %>% 
  pack_rows("Benchmarks", 1, 2) %>% 
  pack_rows("Labeling", 3, 3) %>%
  pack_rows("Policy mix", 4, 5) %>%
  pack_rows("Price policy", 6, 7) 

export_table %>% 
  save_kable("Tables/Table_7_change_NS.pdf")

sink("Tables/Table_7_change_NS.tex")
export_table
sink()

## tests
NStests <- NSstats %>% 
  group_by(subject, treatment, caddy, NS) %>% 
  tally() %>% 
  group_by(treatment, NS) %>% 
  group_modify(~tidy(t.test(.$n~.$caddy)))
