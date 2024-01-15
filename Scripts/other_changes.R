## looking at other things that change across baskets

## 1. mean price per item and mean number of items
##
## question: do subjects use the subsidy to buy more expensive items or more items?

changes <- df %>% 
  select(subject, treatment, caddy, quantity, price) %>% 
  mutate(totprice = price*quantity) %>% 
  filter(quantity > 0) %>% 
  ## compute the average price per item and the average N products before and after
  group_by(subject, treatment, caddy) %>% 
  summarise(Nitem = sum(quantity), 
            avgprice = mean(price))

# nice table
nice_table <- changes %>% 
  pivot_wider(names_from = caddy, values_from = c(Nitem, avgprice), values_fill = 0) %>% 
  mutate(subject = as.character(subject)) %>% 
  mutate(diffN = Nitem_2 - Nitem_1, 
         diffp = avgprice_2 - avgprice_1) %>% 
  group_by(treatment) %>% 
  summarise(across(where(is.numeric), ~round(mean(.x), 2))) %>% 
  select(treatment, N1 = Nitem_1, N2 = Nitem_2, diffN, p1 = avgprice_1, p2 = avgprice_2, diffp)

# exporting to latex and pdf
export_table <- nice_table %>% 
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
  arrange(desc(treatment)) %>% 
  kbl(booktabs = T, 
      col.names = c("", rep(c("Basket 1", "Basket 2", "$\\Delta$"),2)), 
      align = "lrrrrrr",
      format = "latex", 
      caption = "Policy-induced changes in quantity and mean price", 
      label = "change_n_prod_NS") %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>% 
  add_header_above(c( " " = 1, "Number of items" = 3, "Average price per item" = 3)) %>% 
  pack_rows("Benchmarks", 1, 2) %>% 
  pack_rows("Labeling", 3, 3) %>%
  pack_rows("Policy mix", 4, 5) %>%
  pack_rows("Price policy", 6, 7) 

export_table %>% 
  save_kable("Tables/Table_7_price_changes.pdf")

sink("Tables/Table_7_price_changes.tex")
export_table
sink()


# tests
changes %>% 
  pivot_longer(c("Nitem", "avgprice"), names_to = "indicator", values_to = "values") %>% 
  group_by(treatment, indicator) %>% 
  group_modify(~tidy(t.test(.$values~ .$caddy)))
  
