## analyzing Hypothesis 7: impact of price changes is regressive

## from Muller et al 2017 -- EJ 
## price interventions are regressive as lower revenue pay more taxes and get less subsidies

## get a dataset where we focus on sepcific variables and treatments of interest
focus <- df %>% 
  select(subject, caddy, treatment, income, income2, code, quantity, price, price_percent, price_cent, NS_shown, NS) %>% 
  filter(treatment %in% c("pc_exp", "pc_imp", "NS_ct_exp", "NS_pc_exp"))

## computing the tax and subvention for each item
focus <- focus %>% 
  mutate(paid_price = if_else(treatment == "NS_ct_exp", price_cent, price_percent),
         taxsub = if_else(treatment == "NS_ct_exp", quantity*(price - price_cent), quantity*(price - price_percent)), 
         expenditure = quantity*paid_price)


## summarising over treatments first: on average subjects do get a subsidy from the gov't
nice_table <- focus %>% 
  filter(caddy == 2) %>% 
  group_by(subject, treatment) %>% 
  summarise(taxsub = sum(taxsub), 
            expenditure = sum(expenditure), 
            sharetaxsub = taxsub/expenditure) %>% 
  arrange(expenditure) %>% 
  group_by(treatment) %>% 
  summarise(`Mean expenditure` = paste0(round(mean(expenditure),2), " €"),
            `Net subsidy` = paste0(round(mean(taxsub),2), " €"),
            `Share of expenditure` = paste0(round(mean(sharetaxsub)*100,2),"%")) %>% 
  mutate(treatment = case_when(treatment == "NS_pc_exp" ~ "NS + large price",
                               treatment == "NS_ct_exp" ~ "NS + small price",
                               treatment == "pc_imp"    ~ "Implicit price",
                               treatment == "pc_exp"    ~ "Explicit price"))

## export to tex

sink(file = "Tables/Table_A2_net_subsidy.tex")
subsidy_table <- nice_table %>% 
  ungroup() %>% 
  kbl(booktabs = T,
      format = "latex", 
      caption = "Mean expenditure and net subsidy by treatment", 
      align = "lrrr",
      label = "expenditure_net_subsidy"
  ) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") 
subsidy_table
sink()

## export to pdf
subsidy_table %>% 
  save_kable("Tables/Table_A2_net_sub.pdf")

## distribution of subjects by income
coarse_n <- focus %>% 
  select(subject, income = income2) %>% 
  distinct() %>% 
  count(income)

finegrained_n <- focus %>% 
  select(subject, income) %>% 
  distinct() %>% 
  count(income)

n_tab <- coarse_n %>% bind_rows(finegrained_n)

## income gradient of net subsidies: income
finegrained <- focus %>% 
  filter(caddy == 2) %>% 
  mutate(condition = case_when(treatment == "NS_ct_exp" ~ "small price change",
                               TRUE ~ "large price change")) %>% 
  group_by(subject, income = income, condition) %>% 
  summarise(taxsub = sum(taxsub), 
            expenditure = sum(expenditure), 
            sharetaxsub = taxsub/expenditure) %>% 
  arrange(expenditure) %>% 
  group_by(condition, income) %>% 
  summarise(`Mean expenditure` = mean(expenditure),
            `Net subsidy` = mean(taxsub),
            `Share of expenditure` = mean(sharetaxsub)*100)



## income gradient of net subsidies: income2
coarse <- focus %>% 
  filter(caddy == 2) %>% 
  mutate(condition = case_when(treatment == "NS_ct_exp" ~ "small price change",
                               TRUE ~ "large price change")) %>% 
  group_by(subject, income = income2, condition) %>% 
  summarise(taxsub = sum(taxsub), 
            expenditure = sum(expenditure), 
            sharetaxsub = taxsub/expenditure) %>% 
  arrange(expenditure) %>% 
  group_by(condition, income) %>% 
  summarise(`Mean expenditure` = mean(expenditure),
            `Net subsidy` = mean(taxsub),
            `Share of expenditure` = mean(sharetaxsub)*100)

## nice table

nice_table <- coarse %>% 
  bind_rows(finegrained) %>% 
  mutate(across(where(is.numeric), ~round(.x,2))) %>% 
  left_join(n_tab, join_by(income)) %>% 
  mutate(income = case_when(income == "lower"  ~  "< 2000", 
                            income == "middle" ~   "2000 - 3000",
                            income == 'higher' ~ ">3000",
                            income == "0_1000" ~ "< 1000", 
                            income == "1000_2000" ~ "1000-2000",
                            income == "2000_3000" ~ "2000-3000",
                            income == "3000_4000" ~ "3000-4000",
                            income == "4000_5000" ~ "4000-5000",
                            income == "5000_6000" ~ "5000-6000",
                            income == "6000_7000" ~ "6000-7000",
                            income == "8000_plus" ~ "> 8000",)) %>% 
  pivot_wider(names_from = condition, 
              values_from = c(`Mean expenditure`, `Net subsidy`, `Share of expenditure`)) %>% 
  select(income, n, 
         "Mean expenditure_small price change", "Net subsidy_small price change", "Share of expenditure_small price change", 
         "Mean expenditure_large price change", "Net subsidy_large price change", "Share of expenditure_large price change"
         )

## saving to file the latex code of the table
sink("Tables/Table_5_income_gradient.tex")
income_table <- nice_table %>% 
  kbl(booktabs = T, 
      col.names = c("", "N", rep(c("Expenditure (€)","Net subsidy (€)","Share (%)"),2)), 
      align = "lrrrrrrr",
      format = "latex", 
      caption = "Mean expenditure and net subsidy by income levels", 
      label = "income_graident") %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  add_header_above(c( " " = 2, "Small price change" = 3, "Large price change" = 3)) %>% 
  pack_rows("Coarse income level", 1, 3) %>% 
  pack_rows("Fine-grained income level", 4, 11)
income_table
sink()

## export same table to pdf
income_table %>% 
  save_kable("Tables/Table_5_income.pdf")


### tests

## coarse classification

coarsetest <- focus %>% 
  filter(caddy == 2) %>% 
  mutate(condition = case_when(treatment == "NS_ct_exp" ~ "small price change",
                               TRUE ~ "large price change")) %>% 
  group_by(subject, income = income2, condition) %>% 
  summarise(taxsub = sum(taxsub), 
            expenditure = sum(expenditure), 
            sharetaxsub = taxsub/expenditure)

## tests: expenditure
coarsetest %>% 
  group_by(condition) %>% 
  group_modify(~tidy(pairwise.t.test(.$expenditure, .$income)))

## tests: net subsidy
coarsetest %>% 
  group_by(condition) %>% 
  group_modify(~tidy(pairwise.t.test(.$taxsub, .$income)))

## tests: share
coarsetest %>% 
  group_by(condition) %>% 
  group_modify(~tidy(pairwise.t.test(.$sharetaxsub, .$income)))


## fine grained classification

finegrainedtest <- focus %>% 
  filter(caddy == 2) %>% 
  mutate(condition = case_when(treatment == "NS_ct_exp" ~ "small price change",
                               TRUE ~ "large price change")) %>% 
  group_by(subject, income = income, condition) %>% 
  summarise(taxsub = sum(taxsub), 
            expenditure = sum(expenditure), 
            sharetaxsub = taxsub/expenditure)

## tests: expenditure
# large
t.test(finegrainedtest$expenditure[finegrainedtest$condition == "large price change" & finegrainedtest$income %in% c("0_1000", "5000_6000")] ~
       finegrainedtest$income[finegrainedtest$condition == "large price change" & finegrainedtest$income %in% c("0_1000", "5000_6000")]) %>% 
  tidy()

# small
t.test(finegrainedtest$expenditure[finegrainedtest$condition == "small price change" & finegrainedtest$income %in% c("0_1000", "5000_6000")] ~
         finegrainedtest$income[finegrainedtest$condition == "small price change" & finegrainedtest$income %in% c("0_1000", "5000_6000")]) %>% 
  tidy()


## tests: net subsidy
pairwise.t.test(finegrainedtest$taxsub[finegrainedtest$condition == "large price change"], 
                finegrainedtest$income[finegrainedtest$condition == "large price change"], p.adjust.method = 'none')

finegrainedtest %>% 
  filter(income != "8000_plus") %>% # R does not like when running t-tests over groups that have 1 observation only, so dropping
  group_by(condition) %>% 
  group_modify(~tidy(pairwise.t.test(.$taxsub, .$income))) %>% 
  filter(p.value < 1)  # we must look at tests that have some menaingful p-value, so we look only at these

## tests: share
finegrainedtest %>% 
  filter(income != "8000_plus") %>% # R does not like when running t-tests over groups that have 1 observation only, so dropping
  group_by(condition) %>% 
  group_modify(~tidy(pairwise.t.test(.$sharetaxsub, .$income))) %>% 
  filter(p.value < 1)  # we must look at tests that have some menaingful p-value, so we look only at these

## cleaning up
rm(coarse, coarse_n, coarsetest)
rm(finegrained, finegrained_n, finegrainedtest)
rm(focus, n_tab, nice_table)
rm(subsidy_table, income_table)
