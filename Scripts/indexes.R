## Analysis of the Laspeyres and Paasche price indexes

## get a dataset where we focus on sepcific variables and treatments of interest
index <- df %>% 
  select(subject, caddy, treatment, income, income2, code, quantity, price, price_percent, price_cent, NS_shown, NS) %>% 
  filter(treatment %in% c("pc_exp", "pc_imp", "NS_ct_exp", "NS_pc_exp"))

## computing the tax and subvention for each item
index <- index %>% 
  mutate(paid_price = if_else(treatment == "NS_ct_exp", price_cent, price_percent),
         taxsub = if_else(treatment == "NS_ct_exp", quantity*(price - price_cent), quantity*(price - price_percent)), 
         expenditure = quantity*paid_price)

## Some preliminaries

## the Laspeyres and Paasche indexes compute differences in expenditure on periods 0 to 1
## of the bought items, with respect to a reference basket:
##    - in period 0 (Laspeyres), i.e. not taking into account any behavioral change;
##    - in period 1 (Paasche), i.e., considering the NEW shopping basked after change.

## preparing prices for caddy 1 and caddy 2
index <- index %>% 
  mutate(price_p1 = price, 
         price_p2 = if_else(treatment == "NS_ct_exp", price_cent, price_percent), 
         pq_1 = price_p1*quantity, 
         pq_2 = price_p2*quantity)


## given these prices, the Laspeyres index is simply the sum for caddy 1
## the paasche the sum for caddy 2
  
## computing by individual
Laspeyres <- index %>% 
  filter(caddy == 1) %>% 
  group_by(treatment, income2, subject) %>% 
  summarise(Laspeyres = sum(pq_2)/sum(pq_1)) %>% 
  arrange(treatment, income2)

Paasche <- index %>% 
  filter(caddy == 2) %>% 
  group_by(treatment, income2, subject) %>% 
  summarise(Paasche = sum(pq_2)/sum(pq_1)) %>% 
  arrange(treatment, income2)

## merging and applying beautiful treatment names
indexes <- Laspeyres %>% 
  left_join(Paasche, by = join_by(treatment, income2, subject)) %>% 
  mutate(treatment = case_when(treatment == "NS_pc_exp" ~ "NS + large price",
                               treatment == "NS_ct_exp" ~ "NS + small price",
                               treatment == "pc_imp"    ~ "Implicit price",
                               treatment == "pc_exp"    ~ "Explicit price"))



## Analyses

# 1: overall Laspeyres & Paasche by treatment

# means
indexes %>% 
  group_by(treatment) %>% 
  summarise(across(Laspeyres | Paasche, mean))

# tests: are indexes < 1? (all of them are)
indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  group_by(treatment, index) %>% 
  group_modify(~broom::tidy(t.test(.$value, mu = 1)))

# tests: is Paasche < Laspeyres? (in all cases it is)
indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  group_by(treatment) %>% 
  group_modify(~broom::tidy(t.test(.$value ~.$index)))

# plots
indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  ggplot(aes(value, treatment, color = index))+
  stat_summary(position = position_dodge(width = 0.2), size = 0.8)+
  geom_vline(xintercept = 1, color = "indianred")+
  scale_color_brewer(palette = "Set2", name = "")+
  labs(x = "Laspeyres and Paasche indexes -- mean and st.err. of the mean", y = "")+
  theme_ipsum_ps()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey70"),
        plot.background = element_rect(color = "white", fill = "white"),
        strip.text.x = element_text(face = "bold", size = 20),
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 14))
ggsave("Figures/indexes_aggregate.png",
       width = 13/1.5, height = 8/1.5, units = "in", dpi = 300)



# by income group

# means
indexes %>% 
  group_by(treatment, income2) %>% 
  summarise(across(Laspeyres | Paasche, mean))

# tests: are they lower than 1? (all of them are)
test_indexes_lower_1 <- indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  group_by(treatment, income2, index) %>% 
  group_modify(~broom::tidy(t.test(.$value, mu = 1))) %>% 
  mutate(p.value = round(p.value, 3))

# tests: is Paasche Lower than Laspeyres for eveyrone? (yes when in presence of NS; otherwise mostly not)
indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  group_by(treatment, income2) %>% 
  group_modify(~broom::tidy(t.test(.$value ~.$index)))

# tests: do different income levels show different indexes? (none of them are)
tests_different_income_group_indexes <- indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  group_by(treatment, index) %>% 
  group_modify(~broom::tidy(pairwise.t.test(.$value, .$income2)))


# plots
indexes %>% 
  pivot_longer(Laspeyres | Paasche, names_to = "index", values_to = "value") %>% 
  ggplot(aes(value, income2, color = index))+
  stat_summary(position = position_dodge(), size = 2)+
  geom_vline(xintercept = 1, color = "indianred")+
  scale_color_brewer(palette = "Set2", name = "")+
  labs(x = "Laspeyres and Paasche indexes -- mean and 95% c.i.", y = "")+
  theme_ipsum_ps()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey70"),
        plot.background = element_rect(color = "white", fill = "white"),
        strip.text.x = element_text(face = "bold", size = 20),
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 14))+
  facet_wrap(~treatment, scales = "free")
