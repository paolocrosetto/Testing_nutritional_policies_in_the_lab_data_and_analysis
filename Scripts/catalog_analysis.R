
## generates plots and tables for the in-depth analysis of the catalog

# NS colors
NScolors <- c("#018241", "#86BC30", "#FFCC01", "#F18101", "#E73F12", "grey80")

products <- products %>% 
  mutate(realNS = if_else(NS_shown == 0, "None", NS))

## plot 0 (unused in the paper, used for presentations) 
## just a simple bar chart of count of products by NS category
counts <- products %>% 
  count(realNS)

counts %>% 
  ggplot(aes(realNS, n, fill = realNS))+
  geom_col()+
  scale_fill_manual(values = NScolors)+
  theme_ipsum_ps()+
  theme(legend.position = "none")+
  labs(x = "", y = "number of products")
ggsave("Figures/catalog_composition_simple.png", width = 10/1.2, height = 6/1.2, units = "in", dpi = 300)


## plot 1 (Figure 1): waffle plot of distribution of NS across catalog
## depends on libraries {waffle} and {ggwaffle}

products %>% 
  count(microcategory, realNS) %>% 
  ggplot(aes(x = microcategory, y = n, fill = realNS, values = n)) +
  geom_waffle(color = "white", size = .5, n_rows = 5, radius = unit(3, "pt"), flip = F, na.rm = T) +
  facet_wrap(~microcategory, ncol = 3, strip.position = "top") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values = NScolors)+
  labs(x = "", y = "") +
  theme_void(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "bottom", axis.text = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 16),
        strip.clip = "off",
        plot.background = element_rect(fill = "white", color = "white"))+
  guides(fill = guide_legend(nrow = 1))+
  coord_equal()
ggsave("Figures/catalog_composition_waffle.png", width = 10/1.2, height = 6/1.2, units = "in", dpi = 300)



## plot 2 (Figure 2): price change distribution

products %>% 
  rename(baseline = price, 
         cents = price_cent, 
         percent = price_percent) %>% 
  select(product, baseline, cents, percent) %>% 
  gather(key, value, -product) %>% 
  mutate(key = as_factor(key)) %>% 
  mutate(key = fct_recode(key, "Baseline" = "baseline",
                          "\u00B1 1 or 2 cents" = "cents",
                          "\u00B1 10% or 20%" = "percent")) %>% 
  ggplot(aes(value, key, fill = key))+
  geom_density_ridges(alpha = 0.3, scale = 3)+
  labs(y = "", x = "Price (€)",
       title = "" )+
  scale_fill_grey() +
  scale_y_discrete(expand = expand_scale(add = c(0.5,3)))+
  theme_ipsum_ps()+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("Figures/price_distribution_catalog.png", width = 11/1.2, height = 6/1.2, units = "in", dpi = 300)
  

## plot 3: average price changes by microcategory (39)
products %>% 
  group_by(category) %>% 
  summarise(orig = mean(price), orig_sd = sd(price),
            perc = mean(price_percent), perc_sd = sd(price_percent),
            cent = mean(price_cent), cent_sd = sd(price_cent)) %>% 
  mutate(signchange = perc>orig) %>% 
  mutate(signchange = as_factor(as.character(signchange)),
         signchange = fct_recode(signchange, "Price increase" = "TRUE", "Price decrease" = "FALSE")) %>% 
  ggplot(aes(reorder(category,orig), orig))+
  geom_segment(aes(xend = reorder(category,orig), y = orig, yend = perc, color = signchange), size = 1.7)+ 
  geom_point(shape = 21, fill = "grey", size = 2)+
  geom_point(aes(y = perc, shape = signchange, fill = signchange, color = signchange), size = 3)+
  scale_color_manual(name = "", values = c("#E73F12", "#018241"))+
  scale_fill_manual(name = "", values = c("#E73F12", "#018241"))+
  scale_shape_manual(values = c(24, 25))+
  labs(x = "", y = "Price (€)")+
  theme_ipsum_ps()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.position = "none")
ggsave("Figures/price_change_by_category_perc.png", width = 12/1.3, height = 9/1.3, units = "in", dpi = 300)

# average price changes by macrocategory (7)
products %>% 
  group_by(microcategory) %>% 
  summarise(orig = mean(price), orig_sd = sd(price),
            perc = mean(price_percent), perc_sd = sd(price_percent),
            cent = mean(price_cent), cent_sd = sd(price_cent)) %>% 
  mutate(signchange = perc>orig) %>% 
  mutate(signchange = as_factor(as.character(signchange)),
         signchange = fct_recode(signchange, "\u25C0" = "FALSE", "\u25B6" = "TRUE")) %>% 
  ggplot(aes(reorder(microcategory,orig), orig))+
  geom_segment(aes(xend = reorder(microcategory,orig), y = orig, yend = perc, color = signchange), size = 1.3)+ 
  geom_point(shape = 21, fill = "grey", size = 2)+
  geom_text(aes(y = perc, label = signchange, color = signchange), size = 4)+
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(name = "", values = c("#E73F12", "#018241"))+
  scale_fill_manual(name = "", values = c("#E73F12", "#018241"))+
  scale_y_continuous(limits = c(1.4,2.4), breaks = seq(1.4,2.4,0.1))+
  theme(panel.grid.minor.x = element_blank())+
  coord_flip()+
  labs(x = "", y = "Average product price")+
  theme_ipsum_ps()+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.position = "none")
#ggsave("Figures/price_change_by_MACRO_category_perc.png", width = 12/1.3, height = 9/1.3, units = "in", dpi = 200)


# price distribution: table
prod2 <-  products %>% 
  rename(baseline = price, 
         cents = price_cent, 
         percent = price_percent) 

tests <- prod2 %>% 
  select(baseline, cents, percent) %>% 
  gather() %>% 
  kruskal.test(value~key, data = .) %>% 
  tidy() %>% 
  select(p.value)
  
  
nicetab <- prod2 %>% 
  select(baseline, cents, percent) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = round(mean(value),3), sd = round(sd(value),3)) %>% 
  bind_cols(tests)

sink("Tables/price_catalog.tex")
nicetab %>% 
  mutate(price = paste0(round(mean,2), " (", round(sd,2), ")")) %>% 
  select(key, price, p.value) %>% 
  kbl(booktabs = T, 
      col.names = c("", "price", "p-value"), 
      format = "latex",
      caption = "Impact of rpice policies on catalog prices", 
      label = "price_catalog"
  ) %>% 
  kable_styling(latex_options = c("hold_position"),
                position = "center")
sink()

## cleaning up
rm(counts)
rm(prod2)
rm(tests)
rm(nicetab)

