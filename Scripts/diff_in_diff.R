### generate the clean dataset to run the estimation

glance <- df %>% 
  group_by(treatment, subject, income2, caddy) %>% 
  summarise(scoreFSA = (sum(FSAKcal))/sum(actual_Kcal),
            expenditure = 2000*sum(actual_observedprice)/sum(actual_Kcal)) %>% 
  pivot_longer(scoreFSA:expenditure, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(names_from = caddy, values_from = value) %>% 
  mutate(diff = (`2`-`1`)) 

## renaming the treatments for plotting (possibly: move this up so that it applies everywhere)
glance <- glance %>% 
  mutate(treatment = as.factor(treatment),
         treatment = fct_recode(treatment, "NutriScore" = "NS",
                                "NS + large price" = "NS_pc_exp",
                                "NS + small price" = "NS_ct_exp",
                                "Implicit price" = "pc_imp",
                                "Explicit price" = "pc_exp")) %>% 
  mutate(treatment = fct_relevel(treatment, "NutriScore", "NS + small price", "NS + large price"),
         treatment = fct_rev(treatment))

## creating variables for faceting and layout
glance <- glance %>% 
  mutate(policy = case_when(treatment == "NutriScore" ~ "Labeling",
                            treatment %in% c("NS + large price","NS + small price") ~ "Policy mix",
                            treatment %in% c("Implicit price", "Explicit price") ~ "Price policy"),
         policy = as.factor(policy)) %>% 
  mutate(indicator = case_when(indicator == "scoreFSA" ~ "Δ scoreFSA",
                               indicator == "expenditure" ~ "Δ expenditure"),
         indicator = fct_relevel(indicator, "Δ scoreFSA"))



## NUTRITIONAL VALUES

## subsetting to scoreFSA only
regdf <- glance %>% 
  filter(indicator == "Δ scoreFSA") %>% 
  pivot_longer(cols = `1` | `2`, names_to = "caddy", values_to = "value")

# regression
reg_FSA <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = 3)) %>% 
  feols(value~caddy*treatment, cluster = "subject" ) 

## NUTRITION

# 1. 2016 vs 2019
car::linearHypothesis(reg_FSA, "caddy2:treatmentNS2016 - caddy2:treatmentNutriScore = 0")

# 2. implicit vs explicit
car::linearHypothesis(reg_FSA, "caddy2:treatmentImplicit price - caddy2:treatmentExplicit price = 0")

# 3. labels vs explicit prices
car::linearHypothesis(reg_FSA, "caddy2:treatmentExplicit price - caddy2:treatmentNutriScore = 0")

# 4. labels vs implicit prices
car::linearHypothesis(reg_FSA, "caddy2:treatmentImplicit price - caddy2:treatmentNutriScore = 0")

# 5. labels vs label + large prices
car::linearHypothesis(reg_FSA, "caddy2:treatmentNutriScore - caddy2:treatmentNS + large price = 0")

# 6. labels vs label + small prices
car::linearHypothesis(reg_FSA, "caddy2:treatmentNutriScore - caddy2:treatmentNS + small price = 0")

# 7. small vs large prices
car::linearHypothesis(reg_FSA, "caddy2:treatmentNS + small price - caddy2:treatmentNS + large price = 0")



## EXPENDITURE

## subsetting to Expenditure only
regdf <- glance %>% 
  filter(indicator == "Δ expenditure") %>% 
  pivot_longer(cols = `1` | `2`, names_to = "caddy", values_to = "value")

# regression
reg_exp <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = 3)) %>% 
  feols(value~caddy*treatment, cluster = "subject") 

## A. in absence of labels

# 1. implicit vs benchmark
car::linearHypothesis(reg_exp, "caddy2 - caddy2:treatmentImplicit price = 0")

# 2. explicit vs benchmark
car::linearHypothesis(reg_exp, "caddy2 - caddy2:treatmentExplicit price = 0")

# B. in presence of labels

# 0. NS vs benchmark to show the impact of labels
car::linearHypothesis(reg_exp, "caddy2:treatmentNutriScore - caddy2 = 0")

# 1. NS + small vs NS
car::linearHypothesis(reg_exp, "caddy2:treatmentNutriScore - caddy2:treatmentNS + small price = 0")

# 2. NS + large vs NS
car::linearHypothesis(reg_exp, "caddy2:treatmentNutriScore - caddy2:treatmentNS + large price = 0")



## export regressions to latex

modelsummary(list("ScoreFSA" = reg_FSA, 
                                "Expenditure" = reg_exp),
                           fmt = "%.3f",
                           estimate = "{estimate} ({std.error}){stars}",
                           statistic = NULL,
                           coef_rename = c("caddy2" = "Cart 2: Benchmark", 
                                           "(Intercept)" = "Cart 1: Benchmark",
                                           "treatmentNS2016" = "Cart 1 $\\times$ NutriScore 2016",
                                           "treatmentExplicit price" = "Cart 1 $\\times$ Explicit large price change",
                                           "treatmentImplicit price" = "Cart 1 $\\times$ Implicit large price change",
                                           "treatmentNS + large price" = "Cart 1 $\\times$ NutriScore and large price change",
                                           "treatmentNS + small price" = "Cart 1 $\\times$ NutriScore and small price change",
                                           "treatmentNutriScore" = "Cart 1 $\\times$ NutriScore",
                                           "caddy2:treatmentNS2016" = "Cart 2 $\\times$ NutriScore 2016",
                                           "caddy2:treatmentExplicit price" = "Cart 2 $\\times$ Explicit large price change",
                                           "caddy2:treatmentImplicit price" = "Cart 2 $\\times$ Implicit large price change",
                                           "caddy2:treatmentNS + large price" = "Cart 2 $\\times$ NutriScore and large price change",
                                           "caddy2:treatmentNS + small price" = "Cart 2 $\\times$ NutriScore and small price change",
                                           "caddy2:treatmentNutriScore" = "Cart 2 $\\times$ NutriScore"),
                           output = "Tables/regression_table.tex",
                           title = "Difference-in-difference fixed-effect regression results. Standard error clustered by subject.")



