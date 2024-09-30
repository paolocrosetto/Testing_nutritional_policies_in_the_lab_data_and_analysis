### this script tries a different approach to the estimation
##
## instead of creating the indicator by subject/caddy before regressiong
## we regress the individual choices
## this should allow us to have full random effects as N is way larger

### cleaning individual choices for estimation

glance <- df %>% 
  group_by(treatment, subject, age, female, income2, caddy) %>% 
  mutate(scoreFSA = FSAKcal/actual_Kcal,
         expenditure = 2000*actual_observedprice/actual_Kcal) %>% 
  select(subject, treatment, scoreFSA, expenditure, caddy, age, female, profession, education, income, income2) %>% 
  group_by(subject) %>% 
  pivot_longer(scoreFSA:expenditure, names_to = "indicator", values_to = "value")


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
  mutate(caddy = as.factor(caddy))


regdf %>%
  group_by(subject, caddy) %>%
  summarise(mean_value = mean(value)) %>%
  spread(caddy, mean_value) %>%
  mutate(diff = `2` - `1`) %>% 
  ggplot(aes(diff))+
  geom_histogram()
ggsave("forchatgpt.png")
# Fixed effect model clustered at the subject level
fixeff_FSA <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  feols(value~caddy*treatment, cluster = "subject" ) 


# Random effects model
randeff_FSA <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  lmer(value ~ caddy * treatment + (caddy | subject), data = .)

# vcov clustered at the subject level (the vcovCR automatically detects subject as the cluster)
varcov_reFSA <- vcovCR(randeff_FSA, type = "CR0")

# Random effects model with controls
randeff_FSA_controls <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  lmer(value ~ caddy * treatment + female + age + (caddy | subject), data = .)

# vcov clustered at the subject level (the vcovCR automatically detects subject as the cluster)
varcov_reFSA_controls <- vcovCR(randeff_FSA, type = "CR0")


## EXPENDITURE

## subsetting to Expenditure only
regdf <- glance %>% 
  filter(indicator == "Δ expenditure") %>% 
  mutate(caddy = as.factor(caddy))


# Fixed effect model clustered at the subject level
fixeff_EXP <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  feols(value~caddy*treatment, cluster = "subject" ) 


# Random effects model
randeff_EXP <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  lmer(value ~ caddy * treatment + (caddy | subject), data = .)

# vcov clustered at the subject level (the vcovCR automatically detects subject as the cluster)
varcov_reEXP <- vcovCR(randeff_EXP, type = "CR0")

# Random effects model with controls
randeff_EXP_controls <- regdf %>% 
  mutate(treatment = relevel(treatment, ref = "Neutre2016")) %>% 
  lmer(value ~ caddy * treatment + female + age + (caddy | subject), data = .)

# vcov clustered at the subject level (the vcovCR automatically detects subject as the cluster)
varcov_reEXP_controls <- vcovCR(randeff_EXP, type = "CR0")


## export regressions to latex:
## ScoreFSA

modelsummary(list("Fixed effects" = reg_FSA, 
                  "Random intercept" = reg_FSA_raneff,
                  "Random intercept controls" = reg_FSA_raneff_controls,
                  "Fixed effects" = fixeff_FSA, 
                  "Random effects" = randeff_FSA, 
                  "Random effects controls" = randeff_FSA_controls
                  ),
             fmt = "%.3f",
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             vcov = list(as.matrix(vcov(reg_FSA)), 
                         as.matrix(vcovCR(reg_FSA_raneff, type = "CR0")), 
                         as.matrix(vcovCR(reg_FSA_raneff_controls, type = "CR0")), 
                         as.matrix(vcov(fixeff_FSA)), 
                         as.matrix(vcovCR(randeff_FSA, type = "CR0")), 
                         as.matrix(vcovCR(randeff_FSA_controls, type = "CR0"))
                         ),
             coef_rename = c("caddy2" = "Cart 2", 
                             "(Intercept)" = "Intercept",
                             "treatmentNS2016" = "NutriScore 2016",
                             "treatmentExplicit price" = "Explicit large price change",
                             "treatmentImplicit price" = "Implicit large price change",
                             "treatmentNS + large price" = "NutriScore and large price change",
                             "treatmentNS + small price" = "NutriScore and small price change",
                             "treatmentNutriScore" = "NutriScore 2019",
                             "caddy2:treatmentNS2016" = "Cart 2 $\\times$ NutriScore 2016",
                             "caddy2:treatmentExplicit price" = "Cart 2 $\\times$ Explicit large price change",
                             "caddy2:treatmentImplicit price" = "Cart 2 $\\times$ Implicit large price change",
                             "caddy2:treatmentNS + large price" = "Cart 2 $\\times$ NutriScore and large price change",
                             "caddy2:treatmentNS + small price" = "Cart 2 $\\times$ NutriScore and small price change",
                             "caddy2:treatmentNutriScore" = "Cart 2 $\\times$ NutriScore 2019"),
             #output = "Tables/Table_4_regression.tex",
             output = "kableExtra",
             title = "Difference-in-difference fixed-effect regression results. Standard error clustered by subject.") %>% 
  add_header_above(c(" " = 1, "Subject indicators" = 3, "Raw shopping data" = 3)) %>%           
  add_header_above(c(" " = 1, "ScoreFSA" = 6))




## Expenditure

modelsummary(list("Fixed effects means" = reg_exp, 
                  "Random intercept means" = reg_EXP_raneff,
                  "Random intercept controls" = reg_EXP_raneff_controls,
                  "Fixed effects raw" = fixeff_EXP,
                  "Random effects raw" = randeff_EXP, 
                  "Random effects raw controls" = randeff_EXP_controls),
             fmt = "%.3f",
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             vcov = list(as.matrix(vcov(reg_exp)), 
                         as.matrix(vcovCR(reg_EXP_raneff, type = "CR0")), 
                         as.matrix(vcovCR(reg_EXP_raneff_controls, type = "CR0")), 
                         as.matrix(vcov(fixeff_EXP)), 
                         as.matrix(vcovCR(randeff_EXP, type = "CR0")),
                         as.matrix(vcovCR(randeff_EXP_controls, type = "CR0"))
             ),
             coef_rename = c("caddy2" = "Cart 2", 
                             "(Intercept)" = "Intercept",
                             "treatmentNS2016" = "NutriScore 2016",
                             "treatmentExplicit price" = "Explicit large price change",
                             "treatmentImplicit price" = "Implicit large price change",
                             "treatmentNS + large price" = "NutriScore and large price change",
                             "treatmentNS + small price" = "NutriScore and small price change",
                             "treatmentNutriScore" = "NutriScore 2019",
                             "caddy2:treatmentNS2016" = "Cart 2 $\\times$ NutriScore 2016",
                             "caddy2:treatmentExplicit price" = "Cart 2 $\\times$ Explicit large price change",
                             "caddy2:treatmentImplicit price" = "Cart 2 $\\times$ Implicit large price change",
                             "caddy2:treatmentNS + large price" = "Cart 2 $\\times$ NutriScore and large price change",
                             "caddy2:treatmentNS + small price" = "Cart 2 $\\times$ NutriScore and small price change",
                             "caddy2:treatmentNutriScore" = "Cart 2 $\\times$ NutriScore 2019"),
             #output = "Tables/Table_4_regression.tex",
             output = "kableExtra",
             title = "Difference-in-difference fixed-effect regression results. Standard error clustered by subject.") %>% 
  add_header_above(c(" " = 1, "Subject indicators" = 3, "Raw shoppig data" = 3)) %>%           
  add_header_above(c(" " = 1, "Expenditure" = 6))
            



