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


### For the referees: 
### - we export all regressions, both using indicators and using the raw shopping data
### - we note that the only way we can have a random slope model is by resorting to raw data
### - this comes at three prices 
###   1. it departs from pre-reg, where we stated that we'd build individual/caddy indicators and focus on those
###   2. it forces us to work with different objects, especially for expenditure: 
###      the indicator computes the average expenditure for 2000Kcal across all products
###      while the individual data tells us only expenditure per 200Kcal of ONE product
###   3. the model shows a -1 perfect corr between the random slope and random intercept, signaling a convergence problem
###      and all diagnostics point us to ditch the random slope and keep only a random intercept
### - if we keep only the random intercept, there is no reason to go for the raw data, depart from pre-reg, and deal with awkward indicators
###
### so in the end we just run the fixed effect and random intercept with and without control, and att those to the paper


### Large tables for the referees and editor

## ScoreFSA

modelsummary(list("FE, individual" = reg_FSA_fe, 
                  "RE, individual" = reg_FSA_raneff,
                  "RE controls, individual" = reg_FSA_raneff_controls,
                  "FE, purchase" = fixeff_FSA, 
                  "RE, purchase" = randeff_FSA, 
                  "RE, purchase" = randeff_FSA_controls
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
             output = "Extra_tables/All_models_run_FSA.tex",
             title = "Regressions using different speciications and data, ScoreFSA.")




## Expenditure

modelsummary(list("FE, ind" = reg_exp_fe, 
                  "RE, ind" = reg_EXP_raneff,
                  "RE controls, ind" = reg_EXP_raneff_controls,
                  "FE, item" = fixeff_EXP,
                  "RE, item" = randeff_EXP, 
                  "RE controls, item" = randeff_EXP_controls),
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
             output = "Extra_tables/All_models_run_EXP.tex",
             title = "Regressions using different speciications and data, Expenditure.") 
            

### simpler tables for the appendix

## FSA
modelsummary(list("Fixed effects" = reg_FSA_fe, 
                  "Random intercept" = reg_FSA_raneff,
                  "Random intercept controls" = reg_FSA_raneff_controls
                  ),
             fmt = "%.3f",
             estimate = "{estimate} ({std.error}){stars}",
             conf_level = 0.95,
             statistic = NULL,
             vcov = list(as.matrix(vcov(reg_FSA_fe)), 
                         as.matrix(vcovCR(reg_FSA_raneff, type = "CR0")), 
                         as.matrix(vcovCR(reg_FSA_raneff_controls, type = "CR0"))
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
             output = "Tables/Table_A3_robustness_FSA.tex",
             title = "Fixed and Random intercept model with and without controls, ScoreFSA. Standard error clustered by subject.")


## EXP
modelsummary(list("Fixed effects" = reg_exp_fe, 
                  "Random intercept" = reg_EXP_raneff,
                  "Random intercept controls" = reg_EXP_raneff_controls
            ),
            fmt = "%.3f",
            estimate = "{estimate} ({std.error}){stars}",
            statistic = NULL,
            vcov = list(as.matrix(vcov(reg_exp_fe)), 
                        as.matrix(vcovCR(reg_EXP_raneff, type = "CR0")), 
                        as.matrix(vcovCR(reg_EXP_raneff_controls, type = "CR0"))
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
            output = "Tables/Table_A4_robustness_EXP.tex",
            title = "Fixed and Random intercept model with and without controls, Expenditure Standard error clustered by subject.")
