## demographic table

# computing the income per consumption unit
demo <- df %>% 
  # taking the mid point of income to generate a numeric variable
  mutate(rev = case_when(income=="0_1000"    ~ 1000,
                         income=="1000_2000" ~ 1500,
                         income=="2000_3000" ~ 2500,
                         income=="4000_5000" ~ 4500,
                         income=="5000_6000" ~ 5500,
                         income=="3000_4000" ~ 3500,
                         income=="6000_7000" ~ 6500,
                         income=="7000_8000" ~ 7500,
                         income=="8000_plus" ~ 8000)) %>% 
  # if number of children is missing, it is actually zero
  mutate(children = if_else(is.na(children), 0, children)) %>% 
  # compute the income per consumption unit
  mutate(uc = (1+(familysize-children-1)*0.5 + children*0.3)) %>% 
  mutate(niveau_vie = rev/uc) %>% 
  mutate(niveau_vie_an = niveau_vie*12) %>%
  ungroup() %>% 
  mutate(revenu2 = niveau_vie_an) %>% 
  mutate(revenu = ntile(niveau_vie_an, 3)) %>% 
  mutate(revenu = as_factor(revenu)) %>% 
  mutate(revenu = fct_recode(revenu,  "<2000€"= "1", "2000-3000€" = "2", ">3000€" = "3"))



# changing variable level names and translating things into English
demo <- demo %>% 
  mutate(female = as.factor(female), 
         female = fct_recode(female, "Female" = "TRUE", "Male" = "FALSE")) %>% 
  rename(genre = female) %>% 
  mutate(age = cut(age, breaks = c(0,20,35,50,100), labels = c("<20", "20-35", "35-50", ">50"))) %>% 
  mutate(age = fct_relevel(age, "<20", "20-35", "35-50", ">50")) %>% 
  mutate(education = fct_recode(education, "Less than high school" = "aucun", "Less than high school" = "bp",
                                "Less than high school" = "cap", "High school" = "bac", "University or more" = "sup2",
                                "University or more" = "sup4", "Less than high school" = "autre")) %>% 
  mutate(profession = as.factor(profession),
         profession = fct_recode(profession, "Handicraft" = "Artisans", 
                                             "Exectuives" = "Cadres",
                                             "White collars" = "Employés",
                                             "Blue collars" = "Ouvriers",
                                             "Professionals" = "ProfInt",
                                             "Unemployed" = "Sans"))

# narrowing down to the needed variables and deleting duplicated rows
demo <- demo %>% 
  #filter(treatment != "Neutre2016" & treatment != "NS2016") %>% 
  select(subject, treatment, income = revenu, profession, education, gender = genre, age) %>% 
  distinct()


demotab <- demo %>% 
  gather(variable, value, -subject, -treatment) %>% 
  group_by(treatment, variable, value) %>% 
  tally() %>% 
  mutate(N = sum(n), share = round(100*n/N,2)) %>% 
  select(treatment, variable, value, share) %>% 
  group_by(variable, value) %>% 
  spread(treatment, share, fill = 0)

## chi2 tests

tests <- demo %>% 
  gather(variable, value, -subject, - treatment) %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(test = map(data,  ~chisq.test(.x$value, .x$treatment))) %>% 
  mutate(tidied = map(test, tidy)) %>% 
  unnest(tidied, .drop = T) %>% 
  mutate(chisquare_p.value = round(p.value, 5)) %>% 
  select(variable, chi2_2019_only = chisquare_p.value)


# overall sample
overall <- demo %>% 
  gather(variable, value, -subject) %>% 
  group_by(variable, value) %>% 
  tally() %>% 
  mutate(N = sum(n), overall = round(100*n/N,2)) %>% 
  select(variable, value, overall)

# adding overall counts and renaming variables
demotab <- demotab %>% 
  left_join(tests, by = "variable") %>% 
  left_join(overall, by = c("variable","value")) %>% 
  select(variable, value, overall, 
         "Benchmark" = Neutre2016,
         "NutriScore 2016" = NS2016,
         NutriScore = NS,
         "NS + large price" = "NS_pc_exp",
         "NS + small price" = "NS_ct_exp",
         "Implicit price" = "pc_imp",
         "Explicit price" = "pc_exp",
         "Chi2 p-value" = `chi2_2019_only`)

# exporting to csv
demotab %>% 
  write_csv("Tables/Table_3_demographics.csv")

# creating good-looking latex table
sink(file = "Tables/Table_3_demo.tex")
final_table <- demotab %>% 
  ungroup() %>% 
  select(-variable) %>% 
  kbl(booktabs = T,
      format = "latex", 
      caption = "Overview of nutritional and economic results", 
      label = "tab:demo"
  ) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center") %>% 
  pack_rows("Age", 1, 3) %>%
  pack_rows("Education", 4, 6) %>%
  pack_rows("Gender", 7, 8) %>% 
  pack_rows("Income", 9,11) %>% 
  pack_rows("Profession", 12, 17) 
final_table
sink()

# creating good-looking pdf table 
final_table %>% 
  save_kable("Tables/Table_3_demographics.pdf")

## cleaning up
rm(demo)
rm(demotab)
rm(overall)
rm(tests)
rm(final_table)

