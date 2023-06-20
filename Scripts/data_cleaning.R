### cleaning the data ###

## correct order of factors

#todo


### COMPUTING LIM ###
#######################################
# 
# # lim 1/3 * [AGS100g/22]*100 + [Sucre100g/50]*100 + [Sel100g/8]*100
# # NB:it would be 3.153 if we had Sodium data; we just have NaCl, so we use 8grams
# 
df$LIM <- ((df$ags_100/22)*100 + (df$sucres_100/50)*100 + (df$sel_100/8)*100)/3
#LIM is multiplied 2.5x for sodas
df$LIM[df$category=="Sodas"] <- 2.5*df$LIM[df$category=="Sodas"]




# #######################################
# ### refactoring demographic variables  ###
# #######################################
df <- df %>% mutate(income2 = fct_recode(income, NULL = "", lower = "0_1000", lower = "1000_2000",
                                         middle = "2000_3000", higher = "3000_4000", higher = "4000_5000",
                                         higher = "5000_6000", higher = "6000_7000", higher = "8000_plus",
                                         higher = "7000_8000"))



######### weight and other units
df$weight <- as.factor(df$weight)

# weight is sometimes in Kg, sometimes in g; sometimes in cl, sometimes 'pièce'. Solving the issues.
# packs of coke have a multiplication notation for weight
levels(df$weight)[levels(df$weight)=="8*25"]<- "200"
levels(df$weight)[levels(df$weight)=="15*25"]<- "375"
levels(df$weight)[levels(df$weight)=="12*15"]<- "180"
levels(df$weight)[levels(df$weight)=="2*25"]<- "50"
levels(df$weight)[levels(df$weight)=="3*20"]<- "60"

## solving problems with products sold per unit
df$weight[df$product == "Concombre<br />&nbsp;"] <- 200
df$weight[df$product == "Kiwi<br />&nbsp;"] <- 100
df$weight[df$product == "Avocat<br />&nbsp;"] <- 300

## eggs
df$weight[df$product == "6 Gros oeufs<br />&nbsp;"] <- 72
df$weight[df$product == "6 Gros oeufs plein air<br />&nbsp;"] <- 72
df$weight[df$product == "6 Oeufs bio fermiers<br />&nbsp;"] <- 60


#converting factor to numeric
df$weight <- as.numeric(as.character(df$weight))

#unit conversion
df$weight[df$unit_weight == "kg"]<- df$weight[df$unit_weight == "kg"]*1000
df$weight[df$unit_weight == "L"]<- df$weight[df$unit_weight == "L"]*1000
df$weight[df$unit_weight == "cL"]<- df$weight[df$unit_weight == "cL"]*10
df$weight[df$unit_weight == "cl"]<- df$weight[df$unit_weight == "cl"]*10
df$weight[df$unit_weight == "ml"]<- df$weight[df$unit_weight == "ml"]

#recode unit variable
levels(df$unit_weight)[levels(df$unit_weight)=='cL']<-'ml'
levels(df$unit_weight)[levels(df$unit_weight)=='cl']<-'ml'
levels(df$unit_weight)[levels(df$unit_weight)=='kg']<-'g'
levels(df$unit_weight)[levels(df$unit_weight)=='L']<-'ml'
levels(df$unit_weight)[levels(df$unit_weight)=='ml']<-'ml'
levels(df$unit_weight)[levels(df$unit_weight)=='pièce']<-'g'
levels(df$unit_weight)[levels(df$unit_weight)=='pièces']<-'g'





#creating a price variable that takes into account the correct prices at the correct moment
df <- df %>% 
  mutate(observedprice = case_when(caddy == 1                            ~ price,
                                   caddy == 2 & treatment == "NS_pc_exp" ~ price_percent,
                                   caddy == 2 & treatment == "NS"        ~ price,
                                   caddy == 2 & treatment == "NS2016"    ~ price,
                                   caddy == 2 & treatment == "Neutre2016"~ price,
                                   caddy == 2 & treatment == "pc_imp"    ~ price_percent,
                                   caddy == 2 & treatment == "pc_exp"    ~ price_percent,
                                   caddy == 2 & treatment == "NS_ct_exp" ~ price_cent))


### adding weighted variables
#actual weight & price -- using the original price
df <- df %>% mutate(actual_weight = weight*quantity,
                    actual_price = price*quantity,
                    actual_observedprice = observedprice*quantity)

#computing indicators for Kcal, Salt, Sugar, AGS
df <- df %>%           mutate(actual_Kcal  = (kcal_100/100)*actual_weight,
                              actual_Salt  = (sel_100/100)*actual_weight ,
                              actual_Sugar = (sucres_100/100)*actual_weight,
                              actual_AGS   = (ags_100/100)*actual_weight,
                              actual_Score = (scoreFSA/100)*actual_weight,
                              actual_Protein = (proteines_100/100)*actual_weight,
                              actual_Fibres = (fibres_100/100)*actual_weight,
                              actual_lipides = (lipides_100/100)*actual_weight,
                              actual_glucides = (glucides_100/100)*actual_weight)


#### main indicator
##compute the indicators
#FSA
df <- df %>% mutate(FSAKcal = scoreFSA * actual_Kcal)

#LIM
df <- df %>% mutate(LIMKcal = LIM * actual_Kcal)

#weight
df <- df %>% mutate(FSAgram = scoreFSA * actual_weight)


### age bug
# for some reasons "age" is listed as "age-18 on Gaelexperience. 
# the infile corrects this adding +13; so here we further add +5 in order not to have to re-run the infile script.
df$age <- df$age + 5

## outliers: from 2016
## one subject that had one product in caddy 1 and 12 in caddy 2
df <- df %>% filter(subject != 5924054)

## 3 subjects that have just one of the two caddies
df <- df %>% filter(subject != 5407546 & subject!= 5044054 & subject != 5091645)

## 2 subjects that we completed by hand in 2016 and that we do not need right now here
## TODO: change this!
df <- df %>% filter(subject != 4963171 & subject != 4984054)

## outliers: from 2019
## one subject who had NO caddy 2 (possibly to be added by hand later)
df <- df %>% filter(subject != 7482428)
