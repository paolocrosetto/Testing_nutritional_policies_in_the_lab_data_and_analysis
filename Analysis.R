### LabelPrix data Analysis


# libraries
library(tidyverse)      ## R dialect used in these scripts
library(broom)          ## tidy test output so that it fits with the tidyverse
library(kableExtra)     ## nice formatting of Latex tables

library(fixest)         ## fixed effect diff in diff estimation
library(car)            ## linear hypothesis testing for post-estimation testing
library(modelsummary)   ## swiss army knofe tool to export regression tables in any format

library(ggbeeswarm)     ## used for raincloud plots
library(gghalves)       ## used for raincloud plots
library(waffle)         ## waffle chart to display the catalog by NS letter
library(ggridges)       ## used for ridgeline distribution of price plot

library(hrbrthemes)     ## nice-looking ggplot themes
library(scico)          ## nice colorblind friendly and science-based color palettes


######## importing data ########

## all data
df <- read_csv("Data/experimental_data.csv")

## a dataset containing only product characteristics 
products <- read_csv("Data/products.csv")


######## data cleaning ########
source("Scripts/data_cleaning.R")


### Demographic table across treatments
source("Scripts/demo_table.R")

### Subjects, overall and by treatment
source("Scripts/subjects_treatments.R")

### Catalog analysis
source("Scripts/catalog_analysis.R")


## confirmatory analyses


## Regression with fixest + linear hp testing from regression
source("Scripts/diff_in_diff.R")


## At a glance: FSA and expenditure deltas
source("Scripts/at_a_glance.R")


## differential impact by revenue
source("Scripts/regressive.R")

## exploratory analyses

## change in NS categories
source("Scripts/NS_change.R")

## change in mean price of items
source("Scripts/other_changes.R")

## Cost for the government if we assume our effects hold generally
source("Scripts/cost_state.R")


######################################
## extra analyses used for presentation purposes

## Focus on replication of NS2016
source("Scripts/replication.R")

## Focus on comparison of policy mix 1: nS + price
source("Scripts/NS_and_prices.R")

## Focus on policy mix : price vs NS + price + NS
source("Scripts/prices_vs_label.R")

## Focus on implicit vs explicit
source("Scripts/implicit_explicit.R")

