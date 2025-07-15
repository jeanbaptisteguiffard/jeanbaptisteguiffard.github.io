####################################################
## DATA AND CLIMATE - Session 6 - Regressions with R...
####################################################

# L'économétrie sur R (avec le langage de base)

setwd('C:/Users/jbgui/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat')

library(dplyr)
library(tidyr)
data <- read.csv2('seance_6/owid-co2-data-wb.csv', sep=";")

data_ex <- mutate(data, gdp_per_capita1 = as.numeric(gdp)/as.numeric(population))
data <- data %>%
  mutate(gdp_per_capita = as.numeric(gdp)/as.numeric(population),
         co2_per_capita = as.numeric(co2_per_capita)+1)
sel_data <- data %>% 
  select(c(gdp_per_capita,co2_per_capita))  %>%
  drop_na()

summary(sel_data)

plot(log(co2_per_capita)~log(gdp_per_capita), data=sel_data, pch=1, cex=.3)

reg.1 <- lm(log(co2_per_capita)~gdp_per_capita, data=sel_data)
summary(reg.1)

data <- data %>%
  mutate(energy_per_capita = as.numeric(energy_per_capita))
reg.2 <- lm(log(co2_per_capita)~gdp_per_capita+energy_per_capita, data=data)
summary(reg.2)


library(leaps)
choix <- regsubsets(log(co2_per_capita)~gdp_per_capita+energy_per_capita+methane+nitrous_oxide , data=data, nbest=1, nvmax=11)
plot(choix, scale="bic")



#install.packages('explore')
library(explore)
head(data %>% explore::describe())


# Quelques packages et fonctions pour les statistiques descriptives

#install.packages('table1')
#install.packages('kableExtra')
library(table1)
library(kableExtra)
tab1 <- table1(~  gdp_per_capita + co2_per_capita + energy_per_capita, data=data)

tab2 <- table1(~  gdp_per_capita + co2_per_capita + energy_per_capita | IncomeGroup, data=data)
tab3 <- table1(~  gdp_per_capita + co2_per_capita + energy_per_capita | Region, data=data)

kable(as.data.frame(tab1), booktabs=TRUE, format='latex')

kable(as.data.frame(tab2), booktabs=TRUE)

# Le package Fixest

library(fixest)
reg.3 = feols(log(co2_per_capita) ~ gdp_per_capita + energy_per_capita , data)
summary(reg.3)

reg.4 <- feols(log(co2_per_capita) ~ gdp_per_capita + energy_per_capita | year , data)
summary(reg.4)

data('trade')
head(trade %>% explore::describe())

trade <- trade %>%
  mutate(lndist_km = log(dist_km))

gravity_pois1 = feols(Euros ~ lndist_km | Year, trade)
print(gravity_pois1)

gravity_pois2 = fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)
print(gravity_pois2)

gravity_pois3 = fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, cluster = "Product", trade)
print(gravity_pois3)

etable(gravity_pois3, cluster = ~Origin+Destination, tex = TRUE)

# Tableau de résultats avec modelsummary

#install.packages('modelsummary')
library(modelsummary)


sum.1 <- modelsummary(gravity_pois1, stars = c("*"=0.1,"**"=.05,"***"=.01))

models <- list()
models[['Year fe']] <- gravity_pois1
models[['Several fe']] <- gravity_pois2
models[['Clustered fe']] <- gravity_pois3

sum.2 <- modelsummary(models, stars = c("*"=0.1,"**"=.05,"***"=.01))