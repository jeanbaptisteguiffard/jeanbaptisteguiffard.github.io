################################################################################
##############                 DATA AND CLIMATE                   ##############                  
##############                   First Session                    ##############                  
##############               Jean-Baptiste Guiffard               ##############
################################################################################



#################### THE BASICS OF R ######################

# R, as calculator ?

2+2.5
7*5
100/25


# Get help on R

?mean
help(mean)


# Set working directory

setwd("C:/Users/Giffarrd/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat")


# Download data

data <- read.csv2('DATA/owid-co2-data.csv', sep=",")
View(data)
# Install and load packages

install.package('tidyverse')

library(tidyverse)
require(tidyverse)


#################### OBJECTS IN R ######################

#Some Basic objects on R

#Scalar
a <- 1
b <- "Le Seminaire"
b1 <- FALSE

class(b)
mode(b1)


#Vecteur
c <- c(3, 4, 5, 6)
d <- c(7, 8, 9, 10)

notes <- c(13,16,18,12,15)

bonus <- 1

nouvelles_notes <- notes + bonus

seq(1,10,by=1)
seq(1,10,by=0.5)

rep(1,8)

e <- c("Initiation", "_", "R")

#Matrix

matrix_1 <- matrix(1, nrow = 4, ncol=2)

length(matrix_1)
dim(matrix_1)

comb_c_d <- c(c,d)

matrix_2 <- matrix(
  comb_c_d, # elements 
  nrow=4,             
  ncol=2,             
  byrow = FALSE)  #fill the matrix by rows 

matrix_2[,2] # column 2
matrix_2[4,] # row 3
matrix_2[1,1] #2nd row, 2nd column 



#Liste 
i <- list(b, d, matrix_1, "h")
j <- list(i, "Poupée russe de liste")


#Data frame
taille <- c(152, 156, 160, 160, 163, 167, 169, 173, 174, 174)
masse <- c(51, 51, 54, 60, 61, 64, 70, 71, 72, 73)
sexe <-c("M","F","F","M", "M","F","F","M", "F", "F")
df <- data.frame(taille,masse,sexe)
print(df)
nrow(df)
ncol(df)


mean(df$taille)
var(df$taille)
sd(df$taille)
median(df$taille)

cov(df$taille, df$masse)


colnames(df) #list of the names of the variables in df
head(df, n=4) #First rows in df
summary(df) #summarize all variables
summary(df$taille) #summarize one variable 
table(df$sexe)
hist(df$taille)


# an other example of a function

set.seed(140) # fix the seed of the generator ... allows to find the same results from one simulation to another.
rnorm(n=4)


#################### MANIPULATE DATASETS WITH DPLYR ######################


library(dplyr)
data_pollution <- read.csv2('DATA/owid-co2-data.csv', sep=",")
#View(data_pollution)

as.numeric(data_pollution$co2) %>% mean(na.rm=T)

mean(as.numeric(data_pollution$co2), na.rm=T)


# Subset observations
data_pollution_num <- data_pollution %>%
  select(-c(country,iso_code))%>%
  mutate_if(is.character, as.numeric) %>%
  cbind(data_pollution[,c("country","iso_code")])


data_continents <- data_pollution_num %>%
  filter(country %in% c("Africa","Europe","Oceania","Asia",
                        "North America","South America"))
data_france <- data_pollution_num %>%
  filter(country == "France")

sample_data_pollution <- data_pollution_num %>%
  sample_n(10)


slice_data_pollution <- data_pollution_num %>%
  slice(25:40)


distinct_data_pollution <- data_pollution_num %>%
  distinct(country,.keep_all = T)

# Modify variables
data_continents <- data_continents %>%
  select(country, year, population, co2,cumulative_co2)

data_continents <- data_continents %>%
  rename(CO2 = co2)

data_continents %>% 
  dplyr::count(country)

# Summarise data

data_continents %>% 
  group_by(country) %>%
  summarise(number_obs = n())


data_continents %>% 
  filter(year >=2010 & year < 2020) %>%
  summarise(sum_co2 = sum(CO2))


data_continents %>% 
  group_by(year) %>%
  summarise(sum_co2 = sum(CO2))


data_continents %>% 
  group_by(country) %>%
  summarise(mean_co2_emis = mean(CO2, na.rm=T))

data_continents %>% 
  group_by(country) %>%
  summarise(mean_co2_emis = mean(CO2, na.rm=T))%>%
  mutate(rank = rank(-mean_co2_emis))

# Generate a numeric variable

data_continents %>%
  mutate(co2_per_capita = CO2/population) %>%
  group_by(country) %>%
  summarise(mean_co2_per_capita = mean(co2_per_capita, na.rm=T)) %>%
  mutate(rank = rank(-mean_co2_per_capita))

summary(data_continents$CO2)
# Generate a categorical variable
data_continents <- data_continents %>%
  mutate(cat_co2 = case_when(
    CO2 < 23 ~ '[0,23[',
    CO2 >= 23 & CO2 < 220 ~ '[23,220[',
    CO2 >= 220 & CO2 < 1385 ~ '[220,1385[',
    CO2 > 1385  ~ '[1385,20609[',
  ))
data_continents %>%
  filter(year==2019)

# Join datasets
Metadata_Country <- read.csv2('DATA/Metadata_Country.csv', sep=",")%>%
  rename("Country_code" = "Country.Code")

join_pollution_wb_data <- data_pollution_num %>%
  dplyr::inner_join(Metadata_Country, by = c("iso_code" = "Country.Code"))


antijoin_pollution_wb_data <- data_pollution_num %>%
  anti_join(Metadata_Country, by = c("iso_code" = "Country.Code")) 

head(antijoin_pollution_wb_data %>%
       group_by(country) %>%
       count())
