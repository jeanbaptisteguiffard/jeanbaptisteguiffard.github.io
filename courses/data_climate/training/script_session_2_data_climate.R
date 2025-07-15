################################################################################
##############                 DATA AND CLIMATE                   ##############                  
##############                   Second Session                   ##############                  
##############               Jean-Baptiste Guiffard               ##############
################################################################################



#################### Graphic representations with R #####################

library(dplyr)
#setwd("C:/Users/jbgui/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat/")


data_pollution <- read.csv2('DATA/owid-co2-data.csv', sep=",")
Metadata_Country <- read.csv2('DATA/Metadata_Country.csv', sep=",") %>%
#  rename("Country_code" = "ï..Country.Code")
rename("Country_code" = "Country.Code")

data_pollution_num <- data_pollution %>%
  select(-c(country,iso_code))%>%
  mutate_if(is.character, as.numeric) %>%
  cbind(data_pollution[,c("country","iso_code")])


data_pollution_num_2018 <- data_pollution_num %>%
  filter(year==2018)
hist(data_pollution_num_2018$co2_per_capita)

boxplot(data_pollution_num_2018$co2_per_capita)

data_pollution_num_2018$rich <- ifelse(data_pollution_num_2018$gdp > 500000000000,1,0)
table(data_pollution_num_2018$rich)
boxplot(data_pollution_num_2018$co2_per_capita ~ data_pollution_num_2018$rich)


join_pollution_wb_data <- data_pollution_num %>%
  dplyr::inner_join(Metadata_Country, by = c("iso_code" = "Country_code"))

join_pollution_wb_data <- join_pollution_wb_data %>%
  filter(country != "") %>%
  filter(IncomeGroup !="")


join_pollution_wb_data <- join_pollution_wb_data %>%
  mutate(gdp_per_capita = gdp/population,
         co2_per_capita_en_kg = co2/population*1000000000)


data_pollution_num_mean <- join_pollution_wb_data %>%
  filter(year >= 1990 & year <= 2020) %>%
  group_by(country,IncomeGroup) %>%
  summarise(mean_gdp_per_capita = mean(gdp_per_capita, na.rm=T),
            mean_co2_per_capita = mean(co2_per_capita_en_kg, na.rm=T))


data_pollution_num_mean <- data_pollution_num_mean %>%
  na.omit()


data_pollution_region_mean <- join_pollution_wb_data %>%
  filter(year >= 1990 & year <= 2020) %>%
  group_by(country,Region) %>%
  summarise(mean_gdp_per_capita = mean(gdp_per_capita, na.rm=T),
            mean_co2_per_capita = mean(co2_per_capita_en_kg, na.rm=T))


plot(data_pollution_num_mean$mean_gdp_per_capita, data_pollution_num_mean$mean_co2_per_capita)

plot(log(data_pollution_num_mean$mean_gdp_per_capita), log(data_pollution_num_mean$mean_co2_per_capita))


#################### GGPLOT #####################


library(ggplot2)

etape1 <- ggplot(data_pollution_num_mean)

etape2 <- etape1 + aes(x=mean_co2_per_capita)

etape3 <- etape2 + geom_histogram()

plot(etape3)

etape1 <- ggplot(data_pollution_num_mean)
etape2 <- etape1 + aes(x=IncomeGroup)
etape3 <- etape2 + geom_bar(aes(fill=IncomeGroup))
etape4 <- etape3 + scale_fill_brewer(palette = "Reds")
plot(etape3) 
plot(etape4)


etape1 <- ggplot(data_pollution_region_mean)
etape2 <- etape1 + aes(x=Region)
etape3 <- etape2 + geom_bar(aes(fill=Region))
etape4 <- etape3 + scale_fill_brewer(palette = "Greens")
plot(etape3) 
plot(etape4)

pdf('mes_graphes.pdf')
plot(etape3) 
plot(etape4)
dev.off()


graph1 <- ggplot(data_pollution_num_mean,
                 aes(x=log(mean_gdp_per_capita),
                     y=log(mean_co2_per_capita)))
plot(graph1)


graph1_b <- graph1 + geom_point()
plot(graph1_b)


graph1_1 <- graph1 +
  geom_point(size=2, shape=17)

graph1_2 <- graph1 +
  geom_point(size=2, shape=24, colour='black', fill="red")

plot(graph1_1)
plot(graph1_2)

graph1_3 <- graph1  +
  geom_point(size=2, shape=24, colour='black', fill="red")+ 
  theme_classic()

graph1_4 <- graph1  +
  geom_point(size=2, shape=24, colour='black', fill="red") +
  ggtitle('Scatter plot Wealth and Pollution (1990-2020)')+
  theme_classic()+
  labs(caption ="Source: Our World in data",
       x='GDP per capita (log)',
       y='CO2 emission per capita (log)')

plot(graph1_3)
plot(graph1_4)

graph1_5 <- ggplot(data_pollution_num_mean,
                   aes(x=log(mean_gdp_per_capita),
                       y=log(mean_co2_per_capita), 
                       colour = factor(IncomeGroup),
                       label = country))+
  geom_point()+
  theme_classic() + 
  labs(caption ="Source: Our World in data",
       x='GDP per capita (log)',
       y='CO2 emission per capita (log)')

graph1_5 + geom_text(size=3)

plot(graph1_5)

graph1_6 <- graph1 +
  geom_point(size=1) +
  geom_smooth()

graph1_7 <- graph1 +
  geom_point(size=1) +
  geom_smooth(method=lm, se=FALSE)

plot(graph1_6)
plot(graph1_7)

graph2 <- ggplot(data_pollution_num_mean, 
                 aes(x=factor(IncomeGroup),
                     y=log(mean_co2_per_capita)))
graph2_1 <- graph2 + geom_boxplot()
plot(graph2_1)

graph2_2 <- graph2 + geom_bar(stat = "identity")

plot(graph2_2)


#################### What graphs from this dataset? #####################

data_pollution_num_by_group <- join_pollution_wb_data %>%
  group_by(IncomeGroup, year) %>%
  summarise(sum_emission = sum(co2, na.rm=T))

graph3_1 <- ggplot(data_pollution_num_by_group)
graph3_2 <- graph3_1 + aes(x = year, 
                           y = sum_emission,
                           group = IncomeGroup,
                           colour = IncomeGroup) 
graph3_3 <- graph3_2 + geom_line()

data_pollution_num_by_group$IncomeGroup <- factor(data_pollution_num_by_group$IncomeGroup, 
                                                  levels=c('High income', 'Upper middle income', 'Lower middle income', 'Low income'))
graph4_1 <- ggplot(data_pollution_num_by_group)
graph4_2 <- graph4_1 + aes(x = year, 
                           y = sum_emission,
                           group = IncomeGroup,
                           colour = IncomeGroup) 
graph4_3 <- graph4_2 + geom_line()

plot(graph3_3)
plot(graph4_3)

data_pollution_num_by_cont <- join_pollution_wb_data %>%
  group_by(Region, year) %>%
  summarise(sum_emission = sum(co2, na.rm=T))

data_pollution_num_by_cont$Region <- factor(data_pollution_num_by_cont$Region)
graph5_1 <- ggplot(data_pollution_num_by_cont)
graph5_2 <- graph5_1 + aes(x = year, 
                           y = sum_emission,
                           group = Region,
                           colour = Region) 
graph5_3 <- graph5_2 + geom_line()

plot(graph5_3)

library(treemap)

data_pollution_region_mean <- join_pollution_wb_data %>%
  filter(year >= 1990 & year <= 2020) %>%
  group_by(country,Region) %>%
  summarise(mean_gdp_per_capita = mean(gdp_per_capita, na.rm=T),
            mean_co2_per_capita = mean(co2_per_capita_en_kg, na.rm=T),
            mean_co2 = mean(co2, na.rm=T))

selection_1 <- data_pollution_region_mean %>%
  filter(mean_co2_per_capita>5000)

selection_2 <- data_pollution_region_mean %>%
  filter(mean_co2>140)

treemap(selection_2, index=c("Region","country"), 
        vSize="mean_co2", 
        type="index",
        title="CO2 emissions by country (in millions of tons of CO2)")

treemap(selection_1, index=c("Region","country"), 
        vSize="mean_co2_per_capita", type="index", 
        
        title="CO2 emissions per capita by country (in tons of CO2)")


data_pollution_incomegroup_mean <- join_pollution_wb_data %>%
  filter(year >= 1990 & year <= 2020) %>%
  group_by(country,IncomeGroup) %>%
  summarise(mean_gdp_per_capita = mean(gdp_per_capita, na.rm=T),
            mean_co2_per_capita = mean(co2_per_capita_en_kg, na.rm=T),
            mean_co2 = mean(co2, na.rm=T))

treemap(data_pollution_incomegroup_mean, index=c("IncomeGroup","country"), 
        vSize="mean_co2", type="index", 
        
        title="CO2 emissions by country (in tons of CO2)")


selection_1_incomegroup <- data_pollution_incomegroup_mean %>%
  filter(mean_co2>54)

treemap(selection_1_incomegroup, index=c("IncomeGroup","country"), 
        vSize="mean_co2", type="index", 
        
        title="CO2 emissions by country (in tons of CO2)")

selection_2_incomegroup <- data_pollution_incomegroup_mean %>%
  filter(mean_co2_per_capita>6842.37)

treemap(selection_2_incomegroup, index=c("IncomeGroup","country"), 
        vSize="mean_co2_per_capita", type="index", 
        
        title="CO2 emissions per capita by country (in tons of CO2)")
