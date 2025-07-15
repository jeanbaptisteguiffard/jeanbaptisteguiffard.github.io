## Exercice

#setwd('C:/Users/jbgui/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat')
library(rvest)
library(ggplot2)
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_ecological_footprint"
code_html <- read_html(url, encoding="UTF-8")


table <- code_html %>% html_nodes('div[id="mw-content-text"]') %>% 
  html_nodes("table") %>% 
  html_nodes(xpath = "//table[@class='wikitable sortable']")



ecofootprint_table <- as.data.frame(table %>% html_table(fill=TRUE))
ecofootprint_table <- ecofootprint_table[2:nrow(ecofootprint_table),]



ecofootprint_table$Rank <- as.numeric(ecofootprint_table$Rank)
ecofootprint_table$Ecologicalfootprint <- as.numeric(ecofootprint_table$Ecologicalfootprint)
ecofootprint_table <- transform(ecofootprint_table, Country.region = reorder(Country.region, Rank))

ggplot(data=subset(ecofootprint_table, Rank<10), aes(x=Country.region, y=Ecologicalfootprint)) +
  geom_bar(stat="identity")


ecofootprint_table$Country.region <- as.character(ecofootprint_table$Country.region)

ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'United States'] <- 'United States of America'
ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'Russia'] <- 'Russian Federation'
ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'Iran'] <- 'Iran (Islamic Republic of)'
ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'Congo, Democratic Republic of the'] <- 'Democratic Republic of the Congo'
ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'United Kingdom'] <- "U.K. of Great Britain and Northern Ireland"
ecofootprint_table$Country.region[ecofootprint_table$Country.region == 'Tanzania'] <- "United Republic of Tanzania"


write.csv2(ecofootprint_table, 'ecofootprint_table.csv')


library(sf)
world_map <- st_read('DATA/world-administrative-boundaries.shp')
world_map <- merge(world_map, ecofootprint_table, by.x="name", by.y="Country.region", all.x=T)

#pdf('Ecologicalfootprint.pdf')
ggplot()+
  geom_sf(data = world_map, aes(fill=Ecologicalfootprint), size=0.3, show.legend = TRUE)
#dev.off()