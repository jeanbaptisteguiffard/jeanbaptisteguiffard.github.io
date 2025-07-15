################################################################################
##############                 DATA AND CLIMATE                   ##############                  
##############                   Third Session                    ##############                  
##############               Jean-Baptiste Guiffard               ##############
################################################################################

# Données et Climat Session 3 

#setwd('C:/Users/jbgui/OneDrive - Université Paris 1 Panthéon-Sorbonne/COURS_DISPENSES/IEDES_2022_2023/Data_Climat')

library(flextable)
library(dplyr)
library(magrittr)

# Traitement des données cartographiques avec le package sf

library(sf)
world_map <- st_read('DATA/world-administrative-boundaries.shp')
plot(st_geometry(world_map))

france_poly <- subset(world_map, iso3=='FRA')
plot(france_poly)
plot(world_map['color_code'])
plot(world_map['continent'])

Europe_map <- subset(world_map, continent=="Europe" & color_code != "RUS" )
plot(Europe_map['color_code'])

union_poly <- st_union(Europe_map) 
plot(st_geometry(Europe_map),border="green",lwd=1)
plot(st_geometry(union_poly),border="red",lwd=1,add=T)

Europe_centroids <- st_centroid(Europe_map)
plot(st_geometry(Europe_map))
plot(st_geometry(Europe_centroids), add=TRUE, cex=1, col="red", pch=20)

#install.packages("reshape2")
library(reshape2)
matrix_distance <- st_distance(Europe_centroids)
rownames(matrix_distance) <- Europe_centroids$color_code
colnames(matrix_distance) <- Europe_centroids$color_code

df_distance <- as.data.frame(as.table(matrix_distance))
df_distance$distance_km <- df_distance$Freq/1000

df_distance_france <- subset(df_distance, Var1=='FRA')
Europe_map <- merge(Europe_map,df_distance_france, by.x="color_code", by.y="Var2" )

plot(Europe_map['distance_km'])

power_plants_points <- st_read('DATA/Power_Plants.shp')
power_plants_points_europe <- subset(power_plants_points, 
                                     country %in% unique(Europe_map$iso3))
table(power_plants_points_europe$fuel1)
nuclear_pw_plants <- subset(power_plants_points_europe, fuel1 == "Nuclear")
nuclear_pw_plants.pts <- st_as_sf(nuclear_pw_plants, 
                                  coords = c("longitude", "latitude"),
                                  crs=st_crs(Europe_map))

plot(st_geometry(Europe_map))
plot(nuclear_pw_plants.pts,col="orange",cex=1,pch=16,add=T)

n_pw_plts_country <- st_intersects(Europe_map, nuclear_pw_plants.pts)
Europe_map$n_nuclear_pw <- sapply(X = n_pw_plts_country, FUN = length) 
plot(Europe_map['n_nuclear_pw'])

plot(st_geometry(Europe_map))
plot(st_geometry(Europe_map[Europe_map$n_nuclear_pw==0,]), col = "blue", add=TRUE)
plot(st_geometry(Europe_map[Europe_map$n_nuclear_pw<=4&Europe_map$n_nuclear_pw>0,]), col = "orange", add=TRUE)
plot(st_geometry(Europe_map[Europe_map$n_nuclear_pw>4,]), col = "red", add=TRUE)
plot(nuclear_pw_plants.pts, pch = 20, col = "white", add=TRUE, cex = 1)

#install.packages('leaflet')
library(leaflet)
m <- leaflet() %>% addTiles()

m1 <- leaflet(data = power_plants_points_europe) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(fuel1), label = ~as.character(fuel1),
             clusterOptions = markerClusterOptions())


# Notre première carte avec ggplot

data_pollution <- read.csv2('DATA/co2_clean.csv', sep=";")
Metadata_Country <- read.csv2('DATA/Metadata_Country.csv', sep=",")
#rename("Country_code" = "Country.Code")

join_pollution_wb_data <- data_pollution %>%
  dplyr::inner_join(Metadata_Country, by = c("iso_code" = "Country.Code"))

join_pollution_wb_data <- join_pollution_wb_data %>%
  filter(country != "") %>%
  filter(IncomeGroup !="")

library(ggplot2)
map1 <- ggplot() +
  geom_sf(data = world_map, col="grey", aes(fill=continent), show.legend = TRUE, size=0.1)+
  theme_minimal() +
  theme(legend.position = "right")+
  scale_fill_brewer(name = "Continents", na.value = "grey") +
  labs(x = NULL, 
       y = NULL, 
       title = "Carte des Continents", 
       caption = "Source: opendatasoft.com")

plot(map1)

join_pollution_wb_data_2019 <- subset(join_pollution_wb_data, year==2019)
world_map_co2 <- world_map %>%
  right_join(join_pollution_wb_data_2019, by=c('iso3'='iso_code'))
library(viridis)
map2 <- ggplot() +
  geom_sf(data = world_map_co2, col="grey", aes(fill=population), show.legend = FALSE, size=0.1)+
  theme_minimal() +
  theme(legend.position = "right")+
  scale_fill_viridis()+
  labs(x = NULL, 
       y = NULL, 
       title = "Carte de la Population mondiale", 
       caption = "Source: World in Data")

map2

join_pollution_wb_data <- join_pollution_wb_data %>%
  mutate(gdp_per_capita = gdp/population,
         co2_per_capita_en_kg = co2/population*1000000000)

data_pollution_region_mean <- join_pollution_wb_data %>%
  filter(year >= 1990 & year <= 2020) %>%
  group_by(country, iso_code,Region) %>%
  summarise(mean_gdp_per_capita = mean(gdp_per_capita, na.rm=T),
            mean_co2_per_capita = mean(co2_per_capita_en_kg, na.rm=T),
            mean_co2 = mean(co2, na.rm=T))

world_map_co3 <- world_map %>%
  right_join(data_pollution_region_mean, by=c('iso3'='iso_code'))

library(viridis)
map3 <- ggplot() +
  geom_sf(data = world_map_co3, col="grey", aes(fill=mean_co2_per_capita), show.legend = FALSE, size=0.1)+
  theme_minimal() +
  theme(legend.position = "right")+
  scale_fill_viridis()+
  
  labs(x = NULL, 
       y = NULL, 
       title = "Carte des émissions de CO2 par tête sur la période 1990-2020", 
       caption = "Source: World in Data")

map3

summary(world_map_co3$mean_co2_per_capita)
world_map_co3$brks <-  cut(world_map_co3$mean_co2_per_capita,
                           breaks=c(0,500,1000,2000,3000,4000,5000,6000,7000,48810),
                           labels=c("[0;500[", "[500;1000[", "[1000;2000[", "[2000;3000[", "[3000;4000[", "[4000;5000[","[5000;6000[","[6000;7000[","[7000;48810["))

map4 <- ggplot() +
  geom_sf(data = world_map_co3, col="grey", aes(fill=brks), show.legend = TRUE, size=0.1)+
  theme_minimal() +
  theme(legend.position = "right")+
  labs(x = NULL, 
       y = NULL, 
       title = "Carte des émissions de CO2 par tête sur la période 1990-2020", 
       caption = "Source: World in Data")

map4


## Exercice

coal_pw_plants <- subset(power_plants_points, fuel1 == "Coal")

n_pw_plts_coal <- st_intersects(world_map, coal_pw_plants)
world_map$n_coal_pw <- sapply(X = n_pw_plts_coal, FUN = length) 


world_map$breaks_coal <- ifelse(is.na(world_map$breaks_coal),"0",world_map$breaks_coal)
world_map$breaks_coal <-  cut(as.numeric(world_map$n_coal_pw),
                              breaks=c(0,10,20,30,40,50,100,700),
                              labels=c("[0;10[", "[10;20[", "[20;30[", "[30;40[","[40;50[", "[50;100[", "[100;700["))
 

map_coal <- ggplot() +
  geom_sf(data = world_map, col="grey", aes(fill=log(n_coal_pw+1)), show.legend = TRUE, size=0.1)+
  theme_minimal() +
  theme(legend.position = "right")+
  labs(x = NULL, 
       y = NULL, 
       title = "Carte du nombre de central à charbon en activité dans le Monde", 
       caption = "Source: World in Data")

map_coal

# Travailler avec un autre grand type de données cartographiques: les données raster
library(raster)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10)
gain(tmax_data) <- 0.1 #must be multiplied by 0.1 to convert back to degrees Celsius
tmax_data$tmax5

plot(tmax_data$tmax5) # température maximum en mai

# Converting the raster object into a dataframe
tmax_data_may_df <- as.data.frame(tmax_data$tmax5, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_may_df) <- c()

ggplot(
  data = tmax_data_may_df,
  aes(x = x, y = y)
) +
  geom_raster(aes(fill = tmax5)) +
  labs(
    title = "Maximum temperature in May",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )
