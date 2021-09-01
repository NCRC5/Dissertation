#install.packages("igraph")

#library(devtools)
# coordinate transformation package geoChina developed by Jun Cai
# https://github.com/qlycool/geoChina
#devtools::install_github("qlycool/geoChina")

library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(RColorBrewer)
library(spatstat)
library(igraph)
library(geoChina)
library(mapboxapi)
library(corrplot)

#Sys.setlocale(category = "LC_ALL", locale = "Chinese")


########### Accessibility of public facilities in Garze ###########
# Use Mapbox API
MY_token = "pk.eyJ1Ijoia3lsaW5sZWkiLCJhIjoiY2tzaHFoam9wMXgxYjJvcWt5YnZ1ajZ3cyJ9.xRX24vphoCzG-OI2Q-saMA"

# Read school data
Garze_schools <- read_excel('data/Garze_schools.xlsx', 1)

# Coordinate Transformation
for(s in 1:nrow(Garze_schools)){
  # transform the coordinates to WGS84
  poi_wgs <- bd2wgs(Garze_schools$BD_lat[s], Garze_schools$BD_lon[s])
  
  Garze_schools$WGS_lon[s] <- poi_wgs$lng
  Garze_schools$WGS_lat[s] <- poi_wgs$lat
}

write.csv(Garze_schools, 'data/Garze_schools_WGS.csv')

# Read hospital data
Garze_hospitals <- read_excel('data/Garze_hospitals.xlsx', 1)

# Coordinate Transformation
for(h in 1:nrow(Garze_hospitals)){
  # transform the coordinates to WGS84
  poi_wgs <- bd2wgs(Garze_hospitals$BD_lat[h], Garze_hospitals$BD_lon[h])
  
  # store in SZ_mi_xlsx
  Garze_hospitals$WGS_lon[h] <- poi_wgs$lng
  Garze_hospitals$WGS_lat[h] <- poi_wgs$lat
}

write.csv(Garze_hospitals, 'data/Garze_hospitals_WGS.csv')


########### Centrality of all counties in Sichuan ###########

# load Sixhuan scope(county level)
SC_county <- st_read('data/Sichuan_county_4572.shp')

# load road networks in 2014 to 2019
SC_roads_14 <- st_read('data/Sichuan_roads_clean_14.shp')
SC_roads_15 <- st_read('data/Sichuan_roads_clean_15.shp')
SC_roads_16 <- st_read('data/Sichuan_roads_clean_16.shp')
SC_roads_17 <- st_read('data/Sichuan_roads_clean_17.shp')
SC_roads_18 <- st_read('data/Sichuan_roads_clean_18.shp')
SC_roads_19 <- st_read('data/Sichuan_roads_clean_19.shp')

# load nodes of road networks
SC_nodes_14 <- st_read('data/Sichuan_roads_nodes_14.shp')
SC_nodes_15 <- st_read('data/Sichuan_roads_nodes_15.shp')
SC_nodes_16 <- st_read('data/Sichuan_roads_nodes_16.shp')
SC_nodes_17 <- st_read('data/Sichuan_roads_nodes_17.shp')
SC_nodes_18 <- st_read('data/Sichuan_roads_nodes_18.shp')
SC_nodes_19 <- st_read('data/Sichuan_roads_nodes_19.shp')

SC_nodes_14_df <- data.frame(SC_nodes_14$num)
SC_nodes_15_df <- data.frame(SC_nodes_15$num)
SC_nodes_16_df <- data.frame(SC_nodes_16$num)
SC_nodes_17_df <- data.frame(SC_nodes_17$num)
SC_nodes_18_df <- data.frame(SC_nodes_18$num)
SC_nodes_19_df <- data.frame(SC_nodes_19$num)

# create graph
SC_graph_14 <- data.frame(from = SC_roads_14$i,
                          to = SC_roads_14$j,
                          weight = SC_roads_14$length)
SC_graph_14 <- graph_from_data_frame(SC_graph_14, directed = FALSE, vertices = SC_nodes_14_df)

SC_graph_15 <- data.frame(from = SC_roads_15$i,
                          to = SC_roads_15$j,
                          weight = SC_roads_15$length)
SC_graph_15 <- graph_from_data_frame(SC_graph_15, directed = FALSE, vertices = SC_nodes_15_df)

SC_graph_16 <- data.frame(from = SC_roads_16$i,
                          to = SC_roads_16$j,
                          weight = SC_roads_16$length)
SC_graph_16 <- graph_from_data_frame(SC_graph_16, directed = FALSE, vertices = SC_nodes_16_df)

SC_graph_17 <- data.frame(from = SC_roads_17$i,
                          to = SC_roads_17$j,
                          weight = SC_roads_17$length)
SC_graph_17 <- graph_from_data_frame(SC_graph_17, directed = FALSE, vertices = SC_nodes_17_df)

SC_graph_18 <- data.frame(from = SC_roads_18$i,
                          to = SC_roads_18$j,
                          weight = SC_roads_18$length)
SC_graph_18 <- graph_from_data_frame(SC_graph_18, directed = FALSE, vertices = SC_nodes_18_df)

SC_graph_19 <- data.frame(from = SC_roads_19$i,
                          to = SC_roads_19$j,
                          weight = SC_roads_19$length)
SC_graph_19 <- graph_from_data_frame(SC_graph_19, directed = FALSE, vertices = SC_nodes_19_df)

# calculate
SC_between_14 <- betweenness(SC_graph_14, v = V(SC_graph_14), directed = FALSE, normalized = FALSE)
SC_close_14 <- closeness(SC_graph_14, vids = V(SC_graph_14), normalized = FALSE)
SC_degree_14 <- degree(SC_graph_14, v = V(SC_graph_14), normalized = FALSE)

SC_between_15 <- betweenness(SC_graph_15, v = V(SC_graph_15), directed = FALSE, normalized = FALSE)
SC_close_15 <- closeness(SC_graph_15, vids = V(SC_graph_15), normalized = FALSE)
SC_degree_15 <- degree(SC_graph_15, v = V(SC_graph_15), normalized = FALSE)

SC_between_16 <- betweenness(SC_graph_16, v = V(SC_graph_16), directed = FALSE, normalized = FALSE)
SC_close_16 <- closeness(SC_graph_16, vids = V(SC_graph_16), normalized = FALSE)
SC_degree_16 <- degree(SC_graph_16, v = V(SC_graph_16), normalized = FALSE)

SC_between_17 <- betweenness(SC_graph_17, v = V(SC_graph_17), directed = FALSE, normalized = FALSE)
SC_close_17 <- closeness(SC_graph_17, vids = V(SC_graph_17), normalized = FALSE)
SC_degree_17 <- degree(SC_graph_17, v = V(SC_graph_17), normalized = FALSE)

SC_between_18 <- betweenness(SC_graph_18, v = V(SC_graph_18), directed = FALSE, normalized = FALSE)
SC_close_18 <- closeness(SC_graph_18, vids = V(SC_graph_18), normalized = FALSE)
SC_degree_18 <- degree(SC_graph_18, v = V(SC_graph_18), normalized = FALSE)

SC_between_19 <- betweenness(SC_graph_19, v = V(SC_graph_19), directed = FALSE, normalized = FALSE)
SC_close_19 <- closeness(SC_graph_19, vids = V(SC_graph_19), normalized = FALSE)
SC_degree_19 <- degree(SC_graph_19, v = V(SC_graph_19), normalized = FALSE)

# merge centrality to nodes
SC_nodes_14$between <- SC_between_14
SC_nodes_14$close <- SC_close_14
SC_nodes_14$degree <- SC_degree_14

SC_nodes_15$between <- SC_between_15
SC_nodes_15$close <- SC_close_15
SC_nodes_15$degree <- SC_degree_15

SC_nodes_16$between <- SC_between_16
SC_nodes_16$close <- SC_close_16
SC_nodes_16$degree <- SC_degree_16

SC_nodes_17$between <- SC_between_17
SC_nodes_17$close <- SC_close_17
SC_nodes_17$degree <- SC_degree_17

SC_nodes_18$between <- SC_between_18
SC_nodes_18$close <- SC_close_18
SC_nodes_18$degree <- SC_degree_18

SC_nodes_19$between <- SC_between_19
SC_nodes_19$close <- SC_close_19
SC_nodes_19$degree <- SC_degree_19

# max-min normalize closeness and regularization betweenness
SC_nodes_14 <- SC_nodes_14 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

SC_nodes_15 <- SC_nodes_15 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

SC_nodes_16 <- SC_nodes_16 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

SC_nodes_17 <- SC_nodes_17 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

SC_nodes_18 <- SC_nodes_18 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

SC_nodes_19 <- SC_nodes_19 %>%
  mutate(., between_log = log10(between + 1)) %>%
  mutate(., close_nor = (close - min(close))/(max(close) - min(close)))

for(c in 1:nrow(SC_county)){
  county <- SC_county[c,]
  
  county_nodes_14 <- SC_nodes_14[county,]
  county_nodes_15 <- SC_nodes_15[county,]
  county_nodes_16 <- SC_nodes_16[county,]
  county_nodes_17 <- SC_nodes_17[county,]
  county_nodes_18 <- SC_nodes_18[county,]
  county_nodes_19 <- SC_nodes_19[county,]
  
  SC_county$nodes_14[c] <- nrow(county_nodes_14)
  SC_county$nodes_15[c] <- nrow(county_nodes_15)
  SC_county$nodes_16[c] <- nrow(county_nodes_16)
  SC_county$nodes_17[c] <- nrow(county_nodes_17)
  SC_county$nodes_18[c] <- nrow(county_nodes_18)
  SC_county$nodes_19[c] <- nrow(county_nodes_19)
  
  SC_county$betweenLog_avg_14[c] <- mean(county_nodes_14$between_log)
  SC_county$closeNor_avg_14[c] <- mean(county_nodes_14$close_nor)
  SC_county$degree_avg_14[c] <- mean(county_nodes_14$degree)
  
  SC_county$betweenLog_avg_15[c] <- mean(county_nodes_15$between_log)
  SC_county$closeNor_avg_15[c] <- mean(county_nodes_15$close_nor)
  SC_county$degree_avg_15[c] <- mean(county_nodes_15$degree)
  
  SC_county$betweenLog_avg_16[c] <- mean(county_nodes_16$between_log)
  SC_county$closeNor_avg_16[c] <- mean(county_nodes_16$close_nor)
  SC_county$degree_avg_16[c] <- mean(county_nodes_16$degree)
  
  SC_county$betweenLog_avg_17[c] <- mean(county_nodes_17$between_log)
  SC_county$closeNor_avg_17[c] <- mean(county_nodes_17$close_nor)
  SC_county$degree_avg_17[c] <- mean(county_nodes_17$degree)
  
  SC_county$betweenLog_avg_18[c] <- mean(county_nodes_18$between_log)
  SC_county$closeNor_avg_18[c] <- mean(county_nodes_18$close_nor)
  SC_county$degree_avg_18[c] <- mean(county_nodes_18$degree)
  
  SC_county$betweenLog_avg_19[c] <- mean(county_nodes_19$between_log)
  SC_county$closeNor_avg_19[c] <- mean(county_nodes_19$close_nor)
  SC_county$degree_avg_19[c] <- mean(county_nodes_19$degree)
}

Sichuan_centrality <- subset(SC_county,
                             select = c(PAC,
                                        nodes_14, betweenLog_avg_14, closeNor_avg_14, degree_avg_14,
                                        nodes_15, betweenLog_avg_15, closeNor_avg_15, degree_avg_15,
                                        nodes_16, betweenLog_avg_16, closeNor_avg_16, degree_avg_16,
                                        nodes_17, betweenLog_avg_17, closeNor_avg_17, degree_avg_17,
                                        nodes_18, betweenLog_avg_18, closeNor_avg_18, degree_avg_18,
                                        nodes_19, betweenLog_avg_19, closeNor_avg_19, degree_avg_19))

#st_write(Sichuan_centrality, 'data/SC_county_centrality.shp')

#############################################################



########### calculate the shortest distance ###########
# Read government data
governments <- read_excel('data/governments.xlsx', 1)

# Coordinate Transformation
for(g in 1:nrow(governments)){
  # transform the coordinates to WGS84
  poi_wgs <- bd2wgs(governments$BD_lat[g], governments$BD_lon[g])
  
  governments$WGS_lon[g] <- poi_wgs$lng
  governments$WGS_lat[g] <- poi_wgs$lat
}

write.csv(governments, 'data/governments_WGS.csv')

# in QGIS, calculate the shortest distance and save attribute table as xlsx

#######################################################



########### transportation index ###########

# including 3 centrality and shortest distance to Chengdu
# load Garze scope(county level)
Garze_county <- st_read('data/Garze_4572.shp')

# merge needed data
for(c in 1:nrow(Garze_county)){
  PAC_code <- Garze_county$PAC[c]
  
  this_county_centrality <- Sichuan_centrality %>%
    filter(., PAC == PAC_code)
  
  Garze_county$close_14[c] <- this_county_centrality$closeNor_avg_14
  Garze_county$between_14[c] <- this_county_centrality$betweenLog_avg_14
  Garze_county$degree_14[c] <- this_county_centrality$degree_avg_14
  
  Garze_county$close_15[c] <- this_county_centrality$closeNor_avg_15
  Garze_county$between_15[c] <- this_county_centrality$betweenLog_avg_15
  Garze_county$degree_15[c] <- this_county_centrality$degree_avg_15
  
  Garze_county$close_16[c] <- this_county_centrality$closeNor_avg_16
  Garze_county$between_16[c] <- this_county_centrality$betweenLog_avg_16
  Garze_county$degree_16[c] <- this_county_centrality$degree_avg_16
  
  Garze_county$close_17[c] <- this_county_centrality$closeNor_avg_17
  Garze_county$between_17[c] <- this_county_centrality$betweenLog_avg_17
  Garze_county$degree_17[c] <- this_county_centrality$degree_avg_17
  
  Garze_county$close_18[c] <- this_county_centrality$closeNor_avg_18
  Garze_county$between_18[c] <- this_county_centrality$betweenLog_avg_18
  Garze_county$degree_18[c] <- this_county_centrality$degree_avg_18
  
  Garze_county$close_19[c] <- this_county_centrality$closeNor_avg_19
  Garze_county$between_19[c] <- this_county_centrality$betweenLog_avg_19
  Garze_county$degree_19[c] <- this_county_centrality$degree_avg_19
}

Garze_county$centrality_14 <- Garze_county$close_14 * Garze_county$degree_14 * Garze_county$between_14
Garze_county$centrality_15 <- Garze_county$close_15 * Garze_county$degree_15 * Garze_county$between_15
Garze_county$centrality_16 <- Garze_county$close_16 * Garze_county$degree_16 * Garze_county$between_16
Garze_county$centrality_17 <- Garze_county$close_17 * Garze_county$degree_17 * Garze_county$between_17
Garze_county$centrality_18 <- Garze_county$close_18 * Garze_county$degree_18 * Garze_county$between_18
Garze_county$centrality_19 <- Garze_county$close_19 * Garze_county$degree_19 * Garze_county$between_19


# read distance csv
distance_14 <- read_excel('data/Distances_14.xlsx',1)
distance_15 <- read_excel('data/Distances_15.xlsx',1)
distance_16 <- read_excel('data/Distances_16.xlsx',1)
distance_17 <- read_excel('data/Distances_17.xlsx',1)
distance_18 <- read_excel('data/Distances_18.xlsx',1)
distance_19 <- read_excel('data/Distances_19.xlsx',1)

# merge needed data
for(c in 1:nrow(Garze_county)){
  county_name <- Garze_county$NAME[c]
  
  county_dis_14 <- distance_14 %>%
    filter(., destination_id == county_name)
  county_dis_15 <- distance_15 %>%
    filter(., destination_id == county_name)
  county_dis_16 <- distance_16 %>%
    filter(., destination_id == county_name)
  county_dis_17 <- distance_17 %>%
    filter(., destination_id == county_name)
  county_dis_18 <- distance_18 %>%
    filter(., destination_id == county_name)
  county_dis_19 <- distance_19 %>%
    filter(., destination_id == county_name)
  
  Garze_county$distance_14[c] <- county_dis_14$total_cost / 1000
  Garze_county$distance_15[c] <- county_dis_15$total_cost / 1000
  Garze_county$distance_16[c] <- county_dis_16$total_cost / 1000
  Garze_county$distance_17[c] <- county_dis_17$total_cost / 1000
  Garze_county$distance_18[c] <- county_dis_18$total_cost / 1000
  Garze_county$distance_19[c] <- county_dis_19$total_cost / 1000
}

Garze_county$TransIndex_14 <- 100 * Garze_county$centrality_14 / Garze_county$distance_14
Garze_county$TransIndex_15 <- 100 * Garze_county$centrality_15 / Garze_county$distance_15
Garze_county$TransIndex_16 <- 100 * Garze_county$centrality_16 / Garze_county$distance_16
Garze_county$TransIndex_17 <- 100 * Garze_county$centrality_17 / Garze_county$distance_17
Garze_county$TransIndex_18 <- 100 * Garze_county$centrality_18 / Garze_county$distance_18
Garze_county$TransIndex_19 <- 100 * Garze_county$centrality_19 / Garze_county$distance_19

for(c in 1:nrow(Garze_county)){
  this_county <- Garze_county[c,]
  
  this_county_roads_14 <- SC_roads_14[this_county,]
  this_county_roads_15 <- SC_roads_15[this_county,]
  this_county_roads_16 <- SC_roads_16[this_county,]
  this_county_roads_17 <- SC_roads_17[this_county,]
  this_county_roads_18 <- SC_roads_18[this_county,]
  this_county_roads_19 <- SC_roads_19[this_county,]
  
  this_roads_14_len <- sum(st_length(this_county_roads_14))
  this_roads_15_len <- sum(st_length(this_county_roads_15))
  this_roads_16_len <- sum(st_length(this_county_roads_16))
  this_roads_17_len <- sum(st_length(this_county_roads_17))
  this_roads_18_len <- sum(st_length(this_county_roads_18))
  this_roads_19_len <- sum(st_length(this_county_roads_19))
  
  this_roads_ratio_14 <- this_roads_14_len / this_roads_14_len
  this_roads_ratio_15 <- this_roads_15_len / this_roads_14_len
  this_roads_ratio_16 <- this_roads_16_len / this_roads_14_len
  this_roads_ratio_17 <- this_roads_17_len / this_roads_14_len
  this_roads_ratio_18 <- this_roads_18_len / this_roads_14_len
  this_roads_ratio_19 <- this_roads_19_len / this_roads_14_len
  
  Garze_county$roads_ratio_14[c] <- this_roads_ratio_14
  Garze_county$roads_ratio_15[c] <- this_roads_ratio_15
  Garze_county$roads_ratio_16[c] <- this_roads_ratio_16
  Garze_county$roads_ratio_17[c] <- this_roads_ratio_17
  Garze_county$roads_ratio_18[c] <- this_roads_ratio_18
  Garze_county$roads_ratio_19[c] <- this_roads_ratio_19
}


Garze_county$MTransIndex_14 <- Garze_county$TransIndex_14 * Garze_county$roads_ratio_14
Garze_county$MTransIndex_15 <- Garze_county$TransIndex_15 * Garze_county$roads_ratio_15
Garze_county$MTransIndex_16 <- Garze_county$TransIndex_16 * Garze_county$roads_ratio_16
Garze_county$MTransIndex_17 <- Garze_county$TransIndex_17 * Garze_county$roads_ratio_17
Garze_county$MTransIndex_18 <- Garze_county$TransIndex_18 * Garze_county$roads_ratio_18
Garze_county$MTransIndex_19 <- Garze_county$TransIndex_19 * Garze_county$roads_ratio_19

############################################


########### education & health index ###########
# read statistical data of Garze
Garze_stat_1419 <- read_excel('data/Garze_data_14-19.xlsx', 1)

Garze_stat_14 <- Garze_stat_1419 %>%
  filter(., year == 2014)
Garze_stat_15 <- Garze_stat_1419 %>%
  filter(., year == 2015)
Garze_stat_16 <- Garze_stat_1419 %>%
  filter(., year == 2016)
Garze_stat_17 <- Garze_stat_1419 %>%
  filter(., year == 2017)
Garze_stat_18 <- Garze_stat_1419 %>%
  filter(., year == 2018)
Garze_stat_19 <- Garze_stat_1419 %>%
  filter(., year == 2019)

Garze_county$PERate_14 <- Garze_stat_14$primary_enrollmen_rate
Garze_county$PERate_15 <- Garze_stat_15$primary_enrollmen_rate
Garze_county$PERate_16 <- Garze_stat_16$primary_enrollmen_rate
Garze_county$PERate_17 <- Garze_stat_17$primary_enrollmen_rate
Garze_county$PERate_18 <- Garze_stat_18$primary_enrollmen_rate
Garze_county$PERate_19 <- Garze_stat_19$primary_enrollmen_rate

Garze_county$PJRate_14 <- Garze_stat_14$junior_enrollment / Garze_stat_14$primary_graduate
Garze_county$PJRate_15 <- Garze_stat_15$junior_enrollment / Garze_stat_15$primary_graduate
Garze_county$PJRate_16 <- Garze_stat_16$junior_enrollment / Garze_stat_16$primary_graduate
Garze_county$PJRate_17 <- Garze_stat_17$junior_enrollment / Garze_stat_17$primary_graduate
Garze_county$PJRate_18 <- Garze_stat_18$junior_enrollment / Garze_stat_18$primary_graduate
Garze_county$PJRate_19 <- Garze_stat_19$junior_enrollment / Garze_stat_19$primary_graduate

Garze_county$BedRate_14 <- (Garze_stat_14$medical_beds / Garze_stat_14$pop) * 1000
Garze_county$BedRate_15 <- (Garze_stat_15$medical_beds / Garze_stat_15$pop) * 1000
Garze_county$BedRate_16 <- (Garze_stat_16$medical_beds / Garze_stat_16$pop) * 1000
Garze_county$BedRate_17 <- (Garze_stat_17$medical_beds / Garze_stat_17$pop) * 1000
Garze_county$BedRate_18 <- (Garze_stat_18$medical_beds / Garze_stat_18$pop) * 1000
Garze_county$BedRate_19 <- (Garze_stat_19$medical_beds / Garze_stat_19$pop) * 1000

# load Garze roads and isochrone-covered roads
school_area_14 <- st_read('data/school_area_14.shp')
school_area_15 <- st_read('data/school_area_15.shp')
school_area_16 <- st_read('data/school_area_16.shp')
school_area_17 <- st_read('data/school_area_17.shp')
school_area_18 <- st_read('data/school_area_18.shp')
school_area_19 <- st_read('data/school_area_19.shp')

hospital_area_14 <- st_read('data/hospital_area_14.shp')
hospital_area_15 <- st_read('data/hospital_area_15.shp')
hospital_area_16 <- st_read('data/hospital_area_16.shp')
hospital_area_17 <- st_read('data/hospital_area_17.shp')
hospital_area_18 <- st_read('data/hospital_area_18.shp')
hospital_area_19 <- st_read('data/hospital_area_19.shp')

for(c in 1:nrow(Garze_county)){
  county <- Garze_county[c,]
  
  county_school_14 <- school_area_14[county,]
  county_school_15 <- school_area_15[county,]
  county_school_16 <- school_area_16[county,]
  county_school_17 <- school_area_17[county,]
  county_school_18 <- school_area_18[county,]
  county_school_19 <- school_area_19[county,]
  
  school_14_len <- sum(st_length(county_school_14))
  school_15_len <- sum(st_length(county_school_15))
  school_16_len <- sum(st_length(county_school_16))
  school_17_len <- sum(st_length(county_school_17))
  school_18_len <- sum(st_length(county_school_18))
  school_19_len <- sum(st_length(county_school_19))
  
  school_rate_14 = school_14_len / school_14_len
  school_rate_15 = school_15_len / school_14_len
  school_rate_16 = school_16_len / school_14_len
  school_rate_17 = school_17_len / school_14_len
  school_rate_18 = school_18_len / school_14_len
  school_rate_19 = school_19_len / school_14_len
  
  Garze_county$SchCoverRate_14[c] <- school_rate_14
  Garze_county$SchCoverRate_15[c] <- school_rate_15
  Garze_county$SchCoverRate_16[c] <- school_rate_16
  Garze_county$SchCoverRate_17[c] <- school_rate_17
  Garze_county$SchCoverRate_18[c] <- school_rate_18
  Garze_county$SchCoverRate_19[c] <- school_rate_19
}

for(c in 5:nrow(Garze_county)){
  county <- Garze_county[c,]
  
  county_hospital_14 <- hospital_area_14[county,]
  county_hospital_15 <- hospital_area_15[county,]
  county_hospital_16 <- hospital_area_16[county,]
  county_hospital_17 <- hospital_area_17[county,]
  county_hospital_18 <- hospital_area_18[county,]
  county_hospital_19 <- hospital_area_19[county,]
  
  hospital_14_len <- sum(st_length(county_hospital_14))
  hospital_15_len <- sum(st_length(county_hospital_15))
  hospital_16_len <- sum(st_length(county_hospital_16))
  hospital_17_len <- sum(st_length(county_hospital_17))
  hospital_18_len <- sum(st_length(county_hospital_18))
  hospital_19_len <- sum(st_length(county_hospital_19))
  
  hospital_rate_14 = hospital_14_len / hospital_14_len
  hospital_rate_15 = hospital_15_len / hospital_14_len
  hospital_rate_16 = hospital_16_len / hospital_14_len
  hospital_rate_17 = hospital_17_len / hospital_14_len
  hospital_rate_18 = hospital_18_len / hospital_14_len
  hospital_rate_19 = hospital_19_len / hospital_14_len
  
  Garze_county$HosCoverRate_14[c] <- hospital_rate_14
  Garze_county$HosCoverRate_15[c] <- hospital_rate_15
  Garze_county$HosCoverRate_16[c] <- hospital_rate_16
  Garze_county$HosCoverRate_17[c] <- hospital_rate_17
  Garze_county$HosCoverRate_18[c] <- hospital_rate_18
  Garze_county$HosCoverRate_19[c] <- hospital_rate_19
}

Garze_county$EduIndex_14 <- (Garze_county$SchCoverRate_14 * Garze_county$PERate_14 * Garze_county$PJRate_14)
Garze_county$EduIndex_15 <- (Garze_county$SchCoverRate_15 * Garze_county$PERate_15 * Garze_county$PJRate_15)
Garze_county$EduIndex_16 <- (Garze_county$SchCoverRate_16 * Garze_county$PERate_16 * Garze_county$PJRate_16)
Garze_county$EduIndex_17 <- (Garze_county$SchCoverRate_17 * Garze_county$PERate_17 * Garze_county$PJRate_17)
Garze_county$EduIndex_18 <- (Garze_county$SchCoverRate_18 * Garze_county$PERate_18 * Garze_county$PJRate_18)
Garze_county$EduIndex_19 <- (Garze_county$SchCoverRate_19 * Garze_county$PERate_19 * Garze_county$PJRate_19)

Garze_county$HealIndex_14 <- Garze_county$HosCoverRate_14 * Garze_county$BedRate_14
Garze_county$HealIndex_15 <- Garze_county$HosCoverRate_15 * Garze_county$BedRate_15
Garze_county$HealIndex_16 <- Garze_county$HosCoverRate_16 * Garze_county$BedRate_16
Garze_county$HealIndex_17 <- Garze_county$HosCoverRate_17 * Garze_county$BedRate_17
Garze_county$HealIndex_18 <- Garze_county$HosCoverRate_18 * Garze_county$BedRate_18
Garze_county$HealIndex_19 <- Garze_county$HosCoverRate_19 * Garze_county$BedRate_19

################################################

CPI_1419 = c(1, 1.0239, 1.0557, 1.0739, 1.0961, 1.1184)

########### life standard index ###########
# income/expend ratio



Garze_county$IERate_14 <- (Garze_stat_14$urban_income * Garze_stat_14$urbanization_rate + Garze_stat_14$rural_income * (1 - Garze_stat_14$urbanization_rate)) / (Garze_stat_14$urban_expend * Garze_stat_14$urbanization_rate + Garze_stat_14$rural_expend * (1 - Garze_stat_14$urbanization_rate))
Garze_county$IERate_15 <- (Garze_stat_15$urban_income * Garze_stat_15$urbanization_rate + Garze_stat_15$rural_income * (1 - Garze_stat_15$urbanization_rate)) / (Garze_stat_15$urban_expend * Garze_stat_15$urbanization_rate + Garze_stat_15$rural_expend * (1 - Garze_stat_15$urbanization_rate))
Garze_county$IERate_16 <- (Garze_stat_16$urban_income * Garze_stat_16$urbanization_rate + Garze_stat_16$rural_income * (1 - Garze_stat_16$urbanization_rate)) / (Garze_stat_16$urban_expend * Garze_stat_16$urbanization_rate + Garze_stat_16$rural_expend * (1 - Garze_stat_16$urbanization_rate))
Garze_county$IERate_17 <- (Garze_stat_17$urban_income * Garze_stat_17$urbanization_rate + Garze_stat_17$rural_income * (1 - Garze_stat_17$urbanization_rate)) / (Garze_stat_17$urban_expend * Garze_stat_17$urbanization_rate + Garze_stat_17$rural_expend * (1 - Garze_stat_17$urbanization_rate))
Garze_county$IERate_18 <- (Garze_stat_18$urban_income * Garze_stat_18$urbanization_rate + Garze_stat_18$rural_income * (1 - Garze_stat_18$urbanization_rate)) / (Garze_stat_18$urban_expend * Garze_stat_18$urbanization_rate + Garze_stat_18$rural_expend * (1 - Garze_stat_18$urbanization_rate))
Garze_county$IERate_19 <- (Garze_stat_19$urban_income * Garze_stat_19$urbanization_rate + Garze_stat_19$rural_income * (1 - Garze_stat_19$urbanization_rate)) / (Garze_stat_19$urban_expend * Garze_stat_19$urbanization_rate + Garze_stat_19$rural_expend * (1 - Garze_stat_19$urbanization_rate))

Garze_county$Engel_14 <- (Garze_stat_14$urban_engel * Garze_stat_14$urbanization_rate + Garze_stat_14$rural_engel * (1 - Garze_stat_14$urbanization_rate)) / 2
Garze_county$Engel_15 <- (Garze_stat_15$urban_engel * Garze_stat_15$urbanization_rate + Garze_stat_15$rural_engel * (1 - Garze_stat_15$urbanization_rate)) / 2
Garze_county$Engel_16 <- (Garze_stat_16$urban_engel * Garze_stat_16$urbanization_rate + Garze_stat_16$rural_engel * (1 - Garze_stat_16$urbanization_rate)) / 2
Garze_county$Engel_17 <- (Garze_stat_17$urban_engel * Garze_stat_17$urbanization_rate + Garze_stat_17$rural_engel * (1 - Garze_stat_17$urbanization_rate)) / 2
Garze_county$Engel_18 <- (Garze_stat_18$urban_engel * Garze_stat_18$urbanization_rate + Garze_stat_18$rural_engel * (1 - Garze_stat_18$urbanization_rate)) / 2
Garze_county$Engel_19 <- (Garze_stat_19$urban_engel * Garze_stat_19$urbanization_rate + Garze_stat_19$rural_engel * (1 - Garze_stat_19$urbanization_rate)) / 2

# Savings deposits change index
savings_deposits_ratio_14 = Garze_stat_14$savings_deposits / Garze_stat_14$savings_deposits
savings_deposits_ratio_15 = Garze_stat_15$savings_deposits / Garze_stat_14$savings_deposits
savings_deposits_ratio_16 = Garze_stat_16$savings_deposits / Garze_stat_14$savings_deposits
savings_deposits_ratio_17 = Garze_stat_17$savings_deposits / Garze_stat_14$savings_deposits
savings_deposits_ratio_18 = Garze_stat_18$savings_deposits / Garze_stat_14$savings_deposits
savings_deposits_ratio_19 = Garze_stat_19$savings_deposits / Garze_stat_14$savings_deposits

Garze_county$LifeIndex_14 <- (Garze_county$IERate_14 / Garze_county$Engel_14) * 100 * savings_deposits_ratio_14
Garze_county$LifeIndex_15 <- (Garze_county$IERate_15 / Garze_county$Engel_15) * 100 * savings_deposits_ratio_15
Garze_county$LifeIndex_16 <- (Garze_county$IERate_16 / Garze_county$Engel_16) * 100 * savings_deposits_ratio_16
Garze_county$LifeIndex_17 <- (Garze_county$IERate_17 / Garze_county$Engel_17) * 100 * savings_deposits_ratio_17
Garze_county$LifeIndex_18 <- (Garze_county$IERate_18 / Garze_county$Engel_18) * 100 * savings_deposits_ratio_18
Garze_county$LifeIndex_19 <- (Garze_county$IERate_19 / Garze_county$Engel_19) * 100 * savings_deposits_ratio_19

Garze_county$CLifeIndex_14 <- (Garze_county$IERate_14 / Garze_county$Engel_14) * 100 * savings_deposits_ratio_14 / CPI_1419[1]
Garze_county$CLifeIndex_15 <- (Garze_county$IERate_15 / Garze_county$Engel_15) * 100 * savings_deposits_ratio_15 / CPI_1419[2]
Garze_county$CLifeIndex_16 <- (Garze_county$IERate_16 / Garze_county$Engel_16) * 100 * savings_deposits_ratio_16 / CPI_1419[3]
Garze_county$CLifeIndex_17 <- (Garze_county$IERate_17 / Garze_county$Engel_17) * 100 * savings_deposits_ratio_17 / CPI_1419[4]
Garze_county$CLifeIndex_18 <- (Garze_county$IERate_18 / Garze_county$Engel_18) * 100 * savings_deposits_ratio_18 / CPI_1419[5]
Garze_county$CLifeIndex_19 <- (Garze_county$IERate_19 / Garze_county$Engel_19) * 100 * savings_deposits_ratio_19 / CPI_1419[6]

###########################################

st_write(Garze_county, 'data/Garze_data.shp')
# Through comparison, it is found that excel will be more convenient for data connection in the next step

###########################################
Garze_stat_result <- read_excel('data/Garze_data_1419_results.xlsx', 1)

Garze_stat_result <- Garze_stat_result %>%
  mutate(.,
         GDP_per = gdp * 10000 / pop,
         Income = urban_income * U + rural_income * (1 - U))

Garze_stat_cor <- Garze_stat_result %>%
  dplyr::select(., -c(urban_income, rural_income, U, county, year, pop, gdp))

GarzeCorMatrix <- cor(Garze_stat_cor)

corrplot(GarzeCorMatrix, type = 'upper', method = 'square', tl.pos = 'tp', tl.col = "black")
corrplot(GarzeCorMatrix, add = TRUE, type = 'lower', method = 'number', tl.col = "black",
         diag = FALSE, tl.pos = 'n', cl.pos = 'n', tl.srt = 10)

write.csv(GarzeCorMatrix, 'data/Correlation_Matrix.csv')

#############################################################################