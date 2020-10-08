#library(tidyverse)
#library(readxl)
#library(TSP) #for TSP() and solve_TSP()

#read in city data
my_cities <- 
  read_excel("city names and coordinates.xlsx", sheet = "summary") %>% 
  arrange(type, long, lat)

my_cities2 <- st_as_sf(my_cities, coords = c("long", "lat")) #convert to sf
st_crs(my_cities2) <- 4326 #set coordinate reference system to match usa map
my_cities2$type2 <- paste(my_cities2$type, "\n", sep="") #adding \n prevents the g's from being cut off

#get list of city names
city_names <- unique(my_cities$type)

#initialize data frame
salesperson_order <- data.frame(type=character(),
                                order=numeric(),
                                lat=numeric(),
                                long=numeric())

#calculate routes
for(i in 1:length(city_names)){
  temp_name <- city_names[i]
  temp_data <- #filter data by city name
    my_cities %>% 
    filter(type==temp_name) %>% 
    select(lat, long)
  temp_dist <- dist(temp_data) #make euclidean distance matrix
  temp_tsp <- TSP(temp_dist) #convert to TSP
  temp_tsp2 <- solve_TSP(temp_tsp, two_opt=TRUE, rep=10) #calculate order
  temp_tsp3 <- as.numeric(temp_tsp2) #final order of points
  
  #put it all together
  final_order <- data.frame(type=rep(temp_name, length(temp_tsp3)),
                            order=temp_tsp3,
                            lat=temp_data$lat[temp_tsp3],
                            long=temp_data$long[temp_tsp3],
                            series_order=seq(1, length(temp_tsp3), 1))
  
  salesperson_order <- rbind(salesperson_order, final_order)
}

salesperson_order$type2 <- paste(salesperson_order$type, "\n", sep="") #adding \n prevents the g's from being cut off

#get start and end points for maps
salesperson_order_start <- 
  salesperson_order %>% 
  filter(series_order==1)

salesperson_order_start2 <- st_as_sf(salesperson_order_start, coords = c("long", "lat")) #convert to sf
st_crs(salesperson_order_start2) <- 4326 #set coordinate reference system to match usa map

salesperson_order_end <- 
  salesperson_order %>% 
  group_by(type) %>% 
  filter(series_order==max(series_order))

salesperson_order_end2 <- st_as_sf(salesperson_order_end, coords = c("long", "lat")) #convert to sf
st_crs(salesperson_order_end2) <- 4326 #set coordinate reference system to match usa map

#make paths as sf
salesperson_order2 <- st_as_sf(salesperson_order, coords = c("long", "lat")) #convert to sf
st_crs(salesperson_order2) <- 4326 #set coordinate reference system to match usa map

salesperson_order3 <- #group points into sf paths
  salesperson_order2 %>% 
  group_by(type2) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

#check the calculated paths
ggplot(salesperson_order, aes(x=long, y=lat, group=type))+
  geom_point()+
  geom_path()+
  geom_point(data=salesperson_order_end, aes(x=long, y=lat), color="red")+
  geom_point(data=salesperson_order_start, aes(x=long, y=lat), color="green")+
  facet_wrap(~type)

