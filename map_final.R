library(sf) #for st_as_sf
library(tidyverse)
library(spdep) #for coloring
library(USAboundaries) #for map base
library(readxl) #for read_excel
library(TSP) #for TSP() and solve_TSP()
source("nacol.R") #function for calculating state colors
source("traveling_salesperson_general.R") #generate routes using traveling salesperson algorithm

#get base map
my_states <- us_boundaries(map_date = NULL, type = "state", resolution = "low", states = NULL)

my_states2 <- st_as_sf(my_states) #convert to sf

my_states2_48 <- my_states2 %>% #remove non-contiguous states
    filter(!(state_name %in% c("Alaska", "Hawaii", "Puerto Rico")))

my_states2_48$C = nacol(my_states2_48) #color the states

p.final <- 
  ggplot() +
  geom_sf(data = my_states2_48, aes(fill=as.factor(C)), alpha=0.5)+ #map
  geom_sf(data=salesperson_order3)+ #lines
  geom_sf(data = my_cities2, fill="black", color="white", size=1, shape=21)+ #points
  geom_sf(data=salesperson_order_start2, size=1, shape=21, color="white", fill="green")+
  geom_sf(data=salesperson_order_end2, size=1, shape=21, color="white", fill="red")+
  scale_fill_brewer(palette = "Set1")+
  theme_void()+
  guides(fill=FALSE)+
  coord_sf()+
  facet_wrap(~type2)+
  theme(strip.text = element_text(size=14), plot.title = element_text(hjust = 0.5, size=20))+
  ggtitle("Tours of the Most Common US Place Names\n")

p.final

ggsave("final.png", p.final, width=7, height=5.25, units="in")

