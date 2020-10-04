#us_states.R

library(sf) #for st_as_sf
library(tidyverse)
library(spdep) #for coloring
library(USAboundaries) #for map base
library(readxl)

my_cities <- read_excel("city names and coordinates.xlsx", sheet = "summary")
head(my_cities)

my_cities2 <- st_as_sf(my_cities, coords = c("long", "lat")) #convert to sf
st_crs(my_cities2) <- 4326 #set coordinate reference system to match usa map

my_states <- us_boundaries(map_date = NULL, type = "state",
                           resolution = c("low", "high"), states = NULL)

my_states2 <- st_as_sf(my_states) #convert to sf
head(my_states2)

my_states2_48 <- my_states2 %>% #remove non-contiguous states
    filter(!(state_name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#nacol from https://stackoverflow.com/questions/28778734/coloring-differently-adjacent-regions-on-a-map-with-ggplot

nacol <- function(spdf){ #color map with 5 non-adjacent colors
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  nunique <- function(x){unique(x[!is.na(x)])}
  np = nrow(spdf) #calculate number of rows; each row is a polygon
  adjl = spdep::poly2nb(spdf) #build a neighbors list
  cols = rep(NA, np) #initialize vector
  cols[1]=1
  nextColour = 2
  
  for(k in 2:np){
    adjcolours = nunique(cols[adjl[[k]]])
    if(length(adjcolours)==0){
      cols[k]=resample(cols[!is.na(cols)],1)
    }else{
      avail = setdiff(nunique(cols), nunique(adjcolours))
      if(length(avail)==0){
        cols[k]=nextColour
        nextColour=nextColour+1
      }else{
        cols[k]=resample(avail,size=1)
      }
    }
  }
  return(cols)
}
my_states2_48$C = nacol(my_states2_48) #color the states

#all lower 48
ggplot() +
  geom_sf(data = my_states2_48, aes(fill=as.factor(C)), size=1, alpha=0.5)+
  geom_sf(data = my_cities2, fill="white", color="black", size=3, aes(shape=type))+
  scale_fill_brewer(palette = "Set1")+
  theme_void()+
  guides(fill=FALSE)+
  scale_shape_manual(values=c(21, 22, 23, 24))+
  labs(shape="Name:")+
  theme(legend.position = "top")+
  coord_sf()

my_cities2$type2 <- paste(my_cities2$type, "\n", sep="") #adding \n prevents the g's from being cut off

p.final <- 
  ggplot() +
  geom_sf(data = my_states2_48, aes(fill=as.factor(C)), alpha=0.5)+
  geom_sf(data = my_cities2, fill="black", color="white", size=1.5, shape=21)+
  scale_fill_brewer(palette = "Set1")+
  theme_void()+
  guides(fill=FALSE)+
  coord_sf()+
  facet_wrap(~type2)+
  theme(strip.text = element_text(size=14), plot.title = element_text(hjust = 0.5, size=20))+
  ggtitle("Locations of Cities, Towns, and Other Communities Named\n")

p.final

ggsave("final.png", p.final, width=7.5, height=5, units="in")
