#library(sf) #for st_as_sf
#library(spdep) #for coloring

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