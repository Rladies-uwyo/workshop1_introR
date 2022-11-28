#R workshop Nov 28, 2022
# code for apply and lapply

#packages
library(moveVis)
library(move)
library(sf)
library(tidyr)
library(dplyr)
library(lubridate)

############################
# apply functions

# take a look at the apply functions using iris data
View(iris)

#iris dataset has a species column, which we can't summarize over without some manipulation,
#so we will do our summaries over the first four columns only

#returns a list
apply(iris[, 1:4], 2, summary)  #the 2 here states that this summary function should be applied over columns, 1 would = rows

#returns vector or matrix
sapply(iris[, 1:4], summary)

#returns list
lapply(iris[,1:4], summary)

#will iterate over species, but you can't pass it the whole df
tapply(iris$Sepal.Length, iris$Species, summary)

#to return dataframes, use the following for each apply function
as.data.frame(apply(iris[, 1:4], 2, summary))

as.data.frame(sapply(iris[, 1:4], summary))

do.call(cbind, lapply(iris[,1:4], summary))

#############################################
#examples with the fisher data

#read in data
# loginStored <- movebankLogin(username="MovebankWorkshop", password="genericuserforthisexercise")
# 
# fisher.move <- getMovebankData(study = "Martes pennanti LaPoint New York", 
#                                login = loginStored)
# head(fisher.move)
# 
# fisher.dat <- as(fisher.move, "data.frame")

#get a column of date released for each animal

ids <- unique(fisher.dat$local_identifier)

fisher.dat <- do.call(rbind, lapply(1:length(ids), function(i){
  
  dat <- fisher.dat %>%
    filter(local_identifier == ids[i]) 
  
  dat <- dat %>%
    mutate(releaseDate = min(timestamps))
}))

##########################

#extra examples 

#figure out group size 
library(spatsoc)
library(data.table)

#get out of sf dataframe and into data table
fdt <- data.table::setDT(fisher.dat)

#identify groups of animals by space and time (1 hour either way of gps locs and 100m apart)
group_times(DT = fdt, datetime = 'timestamps', threshold = '5 minutes')

#this will give you a warning because this GPS data is a mess (lots of oddly timed fixes that I didn't feel like cleaning), 
#but for this example, we can just roll with it
group_pts(fdt, threshold = 50, id = 'local_identifier', coords =  c('location_long', 'location_lat'), timegroup = 'timegroup')

#if you want to look at what do you have
#table(fdt$group, fdt$local_identifier)

#create table of group numbers and sizes
x <- data.frame(table(fdt$group))

names(x) <- c("group", "groupSize")
x$group <- as.numeric(x$group)
#unique(x$groupSize)

#join to df
fisher.dat <- left_join(fisher.dat, x)
table(fisher.dat$groupSize)

#group type
fisher.dat$groupType <- do.call(c, lapply(1:nrow(sf), function(e){
  if(fisher.dat$groupSize[e] == 1){
    return("none")
  }else{
    if(fisher.dat$groupSize[e]> 1 & sf$groupSize[e] <= 4){
      return("small")
    }else{
      if(fisher.dat$groupSize[e] > 4){
        return("large")
      }
    }
  }
}))

#########################################
#if you had an sf of group centroids, you could calculate distance to nearest group
fisher.dat$grpCenter <- sapply(1:nrow(fisher.dat), 
                           function(i){
                             min(
                               st_distance(
                                 fisher.dat[i,],
                                 centr[centr$date == fisher.dat$timestamps[i],])
                             )
                           }
)


##################################################
#examples where I've used apply in my code 
#you won't have the data sets for this but thought it might be helpful to see

#data sets are "data" [regular GPS df in sf format] and 
#"fence_dist" [matrix of the distance of each GPS location to each fence polygon; there are 7 fence polygons] 

#to get a column in your dataframe that pulls the closest distance to each fence polygon at each location 
data$dist2fence <- apply(fence_dist, 1, min) # (remember, the 1 specifies that this is by row and not by column) 
data$fenceNumb <- apply(fence_dist, 1, which.min) #this will give you the fence polygon id that is the closest
