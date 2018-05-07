usethis::use_git()
#Income Data Link
#http://stat.data.abs.gov.au/
#Economy -> Finance -> Household Income -> Census 2016, Total Household Income (Weekly) by Household Composition (LGA)
#Customise layout(): 
#Page -  (Census Year, Household Composition)
#Row - (State, Region, Geography Level)
#Column : Total Household Income Weekly
#The column "Total Household income" was split into Weekly and Annual Income in excel

#Shapefile ESRI Data Link
#http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
#Name of the file (Zip) to download: Local Government Areas ASGS Ed 2016 Digital Boundaries in ESRI Shapefile Format 

library(tidyverse)
library(readr)

#rename state in Attribute Dataset
IncomeData <- readr::read_csv('Data/Income_Data.csv') %>%
  mutate(State = case_when(
    State == 'New South Wales' ~ 'NSW',
    State == 'Victoria' ~ 'VIC',
    State == 'Queensland' ~ 'QLD',
    State == 'South Australia' ~ 'SA',
    State == 'Western Australia' ~ 'WA',
    State == 'Tasmania' ~ 'TAS',
    State == 'Northern Territory' ~ 'NT',
    State == 'Australian Capital Territory' ~ 'ACT',
    TRUE ~ as.character(State))) %>% filter(Weekly_Household_Income =="$4,500-$4,999"|Household_Composition == "Total Households")
#Might need to change Column names before this step


IncomeData %>%
  group_by(LGA_2016) %>%
  summarise(NOP_8000AUDpweek = sum(`Value`)) -> LGA_Income


## LGA
#Reading OGR vector maps into Spatial objects
#The Geospatial Data Abstraction Library (GDAL) is a computer software library for reading and writing raster and vector geospatial data formats
#The rgdal package has been around for more than a decade and provides bindings to the incredible Geospatial Data Abstraction Library (GDAL) for reading, writing and converting between spatial formats.
LGAShp <- rgdal::readOGR('LGA_shapefiles',layer="LGA_2016_AUST")
# for topologically-aware polygon simplification
LGA_subset <- rmapshaper::ms_simplify(LGAShp, keep = 0.05)
save(LGA_subset,file = "Data/GA_subset.Rda")

#Creating k nearest Neighbours
# knn2nb converts a knn object returned by knearneigh into a neighbours list of class nb with a list of integer vectors containing neighbour region number ids.
coords <-coordinates(LGA_subset)
length(coords)
IDs <- row.names(as(LGA_subset, "data.frame"))
LGA_nn_1 <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)
LGA_nn_2 <- knn2nb(knearneigh(coords, k = 2), row.names = IDs)
LGA_nn_3 <- knn2nb(knearneigh(coords, k = 3), row.names = IDs)
LGA_nn_4 <- knn2nb(knearneigh(coords, k = 4), row.names = IDs)

#Plotting the neighbours

plot(LGA_nn_1,coords)
plot(LGA_nn_2,coords)
plot(LGA_nn_3,coords)
plot(LGA_nn_3,coords)

# Checking if the neighbours object is symmetric

nb_list <- list(k1 = LGA_nn_1, k2 = LGA_nn_2, k3=LGA_nn_1, k4 = LGA_nn_1)
sapply(nb_list, function(x) is.symmetric.nb(x, verbose = FALSE,force = TRUE))
#If need be, k-nearest neighbour objects can be made symmetrical using the make.sym.nb function
LGA_nn_1 <- make.sym.nb(LGA_nn_1)

#To calculate a list of vectors of distances
#corresponding to the neighbour object for first nearest neighbours. The
#greatest value will be the minimum distance needed to make sure that all the
#areas are linked to at least one neighbour
dsts <- unlist(nbdists(LGA_nn_1, coords))
summary(dsts)
max_1nn <- max(dsts)
max_1nn

#nb2listw function takes a neighbours list object and converts it into a weights object
#For style="W", the weights vary between unity divided by the largest and
#smallest numbers of neighbours, and the sums of weights for each areal entity
#are unity
#B is the basic binary coding

Weights_W <- nb2listw(LGA_nn_1,style="W")
Weights_B <- nb2listw(LGA_nn_1,style="B")
Weights_C <- nb2listw(LGA_nn_1,style= "C")
Weights_U <- nb2listw(LGA_nn_1,style="U")
Weights_S <- nb2listw(LGA_nn_1,style="S")

names(Weights_W)
names(attributes(Weights_W))
1/rev(range(card(Weights_W$neighbours)))
summary(unlist(Sy0_lw_W$weights))
set.seed(987654)
n <- length(LGA_nn_1)
uncorr_x <- rnorm(n)
rho <- 0.5
autocorr_x <- invIrW(Weights_W, rho) %*% uncorr_x


LGA_data <- LGA_subset@data
LGA_data$id <- row.names(LGA_data)
LGA_data$LGA_CODE16 <- as.integer(as.character(LGA_data$LGA_CODE16))
LGA_map <- ggplot2::fortify(LGA_subset)
LGA_map$group <- paste("g",LGA_map$group,sep=".")
LGA_map$piece <- paste("p",LGA_map$piece,sep=".")
library(ggplot2)
ggplot(LGA_map) + geom_polygon(aes(long, lat, group = group), colour = 'grey')

moran.plot()

LGA_data %>%
  select(id, LGA_CODE16, LGA_NAME16) %>%
  rename(name = LGA_NAME16) %>%
  right_join(IncomeData,by=c("LGA_CODE16" = "LGA_2016")) %>%
  right_join(LGA_map)-> LGA_map_2


ggplot(LGA_map_2) + geom_polygon(aes(long, lat, group = group,fill=Value))

##With LGA_map_2

coords= LGA_map_2[,c("long","lat")]
length(coords)
IDs <- row.names(as(LGA_map_2, "data.frame"))
LGA_nn_1 <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)
LGA_nn_2 <- knn2nb(knearneigh(coords, k = 2), row.names = IDs)
LGA_nn_3 <- knn2nb(knearneigh(coords, k = 3), row.names = IDs)
LGA_nn_4 <- knn2nb(knearneigh(coords, k = 4), row.names = IDs)

#Plotting the neighbours

plot(LGA_nn_1,coords)
plot(LGA_nn_2,coords)
plot(LGA_nn_3,coords)
plot(LGA_nn_3,coords)

# Checking if the neighbours object is symmetric

nb_list <- list(k1 = LGA_nn_1, k2 = LGA_nn_2, k3=LGA_nn_1, k4 = LGA_nn_1)
sapply(nb_list, function(x) is.symmetric.nb(x, verbose = FALSE,force = TRUE))
#If need be, k-nearest neighbour objects can be made symmetrical using the make.sym.nb function
LGA_nn_1 <- make.sym.nb(LGA_nn_1)

#To calculate a list of vectors of distances
#corresponding to the neighbour object for first nearest neighbours. The
#greatest value will be the minimum distance needed to make sure that all the
#areas are linked to at least one neighbour
dsts <- unlist(nbdists(LGA_nn_1, coords))
summary(dsts)
max_1nn <- max(dsts)
max_1nn

#nb2listw function takes a neighbours list object and converts it into a weights object
#For style="W", the weights vary between unity divided by the largest and
#smallest numbers of neighbours, and the sums of weights for each areal entity
#are unity
#B is the basic binary coding

Weights_W <- nb2listw(LGA_nn_1,style="W")
Weights_B <- nb2listw(LGA_nn_1,style="B")
Weights_C <- nb2listw(LGA_nn_1,style= "C")
Weights_U <- nb2listw(LGA_nn_1,style="U")
Weights_S <- nb2listw(LGA_nn_1,style="S")

names(Weights_W)
names(attributes(Weights_W))
1/rev(range(card(Weights_W$neighbours)))
summary(unlist(Sy0_lw_W$weights))
set.seed(987654)
n <- length(LGA_nn_1)
uncorr_x <- rnorm(n)
rho <- 0.5
autocorr_x <- invIrW(Weights_W, rho) %*% uncorr_x
