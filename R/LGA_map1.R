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
#- http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
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
    TRUE ~ as.character(State))) %>% filter(Weekly_Household_Income =="$1,000-$1,249"|Household_Composition == "Total Households")
#Might need to change Column names before this step


IncomeData %>%
  group_by(LGA_2016) %>%
  summarise(NOP_8000AUDpweek = sum(`Value`)) -> LGA_Income

#line plot of distribution of number of households across LGAs
IncomeData_r %>%
  group_by(LGA_2016) %>%
  summarise(NOP_WHI = sum(`Value`)) -> LGA_Income_r

#summary(LGA_Income_r)
#sort(LGA_Income_r$NOP_WHI)

ggplot(IncomeData_r,aes(x=LGA_2016,y=Value))+geom_line()+ylab("# of households")+xlab("LGA")



## LGA
#Reading OGR vector maps into Spatial objects
#The Geospatial Data Abstraction Library (GDAL) is a computer software library for reading and writing raster and vector geospatial data formats
#The rgdal package has been around for more than a decade and provides bindings to the incredible Geospatial Data Abstraction Library (GDAL) for reading, writing and converting between spatial formats.
LGAShp <- rgdal::readOGR('LGA_shapefiles',layer="LGA_2016_AUST")
# for topologically-aware polygon simplification
LGA_subset <- rmapshaper::ms_simplify(LGAShp, keep = 0.05)
save(LGA_subset,file = "Data/GA_subset.Rda")

#Some LGA codes not in LGAData but present in Income Data
#For which income level are available but no shape file available
#Deleting those LGA codes for the purpose of carrying out further analysis


LGA_data <- LGA_subset@data

LGA_data$id <- row.names(LGA_data)
LGA_data$LGA_CODE16 <- as.integer(as.character(LGA_data$LGA_CODE16))
D = setdiff(IncomeData$LGA_2016,LGA_data$LGA_CODE16)
IncomeData_r = IncomeData[-which(IncomeData$LGA_2016 %in% D),]



IncomeData_r %>%
  group_by(LGA_2016) %>%
  summarise(NOP_WHI = sum(`Value`)) -> LGA_Income_r


LGA_data$id <- row.names(LGA_data)
LGA_data$LGA_CODE16 <- as.integer(as.character(LGA_data$LGA_CODE16))
LGA_map <- ggplot2::fortify(LGA_subset)
LGA_map$group <- paste("g",LGA_map$group,sep=".")
LGA_map$piece <- paste("p",LGA_map$piece,sep=".")
library(ggplot2)
ggplot(LGA_map) + geom_polygon(aes(long, lat, group = group), colour = 'grey')

LGA_data %>%
  select(id, LGA_CODE16, LGA_NAME16) %>%
  rename(name = LGA_NAME16) %>%
  right_join(IncomeData_r,by=c("LGA_CODE16" = "LGA_2016")) %>%
  right_join(LGA_map)-> LGA_map_2

ggplot(LGA_map_2) + geom_polygon(aes(long, lat, group = group,fill=Value)) + coord_fixed()

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
dsts <- unlist(nbdists(LGA_nn_1, coords,longlat=TRUE))
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
#If global linear spatial association exists
Moran_test <-moran.test(LGA_Income_r$NOP_WHI, Weights_W)
#Spatial dependence is there as p-value is very low
#Moran I statistic       Expectation          Variance 
#0.496028646      -0.001838235       0.002188406 
#If correlation coefficient is close to 1, it means there is no variation betweeen data points and line of best fit
#Here Moran's statistic is 0.49, implying there are large variation between data points and line of best fit. However, major variation
#comes from the outliers.For most of the points, it seems to give a good fit. 
#Global Geary Test
Geary_test <-geary.test(LGA_Income_r$NOP_WHI, Weights_W,randomisation=TRUE)
#Geary test suggests that there is some kind of non linear spatial association in the data - globally

summary(moran.plot(LGA_Income_r$NOP_WHI, Weights_W))
                                                                                                
#Local measures of spatial autocorrelation
local_Moran <- localmoran(LGA_Income_r$NOP_WHI, Weights_W)
summary(local_Moran)


#creating scaled resonse variable
LGA_Income_r$NOP_WHI_s <- scale(LGA_Income_r$NOP_WHI)  %>% as.vector()
# create a spatially lagged variable and save it to a new column
sLGA_Income_r$NOP_WHI_s_lag <- scale(lag.listw(Weights_W, LGA_Income_r$NOP_WHI))%>%as.vector()
# summary of variables, to inform the analysis
summary(LGA_Income_r$NOP_WHI_s)
summary(LGA_Income_r$NOP_WHI_s_lag)

Moran_MC<-moran.mc(LGA_Income_r$NOP_WHI, listw = Weights_W, nsim=10000)
plot(Moran_MC)

Geary_MC<-geary.mc(LGA_Income_r$NOP_WHI, listw = Weights_W, nsim=10000)
plot(Geary_MC)


LGA_Income_r$local_Moran_stat <- NA

# high-high quadrant
LGA_Income_r[(LGA_Income_r$NOP_WHI_s >= 0 & 
                LGA_Income_r$NOP_WHI_s_lag >= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "high-high"
# low-low quadrant

LGA_Income_r[(LGA_Income_r$NOP_WHI_s <= 0 & 
                LGA_Income_r$NOP_WHI_s_lag <= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "low-low"

# high-low quadrant

LGA_Income_r[(LGA_Income_r$NOP_WHI_s >= 0 & 
                LGA_Income_r$NOP_WHI_s_lag <= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "high-low"

# low-high quadrant

LGA_Income_r[(LGA_Income_r$NOP_WHI_s <= 0 & 
                LGA_Income_r$NOP_WHI_s_lag >= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "low-high"


# non-significant observations
LGA_Income_r[(local_Moran[, 5] > 0.05), "local_Moran_stat"] <- "not signif."  


LGA_Income_r %>% 
  group_by(local_Moran_stat) %>% 
  tally()


# plotting the map for local_Morans
df <- fortify(LGA_Income_r, region="LGA_2016")
df %>% right_join(IncomeData_r) %>%right_join(LGA_data,by=c("LGA_2016"="LGA_CODE16")) %>% right_join(LGA_map)-> LGA_map_3

LGA_map_3 %>% 
    ggplot(aes(long, lat, group = group, fill = local_Moran_stat)) + 
    geom_polygon(color = "white", size = .05)  + coord_equal() + 
    theme_void() + scale_fill_brewer( palette = "Set1") +coord_fixed()


#G and Gstar local spatial statistics
local_G <-localG(LGA_Income_r$NOP_WHI, Weights_W)
summary(local_G)

local_G_new<- NA

local_G_new[local_G >= 1.96] <- "high-high"
local_G_new[(local_G <= -1.96)] <- "low-low"
local_G_new[(-1.96<local_G)& (local_G<1.96)] <- "not_signif."

LGA_Income_r2=cbind(LGA_Income_r,local_G_new)


# plotting the map
df <- fortify(LGA_Income_r2, region="LGA_2016")

df %>% right_join(IncomeData_r) %>%right_join(LGA_data,by=c("LGA_2016"="LGA_CODE16")) %>% right_join(LGA_map)-> LGA_map_4

LGA_map_4 %>% 
  ggplot(aes(long, lat, group = group, fill = local_G_new)) + 
  geom_polygon(color = "white", size = .05)  + coord_equal() + 
  theme_void() + scale_fill_brewer( palette = "Set1")+ coord_fixed()


#LGA_map_4 %>% filter(local_Moran_stat == c(""high-high","low-low"))
##LGA where local Moran stat is significant and high high
LGA_map_4 %>% 
  group_by(local_Moran_stat,Region,STE_NAME16) %>% filter(local_Moran_stat=="high-high") %>% tally() ->Moran_High_High

LGA_map_4 %>% 
  group_by(local_Moran_stat,Region,STE_NAME16) %>% filter(local_Moran_stat=="low-low")%>%tally()->Moran_low_low
##LGA where local  G stat is significant and high high
LGA_map_4 %>% 
  group_by(local_G_new,Region,STE_NAME16) %>% filter(local_G_new=="high-high")%>%tally()->G_High_High


LGA_map_4 %>% 
  group_by(local_G_new,Region,STE_NAME16) %>% filter(local_G_new=="low-low") %>%tally()->G_low_low

#spatially significant in local Moran but not in local Gi
D1 = setdiff(Moran_High_High$Region,G_High_High$Region)
#spatially significant in local Gi but not in local Moran 
D2 = setdiff(G_High_High$Region,Moran_High_High$Region)

##Done till here
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
#Great Circle distances 
dsts <- unlist(nbdists(LGA_nn_1, coords,longlat = TRUE))
# Euclidean distances 
dsts <- unlist(nbdists(LGA_nn_1, coords,longlat = TRUE))

summary(dsts)
max_1nn <- max(dsts)
max_1nn

#nb2listw function takes a neighbours list object and converts it into a weights object
#For style="W", the weights vary between unity divided by the largest and
#smallest numbers of neighbours, and the sums of weights for each areal entity
#are unity
#B is the basic binary coding



