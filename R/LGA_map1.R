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

#remane state in Attribute Dataset
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
    TRUE ~ as.character(State))) %>% filter(Weekly_Household_Income =="$8,000 or more"|Household_Composition == "Total Households")
#Might need to change Column names before this step


IncomeData %>%
  group_by(LGA_2016) %>%
  summarise(NOP_8000AUDpweek = sum(`Value`)) -> LGA_Income


## LGA
#Reading OGR vector maps into Spatial objects
#The Geospatial Data Abstraction Library (GDAL) is a computer software library for reading and writing raster and vector geospatial data formats
#The rgdal package has been around for more than a decade and provides bindings to the incredible Geospatial Data Abstraction Library (GDAL) for reading, writing and converting between spatial formats.
LGAShp <- rgdal::readOGR('LGA_shapefiles',layer="LGA_2016_AUST")
LGA_subset <- rmapshaper::ms_simplify(LGAShp, keep = 0.05)
save(LGA_subset,file = "Data/GA_subset.Rda")
LGA_data <- LGA_subset@data
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
right_join(IncomeData,by=c("LGA_CODE16" = "LGA_2016")) %>%
  right_join(LGA_map)-> LGA_map_2

 
ggplot(LGA_map_2) + geom_polygon(aes(long, lat, group = group), colour = 'grey')
# 

## Electorates

# electShp <- rgdal::readOGR('elect')
#electSmall <- rmapshaper::ms_simplify(electShp, keep = 0.05)
#save(electSmall, file = "electSmall.Rda")
# load("~/mapsR/electSmall.Rda")
# 
# elect_data <- electSmall@data
# elect_data$id <- row.names(elect_data)
# elect_map <- ggplot2::fortify(electSmall)
# elect_map$group <- paste("g",elect_map$group,sep=".")
# elect_map$piece <- paste("p",elect_map$piece,sep=".")
# 
# # Incorporate population
# elect_data %>%
#   select(id, Elect_div, State, Area_SqKm, Total_Population) %>%
#   rename(name =  Elect_div,
#          pop = Total_Population) %>%
#   right_join(elect_map) -> elect_map
# 
# 
# ggplot(elect_map) + geom_polygon(aes(long, lat, group = group),
#                                  colour = 'grey')


##Australian map
#ausShp <- rgdal::readOGR('Aus')
#ausSmall <- rmapshaper::ms_simplify(ausShp, keep = 0.05)
#load("~/mapsR/ausSmall.Rda")
# 
# aus_data <- ausSmall@data
# aus_data$id <- row.names(aus_data)
# aus_map <- ggplot2::fortify(ausSmall)
# aus_map$group <- paste("g", aus_map$group, sep = ".")
# aus_map$piece <- paste("p", aus_map$piece, sep = ".")
# 
# # Switch states to the codes and attach population data to map
# aus_data %>%
#   mutate(State = case_when(
#     STE_NAME16 == 'New South Wales' ~ 'NSW',
#     STE_NAME16 == 'Victoria' ~ 'VIC',
#     STE_NAME16 == 'Queensland' ~ 'QLD',
#     STE_NAME16 == 'South Australia' ~ 'SA',
#     STE_NAME16 == 'Western Australia' ~ 'WA',
#     STE_NAME16 == 'Tasmania' ~ 'TAS',
#     STE_NAME16 == 'Northern Territory' ~ 'NT',
#     STE_NAME16 == 'Australian Capital Territory' ~ 'ACT',
#     TRUE ~ as.character(STE_NAME16))) %>%
#   select(id, State) %>%
#   right_join(statePopulation) %>%
#   right_join(aus_map) -> aus_map
