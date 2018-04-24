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

 
ggplot(LGA_map_2) + geom_polygon(aes(long, lat, group = group,fill=Value))
 
