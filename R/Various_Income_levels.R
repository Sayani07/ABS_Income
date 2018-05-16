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
Cat_Income <-  readr::read_csv('Data/Income_Data.csv') %>% mutate(State = case_when(
  State == 'New South Wales' ~ 'NSW',
  State == 'Victoria' ~ 'VIC',
  State == 'Queensland' ~ 'QLD',
  State == 'South Australia' ~ 'SA',
  State == 'Western Australia' ~ 'WA',
  State == 'Tasmania' ~ 'TAS',
  State == 'Northern Territory' ~ 'NT',
  State == 'Australian Capital Territory' ~ 'ACT',
  TRUE ~ as.character(State)))%>%
  mutate(
    Weekly_Household_Income = case_when(
      Weekly_Household_Income %in% c("$1-$149", "$150-$299", "$300-$399", "$400-$499") ~ 'Low',
      Weekly_Household_Income %in% c(
        "$500-$649",
        "$650-$799",
        "$800-$999",
        "$1,000-$1,249",
        "$1,250-$1,499" ) ~ 'Medium',
      Weekly_Household_Income %in% c("$1,
                                     500-$1,749","$1,500-$1,749",
                                     "$1,750-$1,999","$2,000-$2,499","$2,500-$2,999","$3,000-$3,499","$3,500-$3,999","$4,000-$4,499","$4,500-$4,999"
                                     ,"$5,000-$5,999","$6,000-$7,999","$8,000 or more") ~ 'High',TRUE~as.character(Weekly_Household_Income)
    ))
IncomeData_Low <- filter(Cat_Income,Weekly_Household_Income == "Low")
IncomeData_Medium <- filter(Cat_Income,Weekly_Household_Income == "Medium")
IncomeData_High <- filter(Cat_Income,Weekly_Household_Income == "High")

#Region wise and Income Category wise number of people
Cat_Income %>%
  group_by(LGA_2016,Weekly_Household_Income) %>%
  summarise(Number_People = sum(`Value`)) -> LGA_Income_catg

#Assuming high income class has income 5000/week, medium has 1000/week and low has 250/week -  Numbers are median of intervals provided
Income_Median <- LGA_Income_catg %>% filter(Number_People>0)%>%mutate(Median_Income = case_when(Weekly_Household_Income=='Low'~ 250,
  Weekly_Household_Income=='Medium'~1000,
  Weekly_Household_Income=='High'~5000)
)

#One income for one region

Income_Median %>%
  group_by(LGA_2016) %>%
  summarise(Median_Income_LGA = sum(`Number_People`*`Median_Income`)/sum(`Number_People`)) ->OneIncome_LGA


  
LGA_data %>%
  select(id, LGA_CODE16, LGA_NAME16) %>%
  rename(name = LGA_NAME16) %>%
  right_join(OneIncome_LGA,by=c("LGA_CODE16" = "LGA_2016")) %>%
  right_join(LGA_map)-> LGA_map_Income

ggplot(LGA_map_Income) + geom_polygon(aes(long, lat, group = group,fill=Median_Income_LGA)) + coord_fixed()

#Weights and Neighbors same as LGA_map1.R

Moran_test <-moran.test(OneIncome_LGA$Median_Income_LGA, Weights_W)



summary(moran.plot(OneIncome_LGA$Median_Income_LGA, Weights_W))

#Local measures of spatial autocorrelation
local_Moran <- localmoran(OneIncome_LGA$Median_Income_LGA, Weights_W)
summary(local_Moran)



#creating scaled resonse variable
OneIncome_LGA$Median_Income_LGA_s <- scale(OneIncome_LGA$Median_Income_LGA)  %>% as.vector()
# create a spatially lagged variable and save it to a new column
OneIncome_LGA$Median_Income_LGA_s_lag <- scale(lag.listw(Weights_W,OneIncome_LGA$Median_Income_LGA))%>%as.vector()
# summary of variables, to inform the analysis
summary(OneIncome_LGA$Median_Income_LGA_s)
summary(OneIncome_LGA$Median_Income_LGA_s_lag)


OneIncome_LGA$local_Moran_stat <- NA

# high-high quadrant
OneIncome_LGA[(OneIncome_LGA$Median_Income_LGA_s >= 0 & 
                 OneIncome_LGA$Median_Income_LGA_s_lag >= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "high-high"
# low-low quadrant

OneIncome_LGA[(OneIncome_LGA$Median_Income_LGA_s <= 0 & 
                 OneIncome_LGA$Median_Income_LGA_s_lag <= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "low-low"

# high-low quadrant

OneIncome_LGA[(OneIncome_LGA$Median_Income_LGA_s >= 0 & 
                 OneIncome_LGA$Median_Income_LGA_s_lag  <= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "high-low"

# low-high quadrant

OneIncome_LGA[(OneIncome_LGA$Median_Income_LGA_s <= 0 & 
                 OneIncome_LGA$Median_Income_LGA_s_lag >= 0) & 
               (local_Moran[, 5] <= 0.05), "local_Moran_stat"] <- "low-high"


# non-significant observations
OneIncome_LGA[(local_Moran[, 5] > 0.05), "local_Moran_stat"] <- "not signif."  


OneIncome_LGA %>% 
  group_by(local_Moran_stat) %>% 
  tally()


# plotting the map for local_Morans
df_OneIncome <- fortify(OneIncome_LGA, region="LGA_2016")
df_OneIncome %>% right_join(OneIncome_LGA) %>%right_join(LGA_data,by=c("LGA_2016"="LGA_CODE16")) %>% right_join(LGA_map)-> LGA_OneIncome

LGA_OneIncome %>% 
  ggplot(aes(long, lat, group = group, fill = local_Moran_stat)) + 
  geom_polygon(color = "white", size = .05)  + coord_equal() + 
  theme_void() + scale_fill_brewer( palette = "Set1") +coord_fixed()


#G and Gstar local spatial statistics
local_G <-localG(OneIncome_LGA$Median_Income_LGA, Weights_W)
summary(local_G)

local_G_new<- NA

local_G_new[local_G >= 1.96] <- "high-high"
local_G_new[(local_G <= -1.96)] <- "low-low"
local_G_new[(-1.96<local_G)& (local_G<1.96)] <- "not_signif."

LGA_OneIncome_r=cbind(OneIncome_LGA,local_G_new)


# plotting the map
df_Gstar_OneIn <- fortify(LGA_OneIncome_r, region="LGA_2016")

df_Gstar_OneIn %>% right_join(OneIncome_LGA) %>%right_join(LGA_data,by=c("LGA_2016"="LGA_CODE16")) %>% right_join(LGA_map)-> LGA_OneIncome_Gstar

LGA_OneIncome_Gstar %>% 
  ggplot(aes(long, lat, group = group, fill = local_G_new)) + 
  geom_polygon(color = "white", size = .05)  + coord_equal() + 
  theme_void() + scale_fill_brewer( palette = "Set1")+ coord_fixed()


#LGA_map_4 %>% filter(local_Moran_stat == c(""high-high","low-low"))
##LGA where local Moran stat is significant and high high
LGA_OneIncome_Gstar %>% 
  group_by(local_Moran_stat,LGA_NAME16,STE_NAME16) %>% filter(local_Moran_stat=="high-high") %>% tally() ->Moran_High_High

LGA_OneIncome_Gstar %>% 
  group_by(local_Moran_stat,LGA_NAME16,STE_NAME16) %>% filter(local_Moran_stat=="low-low")%>%tally()->Moran_low_low
##LGA where local  G stat is significant and high high
LGA_OneIncome_Gstar %>% 
  group_by(local_G_new,LGA_NAME16,STE_NAME16) %>% filter(local_G_new=="high-high")%>%tally()->G_High_High


LGA_OneIncome_Gstar %>% 
  group_by(local_G_new,LGA_NAME16,STE_NAME16) %>% filter(local_G_new=="low-low") %>%tally()->G_low_low

#spatially significant in local Moran but not in local Gi
Moran_not_G = setdiff(Moran_High_High$LGA_NAME16,G_High_High$LGA_NAME16)
#spatially significant in local Gi but not in local Moran 
G_not_Moran = setdiff(G_High_High$LGA_NAME16,Moran_High_High$LGA_NAME16)

