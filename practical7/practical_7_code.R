library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)

download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="prac9_data/statistical-gis-boundaries-london.zip")

getwd

listfiles<-dir_info(here::here("practical7")) %>%
  dplyr::filter(str_detect(path, 'zip'))%>%
  dplyr::select(path)%>%
  pull()%>%
  
  print()%>%
  as.character()%>%
  utils::unzip(exdir = here::here("practical7"))

Londonwards<-dir_info(here::here("practical7", 
                                 "statistical-gis-boundaries-london", 
                                 "ESRI"))%>%
  
  dplyr::filter(str_detect(path,
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  st_read()

Londonwards<- st_read("C:/Users/zhu/Documents/gis_code/practical7/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

Londonwards<- st_read(here::here("practical7", 
                                 "statistical-gis-boundaries-london", 
                                 "ESRI",
                                 "London_Ward_CityMerged.shp"))


qtm(Londonwards)

LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               col_names = TRUE, 
                               locale = locale(encoding = 'Latin1'))

Datatypelist <- LondonWardProfiles %>%
  summarise(across(everything(),class))%>% # replace summarise_all with summarise(across.
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

Datatypelist <- LondonWardProfiles %>%
  summarise(across(everything(),class))%>% # replace summarise_all with summarise(across.
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist


LonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles,
            by = c("GSS_CODE" = "New code"))

tmap_mode("plot")
qtm(LonWardProfiles,
    fill = "Average GCSE capped point scores - 2014",
    borders = NULL,
    fill.palette = "Blues")


london_schools <- read_csv("https://data.london.gov.uk/download/london-schools-atlas/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)
lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

tmap_mode("plot")
qtm(lond_sec_schools_sf)

q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=LonWardProfiles)

q + stat_smooth(method = 'lm', se=FALSE,size=1)+
  geom_jitter()


Regressiondata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

model1 <- Regressiondata %>%
  lm(average_gcse_capped_point_scores_2014 ~ 
              unauthorised_absence_in_all_schools_percent_2013,
     data=.)

summary(model1)

library(broom)
tidy(model1)
glance(model1)


install.packages("tidypredict")           
library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model1)

LonWardProfiles <- LonWardProfiles %>%
  clean_names()

ggplot(LonWardProfiles, aes(x=average_gcse_capped_point_scores_2014)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 5)+
  geom_density(colour="red",
               size=1,
               adjust=1)



ggplot(LonWardProfiles, aes(x=unauthorised_absence_in_all_schools_percent_2013)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 5)+
  geom_density(colour="red",
               size=1,
               adjust=1)


ggplot(LonWardProfiles, aes(x=median_house_price_2014))+
  geom_histogram()


qplot(x = median_house_price_2014,
       y = average_gcse_capped_point_scores_2014,
       data = LonWardProfiles)

ggplot(LonWardProfiles, aes(x=log(median_house_price_2014)))+
  geom_histogram()

library(car)
symbox(~median_house_price_2014,
       LonWardProfiles,
       na.rm=T,
       powers = seq(-3,3,by=.5))

ggplot(LonWardProfiles, aes(x=(median_house_price_2014)^-1))+
  geom_histogram()

qplot(x = (median_house_price_2014)^-1,
      y = average_gcse_capped_point_scores_2014,
      data= LonWardProfiles)
              
qplot(x = log(median_house_price_2014),
      y = average_gcse_capped_point_scores_2014,
      data= LonWardProfiles)

model_data <- model1 %>%
  augment(., Regressiondata)

model_data %>%
  dplyr::select(.resid)%>%
    pull()%>%
    qplot()+
    geom_histogram()


Regressiondata2<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014)



model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = Regressiondata2)

tidy(model2)
glance(model2)

model_data2 <- model2 %>%
  augment(.,Regressiondata2)

LonWardProfiles <- LonWardProfiles %>%
  mutate(model2resids = residuals(model2))

install.packages("corrr")
library(corrr)

Correlation <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014) %>%
mutate(median_house_price_2014 = log(median_house_price_2014))%>%
  correlate()%>%
  focus(-average_gcse_capped_point_scores_2014, mirror = TRUE)

rplot(Correlation)


vif(model2)


position <- c(10:74)

Correlation_all<- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(position)%>%
  correlate()

rplot(Correlation_all)

Homoscedasticity

par(mfrow=c(2,2))
plot(model2)

DW <- durbinWatsonTest(model2)
tidy(DW)

tmap_mode("view")

tm_shape(LonWardProfiles)+
  tm_polygons("model2resids",
              palette = "RdYlBu")+
  tm_shape(lond_sec_schools_sf) + tm_dots(col = "TYPE")



coordsW <- LonWardProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)


LWard_nb <- LonWardProfiles %>%
  poly2nb(.,queen = T)

knn_wards <- coordsW %>%
  knearneigh(.,k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

plot(LWard_nb, st_geometry(coordsW), col="red")


plot(LWard_knn, st_geometry(coordsW), col="blue")


Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="W")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="W")


Queen <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()


Nearest_neighbour <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen

Nearest_neighbour
