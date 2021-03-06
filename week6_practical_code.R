library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
install.packages('GISTools')

setwd('f:/Rdata')
getwd()
LondonBroughs <- st_read(here::here("week1","statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

LondonBoroughs <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson")


"library(stringr)
BroughMap <- LondonBroughs %>%
  dplyr::filter(str_detect（lad15cd, '^E09')%>%
  st_transform(., 27700)

qtm(BrounghMap)"

library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(lad15cd, "^E09"))%>%
  st_transform(., 27700)


qtm(BoroughMap)

summary(BoroughMap)


BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(., 27700)

summary(BluePlaques)

tmap_mode('plot')

tm_shape(BoroughMap)+
  tm_polygons(col = NA, alpha = 0.5)+
tm_shape(BluePlaques)+
  tm_dots(col = 'blue')

library(tidyverse)

library(sf)
BluePlaques <-distinct(BluePlaques)

BluePlaquesSub <- BluePlaques[BoroughMap,]

tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

Harrow <- BoroughMap %>%
  filter(., lad15nm=="Harrow")

tm_shape(Harrow)+
  tm_polygons(col=NA, alpha = 0.5)

BluePlaquesSub <- BluePlaques[Harrow,]
tmap_mode("plot")

tm_shape(Harrow)+
  tm_polygons(col = NA, alpha = 0.5)+
tm_shape(BluePlaquesSub)+
  tm_dots(col = "blue")

install.packages('spatstat')
library(spatstat)
window <- as.owin(Harrow)
plot(window)


BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')

BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)

BluePlaquesSub@coords[,2]
BluePlaquesSub@coords

BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")

BluePlaquesSub.ppp%>%
  density(., sigma=500)%>%
  plot()


BluePlaquesSub.ppp %>%
  density(., sigma=1000) %>%
  plot()

plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")


Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)
Qcount

Qcount %>% 
  summarise_all(class)
Qcount


sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1)
sums


lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)
lambda
factorial

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))
QCountTable

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)




teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")




K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

library(raster)
library(fpc)

install.packages("fpc")
st_geometry(BoroughMap)



#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)
plot(BluePlaquesSubPoints)

library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

install.packages("dbscan")

library(ggplot2)

db

db$cluster


BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)


chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])


chulls <- chulls %>%
  filter(dbcluster >=1)


dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()


HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()



library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")


#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5) 
library(here)
library(janitor)
library(dplyr)
LondonWards <- st_read("F:/Rdata/week6/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")%>%
st_transform(.,27700)

LondonWardsMerged <- st_read("F:/Rdata/week6/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")%>%
  st_transform(.,27700)

WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     na = c("NA", "n/a"),locale=locale(encoding="latin1")) %>%
  clean_names()


LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData,
            by = c('GSS_CODE' = 'new_code'))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

st_crs(LondonWardsMerged)

tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")


BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

library(sf)
points_sf_joined <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  add_count(ward_name)%>% #add_count是啥意思
  janitor::clean_names()%>% 
  mutate(area=st_area(.))%>%  #st_area啥意思
  mutate(density = n/area)%>%  #n从哪里来
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

points_sf_joined<- points_sf_joined %>%  
  group_by(gss_code)%>%
  summarise(density = first(density),
            ward_name = first(ward_name),#first（）啥意思
            plaquecount= first(n))
 
tm_shape(points_sf_joined)+
  tm_polygons("density",
              style="jenks",
              palette="Puor",
              midpoint=NA,
              popup.vars = c("wardname","density"),
              title="Blue Plaque Density")#出错了，某个地方敲错了？

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

install.packages('spdep')
library(spdep)
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)



LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)

plot(LWard_nb, st_geometry(coordsW), col="red")
plot(points_sf_joined$geometry, add=T)


Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

sum(Lward.lw)

sum(Lward.lw[,1])

Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

Lward.lw

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density)%>%
  as.vector()%>%
  moran.test(.,Lward.lw)

I_LWard_Global_Density


C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density



I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

slice_head(I_LWard_Local_Density, n=5)


points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

install.packages('RColorBrewer ')
library(RColorBrewer )
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))


Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)
tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")


slice_head(points_sf_joined, n=2)

Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist


I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")

G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")

  