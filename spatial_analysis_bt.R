# BTV virus in Ecuador ----

# Loading libraries ----
library(raster)
library(rgeos)
library(dplyr)
library(rgdal)
library(ggplot2)
library(ggsn)
library(paletteer)
library(pals)
library(ggmap)
library(plotly)
# devtools::install_github('oswaldosantos/ggsn')

# To improve definition of time to 0.5 arc I need the tile points
# https://rsh249.github.io/spatial_bioinformatics/worldclim.html
# https://gis.stackexchange.com/questions/227585/using-r-to-extract-data-from-worldclim?newreg=3d134adbf73948c8864e97e6a22f1841

setwd("~/papers/BTV/")

r <- getData(name="worldclim", var="bio", res=2.5)

r <- r[[c(1,4,5,6,8,9,12,13,14,15,18,19)]]
names(r) <- c("Temp", "tseason", "max", "min", "twettest", 
              "tdriest", "Prec", "pwettes", "pdriest", "pseason", "pwarm", "pcold")

#See the global information extracted
plot(r)

#Import my shapefile
ec3<-rgdal::readOGR(dsn="~/papers/BTV/",layer="nxparroquias")
ec3 <- subset(ec3, DPA_DESPRO != "GALAPAGOS")

# ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/10.2020 II sem/FLI/case-control/bayesian/",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
trueCentroids <- data.frame(gCentroid(ec3, byid=TRUE))

#Add centroids to the shapefile
ec3@data$lat <- trueCentroids$y
ec3@data$long <- trueCentroids$x

# Checking locations
ggplot()+
           geom_polygon(data=ec3, aes(x=long, y=lat, group = group), colour = "grey", fill=NA) +
           geom_point(data=ec3@data, aes(x=long, y=lat),
                      size=1, shape=21, fill="orange", color="black")

#Create a layer of spatial points
# centroid_points <- SpatialPoints(trueCentroids, proj4string = r@crs)

#Extract bio data to spatial points
values <- raster::extract(r,trueCentroids)

df <- cbind.data.frame(trueCentroids,values)

head(df)
# Transform temperatures /10 
df$Temp <- df$Temp/10
df$max <- df$max/10
df$min <- df$min/10
df$twettest <- df$twettest/10
df$tdriest <- df$tdriest/10

#Transfer data points to shape data
ec3@data$Temp <- df$Temp
ec3@data$max <- df$max
ec3@data$min <- df$min
ec3@data$twettest <- df$twettest
ec3@data$tdriest <- df$tdriest
ec3@data$Prec <- df$Prec
ec3@data$pwettest <- df$pwettes
ec3@data$pdriest <- df$pdriest
ec3@data$tseason <- df$tseason
ec3@data$pseason <- df$pseason
ec3@data$pwarm <- df$pwarm
ec3@data$pcold <- df$pcold

# See the data of Ecuador
plot(r, xlim=c(-82,-75), ylim=c(-6,2))
datos <- data.frame(ec3@data)

#Creating a fortify map to plot the information
ec3$id <- rownames(ec3@data)
map1 <- fortify(ec3)
map1$DPA_PARROQ <- ec3@data$DPA_PARROQ[match(map1$id, ec3@data$id)]
map1$Temp <- ec3@data$Temp[match(map1$id, ec3@data$id)]
map1$min <- ec3@data$min[match(map1$id, ec3@data$id)]
map1$max <- ec3@data$max[match(map1$id, ec3@data$id)]
map1$prec <- ec3@data$Prec[match(map1$id, ec3@data$id)]

# Creating a spatial data points of outbreaks locations
c <- read.csv(file = "Coordinates.csv")
d <- read.csv(file = "Coordinates.Wahis.csv")
e <- read.csv(file = "manabi.lon.lat.csv")
f <- read.csv(file = "BTV_lat_long.csv")
f <- f[f$definitivo == "caso", ]

#Changing names
colnames(c)[1] <- "id"
colnames(e)[2] <- "id"
colnames(e)[5] <- "X"
colnames(e)[6] <- "Y"
colnames(e)[7] <- "x"
colnames(e)[8] <- "y"
colnames(f)[2] <- "id"
colnames(f)[49] <- "x"
colnames(f)[48] <- "y"

points <- rbind(c, d[,c(1,5,6)], e[,c(2,7,8)], f[,c(2,49,48)])

# Where they are 
ggplot()+
           geom_point(data=points, aes(x=x, y=y, label=rownames(points)))+
           geom_label()


# Checking is there is duplicate e in f
table(c$x %in% f$x)

#deleting NA in points #341 observations final
points <- points[!is.na(points$x),]
points <- points[!is.na(points$y),]

# Maps
# Import the base map of ecuador ---
# Download map looking at the bordes of colombia and Peru
register_stadiamaps(key="76b5ee9f-672a-4475-95ac-c3e2cbfbb72a")
ecu2 <- get_stadiamap(bbox = c(left = - 81.4, 
                               bottom = -5.1, 
                               right = -75, 
                               top = 1.6),
                      zoom = 8, 
                      maptype = "stamen_terrain_background")
plot(ecu2)
# import Quito limits
uio <-rgdal::readOGR(dsn="~/papers/BTV/",layer="quito")
uio <- spTransform(uio, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# import GYE limits
gye<-rgdal::readOGR(dsn="~/papers/BTV/",layer="guayaquil")
gye <- spTransform(gye, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Fig.2A. tem Max temperature of warmest month ----
# Plotting and saving the image

tiff(filename = "Fig.1.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

tem <- ggmap(ecu2) +  
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=Temp),
               alpha=0.7) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            color="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme_minimal(base_size = 8)+
  theme(title = NULL,
        legend.key.width = unit(0.25, 'cm'))+
  labs(fill=("°C"),
       x=NULL,
       y=NULL) +
  geom_text(aes(x = -76, y = 1), label="Colombia", size = 2, color = "black") +
  geom_text(aes(x = -76, y = -3.5), label="Peru", size = 2, color = "black") +
  geom_polygon(data=uio, aes(x=long, y=lat,group=group), #ecuador limits
               color="red", size=0.3, alpha=0.5, color="white")+
  geom_polygon(data=gye, aes(x=long, y=lat,group=group), #ecuador limits
               color="red", size=0.3, alpha=0.5, color="white")+
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09) +
  annotate("text", label="Quito", x=-78.75, y=-0.07, size= 1.5, color="black")+
  annotate("text", label="Guayaquil", x=-80.40, y=-2.1, size= 1.5, color="black")
  
dev.off()

# Temperature min map ----
tiff(filename = "Fig.3temp_min.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=min)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("Min°C"),
       x=NULL,
       y=NULL) +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  theme_minimal()+
  north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)+
  geom_point(data=points, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black")

dev.off()


# Fig. 2C pm Precipitation map ----
tiff(filename = "Fig.Precipitation.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

pm <- ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=prec)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme_minimal(base_size = 8)+
  theme(title = NULL,
        legend.key.width = unit(0.25, 'cm'))+
  labs(fill=("mm"),
       x=NULL,
       y=NULL) +
  xlim(-81.1,-75.1)+
  ggsn::north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)

dev.off()


# Locating the outbreaks in the parish ----
# Create the spatial points
sp <- SpatialPoints(points[,c(2,3)])

# Use the entire map to intersect the points and polygons
ec3@data$inter <- over(SpatialPolygons(ec3@polygons), sp)

#number of points in the polygon
ec3@data$inter2 <- sapply(over(SpatialPolygons(ec3@polygons), sp, returnList = TRUE), length)

# ec3@data$inter2[ec3@data$inter2 == 0] <- NA

# Which are in False 153, True 887
table(is.na(ec3@data$inter)) 

#Please change the working directory in dsn
map1$DPA_PARROQ <- ec3@data$DPA_PARROQ[match(map2$id, ec3@data$id)]
map1$inter <- ec3@data$inter[match(map1$id, ec3@data$id)]
map1$inter2 <- ec3@data$inter2[match(map1$id, ec3@data$id)]

# Locations of outbreaks and number of outbreaks in areas on the map ----
ggplot()+
    geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=inter2)) +
    geom_point(data=points, aes(x=x, y=y),
               size=1, shape=21, fill="orange", color="black")


# Temperature and precipitation descriptive analysis ----
# Parishes with bluethonge Max temp 20.62
summary(ec3@data$Temp[!is.na(ec3@data$inter)])
sd(ec3@data$Temp[!is.na(ec3@data$inter)])
IQR(ec3@data$Temp[!is.na(ec3@data$inter)])

boxplot(ec3@data$Temp[!is.na(ec3@data$inter)])

ec3@data <- ec3@data %>% 
  mutate(bt = ifelse(is.na(inter),0,1)) 

ec3@data %>% 
  group_by(bt) %>% 
  summarise(mean(Temp, na.rm=TRUE)) 

ec3@data %>% 
  group_by(bt) %>% 
  summarise(mean(Prec, na.rm=TRUE)) 

# Parishes Temp withouth bluethonge
summary(ec3@data$Temp[is.na(ec3@data$inter)])
IQR(ec3@data$Temp[is.na(ec3@data$inter)], na.rm = TRUE)
sd(ec3@data$Temp[is.na(ec3@data$inter)], na.rm = TRUE)


# Precipitation
# Parishes with bluethonge Mean prec 1983 mm
summary(ec3@data$Prec[!is.na(ec3@data$inter)])
IQR(ec3@data$Prec[!is.na(ec3@data$inter)])

# Parishes Prep withouth bluethonge 1373 mm
summary(ec3@data$Prec[is.na(ec3@data$inter)])
IQR(ec3@data$Prec[is.na(ec3@data$inter)], na.rm = TRUE)
sd(ec3@data$Prec[is.na(ec3@data$inter)], na.rm = TRUE)


# Filtering optimal temperature ----
ec3@data <- ec3@data %>% 
  mutate(opt = ifelse(min >12, 1, 0))%>%
  mutate(opt2 = ifelse(max <32, 1, 0)) %>% 
  mutate(opt3 = opt+opt2) %>% 
  mutate(opt3 = ifelse(opt3==2, Temp, NA))

# Calculating the area
ec3@data$area <- raster::area(ec3) /1000000
sum(ec3@data$area)
sum(ec3@data$area[!is.na(ec3@data$opt3)])
sum(ec3@data$area[!is.na(ec3@data$opt3)])/sum(ec3@data$area)
# 73% of territory
sum(ec3@data$area[is.na(ec3@data$opt3)]) / sum(ec3@data$area)

# Area with ideal temperature
sum(ec3@data$area)*0.73

# How many parishes have the ideal conditions for Cullicoides spp?
table(!is.na(ec3@data$opt3))
561/1040
#  54% of the territory

# Fig.2B. ot Map of optimal temperatures ----
map1$opt3 <- ec3@data$opt3[match(map1$id, ec3@data$id)]

tiff(filename = "Fig.4map_ideal_all.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ot <- ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=opt3)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", linewidth=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme_minimal(base_size = 8)+
  theme(title = NULL,
        legend.key.width = unit(0.25, 'cm'))+
  labs(fill=("°C"),
       x=NULL,
       y=NULL) +
  xlim(-81.1,-75.1)+
  north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09) +
  geom_point(data=points, aes(x=x, y=y),
             size=0.3, shape=21, fill="orange", color="black")+
  geom_point(data=c, aes(x=x, y=y),
             size=0.3, shape=21, fill="orange", color="black")

  
dev.off()

# 09/01/2024
# update cattle population 
# p <- AFTOSA_FASE_2023_EC <- read_excel("AFTOSA_FASE_2023_EC.xlsx")
# colnames(p)
# sum(p$total_aplicado)
# 
# length(unique(p$parroquia))
# 
# 
# vigi <- p %>%
#   group_by(provincia, canton, parroquia) %>%
#   summarise(cantidad = sum(total_aplicado))
# 
# sum(vigi$cantidad , na.rm = TRUE)#59
# ### Mapa FINAL
# 
# # Usando o mapa para relocalizar en provincia cantón y parroquia
# source("~/papers/BTV/passthemap_guia.R")
# 
# # results
# # 1 el oro las lajas platanillos
# # 2 esmeraldas esmeraldas simon plata torres
# # 4 loja loja santiago 'san salvador o james'
# # [1] "cantidad faltante en el mapa"
# # [1] 3654
# str(d)
# str(ec3@data)
# write.csv(ec3@data, file="animal_population.csv")

# Reading population

library(readr)
pop <- read_csv(file = "animal_population.csv", col_types = cols(.default = "c"))

str(pop)
pop$cantidad <- as.numeric(pop$cantidad)
sum(pop$cantidad, na.rm = TRUE) #4656389

# transfer number of animals
ec3@data$pop <- pop$cantidad[match(ec3@data$DPA_PARROQ, pop$DPA_PARROQ)]
sum(ec3@data$pop, na.rm = TRUE)

# Calculate density
ec3@data$popden <- ec3@data$pop/ec3@data$area
  
# transfering to the fortified map to plot
map1$pop <- ec3@data$pop[match(map1$id, ec3@data$id)]
map1$popden <- ec3@data$popden[match(map1$id, ec3@data$id)]

# Fig. 2D pp Graphic population and pop densities ----

pp <- ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=pop)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", linewidth=0.01) +
  scale_fill_viridis_c(direction = -1, option = "D", na.value = "gray90")+
  theme_minimal(base_size = 8)+
  theme(title = NULL,
        legend.key.width = unit(0.25, 'cm'))+
  labs(fill=("Cattle
pop"),
       x=NULL,
       y=NULL) +
  xlim(-81.1,-75.1)+
  north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)


# Density map
pd <- ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=popden)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", linewidth=0.01) +
  scale_fill_viridis_c(direction = -1, option = "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("Cattle
density"),
       x=NULL,
       y=NULL,
       tag = "b") +
  xlim(-81.1,-75.1)+
  theme_minimal(base_size = 8)+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6),
        axis.title.x = element_text(size = 4))+
  north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)+
  geom_point(data=points, aes(x=x, y=y),
             size=0.02, color="red", alpha=0.7)

# Analyzing densitites
summary(ec3@data$pop)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  2.0    838.8   2326.0   4780.7   5353.8 121853.0       58 

summary(ec3@data$popden)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0024   8.3193  22.2637  35.3001  47.2803 808.7492       58 
  
summary(ec3@data$min)

popdb <- ec3@data %>% 
  group_by(DPA_DESPRO) %>% 
  summarize(p=sum(pop, na.rm = TRUE), 
                  t=mean(min, na.rm = TRUE))%>% 
  arrange(desc(p))

popdendb <- ec3@data %>% 
  group_by(DPA_DESPRO) %>% 
  summarize(d=mean(popden, na.rm = TRUE)) %>% 
  arrange(desc(d))

popdb$density <- popdendb$d

popdb <- popdb[,c(1,2,4,3)]

# write.csv(popdb, file = "population-density-province.csv")

ec3@data
colnames(ec3@data)
ec2pop <- ec3@data[, c(1,8,6,2,25,24,26,11:22)]
# write.csv(popdb, file = "population-density-parish.csv")

#04/02/2024
# Update no cases of BTV from passive surveillance only ----

nbtv <- read.csv("nobtv.csv")
colnames(nbtv)
sum(nbtv$value)

length(unique(nbtv$parroquia))


vigi <- nbtv %>%
  group_by(provincia, canton=cantón, parroquia) %>%
  summarise(cantidad = sum(value))

sum(vigi$cantidad , na.rm = TRUE)#4634
### Mapa FINAL

# Usando o mapa para relocalizar en provincia cantón y parroquia
source("~/papers/BTV/passthemap_sizse.R")

# 1 cotopaxi sigchos chugchilan                                       
# 2 guayas daule la aurora                                            
# 3 guayas salitre general vernaza                                    
# 4 guayas san jacinto de yaguachi yaguachi nuevo                     
# 5 imbabura cotacachi el sagrario                                    
# 6 imbabura ibarra ibarra                                            
# 7 imbabura ibarra la carolina                                       
# 8 imbabura pimampiro sigsipamba                                     
# 9 imbabura san miguel de san miguel de urcuqui cahuasqui            
# 10 imbabura san miguel de san miguel de urcuqui la merced de buenos …
# # ℹ 12 more rows
# # ℹ Use `print(n = ...)` to see more rows
# [1] "cantidad faltante en el mapa"
# [1] 78

str(ec3@data)
write.csv(ec3@data, file="farms_negative_surveilance.csv")

# Reading population
library(readr)
# negative surveillance sn
sn <- read_csv(file = "farms_negative_surveilance.csv", col_types = cols(.default = "c"))
647/(647+393)

str(sn)
sn$cantidad <- as.numeric(sn$cantidad)
sum(sn$cantidad, na.rm = TRUE) #4556

# transferring to the fortified map to plot
# map1 <- fortify(ec3)
# ec3$id <- rownames(ec3@data)
map1$sn <- ec3@data$cantidad[match(map1$id, ec3@data$id)]

# Fig. 1 sn Graphic population and pop densities ----

sn <- ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=sn)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", linewidth=0.01) +
  scale_fill_viridis_c(direction = -1, option = "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("Premises (-)
Gen. surv."),
       x=NULL,
       y=NULL,
       tag = "") +
  xlim(-81.1,-75.1)+
  theme(strip.background = element_blank(), title = NULL,
        legend.key.width = unit(0.25, 'cm'))+
  theme_minimal(base_size = 8)+
  north(map1, symbol = 3) +
  ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09) +
  geom_point(data=points, aes(x=x, y=y),
             size=0.3, shape=21, fill="orange", color="black")+
  geom_point(data=c, aes(x=x, y=y),
             size=0.3, shape=21, fill="orange", color="black")


# Fig 2 Creating composite map 4 maps in 1 ----
tiff(filename = "Fig-3-4mapsx.tiff",
     width=16, height=13, units="cm", res=1200,
     compression = "lzw", pointsize = 12)

ggpubr::ggarrange(tem, ot, pm, pp, ncol = 2, nrow = 2,
                  common.legend = FALSE)
dev.off()

# Analyzing densitites

summary(ec3@data$pop)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  2.0    838.8   2326.0   4780.7   5353.8 121853.0       58 

summary(ec3@data$popden)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0024   8.3193  22.2637  35.3001  47.2803 808.7492       58 

summary(ec3@data$min)

popdb <- ec3@data %>% 
  group_by(DPA_DESPRO) %>% 
  summarize(p=sum(pop, na.rm = TRUE), 
            t=mean(min, na.rm = TRUE))%>% 
  arrange(desc(p))

popdendb <- ec3@data %>% 
  group_by(DPA_DESPRO) %>% 
  summarize(d=mean(popden, na.rm = TRUE)) %>% 
  arrange(desc(d))

popdb$density <- popdendb$d

popdb <- popdb[,c(1,2,4,3)]

write.csv(popdb, file = "population-density-province.csv")

ec3@data
colnames(ec3@data)
ec2pop <- ec3@data[, c(1,8,6,2,25,24,26,11:22)]
write.csv(popdb, file = "population-density-parish.csv")


# Logistic model ----
# +++++++++++++++++++
library(stats)
library(epiDisplay)
library(ResourceSelection)
library(modEvA)
library(PerformanceAnalytics)

d <- data.frame(ec3@data)
colnames(d)
d <- d %>% 
  mutate(caso = ifelse(is.na(inter),0,1)) 

# 0 Dependent variable Case ----
d$fcaso <- factor(d$caso)

# Modifying observation that are outside the location
d$fcaso[d$DPA_PARROQ == "050555"] <-0
d$fcaso[d$DPA_PARROQ == "180350"] <-0
d$fcaso[d$DPA_PARROQ == "060150"] <-0
d$fcaso[d$DPA_PARROQ == "060651"] <-0
d$fcaso[d$DPA_PARROQ == "030157"] <-0

# Variable optimal temperature
d <- d %>% 
  mutate(optimal = ifelse(is.na(opt3),0,1))

d$ftoptimal <- factor(d$optimal)
table(d$ftoptimal) #561 ok

table(is.na(d$pop))
d <- d %>% 
  mutate(pop2 = ifelse(is.na(pop),0,pop))

# Descriptive
colnames(d)
# all variables 
chart.Correlation(d[,c(11:22,37)])

# Not correlated
chart.Correlation(d[,c(13:22,37)])
chart.Correlation(d[,c(16,19:20,28)])

cor(d[,c(11:22,25)])

# Model selection
# Backwards ----
m0 <- glm(d$fcaso ~ d$max + d$min + d$tdriest + d$pdriest + d$pwettest + d$pseason + d$pwarm +
            d$pcold + d$tseason + d$Prec + d$twettest + d$Temp, family = binomial(logit)) #AIC 758.5
summary(m0)
logistic.display(m0)

m1 <- glm(d$fcaso ~ d$max + d$min + d$tdriest + d$pdriest + d$pwettest + d$pseason + d$pwarm +
            d$pcold + d$tseason + d$Prec + d$twettest + d$Temp + d$pop2, family = binomial(logit)) #AIC 758.5
summary(m1)
logistic.display(m1)

# Univariables models ----
m1 <- glm(d$fcaso ~ d$pwettest, family = binomial(logit))
summary(m1)
cbind(exp(coef(m1)), exp(confint(m1)))

m2 <- glm(d$fcaso ~ d$Prec, family = binomial(logit))
summary(m2)
cbind(exp(coef(m2)), exp(confint(m2)))

m3 <- glm(d$fcaso ~ d$pop2, family = binomial(logit))
summary(m3)
cbind(exp(coef(m3)), exp(confint(m3)))

m4 <- glm(d$fcaso ~ d$pdriest, family = binomial(logit))
summary(m4)
cbind(exp(coef(m4)), exp(confint(m4)))

m5 <- glm(d$fcaso ~ d$max, family = binomial(logit))
summary(m5)
cbind(exp(coef(m5)), exp(confint(m5)))

m6 <- glm(d$fcaso ~ d$min, family = binomial(logit))
summary(m6)
cbind(exp(coef(m6)), exp(confint(m6)))

m7 <- glm(d$fcaso ~ d$tseason, family = binomial(logit))
summary(m7)
cbind(exp(coef(m7)), exp(confint(m7)))

# Multivariable ----
m1 <- glm(d$fcaso ~ d$pwettest + d$Prec + d$max + d$pop2 + d$min + d$pdriest +
            d$tseason, family = binomial(logit))
mr <- m1

m2 <- glm(d$fcaso ~ d$pwettest + d$pop2 + d$pdriest + d$max + d$min + d$tseason, family = binomial(logit)) #winner model
mr <- m2

m3 <- glm(d$fcaso ~ d$pwettest + d$pop2 + d$min + d$tseason , family = binomial(logit))
mr <- m3

anova(m2,m3, test = "Chisq")

summary(mr)
logistic.display(mr)

d$m_prob <- predict.glm(mr, type="response")
d$predicted <- ifelse(d$m_prob < 0.5, 0, 1)
d$correct <- ifelse(d$predicted == d$caso, 1, 0)
table(d$correct)
table(d$correct)/length(d$fcaso)
table(d$correct[d$fcaso == 1]) #11
table(d$correct[d$fcaso == 0]) #874
hoslem.test(mr$y, fitted(mr)) #70% good fit
plotGLM(obs = mr$y, pred = mr$fitted.values) #r2:0.08, d2=0.10
AUC(model = mr) #0.73
library(car)
car::vif(mr)
car::outlierTest(mr)
car::influenceIndexPlot(mr, cex=0.01, main=NULL)

d <- d[-851,]
d <- d[-611,]


#< Predicted probability ----
#Individual probability of having the 
predicted.data <- data.frame(
  probabilidade.caso=mr$fitted.values,
  p.caso=mr$y)

predicted.data <- predicted.data[
  order(predicted.data$probabilidade.caso, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

head(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probabilidade.caso)) +
  geom_point(aes(x=rank, y=probabilidade.caso), stroke=0.05)+ 
  geom_point(aes(color=probabilidade.caso), alpha=0.9, shape=1, stroke=0.5) +
  theme(text = element_text(size = 10))+
  xlab("Predicted") +
  ylab("Case probability") +
  labs(color = "Pred.
Prob")+
  scale_color_viridis_c()+
  labs(tag = "b")+
  theme_linedraw()

# +++++++++++++++++++++++++++++++++++++++++
# Agregating t the map
map1$fcaso <- d$fcaso[match(map1$id, d$id)]
map1$predicted <- d$predicted[match(map1$id, d$id)]
map1$correct <- d$correct[match(map1$id, d$id)]
map1$prob <- d$m_prob[match(map1$id, d$id)]

# plot parish with presence of BTV and points
  ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, label= DPA_PARROQ, group = group, fill=factor(fcaso))) +
  geom_label()+
  geom_point(data=points, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")
  
# plot parish with predicted presence of Cullicoides
  tiff(filename = "Figpresence-cullicoides.tiff", width=13, height=10,
       units="cm", res=600,compression = "lzw", pointsize = 12)
  
  ggplot()+
    geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=factor(predicted))) +
    geom_path(data=map1, aes(x=long, y=lat, group=group),
              colour="black", size=0.01) +
    geom_point(data=points, aes(x=x, y=y),
               size=1, shape=21, fill="orange", color="black")
  
  dev.off()
  
# Predicao correta
  tiff(filename = "Fig.prediction.tiff", width=13, height=10,
       units="cm", res=300,compression = "lzw", pointsize = 12)
  
  ggplot()+
    geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=factor(correct))) +
    geom_path(data=map1, aes(x=long, y=lat, group=group),
              colour="black", size=0.01) +
    geom_point(data=points, aes(x=x, y=y),
               size=1, shape=21, fill="orange", color="black")
  dev.off()
  
  # Fig 3. Probability map ----

  tiff(filename = "Fig.predictionM2.tiff", width=11.5, height=10,
       units="cm", res=1200,compression = "lzw", pointsize = 12)
  
    ggplot()+
    geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=prob)) +
    geom_path(data=map1, aes(x=long, y=lat, group=group),
                       colour="black", size=0.01) +
    scale_fill_viridis_c(direction = -1, option = "C", na.value = "gray90")+
    labs(fill=("BTV
prob"),
           x=NULL,
           y=NULL) +
    geom_point(data=points, aes(x=x, y=y),
                 size=0.01, shape=3, color="blue") +
    theme_minimal(base_size = 8)+
    theme(strip.background = element_blank(), title = NULL,
            legend.key.width = unit(0.25, 'cm'))+
    north(map1, symbol = 3) +
    ggsn::scalebar(map1, dist = 100, dist_unit = "km",transform = TRUE,
                     model = "WGS84", st.size = 2, st.dist = 0.05,
                     anchor = c(x = -76, y = -4.5),
                     height = 0.02, border.size = 0.09)
  
    dev.off()

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
    