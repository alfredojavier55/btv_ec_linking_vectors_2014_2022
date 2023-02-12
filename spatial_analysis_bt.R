# Bluetongue in Ecuador
# Alfredo Acosta alfredojavier55@gmail.com

# Ploting the maps ----

library(raster); library(rgeos); library(dplyr);library(rgdal)
library(ggplot2); library(ggsn)

# To improve definition of time to 0.5 arc I need the tile points
# https://rsh249.github.io/spatial_bioinformatics/worldclim.html
# https://gis.stackexchange.com/questions/227585/using-r-to-extract-data-from-worldclim?newreg=3d134adbf73948c8864e97e6a22f1841

setwd("~/Dropbox/0.Postdoc/Publications/Bluetongue/")
r <- getData(name="worldclim", var="bio", res=2.5)

r <- r[[c(1,6,5,12)]]
names(r) <- c("Temp","min", "max", "Prec")

#See the global information extracted
plot(r)

#Import my shapefile
ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/10.2020 II sem/FLI/case-control/bayesian/",layer="ec3.1006")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
trueCentroids <- data.frame(gCentroid(ec3,byid=TRUE))

#Add centroids to the shapefile
ec3@data$lat <- trueCentroids$y
ec3@data$long <- trueCentroids$x

#Create a layer of spatial points
points <- SpatialPoints(trueCentroids, proj4string = r@crs)

#Extract bio data to spatial points
values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)

df$Temp <- df$Temp/10
df$min <- df$min/10
df$max <- df$max/10

#Transfer data points to shape data
ec3@data$Temp <- df$Temp
ec3@data$Prec <- df$Prec
ec3@data$min <- df$min
ec3@data$max <- df$max

# See the data of Ecuador
plot(r, xlim=c(-82,-70), ylim=c(-6,2))
datos <- data.frame(ec3@data)

#Removing galapagos province
#I have to redo just to galapagos to see the surv forecast to the island
ec <- ec3
ec <- subset(ec, DPA_DESPRO != "GALAPAGOS")
ec$id <- rownames(ec@data)

#creating a map to plot and bringing the information
map1 <- fortify(ec)
map1$DPA_PARROQ <- ec@data$DPA_PARROQ[match(map1$id, ec@data$id)]
map1$temp <- ec@data$Temp[match(map1$id, ec@data$id)]
map1$prec <- ec@data$Prec[match(map1$id, ec@data$id)]
map1$min <- ec@data$min[match(map1$id, ec@data$id)]
map1$max <- ec@data$max[match(map1$id, ec@data$id)]

# Reading outbreaks coordinatess
c <- read.csv(file = "Coordinates.csv")
d <- read.csv(file = "Coordinates.Wahis.csv")
e <- read.csv(file = "manabi.lon.lat.csv")

# Temperature mean map ----
tiff(filename = "Fig.3temp_mean.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=temp)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("Mean°C"),
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
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black") +
  geom_point(data=d, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=e, aes(x=lon, y=lat),
             size=1, shape=21, fill="orange", color="black")

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
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black") +
  geom_point(data=d, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=e, aes(x=lon, y=lat),
             size=1, shape=21, fill="orange", color="black")

dev.off()

# Temperature max map ----
tiff(filename = "Fig.3temp_max.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=max)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("Max°C"),
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
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black") +
  geom_point(data=d, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=e, aes(x=lon, y=lat),
             size=1, shape=21, fill="orange", color="black")

dev.off()



# Affected zone only zoom
tiff(filename = "Fig.2afectedBT.tiff", width=10, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map1, aes(x=long, y=lat, group = group, fill=temp)) +
  geom_path(data=map1, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("°C"),
       x="Longitude",
       y="Latitude") +
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  theme_minimal()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)+
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black")+
  coord_cartesian(xlim=c(-79.5,-79), ylim=c(-1, 0.5))

dev.off()  


# Creating a spatial data points
# Reading coordinatess of outbreak
c <- read.csv(file = "Coordinates.csv")
d <- read.csv(file = "Coordinates.Wahis.csv")
e <- read.csv(file = "manabi.lon.lat.csv")

#Changing names
colnames(c)[1] <- "id"
colnames(e)[2] <- "id"
colnames(e)[5] <- "X"
colnames(e)[6] <- "Y"
colnames(e)[7] <- "x"
colnames(e)[8] <- "y"
points <- rbind(c, d[,c(1,5,6)], e[,c(2,7,8)])

# Create the spatial points
sp <- SpatialPoints(points[,c(2,3)])

# Use the entire map to intersect the points and polygons
ec3@data$inter <- over(SpatialPolygons(ec3@polygons), sp)

table(is.na(ec3@data$inter))

# Parishes with bluethonge
summary(ec3@data$Temp[!is.na(ec3@data$inter)])

# Parishes withouth bluethonge
summary(ec3@data$Temp[is.na(ec3@data$inter)])


# Selecting the risk areas
ec3@data %>% 
  group_by(inter) %>% 
  summarize(m=mean(Temp))
  ggplot(aes())

mean(c(23.6, 12.9))
  

# Subseting maps with 12-26 degrees do not work because it deletes que polygons
# map2 <- subset(map1, temp >12 & temp <26)
# summary(map2$temp)

map2 <- map1
map2$opt <- map2$min[map2 >12.9,] 

temp <- map2 %>% 
  mutate(opt = ifelse(min >12, 1, 0))%>%
  mutate(opt2 = ifelse(max <26, 1, 0)) %>% 
  mutate(opt3 = ifelse(sum(opt,opt2)==2,1,0))
  
ec3@data <- ec3@data %>% 
  mutate(opt = ifelse(min >12, 1, 0))%>%
  mutate(opt2 = ifelse(max <32, 1, 0)) %>% 
  mutate(opt3 = opt+opt2) %>% 
  mutate(opt3 = ifelse(opt3==2, Temp, NA))

#Please change the working directory in dsn
ec <- ec3
ec <- subset(ec, DPA_DESPRO != "GALAPAGOS")
ec$id <- rownames(ec@data)
map2 <- fortify(ec)

map2$DPA_PARROQ <- ec@data$DPA_PARROQ[match(map2$id, ec@data$id)]
map2$temp <- ec@data$Temp[match(map2$id, ec@data$id)]
map2$prec <- ec@data$Prec[match(map2$id, ec@data$id)]
map2$min <- ec@data$min[match(map2$id, ec@data$id)]
map2$max <- ec@data$max[match(map2$id, ec@data$id)]
map2$temp2 <- ec@data$opt3[match(map2$id, ec@data$id)]

f <- read.csv(file = "manabi.lon.lat.csv")

tiff(filename = "Fig.4map_ideal.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map2, aes(x=long, y=lat, group = group, fill=temp2)) +
  geom_path(data=map2, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("°C"),
       x="Longitude",
       y="Latitude") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  theme_minimal()+
  north(map2, symbol = 3) +
  ggsn::scalebar(map2, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)+
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black")+
  geom_point(data=d, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=f, aes(x=lon, y=lat),
             size=1, shape=21, fill="orange", color="black")

dev.off()


ec3@data <- ec3@data %>% 
  mutate(opt = ifelse(min >12, 1, 0))%>%
  mutate(opt2 = ifelse(max <32, 1, 0)) %>% 
  mutate(opt3 = opt+opt2) %>% 
  mutate(opt3 = ifelse(opt3==2, Temp, NA))

#Please change the working directory in dsn
ec <- ec3
ec <- subset(ec, DPA_DESPRO == "GALAPAGOS")
ec$id <- rownames(ec@data)
map2 <- fortify(ec)

map2$DPA_PARROQ <- ec@data$DPA_PARROQ[match(map2$id, ec@data$id)]
map2$temp <- ec@data$Temp[match(map2$id, ec@data$id)]
map2$prec <- ec@data$Prec[match(map2$id, ec@data$id)]
map2$min <- ec@data$min[match(map2$id, ec@data$id)]
map2$max <- ec@data$max[match(map2$id, ec@data$id)]
map2$temp2 <- ec@data$opt3[match(map2$id, ec@data$id)]

f <- read.csv(file = "manabi.lon.lat.csv")

tiff(filename = "Fig.4map_ideal.tiff", width=13, height=10,
     units="cm", res=600,compression = "lzw", pointsize = 12)

ggplot()+
  geom_polygon(data=map2, aes(x=long, y=lat, group = group, fill=temp2)) +
  geom_path(data=map2, aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  scale_fill_viridis_c(direction = 1, option =  "D", na.value = "gray90")+
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("°C"),
       x="Longitude",
       y="Latitude") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  theme_minimal()+
  north(map2, symbol = 3) +
  ggsn::scalebar(map2, dist = 100, dist_unit = "km",transform = TRUE,
                 model = "WGS84", st.size = 2, st.dist = 0.05,
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09)+
  geom_point(data=c, aes(x=x, y=y),
             size=2, shape=21, fill="red", color="black")+
  geom_point(data=d, aes(x=x, y=y),
             size=1, shape=21, fill="orange", color="black")+
  geom_point(data=f, aes(x=lon, y=lat),
             size=1, shape=21, fill="orange", color="black")

dev.off()

