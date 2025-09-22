#06_Geographic 

library(fs)
library(terra)
library(sf)
library(dplyr)

data_dir <- fs::path("Required_Spatial_Files")
geo_dir <- fs::path(data_dir, "05_geographical_data")

out_dir <- fs::path("outputs")
draft_out <- fs::path(out_dir, "draft")


# Loading raster and create a blank raster -------------------------------------------

rtemp <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "cum_currmap_bc100m.tif"))
rtemp[rtemp > -1] <- 0

#rtemp_poly <- as.polygons(rtemp)
aoi <- st_read(path(data_dir, "AOI", "BC_Outline.gpkg")) |> 
  rename("aoi" = juri_en) |> 
  select(aoi)

# read in geographic dataset and create raster stack
list.files(geo_dir)

# read in the static conservation area

#https://bcparks.ca/about/our-mission-responsibilities/types-parks-protected-areas/


#1) Admin conservation Lands

#list.files(path(geo_dir, "Adminstrative Conservation Lands"))

ad <- st_read(path(geo_dir, "Adminstrative Conservation Lands", "WCLCNSRVTN_polygon.gpkg" )) |>
  mutate(type = "admin_cons_lands") |> 
  mutate(type_score = 1) 

ad <- st_intersection(ad, aoi)

# adsf <- ad |> 
#   mutate(area_ha = as.numeric(st_area(geom)/10000))
# 
# st_write(adsf, path( draft_out,"TEST.gpkg"))

adr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
adr[is.na(adr)]<- 0
adr <- mask(adr, rtemp)


#2) bc parks conservancy 

#list.files(path(geo_dir, "BC Parks Conservancy"))

ad <- st_read(path(geo_dir, "BC Parks Conservancy", "TA_CA_SVW_polygon.gpkg" )) |>
  mutate(type = "conservancy")|> 
  mutate(type_score = 1) 

cor <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
cor[is.na(cor)]<- 0
cor <- mask(cor, rtemp)


# 3) Protected areas 
#list.files(path(geo_dir, "Protected Areas"))

ad <- st_read(path(geo_dir, "Protected Areas", "TA_PEP_SVW_polygon.gpkg" )) |>
  mutate(type = PROT_DESG)|> 
  mutate(type_score = 1) 

prr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
prr[is.na(prr)]<- 0
prr <- mask(prr, rtemp)




#4) Protected conservation areas (federal)

#st_layers(path(geo_dir, "Protected Conserved Areas", "ProtectedConservedArea_2024.gpkg" ))
ad <- st_read(path(geo_dir, "Protected Conserved Areas", "ProtectedConservedArea_2024.gpkg" )) |>
  select(TYPE_E) |> 
  filter(TYPE_E != "Old Growth Management Areas (Mapped Legal)") |> 
  st_cast("MULTIPOLYGON")

ad <- st_intersection(ad, aoi) 
ad <- ad |> mutate(type_score = 1) 


pcr <- rasterize(ad, rtemp, field ="type_score", cover = FALSE, touches = TRUE)
pcr[is.na(pcr)]<- 0
pcr <- mask(pcr, rtemp)

# Lets join the protected areas to review 

pro <- adr +  cor + prr + pcr

plot(pro)

writeRaster(pro, path( draft_out, "2_geographic_static.tif"))

#################################################################################

# identification of connecting pieces, refugia? 

combo <- out[[1]]+ out[[2]] + out[[3]]+ out[[4]]+ out[[5]] +out[[6]] + out[[7]]+
         out[[8]]+ out[[9]]+ out[[10]]+ out[[11]]



#######
## catergorise the flow of climate or connectivity

# read in connectivity layer 
#list.files(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale"))

con <- rast(fs::path(data_dir, "05_geographical_data", "General Landscape Connectivity BC","Provincial scale", "normalized_cum_currmap_bc100m.tif"))

# reclass the raster 
m <- c(0, 0.2, 10,
       0.2, 0.7, 2,
       0.7, 1.3 , 1,
       1.3, 1.7, 3,
       1.7, 50, 5 ) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
con_class <- classify(con, rclmat, include.lowest=TRUE)

con_class[is.na(con_class )]<- 0
con_class <- mask(con_class , rtemp)
plot(con_class)

#writeRaster(th_class , file.path(draft_out , "2_threat_focal_class.tif"), overwrite = TRUE)




# read in the parks 
library(terra)
library(sf)
library(purrr)
#library(stats)

parks <- rast(path( draft_out, "2_geographic_static.tif"))

# extract the level 3 parks as test 

# sieve the holes and remove the smallest areas 
park <- sieve(parks, threshold = 50, directions = 8)

# reclass the raster 
m <- c(0, 2, 0,
       2, 4 , 1) # highest eco value

rclmat <- matrix(m, ncol=3, byrow=TRUE)
parks_class <- classify(park, rclmat, include.lowest=TRUE)
plot(parks_class)
parks_class <- mask(parks_class, rtemp)

# convert to polygon 
parks_sf <- st_as_sf(as.polygons(parks_class))

parks_sf <- parks_sf |> 
  filter(type_score == 1) |> 
  st_cast("POLYGON") 

#parks_sf <- parks_sf |> 
#  mutate(area_ha = as.numeric(st_area(geometry)/10000))

#parks_sf <- parks_sf |> 
#  filter(area_ha > 100)

# testing lines 
parks <- parks_sf[1:3,]

# Get park centroids
park_coords <- st_coordinates(st_centroid(parks))
n_parks <- nrow(park_coords)

# Create all park pairs
#park_pairs <- expand.grid(i = 1:(n_parks-1), j = 2:n_parks) %>%
#  filter(i < j)

# 
# r <- rast(ncols=10, nrows=10,  xmin=0, xmax=10, ymin=0, ymax=10, 
#           vals=10, crs="+proj=utm +zone=1 +datum=WGS84")
# r[5, 1] <- -10
# r[2:3, 1] <- r[1, 2:4] <- r[2, 5] <- 0
# r[3, 6] <- r[2, 7] <- r[1, 8:9] <- 0
# r[6, 6:10] <- NA
# r[6:9, 6] <- NA
# 
# d <- costDist(r, -10)
# plot(d)
# text(d, digits=1, cex=.8)



# add the type score to con_class 

parks_class

# reorder the values
parks_class1 <- subst(parks_class, 1, -100)

con_class1 <- parks_class1 + con_class
plot(con_class1)


d <- con_class1d <- costDist(con_class, 1)




# example of cost distance and path 
library("gdistance")
library("raster")


r <- raster(con_class)

#r <- raster(system.file("external/maungawhau.grd", package = "gdistance"))
altDiff <- function(x){x[2] - x[1]}
hd <- transition(r, altDiff, 8, symm=FALSE)
plot(raster(hd), sub="non geo-corr", cex=0.8)
#text(raster(hd),digits=3)

# 3) This is then geoCorrected to slope, i.e. altitude diff / distance travelled
slope <- geoCorrection(hd,scl=FALSE)
#plot(raster(slope), sub="slope (height diff / distance))", cex.sub=0.8)
#text(raster(slope),digits=3)

adj <- adjacent(r, cells = 1:ncell(r), pairs = TRUE, directions = 8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))

Conductance <- geoCorrection(speed) 


A <- c(2667670, 6479000)
B <- c(2667800, 6479400)
AtoB <- shortestPath(Conductance, A, B, output = "SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output = "SpatialLines")

plot(r, xlab = "x coordinate (m)", ylab = "y coordinate (m)", legend.lab = "Altitude (masl)")
lines(AtoB, col = "red", lwd = 2)
lines(BtoA, col = "blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")













