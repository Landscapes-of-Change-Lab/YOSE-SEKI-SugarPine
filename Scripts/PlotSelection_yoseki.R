## ---------------------------
##
## Script name: Plot selection for sugar pine in Yosemite
##
## Author: Joan Dudney, Michelle Mohr
##
## Date Created: June 20, 2024
##
## ---------------------------
##
## Notes: Randomly samples 5 plots using a stratified random sampling design
##  for Yosemite sugar pine
## ---------------------------



## Install the librarian package if not yet installed using the Require package 

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sgsR, tmaptools,tmap, rsconnect)

## ggplot theme
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                   STEP 1                
#           Reading in spatial data, cleaning them, and reprojecting 
#           them so they all have the same CRS
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## pila polygon
yose_pila <- "Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
plot(pila.shp)

## creating a project CRS
prjcrs <- toString(crs(pila.shp))

## reading in roads and trails data
roads_trails <- vect("Data/Trails and Roads/bufferToLines_YOSEtrailsRoads.shp")
plot(roads_trails)

## project to project crs
proj_roadstrails <- project(roads_trails, prjcrs)
plot(proj_roadstrails)

## read in using sf package for the sgsR package
roads_sf <- st_read("Data/Trails and Roads/Union_roads_trails_YOSE.shp")
proj_roads_sf <- st_transform(roads_sf, prjcrs)

## reading in prism data that's loosely masked to pila distribution
vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
file.exists(vpd)

vpd_prism <- rast(vpd)

## summary stats
hist(vpd_prism)
dataprism <- data.frame(vpd_prism)
quantile(dataprism$PRISM_vpdmax_30yr_normal_800mM4_annual_bil,probs = c(.1, .3, .6, .9))

vpd_proj <- project(vpd_prism, prjcrs)
cropvpd <- crop(vpd_proj, yose.shp)

## making sure projections are correct
plot(cropvpd)
plot(pila.shp, add=TRUE, col="blue")

## calculating the prism strata
maskvpdpila <- mask(cropvpd, pila.shp)
data_maskvpd <- data.frame(maskvpdpila)
percentile <- ecdf(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
round(percentile(c(12, 14, 16, 18)),digits =2)
round(percentile(c(12.5, 14, 16, 18)),digits =2)

max(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
min(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)


## reading in buffer data that includes rivers, streams and lakes
buffer_streams <- vect("Data/Lakes and Streams/buffer_lakesstreams_25m10m.shp")
buff_streamsrivers <- project(buffer_streams,prjcrs)


## visualizing again
plot(cropvpd)
plot(pila.shp, add=TRUE, col="blue")
plot(buff_streamsrivers, add=TRUE, col="purple")


## reading in slope data and creating a new raster file with slope < 35°
## slope data
slope <- rast("Data/YOSE DEM/YoseSlope.tif")
plot(slope)

## clipping by 35°
slope_rast <- app(slope, fun=function(x) { 
  x[x > 35] <- NA; return(x)} )
plot(slope_rast)

## reproject the file
proj_slope <- project(slope_rast, prjcrs)

## checking results
m.slope <- as.matrix(slope_rast)
max(m.slope, na.rm=T)
hist(m.slope) ## it worked!



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                        STEP 2
#      
#       Extract climate data by sugar pine range
#       and create a buffer for streams and lakes
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## first masking pila dist by slope
mask_slope <- proj_slope %>% 
  mask(pila.shp)

plot(mask_slope)

## resampling vpd to match with proj_slope
resampvpd <- resample(cropvpd, proj_slope)
#plot(resampvpd)

## masking vpd with slope raster
mask_vpd <- mask(resampvpd,mask_slope)
plot(mask_vpd)
hist(mask_vpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

mask_all <- mask(mask_vpd, buff_streamsrivers, inverse=T)
plot(mask_all)


## creating the climate strata
maskalldat <- data.frame(mask_all) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

percentile <- ecdf(maskalldat$vpd)
round(percentile(c(12.5, 14, 16, 18)),digits =2)

strata_all <- c(12.5, 14, 16, 18)

## create the stratified raster
raster_strat_buff <- strat_breaks(
  mraster = mask_all,
  breaks =  strata_all)


## visualize and make sure it worked
plot(raster_strat_buff)


##calculating strata
plots_pila <- sample_strat(sraster = raster_strat_buff,# input mraster
                                allocation = "equal",
                                nSamp = 3, # number of desired samples
                                access = proj_roads_sf, # define access road network
                                mindist = 400, # minimum distance samples must be apart from one another
                                buff_inner = 10, # inner buffer - no samples within this distance from road
                                buff_outer = 500, # outer buffer - no samples further than this distance from road
                                plot = TRUE) # plot



plots_random <- vect(plots_pila)
crs(plots_random) <- prjcrs

plot(pila.shp, col="blue")
plot(plots_random,add=T, col = "orange")


## interactive viewing
sf_plots <- st_as_sf(plots_random)
crs(sf_plots)
tmap_mode("view")

sf_plots$strata = as.factor(sf_plots$strata)

tm_shape(sf_plots)+
  tm_dots('strata')



## writing a kml file
st_write(sf_plots, "Data/RandomSP_YOSE.kml", driver = "kml")




