# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reef_Residency

#### LIBRARIES ####
# install.packages(c("easypackages", "here", "dplyr", "raster"))

library(easypackages)
libraries("here", "dplyr", "raster")

#### DIRECTORIES ####
# set working directory and relative project path
setwd("E:/Data/Florida/FL_Patch_Reef_Residency/")
# set_here() # set if using here() in this location for the first time
here::i_am(".here")
here::here() 

# change where large temp rasters are saved
rasterOptions(tmpdir = here("Temporary"))

# save PROJ.4 string for standard projection (ESPG:26958 NAD 83/Florida East) 
my_crs = CRS("+proj=tmerc +lat_0=24.3333333333333 +lon_0=-81 +k=0.999941177 +x_0=200000 +y_0=0 +ellps=GRS80 +towgs84=-2,0,4,0,0,0,0 +units=m +no_defs +type=crs")

# read in the habitat raster from our study area
habitat = raster(here("Data", "Source_Data", "Courtney_Stuart_Rasters", "Habitat.tif"))
raster::crs(habitat) = my_crs # set coordinate system info

URM_classes = read.csv(here("Data", "Source_Data", "URM_ClassLv1_IDs.csv"))
reef = filter(URM_classes, ClassLv1 == "Aggregate Reef")
reef_ID = as.numeric(reef$ID)

# create a new raster, where the value in each cell reflects the
# Euclidean distance (meters) from that cell to the nearest aggregate 
# reef cell.
reef_dist = writeRaster(raster::mask(raster::crop(gridDistance(habitat, origin = reef_ID),
                                                  habitat), habitat),
                        file = here("Data", "Reef_Dist.asc"),
                        format = "ascii", overwrite = T)
