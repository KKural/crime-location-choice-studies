
# Chennai_WardsAndGrids_02.R ####
# June 2021 (WB/KA)


# Load all required packages ####
library(sf)             # spatial processing
library(car)            # test linear hypotheses
library(ggspatial)      # mapping
library(ggcorrplot)     # plot correlation table
library(cowplot)        # graphics in panels
library(tidyverse)      # data manipulation commands
library(readxl)         # read excel file
library(survival)       # estimate conditional logit
library(mctest)         # test multicollinearity
# clear the workspace 
rm(list=ls())

# Set folder names (must be adapted locally)
# Root folder
PROJECT    <- "C:/Users/wimbe/KINGSTON/Projects/ChennaiSnatching/"
# This is where the original source data are
DATAFOLDER <- paste0(PROJECT, "Data/Original/Chennai_2/")
# This folder is for intermediate data and results
WORKFOLDER <- paste0(PROJECT, "Data/Workdata/")
# This folder is for stored output
OUTPUTFOLDER <- paste0(PROJECT, "Output/")


# Helper function copied from 
# https://github.com/datameet/Municipal_Spatial_Data
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# Read ward population data
WardPopulation <- 
  read_xlsx(paste(DATAFOLDER,
                  "20200608_New_Chennai_ward_wise_population.xlsx", 
                  sep="")) %>%
  rename(Residences = No_of_Residential_Building,
         Households = `No_ of_Household`)


# Read Chennai ward polygons 
#  (this uses WGS 1984, EPSG:4326, with longitude and latitude)
Chennai_Wards_WGS84 <- 
  sf::read_sf(dsn=paste(DATAFOLDER, "Wards.geojson", sep=""), quiet=FALSE)
# Convert to (planar) projection EPSG:24381 (appropriate for Chennai)
Chennai_Wards_PL  <- st_transform(Chennai_Wards_WGS84, 24381) %>% 
  # and add the population data
  left_join(WardPopulation, by="Ward_No")

# Function: Create square grids of varying sizes
MakeGridPolygon <- function(sfc, size) {
  SquareGrid <-
    sfc %>%
    st_make_grid(cellsize=size, square=TRUE)
  SquarePolygon <-
    st_sf(tibble(ID=1:length(SquareGrid), 
                 geometry=(st_sfc(SquareGrid))))
  # get centroid
  SquarePolygon$centroids  <- 
    SquarePolygon %>% st_centroid() %>% st_geometry()
  # copy centroid coordinates into 'Centroid_x' and 'Centroid_y'
  SquarePolygon <- sfc_as_cols(SquarePolygon, 
                               st_centroid(centroids),
                               names=c("Centroid_x", "Centroid_y"))
  # calculate area surface from geometry (in square km2)
  SquarePolygon$area <- st_area(SquarePolygon) / 1000000
  return(SquarePolygon)
}

# Create a single polygon (outer boundary) for Chennai
ChennaiBoundary_PL <- st_union(Chennai_Wards_PL)

# Create grids with grid cell sizes 200, 500, 1000 and 1500m
ChennaiSquarePolygon_200m  <- MakeGridPolygon(Chennai_Wards_PL, 200)
ChennaiSquarePolygon_500m  <- MakeGridPolygon(Chennai_Wards_PL, 500)
ChennaiSquarePolygon_1000m <- MakeGridPolygon(Chennai_Wards_PL,1000)
ChennaiSquarePolygon_1500m <- MakeGridPolygon(Chennai_Wards_PL,1500)
# Crop on Chennia boundary (cut off areas outside the outer boundary of Chennai)
ChennaiSquarePolygonCrop_200m  <- st_intersection(ChennaiBoundary_PL, ChennaiSquarePolygon_200m)
ChennaiSquarePolygonCrop_500m  <- st_intersection(ChennaiBoundary_PL, ChennaiSquarePolygon_500m)
ChennaiSquarePolygonCrop_1000m <- st_intersection(ChennaiBoundary_PL, ChennaiSquarePolygon_1000m)
ChennaiSquarePolygonCrop_1500m <- st_intersection(ChennaiBoundary_PL, ChennaiSquarePolygon_1500m)

# Function: Plot wards and grids
ggPlotWithGrid <- function(base, grid, color="black") {
  base %>%
    ggplot() +
    geom_sf(show.legend=FALSE) +
    #geom_point(aes(x=WardCentroid_x, y=WardCentroid_y), size=.6) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(),  axis.text.y=element_blank(),        
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          legend.position="right") +
    # scale bar
    annotation_scale(location = "tl", width_hint = 0.5, 
                     height = unit(0.10, "cm"),
                     text_cex = 0.6) +
    geom_sf(data=grid, color=color, fill=NA, show.legend=FALSE)
}

# Plot the uncropped grids
ggp_1500m <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiSquarePolygon_1500m, "blue")
ggp_1000m <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiSquarePolygon_1000m,  "purple")
ggp_500m  <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiSquarePolygon_500m,  "orange")
ggp_200m  <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiSquarePolygon_200m,  "red")
# Plot the cropped grids
ggp_crop1500m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiSquarePolygonCrop_1500m, "blue")
ggp_crop1000m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiSquarePolygonCrop_1000m,  "purple")
ggp_crop500m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiSquarePolygonCrop_500m,  "orange")
ggp_crop200m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiSquarePolygonCrop_200m, "red")

# Plot in panels
plot_grid(ggp_crop1500m, ggp_crop1000m, ggp_crop500m, ggp_crop200m, nrow=1 )

# Spatial interpolation of population data (on cropped grids)
aw1500m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                     to=ChennaiSquarePolygonCrop_1500m, extensive=TRUE)
aw1000m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                           to=ChennaiSquarePolygonCrop_1000m, extensive=TRUE)
aw500m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                           to=ChennaiSquarePolygonCrop_500m, extensive=TRUE)
aw200m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                          to=ChennaiSquarePolygonCrop_200m, extensive=TRUE)

# function to plot number of residences (on cropped grids)
PlotResidences <- function(x) {
  x %>%
    ggplot() +
    geom_sf(aes(fill=Residences), size=.05, color="lightgrey") +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(),  axis.text.y=element_blank(),        
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          legend.position="right") +
    # scale bar
    annotation_scale(location = "tl", width_hint = 0.5, 
                     height = unit(0.10, "cm"),
                     text_cex = 0.6) 
}

# Plot # of residences per grid cell
ggp_Residences1500m <- PlotResidences(aw1500m)
ggp_Residences1000m <- PlotResidences(aw1000m)
ggp_Residences500m <- PlotResidences(aw500m)
ggp_Residences200m <- PlotResidences(aw200m)

# Plot in panels (dropped ggp_Residences200m as its scale is too small to plot nicely)
plot_grid(ggp_Residences1500m, ggp_Residences1000m, ggp_Residences500m, nrow=1 )