
# Chennai_MultiScale_v05.R ####
# Sept 9, 2021 (WB/KA)
#install.packages(c("sf", "car", "ggspatial", "ggcorrplot", "tidyverse",
#                   "readxl", "survival", "mctest", "cowplot", "wk"))

# Load all required packages ####
library(sf)             # spatial processing
library(car)            # test linear hypotheses
library(ggspatial)      # mapping
library(ggcorrplot)     # plot correlation table
library(tidyverse)      # data manipulation commands
library(readxl)         # read excel file
library(survival)       # estimate conditional logit
library(mctest)         # test multicollinearity
library(cowplot)        # plot_grid functions
library(wk)             # geometry-parsing
# clear the workspace 
rm(list=ls())

# Set folder names (must be adapted locally)
# Root folder
PROJECT    <- "C:/Users/wimbe/KINGSTON/Projects/ChennaiMultiScale/"
# This is where the original source data are
DATAFOLDER <- paste0(PROJECT, "Data/Original/Chennai/")
# This folder is for intermediate data and results
WORKFOLDER <- paste0(PROJECT, "Data/Workdata/")
# This folder is for stored output
OUTPUTFOLDER <- paste0(PROJECT, "Output/")
# This is where scripts are
SCRIPTFOLDER <- paste0(PROJECT, "Scripts/")

# execute these scripts
source(paste0(SCRIPTFOLDER, "01_Functions.R"))          # load functions
source(paste0(SCRIPTFOLDER, "02_ProcessWards.R"))       # process the 201 wards
source(paste0(SCRIPTFOLDER, "03_ProcessFacilities.R"))  # shops, hospitals, temples ..

# Create a single polygon (outer boundary) for Chennai
ChennaiBoundary_PL <- st_union(Chennai_Wards_PL)

# Create grids with grid cell sizes 200, 500, 1000 and 1500m
ChennaiGrid_200m  <- MakeGridPolygon(Chennai_Wards_PL, 200)
ChennaiGrid_500m  <- MakeGridPolygon(Chennai_Wards_PL, 500)
ChennaiGrid_1000m <- MakeGridPolygon(Chennai_Wards_PL,1000)
ChennaiGrid_1500m <- MakeGridPolygon(Chennai_Wards_PL,1500)

# Crop on Chennai boundary (cut off areas outside the outer boundary of Chennai)
ChennaiGridCropped_1500m <- st_intersection(ChennaiGrid_1500m, ChennaiBoundary_PL)
ChennaiGridCropped_1000m <- st_intersection(ChennaiGrid_1000m, ChennaiBoundary_PL)
ChennaiGridCropped_500m <- st_intersection(ChennaiGrid_500m, ChennaiBoundary_PL)
ChennaiGridCropped_200m <- st_intersection(ChennaiGrid_200m, ChennaiBoundary_PL)

# Plot the uncropped grids
ggp_1500m <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiGrid_1500m, "blue")
ggp_1000m <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiGrid_1000m,  "purple")
ggp_500m  <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiGrid_500m,  "orange")
ggp_200m  <- ggPlotWithGrid(Chennai_Wards_PL, ChennaiGrid_200m,  "red")
# Plot the cropped grids
ggp_crop1500m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiGridCropped_1500m, "blue")
ggp_crop1000m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiGridCropped_1000m,  "purple")
ggp_crop500m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiGridCropped_500m,  "orange")
ggp_crop200m <- ggPlotWithGrid(ChennaiBoundary_PL, ChennaiGridCropped_200m, "red")

# Plot in panels
ggp_all <-
  plot_grid(ggp_1500m, ggp_1000m, ggp_500m, ggp_200m, nrow=1 )
ggp_crop_all <-
  plot_grid(ggp_crop1500m, ggp_crop1000m, ggp_crop500m, ggp_crop200m, nrow=1 )

PipWards <- function(sf1, sf2, target) {
  t<- enquo(target)
  return(
    st_join(sf1, sf2, join=st_within) %>%
      st_set_geometry(NULL) %>%
      group_by(Ward_No, !!t) %>%
      summarise(Count= n()) %>%
      pivot_wider(names_from=!!t, values_from="Count") %>%
      # remove any facilitities collected in Ward_No == "NA"
      #  (as these are outside-boundary faciltities)
      filter(!is.na(Ward_No)) %>%
      # next line converts all other "NA" to 0
      ungroup() %>%
      # I renamed, to correct spelling errors, to remove spaces from fieldnames 
      #   and to shorten fieldnames
      mutate_all(~replace_na(.,0)) %>%
      left_join(WardPopulation, by="Ward_No")
  )
}  

Facilities_in_Wards_WGS84 <-
  PipWards(Facilities_WGS84, Chennai_Wards_WGS84,
           target = Category)
Facilities_in_Wards_PL <- 
  PipWards(Facilities_PL, Chennai_Wards_PL,
           target=Category)

AggFacilities_in_Wards_WGS84 <-
  PipWards(AggFacilities_WGS84, Chennai_Wards_WGS84,
           target=AggCategory)
AggFacilities_in_Wards_PL    <- 
  PipWards(AggFacilities_PL, Chennai_Wards_PL,
           target=AggCategory)


# point-in-polygon for grid cells
PipGrids <- function(sf1, sf2, target) {
  t<- enquo(target)
  return(x<-
    st_join(sf1, sf2, join=st_within) %>%
      st_set_geometry(NULL) %>%
      group_by(ID, !!t) %>%
      summarise(Count= n(), .groups="drop") %>%
      pivot_wider(names_from=!!t, values_from="Count") %>%
      ungroup() %>%
      # next line converts all other "NA" to 0
      mutate_all(~replace_na(.,0)) 
  )
} 

# Spatial interpolation of population data (on the cropped grids)
aw1500m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                           to=ChennaiGridCropped_1500m, extensive=TRUE) 
aw1000m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                           to=ChennaiGridCropped_1000m, extensive=TRUE) 
aw500m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                          to=ChennaiGridCropped_500m, extensive=TRUE) 
aw200m<-st_interpolate_aw(Chennai_Wards_PL[c("Households", "Population", "Residences")], 
                          to=ChennaiGridCropped_200m, extensive=TRUE) 

# make grids complete (geometry + facilities + population)
FacilitiesInGrids1500m <- 
  PipGrids(Facilities_PL, ChennaiGridCropped_1500m, target=Category) %>%
  right_join(ChennaiGridCropped_1500m, by = "ID") %>%
  right_join(aw1500m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
FacilitiesInGrids1000m <- 
  PipGrids(Facilities_PL, ChennaiGridCropped_1000m, target=Category) %>%
  right_join(ChennaiGridCropped_1000m, by = "ID") %>%
  right_join(aw1000m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
FacilitiesInGrids500m <- 
  PipGrids(Facilities_PL, ChennaiGridCropped_500m, target=Category) %>%
  right_join(ChennaiGridCropped_500m, by = "ID") %>%
  right_join(aw500m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
FacilitiesInGrids200m <- 
  PipGrids(Facilities_PL, ChennaiGridCropped_200m, target=Category) %>%
  right_join(ChennaiGridCropped_200m, by = "ID") %>%
  right_join(aw200m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)

# make grids complete (geometry + facilities + population)
AggFacilitiesInGrids1500m <- 
  PipGrids(AggFacilities_PL, ChennaiGridCropped_1500m, 
           target=AggCategory) %>%
  right_join(ChennaiGridCropped_1500m, by = "ID") %>%
  right_join(aw1500m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
AggFacilitiesInGrids1000m <- 
  PipGrids(AggFacilities_PL, ChennaiGridCropped_1000m, 
           target = AggCategory) %>%
  right_join(ChennaiGridCropped_1000m, by = "ID") %>%
  right_join(aw1000m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
AggFacilitiesInGrids500m <- 
  PipGrids(AggFacilities_PL, ChennaiGridCropped_500m, 
           target = AggCategory) %>%
  right_join(ChennaiGridCropped_500m, by = "ID") %>%
  right_join(aw500m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)
AggFacilitiesInGrids200m <- 
  PipGrids(AggFacilities_PL, ChennaiGridCropped_200m, 
           target = AggCategory) %>%
  right_join(ChennaiGridCropped_200m, by = "ID") %>%
  right_join(aw200m, by = "geometry") %>%
  mutate_all(~replace_na(.,0)) %>%
  arrange(ID)





# Perform some exploratory analyses on ward attribute data
ggplot_WardAttributeFrequencies <- function(tbl) {
  return(tbl %>%
           pivot_longer(cols=c(-Ward_No,-Population, -Households, -Residences), 
                        names_to="Facility", 
                        values_to="Frequency") %>%
           ggplot() +
           geom_bar(mapping=aes(x=Frequency, fill=Facility),
                    show.legend=FALSE) +
           facet_wrap(~Facility, scales="free")
  )
}

Chennai_Wards_Attributes_Frequencies <-
  ggplot_WardAttributeFrequencies(Facilities_in_Wards_PL)
Chennai_Wards_Attributes_Frequencies
ggsave_pdf_jpeg(Chennai_Wards_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)

Chennai_AggWards_Attributes_Frequencies <-
  ggplot_WardAttributeFrequencies(AggFacilities_in_Wards_PL)
Chennai_AggWards_Attributes_Frequencies
ggsave_pdf_jpeg(Chennai_AggWards_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)



# This function is a revision of ggplot_WardAttributeFrequencies() that works on the grid cell datsets
ggplot_GridAttributeFrequencies <- function(tbl) {
  return(tbl %>% 
           select(-geometry, -area, -centroids, -Centroid_x, -Centroid_y) %>%
           pivot_longer(cols=c(-ID,-Population, -Households, -Residences), 
                        names_to="Facility", 
                        values_to="Frequency") %>%
           ggplot() +
           geom_bar(mapping=aes(x=Frequency, fill=Facility),
                    show.legend=FALSE) +
           facet_wrap(~Facility, scales="free")
  )
}
# Facilities not aggregated
ChennaiGrid_1500m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(FacilitiesInGrids1500m)
ggsave_pdf_jpeg(ChennaiGrid_1500m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
ChennaiGrid_1000m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(FacilitiesInGrids1000m)
ggsave_pdf_jpeg(ChennaiGrid_1000m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
ChennaiGrid_500m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(FacilitiesInGrids500m)
ggsave_pdf_jpeg(ChennaiGrid_500m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
ChennaiGrid_200m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(FacilitiesInGrids200m)
ggsave_pdf_jpeg(ChennaiGrid_200m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
# Facilities agregated
AggChennaiGrid_1500m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(AggFacilitiesInGrids1500m)
ggsave_pdf_jpeg(AggChennaiGrid_1500m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
AggChennaiGrid_1000m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(AggFacilitiesInGrids1000m)
ggsave_pdf_jpeg(AggChennaiGrid_1000m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
AggChennaiGrid_500m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(AggFacilitiesInGrids500m)
ggsave_pdf_jpeg(AggChennaiGrid_500m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)
AggChennaiGrid_200m_Attributes_Frequencies <-
  ggplot_GridAttributeFrequencies(AggFacilitiesInGrids200m)
ggsave_pdf_jpeg(AggChennaiGrid_200m_Attributes_Frequencies, OUTPUTFOLDER,
                width=15, units="cm", dpi=600, scale=2)


#########################################################*
# From here onward the code is just the code for the 
#   JQC paper (WB)
#########################################################*

# Spearman correlations between attributes
ggplot_Spearman <- function(tbl) {
  return(
    tbl %>%
      ungroup() %>%
      select(-Ward_No) %>%
      as.matrix() %>%
      cor(use="pairwise.complete.obs", method="spearman") %>%
      ggcorrplot(type="upper", lab=TRUE, title="",
                 show.legend=TRUE, tl.cex=7, pch.cex=5, lab_size=2,
                 insig="blank", legend.title="Spearman",
                 ggtheme =theme_minimal)    
  )
}
# ggplot_Spearman correlations() adapted for grids
Chennai_Wards_Attributes_Correlations <-
  ggplot_Spearman(Facilities_in_Wards_PL)
Chennai_Wards_Attributes_Correlations
ggsave_pdf_jpeg(Chennai_Wards_Attributes_Correlations, OUTPUTFOLDER)

Chennai_AggWards_Attributes_Correlations <-
  ggplot_Spearman(AggFacilities_in_Wards_PL)
Chennai_AggWards_Attributes_Correlations
ggsave_pdf_jpeg(Chennai_AggWards_Attributes_Correlations, OUTPUTFOLDER)

# SpearmanAdapted for grids
ggplot_GridSpearman <- function(tbl) {
  return(
    tbl %>%
      ungroup() %>%
      select(-ID, -geometry, -area, -centroids, -Centroid_x, -Centroid_y) %>%
      as.matrix() %>%
      cor(use="pairwise.complete.obs", method="spearman") %>%
      ggcorrplot(type="upper", lab=TRUE, title="",
                 show.legend=TRUE, tl.cex=7, pch.cex=5, lab_size=2,
                 insig="blank", legend.title="Spearman",
                 ggtheme =theme_minimal)    
  )
}

Chennai_Grids1500_Attributes_Correlations <-
  ggplot_GridSpearman(FacilitiesInGrids1500m)
ggsave_pdf_jpeg(Chennai_Grids1500_Attributes_Correlations, OUTPUTFOLDER)
Chennai_Grids1000_Attributes_Correlations <-
  ggplot_GridSpearman(FacilitiesInGrids1000m)
ggsave_pdf_jpeg(Chennai_Grids1000_Attributes_Correlations, OUTPUTFOLDER)
Chennai_Grids500_Attributes_Correlations <-
  ggplot_GridSpearman(FacilitiesInGrids500m)
ggsave_pdf_jpeg(Chennai_Grids500_Attributes_Correlations, OUTPUTFOLDER)
Chennai_Grids200_Attributes_Correlations <-
  ggplot_GridSpearman(FacilitiesInGrids200m)
ggsave_pdf_jpeg(Chennai_Grids200_Attributes_Correlations, OUTPUTFOLDER)

###################################################################*
# From here onward the code is just the code for the  JQC paper"
###################################################################*


# VIF - Variance Inflation Factors
# Variables with VIF values above 5 may need to be removed
formula_01 <-
  Ward_No ~ `Barber shop` + `Bus stop` + `Church` + `Education` + 
  `General store` + `Government office` + `Hospital` + `Jewellery` + 
  `Marriage hall` + `Medical shop` + `Mosque` +              
  `Park` + `Police station` + `Recreation` + `Restaurant` +
  `Saloon` + `School and college` + `Spa`  + `Supermarket` +  `Temple` + 
  `Textiles` + `Vegetables` + `Beauty parlour`  + `Railway station` +
  `Population` + `Households` + `Residences`  
ward_model_01 <-  lm(formula_01, data=Facilities_in_Wards_PL)
omcdiag(ward_model_01)
imcdiag(ward_model_01)
#CN(model.matrix(formula(formula_01), data=Facilities_in_Wards_PL))
# remove `households`

formula_02 <-
  Ward_No ~ `Barber shop` + `Bus stop` + `Church` + `Education` + 
  `General store` + `Government office` + `Hospital` + `Jewellery` + 
  `Marriage hall` + `Medical shop` + `Mosque` +              
  `Park` + `Police station` + `Recreation` + `Restaurant` +
  `Saloon` + `School and college` + `Spa`  + `Supermarket` +  `Temple` + 
  `Textiles` + `Vegetables` + `Beauty parlour`  + `Railway station` +
  `Population` + `Residences`

ward_model_02 <-  lm(formula_02, data=Facilities_in_Wards_PL)
omcdiag(ward_model_02)
imcdiag(ward_model_02)
#CN(model.matrix(formula(formula_02), data=Facilities_in_Wards_PL))

# 
VIFS <- enframe(vif(ward_model_02), name="Facility", value="VIF") %>%
  mutate(Facility=gsub("\`", "", Facility)) %>%
  mutate(Threshold = case_when(VIF >=5           ~ "[5+)",
                               VIF >=3 & VIF < 5 ~ "[3-5]",
                               VIF < 3           ~ "[0-3)"
  ))

# Plot the VIF values
VarianceInflationFactors <-
  VIFS %>%
  arrange(desc(VIF)) %>%
  ggplot() + 
  geom_col(mapping=aes(x=reorder(Facility, VIF), y=VIF, fill=Threshold)) +
  theme(axis.text.x = element_text(size = 10, angle = 90)) +
  xlab("Facility")
VarianceInflationFactors
ggsave_pdf_jpeg(VarianceInflationFactors, OUTPUTFOLDER)



# VIF - Variance Inflation Factors
# Variables with VIF values above 5 may need to be removed
aggformula_01 <-
  Ward_No ~ `Personal care` + `Transit station` + `Church` + 
  `Educational institution` + `Retail store` + `Park and recreation` +
  `Government office` + `Hospital` + `Jewellery` + 
  `Marriage hall` + `Mosque` + `Police station` + `Restaurant` +
  `Temple` + `Textiles` + `Population` + `Households` + `Residences`
aggward_model_01 <- 
  lm(aggformula_01,  
     data=AggFacilities_in_Wards_PL)
omcdiag(aggward_model_01)
imcdiag(aggward_model_01)
# remove `households`
aggformula_02 <-
  Ward_No ~ `Personal care` + `Transit station` + `Church` + 
  `Educational institution` + `Retail store` + `Park and recreation` +
  `Government office` + `Hospital` + `Jewellery` + 
  `Marriage hall` + `Mosque` + `Police station` + `Restaurant` +
  `Temple` + `Textiles` +
  `Population` + `Residences`
aggward_model_02 <-
  lm(aggformula_02,  
     data=AggFacilities_in_Wards_PL)
omcdiag(aggward_model_02)
imcdiag(aggward_model_02)

# 
aggVIFS <- enframe(vif(aggward_model_02), name="Facility", value="VIF") %>%
  mutate(Facility=gsub("\`", "", Facility)) %>%
  mutate(Threshold = case_when(VIF >=5           ~ "[5+)",
                               VIF >=3 & VIF < 5 ~ "[3-5]",
                               VIF < 3           ~ "[0-3)"
  ))

# Plot the VIF values
AggVarianceInflationFactors <-
  aggVIFS %>%
  arrange(desc(VIF)) %>%
  ggplot() + 
  geom_col(mapping=aes(x=reorder(Facility, VIF), y=VIF, fill=Threshold)) +
  theme(axis.text.x = element_text(size = 10, angle = 90)) +
  xlab("Facility")
AggVarianceInflationFactors
ggsave_pdf_jpeg(AggVarianceInflationFactors, OUTPUTFOLDER)



###############################################################
# Transform Ward and attributes into a plain dataframe (tibble)
###############################################################
Chennai_Wards_DF <-
  as_tibble(Chennai_Wards_PL) %>%
  select(-centroids, -Zone_Name, -AREA, -PERIMETER, -geometry,
         -Households, -Residences, -Population) %>%
  inner_join(Facilities_in_Wards_PL, by = "Ward_No")

Chennai_AggWards_DF <-
  as_tibble(Chennai_Wards_PL) %>%
  select(-centroids, -Zone_Name, -AREA, -PERIMETER, -geometry,
         -Households, -Residences, -Population) %>%
  inner_join(AggFacilities_in_Wards_PL, by = "Ward_No")


###############################################################
# Use 'read_xlsx'  from package "readxl" to read Excel file (.XLSX)
# Use 'read_xls if your Excel file has extension .XLS
# To 'see' the data, use View(CrimeEventData)
###############################################################
CrimeEventData <- read_xlsx(paste(DATAFOLDER, 
                                  "20200513_CrimeEventSample.xlsx", sep=""))

###############################################################
# Transform data into a spatial data object based on the
# offender residence locations (not the crime locations)
# The CrimeEventData uses latitude-longitude according to 
#    WGS 1984, EPSG:4326)
###############################################################
OffenderResidence_WGS84 <- st_as_sf(CrimeEventData, 
                                    coords=c("Offender_Lon", "Offender_Lat"), 
                                    dim="XY", crs=4326)
###############################################################
# Transform to a planar projection
###############################################################
OffenderResidence_PL <- st_transform(OffenderResidence_WGS84, 24381)

###############################################################
# Transform data into a spatial data object based on the
# crime locations (not the offender residence locations)
# The CrimeEventData uses latitude-longitude according to 
#    WGS 1984, EPSG:4326). 
# (Only for plotting, we use Ward centroids in the data model)
###############################################################
CrimeLocation_WGS84 <- st_as_sf(CrimeEventData, 
                                coords=c("Crime_Lon", "Crime_Lat"), 
                                dim="XY", crs=4326)
###############################################################
# Transform to a planar projection 
# (Only for plotting, we use Ward centroids in the data model)
###############################################################
CrimeLocation_PL <- st_transform(CrimeLocation_WGS84, 24381)

###############################################################
# Copy coordinates 'Offender_x' and 'Offender_y'
###############################################################
Offenders_PL <- sfc_as_cols(OffenderResidence_PL, 
                            st_geometry(geometry),
                            names=c("Offender_x", "Offender_y")) 
###############################################################
# Copy coordinates 'Crime_x' and 'Crime_y'
###############################################################
Crimes_PL <- sfc_as_cols(CrimeLocation_PL, 
                         st_geometry(geometry),
                         names=c("Crime_x", "Crime_y")) 

# Plot offender homes and crime sites
Chennai_Wards_WGS84_HomesCrimes_Map <-
  ggplot(data = Chennai_Wards_WGS84) +
  geom_sf(show.legend=FALSE) +
  geom_sf(data=OffenderResidence_WGS84, aes(color="Offender home"), size=.1,
          show.legend="line") +
  geom_sf(data=CrimeLocation_WGS84, aes(color="Crime site"), size=.1, 
          show.legend=TRUE) +
  labs(color = "Location type") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="right") +
  annotation_scale(location = "tl", width_hint = 0.5, 
                   height = unit(0.10, "cm"),
                   text_cex = 0.6)
Chennai_Wards_WGS84_HomesCrimes_Map
ggsave_pdf_jpeg(Chennai_Wards_WGS84_HomesCrimes_Map, OUTPUTFOLDER) 

# Plot offender homes, crime sites and lines between them
# First create the lines
JTC_Lines_WGS84 <- 
  CrimeEventData %>%
  select(Crime_ID, Offender_Lat, Offender_Lon, Crime_Lat, Crime_Lon) %>%
  pivot_longer(cols=c(Offender_Lat, Offender_Lon, Crime_Lat, Crime_Lon),
               names_to=c("source", "dir"), names_sep="_") %>%
  pivot_wider(names_from="dir") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs=4326) %>% # create points
  group_by(Crime_ID) %>%
  summarise() %>% # union points into lines using our created lineid
  st_cast("MULTIPOINT") %>%
  st_cast("LINESTRING") 


# Graph of length of journey-to-crime
JTC_Lines_WGS84$distance <- as.numeric(st_length(JTC_Lines_WGS84) / 1000)
JTC_DistancePlot <-
  JTC_Lines_WGS84 %>%
  ggplot() +
  geom_histogram(mapping=aes(distance), binwidth=1, 
                 color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  xlab("km") +
  ylab("frequency")
JTC_DistancePlot
ggsave_pdf_jpeg(JTC_DistancePlot, OUTPUTFOLDER) 

# Map of all journeys-to-crime
Chennai_Wards_WGS84_JourneyToCrime_Map <-
  ggplot(data = Chennai_Wards_WGS84) +
  geom_sf(show.legend=FALSE) +
  geom_sf(data=OffenderResidence_WGS84, aes(color="Offender home"), size=.1,
          show.legend="line") +
  geom_sf(data=CrimeLocation_WGS84, aes(color="Crime site"), size=.1, 
          show.legend=TRUE) +
  geom_sf(data=JTC_Lines_WGS84, aes(color="Journey-to-crime"),  alpha=.2,
          show.legend=TRUE) +
  labs(color = "Chennai") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="right") +
  annotation_scale(location = "tl", width_hint = 0.5, 
                   height = unit(0.15, "cm"),
                   text_cex = 0.6)
Chennai_Wards_WGS84_JourneyToCrime_Map
ggsave_pdf_jpeg(Chennai_Wards_WGS84_JourneyToCrime_Map, OUTPUTFOLDER)

###############################################################
# Transform into a plain dataframe (tibble)
###############################################################
Crimes_DF <-
  as_tibble(Offenders_PL) %>%
  select(-Crime_Lat, -Crime_Lon, -geometry)


###############################################################
# Create the complete dataset for location choice estimation
#   (this creates a file of C x W records, where C is the number
#    of crimes and W is the number of wards (=201))
###############################################################

CreateLocChoiceData <- function(tbl_crimes, tbl_wards) {
  merge(tbl_crimes, tbl_wards) %>%
    # which ward was chosen?
    mutate(chosen = as.numeric(Crime_Ward == Ward_No)) %>%
    # calculate distance from offender residence to ward centroid
    mutate(distance = sqrt((Offender_x - WardCentroid_x)^2 +
                             (Offender_y - WardCentroid_y)^2) / 1000,
           neglogdistance = -log(distance),
           # recode into 1000m bandwidth intervals
           dist1000bw = -trunc(distance)*1000,
           # set to 10km distances >10km, and make it a factor
           dist1000bw = factor(if_else(distance >= 10, -10000, dist1000bw)),
           # recode into 500m bandwidth intervals
           dist500bw = -trunc(distance/.5)*500,
           # set to 5km distances >5km, and make it a factor
           dist500bw = factor(if_else(distance >= 5, -5000, dist500bw)),
           # same operation for 400m distance bands
           dist400bw = -trunc(distance/.4)*400,
           dist400bw = factor(if_else(distance >= 4, -4000, dist400bw)),              
           # same operation for 300m distance bands
           # Distinguish young offenders from adult offenders
           Minor_Offender = as.numeric(Age < 19),
           Adult_Offender = as.numeric(Age >= 19),
           # Define separate distances for minors and adults
           Minor_Distance = Minor_Offender * distance,
           Adult_Distance = Adult_Offender * distance,
           # Population in 1000s
           Population1000 = Population / 1000,
           `Place of worship` = Church + Mosque + Temple) %>%
    # how many prior offences committed by same offender in this ward?
    # complicated!
    group_by(Offender_ID, Ward_No) %>%
    arrange(Crime_Year, Crime_Month, Crime_Day) %>%
    mutate(prior_crimes = cumsum(chosen) - chosen) 
}

LocChoiceData <- CreateLocChoiceData(Crimes_DF, Chennai_Wards_DF)
AggLocChoiceData <- CreateLocChoiceData(Crimes_DF, Chennai_AggWards_DF)


# estimate the model (only distance)
Model_01 <-
  clogit(chosen ~ distance + strata(Crime_ID),
         data=AggLocChoiceData) 
summary(Model_01)
AIC(Model_01)


Model_01logd <-
  clogit(chosen ~ neglogdistance + strata(Crime_ID),
         data=AggLocChoiceData) 
summary(Model_01logd)
AIC(Model_01logd)


Model_01_1000m <-
  clogit(chosen ~ dist1000bw +
           strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_01_1000m)
AIC(Model_01_1000m)
BIC(Model_01_1000m)

Model_01_500m <-
  clogit(chosen ~ dist500bw +
           strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_01_500m)
AIC(Model_01_500m)
BIC(Model_01_500m)

Model_01_400m <-
  clogit(chosen ~ dist400bw + 
           strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_01_400m)
AIC(Model_01_400m)
BIC(Model_01_400m)

distcatplot400 <- 
  tibble(distance = seq(from=(9*400),to=0, by=-400),
         estimate = Model_01_400m$coefficients,
         se = sqrt(diag(Model_01_400m$var)),
         ci95min = estimate - 2 * se,
         ci95max = estimate + 2 * se) %>%
  ggplot() +
  geom_point(aes(x=distance, y=estimate)) +
  scale_x_continuous(breaks=seq(0,3600,400))
distcatplot400

distcatplot400ebar <-
  tibble(distance = seq(from=(9*400),to=0, by=-400),
         estimate = exp(Model_01_400m$coefficients),
         se = sqrt(diag(Model_01_400m$var)),
         ci95min = exp(Model_01_400m$coefficients - 2 * se),
         ci95max = exp(Model_01_400m$coefficients + 2 * se)) %>%
  ggplot(mapping=aes(x=distance, y=estimate)) +
  geom_line(linetype=2) +
  geom_point() +
  geom_errorbar(aes(ymin=ci95min, ymax=ci95max), 
                color="blue", width=100) +
  ylab("odds ratio")
distcatplot400ebar


distcatplot500 <- 
  tibble(distance = seq(from=(9*500),to=0, by=-500),
         estimate = Model_01_500m$coefficients) %>%
  ggplot() +
  geom_point(aes(x=distance, y=estimate)) +
  scale_x_continuous(breaks=seq(0,4500,500))
distcatplot500

distcatplot500ebar <-
  tibble(distance = seq(from=(9*500),to=0, by=-500),
         estimate = exp(Model_01_500m$coefficients),
         se = sqrt(diag(Model_01_500m$var)),
         ci95min = exp(Model_01_500m$coefficients - 2 * se),
         ci95max = exp(Model_01_500m$coefficients + 2 * se)) %>%
  ggplot(mapping=aes(x=distance, y=estimate)) +
  geom_line(linetype=2) +
  geom_point() +
  geom_errorbar(aes(ymin=ci95min, ymax=ci95max), 
                color="blue", width=100) +
  ylab("odds ratio")
distcatplot500ebar


distcatplot1000 <- 
  tibble(distance = seq(from=(9*1000),to=0, by=-1000),
         estimate = Model_01_1000m$coefficients) %>%
  ggplot() +
  geom_point(aes(x=distance, y=estimate)) +
  scale_x_continuous(breaks=seq(0,10000,1000))
distcatplot1000




summary(clogit(chosen ~ dist1000bw + prior_crimes +
                 strata(Crime_ID),
               data=AggLocChoiceData) )

summary(x1 <- clogit(chosen ~ dist1000bw + prior_crimes + area +
                       strata(Crime_ID),
                     data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 `Transit station` + strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 `Transit station` + `Retail store` + strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 `Transit station` + `Retail store` + `Textiles` + 
                 strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 `Transit station` + `Retail store` + `Textiles` + 
                 `Personal care` + strata(Crime_ID),
               data=AggLocChoiceData) )

summary(clogit(chosen ~ dist1000bw + prior_crimes + area + Population1000 +
                 `Transit station` + `Retail store` + `Textiles` + 
                 `Personal care` + `Place of worship` + strata(Crime_ID),
               data=AggLocChoiceData) )




# estimate another model (distance + area size)
Model_02 <-
  clogit(chosen ~ distance + area + strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_02)

# We find that the negative effect of distance on location choice 
#    is somewhat stronger (more negative, OR=.75) for minors 
#    than for adults (less negative, OR=.81)
Model_03 <-
  clogit(chosen ~ Minor_Distance + Adult_Distance + area + strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_03)

# Note that Model_03 is the same model as Model_03alt but
#   parameterized differently:
Model_03alt <-
  clogit(chosen ~ distance + Adult_Distance + area + strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_03alt)
# The effect of 'distance' is for minors and the effect of
#  'Adult_Distance' is the effect for adults reative to minors,
#  so 0.7517 * 1.0710 = 0.8051, which is the effect of
#  'Adult_Distance' in Model_03
#



# distance + area + nr of prior crimes
Model_04 <-
  clogit(chosen ~ distance + area + prior_crimes + strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_04)

Model_05 <-
  clogit(chosen ~ distance + area + prior_crimes +
           Population + strata(Crime_ID),
         data=LocChoiceData %>% mutate(Population = Population / 1000)) 
summary(Model_05)


Model_06 <-
  clogit(chosen ~ distance + area + 
           `Barber shop` + `Bus stop` + `Church` + `Education` + 
           `General store` + `Government office` + `Hospital` + `Jewellery` + 
           `Marriage hall` + `Medical shop` + `Mosque` +              
           `Park` + `Police station` + `Recreation` + `Restaurant` +
           `Saloon` + `School and college` + `Spa`  + `Supermarket` +  `Temple` + 
           `Textiles` + `Vegetables` + `Beauty parlour`  + `Railway station` +
           `Population` + `Residences` + strata(Crime_ID),
         data=LocChoiceData) 
summary(Model_06)





Model_06a <-
  clogit(chosen ~ distance + area + 
           `Personal care` + `Transit station` + `Church` + 
           `Educational institution` + `Retail store` + `Park and recreation` +
           `Government office` + `Hospital` + `Jewellery` + 
           `Marriage hall` + `Mosque` + `Police station` + `Restaurant` +
           `Temple` + `Textiles` +
           `Population` + `Residences` + strata(Crime_ID),
         data=AggLocChoiceData) 
summary(Model_06a)

###############################################################
# Also some descriptions of the data
# (Use 'Crime_DF' here. It contains a single row for each crime
###############################################################

# Describe year (Crime_ID No. 1406 has unknown date)
Crimes_DF %>% group_by(Crime_Year) %>% summarize(Frequency = n())
# Alternative way to do the same:
table(Crimes_DF$Crime_Year, useNA="ifany")

# Describe age 
Crimes_DF %>% group_by(Age) %>% summarize(Frequency = n()) %>% print(n=100)
# Alternative way to do the same:
table(Crimes_DF$Age, useNA="ifany")

# Describe gender 
Crimes_DF %>% group_by(Gender) %>% summarize(Frequency = n()) %>% print(n=100)
# Alternative way to do the same:
table(Crimes_DF$Gender, useNA="ifany")

# How many crimes were committed in the offender's home ward?
#   First create a variable called 'HomeCrime'
Crimes_DF$Home_Crime <- Crimes_DF$Crime_Ward==Crimes_DF$Offender_Ward
#   then tabulate it
Crimes_DF %>% group_by(Home_Crime) %>% summarize(Frequency = n())
# Alternative way to do the same:
table(Crimes_DF$Home_Crime, useNA="ifany")
# Display the number of crimes per offender
Crimes_DF %>% 
  group_by(Offender_ID) %>%
  summarize(Nr_Of_Crimes = n()) %>%
  group_by(Nr_Of_Crimes) %>%
  summarize(Frequency = n()) %>%
  mutate(Crimes = Nr_Of_Crimes * Frequency)







