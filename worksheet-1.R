####################################  Introduction to Remote Sensing   #######################################
############################  Analyze and map flooding from RITA hurricane  #######################################
#This script performs analyses for the Exercise 5 of the Short Course using reflectance data derived from MODIS.
#The goal is to map flooding from RITA using various reflectance bands from Remote Sensing platforms.
#Additional data is provided including FEMA flood region. 
#
#AUTHORS: Benoit Parmentier                                             
#DATE CREATED: 03/05/2018 
#DATE MODIFIED: 03/31/2018
#Version: 1
#PROJECT: SESYNC and AAG 2018 Geospatial Short Course and workshop preparation
#TO DO:
#
#COMMIT: clean up of code
#
#################################################################################################

###Loading R library and packages                                                      

library(sp) # spatial/geographfic objects and functions
library(rgdal) #GDAL/OGR binding for R with functionalities
library(spdep) #spatial analyses operations, functions etc.
library(gtools) # contains mixsort and other useful functions
library(maptools) # tools to manipulate spatial data
library(parallel) # parallel computation, part of base package no
library(rasterVis) # raster visualization operations
library(raster) # raster functionalities
library(forecast) #ARIMA forecasting
library(xts) #extension for time series object and analyses
library(zoo) # time series object and analysis
library(lubridate) # dates functionality
library(colorRamps) #contains matlab.like color palette
library(rgeos) #contains topological operations
library(sphet) #contains spreg, spatial regression modeling
library(BMS) #contains hex2bin and bin2hex, Bayesian methods
library(bitops) # function for bitwise operations
library(foreign) # import datasets from SAS, spss, stata and other sources
library(gdata) #read xls, dbf etc., not recently updated but useful
library(classInt) #methods to generate class limits
library(plyr) #data wrangling: various operations for splitting, combining data
#library(gstat) #spatial interpolation and kriging methods
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities
library(sf) # spatial objects classes
library(plotrix) #various graphic functions e.g. draw.circle

###### Functions used in this script

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#####  Parameters and argument set up ###########

in_dir_reflectance <- "data/reflectance_RITA"
in_dir_var <- "data"
out_dir <- "."

#region coordinate reference system
#http://spatialreference.org/ref/epsg/nad83-texas-state-mapping-system/proj4/
CRS_reg <- "+proj=lcc +lat_1=27.41666666666667 +lat_2=34.91666666666666 +lat_0=31.16666666666667 +lon_0=-100 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
file_format <- ".tif" # Output format for raster images that are written out.
NA_flag_val <- -9999
out_suffix <-"exercise5_03312018" #output suffix for the files and ouptu folder #PARAM 8
create_out_dir_param <- TRUE 

### Input data files used:
infile_reg_outline <- "new_strata_rita_10282017.shp" # Region outline and FEMA zones
infile_modis_bands_information <- "df_modis_band_info.txt" # MOD09 bands information.
nlcd_2006_filename <- "nlcd_2006_RITA.tif" # NLCD2006 Land cover data aggregated at ~ 1km.
infile_name_nlcd_legend <- "nlcd_legend.txt" #Legend information for 2006 NLCD.
#MOD09 surface reflectance product on 2005-09-22 or day of year 2005265
infile_reflectance_date1 <- "mosaiced_MOD09A1_A2005265__006_reflectance_masked_RITA_reg_1km.tif"
#MOD09 surface reflectance product on 2005-09-30 or day of year 2005273
infile_reflectance_date2 <- "mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.tif"

################# START SCRIPT ###############################

### PART I: READ AND PREPARE DATA FOR ANALYSES #######

## First create an output directory

if(is.null(out_dir)){
  out_dir <- dirname(in_dir) #output will be created in the input dir
}

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix_s)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

##### PART I: DISPLAY AND EXPLORE DATA ##############

###### Read in MOD09 reflectance images before and after Hurrican Rita.
r_before <- brick(file.path(in_dir_var,infile_reflectance_date1)) # Before RITA, Sept. 22, 2005.
r_after <- brick(file.path(in_dir_var,infile_reflectance_date2)) # After RITA, Sept 30, 2005.

plot(r_before) # Note that this is a multibands image.

reg_sf <- st_read(file.path(in_dir_var,infile_reg_outline))
reg_sf <- st_transform(reg_sf,
                       crs=CRS_reg)
reg_sp <-as(reg_sf, "Spatial") #Convert to sp object before rasterization

r_ref <- rasterize(reg_sp,
                   r_before,
                   field="OBJECTID_1",
                   fun="first")
plot(r_ref) # zone 2 is flooded and zone 1 is not flooded

#### Let's examine a location within FEMA flooded zone and outside: use centroids
centroids_sf <- st_centroid(reg_sf)

df_before <- extract(r_before,centroids_sf)
df_after <- extract(r_after,centroids_sf)

### Plot values of bands before and after for flooded region:
plot(df_before[2,],type="l")
lines(df_after[2,],col="red")

## Read band information since it is more informative!!
df_modis_band_info <- read.table(file.path(in_dir_var,infile_modis_bands_information),
                                 sep=",",
                                 stringsAsFactors = F)
print(df_modis_band_info)

names(r_before) <- df_modis_band_info$band_name
names(r_after) <- df_modis_band_info$band_name

### Order band in terms of wavelenth:

df_modis_band_info <- df_modis_band_info[order(df_modis_band_info$start_wlength),]
#SWIR1 (1230–1250 nm), SWIR2 (1628–1652 nm) and SWIR3 (2105–2155 nm).
band_refl_order <- df_modis_band_info$band_number

plot(df_before[2,band_refl_order],type="l",
     xlab="Bands",
     ylab="Reflectance",
     main="Reflectance profile for centroid of flooded area")
lines(df_after[2,band_refl_order],col="red")
names_vals <- c("Before Rita","After Rita")
legend("topleft",legend=names_vals,
       pt.cex=0.7,cex=0.7,col=c("black","red"),
       lty=1, #add circle symbol to line
       bty="n")

###############################################
##### PART II: Examine spectral class signatures for land cover NLCD classes ##############

###### Read in NLDC 2006, note that it was aggregated at 1km.
nlcd2006_reg <- raster(file.path(in_dir_var,nlcd_2006_filename))
#### Extract zonal averages by NLCD classes using zonal raster
avg_reflectance_nlcd <- as.data.frame(zonal(r_before,nlcd2006_reg,fun="mean"))

#### read in NLCD legend to add to the average information
lc_legend_df <- read.table(file.path(in_dir_var,infile_name_nlcd_legend),
                           stringsAsFactors = F,
                           sep=",")
print(avg_reflectance_nlcd)

names(lc_legend_df)
### Add relevant categories
lc_legend_df_subset <- subset(lc_legend_df,select=c("ID","NLCD.2006.Land.Cover.Class"))
names(lc_legend_df_subset) <- c("ID","cat_name")

avg_reflectance_nlcd <- merge(avg_reflectance_nlcd,lc_legend_df_subset,by.x="zone",by.y="ID",all.y=F)
head(avg_reflectance_nlcd)

names(avg_reflectance_nlcd)
col_ordering <- band_refl_order + 1

plot(as.numeric(avg_reflectance_nlcd[9,col_ordering]),
     type="l",
     col="green",
     xlab="bands",
     ylab="reflectance") #42 evergreen forest
lines(as.numeric(avg_reflectance_nlcd[6,col_ordering]),
      type="l",
      col="darkred") #22 developed,High intensity

####### Explore data in Feature space:

#Feature space NIR1 and Red
plot(r_before$Red, r_before$NIR)
plot(r_before$Green,r_before$Red)
plot(r_before$SWIR1,r_before$NIR)
plot(r_before$Red,r_before$SWIR1)

df_r_before_nlcd <- as.data.frame(stack(r_before,nlcd2006_reg))
head(df_r_before_nlcd)

#Evergreen Forest:
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==42)
plot(df_subset$Green,
     df_subset$Red,
     col="green",
     cex=0.15,
     xlab="Green",
     ylab="Red",
     main="Green-Red feature space")

#Urban: dense
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==22)
points(df_subset$Green,
       df_subset$Red,
       col="darkred",
       cex=0.15)

#Water: 11 for NLCD
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==22)
points(df_subset$Green,
       df_subset$Red,
       col="blue",
       cex=0.15)

title("Feature space Green-Red")
names_vals <- c("Evergreen Forest","Developed High Intensity","Water")
legend("topright",legend=names_vals,
       pt.cex=0.7,cex=0.7,col=c("green","darkred","blue"),
       pch=1, #add circle symbol to line
       bty="n")

#### Feature space Red and NIR

#Evergreen Forest:
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==42)
plot(df_subset$Red,
     df_subset$NIR,
     col="green",
     cex=0.15,
     xlab="Red",
     ylab="NIR")

#Developed, High Intensity
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==22)
points(df_subset$Red,
       df_subset$NIR,
       col="darkred",
       cex=0.15)

#Water
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==11)
points(df_subset$Red,
       df_subset$NIR,
       col="blue",
       cex=0.15)

title("Feature space Red-NIR")
names_vals <- c("Evergreen Forest","Developed High Intensity","Water")
legend("topright",legend=names_vals,
       pt.cex=0.7,cex=0.7,col=c("green","darkred","blue"),
       pch=1, #add circle symbol to line
       bty="n")

#### Feature space Blue and Red

#Water:
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==11)
plot(df_subset$Blue,
       df_subset$Red,
       col="blue",
       cex=0.15,
       xlab="Blue",
       ylab="Red")

#Evergreen Forest:
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==42)
points(df_subset$Blue,
       df_subset$Red,
       col="green",
       cex=0.15)

#Urban dense:
df_subset <- subset(df_r_before_nlcd,nlcd_2006_RITA==22)
points(df_subset$Blue,
       df_subset$Red,
       col="darkred",
       cex=0.15)

title("Feature space Blue-Red")
names_vals <- c("Evergreen Forest","Developed High Intensity","Water")
legend("topright",legend=names_vals,
       pt.cex=0.7,cex=0.7,col=c("green","darkred","blue"),
       pch=1, #add circle symbol to line
       bty="n")

#### Generate Color composites
### True color composite

plotRGB(r_before,
        r=1,
        g=4,
        b=3,
        scale=0.6,
        strech="hist")

### False color composite:

plotRGB(r_before,
        r=2,
        g=1,
        b=4,
        scale=0.6,
        strech="hist")

plotRGB(r_after,
        r=2,
        g=1,
        b=4,
        scale=0.6,
        strech="hist")

### Note the effect of flooding is particularly visible in the false color composites.
### Let's examine different band combination to enhance features in the image, including flooded areas.

###############################################
##### PART III: Band combination: Indices and thresholding for flood mapping ##############

### NIR experiment with threshold to  map water/flooding

plot(subset(r_before,"NIR"))
plot(subset(r_after,"NIR"))

### Lower NIR often correlates to areas with high water fraction or inundated:
r_rec_NIR_before <- subset(r_before,"NIR") < 0.2
r_rec_NIR_after <- subset(r_after,"NIR") < 0.2

### THis is suggesting flooding!!!
plot(r_rec_NIR_before, main="Before RITA, NIR > 0.2")
plot(r_rec_NIR_after, main="After RITA, NIR > 0.2")

#Compare to actual flooding data
freq_fema_zones <- as.data.frame(freq(r_ref))
xtab_threshold <- crosstab(r_ref,r_rec_NIR_after,long=T)

## % overlap between the flooded area and values below 0.2 in NIR
(xtab_threshold[5,3]/freq_fema_zones[2,2])*100 #agreement with FEMA flooded area in %.

############## Generating indices based on raster algebra of original bands
## Let's generate a series of indices, we list a few possibility from the literature.
#1) NDVI = (NIR - Red)/(NIR+Red)
#2) NDWI = (Green - NIR)/(Green + NIR)
#3) MNDWI = Green - SWIR2 / Green + SWIR2
#4) NDWI2 (LSWIB5) =  (NIR - SWIR1)/(NIR + SWIR1)
#5) LSWI (LSWIB5) =  (NIR - SWIR2)/(NIR + SWIR2)

names(r_before)
r_before_NDVI <- (r_before$NIR - r_before$Red) / (r_before$NIR + r_before$Red)
r_after_NDVI <- subset(r_after,"NIR") - subset(r_after,"Red")/(subset(r_after,"NIR") + subset(r_after,"Red"))

plot(r_before_NDVI,zlim=c(-1,1),col=matlab.like(255))
plot(r_after_NDVI,zlim=c(-1,1),col=matlab.like2(255))

### Experiment with different threshold values:
# THis is suggesting flooding!!!

histogram(r_before_NDVI) # Examine histogram to look for potential threshold values
histogram(r_after_NDVI) # Examine histogram to look for poential threshold values

plot(r_before_NDVI < -0.5)
plot(r_after_NDVI < -0.5)
plot(r_before_NDVI < -0.1)
plot(r_after_NDVI < -0.1)

#2) NDWI = (Green - NIR)/(Green + NIR)
#3) MNDWI = Green - SWIR2 / Green + SWIR2

#Modified NDWI known as MNDWI : Green - SWIR2/(Green + SWIR2)

#According to Gao (1996), NDWI is a good indicator for vegetation liquid water content and 
# is less sensitive to atmospheric scattering effects than NDVI. In some studies, MODIS band 6 
# is used for the NDWI calculation, because it is sensitive to water types and contents 
# (Li et al., 2011), while band 5 is sensitive to vegetation liquid water content (Gao, 1996).

r_before_MNDWI <- (r_before$Green - r_before$SWIR2) / (r_before$Green + r_before$SWIR2)
r_after_MNDWI <- (r_after$Green - r_after$SWIR2) / (r_after$Green + r_after$SWIR2)

plot(r_before_MNDWI,zlim=c(-1,1),col=rev(matlab.like(255)))
plot(r_after_MNDWI,zlim=c(-1,1),col=rev(matlab.like(255)))

### THis is suggesting flooding!!!
plot(r_before_MNDWI > 0.5)
plot(r_after_MNDWI > 0.5)
plot(r_before_MNDWI > 0.1)
plot(r_after_MNDWI > 0.1)

r_diff_MNDWI <- r_after_MNDWI - r_before_MNDWI
plot(r_diff_MNDWI)
plot(r_diff_MNDWI > 0.2) ## Increase in MNDWI: Areas that may have become flooded

#Standardize image:
mean_val <- cellStats(r_diff_MNDWI,"mean")
sd_val <- cellStats(r_diff_MNDWI,"sd")

r_diff_std <- (r_diff_MNDWI - mean_val)/ sd_val
plot(r_diff_std)
histogram(r_diff_std) 
plot(r_diff_std > 2) ## increase in MNDWI 2 standard deiation away
plot(r_diff_std > 1) ## increase in MNDWI 2 standard deiation away

### Generate a map of flooding with MNDWI and compare to FEMA map:

r_after_flood <- r_diff_std > 1 
plot(r_after_flood)

#reclass in zero/1!!!

df <- data.frame(id=c(1,2), v=c(0,1))
r_ref_rec <- subs(r_ref, df)
plot(r_ref_rec)
xtab_tb <- crosstab(r_after_flood,r_ref_rec)

## Examine overlap between flooded area and FEMA:

plot(r_after_flood)
r_overlap <- r_ref_rec * r_after_flood
plot(r_overlap) ## Flood map area of aggreement with FEMA

###############################################
##### PART IV: Principal Component Analysis and band transformation
## Another avenue to combine original bands into indices is to use linear transformation.
## We examine Principal Component Analysis and the Tassel Cap Transform.

#Correlate long term mean to PC!
cor_mat_layerstats <- layerStats(r_before, 'pearson', na.rm=T)
cor_matrix <- cor_mat_layerstats$`pearson correlation coefficient`
class(cor_matrix)
print(cor_matrix) #note size is 7x7

pca_mod <- principal(cor_matrix,nfactors=7,rotate="none")
class(pca_mod$loadings)
print(pca_mod$loadings)

plot(pca_mod$loadings[,1][band_refl_order],type="b",
     xlab="time steps",
     ylab="PC loadings",
     ylim=c(-1,1),
     col="blue")
lines(-1*(pca_mod$loadings[,2][band_refl_order]),type="b",col="red")
lines(pca_mod$loadings[,3][band_refl_order],type="b",col="black")
title("Loadings for the first three components using T-mode")

##Make this a time series
loadings_df <- as.data.frame(pca_mod$loadings[,1:7])
title("Loadings for the first three components using T-mode")
names_vals <- c("pc1","pc2","pc3")
legend("topright",legend=names_vals,
       pt.cex=0.8,cex=1.1,col=c("blue","red","black"),
       lty=c(1,1), # set legend symbol as lines
       pch=1, #add circle symbol to line
       lwd=c(1,1),bty="n")

## Add scree plot
plot(pca_mod$values,main="Scree plot: Variance explained",type="b")

### Generate scores from eigenvectors
### Using predict function: this is recommended for raster.
r_pca <- predict(r_before, pca_mod, index=1:7,filename="pc_scores.tif",overwrite=T) # fast
plot(r_pca,y=2,zlim=c(-2,2))
plot(r_pca,y=1,zlim=c(-2,2))
plot(r_pca,y=3,zlim=c(-2,2))

plot(subset(r_pca,1),subset(r_pca,2))
plot(subset(r_pca,2),subset(r_pca,3))

#### Generate a plot for PCA with loadings and compare to Tassel Cap

var_labels <- rownames(loadings_df)

plot(loadings_df[,1],loadings_df[,2],
     type="p",
     pch = 20,
     col ="blue",
     xlab=names(loadings_df)[1],
     ylab=names(loadings_df)[2],
     ylim=c(-1,1),
     xlim=c(-1,1),
     axes = FALSE,
     cex.lab = 1.2)
axis(1, at=seq(-1,1,0.2),cex=1.2)
axis(2, las=1,at=seq(-1,1,0.2),cex=1.2) # "1' for side=below, the axis is drawned  on the right at location 0 and 1

box()    #This draws a box...

title(paste0("Loadings for component ", names(loadings_df)[1]," and " ,names(loadings_df)[2] ))
draw.circle(0,0,c(1.0,1.0),nv=500)#,border="purple",
text(loadings_df[,1],loadings_df[,2],var_labels,pos=1,cex=1)            
grid(2,2)

##### plot feature space:

df_raster_val <- as.data.frame(stack(r_after,r_pca,nlcd2006_reg))
head(df_raster_val)

#Water
plot(df_raster_val[df_raster_val$nlcd_2006_RITA==11,c("pc_scores.1")],
     df_raster_val[df_raster_val$nlcd_2006_RITA==11,c("pc_scores.2")],
     col="blue",cex=0.15,
     ylim=c(-2,2),
     xlim=c(-2,2))

#Urban: dense
points(df_raster_val[df_raster_val$nlcd_2006_RITA==22,c("pc_scores.1")],
       df_raster_val[df_raster_val$nlcd_2006_RITA==22,c("pc_scores.2")],
       col="brown",cex=0.15)

#Forest:
points(df_raster_val[df_raster_val$nlcd_2006_RITA==42,c("pc_scores.1")],
       df_raster_val[df_raster_val$nlcd_2006_RITA==42,c("pc_scores.2")],
       col="green",cex=0.15)

### Note the negative values are related to Forest and positve to water on PC2
plot(r_pca$pc_scores.2 > 0.1) #water related?
plot(r_pca$pc_scores.2 < - 0.1) #vegetation related?

#### Examine relationship with Tasselcap transform adapted for MODIS:
#6) TCWI =  0.10839 * Red+ 0.0912 * NIR +0.5065 * Blue+ 0.404 * Green 
#            - 0.241 * SWIR1- 0.4658 * SWIR2-
#           0.5306 * SWIR3
#7) TCBI = 0.3956 * Red + 0.4718 * NIR +0.3354 * Blue+ 0.3834 * Green
#           + 0.3946 * SWIR1 + 0.3434 * SWIR2+ 0.2964 * SWIR3

r_TCWI =  0.10839 * r_before$Red + 0.0912 * r_before$NIR +0.5065 * r_before$Blue+ 0.404 * r_before$Green 
- 0.241 * r_before$SWIR1- 0.4658 * r_before$SWIR2 - 0.5306 * r_before$SWIR3

r_TCBI = 0.3956 * r_before$Red + 0.4718 * r_before$NIR +0.3354 * r_before$Blue+ 0.3834 * r_before$Green
          + 0.3946 * r_before$SWIR1 + 0.3434 * r_before$SWIR2+ 0.2964 * r_before$SWIR3

#r_TCWI <- r_TCWI*10
histogram(r_TCWI)
plot(r_TCWI)
plot(r_TCWI,zlim=c(0,0.2))

plot(r_TCWI>0.1)

histogram(r_TCBI)
plot(r_TCBI)
plot(r_TCBI,zlim=c(0,0.2))

r_stack <- stack(r_TCWI,r_TCBI, r_pca)
names(r_stack) <- c("TCWI","TCBI",names(r_pca))
cor_r_TC <- layerStats(r_stack,"pearson",na.rm=T)
cor_r_TC <- as.data.frame(cor_r_TC$`pearson correlation coefficient`)
print(cor_r_TC)

##### Plot on the loading space using TCBI and TCWI as supplementary variables
var_labels <- rownames(loadings_df)

plot(loadings_df[,1],loadings_df[,2],
     type="p",
     pch = 20,
     col ="blue",
     xlab=names(loadings_df)[1],
     ylab=names(loadings_df)[2],
     ylim=c(-1,1),
     xlim=c(-1,1),
     axes = FALSE,
     cex.lab = 1.2)

points(cor_r_TC$pc_scores.1[1],cor_r_TC$pc_scores.2[1],col="red")
points(cor_r_TC$pc_scores.1[2],cor_r_TC$pc_scores.2[2],col="green")

axis(1, at=seq(-1,1,0.2),cex=1.2)
axis(2, las=1,at=seq(-1,1,0.2),cex=1.2) # "1' for side=below, the axis is drawned  on the right at location 0 and 1
box()    #This draws a box...

title(paste0("Loadings for component ", names(loadings_df)[1]," and " ,names(loadings_df)[2] ))
draw.circle(0,0,c(1.0,1.0),nv=500)#,border="purple",
text(loadings_df[,1],loadings_df[,2],var_labels,pos=1,cex=1)            
text(cor_r_TC$pc_scores.1[1],cor_r_TC$pc_scores.2[1],"TCWI",pos=1,cex=1)
text(cor_r_TC$pc_scores.1[2],cor_r_TC$pc_scores.2[2],"TCBI",pos=1,cex=1)
grid(2,2)

########################## End of Script ###################################

