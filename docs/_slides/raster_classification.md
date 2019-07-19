---
editor_options: 
  chunk_output_type: console
excerpt: Raster Classification using flood mapping analysis
---




### Set up arguments and parameters



~~~r
in_dir <- "data"

out_dir_suffix <- "rsc_lesson"
out_dir <- paste("output_", out_dir_suffix, sep = "")
dir.create(out_dir, showWarnings = FALSE)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

Regional coordinate reference system taken from: 
http://spatialreference.org/ref/epsg/nad83-texas-state-mapping-system/proj4/



~~~r
CRS_reg <- "+proj=lcc +lat_1=27.41666666666667 +lat_2=34.91666666666666 +lat_0=31.16666666666667 +lon_0=-100 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

file_format <- ".tif" 

NA_flag_val <- -9999

out_suffix <- "raster_classification" # output suffix for the files and output folder  
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

### Define input data file names



~~~r
infile_RITA_reflectance_date2 <- "mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.tif"

infile_reg_outline <- "new_strata_rita_10282017" # Region outline and FEMA zones

infile_modis_bands_information <- "df_modis_band_info.txt" # MOD09 bands information.

nlcd_2006_filename <- "nlcd_2006_RITA.tif" # NLCD2006 Land cover data aggregated at ~ 1km.
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

## Part I: Read and map flooding from RITA hurricane 

===

### Display and explore the data

MOD09 raster image after hurricane Rita.



~~~r
r_after <- brick(file.path(in_dir, infile_RITA_reflectance_date2))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> head(r_after)
~~~
{:title="Console" .input}


~~~
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.1
 [1,]                                                       0.04558113
 [2,]                                                       0.04460510
 [3,]                                                       0.04348749
 [4,]                                                       0.04507260
 [5,]                                                       0.04715690
 [6,]                                                       0.03485106
 [7,]                                                       0.03807684
 [8,]                                                       0.04214056
 [9,]                                                       0.04512509
[10,]                                                       0.04687950
[11,]                                                       0.04639116
[12,]                                                       0.04591873
[13,]                                                       0.04590000
[14,]                                                       0.05590000
[15,]                                                       0.05367381
[16,]                                                       0.05220000
[17,]                                                       0.05146399
[18,]                                                       0.04496353
[19,]                                                       0.04380151
[20,]                                                       0.06080000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.2
 [1,]                                                        0.3027903
 [2,]                                                        0.3043112
 [3,]                                                        0.2998908
 [4,]                                                        0.3017146
 [5,]                                                        0.3239896
 [6,]                                                        0.2683164
 [7,]                                                        0.2991171
 [8,]                                                        0.3176138
 [9,]                                                        0.3125040
[10,]                                                        0.2979249
[11,]                                                        0.3101786
[12,]                                                        0.3076983
[13,]                                                        0.3076000
[14,]                                                        0.2953000
[15,]                                                        0.3113045
[16,]                                                        0.3219000
[17,]                                                        0.3242094
[18,]                                                        0.3254988
[19,]                                                        0.3138167
[20,]                                                        0.3098000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.3
 [1,]                                                       0.02379627
 [2,]                                                       0.02354558
 [3,]                                                       0.02374887
 [4,]                                                       0.02426019
 [5,]                                                       0.02511090
 [6,]                                                       0.01941371
 [7,]                                                       0.02170737
 [8,]                                                       0.02348050
 [9,]                                                       0.02387191
[10,]                                                       0.02560453
[11,]                                                       0.02765067
[12,]                                                       0.02635150
[13,]                                                       0.02630000
[14,]                                                       0.03030000
[15,]                                                       0.02999916
[16,]                                                       0.02980000
[17,]                                                       0.02756310
[18,]                                                       0.02447605
[19,]                                                       0.02306678
[20,]                                                       0.03420000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.4
 [1,]                                                       0.05476730
 [2,]                                                       0.05586702
 [3,]                                                       0.05381133
 [4,]                                                       0.05503851
 [5,]                                                       0.05962222
 [6,]                                                       0.04450204
 [7,]                                                       0.04927124
 [8,]                                                       0.05364913
 [9,]                                                       0.05487191
[10,]                                                       0.05564768
[11,]                                                       0.05650510
[12,]                                                       0.05544214
[13,]                                                       0.05540000
[14,]                                                       0.05870000
[15,]                                                       0.06014401
[16,]                                                       0.06110000
[17,]                                                       0.06231735
[18,]                                                       0.05556127
[19,]                                                       0.05338556
[20,]                                                       0.06660000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.5
 [1,]                                                        0.3318335
 [2,]                                                        0.3138186
 [3,]                                                        0.3123330
 [4,]                                                        0.3193893
 [5,]                                                        0.3417773
 [6,]                                                        0.2855803
 [7,]                                                        0.3055280
 [8,]                                                        0.3266653
 [9,]                                                        0.3330796
[10,]                                                        0.3139284
[11,]                                                        0.3352741
[12,]                                                        0.3151959
[13,]                                                        0.3144000
[14,]                                                        0.3485000
[15,]                                                        0.3409190
[16,]                                                        0.3359000
[17,]                                                        0.3346323
[18,]                                                        0.3365578
[19,]                                                        0.3272987
[20,]                                                        0.3235000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.6
 [1,]                                                        0.1902746
 [2,]                                                        0.1842244
 [3,]                                                        0.1855501
 [4,]                                                        0.1913280
 [5,]                                                        0.1930779
 [6,]                                                        0.1609111
 [7,]                                                        0.1696934
 [8,]                                                        0.1781321
 [9,]                                                        0.1907084
[10,]                                                        0.1913635
[11,]                                                        0.1840112
[12,]                                                        0.1886174
[13,]                                                        0.1888000
[14,]                                                        0.2295000
[15,]                                                        0.2199936
[16,]                                                        0.2137000
[17,]                                                        0.2036836
[18,]                                                        0.1829669
[19,]                                                        0.1818172
[20,]                                                        0.2122000
      mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.7
 [1,]                                                       0.07314926
 [2,]                                                       0.06792310
 [3,]                                                       0.07248760
 [4,]                                                       0.07654412
 [5,]                                                       0.07032830
 [6,]                                                       0.05962812
 [7,]                                                       0.06153552
 [8,]                                                       0.06410249
 [9,]                                                       0.07345820
[10,]                                                       0.07704768
[11,]                                                       0.06562383
[12,]                                                       0.07223780
[13,]                                                       0.07250001
[14,]                                                       0.10140000
[15,]                                                       0.09135209
[16,]                                                       0.08470000
[17,]                                                       0.07624704
[18,]                                                       0.06806806
[19,]                                                       0.06944025
[20,]                                                       0.09370000
~~~
{:.output}


~~~r
> inMemory(r_after) # check if raster in memory
~~~
{:title="Console" .input}


~~~
[1] FALSE
~~~
{:.output}


===

Read band information since it is more informative.



~~~r
df_modis_band_info <- read.table(file.path(in_dir, infile_modis_bands_information),
                                 sep=",", stringsAsFactors = F)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> df_modis_band_info
~~~
{:title="Console" .input}


~~~
  band_name band_number start_wlength end_wlength
1       Red           3           620         670
2       NIR           4           841         876
3      Blue           1           459         479
4     Green           2           545         565
5     SWIR1           5          1230        1250
6     SWIR2           6          1628        1652
7     SWIR3           7          2105        2155
~~~
{:.output}


===



~~~r
names(r_after) <- df_modis_band_info$band_name
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> names(r_after)
~~~
{:title="Console" .input}


~~~
[1] "Red"   "NIR"   "Blue"  "Green" "SWIR1" "SWIR2" "SWIR3"
~~~
{:.output}


===

Define bands of interest: Use subset instead of $ if you want to wrap code into function


~~~r
raft_gr <- subset(r_after, "Green") 
raft_SWIR <- subset(r_after, "SWIR2")
raft_NIR <- subset(r_after, "NIR") 
raft_rd <- subset(r_after, "Red")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

Manipulate bands of interest


~~~r
r_after_MNDWI <- (raft_gr - raft_SWIR) / (raft_gr + raft_SWIR)

plot(r_after_MNDWI, zlim = c(-1,1))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-12-1.png" %})
{:.captioned}



~~~r
r_after_NDVI <- (raft_NIR - raft_rd) / (raft_NIR + raft_rd)

plot(r_after_NDVI)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-13-1.png" %})
{:.captioned}



~~~r
> inMemory(r_after_MNDWI) # check if raster in memory
~~~
{:title="Console" .input}


~~~
[1] TRUE
~~~
{:.output}


~~~r
> inMemory(r_after_NDVI)
~~~
{:title="Console" .input}


~~~
[1] TRUE
~~~
{:.output}


===

Write out rasters


~~~r
> dataType(r_after_NDVI)
~~~
{:title="Console" .input}


~~~
[1] "FLT4S"
~~~
{:.output}




~~~r
data_type_str <- dataType(r_after_NDVI)

NAvalue(r_after_MNDWI) <- NA_flag_val # set NA value
out_filename_mn <- file.path(out_dir, paste0("mndwi_post_Rita","_", out_suffix, file_format))
writeRaster(r_after_MNDWI, filename = out_filename_mn, datatype = data_type_str,
            overwrite = T)
 
NAvalue(r_after_NDVI) <- NA_flag_val
out_filename_nd <- file.path(out_dir, paste0("ndvi_post_Rita","_", out_suffix, file_format))
writeRaster(r_after_NDVI, filename = out_filename_nd, datatype = data_type_str,
            overwrite = T)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

Remove rasters from memory


~~~r
rm(r_after_MNDWI)
rm(r_after_NDVI)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}

Can view the .tif images written out in photo viewer.  

Read in rasters 


~~~r
r_after_MNDWI <- raster(out_filename_mn)
r_after_NDVI <- raster(out_filename_nd)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> inMemory(r_after_MNDWI) ; inMemory(r_after_NDVI)
~~~
{:title="Console" .input}


~~~
[1] FALSE
~~~
{:.output}


~~~
[1] FALSE
~~~
{:.output}


===

### Groundtruthing data
Three levels of flooding (water content) determined via external grountruthing. 
Class 1: vegetation and other (small water fraction)
Class 2: Flooded vegetation
Class 3: Flooded area, or water (lake etc)

===

Read in a polygon for each class:



~~~r
class1_data_sf <- st_read(file.path(in_dir, "class1_sites")) 
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
Reading layer `class1_sites' from data source `/nfs/public-data/training/class1_sites' using driver `ESRI Shapefile'
Simple feature collection with 6 features and 2 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 1588619 ymin: 859495.9 xmax: 1830803 ymax: 941484.1
epsg (SRID):    NA
proj4string:    NA
~~~
{:.output}


~~~r
class2_data_sf <- st_read(file.path(in_dir, "class2_sites"))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
Reading layer `class2_sites' from data source `/nfs/public-data/training/class2_sites' using driver `ESRI Shapefile'
Simple feature collection with 8 features and 2 fields
geometry type:  MULTIPOLYGON
dimension:      XY
bbox:           xmin: 1605433 ymin: 848843.8 xmax: 1794265 ymax: 884027.8
epsg (SRID):    NA
proj4string:    NA
~~~
{:.output}


~~~r
class3_data_sf <- st_read(file.path(in_dir, "class3_sites"))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
Reading layer `class3_sites' from data source `/nfs/public-data/training/class3_sites' using driver `ESRI Shapefile'
Simple feature collection with 9 features and 2 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 1587649 ymin: 845271.3 xmax: 1777404 ymax: 891073.1
epsg (SRID):    NA
proj4string:    NA
~~~
{:.output}




~~~r
> class1_data_sf
~~~
{:title="Console" .input}


~~~
Simple feature collection with 6 features and 2 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 1588619 ymin: 859495.9 xmax: 1830803 ymax: 941484.1
epsg (SRID):    NA
proj4string:    NA
  record_ID class_ID                       geometry
1         1        1 POLYGON ((1610283 929863.7,...
2         2        1 POLYGON ((1755141 900167.2,...
3         3        1 POLYGON ((1688209 924699.1,...
4         4        1 POLYGON ((1821103 866597.2,...
5         5        1 POLYGON ((1806876 941161.3,...
6         6        1 POLYGON ((1588619 899198.8,...
~~~
{:.output}


~~~r
> plot(r_after_MNDWI, zlim=c(-1,1)) 
> plot(class3_data_sf["class_ID"], add = TRUE, color = NA, border = "blue", lwd = 3)
~~~
{:title="Console" .input}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-21-1.png" %})
{:.captioned}

===

Combine polygons; note they should be in the same projection system.


~~~r
class_data_sf <- rbind(class1_data_sf, class2_data_sf, class3_data_sf)

class_data_sf$poly_ID <- 1:nrow(class_data_sf) # unique ID for each polygon
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


23 different polygons used as ground truth data.


~~~r
> nrow(class_data_sf) 
~~~
{:title="Console" .input}


~~~
[1] 23
~~~
{:.output}


===

Make new rasters and raster stack.


~~~r
r_x <- init(r_after, "x") # initializes a raster with coordinates x
r_y <- init(r_after, "y") # initializes a raster with coordiates y

r_stack <- stack(r_x, r_y, r_after, r_after_NDVI, r_after_MNDWI)
names(r_stack) <- c("x", "y", "Red", "NIR", "Blue", "Green", "SWIR1", "SWIR2", "SWIR3", "NDVI", "MNDWI")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> str(r_stack, max.level = 2)
~~~
{:title="Console" .input}


~~~
Formal class 'RasterStack' [package "raster"] with 11 slots
  ..@ filename: chr ""
  ..@ layers  :List of 11
  ..@ title   : chr(0) 
  ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  ..@ rotated : logi FALSE
  ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  ..@ ncols   : int 298
  ..@ nrows   : int 116
  ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  ..@ history : list()
  ..@ z       : list()
~~~
{:.output}




~~~r
pixels_extracted_df <- extract(r_stack, class_data_sf, df=T)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> dim(pixels_extracted_df) 
~~~
{:title="Console" .input}


~~~
[1] 1547   12
~~~
{:.output}

We have 1547 pixels extracted.

===

Sometimes useful to store data as a csv: have to remove geometry column.


~~~r
class_data_df <- class_data_sf
st_geometry(class_data_df) <- NULL # This will coerce the sf object into a data.frame by removing the geometry
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Make the dataframe used for classification.


~~~r
pixels_df <- merge(pixels_extracted_df, class_data_df, by.x="ID", by.y="poly_ID")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> head(pixels_df)
~~~
{:title="Console" .input}


~~~
  ID       x        y        Red       NIR       Blue      Green     SWIR1
1  1 1615340 936279.2 0.05304649 0.3035397 0.02652384 0.05662384 0.3101271
2  1 1614413 935352.5 0.05782417 0.3295924 0.02751791 0.06305390 0.3453724
3  1 1615340 935352.5 0.05735013 0.3037102 0.02596963 0.05654212 0.3176476
4  1 1616266 935352.5 0.05805403 0.3081988 0.02711912 0.05750944 0.3229090
5  1 1617193 935352.5 0.04147716 0.3058677 0.01839499 0.05076184 0.3003513
6  1 1618120 935352.5 0.04209801 0.3089238 0.01832494 0.05200353 0.2943777
      SWIR2      SWIR3      NDVI      MNDWI record_ID class_ID
1 0.1936452 0.07864757 0.7024759 -0.5474963         1        1
2 0.2102322 0.08679578 0.7014884 -0.5385503         1        1
3 0.2073480 0.08732812 0.6823238 -0.5714722         1        1
4 0.2079238 0.08741243 0.6829839 -0.5666749         1        1
5 0.1692552 0.06590501 0.7611759 -0.5385644         1        1
6 0.1697936 0.06790353 0.7601402 -0.5310712         1        1
~~~
{:.output}




~~~r
table(pixels_df$class_ID) # count by class of pixels ground truth data
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~

  1   2   3 
508 285 754 
~~~
{:.output}


===

### Examining data used for the classification

Define geographic range.



~~~r
x_range <- range(pixels_df$Green, na.rm=T)
y_range <- range(pixels_df$NIR, na.rm=T)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Let's use a palette that reflects flooding or level of water.



~~~r
col_palette <- c("green", "lightblue", "darkblue")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

Many ways of looking at these data:



~~~r
> plot(NIR~Green, xlim=x_range, ylim=y_range, cex=0.3, col=col_palette[1], subset(pixels_df, class_ID==1))
> points(NIR~Green, col=col_palette[2], cex=0.3, subset(pixels_df, class_ID==2))
> points(NIR~Green, col=col_palette[3], cex=0.3, subset(pixels_df, class_ID==3))
> names_vals <- c("vegetation","wetland","water")
> legend("topleft", legend=names_vals,
+        pt.cex=0.7, cex=0.7, col=col_palette,
+        pch=20, # add circle symbol to line
+        bty="n")
~~~
{:title="Console" .input}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-34-1.png" %})
{:.captioned}

===



~~~r
plot(NDVI ~ MNDWI, xlim=c(-1,1), ylim=c(-1,1), cex=0.3, col=col_palette[1], subset(pixels_df, class_ID==1))
points(NDVI ~ MNDWI, cex=0.3, col=col_palette[2], subset(pixels_df, class_ID==2))
points(NDVI ~ MNDWI, cex=0.3, col=col_palette[3], subset(pixels_df, class_ID==3))
names_vals <- c("vegetation","wetland","water")
legend("topright", legend=names_vals,
       pt.cex=0.7,cex=0.7, col=col_palette,
       pch=20, # add circle symbol to line
       bty="n")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-35-1.png" %})
{:.captioned}

===



~~~r
rasterVis::histogram(r_after)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-36-1.png" %})
{:.captioned}

===



~~~r
pixels_df$class_ID <- factor(pixels_df$class_ID, levels = c(1,2,3), labels = names_vals)

boxplot(MNDWI~class_ID, pixels_df, xlab="category",
        main="Boxplot for MNDWI per class")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-37-1.png" %})
{:.captioned}

===

## Part II: Split data into training and testing datasets 

===

Let's keep 30% of data for testing for each class, and set a fixed seed for reproducibility.



~~~r
set.seed(100)
prop <- 0.3
table(pixels_df$class_ID)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~

vegetation    wetland      water 
       508        285        754 
~~~
{:.output}


===



~~~r
list_data_df <- vector("list", length=3)
level_labels <- names_vals

for(i in 1:3){
  data_df <- subset(pixels_df, class_ID==level_labels[i])
  data_df$pix_id <- 1:nrow(data_df)
  indices <- as.vector(createDataPartition(data_df$pix_id, p=(1-prop), list=F))
  data_df$training <- as.numeric(data_df$pix_id %in% indices)
  list_data_df[[i]] <- data_df
}

data_df <- do.call(rbind, list_data_df)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> dim(data_df)
~~~
{:title="Console" .input}


~~~
[1] 1547   16
~~~
{:.output}


~~~r
> head(data_df)
~~~
{:title="Console" .input}


~~~
  ID       x        y        Red       NIR       Blue      Green     SWIR1
1  1 1615340 936279.2 0.05304649 0.3035397 0.02652384 0.05662384 0.3101271
2  1 1614413 935352.5 0.05782417 0.3295924 0.02751791 0.06305390 0.3453724
3  1 1615340 935352.5 0.05735013 0.3037102 0.02596963 0.05654212 0.3176476
4  1 1616266 935352.5 0.05805403 0.3081988 0.02711912 0.05750944 0.3229090
5  1 1617193 935352.5 0.04147716 0.3058677 0.01839499 0.05076184 0.3003513
6  1 1618120 935352.5 0.04209801 0.3089238 0.01832494 0.05200353 0.2943777
      SWIR2      SWIR3      NDVI      MNDWI record_ID   class_ID pix_id
1 0.1936452 0.07864757 0.7024759 -0.5474963         1 vegetation      1
2 0.2102322 0.08679578 0.7014884 -0.5385503         1 vegetation      2
3 0.2073480 0.08732812 0.6823238 -0.5714722         1 vegetation      3
4 0.2079238 0.08741243 0.6829839 -0.5666749         1 vegetation      4
5 0.1692552 0.06590501 0.7611759 -0.5385644         1 vegetation      5
6 0.1697936 0.06790353 0.7601402 -0.5310712         1 vegetation      6
  training
1        0
2        1
3        1
4        1
5        1
6        0
~~~
{:.output}




~~~r
data_training <- subset(data_df, training==1)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


===

## Part III: Generate classification using classification tree (CART) and support vector machine (SVM)

===

### Classification and Regression Tree model (CART) 

Fit model using training data for CART.



~~~r
mod_rpart <- rpart(class_ID ~ Red + NIR + Blue + Green + SWIR1 + SWIR2 + SWIR3,
                   method="class", data=data_training)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Plot the fitted classification tree.



~~~r
plot(mod_rpart, uniform = TRUE, main = "Classification Tree", mar = c(0.1, 0.1, 0.1, 0.1))
text(mod_rpart, cex = .8)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-43-1.png" %})
{:.captioned}

===

Now predict the subset data based on the model.



~~~r
raster_out_filename_rp <- file.path(out_dir, paste0("r_predicted_rpart_", out_suffix, file_format))

r_predicted_rpart <- predict(r_stack, mod_rpart, type='class', filename = raster_out_filename_rp,
                             progress = 'text', overwrite = T)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> plot(r_predicted_rpart)
~~~
{:title="Console" .input}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-45-1.png" %})
{:.captioned}



~~~r
r_predicted_rpart <- ratify(r_predicted_rpart) # creates a raster attribute table (rat)
rat <- levels(r_predicted_rpart)[[1]]
rat$legend <- c("vegetation", "wetland", "water")
levels(r_predicted_rpart) <- rat

levelplot(r_predicted_rpart, maxpixels = 1e6,
          col.regions = c("green","blue","darkblue"),
          scales=list(draw=FALSE),
          main = "Classification Tree")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-46-1.png" %})
{:.captioned}

===

### Support Vector Machine classification (SVM)

Set class_ID as factor to generate classification.



~~~r
mod_svm <- svm(class_ID ~ Red + NIR + Blue + Green + SWIR1 + SWIR2 + SWIR3,
               data=data_training, method="C-classification",
               kernel="linear") # can be radial
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> summary(mod_svm)
~~~
{:title="Console" .input}


~~~

Call:
svm(formula = class_ID ~ Red + NIR + Blue + Green + SWIR1 + SWIR2 + 
    SWIR3, data = data_training, method = "C-classification", 
    kernel = "linear")


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  1 

Number of Support Vectors:  111

 ( 11 55 45 )


Number of Classes:  3 

Levels: 
 vegetation wetland water
~~~
{:.output}


===

Now predict the subset data based on the model.



~~~r
raster_out_filename_sv <- file.path(out_dir, paste0("r_predicted_svm_", out_suffix, file_format))
  
r_predicted_svm <- predict(r_stack, mod_svm, progress = 'text',
                           filename = raster_out_filename_sv, overwrite = T)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
plot(r_predicted_svm)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-50-1.png" %})
{:.captioned}



~~~r
rasterVis::histogram(r_predicted_svm)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-51-1.png" %})
{:.captioned}



~~~r
r_predicted_svm <- ratify(r_predicted_svm)
rat <- levels(r_predicted_svm)[[1]]
rat$legend <- c("vegetation", "wetland", "water")
levels(r_predicted_svm) <- rat

levelplot(r_predicted_svm, maxpixels = 1e6,
          col.regions = c("green","blue","darkblue"),
          scales = list(draw = FALSE),
          main = "SVM classification")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}
![ ]({% include asset.html path="images/raster_classification/unnamed-chunk-52-1.png" %})
{:.captioned}

===

## Part IV: Assessment and comparison of model performance

Set up testing data:
Full dataset, let's use data points for testing.


~~~r
> dim(data_df) 
~~~
{:title="Console" .input}


~~~
[1] 1547   16
~~~
{:.output}




~~~r
# omit values that contain NA, because may be problematic with SVM.
data_testing <- na.omit(subset(data_df, training==0)) 
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}




~~~r
> dim(data_testing)
~~~
{:title="Console" .input}


~~~
[1] 452  16
~~~
{:.output}


===

Predict on testing data using rpart model fitted with testing data.



~~~r
testing_rpart <- predict(mod_rpart, data_testing, type='class')
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Predict on testing data using SVM model fitted with testing data.



~~~r
testing_svm <- predict(mod_svm, data_testing, type='class')
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Predicted number of pixels in each class:



~~~r
table(testing_rpart)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
testing_rpart
vegetation    wetland      water 
       153         77        222 
~~~
{:.output}


~~~r
table(testing_svm)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
testing_svm
vegetation    wetland      water 
       152         84        216 
~~~
{:.output}


===

### Generate confusion matrix to assess the accuracy of the image classification

More info on confusion matrices here: http://spatial-analyst.net/ILWIS/htm/ilwismen/confusion_matrix.htm

Groundtruth data: 



~~~r
> table(data_testing$class_ID)
~~~
{:title="Console" .input}


~~~

vegetation    wetland      water 
       151         84        217 
~~~
{:.output}


Classification model results:



~~~r
> table(testing_rpart)
~~~
{:title="Console" .input}


~~~
testing_rpart
vegetation    wetland      water 
       153         77        222 
~~~
{:.output}


~~~r
> table(testing_svm)
~~~
{:title="Console" .input}


~~~
testing_svm
vegetation    wetland      water 
       152         84        216 
~~~
{:.output}


To generate confusion matrix, need to use: 
testing_rpart: contains classification model prediction in the rows
data_testing$class_ID: this column contains groundtruth data



~~~r
tb_rpart <- table(testing_rpart, data_testing$class_ID)
tb_svm <- table(testing_svm, data_testing$class_ID)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Matrices identifying correct and incorrect classifications by models:



~~~r
> tb_rpart
~~~
{:title="Console" .input}


~~~
             
testing_rpart vegetation wetland water
   vegetation        148       5     0
   wetland             3      70     4
   water               0       9   213
~~~
{:.output}


~~~r
> tb_svm
~~~
{:title="Console" .input}


~~~
            
testing_svm  vegetation wetland water
  vegetation        151       1     0
  wetland             0      77     7
  water               0       6   210
~~~
{:.output}


===

Accuracy (producer's accuracy): the fraction of correctly classified pixels compared to all pixels of that groundtruth class.



~~~r
tb_rpart[1]/sum(tb_rpart[,1]) # producer accuracy
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
[1] 0.9801325
~~~
{:.output}


~~~r
tb_svm[1]/sum(tb_svm[,1]) # producer accuracy
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
[1] 1
~~~
{:.output}


Looking at accuracy more closely:



~~~r
# overall accuracy for rpart
sum(diag(tb_rpart))/sum(table(testing_rpart))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
[1] 0.9535398
~~~
{:.output}


~~~r
# overall accuracy for svm
sum(diag(tb_svm))/sum(table(testing_svm))
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


~~~
[1] 0.9690265
~~~
{:.output}


===

Generate more accurate measurements from functions in the caret package:



~~~r
accuracy_info_rpart <- confusionMatrix(testing_rpart, data_testing$class_ID, positive = NULL)
accuracy_info_svm <- confusionMatrix(testing_svm, data_testing$class_ID, positive = NULL)
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}


Overall accuracy produced:



~~~r
> accuracy_info_rpart$overall
~~~
{:title="Console" .input}


~~~
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
  9.535398e-01   9.249994e-01   9.298553e-01   9.710138e-01   4.800885e-01 
AccuracyPValue  McnemarPValue 
 3.536418e-108            NaN 
~~~
{:.output}




~~~r
> accuracy_info_svm$overall
~~~
{:title="Console" .input}


~~~
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
  9.690265e-01   9.503390e-01   9.485775e-01   9.829652e-01   4.800885e-01 
AccuracyPValue  McnemarPValue 
 3.961900e-118            NaN 
~~~
{:.output}


Write out the results:



~~~r
table_out_filename <- file.path(out_dir, "confusion_matrix_rpart.txt")
write.table(accuracy_info_rpart$table, table_out_filename, sep=",")

table_out_filename <- file.path(out_dir, "confusion_matrix_svm.txt")
write.table(accuracy_info_svm$table, table_out_filename, sep=",")
~~~
{:title="{{ site.data.lesson.handouts[0] }}" .text-document}

