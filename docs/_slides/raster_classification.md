---
editor_options: 
  chunk_output_type: console
excerpt: Raster Classification using flood mapping analysis
---




### Set up arguments and parameters



~~~r
in_dir <- "data"

out_dir_suffix <- "rsc_lesson"
out_dir <- paste("output_", out_dir_suffix, sep="")
dir.create(out_dir, showWarnings = FALSE)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

Regional coordinate reference system taken from: 
http://spatialreference.org/ref/epsg/nad83-texas-state-mapping-system/proj4/



~~~r
CRS_reg <- "+proj=lcc +lat_1=27.41666666666667 +lat_2=34.91666666666666 +lat_0=31.16666666666667 +lon_0=-100 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

file_format <- ".tif" 

NA_flag_val <- -9999

out_suffix <- "raster_classification" # output suffix for the files and output folder  
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

### Define input data file names



~~~r
infile_RITA_reflectance_date2 <- "mosaiced_MOD09A1_A2005273__006_reflectance_masked_RITA_reg_1km.tif"

infile_reg_outline <- "new_strata_rita_10282017" # Region outline and FEMA zones

infile_modis_bands_information <- "df_modis_band_info.txt" # MOD09 bands information.

nlcd_2006_filename <- "nlcd_2006_RITA.tif" # NLCD2006 Land cover data aggregated at ~ 1km.
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

## Part I: Read and map flooding from RITA hurricane 

===

### Display and explore the data

MOD09 raster image after hurricane Rita.



~~~r
r_after <- brick(file.path(in_dir, infile_RITA_reflectance_date2))
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

Read band information since it is more informative.



~~~r
df_modis_band_info <- read.table(file.path(in_dir, infile_modis_bands_information),
                                 sep=",",
                                 stringsAsFactors = F)
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> df_modis_band_info
~~~
{:.input title="Console"}


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
df_modis_band_info$band_number <- c(3,4,1,2,5,6,7)

band_refl_order <- df_modis_band_info$band_number

names(r_after) <- df_modis_band_info$band_name
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> names(r_after)
~~~
{:.input title="Console"}


~~~
[1] "Red"   "NIR"   "Blue"  "Green" "SWIR1" "SWIR2" "SWIR3"
~~~
{:.output}


===

Note: Use subset instead of $ if you want to wrap code into function.



~~~r
r_after_MNDWI <- (subset(r_after, "Green") - subset(r_after, "SWIR2")) / (subset(r_after, "Green") + subset(r_after, "SWIR2"))

plot(r_after_MNDWI, zlim = c(-1,1))
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-10-1.png)
{:.captioned}

===



~~~r
r_after_NDVI <- (subset(r_after,"NIR") - subset(r_after,"Red")) / (subset(r_after,"NIR") + subset(r_after,"Red"))

plot(r_after_NDVI)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-11-1.png)
{:.captioned}



~~~r
data_type_str <- dataType(r_after_NDVI)

NAvalue(r_after_MNDWI) <- NA_flag_val
out_filename <- paste("mndwi_post_Rita","_", out_suffix, file_format, sep="")
out_filename <- file.path(out_dir, out_filename)
writeRaster(r_after_MNDWI,
            filename = out_filename,
            datatype = data_type_str,
            overwrite = T)
 
NAvalue(r_after_NDVI) <- NA_flag_val
out_filename <- paste("ndvi_post_Rita","_", out_suffix, file_format, sep="")
out_filename <- file.path(out_dir, out_filename)
writeRaster(r_after_NDVI,
            filename = out_filename,
            datatype = data_type_str,
            overwrite = T)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

Classes for Classification:

1. vegetation and other (small water fraction)
2. Flooded vegetation
3. Flooded area, or water (lake, etc.)

===

Make a dataframe for each class:



~~~r
class1_data_sf <- st_read(file.path(in_dir, "class1_sites"))
~~~
{:.text-document title="{{ site.handouts[0] }}"}


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
{:.text-document title="{{ site.handouts[0] }}"}


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
{:.text-document title="{{ site.handouts[0] }}"}


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


===

Combine objects; note they should be in the same projection system.


~~~r
class_data_sf <- rbind(class1_data_sf, class2_data_sf, class3_data_sf)

class_data_sf$poly_ID <- 1:nrow(class_data_sf) # unique ID for each polygon
~~~
{:.text-document title="{{ site.handouts[0] }}"}


23 different polygons used as ground truth data.


~~~r
> nrow(class_data_sf) 
~~~
{:.input title="Console"}


~~~
[1] 23
~~~
{:.output}


===



~~~r
class_data_sp <- as(class_data_sf, "Spatial")

r_x <- init(r_after, "x") # raster with coordinates x
r_y <- init(r_after, "x") # raster with coordiates y

r_stack <- stack(r_x, r_y, r_after, r_after_NDVI, r_after_MNDWI)
names(r_stack) <- c("x", "y", "Red", "NIR", "Blue", "Green", "SWIR1", "SWIR2", "SWIR3", "NDVI", "MNDWI")
pixels_extracted_df <- extract(r_stack, class_data_sf, df=T)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result

Warning in merge.data.frame(as.data.frame(x), as.data.frame(y), ...):
column name 'x' is duplicated in the result
~~~
{:.output}




~~~r
> dim(pixels_extracted_df) 
~~~
{:.input title="Console"}


~~~
[1] 1547   12
~~~
{:.output}

We have 1547 pixels extracted.

===



~~~r
class_data_df <- class_data_sf
st_geometry(class_data_df) <- NULL # This will coerce the sf object into a data.frame

pixels_df <- merge(pixels_extracted_df, class_data_df, by.x="ID", by.y="poly_ID")
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> head(pixels_df)
~~~
{:.input title="Console"}


~~~
  ID       x       y        Red       NIR       Blue      Green     SWIR1
1  1 1615340 1615340 0.05304649 0.3035397 0.02652384 0.05662384 0.3101271
2  1 1614413 1614413 0.05782417 0.3295924 0.02751791 0.06305390 0.3453724
3  1 1615340 1615340 0.05735013 0.3037102 0.02596963 0.05654212 0.3176476
4  1 1616266 1616266 0.05805403 0.3081988 0.02711912 0.05750944 0.3229090
5  1 1617193 1617193 0.04147716 0.3058677 0.01839499 0.05076184 0.3003513
6  1 1618120 1618120 0.04209801 0.3089238 0.01832494 0.05200353 0.2943777
      SWIR2      SWIR3      NDVI      MNDWI record_ID class_ID
1 0.1936452 0.07864757 0.7024759 -0.5474962         1        1
2 0.2102322 0.08679578 0.7014884 -0.5385502         1        1
3 0.2073480 0.08732812 0.6823238 -0.5714722         1        1
4 0.2079238 0.08741243 0.6829839 -0.5666749         1        1
5 0.1692552 0.06590501 0.7611759 -0.5385644         1        1
6 0.1697936 0.06790353 0.7601402 -0.5310712         1        1
~~~
{:.output}




~~~r
table(pixels_df$class_ID) # count by class of pixels ground truth data
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~

  1   2   3 
508 285 754 
~~~
{:.output}


===

### Examining site data used for the classification

Water



~~~r
x_range <- range(pixels_df$Green, na.rm=T)
y_range <- range(pixels_df$NIR, na.rm=T)

plot(NIR~Green, xlim=x_range, ylim=y_range, cex=0.2, col="blue", subset(pixels_df, class_ID==1))
points(NIR~Green, col="green", cex=0.2, subset(pixels_df, class_ID==2))
points(NIR~Green, col="red", cex=0.2, subset(pixels_df, class_ID==3))
names_vals <- c("water class 1", "water class 2", "water class 3")
legend("topleft", legend=names_vals,
       pt.cex=0.7, cex=0.7, col=c("blue", "green", "red"),
       pch=20, # add circle symbol to line
       bty="n")
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-21-1.png)
{:.captioned}

===

Let's use a palette that reflects wetness or level of water. 



~~~r
col_palette <- c("green","blue","darkblue")

plot(NDVI ~ MNDWI, xlim=c(-1,1), ylim=c(-1,1), cex=0.2, col=col_palette[1], subset(pixels_df,class_ID==1))
points(NDVI ~ MNDWI, cex=0.2, col=col_palette[2], subset(pixels_df,class_ID==2))
points(NDVI ~ MNDWI, cex=0.2, col=col_palette[3], subset(pixels_df,class_ID==3))
names_vals <- c("vegetation","wetland","water")
legend("topright",legend=names_vals,
       pt.cex=0.7,cex=0.7,col=col_palette,
       pch=20, # add circle symbol to line
       bty="n")
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-22-1.png)
{:.captioned}

===



~~~r
rasterVis::histogram(r_after)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-23-1.png)
{:.captioned}

===



~~~r
pixels_df$class_ID <- factor(pixels_df$class_ID,
                      levels = c(1,2,3),
                      labels = names_vals)

boxplot(MNDWI~class_ID,
        pixels_df,
        xlab="category",
        main="Boxplot for MNDWI per class")
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-24-1.png)
{:.captioned}

===

## Part II: Split data into training and testing datasets 

===

Let's keep 30% of data for testing for each class, and set a fixed seed for reproducibility.



~~~r
pixels_df$pix_ID <- 1:nrow(pixels_df)
set.seed(100)
prop <- 0.3
table(pixels_df$class_ID)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~

vegetation    wetland      water 
       508        285        754 
~~~
{:.output}


===

This is for one class: better to do this as a function but we use a loop for clarity here.



~~~r
list_data_df <- vector("list", length=3)
level_labels <- names_vals

for(i in 1:3){
  data_df <- subset(pixels_df, class_ID==level_labels[i])
  data_df$pix_id <- 1:nrow(data_df)
  indices <- as.vector(createDataPartition(data_df$pix_ID, p=0.7, list=F))
  data_df$training <- as.numeric(data_df$pix_id %in% indices)
  list_data_df[[i]] <- data_df
}

data_df <- do.call(rbind, list_data_df)
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> dim(data_df)
~~~
{:.input title="Console"}


~~~
[1] 1547   17
~~~
{:.output}


~~~r
> head(data_df)
~~~
{:.input title="Console"}


~~~
  ID       x       y        Red       NIR       Blue      Green     SWIR1
1  1 1615340 1615340 0.05304649 0.3035397 0.02652384 0.05662384 0.3101271
2  1 1614413 1614413 0.05782417 0.3295924 0.02751791 0.06305390 0.3453724
3  1 1615340 1615340 0.05735013 0.3037102 0.02596963 0.05654212 0.3176476
4  1 1616266 1616266 0.05805403 0.3081988 0.02711912 0.05750944 0.3229090
5  1 1617193 1617193 0.04147716 0.3058677 0.01839499 0.05076184 0.3003513
6  1 1618120 1618120 0.04209801 0.3089238 0.01832494 0.05200353 0.2943777
      SWIR2      SWIR3      NDVI      MNDWI record_ID   class_ID pix_ID
1 0.1936452 0.07864757 0.7024759 -0.5474962         1 vegetation      1
2 0.2102322 0.08679578 0.7014884 -0.5385502         1 vegetation      2
3 0.2073480 0.08732812 0.6823238 -0.5714722         1 vegetation      3
4 0.2079238 0.08741243 0.6829839 -0.5666749         1 vegetation      4
5 0.1692552 0.06590501 0.7611759 -0.5385644         1 vegetation      5
6 0.1697936 0.06790353 0.7601402 -0.5310712         1 vegetation      6
  pix_id training
1      1        0
2      2        1
3      3        0
4      4        0
5      5        1
6      6        0
~~~
{:.output}




~~~r
data_training <- subset(data_df, training==1)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


===

## Part III: Generate classification using classification tree (CART) and support vector machine (SVM)

===

### Classification and Regression Tree model (CART) 

Fit model using training data for CART.



~~~r
mod_rpart <- rpart(class_ID ~ Red + NIR + Blue + Green + SWIR1 + SWIR2 + SWIR3,
                   method="class", 
                   data=data_training)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


Plot the fitted classification tree.



~~~r
plot(mod_rpart, uniform = TRUE, main = "Classification Tree", mar = c(0.1, 0.1, 0.1, 0.1))
text(mod_rpart, cex = .8)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-30-1.png)
{:.captioned}

===

Now predict the subset data based on the model; prediction for entire area takes more time.



~~~r
raster_out_filename <- paste0("r_predicted_rpart_", out_suffix, file_format)
raster_out_filename <- file.path(out_dir, raster_out_filename)

r_predicted_rpart <- predict(r_stack, mod_rpart, 
                             type='class',
                             filename=raster_out_filename,
                             progress = 'text',
                             overwrite=T)
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
plot(r_predicted_rpart)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-32-1.png)
{:.captioned}



~~~r
r_predicted_rpart <- ratify(r_predicted_rpart)
rat <- levels(r_predicted_rpart)[[1]]
rat$legend <- c("vegetation","wetland","water")
levels(r_predicted_rpart) <- rat

levelplot(r_predicted_rpart, maxpixels = 1e6,
          col.regions = c("green","blue","darkblue"),
          scales=list(draw=FALSE),
          main = "Classification Tree")
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-33-1.png)
{:.captioned}

===

### Support Vector Machine classification (SVM)

Set class_ID as factor to generate classification.



~~~r
mod_svm <- svm(class_ID ~ Red +NIR + Blue + Green + SWIR1 + SWIR2 + SWIR3,
               data=data_training,
               method="C-classification",
               kernel="linear") # can be radial
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> summary(mod_svm)
~~~
{:.input title="Console"}


~~~

Call:
svm(formula = class_ID ~ Red + NIR + Blue + Green + SWIR1 + SWIR2 + 
    SWIR3, data = data_training, method = "C-classification", 
    kernel = "linear")


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  1 
      gamma:  0.1428571 

Number of Support Vectors:  110

 ( 12 54 44 )


Number of Classes:  3 

Levels: 
 vegetation wetland water
~~~
{:.output}


===

Now predict the subset data based on the model; prediction for entire area takes longer time.



~~~r
raster_out_filename <- paste0("r_predicted_svm_", out_suffix, file_format)
raster_out_filename <- file.path(out_dir, raster_out_filename)
  
r_predicted_svm <- predict(r_stack, mod_svm,
                           progress = 'text',
                           filename = raster_out_filename,
                           overwrite = T)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
  |                                                                         |                                                                 |   0%  |                                                                         |================                                                 |  25%  |                                                                         |================================                                 |  50%  |                                                                         |=================================================                |  75%  |                                                                         |=================================================================| 100%

~~~
{:.output}


~~~r
plot(r_predicted_svm)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-36-1.png)
{:.captioned}



~~~r
rasterVis::histogram(r_predicted_svm)
~~~
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-37-1.png)
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
{:.text-document title="{{ site.handouts[0] }}"}
![ ]({{ site.baseurl }}/images/raster_classification/unnamed-chunk-38-1.png)
{:.captioned}

===

## Part IV: Assessment and comparison of model performance

Set up testing data:
Full dataset, let's use data points for testing


~~~r
> dim(data_df) 
~~~
{:.input title="Console"}


~~~
[1] 1547   17
~~~
{:.output}




~~~r
# omit values that contain NA, because may be problematic with SVM.
data_testing <- na.omit(subset(data_df, training==0)) 
~~~
{:.text-document title="{{ site.handouts[0] }}"}




~~~r
> dim(data_testing)
~~~
{:.input title="Console"}


~~~
[1] 450  17
~~~
{:.output}


===

Predict on testing data using rpart model fitted with training data.



~~~r
testing_rpart <- predict(mod_rpart, data_testing, type='class')
~~~
{:.text-document title="{{ site.handouts[0] }}"}


Predict on testing data using SVM model fitted with training data



~~~r
testing_svm <- predict(mod_svm, data_testing, type='class')
~~~
{:.text-document title="{{ site.handouts[0] }}"}


Predicted classes:



~~~r
table(testing_svm)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
testing_svm
vegetation    wetland      water 
       150         78        222 
~~~
{:.output}


===

### Generate confusion matrix to assess the accuracy of the image classification

More info on confusion matrices here: http://spatial-analyst.net/ILWIS/htm/ilwismen/confusion_matrix.htm

Need to use: 
testing_rpart: contains map prediction in the rows
data_testing$class_ID: this column contains ground truth data



~~~r
tb_rpart <- table(testing_rpart, data_testing$class_ID)
tb_svm <- table(testing_svm, data_testing$class_ID)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


classification, map results



~~~r
> table(testing_rpart)
~~~
{:.input title="Console"}


~~~
testing_rpart
vegetation    wetland      water 
       149         77        224 
~~~
{:.output}


reference, ground truth in columns



~~~r
> table(data_testing$class_ID)
~~~
{:.input title="Console"}


~~~

vegetation    wetland      water 
       149         84        217 
~~~
{:.output}


===

Accuracy (producer's accuracy): the fraction of correctly classified pixels compared to all pixels of that ground truth class.



~~~r
tb_rpart[1]/sum(tb_rpart[,1]) # producer accuracy
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
[1] 0.9798658
~~~
{:.output}


~~~r
tb_svm[1]/sum(tb_svm[,1]) # producer accuracy
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
[1] 1
~~~
{:.output}


Looking at accuracy more closely:



~~~r
# overall accuracy for svm
sum(diag(tb_svm))/sum(table(testing_svm))
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
[1] 0.9644444
~~~
{:.output}


~~~r
# overall accuracy for rpart
sum(diag(tb_rpart))/sum(table(testing_rpart))
~~~
{:.text-document title="{{ site.handouts[0] }}"}


~~~
[1] 0.9444444
~~~
{:.output}


===

Generate more accurate measurements from functions in the caret package:



~~~r
accuracy_info_svm <- confusionMatrix(testing_svm, data_testing$class_ID, positive = NULL)
accuracy_info_rpart <- confusionMatrix(testing_rpart, data_testing$class_ID, positive = NULL)
~~~
{:.text-document title="{{ site.handouts[0] }}"}


Overall accuracy produced:



~~~r
> accuracy_info_rpart$overall
~~~
{:.input title="Console"}


~~~
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
  9.444444e-01   9.101603e-01   9.190787e-01   9.637284e-01   4.822222e-01 
AccuracyPValue  McnemarPValue 
 1.266887e-101            NaN 
~~~
{:.output}




~~~r
> accuracy_info_svm$overall
~~~
{:.input title="Console"}


~~~
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
  9.644444e-01   9.425947e-01   9.429011e-01   9.795429e-01   4.822222e-01 
AccuracyPValue  McnemarPValue 
 9.645095e-114            NaN 
~~~
{:.output}


Write out the results:



~~~r
table_out_filename <- file.path(out_dir, "confusion_matrix_rpart.txt")
write.table(accuracy_info_rpart$table, table_out_filename, sep=",")

table_out_filename <- file.path(out_dir, "confusion_matrix_svm.txt")
write.table(accuracy_info_svm$table, table_out_filename, sep=",")
~~~
{:.text-document title="{{ site.handouts[0] }}"}

