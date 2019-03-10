---
editor_options: 
  chunk_output_type: console
excerpt: Introduction to Raster Classification
---

Remote Sensing allows for fast generation of maps before and after events. In this exercise, you will map flooding using machine learning methods and MODIS reflectance data (MOD09). Hurricanes can strongly impact vegetation and human settlements. Hurricane Rita made landfall on September 24, 2005 in Louisiana near the border with Texas. In addition to the rainfall, the hurricane was accompanied by a storm surge that generated flooding and a large amount of damage to ground infrastructure and vegetation. As a geospatial analyst, you will experiment with different classifiers for rapid mapping and examine their performance with training and testing data. All work must be repeatable and documented in a scripting environment to be shared with your team.

===

## Inputs

- FEMA flood map are as vector data
- MOD09 reflectance image after Hurricane Rita
- Vector polygons of ground truth for training and testing classifiers
- NLCD 2006 land cover aggregated at 1km.

===

## Outputs

- classified map using rpart classifier
- classified map using svm classifier
- confusion matrices and overall accuracies by classifiers
 
===
             
## Tools/Functions

- brick
- writeRaster
- st_read
- subset (raster)
- extract
- predict
- levelplot
- raster algebra: “*”,” /”,”+”,”-“
- histogram
 
===

## Part I: Read and map flooding from RITA hurricane  

Goal: Map hurricane flooding using reflectance data derived from MODIS.  

Explore the datasets provided. Use ground truth vector datasets and extract values for pixels for the raster stack containing the original MOD09 bands and indices. Visualize ground truth pixels in feature space and using boxplots.
{:.notes}

Products:
- Scatterplot in feature space with the three ground classes.
- Boxplots of values for MNDWI indices.

===

## Part II: Split data into training and testing datasets 

Assessment of classifications require splitting of the ground truth data into training and testing samples. The training data is used in the fitting of the model while the testing dataset is used accuracy assessment. Using the data provided randomly select 30% ground truth data for testing.

Products:
- Training and testing datasets at 30%. Save these into shapefiles.

From this product, the analyst should be able to answer the following question:
- What is the frequency of observations in each class for training and testing data?

===

## Part III:  Generate classification using classification tree (CART) and support vector machine (SVM) 

You will use MOD09 reflectance bands to generate a classification of three classes (“vegetation”, “wetland”, “water”). Fit a model with the training data for each method and generate a classification on a raster using the “predict” command.
 
RPART: It is based on CART and generates classification by splitting data points into branches successively and generating classified pixels. The rules generated from the tree can then be applied to the full raster image to produce a prediction.

SVM: Support Vector Machine  generate hyperplanes that maximize separabiltiy between classes.These hyperplanes boundaries are used in the classification process and generate predictions.

Products:
- Model object for rpart with a predicted raster.
- Model object for svm with a predicted raster

===

## Part IV: Assessment and comparison of model performance

To validate the model, we use the testing data and generate new predictions for rpart and SVM. We compare the predictions with the original testing information.

Overall accuracy: total number of correctly classified pixels for all classes combined over the total number of pixels. 

Producer accuracy: is the fraction of correctly classified pixels with regard to all pixels of the ground truth class.

Products:
- Overall accuracy and producer accuracy metrics.
- Confusion matrix for rpart and SVM.

From output answer the following questions:
- What classifier had the best overall accuracy?

===

Using the analyses performed above, and datasets provided answer to the following questions:

- Modify the splitting of training and testing by using 40% and then 50% for testing. Compare predicted maps with previous results.
- Generate confusion matrices for 40% and 50% testing.
- Generate a classification prediction using randomForest and compare it to svm and rpart with 30% holdout.
