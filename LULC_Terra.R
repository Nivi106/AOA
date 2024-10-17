# Load the required packages
library(terra)
library(caret)
library(mapview)
library(sf)
library(CAST)
library(tmap)
library(doParallel)
library(parallel)
library(Orcs)

# Load the Uppsala raster as a terra object
sen_le <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Uppsala_Stack.grd")
print(sen_le)

# RGB plotting using terra for Uppsala
plotRGB(sen_le, r = 3, g = 2, b = 1, stretch = "lin")




# Load the Florence raster
sen_es <- rast("C:/Users/Nivethitha/OneDrive/Desktop/Florence_Stack.grd")
# RGB plotting using terra for Florence
plotRGB(sen_es, r = 3, g = 2, b = 1, stretch = "lin")

# Load the training sites
trainSites <- st_read("C:/Users/Nivethitha/Documents/uppsalatrain.gpkg")
print(trainSites)

# Stack the RGB bands into a single SpatRaster for Uppsala
rgb_raster_le <- c(sen_le[[3]], sen_le[[2]], sen_le[[1]])

# Simplify the legend by setting fewer breaks for mapview
breaks <- seq(0, 3000, length.out = 10)  # Adjust the number of breaks as needed

# Display the RGB raster and training sites in mapview with simplified legend
mapview(rgb_raster_le, rgb = TRUE, at = breaks, map.types = "Esri.WorldImagery", legend = TRUE) +
  mapview(trainSites)

# Extract values from terra object for Uppsala
extr <- extract(sen_le, vect(trainSites), df=TRUE)
extr <- merge(extr, trainSites, by.x="ID", by.y="PolygonID")
head(extr)

# Sample training data
set.seed(100)
trainids <- createDataPartition(extr$ID, list=FALSE, p=0.05)
trainDat <- extr[trainids,]

predictors <- names(sen_le)
response <- "Label"

# Create spatial folds
indices <- CreateSpacetimeFolds(trainDat, spacevar = "ID", k=3, class="Label")
ctrl <- trainControl(method="cv", index = indices$index, savePredictions = TRUE)

# Train the Random Forest model
set.seed(100)
model <- ffs(trainDat[,predictors], trainDat[,response], method="rf", metric="Kappa", trControl=ctrl, importance=TRUE, ntree=75)
print(model)
plot(varImp(model))

# Get cross-validated predictions
cvPredictions <- model$pred[model$pred$mtry == model$bestTune$mtry,]
table(cvPredictions$pred, cvPredictions$obs)

# Predict using the terra object for Uppsala
prediction <- predict(sen_le, model, na.rm = TRUE)
cols <- c("sandybrown", "green", "darkred", "blue", "forestgreen", "lightgreen", "red")

# Plot the prediction for Uppsala using tmap
pred_le<-tm_shape(prediction) +
  tm_raster(palette = cols, title = "Land Use Classification") +
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75)
pred_le
# Calculate Area of Applicability (AOA) for Uppsala
cl <- makeCluster(4)
registerDoParallel(cl)
AOA <- aoa(sen_le, model, useWeight = FALSE)


summary(AOA$AOA)

highAOA <- AOA$AOA >= 1
tm_shape(highAOA) + 
  tm_raster(palette = "Blues", title = "High Applicability Areas") +
  tm_layout(main.title = "Filtered AOA (>= 1)")


# Create a mask for areas where AOA = 1 (valid prediction areas)
prediction_aoa <- mask(prediction, AOA$AOA == 1)

# Create a mask for areas where AOA = 0 (grey areas)
aoa_grey <- ifel(AOA$AOA == 0, 1, NA)  # Areas with AOA = 0 will be grey

# Create a final raster to display predictions only where AOA = 1
final_prediction_map <- ifel(AOA$AOA == 1, prediction, NA)

# Plotting with custom legend for AOA
pred_aoa_le<-tm_shape(final_prediction_map) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = TRUE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 0.5, title = "", legend.show = FALSE) +  # No legend for AOA grey areas
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75) +
  tm_add_legend(type = "fill", labels = c("AOA = 0"), col = "grey", alpha = 0.5)  # Custom legend entry

pred_aoa_le
first_band_le <- sen_le[[1]]
first_band_plot_le <- tm_shape(first_band_le) +
  tm_raster(title = "First Band - Uppsala", palette = "viridis") +
  tm_layout(main.title = "First Band - Uppsala")


tmap_arrange(pred_le,pred_aoa_le,first_band_plot_le,ncol=2)


# Predict using the Florence raster
prediction_es <- predict(sen_es, model, na.rm=TRUE)
cols <- c("sandybrown", "green", "darkred", "blue", "forestgreen", "lightgreen", "red")

# Plot the prediction for Uppsala using tmap
pred_es<-tm_shape(prediction_es) +
  tm_raster(palette = cols, title = "Land Use Classification") +
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75)
pred_es

# Calculate AOA for Florence
AOA_es <- aoa(sen_es, model, useWeight = FALSE)

# Create a mask for areas where AOA = 1 (valid prediction areas)
prediction_aoa <- mask(prediction_es, AOA_es$AOA == 1)

# Create a mask for areas where AOA = 0 (grey areas)
aoa_grey <- ifel(AOA_es$AOA == 0, 1, NA)  # Areas with AOA = 0 will be grey

# Create a final raster to display predictions only where AOA = 1
final_prediction_map <- ifel(AOA_es$AOA == 1, prediction_es, NA)

# Plotting with custom legend for AOA
pred_aoa_es<-tm_shape(final_prediction_map) +
  tm_raster(palette = cols, title = "Land Use Classification (AOA = 1)", legend.show = TRUE) +
  tm_shape(aoa_grey) +
  tm_raster(palette = "grey", alpha = 0.5, title = "", legend.show = FALSE) +  # No legend for AOA grey areas
  tm_scale_bar(bg.color = "white", bg.alpha = 0.75) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.75) +
  tm_add_legend(type = "fill", labels = c("AOA = 0"), col = "grey", alpha = 0.5)  # Custom legend entry


pred_aoa_es
first_band_es <- sen_es[[1]]
first_band_plot_es <- tm_shape(first_band_es) +
  tm_raster(title = "First Band - Florence", palette = "viridis") +
  tm_layout(main.title = "First Band - Florence")


tmap_arrange(pred_es,pred_aoa_es,first_band_plot_es,ncol=2)


