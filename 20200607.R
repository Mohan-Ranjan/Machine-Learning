# Make Eniroment Clean
remove(list=ls())

#Load Library to reading excel file
library(readxl)

#Input the data set
df<- read_excel("/Users/shanmugaratnammohanaranjan/Desktop/Machine Learning/vehicles.xlsx") 

# View the data
View(df)

#creating a variable without sample numbers column and class column 
sample = dplyr::select(df, 2:19)
sample

# Calculate the z-score for each attribute
z_scores <- apply(sample, 2, function(x) abs((x - mean(x))/sd(x)))
z_scores

# Set a threshold for z-score 
z_threshold <- 3
z_threshold

# Identify and remove outliers with z-score greater than the threshold 
clean_data <- sample[rowSums(z_scores > z_threshold) == 0,]
clean_data

# Scale the cleaned data set 
scale_data <- scale(clean_data)
scale_data

#creating a data frame after scaling the data set 
clustering <- as.data.frame(scale_data)
clustering

#NBClust 

#installing and loading nbclust package 
# install.packages("NbClust")

# Load NbClust
library(NbClust)

#Rundom number creator for working with random numbers
set.seed(1234)

#assign value to nb
nb <- NbClust(scale_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
# Print
cat("optimal number of clusters by nb:", nb$Best.nc, "\n")

#Elbow 
set.seed(1234)
sse <- numeric(10) 

for(k in 2:10){
  kmeans_out <- kmeans(scale_data, centers = k, nstart = 25)
  sse[k] <- kmeans_out$tot.withinss 
}

# Plot the sum of squared errors (SSE) for a range of cluster sizes 
plot(2:10, sse[2:10], type = "b", main = "Elbow Method to Calculate the Cluster Number")

# Load the cluster library, which contains the clusGap function
library(cluster)

# Set the seed for reproducibility
set.seed(1234)

# Calculate the gap statistic for a range of cluster sizes (from 1 to 10 in this case)
# using the k-means algorithm with 25 random starts per cluster size
gap_stat <- clusGap(scale_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50) 

# Plot the gap statistic results to determine the number of clusters
plot(gap_stat, main = "Gap Statistic to Calculate  Cluster Number")

#Silhouette Method 
# Load the cluster library
library(cluster) 
set.seed(1234)

# Create an empty vector to store the average silhouette widths
sil_width <- numeric(10) 

#calculate the average silhouette width
for(k in 2:10){
  kmeans_out <- kmeans(scale_data, centers = k, nstart = 25) 
  sil_obj <- silhouette(kmeans_out$cluster, dist(scale_data)) 
  sil_width[k] <- mean(sil_obj[,3])
}

# Plot the average silhouette width for each cluster size
plot(2:10, sil_width[2:10], type = "b", main = "Silhouette Method to Determine Cluster Number")

#Assuming k is 3
set.seed(1234)
# The resulting cluster assignments are stored in the kmeans_out object
kmeans_out <- kmeans(scale_data, centers = 3, nstart = 25)

# Cluster centers
centers <- kmeans_out$centers
# Print results 
print(centers)

# Cluster assignments
clusters <- kmeans_out$cluster
# Print results 
cat("Cluster centers:\n")
print(clusters)

# TSS
TSS <- sum(rowSums(scale_data)^2)
# Print results 
cat("\nTotal sum of squares (TSS):", TSS, "\n") 

# BSS
BSS <- sum(kmeans_out$withinss)
# Print results 
cat("\nBetween-cluster sum of squares (BSS):", BSS, "\n")

# Ratio
bss_tss__ratio <- BSS / TSS
# Print results 
cat("\nRatio between BSS & TSS:", bss_tss__ratio, "\n") 

# WSS
WSS <- sum(kmeans_out$withinss)
# Print results 
cat("\nWithin-cluster sum of squares (WSS):", WSS, "\n")

# WSS Plot function
# generates a plot of within-groups sum of squares (WSS) for different numbers of clusters
wss_plot <- function(scale_data, nc=15, seed=1234) 
  {
    wss <- (nrow(scale_data)-1)*sum(apply(scale_data,2,var)) 
    for (i in 2:nc) {
      set.seed(seed)
      wss[i] <- sum(kmeans(scale_data, centers = i)$withinss)
    } 
    # plot of the WSS for each number of clusters
    plot(1:nc, wss, type="b", xlab="Number of clusters",
    ylab="within groups sum of squares") 
}

wss_plot(scale_data)
KM = kmeans(scale_data,3) # KM object

# Install the package
#install.packages("ggfortify")  

# Load the package 
library(ggfortify) 
library(factoextra)

# Method 1
autoplot(KM,scale_data,frame=TRUE)

# Method 2
fviz_cluster(kmeans_out, data = scale_data,
             palette = c("#B4EEB4", "#0FF7F0", "#E34800", "#bB00e7"),
             geom = "point", ellipse.type = "convex", ggtheme = theme_bw() )

#Silhouette Plot Average
library(cluster)
set.seed(123)
kmeans_out <- kmeans(scale_data, centers = 3, nstart = 25)

library(factoextra)
# Calculate average silhouette width
sil_obj <- silhouette(kmeans_out$cluster, dist(scale_data)) 
sil_avg <- mean(sil_obj[,3])
sil_avg

# Plot silhouette plot with color palette
fviz_silhouette(sil_obj, palette = "Set2", main = paste0("Silhouette Plot (Avg. Silhouette Width: ", round(sil_avg, 2), ")"))

# Perform PCA analysis
pca <- prcomp(scale_data, center = TRUE, scale. = TRUE)
# View the results 
summary(pca)

# Get the eigenvalues and eigenvectors 
eigenvalues <- pca$sdev^2 
eigenvalues
eigenvectors <- pca$rotation
eigenvectors
# Calculate the cumulative score per principal component 
per_var <- eigenvalues/sum(eigenvalues) 
cumu_per_var <- cumsum(per_var)

# Print the cumulative proportion of variance explained by each principal component 
cat("Cumulative proportion of variance explained by each PC:\n") 
print(cumu_per_var)
# Create a new dataset with principal components as attributes 
pca_data <- as.data.frame(pca$x)
# Choose PCs that provide at least cumulative score > 92% 
chosed_pcs <- which(cumu_per_var > 0.92)
# Print the chosen PCs
cat("Chosen PCs:", chosed_pcs, "\n")
# Subset the transformed dataset to include only the chosen PCs 
transform_data <- pca_data[, chosed_pcs]

#Looad Libraries
library(NbClust) 
library(cluster) 
library(factoextra)

set.seed(1234)

nb <- NbClust(transform_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
cat("optimal number of clusters by nb:", nb$Best.nc, "\n")

# Elbow Method
fviz_nbclust(transform_data, kmeans, method = "wss")

# Run gap statistic method
# Method 1
fviz_nbclust(transform_data, kmeans, method = 'gap_stat')
# Method 2
gap_stat <- clusGap(transform_data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50) 
fviz_gap_stat(gap_stat)

# Silhoute Method
fviz_nbclust(transform_data, kmeans, method = 'silhouette') 

# Perform k-means clustering on transformed data with k=2
kmeans_output <- kmeans(transform_data, 2)

# Print the k-means output 
kmeans_output
# Print the centers of each cluster 
kmeans_output$centers
# Print the cluster assignments for each observation 
kmeans_output$cluster

#Between_cluster_sums_of_squares (BSS) 
BSS <- sum(kmeans_output$size * dist(kmeans_output$centers)^2)
# (TSS) Total_sum_of_Squares
TSS <- sum(dist(transform_data)^2)
# Calculate the ratio of BSS and TSS
BSS_TSS_ratio <- BSS / TSS 
cat("BSS/TSS ratio:", BSS_TSS_ratio, "\n")
#ploting new clustures
newKM = kmeans(transform_data,2)

#install.packages("ggfortify")
# Install the package 

library(ggfortify) # Load the package 
autoplot(newKM,transform_data,frame=TRUE)

# Create the silhouette plot
library(factoextra)
library(cluster)

sil_widths <- silhouette(kmeans_output$cluster, dist(transform_data)) 
avg_sil_width <- mean(sil_widths[, 2])
fviz_silhouette(sil_widths) + ggtitle(paste0("Silhouette Plot (Avg. Width = ", round(avg_sil_width, 2), ")"))

# Compute the between-cluster sum of squares (BSS)
bssCH <- sum(kmeans_output$size * dist(kmeans_output$centers)^2)
bssCH
# Compute the within-cluster sum of squares (WSS) 
wssCH <- sum(kmeans_output$withinss)
wssCH
# Compute the total sum of squares (TSS) 
tssCH <- bssCH + wssCH
tssCH
# Compute the number of clusters 
new_nclusters <- 2
new_nclusters

# Compute the Calinski-Harabasz index
# Assume kCH = 10
kCH <- 10
ch_index <- (bssCH / (new_nclusters - 1)) / (wssCH / (nrow(transform_data) - kCH))
# Print the result
cat("Calinski-Harabasz Index:", ch_index, "\n")

#importing libraries 
library(readxl) 
library(neuralnet) 
library(ggplot2) 
library(readxl)
#Load dataset
uow_consumption <- read_excel("/Users/shanmugaratnammohanaranjan/Desktop/Machine Learning/uow_consumption.xlsx") 
View(uow_consumption)

# Rename columns
colnames(uow_consumption) <- c("date", "hour_18", "hour_19", "hour_20")

# Import required library
library(MASS)
library(dplyr)
uow_consumption <- uow_consumption %>% mutate(
  map_1 = lag(hour_20, 1), 
  map_2 = lag(hour_20, 2),
  map_3 = lag(hour_20, 3),
  map_4 = lag(hour_20, 4), 
  map_7 = lag(hour_20, 7)
) %>% 
  na.omit()

#rnorm(uow_consumption)

#splitting dataset into to parts as instructed
#Train data
training_data <- head(uow_consumption, 380)
training_data
#Test data
testing_data <- tail(uow_consumption, nrow(uow_consumption) - 380)
testing_data

# Training of Normalized data
train_normalized <- as.data.frame(scale(training_data[-1])) 
train_normalized

# Testing of Normalized Data
test_normalized <- as.data.frame(scale(testing_data[-1]))
test_normalized
# Set the column names of the test_normalized data frame 
colnames(test_normalized) <- colnames(train_normalized)

# Install Reshap2
#install.packages("reshape2") 

# Load Library
library(reshape2) 
library(ggplot2)

# Define color palettes for the plots
# Train Data After Normalized
train_palette <- c("#E00F00", "#5344E9", "#080E73", "#F0C942", "#0B32B2", "#D09E00", "#CC79A7", "#00BFFF")
# Print Colours
train_palette
# Test Data After Normalized
test_palette <- c("#007D02", "#D89E00", "#CBB9A7", "#F09F00", "#23DEE9", "#008773", "#F00042", "#00BFFF")
# Print Colours
test_palette

# Display training normalized data with color
ggplot(melt(train_normalized), aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot() + ggtitle("Train Normalized Data") + xlab("Variables") +
  ylab("Normalized Values") + scale_fill_manual(values = train_palette)

# Display testing normalized data with color
ggplot(melt(test_normalized), aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  ggtitle("Test Normalized Data") + xlab("Variables") +
  ylab("Normalized Values") + scale_fill_manual(values = test_palette)

# Define a list of input vectors for different combinations of maps
input_vectors <- list(
c("map_1"),
c("map_1", "map_2"),
c("map_1", "map_2", "map_3"),
c("map_1", "map_2", "map_3", "map_4"), c("map_1", "map_7"),
c("map_1", "map_2", "map_7"),
c("map_1", "map_2", "map_3", "map_7"), c("map_1", "map_2", "map_3", "map_4", "map_7")
)

# Load Neuralnet
library(neuralnet) 

# a multilayer perceptron (MLP) neural network model using the neuralnet package
mlp_mod <- function(train_data, test_data, input_vars, hidden_structure) { 
  # Define the formula for the MLP model using the input variables and the target variable of hour_20
  formula <- paste("hour_20 ~", paste(input_vars, collapse = " + "))
  # Fit the MLP model using the neuralnet function with the training data and the specified hidden layer structure
  nn <- neuralnet(as.formula(formula), train_data, hidden = hidden_structure) 
  # Prepare the test data by extracting the input variables and renaming the columns to match the training data
  test_matrix <- as.matrix(test_data[, input_vars, drop = FALSE]) 
  colnames(test_matrix) <- colnames(train_data[, input_vars, drop = FALSE]) 
  # Use the fitted MLP model to predict the hour_20 values for the test data
  predictions <- predict(nn, test_matrix)
  # Return a list containing the fitted model and the predicted values for the test data
  return(list(model = nn, predictions = predictions)) 
}

# Install Neuralnet
# install.packages("neuralnet") 

# Load Neuralnet
library(neuralnet)

# Initialize an empty list to store the trained models
models <- list()
# Train a separate model for each input vector and store it in the models list
for (i in 1:length(input_vectors)) {
  models[[i]] <- mlp_mod(train_normalized, 
                         test_normalized, input_vectors[[i]], c(5)) 
}

# Define a function to calculate evaluation metrics for a given set of actual and predicted values
metrics_calcu <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  smape <- mean(abs(actual - predicted) / (abs(actual) + abs(predicted)) * 2) * 100 
  return(list(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape))
}

# Initialize an empty list to store the evaluation metrics for each model
evaluation_metrics <- list()

# Calculate the evaluation metrics for each model and store them in the evaluation_metrics list
for (i in 1:length(models)) {
  evaluation_metrics[[i]] <- metrics_calcu(test_normalized$hour_20, models[[i]]$predictions) 
}
 
# Calculate the evaluation metrics for each model and store them in the evaluation_metrics list
comparison_table <- data.frame(
  Model_Description = c("AR(1)", "AR(2)", "AR(3)", "AR(4)", "AR(1,7)", "AR(2,7)",
                        "AR(3,7)", "AR(4,7)"),
  RMSE = sapply(evaluation_metrics, function(x) x$RMSE),
  MAE = sapply(evaluation_metrics, function(x) x$MAE), 
  MAPE = sapply(evaluation_metrics, function(x) x$MAPE), 
  sMAPE = sapply(evaluation_metrics, function(x) x$sMAPE)
) 

# Print
print(comparison_table)

# Efficiency comparison between one-hidden layer and two-hidden layer networks
model_1_hidden <- mlp_mod(train_normalized, test_normalized, c("map_1", "map_2", "map_3", "map_7"), c(5))
#model_1_hidden
model_2_hidden <- mlp_mod(train_normalized, test_normalized, c("map_1", "map_2", "map_3", "map_7"), c(3, 2))
#model_2_hidden

# Check the total number of weight parameters per network 
num_weights_1_hidden <- sum(sapply(model_1_hidden$model$weights, length)) 
num_weights_1_hidden
num_weights_2_hidden <- sum(sapply(model_2_hidden$model$weights, length))
num_weights_2_hidden

# Print 
cat("Total number of weight parameters for the one-hidden layer network:", 
    num_weights_1_hidden, "\n")
cat("Total number of weight parameters for the two-hidden layer network:", 
    num_weights_2_hidden, "\n")

# Method 2
# Applying Neural network concepts
# Load neuralnet package
library(neuralnet)

# Get column names of uow_consumption
n <- names(uow_consumption)

# Define formula for neural network input
f <- as.formula(paste("train_normalized ~",
                      paste(n[!n %in% "train_normalized"],
                            collapse = " + ")))

# Train neural network model with two hidden layers of 4 and 2 nodes, and linear output
nn <- neuralnet(train_normalized, data = uow_consumption,
                hidden = c(1, 2),
                linear.output = T)

# Plotting the graph
plot(nn)

# Add the 18th and 19th hour attributes to the input vectors 
narx_input_vectors <- list(
  c("map_1", "hour_18", "hour_19"), # First input vector
  c("map_1", "map_2", "hour_18", "hour_19"),
  c("map_1", "map_2", "map_3", "hour_18", "hour_19"),
  c("map_1", "map_2", "map_3", "map_7", "hour_18", "hour_19"), 
  c("map_1", "map_2", "map_3", "map_4", "map_7", "hour_18", "hour_19")  # Fifth input vector
)

# Build NARX models
narx_models <- list()
for (i in 1:length(narx_input_vectors)) {
  narx_models[[i]] <- mlp_mod(train_normalized, test_normalized, narx_input_vectors[[i]], c(5)) # Build the i-th NARX model
}

# Evaluate NARX models 
narx_evaluation_metrics <- list() 
for (i in 1:length(narx_models)) {
  narx_evaluation_metrics[[i]] <- metrics_calcu(test_normalized$hour_20, narx_models[[i]]$predictions) # Evaluate the i-th NARX model
}

# Create a comparison table for NARX models 
narx_comparison_table <- data.frame(
  # Model descriptions
Model_Description = c("NARX(1,18,19)", "NARX(2,18,19)", "NARX(3,18,19)", "NARX(3,7,18,19)", "NARX(4,7,18,19)"),
# Root Mean Squared Error
  RMSE = sapply(narx_evaluation_metrics, function(x) x$RMSE),   
# Mean Absolute Error
  MAE = sapply(narx_evaluation_metrics, function(x) x$MAE),  
# Mean Absolute Percentage Error
  MAPE = sapply(narx_evaluation_metrics, function(x) x$MAPE),   
# Symmetric Mean Absolute Percentage Error
  sMAPE = sapply(narx_evaluation_metrics, function(x) x$sMAPE))  

# Print the comparison table
print(narx_comparison_table)

# De-normalize the predictions
denormalize <- function(x, min_value, max_value) {
  return(x * (max_value - min_value) + min_value)
}

# Find the index of the best model based on the RMSE
best_model_index <- which.min(sapply(evaluation_metrics, function(x) x$RMSE))
#best_model_index

# Get the best model
best_model <- models[[best_model_index]]
#best_model

# Get the predictions of the best model
best_model_predictions <- best_model$predictions
#best_model_predictions

# Get the minimum value of the target variable in the training data
min_value <- min(training_data$hour_20) 
# Get the maximum value of the target variable in the training
max_value <- max(training_data$hour_20)

denormalized_predictions <- denormalize(best_model_predictions, min_value, max_value) 

# Plot the predicted output vs. desired output
# Create the plot with custom settings 
plot(testing_data$hour_20, type = "l", col = "#3399FF", lwd = 2,
  xlab = "Time", ylab = "Consumption (Hour 20)",
  # Add title to the plot
  main = "Comparison of Desired and Predicted Outputs",
  ylim = range(c(testing_data$hour_20, denormalized_predictions)), 
  cex.main = 1.2, cex.lab = 1.1, cex.axis = 0.9)
lines(denormalized_predictions, col = "#FF6666", lwd = 2)

# Add a legend with custom settings
legend("topright", legend = c("Desired Output", "Predicted Output"),
       col = c("#3399FF", "#FF6666"), lty = 1, lwd = 2, cex = 0.9,
       box.col = NA, bg = "white", inset = 0.02)
#################################    END      ##############################