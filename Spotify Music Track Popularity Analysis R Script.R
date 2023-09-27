# set WD
setwd ("/Users/zhakaeric-m1air/Downloads")
#setwd ("/Users/branata.kurniawan/Documents/personal/ICT513/Group Project")

# library
library(tidyverse)
library(readxl)
library(readr)
library(psych)
library(ggplot2)
library(scales)
library(caret)
library(corrplot)
library(randomForest)

# import data
data <- read.csv("spotify_data.csv")

# filter data for last year 2022 (subset due to device limtation)
data <- subset(data, year == 2022)

# random sampling
# Randomly select a subset of 10% of data points (subset due to device limtation)
set.seed(1000) # for reproducibility
data.indices <- createDataPartition(data$popularity,p = 0.1, list = FALSE)
data <- data[data.indices, ]

# check for missing data
any(is.na(data))

# data summary statistics
glimpse(data)
summary(data)
describe(data)

# Correlation for numeric features

data.cor <- cor(cbind(
  popularity = data$popularity,
  #year = data$year, # filtered
  danceability = data$danceability,
  energy = data$energy,
  key = data$key,
  loudness = data$loudness,
  mode = data$mode,
  speechiness = data$speechiness,
  acousticness = data$acoustiness,
  instrumentalness = data$instrumentalness,
  liveness = data$liveness,
  valence = data$valence,
  tempo = data$tempo,
  duration_ms = data$duration_ms, 
  time_signature = data$time_signature
))

corrplot(data.cor, method = "square", type = "upper", diag = FALSE, addCoef.col = "black", tl.cex = 0.8)
title("Correlation Plot")


# Identify highly correlated positive pairs
positive_corr <- which(data.cor >= 0.5 & data.cor < 1, arr.ind = TRUE)
highly_correlated_positive_pairs <- data.frame(Variable1 = rownames(data.cor)[positive_corr[, 1]],
                                               Variable2 = colnames(data.cor)[positive_corr[, 2]],
                                               Correlation = data.cor[positive_corr])
print("Highly Correlated Pairs: \n")
print(highly_correlated_positive_pairs)

# Identify highly correlated negative pairs
negative_corr <- which(data.cor <= -0.5 & data.cor > -1, arr.ind = TRUE)
highly_correlated_negative_pairs <- data.frame(Variable1 = rownames(data.cor)[negative_corr[, 1]],
                                               Variable2 = colnames(data.cor)[negative_corr[, 2]],
                                               Correlation = data.cor[negative_corr])
print("\nHighly Correlated Negative Pairs: \n")
print(highly_correlated_negative_pairs)  

################################

# Create a new column 'verdict' in the dataframe and set it to 'low' or 'popular' based on popularity score
data$verdict <- ifelse(data$popularity >= 50, "popular", "low")


# EDA 
#1. Artist Name // Bar Chart
  # Get the table of frequencies
  artist_freq <- table(data$artist_name)

  # Sort the table in descending order based on frequency
  sorted_artist_freq <- sort(artist_freq, decreasing = TRUE)

  # Extract the top 10 rows
  top_10_artists <- head(sorted_artist_freq, n = 10)

  # Create the barplot for the top 10 artists
  barplot(top_10_artists, xlab = "Artist Name", ylab = "Frequency", 
        main = "Top 10 Most Song Artists", col = "skyblue", las = 2)

#2a. Top 10 Songs // Bar Chart
  data$popularity_score <- as.numeric(data$popularity)
  
  # Sort the data frame in descending order based on popularity_score
  sorted_songs_data <- data[order(-data$popularity_score), ]
  
  # Select the top 10 songs based on popularity score
  top_10_songs <- head(sorted_songs_data, 10)
  
  # Plot a bar chart to visualize the top 10 songs
  barplot(top_10_songs$popularity_score, names.arg = top_10_songs$track_name,
          xlab = "Song Name", ylab = "Popularity Score", main = "Top 10 Songs by Popularity Score",
          col = "blue", border = "black", las = 2)

  
#2b. Popularity // Continuous  
  hist(data$popularity, xlab = "Popularity", 
       ylab = "Frequency", main = "Histogram of Popularity")
  
  boxplot(data$popularity, ylab = "Popularity", main = "Boxplot of Popularity")
  median_popularity <- median(data$popularity)
  first_quartile <- quantile(data$popularity, 0.25)
  third_quartile <- quantile(data$popularity, 0.75)
  # Add data labels as text annotations
  text(1, median_popularity, paste("Median:", median_popularity), pos = 3, offset = 1)
  text(1, first_quartile, paste("1st Quartile:", first_quartile), pos = 3, offset = 1)
  text(1, third_quartile, paste("3rd Quartile:", third_quartile), pos = 3, offset = 1)


#3. Year - removed due to subset
#  barplot(table(data$year), xlab = "Year", 
#          ylab = "Frequency", main = "Histogram of Year")
  
#4. Top 10 Genre by count // Bar Chart
  # Get the table of frequencies
  genre_freq <- table(data$genre)
  
  # Sort the table in descending order based on frequency
  sorted_genre_freq <- sort(genre_freq, decreasing = TRUE)
  
  # Extract the top 10 rows
  top_10_genre <- head(sorted_genre_freq, n = 10)
  
  # Create the barplot for the top 10 artists
  barplot(top_10_genre, xlab = "Genre", ylab = "Frequency", 
          main = "Top 10 Genre", col = "skyblue", las = 2)
  
#5. Danceability // Continuous 
  hist(data$danceability, xlab = "Danceability", 
       ylab = "Frequency", main = "Histogram of Danceability")
  boxplot(data$danceability, ylab = "Danceability", main = "Boxplot of Danceability")
  
#6. Energy // Continuous  
  hist(data$energy, xlab = "Energy", 
       ylab = "Frequency", main = "Histogram of Energy")
  boxplot(data$energy, ylab = "Energy", main = "Boxplot of Energy")
  
#7. Key // Continuous 
  hist(data$key, xlab = "Key", 
       ylab = "Frequency", main = "Histogram of Key")
  boxplot(data$key, ylab = "Key", main = "Boxplot of Key")
  
#8. Loudness // Continuous
  hist(data$loudness, xlab = "Loudness", 
       ylab = "Frequency", main = "Histogram of Loudness")
  boxplot(data$loudness, ylab = "Loudness", main = "Boxplot of Loudness")
  

#9. Mode
  ggplot(data, aes(x = mode)) +
    geom_bar() +
    xlab("Mode") +
    ylab("Count") +
    ggtitle("Bar Chart for Mode") +
    scale_y_continuous(labels = comma) 

#10. Speechiness // Continuous 
  hist(data$speechiness, xlab = "Speechiness", 
       ylab = "Frequency", main = "Histogram of Speechiness")
  boxplot(data$speechiness, ylab = "Speechiness", main = "Boxplot of Speechiness")
  

#11. Acousticsness // Continuous
  hist(data$acousticness, xlab = "Acoutsticness", 
       ylab = "Frequency", main = "Histogram of Acousticness")
  boxplot(data$acousticness, ylab = "Acousticness", main = "Boxplot of Acousticness")
  
#12. Instrumentalness // Continuous 
  hist(data$instrumentalness, xlab = "Instrumentalness", 
       ylab = "Frequency", main = "Histogram of Instrumentalness")
  boxplot(data$instrumentalness, ylab = "Instrumentalness", main = "Boxplot of Instrumentalness")

#13. Liveness // Continuous 
  hist(data$liveness, xlab = "Liveness", 
       ylab = "Frequency", main = "Histogram of Liveness")
  boxplot(data$liveness, ylab = "Liveness", main = "Boxplot of Liveness")
  
#14. Valence // Continuous  
  hist(data$valence, xlab = "Valence", 
       ylab = "Frequency", main = "Histogram of Valence")
  boxplot(data$valence, ylab = "Valence", main = "Boxplot of Valence")
  
#15. Tempo // Continuous
  hist(data$tempo, xlab = "Tempo", 
       ylab = "Frequency", main = "Histogram of Tempo")
  boxplot(data$tempo, ylab = "Tempo", main = "Boxplot of Tempo")
  

#16. Duration_ms // Continuous 
  hist(data$duration_ms, xlab = "Duration (ms)", 
       ylab = "Frequency", main = "Histogram of Duration (ms)")
  boxplot(data$duration_ms, ylab = "Duration (ms)", main = "Boxplot of Duration (ms)")
  
  
#17. time_signature // Continuous 
  hist(data$time_signature, xlab = "Time Signature", 
       ylab = "Frequency", main = "Histogram of Time Signature")
  boxplot(data$time_signature, ylab = "Time Signature", main = "Boxplot of Time Signature")

  
  
#### Interesting Findings 

#18. Verdict Distribution Bar Chart 
  
  # Create a summary of "verdict" by count
  verdict_counts <- table(data$verdict)
  
  # Convert the table to a data frame
  verdict_df <- as.data.frame(verdict_counts)
  
  # Rename the columns for clarity
  colnames(verdict_df) <- c("Verdict", "Count")
  
  # Create the bar chart
  ggplot(verdict_df, aes(x = Verdict, y = Count)) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Add labels to the bars
    xlab("Verdict") +
    ylab("Count") +
    ggtitle("Verdict Counts") +
    theme_minimal()

#19. Genre Bar Chart with Verdict - High
  # Sort the data by popularity in descending order and keep only the top 10 genres with high popularity
  top10_high_popularity <- data[order(-data$popularity), ][1:10, ]
  
  # Create the stacked bar chart
  ggplot(top10_high_popularity, aes(x = genre, y = popularity, fill = verdict)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 Genres with High Popularity",
         x = "Genre", y = "Popularity Score") +
    theme_minimal()

#20. Danceability by Popularity Scatter plot - verdict
  
# Create the scatter plot
  ggplot(data, aes(x = popularity, y = danceability, color = verdict)) +
    geom_point() +
    labs(title = "Scatter Plot of Popularity vs Danceability",
         x = "Popularity", y = "Danceability") +
    theme_minimal()
  
#21. Speechiness by Loudness scatter plot - verdict color
  # Create the scatter plot
  ggplot(data, aes(x = speechiness, y = loudness, color = verdict)) +
    geom_point() +
    labs(title = "Scatter Plot of Speechiness vs Loudness",
         x = "Speechiness", y = "Loudness") +
    theme_minimal()
  
  
#22. Liveness barchart - Verdict coloured 
  # Calculate the average liveness for each verdict
  avg_liveness <- aggregate(data$liveness, by = list(data$verdict), FUN = mean)
  
  # Create the bar chart
  ggplot(avg_liveness, aes(x = Group.1, y = x, fill = Group.1)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Liveness by Verdict",
         x = "Verdict", y = "Average Liveness") +
    theme_minimal()

#23. Duration_ms split by Verdict 
  # Calculate the average duration_ms for each verdict
  avg_duration <- aggregate(data$duration_ms, by = list(data$verdict), FUN = mean)
  
  # Create the bar chart
  ggplot(avg_duration, aes(x = Group.1, y = x, fill = Group.1)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Duration by Verdict",
         x = "Verdict", y = "Average Duration (ms)") +
    theme_minimal()

# Log-transform multiple columns
#data$tempo <- log(data$tempo)
#data$duration_ms <- log(data$duration_ms)
#data$time_signature <- log(data$time_signature)

################################

# Train-Test Split
set.seed(100)

performance <- c("Accuracy", "95% CI", "Kappa", "F1", "MSE", "MAE", "Precision Rate", "Recall Rate")

index <- sample(nrow(data), nrow(data)*0.8)
train_data <- data[index,]
test_data <- data[-index,]

###############################

## Part 1 

# song popularity - logistic regression model

set.seed(101)

# Train the model using N-Fold Cross Validation
control <- trainControl(method="cv", number=5)
lr <- train(verdict~danceability + loudness + speechiness + instrumentalness + liveness + duration_ms, data=train_data, method="glm", trControl=control)

# Test the model
lrPredResult <- predict(lr,newdata=test_data)
lrPredResult %>% head

# Retrieve the factor levels from test_data$verdict
verdict_levels <- levels(factor(test_data$verdict))

# Set the factor levels for lrPredResult
lrPredResult <- factor(lrPredResult, levels = verdict_levels)

# Create confusion matrix
lrPredResultDf <- confusionMatrix(factor(test_data$verdict), lrPredResult)

ci <- paste('(', round(lrPredResultDf$overall["AccuracyLower"], 4), ", ", round(lrPredResultDf$overall["AccuracyUpper"], 4), ")")
mse <- mean((lrPredResult - test_data$verdict)^2)
mae <- mean(abs(lrPredResult - test_data$verdict))

lrPerformanceResult <- c(
  round(lrPredResultDf$overall["Accuracy"], 4), 
  ci, 
  round(lrPredResultDf$overall["Kappa"], 4), 
  round(lrPredResultDf$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(lrPredResultDf$byClass["Precision"], 4), 
  round(lrPredResultDf$byClass["Recall"], 4)
)

#==============
# song popularity - random forest model

set.seed(101)

# Random Forest - 500
# Train the model using N-Fold Cross Validation
control <- trainControl(method = "cv", number = 5)
rf <- train(verdict~danceability + loudness + speechiness + instrumentalness + liveness + duration_ms, data=train_data, method="rf", trControl=control, tuneLength=3, ntree=500, type='classification')

# Test the model
rfPredResult <- predict(rf,newdata=test_data)
rfPredResult %>% head

rfPredResultDf <- confusionMatrix(factor(test_data$verdict), factor(rfPredResult))

ci <- paste('(', round(rfPredResultDf$overall["AccuracyLower"], 4), ", ", round(rfPredResultDf$overall["AccuracyUpper"], 4), ")")
mse <- mean((rfPredResult - test_data$verdict)^2)
mae <- mean(abs(rfPredResult - test_data$verdict))

rfPerformanceResult <- c(
  round(rfPredResultDf$overall["Accuracy"], 4), 
  ci, 
  round(rfPredResultDf$overall["Kappa"], 4), 
  round(rfPredResultDf$byClass["F1"], 4), 
  round(mse, 4),
  round(mae, 4), 
  round(rfPredResultDf$byClass["Precision"], 4), 
  round(rfPredResultDf$byClass["Recall"], 4)
)

plot(varImp(rf, scale=F), main = "Important Variables: RF 500 5 Fold CV")
plot(varImp(rf, scale=F), top = 5, main = "Top 5 Important Variables: RF 500 5 Fold CV")

  
# evaluation 
finalPerformance <- data.frame(
  Performance=performance,
  LR=lrPerformanceResult,
  RF500=rfPerformanceResult
)
###################################
## Part 2 PCA

# Scaling of data
data.cleaned <- select(data, -artist_name, -track_name, -track_id, -popularity, -genre, -verdict)
data.scaled <- as.data.frame(scale(data.cleaned))
data.scaled <- select(data, -artist_name, -track_name, -track_id, -popularity, -genre)
colnames(data.scaled) <- colnames(data.cleaned)

# PCA
data.pca <- prcomp(~danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+duration_ms+time_signature, 
                          data = data, 
                          scale = TRUE)

data.pca$x
data.pca$rotation
(data.pca$sdev)^2 # Note: Eigenvalues

biplot(data.pca, choices = c(1, 2))
biplot(data.pca, choices = c(1, 3))
biplot(data.pca, choices = c(2, 3))

ggbiplot::ggbiplot(data.pca, choices = 1:2)

plot(((data.pca$sdev)^2 / sum((data.pca$sdev)^2)*100), 
     type = "b",
     xaxt = "n",
     xlab = "PCs",
     ylab = "Percentage of Total Variance",
     main = "Screeplot of Variance Accounted for by the PCs")
axis(side = 1, at = 1:13, 
     labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13"))

loadings_pc1 <- data.pca$rotation[, 1]
loadings_pc2 <- data.pca$rotation[, 2]
loadings_pc3 <- data.pca$rotation[, 3]

# PC1 
# Calculate squared loadings
#squared_loadings_pc1 <- loadings_pc1^2
# Calculate percentage contributions
#percentage_contributions_pc1 <- (squared_loadings_pc1 / sum(squared_loadings_pc1)) * 100
# Extract percentage contributions of the first five variables
#percentage_contributions_first_five <- percentage_contributions_pc1[1:13]
#percentage_contributions_first_five


# Sort the variables by values
#sorted_indices <- order(percentage_contributions_first_five, decreasing = TRUE)
#sorted_data <- percentage_contributions_first_five[sorted_indices]

# Select only the top 5 variables
#top_5_data <- sorted_data[1:5]

# Create a bar chart with the selected data
#barplot(top_5_data, xlab = "Variables", ylab = "Percentage Contribution",
#        main = "Percentage Contributions of Top 5 Variables", col = "blue")

# Add data labels to the bars
#text(x = 1:length(sorted_data), y = sorted_data, labels = round(sorted_data, 2), 
#     pos = 1, cex = 0.7, col = "black")

####################################
## Part 3 - Hierarchical Clustering // NO MEMORY 

# Assuming you have selected certain variables and stored them in a data frame called "selected_data"
# You can specify the columns you want to use for clustering
selected_variables <- data[, c("loudness", "energy", "acousticness","instrumentalness","valence")]

# Calculate the distance matrix using Euclidean distance
distance_matrix <- dist(selected_variables, method = "euclidean")

# Perform hierarchical clustering using complete linkage
hierarchical_cluster <- hclust(distance_matrix, method = "complete")

# Plot the dendrogram
plot(hierarchical_cluster, main = "Hierarchical Clustering Dendrogram", xlab = "Observations", ylab = "Distance")
rect.hclust(hierarchical_cluster, k = 3, border = 1)

# Cut the dendrogram to form clusters
num_clusters <- 3  # Specify the number of clusters you want
clusters <- cutree(hierarchical_cluster, k = num_clusters)

# Add cluster information to the selected data frame
selected_variables$cluster <- clusters

# Create confusion matrix
table(clusters, data$verdict)

# Extract features from each cluster
cluster1_data <- data[selected_variables$cluster == 1, ]
cluster2_data <- data[selected_variables$cluster == 2, ]
cluster3_data <- data[selected_variables$cluster == 3, ]


# Create a data frame with the original data and cluster assignment
data_with_clusters <- cbind(data, cluster = clusters)

# Calculate cluster profiles
cluster_profiles <- aggregate(data_with_clusters[, -ncol(data_with_clusters)],
                              by = list(cluster = data_with_clusters$cluster),
                              FUN = mean)  
## Note: can use other summary statistics like median if needed


