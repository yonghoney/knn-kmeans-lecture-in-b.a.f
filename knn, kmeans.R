# install.packages(c("class", "factoextra", "cluster", "jpeg", "ggplot2"))
# library(class); library(factoextra); library(cluster); library(jpeg); library(ggplot2);

# knn
rm(list=ls())
# algorithm example plot
set.seed(25)
x1 = c(rnorm(15, 0.3, 0.3), rnorm(15, 0.7, 0.3))
x2 = runif(30, 0, 1)
y = c(rep(0, 15), rep(1, 15))

plot(x1, x2, col = ifelse(y == 0, "blue", "red"), pch = 16, cex = 2,
     xlim = c(min(x1) - 0.2, max(x1) + 0.2), ylim = c(0, 1))

index = which(x1 == max(x1[y == 0]))
points(x = x1[index] + 0.03, y = x2[index] - 0.03,
       col = "orange", cex = 2, pch = 16)
dev.off()

############# KNN #############
rm(list=ls())
library(class)
data(iris)

# train, test split
set.seed(2020)
allrows <- 1:nrow(iris)
trainrows <- sample(allrows, replace = F, size = 0.6 * length(allrows))
train_iris <- iris[trainrows, 1:4]
train_target <- iris[trainrows, 5]

test_iris <- iris[-trainrows, 1:4]
test_target <- iris[-trainrows, 5]
table(test_target)

# modeling
result_knn <- knn(train = train_iris, test = test_iris, cl = train_target, k = 3)

# result
table(test_target, result_knn)

# find best k
# overfitting을 확인하기 위해 train set 예측력도 확인
error.train <- rep(0, 10)
error.test <- rep(0, 10)

for(k in 1:10){
  pred_iris <- knn(train = train_iris, test = train_iris, cl = train_target, k)
  error.train[k]<- 1 - mean(pred_iris == train_target)
  pred_iris <- knn(train = train_iris, test = test_iris, cl = train_target, k)
  error.test[k]<- 1 - mean(pred_iris == test_target)
}

plot(error.train, type = "o", ylim = c(0,0.15),
     col = "blue", xlab = "K values", ylab = "Misclassification errors")
lines(error.test, type = "o", col="red")
legend("topright", legend = c("Training error","Test error"),
       col = c("blue","red"), lty = 1:1)
dev.off()

############# Kmeans #############
rm(list=ls())
# dataset 객체 할당 및 target 변수 분리
data(iris)
data_iris <- iris
data_iris <- data_iris[, -5]
target <- iris$Species

# n_cluster = 3으로 설정
set.seed(1234)
model_km <- kmeans(data_iris, 3, nstart = 5)
model_km

table(target, model_km$cluster)

plot(data_iris[, c("Sepal.Length", "Sepal.Width")],
     col = model_km$cluster, cex = 2, pch = 1)
points(model_km$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, cex = 3, pch = 17)
dev.off()

# elbow - method 1
set.seed(1234)
func_kmeans <- function(data, k){
  kmeans(data, k, nstart = 5)$tot.withinss
}

k.values <- 1:15
SSE <- rep(0, 15)
for(k in k.values){
  SSE[k] <- func_kmeans(data_iris, k)
}

plot(x = k.values, y = SSE, type = "b", pch = 19,
     xlab = "Number of Cluster K", ylab = "Total Within cluster sum of squares")
dev.off()

# elbow - method 2
library(factoextra)
fviz_nbclust(data_iris, kmeans, method = "wss")
dev.off()

# silhouette
library(cluster)
windows()
par(mfrow = c(2, 2))
for(k in 2:5){
  set.seed(1234)
  model_km <- kmeans(data_iris, k, nstart = 5)
  plot(silhouette(model_km$cluster, dist = dist(data_iris)))
}
dev.off()

# method 2
library(factoextra)
fviz_nbclust(data_iris, kmeans, method = "silhouette")
dev.off()

############# kmeans를 활용한 image 분석 #############
setwd("C:/Users/yongheon/Desktop/동국대학교/비어플/knn_kmeans")
# Load the package
library(jpeg)
library(ggplot2)

img <- readJPEG("ColorfulBird.jpg") # Read the image
str(img)
dim(img)
class(img)
img[100:105, 100:105, ]

# Obtain the dimension
imgDm <- dim(img)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

head(imgRGB)
rgb(head(imgRGB[c("R", "G", "B")]))
summary(imgRGB)

# Plot the image
windows()
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: Colorful Bird") +
  xlab("x") +
  ylab("y")
dev.off()

# clustering
windows()
kClusters <- 2
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
dev.off()

# clustering
windows()
kClusters <- 3
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
dev.off()

# clustering
windows()
kClusters <- 4
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
dev.off()

# clustering
windows()
kClusters <- 5
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
dev.off()

# clustering
windows()
kClusters <- 10
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
dev.off()

