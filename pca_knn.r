## PCA+KNN implemented
## Accuracy : 0.9556
#install.packages("caret")

library("caret")

####update the file path####
wine <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\wine.data", header=F, sep=",")
k = 100L
#removing categorical column
data <- wine[,c(2:14)]
############ visualizing reduced dimentions
visualize <- prcomp(data, scale = TRUE)
visualize <- visualize$x[,1:2]
visualize <- cbind(V1 = as.character(wine$V1), visualize)
plot(visualize[, 2L], visualize[, 3L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[strtoi(visualize[,1L])],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)

##############

##splitting dataset into test and train
smp_size <- floor(0.75 * nrow(wine))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(wine)), size = smp_size)

train <- wine[train_ind, ]
test <- wine[-train_ind, ]
train_data <- train[,c(2:14)]
test_data <- test[,c(2:14)]

#dimensionality reduction through PCA on test and train data
pca_train <- prcomp(train_data, scale = TRUE)
newdat_train <- pca_train$x[ ,1:8]
newdat_train <- cbind(V1 = as.character(train$V1), newdat_train)

pca_test <- prcomp(test_data, scale = TRUE)
newdat_test <- pca_test$x[ ,1:8]
newdat_test <- cbind(V1 = as.character(test$V1), newdat_test)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
t <- newdat_train[,c(2:9)]
train_classes <- newdat_train[,c(1)]

#training the train data using knn algorithm
knn_fit <- train(t, train_classes, method = "knn",tuneLength = 10,trControl=trctrl)
#predicting on the test data
test_pred <- predict(knn_fit, newdata = newdat_test)
newdat_test <- as.data.frame(newdat_test)
#getting accuracy
confusionMatrix(test_pred, newdat_test$V1)


