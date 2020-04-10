#RCA + KNN implemented
#Accuracy : 1
library("caret")

####update the file path####
wine <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\wine.data", header=F, sep=",")
k <- 100L
x <- wine[,c(2:14)]
y <- wine[,c(1)]

#computing chunks
chunks <- vector("list", 178)
for (i in 1:59) chunks[[i]] <- sample(1L:59L, 10L)
for (i in 60:130) chunks[[i]] <- sample(60L:130L, 10L)
for (i in 131:178) chunks[[i]] <- sample(131L:178L, 10L)
chks <- x[unlist(chunks), ]

chunksvec <- rep(-1L, nrow(x))
for (i in 1L:length(chunks)) {
  for (j in 1L:length(chunks[[i]])) {
    chunksvec[chunks[[i]][j]] <- i
  }
}

rcs <- rca(x, chunksvec, 2)

xnew <- rcs$newX
xnew <- cbind(V1 = as.character(wine$V1), xnew)

############ visualizing the reduced dimensions
plot(xnew[, 2L], xnew[, 3L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[y],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)
##########

smp_size <- floor(0.75 * nrow(xnew))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(xnew)), size = smp_size)

train <- xnew[train_ind, ]
test <- xnew[-train_ind, ]
x_train <- train[,c(2:3)]
y_train <- train[,c(1)]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#training the train data using knn algorithm
knn_fit <- train(x_train, y_train, method = "knn",tuneLength = 10,trControl=trctrl)
#predicting on the test data
test_pred <- predict(knn_fit, newdata = test)
test_transformed <- as.data.frame(test) 

#computing accuracy
confusionMatrix(test_pred,test_transformed$V1)