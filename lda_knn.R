#LDA implemented
#Accuracy : 0.9778
#install.packages("caret")
library("caret")
library('MASS')

####update the file path####
wine <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\wine.data", header=F, sep=",")
k <- 100L
##splitting dataset into test and train
smp_size <- floor(0.75 * nrow(wine))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(wine)), size = smp_size)

train <- wine[train_ind, ]
test <- wine[-train_ind, ]

model <- lda(V1 ~., data = train)


test_transformed = predict( model, test )
train_transformed = predict( model, train)
train_transformed <- train_transformed$x
test_transformed <- test_transformed$x

###############visualizing the reduced dimentions
plot(train_transformed[, 1L], train_transformed[, 2L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[train$V1],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)
################
train_transformed <- cbind(V1 = as.character(train$V1), train_transformed)
test_transformed <- cbind(V1 = as.character(test$V1), test_transformed)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
t <- train_transformed[,c(2:3)]
train_classes <- train_transformed[,c(1)]

#training the train data using knn algorithm
knn_fit <- train(t, train_classes, method = "knn",tuneLength = 10,trControl=trctrl)
#predicting on the test data
test_pred <- predict(knn_fit, newdata = test_transformed)
test_transformed <- as.data.frame(test_transformed)

#computing accuracy
confusionMatrix(test_pred,test_transformed$V1)