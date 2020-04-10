#LDA implemented
#install.packages("caret")
library("caret")
library('MASS')

####update the file path####
iris <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\iris.data", header=F, sep=",")
iris <- iris[,c(1:4)]
iris <- cbind(iris, V5 = transpose(c(rep(1,50),rep(2,50),rep(3,50))))
k <- 100L
##splitting dataset into test and train
smp_size <- floor(0.75 * nrow(iris))

# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

model <- lda(V5 ~., data = train)


test_transformed = predict( model, test )
train_transformed = predict( model, train)
train_transformed <- train_transformed$x
test_transformed <- test_transformed$x

###############
plot(train_transformed[, 1L], train_transformed[, 2L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[train$V5],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)
################