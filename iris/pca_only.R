## PCA+KNN implemented
## Accuracy : 0.9556
#install.packages("caret")

library("caret")
#removing categorical column
k = 100L
####update the file path####
iris <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\iris.data", header=F, sep=",")
data <- iris[,c(1:4)]
############
visualize <- prcomp(data, scale = TRUE)
visualize <- visualize$x[,1:2]
visualize <- cbind(V1 = as.character(iris$V1), visualize)
plot(visualize[, 2L], visualize[, 3L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[transpose(c(rep(1,50),rep(2,50),rep(3,50)))],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)

##############



