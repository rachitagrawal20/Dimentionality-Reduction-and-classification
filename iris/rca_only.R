#RCA implemented
#Accuracy : 1
library("caret")

####update the file path####
iris <- read.csv("C:\\Users\\Rachit Agrawal\\Downloads\\iris.data", header=F, sep=",")
k <- 100L
x <- wine[,c(1:4)]
y <- wine[,c(5)]

chunks <- vector("list", 150)
for (i in 1:50) chunks[[i]] <- sample(1L:50L, 10L)
for (i in 50:100) chunks[[i]] <- sample(50L:100L, 10L)
for (i in 100:150) chunks[[i]] <- sample(100L:150L, 10L)
chks <- x[unlist(chunks), ]

chunksvec <- rep(-1L, nrow(x))
for (i in 1L:length(chunks)) {
  for (j in 1L:length(chunks[[i]])) {
    chunksvec[chunks[[i]][j]] <- i
  }
}

rcs <- rca(x, chunksvec, 2)

xnew <- rcs$newX
xnew <- cbind(V1 = as.character(iris$V5), xnew)

############ visualize
plot(xnew[, 2L], xnew[, 3L],
     bg = c("#E41A1C", "#377EB8", "#4DAF4A")[transpose(c(rep(1,50),rep(2,50),rep(3,50)))],
     pch = c(rep(22, k), rep(21, k), rep(25, k))
)
##########