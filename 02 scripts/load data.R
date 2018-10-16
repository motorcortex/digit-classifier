library(data.table)
library(tidyverse)
library(spatstat)

train <- fread(file = "01 data/train.csv")
train <- read.csv(file = "01 data/train.csv", header=T)

colnames(train)
print(table(train$label))

## Explore the data set ##
plot_digits= function(pixeldata){
  
  pixelmatrix <- matrix(
    sapply(pixeldata[1, c(2:785)], as.numeric), 
    nrow=28, ncol=28)
  
  pixelmatrix <- apply(pixelmatrix, 1, rev)
  
  image(t(pixelmatrix),
      axes = FALSE, col = grey(seq(0, 1, length = 256)))
  
  title(main=pixeldata$label)
}

par(mfrow=c(4,4))
for (idx in c(1:16)){
  plot_digits(train[idx, ])
  }





