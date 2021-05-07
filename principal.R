#Slump

#https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/

rm(list=ls()) # Fjerner alle matriser
while (dev.cur()>1) dev.off()  #Fjerner alle figurer

library(tidyverse)
#library(reticulate)


#source_python("/home/morten/python/slump/slump.py")

data("iris")

iris <- iris

str(iris)


set.seed(111)

ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))

training <- iris[ind==1,]

testing <- iris[ind==2,]




# Figurer -----------------------------------------------------------------

library(psych)


pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)


pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)

attributes(pc)

pc$scale

print(pc)

summary(pc)


pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)


library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)


trg <- predict(pc, training)

trg <- data.frame(trg, training[5])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[5])

library(nnet)

trg$Species <- relevel(trg$Species, ref = "setosa")
mymodel <- multinom(Species~PC1+PC2, data = trg)
summary(mymodel)