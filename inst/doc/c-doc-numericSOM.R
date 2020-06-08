## ----setup, include=TRUE-------------------------------------------------
knitr::opts_chunk$set(include = FALSE)

## ----loading, results='hide', echo=FALSE, warning=FALSE, message=FALSE----
library("ggplot2")
library("SOMbrero")

## ----dataGeneration------------------------------------------------------
set.seed(4031719)
the.data <- data.frame("x1" = runif(500), "x2" = runif(500))
ggplot(the.data, aes(x = x1, y = x2)) + geom_point() + theme_bw()

## ----dataTrain-----------------------------------------------------------
set.seed(1105)
# run the SOM algorithm with 10 intermediate backups and 2000 iterations
my.som <- trainSOM(x.data=the.data, dimension=c(5,5), nb.save=10, maxit=2000, 
                   scaling="none", radius.type="letremy", topo="square",
                   dist.type = "letremy")

## ----energy, include=TRUE------------------------------------------------
plot(my.som, what="energy")

## ----hitmapObs, fig.height=6, fig.width=6--------------------------------
plot(my.som, what = "obs", type = "hitmap")

## ----clusteredData, include=TRUE, cache=TRUE-----------------------------
# prepare a vector of colors
my.colors <- rainbow(prod(my.som$parameters$the.grid$dim))[my.som$clustering]

# points depicted with the same color are in the same final cluster
plot(my.som$data[,1], my.som$data[,2], col=my.colors, pch=19, xlab="x1", 
     ylab="x2", main="Data according to final clustering")

## ----colorProto, fig.width=5, fig.height=2.5, include=TRUE---------------
par(mfrow=c(1,2))
plot(my.som, what="prototypes", type="color", var=1)
plot(my.som, what="prototypes", type="color", var=2)

## ----colorObs, fig.width=5, fig.height=2.5-------------------------------
par(mfrow=c(1,2))
plot(my.som, what="obs", type="color", var=1)
plot(my.som, what="obs", type="color", var=2)

## ----protoEvoluation, fig.width=15, fig.height=6, include=TRUE, echo=FALSE----
# Get the neighbours between prototypes
values <- protoDist(my.som, "neighbors")
tmp <- data.frame("prot1" = rep.int(1:prod(my.som$parameters$the.grid$dim), 
                                    times=sapply(values, length)), 
                  "nei" = as.numeric(as.character(names(unlist(values)))))
tmp <- tmp[tmp[ ,1] < tmp[ ,2], ]

# plot the prototypes
par(mfrow=c(2, 5),mar=c(3,2,2,1))
invisible(sapply(1:my.som$parameters$nb.save, function(ind){
  plot(my.som$backup$prototypes[[ind]][,1], my.som$backup$prototypes[[ind]][,2],
       xlab="", ylab="", main=c("iteration ", my.som$backup$steps[ind]))
  for (i in 1:nrow(tmp)){
    segments(x0=my.som$backup$prototypes[[ind]][tmp[i,1],1], 
             y0=my.som$backup$prototypes[[ind]][tmp[i,1],2],
             x1=my.som$backup$prototypes[[ind]][tmp[i,2],1], 
             y1=my.som$backup$prototypes[[ind]][tmp[i,2],2], 
             col="red", pch=19)
  }
}))

## ----irisTrain, cache=TRUE, include=TRUE---------------------------------
set.seed(255)
# run the SOM algorithm with verbose set to TRUE
iris.som <- trainSOM(x.data = iris[,1:4], dimension = c(5,5), verbose = TRUE, 
                     nb.save = 5, topo = "hexagonal")
iris.som

## ----energyIris, include=TRUE--------------------------------------------
plot(iris.som, what="energy")

## ----irisClusters, include=TRUE------------------------------------------
iris.som$clustering
table(iris.som$clustering)

## ----irisHitmap, include=TRUE--------------------------------------------
plot(iris.som, what="obs", type="hitmap")

## ----irisSummary, include=TRUE-------------------------------------------
summary(iris.som)

## ----irisPred1, include=TRUE---------------------------------------------
# call predict.somRes
predict(iris.som, iris[1,1:4])
# check the result of the final clustering with the SOM algorithm
iris.som$clustering[1]

## ----irisGraphOP, include=TRUE-------------------------------------------
par(mfrow = c(2,2))
plot(iris.som, what = "obs", type = "color", variable = 1)
plot(iris.som, what = "obs", type = "color", variable = 2)
plot(iris.som, what = "obs", type = "color", variable = 3)
plot(iris.som, what = "obs", type = "color", variable = 4)

## ----irisGraphOP2--------------------------------------------------------
plot(iris.som, what = "prototypes", type = "lines", show.names = TRUE) + 
  theme(axis.text.x = element_blank())
plot(iris.som, what = "obs", type = "barplot", show.names = TRUE) + 
  theme(axis.text.x = element_blank())

## ----irisObs, warning=FALSE----------------------------------------------
plot(iris.som, what = "obs", type = "boxplot", show.names = TRUE)
plot(iris.som, what = "obs", type = "lines", show.names = TRUE)
plot(iris.som, what = "obs", type = "names", show.names = TRUE)

## ----irisProto-----------------------------------------------------------
par(mfrow=c(2,2))
plot(iris.som, what = "prototypes", type = "3d", variable = 1)
plot(iris.som, what = "prototypes", type = "3d", variable = 2)
plot(iris.som, what = "prototypes", type = "3d", variable = 3)
plot(iris.som, what = "prototypes", type = "3d", variable = 4)

## ----irisDistProto, warning=FALSE, include=TRUE--------------------------
plot(iris.som, what = "prototypes", type = "poly.dist", show.names = FALSE)

## ----irisDistProto2, warning=FALSE---------------------------------------
plot(iris.som, what = "prototypes", type = "umatrix")
plot(iris.som, what = "prototypes", type = "smooth.dist")
plot(iris.som, what = "prototypes", type = "mds")
plot(iris.som, what = "prototypes", type = "grid.dist")

## ----irisAdd1, include=TRUE----------------------------------------------
class(iris$Species)
levels(iris$Species)
plot(iris.som, what = "add", type = "pie", variable = iris$Species) +
  scale_fill_brewer(type = "qual") + 
  guides(fill = guide_legend(title = "Species"))

## ----irisAdd2------------------------------------------------------------
plot(iris.som, what = "add", type = "color", variable = iris$Sepal.Length, 
     show.names = FALSE)

## ----irisMatCont, echo=FALSE---------------------------------------------
my.cont.mat <- matrix(data=c(rep(c(rep(1,50), rep(0,150)), 2), rep(1,50)), 
                      nrow = 150, ncol = 3)
colnames(my.cont.mat) <- levels(iris$Species)

## ----irisAdd4------------------------------------------------------------
head(my.cont.mat)
plot(iris.som, what = "add", type = "words", variable = my.cont.mat, 
     show.names = FALSE)

## ----irisAdd5, warning=FALSE---------------------------------------------
plot(iris.som, what = "add", type = "names", variable = rownames(iris)) 

## ----irisAdd5bis, warning=FALSE------------------------------------------
plot(iris.som, what = "add", type = "names", variable = iris$Species)

## ----irisQuality, include=TRUE-------------------------------------------
quality(iris.som)

## ----saveQual, echo=FALSE------------------------------------------------
qualities <- quality(iris.som)

## ----irisSC, include=TRUE------------------------------------------------
plot(superClass(iris.som))

## ----irisSC3, include=TRUE-----------------------------------------------
my.sc <- superClass(iris.som, k = 3)
summary(my.sc)
plot(my.sc, plot.var = FALSE)

## ----irisSCplot, fig.width=6, fig.height=4, include=TRUE-----------------
plot(my.sc, type = "grid")

## ----irisSCplot3d--------------------------------------------------------
plot(my.sc, type = "dendro3d")

## ----irisSCplot2, fig.width=6, fig.height=5------------------------------
plot(my.sc, what = "obs", type = "hitmap", maxsize = 20)

## ----irisSCplot2B--------------------------------------------------------
plot(my.sc, what = "prototypes", type = "lines")
plot(my.sc, what = "prototypes", type = "barplot")

## ----irisSCplot2C, fig.height=4, fig.width=6-----------------------------
plot(my.sc, what = "prototypes", type = "mds")

## ----irisSCplot3, include=TRUE-------------------------------------------
plot(my.sc, what = "prototypes", type = "color", variable = "Sepal.Length")
plot(my.sc, what = "prototypes", type = "poly.dist")

## ----irisSCplot4---------------------------------------------------------
plot(my.sc, what = "add", type = "pie", variable = iris$Species) +
  scale_fill_brewer(type = "qual")
plot(my.sc, what = "add", type = "color", variable = iris$Sepal.Length)

## ----sessionInfo, include=TRUE-------------------------------------------
sessionInfo()

