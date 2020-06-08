## ----setup, include=TRUE-------------------------------------------------
knitr::opts_chunk$set(include = FALSE)

## ----loading, results='hide', echo=FALSE, warning=FALSE, message=FALSE----
library("ggplot2")
library("SOMbrero")

## ----lesmisDescr, fig.width=12, fig.height=12, include=TRUE--------------
data(lesmis)
lesmis
plot(lesmis, vertex.size = 0)

## ----lesmisTrain, cache=TRUE---------------------------------------------
set.seed(622)
mis.som <- trainSOM(x.data=dissim.lesmis, type = "relational", nb.save = 10,
                   init.proto = "random", radius.type = "letremy")
plot(mis.som, what="energy")

## ----lesmisClustering, include=TRUE--------------------------------------
mis.som$clustering
table(mis.som$clustering)
plot(mis.som)

## ----lesmisPseudoNamesPlot, fig.height=12, fig.width=12, warning=FALSE----
plot(mis.som, what = "obs", type = "names")

## ----lesmisProjGraph-----------------------------------------------------
plot(mis.som, what = "add", type = "graph", var = lesmis)

## ----lesmisProtoProfiles-------------------------------------------------
plot(mis.som, what = "prototypes", type = "lines")  +
  guides(color = guide_legend(keyheight = 0.5, ncol = 2, label.theme = element_text(size = 6))) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
plot(mis.som, what = "prototypes", type = "barplot")  +
  guides(fill = guide_legend(keyheight = 0.5, ncol = 2, label.theme = element_text(size = 6))) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ----lesmisProtoDist, include=TRUE---------------------------------------
plot(mis.som, what = "prototypes", type = "poly.dist")

## ----lesmisProtoDistb----------------------------------------------------
plot(mis.som, what = "prototypes", type = "smooth.dist")
plot(mis.som, what = "prototypes", type = "umatrix")
plot(mis.som, what = "prototypes", type = "mds")
plot(mis.som, what = "prototypes", type = "grid.dist")

## ----lesmisColorOverview, fig.height=12, fig.width=12, include=TRUE------
plot(lesmis, vertex.label.color = rainbow(25)[mis.som$clustering], 
     vertex.size = 0)
legend(x = "left", legend = 1:25, col = rainbow(25), pch = 19)

## ----lesmisSCOverview----------------------------------------------------
plot(superClass(mis.som))

## ----lesmisSC, include=TRUE----------------------------------------------
sc.mis <- superClass(mis.som, k = 5)
summary(sc.mis)
table(sc.mis$cluster)
plot(sc.mis)

## ----lesmisSC2-----------------------------------------------------------
plot(sc.mis, what = "prototypes", type = "grid")
plot(sc.mis, what = "prototypes", type = "lines")
plot(sc.mis, what = "prototypes", type = "mds")
plot(sc.mis, type = "dendro3d")

## ----lesmisSCColorOverview, fig.width=12, fig.height=12, include=TRUE----
plot(lesmis, vertex.size = 0, 
     vertex.label.color = rainbow(5)[sc.mis$cluster[mis.som$clustering]])
legend(x = "left", legend = paste("SC", 1:5), col = rainbow(5), pch = 19)

## ----lesmisSCProjGraph, include=TRUE-------------------------------------
projectIGraph(sc.mis, lesmis)
par(mar = rep(0,4))
plot(sc.mis, what = "add", type = "projgraph", variable = lesmis, s.radius = 2)

## ----sessionInfo, include=TRUE-------------------------------------------
sessionInfo()

