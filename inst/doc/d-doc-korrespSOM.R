## ----setup, include=TRUE------------------------------------------------------
knitr::opts_chunk$set(include = FALSE)

## ----loading, results='hide', echo=FALSE, warning=FALSE, message=FALSE--------
library("ggplot2")
library("SOMbrero")

## ----loadData, include=TRUE---------------------------------------------------
data(presidentielles2002)
apply(presidentielles2002, 2, sum)

## ----presiTrain, cache=TRUE, include=TRUE-------------------------------------
set.seed(01091407)
korresp.som <- trainSOM(x.data = presidentielles2002, dimension = c(8,8),
                        type = "korresp", scaling = "chi2", nb.save = 10,
                        topo = "hexagonal", maxit = 500)
korresp.som

## ----energyPresi, include=TRUE------------------------------------------------
plot(korresp.som, what = "energy")

## ----presiClusters, include=TRUE----------------------------------------------
korresp.som$clustering

## ----presiHitmap, include=TRUE------------------------------------------------
plot(korresp.som, what = "obs", type = "hitmap", show.names = FALSE)

## ----presiGraphObs, warning=FALSE, fig.width=12, fig.height=12, include=TRUE----
plot(korresp.som, what="obs", type="names")

## ----presiProtoL--------------------------------------------------------------
# plot the line prototypes (106 French departements)
plot(korresp.som, what = "prototypes", type = "lines", view = "r", 
     show.names = TRUE) +    
  guides(color = guide_legend(keyheight = 0.5, ncol = 2,
                              label.theme = element_text(size = 4))) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# plot the column prototypes (16 candidates)
plot(korresp.som, what = "prototypes", type = "lines", view = "c", 
     show.names = TRUE) +
  guides(color = guide_legend(keyheight = 0.5, ncol = 1, 
                              label.theme = element_text(size = 6))) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ----presiProtoC3d, fig.width=12, fig.height=6, include=TRUE------------------
plot(korresp.som, what = "prototypes", type = "color", variable = "LE_PEN")

## ----presiProtoC3e, fig.width=12, fig.height=6--------------------------------
plot(korresp.som, what = "prototypes", type = "3d", variable = "la_reunion")

## ----presiProtoNumber, fig.width=12, fig.height=6-----------------------------
plot(korresp.som, what = "prototypes", type = "color", variable = 5, view = "c")
plot(korresp.som, what = "prototypes", type = "3d", variable = 5, view = "c")

## ----presiGraphProto2, include=TRUE-------------------------------------------
plot(korresp.som, what = "prototypes", type = "poly.dist", show.names = FALSE)

## ----presiGraphProto3---------------------------------------------------------
plot(korresp.som, what = "prototypes", type = "umatrix")
plot(korresp.som, what = "prototypes", type = "smooth.dist")
plot(korresp.som, what = "prototypes", type = "mds")
plot(korresp.som, what = "prototypes", type = "grid.dist")

## ----presiQuality, include=TRUE-----------------------------------------------
quality(korresp.som)

## ----presiSC1, include=TRUE---------------------------------------------------
plot(superClass(korresp.som))

## ----presiSC2, include=TRUE---------------------------------------------------
my.sc <- superClass(korresp.som, k = 3)
summary(my.sc)
plot(my.sc, plot.var = FALSE)

## ----presiSC3, include=TRUE---------------------------------------------------
plot(my.sc, type = "grid")

## ----presiSC3b----------------------------------------------------------------
plot(my.sc, type = "dendro3d")

## ----presiSC4-----------------------------------------------------------------
plot(my.sc, what = "obs", type = "hitmap")
plot(my.sc, what = "prototypes", type = "lines", show.names = TRUE, view = "c")
plot(my.sc, what = "prototypes", type = "poly.dist")
plot(my.sc, what = "prototypes", type = "mds")

## ----presiSC5-----------------------------------------------------------------
plot(my.sc, what = "prototypes", type = "color", view = "r", 
     variable = "correze")
plot(my.sc, what = "prototypes", type = "color", view = "c", 
     variable = "JOSPIN")

## ----sessionInfo, include=TRUE------------------------------------------------
sessionInfo()

