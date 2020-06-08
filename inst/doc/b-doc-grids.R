## ----loading, results='hide', echo=FALSE, warning=FALSE, message=FALSE----
library("SOMbrero")

## ----initGird------------------------------------------------------------
first_grid <- initGrid(dimension = c(5,6), topo = "square", dist.type = "maximum")

## ----plotFirstGrid-------------------------------------------------------
print(first_grid)

## ----summaryMyGrid-------------------------------------------------------
summary(first_grid)

## ----plotRainbowGrid-----------------------------------------------------
plot(first_grid)
plot(first_grid) + ggplot2::scale_fill_manual(values = rep("white", 30))
my_palette <- colorRampPalette(c("white", "pink", "purple"))(30)
plot(first_grid, show.names = FALSE) + 
  ggplot2::scale_fill_manual(values = my_palette)

## ----plotHexa------------------------------------------------------------
second_grid <- initGrid(dimension = c(4, 5), topo = "hexagonal")
plot(second_grid, names = paste0("N", 1:20)) + 
  ggplot2::ggtitle("Hexagonal SOM grid")

## ----sessionInfo---------------------------------------------------------
sessionInfo()

