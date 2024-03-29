## The SOMbrero package
## ----------------------------------------------------------------------------

#' @title Self Organizing Maps Bound to Realize Euclidean and Relational Outputs
#' @name SOMbrero-package
#' @aliases SOMbrero
#' 
#' @description This package implements the stochastic (also called on-line) 
#' Self-Organizing Map (SOM) algorithms for numeric and relational data.
#' 
#' It is based on a grid (see \code{\link{initGrid}}), which is part of the 
#' parameters given to the algorithm (see \code{\link{initSOM}} and 
#' \code{\link{trainSOM}}). Many graphs can help you with the results (see 
#' \code{\link{plot.somRes}}).
#' 
#' The version of the SOM algorithm implemented in this package is the 
#' stochastic version.
#' 
#' Several variants able to handle non-vectorial data are also implemented in 
#' their stochastic versions: \code{type = "korresp"} for contingency tables, as 
#' described in Cottrell et al. (2004) (with the observation weights defined in 
#' Cottrell and Letrémy, 2005a) and \code{type = "relational"} for dissimilarity 
#' data, as described in Olteanu and Villa-Vialaneix (2015a) with the fast 
#' implementation of Mariette \emph{et al.} (2017). A special focus has been put 
#' on representing graphs, as described in Olteanu and Villa-Vialaneix (2015b).
#' 
#' In addition, the numeric version of the algorithm handles missing values: 
#' missing entries are not used during training but the resulting map can be 
#' used to fill missing entries (using the entry of the corresponding 
#' prototype). The method is taken from Cottrell and Letrémy (2005b).
#' 
#' @author Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}\cr
#' Élise Maigné \email{elise.maigne@inrae.fr}\cr
#' Jérome Mariette \email{jerome.mariette@inrae.fr}\cr
#' Madalina Olteanu \email{olteanu@ceremade.dauphine.fr}\cr
#' Fabrice Rossi \email{fabrice.rossi@apiacoa.org}\cr
#' Laura Bendhaïba \email{laurabendhaiba@gmail.com}\cr
#' Julien Boelaert \email{julien.boelaert@gmail.com}\cr \cr
#' Maintainer: Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @references 
#' Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg:
#' Springer-Verlag, 3rd edition.
#' 
#' Cottrell M., Ibbou S., Letrémy P. (2004) SOM-based algorithms for qualitative
#' variables. \emph{Neural Networks}, \strong{17}, 1149-1167.
#' 
#' Cottrell M., Letrémy P. (2005a) How to use the Kohonen algorithm to 
#' simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
#' \strong{21}, 119-138.
#' 
#' Cottrell M., Letrémy P. (2005b) Missing values: processing with the Kohonen 
#' algorithm. \emph{Proceedings of Applied Stochastic Models and Data Analysis
#' (ASMDA 2005)}, 489-496.
#' 
#' Letrémy P. (2005) Programmes basés sur l'algorithme de Kohonen et dediés à
#' l'analyse des données. SAS/IML programs for 'korresp'.
#' 
#' Mariette J., Rossi F., Olteanu M., Villa-Vialaneix N. (2017) Accelerating 
#' stochastic kernel SOM. In: M. Verleysen, \emph{XXVth European Symposium on 
#' Artificial Neural Networks, Computational Intelligence and Machine Learning 
#' (ESANN 2017)}, i6doc, Bruges, Belgium, 269-274.
#' 
#' Olteanu M., Villa-Vialaneix N. (2015a) On-line relational and multiple 
#' relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 
#' 
#' Olteanu M., Villa-Vialaneix N. (2015b) Using SOMbrero for clustering and 
#' visualizing graphs. \emph{Journal de la Société Française de Statistique},
#' \strong{156}, 95-119.
#' 
#' Rossi F. (2013) yasomi: Yet Another Self-Organising Map Implementation. R 
#' package, version 0.3. \url{https://github.com/fabrice-rossi/yasomi}
#' 
#' Villa-Vialaneix N. (2017) Stochastic self-organizing map variants with the R
#' package SOMbrero. In: J.C. Lamirel, M. Cottrell, M. Olteanu, \emph{12th 
#' International Workshop on Self-Organizing Maps and Learning Vector 
#' Quantization, Clustering and Data Visualization (Proceedings of WSOM 2017)}, 
#' IEEE, Nancy, France.
#' 
#' @seealso \code{\link{initGrid}}, \code{\link{trainSOM}}, 
#' \code{\link{plot.somRes}} and \code{\link{sombreroGUI}}.
#' 
#' @import igraph
#' @import ggplot2
#' @import markdown
#' @importFrom grDevices trans3d hcl
#' @importFrom graphics layout legend par plot rect text title points
#' @importFrom stats aov as.dist chisq.test cmdscale cutree dist hclust pf
#' @importFrom stats princomp quantile rect.hclust runif sd aggregate median
#' @importFrom stats reshape var na.omit
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom metR geom_contour_fill	
#' @importFrom interp interp
#' @importFrom utils packageVersion


NULL