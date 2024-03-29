## These functions handle the superclustering of somRes objects
## ----------------------------------------------------------------------------
############################### subfunctions ##################################
dendro3dProcess <- function(v.ind, ind, tree, coord, mat.moy, scatter) {
  ## TODO check (and probably fix) heights
  if (tree$merge[ind, v.ind] < 0) {
    res <- coord[abs(tree$merge[ind, v.ind]), ]
    scatter$points3d(matrix(c(res, 0, res,tree$height[ind]), ncol = 3, 
                            byrow = TRUE),
                     type = "l")
  } else {
    res <- mat.moy[tree$merge[ind, v.ind], ]
    scatter$points3d(matrix(c(res, tree$height[tree$merge[ind, v.ind]],
                              res, tree$height[ind]), ncol = 3, byrow = TRUE), 
                     type = "l")
  }
  return(res)
}

########################## super classes #####################################
#' @title Create super-clusters from SOM results
#' @name superClass
#' @export
#' 
#' @aliases superClass.somRes
#' @aliases print.somSC
#' @aliases summary.somSC
#' @aliases projectIGraph.somSC
#' @aliases plot.somSC
#' @aliases somSC-class
#' 
#' @description Aggregate the resulting clustering of the SOM algorithm into 
#' super-clusters.
#' 
#' @param sommap A \code{somRes} object.
#' @param method Argument passed to the \code{\link{hclust}} function.
#' @param members Argument passed to the \code{\link{hclust}} function.
#' @param k Argument passed to the \code{\link{cutree}} function (number of 
#' super-clusters to cut the dendrogram).
#' @param h Argument passed to the \code{\link{cutree}} function (height where 
#' to cut the dendrogram).
#' @param x A \code{somSC} object.
#' @param object A \code{somSC} object.
#' @param init.graph An \link[igraph]{igraph} object which is projected 
#' according to the super-clusters. The number of vertices of \code{init.graph} 
#' must be equal to the number of rows in the original dataset processed by the 
#' SOM (case \code{"korresp"} is not handled by this function). In the projected
#' graph, the vertices are positionned at the center of gravity of the 
#' super-clusters (more details in the section \strong{Details} below).
#' @param what What you want to plot for superClass object. Either the 
#' observations (\code{obs}), the prototypes (\code{prototypes}) or an 
#' additional variable (\code{add}), or \code{NULL} if not appropriate. 
#' Automatically set for types "hitmap" (to \code{"obs"}), 'grid' 
#' (to \code{"prototypes"}), default to "obs" otherwise.
#' If \code{what='add'}, the function \code{\link{plot.somRes}} will be called with
#' the argument \code{what} set to \code{"add"}.
#' @param type The type of plot to draw. Default value is \code{"dendrogram"}, 
#' to plot the dendrogram of the clustering. Case \code{"grid"} plots the grid 
#' in color according to the super clustering. Case \code{"projgraph"} uses an
#' \link[igraph]{igraph} object passed to the argument \code{variable} and plots
#' the projected graph as defined by the function \code{projectIGraph.somSC}.
#' All other cases are those available in the function \code{\link{plot.somRes}} 
#' and surimpose the super-clusters over these plots.
#' @param plot.var A boolean indicating whether a graph showing the evolution of
#' the explained variance should be plotted. This argument is only used when 
#' \code{type="dendrogram"}, its default value is \code{TRUE}.
#' @param show.names Whether the cluster titles must be printed in center of
#' the grid or not for \code{type="grid"}. Default to \code{FALSE} (titles not 
#' displayed).
#' @param names If \code{show.names = TRUE}, values of the title to 
#' display for \code{type="grid"}. Default to "Cluster " followed by the cluster
#' number.
#' @param \dots Used for \code{plot.somSC}: further arguments passed either to
#' the function \code{\link{plot}} (case \code{type="dendro"}) or to 
#' \code{\link{plot.myGrid}} (case \code{type="grid"}) or to 
#' \code{\link{plot.somRes}} (all other cases).
#' 
#' @details The \code{superClass} function can be used in 2 ways: \itemize{
#'   \item to choose the number of super clusters via an \code{\link{hclust}} 
#'   object: then, both arguments \code{k} and \code{h} are not filled.
#'   \item to cut the clustering into super clusters: then, either argument 
#'   \code{k} or argument \code{h} must be filled. See \code{\link{cutree}} for
#'   details on these arguments.
#' }
#'   
#' The squared distance between prototypes is passed to the algorithm.
#' 
#' \code{summary} on a \code{superClass} object produces a complete summary of 
#' the results that displays the number of clusters and super-clusters, the 
#' clustering itself and performs ANOVA analyses. For \code{type="numeric"} the 
#' ANOVA is performed for each input variable and test the difference of this
#' variable across the super-clusters of the map. For \code{type="relational"} 
#' a dissimilarity ANOVA is performed (see (Anderson, 2001), except that in the 
#' present version, a crude estimate of the p-value is used which is based on 
#' the Fisher distribution and not on a permutation test.
#' 
#' On plots, the different super classes are identified in the following ways:
#' \itemize{ 
#'   \item either with different color, when \code{type} is set among: 
#'   \code{"grid"} (N, K, R), \code{"hitmap"} (N, K, R), \code{"lines"} (N, K, R), 
#'   \code{"barplot"} (N, K, R), \code{"boxplot"}, \code{"poly.dist"} (N, K, R), 
#'   \code{"mds"} (N, K, R), \code{"dendro3d"} (N, K, R), \code{"graph"} (R),
#'   \code{"projgraph"} (R)
#'   \item or with title, when \code{type} is set among: \code{"color"} (N, K),
#'   \code{"pie"} (N, R)
#' }
#' 
#' In the list above, the charts available for a \code{numerical} SOM are marked
#' with a N, with a K for a \code{korresp} SOM and with a R for 
#' \code{relational} SOM.
#' 
#' \code{\link{projectIGraph.somSC}} produces a projected graph from the 
#' \link[igraph]{igraph} object passed to the argument \code{variable} as 
#' described in (Olteanu and Villa-Vialaneix, 2015). The attributes of this 
#' graph are the same than the ones obtained from the SOM map itself in the 
#' function \code{\link{projectIGraph.somRes}}. \code{\link{plot.somSC}} used with 
#' \code{type="projgraph"} calculates this graph and represents it by 
#' positionning the super-vertexes at the center of gravity of the 
#' super-clusters. This feature can be combined with \code{pie.graph=TRUE} to 
#' super-impose the information from an external factor related to the 
#' individuals in the original dataset (or, equivalently, to the vertexes of the
#'  graph).
#'  
#' @references
#' Anderson M.J. (2001). A new method for non-parametric multivariate analysis 
#' of variance. \emph{Austral Ecology}, \strong{26}, 32-46.
#' 
#' Olteanu M., Villa-Vialaneix N. (2015) Using SOMbrero for clustering and 
#' visualizing graphs. \emph{Journal de la Societe Francaise de Statistique}, 
#' \strong{156}, 95-119.
#' 
#' @return The \code{superClass} function returns an object of class 
#' \code{somSC} which is a list of the following elements: 
#'   \item{cluster}{The super clustering of the prototypes (only if either 
#'   \code{k} or \code{h} are given by user).}
#'   \item{tree}{An \code{\link{hclust}} object.}
#'   \item{som}{The \code{somRes} object given as argument (see 
#'   \code{\link{trainSOM}} for details).}
#'   
#' The \code{projectIGraph.somSC} function returns an object of class 
#' \code{\link{igraph}} with the following attributes: 
#'   \item{\code{layout}}{ provides the layout of the projected graph 
#'   according to the center of gravity of the super-clusters positioned on 
#'   the SOM grid (graph attribute);}
#'   \item{\code{name} and \code{size}}{ respectively are the vertex number on 
#'   the grid and the number of vertexes included in the corresponding cluster 
#'   (vertex attribute);}
#'   \item{\code{weight}}{ gives the number of edges (or the sum of the weights)
#'    between the vertexes of the two corresponding clusters (edge attribute).}
#' 
#' @author Élise Maigné \email{elise.maigne@inrae.fr}\cr
#' Madalina Olteanu \email{olteanu@ceremade.dauphine.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{hclust}}, \code{\link{cutree}}, \code{\link{trainSOM}}, 
#' \code{\link{plot.somRes}}
#' 
#' @examples 
#' set.seed(11051729)
#' my.som <- trainSOM(x.data = iris[,1:4])
#' # choose the number of super-clusters
#' sc <- superClass(my.som)
#' plot(sc)
#' # cut the clustering
#' sc <- superClass(my.som, k = 4)
#' summary(sc)
#' plot(sc)
#' plot(sc, type = "grid")
#' plot(sc, what = "obs", type = "hitmap")

superClass <- function(sommap, method, members, k, h, ...) {
  UseMethod("superClass")
}

#' @export

superClass.somRes <- function(sommap, method = "ward.D", members = NULL, 
                              k = NULL, h = NULL, ...) {
  if (method == "ward.D2") {
    warning("'method=ward.D2' should not be used with this function because the current code implements the computation of squared distances.", 
            call. = TRUE)
  }
  
  if (sommap$parameters$type == "relational") {
    the.distances <- protoDist(sommap, "complete")
    if (sum(the.distances<0) > 0) {
      stop("Impossible to make super clustering!", call. = TRUE)
    } else the.distances <- as.dist(the.distances)
  } else the.distances <- as.dist(protoDist(sommap, "complete")^2)
  
  hc <- hclust(the.distances, method, members)
  if (!is.null(k) || !is.null(h)) {
    sc <- cutree(hc, k, h)
    res <- list("cluster" = as.numeric(sc), "tree" = hc, "som" = sommap)
  } else {
    res <- list("tree" = hc, "som" = sommap)
  }
  class(res) <- "somSC"
  return(res)
}

################################ S3 functions #################################
#' @export
#' @rdname superClass
print.somSC <- function(x, ...) {
  cat("\n   SOM Super Classes\n")
  cat("     Initial number of clusters : ", prod(x$som$parameters$the.grid$dim),
      "\n")
  if (length(x) > 2) {
    cat("     Number of super clusters   : ", length(unique(x$cluster)), "\n\n")
  } else cat("     Number of super clusters not chosen yet.\n\n")
}

#' @method summary somSC
#' @export
#' @rdname superClass
summary.somSC <- function(object, ...) {
  print(object)
  if (length(object) > 2) {
    cat("\n  Frequency table")
    print(table(object$cluster))
    cat("\n  Clustering\n")
    output.clustering <- object$cluster
    names(output.clustering) <- seq_along(object$cluster)
    print(output.clustering)
    cat("\n")
    
    if (object$som$parameters$type == "numeric") {
      sc.clustering <- object$cluster[object$som$clustering]
      cat("\n  ANOVA\n")
      res.anova <- as.data.frame(t(sapply(1:ncol(object$som$data), function(ind) {
        res.aov <- summary(aov(object$som$data[ ,ind] ~ as.factor(sc.clustering)))
        c(round(res.aov[[1]][1,4], digits = 3), 
          round(res.aov[[1]][1,5], digits = 8))
      })))
      names(res.anova) <- c("F", "pvalue")
      res.anova$significativity <- rep("", ncol(object$som$data))
      res.anova$significativity[res.anova$"pvalue" < 0.05] <- "*"
      res.anova$significativity[res.anova$"pvalue" < 0.01] <- "**"
      res.anova$significativity[res.anova$"pvalue" < 0.001] <- "***"
      rownames(res.anova) <- colnames(object$som$data)
      
      cat("\n        Degrees of freedom : ", 
          summary(aov(object$som$data[ ,1] ~ as.factor(sc.clustering)))[[1]][1,1],
          "\n\n")
      print(res.anova)
      cat("\n")
    } else if (object$som$parameters$type == "relational") {
      if (object$som$parameters$scaling == "cosine") {
        norm.data <- preprocessData(object$som$data, object$parameters$scaling)
      } else norm.data <- object$som$data
      sse.total <- sum(norm.data) / (2 * nrow(norm.data))
      
      sc.clustering <- object$cluster[object$som$clustering]
      
      sse.within <- sum(sapply(unique(sc.clustering), function(clust)
        sum(norm.data[sc.clustering == clust,sc.clustering==clust]) /
          (2 * sum(sc.clustering == clust))))
      
      n.clusters <- length(unique(sc.clustering))
      F.stat <- ((sse.total - sse.within)/sse.within) * 
        ((nrow(norm.data) - n.clusters) / (n.clusters - 1))
      
      p.value <- 1 - pf(F.stat, n.clusters - 1, nrow(norm.data) - n.clusters)
      sig <- ""
      if (p.value < 0.001) {
        sig <- "***"
      } else if (p.value < 0.1) {
        sig <- "**"
      } else if (p.value < 0.05) sig <- "*"
      
      cat("\n  ANOVA\n")
      cat("         F                       : ", F.stat, "\n")
      cat("         Degrees of freedom      : ", n.clusters - 1, "\n")
      cat("         p-value                 : ", p.value, "\n")
      cat("                 significativity : ", sig, "\n")
    }
  }
}

#' @export
#' @rdname superClass
plot.somSC <- function(x, what = c("obs", "prototypes", "add"), 
                       type = c("dendrogram", "grid", "hitmap", "lines", 
                                "meanline", "barplot", "boxplot", "mds", 
                                "color", "poly.dist", "pie", "graph", 
                                "dendro3d", "projgraph"),
                       plot.var = TRUE,  show.names = TRUE, 
                       names = 1:prod(x$som$parameters$the.grid$dim),
                       ...) {
  # TODO: add types "names" and "words"
  args <- list(...)
  type <- type[1]
  
  calls <- names(sapply(match.call(), deparse))[-1]
  if (any("print.title" %in% calls)) {
    warning("'print.title' will be deprecated, please use 'show.names' instead",
            call. = FALSE, immediate. = TRUE)
    show.names <- args$print.title
  }
  if (any("the.titles" %in% calls)) {
    warning("'the.titles' will be deprecated, please use 'names' instead", 
            call. = FALSE, immediate. = TRUE)
    names <- args$the.titles
  }
  if (any("add.type" %in% calls)) {
    warning("'add.type' will be deprecated, please use `what='add'` instead", 
            call. = FALSE, immediate. = TRUE)
    what <- "add"
  }
  
  if (!(type %in% c("dendrogram", "dendro3d", "grid"))) {
    args$what <- match.arg(what)
    # Type control (if not in dendro, dendro3d)
    authorizedtypes <-
      list("numeric" = list("obs" = c("hitmap", "lines", "meanline", "barplot",
                                      "boxplot","color"),
                            "prototypes" = c("grid", "lines", 
                                             "barplot", "mds", "color", 
                                             "poly.dist"),
                            "add" = c("lines", "meanline", "barplot", "boxplot",
                                      "color", "pie", "graph", "projgraph")),
           "korresp" = list("obs" = c("hitmap"),
                            "prototypes" = c("grid", "lines", 
                                             "barplot", "mds", "color", 
                                             "poly.dist"),
                            "add" = NULL),
           "relational" = list("obs" = c("hitmap"),
                               "prototypes" = c("grid", "lines", 
                                                "barplot", "mds", "poly.dist"),
                               "add" = c("lines", "meanline", "barplot", "pie",
                                         "graph", "projgraph"))
      )
    
    if (!is.element(type, authorizedtypes[[x$som$parameters$type]][[args$what]])) {
      namedwhat <- switch (args$what,
                           "obs" = "observations",
                           "prototypes" = "prototypes",
                           "add" = "additional variables"
      )
      if (args$what == "add" && x$som$parameters$type == "korresp") {
        stop(paste0("Incorrect type. For ", x$som$parameters$type,
                    " super classes, plots for ", namedwhat, 
                    " are not authorized"), call. = TRUE)
      } else {
        stop(paste0("Incorrect type. For ", x$som$parameters$type,
                    " super classes, plots for ", namedwhat, " can be '",
                    paste(authorizedtypes[[x$som$parameters$type]][[args$what]],
                          collapse="', '"),
                    "'"), call. = TRUE)
      }
    }
  }
  
  # Colors (used only for dendrogram, dendro3d, graph and projgraph)
  if (!is.null(x$cluster)) {
    nbclust <- max(x$cluster)
    if ((!is.null(args$col)) & (length(args$col) == nbclust)) {
      clust.col.pal <- args$col
    } else {
      if (!is.null(args$col))
        warning("Incorrect number of colors
                  (does not fit the number of super-clusters);
                  using the default palette.\n", call.=TRUE, immediate.=TRUE)
      clust.col.pal <- gg_color(nbclust)
    }
    clust.col <- clust.col.pal[x$cluster]
  } else {
    nbclust <- 1
  }
  
  if (type == "dendrogram") {
    args$x <- x$tree
    if (is.null(args$main)) args$main <- "Super-clusters dendrogram"
    if ((x$tree$method == "ward.D") & (plot.var)) {
      layout(matrix(c(2, 2, 1), ncol = 3))
      Rsq <- cumsum(x$tree$height / sum(x$tree$height))
      plot(length(x$tree$height):1, Rsq, type = "b", pch = "+",
           xlab = "Number of clusters", 
           ylab = "proportion of unexplained variance",
           main = "Proportion of variance\n not explained by\n super-clusters")
      do.call("plot", args)
    } else  {
      do.call("plot", args)
    }
    if (length(x) > 2) {
      rect.hclust(x$tree, k = max(x$cluster), cluster = x$cluster,
                  border = clust.col.pal[unique(x$cluster[x$tree$order])])
      legend("topright", col = clust.col.pal, pch = 19, 
             legend = paste("SC", 1:nbclust), cex = 1)
    } else warning("Impossible to plot the rectangles: no super clusters.\n",
                   call. = TRUE, immediate. = TRUE)
    layout(1)
    
  } else if (type == "dendro3d") {
    if (length(x) < 3) {
      clust.col <- "black"
    } 
    x.y.coord <- x$som$parameters$the.grid$coord + 0.5
    if (floor(max(x$tree$height[-which.max(x$tree$height)])) == 0) {
      z.max <- max(x$tree$height[-which.max(x$tree$height)])
    } else {
      z.max <- ceiling(max(x$tree$height[-which.max(x$tree$height)]))
    }
    if (is.null(args$angle)) args$angle <- 40
    posleg <- "bottomright"
    if (args$angle > 80) posleg <- "topright"
    spt <- scatterplot3d(x = x.y.coord[,1], y = x.y.coord[,2], 
                         z = rep(0,prod(x$som$parameters$the.grid$dim)), 
                         xlim = c(min(x.y.coord[ ,1]) - 0.5, 
                                  max(x.y.coord[ ,1]) + 0.5), 
                         ylim = c(min(x.y.coord[ ,2]) - 0.5, 
                                  max(x.y.coord[ ,2]) + 0.5),
                         zlim = c(0, z.max), angle = args$angle, pch = 19, 
                         color = clust.col, xlab = "x", ylab = "y", zlab = "", 
                         x.ticklabs = "", y.ticklabs = "")
    if (length(x) > 2) {
      legend(posleg, col = clust.col.pal, pch = 19, 
             legend = paste("SC", 1:nbclust), cex = 1)
    }
    horiz.ticks <- matrix(NA, nrow = prod(x$som$parameters$the.grid$dim) - 1,
                          ncol = 2)
    for (neuron in 1:(prod(x$som$parameters$the.grid$dim) - 1)) {
      vert.ticks <- sapply(1:2, dendro3dProcess, ind = neuron, tree = x$tree, 
                           coord = x.y.coord, mat.moy = horiz.ticks, 
                           scatter = spt)
      horiz.ticks[neuron, ] <- rowMeans(vert.ticks)
      spt$points3d(matrix(c(vert.ticks[ ,1], x$tree$height[neuron],
                            vert.ticks[ ,2], x$tree$height[neuron]), ncol = 3,
                          byrow = TRUE), 
                   type = "l")
    }
  } else {
    if (length(x) < 3) {
      stop("No super clusters: plot unavailable.\n")
    } else {
      if (type == "grid") {
        args$sc <- max(x$cluster)
        ggplotGrid("prototypes", type = "grid", values = x$cluster, 
                   clustering = as.numeric(as.character(rownames(x$som$prototypes))), 
                   show.names = show.names, names=names, 
                   the.grid = x$som$parameters$the.grid, args)
        
      } else if (type=="hitmap") {
        args$sc <- max(x$cluster)
        ggplotGrid("observations", type = "hitmap",  
                   values = x$cluster[x$som$clustering], 
                   clustering = x$som$clustering, 
                   show.names = show.names, names = names, 
                   the.grid = x$som$parameters$the.grid, args)
        
      } else if (type %in% c("lines", "meanline", "barplot", "boxplot", "mds",
                             "color", "poly.dist", "pie", "graph")) {
        if (type == "graph") {
          neclust <- which(!is.na(match(1:prod(x$som$parameters$the.grid$dim),
                                        unique(x$som$clustering))))
          if (is.null(args$pie.graph)) args$pie.graph <- FALSE
          if (!args$pie.graph) {
            args$vertex.color <- clust.col[neclust]
            args$vertex.frame.color <- clust.col[neclust]
          } else {
            show.names <- TRUE
            args$vertex.label <- paste("SC",x$cluster[neclust])
            args$vertex.label.color <- "black"
          }
        }
        args$x <- x$som
        args$type <- type
        
        # manage titles
        args$show.names <- show.names
        if (type %in% c("lines", "meanline", "barplot", "boxplot", "poly.dist")) {
          args$names <- names
        } else {
          args$names <- paste("SC", x$cluster)
        }
        
        # manage colors 
        if (args$what == "obs" | args$what == "add") {
          args$varcolor <- x$cluster[x$som$clustering]
          args$sc <- x$cluster[x$som$clustering] 
        }
        if (args$what == "prototypes") {
          args$varcolor <- x$cluster
          args$sc <-  x$cluster
        }
        
        if (args$what == "add") {
          if (is.null(args$variable)) {
            stop("Specify the variable to plot", call. = TRUE)
          }
          callvar <- match.call(expand.dots = FALSE)$...[["variable"]]
          args$varname <- deparse(substitute(callvar))
        }
        
        do.call("plot.somRes", args)
        
      } else if (type == "projgraph") {
        # check arguments
        if (x$som$parameters$type == "korresp")
          stop(type, " plot is not available for 'korresp' super classes\n",
               call. = TRUE)
        if (is.null(args$variable)) {
          stop("for type='projgraph', the argument 'variable' must be supplied (igraph object)\n",
               call. = TRUE)
        }
        if (!is.igraph(args$variable)) {
          stop("for type='projgraph', argument 'variable' must be an igraph object\n",
               call. = TRUE)
        }
        if (length(V(args$variable)) != nrow(x$som$data)) {
          stop("number of nodes in graph does not fit length of the original data", 
               call. = TRUE)
        }
        
        args$vertex.color <- clust.col.pal
        args$vertex.frame.color <- clust.col.pal
        
        # case of pie
        if (is.null(args$pie.graph)) args$pie.graph <- FALSE
        if (args$pie.graph) {
          if (is.null(args$pie.variable))
            stop("pie.graph is TRUE, you must supply argument 'pie.variable'\n",
                 call.=TRUE)
          if (nrow(as.matrix(args$pie.variable)) != nrow(x$som$data)) {
            stop("length of argument 'pie.variable' does not fit length of the original data",
                 call.=TRUE)
          }
          args$vertex.shape <- "pie"
          if (is.null(args$vertex.pie.color)) args$vertex.pie.color <- NULL
          proj.pie <- projectFactor(args$variable, x$cluster[x$som$clustering],
                                    args$pie.variable,
                                    pie.color=args$vertex.pie.color)
          args$vertex.pie <- proj.pie$vertex.pie
          args$vertex.pie.color <- proj.pie$vertex.pie.color
        } else if (is.null(args$vertex.shape)) args$vertex.shape <- "circle"
        
        # find projected graph
        proj.graph <- projectIGraph.somSC(x, args$variable)
        args$proj.graph <- proj.graph
        args$variable <- NULL
        do.call("plotProjGraph", args)
        
      } else stop("Sorry, this type is not implemented yet\n", call.=TRUE) 
    }
  }
}

#' @export
#' @rdname superClass
projectIGraph.somSC <- function(object, init.graph, ...) {
  if (length(object) <= 2) 
    stop("The number of clusters has not been chosen yet. Cannot project the graph on super-clusters.\n",
         call. = TRUE)
  if (object$som$parameters$type == "korresp")
    stop("projectIGraph is not available for 'korresp' super classes\n", 
         call. = TRUE)
  # project the graph into the SOM grid
  proj.graph <- projectIGraph.somRes(object$som, init.graph)
  # clustering of the non empty clusters
  induced.clustering <- object$cluster[as.numeric(V(proj.graph)$name)]
  # search for the positions (center of gravity) of the superclusters
  original.positions <- object$som$parameters$the.grid$coord
  positions <- cbind(tapply(original.positions[ ,1], object$cluster, mean),
                     tapply(original.positions[ ,2], object$cluster, mean))
  
  proj.graph.sc <- projectGraph(proj.graph, induced.clustering, positions)
  
  proj.graph.sc <- set.graph.attribute(proj.graph.sc, "layout", positions)
  return(proj.graph.sc)
}