# SOMbrero 1.4-2 [2024-01-24]

## Bug fix
  * corrected two notes (on documentation and vignettes) from CRAN checks
  * replaced aes_string by aes
  
## Misc
  * changed CITATION format according to new standards
  * removed cache from vignettes

# SOMbrero 1.4-1

## Fixed bugs:
  * fixed a bug related to issue #4 on Github

# SOMbrero 1.4 (major release)

## New features:
  * numeric SOM can now handle missing data: training is done without accounting
  * for missing entries and resulting SOM can be used to impute missing entries

## Fixed bugs:
  * proto0 was incorrectly used for 'numeric' (see bug report #4 on Github)
  * dist.type was improperly handled for types other than 'euclidean' and 
  'letremy' when radius.type was letremy
  * minor bug in computation of tolerance for numerical approximations with 
  Gaussian radius
  * removed 'tolerance' from the prediction (not used any more)
  * fixed minor issues in OO for trainSOM
  
## Improvements:
  * Heskes' affectation now account for total neighbord weight in order to not 
  favor too much the borders of the map
  
## Misc: 
  * refactorization of most of the code

# SOMbrero 1.3-1

## Improvements:
  * make trainSOM a method for different classes
  * create a trainSOM method for outputs of mixKernel package

# SOMbrero 1.3

## Fixed bugs:
  eps0 parameter taken into account in shiny app
  correct use of "what" parameter for superclasses plot
  fixed a minor problem in Heskes' assignment when distance is Euclidean

## Improvements:
  * hexagonal topography is now available
  * plots are now ggplot2 based and former plot arguments are no longer valid 
  since ggplot2 syntax is now used
  * the shiny app has been entirely remade
  * data from the environment can be loaded directly in the shiny application 
  and 3 examples datasets are loaded automatically in the shiny app
  * titles and legends in plots have been improved
  * code used in the app is printed to ease reproducibility
  * protoDist (for neighbors) has been improved by adding, for the case 
  'neighbors' a radius argument. When this radius is passed the Euclidean 
  distance between unit coordinates
  * print.titles is deprecated and has been replaced by show.names
  
## Misc:
  * radar plots are no longer available

# SOMbrero 1.2-5

## Fixed bugs:
  * adapted script to the new stringsAsFactor policy on devel
  * fixed a bug for non significant variables in summary
  
## Misc:
  * roxygenized documentation
  * changed maintainer name and address, added new contributors
  * fixed URI in package vignette

# SOMbrero 1.2-4

## Fixed bugs:
  * changed unitary tests because of changes in RGN for devel
  
## Misc:
  * changed maintainer name and address

# SOMbrero 1.2-3

## Fixed bugs:
  * removed unecessary scaling checks for 'relational' in predict
  
## Improvements:
  * used testthat to implement unitary tests
  * remove redundant code in trainSOM
  * fixed seed in unitary tests for reproducibility reasons

## Misc:
  * changed message at start (that might be construed as offensive by 
  colorblinds)

# SOMbrero 1.2-2

## Fixed bugs:
  * fix a bug in plot.somSC for customized colors (reported by Duncan Murdoch)
  * fix a bug in shiny app (downloading CSV files with rownames)
  * fix a bug in 'plot.legend' for SC plot of type 'color'
  
## Improvements:
  * improved rendering of data in shiny app
  * added 'legend' to all SC plots for shiny app
  * added features to display titles (cluster number) in package and shiny app
  * added CSV download for result of the clustering in shiny app
  
## Misc:
  * updated (and improved references)
  * changed maintener's email
  
# SOMbrero 1.2

## Fixed bugs:
  * fixed heskes predictions for relation SOM
  * updated scripts for latest igraph version
  * removed predict.somRes for cosine preprocessing (relational)

## Code improvements:
  * faster version of dissimilarity SOM as in Mariette et al., 2016

# SOMbrero 1.1

## Fixed bugs:
  * Changed observation weights in KORRESP: they now correspond to the 2005 KDISJ
version and not to the 1993 version
  * Fixed bug when passing proto0 (reported by Renaud Dufour)

## Code improvements:
  * changed the function predict.somRes to speed it up

## SOMbrero 1.0 (CRAN first release)

## New features:
  * projectIGraph now handles weighted graphs
  * added a type in plot.somSC that displays a projected graph based on 
super-graph (see Olteanu & Villa-Vialaneix, Journal de la SFdS, 2015)

## Fixed bugs:
  * Fixed a bug related to proto0 when type was not set in trainSOM
  * Fixed a bug in summary.somSC when the number of clusters is not chosen yet
  * Removed parameter 'view' that was not used in internal function 'plotAdd'

## Misc:
  * Changed default neighbourhood to Gaussian. The default parameters for this
neighbourhood are those already implemented in the R package 'yasomi'
  * Changed the internal function projGraph to handle projection for the SOM grid
and for the super-clustering in a single function
  * Added the neighborhood type of the SOM in method print for the class somRes

# SOMbrero 0.5

* New features:
  * Added an option for affectation type ("standard"/hard or "heskes"/soft
affectation)
  * Updated shiny interface accordingly

* Fixed bugs:
  * Fixed a bug related to dist.type in selectNei. This bug did not affect the
learning in previous versions but could provide false values for protoDist
after the training.

# SOMbrero 0.4-2

* New features:
  * Added PCA initialization
  * Added Gaussian neighborhood (default parameters are still tentative)
  * Added cosine normalization for dissimilarity
  * Added ANOVA like analyses for relational SOM interpretation
  * Included a function to project an 'igraph' object on the grid

* Fixed bugs:
  * Fixed a bug in the computation of projected graph. Only edges with non zero
counts for the number of links are now listed in the edge list.

* Misc:
  * Added tests: to check the equivalence between automatic and manual scaling and
the equivalence between predicted clustering and the output of predict
  * Updated vignettes

# SOMbrero 0.4-1

* New features:
  * Added a parameter that tunes the gradient descent step
  * Added a web user interface, implemented with shiny
  * Added the topographic error to somRes.summary
  * Improved the summary of somSC (table of neuron numbers and superclass 
  numbers)

* Code improvements:
  * Changed default value of init.proto to "obs" for "relational" algorithm

* Fixed bugs:
  * Changed scree plot title and ylabel to "proportion of unexplained variables"

* Misc:
  * Added test: equivalence of numeric SOM and relational SOM on squared 
euclidian distance

# SOMbrero 0.4

* New features:
  * Implementation of the "dissimilarity" algorithm to handle dissimilarity data
  * Plots, quality functions and super-clustering have been updated to handle 
tjos case

* Code improvements:
  * New way to handle distance calculations
  * New way to handle the 'korresp' preprocessing (full matrix is now included)
  * Slightly changed default values for Letremy's radius and neighborhood: they
are now exactly the same than in the original programs

* Fixed bugs:
  * Fixed bug with default options in korresp case
  * Fixed bug with affectation step in korresp case
  * Fixed bug for what="add" and type="graph" (node sizes were wrong)

* Misc:
  * Added markdown vignettes: one general and one for each case; hence, the
package now depends on R version 3.0 or higher
  * Added citation file
  * Added demos
  * Removed unecessary tests and examples

# SOMbrero 0.3

* New features:
  * Implementation of the "korresp" algorithm to handle contingency table
  * Plots, quality functions and super-clustering have been updated to handle 
this case

* Fixed bugs:
  * Bug fixed in the dendrogram of super-clustering (percentage of explained
variance was incorrectly displayed)
  * Bug fixed in smooth.dist and umatrix (distances were reversed)

# SOMbrero 0.2-2

* New options:
  * The argument 'the.titles' has been introduced in plot.somRes

* New features:
  * Implementation of a quality function for somRes object
  * Implementation of a super-clustering for the resulting prototypes
  * Implementation of a plot method for the results of the super-clustering

# SOMbrero 0.2

* Name changes:
  * The argument 'scale' of 'somRes' object is now called 'scaling' (conflict 
  with a R base function)

*Argument changes:
  * The argument 'dist.type' has been transfered from 'somRes' to 'myGrid' 
  objects.

* New options:
  * Distance type on the grid   :   maximum, euclidean, manhattan, canberra, 
  binary or minkowski

* New plots:
  * Observations                :   color, lines, barplot, radar, boxplot, and 
                                    names
  * Prototypes                  :   lines, barplot, radar, 3d, poly.dist, 
                                    umatrix, and smooth.disy
  * Additional variable         :   pie, color, lines, barplot, radar, boxplot, 
                                    words, names, and graph

# SOMbrero 0.1

* The initial version of onlineSOM package considers only numerical data.

* Available parameters of the SOM algorithm:
  * Grid                        :   square topology
  * SOM type                    :   numeric
  * SOM mode                    :   online
  * Radius type                 :   letremy
  * Prototypes initialization   :   random or obs
  * Scale                       :   unitvar, center or none

* Available plots:
  * Observations                :   hitmap
  * Prototypes                  :   color
  * Energy
