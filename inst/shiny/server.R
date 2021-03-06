###############################################################################
# Server
shinyServer(function(input, output, session) {

  #############################################################################
  ## Server variables
  RVserver.env <- reactiveValues(current.som = NULL, current.call = NULL) # used to allocate in functions
  val <- reactiveValues(data=NULL, dataadd=NULL)  

  #### Panel 'Self Organize' 
  ############################################################################## 

  ####### bsPanel 'Type of algorithm' 
  ############################################################################## 
  output$typealgo <- renderUI({
    if(input$somtype==""){
      text <- "1. Type of algorithm"
    } else {
      text <- paste("1. Type of algorithm, selected : ", input$somtype)
    }
    HTML(text)
  })
  
  observeEvent(input$somtype, {
    if(input$somtype!=""){
      shinyBS::updateCollapse(session, "collapsestep1", open = "bscoll2")
    }
  })

  ####### bsPanel 'Data preparation'
  ##############################################################################

  output$texttypedata <- renderUI({
    switch(input$somtype,
           "numeric" = HTML("You have chosen the Numeric algorithm. Your data is expected to be a dataset with numeric variables. You can take <b>'iris'</b> as an example."),
           "korresp" = HTML("You have chosen the Korresp algorithm. Your data is expected to be a contengency table between two factors. You can take <b>'presidentielles2002'</b> as an example."),
           "relational" = HTML("You have chosen the numeric algorithm. Your data is expected to be a dissimilarity matrix, square, symetric and with a null diagonal. You can take <b>'dissim.lesmis'</b> as an example."),
           ""
    )
  })

  #### Panel 'Import data'
  ##############################################################################
  
  observeEvent(input$loaddatabutton, {
    val$data <- get(input$file1envir, envir = .GlobalEnv)
  }, ignoreInit=T)
  
  observeEvent(c(input$file1, input$sep, input$quote, input$dec, input$header, input$rownames), {
    the.sep <- switch(input$sep, "Comma"=",",
                      "Semicolon"=";",
                      "Tab"="\t",
                      "Space"="")
    the.quote <- switch(input$quote, "None"="",
                        "Double Quote"='"',
                        "Single Quote"="'")
    the.dec <- switch(input$dec,
                      "Period"=".",
                      "Comma"=",")
    if (input$rownames) {
      the.table <- read.table(input$file1$datapath, header=input$header,
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
    } else {
      the.table <- read.table(input$file1$datapath, header=input$header,
                              sep=the.sep, quote=the.quote, dec=the.dec)
    }
    val$data <- the.table
  }, ignoreInit=T)
  
  
  output$dataready <- renderText({
    text <- "No data loaded"
    if(is.null(val$data)==F){
      text <- "Preview of the data"
    } 
    text
  })
  
  dInput <- reactive({
    if (is.null(val$data))
      return(NULL)
    
    the.table <- val$data
    
    output$varchoice <- renderUI(
      selectInput(inputId="varchoice", label="Input variables:", multiple=T,
                         choices=as.list(colnames(the.table)),
                         selected=as.list(colnames(the.table)[
                           sapply(the.table, class) %in%
                             c("integer", "numeric")])))

    # update the map dimensions
    updateNumericInput(session, inputId="dimx", value={
      if (input$somtype =="korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })
    updateNumericInput(session, inputId="dimy", value={
      if (input$somtype =="korresp") {
        max(5,min(10,ceiling(sqrt((nrow(the.table)+ncol(the.table))/10))))
      } else max(5,min(10,ceiling(sqrt(nrow(the.table)/10))))
    })
    
    # update the max. iterations option
    updateNumericInput(session, "maxit", value=5 * nrow(the.table))

    # return the table
    the.table
  })
  
  # data preview table
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  }, rownames = TRUE, spacing='xs')

  output$missingrows <- renderText({
    shiny::validate(need(is.null(dInput())==F, 'Choose data'))
    nrowmissing <- nrow(dInput())-input$nrow.preview
    ncolmissing <- ncol(dInput())-input$ncol.preview
    if(nrowmissing>0 & ncolmissing<=0){
      text <- paste(nrowmissing, "rows not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing<=0 & ncolmissing>0){
      text <- paste(ncolmissing, "cols not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing>0 & ncolmissing>0){
      text <- paste(nrowmissing, "rows  and", ncolmissing, "columns not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else {
      text <- NULL
    }
    text
  })
  
  #### Panel 'Self-organize'
  #############################################################################

  observeEvent(input$showadvlink, {
    shinyjs::toggleElement(id='divadvancedoptions')
  })
  
  observe({
    # update the scaling option when input$somtype is changed
    updateSelectInput(session, inputId="scaling",  
                  choices=switch(input$somtype,
                                 "numeric"=c("unitvar", "none", "center"),
                                 "korresp"=c("chi2"),
                                 "relational"=c("none","cosine")),
                  selected=switch(input$somtype, "numeric"="unitvar",
                                  "korresp"="chi2", "relational"="none")
                  )
    
    # update the initialization method when input$somtype is changed
    updateSelectInput(session, "initproto", 
                      selected=switch(input$somtype, 
                                      "numeric"="random",
                                      "korresp"="random",
                                      "relational"="obs"))
  })

  # update the distance option when input$radiustype is changed
  observe({
    if(input$topo=="square"){
      updateSelectInput(session, inputId="disttype", label="Distance scaling:",
                        choices=switch(input$radiustype,
                                       "letremy"=c("letremy", "maximum", "euclidean",
                                                   "manhattan", "canberra", "binary",
                                                   "minkowski"),
                                       "gaussian"=c("maximum", "euclidean", 
                                                    "manhattan", "canberra", "binary",
                                                    "minkowski")),
                        selected=switch(input$radiustype, "letremy"="letremy",
                                        "gaussian"="euclidean"))
    } else {
      updateSelectInput(session, inputId="disttype", label="Distance scaling (only euclidean authorized for hexagonal topology):",
                        choices = "euclidean", selected="euclidean")
    }
  })
  
  observe({
    if(is.null(input$somtype) | is.null(dInput())){
      shinyjs::disable("trainbutton")
    } else {
      shinyjs::enable("trainbutton")
    }
  })
  
  # Train the SOM when the button is hit
  observeEvent(input$trainbutton, {
    RVserver.env$current.som <- trainTheSom(dInput(), input$somtype, 
                                                    input$topo,
                                                    input$dimx, input$dimy, 
                                                    input$affectation,
                                                    input$disttype, input$maxit, 
                                                    varnames=input$varchoice, 
                                                    rand.seed=input$randseed, 
                                                    scaling=input$scaling, 
                                                    eps0=input$eps0, 
                                                    init.proto=input$initproto, 
                                                    nb.save=input$nb.save,
                                                    radiustype=input$radiustype)
    
    unused <- colnames(dInput())
    unused <- unused[-match(input$varchoice, unused)]
    if(length(unused)>0){
      updateSelectInput(session, "unusedvar", choices=unused, selected = unused)
    }
        
    if(is.null(input$file1)==F){
      namedata <- "data"
    } else {
      namedata <- input$file1envir
    }
    RVserver.env$current.call <- paste0("set.seed(", input$randseed, ")\n",
                                         "mysom <- trainSOM(", namedata,
                                         "[,c('", paste(input$varchoice, collapse="', '"), "')],\n",
                                         "type='", input$somtype, "', ",
                                         "topo='", input$topo, "', ",
                                         "dimension=c(", input$dimx, ",", input$dimy, "),\n",
                                         "affectation='", input$affectation, "', ",
                                         "dist.type='", input$disttype, "', ",
                                         "maxit=", input$maxit, ", ", 
                                         "scaling='", input$scaling, "',\n",
                                         "init.proto='", input$initproto, "', ", 
                                         "nb.save=", input$nb.save, ", ",
                                         "radius.type='", input$radiustype, "', ",
                                         "eps0=", input$eps0,
                                         ")\n")
      updatePlotSomVar() # update variable choice for som plots
      updatePlotScVar() # update variable choice for sc plots
  })
  
  output$runcodesom <- renderText({
        shiny::validate(need(input$somtype!="", "Choose a type of algorithm."))
        shiny::validate(need(is.null(val$data)==F, "First import a dataset."))
        shiny::validate(need(input$trainbutton!=0, "Hit the Train button to train the map."))
        shiny::validate(need(is.null(RVserver.env$current.call)==F, "Hit the Train button to train the map."))
        RVserver.env$current.call
    })

  # Render the summary of the SOM
  output$summary <- renderPrint({
    shiny::validate(need(input$somtype!="", "Choose a type of algorithm."))
    shiny::validate(need(is.null(val$data)==F, "First import a dataset."))
    shiny::validate(need(input$trainbutton!=0, "Hit the Train button to train the map."))
    shiny::validate(need(is.null(RVserver.env$current.som)==F, "Hit the Train button to train the map."))
    summary(RVserver.env$current.som)
  })

  observeEvent(RVserver.env$current.som, {
    if(is.null(RVserver.env$current.som)){
      shinyjs::disable("som.download")
      shinyjs::hide("nextplot")
    } else {
      shinyjs::enable("som.download")
      shinyjs::show("nextplot")
    }
  })
  
  # Output the computed som object to be downloaded
  output$som.download <- {
    downloadHandler(filename=function() {
        paste0("som",format(Sys.time(),format="%Y%m%d_%H%M"),".rda",sep="")
      },
      content=function(file) {
        som.export <- RVserver.env$current.som
        save(som.export, file=file)
      })
  }
  
  observeEvent(RVserver.env$current.som$clustering, {
    if(is.null(RVserver.env$current.som$clustering)){
      shinyjs::disable("clustering.download")
    } else {
      shinyjs::enable("clustering.download")
    }
  })
  
  output$clustering.download <- {
    downloadHandler(filename = function() {
      paste0("clustering", format(Sys.time(), format="%Y%m%d_%H%M"),
             ".txt")
    },
    content = function(file) {
      som.export <- data.frame("name" = names(RVserver.env$current.som$clustering),
                               "cluster" = RVserver.env$current.som$clustering)
      write.table(som.export, file = file, row.names = FALSE, sep = "\t")
    })
  }
  
  observeEvent(input$nextplot, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "PlotMap")
  }) 
  
  #### Panel 'Plot'
  ##############################################################################
  
  # Adapt plottype to the somtype and the "what" arguments
  observe({
    choices <- all.somplot.types[[input$somtype]][[input$somplotwhat]]
    selected <- isolate(input$somplottype)
    if(selected %notin% choices) selected <- choices[1]
    updateSelectInput(session, "somplottype", 
                      choices=choices, 
                      selected = selected)
  })
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    tmp.names <- colnames(RVserver.env$current.som$data)
    if (input$somtype =="korresp"){
      if(input$somplotrowcol=="r"){
        tmp.names <- rownames(RVserver.env$current.som$data)
      }
    }
    updateSelectInput(session, "somplotvar", choices=tmp.names)
    updateSelectInput(session, "somplotvar2", choices=tmp.names, selected=tmp.names)
  })
  
 
  varsomplot <- reactive({
    tmp.var <- 1
    if(input$somplottype %in% c("color", "3d")){
      tmp.var <- input$somplotvar
    }
    if(input$somplottype %in% c("boxplot", "barplot", "lines", "meanline")){
      tmp.var <- input$somplotvar2
    }
    if(input$somplottype == 'names'){
      tmp.var <- "row.names"
    }
    tmp.var
  })
  
  
  output$runcodeplot <- renderText({
    variable <- NULL
    if(input$somplottype  %in% c("lines", "meanline", "barplot", "boxplot", "color", "3d")){
      variable <- varsomplot()
    }
    
    codeplot <- paste0("plot(mysom, ",
                       "what='", input$somplotwhat, "'")
    if(input$somplottype != "energy"){
      codeplot <- paste0(codeplot, ", type='", input$somplottype, "'")
    }
    if(input$somplottype %in% c("lines", "meanline", "barplot", "boxplot", "color", 
                                "3d", "umatrix", "names")){
      codeplot <- paste0(codeplot, ", show.names=", input$somplottitle)
    }
    if(input$somplottype  %in% c("lines", "meanline", "barplot", "boxplot", "color", "3d")){
      if(length(variable)==1) {
        textevar <- paste0("'", variable, "'")
      } else {
        textevar <- paste0("c('", paste(variable, collapse="','"), "')")
      }
      codeplot <- paste0(codeplot, ",\nvariable=", textevar)
    }
    if(input$somtype == "korresp" & 
       input$somplottype %in% c("lines", "meanline", "barplot", "boxplot", "color", "3d")){
      codeplot <- paste0(codeplot, ",\nview='", input$somplotrowcol, "'")
    }
    if(input$somplottype =="3d"){
      codeplot <- paste0(codeplot, ",\ntheta=", input$theta, ", phi=", input$phi)
    }
    codeplot <- paste0(codeplot, ")")
    if(input$somplotlegend==T){
      codeplot <- paste0(codeplot, " + ggplot2::theme(legend.position = 'none')")
    }
    codeplot <- paste0(codeplot, "\n")
    codeplot
  })

  # Plot the SOM
  output$somplot <- renderPlot({
    # Add validates to wait until the var inputs are updated
    shiny::validate(need(is.null(dInput())==F, "No SOM trained (See 'Self Organize' tab)."))
    shiny::validate(need(is.null(RVserver.env$current.som)==F, "No SOM trained (See 'Self Organize' tab)."))
    shiny::validate(need(input$somplottype %in% all.somplot.types[[input$somtype]][[input$somplotwhat]],
                         "wait..."))
    if(input$somplottype %in% c("boxplot", "barplot", "lines", "meanline"))
      shiny::validate(need(identical(varsomplot(), input$somplotvar2), "wait..."))
    if(input$somplottype %in% c("color", "3d"))
      shiny::validate(need(identical(varsomplot(), input$somplotvar), "wait..."))
    if(input$somplottype %in% c("names"))
      shiny::validate(need(identical(varsomplot(), "row.names"), "wait..."))
    
    p <- plot(x=RVserver.env$current.som, what=input$somplotwhat, type=input$somplottype,
                    variable=varsomplot(), show.names=input$somplottitle,
                    view=input$somplotrowcol, theta = input$theta, phi=input$phi)
    if(input$somplotlegend==T){
      p <- p + ggplot2::theme(legend.position = "none")
    }
    p
  })
  
  #### Panel 'Superclass'
  ##############################################################################
  # Input number of superclasses or cutting height
  output$scHorK <- renderUI(
    switch(input$sc.cut.choice, 
           "Number of superclasses"=
             numericInput("sc.k", "Number of superclasses:", 2, min=2,
                          max=max(input$dimx*input$dimy-1, 2)), 
           "Height in dendrogram"=
             numericInput("sc.h", "Height in dendrogram:", 10, min=0))
  )

  # Compute superclasses when the button is hit
  computeSuperclasses <- reactive({
    if (is.null(dInput()) | is.null(RVserver.env$current.som))
      return(NULL)
    if (input$superclassbutton==0) {
      superClass(sommap=RVserver.env$current.som, method=input$scmethod)
    } else {
      isolate(switch(input$sc.cut.choice,
                            "Number of superclasses"=
                              superClass(sommap=RVserver.env$current.som, k=input$sc.k, method=input$scmethod),
                            "Height in dendrogram"=
                              superClass(sommap=RVserver.env$current.som, h=input$sc.h, method=input$scmethod))
      )
    }
  })

  output$sc.summary <- renderPrint({
    if (input$superclassbutton==0)
      return("Hit the 'Compute superclasses' button to see the results.")
    tmp.sc <- computeSuperclasses()
    summary(tmp.sc)
  })

  observeEvent(c(computeSuperclasses(), input$superclassbutton), {
    if(is.null(computeSuperclasses()) | input$superclassbutton==0){
      shinyjs::disable("sc.download")
      shinyjs::hide("nextplotsc")
      codesc <- paste0("superClass(sommap=mysom, method='", input$scmethod, "')")
    } else {
      shinyjs::enable("sc.download")
      shinyjs::show("nextplotsc")
      if(input$sc.cut.choice=="Number of superclasses"){
        codesc <- paste0("superClass(sommap=mysom, method='", input$scmethod, "', k=", input$sc.k, ")")
      } else {
        codesc <- paste0("superClass(sommap=mysom, method='", input$scmethod, "', h=", input$sc.h, ")")
      }
    }
    output$runcodesc <- renderText({ codesc })
  })
  
  observeEvent(input$nextplotsc, {
    shinyBS::updateCollapse(session, "collapsesuperclass", open = "plotsc")
  })
  
  # Download the superclass classification
  output$sc.download <- {
    downloadHandler(
      filename=function() {
        paste0("superclasses", format(Sys.time(), format="%Y%m%d_%H%M"),
              ".csv", sep="")
      },
      content=function(file) {
        the.sc <- computeSuperclasses()
        classes.export <- data.frame(obs=row.names(the.sc$som$data),
                                     cluster=the.sc$cluster[
                                       the.sc$som$clustering])
        write.csv(classes.export, file=file, row.names=FALSE)
      })
  }

  # Adapt scplottype to the somtype and the "what" arguments
  observe({
    choices <- all.scplot.types[[input$somtype]][[input$scplotwhat]]
    selected <- isolate(input$scplottype)
    if(selected %notin% choices) selected <- choices[1]
    updateSelectInput(session, "scplottype", choices=choices, selected=selected)
  })
  
  # update variables available for plotting
  updatePlotScVar <- function() observe({
    tmp.names <- colnames(RVserver.env$current.som$data)
    if (input$somtype =="korresp")
      if(input$scplotrowcol=="r")
      tmp.names <- rownames(RVserver.env$current.som$data)
    updateSelectInput(session, "scplotvar", choices=tmp.names)
    updateSelectInput(session, "scplotvar2", choices=tmp.names, 
                      selected=tmp.names[1:length(tmp.names)])
  })
  
  # Update SuperClass plot
  output$somplotscdendro <- renderPlot({
    shiny::validate(need(is.null(dInput)==F, "No SOM trained (See 'Self Organize' tab)."))
    shiny::validate(need(is.null(RVserver.env$current.som)==F, "No SOM trained (See 'Self Organize' tab)."))
    plot(computeSuperclasses(), type="dendrogram")
  })
  
  output$runcodescplot <- renderText({
    if (input$scplottype  %in% c("lines", "meanline", "barplot", "boxplot")) {
      tmp.var <-  input$scplotvar2
    } else tmp.var <- input$scplotvar
    
    codeplot <- paste0("plot(mysomSC, ",
                       "what='", input$scplotwhat, "', ",
                       "type='", input$scplottype, "'")
    if(input$scplottype  %in% c("lines", "meanline", "barplot", "boxplot", "color")){
      if(length(tmp.var)==1) {
        textevar <- paste0("'", tmp.var, "'")
      } else {
        textevar <- paste0("c('", paste(tmp.var, collapse="',"), "')")
      }
      codeplot <- paste0(codeplot, ",\nvariable=", textevar)
    }
    if(input$somtype == "korresp"  & input$scplottype %in% c("lines", "meanline", "barplot", "boxplot", "color")){
      codeplot <- paste0(codeplot, ",\nview='", input$scplotrowcol, "'")
    }
    if(input$scplottype =="dendro3d"){
      codeplot <- paste0(codeplot, ",\nangle=", input$scangle3d)
    }
    codeplot <- paste0(codeplot, ")\n")
    codeplot
  })
  
  # Update SuperClass plot
  output$scplot <- renderPlot({
    shiny::validate(need(input$scplottype %in% all.scplot.types[[input$somtype]][[input$scplotwhat]],
                  "wait..."))
    if(is.null(dInput()))
      return(NULL)
    the.sc <- computeSuperclasses()
    if(input$superclassbutton ==0)
      return(NULL)

    if (input$scplottype %in% c("dendrogram"))
      return(plot(the.sc, type=input$scplottype))
    
    if (input$scplottype %in% c("dendro3d"))
      return(plot(the.sc, type=input$scplottype, angle = input$scangle3d))

    if (input$scplottype %in% c("lines", "meanline", "barplot", "boxplot")) {
      tmp.var <-  input$scplotvar2
    } else tmp.var <- input$scplotvar
    
    plot(x = the.sc, what = input$scplotwhat, type = input$scplottype,
         variable = tmp.var, view = input$scplotrowcol)

  })

  #### Panel 'Combine with additional data'
  ##############################################################################
  
  observeEvent(input$loadunusedvarbutton, {
    val$dataadd <- data.frame(dInput()[,c(input$unusedvar)])
    colnames(val$dataadd)<- input$unusedvar
  }, ignoreInit=T)
  
  observeEvent(input$loaddatabuttonadd, {
    val$dataadd <- get(input$file2envir, envir = .GlobalEnv)
  }, ignoreInit=T)
  
  observeEvent(c(input$file2, input$sep2, input$quote2, input$dec2, input$header2, input$rownames2), {
    the.sep <- switch(input$sep2, "Comma"=",", "Semicolon"=";", "Tab"="\t", "Space"="")
    the.quote <- switch(input$quote2, "None"="","Double Quote"='"', "Single Quote"="'")
    the.dec <- switch(input$dec2, "Period"=".", "Comma"=",")
    
    if (input$rownames2) {
      the.table <- read.table(input$file2$datapath, header=input$header2, 
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
    } else the.table <- read.table(input$file2$datapath, header=input$header2, 
                                   sep=the.sep, quote=the.quote, dec=the.dec)
    val$dataadd <- the.table
  }, ignoreInit=T)
  
  # File input for additional variables
  dInputAdd <- reactive({
    if (is.null(val$dataadd))
      return(NULL)
    
    the.table <- val$dataadd
    shiny::validate(need(nrow(val$data)==nrow(val$dataadd), 
                         "Additional data doesn't have the same number of rows than the data used for SOM"))
    updateAddPlotVar() # update variable selector    
  
    the.table
  })
  
  output$data2ready <- renderText({
    text <- "No data loaded"
    if(is.null(val$dataadd)==F){
      text <- "Preview of the data"
    } 
    text
  })
  
  # additional data preview table
  output$addview <- renderTable({
    shiny::validate(need(is.null(RVserver.env$current.som)==F, "No SOM trained (See 'Self Organize' tab)."))
    d.input <- dInputAdd()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview.add) 
      d.input <- d.input[,1:input$ncol.preview.add]
    head(d.input, n=input$nrow.preview.add) 
  })
  
  # Adapt available variables from second file
  updateAddPlotVar <- function() observe({
    d.input <- dInputAdd()
    if(is.null(d.input))
      return(NULL)
    updateSelectInput(session, "addplotvar", choices=colnames(d.input), 
                      selected=colnames(d.input)[1])
    updateSelectInput(session, "addplotvar2", choices=colnames(d.input), 
                      selected=colnames(d.input)[1:min(5,ncol(d.input))])
  })
  
  output$missingrowsadd <- renderText({
    if(is.null(RVserver.env$current.som)) return(NULL)
    shiny::validate(need(is.null(dInputAdd())==F, 'Choose data'))
    nrowmissing <- nrow(dInputAdd())-input$nrow.preview
    ncolmissing <- ncol(dInputAdd())-input$ncol.preview
    if(nrowmissing>0 & ncolmissing<=0){
      text <- paste(nrowmissing, "rows not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing<=0 & ncolmissing>0){
      text <- paste(ncolmissing, "cols not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else if(nrowmissing>0 & ncolmissing>0){
      text <- paste(nrowmissing, "rows  and", ncolmissing, "columns not shown in the preview (the map will be based on the full dataset)", sep=" ")
    } else {
      text <- NULL
    }
    text
  })
  
  output$runcodeaddplot <- renderText({
    if(input$addplottype == "graph"){
      codeplot <- "adjBin <- as.matrix(dataAdd!=0)\n"
      codeplot <- paste0(codeplot, "tmpGraph <- graph.adjacency(adjBin, mode='undirected')\n")
      codeplot <- paste0(codeplot, "plot(mysom, what='add', ",
                         "type='", input$addplottype, "',",
                         "\nvariable=tmpGraph)")
    } else {
      if (input$addplottype %in% c("pie","color","names")) {
        tmp.var <- input$addplotvar
      } else tmp.var <- input$addplotvar2
      
      codeplot <- paste0("plot(mysom, what='add', ",
                         "type='", input$addplottype, "'")
      if(input$addplottype  %in% c("lines", "meanline", "barplot", "boxplot", "color", "words", "pie")){
        if(length(tmp.var)==1) {
          textevar <- paste0("dataAdd$", tmp.var)
        } else {
          textevar <- paste0("dataAdd[,c('", paste(tmp.var, collapse="',"), "')]")
        }
        codeplot <- paste0(codeplot, ",\nvariable=", textevar)
      }
      codeplot <- paste0(codeplot, ")\n")
    }
    codeplot
  })
  
  # function to render Additional data Plot
  output$addplot <- renderPlot({
    shiny::validate(need(is.null(RVserver.env$current.som)==F, "No SOM trained (See 'Self Organize' tab)."))
    dataAdd <- dInputAdd()
    if (is.null(dataAdd)) return(NULL)
    
    if (input$addplottype !="graph") {
      if (input$addplottype %in% c("pie","color","names")) {
        tmp.var <- input$addplotvar
      } else tmp.var <- input$addplotvar2
      plot(x=RVserver.env$current.som, what="add", type=input$addplottype, 
           variable=data.frame(dataAdd[,tmp.var]), varname=paste0("dataAdd$", tmp.var))
    } else {
      adjBin <- as.matrix(dataAdd!=0)
      tmpGraph <- graph.adjacency(adjBin, mode="undirected")
      
      plot(RVserver.env$current.som, what="add", type="graph", variable=tmpGraph)
    }
  })
})
