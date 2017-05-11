## install.packages("designmatch")
library(designmatch)

## To deploy app
## rsconnect::deployApp("/Users/gwynn/Documents/PostDocKleinmanLab/Bins/Shiny/Again")

## The first is used for its parallelplot() function
## The second to add limits to all y-axises, and helps
## us make the final plot.
library(lattice)
library(grid)
library(gridGraphics)

## To add a table to the plot
library(plotrix)

## Used to read in the data
## install.packages("readxl")
library(readxl)
library(shiny)

## Used to get the right colors in
library("RColorBrewer")
mypalette<-brewer.pal(11, "RdBu")

## Functions that are used to copy and paste the parallelplot
## into soemthing that talks to viewports.
latticeGrob <- function(p){
  grob(p=p, cl="lattice")
}

drawDetails.lattice <- function(x, recording=FALSE){
  lattice:::plot.trellis(x$p, newpage=FALSE)
} 


## Function that draws the plot and saves it in a file
## Graphing Ks
makePlot <- function(V, P, D, Tm, NVs){
  ## Tm Number of times we randomize (300 by default)
  ## NVs - number of variables
  ## When there are 2 variables to be plotted on 1 panel it is easier
  ## to do this in layout then in coding below. Number of Panels
  LV = length(V)
  NP = 2 * ceiling(LV/2)
  
  ## No. of full panels is found this way.
  NFP = ifelse(LV%/% 2 == 1, (LV - 3)/2, (LV - 2)/2)
  
  if(LV%/%2 == 1){
    
    layout(matrix(c(0, 1:NP, 0, rep(0, NP + 2)),
                  byrow = TRUE, nrow = 2), 
           widths = c(lcm(0.9), rep(1, NFP), rep(.5, 4), rep(1, NFP), lcm(0.9)),
           heights = c(1, lcm(1)))
  }else{
    
    layout(matrix(c(0, 1:NP, 0, rep(0, NP + 2)),
                  byrow = TRUE, nrow = 2), 
           widths = c(lcm(0.9), rep(1, NFP), rep(.5, 2), rep(1, NFP), lcm(0.9)),
           heights = c(1, lcm(1)))
  }
  
  print(layout.show(NP))
  
  BS = ifelse(LV %% 2 == 0, 2, 4) ## Even number of variables - 1 plot contains
  ## 2 vertical histograms, if not 2 plots have 2 histograms
  
  S = c(rep("R", NFP), rep("B", BS), rep("L", NFP)) ## sequence to tell 
  ## if it's LHS, or RHS plot. 
  
  ## Setting the margins to be 0
  par(mar = rep(0,4))
  
  ## Making the first lot of vertical histograms
  if(NFP > 0){
    for(i in 1:NFP){
      par(mar = rep(0,4))
      
      A = hist(D[,V[[i]][[1]]], plot = FALSE)
      a = range(A$breaks)
      plot(c(0, 1), a, type = "n",
           ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
      
      VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02,
          xwidth = 1,  hist = A, Side = S[i])
      m = range(A$mids)
      axis(2, cex.axis = 0.7, las = 2, col = mypalette[4],
           at = m, labels = as.character(m), tck = 0)
      mtext(P[[2]][i, "L"], side = 1, col = "grey50", cex = 0.5, adj = 0)
    }
  }
  
  ## Making the smaller panels.
  ## There are 2 panels if there are an odd number of variables
  if(LV%/%2 == 1){
    
    ## The first thing on the first panel
    j = floor(LV/2)
    A = hist(D[,V[[j]][[1]]], plot = FALSE)
    
    ## This one gets plotted twice
    k = ceiling(LV/2)
    B = hist(D[,V[[k]][[1]]], plot = FALSE)
    
    ## This is the last thing on the last panel
    l = ceiling(LV/2) + 1
    C = hist(D[,V[[l]][[1]]], plot = FALSE)
    
    ## Plotting A first
    a = range(A$breaks)
    plot(c(0, 1), a, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = A, Side = "R")
    ## Plotting labels
    m = range(A$mids)
    axis(2, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m, labels = as.character(m), tck = 0)
    mtext(P[[2]][j, "L"], side = 1, col = "grey50", cex = 0.5, adj = 0)
    
    ## Plotting B second
    b = range(B$breaks)
    plot(c(0, 1), b, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = B, Side = "L")
    
    ## Plotting labels
    m1 = range(B$mids)
    axis(4, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m1, labels = as.character(m1), tck = 0)
    mtext(P[[2]][k, "L"], side = 1, col = "grey50", cex = 0.5, adj = 1)
    
    ## Plotting B third
    plot(c(0, 1), b, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = B, Side = "R")
    
    ## Plotting labels
    axis(2, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m1, labels = as.character(m1), tck = 0)
    mtext(P[[2]][k, "L"], side = 1, col = "grey50", cex = 0.5, adj = 0)
    
    ## Plotting C last
    d = range(C$breaks)
    plot(c(0, 1), d, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = C,  Side = "L")
    
    ## Plotting labels
    m1 = range(C$mids)
    axis(4, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m1, labels = as.character(m1), tck = 0)
    mtext(P[[2]][l, "L"], side = 1, col = "grey50", cex = 0.5, adj = 1)
    
  }else{
    
    ## This will be plotted first
    j = LV/2
    A = hist(D[,V[[j]][[1]]], plot = FALSE)
    
    ## This will be plotted second
    k = LV/2 +1
    B = hist(D[,V[[k]][[1]]], plot = FALSE)
    
    ## Plotting A first
    a = range(A$breaks)
    plot(c(0, 1), a, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = A, Side = "R")
    
    ## Adding labels
    m = range(A$mids)
    axis(2, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m, labels = as.character(m), tck = 0)
    mtext(P[[2]][j, "L"], side = 1, col = "grey50", cex = 0.5, adj = 0)
    
    ## Plotting B second
    b = range(B$breaks)
    plot(c(0, 1), b, type = "n",
         ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
    VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02/2,
        xwidth = 1,  hist = B, Side = "L")
    
    ## Adding labels
    m1 = range(B$mids)
    axis(4, cex.axis = 0.7, las = 2, col = mypalette[4],
         at = m1, labels = as.character(m1), tck = 0)
    mtext(P[[2]][k, "L"], side = 1, col = "grey50", cex = 0.5, adj = 1)
  }
  
  
  ## Making the last lot of histograms, the left ones 
  if(NFP > 0){
    Lt = which(S == "L")
    for(i in 1:NFP){
      j = Lt[i]
      
      A = hist(D[,V[[j]][[1]]], plot = FALSE)
      a = range(A$breaks)
      plot(c(0, 1), a, type = "n",
           ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
      
      VHV(fillCol = mypalette[5], lineCol = mypalette[4], xscale = .02,
          xwidth = 1,  hist = A, Side = S[j])
      m = range(A$mids)
      axis(4, cex.axis = 0.7, las = 2, col = mypalette[4],
           at = m, labels = as.character(m), tck = 0)
      mtext(P[[2]][j, "L"], side = 1, col = "grey50", cex = 0.5, adj = 1)
    }
  }
  
  grid.echo()
  grid.grab() -> p2
  
  ## Including means of columns and indicator that tells
  ## whether or not it is to be included in plot as mean (light blue)
  ## or regular data point
  CM = colMeans(P[[3]])
  G = c(rep("A", Tm), "B")
  P[[3]] = cbind(rbind(P[[3]], CM), G)
  
  ## Setting the upper values for the parallelplot
  upper <- as.numeric(P[[2]][,"Maxs"])
  
  p1 = parallelplot(~P[[3]][1:NVs], P[[3]], groups = G, horizontal.axis = FALSE,
                    scales = list(x = list(rot = 90),  draw = FALSE,
                                  y = list(draw = FALSE)),
                    col = c("grey50", "black"), lwd = c(1,3), lower = 0, upper = upper,
                    par.settings = list(axis.line = list(col = 0)),
                    panel = function(...) {
                      panel.parallel(...)
                      grid.text(upper,
                                x=unit(1:11, "native"),
                                y=unit(1, "npc") - unit(2, "mm"), 
                                just="top",
                                gp=gpar(col="grey", cex=.7))
                      
                    })
  
  A  = list()
  A[[1]] = p2
  A[[2]] = latticeGrob(p1)
  
  A 
}

## Function used to make vertical histograms that vary.
VHV <- function(xscale = NULL, xwidth, hist, fillCol, lineCol, Side) {
  
  ## Right histograms - histograms that start at the LHS and branch out from the 
  ## right
  if(Side == "R"){
    binWidth <- hist$breaks[2] - hist$breaks[1]
    n <- length(hist$counts)
    x.l <- rep(0, n)
    x.r <- x.l + hist$counts * xscale
    y.b <- hist$breaks[1:n]
    y.t <- hist$breaks[2:(n + 1)]
    rect(xleft = x.l, ybottom = y.b, xright = x.r, ytop = y.t,
         col = fillCol, border = lineCol)
  }
  
  ## Left histograms - histograms that start from the RHS and spread out left
  if(Side == "L"){
    binWidth <- hist$breaks[2] - hist$breaks[1]
    n <- length(hist$counts)
    x.r <- rep(1, n)
    x.l <- x.r - hist$counts * xscale
    y.b <- hist$breaks[1:n]
    y.t <- hist$breaks[2:(n + 1)]
    rect(xleft = x.l, ybottom = y.b, xright = x.r, ytop = y.t,
         col = fillCol, border = lineCol)
  }
}


## reading the data
D = read_excel("SwapOut_Randomization_Hosp_Data_170329.xlsx", sheet=3)[1:140,]
D = as.data.frame(D)

## Removing all the \r\n so that selectInput will work
colnames(D) <- labs <- gsub("\r\n"," ", colnames(D))

## Numeric variables for default inputs
nums = labs[which(sapply(D, is.numeric) == TRUE)]

# Function that does the randomizations
# M is the number of randomizations
# vars is a list of parameters which includes
# D is the data set from which the columns to be matched on are drawn
# names is the file name
# S is the optimisation method

make.Ks = function(M, vars, D, Plot, S, ToC){
  r.I = length(vars)
  No.cols = length(vars[[1]])
  I = matrix(NA, nrow = r.I, ncol = No.cols)
  for(i in 1:r.I) I[i,] = unlist(vars[[i]])
  
  dimnames(I) = list(rep(NULL, r.I), c("cols", "w", "L", "Mins", "Maxs"))
  
  ## No. of participant hospitals
  N = dim(D)[1]
  
  mymat = sdmat = matrix(NA, nrow = N, ncol = r.I)
  
  ## Combining columns
  for(i in 1:r.I) mymat[,i] = D[,I[i,"cols"]]
  
  ## Standardising and adding weights
  col.means = colMeans(mymat)
  col.sds = apply(mymat, 2, sd)
  w = as.numeric(I[,"w"])
  for(i in 1:r.I) sdmat[,i] = 
    w[i]*(mymat[,i] - col.means[i])/col.sds[i]
  
  ## Making the distance matrix
  dist_mat_obj = dist(sdmat, diag = TRUE, upper = TRUE)
  dist_mat = as.matrix(dist_mat_obj)
  
  ## Telling the computer wich method to use to solve
  t_max = 60*5
  solver = S
  approximate = 0
  solver = list(name = solver, t_max = t_max, approximate = approximate, 
                round_cplex = 0, trace_cplex = 0)
  
  ## Solving
  out = nmatch(dist_mat = dist_mat, total_pairs = floor(N/2), 
               solver = solver, subset_weight = NULL)
  
  # These guys have the row numbers of the matched pairs
  id_1 = out$id_1  
  id_2 = out$id_2 
  
  ## If there are any leftovers they get assigned where
  ## the user wants them assigned
  X = dim(D)[1] %% 2 == 0
  if(X == FALSE) {LO = sum(1:dim(D)[1]) - sum(id_1, id_2)}
  
  ## Will be used in the loop below - a place to put Ks, 
  ## filled below, then graphed after using parallel coordinate plot
  Ks = matrix(NA, nrow = M, ncol = r.I)
  
  ## Used to run the loop below
  M.seq = seq_along(1:M)
  
  for(i in M.seq){
    ## Randomising once - 0 ctl, 1 trt, (subtract one because when 
    ## making Trt below the middle step has to identify the length of 
    ## Trt or it gets fussy and won't add on the last value)
    R = replicate(length(id_1), rbinom(1, size = 1, prob = 0.5))
    S = 1 - R
    
    ## The 1 is for the leftover row that goes to the trt arm
    Trt = c(R, S)
    
    ## Making the data, TA is included and goes to the trt arm
    if (X == TRUE) {
      Dt = data.frame(mymat[c(id_1, id_2),], Trt)
    } else {
      Dt = data.frame(mymat[c(id_1, id_2, LO),], c(Trt, (ToC - 1)))
    }
    
    ## Picking out trt and ctl covariates
    Trt.CV = Dt[which(Trt == 1), 1:r.I]
    Ctl.CV = Dt[-which(Trt == 1), 1:r.I]
    
    Ks[i,] = abs((apply(Trt.CV, 2, sum) - 
                    apply(Ctl.CV, 2, sum))/length(R))
  }## ending for i in M.seq
  
  ## So the labels in parcoord come out nicely
  ## NEW colnames here
  colnames(Ks) = I[,'L']
  
  P = list()
  P[[1]] = vars
  P[[2]] = I
  P[[3]] = as.data.frame(Ks)
  
  P
} ## closing the function make.Ks

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Universe of Randomizations: Graphic"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(id = "inTabset",
    tabPanel("General Inputs",
             
             sidebarPanel(
               
               helpText("Input the number of variables to be considered in this 
                        randomization scheme, the number of times we will randomize, 
                        and if leftover units should be in treatment or control arm.
                        The next tab will be used to build the rest of the plot."),
               
               sliderInput("Times",
                           "No of times we randomize:",
                           min = 50,  max = 500, value = 300),
               
               helpText("This controls the number of inputs on the next tab."),
               numericInput("NoVars","No. of matching variables", 
                            value = 3, min = 2, max = dim(D)[2]),
               
               helpText("If there are an odd number of units to randomize 
                        the remainder can be in either treatment or control."),
               radioButtons("ToC", label = h3("Treatment or Control"),
                            choices = list("Treatment" = 2, "Control" = 1), 
                            selected = 2),
               actionButton("Tab2", "Go on to the next tab")
             )),
    
    tabPanel("Matching Variables Info",
             value = "panel2",
             sidebarPanel(
               helpText("Choose variables below to be randomized, then input labels, 
                        weights, and minimum and maximum for the y-axis for that 
                        variable. After you've finished hit the Go button to
                        see the plot. The blue line in the top plot is the mean 
                        difference in the two arms and the bottom plots are 
                        vertical histograms of the raw data, in red."),
               
               br(),
               
               actionButton("go", "Go", width = "150px", style = "background-color:red"),
               br(),
               
               helpText("Once pleasing weights are found, hit this button to share
                        the inputs with others."),
               br(),
               
               bookmarkButton(id = "bookmark1"),
               
               uiOutput("VarsInput")
             )),
    
    tabPanel("Summaries",
             sidebarPanel(
               helpText("Below are summaries of the raw data of the columns choosen 
                        in the randomization tab."), 
               verbatimTextOutput("summary")
             )),
    
    tabPanel("Download",
             sidebarPanel(
               helpText("Use this page to download both graphs, and all inputs
                        used to make them."), 
               textInput("FN", label = h3("File name:"), 
                         value = "File name here"),
               textInput("YN", label = h3("Your name:"), 
                         value = "Your name here"),
               radioButtons('format', 'Document format', c('PDF', 'Word'),
                            inline = TRUE),
               downloadButton('downloadReport',"Generate report")
               )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  K <- reactive({
    input$NoVars
  })
  
  output$VarsInput <- renderUI({
    C = sapply(1:K(), function(i){paste0("cols",i)})
    L = sapply(1:K(), function(i){paste0("label",i)})
    W = sapply(1:K(), function(i){paste0("weight",i)})
    S = sapply(1:K(), function(i){paste0("slider",i)})
    
    output = tagList()
    
    for(i in seq_along(1:K())){
      output[[i]] = tagList()
      output[[i]][[1]] = br()
      output[[i]][[2]] = hr(style="height:5px;background-color:blue")
      output[[i]][[3]] = helpText("Input information for a variable below:")
      output[[i]][[4]] = selectInput(C[i], "Variable to randomize:",
                                     labs, selected = nums[i])
      output[[i]][[5]] = textInput(L[i], "Label for variable:", 
                                   value = "Label for variable here")
      output[[i]][[6]] = textInput(W[i], "Weight for variable:",
                                   value = "1")
      output[[i]][[7]] = textInput(S[i], "Max for variable",
                                   value = "5")
    } ## for loop
    
    output
  })
  
  Dat <- eventReactive(input$go, {
    
    C = sapply(1:K(), function(i){input[[paste0("cols",i)]]})
    L = sapply(1:K(), function(i){input[[paste0("label",i)]]})
    W = sapply(1:K(), function(i){input[[paste0("weight",i)]]})
    S = sapply(1:K(), function(i){input[[paste0("slider",i)]]})
    
    V = list()
    for(i in 1:K()){
      V[[i]] = list()
      V[[i]][[1]] = C[i]
      V[[i]][[2]] = as.numeric(W[i])
      V[[i]][[3]] = L[i]
      V[[i]][[4]] = 0
      V[[i]][[5]] = as.numeric(S[i])
    }
    
    make.Ks(M = input$Times, D = D, vars = V, ToC = isolate({input$Toc}), S = "glpk")
  }) ## eventReactive
  
  observeEvent(input$Tab2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  output$plot <- renderPlot({
    A = makePlot(V = Dat()[[1]], P = Dat(), D = D, Tm = input$Times, NVs = K())
    
    ## Using pushViewport to combine the lovely graphs and make one big one
    grid.newpage()
    pushViewport(viewport(x=0, y=0.02, width=1, height=.25,
                          just=c("left", "bottom")))
    grid.draw(A[[1]])
    upViewport()
    
    
    pushViewport(viewport(x=0, y=0.17, width=1, height=.83,
                          just=c("left", "bottom")))
    drawDetails.lattice(A[[2]])
    upViewport()
    
  }) ## renderPlot
  
  ## Summary of data
  output$summary <- renderPrint({
    C <- sapply(1:K(), function(i) {input[[paste0("cols",i)]]})
    
    summary(D[, C])
  })
  
  # observe({
  #   input$Go <- isolate(input$go)
  # })
  # 
  # onBookmark(function(state) {
  #   state$input$Go <- input$Go
  #   # state$Tab2 <- input$Tab2
  # })
  # 
  # onRestore(function(state) {
  #   input$go <- state$input$Go 
  #   #input$Tab2 <- state$Tab2
  # })
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$FN, sep = '.', switch(
        input$format, PDF = 'pdf', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), Word = word_document()
      ))
      
      file.rename(out, file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")


