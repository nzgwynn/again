## This app does not used the vertical histograms in the graph, it also does not 
## save the number of vars that you input on the second page. In addition, the setwd()
## thing to download a report is also not working, but reports download fine. 

library(designmatch)

## To deployApp
## rsconnect::deployApp("/Users/gwynn/Documents/PostDocKleinmanLab/Bins/Shiny/Again")

## The first is used for its parallelplot() function
## The second to add limits to all y-axises
library(lattice)
library(grid)
library(ggplot2)
library(GGally)

## To add a table to the plot
library(plotrix)

## Used to read in the data
## install.packages("readxl")
library(readxl)
library(shiny)

## Used to get the right colors in
library("RColorBrewer")
M = 9 # Max number of colors allowed in pallete

## Function that draws the plot and saves it in a file
## Graphing Ks
make.order = function(column, data){
  data = data[order(data[, column]),] 
  ## Grouping for color
  G = factor(rep(1:M, each = (dim(data)[1]%/%M + 1))[1:dim(data)[1]])
  data = cbind(data, G)
}

make.plot = function(data, I){
  upper <- as.numeric(I[,"Maxs"])
  lower <- as.numeric(I[,"Mins"])
  N <- length(upper)
  
  for(i in 1:N){
    data[[i]] <- data[[i]]/upper[i]
  }
  
  # Basic plot to update
  p = ggparcoord(data, columns = 1:N, groupColumn = (N + 1), scale = "globalminmax", 
                 shadeBox = NULL) + coord_cartesian(ylim = c(0,1))
  
  p <- p + scale_colour_brewer(palette = "YlGnBu")
  
  # Start with a basic theme
  p <- p + theme_minimal()
  
  # Decrease amount of margin around x, y values
  p <- p + scale_y_continuous(expand = c(0.02, 0.02))
  p <- p + scale_x_discrete(expand = c(0.02, 0.02))
  
  # Remove axis ticks and labels
  p <- p + theme(axis.ticks = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(axis.text.y = element_blank())
  
  # Clear axis lines
  p <- p + theme(panel.grid.minor = element_blank())
  p <- p + theme(panel.grid.major.y = element_blank())
  
  # Removing the legend
  p <- p + theme(legend.position="none")
  
  # Adding a border
  p <- p + theme(panel.border = element_rect(colour = "darkgrey", 
                                             fill=NA, size=0.5))
  
  # Calculate label positions for each veritcal bar
  lab_x <- rep(1:(N), times = 2) # 2 times, 1 for min 1 for max
  lab_y <- rep(c(0, 1), each = (N))
  
  # min and max values from original dataset
  lab_z <- c(rep(0, N), upper)
  
  # Convert to character for use as labels
  lab_z <- as.character(lab_z)
  
  # Add labels to plot
  p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
  
  # Display parallel coordinate plot
  print(p)
}


make.Ks = function(M, vars, D, name, Plot, S, ToC){
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
  out = designmatch::nmatch(dist_mat = dist_mat, total_pairs = floor(N/2), 
               solver = solver, subset_weight = NULL)
  
  # These guys have the row numbers of the matched pairs
  id_1 = out$id_1  
  id_2 = out$id_2 
  Ms = data.frame(cbind(id_1, id_2))
  colnames(Ms) = c("Row number","Match")
  
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
    Trt = c(R, S)
    
    ## Making the data
    if (X == TRUE) {
      
      ## If nothing is leftover we don't need ToC
      Dt = data.frame(mymat[c(id_1, id_2),], Trt)
    } else {
      
      ## If anything is, then we do
      ## ToC is 2 for trt and 1 for control, added at end
      Dt = data.frame(mymat[c(id_1, id_2, LO),], c(Trt, (ToC - 1)))
    }
    
    ## Picking out trt and ctl covariates
    Trt.CV = Dt[which(Trt == 1), 1:r.I]
    Ctl.CV = Dt[-which(Trt == 1), 1:r.I]
    
    Ks[i,] = abs((apply(Trt.CV, 2, sum) - 
                    apply(Ctl.CV, 2, sum))/N)
  }## ending for i in M.seq
  
  ## So the labels in parcoord come out nicely
  ## NEW colnames here
  colnames(Ks) = I[,'L']
  
  P = list()
  P[[1]] = name
  P[[2]] = I
  P[[3]] = as.data.frame(Ks)
  P[[4]] = Ms
  P[[5]] = LO
  P[[6]] = X
  P[[7]] = Dt
  
  P
} ## closing the function make.Ks

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Goldilocks randomization: Graphic"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(id = "inTabset",
              
              tabPanel("Upload Data",
                       sidebarPanel(
                         helpText("Input an excel file here (.xlsx). We assume the data are in the 
                                  first sheet with titles in the first row. This method does
                                  not work with missing data in any of the matching variables."),
                         br(),
                         
                         fileInput("file1", "Choose Excel File",
                                   accept = c(
                                     "xls",
                                     "xlsx")
                         ),
                         br(),
                         
                         helpText("In the next tab we will input more information to build
                                  the graph."),
                         actionButton("Tab2", "Go on to the next tab")
                         )),
              
              tabPanel("General Inputs",
                       value = "panel2",
                       
                       sidebarPanel(
                         
                         helpText("Input the number of times we will randomize, the number of 
                                  variables to be considered in this randomization scheme,
                                  and if leftover units should be in treatment or control arm.
                                  The next tab will be used to build the rest of the plot."),
                         
                         sliderInput("Times",
                                     "No of times we randomize:",
                                     min = 50,  max = 500, value = 300),
                         
                         helpText("This controls the number of inputs on the next tab."),
                         uiOutput("NVs"),
                         
                         helpText("If there are an odd number of units to randomize 
                                  the remainder can be in either treatment or control."),
                         radioButtons("ToC", label = h3("Treatment or Control"),
                                      choices = list("Treatment" = 2, "Control" = 1), 
                                      selected = 2),
                         actionButton("Tab3", "Go on to the next tab")
                         )),
              
              tabPanel("Matching Variables Info",
                       value = "panel3",
                       sidebarPanel(
                         helpText("Choose variables below to be randomized, then input labels, 
                                  weights, and minimum and maximum for the y-axis for that 
                                  variable. After you've finished hit the Go button to
                                  see the plot. The black line in the plot is the mean difference
                                  for each arm."),
                         
                         br(),
                         
                         actionButton("go", "Go", width = "150px", style = "background-color:red"),
                         br(),
                         
                         helpText("Once suitable weights are found, hit this button to share
                                  the inputs with others."),
                         
                         bookmarkButton(id = "bookmark1", width = "150px"),
                         
                         uiOutput("VarsInput")
                         )),
              
              tabPanel("Summaries",
                       sidebarPanel(
                         helpText("Below are summaries of the raw data of the columns choosen 
                                  in the randomization tab."), 
                         verbatimTextOutput("summary"),
                         br(),
                         
                         helpText("This is the number of pairs used to make the graph."),
                         verbatimTextOutput("NM")
                         )),
              
              tabPanel("Download",
                       sidebarPanel(
                         helpText("Use this page to download the graph, and all inputs
                                  used to make them."), 
                         textInput("FN", label = h3("File name:"), 
                                   value = "File name"),
                         textInput("YN", label = h3("Your name:"), 
                                   value = "Your name"),
                         radioButtons('format', 'Document format', c('PDF', 'Word'),
                                      inline = TRUE),
                         downloadButton('downloadReport',"Share matching graph")
                         )),
              
              tabPanel("Matches",
                       sidebarPanel(
                         helpText("Below find the row numbers of the matches. If there 
                                  are an odd number of matching units, the leftover 
                                  is listed first."), 
                         br(),
                         
                         helpText("Check the box below and choose the 
                                  column with labels if you prefer information
                                  that way."),
                         
                         checkboxInput("labs", "Label of your choice"),
                         
                         conditionalPanel(
                           condition = "input.labs == true",
                           uiOutput("ID")
                         ),
                         
                         conditionalPanel(
                           condition = "input.labs == false",
                           verbatimTextOutput("MA")
                         ),
                         
                         conditionalPanel(
                           condition = "input.labs == true",
                           verbatimTextOutput("MAL")
                         )
                         )),
              
              tabPanel("Download Final Plot",
                       sidebarPanel(
                         helpText("This page downloads the final graph to be used 
                                  in publications. Use matches in previous tab to 
                                  randomize. After, input results below. If there 
                                  are any leftovers they are first."), 
                         textInput("FN1", label = h3("File name:"), 
                                   value = "File name"),
                         helpText("Use this button when finished inputting.
                                  A label must be choosen in the 'Matches' tab
                                  for this to work."),
                         downloadButton('downloadReport1',"Final randomization graph"),
                         uiOutput("RanInput")
                         )),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput("plot", click = "plot_dblclick")
              )
              )
                       )

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  K <- reactive({
    input$NoVars
  })
  
  LID <- reactive({
    input$LID
  })
  
  M = reactive({
    inFile <- input$file1
    M = tagList()
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    D = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    D = as.data.frame(D)
    
    ## Upating file names
    colnames(D) <- labs <- gsub("\r\n"," ", colnames(D))
    nums = labs[which(sapply(D, is.numeric) == TRUE)]
    
    M[[1]] = D
    M[[2]] = nums
    
    M
  })
  
  j <- reactive({
    ceiling(dim(M()[[1]])[1]/2)
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
                                     M()[[2]], selected = M()[[2]][i])
      output[[i]][[5]] = textInput(L[i], "Label for variable:", 
                                   value = paste0("Lab", i))
      output[[i]][[6]] = textInput(W[i], "Weight for variable:",
                                   value = "1")
      output[[i]][[7]] = textInput(S[i], "Max for variable",
                                   value = "5")
    } ## for loop
    
    output
  })
  
  output$NVs <- renderUI({
    numericInput("NoVars","No. of matching variables",
                 value = 3, min = 2, max = length(M()[[2]]))
  })
  
  R <- reactive({
    sapply(1:j(), function(i){paste0("input$Rand",i)})
  })
  
  output$RanInput <- renderUI({
    if(Dat()[[6]] == TRUE){
      G = M()[[1]][Dat()[[4]][,1], LID()]
    }else{
      G = c(M()[[1]][Dat()[[5]], LID()], M()[[1]][Dat()[[4]][,1], LID()])
    }
    
    output = tagList()
    
    for(i in seq_along(1:j())){
      output[[i]] = tagList()
      output[[i]][[1]] = hr(style="height:5px;background-color:blue")
      output[[i]][[2]] = G[i]
      output[[i]][[3]] = br()
      output[[i]][[4]] = selectInput(R()[i], "Treatment or control:",
                                     c("Treatment" = "1", "Control" = "0"))
      
    } ## for loop
    
    output
  })
  
  
  output$ID <- renderUI({
    selectInput("LID", "Label:",
                colnames(M()[[1]]), selected = 1)
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
    
    make.Ks(M = input$Times, D = M()[[1]], vars = V, name = "",
            ToC = as.numeric(isolate({input$ToC})), S = "glpk")
  }) ## eventReactive
  
  observeEvent(input$Tab2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$Tab3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  ## The first column is selected upon initiation
  C <- reactiveVal(1)       # rv <- reactiveValues(value = 0)
  
  ## Updating to the column of choice.
  observeEvent(input$plot_dblclick$x, {
    newC <- round((K() - 1)*input$plot_dblclick$x + 1)
    C(newC)
  })
  
  ## Making the plot by ordering it and then drawing the plot.
  output$plot <- renderPlot({
    E = make.order(column = C(), data = Dat()[[3]])
    make.plot(data = E, I = Dat()[[2]])
  })
  
  
  ## Summary of data
  output$summary <- renderPrint({
    C <- sapply(1:K(), function(i) {input[[paste0("cols",i)]]})
    summary(M()[[1]][, C])
  })
  
  output$NM <- renderPrint({
    
    if(Dat()[[6]] == TRUE){
      paste0("There were ", floor(dim(M()[[1]])[1]/2), 
             " matched pairs")
    }else{
      paste0("There were ", floor(dim(M()[[1]])[1]/2), 
             " matched pairs and one unit leftover.")
    }
  })
  
  output$MA <- renderPrint({
    Dat()[[4]]
  })
  
  output$MAL <- renderPrint({
    if(Dat()[[6]] == TRUE){
      cbind(M()[[1]][Dat()[[4]][,1], LID()], 
            M()[[1]][Dat()[[4]][,2], LID()])
    }else{
      rbind(c(M()[[1]][Dat()[[5]], LID()], "NA"),
            cbind(M()[[1]][Dat()[[4]][,1], LID()], 
                  M()[[1]][Dat()[[4]][,2], LID()]))
    }
  })
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  output$downloadReport1 <-  downloadHandler(
    ## Making the filename for the report here. Using two different 
    ## extensions for the file
    filename = function() {
      paste(input$FN1, sep = '.', switch(input$format, PDF = 'pdf', 
                                        Word = 'docx'))
    },
    
    content = function(file) {
      ## Copy the report file to a temporary directory before processing it, in
      ## case we don't have write permissions to the current working dir (which
      ## can happen when deployed).
      tempReport1 <- file.path(tempdir(), "report1.Rmd")
      file.copy("report1.Rmd", tempReport1, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(I1 = Dat()[[1]], # name
                     I2 = Dat()[[2]], # Labels and maxs for plot
                     I3 = Dat()[[3]], # Ks things to plot
                     j = j(), # even or odd no. of units
                     R = sapply(R(), function(x) input[[x]]), # trt or ctrl
                     M = M()[[1]], # Orig data
                     Dt = Dat()[[7]]) #Data for each 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      library(rmarkdown)
      rmarkdown::render(tempReport1, 
                        switch(input$format,
                               PDF = pdf_document(), Word = word_document()),
                        output_file = file, params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadReport <-  downloadHandler(
    ## Making the filename for the report here. Using two different 
    ## extensions for the file
    filename = function() {
      paste(input$FN, sep = '.', switch(input$format, PDF = 'pdf', 
                                        Word = 'docx'))
    },
    
    content = function(file) {
      ## Copy the report file to a temporary directory before processing it, in
      ## case we don't have write permissions to the current working dir (which
      ## can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(I1 = Dat()[[1]],
                     I2 = Dat()[[2]],
                     I3 = Dat()[[3]],
                     N = input$YN)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      library(rmarkdown)
      rmarkdown::render(tempReport, 
                        switch(input$format,
                               PDF = pdf_document(), Word = word_document()),
                        output_file = file, params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")