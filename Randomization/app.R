#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

make.plot1 = function(data, I){
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

make.zoom.plot1 = function(data, I, Min, Max){
  upper <- as.numeric(I[,"Maxs"])
  lower <- as.numeric(I[,"Mins"])
  N <- length(upper)
  
  for(i in 1:N){
    data[[i]] <- data[[i]]/upper[i]
  }
  
  # Basic plot to update
  # The columns Min:Max makes it zoom!!!!!
  p = ggparcoord(data, columns = Min:Max, groupColumn = (N + 1), scale = "globalminmax", 
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


make.plot <- function(data){
  p <- ggplot(data, aes(year, n))
  p <- p + geom_line() 
  p <- p + theme_bw(base_size = 16)
  print(p)
}

make.zoom.plot <- function(data, Min, Max){
  xMin = 0.075
  xMax = 0.9875
  yMin = min(data[,"year"])
  yMax = max(data[,"year"])
  
  grad = (yMax - yMin)/(xMax - xMin)
  int = yMin - grad*xMin
  
  newMin = int + grad*Min
  newMax = int + grad*Max
  
  p <- ggplot(data, aes(year, n))
  p <- p + geom_line() 
  p <- p + theme_bw(base_size = 16)
  p <- p + xlim(newMin, newMax)
  print(p)
}

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Title Here"),
  tabsetPanel(tabPanel("Upload Data",
    sidebarPanel(
      helpText("Upload data here."),
      fileInput("file1", "Choose Excel File",
                accept = c(
                  "xls",
                  "xlsx")
              ),
      actionButton("go", "Go", width = "150px", style = "background-color:red")
    ))),
  mainPanel(
    plotOutput("zoom", height = "350px"),
    plotOutput("overall", height = "150px",
               brush =  brushOpts(id = "brush", direction = "x")
    )
  )
)

server <- function(input, output) {
  M = reactive({
    inFile <- input$file1
    M = tagList()
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    D = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    D = as.data.frame(D)
    
    ## Upating column names
    colnames(D) <- labs <- gsub("\r\n"," ", colnames(D))
    nums = labs[which(sapply(D, is.numeric) == TRUE)]
    
    M[[1]] = D
    M[[2]] = nums
    
    M
  })
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$go, {
    v$data <- data.frame(
      n = as.numeric(sunspots),
      year = rep(1749:1983, each = 12) + (0:11)/12)
  })
  
  output$overall <- renderPlot({
    if (is.null(v$data)) return()
    make.plot(data = v$data)
  })
 
  output$zoom <- renderPlot({
    if (is.null(input$brush) || is.null(v$data)) return()
    make.zoom.plot(data = v$data, Min = input$brush$xmin, 
                   Max = input$brush$xmax)
  })
}

shinyApp(ui, server)