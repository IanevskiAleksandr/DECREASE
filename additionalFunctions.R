# load packages
lapply(c("ggplot2","reshape2"), library, character.only = !0)

#############################################################################################################
#### Plots

# plot dose-response matrix (args: matrix, title, agent names)
PlotRespMatr <- function(matr, name = "none", d1N = "", d2N = ""){
  
  data.plot <- melt(matr)
  colnames(data.plot) <- c("y","x","Inhibition")
  data.plot$Inhibition <- round(c(matr), 1)
  data.plot$x <- as.factor(data.plot$x)
  data.plot$y <- as.factor(data.plot$y)
  
  axis.x.text <- round(as.numeric(colnames(matr)), 1)
  axis.y.text <- round(as.numeric(rownames(matr)), 1)
  dose.response.p <- ggplot(data.plot, aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "Inhibition"), color = "#FCFCFC", size=1) + 
    theme(title = element_text(face = "bold", size = 10)) + 
    geom_text(aes_string(fill = "Inhibition", label = "Inhibition"), size = 3.5) + 
    scale_fill_gradient2(low = "grey", high = "#4682b4", midpoint = 0, name = paste0("% cell \ninhibition"),na.value="white", limits=c(0, 100)) + 
    scale_x_discrete(labels = axis.x.text) + scale_y_discrete(labels = axis.y.text) + 
    labs(x = d2N, y = d1N) + theme(plot.title = element_text(hjust = 0.5, size = 16)) #+ guides(fill=F)
  
  dose.response.p <- dose.response.p + theme(axis.text.x = element_text(color = "black", face = "bold", size = 11))
  dose.response.p <- dose.response.p + theme(axis.text.y = element_text(color = "black", face = "bold", size = 11))
  dose.response.p <- dose.response.p + theme(axis.title = element_text(size = 14))
  dose.response.p <- dose.response.p + ggtitle(paste0("\nDose-response matrix (",name,")\n"));
  list(pl = dose.response.p)
}


# combine multiple plots
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}