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

# plot interaction matrix
PlotIntMatr <- function(matr, d1N, d2N){ 
  
  # single drug deviations
  D1Len = matr[,1]; names(D1Len)[1] = 1e-6; D2Len = matr[1,]; names(D2Len)[1] = 1e-6;
  d1 = tryCatch({ 
    predict(CALC_IC50_EC50_DSS(D1Len, DSS_typ = 2, drug_name = "")$nls_result_ic50) 
  }, error = function(e){D1Len})
  d2 = tryCatch({ 
    predict(CALC_IC50_EC50_DSS(D2Len, DSS_typ = 2, drug_name = "")$nls_result_ic50)
  }, error = function(e){D2Len})
  
  matr[,1] = d1; matr[1,] = d2;
  
  # calculate Bliss approximation
  bliss.mat = matr
  for (k in 2:nrow(matr))
    for (j in 2:ncol(matr))
      bliss.mat[k, j] <- matr[k,1] + matr[1,j] - matr[k,1] *  matr[1,j] / 100
  
  
  syn = matr - bliss.mat
  
  synergy.score = list();
  synergy.score$drug.pairs = data.frame(drug.row = d1N,  drug.col = d2N, concUnit = "nM", blockIDs = 1)
  synergy.score$scores = append(synergy.score$scores, list(syn))
  
  ss  = calcsyn(syn, synergy.score$drug.pairs)
  PlotSynergyShiny(ss, graphnumber = 2, gridsize2 =1)
}

##################################################################################
##### Plot 2D and 3D synergy interaction maps
###################################################################################

PlotSynergyShiny <- compiler::cmpfun(function (data, type = "2D", graphnumber = 1, brushx = NULL, brushy = NULL, gridsize = 1, gridsize2 = 0, 
                                               savee2D = NULL, savee3D = NULL, newscore = NULL, name_3D = NULL, method_ = "Bliss", synScoresMtx = NULL, mostsynarea = 1) 
{
  print("plotinside")
  
  !is.list(data) && {stop("Input data is not a list format!")}
  if (gridsize == -1) {colmap = !0; gridsize = 1} else { colmap = !1 }
  
  summary.score <- data$summary.score; cMat <- data$c
  drug.row <- data$drug.row; drug.col <- data$drug.col
  x.conc <- data$x.conc; y.conc <- data$y.conc
  start.point <- data$start.point; end.point <- data$end.point
  
  if (method_ == "ZIP") {
    if (!is.null(newscore)) {plot.title =  plot.title2 = bquote(~delta ~ " - score: " ~ .(newscore))
    } else { plot.title <- bquote(~delta ~ " - score: " ~ .(summary.score)); plot.title2 <- paste0("delta score: ", summary.score)}
    title3D = paste0(drug.row, " & ", drug.col, " <br>\U03B4 - score: ", summary.score)
  }
  else {
    if (!is.null(newscore)) { plot.title = plot.title2 = paste0(method_, " synergy score: ", newscore)
    } else { plot.title <- paste0(method_, " synergy score: ", summary.score); 
    plot.title2 <- paste0(method_, " synergy score: ",  summary.score);}
    title3D = paste0(drug.row, " & ", drug.col, " <br> ", method_, " synergy score: ", summary.score)
  }
  print(paste0("size- ", gridsize))
  
  if (graphnumber == 2){
    plot2d = melt(cMat);
    myPalette <- colorRampPalette(c("green2", "white", "red1"))(100)
    names(plot2d) <- c("x","y","z")
    gplot2d <- 
      ggplot(plot2d) + aes(x, y, z = z, fill = z)  + geom_raster(interpolate = !0) + 
      geom_contour(color = "white", alpha = 0.5) + 
      scale_fill_gradientn(expression(delta ~ -score), colours = myPalette, limits = c(start.point, end.point), 
                           values = rescale(c(-3, -1, 0, 1, 3))) + 
      scale_x_continuous(drug.col, expand = c(0, 0), 
                         breaks = seq(min(plot2d$x), max(plot2d$x), by = (max(plot2d$x) - min(plot2d$x))/(length(x.conc) - 1)), 
                         labels = round(x.conc, 2)) + 
      scale_y_continuous(drug.row, expand = c(0, 0), 
                         breaks = seq(min(plot2d$y), max(plot2d$y), by = (max(plot2d$y) - min(plot2d$y))/(length(y.conc) - 1)), 
                         labels = round(y.conc, 2)) + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      theme(axis.text = element_text(size = 10)) + 
      theme(title = element_text(vjust = 12)) 
    
    
    byx = (max(plot2d$x) - min(plot2d$x))/(length(x.conc) - 1);
    byy = (max(plot2d$y) - min(plot2d$y))/(length(y.conc) - 1);
    
    if(gridsize2 != 0)
    {     
      gplot2d <- gplot2d + geom_vline(xintercept = seq(min(plot2d$x), max(plot2d$x), by = byx), linetype = "dotted") + 
        geom_hline(yintercept = seq(min(plot2d$y), max(plot2d$y), by = byy), linetype = "dotted") 
      
      gplot2d + ggtitle(plot.title) + coord_cartesian(xlim = c(brushx[1], brushx[2]), ylim = c(brushy[1], brushy[2]))  + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    
  }
})


###################################################################################
##### Calculate synergy surfaces out of scores
###################################################################################

calcsyn <- compiler::cmpfun(function(scores, drug.pairs)
{
  scores.dose <- t(scores)  
  combscores = scores.dose[-1, -1]; combscores[nrow(combscores),ncol(combscores)] <- 'NA'
  summary.score <- round(mean(as.numeric(combscores), na.rm = !0), 3)
  x.conc <- as.numeric(rownames(scores.dose))
  y.conc <- as.numeric(colnames(scores.dose))
  conc.unit <- drug.pairs$concUnit
  unit.text <- paste0("(", conc.unit, ")")
  drug.row <- paste0(drug.pairs$drug.row, " ", unit.text)
  drug.col <- paste0(drug.pairs$drug.col, " ", unit.text)
  color.range <- round(max(abs(max(as.numeric(combscores), na.rm = !0)), abs(min(as.numeric(combscores), na.rm = !0))) + 5, -1)
  start.point <- -color.range; end.point <- color.range  
  pixels.num = 5 * (length(x.conc) - 1) + 2;
  
  # only for visualization  (max BzCl)
  scores.dose[nrow(scores.dose),ncol(scores.dose)] <- max(scores.dose[nrow(scores.dose)-1,ncol(scores.dose)],
                                                          scores.dose[nrow(scores.dose),ncol(scores.dose)-1],
                                                          scores.dose[nrow(scores.dose)-1,ncol(scores.dose)-1])    
  
  kriged =  tryCatch({
    tmp <- cbind(expand.grid(c(0:(length(x.conc) - 1)), c(0:(length(y.conc) - 1))), c(as.matrix(scores.dose)))        
    kriging(tmp[, 1],tmp[, 2], tmp[, 3], lags = ifelse(dim(scores.dose)[1] < 8, 2 ,3), 
            pixels = pixels.num, model = "spherical")
  },error = function(e){
    appro <- function(x, n) approx(x, n=n)$y
    tryCatch({
      m = apply(t(apply(scores.dose, 1, function(x) appro(x, ncol(scores.dose)*2))), 2, function(x) appro(x, nrow(scores.dose)*2))
      tmp2<- cbind(expand.grid(c(0:(nrow(m)-1)),c(0:(ncol(m)-1))), c(as.matrix(m)))
      kriging(tmp2[, 1], tmp2[, 2], tmp2[, 3], lags = ifelse(dim(m)[1] < 8, 2 ,3), pixels = pixels.num, model = "spherical")
    },error = function(e){ 
      m = apply(t(apply(scores.dose, 1, function(x) appro(x, ncol(scores.dose)*3))), 2, function(x) appro(x, nrow(scores.dose)*3))
      tmp2<- cbind(expand.grid(c(0:(nrow(m)-1)),c(0:(ncol(m)-1))), c(as.matrix(m)))
      kriging(tmp2[, 1], tmp2[, 2], tmp2[, 3], lags = ifelse(dim(m)[1] < 8, 2 ,3), pixels = pixels.num, model = "spherical")
    })
  })
  
  xseq <- round(kriged[["map"]]$x/kriged$pixel)
  yseq <- round(kriged[["map"]]$y/kriged$pixel)
  a <- min(xseq):max(xseq); b <- min(yseq):max(yseq)
  na <- length(a); nb <- length(b)
  res1 <- as.double(rep(0, na * nb))
  res2 <- as.integer(rep(0, na * nb))
  
  z.len = length(kriged[["map"]]$pred); #
  for(idx1 in 1:na) { #
    for(idx2 in 1:nb) { #
      for(idx3 in 1:z.len) { #
        if(xseq[idx3] == a[idx1] && yseq[idx3] == b[idx2]) { #
          indx_ = idx2+(idx1-1)*nb; #
          res1[indx_] <- kriged[["map"]]$pred[idx3] #
          res2[indx_] <- 1 #
          break #
        } #
      } #
    } #
  } #
  
  res1[res2 == 0] <- NA #
  cMat <- matrix(res1, na, nb, byrow = !0)
  
  # most synergystic region
  max_ = r_ = c_ = -999;
  for(i in 1:(ncol(scores.dose)-2)){
    for(j in 1:(nrow(scores.dose)-2)){
      mean_ = mean(scores.dose[j:(j+2),i:(i+2)], na.rm = !0)
      if(mean_ > max_) {
        max_ = mean_; r_ = j; c_ = i;
      }
    }
  }
  
  return(list(c = cMat, conc.unit = conc.unit, drug.row = drug.row, 
              drug.col = drug.col, start.point = start.point, end.point = end.point, 
              summary.score = summary.score, x.conc = x.conc, y.conc = y.conc, pixels.num = pixels.num, r_ = r_, c_ = c_, max_ = round(max_,3)))
})



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