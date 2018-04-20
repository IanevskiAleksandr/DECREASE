transformInputData <- compiler::cmpfun(function(dataOutput, data_typ = NULL){

  if (data_typ == "viability") dataOutput$Response = 100 - dataOutput$Response

  dose.response.mats <- list()
  num.pairs <- length(unique(dataOutput$PairIndex)); pairs_ = unique(dataOutput$PairIndex);
  drug.pairs = data.frame(drug.row = 1:num.pairs, drug.col = 1:num.pairs, concUnit = 1:num.pairs, PairIndex = 1:num.pairs)
  error_ <- rep(1, num.pairs); # errors initially 1
  warning_ <- rep("", num.pairs); # warnings initially ""
  
  for(i in 1:num.pairs){
    df2 <- dplyr::arrange(dataOutput[dataOutput$PairIndex==pairs_[i],],Conc1,Conc2)
    Conc1 <- sort(as.numeric(unique(df2$Conc1))); Conc2 <- sort(as.numeric(unique(df2$Conc2)));
    
    tryCatch({
      aaaa = matrix(nrow = length(Conc1), ncol = length(Conc2), byrow = T)
      dimnames(aaaa) = list(Conc1, Conc2) # rowname, colname
      
      for(j in 1:length(Conc1)){
        for(k in 1:length(Conc2)){
          resp = as.numeric(df2[((df2$Conc1 == Conc1[j]) & (df2$Conc2 == Conc2[k])),"Response"]);
          if(!identical(resp, numeric(0))) aaaa[j,k] = resp else aaaa[j,k] = NA
        }
      }
      ###dose.response.mats[[i]] = matrix(as.numeric(df2$Response), nrow = length(Conc1), ncol = length(Conc2), byrow = T)
      dose.response.mats[[i]] = aaaa;
      dimnames(dose.response.mats[[i]]) = list(Conc1, Conc2) # rowname, colname
      drug.pairs[i,]$drug.row <- df2$Drug1[1]; drug.pairs[i,]$drug.col<- df2$Drug2[1];
      drug.pairs[i,]$concUnit <- df2$ConcUnit[1]; drug.pairs[i,]$PairIndex <- df2$PairIndex[1];
      error_[i] <- 0;
      if(length(Conc1) * length(Conc2) != length(df2$Response))
        warning_[i] <- paste0("The provided number of responses for ",i," PairIndex is: <b>",length(df2$Response), "</b>. However <b>",length(Conc1) * length(Conc2), "</b> responses were expected. Please reconsider <b>",df2$Drug1[1]," & ",df2$Drug2[1], "</b> drug pair, or continue at your own risk " )
    })
  }
  return(list(dose.response.mats = dose.response.mats, drug.pairs = drug.pairs, error = error_, warning = warning_))
})


transformInputDataMatrix <- compiler::cmpfun(function(dataOutput, data_typ = NULL){
  dose.response.mats <- list()
  D1 = grep("Drug1", dataOutput[,1]); D2 = grep("Drug2", dataOutput[,1]); CU = grep("ConcUnit", dataOutput[,1]);
  num.pairs <- length(D1);
  drug.pairs = data.frame(drug.row = 1:num.pairs, drug.col = 1:num.pairs, concUnit = 1:num.pairs, PairIndex = 1:num.pairs)
  error_ <- rep(1, num.pairs); # errors initially 1
  warning_ <- rep("", num.pairs); # warnings initially ""
  #browser();browser();browser();browser();browser();
  for(i in 1:length(D1)){
    if(i != length(D1)) tmpMat <- sapply(dataOutput[(CU[i]+1):(D1[i+1]-1),], as.numeric) 
      else tmpMat <- sapply(dataOutput[(CU[i]+1):nrow(dataOutput),], as.numeric) 
    tmpMat <- tmpMat[,!apply(is.na(tmpMat) | tmpMat == "", 2, all)] # remove cols with all NA
     
    row_names_ = tmpMat[2:nrow(tmpMat),1]; col_names_ = as.numeric(na.omit(tmpMat[1,1:ncol(tmpMat)]))
    resp.mat = matrix(tmpMat[2:nrow(tmpMat), 2:ncol(tmpMat)], nrow = length(row_names_), ncol = length(col_names_),
                                     dimnames = list(row_names_, col_names_))
    resp.mat = resp.mat[order(as.numeric(rownames(resp.mat))),]; resp.mat = resp.mat[,order(as.numeric(colnames(resp.mat)))];
    if (data_typ == "viability") resp.mat = 100 - resp.mat
    
    # return data
    dose.response.mats[[i]] = resp.mat;
    drug.pairs[i,]$drug.row <- dataOutput[D1[i],2]; drug.pairs[i,]$drug.col<- dataOutput[D2[i],2];
    drug.pairs[i,]$concUnit <- dataOutput[CU[i],2]; drug.pairs[i,]$PairIndex <- i;
    
    if(anyNA(resp.mat))
      warning_[i] <- paste0("The dose-response matrix for ", drug.pairs[i,]$drug.row," and ", drug.pairs[i,]$drug.col," has some missing values. Please consider estimation of NA values feature (see <a style='cursor: pointer;' onclick='javascript:techdoc()'>technical documentation</a>), or continue at your own risk" )
  }
  
  return(list(dose.response.mats = dose.response.mats, drug.pairs = drug.pairs, error = error_, warning = warning_))
})
