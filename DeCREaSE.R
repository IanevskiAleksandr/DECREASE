# load packages
lapply(c("scales","drc","reshape2","xgboost","parallel", "openxlsx", "dplyr", "NNLM", "modeest", "mlrMBO"), require, character.only = !0)

# remove outliers
outlier_remove <- compiler::cmpfun(function(xTmp, iqr_ = 1.5){
  qq <- unname(quantile(xTmp, probs=c(.25, .75), na.rm = T))
  outlier_detector <- iqr_ * IQR(xTmp, na.rm = T)
  xTmp < (qq[1] - outlier_detector) | xTmp > (qq[2] + outlier_detector)
})


# fit single agent dose-response curve
CALC_IC50_EC50_DSS = compiler::cmpfun(function(xpr_tbl, DSS_typ, readoutCTX = F, drug_name="drug_name")
{
  tryCatch({
    
    mat_tbl <- data.frame(inhibition=as.numeric(xpr_tbl), dose = as.numeric(names(xpr_tbl))); #dose = 10**(1:length(xpr_tbl)));
    mat_tbl$logconc = log10(mat_tbl$dose); mat_tbl$viability = 100 - mat_tbl$inhibition;
    mat_tbl$inhibition2 = mat_tbl$inhibition; mat_tbl$viability2 = mat_tbl$viability;
    mat_tbl <- mat_tbl[order(mat_tbl[,"dose"]),] 
    
    if(any(duplicated(mat_tbl$inhibition))) mat_tbl$inhibition <- seq(from = 0, length.out = length(mat_tbl$inhibition), by = 0.01) + mat_tbl$inhibition; 
    
    
    estimate_param <- tryCatch({drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA, NA, NA,NA),names = c("SLOPE","MIN","MAX","IC50")),logDose=10,control = drmc(errorm = F))}, 
                               warning=function(w){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)},
                               error=function(e){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)})
    coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50"); coef_estim["SLOPE"] <- coef_estim["SLOPE"]*-1 
    coef_estim["IC50"] <- ifelse(coef_estim["MAX"]<=coef_estim["MIN"] | coef_estim["IC50"]>max(mat_tbl$dose,na.rm=T), max(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
    coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,min(mat_tbl$dose,na.rm=T),coef_estim["IC50"]); coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,mean(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
    coef_estim["IC50"] <- log10(coef_estim["IC50"]); coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<min(mat_tbl$logconc),max(mat_tbl$logconc),coef_estim["IC50"])
    coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition<0),max(mat_tbl$logconc,na.rm=T),coef_estim["IC50"]); coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition,na.rm=T)
    min_lower <- ifelse(min(mat_tbl$inhibition,na.rm=T) > 0,min(mat_tbl$inhibition,na.rm=T),0); min_lower <- ifelse(min_lower >= 100,99,min_lower)
    coef_estim["MAX"] <- ifelse(coef_estim["MAX"]>100,100,coef_estim["MAX"]); coef_estim["MAX"] <- ifelse(coef_estim["MAX"]<0,100,coef_estim["MAX"])
    max_lower <- ifelse(max(mat_tbl$inhibition,na.rm=T)>100,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T)); max_lower <- ifelse(max_lower < 0,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T)); 
    max_lower <- ifelse(max_lower < 0,0,max_lower); max_lower <- ifelse(max_lower > 100,100,max_lower); run_avg <- caTools::runmean(mat_tbl$inhibition, 10); max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)]>run_avg[nrow(mat_tbl)]),max(mat_tbl$inhibition[run_avg>run_avg[nrow(mat_tbl)]]),coef_estim["MAX"])
    max_upper <- ifelse(any(mat_tbl$inhibition > max_upper),mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper])+5,max_upper)
    max_upper <- ifelse(max_upper < 0,coef_estim["MAX"],max_upper); max_upper <- ifelse(max_upper > 100,100,max_upper) #coef_estim["MAX"]
    max_upper <- ifelse(max_lower > max_upper,coef_estim["MAX"],max_upper);mean_inh_last = mean(tail(mat_tbl$inhibition,2),na.rm=T)
    if(mean_inh_last < 60) {
      if(mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc,na.rm=T) else if(mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)}
    if(mean(mat_tbl$inhibition[1:3],na.rm=T)<5) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)
    if(unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
    
    #adaptive nonlinear Least-Squares algorithm NL2SOL to estimate parameters.
    nls_result_ic50_old <- function(){
      tryCatch({
        nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),lower=list(SLOPE=0,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),upper=list(SLOPE=4,MIN=0,MAX=100, IC50=max(mat_tbl$logconc)),control=list(warnOnly=T,minFactor = 1/2048))
      }, error = function(e) {minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl,start=list(SLOPE=1, MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),lower=c(SLOPE=0, MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),upper=c(SLOPE=4, MIN=0,MAX=100, IC50=max(mat_tbl$logconc)))
      })} 
    # IC50 first
    nls_result_ic50 <- nls_result_ic50_old();
    nls_result_ic50_2 <- tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]], IC50=median(mat_tbl$logconc)),lower=list(SLOPE=0,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),upper=list(SLOPE=4,MIN=0,MAX=100, IC50=max(mat_tbl$logconc)),control=list(warnOnly=T,minFactor = 1/2048))
    },warning = function(w) {nls_result_ic50_old()},error = function(e) {nls_result_ic50_old()})
    
    tryCatch({
      aaa=tryCatch({summary(nls_result_ic50)},error=function(e){summary(nls_result_ic50_2)})
      bbb=tryCatch({summary(nls_result_ic50_2)},error=function(e){summary(nls_result_ic50)})
      
      sumIC50 = list(aaa,bbb)
      ic50std_resid <- round(sqrt(sum((sumIC50[[1]]$residuals)^2)/(length(sumIC50[[1]]$residuals)-1)),1);
      ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2)/(length(sumIC50[[2]]$residuals)-1)),1);
      # continue with the best
      switch_ = which.min(c(ic50std_resid, ic50std_resid2)); nls_result_ic50 = list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
    }, error = function(e){})
    
    if(coef(nls_result_ic50)["SLOPE"] <= 0.2){if(mean_inh_last > 60) coef_estim["IC50"] <- min(mat_tbl$logconc,na.rm=T);
    nls_result_ic50 <- nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port",start=list(SLOPE=1, MIN=unname(coef_estim["MIN"]),MAX=unname(coef_estim["MAX"]),IC50=unname(coef_estim["IC50"])),lower=list(SLOPE=0.1,MIN=min_lower,MAX=max_lower,IC50=min(mat_tbl$logconc)),upper=list(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),control=list(warnOnly=T,minFactor = 1/2048))
    }
    #Calculate the standard error scores
    sumIC50 = summary(nls_result_ic50); ic50std_Error <- sumIC50$coefficients["IC50","Std. Error"];
    ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2)/(length(sumIC50$residuals)-1)),1);
    max_signal <- max(mat_tbl$dose,na.rm=T); min_signal <- min(mat_tbl$dose,na.rm=T)
    
    #prepare final data and convert IC50 back from log scale (inverse)
    coef_ic50 <- coef(nls_result_ic50)[c("IC50", "SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]; coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"]<0,max_signal,coef_ic50["IC50"])
    coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<0,max_signal,coef_ic50["IC50"]);coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<10,max_signal,coef_ic50["IC50"])
    coef_ic50["MAX"] <- ifelse(coef_ic50["MAX"]<0,0,coef_ic50["MAX"]);coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition,na.rm=T),min(mat_tbl$inhibition,na.rm=T))>50),min_signal,coef_ic50["IC50"])
    x <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc), length=100); yic <- predict(nls_result_ic50, data.frame(logconc=x))
    perInh <- t(matrix(mat_tbl[,"inhibition"],dimnames=list(paste0(rep("D", length(mat_tbl[,"inhibition"])), 1:length(mat_tbl[,"inhibition"])))))
    coef_tec50 = coef_ic50; 
    coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose,na.rm=T))
    if(readoutCTX){names(coef_tec50) <- c("TC50","SLOPE","MAX","MIN"); ytec <- yic; perViaTox <- perInh;} else{
      names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN"); coef_tec50["SLOPE"] = -1 * coef_tec50["SLOPE"]; # min - 0, max - 77 in ec50 it is max - 100, min - 23
      tmp = coef_tec50["MAX"]; coef_tec50["MAX"] = 100 - coef_tec50["MIN"]; coef_tec50["MIN"] = 100 - tmp; ytec <- 100 - yic;
      perViaTox <- 100 - perInh;
    }
    ############################# 
    #############    DSS
    dss_score <- 100#round(as.numeric(dss(coef_ic50["IC50"],coef_ic50["SLOPE"],coef_ic50["MAX"],min_signal,max_signal, DSS.type=as.integer(DSS_typ))),1);
    coef_ic50 <- c(coef_ic50,Min.Conc.tested=min_signal,Max.Conc.tested=max_signal,IC50_std_error=ic50std_Error,DSS=dss_score)
    coef_tec50 <- c(coef_tec50,Min.Conc.tested=min_signal,Max.Conc.tested=max_signal,TEC50_std_error=ic50std_Error)
    IC50_df <- data.frame(DRUG_NAME="drug_name",ANALYSIS_NAME="IC50", t(as.matrix(coef_ic50)), perInh,GRAPH=NA, DSS = as.numeric(dss_score), sDSS = "", SE_of_estimate = as.numeric(ic50std_resid))
    
    return (list(coef_ic50=coef_ic50,nls_result_ic50=nls_result_ic50));
  })
})
  
  data_cell <- readRDS("annot.RDS")
  set.seed(42); influentPoint = NULL # for now
  MatrTr = reshape2::acast(data_cell, Conc1~Conc2, value.var = "Response")

  # check [0,0] conc.
  if(MatrTr[1,1] < max(MatrTr[2,1], MatrTr[1,2])) MatrTr[1,1] = 100

  # calculate Bliss approximation
  MatrTr = 100 - MatrTr; bliss.mat = MatrTr; bliss.mat[bliss.mat>100]=100; bliss.mat[bliss.mat<0]=0;
    for (k in 2:nrow(bliss.mat))
    for (j in 2:ncol(bliss.mat))
      bliss.mat[k, j] <- bliss.mat[k,1] + bliss.mat[1,j] - bliss.mat[k,1] *  bliss.mat[1,j] / 100

  # fit single-agent 
  D1Len = MatrTr[,1]; names(D1Len)[1] = 1e-6; D2Len = MatrTr[1,]; names(D2Len)[1] = 1e-6;
  d1 = tryCatch({
      predict(CALC_IC50_EC50_DSS(D1Len, DSS_typ = 2, drug_name = "")$nls_result_ic50)
  }, error = function(e){D1Len})
  d2 = tryCatch({
    predict(CALC_IC50_EC50_DSS(D2Len, DSS_typ = 2, drug_name = "")$nls_result_ic50)
  }, error = function(e){D2Len})

  # single-agent deviations
  devD1 = abs(d1 - (MatrTr[,1])); devD2 = abs(d2 - (MatrTr[1,])); dev_ = abs(bliss.mat - MatrTr);
  MatrOutl = matrix(!1, nrow = nrow(MatrTr), ncol = ncol(MatrTr))

  # check deviations with Bliss
  MatrOutl[-1,-1] = outlier_remove(abs(dev_[-1,-1] - median(dev_[-1,-1], na.rm = T)), iqr_ = 5) & (dev_[-1,-1] > 25)
  MatrOutl[,1] = as.logical(colSums(MatrOutl, na.rm = T)) & devD1 > 10 | devD1 > 15
  MatrOutl[1,] = as.logical(rowSums(MatrOutl, na.rm = T)) & devD2 > 10 | devD1 > 15

  # remove possible outliers
  MatrOutl[is.na(MatrOutl)] = !1; MatrTr[MatrOutl] = NA
  if(any(MatrOutl)){ influentPoint = T; warning("Possible outliers were removed");}

  matr_Out = reshape2::melt( 100 - MatrTr ); colnames(matr_Out) <- c("Conc1", "Conc2", "Response");
  matr_Out = dplyr::arrange(matr_Out, Conc1, Conc2)
  
  # fill single-agent response columns (check the design section of manuscript)
  matr_Out$R1 <- sapply(1:nrow(matr_Out), function(i){
    matr_Out$Response[matr_Out$Conc1 == matr_Out[i,]$Conc1 & matr_Out$Conc2 == 0]
  })
  matr_Out$R2 <- sapply(1:nrow(matr_Out), function(i){
    matr_Out$Response[matr_Out$Conc2 == matr_Out[i,]$Conc2 & matr_Out$Conc1 == 0]
  })
  
  # training and test
  trainInd <- which(!is.na(matr_Out$Response));  testInd <- which(is.na(matr_Out$Response))
  data_cell_Training <- matr_Out[trainInd,]; data_cell_Test <- matr_Out[testInd,]; 
  
  ######################################################################################################################
  ################################################     fit cNMF      ###################################################
  
  cNMFpred = do.call("cbind", mclapply(1:120, function(i){

    #bag_ = sample(1:nrow(data_cell_Training), sample(2:round(nrow(data_cell_Training)/6),1)); #data_cell_TrainingTmp$Response[bag_] = NA
    MatrTr = reshape2::acast(rbind(data_cell_Training, data_cell_Test), Conc1~Conc2, value.var="Response")
    MatrTr = MatrTr + matrix(runif(1, -0.001, 0.001), nrow = nrow(MatrTr), ncol = ncol(MatrTr))

    if(length(influentPoint) != 0){
      if(is.na(MatrTr[1,1])){ MatrTr[1,1] = 100 }; if(is.na(MatrTr[nrow(MatrTr),1])) {MatrTr[nrow(MatrTr),1] = MatrTr[nrow(MatrTr)-1,1]}
      if(is.na(MatrTr[1, ncol(MatrTr)])) {MatrTr[1, ncol(MatrTr)] = MatrTr[1, ncol(MatrTr)-1]};
      MatrTr[,1] = zoo::na.approx(MatrTr[,1], rule=2); MatrTr[1,] = zoo::na.approx(MatrTr[1,], rule=2);
    }

    nsclc2.nmf <- NNLM::nnmf(as.matrix(MatrTr), sample(2:3,1), verbose = F, beta = sample(seq(.1,1,.01), 3), alpha = sample(seq(.1,1,.01), 3), #rep(.001,3), alpha = rep(.001,3),
                             max.iter = 500L, loss = "mse", check.k = F);
    nsclc2.hat.nmf <- with(nsclc2.nmf, W %*% H);

    matr_Out =reshape2::melt( nsclc2.hat.nmf ); colnames(matr_Out) <- c("Conc1", "Conc2", "Response");
    dplyr::arrange(matr_Out, Conc1, Conc2)$Response

  }, mc.cores = 4))

  # prepare predictions
  if(sum(colSums(cNMFpred == 0)==0)>1) cNMFpred = cNMFpred[,colSums(cNMFpred == 0) == 0]; 
  cNMFpred = sapply(1:nrow(cNMFpred), function(i) modeest::venter(cNMFpred[i,]))
  
  ######################################################################################################################
  ##############################################     fit XGBoost     ###################################################
  
  obj.fun = compiler::cmpfun(makeSingleObjectiveFunction(
    name = "XGBoost",
    fn = function(x) {
      logNtree = x[1]; lambda = x[2]; alpha = x[3]; maxdepth = x[4]; subsample = x[5]; colsample_bytree = x[6]; eta = x[7];  

      # repeated CV
      MAD_ <- mclapply(1:3, function(repCv){

        MAD_i = 0
        flds <- caret::createFolds(data_cell_Training$Response, k = 3, list = T, returnTrain = F);

        for(k in 1:length(flds)){
          testData <- data_cell_Training[flds[[k]], ]; trainData <- data_cell_Training[-flds[[k]], ]

          fit = xgboost(data=as.matrix(trainData[,c("R1","R2","Conc1","Conc2")]),label = trainData$Response, verbose = F,
                        nrounds=round(2**logNtree), nthread = 1, save_name = paste0("xgboost",repCv,"_",k,".model"),
                        params=list(objective = "reg:linear", max.depth=maxdepth, eta=eta, lambda = lambda, alpha = alpha,
                                    subsample=subsample, colsample_bytree = colsample_bytree))


          ypred = predict(fit, as.matrix(testData[,c("R1","R2","Conc1","Conc2")]));
          MAD_i <- MAD_i + mean(abs(ypred - testData$Response), na.rm = T);

        }
        MAD_i
      }, mc.cores = 3)

      Reduce(sum, MAD_)

    },
    par.set = makeParamSet(
      makeNumericVectorParam("logNtree", len = 1, lower = 4, upper = 9),
      makeNumericVectorParam("lambda", len = 1, lower = 0, upper = 3),
      makeNumericVectorParam("alpha", len = 1, lower = 0, upper = 3),
      makeIntegerVectorParam("maxdepth", len = 1, lower = 1, upper = 6),
      makeNumericVectorParam("subsample", len = 1, lower = .4, upper = 1),
      makeNumericVectorParam("colsample_bytree", len = 1, lower = .4, upper = 1),
      makeNumericVectorParam("eta", len = 1, lower = .001, upper = .1)
    ),
    minimize = !0
  ))

  des = generateDesign(n = 20, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)

  des$y = apply(des, 1, obj.fun) #as.numeric(mclapply(1:nrow(des), function(i) obj.fun(des[i,]), mc.cores = 4))

  surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = F))

  # library(parallelMap) # parallelStartMulticore(cpus = 4, show.info = T)
  modelsXGBoost = mbo(obj.fun, design = des, learner = surr.km, show.info = !0,
                      control = setMBOControlInfill(setMBOControlTermination(makeMBOControl(), iters = 30),
                                                    crit = makeMBOInfillCritEI()))$opt.path$env[["path"]]
  # order based on error
  orderMAD = order(modelsXGBoost$y); models = modelsXGBoost[orderMAD, ]; #run = XGBoostRun[orderMAD]

  # Fit with 4 models with best parameters
  XGBoostpred <- do.call("cbind", mclapply(1:4, function(i){
    fit <- xgboost(as.matrix(data_cell_Training[,c("R1","R2","Conc1","Conc2")]), label = data_cell_Training$Response,
                   verbose = F, nrounds=round(2**models[i,"logNtree"]) , nthread = 1,
                   params=list(objective = "reg:linear", max.depth=models[i,"maxdepth"], eta=models[i,"eta"], lambda = models[i,"lambda"],
                               alpha = models[i,"alpha"], subsample=models[i,"subsample"], colsample_bytree = models[i,"colsample_bytree"]))
    predict(fit, as.matrix(matr_Out[,c("R1","R2","Conc1","Conc2")]));
  }))
  XGBoostpred = sapply(1:nrow(XGBoostpred), function(i) modeest::venter(XGBoostpred[i,]))

  # final prediction
  finalpred_ = (XGBoostpred + cNMFpred) / 2
  
  matr_Out$cNMFpred = cNMFpred; matr_Out$XGBoostpred = XGBoostpred; matr_Out$finalpred_ = finalpred_;
  matr_Out$finalpred_[trainInd] = matr_Out$Response[trainInd] 