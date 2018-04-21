# load packages
lapply(c("scales","drc","reshape2","xgboost","parallel", "openxlsx", "dplyr", "NNLM", "modeest", "mlrMBO"), require, character.only = !0)
source("HelpFunctions.R")
  
  # load and prepare data
  data_cell <- readRDS("annot.RDS"); set.seed(42); influentPoint = NULL # for now
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
