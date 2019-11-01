# remove outliers
outlier_remove <- compiler::cmpfun(function(xTmp, iqr_ = 1.5){
  qq <- unname(quantile(xTmp, probs=c(.25, .75), na.rm = T))
  outlier_detector <- iqr_ * IQR(xTmp, na.rm = T)
  xTmp < (qq[1] - outlier_detector) | xTmp > (qq[2] + outlier_detector)
})


# fit single agent dose-response curve (restricted to 0-100 range)
CALC_IC50_EC50_DSS = compiler::cmpfun(function(xpr_tbl, DSS_typ, readoutCTX = F, drug_name="drug_name")
{
  tryCatch({
    
    mat_tbl <- data.frame(inhibition=as.numeric(xpr_tbl), dose = as.numeric(names(xpr_tbl))); mat_tbl$logconc = log10(mat_tbl$dose); mat_tbl$viability = 100 - mat_tbl$inhibition;
    mat_tbl$inhibition2 = mat_tbl$inhibition; mat_tbl$viability2 = mat_tbl$viability; mat_tbl <- mat_tbl[order(mat_tbl[,"dose"]),] 
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
    
    coef_ic50 <- c(coef_ic50,Min.Conc.tested=min_signal,Max.Conc.tested=max_signal,IC50_std_error=ic50std_Error)
    return (list(coef_ic50=coef_ic50,nls_result_ic50=nls_result_ic50));
  })
})
