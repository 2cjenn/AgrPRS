library(stringr)
options("scipen"=100)

# Run chunks of R code in arbitrary working directory
# From Hadley Wickham
# https://github.com/yihui/knitr/issues/38
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

pretty_dp <- function(x, dp=0, pct=FALSE, comma=FALSE){
  if(pct){x <- 100*x}
  if(comma){
    format(round(x, dp), digits=dp, nsmall=dp, big.mark=",") %>% trimws
  } else {
    format(round(x, dp), digits=dp, nsmall=dp) %>% trimws
  }
}

pretty_confint <- function(lci, uci, dp, pct=FALSE){
  paste0("(", pretty_dp(x=lci, dp=dp, pct=pct), ", ", pretty_dp(x=uci, dp=dp, pct=pct), ")")
}

pretty_pval <- function(p, cutoff=0.001, string="<0.001", dp=3){
  ifelse(p<cutoff, string, pretty_dp(p, dp))
}

lower <- function(x){
  paste0(tolower(substring(x, 1,1)), substring(x, 2))
}

upper <- function(x){
  paste0(toupper(substring(x, 1,1)), substring(x, 2))
}

prettyfunc <- function(x, pnames=list(), upper=FALSE, bold=FALSE, flist=c()){
  out <- x
  if(x %in% names(pnames)){
    out <- pnames[[x]]
    if(upper){
      out <- upper(out)
    }
    if(bold){
      out <- paste0("**", out, "**")
    }
  }
  if(x %in% flist){
    if(!exists("footnote_no")){
      footnote_no <<- 1
    }
    out <- paste0(out, "^", footnote_no, "^")
    footnote_no <<- footnote_no + 1
  }
  return(out)
}

percentile<-function(x){
  cut(x,breaks=c(quantile(x,probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),na.rm=TRUE)),
      labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%"),
      include.lowest=TRUE)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

table1_standardised <- function(data, varlist, adj, stratify, strata=NULL, 
                                pretty_names=list(), singlecol=TRUE, dp=1, show_crude=FALSE){
  #' Create age-standardised Table 1
  #'
  #' @param data The data
  #' @param varlist The list of variables to include in the table
  #' @param adj The variable to adjust by, as a factor. Eg age in single-year brackets.
  #' @param stratify The variable to stratify by, as a factor
  #' @param strata If you don't want to use all levels of the stratifying variable, specify desired levels here
  #' @param pretty_names List of human readable names corresponding to variable names
  #' @param singlecol Whether to stack variable names and levels into one column (TRUE - default) or spread across two columns (FALSE)
  #' @param dp Number of decimal places to display in the table (default 1)
  #' @param show_crude Include crude proportions in the form "crude (adjusted)". Advisable for sanity check but not for final presentation.
  #' 
  #' @return A dataframe formatted appropriately to output as Table 1
  #' @export
  #'
  #' @examples
  #' 
  if(is.null(strata)) { strata <- levels(data[[stratify]]) }
  
  table <- c()
  colnames <- c()
  
  for(s in strata) {
    colnames <- c(colnames, paste0(s, " (N=", nrow(data[data[[stratify]]==s,]), ")"))
    col <- c()
    
    for(var in varlist){
      if(is.factor(data[[var]])){
        if(singlecol){ col <- c(col, "") }
        for(l in levels(data[[var]])){
          
          count <- table(data[[adj]][data[[var]]==l & data[[stratify]]==s], useNA='ifany')
          pop <- table(data[[adj]][data[[stratify]]==s], useNA='ifany')
          stdpop <- table(data[[adj]], useNA='ifany')
          
          proportions <- ageadjust.direct(count=count, pop=pop, stdpop=stdpop)
          crude.prop <- pretty_dp(100*proportions[["crude.rate"]],dp)
          adj.prop <- pretty_dp(100*proportions[["adj.rate"]],dp)
          
          if(show_crude){
            col <- c(col, paste0(crude.prop, " (", adj.prop, ")"))
          } else {
            col <- c(col, adj.prop)
          }
        }
      } else {
        col <- c(col, paste0(pretty_dp(mean(data[[var]][data[[stratify]]==s]), dp), 
                             " (", pretty_dp(sd(data[[var]][data[[stratify]]==s]), dp), ")"))
      }
    }
    table <- cbind(table, col)
  }
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=data, varname=var, pretty_names=pretty_names, onecol=singlecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  colnames(table) <- colnames
  table <- cbind(coeffnames%>%select(-IDcol), table)
  rownames(table) <- NULL
  
  return(table)
}

# Normal Table 1
# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, singlecol=FALSE, 
                             pretty_names=list(), footnote_list=c()){
  if(!exists("footnote_no")){footnote_no <<- 1} # Note use of <<- instead of <- to assign this globally
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){ # Categorical variables (factors) need a row per level, n's and %'s
      n <- table(df[[var]], useNA='ifany')
      pct <- pretty_dp(prop.table(n), dp=1, pct=TRUE)
      npct <- paste0(n, " (", pct, "%)")
      variable <- c(prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)))
        outtable <- rbind(outtable, 
                          c(var, paste0("**", variable, "**"), "", "", "", pval), 
                          cbind(paste0(var, levels), levels, n, pct, npct, ""))
      } else{
        outtable<- rbind(outtable, 
                         c(var, paste0("**", variable, "**"), "", "", ""), 
                         cbind(paste0(var, levels), levels, n, pct, npct))
      }
    } else { # Continuous variables need the mean (and SD) or median (and IQR)
      if(contavg=="mean"){
        n <- pretty_dp(mean(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- pretty_dp(sd(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        npct <- paste0(n, " (", pct, ")")
        variable <- paste0("Mean ", prettyfunc(var, pretty_names, upper=FALSE, flist=footnote_list), " (SD)")
      } else if (contavg=="median"){
        n <- pretty_dp(median(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        IQR <- pretty_dp(quantile(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        npct <- paste0(n, " ", pct)
        variable <- paste0("Median ", prettyfunc(var, pnames=pretty_names, upper=FALSE, flist=footnote_list), " (IQR)")
      } else if(contavg=="n"){
        n <- nrow(df[!is.na(df[[var]]),])
        pct <- NA
        npct <- NA
        variable <- prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list)
      }
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        p <- pretty_pval(tt$p.value)
        outtable <- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, npct, p))
      } else {
        outtable<- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, npct))
      }
    }
  }
  rownames(outtable) <- c()

  outdf <- as.data.frame(outtable, stringsAsFactors=FALSE)
  if(singlecol){
    outdf <- outdf %>% select(-c(n, pct))
  } else {
    outdf <- outdf %>% select(-npct)
  }
  return(outdf)
}

printMIresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  regression <- data.frame(
    IDcol=modeloutput$term,
    HR=pretty_dp(exp(modeloutput$estimate),dp=2),
    CI=pretty_confint(exp(modeloutput$estimate-1.96*modeloutput$std.error),
                            exp(modeloutput$estimate+1.96*modeloutput$std.error),
                            dp=2),
    p=pretty_pval(modeloutput$p.value),
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  results <- results[,c(coeffcols, "HR", "CI", "p")]
  names(results) <- c(coeffcols, "HR", "95% CI", "p")
  
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}

# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  conf <- summ$conf.int
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    HR=pretty_dp(coeff[,2], dp=2), # HR
    CI=pretty_confint(conf[,3], conf[,4], dp=2), # 95% CI
    p=pretty_pval(coeff[,5]), # p-value
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  results <- results[,c(coeffcols, "HR", "CI", "p")]
  names(results) <- c(coeffcols, "HR", "95% CI", "p")
    
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}

# Pretty print the results from a logistic regression model
printlogresults <- function(model, coeffnames=NULL, IDcol=FALSE){
  summ <- summary(model)
  coeff <- summ$coefficients
  # NOMVAR <- rownames(coeff)
  regression <- data.frame(
    IDcol=(rownames(coeff)),
    OR=pretty_dp(exp(coeff[,1]), dp=2), # OR
    CI=pretty_confint(exp(coeff[,1]-(1.96*coeff[,2])),
                      exp(coeff[,1]+(1.96*coeff[,2])),
                      dp=2), # 95% CI
    p=pretty_pval(coeff[,4]), # p-value
    stringsAsFactors=FALSE
  )
  if(is.null(coeffnames)){
    results <- regression
    names(results) <- c("Coefficient", "OR", "95% CI", "p")
  } else {
    results <- merge(coeffnames, regression, all.x=TRUE)
    results$OR[is.na(results$OR)] <- "1"
    results <- results[match(coeffnames$IDcol, results$IDcol),]
    
    coeffcols <- colnames(coeffnames)
    if(IDcol==FALSE){
      coeffcols <- coeffcols[coeffcols != "IDcol"]
    }
    results <- results[,c(coeffcols, "OR", "CI", "p")]
    names(results) <- c(coeffcols, "OR", "95% CI", "p")
  }
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Round to the nearest m
mround <- function(x, base){
  base*round(x/base)
}


# Make a pretty proportion table
# To use this,
# tab <- table(data$VIhyp, data$prevHBP, useNA='ifany')
# kable(propped(tab), caption="")
propped <- function(table, margin=NULL) {
  prop <- round(100*prop.table(table, margin=margin),2)
  tabsums <- addmargins(table)
  dimt <- dim(table)
  for(i in c(1:dimt[1])) {
    if(length(dimt)>1){
      for(j in c(1:dimt[2])) {
        tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
      }
    }
    else{
      tabsums[i] <- paste0(table[i], " (", prop[i], "%)") 
    }
  }
  return(tabsums)
}

# Check correlation among all pairs of covariates in a given dataframe 
# (this will need extending to be more robust if I want to use it for non-categorical covariates)
corr_mat <- function(data){
  corr_matrix <- matrix(0L, nrow=ncol(data), ncol=ncol(data))
  colnames(corr_matrix) <- colnames(data)
  rownames(corr_matrix) <- colnames(data)
  for(i in c(1:(ncol(data)-1))){
    x <- data[[i]]
    for(j in c((i+1):ncol(data))){
      y <- data[[j]]
      if(is.factor(x) & is.factor(y)){
        corr_matrix[i, j] <- (cramerV(table(x, y)))
      } else if (is.numeric(x) & is.numeric(y)) {
        corr_matrix[i, j] <- cor(x, y, method="pearson")
      } else {
        print("Unanticipated type")
      }
    }
  }
  return(corr_matrix)
}

preparecoefflist_1col <- function(df, varname, pretty_names=list()){
  pretty_varname <- prettyfunc(varname, pnames=pretty_names, bold=TRUE, upper=TRUE)
  if(is.factor(df[[varname]])){
    if(is.ordered(df[[varname]])){
      poly <- length(levels(df[[varname]]))
      levels <- c("Ref", ".L", ".Q", ".C")
      if(poly > 4){
        powers <- c(4:(poly-1))
        levels <- c(levels, paste0("^", powers))
      }
      levels <- levels[1:poly]
    } else {
      levels <- levels(df[[varname]])
    }
    variable <- c(pretty_varname, levels)
    coeffname <- c(pretty_varname, paste0(varname, levels))
  } else {
    variable <- pretty_varname
    coeffname <- varname
  }
  output <- data.frame(coeffname, variable, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient")
  rownames(output) <- NULL
  return(output)
}

preparecoefflist_2col <- function(df, varname, pretty_names=list()){
  pretty_varname <- prettyfunc(varname, pnames=pretty_names, upper=TRUE)
  if(is.factor(df[[varname]])){
    if(is.ordered(df[[varname]])){
      poly <- length(levels(df[[varname]]))
      levels <- c("Ref", ".L", ".Q", ".C")
      if(poly > 4){
        powers <- c(4:(poly-1))
        levels <- c(levels, paste0("^", powers))
      }
      levels <- levels[1:poly]
    } else {
      levels <- levels(df[[varname]])
    }
    variable <- c(pretty_varname,
                  rep(NA, length(levels)-1))
    coeffname <- paste0(varname,levels)
  } else {
    levels <- NA
    variable <- pretty_varname
    coeffname <- varname
  }
  output <- data.frame(coeffname, variable, levels, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient", "Levels")
  rownames(output) <- NULL
  return(output)
}

preparecoefflist <- function(onecol=FALSE, ...){
  if(onecol) {
    preparecoefflist_1col(...)
  } else {
    preparecoefflist_2col(...)
  }
}

regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender"), pretty_names=list(), IDcol=TRUE){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0(outcome, " ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=IDcol)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}

# Convert output of etmCIF to ggplot acceptable input
#https://stackoverflow.com/questions/56784714/write-a-summary-to-as-data-frame-for-use-in-ggplot-r
etm_to_df <- function(object, ci.fun = "cloglog", level = 0.95, ...) {
  l.X <- ncol(object$X)
  l.trans <- nrow(object[[1]]$trans)
  res <- list()
  for (i in seq_len(l.X)) {
    temp <- summary(object[[i]], ci.fun = ci.fun, level = level)
    res[[i]] <- data.table::rbindlist(
      temp[object$failcode + 1], idcol = "CIF"
    )[, CIF := names(object)[i]]
  }
  do.call(rbind, res)
}

# Plot cumulative incidence function (with competing risk)
cuminc_plot<-function(data,group,start="TEU_BaC_AgeAtRec",end="TEU_BrCa_age",cr_status="cr_status",status="TEU_BrCa_status",failcode=1,smooth=FALSE,
                      title,legend.title,xlim=c(40,79),xlab="Age in years", ylim,
                      caption="Note: Age was truncated at 79 years old;\n CR cases stands for non-BrCa death cases"){
  
  #' Create cumulative incidence plot with competing risk (can adapt left-truncation, smoothing)
  #' Basically a wrapper of etm::etmCIF
  #' https://cran.r-project.org/web/packages/etm/vignettes/etmCIF_tutorial.pdf
  #'
  #' @param data The data
  #' @param group The group of interest, specify 1 if not interested plots by groups
  #' @param start Entry time if left-truncation exists, specify NULL if no left-truncation
  #' @param end Follow up time 
  #' @param cr_status competing risk event indicator (0=censored,1=event of interest,2=competing risk event)
  #' @param status follow up status (i.e.no competing risk indicator) (0=censored,1=event of interest)
  #' @param failcode Indicates the failure type of interest for plotting (default 1)
  #' @param title Title of the plot
  #' @param legend.title Legend title
  #' @param xlim limit of x
  #' @param xlab Label of x-axis
  #' @param ylim limit of y
  #' @param smooth Show smoothed curve using cubic spline technique with 10 knots (default FALSE)
  #' 
  #' @return Cumulative incidence plot
  
  
  # Start time: TEU_BaC_AgeAtRec; Stop time: TEU_BrCa_age; What counts as events: cr_status!=0
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", cr_status,"!= 0)~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", cr_status,"!= 0)~",group))}
  
  
  # etype specifies the type of events; failcode=1 means we are only interested in plotting cr_status==1 which is BrCa incident case
  fit<-etmCIF(formula,data=data,etype = cr_status,failcode=failcode)
  
  # Transform to ggplot acceptable format
  res<-etm_to_df(fit)%>%mutate(CIF=factor(CIF,levels=paste0(group,"=",levels(data[[group]])),labels = levels(data[[group]])))
  
  # Number of brca cases and non-brca dth cases within each PRS category
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)),nonbr=(sum(!!sym(cr_status))-sum(!!sym(status)))/2)%>%na.omit()
  
  if(smooth==FALSE){
    plot<-ggplot(res, aes(time, P)) +
      #geom_ribbon(aes(ymin = lower, ymax = upper, fill = CIF), alpha = 0.2) +
      geom_line(aes(color = CIF),size=1) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }else{
    
    # Smoothing using spline
    plot<-ggplot(res)+geom_spline(aes(x=time,y=P,colour=CIF),nknots = 10) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }
  
  return(plot)
}

# Plot cumulative incidence function (without competing risk)
cuminc_nocr_plot<-function(data,start,end,status,group,title,legend.title,xlab="Age in years",xlim=c(40,80)){
  
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", status,")~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", status,")~",group))}
  
  
  fit<-surv_fit(formula,data=data)
  
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)))%>%na.omit()
  
  ggsurv<-ggsurvplot(fit,data=data,fun="event",title=title,
                     xlab=xlab,ylab="Cumulative incidence",#risk.table = TRUE,
                     xlim=xlim,break.x.by=5,
                     censor.size=1,size=1,ggtheme = theme_bw(),
                     legend.labs=paste(labels[[group]],"(",labels$n,"events)"),legend.title=legend.title)
  
  plot=ggsurv$plot+theme(legend.position="left")
  
  return(plot)
}

# Copied from https://alistaire.rbind.io/blog/coalescing-joins/
coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3900052/
interpretKappa <- function(k) {
  dplyr::case_when(
    k <= 0.2 ~ "none",
    k < 0.4 ~ "minimal",
    k < 0.6 ~ "weak",
    k < 0.8 ~ "moderate",
    k < 0.9 ~ "strong",
    k >= 0.9 ~ "almost perfect"
  )
}

blandaltplot <- function(df, title, caption) {
  df$avg <- rowMeans(df)
  df$diff <- df[,1, drop=TRUE] - df[,2, drop=TRUE]
  mean_diff <- mean(df$diff)
  lower <- mean_diff - 1.96*sd(df$diff)
  upper <- mean_diff + 1.96*sd(df$diff)
  
  #create Bland-Altman plot
  ggplot(df, aes(x = avg, y = diff)) +
    geom_point(size=2) +
    geom_hline(yintercept = mean_diff) +
    geom_hline(yintercept = lower, color = "red", linetype="dashed") +
    geom_hline(yintercept = upper, color = "red", linetype="dashed") +
    labs(title=title,caption=caption,
         y="Difference Between Measurements", x="Average Measurement")
}

adjAUC <- function(status, prediction, dp=3, ci=TRUE) {
  roc <- roc(status, prediction,
             levels=c(0,1), direction="<", ci=TRUE)
  auc <- round(roc$ci, dp)
  if(ci==TRUE) {
    return(paste0(pretty_dp(auc[2], dp), " ", pretty_confint(auc[1], auc[3], dp=dp)))
  } else {
    return(pretty_dp(auc[2], dp))
  }
}

nribin_silent <- function(...){
  sink(nullfile())
  nri <- suppressMessages(suppressWarnings(nribin(...)))
  sink() 
  return(nri)
}
  

contNRI <- function(x1, x2, y, dp=3, ci=TRUE) {
  #cNRI <- Hmisc::improveProb(x1, x2, y)
  if(ci==TRUE) {
    cNRI <- nribin_silent(event = y, p.std = x1, p.new = x2, updown="diff", cut=0, niter=1000)
    return(paste0(pretty_dp(cNRI$nri$Estimate[1], dp), " ", pretty_confint(cNRI$nri$Lower[1], cNRI$nri$Upper[1], dp=dp)))
  } else {
    cNRI <- nribin_silent(event = y, p.std = x1, p.new = x2, updown="diff", cut=0, niter=0)
    return(pretty_dp(cNRI$nri$Estimate[1], dp))
  }
}

OR_CI <- function(model, row, dp, ci=TRUE) {
  coeff <- summary(model)$coefficients
  OR <- pretty_dp(exp(coeff[row,1]), dp=dp)
  if(ci==TRUE) {
    LCI <- exp(coeff[row,1] - 1.96 * coeff[row,2])
    UCI <- exp(coeff[row,1] + 1.96 * coeff[row,2])
    CI <- pretty_confint(LCI, UCI, dp=dp)
    return(paste0(OR, " ", CI))
  } else {
    return(OR)
  }
  
}
