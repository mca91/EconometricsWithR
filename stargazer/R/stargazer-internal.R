.onAttach <- 
function(libname, pkgname) {
  packageStartupMessage("\nPlease cite as: \n")
  packageStartupMessage(" Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.")
  packageStartupMessage(" R package version 5.2.3. https://CRAN.R-project.org/package=stargazer \n")
}

.stargazer.wrap <-
  function(..., type, title, style, summary, out, out.header, covariate.labels, column.labels, column.separate, 
           dep.var.caption, dep.var.labels, dep.var.labels.include, align, coef, se, t, p, t.auto, 
           p.auto, ci, ci.custom, ci.level, ci.separator, add.lines, apply.coef, apply.se, apply.t, apply.p, apply.ci,
           colnames,
           column.sep.width, decimal.mark, df, digit.separate, digit.separator, digits, digits.extra, 
           flip, float, 
           float.env, font.size, header, initial.zero, intercept.bottom, intercept.top, keep, keep.stat, 
           label, model.names, model.numbers, multicolumn, no.space, notes, notes.align, notes.append, 
           notes.label, object.names, omit, omit.labels, omit.stat, omit.summary.stat, omit.table.layout,
           omit.yes.no, order, ord.intercepts, perl, report, rownames,
           rq.se, selection.equation, single.row, star.char, star.cutoffs, suppress.errors, 
           table.layout, table.placement, 
           zero.component, summary.logical, summary.stat, nobs, mean.sd, min.max, median, iqr, warn) {
     
  .add.model <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL) {
    
    if (class(object.name)[1] == "Glm") {
        .summary.object <<- summary.glm(object.name)
    }
    else if (!(.model.identify(object.name) %in% c("aftreg", "coxreg","phreg","weibreg", "Glm", "bj", "cph", "lrm", "ols", "psm", "Rq"))) {
      .summary.object <<- summary(object.name)
    }
    else {
      .summary.object <<- object.name
    }
    
    if (.model.identify(object.name) == "rq") {
      .summary.object <<- suppressMessages(summary(object.name, se=.format.rq.se))
    }
    
    model.num.total <- 1   # model number for multinom, etc.
    if (.model.identify(object.name) == "multinom") {
      if (!is.null(nrow(.summary.object$coefficients))) {
        model.num.total <-  nrow(.summary.object$coefficients)
      }
    }
    
    for (model.num in 1:model.num.total) {
                                                       
      .global.models <<- append(.global.models, .model.identify(object.name))
  	
  	  .global.dependent.variables <<- append(.global.dependent.variables, .dependent.variable(object.name, model.num))
  	  .global.dependent.variables.written <<- append(.global.dependent.variables.written, .dependent.variable.written(object.name, model.num))

  	  .global.N <<- append(.global.N, .number.observations(object.name))
  	  .global.LL <<- append(.global.LL, .log.likelihood(object.name))
  	  .global.R2 <<- append(.global.R2, .r.squared(object.name))
  	  .global.max.R2 <<- append(.global.max.R2, .max.r.squared(object.name))
  	  .global.adj.R2 <<- append(.global.adj.R2, .adj.r.squared(object.name))
  	  .global.AIC <<- append(.global.AIC, .AIC(object.name))
      .global.BIC <<- append(.global.BIC, .BIC(object.name))
  	  .global.scale <<- append(.global.scale, .get.scale(object.name))
      .global.UBRE <<- append(.global.UBRE, .gcv.UBRE(object.name))
  	  .global.sigma2 <<- append(.global.sigma2, .get.sigma2(object.name))
  	  
      
      .global.rho <<- cbind(.global.rho, .get.rho(object.name))
      .global.mills <<- cbind(.global.mills, .get.mills(object.name))
  	  .global.theta <<- cbind(.global.theta, .get.theta(object.name))
  	  .global.SER <<- cbind(.global.SER, .SER(object.name))
  	  .global.F.stat <<- cbind(.global.F.stat, .F.stat(object.name))
  	  .global.chi.stat <<- cbind(.global.chi.stat, .chi.stat(object.name))
  	  .global.wald.stat <<- cbind(.global.wald.stat, .wald.stat(object.name))
  	  .global.lr.stat <<- cbind(.global.lr.stat, .lr.stat(object.name))
  	  .global.logrank.stat <<- cbind(.global.logrank.stat, .logrank.stat(object.name))
  	  .global.null.deviance <<- cbind(.global.null.deviance, .null.deviance(object.name))
  	  .global.residual.deviance <<- cbind(.global.residual.deviance, .residual.deviance(object.name))

  	  max.length <- length(.global.coefficient.variables)+length(.coefficient.variables(object.name))

  	  # add RHS variables and coefficients
  	  coef.var <- .coefficient.variables(object.name)
  	  .global.coef.vars.by.model <<-  cbind(.global.coef.vars.by.model, coef.var)

  	  temp.gcv <- rep(NA,each=1,times=max.length)

  	  temp.gcv[1:length(.global.coefficient.variables)] <- .global.coefficient.variables

  	  how.many.gcv <- length(.global.coefficient.variables)
  	  
  	  # try to find variable
  	  position <- 0
  	  for (i in seq(1:length(coef.var))) {
  	    
  	    found <- FALSE
  	    
  		  for (j in seq(1:length(.global.coefficient.variables))) {
  			    if (coef.var[i] == .global.coefficient.variables[j]) {
  				    found <- TRUE
  				    for (k in 1:how.many.gcv) {
  					    if (coef.var[i]==temp.gcv[k]) {
  					  	  position <- k
  					    }
  				    }
  			    }
  		  }
  		  

  		  # If variable was found, no need to add it
  		  if (found == FALSE) {
   
          # append new variable to list of regressors
          while ((position < how.many.gcv) && (!(temp.gcv[position+1] %in% coef.var))) {
            position <- position + 1
          }
        
  			  temp.gcv <- append(temp.gcv, coef.var[i], after=position)
  			  how.many.gcv <- how.many.gcv + 1
  			  position <- position + 1
  		  }
  	    
  	  }
  	  
  	  .global.coefficient.variables <<- temp.gcv[1:how.many.gcv]
  	  
  	  # build up coefficients from scratch
  	  temp.coefficients <- temp.std.errors <- temp.ci.lb <- temp.ci.rb <- temp.t.stats <- temp.p.values <- matrix(data = NA, nrow = length(.global.coefficient.variables), ncol = ncol(.global.coefficients)+1)
	    rownames(temp.coefficients) <- rownames(temp.std.errors) <- rownames(temp.ci.lb) <- rownames(temp.ci.rb) <- rownames(temp.t.stats) <- rownames(temp.p.values) <- .global.coefficient.variables

  	  # fill in from previous iteration of .global coefficients
	    which.variable <- 0
  	  for (row in .global.coefficient.variables) {
  	    
  	    which.variable <- which.variable + 1
  	    
  	    row.i <- .rename.intercept(row)   # row with intercept renamed to get the omit and keep right
  	    
  	    ### if omitted variable, then advance to the next iteration of the loop --- !!! do this also for index
  	    #skip all of this if omitted based on regular expression
  	    omitted <- FALSE
  	    
  	    if (!is.null(.format.omit.regexp)) {
  	      for (i in seq(1:length(.format.omit.regexp))) {
  	        if (length(grep(.format.omit.regexp[i], row.i, perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE	}
  	      }
  	    }
  	    
  	    if (!is.null(.format.keep.regexp)) {
  	      omitted <- TRUE
  	      for (i in seq(1:length(.format.keep.regexp))) {
  	        if (length(grep(.format.keep.regexp[i], row.i, perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE	}
  	      }
  	    }
  	    
  	    if (!is.null(.format.omit.index)) {
  	       for (i in seq(1:length(.format.omit.index))) {
  	        if (.format.omit.index[i] == which.variable) { omitted <- TRUE }
  	       }
  	    }
  	    
  	    if (!is.null(.format.keep.index)) {
  	      omitted <- TRUE
  	      for (i in seq(1:length(.format.keep.index))) {
  	        if (.format.keep.index[i] == which.variable) { omitted <- FALSE }
  	      }
  	    }
  	    
  	    if (omitted == TRUE) { next }

  	    
  	    ###
  	    
		    for (col in seq(1:ncol(.global.coefficients))) {
  			  if (sum(as.vector(rownames(.global.coefficients[,col, drop=FALSE])==row))!=0) { 
  				  if (!is.null(.global.coefficients)) { temp.coefficients[row, col] <- .global.coefficients[row, col] }
  				  if (!is.null(.global.std.errors)) { temp.std.errors[row, col] <- .global.std.errors[row, col] }
  				  if (!is.null(.global.ci.lb)) { temp.ci.lb[row, col] <- .global.ci.lb[row, col] }
  				  if (!is.null(.global.ci.rb)) { temp.ci.rb[row, col] <- .global.ci.rb[row, col] }
  				  if (!is.null(.global.t.stats)) { temp.t.stats[row, col] <- .global.t.stats[row, col] }
  				  if (!is.null(.global.p.values)) { temp.p.values[row, col] <- .global.p.values[row, col] }
  			  }
  		  }
      
        feed.coef <- NA; feed.se <- NA
        # coefficients and standard errors
  		  if (!is.null(.get.coefficients(object.name, user.coef, model.num=model.num)[row])) { 
          temp.coefficients[row, ncol(temp.coefficients)] <- .get.coefficients(object.name, user.coef, model.num=model.num)[row] 
          feed.coef <- temp.coefficients[, ncol(temp.coefficients)]
  		  }
        if (!is.null(.get.standard.errors(object.name, user.se, model.num=model.num)[row])) { 
          temp.std.errors[row, ncol(temp.std.errors)] <- .get.standard.errors(object.name, user.se, model.num=model.num)[row] 
          feed.se <- temp.std.errors[, ncol(temp.std.errors)]
        }
        
        # confidence interval, left and right bound
        if (!is.null(.get.ci.lb(object.name, user.ci.lb, model.num=model.num)[row])) { temp.ci.lb[row, ncol(temp.ci.lb)] <- .get.ci.lb(object.name, user.ci.lb, model.num=model.num)[row] }
		    if (!is.null(.get.ci.rb(object.name, user.ci.rb, model.num=model.num)[row])) { temp.ci.rb[row, ncol(temp.ci.rb)] <- .get.ci.rb(object.name, user.ci.rb, model.num=model.num)[row] }
      
        # t-stats and p-values
        #if (!is.null(user.coef)) { feed.coef <- user.coef }   # feed user-defined coefficients, if available - check that this does not mess up multinom
		    #if (!is.null(user.se)) { feed.se <- user.se }   # feed user-defined std errors, if available
        if (!is.null(.get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row])) { temp.t.stats[row, ncol(temp.std.errors)] <- .get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row] }
  		  if (!is.null(.get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row])) { temp.p.values[row, ncol(temp.std.errors)] <- .get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)[row] }
  	  }

  	  if (!is.null(temp.coefficients)) { .global.coefficients <<- temp.coefficients }
  	  if (!is.null(temp.std.errors)) { .global.std.errors <<- temp.std.errors }
      if (!is.null(temp.ci.lb)) { .global.ci.lb <<- temp.ci.lb }
      if (!is.null(temp.ci.rb)) { .global.ci.rb <<- temp.ci.rb }
  	  if (!is.null(temp.t.stats)) { .global.t.stats <<- temp.t.stats }
  	  if (!is.null(temp.p.values)) { .global.p.values <<- temp.p.values }
    
    }

 } 

  .adj.r.squared <-
  function(object.name) {

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","fGARCH","Arima","coeftest","maBina", "lmer", "glmer", "nlmer", "Gls"))) {
      if (model.name %in% c("heckit")) {
        return(.summary.object$rSquared$R2adj)
      }
      if (model.name %in% c("felm")) {
        return(.summary.object$r2adj)
      }
  		if (!is.null(suppressMessages(.summary.object$adj.r.squared))) {
  			return(as.vector(suppressMessages(.summary.object$adj.r.squared)))
  		}
  		else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
  			return(as.vector(.summary.object$r.sq))
  		}
      else if (model.name %in% c("plm")) {
        return(as.vector(.summary.object$r.squared["adjrsq"]))
      }
      else if (model.name %in% c("ols")) {
        n <- nobs(object.name)
        p <- length(object.name$coefficients[names(object.name$coefficients)!="Intercept"])
        r2 <- object.name$stats["R2"]
        adj.r2 <- 1-(1-r2)*((n-1) / (n-p-1))
        return(as.vector(adj.r2))
      }
  	}
  	return(NA)
  }

  .adjust.settings.style <-
  function(what.style) {
    style <- tolower(what.style)
  
    if (style == "all") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error","t-stat","p-value")  
    }
  
    else if (style == "all2") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*(p)", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","AIC","BIC","UBRE","rho(se)*(p)","Mills(se)*(p)","residual deviance(df)*","null deviance(df)*","=!","notes")  
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")  
    }
  
    # aer = American Economic Review
    else if (style == "aer") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
    
      .format.until.nonzero.digit <<- FALSE
      .format.max.extra.digits <<- 0    
    
      .format.model.left <<- ""
      .format.model.right <<- ""
    
      .format.note <<- "\\textit{Notes:}"
      .format.note.alignment <<- "l"
      .format.note.content <<- c("$^{***}$Significant at the [***] percent level.","$^{**}$Significant at the [**] percent level.","$^{*}$Significant at the [*] percent level.")
    }
  
    # ajps = American Journal of Political Science
    else if (style == "ajps") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      .format.digit.separator <<- ""
      .format.dependent.variables.left <<- "\\textbf{"
      .format.dependent.variables.right <<- "}"
      .format.column.left <<- "\\textbf{"
      .format.column.right <<- "}"
      .format.models.left <<- "\\textbf{"
      .format.models.right <<- "}"
      .format.numbers.left <<- "\\textbf{Model "
      .format.numbers.right <<- "}"
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error") 
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-square"
      .format.R2 <<- "R-squared"
      .format.adj.R2 <<- "Adj. R-squared"
      .format.max.R2 <<- "Max. R-squared"
      .format.note <<- ""
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }  
  
    # ajs = American Journal of Sociology
    else if (style == "ajs") {
      .format.table.parts <<- c(" ","=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","-!","notes")
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variables.capitalize <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
    
      .format.numbers.left <<- ""
      .format.numbers.right <<- ""
    
      .format.until.nonzero.digit <<- FALSE
      .format.max.extra.digits <<- 0    
    
      .format.model.left <<- ""
      .format.model.right <<- ""
    
      .format.note <<- "\\textit{Notes:}"
      .format.note.alignment <<- "l"
      .format.note.content <<- c("$^{*}$P $<$ [.*]","$^{**}$P $<$ [.**]","$^{***}$P $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
    
      .format.initial.zero <<- FALSE
    }
  
    # apsr = American Political Science Review
    else if (style == "apsr") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "chi$^{2}$"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
    
    # asq = Administrative Science Quarterly
    else if (style == "asq") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.digit.separator <<- ""
      .format.dependent.variables.left <<- "\\textbf{"
      .format.dependent.variables.right <<- "}"
      .format.column.left <<- "\\textbf{"
      .format.column.right <<- "}"
      .format.models.left <<- "\\textbf{"
      .format.models.right <<- "}"
      .format.numbers.left <<- "\\textbf{Model "
      .format.numbers.right <<- "}"
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error") 
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-square"
      .format.R2 <<- "R-squared"
      .format.adj.R2 <<- "Adj. R-squared"
      .format.max.R2 <<- "Max. R-squared"
      .format.note <<- ""
      .format.note.content <<- c("$^{\\bullet}$p $<$ [.*]; $^{\\bullet\\bullet}$p $<$ [.**]; $^{\\bullet\\bullet\\bullet}$p $<$ [.***]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
      .format.stars <<- "\\bullet"
    }  
  
    # asr = American Sociological Review
    else if (style == "asr") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*")
      .format.N <<- "\\textit{N}"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "chi$^{2}$"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
  
    # "demography" = Demography
    else if (style == "demography") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.numbers.left <<- "Model "
      .format.numbers.right <<- ""
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.N <<- "\\textit{N}"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.chi.stat <<- "Chi-Square"
      .format.note <<- ""
      .format.note.content <<- c("$^{*}$p $<$ [.*]; $^{**}$p $<$ [.**]; $^{***}$p $<$ [.***]")
      .format.cutoffs <<- c(0.05, 0.01, 0.001)
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
  
    # io = International Organization
    else if (style == "io") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.coefficient.variables.capitalize <<- TRUE
      .format.s.coefficient.variables.capitalize <<- TRUE
      .format.intercept.name <<- "Constant"
      .format.N <<- "\\textit{Observations}"
      .format.AIC <<- "\\textit{Akaike information criterion}"
      .format.BIC <<- "\\textit{Bayesian information criterion}"
      .format.chi.stat <<- "\\textit{Chi-square}"
      .format.logrank.stat <<- "\\textit{Score (logrank) test}"
      .format.lr.stat <<- "\\textit{LR test}"
      .format.max.R2 <<- "\\textit{Maximum R-squared}"
      .format.R2 <<- "\\textit{R-squared}"
      .format.adj.R2 <<- "\\textit{Adjusted R-squared}"
      .format.UBRE <<- "\\textit{UBRE}"
      .format.F.stat <<- "\\textit{F statistic}"
      .format.LL <<- "\\textit{Log likelihood}"
      .format.SER <<- "\\textit{Residual standard error}"
      .format.null.deviance <<- "\\textit{Null deviance}"
      .format.residual.deviance <<- "\\textit{Residual deviance}"
      .format.scale <<- "\\textit{Scale}"
      .format.wald.stat <<- "\\textit{Wald test}"
      .format.note <<- "\\textit{Notes:}"
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
    }
  
  
    # jpam = Journal of Policy Analysis and Management
    else if (style == "jpam") {
      .format.table.parts <<- c("-!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","AIC","BIC","UBRE","rho(se)*","Mills(se)*","-!","notes")    
      .format.models.skip.if.one <<- TRUE
      .format.dependent.variable.text.on <<- FALSE
      
      .format.models.left <<- ""
      .format.models.right <<- ""
      .format.numbers.left <<- "Model "
      .format.numbers.right <<- ""
      .format.numbers.roman <<- TRUE
      .format.coefficient.table.parts <<- c("variable name","coefficient*","standard error")
      .format.intercept.bottom <<- FALSE
      .format.intercept.top <<- TRUE
      .format.N <<- "N"
      .format.AIC <<- "AIC"
      .format.BIC <<- "BIC"
      .format.note <<- "\\textit{Note:}"
      .format.note.content <<- c("$^{***}$p $<$ [.***]; $^{**}$p $<$ [.**]; $^{*}$p $<$ [.*]")
      .format.note.alignment <<- "l"
      .format.s.stat.parts <<- c("-!","stat names","-","statistics1","-!","notes")
      .format.s.statistics.names <<- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","SD"), c("median","Median"), c("min","Minimum"), c("max","Maximum"), c("mad","Median Abs. Dev."), c("p","Percentile(!)"))
      
    }
  
    # "qje" = Quarterly Journal of Economics
    else if (style=="qje") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","omit","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")    
      .format.dependent.variable.text.on <<- FALSE
      .format.s.stat.parts <<- c("-!","stat names","=","statistics1","=!","notes")
      .format.N <<- "\\textit{N}"
      .format.note <<- "\\textit{Notes:}"
      .format.note.content <<- c("$^{***}$Significant at the [***] percent level.", "$^{**}$Significant at the [**] percent level.", "$^{*}$Significant at the [*] percent level.") 
    }
  
    # find style based on journal ("default" or other)
    else if (style=="commadefault") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
      .format.digit.separator <<- " "
      .format.decimal.character <<- ","
    }
  
    else if (style=="default") {
      .format.table.parts <<- c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
    }
  }
  
  .apply <-
  function(auto.t, auto.p)
  {
    if ((!is.null(apply.coef)) || ((!is.null(apply.se)))) {
      if (!is.null(apply.coef)) { .global.coefficients <<- apply(.global.coefficients, c(1,2), apply.coef) }
      if (!is.null(apply.se)) { .global.std.errors <<- apply(.global.std.errors, c(1,2), apply.se) }
      
      if (auto.t == TRUE) { .global.t.stats <<- .global.coefficients / .global.std.errors }
      if (auto.p == TRUE) { .global.p.values <<- 2 * pnorm( abs( .global.t.stats ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) }
        
    }
    
    if (!is.null(apply.t)) { .global.t.stats <<- apply(.global.t.stats, c(1,2), apply.t) }
    if (!is.null(apply.p)) { .global.p.values <<- apply(.global.p.values, c(1,2), apply.p) }
  }

  .AIC <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("coeftest")) {
      return(NA)
    }
    
    if (model.name %in% c("lmer","lme","nlme","glmer","nlmer", "ergm", "gls", "Gls", "lagsarlm", "errorsarlm", "", "Arima")) {
      return(as.vector(AIC(object.name)))
    }
    
    if (model.name %in% c("censReg")) {
      return(as.vector(AIC(object.name)[1]))
    }
    
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$ics["AIC"])
    }
    
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$w$aic))
    }
    
    if (model.name %in% c("arima")) {
      return(as.vector(object.name$aic))
    }
    else if (!is.null(.summary.object$aic)) {
      return(as.vector(.summary.object$aic)) 
    }
    else if (!is.null(object.name$AIC)) {
      return(as.vector(object.name$AIC)) 
    }

    return(NA)
  }
  
  .BIC <-
    function(object.name) {
      
      model.name <- .get.model.name(object.name)
      
      if (model.name %in% c("coeftest","maBina","Arima")) {
        return(NA)
      }
      
      if (model.name %in% c("censReg")) {
        return(as.vector(BIC(object.name)[1]))
      }
      
      if (model.name %in% c("fGARCH")) {
        return(object.name@fit$ics["BIC"])
      }
      
      if (model.name %in% c("lmer","lme","nlme","glmer","nlmer", "ergm", "gls", "Gls")) {
        return(as.vector(BIC(object.name)))
      }
      
      if (model.name %in% c("arima")) {
        return(as.vector(object.name$bic))
      }
      else if (!is.null(.summary.object$bic)) {
        return(as.vector(.summary.object$bic)) 
      }
      else if (!is.null(object.name$BIC)) {
        return(as.vector(object.name$BIC)) 
      }
      
      return(NA)
    }
  

  .chi.stat <-
  function(object.name) {
    chi.output <- as.vector(rep(NA,times=3))
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","lmer", "Gls", "glmer", "nlmer", "normal.gam","logit.gam","probit.gam","poisson.gam","gam()"))) {
      if (!is.null(.summary.object$chi)) {
        chi.value <- suppressMessages(.summary.object$chi)
        df.value <- suppressMessages(.summary.object$df) - suppressMessages(.summary.object$idf)
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
      else if (model.name %in% c("cph", "lrm", "ols", "psm")) {
        chi.value <- object.name$stat["Model L.R."]
        df.value <- object.name$stat["d.f."]
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
      else if (model.name %in% c("probit.ss")) {
        chi.value <- object.name$LRT$LRT
        df.value <- object.name$LRT$df
        chi.p.value <- pchisq(chi.value, df.value, ncp=0, lower.tail = FALSE, log.p = FALSE)
        chi.output <- as.vector(c(chi.value, df.value, chi.p.value))
      }
    }
  
    names(chi.output) <- c("statistic","df1","p-value")
    return(cbind(chi.output))
  }

  .coefficient.table.part <-
  function(part, which.variable, variable.name=NULL) {
    
  	# coefficient variable name
  	if (part=="variable name") {
    
  		# use intercept name for intercept, otherwise variable name
      if (is.na(.format.covariate.labels[.which.variable.label])) {
        if (.format.coefficient.variables.capitalize == TRUE) { cat(" ", .format.coefficient.variables.left, toupper(variable.name), .format.coefficient.variables.right, sep="") }
  		  else { cat(" ", .format.coefficient.variables.left, variable.name, .format.coefficient.variables.right, sep="") }
      }
      else { cat(" ", .format.coefficient.variables.left, .format.covariate.labels[.which.variable.label], .format.coefficient.variables.right, sep="") }
  	}
	
  	# coefficients and stars
  	else if ((part=="coefficient") || (part=="coefficient*")) {
  		for (i in seq(1:length(.global.models))) {
  			if (!is.na(.global.coefficients[.global.coefficient.variables[which.variable],i])) {
				
  				# report the coefficient
				  cat(" & ", .iround(.global.coefficients[.global.coefficient.variables[which.variable],i],.format.round.digits),sep="")
          
  				# add stars to denote statistical significance
  				if (part=="coefficient*") { 
  					p.value <- .global.p.values[.global.coefficient.variables[which.variable],i]
  					.enter.significance.stars(p.value) 
  				}
			
  			}
  			else {
  				cat(" & ",sep="")
  			}
        
        # if single-row, follow up with standard error / confidence interval
        if ((.format.single.row == TRUE) && (("standard error" %in% .format.coefficient.table.parts) || ("standard error*" %in% .format.coefficient.table.parts))) {
            
            if (.format.dec.mark.align == TRUE) { space.char <- "$ $"}
            else { space.char <- " "}
            
  			    if (!is.na(.global.std.errors[.global.coefficient.variables[which.variable],i])) {
  			    
  			      # report standard errors or confidence intervals
              
              .format.ci.use <- .format.ci[i]
              if (is.na(.format.ci.use)) {
                for (j in i:1) {
                  if (!is.na(.format.ci[j])) {
                    .format.ci.use <- .format.ci[j]
                    break
                  }
                }
              }
              
  			      if (.format.ci.use == TRUE) {
                
                # if ci level is NA, find the most recent set level
                .format.ci.level.use <- .format.ci.level[i]
                if (is.na(.format.ci.level.use)) {
                  for (j in i:1) {
                    if (!is.na(.format.ci.level[j])) {
                      .format.ci.level.use <- .format.ci.level[j]
                      break
                    }
                  }
                }
                
  			        z.value <- qnorm((1 + .format.ci.level.use)/2)
  			        coef <- .global.coefficients[.global.coefficient.variables[which.variable],i]
  			        se <- .global.std.errors[.global.coefficient.variables[which.variable],i]
  			        ci.lower.bound <- coef - z.value * se
  			        ci.upper.bound <- coef + z.value * se
                
                if (!is.null(ci.custom[[i]])) {
                  ci.lower.bound.temp <- .global.ci.lb[.global.coefficient.variables[which.variable],i]
                  ci.upper.bound.temp <- .global.ci.rb[.global.coefficient.variables[which.variable],i]
                  if (!is.na(ci.lower.bound.temp)) (ci.lower.bound <- ci.lower.bound.temp)
                  if (!is.na(ci.upper.bound.temp)) (ci.upper.bound <- ci.upper.bound.temp)
                }
                
                if (!is.null(apply.ci)) { 
                  ci.lower.bound <- do.call(apply.ci, list(ci.lower.bound))
                  ci.upper.bound <- do.call(apply.ci, list(ci.upper.bound))
                }

  			        if (.format.dec.mark.align == TRUE) {
  			          hyphen <- paste("$",.format.ci.separator,"$", sep="")
  			        }
  			        else {
  			          hyphen <- .format.ci.separator
  			        }
  			      
                cat(space.char, .format.std.errors.left, .iround(ci.lower.bound,.format.round.digits),hyphen,.iround(ci.upper.bound,.format.round.digits),.format.std.errors.right,sep="")              
  			        
  			      }
  			      else { 
  			        cat(space.char, .format.std.errors.left, .iround(.global.std.errors[.global.coefficient.variables[which.variable],i],.format.round.digits),.format.std.errors.right,sep="")
  			      }
  			    
  			      # add stars to denote statistical significance
              if ("standard error*" %in% .format.coefficient.table.parts) { 
  			         p.value <- .global.p.values[.global.coefficient.variables[which.variable],i]
  			        .enter.significance.stars(p.value) 
  			      }
  			    
  			    }
  		  }
  		}
  		cat(" \\\\ \n ")
  	}

  	# standard errors
  	else if (((part=="standard error") || (part=="standard error*")) && (.format.single.row==FALSE)) {
  		for (i in seq(1:length(.global.models))) {
  			if (!is.na(.global.std.errors[.global.coefficient.variables[which.variable],i])) {

  				# report standard errors or confidence intervals
  			  .format.ci.use <- .format.ci[i]
  			  if (is.na(.format.ci.use)) {
  			    for (j in i:1) {
  			      if (!is.na(.format.ci[j])) {
  			        .format.ci.use <- .format.ci[j]
  			        break
  			      }
  			    }
  			  }
          
          if (.format.ci.use == TRUE) {
            # if ci level is NA, find the most recent set level
            .format.ci.level.use <- .format.ci.level[i]
            if (is.na(.format.ci.level.use)) {
              for (j in i:1) {
                if (!is.na(.format.ci.level[j])) {
                  .format.ci.level.use <- .format.ci.level[j]
                  break
                }
              }
            }
            
            z.value <- qnorm((1 + .format.ci.level.use)/2)
            coef <- .global.coefficients[.global.coefficient.variables[which.variable],i]
            se <- .global.std.errors[.global.coefficient.variables[which.variable],i]
            ci.lower.bound <- coef - z.value * se
            ci.upper.bound <- coef + z.value * se
            
            if (!is.null(ci.custom[[i]])) {
              ci.lower.bound.temp <- .global.ci.lb[.global.coefficient.variables[which.variable],i]
              ci.upper.bound.temp <- .global.ci.rb[.global.coefficient.variables[which.variable],i]
              if (!is.na(ci.lower.bound.temp)) (ci.lower.bound <- ci.lower.bound.temp)
              if (!is.na(ci.upper.bound.temp)) (ci.upper.bound <- ci.upper.bound.temp)
            }
            
            if (!is.null(apply.ci)) { 
              ci.lower.bound <- do.call(apply.ci, list(ci.lower.bound))
              ci.upper.bound <- do.call(apply.ci, list(ci.upper.bound))
            }
            
            if (.format.dec.mark.align == TRUE) {
              hyphen <- paste("$",.format.ci.separator,"$", sep="")
            }
            else {
              hyphen <- .format.ci.separator
            }
            
            if (.format.dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{", .format.std.errors.left, .iround(ci.lower.bound,.format.round.digits),hyphen,.iround(ci.upper.bound,.format.round.digits),.format.std.errors.right,"}",sep="")
            }
            else {
              cat(" & ", .format.std.errors.left, .iround(ci.lower.bound,.format.round.digits),hyphen,.iround(ci.upper.bound,.format.round.digits),.format.std.errors.right,sep="")              
            }


          }
          else { 
  				  cat(" & ", .format.std.errors.left, .iround(.global.std.errors[.global.coefficient.variables[which.variable],i],.format.round.digits),.format.std.errors.right,sep="")
          }

  				# add stars to denote statistical significance
  				if (part=="standard error*") { 
  					p.value <- .global.p.values[.global.coefficient.variables[which.variable],i]
  					.enter.significance.stars(p.value) 
  				}

  			}
  			else {
  				cat(" & ",sep="")
  			}
  		}
  		cat(" \\\\ \n ")
  	}


  	# p-values
  	else if ((part=="p-value") || (part=="p-value*")) {
  		for (i in seq(1:length(.global.models))) {
  			if (!is.na(.global.p.values[.global.coefficient.variables[which.variable],i])) {

  				# report p-values
  				cat(" & ", .format.p.values.left, .iround(.global.p.values[.global.coefficient.variables[which.variable],i],.format.round.digits,round.up.positive=TRUE),.format.p.values.right,sep="")

  				# add stars to denote statistical significance
  				if (part=="p-value*") { 
  					p.value <- .global.p.values[.global.coefficient.variables[which.variable],i]
  					.enter.significance.stars(p.value) 
  				}

  			}
  			else {
  				cat(" & ",sep="")
  			}
  		}
  		cat(" \\\\ \n ")
  	}

  	# t-statistics
  	else if ((part=="t-stat") || (part=="t-stat*")) {
  		for (i in seq(1:length(.global.models))) {
  			if (!is.na(.global.t.stats[.global.coefficient.variables[which.variable],i])) {
  				# report t-statistics
  				cat(" & ", .format.t.stats.left, .iround(.global.t.stats[.global.coefficient.variables[which.variable],i],.format.round.digits),.format.t.stats.right,sep="")

  				# add stars to denote statistical significance
  				if (part=="t-stat*") { 
  					p.value <- .global.p.values[.global.coefficient.variables[which.variable],i]
  					.enter.significance.stars(p.value) 
  				}

  			}
  			else {
  				cat(" & ",sep="")
  			}
  		}
  		cat(" \\\\ \n ")
  	}


  	# empty line
  	else if (part==" ") {
  		.table.empty.line()
  	}

  	# horizontal line
  	else if (part=="-") {
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n")
  	}

  	# double horizontal line
  	else if (part=="=") {
  		cat("\\hline \n") 
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n")
  	}

  }

  .coefficient.variables <-
  function(object.name) {
	
  	model.name <- .get.model.name(object.name)

  	if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.gee", "logit.gee", "probit.gee", "poisson.gee", "normal.gam", 
  				    "logit.gam", "probit.gam", "poisson.gam", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.gee", "gamma.survey",
  				    "exp", "weibull", "coxph", "clogit", "lognorm", "tobit", "tobit(AER)", "brglm", "glm()", "Glm()", "svyglm()", "gee()", "survreg()", "gam()", "plm", "ivreg", "pmg", "lmrob", "glmrob", 
              "dynlm", "gls", "rq", "lagsarlm", "errorsarlm", "gmm", "mclogit")) {
  		return(as.vector(names(object.name$coefficients)))
  	}
  	else if (model.name %in% c("Arima")) {
  	  return(names(object.name$coef))
  	}
  	else if (model.name %in% c("fGARCH")) {
  	  return(rownames(object.name@fit$matcoef))
  	}
  	else if (model.name %in% c("censReg")) {
  	  return(rownames(.summary.object$estimate))
  	}
  	else if (model.name %in% c("mnlogit")) {
  	  return(rownames(.summary.object$CoefTable))
  	}
  	else if (model.name %in% c("lme","nlme")) {
  	  return(rownames(.summary.object$tTable))
  	}
  	else if (model.name %in% c("felm")) {
  	  return(row.names(object.name$coefficients))
    }
  	else if (model.name %in% c("maBina")) {
  	  return(as.vector(rownames(object.name$out)))
  	}
  	else if (model.name %in% c("mlogit")) {
  	  return(as.vector(rownames(.summary.object$CoefTable)))
  	}
    else if (model.name %in% c("hetglm")) {
      return(as.vector(names(object.name$coefficients$mean)))
    }
  	else if (model.name %in% c("selection","heckit")) {
      if (!.global.sel.equation) {
  	    indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
  	  return(as.vector(names(.summary.object$estimate[indices, 1])))
  	}
    else if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(names(.summary.object$estimate[,1])))
    }
  	else if (model.name %in% c("coeftest")) {
  	  return(as.vector(rownames(object.name)))
  	}
    else if (model.name %in% c("clm")) {
      if (.format.ordered.intercepts == FALSE) { return(as.vector(names(object.name$beta))) }
      else { return(c(as.vector(names(object.name$beta)), as.vector(names(object.name$alpha)))) }
    }
    else if (model.name %in% c("lmer", "glmer", "nlmer", "pgmm")) {
      return(as.vector(rownames(.summary.object$coefficients)))
    }
    else if (model.name %in% c("ergm", "rem.dyad")) {
      return(as.vector(names(object.name$coef)))
    }
    else if (model.name %in% c("betareg")) {
      return(as.vector(names(object.name$coefficients$mean)))
    }
  	else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (.global.zero.component==FALSE) {
        return(as.vector(names(object.name$coefficients$count)))
      }
      else {
        return(as.vector(names(object.name$coefficients$zero)))
      }
  	}
  	else if (model.name %in% c("cloglog.net", "gamma.net", "logit.net", "probit.net")) {
  		return(as.vector(rownames(.summary.object$coefficients))) 
  	}
    else if (model.name %in% c("rlm")) {
      return(as.vector(rownames(suppressMessages(.summary.object$coefficients))))
    }
  	else if (model.name %in% c("ologit", "oprobit", "polr()")) {
  		coef.temp <- as.vector(rownames(suppressMessages(.summary.object$coefficients)))
  		if (.format.ordered.intercepts == FALSE) { return(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
  		else { return(coef.temp) }
  	}
  	else if (model.name %in% c("arima")) {
  		return(as.vector(names(object.name$coef)))
  	}
    else if (model.name %in% c("multinom")) {
      return(as.vector(object.name$coefnames))
    }
  	else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
  	  return(as.vector(names(object.name$coefficients)))
  	}
    
  	
  	return(NULL)
  }

  .dependent.variable <-
  function(object.name, model.num=1) {
    
    model.name <- .get.model.name(object.name)
    if (model.name %in% c("lmer", "glmer", "nlmer", "gls")) {
      return(as.vector(as.character(formula(object.name))[2]))
    }
    if (model.name %in% c("Arima")) {
      return(as.character(object.name$call$x))
    }
    if (model.name %in% c("fGARCH")) {
        return(as.character(object.name@call$data))
    }
    if (model.name %in% c("multinom")) {
      if (!is.null(rownames(.summary.object$coefficients))) {
        return(as.vector(rownames(.summary.object$coefficients)[model.num]))
      }
    }
    if (model.name %in% c("rem.dyad", "coeftest")) {
      return(as.vector(as.character(" ")))
    }
    if (model.name %in% c("gmm")) {
      formula <- object.name$call[2]
      position <- regexpr("~", formula, fixed=T)
      return( .trim(substr(formula, 1, position-1)) )
    }
    if (model.name %in% c("selection","heckit")) {
      if (!.global.sel.equation) {
        formula <- object.name$call["outcome"]    ### outcome
      }
      else {
        formula <- object.name$call["selection"]    ### outcome
      }
      position <- regexpr("~", formula, fixed=T)
      return( .trim(substr(formula, 1, position-1)))
    }
    if (model.name %in% c("probit.ss","binaryChoice")) {
      formula <- object.name$call["formula"]
      position <- regexpr("~", formula, fixed=T)
      return( .trim(substr(formula, 1, position-1)))
    }
    if (model.name %in% c("maBina")) {
      object.name <- object.name$w
    }
    
    if (model.name %in% c("lme")) {
      object.name$call$formula <- object.name$call$fixed
    }
    if (model.name %in% c("nlme")) {
      object.name$call$formula <- object.name$call$model
    }
    
    if (!is.null(object.name$call$formula)) {
      if (is.symbol(object.name$call$formula)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$call$formula
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {
        return(as.vector(as.character(formula.temp)[2]))
      }
    }
    if (!is.null(object.name$formula)) {
      if (is.symbol(object.name$formula)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$formula
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {   # this is for zelig$result ones
        return(as.vector(as.character(formula.temp)[2])) 
      }
    }
    if (!is.null(object.name$formula2)) {
      if (is.symbol(object.name$formula2)) {
        formula.temp <- as.formula(object.name)  
      }
      else {
        formula.temp <- object.name$formula2
      }
      
      if (length(as.vector(as.character(formula.temp)))>1) {   # z.ls
        return(as.vector(as.character(formula.temp)[2])) 
      }      
    }
    return("")  
  }
  
  .dependent.variable.written <-
  function(object.name, model.num=1) {
	
  	model.name <- .get.model.name(object.name)

  	if (model.name %in% c("tobit","ologit","oprobit", "relogit", "coxph","exp","lognorm","weibull","survreg()","arima",
                          "aftreg", "weibreg", "coxreg", "phreg", "bj", "cph", "psm")) {
  		written.var <- .inside.bracket(.dependent.variable(object.name))[1] 
  	}
  	else if (model.name %in% c("clogit","mclogit")) {
  	  written.var <- .inside.bracket(.dependent.variable(object.name))[2] 
  	}
  	else { written.var <- .dependent.variable(object.name, model.num) }
    
    # some formatting changes
  	# remove everything before and including he last dollar sign from variable name
  	temp <- strsplit(written.var,"$",fixed=TRUE)
  	written.var <- temp[[1]][length(temp[[1]])]
  	
  	# if underscore or ^, etc. in variable name, then insert an escape \ before it
  	written.var <- .remove.special.chars(written.var)
  	
    return(written.var)
  }

  .enter.significance.stars <-
  function(p.value, force.math=FALSE) {
    if ((!is.na(p.value)) && (!is.null(p.value))) {
      
      if (.format.dec.mark.align == TRUE) {
        c <- "" 
      }
      else {
        c <- "$"  
      }
      if (force.math == TRUE) { c <- "$" }
      
      cutoffs <- .format.cutoffs[length(.format.cutoffs):1]
      stars <- .format.stars[length(.format.stars):1]
      
      for (i in 1:length(cutoffs)) {
        if (!is.na(cutoffs[i])) {
          if (p.value < cutoffs[i]) {
            cat(c,"^{",stars[i],"}",c,sep="") 
            break
          }    
        }
      }
    
    
    }

  }

  .F.stat <-
  function(object.name) {
  	F.stat.output <- as.vector(rep(NA,times=4))

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","fGARCH", "Arima", "maBina","coeftest", "lmer", "glmer", "nlmer", "Gls"))) {
      if (model.name %in% c("plm")) {
        F.stat.value <- .summary.object$fstatistic$statistic
        df.numerator <- .summary.object$fstatistic$parameter["df1"]
        df.denominator <- .summary.object$fstatistic$parameter["df2"]
        F.stat.p.value <- .summary.object$fstatistic$p.value
        
        F.stat.output <- as.vector(c(F.stat.value, df.numerator, df.denominator, F.stat.p.value))
      }
  		else if (!is.null(suppressMessages(.summary.object$fstatistic["value"]))) {
  			F.stat.value <- .summary.object$fstatistic["value"]
  			df.numerator <- .summary.object$fstatistic["numdf"]
  			df.denominator <- .summary.object$fstatistic["dendf"]
  			F.stat.p.value <- pf(F.stat.value, df.numerator, df.denominator, lower.tail=FALSE)

  			F.stat.output <- as.vector(c(F.stat.value, df.numerator, df.denominator, F.stat.p.value))
  		}
  	}

  	names(F.stat.output) <- c("statistic","df1","df2","p-value")
  	return(cbind(F.stat.output))
  }

  .gcv.UBRE <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH", "Arima", "maBina", "coeftest", "lmer", "Gls", "glmer", "nlmer"))) {
      if (!is.null(object.name$gcv.ubre)) {
        return(as.vector(object.name$gcv.ubre))
      }
    }
    return(NA)
  }
  
  # fill in NAs into a if b is the longer vector
  .fill.NA <-
  function(a, b) {
    a.temp <- a; b.temp <- b
    if (length(a) >= length(b)) {
      return(a.temp)
    }
    else {
      length(a.temp) <- length(b)
      return(a.temp)
    }
  }

  .get.model.name <-
  function(object.name) {
  	return.value <- .model.identify(object.name)
  	if (substr(return.value,1,5)=="glm()") { return.value <- "glm()" }
  	if (substr(return.value,1,8)=="svyglm()") { return.value <- "svyglm()" }
  	if (substr(return.value,1,5)=="gee()") { return.value <- "gee()" }
  	if (substr(return.value,1,5)=="gam()") { return.value <- "gam()" }
  	if (substr(return.value,1,6)=="polr()") { return.value <- "polr()" }
  	if (substr(return.value,1,9)=="survreg()") { return.value <- "survreg()" }
  	return(return.value)
  }

  .get.p.values.1 <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL,  model.num=1) {

    if (!is.null(user.given)) {
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { 
          user.given <- as.vector(user.given[model.num,]) 
        }
      }
      
      return(user.given) 
    }
    
        
    if (auto == TRUE) {
      if ((!is.null(user.coef)) || (!is.null(user.se))) {
        
        #if (.model.identify(object.name) == "multinom") {
        #  f.coef <- as.vector(f.coef[model.num,])
        #  f.se <- as.vector(f.se[model.num,])
        #}
        
        
        # set the lengths of the vectors to be equal to each other
        coef.div <- .fill.NA(f.coef, f.se)
        se.div <- .fill.NA(f.se, f.coef)
        
        t.out <- (coef.div / se.div)
        
        auto.return <- 2*pnorm(abs(t.out), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
        names(auto.return) <- names(f.coef)
        return( auto.return  )
      }
    }

    model.name <- .get.model.name(object.name)
    
  	if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                            "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "rq", "gmm","mclogit","felm")) {
  		return(.summary.object$coefficients[,4])
  	}
    if (model.name %in% c("censReg")) {
      return(.summary.object$estimate[,4])
    }
    if (model.name %in% c("mnlogit")) {
      return(.summary.object$CoefTable[,4])
    }
    if (model.name %in% c("fGARCH")) {
      return(object.name@fit$matcoef[,4])
    }
    if (model.name %in% c("lme", "nlme")) {
      return(.summary.object$tTable[,5])
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,4]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,4]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,4]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,4]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!.global.sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,4]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,4]))
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,4])
    }
  	if (model.name %in% c("lmer", "glmer", "nlmer")) {
  	  Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
  	  coefs <- .summary.object$coefficients[,1]
  	  se <- sqrt(diag(Vcov))
  	  tstat <- coefs / se
  	  pval <- 2 * pnorm(abs(tstat), lower.tail = FALSE)
      names(pval) <- names(coefs)
  	  return(pval)
  	}
    if (model.name %in% c("Arima")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$var.coef))
      tstat <- coef.temp / se.temp 
      pval <- 2 * pnorm(abs(tstat), lower.tail = FALSE)
      return(pval)
    }
  	if (model.name %in% c("ergm")) {
  	  return(.summary.object$coefs[,4])
  	}
    if (model.name %in% c("clm")) {
      if (.format.ordered.intercepts == FALSE) {
        return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),4])
      }
      else {
        return(.summary.object$coefficients[,4])
      }
    }
    else if (model.name %in% c("pmg")) {
      coef.temp <- .summary.object$coefficients
      std.err.temp <- sqrt(diag(.summary.object$vcov))
      t.stat.temp <- coef.temp / std.err.temp
      df.temp <- length(.summary.object$residuals)
      return( 2 * pt(abs(t.stat.temp), df=df.temp, lower.tail = FALSE, log.p = FALSE) )
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (.global.zero.component==FALSE) {
        return(.summary.object$coefficients$count[,4])  
      }
      else {
        return(.summary.object$coefficients$zero[,4])
      }
      
    }
  	else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee", "probit.gee", "gamma.gee", "gee()")) {
  		return(2*pnorm(abs(.summary.object$coefficients[,"Robust z"]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE))
  	}
  	else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
  		return(.summary.object$p.pv)
  	}
  	else if (model.name %in% c("coxph", "clogit")) {
  		return(.summary.object$coef[,"Pr(>|z|)"])
  	}
  	else if (model.name %in% c("exp","lognorm","weibull","tobit", "survreg()")) {
  		return(.summary.object$table[,"p"])
  	}
    else if (model.name %in% c("rlm")) {
      coef.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
      coef.temp <- 2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(coef.temp)
    }
  	else if (model.name %in% c("ologit", "oprobit", "polr()")) {
  		coef.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
  		if (.format.ordered.intercepts == FALSE) { return(2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)) }
  		else { 
  		  return( 2*pnorm(abs(coef.temp[seq(from=1, to=length(coef.temp))]), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) ) 
      }
  		
  	}
  	else if (model.name %in% c("arima")) {
  		return(2*pnorm( abs(object.name$coef / (sqrt(diag(object.name$var.coef))) ), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE))
  	}
    else if (model.name %in% c("tobit(AER)")){
      return(.summary.object$coefficients[,"Pr(>|z|)"])
    }
    else if (model.name %in% c("multinom")) {
      if (is.null(nrow(.summary.object$coefficients))) {
        coef.temp <- .summary.object$coefficients
        se.temp <- .summary.object$standard.errors
      }
      else {
        coef.temp <- .summary.object$coefficients[model.num,]
        se.temp <- .summary.object$standard.errors[model.num,]
      }
      return( 2*pnorm( abs( (coef.temp) / (se.temp) ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE) )
    }
  	else if (model.name %in% c("betareg")) {
  	  return(.summary.object$coefficients$mean[,"Pr(>|z|)"])
  	}
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$varBeta))
      t.temp <- coef.temp / se.temp
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$var))
      t.temp <- coef.temp / se.temp 
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    else if (model.name %in% c("rem.dyad")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$cov))
      t.temp <- coef.temp / se.temp
      p.temp <- 2*pnorm( abs( t.temp ) , mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      return(p.temp)
    }
    return(NULL)
  }
  
  .get.p.values <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
      out <- .get.p.values.1(object.name, user.given, auto, f.coef, f.se, user.coef, user.se, model.num)
      
      coef.vars <- .coefficient.variables(object.name)
      if (is.null(names(out))) {  
        
        if (length(out) < length(coef.vars)) {
          out.temp <- rep(NA, times=length(coef.vars)-length(out))
          out <- c(out, out.temp)
        }
        else if (length(out) > length(coef.vars)) {
          out <- out[1:length(coef.vars)]
        }
        
        names(out) <- coef.vars   
      }
      else {
        out.temp <- rep(NA, times = length(coef.vars))
        names(out.temp) <- coef.vars
        for (i in 1:length(out)) {
          name <- names(out)[i]
          if (name %in% coef.vars) {
            out.temp[name] <- out[i]
          }
        }
        out <- out.temp
      }
      return(out)
  }
  

  .get.scale <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if (!is.null(object.name$scale)) {
        if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee", "probit.gee", "gamma.gee", "gee()", "exp","lognorm","weibull","tobit","survreg()","tobit(AER)")) {
          return(as.vector(object.name$scale))
        }
      }
    }
    return(NA)
  }

  .get.sigma2 <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("arima","fGARCH","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer")) {
      return(NA)
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$s2)
    }  
    if (!is.null(object.name$sigma2)) {
        return(as.vector(object.name$sigma2))
    }
    return(NA)
  }
  
  .get.rho <-
  function(object.name) {
      
    model.name <- .get.model.name(object.name)
    rho.output <- as.vector(rep(NA,times=4))
      
    if (model.name %in% c("selection")) {
      i <- object.name$param$index$rho
      if (is.null(i)) { i <- object.name$param$index$errTerms["rho"] }
      if (!is.null(i)) {
        rho.output <- as.vector(.summary.object$estimate[i,])
      }
    }
    if (model.name %in% c("heckit")) {
      if (object.name$method == "2step") {
        i <- object.name$param$index$rho
        rho.output <- as.vector(.summary.object$estimate[i,])
      }
    }
      
    names(rho.output) <- c("statistic","se","tstat","p-value")
    return(cbind(rho.output))
  }
  
  .get.mills <-
    function(object.name) {
      
      model.name <- .get.model.name(object.name)
      mills.output <- as.vector(rep(NA,times=4))
      
      if (model.name %in% c("heckit", "selection")) {
        i <- object.name$param$index$Mills
        if (!is.null(i)) {
                mills.output <- as.vector(.summary.object$estimate[i,])
        }
      }
      
      names(mills.output) <- c("statistic","se","tstat","p-value")
      return(cbind(mills.output))
    }

  .get.standard.errors.1 <-
  function(object.name, user.given=NULL, model.num=1) {
    
    if (!is.null(user.given)) { 
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      }
      
      return(user.given) 
    }

  	model.name <- .get.model.name(object.name)

  	if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
                            "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm","mclogit")) {
  		return(.summary.object$coefficients[,"Std. Error"])
  	}
  	if (model.name %in% c("Arima")) {
  	  return(sqrt(diag(object.name$var.coef)))
  	}
  	if (model.name %in% c("censReg")) {
  	  return(.summary.object$estimate[,2])
  	}
  	if (model.name %in% c("mnlogit")) {
  	  return(.summary.object$CoefTable[,2])
  	}
  	if (model.name %in% c("fGARCH")) {
  	  return(object.name@fit$matcoef[,2])
  	}
  	if (model.name %in% c("lme", "nlme")) {
  	  return(.summary.object$tTable[,2])
  	}
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,2]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,2]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,2]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!.global.sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,2]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,2]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,2]))
    }
  	if (model.name %in% c("lmer", "glmer", "nlmer")) {
  	  Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
  	  coefs <-.summary.object$coefficients[,1]
  	  se <- sqrt(diag(Vcov))
      names(se) <- names(coefs)
  	  return(se)
  	}
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,2])
    }    
  	if (model.name %in% c("ergm")) {
  	  return(.summary.object$coefs[,2])
  	}
    if (model.name %in% c("rq","felm")) {
      return(.summary.object$coefficients[,2])
    }
  	if (model.name %in% c("clm")) {
  	  if (.format.ordered.intercepts == FALSE) {
  	    return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),2])
  	  }
  	  else {
  	    return(.summary.object$coefficients[,2])
  	  }
  	}
  	else if (model.name %in% c("pmg")) {
  	  return (sqrt(diag(.summary.object$vcov)))
  	}
  	if (model.name %in% c("zeroinfl", "hurdle")) {
      if (.global.zero.component == FALSE) {
        return(.summary.object$coefficients$count[,"Std. Error"])  
      }
      else {
        return(.summary.object$coefficients$zero[,"Std. Error"])
      }
  	}
  	else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",  "probit.gee", "gamma.gee", "gee()")) {
  		return(.summary.object$coefficients[,"Robust S.E."])
  	}
  	else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
  	  temp.se <- .summary.object$se
      names(temp.se) <- names(.summary.object$p.coeff)
      return(temp.se)
  	}
  	else if (model.name %in% c("coxph")) {
  		return(.summary.object$coef[,"se(coef)"])
  	}
    else if (model.name %in% c("clogit")) {
      return(.summary.object$coef[,"se(coef)"])
      
    }
  	else if (model.name %in% c("exp","lognorm","weibull","tobit","survreg()")) {
  		return(.summary.object$table[,"Std. Error"])
  	}
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"Std. Error"]))
    }
  	else if (model.name %in% c("ologit", "oprobit", "polr()")) {
  		se.temp <- suppressMessages(.summary.object$coefficients[,"Std. Error"])
  		if (.format.ordered.intercepts == FALSE) { return(se.temp[seq(from=1, to=length(se.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
  		else { return(se.temp) }
  	}
  	else if (model.name %in% c("arima")) {
  		return( sqrt(diag(object.name$var.coef)) )
  	}
  	else if (model.name %in% c("tobit(AER)")){
  	  return(.summary.object$coefficients[,"Std. Error"])
  	}
  	else if (model.name %in% c("multinom")) {
  	  if (is.null(nrow(.summary.object$coefficients))) {
  	    se.temp <- .summary.object$standard.errors
  	  }
  	  else {
  	    se.temp <- .summary.object$standard.errors[model.num,]
  	  }
  	  return(se.temp)
  	}
  	else if (model.name %in% c("betareg")) {
  	  return(.summary.object$coefficients$mean[,"Std. Error"])
  	}
    else if (model.name %in% c("gls")) {
      se.temp <- sqrt(diag(object.name$varBeta))
      return(se.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return( sqrt(diag(object.name$var) ) )
    }
    else if (model.name %in% c("rem.dyad")) {
      return( sqrt(diag(object.name$cov) ) )
    }
    return(NULL)
  }
  
  .get.standard.errors <-
  function(object.name, user.given=NULL, model.num=1) {
      out <- .get.standard.errors.1(object.name, user.given, model.num)
      
      coef.vars <- .coefficient.variables(object.name)
      if (is.null(names(out))) {  
        
        if (length(out) < length(coef.vars)) {
          out.temp <- rep(NA, times=length(coef.vars)-length(out))
          out <- c(out, out.temp)
        }
        else if (length(out) > length(coef.vars)) {
          out <- out[1:length(coef.vars)]
        }
        
        names(out) <- coef.vars   
      }
      else {
        out.temp <- rep(NA, times = length(coef.vars))
        names(out.temp) <- coef.vars
        for (i in 1:length(out)) {
          name <- names(out)[i]
          if (name %in% coef.vars) {
            out.temp[name] <- out[i]
          }
        }
        out <- out.temp
      }
      return(out)
  }
  
  .get.ci.lb.1 <-
    function(object.name, user.given=NULL, model.num=1) {
      if (!is.null(user.given)) { 
        if (.model.identify(object.name) == "multinom") {
          if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
        } 
        return(user.given) 
      }
      return(NULL)
    }
  
  .get.ci.lb <-
    function(object.name, user.given=NULL, model.num=1) {

      out <- .get.ci.lb.1(object.name, user.given, model.num)
      
      coef.vars <- .coefficient.variables(object.name)
      if (is.null(names(out))) {  
        
        if (length(out) < length(coef.vars)) {
          out.temp <- rep(NA, times=length(coef.vars)-length(out))
          out <- c(out, out.temp)
        }
        else if (length(out) > length(coef.vars)) {
          out <- out[1:length(coef.vars)]
        }
        
        names(out) <- coef.vars   
      }
      else {
        out.temp <- rep(NA, times = length(coef.vars))
        names(out.temp) <- coef.vars
        for (i in 1:length(out)) {
          name <- names(out)[i]
          if (name %in% coef.vars) {
            out.temp[name] <- out[i]
          }
        }
        out <- out.temp
      }
      return(out)
    }
  
  .get.ci.rb.1 <-
    function(object.name, user.given=NULL, model.num=1) {
      if (!is.null(user.given)) { 
        if (.model.identify(object.name) == "multinom") {
          if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
        } 
        return(user.given) 
      }
      return(NULL)
    }
  
  .get.ci.rb <-
    function(object.name, user.given=NULL, model.num=1) {
      
      out <- .get.ci.rb.1(object.name, user.given, model.num)
      
      coef.vars <- .coefficient.variables(object.name)
      if (is.null(names(out))) {  
        
        if (length(out) < length(coef.vars)) {
          out.temp <- rep(NA, times=length(coef.vars)-length(out))
          out <- c(out, out.temp)
        }
        else if (length(out) > length(coef.vars)) {
          out <- out[1:length(coef.vars)]
        }
        
        names(out) <- coef.vars   
      }
      else {
        out.temp <- rep(NA, times = length(coef.vars))
        names(out.temp) <- coef.vars
        for (i in 1:length(out)) {
          name <- names(out)[i]
          if (name %in% coef.vars) {
            out.temp[name] <- out[i]
          }
        }
        out <- out.temp
      }
      return(out)
    }

  .get.t.stats.1 <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
    
    if (!is.null(user.given)) { 
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { 
          user.given <- as.vector(user.given[model.num,]) 
        }
      }
      
      return(user.given) 
    }
    
    if (auto == TRUE) {
      if ((!is.null(user.coef)) || (!is.null(user.se))) {
        
        #if (.model.identify(object.name) == "multinom") {
        #  f.coef <- as.vector(f.coef[model.num,])
        #  f.se <- as.vector(f.se[model.num,])
        #}
        
        # set the lengths of the vectors to be equal to each other
        coef.div <- .fill.NA(f.coef, f.se)
        se.div <- .fill.NA(f.se, f.coef) 
        
        auto.return <- coef.div / se.div
        names(auto.return) <- names(f.coef)
        
        return(auto.return)
      }
    }

  	model.name <- .get.model.name(object.name)

  	if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
      				    "cloglog.net", "gamma.net", "logit.net", "probit.net", "glm()", "Glm()", "svyglm()","plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm", "mclogit", "felm")) {
  		return(.summary.object$coefficients[,3])
  	}
  	if (model.name %in% c("censReg")) {
  	  return(.summary.object$estimate[,3])
  	}
  	if (model.name %in% c("mnlogit")) {
  	  return(.summary.object$CoefTable[,3])
  	}
  	if (model.name %in% c("fGARCH")) {
  	  return(object.name@fit$matcoef[,3])
  	}
  	if (model.name %in% c("lme", "nlme")) {
  	  return(.summary.object$tTable[,4])
  	}
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,3]))
    }
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,3]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,3]))
    }
    if (model.name %in% c("selection","heckit")) {
      if (!.global.sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,3]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,3]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,3]))
    }
  	if (model.name %in% c("lmer", "glmer", "nlmer")) {
  	  Vcov <- as.matrix(vcov(object.name, useScale = FALSE))
  	  coefs <- .summary.object$coefficients[,1]
  	  se <- sqrt(diag(Vcov))
  	  tstat <- coefs / se
  	  names(tstat) <- names(coefs)
  	  
  	  return(tstat)
  	}
  	if (model.name %in% c("ergm")) {
  	  return((.summary.object$coefs[,1])/(.summary.object$coefs[,2]))
  	}
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,3])
    }    
    if (model.name %in% c("rq")) {
      return(.summary.object$coefficients[,3])
    }
  	if (model.name %in% c("clm")) {
  	  if (.format.ordered.intercepts == FALSE) {
  	    return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),3])
  	  }
  	  else {
  	    return(.summary.object$coefficients[,3])
  	  }
  	}
  	else if (model.name %in% c("pmg")) {
  	  coef.temp <- .summary.object$coef
  	  std.err.temp <- sqrt(diag(.summary.object$vcov))
  	  t.stat.temp <- coef.temp / std.err.temp
  	  return(t.stat.temp)
    }
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (.global.zero.component == FALSE) {
        return(.summary.object$coefficients$count[,3])  
      }
      else {
        return(.summary.object$coefficients$zero[,3])
      }
      
    }
  	else if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",  "probit.gee", "gamma.gee", "gee()")) {
  		return(.summary.object$coefficients[,"Robust z"])
  	}
  	else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
  		return(.summary.object$p.t)
  	}
  	else if (model.name %in% c("coxph", "clogit")) {
  		return(.summary.object$coef[,"z"])
  	}
  	else if (model.name %in% c("exp","lognorm","weibull", "tobit","survreg()")) {
  		return(.summary.object$table[,"z"])
  	}
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"t value"]))
    }
  	else if (model.name %in% c("ologit", "oprobit", "polr()")) {
  		tstat.temp <- suppressMessages(.summary.object$coefficients[,"t value"])
  		if (.format.ordered.intercepts == FALSE) { return(tstat.temp[seq(from=1, to=length(tstat.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
  		else { return(tstat.temp) }
  	}
  	else if (model.name %in% c("arima")) {
  		return( object.name$coef / (sqrt(diag(object.name$var.coef))) )
  	}
  	else if (model.name %in% c("tobit(AER)")){
  	  return(.summary.object$coefficients[,"z value"])
  	}
  	else if (model.name %in% c("multinom")) {
  	  if (is.null(nrow(.summary.object$coefficients))) {
  	    coef.temp <- .summary.object$coefficients
  	    se.temp <- .summary.object$standard.errors
  	  }
  	  else {
  	    coef.temp <- .summary.object$coefficients[model.num,]
  	    se.temp <- .summary.object$standard.errors[model.num,]
  	  }
  	  return( (coef.temp) / (se.temp) )
  	}
  	else if (model.name %in% c("betareg")) {
  	  return(.summary.object$coefficients$mean[,"z value"])
  	}
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$varBeta))
      return(coef.temp / se.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      coef.temp <- object.name$coefficients
      se.temp <- sqrt(diag(object.name$var))
      return(coef.temp / se.temp )
    }
  	else if (model.name %in% c("Arima")) {
  	  coef.temp <- object.name$coef
  	  se.temp <- sqrt(diag(object.name$var.coef))
  	  return(coef.temp / se.temp )
  	}
    else if (model.name %in% c("rem.dyad")) {
      coef.temp <- object.name$coef
      se.temp <- sqrt(diag(object.name$cov))
      return(coef.temp / se.temp )
    }
  	
  	return(NULL)
  }
  
  .get.t.stats <-
  function(object.name, user.given=NULL, auto=TRUE, f.coef=NULL, f.se=NULL, user.coef=NULL, user.se=NULL, model.num=1) {
    out <- .get.t.stats.1(object.name, user.given, auto, f.coef, f.se, user.coef, user.se, model.num)

    coef.vars <- .coefficient.variables(object.name)
    if (is.null(names(out))) {  
      
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
    }
    return(out)
  }
  

  .get.theta <-
  function(object.name) {
    theta.output <- as.vector(rep(NA,times=4))
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if ((!is.null(object.name$theta)) && (!is.null(object.name$SE.theta))) {
        theta.value <- object.name$theta
        theta.se.value <- object.name$SE.theta
        theta.tstat.value <- theta.value / theta.se.value
        theta.p.value <- 2*pnorm(abs(theta.tstat.value), mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
      
        theta.output <- as.vector(c(theta.value, theta.se.value, theta.tstat.value, theta.p.value))
      }
    }
  
    names(theta.output) <- c("statistic","se","tstat","p-value")
    return(cbind(theta.output))
  }
  
  .inside.bracket <-
    function(s) {
      process.string <- ""
      return.vector <- NULL
      if (length(s) > 1) { return("") }
      if (!is.character(s)) { return("") }
      if (is.null(s)) { return("") }
      if (is.na(s)) { return("") }
      if (s=="") { return("") }

      
      inside.inner.bracket <- 0
      for (i in seq(from = (regexpr("(",s,fixed=TRUE)[1])+1, to = nchar(s))) {
        letter <- substr(s,i,i)
        if (letter == "(") { inside.inner.bracket <- inside.inner.bracket + 1 }
        if (letter == ")") { inside.inner.bracket <- inside.inner.bracket - 1 }
        
        if ((letter == ",") && (inside.inner.bracket == 0)) {
          return.vector <- c(return.vector, process.string)
          process.string <- ""
        }
        else if (inside.inner.bracket >= 0) { process.string <- paste(process.string, letter, sep="") }
        else { break } 
      }
      if (process.string != "") { return.vector <- c(return.vector, process.string) }
      return (.trim(return.vector))
    }

  .iround <- 
    function(x, decimal.places=0, round.up.positive=FALSE, simply.output=FALSE) {
      
      x.original <- x
      first.part <- ""
      
      if (is.na(x) || is.null(x)) { return("") }
      
      if (simply.output == TRUE) {
        if (!is.numeric(x)) { return(.remove.special.chars(x)) }
      }
      
      if (x.original < 0) { x <- abs(x) }
      
      if (!is.na(decimal.places)) {
        
        if ((.format.until.nonzero.digit == FALSE) || (decimal.places <= 0)) {
          round.result <- round(x, digits=decimal.places)
        }
        else {
          temp.places <- decimal.places
          if (!.is.all.integers(x)) {
            while ((round(x, digits=temp.places) == 0) && (temp.places < (decimal.places + .format.max.extra.digits))) {
              temp.places <- temp.places + 1
            }
          }
          round.result <- round(x, digits=temp.places)
          decimal.places <- temp.places
        }
        
        if ((round.up.positive==TRUE) && (round.result < x)) {       # useful for p-values that should be rounded up
          if (x > (10^((-1)*(decimal.places+1)))) {
            round.result <- round.result + 10^((-1)*decimal.places)
          }
          else { round.result <- 0 }
        }
      }
      else {      # if the decimal place is NA
        round.result <- x
      }
      
      round.result.char <- as.character(format(round.result, scientific=FALSE))
      split.round.result <- unlist(strsplit(round.result.char, "\\."))
      
      ## first deal with digit separator
      
      for (i in seq(from=1, to=length(.format.digit.separator.where))) {
        if (.format.digit.separator.where[i]<=0) {
          .format.digit.separator.where[i] <<- -1
        }
      }
      
      separator.count <- 1
      length.integer.part <- nchar(split.round.result[1])
      
      digits.in.separated.unit <- 0
      for (i in seq(from=length.integer.part, to=1)) {
        if ((digits.in.separated.unit == .format.digit.separator.where[separator.count]) && (substr(split.round.result[1],i,i)!="-")){
          first.part <- paste(.format.digit.separator,first.part,sep="")
          if (separator.count < length(.format.digit.separator.where)) { separator.count <- separator.count + 1 }
          digits.in.separated.unit <- 0	
        }
        first.part <- paste(substr(split.round.result[1],i,i),first.part,sep="")
        digits.in.separated.unit <- digits.in.separated.unit + 1
        
      }	
      
      # remove initial zero and there are decimal places, if that is requested
      if (.format.initial.zero==FALSE)  {
        if ((round.result > 0) && (round.result < 1)) {
          if ((is.na(decimal.places)) || (decimal.places > 0)) {
            first.part <- ""
          }
        }
      }
      
      if (x.original < 0) {    # use math-mode for a better looking negative sign
        if (.format.dec.mark.align == TRUE) {
          first.part <- paste("-", first.part, sep="")
        }
        else {
          first.part <- paste("$-$", first.part, sep="")  
        }
      }
      
      # now deal with the decimal part
      if (!is.na(decimal.places)) {
        if (decimal.places <= 0) {
          return(first.part) 
        }
      }
      

      
      if (length(split.round.result)==2) {
        if (is.na(decimal.places)) { return(paste(first.part,.format.decimal.character,split.round.result[2],sep="")) }
        if (nchar(split.round.result[2]) < decimal.places) {
          decimal.part <- split.round.result[2]
          for (i in seq(from = 1,to = (decimal.places - nchar(split.round.result[2])))) {
            decimal.part <- paste(decimal.part,"0", sep="")
          }
          return(paste(first.part,.format.decimal.character,decimal.part,sep=""))
        }
        else { return(paste(first.part,.format.decimal.character,split.round.result[2],sep="")) }
      }
      else if (length(split.round.result)==1) { 
        if (is.na(decimal.places)) { return(paste(first.part,.format.decimal.character,decimal.part,sep="")) }
        decimal.part <- ""
        for (i in seq(from = 1,to = decimal.places)) {
          decimal.part <- paste(decimal.part,"0", sep="")
        }
        return(paste(first.part,.format.decimal.character,decimal.part,sep=""))
      }
      else { return(NULL) }
    }
  
  is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  .is.all.integers <-
  function(x) {
      if (!is.numeric(x)) { return(FALSE) }
      if (length(x[!is.na(x)]) == length(is.wholenumber(x)[(!is.na(x)) & (is.wholenumber(x)==TRUE)])) {
        return(TRUE)
      }
      else { return (FALSE) }
    }
  

  .log.likelihood <-
  function(object.name) {

  	model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("coeftest","maBina","gamma.net","logit.net","probit.net","cloglog.net")) {
      return(NA) 
    }
  	if (model.name %in% c("fGARCH")) {
  	  return(object.name@fit$value)
  	}
  	if (model.name %in% c("mlogit", "mnlogit")) {
  	  return(as.vector(object.name$logLik[1]))
  	}
  	if (model.name %in% c("arima", "betareg", "zeroinfl", "hurdle", "hetglm", "Arima")) {
  		return(as.vector(object.name$loglik))
  	}
  	if (model.name %in% c("selection","binaryChoice", "probit.ss")) {
  	  return(as.vector(.summary.object$loglik))
  	}  	
  	if (model.name %in% c("lme","nlme","lmer", "glmer", "nlmer","censReg")) { 
  	  return(as.vector(logLik(object.name)[1]))
  	}
  	if (model.name %in% c("lagsarlm", "errorsarlm")) {
  	  return(as.vector(.summary.object$LL))
  	}  	
  	if (model.name %in% c("clm", "gls")) {
  	  return(as.vector(object.name$logLik))
  	}
  	else if (model.name %in% c("coxph", "clogit", "exp", "weibull", "lognorm","tobit", "tobit(AER)", "survreg()")) {
  		return(as.vector(.summary.object$loglik[2]))
  	}
  	else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg")) {
  	  return(as.vector(object.name$loglik[2]))
  	}
  	else if (!is.null(object.name$aic)) {
  	  return(as.vector(-(0.5)*(object.name$aic-2*length(.summary.object$coefficients[,"Estimate"]))))
  	}
  	return(NA)
  }

  .logrank.stat <-
  function(object.name) {
    logrank.output <- as.vector(rep(NA,times=3))
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina", "coeftest", "Gls", "lmer", "glmer", "nlmer"))) {
      if (!is.null(.summary.object$logtest)) {
        logrank.value <- suppressMessages(.summary.object$sctest[1])
        df.value <- suppressMessages(.summary.object$sctest[2])
        logrank.p.value <- suppressMessages(.summary.object$sctest[3])
        logrank.output <- as.vector(c(logrank.value, df.value, logrank.p.value))
      }
    
    }
  
    names(logrank.output) <- c("statistic","df1","p-value")
    return(cbind(logrank.output))
  }

  .lr.stat <-
  function(object.name) {
    log.output <- as.vector(rep(NA,times=3))
  
    model.name <- .get.model.name(object.name)
    
    if (model.name %in% c("mlogit")) {
      log.value <- as.vector(.summary.object$lratio$statistic["chisq"]) 
      if (!is.null(log.value)) {
        df.value <- as.vector(length(object.name$coeff))
        log.p.value <- as.vector(pchisq(log.value,df.value,lower.tail=FALSE))
        log.output <- as.vector(c(log.value, df.value, log.p.value))
      }
    }
    else if (model.name %in% c("lagsarlm", "errorsarlm")) {
      log.value <- as.vector(.summary.object$LR1$statistic)
      df.value <- as.vector(.summary.object$LR1$parameter)
      log.p.value <- as.vector(.summary.object$LR1$p.value)
      log.output <- as.vector(c(log.value, df.value, log.p.value))
    }
    else if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","Gls","lmer","glmer","nlmer"))) {
      if (!is.null(.summary.object$logtest)) {
        log.value <- suppressMessages(.summary.object$logtest[1])
        df.value <- suppressMessages(.summary.object$logtest[2])
        log.p.value <- suppressMessages(.summary.object$logtest[3])
        log.output <- as.vector(c(log.value, df.value, log.p.value))
      }
      
    }   
  
    names(log.output) <- c("statistic","df1","p-value")
    return(cbind(log.output))
  }

  .max.r.squared <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","fGARCH","Arima","maBina", "coeftest", "lmer", "glmer", "nlmer", "Gls", "Arima"))) {
      if (model.name %in% c("coxph", "clogit")) {
        return(as.vector(.summary.object$rsq[2]))
      }
    }
    return(NA)
  }
  
  .model.identify <-
  function(object.name) {
    
    if (class(object.name)[1]=="NULL") {   #### !!!!! continue this
      return("NULL")
    }
    
    if (class(object.name)[1]=="Arima") {
      return("Arima")
    }
    
    if (class(object.name)[1]=="fGARCH") {
      return("fGARCH")
    }
    
    if (class(object.name)[1]=="censReg") {
      return("censReg")
    }
    
    if (class(object.name)[1]=="ergm") {
      return("ergm")
    }
    
    if (class(object.name)[1]=="mnlogit") {
      return("mnlogit")
    }
    
    if (class(object.name)[1]=="lme") {
      return("lme")
    }
    
    if (class(object.name)[1]=="nlme") {
      return("nlme")
    }
    
    if (class(object.name)[1]=="felm") {
      return("felm")
    }
    if (class(object.name)[1] %in% c("mclogit","mclogitRandeff")) {
      return("mclogit")
    }
    if (class(object.name)[1]=="mlogit") {
      return("mlogit")
    }
    if (class(object.name)[1]=="maBina") {
      return("maBina")
    }
    if (class(object.name)[1]=="coeftest") {
      return("coeftest")
    }
    if (class(object.name)[1]=="rem.dyad") {
      return("rem.dyad")
    }
    if (class(object.name)[1]=="lmerMod") {
      return("lmer")
    }
    if (class(object.name)[1]=="glmerMod") {
      return("glmer")
    }
    if (class(object.name)[1]=="nlmerMod") {
      return("nlmer")
    }
       
   if (!is.null(object.name$call)) {
    
  	if (object.name$call[1]=="lm()") { return("ls") }
  	else if ((object.name$call[1]=="glm()") || (object.name$call[1]=="Glm()")) {
  		if (object.name$family$family=="gaussian") {
  			if (object.name$family$link=="identity") {
  				return("normal")
  			}
  		}
  		else if (object.name$family$family=="binomial") {
  			if (object.name$family$link=="probit") {
  				return("probit")
  			}
  			if (object.name$family$link=="logit") {
  				return("logit")
	  		}

	  	}
	  	else if (object.name$family$family=="poisson") {
  			if (object.name$family$link=="log") {
  				return("poisson")
  			}
  		}
  		else if (object.name$family$family=="Gamma") {
  			if (object.name$family$link=="inverse") {
  				return("gamma")
  			}
  		}
  		return(paste("glm()#",object.name$family$family,"#",object.name$family$link, sep=""))
  	}

  	else if (object.name$call[1]=="svyglm()") {
  		if (object.name$family$family=="gaussian") {
  			if (object.name$family$link=="identity") {
  				return("normal.survey")
  			}
  		}
  		else if ((object.name$family$family=="binomial") || (object.name$family$family=="quasibinomial")) {
  			if (object.name$family$link=="probit") {
  				return("probit.survey")
  			}
  			if (object.name$family$link=="logit") {
  				return("logit.survey")
  			}

  		}
  		else if (object.name$family$family=="poisson") {
  			if (object.name$family$link=="log") {
  				return("poisson.survey")
  			}
  		}
  		else if (object.name$family$family=="Gamma") {
  			if (object.name$family$link=="inverse") {
  				return("gamma.survey")
  			}
  		}
  		return(paste("svyglm()#",object.name$family$family,"#",object.name$family$link, sep=""))
  	}

  	else if (object.name$call[1]=="gam()") {
  		if (object.name$family$family=="gaussian") {
  			if (object.name$family$link=="identity") {
  				return("normal.gam")
  			}
  		}
  		else if (object.name$family$family=="binomial")  {
  			if (object.name$family$link=="probit") {
  				return("probit.gam")
  			}
  			if (object.name$family$link=="logit") {
  				return("logit.gam")
  			}

  		}
  		else if (object.name$family$family=="poisson") {
  			if (object.name$family$link=="log") {
  				return("poisson.gam")
  			}
  		}
  		else if (object.name$family$family=="Gamma") {
  			if (object.name$family$link=="inverse") {
  				return("gamma.gam")
  			}
  		}
  		return(paste("gam()#",object.name$family$family,"#",object.name$family$link, sep=""))
  	}
	
  	else if (object.name$call[1]=="polr()") {
  		if (object.name$method=="logistic") {
  			return("ologit")
  		}
  		else if (object.name$method=="probit") {
  			return("oprobit")
  		}
  		return(paste("polr()#",object.name$method, sep=""))
  	}


  	else if (object.name$call[1]=="gee()") {
  		if (object.name$family$family=="gaussian") {
  			if (object.name$family$link=="identity") {
  				return("normal.gee")
  			}
  		}
  		else if (object.name$family$family=="binomial") {
  			if (object.name$family$link=="probit") {
  				return("probit.gee")
  			}
  			if (object.name$family$link=="logit") {
  				return("logit.gee")
  			}

  		}
  		else if (object.name$family$family=="poisson") {
  			if (object.name$family$link=="log") {
  				return("poisson.gee")
  			}
  		}
  		else if (object.name$family$family=="Gamma") {
  			if (object.name$family$link=="inverse") {
  				return("gamma.gee")
  			}
  		}
  		return(paste("gee()#",object.name$family$family,"#",object.name$family$link, sep=""))
  	}

  	else if (object.name$call[1]=="survreg()") {
  		if (object.name$dist=="exponential") {
  			return("exp")
  		}
  		else if (object.name$dist=="weibull") {
  			return("weibull")
  		}
  		else if (object.name$dist=="lognorm") {
  			return("lognormal")
  		}
      else if (object.name$dist=="gaussian") {
        return("tobit")
      }
  		return(paste("survreg()#",object.name$dist, sep=""))
  	}

  	else if (object.name$call[1]=="glm.nb()") {
  		return("negbin")
  	}
  	else if (object.name$call[1]=="\"glm.nb\"()") {
  	  return("negbin")
  	}
  	
    if (!is.null(object.name$userCall)) {
  	  if (object.name$userCall[1]=="clogit()") {
  	    return("clogit")
  	  }
  	}
  	
    if (object.name$call[1]=="coxph()") {
  		return("coxph")
  	}
  	if (object.name$call[1]=="pmg()") {
  	  return("pmg")
  	}
  	if (object.name$call[1]=="selection()") {
  	  return("selection")
  	}
  	if (object.name$call[1]=="heckit()") {
  	  return("heckit")
  	}
  	if (object.name$call[1]=="probit()") {
  	  return("probit.ss")
  	}
  	if (object.name$call[1]=="binaryChoice()") {
  	  return("binaryChoice")
  	}
  	if (object.name$call[1]=="brglm()") {
  	  return("brglm")
  	}
  	if (object.name$call[1]=="gls()") {
  	  return("gls")
  	}
  	if (object.name$call[1]=="clm()") {
  	  return("clm")
  	}
  	if (object.name$call[1]=="lmrob()") {
  	  return("lmrob")
  	}
     if (object.name$call[1]=="glmrob()") {
       return("glmrob")
     }
  	if (object.name$call[1]=="dynlm()") {
  	  return("dynlm")
  	}
  	if (object.name$call[1]=="rq()") {
  	  return("rq")
  	}
  	if (object.name$call[1]=="gmm()") {
  	  return("gmm")
  	}
  	if (object.name$call[1]=="lagsarlm()") {
  	  return("lagsarlm")
  	}
  	if (object.name$call[1]=="errorsarlm()") {
  	  return("errorsarlm")
  	}
  	if (object.name$call[1]=="rlm()") {
  	  return("rlm")
  	}
  	if (object.name$call[1]=="aftreg()") {
  	  return("aftreg")
  	}
  	if (object.name$call[1]=="coxreg()") {
  	  return("coxreg")
  	}
  	if (object.name$call[1]=="phreg()") {
  	  return("phreg")
  	}
  	if (object.name$call[1]=="weibreg()") {
  	  return("weibreg")
  	}
  	if (object.name$call[1]=="bj()") {
  	  return("bj")
  	}
  	if (object.name$call[1]=="cph()") {
  	  return("cph")
  	}
  	if (object.name$call[1]=="Gls()") {
  	  return("Gls")
  	}
  	if (object.name$call[1]=="lrm()") {
  	  return("lrm")
  	}
  	if (object.name$call[1]=="ols()") {
  	  return("ols")
  	}
  	if (object.name$call[1]=="psm()") {
  	  return("psm")
  	}
  	if (object.name$call[1]=="Rq()") {
  	  return("Rq")
  	}
  	if (object.name$call[1]=="hetglm()") {
  	  return("hetglm")
  	}
    else if (object.name$call[1]=="relogit()") {
      return("relogit")
    }
  	else if (object.name$call[1]=="netbinom()") {
  	  if (object.name$call$LF=="probit") { return("probit.net") }      
      if (object.name$call$LF=="logit") { return("logit.net") }
  	  if (object.name$call$LF=="cloglog") { return("cloglog.net") }
  	}
  	else if (object.name$call[1]=="netgamma()") {
  	  return("gamma.net")
  	}

  	else if (object.name$call[1]=="zelig()") {
        if (object.name$call$model %in% c("ls","normal","logit","probit","relogit","poisson","poisson.survey",
                                           "negbinom","probit.survey","logit.survey","normal.gee","logit.gee","probit.gee",
                                           "poisson.gee","normal.gam","logit.gam","probit.gam","poisson.gam","exp",
                                           "coxph","weibull","lognorm","normal.survey","gamma","gamma.survey",
                                           "gamma.gee","cloglog.net","logit.net","probit.net","gamma.net","ologit",
                                           "oprobit","arima","tobit")) {
            return(object.name$call$model)
  		    }
          else { return("unsupported zelig") }
  	}
    
  	else if (object.name$call[1]=="tobit()") {
  	  return("tobit(AER)")
  	}
    
    else if (object.name$call[1]=="multinom()") {
      return("multinom")
    }
    
  	else if (object.name$call[1]=="betareg()") {
  	  return("betareg")
  	}
  	else if (object.name$call[1]=="zeroinfl()") {
  	  return("zeroinfl")
  	}
  	else if (object.name$call[1]=="hurdle()") {
  	  return("hurdle")
  	}  	
  	else if (object.name$call[1]=="plm()") {
  	  return("plm")
  	}
    else if (object.name$call[1]=="pgmm()") {
       return("pgmm")
    }  	
  	else if (object.name$call[1]=="ivreg()") {
  	  return("ivreg")
  	} 
   }
  	
   return("unknown")
    
  }

  .new.table <-
  function(object.name, user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=TRUE, auto.p=TRUE, user.ci.lb=NULL, user.ci.rb=NULL) {
    
    if (class(object.name)[1] == "Glm") {
      .summary.object <<- summary.glm(object.name)
    }
    else if (!(.model.identify(object.name) %in% c("aftreg", "coxreg","phreg","weibreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq"))) {
      .summary.object <<- summary(object.name)
    }
    else {
      .summary.object <<- object.name
    }
    
    if (.model.identify(object.name) == "rq") {
      .summary.object <<- suppressMessages(summary(object.name, se=.format.rq.se))
    }
    
    model.num.total <- 1   # model number for multinom, etc.
    if (.model.identify(object.name) == "multinom") {
      if (!is.null(nrow(.summary.object$coefficients))) {
        model.num.total <-  nrow(.summary.object$coefficients)
      }
    }
    
    # set to null
    
    .global.models <<- NULL
    
    .global.dependent.variables <<- NULL
    .global.dependent.variables.written <<- NULL
    
    .global.coefficient.variables <<- NULL
    .global.coef.vars.by.model <<- NULL
    .global.coefficients <<- NULL
    .global.std.errors <<- NULL
    .global.ci.lb <<- NULL
    .global.ci.rb <<- NULL
    
    .global.t.stats <<- NULL
    .global.p.values <<- NULL
    
    .global.N <<- NULL
    .global.LL <<- NULL
    .global.R2 <<- NULL
    .global.max.R2 <<- NULL
    .global.adj.R2 <<- NULL
    .global.AIC <<- NULL
    .global.BIC <<- NULL
    .global.scale <<- NULL
    .global.UBRE <<- NULL
    .global.sigma2 <<- NULL
    .global.theta <<- NULL
    .global.rho <<- NULL
    .global.mills <<- NULL
    
    .global.SER <<- NULL
    .global.F.stat <<- NULL
    .global.chi.stat <<- NULL
    .global.wald.stat <<- NULL
    .global.lr.stat <<- NULL
    .global.logrank.stat <<- NULL
    .global.null.deviance <<- NULL
    .global.residual.deviance <<- NULL
    
    for (model.num in 1:model.num.total) {
      
      .global.models <<- c(.global.models, suppressMessages(as.vector(.model.identify(object.name))))
    
  	  .global.dependent.variables <<- c(.global.dependent.variables, suppressMessages(.dependent.variable(object.name, model.num)))
  	  .global.dependent.variables.written <<- c(.global.dependent.variables.written, suppressMessages(.dependent.variable.written(object.name, model.num)))
      .global.coefficient.variables <<- suppressMessages(.coefficient.variables(object.name))
      
      .global.coef.vars.by.model <<-  suppressMessages(cbind(.global.coef.vars.by.model, .global.coefficient.variables))
      
      get.coef <- suppressMessages(.get.coefficients(object.name, user.coef, model.num=model.num))
      get.se <- suppressMessages(.get.standard.errors(object.name, user.se, model.num=model.num))
      
  	  .global.coefficients <<- cbind(.global.coefficients, get.coef)
  	  .global.std.errors <<- cbind(.global.std.errors, get.se)
      
      .global.ci.lb <<- suppressMessages(cbind(.global.ci.lb, .get.ci.lb(object.name, user.ci.lb, model.num=model.num)))
      .global.ci.rb <<- suppressMessages(cbind(.global.ci.rb, .get.ci.rb(object.name, user.ci.rb, model.num=model.num))) 
    
      feed.coef <- NA; feed.se <- NA
      if (!is.null(get.coef)) { feed.coef <- get.coef }
      if (!is.null(get.se)) { feed.se <- get.se }
      if (!is.null(user.coef)) { feed.coef <- user.coef }   # feed user-defined coefficients, if available
      if (!is.null(user.se)) { feed.se <- user.se }   # feed user-defined std errors, if available
    
  	  .global.t.stats <<- suppressMessages(cbind(.global.t.stats, .get.t.stats(object.name, user.t, auto.t, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
  	  .global.p.values <<- suppressMessages(cbind(.global.p.values, .get.p.values(object.name, user.p, auto.p, feed.coef, feed.se, user.coef, user.se, model.num=model.num)))
  	  
  	  
  	  .global.N <<- c(.global.N, suppressMessages(.number.observations(object.name)))
  	  .global.LL <<- c(.global.LL, suppressMessages(.log.likelihood(object.name)))
  	  .global.R2 <<- c(.global.R2, suppressMessages(.r.squared(object.name)))
  	  .global.max.R2 <<- c(.global.max.R2, suppressMessages(.max.r.squared(object.name)))
  	  .global.adj.R2 <<- c(.global.adj.R2, suppressMessages(.adj.r.squared(object.name)))
  	  .global.AIC <<- c(.global.AIC, suppressMessages(.AIC(object.name)))
      .global.BIC <<- c(.global.BIC, suppressMessages(.BIC(object.name)))
      .global.scale <<- c(.global.scale, suppressMessages(.get.scale(object.name)))
      .global.UBRE <<- c(.global.UBRE, suppressMessages(.gcv.UBRE(object.name)))
  	  .global.sigma2 <<- c(.global.sigma2, suppressMessages(.get.sigma2(object.name)))
      
      .global.rho <<- cbind(suppressMessages(.get.rho(object.name)))
      .global.mills <<- cbind(suppressMessages(.get.mills(object.name)))
      .global.theta <<- cbind(suppressMessages(.get.theta(object.name)))
  	  .global.SER <<- cbind(suppressMessages(.SER(object.name)))
  	  .global.F.stat <<- cbind(suppressMessages(.F.stat(object.name)))
      .global.chi.stat <<- cbind(suppressMessages(.chi.stat(object.name)))
  	  .global.wald.stat <<- cbind(suppressMessages(.wald.stat(object.name)))
  	  .global.lr.stat <<- cbind(suppressMessages(.lr.stat(object.name)))
  	  .global.logrank.stat <<- cbind(suppressMessages(.logrank.stat(object.name)))
  	  .global.null.deviance <<- cbind(suppressMessages(.null.deviance(object.name)))
  	  .global.residual.deviance <<- cbind(suppressMessages(.residual.deviance(object.name)))
    }

  }

  .null.deviance <-
  function(object.name) {
  	null.deviance.output <- as.vector(rep(NA,times=3))

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","fGARCH","Arima","coeftest","Gls","lmer","glmer","nlmer", "ergm"))) {
  	  if (model.name %in% c("rem.dyad", "mclogit")) {
  	    null.deviance.value <- object.name$null.deviance
  	    null.deviance.output <- as.vector(c(null.deviance.value, NA, NA))
  	  }
  	  else if (model.name %in% c("maBina")) {
        null.deviance.value <- object.name$w$null.deviance
        df.value <- object.name$w$df.null
        null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
  	  }
  		else if (!is.null(suppressMessages(.summary.object$null.deviance))) {
  			null.deviance.value <- suppressMessages(.summary.object$null.deviance)
  			df.value <- object.name$df.null

  			null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
  		}
  		else if (!is.null(object.name$null.deviance)) {
  		  null.deviance.value <- object.name$null.deviance
  		  df.value <- object.name$df.null
		  
  		  null.deviance.output <- as.vector(c(null.deviance.value, df.value, NA))
  		}
  	}

  	names(null.deviance.output) <- c("statistic","df1","p-value")
  	return(cbind(null.deviance.output))
  }

  .number.observations <-
  function(object.name) {
  
    model.name <- .get.model.name(object.name)
  
    if (model.name %in% c("ls", "normal", "logit", "probit", "relogit",
                          "poisson", "negbin", "normal.survey", "poisson.survey",
                          "probit.survey", "logit.survey", "gamma", "gamma.survey",
                          "z.arima", "brglm","glm()", "Glm()", "svyglm()")) {
      return(length(object.name$residuals))
    }
    else if (model.name %in% c("fGARCH")) {
      return(length(object.name@data))
    }
    else if (model.name %in% c("maBina")) {
      return(length(object.name$w$residuals))
    }
    else if (model.name %in% c("mlogit")) {
      return(sum(object.name$freq))
    }
    else if (model.name %in% c("felm")) {
      return(object.name$N)
    }
    else if (model.name %in% c("mclogit")) {
      return(object.name$N)
    }
    else if (model.name %in% c("selection", "heckit")) {
      return(.summary.object$param$nObs)
    }
    else if (model.name %in% c("binaryChoice", "probit.ss")) {
      return(object.name$param$nObs)
    }
    else if (model.name %in% c("lmer","glmer","nlmer")) {
      return(length(resid(object.name)))  
    }
    else if (model.name %in% c("gmm")) {
      return(object.name$n)
    }
    else if (model.name %in% c("plm", "pgmm", "pmg", "rlm", "lmrob", "glmrob", "dynlm", "rq", "lagsarlm", "errorsarlm", "rem.dyad")) {
      return(as.vector(length(object.name$residual)))
    }
    else if (model.name %in% c("mnlogit")) {
      return(as.vector(.summary.object$model.size$N))
    }
    else if (model.name %in% c("hurdle", "zeroinfl")) {
      return(as.vector(object.name$n))
    }
    else if (model.name %in% c("ivreg","clm","hetglm")) {
      return(as.vector(object.name$nobs))
    }
    if (model.name %in% c("normal.gee", "logit.gee", "poisson.gee",
                          "probit.gee", "gamma.gee", "gee()", "betareg")) {
      return(as.vector(.summary.object$nobs))
    }
    else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam",
                               "poisson.gam", "coxph", "clogit", "exp", "lognorm", "weibull", "survreg()",
                               "gam()")) {
      return(as.vector(.summary.object$n))
    }
    else if (model.name %in% c("ologit", "oprobit", "polr()")) {
      return(as.vector(.summary.object$nobs))
    }
    else if (model.name %in% c("gls")) {
      return(as.vector(object.name$dims$N))
    }
    else if (model.name %in% c("tobit(AER)")) {
      return(as.vector(.summary.object$n["Total"]))
    }
    else if (model.name %in% c("Arima","censReg","lme","nlme","weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return(as.vector(nobs(object.name)))
    }
    return(NA)
  }
  
  .rename.intercept <-
    function(x) {
      out <- x
      for (i in seq(1:length(x))) {
        if (x[i] %in% .global.intercept.strings) { 
          out[i] <- .format.intercept.name
        }
      }
      return(out)
    }
  
  .order.reg.table <- 
    function(order) {
      
      # first, find the position of the intercept and rename the variable to be the intercept string
      intercept.position <- NULL
      for (i in seq(1:length(.global.coefficient.variables))) {
        if (.global.coefficient.variables[i] %in% .global.intercept.strings) { 
          intercept.position <- i 
          
          .global.coefficient.variables[i] <<- .format.intercept.name   
          rownames(.global.coefficients)[i] <<- .format.intercept.name
          rownames(.global.std.errors)[i] <<- .format.intercept.name
          rownames(.global.ci.lb)[i] <<- .format.intercept.name
          rownames(.global.ci.rb)[i] <<- .format.intercept.name
          rownames(.global.t.stats)[i] <<- .format.intercept.name
          rownames(.global.p.values)[i] <<- .format.intercept.name
        }
      }
      
      # put intercept on bottom if necessary
      if (!is.null(intercept.position)) {
        # hold contents of last row in placeholder variables
        placehold.coefficient.variables <- .global.coefficient.variables[-intercept.position]
        intercept.coefficient.variables <- .global.coefficient.variables[intercept.position]
        
        if (.format.intercept.bottom) {
          .global.coefficient.variables <<- c(placehold.coefficient.variables, intercept.coefficient.variables)
        }
        
        if (.format.intercept.top) {
          .global.coefficient.variables <<- c(intercept.coefficient.variables, placehold.coefficient.variables)
        }
      } 
      
      
      # order according to user's wishes
      old.order <- 1:length(.global.coefficient.variables)
      new.order <- NULL; add.these <- NULL
      
      if (!is.null(order)) {
        # if order is regular expression...
        if (is.character(order)) {
          not.ordered.yet <- .global.coefficient.variables
          
          for (i in 1:length(order)) {
            add.these <- grep(order[i], not.ordered.yet, perl=.format.perl, fixed=FALSE)
            not.ordered.yet[add.these] <- NA
            if (length(add.these) != 0) {
              new.order <- c(new.order, add.these)
            }
          }
        }
        else if (is.numeric(order)) { # if order contains indices
          order <- unique(order)
          order <- order[order <= max(old.order)]
          new.order <- old.order[order]
        }
      }
      
      if (!is.null(new.order)) {
        remainder <- old.order[-new.order]
        new.order <- c(new.order, remainder)
      }
      else { new.order <- old.order }
      
      # set the right order
      .global.coefficient.variables[old.order] <<- .global.coefficient.variables[new.order]
    }
  
  .insert.col.front <- function(d, new.col) {
    # values
    d.new <- d
    d.new[,seq(2,ncol(d)+1)] <- d[,seq(1,ncol(d))]
    d.new[,1] <- new.col
    
    # column names
    if (!is.null(colnames(d))) { 
      colnames(d.new)[seq(2,ncol(d)+1)] <- colnames(d)[seq(1,ncol(d))] 
      colnames(d.new)[1] <- ""
    }
    
    return(d.new)
  }
  
  .order.data.frame <- 
    function(d, order, summary=FALSE) {
      
      if ((.format.rownames == TRUE) && (summary == FALSE)) {  # if we want to report rownames, add them to data frame
        if (!is.null(rownames(d))) { d <- .insert.col.front(d, rownames(d)) }
      }
      
      # order according to user's wishes
      old.order <- 1:length(colnames(d))
      new.order <- NULL; add.these <- NULL
      
      if (!is.null(order)) {
        # if order is regular expression...
        if (is.character(order)) {
          not.ordered.yet <- colnames(d)
          
          for (i in 1:length(order)) {
            add.these <- grep(order[i], d, perl=.format.perl, fixed=FALSE)
            not.ordered.yet[add.these] <- NA
            if (length(add.these) != 0) {
              new.order <- c(new.order, add.these)
            }
          }
        }
        else if (is.numeric(order)) { # if order contains indices
          order <- unique(order)
          order <- order[order <= max(old.order)]
          new.order <- old.order[order]
        }
      }
      
      if (!is.null(new.order)) {
        remainder <- old.order[-new.order]
        new.order <- c(new.order, remainder)
      }
      else { new.order <- old.order }
      
      return( d[new.order] )
    }
  

  .print.additional.lines <-
  function(part.number=NULL) {

  	# if no additional lines, then quit the function
  	if (is.null(.format.add.lines)) { return(NULL) }

    max.l <- length(.global.models)+1
    for (line in 1:length(.format.add.lines)) {
      ## add columns if too few, remove if too many
  	  if (max.l > length(.format.add.lines[[line]])) {
        .format.add.lines[[line]] <- c(.format.add.lines[[line]], rep(NA, times=max.l - length(.format.add.lines[[line]])))		
  	  }
  	  else if (max.l < length(.format.add.lines[[line]])) {
        .format.add.lines[[line]] <- .format.add.lines[[line]][1:max.l]
  	  }
      
      .format.add.lines[[line]] <- .format.add.lines[[line]]
        
      ## print each line
      for (i in 1:max.l) {
        if (!is.na(.format.add.lines[[line]][i])) { 
          if (i==1) {
            cat(.format.add.lines[[line]][i], sep="") 
          }
          else {
            cat(" & ",.format.add.lines[[line]][i], sep="") 
          }
        }
        else { 
          if (i==1) {
            cat("   ", sep="") 
          }
          else {
            cat(" & ", sep="") 
          }
        }
      }
      cat(" \\\\ \n")
    }
  	.table.part.published[part.number] <<- TRUE
  }

  .print.table.statistic <-
  function(.global.var.name, .format.var.name, decimal.digits=.format.round.digits, part.string="", part.number=NULL, type.se=FALSE) {
	
  	# default values
  	report.df <- FALSE
    report.p.value <- FALSE
    significance.stars <- FALSE
    report.se <- FALSE
    report.tstat <- FALSE
    intelligent.df <- .format.intelligent.df
    force.math <- FALSE

  	# reporting of df, p-value, significance stars, standard errors, t-stats
  	if (length(grep("(df)", part.string,fixed=TRUE))!=0) { report.df <- TRUE } 
  	if (length(grep("(se)", part.string,fixed=TRUE))!=0) { report.se <- TRUE }
  	if (length(grep("(t)", part.string,fixed=TRUE))!=0) { report.tstat <- TRUE }
  	if (length(grep("(p)", part.string,fixed=TRUE))!=0) { report.p.value <- TRUE } 
  	if (length(grep("*", part.string,fixed=TRUE))!=0) { significance.stars <- TRUE } 


  	# first for vectors (statistics without, say, degrees of freedom)
  	if (is.vector(.global.var.name) == TRUE) {
  		if (sum(!is.na(.global.var.name))!=0) {
  			cat (.format.var.name)
  			for (i in seq(1:length(.global.models))) {
  	 			if (!is.na(.global.var.name[i])) { 
             if (.format.dec.mark.align == TRUE) {
                cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name[i], decimal.digits),"}", sep="")
             }
             else {
               cat(" & ",.iround(.global.var.name[i], decimal.digits), sep="")
             }
          }
  	 			else { cat(" & ", sep="") }
  			}
  			cat(" \\\\ \n")
  			.table.part.published[part.number] <<- TRUE
  		}
  	}
  	else if ((is.matrix(.global.var.name) == TRUE) && (type.se == FALSE)) {     # for statistics that have degrees of freedom
  		if (sum(!is.na(as.vector(.global.var.name["statistic",])))!=0) {

	  		# intelligent df reporting (figure out whether only report it on left side, or also)
	  		report.df.left.column <- FALSE
			
	  		# whittle down unique values
	  		df.all.together <- NULL
	  		for (i in seq(1:length(.global.models))) {
	  			df.string <- ""
	  			for (j in seq(1:(nrow(.global.var.name)- 2))) {
	  				df.string <- paste(df.string,";",as.character(.global.var.name[paste("df",as.character(j),sep=""),i]),sep="")
	  			}
	  			df.all.together <- append(df.all.together, df.string)
	  		}
	  		# remove.na.r
	  		df.all.together.no.NA <- NULL
	  		for (i in seq(1:length(df.all.together))) {
	  			if (substr(df.all.together[i],1,3)!=";NA") { df.all.together.no.NA <- c(df.all.together.no.NA, df.all.together[i]) }
	  		}
	  		df.all.together.no.NA.unique <- sort(unique(df.all.together.no.NA))

	  		# put df on the left if only one unique df in the table, and not just one column w/ given df
	  		if (intelligent.df == TRUE) {
	  			if ((length(df.all.together.no.NA.unique)==1) && (length(df.all.together.no.NA)>=2)) { report.df.left.column <- TRUE }				
	  		}

  			# write down the line	
  			cat (.format.var.name)

  			# report df on left side w/ intelligent reporting
  			if (report.df.left.column == TRUE) {
  				if (report.df == TRUE) {

  					cat(" ",.format.df.left,sep="")
  					df.list <- unlist(strsplit(df.all.together.no.NA.unique[1],";"))

  					for (i in seq(from=2, to=length(df.list))) {
  						if (i>=3) { cat(.format.df.separator) }
  						cat(df.list[i],sep="")
  					}
  					cat(.format.df.right,sep="")
  				}
  			}
		
  			# now, go column by column
  			for (i in seq(1:length(.global.models))) {
  	 			if (!is.na(.global.var.name["statistic",i])) {
             
  	 			  if (.format.dec.mark.align==TRUE) {
  					  cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name["statistic",i], decimal.digits), sep="") 
              force.math <- TRUE
  	 			  }
            else {
              cat(" & ",.iround(.global.var.name["statistic",i], decimal.digits), sep="")
            }

  					# significance stars
  					if ((significance.stars == TRUE) && (!is.na(.global.var.name["p-value",i]))) { .enter.significance.stars(.global.var.name["p-value",i], force.math) }

										
  					# degrees of freedom - only report by statistics if not in the left column already
  					if (report.df.left.column == FALSE) {
  						if ((report.df == TRUE) && (!is.na(.global.var.name["df1",i]))) {
  							cat(" ",.format.df.left,sep="")
  							for (j in seq(1:(nrow(.global.var.name)- 2))) {
  								if (!is.na(.global.var.name[paste("df",as.character(j),sep=""),i])) {
  									if (j>=2) { cat(.format.df.separator) }
  									cat(.global.var.name[paste("df",as.character(j),sep=""),i],sep="")
  								}
  							}
  							cat(.format.df.right,sep="")
  						}
  					}

  					# p-values
  					if ((report.p.value == TRUE) && (!is.na(.global.var.name["p-value",i]))) {
  						cat(" ",.format.p.value.left,sep="")
  						if (!is.na(.global.var.name[paste("df",as.character(j),sep=""),i])) { 
  							cat(.iround(.global.var.name["p-value",i],.format.round.digits, round.up.positive=TRUE),sep="") 
  						}
  						cat(.format.p.value.right,sep="")
  					}
            
            if (.format.dec.mark.align==TRUE) {
              cat("}")  
            }
            else {
              cat("")
            }
            
  				}
  				else { cat(" & ", sep="") }
  			}
  			cat(" \\\\ \n")			
  			.table.part.published[part.number] <<- TRUE
  		}
  	}
  	else if ((is.matrix(.global.var.name) == TRUE) && (type.se == TRUE)) {       # for statistics that have a standard error
  	  if (sum(!is.na(as.vector(.global.var.name["statistic",])))!=0) {
	    
  	    # write down the line	
  	    cat (.format.var.name)
	    
  	    # now, go column by column
  	    for (i in seq(1:length(.global.models))) {
  	      if (!is.na(.global.var.name["statistic",i])) { 

            if (.format.dec.mark.align == TRUE) {
              cat(" & \\multicolumn{1}{c}{",.iround(.global.var.name["statistic",i], decimal.digits), sep="")  
            }
            else {
              cat(" & ",.iround(.global.var.name["statistic",i], decimal.digits), sep="")
            }
  	        
	        
  	        # significance stars
  	        if ((significance.stars == TRUE) && (!is.na(.global.var.name["p-value",i]))) { .enter.significance.stars(.global.var.name["p-value",i], force.math) }
	        
  	        # standard errors
  	        if ((report.se == TRUE) && (!is.na(.global.var.name["se",i]))) { cat(" ",.format.se.left,.iround(.global.var.name["se",i], decimal.digits),.format.se.right,sep="") }
          
  	        # t-statistics
  	        if ((report.tstat == TRUE) && (!is.na(.global.var.name["tstat",i]))) { cat(" ",.format.tstat.left, .iround(.global.var.name["tstat",i], decimal.digits),.format.tstat.right,sep="") }
          
  	        # p-values
  	        if ((report.p.value == TRUE) && (!is.na(.global.var.name["p-value",i]))) { cat(" ",.format.p.value.left,.iround(.global.var.name["p-value",i], decimal.digits),.format.p.value.right,sep="") }
            
            if (.format.dec.mark.align == TRUE) {
              cat("}")
            }
            else {
              cat("")
            }
  	      }
  	      else { cat(" & ", sep="") }
  	    }
  	    cat(" \\\\ \n")			
  	    .table.part.published[part.number] <<- TRUE
  	  }
  	}
  }

  .publish.table <-
  function() {

  	.table.info.comment()

  	# table header
	
  	.table.header()
  	.table.insert.space()

  	.table.part.published <<- as.vector(rep(NA, times=length(.format.table.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
  	.publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)

  	if (length(.format.table.parts)>=1) {
  		for (i in seq(1:length(.format.table.parts))) {
  			.publish.table.part(part=.format.table.parts[i], which.part.number=i)

  			if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
  			if ((.format.table.parts[i]=="-") || (.format.table.parts[i]=="-!") || (.format.table.parts[i]=="=") || (.format.table.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
  		}
  	}

  	cat("\\end{tabular} \n")
  	if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
  	else if (!is.null(.format.font.size)) {
  	  cat("\\endgroup \n",sep="")
  	}
	
  }

  .publish.table.part <-
  function(part, which.part.number) {

  	.table.part.published[which.part.number] <<- FALSE
    
  	# dependent variable label line
  	if (part=="dependent variable label") {
  		if (.format.dependent.variable.text.on == TRUE) { 
        cat(" & \\multicolumn{",length(.global.models),"}{c}{",.format.dependent.variable.text, "} \\\\ \n", sep="")
  		  if (.format.dependent.variable.text.underline == TRUE) { cat("\\cline{2-",length(.global.models)+1,"} \n", sep="") }
      }
  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# dependent variables
  	else if (part=="dependent variables") {
  		.table.insert.space()
  		cat(.format.dependent.variables.text)
  		how.many.columns <- 0
      label.counter <- 0
    
  		for (i in seq(1:length(.global.models))) {
        if (is.null(.format.dep.var.labels)) { .format.dep.var.labels <<- NA }
  			how.many.columns <- how.many.columns + 1

  			# write down if next column has different dependent variable, or if end of columns
  			different.dependent.variable <- FALSE
  			if (i == length(.global.models)) {different.dependent.variable <- TRUE}
  			else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1])))  {different.dependent.variable <- TRUE}
        
        if (.format.multicolumn==FALSE) { different.dependent.variable <- TRUE }

  			if (different.dependent.variable == TRUE) {
          label.counter <- label.counter + 1 
   		 		if (how.many.columns == 1) {
            if (.format.dec.mark.align==TRUE) {
  		 		    if (is.na(.format.dep.var.labels[label.counter])) {
  		 		      if (.format.dependent.variables.capitalize == TRUE) { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right,"}", sep="") }
  		 		      else { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right,"}", sep="") }
  		 		    }
  		 		    else { cat(" & \\multicolumn{1}{c}{",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right,"}", sep="") }
            }
            else {
              if (is.na(.format.dep.var.labels[label.counter])) {
  					    if (.format.dependent.variables.capitalize == TRUE) { cat(" & ",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right, sep="") }
  					    else { cat(" & ",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right, sep="") }
              }
              else { cat(" & ",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right, sep="") }
            }
  				}
  				else {
            if (is.na(.format.dep.var.labels[label.counter])) {
  					  if (.format.dependent.variables.capitalize == TRUE) {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,toupper(as.character(.global.dependent.variables.written[i])),.format.dependent.variables.right,"}", sep="")}
  					  else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,as.character(.global.dependent.variables.written[i]),.format.dependent.variables.right,"}", sep="")}
            }
            else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.dependent.variables.left,.format.dep.var.labels[label.counter],.format.dependent.variables.right,"}", sep="")}
  				}

	  			how.many.columns <- 0
	  		}
	  	}
	  	cat(" \\\\ \n")

  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# models
  	else if (part=="models")  {
     	   if ((.format.model.names.include==TRUE) && ((.format.models.skip.if.one == FALSE) || ((.format.models.skip.if.one == TRUE) && (length(unique(.global.models))>=2)))) {
		
  		.table.insert.space()
  		cat(.format.models.text)
 
  		# rename models based on .formatting preferences
  		renamed.global.models <- as.matrix(rbind(.global.models, rep("", times=length(.global.models))))
  		for (i in seq(1:length(.global.models))) {
  			for (j in seq(1:ncol(.format.model.names))) {
  				model.strsplit <- unlist(strsplit(.global.models[i], split="#"))
  				if (.global.models[i]==.format.model.names[1,j]) { 
  					renamed.global.models[1,i] <- .format.model.names[2,j] 
  					renamed.global.models[2,i] <- .format.model.names[3,j]
  				}
  				else if ((model.strsplit[1]=="glm()") || (model.strsplit[1]=="svyglm()") || (model.strsplit[1]=="gee()") || (model.strsplit[1]=="gam()")) {
  					if ( .format.model.function == TRUE ) { renamed.global.models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", .format.model.family, model.strsplit[2], sep="") }
  					else { renamed.global.models[1,i] <- paste(.format.model.family, model.strsplit[2], sep="")}

  					renamed.global.models[2,i] <- paste(.format.model.link, model.strsplit[3], sep="")
  				}
  				else if ((model.strsplit[1]=="survreg()") || (model.strsplit[1]=="polr()")) {
  					if ( .format.model.function == TRUE ) { renamed.global.models[1,i] <- paste(substr(model.strsplit[1],1,nchar(model.strsplit[1])-2),": ", .format.model.dist, model.strsplit[2], sep="") }
  					else { renamed.global.models[1,i] <- paste(.format.model.dist, model.strsplit[2], sep="")}
  					renamed.global.models[2,i] <- ""
  				}
  			}
  		}

  		if (sum(renamed.global.models[2,]==rep("", times=length(.global.models)))==length(.global.models)) { how.many.model.rows <- 1}
  		else { how.many.model.rows <- 2 }

  		for (row in seq(from=1, to=how.many.model.rows)) {
  			how.many.columns <- 0
  			for (i in seq(1:length(.global.models))) {
  				how.many.columns <- how.many.columns + 1

  				# write down if next column has different dependent variable, or if end of columns
  				different.model <- FALSE
  				if (i == length(.global.models)) {different.model <- TRUE}
  				else if ((as.character(.global.models[i])) != (as.character(.global.models[i+1]))) {different.model <- TRUE}
  				else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables
          
  				if (.format.multicolumn==FALSE) { different.model <- TRUE }

  				if (different.model == TRUE) {
  			 		if (how.many.columns == 1) {
              if (.format.dec.mark.align == TRUE) {  
  			 		    cat(" & \\multicolumn{1}{c}{",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right,"}", sep="")
              }
              else {
                cat(" & ",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right, sep="")
              }
  			 		}
  					else {cat(" & \\multicolumn{",how.many.columns,"}{c}{",.format.models.left,as.character(renamed.global.models[row,i]),.format.models.right,"}", sep="")}

  					how.many.columns <- 0
  				}
  			}
  			cat(" \\\\ \n")	
  		}
	

  		# underline models
  		if (.format.underline.models == TRUE) {
  			how.many.columns <- 0
  			for (i in seq(1:length(.global.models))) {
  				how.many.columns <- how.many.columns + 1

  				# underline if next column has different dependent variable, or if end of columns
  				different.model <- FALSE
  				if (i == length(.global.models)) {different.model <- TRUE}
  				else if ((as.character(.global.models[i])) != (as.character(.global.models[i+1])))  {different.model <- TRUE}
  				else if ((as.character(.global.dependent.variables[i])) != (as.character(.global.dependent.variables[i+1]))) {different.model <- TRUE}   # subsume models under dependent variables

  				if (different.model== TRUE) {
  					cat("\\cline{",(i-how.many.columns+1)+1,"-",i+1,"} ",sep="")
	
  					how.many.columns <- 0
  				}
  			}
  		cat("\n")
  		}
  		.table.part.published[which.part.number] <<- TRUE
            }
  	}
    
    # column labels
    else if (part=="columns") {
      if (!is.null(.format.column.labels)) {
        
        if (is.null(.format.column.separate)) { .format.column.separate <- 1 }
        
        # adjust column.separate to have the same number of columns as the table
        models.in.table <- length(.global.models)
        models.in.col <- 0   
        for (i in seq(1:length(.format.column.separate))) {       # count up how many models in column.separate
          models.in.col <- models.in.col + .format.column.separate[i]
        }
        
        excess <- models.in.table - models.in.col
        
        # if too few column labels, add ones to column.separate
        if (excess > 0) {
          last.index <- length(.format.column.separate)
          for (i in seq(1:excess)) {
            .format.column.separate[last.index + i] <- 1
          }
        }
        
        # if too many column labels, then cut down
        if (excess < 0) {
          
          col.total <- 0
          new.format.column.separate <- NULL
          
          for(i in seq(1:length(.format.column.separate))) {
            col.total <- col.total + .format.column.separate[i]
            if (col.total > models.in.table) {
              new.format.column.separate[i] <- .format.column.separate[i] - (col.total - models.in.table)
              if (new.format.column.separate[i] == 0) { new.format.column.separate <- new.format.column.separate[-i] }
              break
            }
            else {
              new.format.column.separate[i] <- .format.column.separate[i]
            }
          }
          
          .format.column.separate <- new.format.column.separate
          
        }
        
        # output column labels
        col.position <- 1
        for (i in seq(1:length(.format.column.separate))) {
          if (is.null(.format.column.labels[col.position])) { .format.column.labels[col.position] <- "" }
          if (is.na(.format.column.labels[col.position])) { .format.column.labels[col.position] <- "" }
          if (.format.column.separate[i]==1) {
            if (.format.dec.mark.align==TRUE) {
              cat(" & \\multicolumn{1}{c}{",.format.column.left,.format.column.labels[col.position],.format.column.right,"}", sep="") 
            }
            else {
              cat(" & ",.format.column.left,.format.column.labels[col.position],.format.column.right, sep="") 
            }
          }
          else {
                cat(" & \\multicolumn{",.format.column.separate[i],"}{c}{",.format.column.left,.format.column.labels[col.position],.format.column.right,"}", sep="") 
          }
          col.position <- col.position + 1
        }
        cat(" \\\\ \n")  
      }
    }

  	# numbers
  	else if (part=="numbers") {
      if ((.format.model.numbers == TRUE) && (length(.global.models)>1)) {
  		  .table.insert.space()
  		  cat(.format.numbers.text)
  		  for (i in seq(1:length(.global.models))) {
          if (.format.dec.mark.align==TRUE) {
  	 		    if (.format.numbers.roman == TRUE) { cat(" & \\multicolumn{1}{c}{",.format.numbers.left,.roman.numeral(i),.format.numbers.right,"}", sep="") }
  	 		    else { cat(" & \\multicolumn{1}{c}{",.format.numbers.left,i,.format.numbers.right,"}", sep="") }
          }
          else {
            if (.format.numbers.roman == TRUE) { cat(" & ",.format.numbers.left,.roman.numeral(i),.format.numbers.right, sep="") }
            else { cat(" & ",.format.numbers.left,i,.format.numbers.right, sep="") }
          }

		    }
		    cat("\\\\ \n")
  		  .table.part.published[which.part.number] <<- TRUE
      }
  	}
    
  	# numbers
  	else if (part=="objects") {
  	  if (.format.object.names == TRUE) {
  	    .table.insert.space()
  	    for (i in seq(1:length(.global.models))) {
  	      if (.format.dec.mark.align==TRUE) {
  	       cat(" & \\multicolumn{1}{c}{",.global.object.names[i],"}", sep="")
  	      }
  	      else {
  	        cat(" & ",.global.object.names[i], sep="")
  	      }
  	    }
  	    cat("\\\\ \n")
  	    .table.part.published[which.part.number] <<- TRUE
  	  }
  	}

  	## coefficients
  	else if (part=="coefficients") { 		
  		.which.variable.label <<- 0
  		if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
		
      # then, enter the coefficients
  
  		for (i in seq(1:length(.global.coefficient.variables))) { .table.enter.coefficients(i) }

  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# number of observations
  	else if (part=="N") { .print.table.statistic(.global.var.name=.global.N, .format.var.name=.format.N, decimal.digits=0, part.number=which.part.number) }

  	# fixed effects table
  	else if (part=="omit") {
      if ((!is.null(.format.omit.regexp)) && (!is.null(.format.omit.labels))) {
  		  .format.omit.table <<- matrix(.format.omit.no, nrow=length(.format.omit.regexp), ncol=length(.global.models)) 
  		  for (i in seq(1:length(.global.models))) {
  				for (j in seq(1:length(.format.omit.regexp))) {
  					for (k in seq(1:length(.global.coef.vars.by.model[,i]))) {
  					  relevant.coef.var <- .global.coef.vars.by.model[k,i]
  					  if (length(grep(.format.omit.regexp[j], relevant.coef.var, perl=.format.perl, fixed=FALSE))!=0) {
  						   .format.omit.table[j,i] <<- .format.omit.yes
  						}
  					}
  				}
  			}
        for (i in seq(1:length(.format.omit.regexp))) {
  			  cat (.format.omit.labels[i])
  			  for (j in seq(1:length(.global.models))) {
            if (.format.dec.mark.align == TRUE) {
  		 		    cat(" & \\multicolumn{1}{c}{",.format.omit.table[i,j],"}", sep="")
            }
            else {
              cat(" & ",.format.omit.table[i,j], sep="")
            }
    			}
  			  cat(" \\\\ \n")
  			}
  			.table.part.published[which.part.number] <<- TRUE
      }
  	}

  	# R-squared
  	else if (part=="R-squared") {	.print.table.statistic(.global.var.name=.global.R2, .format.var.name=.format.R2, part.number=which.part.number) }
  
  	# max R-squared
  	else if (part=="max R-squared") {	.print.table.statistic(.global.var.name=.global.max.R2, .format.var.name=.format.max.R2, part.number=which.part.number) }

  	# adjusted R-squared
  	else if (part=="adjusted R-squared") { .print.table.statistic(.global.var.name=.global.adj.R2, .format.var.name=.format.adj.R2, part.number=which.part.number) }

  	# log likelihood
  	else if (part=="log likelihood") { .print.table.statistic(.global.var.name=.global.LL, .format.var.name=.format.LL, part.number=which.part.number) }

  	# Akaike Information Criterion (AIC)
  	else if (part=="AIC") { .print.table.statistic(.global.var.name=.global.AIC, .format.var.name=.format.AIC, part.number=which.part.number) }
  
  	# Bayesian Information Criterion (BIC)
  	else if (part=="BIC") { .print.table.statistic(.global.var.name=.global.BIC, .format.var.name=.format.BIC, part.number=which.part.number) }
  	
  	# Scale Parameter
  	else if (part=="scale") { .print.table.statistic(.global.var.name=.global.scale, .format.var.name=.format.scale, part.number=which.part.number) }
  
  	# UBRE
  	else if (part=="UBRE") { .print.table.statistic(.global.var.name=.global.UBRE, .format.var.name=.format.UBRE, part.number=which.part.number) }
    
  	# sigma2
  	else if (part=="sigma2") { .print.table.statistic(.global.var.name=.global.sigma2, .format.var.name=.format.sigma2, part.number=which.part.number) }

  	## with degrees of freedom

	  # residual standard error (sigma); standard error of the regression
  	else if (substr(part,1,nchar("SER"))=="SER") { .print.table.statistic(.global.var.name=.global.SER, .format.var.name=.format.SER, part.string=part, part.number=which.part.number) }
	
  	# F-statistic
  	else if (substr(part,1,nchar("F statistic"))=="F statistic") { .print.table.statistic(.global.var.name=.global.F.stat, .format.var.name=.format.F.stat, part.string=part, part.number=which.part.number) }
  
  	# theta
  	else if (substr(part,1,nchar("theta"))=="theta") { .print.table.statistic(.global.var.name=.global.theta, .format.var.name=.format.theta, part.string=part, part.number=which.part.number, type.se=TRUE) }
    
  	# rho
  	else if (substr(part,1,nchar("rho"))=="rho") { .print.table.statistic(.global.var.name=.global.rho, .format.var.name=.format.rho, part.string=part, part.number=which.part.number, type.se=TRUE) }
  	
  	# Inverse Mills ratio
  	else if (substr(part,1,nchar("Mills"))=="Mills") { .print.table.statistic(.global.var.name=.global.mills, .format.var.name=.format.mills, part.string=part, part.number=which.part.number, type.se=TRUE) }
  	
  
    # Chi-squared
  	else if (substr(part,1,nchar("chi2"))=="chi2") { .print.table.statistic(.global.var.name=.global.chi.stat, .format.var.name=.format.chi.stat, part.string=part, part.number=which.part.number) }
  
  	# Wald Test
  	else if (substr(part,1,nchar("Wald"))=="Wald") { .print.table.statistic(.global.var.name=.global.wald.stat, .format.var.name=.format.wald.stat, part.string=part, part.number=which.part.number) }
  
  	# LR Test
  	else if (substr(part,1,nchar("LR"))=="LR") { .print.table.statistic(.global.var.name=.global.lr.stat, .format.var.name=.format.lr.stat, part.string=part, part.number=which.part.number) }
  
  	# Score (Logrank) Test
  	else if (substr(part,1,nchar("logrank"))=="logrank") { .print.table.statistic(.global.var.name=.global.logrank.stat, .format.var.name=.format.logrank.stat, part.string=part, part.number=which.part.number) }

  	# null deviance
  	else if (substr(part,1,nchar("null deviance"))=="null deviance") { .print.table.statistic(.global.var.name=.global.null.deviance, .format.var.name=.format.null.deviance, part.string=part, part.number=which.part.number) }

  	# residual deviance
  	else if (substr(part,1,nchar("residual deviance"))=="residual deviance") { .print.table.statistic(.global.var.name=.global.residual.deviance, .format.var.name=.format.residual.deviance, part.string=part, part.number=which.part.number) }

  	##

  	# single horizontal line, no matter what
  	else if (part=="-!") {
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n") 
  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# single horizontal line, optional
  	else if (part=="-") {
  		if (.publish.horizontal.line==TRUE) {
  			cat("\\hline ")
  			.table.insert.space()
  			cat(" \n") 
  			.table.part.published[which.part.number] <<- TRUE
  		}
  	}

  	# double horizontal line, no matter what
  	else if (part=="=!") {
  		cat("\\hline \n") 
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n")
  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# double horizontal line
  	else if (part=="=") {
  		if (.publish.horizontal.line==TRUE) {
  			cat("\\hline \n") 
  			cat("\\hline ")
  			.table.insert.space()
  			cat(" \n") 
  			.table.part.published[which.part.number] <<- TRUE
  		}
  	}

  	# notes
  	else if (part=="notes") {
  		if (.format.note != "") { cat(.format.note) }
  		for (i in seq(1:length(.format.note.content))) {
  		  
  		  .format.note.content[i] <- .format.note.content[i]
        
        # print individual notes
  			if (.format.note == "") { cat("\\multicolumn{",length(.global.models)+1,"}{",.format.note.alignment,"}{",.format.note.content[i],"} \\\\ \n", sep="") }
  			else { cat(" & \\multicolumn{",length(.global.models),"}{",.format.note.alignment,"}{",.format.note.content[i],"} \\\\ \n", sep="") }
  		}
  		.table.part.published[which.part.number] <<- TRUE
  	}	

  	# empty line
  	else if (part==" ") {
  		.table.empty.line();
  		.table.part.published[which.part.number] <<- TRUE
  	}

  	# additional lines
  	else if (part=="additional") { .print.additional.lines(part.number=which.part.number) }
  }

  .r.squared <-
  function(object.name) {

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest","nlmer", "glmer", "lmer","Gls","Arima"))) {
  	  if (model.name %in% c("heckit")) {
  	    return(.summary.object$rSquared$R2)
  	  }
  	  if (model.name %in% c("felm")) {
  	    return(.summary.object$r2)
  	  }
      if (model.name %in% c("mlogit")) {
        return(.summary.object$mfR2[1])
      }
      if (model.name %in% c("plm")) {
        return(as.vector(.summary.object$r.squared["rsq"]))
      }
      else if (model.name %in% c("betareg")) {
        return(as.vector(.summary.object$pseudo.r.squared))
      }
  		else if (!is.null(.summary.object$r.squared)) {
  			return(as.vector(.summary.object$r.squared)) 
  		}
  		else if (model.name %in% c("coxph", "clogit")) {
  			return(as.vector(.summary.object$rsq[1]))
  		}
      else if (model.name %in% c("pmg")) {
        return(as.vector(.summary.object$rsqr))
      }
      else if (model.name %in% c("cph","lrm","ols","psm")) {
        return(as.vector(object.name$stats["R2"]))
      }
  	}
  	return(NA)
  }
  
  .remove.special.chars <-
  function(s) {
    
    if (!is.character(s)) { s.out <- as.character(s) }
    else { s.out <- s }
    
    # this has to go first
    s.out <- gsub("\\","\\textbackslash ",s.out,fixed=TRUE)
    
    # basic special characters
    s.out <- gsub("_","\\_",s.out,fixed=TRUE)
    s.out <- gsub("#","\\#",s.out,fixed=TRUE)
    s.out <- gsub("~","\\textasciitilde",s.out,fixed=TRUE)
    s.out <- gsub("{","\\{",s.out,fixed=TRUE)
    s.out <- gsub("}","\\}",s.out,fixed=TRUE)    
    s.out <- gsub("%","\\%",s.out,fixed=TRUE)
    s.out <- gsub("$","\\$",s.out,fixed=TRUE)
        
    # pre-defined text-mode commands (add more?)
    s.out <- gsub("*","\\textasteriskcentered ",s.out,fixed=TRUE)
    s.out <- gsub("|","\\textbar ",s.out,fixed=TRUE)
    s.out <- gsub(">","\\textgreater ",s.out,fixed=TRUE)
    s.out <- gsub("<","\\textless ",s.out,fixed=TRUE)
    
    # more substitutions
    s.out <- gsub("^","$\\hat{\\mkern6mu}$",s.out,fixed=TRUE)
        
    return(s.out)
  }
  

  .residual.deviance <-
  function(object.name) {
  	residual.deviance.output <- as.vector(rep(NA,times=3))

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","fGARCH","Arima","coeftest", "Gls","multinom","lmer","glmer","nlmer"))) {
      if (model.name %in% c("rem.dyad")) {
        residual.deviance.value <- object.name$residual.deviance
        residual.deviance.output <- as.vector(c(residual.deviance.value, NA, NA))
      }
      else if (model.name %in% c("mclogit")) {
        residual.deviance.value <- object.name$deviance
        residual.deviance.output <- as.vector(c(residual.deviance.value, NA, NA))
      }
      else if (model.name %in% c("maBina")) {
        residual.deviance.value <- object.name$w$deviance
        df.value <- object.name$w$df.residual
        residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
      }
  		else if (!is.null(.summary.object$deviance)) {
  			residual.deviance.value <- suppressMessages(.summary.object$deviance)
  			df.value <- object.name$df.residual
  			residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
  		}
      else if (!is.null(object.name$deviance)) {
  		  residual.deviance.value <- object.name$deviance
  		  df.value <- object.name$df.residual
  		  residual.deviance.output <- as.vector(c(residual.deviance.value, df.value, NA))
  		}
  	}

  	names(residual.deviance.output) <- c("statistic","df1","p-value")
  	return(cbind(residual.deviance.output))
  }

  .roman.numeral <-
  function(regular.number) {

  	# unique representation only for integers between 1 and 3899
  	if ((regular.number < 1) || (regular.number > 3899)) {
  		return(NULL)
  	}
  	else {
  		roman.output <- ""
  		number.remaining <- regular.number

  		while (number.remaining > 999) {
  			roman.output <- paste(roman.output, "M", sep="")
  			number.remaining <- number.remaining - 1000
  		}

  		if (number.remaining > 899) {
  			roman.output <- paste(roman.output, "CM", sep="")
  			number.remaining <- number.remaining - 900
  		}

  		if (number.remaining > 499) {
  			roman.output <- paste(roman.output, "D", sep="")
  			number.remaining <- number.remaining - 500
  		}

  		if (number.remaining > 399) {
  			roman.output <- paste(roman.output, "CD", sep="")
  			number.remaining <- number.remaining - 400
  		}

  		if (number.remaining > 399) {
  			roman.output <- paste(roman.output, "D", sep="")
  			number.remaining <- number.remaining - 400
  		}

  		while (number.remaining > 99) {
  			roman.output <- paste(roman.output, "C", sep="")
  			number.remaining <- number.remaining - 100
  		}

  		if (number.remaining > 89) {
  			roman.output <- paste(roman.output, "XC", sep="")
  			number.remaining <- number.remaining - 90
  		}

  		if (number.remaining > 49) {
  			roman.output <- paste(roman.output, "L", sep="")
  			number.remaining <- number.remaining - 50
  		}

  		if (number.remaining > 39) {
  			roman.output <- paste(roman.output, "XL", sep="")
  			number.remaining <- number.remaining - 40
  		}

  		while (number.remaining > 9) {
  			roman.output <- paste(roman.output, "X", sep="")
  			number.remaining <- number.remaining - 10
  		}

  		if (number.remaining > 8) {
  			roman.output <- paste(roman.output, "IX", sep="")
  			number.remaining <- number.remaining - 9
  		}

  		if (number.remaining > 4) {
  			roman.output <- paste(roman.output, "V", sep="")
  			number.remaining <- number.remaining - 5
  		}

  		if (number.remaining > 3) {
  			roman.output <- paste(roman.output, "IV", sep="")
  			number.remaining <- number.remaining - 4
  		}

  		if (number.remaining > 3) {
  			roman.output <- paste(roman.output, "IV", sep="")
  			number.remaining <- number.remaining - 4
  		}

  		while (number.remaining > 0) {
  			roman.output <- paste(roman.output, "I", sep="")
  			number.remaining <- number.remaining - 1
  		}

  		return(roman.output)
  	}
  }

  .SER <-
  function(object.name) {
  	SER.output <- as.vector(rep(NA,times=3))

  	model.name <- .get.model.name(object.name)

  	if (!(model.name %in% c("arima","lme","nlme","fGARCH","Arima","maBina","coeftest","lmer","glmer","nlmer","gls","Gls"))) {
      if (model.name %in% c("felm")) {
        SER.output <- as.vector(c(.summary.object$rse, .summary.object$rdf, NA))
      }
  		else if (!is.null(suppressMessages(.summary.object$sigma))) {
  			sigma.value <-suppressMessages(.summary.object$sigma)
        if (model.name %in% c("rlm")) {
          df.residual.value <- .summary.object$df[2]
        } 
        else {
  			  df.residual.value <- object.name$df.residual
        }
  			SER.output <- as.vector(c(sigma.value, df.residual.value, NA))
  		}
  	}

  	names(SER.output) <- c("statistic","df1","p-value")
  	return(cbind(SER.output))
  }

  .stargazer.reg.table <-
  function(...) {

  	list.of.models <- as.list(list(...))
  	how.many.models <- length(list.of.models)
    
    # find how many models user wants to customize
    # max.user <- max(length(coef),length(se),length(t),length(p),length(ci.custom))
    length(coef) <<- length(se) <<- length(t) <<- length(p) <<- length(ci.custom) <<- how.many.models
  
  	if (how.many.models >= 1) {
  		suppressMessages(.new.table(list.of.models[[1]], user.coef=coef[[1]], user.se=se[[1]], user.t=t[[1]], user.p=p[[1]], auto.t=t.auto, auto.p=p.auto, user.ci.lb=ci.custom[[1]][,1], user.ci.rb=ci.custom[[1]][,2]))
  		if (how.many.models >= 2) {
  			for (i in seq(from = 2,to = how.many.models)) { 
          #if (i <= max.user) {
            suppressMessages(.add.model(list.of.models[[i]], user.coef=coef[[i]], user.se=se[[i]], user.t=t[[i]], user.p=p[[i]], auto.t=t.auto, auto.p=p.auto, user.ci.lb=ci.custom[[i]][,1], user.ci.rb=ci.custom[[i]][,2])) 
          #}
          #else {
          #  suppressMessages(.add.model(list.of.models[[i]], user.coef=NULL, user.se=NULL, user.t=NULL, user.p=NULL, auto.t=t.auto, auto.p=p.auto, user.ci.lb=NULL, user.ci.rb=NULL))
          #}
  			}
  		}
      .apply(auto.t=t.auto, auto.p=p.auto)
      .order.reg.table(order)
  		suppressMessages(.publish.table())
  	}
 } 
  
  .set.font.size <- 
    function() {
      if (!is.null(.format.font.size)) {
        cat("\\", .format.font.size," \n", sep="")
      }
    }
  
  .floating.header <-
    function() {
      if (.format.floating==TRUE) {
        cat("\\begin{", .format.floating.environment,"}[", .format.table.placement,"] \\centering \n",sep="")
        cat("  \\caption{", .format.title, "} \n",sep="")   
        cat("  \\label{", .format.label, "} \n",sep="")
        .set.font.size()
      }
      else if (!is.null(.format.font.size)) { # set font size using begingroup
        cat("\\begingroup \n", sep="")
        .set.font.size()
      }
    }
  
  .data.frame.table.header <-
    function(object) {
      .floating.header()
      
      .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}} ", sep="")
      for (i in seq(1:(length(names(object))))) {
        if (.format.dec.mark.align == FALSE) {
          .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
        }
        else {
          .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.s.round.digits,"} ", sep="")
        }
      }
      #
      
      cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
    }
  
  .stargazer.data.frame.table <-
  function(object) {  
    
    # flip objects
    if (.format.flip == TRUE) { 
      
      # keep row- and column names
      obj.rownames <- rownames(object)
      obj.colnames <- colnames(object)
      
      object <- as.data.frame(t(object))
      
      colnames(object) <- obj.rownames
      rownames(object) <- obj.colnames
    }
    
    if ((nrow(object) < 1) || (ncol(object) < 1)) {
      cat("% Error: Data frame must have at least one row and one column.\n")
    }
    else {
      object <- .order.data.frame(object, order)
      
      .table.info.comment()
      
      #create table header
      .data.frame.table.header(object)
      .table.insert.space()
      
      .table.part.published <<- as.vector(rep(NA, times=length(.format.s.stat.parts)))    # to keep track what has been published (to deal intelligently with horizontal lines)
      .publish.horizontal.line <<- TRUE   # should non-compulsory horizontal lines be published? (yes, if something else published since the previous line)
      
      if (length(.format.s.stat.parts)>=1) {
        for (i in seq(1:length(.format.s.stat.parts))) {
          .data.frame.table.part(object,.format.s.stat.parts[i], which.part.number = i)
          
          if (.table.part.published[i]==TRUE) { .publish.horizontal.line <<- TRUE }
          if ((.format.s.stat.parts[i]=="-") || (.format.s.stat.parts[i]=="-!") || (.format.s.stat.parts[i]=="=") || (.format.s.stat.parts[i]=="=!")) { .publish.horizontal.line <<- FALSE }
        }
      }
      
      cat("\\end{tabular} \n")
      if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
      else if (!is.null(.format.font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }
  
  .data.frame.table.part <-
  function(object, part, which.part.number) {
    
    .table.part.published[which.part.number] <<- FALSE
    
    if ((part=="stat names") && (.format.colnames==TRUE)) {
      
      x.which <- 0
      
      if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
      
      for (x in seq(1:length(names(object)))) {
        
        omitted <- FALSE
                
        if (!is.null(.format.omit.regexp)) {
          for (j in seq(1:length(.format.omit.regexp))) {
            if (length(grep(.format.omit.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(.format.keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.regexp))) {
            if (length(grep(.format.keep.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(.format.omit.index)) {
          for (j in seq(1:length(.format.omit.index))) {
            if (.format.omit.index[j] == x) { omitted <- TRUE }
          }
        }
        
        if (!is.null(.format.keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.index))) {
            if (.format.keep.index[j] == x) { omitted <- FALSE }
          }
        }
        
        if (omitted == FALSE) {
          
          x.which <- x.which + 1
          
          if (x >= 2) { cat(" & ", sep="")}
          
          # if underscore or ^ in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[x])
                
          if (is.na(.format.covariate.labels[x.which])) {
            if (.format.coefficient.variables.capitalize == TRUE) { name.printed <- toupper(name.printed) }
          }
          else { name.printed <- .format.covariate.labels[x.which] }
      
        
          if (.format.dec.mark.align==TRUE) {
            cat("\\multicolumn{1}{c}{",.format.s.coefficient.variables.left, name.printed,.format.s.coefficient.variables.right,"}", sep="")  
          }
          else {
            cat(.format.s.coefficient.variables.left, name.printed,.format.s.coefficient.variables.right, sep="")  
          }
        }
        
      }
      
      cat(" \\\\ \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    if (substr(part,1,10)=="statistics") {
      for (y in seq(1:nrow(object))) {
        for (x in seq(1:length(names(object)))) {
          
          omitted <- FALSE
          
          if (!is.null(.format.omit.regexp)) {
            for (j in seq(1:length(.format.omit.regexp))) {
              if (length(grep(.format.omit.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
            }
          }
          
          if (!is.null(.format.keep.regexp)) {
            omitted <- TRUE
            for (j in seq(1:length(.format.keep.regexp))) {
              if (length(grep(.format.keep.regexp[j], names(object)[x], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
            }
          }
          
          if (!is.null(.format.omit.index)) {
            for (j in seq(1:length(.format.omit.index))) {
              if (.format.omit.index[j] == x) { omitted <- TRUE }
            }
          }
          
          if (!is.null(.format.keep.index)) {
            omitted <- TRUE
            for (j in seq(1:length(.format.keep.index))) {
              if (.format.keep.index[j] == x) { omitted <- FALSE }
            }
          }
          
          
          if (omitted == FALSE) {     
            if (x >= 2) { cat(" & ", sep="") }
            
            .how.much.to.round <- .format.round.digits
            if (is.numeric(object[y,x])) {
              
              if (.is.all.integers(object[y,x])) { .how.much.to.round <- 0 }
              
              rounded.object <- .iround(object[y,x], .how.much.to.round)
              
              if (.format.dec.mark.align==TRUE) {
                cat(rounded.object, sep="")  
              }
              else {
                cat("$", rounded.object, "$",sep="")  
              }
            }
            else {
              adjusted.object <- .remove.special.chars(object[y, x])
              if (is.na(adjusted.object)) { adjusted.object <- "" }
                
              if (.format.dec.mark.align==TRUE) {
                cat("\\multicolumn{1}{c}{", adjusted.object, "}", sep="")  
              }
              else {
                cat(adjusted.object, sep="")  
              }
              
            }
            
          }
          
          
            
        }
        # add empty lines
        how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
        if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
        
        for (j in seq(1:how.many.empty.lines)) {
          cat(" \\\\ \n") 
        }
      }
      .table.part.published[which.part.number] <<- TRUE
    }
        
    
    # notes
    else if ((part=="notes") && (!is.null(.format.s.note.content))) {
      if (.format.s.note != "") cat(.format.s.note)
      for (i in seq(1:length(.format.s.note.content))) {
        .format.s.note.content[i] <- .format.s.note.content[i]
        if (.format.s.note == "") { cat("\\multicolumn{",length(names(object)),"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
        else { cat(" & \\multicolumn{",length(names(object)),"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
      }
      .table.part.published[which.part.number] <<- TRUE
    }	
    
    # empty line
    else if (part==" ") {
      .table.empty.line()
      .table.part.published[which.part.number] <<- TRUE
    }
    
    # horizontal line
    else if (part=="-!") {
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
    # double horizontal line
    else if (part=="=!") {
      cat("\\hline \n") 
      cat("\\hline ")
      .table.insert.space()
      cat(" \n")
      .table.part.published[which.part.number] <<- TRUE
    }
    
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
        .table.part.published[which.part.number] <<- TRUE
      }
    }
    
  }

  .stargazer.summ.stat.table <-
  function(object) {
  
    if (length(names(object)) < 1) {
      cat("% Error: Data frame columns do not have any names.\n")
    }
    else if ((nrow(object) < 1) || (ncol(object) < 1)) {
      cat("% Error: Data frame must have at least one row and one column.\n")
    }
    else {
      
      object <- .order.data.frame(object, order, summary=T)
      
      .table.info.comment()
      
  	  # create table header
  	  .summ.stat.table.header(object)
  	  .table.insert.space()

  	  for (i in seq(1:length(.format.s.stat.parts))) {
    		.summ.stat.table.part(object,.format.s.stat.parts[i])
    	}
      
      cat("\\end{tabular} \n")
      if (.format.floating == TRUE) { cat("\\end{", .format.floating.environment,"} \n", sep="") }
      else if (!is.null(.format.font.size)) {
        cat("\\endgroup \n",sep="")
      }
    }
  }

  .summ.stat.publish.statistic <-
  function(object, which.variable, which.statistic) {
    
  	if ((is.numeric(object[,which.variable]) == TRUE) || ((is.logical(object[,which.variable])) && (.format.summ.logical==TRUE)))  {
      
      if ((is.logical(object[,which.variable])) && (.format.summ.logical==TRUE)) {
        temp.var <- rep(NA, time=length(object[,which.variable]))
        temp.var[object[,which.variable]==TRUE] <- 1
        temp.var[object[,which.variable]==FALSE] <- 0
      }
      else {
        temp.var <- object[,which.variable]
      }
      
      which.statistic <- tolower(which.statistic)
  		if (which.statistic == "n") {
  			return(.iround(sum(!is.na(temp.var)), 0))
  		}
  		else if (which.statistic == "nmiss") {
  			return(.iround(sum(is.na(temp.var)), 0))
  		}
  		else if (which.statistic == "mean") {
  			return(.iround(mean(temp.var, na.rm=TRUE), .format.s.round.digits))
  		}
  		else if (which.statistic == "median") {
  			median.value <- median(temp.var, na.rm=TRUE)
     
  			if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
  			else { 
  				if (.is.all.integers(median.value) == TRUE) { how.much.to.round <- 0 }
  				else { how.much.to.round <- 1 }
	  		}

	  		return(.iround(median.value, how.much.to.round))
  		}
  		else if (which.statistic == "sd") {
  			return(.iround(sd(temp.var, na.rm=TRUE), .format.s.round.digits))
  		}
  		else if (which.statistic == "min") {
  			if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
  			else { how.much.to.round <- 0 }

  			return(.iround(min(temp.var, na.rm=TRUE), how.much.to.round))
  		}
  		else if (which.statistic == "max") {
  			if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
  			else { how.much.to.round <- 0 }

  			return(.iround(max(temp.var, na.rm=TRUE), how.much.to.round))
  		}
  		else if (which.statistic == "mad") {
  			return(.iround(mad(temp.var, na.rm=TRUE), .format.s.round.digits))
  		}
  		else if (substr(which.statistic,1,1) == "p") {
      
        percentile.value <- quantile(temp.var, as.numeric(substr(which.statistic,2,nchar(which.statistic))) / 100, na.rm=TRUE)
		  
        if (.is.all.integers(temp.var) == FALSE) { how.much.to.round <- .format.s.round.digits }
  		  else { 
  		    if (.is.all.integers(percentile.value) == TRUE) { how.much.to.round <- 0 }
  		    else { how.much.to.round <- 1 }
  		  }
		  
  		  return(.iround(percentile.value, how.much.to.round))
  		}
  	}
  	else { return(NA) }
  }

  .summ.stat.table.header <-
  function(object) {
    .floating.header()

    #
    .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}}l", sep="")
    
    if (.format.flip == FALSE) { width <- length(.format.s.statistics.list) }
    else { width <- length(.summ.stat.included(object)) }
    
    for (i in seq(1:width)) {
    	  if (.format.dec.mark.align == FALSE) {
          .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
    	  }
        else {
          .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.s.round.digits,"} ", sep="")
        }
      }
      #
  
    cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }
  
  # figure out which variables are included --> returns indices of included variables
  .summ.stat.included <- 
  function(object) {
    
    included <- NULL

    for (i in seq(1:length(names(object)))) {
      
      # skip all of this if omitted based on regular expression
      omitted <- FALSE
        
      if ((is.numeric(object[,i]) == TRUE) || (is.logical(object[,i]) && (.format.summ.logical==TRUE))) {
        
        # also omit if all missing values
        if (!any(!is.na(object[,i]))) { omitted <- TRUE }
        
        if (!is.null(.format.omit.regexp)) {
          for (j in seq(1:length(.format.omit.regexp))) {
            if (length(grep(.format.omit.regexp[j], names(object)[i], perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE  }
          }
        }
        
        if (!is.null(.format.keep.regexp)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.regexp))) {
            if (length(grep(.format.keep.regexp[j], names(object)[i], perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE  }
          }
        }
        
        if (!is.null(.format.omit.index)) {
          for (j in seq(1:length(.format.omit.index))) {
            if (.format.omit.index[j] == i) { omitted <- TRUE }
          }
        }
        
        if (!is.null(.format.keep.index)) {
          omitted <- TRUE
          for (j in seq(1:length(.format.keep.index))) {
            if (.format.keep.index[j] == i) { omitted <- FALSE }
          }
        }
      }
      else { omitted <- TRUE }
      
      if (omitted == FALSE) { included <- c(included, i) }
    }
    
    return(included)
        
  }
  
  .summ.stat.table.part <-
  function(object, part) {
    
    included <- .summ.stat.included(object)
    
    # with summary statistics, always publish horizontal line
    .publish.horizontal.line <<- TRUE

  	if (part=="stat names") {
  		cat(.format.s.statistics.names.label, sep="")
      
      if (.format.flip == FALSE) {
        if (length(.format.s.statistics.list)>=1) {
  		    for (i in seq(1:length(.format.s.statistics.list))) {
  			    for (j in seq(1:ncol(.format.s.statistics.names))) {
  				    if ((substr(.format.s.statistics.list[i],1,1)=="p") && (substr(.format.s.statistics.list[i],1,1)==.format.s.statistics.names[1,j])) {
  					    cat(" & \\multicolumn{1}{c}{", .format.s.statistics.names.left, sub("!", substr(.format.s.statistics.list[i],2,nchar(.format.s.statistics.list[i])), .format.s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), .format.s.statistics.names.right,"}", sep="")
  				    }
  				    else if (.format.s.statistics.list[i]==.format.s.statistics.names[1,j]) {
  				  	  cat(" & \\multicolumn{1}{c}{", .format.s.statistics.names.left, .format.s.statistics.names[2,j], .format.s.statistics.names.right, "}", sep="")
  			  	  }
  			    }
  		    }
        }
      }
      else {   # flipped summary statistic table
        
        if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
          
        i.label <- 0
        
        for (i in included) {
          
          i.label <- i.label + 1
              
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
              
          cat(" & ")
          if (is.na(.format.covariate.labels[i.label])) { 
            if ( .format.s.coefficient.variables.capitalize == TRUE) { cat(.format.s.coefficient.variables.left, toupper(name.printed), .format.s.coefficient.variables.right, sep="") }
            else { cat(.format.s.coefficient.variables.left, name.printed, .format.s.coefficient.variables.right, sep="") }
          }
          else { cat(.format.s.coefficient.variables.left, .format.covariate.labels[i.label], .format.s.coefficient.variables.right, sep="") }        
        }
            
      }
         
  		cat(" \\\\ \n")
  	}

  	if (substr(part,1,10)=="statistics") {
  	  if (is.null(.format.covariate.labels)) { .format.covariate.labels <<- NA }
      
  	  
  	  if (.format.flip == FALSE) {
	  
        i.label <- 0
  		  for (i in included) {
          i.label <- i.label + 1
            
          # if underscore in variable name, then insert an escape \ before it
          name.printed <- .remove.special.chars(names(object)[i])
            
          if (is.na(.format.covariate.labels[i.label])) { 
  			    if ( .format.s.coefficient.variables.capitalize == TRUE) { cat(.format.s.coefficient.variables.left, toupper(name.printed), .format.s.coefficient.variables.right, sep="") }
  				    else { cat(.format.s.coefficient.variables.left, name.printed, .format.s.coefficient.variables.right, sep="") }
            }
          else { cat(.format.s.coefficient.variables.left, .format.covariate.labels[i.label], .format.s.coefficient.variables.right, sep="") }

          if (length(.format.s.statistics.list)>=1) {
	  			  for (j in seq(1:length(.format.s.statistics.list))) {
                
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (.format.dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, i, .format.s.statistics.list[j]), sep="")
                }
              }	    
	  			  }
          }
            
	        # add empty lines
  			  how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
  			  if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
				
  			  for (j in seq(1:how.many.empty.lines)) {
  				  cat(" \\\\ \n")
  			  }
  		  }
  	  }
      else {   # flipped
        if (length(.format.s.statistics.list)>=1) {
          for (i in seq(1:length(.format.s.statistics.list))) {
            for (j in seq(1:ncol(.format.s.statistics.names))) {
              if ((substr(.format.s.statistics.list[i],1,1)=="p") && (substr(.format.s.statistics.list[i],1,1)==.format.s.statistics.names[1,j])) {
                cat(.format.s.statistics.names.left, sub("!", substr(.format.s.statistics.list[i],2,nchar(.format.s.statistics.list[i])), .format.s.statistics.names[2,j], ignore.case =FALSE, fixed=TRUE), .format.s.statistics.names.right, sep="")
              }
              else if (.format.s.statistics.list[i]==.format.s.statistics.names[1,j]) {
                cat(.format.s.statistics.names.left, .format.s.statistics.names[2,j], .format.s.statistics.names.right, sep="")
              }
            }
            for (j in included) {
              # if aligning decimal marks, need to use multicolumn for anything w/o decimal mark
              if (.format.dec.mark.align == FALSE) {   # not aligning
                cat(" & ", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]), sep="")  
              }
              else {     # aligning
                if (.is.all.integers(.summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]))) {
                  cat(" & \\multicolumn{1}{c}{", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]),"}", sep="")
                }
                else {
                  cat(" & ", .summ.stat.publish.statistic(object, j, .format.s.statistics.list[i]), sep="")
                }
              } 
            }
            # add empty lines
            how.many.empty.lines <- as.numeric(substr(part,11,nchar(part)))
            if (is.na(how.many.empty.lines)) { how.many.empty.lines <- 1 } 
            
            for (k in seq(1:how.many.empty.lines)) {
              cat(" \\\\ \n")
            }
          }
        }
		  }
  	}

  	# notes
  	else if ((part=="notes") && (!is.null(.format.s.note.content))) {
  		if (.format.s.note != "") cat(.format.s.note)
      
      if (.format.s.note=="") { offset <- 1 }
      else { offset <- 0 }
      
      if (.format.flip == FALSE) { width <- length(.format.s.statistics.list)+ offset }
      else { width <- length(included) + offset }
      
  		for (i in seq(1:length(.format.s.note.content))) {
  		  .format.s.note.content[i] <- .format.s.note.content[i]
  			if (.format.s.note == "") { cat("\\multicolumn{",width,"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
  			else { cat(" & \\multicolumn{",width,"}{",.format.s.note.alignment,"}{",.format.s.note.content[i],"} \\\\ \n", sep="") }
  		}
  	}	

	
  	# empty line
  	else if (part==" ") {
  		.table.empty.line()
  	}

  	# horizontal line
  	else if (part=="-!") {
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n")
  	}
    
    else if (part=="-") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
      }
    }

  	# double horizontal line
  	else if (part=="=!") {
  		cat("\\hline \n") 
  		cat("\\hline ")
  		.table.insert.space()
  		cat(" \n")
  	}
    
    else if (part=="=") {
      if (.publish.horizontal.line==TRUE) {
        cat("\\hline \n") 
        cat("\\hline ")
        .table.insert.space()
        cat(" \n")
      }
    }
  }

  .table.empty.line <-
  function() {
    if (.format.no.space == FALSE) {
  	  cat(" ")
  	  for (i in seq(1:length(.global.models))) {
  		  cat("& ")
  	  }
  	  cat("\\\\ \n")
    }
  }

  .table.enter.coefficients <-
  function(which.variable) {
    
    if (which.variable > length(.global.coefficients)) {
      return();
    }

  	local.coefficient.var.name <- .global.coefficient.variables[which.variable]

  	#skip all of this if omitted based on regular expression
  	omitted <- FALSE
    
  	if (!is.null(.format.omit.regexp)) {
  		for (i in seq(1:length(.format.omit.regexp))) {
  			if (length(grep(.format.omit.regexp[i], local.coefficient.var.name, perl=.format.perl, fixed=FALSE))!=0) { omitted <- TRUE	}
  		}
  	}
    
    if (!is.null(.format.keep.regexp)) {
      omitted <- TRUE
      for (i in seq(1:length(.format.keep.regexp))) {
        if (length(grep(.format.keep.regexp[i], local.coefficient.var.name, perl=.format.perl, fixed=FALSE))!=0) { omitted <- FALSE	}
      }
    }
    
    if (!is.null(.format.omit.index)) {
      for (i in seq(1:length(.format.omit.index))) {
        if (.format.omit.index[i] == which.variable) { omitted <- TRUE }
      }
    }
    
    if (!is.null(.format.keep.index)) {
      omitted <- TRUE
      for (i in seq(1:length(.format.keep.index))) {
        if (.format.keep.index[i] == which.variable) { omitted <- FALSE }
      }
    }

  	if (omitted == FALSE) {
    
      .which.variable.label <<- .which.variable.label + 1

  		# remove final -TRUE (added by Zelig) from dummy variables
  		if (substr(local.coefficient.var.name, nchar(local.coefficient.var.name)-3, nchar(local.coefficient.var.name)) == "TRUE") {

  			### only remove TRUE if added by Zelig, rather than pre-existing in the formula name
  			if (length(grep(local.coefficient.var.name, .global.formulas.rhs,fixed=TRUE))==0) {    
  				local.coefficient.var.name <- substr(local.coefficient.var.name, 1, nchar(local.coefficient.var.name)-4)
  			}
  		}
      
      # remove everything before and including he last dollar sign from variable name
      temp <- strsplit(local.coefficient.var.name,"$",fixed=TRUE)
      local.coefficient.var.name <- temp[[1]][length(temp[[1]])]
      
      # if underscore or ^ in variable name, then insert an escape \ before it
      local.coefficient.var.name <- .remove.special.chars(local.coefficient.var.name)
      
  		if (length(.format.coefficient.table.parts)>=1) {
  			for (i in seq(1:length(.format.coefficient.table.parts))) {
  				.coefficient.table.part(part=.format.coefficient.table.parts[i], which.variable, variable.name=local.coefficient.var.name)
  			}
  		}
  	}
  }

  .table.header <-
  function() {
      .floating.header()

      #
      .formatting.alignment <- paste("@{\\extracolsep{",.format.column.sep.width,"}}l", sep="")
      for (i in seq(1:length(.global.models))) {
  	    if (.format.dec.mark.align==FALSE) {
          .formatting.alignment <- paste(.formatting.alignment, "c", sep="")
  	    }
        else {
  	    .formatting.alignment <- paste(.formatting.alignment, "D{", .format.decimal.character,"}{", .format.decimal.character,"}{-", .format.round.digits,"} ", sep="")
        }
      }
      #

      cat("\\begin{tabular}{",.formatting.alignment,"} \n",sep="")
  }

  .table.info.comment <-
  function() {
    cat("\n")
    if (.format.header==TRUE) {
      cat("% Table created by ", .global.package.name, " v.", .global.package.version, " by ", .global.package.author.name, ", ", .global.package.author.affiliation, ". E-mail: ", .global.package.author.email, "\n", sep="")  
      cat("% Date and time:", format(Sys.time(), "%a, %b %d, %Y - %X"))
      cat("\n")
      
      required.latex.packages <- NULL
      if (.format.dec.mark.align==TRUE) { required.latex.packages <- c(required.latex.packages, "dcolumn") }
      if (.format.floating.environment=="sidewaystable") { required.latex.packages <- c(required.latex.packages, "rotating") }
      
      if (!is.null(required.latex.packages)) {
        cat("% Requires LaTeX packages: ")
        for (i in 1:length(required.latex.packages)){
          cat(required.latex.packages[i]," ", sep="")
        }
        cat("\n")
      }
    }
  }

  .table.insert.space <-
  function() {
  	cat("\\\\[",.format.space.size,"]",sep="")
  }

  .trim <-
  function (x) gsub("^\\s+|\\s+$", "", x)

  .wald.stat <-
  function(object.name) {
    wald.output <- as.vector(rep(NA,times=3))
  
    model.name <- .get.model.name(object.name)
  
    if (!(model.name %in% c("arima","fGARCH","Arima","maBina","coeftest", "Gls", "ivreg","lmer","glmer","nlmer"))) {
      if (!is.null(.summary.object$waldtest)) {
        wald.value <- suppressMessages(.summary.object$waldtest[1])
        df.value <- suppressMessages(.summary.object$waldtest[2])
        wald.p.value <- suppressMessages(.summary.object$waldtest[3])
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
      }
      else if (model.name %in% c("tobit(AER)")) {
        wald.value <- .summary.object$wald
        df.value <- .summary.object$df - .summary.object$idf
        wald.p.value <- pchisq(wald.value, df.value, lower.tail=FALSE)
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
        
      }
      else if (model.name %in% c("lagsarlm", "errorsarlm")) {
        wald.value <- as.vector(.summary.object$Wald1$statistic)
        df.value <- as.vector(.summary.object$Wald1$parameter)
        wald.p.value <- as.vector(.summary.object$Wald1$p.value)
        wald.output <- as.vector(c(wald.value, df.value, wald.p.value))
      }
    
    }
  
    names(wald.output) <- c("statistic","df1","p-value")
    return(cbind(wald.output))
  }

  .get.coefficients.1 <-
  function(object.name, user.given=NULL, model.num=1) {
    
    if (!is.null(user.given)) { 
      
      if (.model.identify(object.name) == "multinom") {
        if (!is.null(nrow(user.given))) { user.given <- as.vector(user.given[model.num,]) }
      }
        
      return(user.given) 
    }

  	model.name <- .get.model.name(object.name)
	
  	if (model.name %in% c("ls", "normal", "logit", "probit", "relogit", "poisson", "negbin", "normal.survey", "poisson.survey", "probit.survey", "logit.survey", "gamma", "gamma.survey",
     				    "cloglog.net", "gamma.net", "logit.net", "probit.net", "brglm", "glm()", "Glm()", "svyglm()", "plm", "pgmm", "ivreg", "lmrob", "glmrob", "dynlm", "gmm", "mclogit")) {
  		return(.summary.object$coefficients[,"Estimate"])
  	}
  	if (model.name %in% c("Arima")) {
  	  return(object.name$coef)
  	}
  	if (model.name %in% c("censReg")) {
  	  return(.summary.object$estimate[,1])
  	}
  	if (model.name %in% c("mnlogit")) {
  	  return(.summary.object$CoefTable[,1])
  	}
  	if (model.name %in% c("fGARCH")) {
  	  return(object.name@fit$matcoef[,1])
  	}
  	if (model.name %in% c("lme","nlme")) {
  	  return(.summary.object$tTable[,1])
  	}
    if (model.name %in% c("maBina")) {
      return(as.vector(object.name$out[,1]))
    }
    if (model.name %in% c("mlogit")) {
      return(as.vector(.summary.object$CoefTable[,1]))
    }
    if (model.name %in% c("coeftest")) {
      return(as.vector(object.name[,1]))
    }
    if (model.name %in% c("selection", "heckit")) {
      if (!.global.sel.equation) {
        indices <- .summary.object$param$index$betaO                  ### outcome equation
      }
      else {
        indices <- .summary.object$param$index$betaS                  ### selection equation
      }
      return(as.vector(.summary.object$estimate[indices,1]))
    }
    if (model.name %in% c("probit.ss", "binaryChoice")) {
      return(as.vector(.summary.object$estimate[,1]))
    }
    if (model.name %in% c("hetglm")) {
      return(as.vector(.summary.object$coefficients$mean[,1]))
    }
    if (model.name %in% c("lmer","glmer","nlmer")) {
      coefs <- .summary.object$coefficients[,1]
      return(coefs)
    }
    if (model.name %in% c("ergm")) {
      return(.summary.object$coefs[,1])
    }
    if (model.name %in% c("lagsarlm", "errorsarlm")) {
      return(.summary.object$Coef[,1])
    }
    if (model.name %in% c("rq","felm")) {
      return(.summary.object$coefficients[,1])
    }
  	if (model.name %in% c("clm")) {
  	  if (.format.ordered.intercepts == FALSE) {
  	    return(.summary.object$coefficients[(length(object.name$alpha)+1):(length(object.name$coefficients)),1])
  	  }
  	  else {
  	    return(.summary.object$coefficients[,1])
  	  }
  	}
  	else if (model.name %in% c("pmg")) {
  	  return(.summary.object$coefficients)
  	}
    else if (model.name %in% c("zeroinfl", "hurdle")) {
      if (.global.zero.component==FALSE) {
        return(.summary.object$coefficients$count[,"Estimate"])  
      }
      else {
        return(.summary.object$coefficients$zero[,"Estimate"])
      }
    }
  	else if (model.name %in% c("normal.gee", "logit.gee", "probit.gee", "poisson.gee", "gamma.gee", "gee()")) {
  		return(.summary.object$coefficients[,"Estimate"])
  	}
  	else if (model.name %in% c("normal.gam", "logit.gam", "probit.gam", "poisson.gam", "gam()")) {
  		return(.summary.object$p.coeff)
  	}
  	else if (model.name %in% c("coxph", "clogit")) {
  		return(.summary.object$coef[,"coef"])
  	}
  	else if (model.name %in% c("exp","lognorm","weibull","tobit","survreg()")) {
  		return(.summary.object$table[,"Value"])
  	}
    else if (model.name %in% c("rlm")) {
      return(suppressMessages(.summary.object$coefficients[,"Value"]))
    }
  	else if (model.name %in% c("ologit", "oprobit", "polr()")) {
  		coef.temp <- suppressMessages(.summary.object$coefficients[,"Value"])
  		if (.format.ordered.intercepts == FALSE) { return(coef.temp[seq(from=1, to=length(coef.temp)-(length(suppressMessages(.summary.object$lev))-1))]) }
  		else { return(coef.temp) }
  	}
  	else if (model.name %in% c("arima", "rem.dyad")) {
  		return( object.name$coef )
  	}
  	else if (model.name %in% c("tobit(AER)")){
  	  return(.summary.object$coefficients[,"Estimate"])
  	}
    else if (model.name %in% c("multinom")){
      if (is.null(nrow(.summary.object$coefficients))) {
        coef.temp <- .summary.object$coefficients
      }
      else {
        coef.temp <- .summary.object$coefficients[model.num,]
      }
      return(coef.temp)
    }
  	else if (model.name %in% c("betareg")){
  	  return(.summary.object$coefficients$mean[,"Estimate"])
  	}
    else if (model.name %in% c("gls")) {
      coef.temp <- object.name$coefficients
      return(coef.temp)
    }
    else if (model.name %in% c("weibreg", "coxreg", "phreg", "aftreg", "bj", "cph", "Gls", "lrm", "ols", "psm", "Rq")) {
      return( object.name$coefficients )
    }
  	else { return(NULL) }

  }
  
  .get.coefficients <-
  function(object.name, user.given=NULL, model.num=1) {
    out <- .get.coefficients.1(object.name, user.given, model.num)
                               
    coef.vars <- .coefficient.variables(object.name)
    
    if (is.null(names(out))) {  
    
      if (length(out) < length(coef.vars)) {
        out.temp <- rep(NA, times=length(coef.vars)-length(out))
        out <- c(out, out.temp)
      }
      else if (length(out) > length(coef.vars)) {
        out <- out[1:length(coef.vars)]
      }
      
      names(out) <- coef.vars   
    }
    else {
      out.temp <- rep(NA, times = length(coef.vars))
      names(out.temp) <- coef.vars
      for (i in 1:length(out)) {
        name <- names(out)[i]
        if (name %in% coef.vars) {
          out.temp[name] <- out[i]
        }
      }
      out <- out.temp
      
    }
    return(out)
  }
  
  .turn.into.list <-
  function(x) {
    if (is.vector(x) || is.matrix(x)) {
      if (!is.list(x)) { return(as.list(x)) }
    }
    return(x)
  }
  
  .is.list.numeric <- 
    function(x) {
      # tolerate NA or NULL
      if (is.null(x)) { return(TRUE) }
      if (!is.list(x)) { return(FALSE) }
      for (i in 1:length(x)) {
        elem <- x[[i]]
        if (!is.null(elem)) {
          if (length(elem) != length(elem[is.numeric(elem) || (is.na(elem))])) { return(FALSE) }
        }
      }
      return(TRUE)
    }
  
  .is.list.numeric.matrix <- 
    function(x) {
      # tolerate NA or NULL
      if (is.null(x)) { return(TRUE) }
      if (!is.list(x)) { return(FALSE) }
      for (i in 1:length(x)) {
        elem <- as.matrix(x[[i]])
        if (!is.null(elem)) {
          if (length(elem) != length(elem[is.numeric(elem) || (is.na(elem))])) { return(FALSE) }
        }
      }
      return(TRUE)
    }
  
  .get.file.extension <-
  function (path) {
      split <- strsplit(path, "\\.")[[1]]
      return( tolower(split[length(split)]) )
  }
  
  
############## TEXT AND html MODE ##############
  

  .split.line <-    # split line of a LaTeX table into constituent parts separated by &
  function(s) {
    # remove the "\\\\"
    s <- gsub("\\\\", "", s, fixed=TRUE)
    s <- paste("  ",s,"  ", sep="")
    
    return(.trim(strsplit(s, " &", fixed=TRUE)[[1]]))
  }
  
  .remove.extra.spaces <-
  function(s) {
    new.s <- ""
    space <- FALSE
    for (i in 1:nchar(s)) {
      s.i <- substr(s,i,i)
      if (s.i == " ") {
        if (space == FALSE) { 
          space <- TRUE 
          new.s <- paste(new.s, s.i, sep="")
        }
      }
      else {
        space <- FALSE
        new.s <- paste(new.s, s.i, sep="")
      }
    }
    return(new.s)
  }
  
  strpos <-
  function(x, s) {
    return( regexpr(x, s, fixed=TRUE)[1] )
  }
  
  is.alphanumeric <- 
  function(s) {
    alphanum <- FALSE
    
    numbers <- grepl("^[[:digit:]]+$", s) 
    letters <- grepl("^[[:alpha:]]+$", s) 
    both <- grepl("^[[:digit:][:alpha:]]+$", s)
    
    if ((numbers == TRUE) || (letters == TRUE) || (both == TRUE)) {
      alphanum <- TRUE
    }
    
    return(alphanum)
  }
  
  .replace.latex.symbols <-
  function (s) {
    latex.replace <- NULL
    latex.replace <- cbind(latex.replace, c("\\textbackslash","\\"), c("\\_","_"), c("\\#","#"), c("\\textasciitilde","~"), c("\\{","{"), c("\\}","}"), c("\\%","%"))
    latex.replace <- cbind(latex.replace, c("\\textasteriskcentered","*"), c("\\textbar","|"), c("\\textgreater",">"), c("\\textless","<"), c("$\\hat{\\mkern6mu}$","^"))
    
    # Greek letters
    latex.replace <- cbind(latex.replace, c("\\alpha","alpha"), c("\\beta","beta"), c("\\gamma","gamma"), c("\\delta","delta"), c("\\epsilon","epsilon"), c("\\varepsilon","epsilon"), c("\\zeta","zeta"))
    latex.replace <- cbind(latex.replace, c("\\eta","eta"), c("\\theta","theta"), c("\\vartheta","theta"), c("\\iota","iota"), c("\\kappa","kappa"), c("\\lambda","lambda"), c("\\mu","mu"))
    latex.replace <- cbind(latex.replace, c("\\nu","nu"), c("\\xi","xi"), c("\\pi","pi"), c("\\varpi","pi"), c("\\rho","rho"), c("\\varrho","rho"), c("\\sigma","sigma"))
    latex.replace <- cbind(latex.replace, c("\\varsigma","sigma"), c("\\tau","tau"), c("\\upsilon","upsilon"), c("\\phi","phi"), c("\\varphi","phi"), c("\\chi","chi"), c("\\psi","psi"))
    latex.replace <- cbind(latex.replace, c("\\omega","omega"), c("\\Gamma","gamma"), c("\\Delta","delta"), c("\\Theta","theta"), c("\\Lambda","lambda"), c("\\Xi","xi"), c("\\Pi","pi"))
    latex.replace <- cbind(latex.replace, c("\\Sigma","sigma"), c("\\Upsilon","upsilon"), c("\\Phi","phi"), c("\\Psi","psi"), c("\\Omega","omega"))
    
    s.out <- s
    for (item in 1:ncol(latex.replace)) {
      symbol <- latex.replace[1, item]
      replacement <- latex.replace[2, item]
      
      # quick check if any latex characters
      symbol.regexp <- gsub("\\","\\\\",symbol,fixed=TRUE)
      symbol.regexp <- gsub("{","\\{",symbol.regexp,fixed=TRUE)
      symbol.regexp <- gsub("}","\\}",symbol.regexp,fixed=TRUE)
      symbol.regexp <- gsub("$","\\$",symbol.regexp,fixed=TRUE)
      symbol.regexp <- paste(symbol.regexp, "[^[:alnum:]_]+", sep="")
      
      pos <- 1
      while (pos <= nchar(s.out)) {
        
        if (length(grep(symbol.regexp, s.out))==0) { break }
        
        s.pre <- substr(s.out, 1, pos-1)
        s.pos.char <- substr(s.out, pos, pos)
        s.post <- substr(s.out, pos + nchar(symbol), nchar(s.out))
        if (substr(s.out, pos, pos+nchar(symbol)-1) == symbol) {
          if (!is.alphanumeric(substr(s.post, 1, 1))) {
            s.out <- paste(s.pre, replacement, s.post, sep="")
            post <- pos + nchar(replacement) - 1
          }
        }
        pos <- pos + 1
      }
    }
    
    return(s.out)
  }
  
  .remove.control.sequences <-
    function (s, type="text") {
      
      s <- paste("  ",s, "  ", sep="")
      
      # replace latex symbols
      s <- .replace.latex.symbols(s)
      
      # remove dollar signs and underscores  [ what about text-related starts ]
      s <- gsub("\\$", "", s)
      
      # remove extra spaces
      s <- .remove.extra.spaces(s)
      
      # add: replace some sequences with corresponding letters
      
      # walk through the string
      i <- 1
      new.s <- ""
      control.sequence <- ""
      while (i <= nchar(s)) {
        s.i0 <- substr(s, i-1, i)
        s.i <- substr(s, i, i)
        s.i2 <- substr(s, i, i+1)
        
        if ((s.i %in% c("\\", "_", "^")) && (!(s.i2 %in% c("\\_","\\^"))) && (!(s.i0 %in% c("\\_","\\^"))) ) {
          remainder.s <- substr(s, i+1, nchar(s))     # if control character not followed by curly brace
          if ((strpos(" ", remainder.s) < strpos("{", remainder.s)) || (strpos("{", remainder.s)==-1))  {
            i <- i + strpos(" ", remainder.s) + 1
          }
          else {   # control character followed by curly brace
            control.sequence <- substr(s, i, i+strpos("{", remainder.s)-1)
            
            if (type=="html") {
              if (control.sequence == "\\textit") { new.s <- paste(new.s,"<em>",sep="") }
              if (control.sequence == "\\textbf") { new.s <- paste(new.s,"<strong>",sep="") }
              if (control.sequence == "_") { new.s <- paste(new.s,"<sub>",sep="") }
              if (control.sequence == "^") { new.s <- paste(new.s,"<sup>",sep="") }
            }
            if (type=="mmd") {
              if (control.sequence == "\\textit") { new.s <- paste(new.s,"*",sep="") }
              if (control.sequence == "\\textbf") { new.s <- paste(new.s,"**",sep="") }
              if (control.sequence == "~") { new.s <- paste(new.s,"~",sep="") }
              if (control.sequence == "^") { new.s <- paste(new.s,"^",sep="") }
            }
            
            s.sub <- substr(remainder.s, strpos("{", remainder.s), nchar(remainder.s))
            open.brackets <- 0
            bracket.start <- bracket.end <- strpos("{", s.sub)
            
            for (j in 1:nchar(s.sub)) {
              s.sub.j <- substr(s.sub, j, j)
              if (s.sub.j == "{") { 
                open.brackets <- open.brackets + 1
                if (open.brackets == 1) { bracket.start <- j + 1 }
              }
              if (s.sub.j == "}") { 
                open.brackets <- open.brackets - 1 
                if (open.brackets == 0) { bracket.end <- j - 1 }
              }
              if (!(s.sub.j %in% c("{","}"))) {
                if (open.brackets == 0) { break }
              }
            }
            if (bracket.end < bracket.start) { 
              examine.substring <- "" 
            } 
            else {
              examine.substring <- substr(s.sub, bracket.start, bracket.end)
            }
            new.s <- paste(new.s, .remove.control.sequences(examine.substring, type=type), sep="")
            
            if (type=="html") {
              if (control.sequence == "\\textit") { new.s <- paste(new.s,"</em>",sep="") }
              if (control.sequence == "\\textbf") { new.s <- paste(new.s,"</strong>",sep="") }
              if (control.sequence == "_") { new.s <- paste(new.s,"</sub>",sep="") }
              if (control.sequence == "^") { new.s <- paste(new.s,"</sup>",sep="") }
            }
            if (type=="mmd") {
              if (control.sequence == "\\textit") { new.s <- paste(new.s,"*",sep="") }
              if (control.sequence == "\\textbf") { new.s <- paste(new.s,"**",sep="") }
              if (control.sequence == "~") { new.s <- paste(new.s,"~",sep="") }
              if (control.sequence == "^") { new.s <- paste(new.s,"^",sep="") }
            }
            
            i <- i + strpos("{", remainder.s) + bracket.end + 1
            
          }
        }  
        else {  # not inside a control sequence
          new.s <- paste(new.s, s.i, sep="")
          i <- i + 1
        }
        
      }
      
      # replace underscores, etc.
      new.s <- gsub("\\_", "_", new.s, fixed=T)
      new.s <- gsub("\\^", "^", new.s, fixed=T)
      
      return(.trim(new.s))
      
    }
  
  .text.cline <-
  function (cline, max.length, line.char="-") {
      for (i in 1:length(cline)) {
        if ((cline[i]==0) && (sum(cline[i:length(cline)]) != 0)) {
          .repeat.char(" ", rep=max.length[i]+1, new.line=FALSE)
        }
        else if (cline[i]>=1) {
          underline.len <- 0
          for (j in i:(i+cline[i]-1)) {
            underline.len <- underline.len + max.length[j] + 1
          }
          underline.len <- underline.len - 1
          .repeat.char(line.char, rep=underline.len, new.line=FALSE)
          if ((sum(cline[i:length(cline)]) != cline[i])) { cat(" ") }
        }
      }
      cat("\n")
    }
  
  .html.cline <-
    function (cline) {
      cat("<tr>")
      for (i in 1:length(cline)) {
        if ((cline[i]==0) && (sum(cline[i:length(cline)]) != 0)) {
          cat("<td></td>")
        }
        else if (cline[i]>=1) {
          cat("<td colspan=\"",cline[i],"\" style=\"border-bottom: 1px solid black\"></td>",sep="")
        }
      }
      cat("</tr>\n")
    }
  
  .mmd.cline <-
    function (cline) {
      # no support for cline in MMD as far as I am aware
    }
  
  .text.horizontal.line <-
  function (line.char="-", max.length) {
    horizontal.length <- 0
    for (i in 1:length(max.length)) {
      horizontal.length <- horizontal.length + max.length[i] + 1
    }
    horizontal.length = horizontal.length - 1
    
    
    .repeat.char(line.char, rep=horizontal.length, new.line=TRUE)
  }
  
  .html.horizontal.line <-
    function (how.many.columns) {
      cat("<tr><td colspan=\"",how.many.columns,"\" style=\"border-bottom: 1px solid black\"></td></tr>",sep="")
    }
  
  .mmd.horizontal.line <-
    function (how.many.columns) {
      # no support for hline in MMD as far as I am aware
    }
  
  .text.output <-
  function(all.latex.code) {
    
    how.many.tables <- 0
    start.lines <- NULL
    for (i in 1:length(all.latex.code)) {
      if (all.latex.code[i] %in% c("")) { 
        how.many.tables <- how.many.tables + 1
        start.lines <- c(start.lines, i)
      }
    }
    
    
    for (table.number in 1:how.many.tables) {
      
      if (table.number < how.many.tables) {
        latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
      }
      else {
        latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
      }
    
      how.many.columns <- .get.number.of.columns(latex.code)
    
      r <- 0
    
      matrices <- .matrices(latex.code, how.many.columns)
      t <- matrices[[1]]
      c <- matrices[[2]]
      j <- matrices[[3]]
    
      max.l <- .text.column.width(t, c)
      w <- .width.matrix(c, max.l)
    
      cat("\n")

      
      for (row in 1:length(latex.code)) {
        line <- latex.code[row]
        if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
          r <- r + 1
          .text.output.line(t, r, w, c, j)
        }
        else if (strpos("\\caption{", line) != -1) {
          inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
          text.title <- .trim(.remove.control.sequences(inside.caption))
          if (text.title != "") { cat(.remove.control.sequences(inside.caption),"\n", sep="") }
        }
        else if (strpos("\\cline{", line) != -1) {
          s <- paste("  ", line, "  ", sep="")
          cline <- rep(0, times=how.many.columns)
          while (strpos("\\cline{", s) != -1) {
            from <- strpos("\\cline{", s) + 7
            to <- strpos("}", s) - 1
          
            underline.columns <- substr(s, from, to)
            split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
          
            col.underline.begin <- as.numeric(split.columns[1])
            col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
          
            cline[col.underline.begin] <- col.underline.number
          
            s <- substr(s, to+1, nchar(s))
            .text.cline(cline, max.l)
          }
        }
        else if (strpos("\\hline",line) != -1) {
          if (!(is.na(latex.code[row+1]))) {
            if (strpos("\\hline", latex.code[row+1]) != -1) {
              .text.horizontal.line("=", max.l)
            } 
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
              .text.horizontal.line("-", max.l) 
              }
            }
          }
          else {
            if (strpos("\\hline", latex.code[row-1]) == -1) {           
              .text.horizontal.line("-", max.l) 
            }
          }
        }
      }
    }
  }
  
  .html.output <-
    function(all.latex.code) {
      
      how.many.tables <- 0
      start.lines <- NULL
      for (i in 1:length(all.latex.code)) {
        if (all.latex.code[i] %in% c("")) { 
          how.many.tables <- how.many.tables + 1
          start.lines <- c(start.lines, i)
        }
      }
      
      
      for (table.number in 1:how.many.tables) {
        
        if (table.number < how.many.tables) {
          latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
        }
        else {
          latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
        }
        
        how.many.columns <- .get.number.of.columns(latex.code)
        
        r <- 0
        
        matrices <- .matrices(latex.code, how.many.columns, type="html")
        t <- matrices[[1]]
        c <- matrices[[2]]
        j <- matrices[[3]]
        
        max.l <- .text.column.width(t, c)
        w <- .width.matrix(c, max.l)
        
        cat("\n")
        cat("<table style=\"text-align:center\">")
        
        
        for (row in 1:length(latex.code)) {
          line <- latex.code[row]
          if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
            r <- r + 1
            .html.output.line(t, r, w, c, j)
          }
          else if (strpos("\\caption{", line) != -1) {
            inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
            text.title <- .trim(.remove.control.sequences(inside.caption, type="html"))
            if (text.title != "") { cat("<caption><strong>",.remove.control.sequences(inside.caption, type="html"),"</strong></caption>\n", sep="") }
          }
          else if (strpos("\\cline{", line) != -1) {
            s <- paste("  ", line, "  ", sep="")
            cline <- rep(0, times=how.many.columns)
            while (strpos("\\cline{", s) != -1) {
              from <- strpos("\\cline{", s) + 7
              to <- strpos("}", s) - 1
              
              underline.columns <- substr(s, from, to)
              split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
              
              col.underline.begin <- as.numeric(split.columns[1])
              col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
              
              cline[col.underline.begin] <- col.underline.number
              
              s <- substr(s, to+1, nchar(s))
              .html.cline(cline)
            }
          }
          else if (strpos("\\hline",line) != -1) {
            if (!(is.na(latex.code[row+1]))) {
              if (strpos("\\hline", latex.code[row+1]) != -1) {
                .html.horizontal.line(how.many.columns)
              } 
              else {
                if (strpos("\\hline", latex.code[row-1]) == -1) {           
                  .html.horizontal.line(how.many.columns) 
                }
              }
            }
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
                .html.horizontal.line(how.many.columns) 
              }
            }
          }
        }
        cat("</table>\n")
      }
    }
  
  .mmd.output <-
    function(all.latex.code) {
      
      how.many.tables <- 0
      start.lines <- NULL
      for (i in 1:length(all.latex.code)) {
        if (all.latex.code[i] %in% c("")) { 
          how.many.tables <- how.many.tables + 1
          start.lines <- c(start.lines, i)
        }
      }
      
      for (table.number in 1:how.many.tables) {
        
        if (table.number < how.many.tables) {
          latex.code <- all.latex.code[start.lines[table.number]:start.lines[table.number+1]]
        }
        else {
          latex.code <- all.latex.code[start.lines[table.number]:length(all.latex.code)]
        }
        
        how.many.columns <- .get.number.of.columns(latex.code)
        
        r <- 0
        
        matrices <- .matrices(latex.code, how.many.columns, type="mmd")
        t <- matrices[[1]]
        c <- matrices[[2]]
        j <- matrices[[3]]
        
        max.l <- .text.column.width(t, c)
        w <- .width.matrix(c, max.l)
        
        cat("\n")
        cat("<table style=\"text-align:center\">")
        
        
        for (row in 1:length(latex.code)) {
          line <- latex.code[row]
          if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") {
            r <- r + 1
            .mmd.output.line(t, r, w, c, j)
          }
          else if (strpos("\\caption{", line) != -1) {
            inside.caption <- substr(.trim(line), 10, nchar(.trim(line))-1)
            text.title <- .trim(.remove.control.sequences(inside.caption, type="mmd"))
            if (text.title != "") { cat("**",.remove.control.sequences(inside.caption, type="mmd"),"***\n", sep="") }
            ### ADD THE REQUISITE NUMBER OF |s
          }
          else if (strpos("\\cline{", line) != -1) {
            s <- paste("  ", line, "  ", sep="")
            cline <- rep(0, times=how.many.columns)
            while (strpos("\\cline{", s) != -1) {
              from <- strpos("\\cline{", s) + 7
              to <- strpos("}", s) - 1
              
              underline.columns <- substr(s, from, to)
              split.columns <- strsplit(underline.columns,"-", fixed=TRUE)[[1]]
              
              col.underline.begin <- as.numeric(split.columns[1])
              col.underline.number <- as.numeric(split.columns[2]) - col.underline.begin + 1
              
              cline[col.underline.begin] <- col.underline.number
              
              s <- substr(s, to+1, nchar(s))
              .mmd.cline(cline)
            }
          }
          else if (strpos("\\hline",line) != -1) {
            if (!(is.na(latex.code[row+1]))) {
              if (strpos("\\hline", latex.code[row+1]) != -1) {
                .mmd.horizontal.line(how.many.columns)
              } 
              else {
                if (strpos("\\hline", latex.code[row-1]) == -1) {           
                  .mmd.horizontal.line(how.many.columns) 
                }
              }
            }
            else {
              if (strpos("\\hline", latex.code[row-1]) == -1) {           
                .mmd.horizontal.line(how.many.columns) 
              }
            }
          }
        }
        cat("</table>\n")
      }
    }
  
  .text.output.line <-
  function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
    real.c <- 0   # "real" column position
    for (c in 1:ncol(text.matrix)) {
      real.c <- real.c + column.matrix[row,c]
      justify <- justification.matrix[row, c]
      
      if (!(is.na(text.matrix[row,c]))) { 
        .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify=justify)
        if (real.c < ncol(text.matrix)) { cat(" ",sep="")}
      }
    }
    cat("\n")
  }
  
  .html.output.line <-
    function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
      real.c <- 0   # "real" column position
      cat("<tr>")
      for (c in 1:ncol(text.matrix)) {
        cm <- column.matrix[row,c]
        real.c <- real.c + cm
        justify <- justification.matrix[row, c]
        
        if (!(is.na(text.matrix[row,c]))) {
          cat("<td")

            if (cm > 1) { cat(" colspan=\"",cm,"\"", sep="") }
          
            if (justify == "l") { cat(" style=\"text-align:left\"", sep="") }
            if (justify == "r") { cat(" style=\"text-align:right\"", sep="") }
          
          cat(">")

          .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify="n")
          
          cat("</td>")
        }
      }
      cat("</tr>\n")
    }
  
  .mmd.output.line <-
    function(text.matrix, row, width.matrix, column.matrix, justification.matrix) {
      real.c <- 0   # "real" column position
      for (c in 1:ncol(text.matrix)) {
        cm <- column.matrix[row,c]
        real.c <- real.c + cm
        justify <- justification.matrix[row, c]
        
        if (!(is.na(text.matrix[row,c]))) {
          
          .just.cat(text.matrix[row, c], width=width.matrix[row, c], justify=justify)
          
          for (i in 1:cm) { cat("|") }
        }
      }
      cat("\n")
    }
  
  .width.matrix <-
    function(column.matrix, max.length) {
      
      w.matrix <- matrix(NA, nrow = nrow(column.matrix), ncol = ncol(column.matrix))
      
      # enter single widths first
      for (r in 1:nrow(column.matrix)) {
        for (c in 1:ncol(column.matrix)) {
          w.matrix[r,c] <- max.length[c]
        }
      }
      
      
      # think about multicolumns
      for (r in 1:nrow(column.matrix)) {
        from.c <- 0   # from which column do I start hoovering up widths?
        for (c in 1:ncol(column.matrix)) {
          
          from.c <- from.c+1
          
          if (column.matrix[r,c] >= 2) {
            total.width <- 0
            for (i in from.c:(from.c+column.matrix[r,c]-1)) {
              
              total.width <- total.width + max.length[i] + 1 
              if (i > from.c) {
                  for (j in i:ncol(column.matrix)) {
                    if ((j+1) <= ncol(column.matrix)) {
                      w.matrix[r,j] <- w.matrix[r, j+1]
                      w.matrix[r,j+1] <- NA
                    }
                    else {
                      w.matrix[r,j] <- NA
                    }
                  }
              
              }
            }
            w.matrix[r,c] <- total.width - 1
            from.c <- from.c + column.matrix[r,c] - 1
          }
          
        }
      }
      
      return(w.matrix)
    }
  
  .text.column.width <-
  function(text.matrix, column.matrix) {
    
    max.length = rep(1, times=ncol(column.matrix))
    temp.text.matrix <- text.matrix
    
    # first, get the maximum width of single columns
    for (r in 1:nrow(text.matrix)) {
      for (c in 1:ncol(text.matrix)) {
        real.c <- 0   # 'real' column number, adjusted for multicolumn
        for (i in 1:c) {
          real.c <- real.c + column.matrix[r, i]
        }
        if (real.c <= ncol(text.matrix)) {
          if (column.matrix[r,c] == 1) { # only look at singles here
            if (nchar(text.matrix[r,c]) > max.length[real.c]) { max.length[real.c] <- nchar(text.matrix[r,c]) }
          }
        }
      }  
    }
    
    # think about multicolumns
    for (r in 1:nrow(text.matrix)) {
      for (c in 1:ncol(text.matrix)) {
        if (!is.na(column.matrix[r,c])) {
          if (column.matrix[r,c] >= 2) {   # only look at multicolumns
            total.width <- 0
            for (i in c:(c+column.matrix[r,c]-1)) {
              total.width <- total.width + max.length[i] 
            }
            while (total.width < nchar(text.matrix[r,c])) {  # if does not fit into single columns, widen the maxima
              relevant.maxima <- NULL
              for (i in c:(c+column.matrix[r,c]-1)) {
                relevant.maxima <- c(relevant.maxima, max.length[i])
                if (max.length[i] == min(relevant.maxima)) { 
                  total.width <- 0
                  for (j in c:(c+column.matrix[r,c]-1)) {
                    total.width <- total.width + max.length[j] 
                  }
                  if (total.width < nchar(text.matrix[r,c])) { max.length[i] <- max.length[i] + 1 }
                }
              }
            }
          }
        }
      }
    }
    
    return(max.length)
  }
  
  .text.table.rows <-
  function(latex.code) {
      
    # figure out how many columns
    rows <- 0
    for (i in 1:length(latex.code)) {
      line <- latex.code[i]
      if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") { 
        rows <- rows + 1
      }
    }      
    
    return(rows)
  }
  
  .get.number.of.columns <- 
  function(latex.code) {
    
    formatting.string <- ""
    
    for (i in 1:length(latex.code)) {
      line <- latex.code[i]
      if ((substr(line, 1, 7) == "\\begin{") && (regexpr("}}",line,fixed=TRUE)[[1]] != -1)) {
        formatting.string <- substr(line, regexpr("}}",line,fixed=TRUE)[[1]]+2, nchar(line)-1)
      }
    }
    
    columns <- 0
    for (i in 1:nchar(formatting.string)) {
      if (substring(formatting.string, i, i) %in% c("l", "c", "r", "D")) { columns <- columns + 1 }
    }
    return(columns)
  }
  
  .matrices <-
    function(latex.code, how.many.columns, type="text") {
      
      rows <- .text.table.rows(latex.code)
      t.matrix <- matrix(NA, nrow = rows, ncol = how.many.columns)
      c.matrix <- matrix(1, nrow = rows, ncol = how.many.columns)
      j.matrix <- matrix(NA, nrow = rows, ncol = how.many.columns)
      
      line.content.j <- rep("c", how.many.columns)
      
      # put strings into matrix
      row <- 0
      for (i in 1:length(latex.code)) {
        line <- latex.code[i]
        if (substr(line, nchar(line)-2, nchar(line)) == "\\\\ ") { 
          row <- row + 1
          line.content <- .split.line(.remove.control.sequences(line, type=type))
          length(line.content) <- how.many.columns
          t.matrix[row,] <- line.content
          
          line.content.j[1] <- "l"
          line.content.j[2:how.many.columns] <- "c" 
          
          line.split <- .split.line(line)
          
          # add in column widths
          line.column <- rep(1, how.many.columns)
          for (j in 1:length(line.split)) {
            no.of.columns <- 0
            if (regexpr("\\multicolumn{", line.split[j], fixed=TRUE) != -1) {
              # text
              multicolumn.no <- substr(line.split[j], regexpr("{", line.split[j], fixed=TRUE)+1, regexpr("}", line.split[j], fixed=TRUE)-1)
              no.of.columns <- as.numeric(multicolumn.no)
              
              # justification
              from <- regexpr("}{", line.split[j], fixed=TRUE)+2
              rest.of.expression <- substr(line.split[j], from, nchar(line.split[j]))
              to <- regexpr("}", rest.of.expression, fixed=TRUE) - 1
              justification <- substr(rest.of.expression, 1, to)
              line.content.j[j] <- justification
            }
            else {
              no.of.columns <- 1
            }
            line.column[j] <- no.of.columns
          }
          
          # column
          length(line.column) <- how.many.columns
          c.matrix[row,] <- line.column
          
          # justification
          length(line.content.j) <- how.many.columns
          j.matrix[row,] <- line.content.j
          
        }
      }
      return(list(t.matrix,c.matrix,j.matrix))
  }
  
  
  
  .repeat.char <-   
  function(ch, rep=1, new.line=FALSE) {
    if (rep >= 1) {
      out.str <- ""
      for (i in 1:rep) {
        out.str <- paste(out.str, ch, sep="")
      }
      if (new.line == TRUE) { out.str <- paste(out.str, "\n", sep="")}
      cat(out.str)
    }
  }
  
  .just.cat <-         # cat that justifies string appropriately over the next couple of paragraphs
  function(s, width, offset.char=" ", justify="c"){
    len <- nchar(s)
    if (width <= len) {
      cat(s)
    }
    else {
      if (justify == "c") {
        offset <- (width - len) %/% 2
        .repeat.char(offset.char, offset)
        cat(s)
        .repeat.char(offset.char, width - len - offset)
      }
      else if (justify == "l") {
        cat(s)
        .repeat.char(offset.char, width - len)
      }
      else if (justify == "r") {
        .repeat.char(offset.char, width - len)
        cat(s)
      }
      else if (justify == "n") { # no justification, just output
        cat(s)
      }
    }
  }
  
  
  
############## OUTPUT INTO FILE ##############
  
  ### !!!! - add packages
  .output.tex <-
  function (file.out, content, header) {
    
      header.tex <- "\\documentclass{article}\n"
      
      required.latex.packages <- NULL
      if (.format.dec.mark.align==TRUE) { required.latex.packages <- c(required.latex.packages, "dcolumn") }
      if (.format.floating.environment=="sidewaystable") { required.latex.packages <- c(required.latex.packages, "rotating") }
      
      if (!is.null(required.latex.packages)) {
        for (i in 1:length(required.latex.packages)) {
          header.tex <- paste(header.tex, "\\usepackage{", required.latex.packages[i], "}\n", sep="")
        }
      }
      
      if (header == TRUE) {

        cat( 
          header.tex,
          "\\begin{document}",
          paste(content, collapse="\n"),
          "\\end{document}\n", 
          sep="\n",
          file = file.out
        )
      } else {
        cat( 
          paste(content, collapse="\n"), 
          sep="\n",
          file = file.out
        )
      }
  }
  
  .output.html <-
  function (file.out, content, header) {
      
    if (header == TRUE) {
      cat( 
        "<!DOCTYPE html>",
        "<html>",
        "<body>",
        paste(content, collapse="\n"),
        "</body>",
        "</html>\n",
        sep="\n",
        file = file.out
      )
    } else {
      cat( 
        paste(content, collapse="\n"),
        sep="\n",
        file = file.out
      )
    }
    
  }
  
  .output.txt <-
  function (file.out, content, header) {
    cat( 
      paste(content, collapse="\n"), 
      sep="\n",
      file = file.out
    )    
  }
  
  # !!! -  work on this more in a later version
  .output.pdf <-
  function (file.out, content) {
      tex.temp.file <- tempfile("temp", fileext="tex")
      .output.tex(tex.temp.file, content)                     
      capture.output(system(paste( "pdflatex --interaction=nonstopmode", shQuote(tex.temp.file)),  show.output.on.console = FALSE ))
    }
  
  .output.file <-
  function (out, latex.code, text.out, html.out, type, out.header) {
    for (i in 1:length(out)) {
      if (.get.file.extension(out[i])=="tex") { .output.tex(out[i], latex.code, out.header) }
      # else if (.get.file.extension(out[i])=="pdf") { .output.pdf(out[i], latex.code) }
      else if (.get.file.extension(out[i])=="txt") { .output.txt(out[i], text.out, out.header) }
      else if ((.get.file.extension(out[i])=="html") || (.get.file.extension(out[i])=="htm")) { 
        .output.html(out[i], html.out, out.header) 
      }
      else { # if another extension, do latex or text based on 'type'
        if (type == "latex") { .output.tex(out[i], latex.code, out.header) }
        else if (type == "text") { .output.txt(out[i], text.out, out.header) }
        else if (type == "html") { .output.html(out[i], html.out, out.header) }
      }
    }
  }
  
  
###########################################
  .get.objects <- 
    function(list.of.objects) {
    
    objects <- list()
    for (i in 1:length(list.of.objects)) {
      current.object <- list.of.objects[[i]]
      if (class(current.object)[1] == "list") {
        objects <- append(objects, .get.objects(current.object))
      }
      else {
        objects <- append(objects, list(current.object))
      }
    }
    
    return(objects)
  }
  
  # exact object names from ... string
  .get.object.names <- function(s) {
    object.names <- NULL
    inside <- .inside.bracket(s)
    
    for (i in 1:length(inside)) {
      if (substr(inside[i],1,nchar("list("))=="list(") {
        object.names <- c(object.names, .get.object.names(inside[i]))
      }
      else {
        object.names <- c(object.names, inside[i])
      }
    }
    
    return(object.names)
  }


###########################################

    ## invisible output
    invisible.output <- NULL
    latex.code <- NULL
    text.out <- NULL
    
    ## error handling
    error.present <- "\n"
    
	
    # get object names --- !!! CHECK ORDER
    object.names.string <- deparse(substitute(list(...))) ### for further processing to extract object names
    .global.object.names.all <- .get.object.names(object.names.string)
  
	
    # get objects
    list.of.objects <- list(...)
    objects <- as.list(.get.objects(list.of.objects))
    how.many.objects <- length(objects)
	
  
    # should we include a summary statistics table when given a data frame
    .global.summary <- rep(TRUE, times=how.many.objects)
    
    ## check if argument input is ok
    .format.rownames <- TRUE
    .format.colnames <- TRUE
  
    # flip the table?
    .format.flip <- flip
  
    if (how.many.objects < 1) { error.present <- c(error.present, "% Error: At least one object is required.\n") }
    else {
      
      # identify objects
      for (i in seq(1:how.many.objects)) {
        
        if (is.data.frame(objects[[i]])) {
          obj.rownames <- rownames(objects[[i]])
          if (is.null(obj.rownames)) { .format.rownames <- FALSE }
        }
        else if ((is.matrix(objects[[i]])) && (class(objects[[i]])[1] != "coeftest")) { 
          
          .global.summary[i] <- FALSE   # content output default for matrices
          
          obj.rownames <- rownames(objects[[i]])
          obj.colnames <- colnames(objects[[i]])
          
          if (is.null(obj.rownames)) { 
            if (.format.flip == FALSE) { .format.rownames <- FALSE }
            else { .format.colnames <- FALSE }
            obj.rownames <- as.character(c(1:nrow(objects[[i]])))
          }
          if (is.null(obj.colnames)) { 
            if (.format.flip == FALSE) { .format.colnames <- FALSE }
            else { .format.rownames <- FALSE }
            obj.colnames <- as.character(c(1:ncol(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(objects[[i]])
          colnames(objects[[i]]) <- obj.colnames
        }
        else if (is.vector(objects[[i]])) {
          
          .global.summary[i] <- FALSE   # content output default for vectors
          
          obj.names <- names(objects[[i]])
          
          if (is.null(obj.names)) { 
            .format.colnames <- FALSE
            .format.rownames <- FALSE
            obj.names <- as.character(c(1:length(objects[[i]])))
          }
          
          objects[[i]] <- as.data.frame(t(objects[[i]]))
          names(objects[[i]]) <- obj.names
        
          if (.format.flip == TRUE) { .format.colnames <- FALSE } 
          else { .format.rownames <- FALSE }
        }
          
        if (!is.data.frame(objects[[i]])) {
        
          # if zelig$result relevant, identify this automatically
          if (class(objects[[i]])[1] %in% c("coeftest","lmerMod","glmerMod","nlmerMod","fGARCH")) {  # use this to eliminate lmer, glmer, nlmer
            if (.model.identify(objects[[i]])=="unknown") { error.present <- c(error.present, "% Error: Unrecognized object type.\n",i) }
          }
          else {
            if (!is.null(objects[[i]]$zelig.call)) {
              if (!is.null(objects[[i]]$formula)) { formula <- objects[[i]]$formula }
              objects[[i]] <- objects[[i]]$result          
              if (!is.null(formula)) { objects[[i]]$formula2 <- formula }
            }
        
            ###
            if (is.atomic(objects[[i]]) && (!is.null(objects[[i]]))) { error.present <- c(error.present, "% Error: Unrecognized object type.\n") }
            else if (.model.identify(objects[[i]])=="unknown") { error.present <- c(error.present, "% Error: Unrecognized object type.\n") }
            else if (.model.identify(objects[[i]])=="unsupported zelig") { error.present <- c(error.present, "% Error: Unsupported 'zelig' model.\n") }
          }  
        }
      }
    
    }
  
    if (!is.character(type)) { error.present <- c(error.present, "% Error: Argument 'type' must be of type 'character.'\n") }
    if (length(type) != 1) { error.present <- c(error.present, "% Error: Argument 'type' must be of length 1.'\n") }
    if (is.character(type)) {
      if (!(tolower(type) %in% c("latex", "text", "html"))) {
        error.present <- c(error.present, "% Error: 'style' must be either 'latex' (default), 'html' or 'text.'\n")
      }
    }
    
    if (!is.character(title)) { error.present <- c(error.present, "% Error: Argument 'title' must be of type 'character.'\n") }
    
    if (!is.character(style)) { error.present <- c(error.present, "% Error: Argument 'style' must be of type 'character.'\n") }
    if (length(style) != 1) { error.present <- c(error.present, "% Error: Argument 'style' must be of length 1.'\n") }
    if (is.character(style)) {
      if (!(tolower(style) %in% c("all","all2","default","commadefault","aer","ajps","ajs","asq","asr","apsr","demography","io","jpam","qje"))) {
        error.present <- c(error.present, "% Error: 'style' not recognized'\n")
      }
    }
    
    if ((!is.logical(summary)) && (!is.null(summary))) { error.present <- c(error.present, "% Error: Argument 'summary' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    
    if ((!is.character(out)) && (!is.null(out))) { error.present <- c(error.present, "% Error: Argument 'out' must be NULL (default), or a vector of type 'character.' \n") }
    if (!is.logical(out.header)) { error.present <- c(error.present, "% Error: Argument 'out.header' be of type 'logical' (TRUE/FALSE) \n") }
  
    if ((!is.numeric(column.separate)) && (!is.null(column.separate))) { error.present <- c(error.present, "% Error: Argument 'column.separate' must be NULL (default), a vector of type 'numeric.'\n") }
  
    if ((!is.character(column.labels)) && (!is.null(column.labels))) { error.present <- c(error.present, "% Error: Argument 'column.labels' must be NULL (default), or a vector of type 'character.'\n") }
    if ((!is.character(covariate.labels)) && (!is.null(covariate.labels))) { error.present <- c(error.present, "% Error: Argument 'covariate.labels' must be NULL (default), or a vector of type 'character.'\n") }
    if ((!is.character(dep.var.labels)) && (!is.null(dep.var.labels))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels' must be NULL (default), or a vector of type 'character.'\n") }
    
    if ((!is.logical(dep.var.labels.include)) && (!is.null(dep.var.labels.include))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels.include' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(dep.var.labels.include) != 1) && (!is.null(dep.var.labels.include))) { error.present <- c(error.present, "% Error: Argument 'dep.var.labels.include' must be of length 1.'\n") }
    
    if ((!is.character(dep.var.caption)) && (!is.null(dep.var.caption))) { error.present <- c(error.present, "% Error: Argument 'dep.var.caption must be NULL (default), or of type 'character.'\n") }
    if ((length(dep.var.caption) != 1) && (!is.null(dep.var.caption))) { error.present <- c(error.present, "% Error: Argument 'dep.var.caption' must be of length 1.'\n") }  
  
    coef <- .turn.into.list(coef); se <- .turn.into.list(se)
    t <- .turn.into.list(t); p <- .turn.into.list(p)
    
    if ((!.is.list.numeric(coef)))  { error.present <- c(error.present, "% Error: Argument 'coef' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(se)))  { error.present <- c(error.present, "% Error: Argument 'se' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(t)))  { error.present <- c(error.present, "% Error: Argument 't' must be NULL (default), or a list of numeric vectors.\n") }
    if ((!.is.list.numeric(p)))  { error.present <- c(error.present, "% Error: Argument 'p' must be NULL (default), or a list of numeric vectors.\n") }
  
    if (!is.logical(t.auto)) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(t.auto) != 1) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of length 1.'\n") }

    if (!is.logical(p.auto)) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(p.auto) != 1) { error.present <- c(error.present, "% Error: Argument 't.auto' must be of length 1.'\n") }

    if (!is.logical(align)) { error.present <- c(error.present, "% Error: Argument 'align' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(align) != 1) { error.present <- c(error.present, "% Error: Argument 'align' must be of length 1.'\n") }

    if (!is.logical(ci)) { error.present <- c(error.present, "% Error: Argument 'ci' must be of type 'logical' (TRUE/FALSE) \n") }
    
    ci.custom <- .turn.into.list(ci.custom)
    if ((!.is.list.numeric.matrix(ci.custom)))  { error.present <- c(error.present, "% Error: Argument 'ci.custom' must be NULL (default), or a list of numeric matrices. \n") }
    else if (!is.null(ci.custom)) {
      l <- length(ci.custom)
      
      bad.dimension <- FALSE
      for (i in 1:l) {
        if (!is.null(ci.custom[[i]])) {
          if (ncol(ci.custom[[i]]) != 2 ) { bad.dimension <- TRUE }
        }
      }  
      if (bad.dimension) { error.present <- c(error.present, "% Error: The numeric matrix in 'ci.custom' must have two columns (lower bound and upper bound, respectively). \n") }
    }
    
    if (!is.numeric(ci.level)) { error.present <- c(error.present, "% Error: Argument 'ci.level' must be of type 'numeric.' \n") }
  
    if ((!is.character(ci.separator)) && (!is.null(ci.separator))) { error.present <- c(error.present, "% Error: Argument 'ci.separator' must be NULL (default), or of type 'character.'\n") }
    if ((length(ci.separator) != 1) && (!is.null(ci.separator))) { error.present <- c(error.present, "% Error: Argument 'ci.separator' must be of length 1.'\n") }
  
    add.lines <- .turn.into.list(add.lines)
    if ((!is.list(add.lines)) && (!is.null(add.lines))) { error.present <- c(error.present, "% Error: Argument 'add.lines' must be NULL (default), or a list of vectors. \n") }
    if (!is.null(add.lines)) {
      if (length(add.lines) < 1) { error.present <- c(error.present, "% Error: The list in argument 'add.lines' must be of length 1 or more. \n") }
      if (!all(unlist(lapply(add.lines, is.vector)))) { error.present <- c(error.present, "% Error: Argument 'add.lines' must be NULL (default), or a list of vectors. \n") }
    }
  
    if ((!is.function(apply.coef)) && (!is.null(apply.coef))) { error.present <- c(error.present, "% Error: Argument 'apply.coef' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.se)) && (!is.null(apply.se))) { error.present <- c(error.present, "% Error: Argument 'apply.se' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.t)) && (!is.null(apply.t))) { error.present <- c(error.present, "% Error: Argument 'apply.t' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.p)) && (!is.null(apply.p))) { error.present <- c(error.present, "% Error: Argument 'apply.p' must be NULL (default), or a function.'\n") }
    if ((!is.function(apply.ci)) && (!is.null(apply.ci))) { error.present <- c(error.present, "% Error: Argument 'apply.ci' must be NULL (default), or a function.'\n") }

    if (!is.character(column.sep.width)) { error.present <- c(error.present, "% Error: Argument 'column.sep.width' must be of type 'character.'\n") }
    if (length(column.sep.width) != 1)  { error.present <- c(error.present, "% Error: Argument 'column.sep.width' must be of length 1.'\n") }
  
    if ((!is.character(decimal.mark)) && (!is.null(decimal.mark))) { error.present <- c(error.present, "% Error: Argument 'decimal.mark' must be NULL (default), or of type 'character.'\n") }
    if ((length(decimal.mark) != 1) && (!is.null(decimal.mark))) { error.present <- c(error.present, "% Error: Argument 'decimal.mark' must be of length 1.'\n") }
  
    if (!is.logical(df)) { error.present <- c(error.present, "% Error: Argument 'df' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(df) != 1) { error.present <- c(error.present, "% Error: Argument 'df' must be of length 1.'\n") }
    
    if ((!is.numeric(digit.separate)) && (!is.null(digit.separate)) & (!is.character(digit.separate))) { error.present <- c(error.present, "% Error: Argument 'digit.separate' must be NULL (default), a vector of type 'numeric,' or of type 'character.' \n") }
    if (is.character(digit.separate)) {
      if (!(digit.separate %in% c("lakh","japan","china"))) { error.present <- c(error.present, "% Error: If argument 'digit.separate' is of type character, it must be one of \"lakh\"/\"china\"/\"japan\".\n") }
    }
    
    if ((!is.character(digit.separator)) && (!is.null(digit.separator))) { error.present <- c(error.present, "% Error: Argument 'digit.separator' must be NULL (default), or of type 'character.'\n") }
    if ((length(digit.separator) != 1) && (!is.null(digit.separator))) { error.present <- c(error.present, "% Error: Argument 'digit.separator' must be of length 1.'\n") }
    
    if ((!is.numeric(digits)) && (!is.null(digits))) { 
      if (!is.na(digits)) { error.present <- c(error.present, "% Error: Argument 'digits' must be NULL (default), or of type 'numeric.'\n") }
    }
    if ((length(digits) != 1) && (!is.null(digits))) { 
      if (!is.na(digits)) { error.present <- c(error.present, "% Error: Argument 'digits' must be of length 1.'\n") }
    }
    if (!is.null(digits)) {
      if (!is.na(digits)) {
        if ((digits<0) && (is.numeric(digits))) { error.present <- c(error.present, "% Error: Argument 'digits' must be >= 0.'\n") }
      }
    }
    
    if ((!is.numeric(digits.extra)) && (!is.null(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be NULL (default), or of type 'numeric.'\n") }
    if ((length(digits.extra) != 1) && (!is.null(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be of length 1.'\n") }
    if (!is.null(digits.extra)) {
      if ((digits.extra<0) && (is.numeric(digits.extra))) { error.present <- c(error.present, "% Error: Argument 'digits.extra' must be >= 0.'\n") }
    }
  
    if (!is.logical(flip)) { error.present <- c(error.present, "% Error: Argument 'flip' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(flip) != 1) && (!is.null(flip))) { error.present <- c(error.present, "% Error: Argument 'flip' must be of length 1.'\n") }
  
    if (!is.logical(float)) { error.present <- c(error.present, "% Error: Argument 'float' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(float) != 1) && (!is.null(float))) { error.present <- c(error.present, "% Error: Argument 'float' must be of length 1.'\n") }
    
    if (!(float.env %in% c("table","table*","sidewaystable"))) { error.present <- c(error.present, "% Error: Argument 'float.env' must be one of \"table\", \"table*\" or \"sidewaystable\".\n") }  
    if (length(float.env) != 1) { error.present <- c(error.present, "% Error: Argument 'float.env' must be of length 1.'\n") }

    if (!is.null(font.size)) {
      if (!(font.size %in% c("tiny","scriptsize","footnotesize","small","normalsize","large","Large","LARGE","huge","Huge"))) { error.present <- c(error.present, "% Error: Argument 'font.size' must be NULL (default), or one of the available font sizes. See documentation.") }  
    }
    if ((length(font.size) != 1) && (!is.null(font.size))) { error.present <- c(error.present, "% Error: Argument 'font.size' must be of length 1.'\n") }
  
    if (!is.logical(header)) { error.present <- c(error.present, "% Error: Argument 'header' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(header) != 1) { error.present <- c(error.present, "% Error: Argument 'header' must be of length 1.'\n") }
      
    if ((!is.logical(initial.zero)) && (!is.null(initial.zero))) { error.present <- c(error.present, "% Error: Argument 'initial.zero' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(initial.zero) != 1) && (!is.null(initial.zero))) { error.present <- c(error.present, "% Error: Argument 'initial.zero' must be of length 1.'\n") }
  
    if (!is.logical(intercept.bottom)) { error.present <- c(error.present, "% Error: Argument 'intercept.bottom' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(intercept.bottom) != 1) { error.present <- c(error.present, "% Error: Argument 'intercept.bottom' must be of length 1.'\n") }
      
    if (!is.logical(intercept.top)) { error.present <- c(error.present, "% Error: Argument 'intercept.top' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(intercept.top) != 1) { error.present <- c(error.present, "% Error: Argument 'intercept.top' must be of length 1.'\n") }
  
    if (intercept.top && intercept.bottom) { error.present <- c(error.present, "% Error: Arguments 'intercept.bottom' and 'intercept.top' cannot both be TRUE. \n")}
  
    if ((!is.character(keep)) && (!is.numeric(keep)) && (!is.null(keep))) { error.present <- c(error.present, "% Error: Argument 'keep' must be NULL (default; all variables kept), or a vector of type 'character' or 'numeric.'\n") }
  
    if ((!is.character(keep.stat)) && (!is.null(keep.stat))) { error.present <- c(error.present, "% Error: Argument 'keep.stat' must be NULL (default), or a vector of type 'character.'\n") }
    keep.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
    if (is.character(keep.stat)) {
      is.acceptable <- unique(tolower(keep.stat) %in% keep.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'keep.stat' argument.\n") }
    } 
      
    if (!is.character(label)) { error.present <- c(error.present, "% Error: Argument 'label' must be of type 'character.'\n") }
  
    if ((!is.logical(model.names)) && (!is.null(model.names))) { error.present <- c(error.present, "% Error: Argument 'model.names' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(model.names) != 1) && (!is.null(model.names))) { error.present <- c(error.present, "% Error: Argument 'model.names' must be of length 1.'\n") }
    
    if ((!is.logical(model.numbers)) && (!is.null(model.numbers))) { error.present <- c(error.present, "% Error: Argument 'model.numbers' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(model.numbers) != 1) && (!is.null(model.numbers))) { error.present <- c(error.present, "% Error: Argument 'model.numbers' must be of length 1.'\n") }
  
    if (!is.logical(multicolumn)) { error.present <- c(error.present, "% Error: Argument 'multicolumn' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(multicolumn) != 1) { error.present <- c(error.present, "% Error: Argument 'multicolumn' must be of length 1.'\n") }
  
    if ((!is.logical(no.space)) && (!is.null(no.space))) { error.present <- c(error.present, "% Error: Argument 'no.space' must be NULL (default), or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(no.space) != 1) && (!is.null(no.space))) { error.present <- c(error.present, "% Error: Argument 'no.space' must be of length 1.'\n") }
    
    if ((!is.character(notes)) && (!is.null(notes))) { error.present <- c(error.present, "% Error: Argument 'notes' must be NULL (default), or a vector of type 'character.'\n") }
    
    if (!is.null(notes.align)) {
      if (!(tolower(notes.align) %in% c("l","c","r"))) { error.present <- c(error.present, "% Error: Argument 'notes.align' must be NULL (default), or \"l\"/\"c\"/\"r\".\n") }  
    }
    if ((length(notes.align) != 1) && (!is.null(notes.align))) { error.present <- c(error.present, "% Error: Argument 'notes.align' must be of length 1.'\n") }
  
    if (!is.logical(notes.append)) { error.present <- c(error.present, "% Error: Argument 'notes.append' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(notes.append) != 1) { error.present <- c(error.present, "% Error: Argument 'notes.append' must be of length 1.'\n") }
  
    if ((!is.character(notes.label)) && (!is.null(notes.label))) { error.present <- c(error.present, "% Error: Argument 'notes.label' must be NULL (default), or of type 'character.'\n") }
    if ((length(notes.label) != 1) && (!is.null(notes.label))) { error.present <- c(error.present, "% Error: Argument 'notes.label' must be of length 1.'\n") }
  
    if (!is.logical(object.names)) { error.present <- c(error.present, "% Error: Argument 'object.names' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(object.names) != 1) { error.present <- c(error.present, "% Error: Argument 'object.names' must be of length 1.'\n") }
    
    if ((!is.character(omit)) && (!is.numeric(omit)) && (!is.null(omit))) { error.present <- c(error.present, "% Error: Argument 'omit' must be NULL (default; no omissions), or a vector of type 'character' or 'numeric.'\n") }
    if ((!is.character(omit.labels)) && (!is.null(omit.labels))) { error.present <- c(error.present, "% Error: Argument 'omit' must be NULL (default; no omissions), or a vector of type 'character.'\n") }
    if (!is.null(omit.labels)) {
      if (length(omit) != length(omit.labels)) { error.present <- c(error.present, "% Error: Arguments 'omit.labels' must be NULL (default; no omissions), or equal in length to 'omit.labels'.'\n") }
    }
  
    if ((!is.character(omit.stat)) && (!is.null(omit.stat))) { error.present <- c(error.present, "% Error: Argument 'omit.stat' must be NULL (default), or a vector of type 'character.'\n") }
    omit.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
    if (is.character(omit.stat)) {
      is.acceptable <- unique(tolower(omit.stat) %in% omit.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'omit.stat' argument.\n") }
    } 
  
    if ((!is.character(omit.summary.stat)) && (!is.null(omit.summary.stat))) { error.present <- c(error.present, "% Error: Argument 'omit.summary.stat' must be NULL (default), or a vector of type 'character.'\n") }
    omit.summary.stat.acceptable <- c("n","mean","sd","min","p25","median","p75","max")
    if (is.character(omit.summary.stat)) {
      is.acceptable <- unique(tolower(omit.summary.stat) %in% omit.summary.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'omit.summary.stat' argument.\n") }
    } 
  
    if ((!is.character(omit.yes.no)) && (!is.null(omit.yes.no))) { error.present <- c(error.present, "% Error: Argument 'omit.yes.no' must be a vector of type 'character.'\n") }
    if ((length(omit.yes.no) != 2) && (!is.null(omit.yes.no))) { error.present <- c(error.present, "% Error: Argument 'omit.yes.no' must be of length 2.'\n") }
  
    if ((!is.character(order)) && (!is.numeric(order)) & (!is.null(order))) { error.present <- c(error.present, "% Error: Argument 'order' must be NULL (default; no omissions), or a vector of type 'character' or 'numeric.'\n") }
    
    if (!is.logical(ord.intercepts)) { error.present <- c(error.present, "% Error: Argument 'ord.intercepts' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(ord.intercepts) != 1) { error.present <- c(error.present, "% Error: Argument 'ord.intercepts' must be of length 1.'\n") }
    
    if (!is.logical(perl)) { error.present <- c(error.present, "% Error: Argument 'perl' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(perl) != 1) { error.present <- c(error.present, "% Error: Argument 'perl' must be of length 1.'\n") }
  
    if (!(is.logical(colnames)) && (!is.null(colnames))) { error.present <- c(error.present, "% Error: Argument 'colnames' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(colnames) != 1) && (!is.null(colnames))) { error.present <- c(error.present, "% Error: Argument 'colnames' must be of length 1.'\n") }
  
    if (!(is.logical(rownames)) && (!is.null(rownames))) { error.present <- c(error.present, "% Error: Argument 'rownames' must be NULL, or of type 'logical' (TRUE/FALSE) \n") }
    if ((length(rownames) != 1) && (!is.null(rownames))) { error.present <- c(error.present, "% Error: Argument 'rownames' must be of length 1.'\n") }
  
    if (!is.character(rq.se)) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be of type 'character.' \n") }
    if (length(rq.se) != 1) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be of length 1.'\n") }
    if (is.character(rq.se)) {
      if (!(rq.se %in% c("iid", "nid", "ker", "boot"))) { error.present <- c(error.present, "% Error: Argument 'rq.se' must be one of: 'iid', 'nid', 'ker' or 'boot.' \n") }
    }
  
    if (!is.logical(selection.equation)) { error.present <- c(error.present, "% Error: Argument 'selection.equation' must be of type 'logical' (TRUE/FALSE) \n") }
    if ((length(selection.equation) != 1) && (!is.null(selection.equation))) { error.present <- c(error.present, "% Error: Argument 'selection.equation' must be of length 1.'\n") }
  
    if (!is.logical(single.row)) { error.present <- c(error.present, "% Error: Argument 'single.row' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(single.row) != 1) { error.present <- c(error.present, "% Error: Argument 'single.row' must be of length 1.'\n") }
  
    if ((!is.character(star.char)) && (!is.null(star.char))) { error.present <- c(error.present, "% Error: Argument 'star.char' must be NULL (default), or of type 'character.'\n") }
    if ((!(length(star.char) >= 1)) && (!is.null(star.char))) { error.present <- c(error.present, "% Error: Argument 'star.char' must be at least of length 1.'\n") }
    
    if (!is.null(star.cutoffs)) {
      if (sum(is.na(star.cutoffs)) != length(star.cutoffs)) {
        if (!is.numeric(star.cutoffs)) { error.present <- c(error.present, "% Error: Argument 'star.cutoffs' must be NULL (default), or a vector of type 'numeric.'\n") }
      }
      if ( !(length(star.cutoffs) >= 1) && (!is.null(star.cutoffs))) { error.present <- c(error.present, "% Error: Argument 'star.cutoffs' must be a vector with at least one element.\n") }
      if (sum(star.cutoffs[!is.na(star.cutoffs)] == sort(star.cutoffs, decreasing = TRUE, na.last=NA)) != length(star.cutoffs[!is.na(star.cutoffs)])) { error.present <- c(error.present, "% Error: The elements of 'star.cutoffs' must be in weakly decreasing order.\n") }
    }
  
    if ((!is.character(summary.stat)) && (!is.null(summary.stat))) { error.present <- c(error.present, "% Error: Argument 'summary.stat' must be NULL (default), or a vector of type 'character.'\n") }
    summary.stat.acceptable <- c("n","mean","sd","min","p25","median","p75","max")     # list of statistic codes that are acceptable
    if (is.character(summary.stat)) {
      is.acceptable <- unique(tolower(summary.stat) %in% summary.stat.acceptable)
      if (length(is.acceptable)>1) { is.acceptable <- FALSE }
      if (!is.acceptable) { error.present <- c(error.present, "% Error: Unknown statistic in 'summary.stat' argument.\n") }
    } 
  
    if ((!is.character(table.layout)) && (!is.null(table.layout))) { error.present <- c(error.present, "% Error: Argument 'table.layout' must be of type 'character.'\n") }
    if ((length(table.layout) != 1) && (!is.null(table.layout)))  { error.present <- c(error.present, "% Error: Argument 'table.layout' must be of length 1.'\n") }
    if (is.character(table.layout) && (length(table.layout)==1)) {   # test if report only contains allowed letters
      layout.error <- FALSE
      for (i in 1:nchar(table.layout)) {
        ch <- substring(table.layout,i,i)
        if (!(ch %in% c("=","-","!","l","d","m","c","#","b","t","o","a","s","n"))) (layout.error <- TRUE)
      }
      if (layout.error) { error.present <- c(error.present, "% Error: Invalid characters in 'table.layout'. See package documentation. \n") }
    }  
  
    if ((!is.character(omit.table.layout)) && (!is.null(omit.table.layout))) { error.present <- c(error.present, "% Error: Argument 'omit.table.layout' must be of type 'character.'\n") }
    if ((length(omit.table.layout) != 1) && (!is.null(omit.table.layout)))  { error.present <- c(error.present, "% Error: Argument 'omit.table.layout' must be of length 1.'\n") }
    if (is.character(omit.table.layout) && (length(omit.table.layout)==1)) {   # test if report only contains allowed letters
      layout.error <- FALSE
      for (i in 1:nchar(omit.table.layout)) {
        ch <- substring(omit.table.layout,i,i)
        if (!(ch %in% c("=","-","!","l","d","m","c","#","b","t","o","a","s","n"))) (layout.error <- TRUE)
      }
      if (layout.error) { error.present <- c(error.present, "% Error: Invalid characters in 'omit.table.layout'. See package documentation. \n") }
    }  
  
    if (!is.character(table.placement)) { error.present <- c(error.present, "% Error: Argument 'table.placement' must be of type 'character.'\n") }
    if (length(table.placement) != 1)  { error.present <- c(error.present, "% Error: Argument 'table.placement' must be of length 1.'\n") }
    if (is.character(table.placement) && (length(table.placement)==1)) {   # test if table.placement only contains allowed letters
      tp.error <- FALSE
      for (i in 1:nchar(table.placement)) {
        ch <- substring(table.placement,i,i)
        if (!(ch %in% c("h","t","b","p","!","H"))) (tp.error <- TRUE)
      }
      if (tp.error) { error.present <- c(error.present, "% Error: Argument 'table.placement' can only consist of \"h\",\"t\",\"b\",\"p\",\"!\",\"H\".\n") }
    }
  
    if ((!is.character(report)) && (!is.null(report))) { error.present <- c(error.present, "% Error: Argument 'report' must be of type 'character.'\n") }
    if ((length(report) != 1) && (!is.null(report)))  { error.present <- c(error.present, "% Error: Argument 'report' must be of length 1.'\n") }
    if (is.character(report) && (length(report)==1)) {   # test if report only contains allowed letters
      report.error <- FALSE
      for (i in 1:nchar(report)) {
        ch <- substring(report,i,i)
        if (!(ch %in% c("v","c","s","t","p","*"))) (report.error <- TRUE)
      }
      if (report.error) { error.present <- c(error.present, "% Error: Argument 'report' can only consist of \"v\",\"c\",\"s\",\"t\",\"p\",\"*\".\n") }
    }  
  
    if (!is.logical(zero.component)) { error.present <- c(error.present, "% Error: Argument 'zero.component' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(zero.component) != 1) { error.present <- c(error.present, "% Error: Argument 'zero.component' must be of length 1.'\n") }
    
    if (!is.logical(summary.logical)) { error.present <- c(error.present, "% Error: Argument 'summary.logical' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(summary.logical) != 1) { error.present <- c(error.present, "% Error: Argument 'summary.logical' must be of length 1.'\n") }

    if (!is.logical(nobs)) { error.present <- c(error.present, "% Error: Argument 'nobs' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(nobs) != 1) { error.present <- c(error.present, "% Error: Argument 'nobs' must be of length 1.'\n") }
    
    if (!is.logical(mean.sd)) { error.present <- c(error.present, "% Error: Argument 'mean.sd' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(mean.sd) != 1) { error.present <- c(error.present, "% Error: Argument 'mean.sd' must be of length 1.'\n") }
    
    if (!is.logical(min.max)) { error.present <- c(error.present, "% Error: Argument 'min.max' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(min.max) != 1) { error.present <- c(error.present, "% Error: Argument 'min.max' must be of length 1.'\n") }
    
    if (!is.logical(median)) { error.present <- c(error.present, "% Error: Argument 'median' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(median) != 1) { error.present <- c(error.present, "% Error: Argument 'median' must be of length 1.'\n") }
    
    if (!is.logical(iqr)) { error.present <- c(error.present, "% Error: Argument 'iqr' must be of type 'logical' (TRUE/FALSE) \n") }
    if (length(iqr) != 1) { error.present <- c(error.present, "% Error: Argument 'iqr' must be of length 1.'\n") }
    
    ## decide what style to use here: start with all settings, and then make adjustment based on desired journal

    # initialize pseudo-global variables at NULL
    .summary.object <- NULL
    .global.dependent.variables.written <- NULL
    .global.coefficients <- NULL
    .format.model.left <- NULL
    .format.model.right <- NULL
    .which.variable.label <- NULL
    .return.value <- NULL
    .publish.horizontal.line <- NULL
    .table.part.published <- NULL
    .format.omit.table <- NULL

    # info about the package and author
    .global.package.name <- "stargazer"
    .global.package.version <- "5.2.3"
    .global.package.author.name <- "Marek Hlavac"
    .global.package.author.affiliation <- "Social Policy Institute"
    .global.package.author.email <- "marek.hlavac at gmail.com"
    
    # statistics (.global variables)
    .global.formulas.rhs <- NULL
    .global.models <- NULL
    .global.dependent.variables <- NULL
    .global.coefficient.variables <- NULL
    .global.coef.vars.by.model <- NULL  ## list of coefficient variables by model - to be used by omit, omit.labels, etc
    .global.std.errors <- NULL
    .global.ci.lb <- NULL
    .global.ci.rb <- NULL
    .global.t.stats <- NULL
    .global.p.values <- NULL
    .global.N <- NULL
    .global.LL <- NULL
    .global.R2 <- NULL
    .global.mills <- NULL
    .global.max.R2 <- NULL # maximum possible R2 
    .global.adj.R2 <- NULL
    .global.AIC <- NULL
    .global.BIC <- NULL
    .global.scale <- NULL   # estimated scale parameter (gee)
    .global.UBRE <- NULL    # UBRE score (GAM)
    .global.sigma2 <- NULL  # sigma2 from arima
    .global.theta <- NULL   # theta from negative binomial
    .global.rho <- NULL
  
    .global.sel.equation <- NULL # selection equation, as opposed to default outcome equation, in heckit and 
    .global.zero.component <- NULL # zero, as opposed to count, component in hurdle and zeroinfl
    
    # with degrees of freedom
    .global.SER <- NULL   # residual standard error; standard error of the regression
    .global.F.stat <- NULL # F-statistic for the regression
    .global.chi.stat <- NULL  # chi-squared statistic
    .global.wald.stat <- NULL # Wald test statistic (for coxph)
    .global.lr.stat <- NULL  # LR test statistic (for coxph)
    .global.logrank.stat <- NULL # Score (logrank) test (for coxph)
    .global.null.deviance <- NULL 
    .global.residual.deviance <- NULL
    
    # intercept strings
    .global.intercept.strings <- c("(Intercept)", "(intercept)","Intercept")
    
    # .formatting: Default
    .format.space.size <- "-1.8ex"
    
    .format.dependent.variable.text <- "\\textit{Dependent variable:}"
    .format.dependent.variable.text.underline <- TRUE
    .format.dependent.variable.text.on <- TRUE
    
    .format.dep.var.labels <- NULL
    .format.covariate.labels <- NULL
    .format.add.lines <- NULL
    
    .format.dependent.variables.text <- ""
    .format.underline.dependent.variables <- TRUE
    .format.dependent.variables.left <- ""
    .format.dependent.variables.right <- ""
    .format.dependent.variables.capitalize <- FALSE
    
    .format.ordered.intercepts <- TRUE
  
    # column labels
    .format.column.left <- ""
    .format.column.right <- ""
    
    # model numbers
    .format.model.numbers <- TRUE
        
    # common headers for multiple columns?
    .format.multicolumn <- TRUE
    
    # names for models
    .format.model.names.include <- TRUE
    .format.model.names <- NULL
    .format.model.names <- cbind(c("aov","ANOVA",""), c("arima","ARIMA",""), c("Arima","ARIMA",""), c("blogit","bivariate","logistic"))
    .format.model.names <- cbind(.format.model.names, c("bprobit","bivariate","probit"), c("betareg", "beta",""), c("chopit","compound hierarchical","ordered probit"))
    .format.model.names <- cbind(.format.model.names, c("clm","cumulative","link"), c("censReg", "censored", "regression"), c("cloglog.net","network compl.","log log"), c("clogit","conditional","logistic"), c("coxph","Cox","prop. hazards"))
    .format.model.names <- cbind(.format.model.names, c("dynlm","dynamic","linear"), c("lagsarlm","spatial","autoregressive"), c("errorsarlm","spatial","error"))
    .format.model.names <- cbind(.format.model.names, c("ei.dynamic","Quinn dynamic","ecological inference"), c("ei.hier","$2 \times 2$ hierarchical","ecological inference"))
    .format.model.names <- cbind(.format.model.names, c("ei.RxC","hierarchical multinominal-Dirichlet","ecological inference"), c("exp","exponential",""), c("ergm","exponential family","random graph"))
    .format.model.names <- cbind(.format.model.names, c("factor.bayes","Bayesian","factor analysis"), c("factor.mix","mixed data","factor analysis"))
    .format.model.names <- cbind(.format.model.names, c("factor.ord","ordinal data","factor analysis"), c("fGARCH","GARCH",""), c("gamma","gamma",""))
    .format.model.names <- cbind(.format.model.names, c("gamma.gee","gamma generalized","estimating equation"), c("gamma.mixed","mixed effects","gamma"))
    .format.model.names <- cbind(.format.model.names, c("gamma.net","network","gamma"), c("gamma.survey","survey-weighted","gamma"), c("glmrob","robust","GLM"), c("gls","generalized","least squares"))
    .format.model.names <- cbind(.format.model.names, c("gmm","GMM",""), c("rem.dyad", "relational", "event (dyadic)"))
    .format.model.names <- cbind(.format.model.names, c("irt1d","IRT","(1-dim.)"), c("irtkd","IRT","(k-dim.)"))
    .format.model.names <- cbind(.format.model.names, c("logit","logistic",""), c("logit.bayes","Bayesian","logistic"))
    .format.model.names <- cbind(.format.model.names, c("logit.gam","GAM","(logistic)"), c("logit.gee","logistic generalized","estimating equation"))
    .format.model.names <- cbind(.format.model.names, c("logit.mixed","mixed effects","logistic"), c("logit.net","network","logistic"))
    .format.model.names <- cbind(.format.model.names, c("logit.survey","survey-weighted","logistic"), c("lognorm","log-normal",""))
    .format.model.names <- cbind(.format.model.names, c("lmer","linear","mixed-effects"), c("glmer","generalized linear","mixed-effects"), c("nlmer","non-linear","mixed-effects"))
    .format.model.names <- cbind(.format.model.names, c("ls","OLS",""), c("ls.mixed","mixed effect","linear"), c("lme","linear","mixed effects"), c("lmrob","MM-type","linear"))
    .format.model.names <- cbind(.format.model.names, c("ls.net","network","least squares"), c("mlogit","multinomial","logistic"), c("mnlogit","multinomial","logit"))
    .format.model.names <- cbind(.format.model.names, c("mlogit.bayes","Bayesian","multinomial logistic"), c("negbin","negative","binomial"), c("normal","normal",""))
    .format.model.names <- cbind(.format.model.names, c("multinom","multinomial log-linear","(neural networks)"), c("nlme","non-linear","mixed effects"))
    .format.model.names <- cbind(.format.model.names, c("normal.bayes","Bayesian","normal"), c("normal.gam","GAM","(continuous)"))
    .format.model.names <- cbind(.format.model.names, c("normal.gee","normal generalized","estimating equation"), c("normal.net","network","normal"))
    .format.model.names <- cbind(.format.model.names, c("normal.survey","survey-weighted","normal"), c("ologit","ordered","logistic"))
    .format.model.names <- cbind(.format.model.names, c("oprobit","ordered","probit"), c("oprobit.bayes","Bayesian","ordered probit"))
    .format.model.names <- cbind(.format.model.names, c("pmg","mean","groups"), c("poisson","Poisson",""), c("poisson.bayes","Bayesian","Poisson"))
    .format.model.names <- cbind(.format.model.names, c("poisson.gam","GAM","(count)"), c("poisson.mixed","mixed effects","Poisson"))
    .format.model.names <- cbind(.format.model.names, c("poisson.survey","survey-weighted","Poisson"), c("poisson.gee","Poisson generalized","estimation equation"))
    .format.model.names <- cbind(.format.model.names, c("probit","probit",""), c("probit.bayes","Bayesian","probit"))
    .format.model.names <- cbind(.format.model.names, c("probit.gam","GAM","(probit)"), c("probit.gee","probit generalized","estimating equation"))
    .format.model.names <- cbind(.format.model.names, c("probit.mixed","mixed effects","probit"), c("probit.net","network","probit"))
    .format.model.names <- cbind(.format.model.names, c("probit.survey","survey-weighted","probit"), c("relogit","rare events","logistic"))
    .format.model.names <- cbind(.format.model.names, c("rq","quantile","regression"))
    .format.model.names <- cbind(.format.model.names, c("rlm","robust","linear"), c("sur","SUR",""), c("threesls","3SLS",""))
    .format.model.names <- cbind(.format.model.names, c("tobit","Tobit",""), c("tobit(AER)","Tobit",""), c("tobit.bayes","Bayesian","Tobit"))
    .format.model.names <- cbind(.format.model.names, c("twosls","2SLS",""), c("weibull","Weibull",""))
    .format.model.names <- cbind(.format.model.names, c("zeroinfl","zero-inflated","count data"), c("hurdle","hurdle",""))
    .format.model.names <- cbind(.format.model.names, c("plm","panel","linear"), c("pgmm","panel","GMM"), c("ivreg","instrumental","variable"))
    .format.model.names <- cbind(.format.model.names, c("coxreg","Cox",""), c("mlreg","ML","prop. hazards"), c("weibreg","Weibull",""))
    .format.model.names <- cbind(.format.model.names, c("aftreg","accelerated"," failure time"), c("phreg","parametric","prop. hazards"))
    .format.model.names <- cbind(.format.model.names, c("bj","Buckley-James",""), c("cph","Cox",""), c("Gls","generalized","least squares"), c("lrm","logistic",""))
    .format.model.names <- cbind(.format.model.names, c("ols","OLS",""), c("psm","parametric","survival"), c("Rq","quantile","regression"))
    .format.model.names <- cbind(.format.model.names, c("hetglm","heteroskedastic","GLM"), c("coeftest","coefficient","test"))
    .format.model.names <- cbind(.format.model.names, c("heckit","Heckman","selection"), c("selection","selection",""))
    .format.model.names <- cbind(.format.model.names, c("probit.ss","probit",""), c("binaryChoice","binary","choice"))
    .format.model.names <- cbind(.format.model.names, c("brglm","GLM","(bias reduction)"), c("maBina","binary model","(marginal effect)"))
    .format.model.names <- cbind(.format.model.names, c("mclogit","mixed","conditional logit"))
  
    # if you use, say, glm() that does not correspond to one of the pre-defined models, put this as family and link
    .format.model.function <- TRUE
    .format.model.family <- ""
    .format.model.dist <- ""
    .format.model.link <- "link = "
    
    ## names for journal/output styles
    # economics
    .journal.style.names <- cbind(c("aer","American Economic Review"), c("qje","Quarterly Journal of Economics"), c("econometrica","Econometrica"))
    .journal.style.names <- cbind(.journal.style.names, c("jpe","Journal of Political Economy"), c("jel","Journal of Economic Literature"))
    .journal.style.names <- cbind(.journal.style.names, c("jep","Journal of Economic Perspestives"))
    
    .format.coefficient.variables.capitalize <- FALSE
    .format.coefficient.variables.left <- ""
    .format.coefficient.variables.right <- ""
    .format.coefficient.table.parts <- c("variable name","coefficient*","standard error"," ")
    
    ## .formatting of numeric output
    # keep initial zeros?
    .format.initial.zero <- TRUE
    # if all zeros, keep going until you find a non-zero digit
    .format.until.nonzero.digit <- TRUE
    .format.max.extra.digits <- 2
    
    ## threshholds for the stars
    .format.stars <- "*"
    .format.cutoffs <- c(0.1, 0.05, 0.01)
    
    .format.std.errors.left <- "("
    .format.std.errors.right <- ")"
    
    .format.p.values.left <- "p = "
    .format.p.values.right <- ""
    
    .format.t.stats.left <- "t = "
    .format.t.stats.right <- ""
    
    .format.models.text <- ""
    .format.models.left <- "\\textit{"
    .format.models.right <- "}"
    .format.underline.models <- FALSE
    .format.models.skip.if.one <- TRUE # skip models section if only one model in table?
    .format.object.names <- FALSE
    
    .format.numbers.text <- ""
    .format.numbers.left <- "("
    .format.numbers.right <- ")"
    .format.numbers.roman <- FALSE
    
    .format.digit.separator.where <- c(3)    # how 'often' to separate digits (e.g., thousands separator = 3)
    .format.digit.separator <- ","
    .format.ci.separator <- ", "
    .format.round.digits <- 3
    # for decimal comma use: .format.decimal.character <- "{,}"
    .format.decimal.character <- "."
    .format.dec.mark.align <- FALSE
  
    # degrees of freedom - report or not?
    .format.df <- TRUE
    
    .format.table.parts <- c("=!","dependent variable label","dependent variables","models","colums","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","scale","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*(p)","chi2(df)*(p)","Wald(df)*(p)","LR(df)*(p)","logrank(df)*(p)","null deviance(df)","residual deviance(df)","=!","notes")
    
    .format.omit.regexp <- NULL
    .format.omit.labels <- NULL
    .format.omit.yes <- "Yes"
    .format.omit.no <- "No"
  
    .format.keep.regexp <- NULL
    
    .format.N <- "Observations"
    .format.LL <- "Log Likelihood"
    .format.R2 <- "R$^{2}$"
    .format.max.R2 <- "Max. Possible R$^{2}$"
    .format.adj.R2 <- "Adjusted R$^{2}$"
    .format.scale <- "Scale Parameter"
    .format.UBRE <- "UBRE"
    .format.rho <- "$\\rho$"
    .format.mills <- "Inverse Mills Ratio"
    .format.AIC <- "Akaike Inf. Crit."
    .format.BIC <- "Bayesian Inf. Crit."
    .format.sigma2 <- "$\\sigma^{2}$"
    .format.theta <- "$\\theta$"
    
    .format.SER <- "Residual Std. Error"
    .format.F.stat <- "F Statistic"
    .format.chi.stat <- "$\\chi^{2}$"
    .format.wald.stat <- "Wald Test"
    .format.lr.stat <- "LR Test"
    .format.logrank.stat <- "Score (Logrank) Test"
    .format.null.deviance <- "Null Deviance"
    .format.residual.deviance <- "Residual Deviance"
    
    .format.df.left <- "(df = "
    .format.df.right <- ")"
    .format.df.separator <- "; "
    .format.intelligent.df <- TRUE
    
    # this is for se, tstat, p.values at the bottom of the table, by statistics
    .format.se.left <- " ("
    .format.se.right <- ")"
    .format.tstat.left <- " (z = "
    .format.tstat.right <- ")"
    .format.p.value.left <- " (p = "
    .format.p.value.right <- ")"
    
    .format.intercept.name <- "Constant"
    .format.intercept.bottom <- TRUE
    .format.note <- "\\textit{Note:} "
    .format.note.alignment <- "r"
    .format.note.content <- c("$^{*}$p$<$[0.*]; $^{**}$p$<$[0.**]; $^{***}$p$<$[0.***]")
    
    #### summary statistic table
    .format.s.statistics.names <- cbind(c("n","N"), c("nmiss","missing"), c("mean","Mean"), c("sd","St. Dev."), c("median","Median"), c("min","Min"), c("max","Max"), c("mad","Median Abs. Dev."), c("p","Pctl(!)"))
    .format.s.stat.parts <- c("=!","stat names","-","statistics1","-!","notes")
    .format.s.statistics.list <- c("n","mean","sd","min","p25","median","p75","max")
    
    .format.s.statistics.names.left <- ""
    .format.s.statistics.names.right <- ""
    .format.s.statistics.names.label <- "Statistic"
    
    .format.s.coefficient.variables.capitalize <- FALSE
    .format.s.coefficient.variables.left <- ""
    .format.s.coefficient.variables.right <- ""
    
    .format.s.round.digits <- 3
    
    .format.s.note <- ""
    .format.s.note.alignment <- "l"
    .format.s.note.content <- NULL

    ####
    .adjust.settings.style(style)
    
    # continue only if no errors
    if (length(error.present) == 1) {
      
      # summary statistic table or regular table of data frame contents
      if (!is.null(summary)) { 
        
        # make sure summary is as long as the number of objects
        if (length(summary) > how.many.objects) { summary <- summary[1:how.many.objects] }
        if (length(summary) < how.many.objects) { length(summary) <- how.many.objects }
        
        # fill in values of summary, if NA keep deafult
        for (i in 1:how.many.objects) {
          if (!is.na(summary[i])) {
            .global.summary[i] <- summary[i]
          }
          else if (i > 1) {  # if NA fill in previous value of summary
            .global.summary[i] <- summary[i-1]
          }
        }
      }
    
      
      ## use formatting arguments
      
      # header with name, version, etc.
      .format.header <- header
      
      # no empty lines? single row for coefficient and std.error/CI?
      .format.single.row <- single.row
      if (.format.single.row == TRUE) { .format.no.space <- TRUE }
      else { .format.no.space <- FALSE }
      if (!is.null(no.space)) { .format.no.space <- no.space }
      
      # font size
      .format.font.size <- font.size
      
      # floating, floating environment, etc.
      .format.floating <- float
      .format.floating.environment <- float.env
      .format.table.placement <- table.placement
      .format.column.sep.width <- column.sep.width
      
      # if not case-sensitive, transfer to lower case
      if (!is.null(digit.separate)) { digit.separate <- tolower(digit.separate) }
      
      # report df?
      .format.df <- df
      if (.format.df == FALSE) {
        .format.table.parts <- gsub("(df)", "", .format.table.parts, fixed=TRUE)
      }
      
      # column, dependent variable and covariate labels
      .format.column.labels <- column.labels
      .format.column.separate <- column.separate
      .format.covariate.labels <- covariate.labels
      .format.dep.var.labels <- dep.var.labels
      .format.add.lines <- add.lines
      
      if (dep.var.labels.include == FALSE) {
        .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variables"] 
      }
      
      if (!is.null(dep.var.caption)) {
        if (dep.var.caption == "") {
          .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variable label"]
        }
        else {
          .format.dependent.variable.text <- dep.var.caption
        }
      }
      
      # confidence intervals
      .format.ci <- ci
      .format.ci.level <- ci.level
      if (!is.null(ci.separator)) { .format.ci.separator <- ci.separator }
      if (!is.null(ci.custom)) { .format.ci <- TRUE }
      
      # omit
      .format.omit.regexp <- omit
      .format.omit.index <- omit
      if (is.character(omit)) { .format.omit.index <- NULL }
      if (is.numeric(omit)) { .format.omit.regexp <- NULL }
            
      .format.omit.labels <- omit.labels
      if (!is.null(omit.yes.no)) { 
        .format.omit.yes <- omit.yes.no[1]
        .format.omit.no <- omit.yes.no[2]
      }
      
      # keep
      .format.keep.regexp <- keep
      .format.keep.index <- keep
      if (is.character(keep)) { .format.keep.index <- NULL }
      if (is.numeric(keep)) { .format.keep.regexp <- NULL }
      
      # remove omitted statistics from table parts
      if (!is.null(omit.stat)) {
        .lower.omit.stat <- tolower(omit.stat)    # make it all lower-case
        if ("all" %in% .lower.omit.stat) { .lower.omit.stat <- omit.stat.acceptable }
        if ("n" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="N"] }
        if ("rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="R-squared"] }
        if ("adj.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="adjusted R-squared"] }
        if ("max.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="max R-squared"] }
        if ("ll" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="log likelihood"] }
        if ("scale" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="scale"] }
        if ("sigma2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="sigma2"] }        
        if ("theta" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="theta"] }
        if ("aic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="AIC"] }
        if ("bic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="BIC"] }
        if ("ubre" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="UBRE"] }
        if ("rho" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="rho"] }
        if ("mills" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="Mills"] }
        if ("ser" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="SER"] }
        if ("f" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,11)!="F statistic"] }
        if ("chi2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="chi2"] }
        if ("wald" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="Wald"] }
        if ("lr" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,2)!="LR"] }
        if ("logrank" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,7)!="logrank"] }
        if ("null.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,13)!="null deviance"] }
        if ("res.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,17)!="residual deviance"] }
      }
      
      # keep statistics in the table
      if (!is.null(keep.stat)) {
        .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
        
        # do this by omitting everything except what you keep
        .lower.omit.stat <- c("n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho","Mills","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")
        .lower.omit.stat <- .lower.omit.stat[!(.lower.omit.stat %in% .lower.keep.stat) ]
        
        if ("n" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="N"] }
        if ("rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="R-squared"] }
        if ("adj.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="adjusted R-squared"] }
        if ("max.rsq" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="max R-squared"] }
        if ("ll" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="log likelihood"] }
        if ("scale" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="scale"] }
        if ("sigma2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="sigma2"] }        
        if ("theta" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="theta"] }
        if ("aic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="AIC"] }
        if ("bic" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="BIC"] }
        if ("ubre" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[.format.table.parts!="UBRE"] }
        if ("rho" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="rho"] }
        if ("mills" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,5)!="Mills"] }
        if ("ser" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,3)!="SER"] }
        if ("f" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,11)!="F statistic"] }
        if ("chi2" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="chi2"] }
        if ("wald" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,4)!="Wald"] }
        if ("lr" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,2)!="LR"] }
        if ("logrank" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,7)!="logrank"] }
        if ("null.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,13)!="null deviance"] }
        if ("res.dev" %in% .lower.omit.stat) { .format.table.parts <- .format.table.parts[substr(.format.table.parts,1,17)!="residual deviance"] }
      }
      
      # keep statistics in table parts
      if (!is.null(keep.stat)) {
        .lower.keep.stat <- tolower(keep.stat)    # make it all lower-case
        keep.stat.acceptable <- c("all","n","rsq","adj.rsq","max.rsq","ll","aic","bic","scale","ubre","rho(se)*","Mills(se)*","sigma2","ser","f","theta","chi2","wald","lr","logrank","null.dev","res.dev")     # list of statistic codes that are acceptable
        remove.stats <- keep.stat.acceptable[!(keep.stat.acceptable %in% .lower.keep.stat)]
        .format.table.parts <- .format.table.parts[!(.format.table.parts %in% remove.stats)]
      }
      
      # digits, initial.zeros, decimal characters
      if (!is.null(decimal.mark)) { .format.decimal.character <- decimal.mark }
      if (!is.null(align)) { .format.dec.mark.align <- align }
      if (!is.null(digit.separator)) { .format.digit.separator <- digit.separator }
      if (!is.null(initial.zero)) { .format.initial.zero <- initial.zero }
      
      if (!is.null(digit.separate)) { 
        if (digit.separate=="lakh") { .format.digit.separator.where <- c(3,2) }  # lakhs 
        else if ((digit.separate=="china") || (digit.separate=="japan")) { .format.digit.separator.where <- 4 }
        else { .format.digit.separator.where <- digit.separate}
      }
      
      if (!is.null(digits)) { 
        .format.round.digits <- digits 
        .format.s.round.digits <- digits
      }
      
      if (!is.null(digits.extra)) { 
        .format.max.extra.digits <- digits.extra
        if (digits.extra>=1) { .format.until.nonzero.digit <- TRUE }
        else ( .format.until.nonzero.digit <- FALSE )
      }
      
      # intercept top and bottom
      if (!is.null(intercept.top)) { .format.intercept.top <- intercept.top }
      if (!is.null(intercept.bottom)) { .format.intercept.bottom <- intercept.bottom }
        
      # model names, numbers and multicolumn
      if (!is.null(model.names)) { 
        .format.model.names.include <- model.names 
        if (model.names == TRUE) { .format.models.skip.if.one <- FALSE }
      }    
      if (!is.null(model.numbers)) { .format.model.numbers <- model.numbers }
      .format.multicolumn <- multicolumn
      
      # object names
      .format.object.names <- object.names
      
      # report coefs, std errs, t, p?
      if (!is.null(report)) {
        .format.coefficient.table.parts <- NULL
        for (i in 1:nchar(report)) {
          component.letter <- substr(report, i, i)
          if (component.letter == "v") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "variable name") }
          if (component.letter == "c") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "coefficient") }
          if (component.letter == "s") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "standard error") }
          if (component.letter == "t") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "t-stat") }
          if (component.letter == "p") { .format.coefficient.table.parts <- append(.format.coefficient.table.parts, "p-value") }
          if ((component.letter == "*") && (i > 1)) { 
            l <- length(.format.coefficient.table.parts)
            if ((.format.coefficient.table.parts[l] != "variable name") && (substr(report,i-1,i-1) != "*")) {
              .format.coefficient.table.parts[l] <- paste(.format.coefficient.table.parts[l],"*",sep="")
            }
          }
        }
        .format.coefficient.table.parts <- append(.format.coefficient.table.parts, " ")
      }
      
      
      # significance stars
      if (!is.null(star.cutoffs)) { 
        # assign cutoff values
        .format.cutoffs <- star.cutoffs
      }
      
      if (!is.null(star.char)) { 
        .format.stars <- star.char
      }
      
      for (i in 1:length(.format.cutoffs)) {
        if (is.na(.format.stars[i])) {
          .format.stars[i] <- paste(rep(.format.stars[1], i), sep="", collapse="")
        }  
      }
      .format.stars <- .format.stars[1:length(.format.cutoffs)]
      
      # selection equation
      .global.sel.equation <- selection.equation
      
      # colnames and rownames
      if (!is.null(rownames)) { .format.rownames <- rownames }
      if (!is.null(colnames)) { .format.colnames <- colnames }
      
      # zero vs. count component
      .global.zero.component <- zero.component
      
      # notes
      
      replace.dec.mark <- function(s) { return (gsub(".", .format.decimal.character, s, fixed=TRUE))}
      
      # replace star cutoffs in the notes section
      for (i in 1:length(.format.cutoffs)) {
        if (!is.na(.format.stars[i])) {
          star.string <- paste(rep("*", i), sep="", collapse="")
          .format.note.content <- gsub(paste("[.",star.string,"]",sep=""), replace.dec.mark(gsub("^[0]+", "",.format.cutoffs[i])), .format.note.content, fixed=TRUE)  
          .format.note.content <- gsub(paste("[0.",star.string,"]",sep=""), replace.dec.mark(.format.cutoffs[i]), .format.note.content, fixed=TRUE)
          .format.note.content <- gsub(paste("[",star.string,"]",sep=""), replace.dec.mark(.format.cutoffs[i]*100), .format.note.content, fixed=TRUE)        
        }
      }

      
      if (!is.null(notes)) { 
        if (notes.append == TRUE) {
          .format.note.content <- c(.format.note.content, notes)
          .format.s.note.content <- c(.format.s.note.content, notes)
        }
        else {
          .format.note.content <- notes
          .format.s.note.content <- notes
        }
      }
      if (!is.null(notes.align)) { 
        .format.note.alignment <- notes.align 
        .format.s.note.alignment <- notes.align
      }
      
      if (!is.null(notes.label)) { 
        .format.note <- notes.label
        .format.s.note <- notes.label
      }    
      
      # ordered probit/logit, etc. - report intercepts?
      .format.ordered.intercepts <- ord.intercepts
      
      # perl-compatible regular expressions?
      .format.perl <- perl
      
      # standard error for quantile regression
      .format.rq.se <- rq.se
      
      # report logical variables in summary statistics tables?
      .format.summ.logical <- summary.logical
      
      # summary statistics - what statistics to report - !!! this needs to come before summary.stat and omit.summary.stat
      if (!nobs) { .format.s.statistics.list <- .format.s.statistics.list[.format.s.statistics.list!="n"] }
      if (!mean.sd) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="mean")&(.format.s.statistics.list!="sd")]}
      if (!min.max) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="min")&(.format.s.statistics.list!="max")]}
      if (!median) { .format.s.statistics.list <- .format.s.statistics.list[.format.s.statistics.list!="median"] }
      if (!iqr) { .format.s.statistics.list <- .format.s.statistics.list[(.format.s.statistics.list!="p25")&(.format.s.statistics.list!="p75")]}
      
      # keep summary statistics
      if (!is.null(summary.stat)) {
        .lower.keep.summary.stat <- tolower(summary.stat)    # make it all lower-case
        .format.s.statistics.list <- .lower.keep.summary.stat
     } 
      
      # remove omitted statistics from table parts
      if (!is.null(omit.summary.stat)) {
        .lower.omit.summary.stat <- tolower(omit.summary.stat)    # make it all lower-case
        .format.s.statistics.list <- .format.s.statistics.list[!(.format.s.statistics.list %in% .lower.omit.summary.stat)]
      }

      # table layout
      .format.table.parts.nonstat <- c("=","-","-!","=!","dependent variable label",
                                       "dependent variables","models","columns","numbers",
                                       "objects","coefficients","omit","additional","notes")  
                                      # these are the non-model statistics parts of the table
      
      if (!is.null(table.layout)) {
        .format.table.parts.new <- NULL
        for (i in 1:nchar(table.layout)) {
          component.letter <- substr(table.layout, i, i)
          if (component.letter == "=") { .format.table.parts.new <- append(.format.table.parts.new, "=") }
          if (component.letter == "-") { .format.table.parts.new <- append(.format.table.parts.new, "-") }
          if ((component.letter == "!") && (i > 1)) { 
            if (.format.table.parts.new[i-1] %in% c("-","=")) {
              .format.table.parts.new[i-1] <- paste(.format.table.parts.new[i-1], "!", sep="")
            }
          }
          if (component.letter == "l") { .format.table.parts.new <- append(.format.table.parts.new, "dependent variable label") }
          if (component.letter == "d") { .format.table.parts.new <- append(.format.table.parts.new, "dependent variables") }
          if (component.letter == "m") { 
            .format.table.parts.new <- append(.format.table.parts.new, "models") 
            .format.model.names.include <- TRUE 
          }
          if (component.letter == "c") { .format.table.parts.new <- append(.format.table.parts.new, "columns") }
          if (component.letter == "#") { 
            .format.table.parts.new <- append(.format.table.parts.new, "numbers")
            .format.model.numbers <- TRUE
          }  
          if (component.letter == "b") { 
            .format.table.parts.new <- append(.format.table.parts.new, "objects") 
            .format.object.names <- TRUE
          }  
          if (component.letter == "t") { .format.table.parts.new <- append(.format.table.parts.new, "coefficients") }  
          if (component.letter == "o") { .format.table.parts.new <- append(.format.table.parts.new, "omit") }  
          if (component.letter == "a") { .format.table.parts.new <- append(.format.table.parts.new, "additional") }  
          if (component.letter == "n") { .format.table.parts.new <- append(.format.table.parts.new, "notes") }  
          if (component.letter == "s") { 
            .format.table.parts.new <- append(.format.table.parts.new, 
                                              .format.table.parts[!(.format.table.parts %in% .format.table.parts.nonstat)]) 
          }
          
        }
        .format.table.parts <- .format.table.parts.new
      }
      
      # now omit table parts
      if (!is.null(omit.table.layout)) {
        for (i in 1:nchar(omit.table.layout)) {
          component.letter <- substr(omit.table.layout, i, i)
          if (component.letter == "=") { .format.table.parts <- .format.table.parts[.format.table.parts!="="] }
          if (component.letter == "-") { .format.table.parts <- .format.table.parts[.format.table.parts!="-"] }
          if ((component.letter == "!") && (i > 1)) {
            if (substr(omit.table.layout, i-1, i-1) == "=") { .format.table.parts <- .format.table.parts[.format.table.parts!="=!"] }
            if (substr(omit.table.layout, i-1, i-1) == "-") { .format.table.parts <- .format.table.parts[.format.table.parts!="-!"] }
          }
          if (component.letter == "l") { .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variable label"] }
          if (component.letter == "d") { .format.table.parts <- .format.table.parts[.format.table.parts!="dependent variables"] }
          if (component.letter == "m") { .format.table.parts <- .format.table.parts[.format.table.parts!="models"] }
          if (component.letter == "c") { .format.table.parts <- .format.table.parts[.format.table.parts!="columns"] }
          if (component.letter == "#") { .format.table.parts <- .format.table.parts[.format.table.parts!="numbers"] }
          if (component.letter == "b") { .format.table.parts <- .format.table.parts[.format.table.parts!="objects"] }
          if (component.letter == "t") { .format.table.parts <- .format.table.parts[.format.table.parts!="coefficients"] }
          if (component.letter == "o") { .format.table.parts <- .format.table.parts[.format.table.parts!="omit"] }
          if (component.letter == "a") { .format.table.parts <- .format.table.parts[.format.table.parts!="additional"] }
          if (component.letter == "n") { .format.table.parts <- .format.table.parts[.format.table.parts!="notes"] }
          if (component.letter == "s") { .format.table.parts <- .format.table.parts[.format.table.parts %in% .format.table.parts.nonstat] }          
        }
      }
      
      
      # intelligent division of regression tables vs. summary statistics tables
      regression.table.objects <- NULL
      number.of.table <- 0
      title.table <- NULL
      label.table <- NULL
      for (i in seq(1:how.many.objects)) {
        if (is.data.frame(objects[[i]])==TRUE) {
          if (!is.null(regression.table.objects)) { 
            number.of.table <- number.of.table + 1    # allows for multiple table titles and labels
            
            if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
            else { .format.title <- title[length(title)] }
            
            if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
            else { .format.label <- label[length(label)] }
            
            if (type == "latex") {
              do.call(.stargazer.reg.table, as.list(objects[regression.table.objects]))  
              invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
            }
            else if ((type == "text") || (type == "html") || (type == "mmd") ) {
              latex.code <- c(latex.code, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
            }
          }
          
          number.of.table <- number.of.table + 1
          if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
          else { .format.title <- title[length(title)] }
          
          if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
          else { .format.label <- label[length(label)] }
          
          if (.global.summary[i]==TRUE) {
            if (type == "latex") {
              .stargazer.summ.stat.table(objects[[i]])
              invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(.stargazer.summ.stat.table(objects[[i]]),file=NULL)) )
            }
            else if ((type == "text") || (type == "html") || (type == "mmd")) {
              latex.code <- c(latex.code, invisible(capture.output(.stargazer.summ.stat.table(objects[[i]]),file=NULL)) )
            }
          }
          else {
            if (type == "latex") {
              .stargazer.data.frame.table(objects[[i]])
              invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(.stargazer.data.frame.table(objects[[i]]),file=NULL)) )
            }
            else if ((type == "text") || (type == "html") || (type == "mmd")) {
              latex.code <- c(latex.code, invisible(capture.output(.stargazer.data.frame.table(objects[[i]]),file=NULL)) )
            }
          }
          regression.table.objects <- NULL
        }
        else {
          regression.table.objects <- c(regression.table.objects, i)
          .global.object.names <- .global.object.names.all[regression.table.objects]
        }
      }
      
      if (!is.null(regression.table.objects)) {	
        number.of.table <- number.of.table + 1
        if (!is.na(title[number.of.table])) { .format.title <- title[number.of.table] }
        else { .format.title <- title[length(title)] }
        
        if (!is.na(label[number.of.table])) { .format.label <- label[number.of.table] }
        else { .format.label <- label[length(label)] }
        
        if (type == "latex") {
          do.call(.stargazer.reg.table, as.list(objects[regression.table.objects]))  
          invisible.output <- latex.code <- c(invisible.output, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
        }
        else if ((type == "text") || (type == "html") || (type == "mmd")) {
          latex.code <- c(latex.code, invisible(capture.output(do.call(.stargazer.reg.table, as.list(objects[regression.table.objects])),file=NULL)) )
        }
      }
      
      # don't do text output or file outputs if there are errors
      if (type == "text") {
        .text.output(latex.code)
        invisible.output <- invisible(capture.output(.text.output(latex.code)))
      }
      else if (type == "html") {
        .html.output(latex.code)
        invisible.output <- invisible(capture.output(.html.output(latex.code)))
      }
      else if (type == "mmd") {
        .mmd.output(latex.code)
        invisible.output <- invisible(capture.output(.mmd.output(latex.code)))
      }
      
      if (length(out) >= 1) { 
        text.out <- invisible(capture.output(.text.output(latex.code)))
        html.out <- invisible(capture.output(.html.output(latex.code)))
        .output.file(out, latex.code, text.out, html.out, type, out.header) 
      }
    }
    else { 
      if (suppress.errors == FALSE) {
        cat(error.present, sep="")
        invisible.output <- latex.code <- error.present
      }
      else {
        invisible.output <- latex.code <- ""
      }
    }
  
    options(warn=warn)
    return(invisible(invisible.output))
}

