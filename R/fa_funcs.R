##' \description{Takes an fa object, extracts the coefficients, and returns an xtable object of these coefficients (including communalities)}
##'
##' \details{Returns a table suitable for research. (Probably) doesn't meet APA, or indeed any, standards. Thin wrapper around FactorCoeff}
##' @title FactorXtab 
##' @param x an object of class "psych" "fa"
##' @param ...  further arguments passed to the FactorCoeff method
##' @return xtable object of coefficients
##' @author Richard Morrisroe
FactorXtab <-  function (x, names=NULL, ...) {
    x.mat.df <- FactorCoeff(x, names=names)
    fact.xtab <- xtable(x.mat.df, ...)
    return(fact.xtab)
}
##' \description{This function takes an fa object and extracts the coefficients from it} 
##' 
##'  \details{Extracts the loadings and communalities separately and cbinds them} 
##' @title FactorCoeff
##' @param x 
##' @param names 
##' @return a dataframe suitable for passing to xtable
##' @author Richie Morrisroe
FactorCoeff <- function (x, names=NULL) {
   x.load<-x$loadings
    x.comm<-x$communality
    x.names <- colnames(x.load)
    len <- length(colnames(x.load))
   
    x.comm.load<-cbind(x.load, x.comm)

   x.mat.df<-as.matrix.data.frame(x.comm.load)
   if(!is.null(names)) {
       names2 <- c(names, "Communalites")
       colnames(x.mat.df) <- names2
   }
   else {
       x.names[len+1] <- "Communalities"
       colnames(x.mat.df) <- x.names

   }
   return(x.mat.df)
}
##' \description{Extracts the correlations of factors from an fa object}
##' 
##' \details{gets the estimated factor correlations. Only really useful with oblique rotations}
##' @title FactorCor
##' @param x 
##' @param ... Further arguments passed to the xtable method
##' @return an xtable object containing the between-factor correlations
##' @author Richard Morrisroe
FactorCor <- function (x, ...) {
  res <- x$score.cor
  #allnames <- attr(x$loadings, "dimnames")
  factnames <- colnames(x$loadings)
  res <- as.data.frame(res)
  #names(res) <- factnames
  res.x <- xtable(res, ...)
}
##'  \description{This function returns a list of factor names (taken from the FA object) and the items which have an absolute correlation of greater than loadings}
##'
##' \details{Its not really that complicated, and honestly should be implemented generically}
##' @title ExtractLoadings
##' @param x an fa object
##' @param loadings the cutoff point for reporting an association
##' @return the rownames of all items which have an absolute value greater than or equal to loadings
##' @author Richard Morrisroe
ExtractLoadings <- function (x, loadings=0.3) {
    stopifnot(class(x)==c("psych", "fa"))
  x.load <- x$loadings
  x.uc.mat <- as.data.frame(unclass(x.load))
  xitemsload <- apply(x.uc.mat, 2, function (y) names(y[which(abs(y)>=loadings)])) #return the names of items which have appreciable loadings
  xitemsload
}
##' \description{Takes a psych fa object, and extracts the communalities and uniquenesses} (no empty lines) ..
##' 
##' \details{See description} ..
##' @title Extracth2u2
##' @param x a psych fa object
##' @return a data frame containing uniquenesses and communalities
##' @author Richard Morrisroe
Extracth2u2 <- function (x) {
  x.comm <- x$communality
  x.uniq <- x$uniquenesses
  x.ratio <- x.comm/x.uniq
  x.h2u2 <- as.data.frame(cbind(x.comm, x.uniq, x.ratio))
  x.h2u2
 }
##' .. content for \description{Extract TLI (NNFI), BIC and RMSEA from a fa psych object}
##' .. content for \details{} ..
##' @title FitIndices
##' @param x a psych fa object
##' @return A dataframe containing the fit indices
##' @author Richard Morrisroe
FitIndices <- function (x, labels=NULL) {
  tli <- x$TLI
  bic <- x$BIC
  rmsea <- x$RMSEA
  labels <-
  rmsnames <- attr(x$RMSEA, "names")
  res <- as.data.frame(cbind(tli, bic, rmsea[1], rmsea[2], rmsea[3]))
  print(length(res))
  colnames(res) <- c(paste(substitute(x),"TLI", sep=""),paste(substitute(x),"BIC", sep=""),
                     paste(substitute(x),"RMSEA", sep=""),
                     paste(substitute(x),"-90CInt", sep=""),
                     paste(substitute(x),"+90CInt", sep=""))
  res
}
##' \description{Performs a SVD based CV metric used in chemometrics}
##'
##' .. content for \details{Find reference for this - need to check external hard drive as no longer have academic access :(} ..
##' @title Svdcv
##' @param x 
##' @param ... 
##' @return an xtable object containing the SVD rank, prediction error and SD of prediction error
##' @author Richie Morrisroe
Svdcv <- function(x, ...) {
  msep <- x$msep
  K <- nrow(msep)
  rank <- seq(from = 0, to = x$maxrank, by = 1)
    msep.mean <- apply(x$msep, 2, mean)
    msep.se <- apply(x$msep, 2, sd)/sqrt(K)
  res <- as.data.frame(cbind(rank, msep.mean, msep.se))
  names(res) <- c("Rank", "Prediction Error", "Prediction Error SE")
  resxtab <- xtable(res, ...)
}
##' \description{Return the mean, SD, min and max of a set of variables stored in a dataframe}
##'
##' 
##' .. content for \details{return an APA standard mean, se, min and max table for summary statistics} ..
##' @title apademotables
##' @param data a dataframe containing all numeric variables
##' @param FUN function to use (currently not implemented
##' @param xtable Boolean. Return an xtable formatted object or not
##' @param ... other arguments passed to xtable
##' @return a dataframe containing the summary statistics
##' @author Richie Morrisroe
apademotables <- function(data, FUN=mean, xtable=FALSE, ...) {
    fun <- match.call(FUN)
    ## browser()
    data.m <- melt(data)
    data.tab <- ddply(data.m, .(variable), summarise, Mean=mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE), Min=min(value, na.rm=TRUE), Max=max(value, na.rm=TRUE))
    names(data.tab)[1] <- ""
    if(xtable==TRUE) {
        data.tab <- xtable(data.tab)
    }
    return(data.tab)
}
##' \description{Return an averaged factor analysis solution over a number of cross-validated splits} (no empty lines) ..
##'
##' \details{Takes two fa objects, and aggregates them into one set of coefficients. Can be useful either for combining CV selected splits of the correct size, and for bootstrapped FA results} ..
##' @title FactorAverage
##' @param sols a list of factor solutions to average over
##' @param mynames who the hell knows?
##' @param FUN function to aggregate by
##' @param .... other arguments passed to fun
##' @return a matrix containing the averaged fa solutions
##' @author Richie Morrisroe
FactorAverage <- function (sols=list(), mynames=NULL, FUN=mean, ....) {

    sols.coeff.list <- list()
    
    for(i in 1:length(sols)) {
        coeff <- as.data.frame(FactorCoeff(sols[[i]]))
        coeff.ord <- coeff[,mynames]
        sols.coeff.list[[i]] <- coeff.ord
        ## browser()
    }
    sols.coeff.list
    sols.list <- lapply(sols.coeff.list, as.matrix)

    resmat <- apply(simplify2array(sols.list), c(1,2), FUN)
    return(resmat)
}
##' \description{Give meaningful names to a fa solution}) ..
##' .. content for \details{As description} ..
##' @title FactorNames
##' @param fac a psych fa object
##' @param names a character vector of names to give to each factor (in order)
##' @return a psych fa object with the names applied to the coefficient matrix
##' @author Richie Morrisroe
FactorNames <- function(fac, names=NULL) {
    if(is.null(names)) {
        stop("Calling a function based on factor names with no names seems like a bad idea, don't you think?")
    }
    
    colnames(fac$loadings) <- names
    return(fac)
}


