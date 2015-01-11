load("~/Code/thesisR/data/healthoptmind.rda")
lotr <- dat[,with(dat, grep("LOTR", x=names(dat)))]
rand <- dat[,with(dat, grep("RAND", x=names(dat)))]
maas <- dat[,with(dat, grep("MAAS", x=names(dat)))]
##' .. content for \description{} (no empty lines) ..
##' I'm pretty sure I never finished this function
##' .. content for \details{} ..
##' @title MultFactorAnalysis
##' @param data a dataframe with all numeric columns with no non-missing data
##' @param factors the number (or range) of factors to extract
##' @param meth a list containing all the methods of extractions to use, or "all" 
##' @param rotation a list containing all of the rotations to use, or "all" for all available rotations
##' @param scores Boolean, should factor scores be returned
##' @return an object of class mfa, containing all of the relevant information
##' @author Richard Morrisroe
MultFactorAnalysis <- function (data, factors, meth, rotation, scores) {
  orthrotations <- c("none", "varimax", "quartimax", "bentlerT", "geominT" )
  obliquerotations <- c("promax", "oblimin",
          "simplimax", "bentlerQ", "geominQ", "biquartimin")
  allrot <- c(orthrotations, obliquerotations)

  meth <- c("minres", "wls", "gls", "pa", "ml")
  Scores <- c("regression", "Thurstone", "Anderson", "Bartlett", "tenBerge")
  fno <- factors
  rotlist <- list()
  for (i in seq_along(along.with=allrot)) {
     x <- psych::fa(na.omit(data), nfactors=fno, rotate=allrot[i], fm="ml")
  assign(paste("rot", i,sep=""), value=x)

   rotlist[[i]] <- get(paste("rot", i, sep=""))

}
  names(rotlist) <- allrot
  reslist <- rotlist

  fmlist <- list()
  for (j in seq_along(along.with=meth)) {
    y <- fa(na.omit(data), nfactors=fno, rotate="oblimin", fm=meth[j])
    assign(paste("meth", j, sep=""), value=y)
    fmlist[[j]] <- get(paste("meth", j, sep=""))
}
  names(fmlist) <- meth
  fnolist <-  list()
  for (k in seq_along(along.with=fno)) {
      z <- fa(na.omit(data), nfactors=fno[k], rotate="oblimin", fm="minres")
      assign(paste("no_fac", k, sep=""), value=z)
      fnolist[[k]] <- get(paste("fno", k, sep=""))
  }
  
  res <- c(factormethods=fmlist, rotations=reslist, factors=fnolist)
  class(res) <- "mfa"
  res
}
fit_factor_series <- function(data, factors, meth, rotation, scores, ...) {
    fno <- factors
    if(length(fno)==1) {
        stop("factors should be a range of numbers")}
      fnolist <-  list()
  for (k in seq_along(along.with=fno)) {
      z <- fa(na.omit(data), nfactors=fno[k], rotate="oblimin", fm="minres", ...)
      fnolist[[k]] <- z
      ## browser()
  }
    fnolist
}
get_component <- function(fs, component) {
    compq <- component
    fac <- unlist(sapply(fs, `[`, "factors"))
    compdata <- unlist(sapply(fs, `[`, component))
    res <- data.frame(component=compdata, factors=fac)
}
extractor <- function (fs, parameter) {
    function(fs) {
    get_component(fs, component=parameter)
}
    #return a function that extracts the given parameter
}
get_chi_square <- extractor(fs=test, parameter="chi")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title GetLoadings
##' @param mfa
##' @return a list containing the loadings of all mfa solutions in mfa
##' @author Richard Morrisroe
getLoadings <- function (mfa) {
  ind <- lapply(mfa, ExtractLoadings)
  ind
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title CombineLoadings
##' @param mfa a multi-factor object
##' @return a list of mean loadings averaged over all potential factor solutions
##' @author Richard Morrisroe
combineLoadings <-  function (mfa) {
  loadlist <- list()
  for (i in seq_along(along.with=mfa)) {
    loadlist[[i]] <- mfa[[i]]$loadings

  }
  loadlist
  loadings <- list()
  for (j in seq_along(along.with=loadlist)) {


    loadings[[j]] <- as.matrix(unclass(loadlist[[j]]))
  }
  loadings

  meanload <- lapply(loadings, function (x) Reduce('+', x))
}
##' .. content for \description{} (no empty lines) ..
##' Again, I don't think I actually finished this function
##' .. content for \details{} ..
##' @title displayRot
##' @param mfa
##' @param method
##' @param rotreq
##' @return results, yo!
##' @author Richard Morrisroe
displayRot <- function (mfa, method=NULL, rotreq=NULL) {
  rotationreq <- rotreq
  meth <- method
  resind <- grep(rotationreq, x=names(mfa))
  res <- mfa[[resind]]
}
