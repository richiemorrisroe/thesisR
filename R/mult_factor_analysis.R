requireNamespace("psych", quietly=TRUE)
## load("~/Code/thesisR/data/healthoptmind.rda")
## lotr <- dat[,with(dat, grep("LOTR", x=names(dat)))]
## rand <- dat[,with(dat, grep("RAND", x=names(dat)))]
## maas <- dat[,with(dat, grep("MAAS", x=names(dat)))]
##' .. content for \description{} (no empty lines) ..
##' I'm pretty sure I never finished this function
##' .. content for \details{} ..
##' @title mult_factor_analysis
##' @param data a dataframe with all numeric columns with no non-missing data
##' @param factors the number (or range) of factors to extract
##' @param meth a list containing all the methods of extractions to use, or "all" 
##' @param rotation a list containing all of the rotations to use, or "all" for all available rotations
##' @param scores Boolean, should factor scores be returned
##' @return an object of class mfa, containing all of the relevant information
##' @author Richard Morrisroe
mult_factor_analysis <- function (data, factors,
                                  meth=list("minres", "wls", "gls", "pa", "ml"),
                                  rotation=list("none", "varimax", "quartimax", "bentlerT", "geominT","promax", "oblimin","simplimax", "bentlerQ", "geominQ", "biquartimin" ),
                                  scores=list("regression", "Thurstone", "Anderson", "Bartlett", "tenBerge")) {
  orthrotations <- c("none", "varimax", "quartimax", "bentlerT", "geominT" )
  obliquerotations <- c("promax", "oblimin",
          "simplimax", "bentlerQ", "geominQ", "biquartimin")
  allrot <- c(orthrotations, obliquerotations)

  meth <- c("minres", "wls", "gls", "pa", "ml")
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
    y <- psych::fa(na.omit(data), nfactors=fno,
                   rotate="oblimin", fm=meth[j])
    assign(paste("meth", j, sep=""), value=y)
    fmlist[[j]] <- get(paste("meth", j, sep=""))
}
  names(fmlist) <- meth
  fnolist <-  list()
  for (k in seq_along(along.with=fno)) {
      z <- psych::fa(na.omit(data), nfactors=fno[k],
                     rotate="oblimin", fm="minres")
      assign(paste("no_fac", k, sep=""), value=z)
      fnolist[[k]] <- get(paste("fno", k, sep=""))
  }
  
  res <- c(factormethods=fmlist, rotations=reslist, factors=fnolist)
  class(res) <- "mfa"
  res
}
##' .. content for \description{Extract a range of factor solutions} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title fit_factor_series
##' @param data a matrix of data, as per psych::fa()
##' @param factors a range of factors to fit
##' @param meth method of factor extraction to use
##' @param rotation if null, will fit all available rotations
##' @param scores type of factor scores to be returned
##' @param ... other arguments passed through to 
##' @return a factor_series object
##' @author Richard Morrisroe
fit_factor_series <- function(data, factors, meth, rotation, scores, ...) {
    fno <- factors
    if(length(fno) == 1) {
        stop("factors should be a range of numbers")
    }
      fnolist <-  list()
  for (k in seq_along(along.with=fno)) {
      z <- psych::fa(na.omit(data), nfactors=fno[k],
                     rotate="oblimin", fm="minres", ...)
      fnolist[[k]] <- z
      ## browser()
  }
    class(fnolist) <- c("factor_series", "fa")
    fnolist
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title fit_factor_rotations
##' @param data a matrix of data
##' @param factors how many factors to extract
##' @param meth method of factor extraction used
##' @param rotation if NULL, all rotations are extracted
##' @param scores which kind of factor scores to return
##' @param ... further methods passed through to fa
##' @return a factor_series object
##' @author Richard Morrisroe
fit_factor_rotations <- function(data, factors, meth, rotation, scores, ...) {
    rotations <- list("none", "varimax", "quartimax", "bentlerT", "geominT","promax", "oblimin","simplimax", "bentlerQ", "geominQ", "biquartimin" )
    fit_fa <- function(x) fa(data, rotation=x)
    fac_rot <- lapply(rotations, fit_fa(x))
    names(fac_rot) <- rotations
    class(fac_rot) <- c("factor_series", "fa")
    fac_rot
}
    
get_component <- function(fs, component) {
    fac <- unlist(sapply(fs, `[`, "factors"))
    compdata <- sapply(fs, `[`, component)
    if(length(compdata[[1]]) > 1) {
        warning("aggregating results using the mean function")
        compdata <- unlist(lapply(compdata, mean, na.rm=TRUE))
    }
    data.frame(component=compdata, factors=fac)
}
extractor <- function (fs, parameter) {
    function(fs) {
    get_component(fs, component=parameter)
}
    #return a function that extracts the given parameter
}
chi <- extractor(parameter="chi")
rms <- extractor(parameter="rms")
communalities <- extractor(parameter="communality")
communality <- function(fs) {
    #deserves its own function
     lapply(fs, `[`, "communality")

}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title get_loadings
##' @param mfa
##' @return a list containing the loadings of all mfa solutions in mfa
##' @author Richard Morrisroe
get_loadings <- function (mfa) {
  ind <- lapply(mfa, ExtractLoadings)
  ind
}
##' .. content for \description{This seems very similar to FactorAverage, rationalise these functions ASAP} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title combine_loadings
##' @param mfa a multi-factor object
##' @return a list of mean loadings averaged over all potential factor solutions
##' @author Richard Morrisroe
combine_loadings <-  function (mfa) {
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

  lapply(loadings, function (x) Reduce("+", x))
}
##' .. content for \description{} (no empty lines) ..
##' Again, I don't think I actually finished this function
##' .. content for \details{} ..
##' @title display_rot
##' @param mfa
##' @param method
##' @param rotreq
##' @return results, yo!
##' @author Richard Morrisroe
display_rot <- function (mfa, method=NULL, rotreq=NULL) {
  rotationreq <- rotreq
  meth <- method
  resind <- grep(rotationreq, x=names(mfa))
  res <- mfa[[resind]]
}
