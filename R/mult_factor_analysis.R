##' {Fit a series of factor solutions from 1:k}
##' {Fit a series of factor solutions}
##' @title fit_factor_series
##' @inheritParams psych::fa
##' @param data a matrix of numeric data
##' @param factors a sequence of numbers
##' @param meth method of rotation (see \link[psych]{fa}), defaults to minres 
##' @param rotation a method of rotation see \link[psych]{fa}. defaults to oblimin
##' @param scores which kind of factor scores to compute
##' @param ... further arguments passed through to psych::fa
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
##' {Extract loadings from a list of multiple factor solutions}
##' {As above}
##' @title get_loadings
##' @param fs a factor series object
##' @param loadings a scalar specifying the minimum threshold for items to be returned
##' @return a list containing the loadings of all fs solutiions
##' @author Richard Morrisroe
get_loadings <- function (fs, loadings=0.3) {
  ind <- lapply(fs, extract_loadings, loadings)
  ind
}
##' {This seems very similar to FactorAverage, rationalise these functions ASAP}
##' (no empty lines)
##' {}
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
