##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title IrtXtab
##' @param x an IRT model object of <some_bunch> of classes
##' @param ... arguments based to xtable function
##' @return an xtable representation of the difficulty and/or discrimination parameters
##' @author Richard Morrisroe
IrtXtab <- function (x, ...) {
  eta<-x$etapar #$
  se<-x$se.eta #$
  eta.mat<-as.matrix(eta)
  se.eta.mat<-as.matrix(se)
  eta.par.mat<-cbind(eta.mat, se.eta.mat)
  colnames(eta.par.mat) <- c("Ability Estimate", "Standard Error")
  coef.xtab<-xtable(eta.par.mat, ...)
  coef.xtab
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title ggplotGRM
##' @param grm an IRT GRM model object
##' @param ... other methods passed to plotting function
##' @return a ggplot object
##' @author Richard Morrisroe
ggplotGRM <- function (grm, ...) {
  x <- coef(grm)
  x <- as.matrix(x)
  x <- x[,-ncol(x)]
  x.t <- t(x)
  response <- 1:nrow(x.t)
  respind <- ncol(x.t)+1
  x.t <- as.data.frame(x.t)
  x.t$response <- response
  x.tm <- melt(x.t, id="response")
  names(x.tm) <- c("threshold", "item", "ability")
  plot1 <- ggplot(x.tm, aes(x=ability, y=item, shape=as.factor(threshold), colour=as.factor(threshold)), ...)
  plot2 <- plot1+geom_point()+geom_rug()
  plot2
  }
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title coef2mat
##' @param gpcm a gpcm object (not relevant for GRM)
##' @return a matrix containing the estimated parameters from the gpcm model
##' @author Richard Morrisroe
coef2mat <- function (gpcm) {
  if(is.matrix(gpcm)) {
    return(gpcm)
  }
  else {

    len <- lapply(gpcm, length)
    probelem <- which.min(as.matrix(unlist(len)))
    dimcols <- max(as.matrix(unlist(len)))
    dimrows <- length(names(gpcm))
    mat.res <- matrix(NA, nrow=dimrows, ncol=dimcols)
    modlength <- lapply(gpcm, length)
    maxlength <- max(as.matrix(unlist(modlength)))
    for (i in 1:maxlength) {
      column <- lapply(gpcm, "[", i)
      column <- as.matrix(unlist(column))
      mat.res[1:length(column),i] <- column
      mat.res
    }
    rownames(mat.res) <- names(gpcm)
    probelemlength <- length(gpcm[[probelem]])
    missingvalue <- which(is.na(mat.res)) #this gives a scalar, as internally matrices are stored as vectors
    wrongvalue <- missingvalue-nrow(mat.res) #get the element where is the discrimination parameter has ended up
    mat.res[missingvalue] <- mat.res[wrongvalue]
    mat.res[wrongvalue] <- NA

    categories <- lapply(gpcm, names)
    categorynames <- categories[[which.max(sapply(categories, length))]]
    colnames(mat.res) <- categorynames
    return(mat.res)
  }
  mat.res
}
##' Extract the predictions from an IRT fascore object
##'
##' .. content for \details{} ..
##' @title getIrtPreds
##' @param x  an IRT model object
##' @return a dataframe containing observed scores, expected scores, the results of a z-test, and the se of the z-test
##' @author Richie Morrisroe
getIrtPreds <- function (x) {
  res <- x$score.dat[,c("Obs", "Exp", "z1","se.z1")]
  res
}
##' .. compare Z-scores for two IRT models
##'
##' .. content for \details{} ..
##' @title compareIRTscores
##' @param x an IRT model
##' @param y an IRT model
##' @return a list containing the (Pearson) correlations between the two z-scores, and the squared differences between the two sets of scores
##' @author Richie Morrisroe
compareIRTscores <- function (x, y) {
  scores.x <- x$z1
  scores.y <- y$z1
  cor.xy <- cor(scores.x, scores.y, method="pearson", use="pairwise.complete.obs")
  diff.xy <- (scores.x-scores.y)^2
  res <- list(cor=cor.xy, differences=diff.xy)
  res
}
##' .. Unfinished function used to perform cross-validation over IRT models
##'
##' .. content for \details{} ..
##' @title IRTcv
##' @param data a dataframe containing the data to be used
##' @param model the kind of model (either grm or gpcm)
##' @param constraint the constraint to use - see documentation for grm and gpcm objects
##' @param splits  the number of splits to use
##' @param .... 
##' @return a test and train set
##' @author Richie Morrisroe
IRTcv <- function (data, model=c("grm", "gpcm"), constraint=c(TRUE, FALSE, "rasch", "1PL", "gpcm"), splits=10, ....) {
  if(is.dataframe(data) ||is.matrix(data))
    stop("this function needs matrix or dataframe input")
  splittedsamples <- splitSample(data, splits)
  for (i in 1:length(splittedsamples)) {
    testset <- splittedsamples[i]
    trainset <- splittedsamples[!i]
  }
}
##' Yet another unsuccessful IRT CV function (look at this one, there were good ideas in there)
##'
##' .. content for \details{} ..
##' @title IrtCV
##' @param x 
##' @return a dataframe containing observed and expected scores
##' @author Richie Morrisroe
IrtCV <- function(x) {

#get observed frequencies from display command in package ltm
obs <- descript(x)$perc
totscores <- descript(x)$items
totscores[totscores==0] <- NA
model <- grm(x)
model.scores <- factor.scores(model, resp.patterns=x)
abilities <- model.scores$score.dat["z1"]
pointsweights <- model$GH
cutpoints <- pointsweights[[1]]
weights <- pointsweights[[2]]
q <- seq(from=0, to=1, by=0.05) #create 21 points
quadnorm <- qnorm(q) # map 21 points to the normal quantiles
totscores2 <- rowSums(x, na.rm=TRUE)
totscores2[totscores2==0] <- NA
ab.scores <- as.matrix(cbind(totscores2, abilities))
res <- list(obsscores=obs, totscores=totscores2, abscores=ab.scores, model=model, scores=model.scores, abilities=abilities, weights=weights)
}
## getscores <- function(x) { #this function appears to be completely pointless, in fact it may also have been causing my problems with the function below.
##   reslist <- list()
##   x <- as.data.frame(x)
##   probmat <- matrix(NA, nrow=100, ncol=30)
##   uniquescores <- unique(x[,1])
##   unique.sorted <- sort(uniquescores)
##   for(i in seq_along(unique.sorted)) {
##     scoremat <- x[x$totscores2==unique.sorted[i],]
##     uniqueabs <- unique(scoremat[,2])
##     rep1 <- uniqueabs
##     reslist[[i]] <- rep1
##     names(reslist[[i]]) <- paste("score", unique.sorted[i], sep="")
##   }
##   reslist
## }
##' Calculate 
##'
##' .. content for \details{} ..
##' @title probcalc
##' @param x 
##' @param totscores 
##' @return calculated probabilities
##' @author Richie Morrisroe
probcalc <- function(x, totscores) {
    res <- sapply(x, calcprob, totscores)
  }
##' I really wonder if I can salvage anything from these
##'
##' .. content for \details{} ..
##' @title calcprob
##' @param x 
##' @return calculated probabilities
##' @author Richie Morrisroe
calcprob <- function (x) {
  x2 <- x[,2]
  totscores <- x[,1]
  probcal <- list()
   for(i in seq_along(x2)) {
     if(is.na(x[[i]][1])) {
       next
     } else {
       y <- x[[i]]
       y <- sort(y)
       for(j in seq_along(y)) {
         if(is.na(y[1][j])) {
           next
           probcal[[i]] <- NA
         }
         else{
           p1 <- na.omit(length((y==y[j])))/ length(totscores)
           p2 <- length(y==y[j])/length(na.omit(totscores))
           p3 <- na.omit(length(y[j]))/length(na.omit(y))
           ## browser()
           p4 <- p1*p2
           p5 <- p4/p3
           probcal[[i]] <- p5
         }
       }

       probcal
     }
     probcal
   }
  probcal
}

#Estimate conditional distribution of test scores for each trait level p(Ability|score)= p(score)*p(ability)/p(score)
#p(abilities|totscores) (probably need to merge them into one dataframe for this).
#p(abilities|totscores)=P(ability)*P(totscores)/p(abilities)
#Bin participants on total scores
#Get scores for each participant (take from 0, to match typical IRT practice and the interpretation of scores as the number of thresholds successfully completed.
#This should be  distributed as a generalised multinomial for a polytomous model Use fitdistr from MASS to get best parameters of the distribution.
# Method 1: multiply the conditional probability by the weight associated with the quadrature point. (these are stored in the grm model as GH).  This provides an estimation of the expected proportion of participants having an observed test score.
#multiply the expected proportion by the sample size, the result is the expected frequency of of participants having a particular test score
#Method 2: if individual ability estimates are available for all participants, the marginal expected frequency for a given score is the sum of the conditional probabilities for this score across the N participants.
#This can be displayed graphically. In addition, a chi square test can be performed between the observed and expected frequencies, to give a measure of model mis-fit (with all the problems that the Chi-square test is heir too).


##' A test to assess the adaquecy of an IRT model
##'
##' .. content for \details{} ..
##' @title CondProbIrt
##' @param x an IRT object
##' @return Something really cool
##' @author Richie Morrisroe
CondProbIrt <- function(x) {
  abilities <- x[,1]
  totscores <- x[,2]
  s.ord <- order(x$totscores2)
  x.ord <- x[s.ord,]
  x.ord2 <- na.omit(x.ord)
  scores.len <- with(x.ord,tapply(z1, totscores2, length))
  ## browser()
  unique.ab <- with(x.ord,xtabs(z1~totscores2))
  unique.scores <- as.numeric(names(scores.len))
  probmat <- as.data.frame(matrix(NA, ncol=20, nrow=100))
  for(i in seq_along(unique.scores)) {
    cur.score <- x.ord2[x.ord2$totscores2==unique.scores[i],]
    unique.ab <- with(cur.score,table(z1, totscores2))
    unique.sc <- with(x.ord2, table(totscores2))
    for(j in seq_along(unique.ab)) {
      p1 <- unique.ab[j]/length(unique(x.ord2$z1)) #ab prob
      p2 <- nrow(cur.score)/nrow(x.ord2) #score prob
      p3 <- p1*p2
      p4 <- p3/p2
      probmat[j,i] <- p4
    }
    probmat
  }
  scorenames <- paste("Score", unique.scores, sep="")
  names(probmat) <- scorenames
  probmat
}

##' .. content for \description{Maybe a repeat of the earlier functions} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title getIRTestimates
##' @param fscores 
##' @return estimated abilities and their standard errors
##' @author Richie Morrisroe
getIRTestimates <- function(fscores) {
  data <- fscores[["score.dat"]]
  abest <- data[,c("z1", "se.z1")]
  names(abest) <- c("AbilityEst", "StdError")
  return(abest)
}
##' .. content for \description{Actual implemented IRT test on new data function} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title testIRTModels
##' @param oldmodel the original IRT model
##' @param newdata the new data
##' @param gpcmconstraint the constraint if the model is gpcm
##' @param grmconstraint the constraint if the model is grm
##' @param ... other arguments passed through
##' @return A dataframe containing two columns, ErrorApproximation and Correlation between models
##' @author Richie Morrisroe
testIRTModels <- function(oldmodel, newdata, gpcmconstraint=c("rasch", "1PL", "gpcm",), grmconstraint= c(TRUE, FALSE), ...) {
  if(class(oldmodel)=="gpcm") {
    constraint <- gpcmconstraint
  }
     else {
       constraint <- grmconstraint
     }

  comp.para <- length(unique(as.vector(coef(oldmodel))))
  predscores <- getIRTestimates(factor.scores(oldmodel, resp.patterns=newdata))
  if(class(oldmodel)=="gpcm") {
  newmodel <- gpcm(newdata, constraint=constraint)
}
  else {
    newmodel <- grm(newdata, constrained=constraint)
  }
  newscores <- getIRTestimates(factor.scores(newmodel, resp.patterns=newdata))
  diffscores <- mapply("-", predscores[,1], newscores[,1])
  rea <- sqrt(sum(diffscores^2))*log(comp.para)
  scorescor <- cor(predscores[,1], newscores[,1], ...)
  res <- data.frame(ErrorApproximation=rea, Correlation=scorescor)
  return(res)
}
##' .. content for \description{Extract fit functions from an OpenMx object} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title getMxFitFunctions
##' @param mx an mxFit object
##' @param label labels to apply to the fit functions returned
##' @return a dataframe containing the fit functions
##' @author Richie Morrisroe
getMxFitFunctions <- function(mx, label=NULL) {
    summ <- summary(mx)
    bic <- summ$BIC.Mx
    aic <- summ$AIC.Mx
    obs <- summ$numObs
    param <- summ$estimatedParameters

    res <- data.frame( bic, aic, obs, param)
    if(!is.null(label)) {
        rownames(res) <- label
    }
    return(res)
}
##' .. content for \description{Average a set of IRT Models fit on different cross-validation splits} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title irtAverage
##' @param sols IRT Models
##' @return a dataframe containing the averaged coefficients
##' @author Richie Morrisroe
irtAverage <- function(sols=list()) {
    coef <- lapply(sols, coef)
    res <- Reduce(`+`, x=coef)/length(coef)
    return(res)
}
            
##' .. content for \description{Claims to be a smoothed AIC function, but actually just extracts the AIC value from an MxRAMModel or MxModel} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title smoothAIC
##' @param model An MxModel object
##' @return the AIC of the object
##' @author Richie Morrisroe
smoothAIC <- function(model) {
    if(class(model) %in% c("MxRAMModel", "MxModel")) {
        res <- summary(model)$AIC.Mx
    }
    else {
        res <- AIC(model)
    }
    return(res)
}
##' .. content for \description{Calculate the smoothed AIC across a set of models} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title smoothedAIC
##' @param models A set of MxModel objects
##' @return a vector of weights
##' @author Richie Morrisroe
smoothedAIC <- function (models) {
    information <- lapply(models, smoothAIC)
    exp.info <- lapply(information, function(x) exp(-0.5*x))
    info <- Reduce(`+`, exp.info)
    weights <- vector(mode="numeric", length=length(information))
    for (i in 1:length(information)) {
        weights[i] <- exp(-0.5*information[[i]])/info
        ## browser()
    }
    return(weights)
}
        
##' .. content for \description{Average a set of IRT Factor Scores across Cross-validation Splits} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title irtAverageFactorScores
##' @param scores IRT scores 
##' @return the average across all splits
##' @author Richie Morrisroe
irtAverageFactorScores <- function (scores=list) {
    abilities <- sapply(scores, `[`, 1)
    ab.average <- Reduce(`+`, abilities)/length(abilities)
    names(ab.average) <- "AbilityEst"
    return(ab.average)
}
