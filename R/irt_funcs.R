##' {Return an xtable object of an IRT easiness/difficulty parameters}
##' {Takes a GRM or GPCM object and returns a decidely non-standard table} 
##' @title irt_xtab
##' @param x an IRT model object of <some_bunch> of classes
##' @param ... arguments based to xtable function
##' @return an xtable representation of the difficulty and/or discrimination parameters
##' @author Richard Morrisroe
irt_xtab <- function (x, ...) {
    eta <- x$etapar #$
    se <- x$se.eta #$
    eta_mat <- as.matrix(eta)
    se_eta_mat <- as.matrix(se)
    eta_par_mat <- cbind(eta_mat, se_eta_mat)
    colnames(eta_par_mat) <- c("Ability Estimate", "Standard Error")
    coef_xtab <- xtable::xtable(eta_par_mat, ...)
    coef_xtab
}
##' {Wrapper around ggplot for a person-item difficulty plot}
##' {Not much, really}
##' @title ggplot_grm
##' @param grm an IRT GRM model object
##' @param ... other methods passed to plotting function
##' @return a ggplot object
##' @author Richard Morrisroe
ggplot_grm <- function (grm, ...) {
    stopifnot(class(grm) == "grm")
    x <- coef(grm)
    x <- as.matrix(x)
    x <- x[,-ncol(x)]
    xt <- t(x)
    response <- 1:nrow(xt)
    respind <- ncol(xt)+1
    xt <- as.data.frame(xt)
    xt$response <- response
    xtm <- reshape2::melt(xt, id="response")
    names(xtm) <- c("threshold", "item", "ability")
    plot1 <- ggplot2::ggplot(x.tm,
                             aes(x=ability,
                                 y=item,
                                 shape=as.factor(threshold),
                                 colour=as.factor(threshold)), ...)
    plot2 <- plot1 + ggplot2::geom_point() + ggplot2::geom_rug()
    plot2
}
##' {Convert a GPCM object to a matrix for turning into a table} 
##' {In some cases, the output of a gpcm will have a different number of threshold parameters for different items.
##' This function extracts the coefficients from an object of class gpcm, and solves this problem so that the coefficients can be coerced to a data.frame or matrix and the tables reported easily}
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
        mat_res <- matrix(NA, nrow=dimrows, ncol=dimcols)
        modlength <- lapply(gpcm, length)
        maxlength <- max(as.matrix(unlist(modlength)))
        for (i in 1:maxlength) {
            column <- lapply(gpcm, "[", i)
            column <- as.matrix(unlist(column))
            mat_res[1:length(column),i] <- column
            mat_res
        }
        rownames(mat_res) <- names(gpcm)
        probelemlength <- length(gpcm[[probelem]])
        ##this gives a scalar, as internally matrices are stored as vectors
        missingvalue <- which(is.na(mat.res))
        #get the element where is the discrimination parameter has ended up
        wrongvalue <- missingvalue - nrow(mat.res) 
        mat_res[missingvalue] <- mat_res[wrongvalue]
        mat_res[wrongvalue] <- NA

        categories <- lapply(gpcm, names)
        categorynames <- categories[[which.max(sapply(categories, length))]]
        colnames(mat_res) <- categorynames
        return(mat_res)
    }
    mat_res
}
##' Extract the predictions from an IRT fascore object
##'
##' {Selects useful information from an object inheriting from grm or gpcm}
##' @title get_irt_preds
##' @param x  an IRT model object
##' @return a dataframe containing observed scores, expected scores, the results of a z-test, and the se of the z-test
##' @author Richie Morrisroe
get_irt_preds <- function (x) {
    res <- x$score.dat[,c("Obs", "Exp", "z1","se.z1")]
    res
}
##' compare Z-scores for two IRT models
##' {Examine the differences in z-scores between two (and only two) IRT models}
##' @title compare_irt_scores
##' @param x an IRT model
##' @param y an IRT model
##' @return a list containing the (Pearson) correlations between the two z-scores, and the squared differences between the two sets of scores
##' @author Richie Morrisroe
compare_irt_scores <- function (x, y) {
    scores_x <- x$z1
    scores_y <- y$z1
    cor_xy <- cor(scores_x, scores_y,
                  method="pearson", use="pairwise.complete.obs")
    diff_xy <- (scores_x - scores_y) ^ 2
    res <- list(cor=cor_xy, differences=diff_xy)
    res
}
##' .. Unfinished function used to perform cross-validation over IRT models
##'
##' {See Description}
##' @title irt_cv
##' @param data a dataframe containing the data to be used
##' @param model the kind of model (either grm or gpcm)
##' @param constraint the constraint to use - see documentation for grm and gpcm objects
##' @param splits  the number of splits to use
##' @param .... 
##' @return a test and train set
##' @author Richie Morrisroe
irt_cv <- function (data,
                    model=c("grm", "gpcm"),
                    constraint = c(TRUE, FALSE, "rasch", "1PL", "gpcm"),
                    splits = 10, ...) {
    if(is.dataframe(data) ||is.matrix(data))
        stop("this function needs matrix or dataframe input")
    splittedsamples <- split_sample(data, splits)
    for (i in 1:length(splittedsamples)) {
        testset <- splittedsamples[i]
        trainset <- splittedsamples[!i]
        return(list(train=trainset, test=testset))
    }
}
##' Yet another unsuccessful IRT CV function (look at this one, there were good ideas in there)
##'
##' {See description}
##' @title irt_cross_validate
##' @param x 
##' @return a dataframe containing observed and expected scores
##' @author Richie Morrisroe
irt_cross_validate <- function(x) {
#get observed frequencies from display command in package ltm
    obs <- ltm::descript(x)$perc
    totscores <- grm::descript(x)$items
    totscores[totscores == 0] <- NA
    model <- ltm::grm(x)
    model.scores <- ltm::factor.scores(model, resp.patterns=x)
    abilities <- model.scores$score.dat["z1"]
    pointsweights <- model$GH
    cutpoints <- pointsweights[[1]]
    weights <- pointsweights[[2]]
    q <- seq(from=0, to=1, by=0.05) #create 21 points
    quadnorm <- qnorm(q) # map 21 points to the normal quantiles
    totscores2 <- rowSums(x, na.rm=TRUE)
    totscores2[totscores2 == 0] <- NA
    ab.scores <- as.matrix(cbind(totscores2, abilities))
    res <- list(obsscores=obs,
                totscores=totscores2,
                abscores=ab.scores,
                model=model,
                scores=model.scores,
                abilities=abilities,
                weights=weights)
}
##' Calculate  something, depending on some other stuff
##' {See description}
##' @title prob_calc
##' @param x 
##' @param totscores 
##' @return calculated probabilities
##' @author Richie Morrisroe
prob_calc <- function(x, totscores) {
    res <- sapply(x, calcprob, totscores)
}
##' I really wonder if I can salvage anything from these
##' {No really, why did I do all of this?}
##' @title calc_prob
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
                    ##wtf? why is the variable totscores being introduced?
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
##' {See description}
##' @title cond_prob_irt
##' @param x an IRT object
##' @return Something really cool
##' @author Richie Morrisroe
cond_prob_irt <- function(x) {
    abilities <- x[,1]
    totscores <- x[,2]
    s_ord <- order(x$totscores2)
    x_ord <- x[s_ord,]
    x_ord2 <- na.omit(x_ord)
    scores.len <- with(x_ord,tapply(z1, totscores2, length))
    unique_ab <- with(x_ord, xtabs(z1 ~ totscores2))
    unique_scores <- as.numeric(names(scores_len))
    probmat <- as.data.frame(matrix(NA, ncol=20, nrow=100))
    for(i in seq_along(unique.scores)) {
        cur_score <- x_ord2[x_ord2$totscores2 == unique_scores[i],]
        unique_ab <- with(cur_score,table(z1, totscores2))
        unique_sc <- with(x_ord2, table(totscores2))
        for(j in seq_along(unique_ab)) {
            p1 <- unique_ab[j]/length(unique(x_ord2$z1)) #ab prob
            p2 <- nrow(cur_score)/nrow(x_ord2) #score prob
            p3 <- p1 * p2
            p4 <- p3 / p2
            probmat[j,i] <- p4
        }
        probmat
    }
    scorenames <- paste("Score", unique_scores, sep="")
    names(probmat) <- scorenames
    probmat
}

##' {Maybe a repeat of the earlier functions}
##' {Some stuff} 
##' @title get_irt_estimates
##' @param fscores 
##' @return estimated abilities and their standard errors
##' @author Richie Morrisroe
get_irt_estimates <- function(fscores) {
    data <- fscores[["score.dat"]]
    abest <- data[,c("z1", "se.z1")]
    names(abest) <- c("ability_estimations", "stderr")
    return(abest)
}
##' {Actual implemented IRT test on new data function}
##' {Examines the difference in accuracy between a model estimated on the new data, versus the predictions from the old model on the new data} 
##' @title test_irt_models
##' @param oldmodel the original IRT model
##' @param newdata the new data
##' @param gpcmconstraint the constraint if the model is gpcm
##' @param grmconstraint the constraint if the model is grm
##' @param ... other arguments passed through
##' @return A dataframe containing two columns, ErrorApproximation and Correlation between models
##' @author Richie Morrisroe
test_irt_models <- function(oldmodel,
                          newdata,
                          gpcmconstraint=c("rasch", "1PL", "gpcm",),
                          grmconstraint= c(TRUE, FALSE), ...) {
    if(class(oldmodel)=="gpcm") {
        constraint <- gpcmconstraint
    }
    else {
        constraint <- grmconstraint
    }

    comp.para <- length(unique(as.vector(coef(oldmodel))))
    predscores <- get_irt_estimates(
        ltm::factor.scores(oldmodel, resp.patterns=newdata))
    if(class(oldmodel) == "gpcm") {
        newmodel <- ltm::gpcm(newdata, constraint=constraint)
    }
    else {
        newmodel <- ltm::grm(newdata, constrained=constraint)
    }
    newscores <- get_irt_estimates(
        ltm::factor.scores(newmodel, resp.patterns=newdata))
    diffscores <- mapply("-", predscores[,1], newscores[,1])
    rea <- sqrt(sum(diffscores ^ 2)) * log(comp.para)
    scorescor <- cor(predscores[,1], newscores[,1], ...)
    res <- data.frame(error_approximation=rea, correlation=scorescor)
    return(res)
}
##' {Extract fit functions from an OpenMx object} 
##' {See desc} 
##' @title get_mx_fit_functions
##' @param mx an mxFit object
##' @param label labels to apply to the fit functions returned
##' @return a dataframe containing the fit functions
##' @author Richie Morrisroe
get_mxfit_functions <- function(mx, label=NULL) {
    if(!require(OpenMx)) {
        cat("OpenMx is required for this function to operate\n")
    }
    summ <- summary(mx)
    bic <- summ$BIC.Mx
    aic <- summ$AIC.Mx
    obs <- summ$numObs
    param <- summ$estimatedParameters

    res <- data.frame(BIC=bic,
                      AIC=aic,
                      observations=obs,
                      estimated_parameters=param)
    if(!is.null(label)) {
        rownames(res) <- label
    }
    return(res)
}
##' {Average a set of IRT Models fit on different cross-validation splits} 
##' {Simple averaging of the coefficients}
##' @title irt_average
##' @param sols IRT Models
##' @return a dataframe containing the averaged coefficients
##' @author Richie Morrisroe
irt_average <- function(sols=list()) {
    coef <- lapply(sols, coef)
    res <- Reduce(`+`, x=coef)/length(coef)
    return(res)
}

##' {Claims to be a smoothed AIC function, but actually just extracts the AIC value from an MxRAMModel or MxModel}
##' {See above} 
##' @title smooth_AIC
##' @param model An MxModel object
##' @return the AIC of the object
##' @author Richie Morrisroe
smooth_aic <- function(model) {
    if(class(model) %in% c("MxRAMModel", "MxModel")) {
        res <- summary(model)$AIC.Mx
    }
    else {
        res <- AIC(model)
    }
    return(res)
}
##' {Calculate the smoothed AIC across a set of models}
##' {Actually does something, even if its wrong}
##' @title smoothed_AIC
##' @param models A set of MxModel objects
##' @return a vector of weights
##' @author Richie Morrisroe
smoothed_aic <- function (models) {
    information <- lapply(models, smooth_aic)
    exp_info <- lapply(information, function(x) exp(- 0.5 * x))
    info <- Reduce(`+`, exp_info)
    weights <- vector(mode="numeric", length=length(information))
    for (i in 1:length(information)) {
        weights[i] <- exp(-0.5 * information[[i]]) / info
    }
    return(weights)
}

##' {Average a set of IRT Factor Scores across Cross-validation Splits}
##' {Thought I did this above?}
##' @title irt_average_factor_scores
##' @param scores IRT scores 
##' @return the average across all splits
##' @author Richie Morrisroe
irt_average_factor_scores <- function (scores=list) {
    abilities <- sapply(scores, `[`, 1)
    ab_average <- Reduce(`+`, abilities) / length(abilities)
    names(ab_average) <- "ability_estimation"
    return(ab_average)
}
