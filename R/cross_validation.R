##' {Split the data into test and train sets. I'm reasonably certain that this is completely obsoleted by the createDataPartition function in the caret package}
##' {See description - this does not do any kind of balancing to equalise class probabilites or response distribution across splits} ..
##' @title train_test_sets
##' @param x the variable to split on
##' @param data the data to split
##' @return Some kind of list?
##' @author Richie Morrisroe
train_test_sets <- function (x, data) {
    testlist <- list()
    trainlist <- list()
    for (i in 1:length(x)) {
        split <- x[[i]]
        fold.train <- data[split,]
        fold.test <- data[-eval(split),]
        trainlist[[i]] <- fold.train
        testlist[[i]] <- fold.test
    }
    traintestlist <- c(trainlist, testlist)

    train.names <- paste(names(x), ".Train",sep="")
    test.names <- paste(names(x), ".Test",sep="")
    listnames <- c(train.names, test.names)
    names(traintestlist) <- listnames
    traintestlist
}
##' {Takes the result of TestTrainSplit and actually splits it}
##' {See DESC}
##' @title seperate_test_and_train
##' @param data a list of dataframes with sampled data
##' @param test return test if true, else train
##' @return a dataframe
##' @author Richie Morrisroe
seperate_test_and_train <- function(data, test=TRUE) {
    if(test) {
        indtest <- grep("Test$", names(data))
        res <- data[indtest]
    }
    else {
        indtrain <- grep("Train$", names(data))
        res <- data[indtrain]
    }
    res
}
##' {This was a wrapper around train to test the theory that the RF was actually predicting placebo perfectly}
##' {See desc} 
##' @title train_folds
##' @param data a dataframe 
##' @param Form the formula to fit the model with
##' @param control control parameters for train
##' @param sizes something something
##' @param metric otherthing otherthing
##' @param updown who knows?
##' @return model fits from the cv process
##' @author Richie Morrisroe
train_folds <- function(data, Form, control, sizes, metric, updown) {
    cvresults <- list()
    for (i in 1:length(data)) {
        #again, another bug that could be caused by lexical scoping
        res <- caret::train(form=Form, data=data,
                            na.action="na.omit",
                            size=sizes,
                            metric=metric,
                            maximise=updown,
                            control=rfeControl)
        cvresults[[i]] <- res
    }
    names(cvresults) <- names(data)
    cvresults
}

##' {Yet another split sample function}
##' @name package-thesisR
##' @docType package
##' @title split_sample
##' @param x the data
##' @param split the number of splits to make
##' @return a list containing the splits
##' @author Richie Morrisroe
split_sample <- function(x, split) {
    xlen <- nrow(x)
    indices <- sample(1:xlen, xlen, replace=FALSE)
    splitlen <- xlen/split
    splits <- cut(indices, split, labels=FALSE)
    samplist <- list()
    for(i in 1:max(split)) {
        samp <- x[splits == i,]
        samplist[[i]] <- samp
    }
    samplist
}
##' {Something}
##'
##' {more things} 
##' @title create_combinations
##' @param splits a list containing splits from split_sample
##' @return a list containing the splits into test and train
##' @author Richard Morrisroe
create_combinations <-  function(splits) {
    stopifnot(class(splits) == "list")
    splitnumbers <- length(splits)
    facsplits <- choose(splitnumbers, k = (splitnumbers - 1))
    reslist <- list()
    for (i in 1:facsplits) {
        samples <- sample(
            1:splitnumbers,
            size=1,
            replace=FALSE)
        train <- do.call("rbind", splits[-c(samples)])
        test <- as.data.frame(splits[c(samples)])
        reslist[[i]] <- list(train=train, test=test)
    }
    reslist
}
##' {A function for performing repeated Cross-validation}
##' {See description}
##' @title repeat_cv
##' @param form a formula describing the model
##' @param data the data to fit on
##' @param method not implemented
##' @param n number of repeats
##' @param responsevariable dependent variable
##' @param ... arguments passed to the train function
##' @return a nested list containing the confusionMatrix for the model on test data, and the accuracy parameter
##' @author Richie Morrisroe
repeat_cv <- function(form, data, method=method, n, responsevariable, ...) {
    res <- vector(length=n, mode="list")
    Accuracy <- vector(length=n, mode="numeric")
    data2 <- na.omit(data)
    variable <- grep(responsevariable, x=names(data))
    for (i in 1:n) {
        print(i)
        trainind <- with(data2,
                         caret::createDataPartition(
                             data[,variable], p=0.8, list=FALSE))
        trainset <- data2[trainind,]
        testset <- data2[-trainind,]
        train.res <- caret::train(formula=form, data=trainset, ...)

        train.pred <- predict(train.res, testset)
        res[[i]] <- caret::confusionMatrix(train.pred, testset[,responsevariable])
        Accuracy[i] <- res[[i]]$overall[1]
    }
##this was a wierd one, lintr complains if last action in function is assigment
    res2 <- list(res, Accuracy)
    res2
}
##' {calculate the root mean square error of approximattion for a dataframe containing columns named pred and obs} 
##' {See Desc} 
##' @title rmsea
##' @param data a dataframe containing pred and obs columns
##' @param pred_col prediction column
##' @param obs_col observation column
##' @return a scalar number for the RMSEA
##' @author Richie Morrisroe
rmsea <- function(data, pred_col=pred, obs_col=obs) {
    erro <- with(data, pred_col - obs_col)
    err_sq <- erro ^ 2
    root_err <- sqrt(mean(err_sq))
    return(root_err)
}
