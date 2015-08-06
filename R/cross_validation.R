##' {Split the data into test and train sets. I'm reasonably certain that this is completely obsoleted by the createDataPartition function in the caret package}
##' {See description - this does not do any kind of balancing to equalise class probabilites or response distribution across splits} ..
##' @title TrainTestSets
##' @param x the variable to split on
##' @param data the data to split
##' @return Some kind of list?
##' @author Richie Morrisroe
TrainTestSets <- function (x, data) {
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
##' @title SeperateTestandTrain
##' @param data 
##' @param test 
##' @return a dataframe
##' @author Richie Morrisroe
SeperateTestandTrain <- function(data, test=TRUE) {
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
##' @title Trainfolds
##' @param data 
##' @param Form 
##' @param control 
##' @param sizes 
##' @param metric 
##' @param updown 
##' @return models fits from the cv process
##' @author Richie Morrisroe
Trainfolds <- function(data, Form, control, sizes, metric, updown) {
    cvresults <- list()
    for (i in 1:length(data)) {
        res <- train(form=Form, data=data, na.action="na.omit", size=sizes, metric=metric, maximise=updown, control=rfeControl)
        cvresults[[i]] <- res
    }
    names(cvresults) <- names(data)
    cvresults
}

##' {Yet another split sample function}
##' @name package-thesisR
##' @docType package
##' @title some_other_title
##' @param x the data
##' @param split the number of splits to make
##' @return a list containing the splits
##' @author Richie Morrisroe
splitSample <- function(x, split) {
    xlen <- nrow(x)
    indices <- sample(1:xlen, xlen, replace=FALSE)
    splitlen <- xlen/split
    splits <- cut(indices, split, labels=FALSE)
    samplist <- list()
    for(i in 1:max(split)) {
        samp<-x[splits==i,]
        assign(paste("samp", i, sep=""), value=samp, 1)
        samplist[[i]] <- get(paste("samp",i, sep=""))
        ## names(samplist[i]) <- paste("split", i, sep="")
    }
    samplist
}
##' {A function for performing repeated Cross-validation} (no empty lines) ..
##'  \details{See description}
##' @title repeatCV
##' @param form a formula describing the model
##' @param data the data to fit on
##' @param method not implemented
##' @param n number of repeats
##' @param responsevariable dependent variable
##' @param ... arguments passed to the train function
##' @return a nested list containing the confusionMatrix for the model on test data, and the accuracy parameter
##' @author Richie Morrisroe
repeatCV <- function(form, data, method=method, n, responsevariable, ...) {
    res <- vector(length=n, mode="list")
    Accuracy <- vector(length=n, mode="numeric")
    data2 <- na.omit(iatandexpfull)
    variable <- grep(responsevariable, x=names(data))
    for (i in 1:n) {
        print(i)
        trainind <- with(data2, createDataPartition(data[,variable], p=0.8, list=FALSE))
        trainset <- data2[trainind,]
        testset <- data2[-trainind,]
        train.res <- train(formula=form, data=trainset, ...)

        train.pred <- predict(train.res, testset)
        res[[i]] <- confusionMatrix(train.pred, testset[,responsevariable])
        Accuracy[i] <- res[[i]]$overall[1]
    }

    res2 <- list(res, Accuracy)
}
##' {calculate the root mean square error of approximattion for a dataframe containing columns named pred and obs} 
##' \details{See Desc} 
##' @title rmsea
##' @param data a dataframe containing pred and obs columns
##' @return a scalar number for the RMSEA
##' @author Richie Morrisroe
rmsea <- function(data) {
    erro <- with(data, pred-obs)
    err.sq <- erro^2
    root.err <- sqrt(mean(err.sq))
    return(root.err)
}
