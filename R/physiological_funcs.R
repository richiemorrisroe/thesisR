##' {Import all physiological data files}
##'
##' {As above}
##' @title file_import
##' @param directory a directory in which to look for files
##' @param pattern a regualar expression matching a set of files
##' @return A list of files which match the pattern
##' @author Richie Morrisroe
file_import <- function(directory, pattern) {
  files <- list.files(directory, pattern=pattern, full.names=TRUE)
  file_list <- lapply(files, read.table, header=FALSE)
  files2 <- gsub(".*-.*-([0-9][0-9][0-9][0-9][0-9]?).txt", "\\1", x=files)
  names(file_list) <- files2
  file_list
  ## file.df <- do.call(cbind, file.list)
}
##' {Convert a list (of physiological data files) to a dataframe}
##' {Some content}
##' @title list_to_df
##' @param data the datafile
##' @param ind indices to examine
##' @return a dataframe of required data
##' @author Richie Morrisroe
list_to_df <- function(data, ind) {
  dnames <- names(data)
  rows <- sapply(data, nrow)
  maxrows <- max(rows)
  cols <- length(data)
  res <- matrix(NA, nrow=maxrows, ncol=cols)
for (i in seq(from=1, to=length(data))) {
  temp <- data[[i]]
  res[1:nrow(temp),i] <- temp[,ind]
}
  if(ind == 1) {
      colnames(res) <- paste("GSR", dnames, sep="")
  }
  if(ind == 2) {
    colnames(res) <- paste("ECG", dnames, sep="")
  }
  res
}
##' {read in files and write out each column to a seperate file} 
##'
##' {This is hacked together to suit my use cases for physiological files, and is probably not useful to anyone}
##' @title lazy_load
##' @param files the files to read in
##' @param names the pattern of files to read
##' @param cols in which columns are needed to be wrote out again
##' @return NULL (called for side effects)
##' @author Richie Morrisroe
lazy_load <- function (files, names, cols) {
  filetype <- paste(names, "-", sep="")
  outfilenames <- gsub("Richi[e]?-",filetype, x=files)
  outfilenames2 <- gsub(".*/Richieoutput/", "", x=outfilenames)
  for (i in 1:length(files)) {
    temp <- read.table(files[i])
    browser()
    gsr <- temp[,cols]
    write.table(gsr, file=outfilenames2[i])
  }
}
##' {Take a list of files, read them in and calculate the number of lines in each of them} 
##' {This might be useful for someone else, but there are some hardcoded numbers in here that will trip you up}
##' @title lazy_length
##' @param files a list of files
##' @return a matrix containing a file identifier and its length
##' @author Richie Morrisroe
lazy_length <- function(files) {
    tp <- gsub(".*/", "", x=files)
    tp.split <- strsplit(as.character(tp), "-")
    pp <- lapply(tp.split, "[", 3)
    pp <- gsub(".txt", "", x=pp)
    lengthmat <- matrix(NA, 114, ncol=2)
    for (i in 1:length(files)) {
        temp <- read.table(files[i])
        len <- dim(temp)[1]
        lengthmat[i,1] <- pp[i]
        lengthmat[i,2] <- len
        rm(temp); gc()
    }
    lengthmat
}


##' {Internal thesis function to get the participant number for use with lazylength} 
##' {Again, not massively useful for anyone else}
##' @title get_participant_number
##' @param files a series of files which contain a participant number 
##' @return the identifier for each file
##' @author Richie Morrisroe
get_participant_number <- function(files) {
    tp <- gsub(".*/", "", x=files)
    tp.split <- strsplit(as.character(tp), "-")
    pp <- lapply(tp.split, "[", 3)
    pp <- gsub(".txt", "", x=pp)
    pp
}
##' {A better implementation of lazylength} 
##' {I love reimplementing things!}
##' @title lazy_length2
##' @param files a vector of files
##' @return a file identifier and the nrow() of each file 
##' @author Richie Morrisroe
lazy_length2 <- function(files) {
    pp <- get_participant_number(files)
    lengthmat <- matrix(NA, 114, ncol=2)
    for (i in 1:length(files)) {
        temp <- read.table(files[i])
        len <- dim(temp)[1]
        lengthmat[i,1] <- pp[i]
        lengthmat[i,2] <- len
        rm(temp); gc()
    }
    lengthmat
}
##' {Calculate the overall mean by file for each file} 
##'
##' {Take a list of files and calculate the mean of each file}
##' @title lazy_mean
##' @param path path in which files can be found
##' @param pattern a regular expression matching the files
##' @param ... other arguments passed through to list files
##' @return a matrix containing a mean response for each file in the specified column
##' @author Richie Morrisroe
lazy_mean <- function( path, pattern, ...) {
    lsfiles <- list.files(path, pattern, ...)
    pp <- get_participant_number(lsfiles)
    meanmat <- matrix(NA, length(lsfiles), ncol=2)
    for(i in 1:length(lsfiles)) {
        temp <- read.table(lsfiles[i])
        mu <- mean(temp[,1])
        meanmat[i,] <- c(pp[i], mu)
}
    meanmat
}
##' {roll up a series of large files into simpler aggregates} 
##'
##' {Now this could actually be useful. Take a set of files, calculate some summary statistics over particular windows and return a new, smaller file consisting of the aggregated results} 
##' @title lazy_downsample
##' @param path a path where files can be found
##' @param pattern a pattern to match each file
##' @param aggregate the number of rows to aggregate
##' @param FUN the function to aggregate by
##' @param ... other arguments passed to the aggregation function
##' @return a dataframe containing the aggregated data
##' @author Richie Morrisroe
lazy_downsample <- function(path, pattern, aggregate=1000, FUN=mean, ...) {
  stopifnot(is.numeric(aggregate))
    lsfiles <- list.files(path, pattern, full.names=TRUE)
    pp <- get_participant_number(lsfiles)
    mymat <- matrix(NA, 3200, ncol=length(lsfiles))
    for(i in 1:length(lsfiles)) {

        temp <- read.table(lsfiles[i])
        dim <- dim(temp)[1]
        dimsec <- ceiling(dim/aggregate)
        myrep <- sort(rep(1:dimsec, length.out=dim))
                temp[,"myrep"] <- as.factor(myrep)
        ds <- as.data.frame(with(temp, tapply(x, myrep, FUN, ...)))
        print(i)
        dimds <- dim(ds)[1]
        mymat[1:dimds,i] <- ds[,1]
    }
    colnames(mymat) <- pp
    mymat
}
        
##' {Calculate the range of a set of data} 
##' {get range of a set of data, useful for HRV peak analysis}
##' @title diff_func
##' @param data a dataframe containing one variable
##' @return the difference between the maximum and the minimum values
##' @author Richie Morrisroe
diff_func <- function(data) {
  max <- which.max(data)
  min <- which.min(data)
  time <- abs(max-min)
  return(time)
}
##' {A function to interpolate lower sample data to match higher sample data - locf interpolation} 
##' {This has a truly terrible name, and some godawful hacks that make it probable that it contains bugs}
##' @title interpolate_pain
##' @param pain the data to interpolate
##' @param padding the offsets at which to start the interpolation
##' @return A dataframe containing the interpolated data
##' @author Richie Morrisroe
interpolate_pain <- function(pain, padding) {
    max.padding <- with(padding, max(FirstPainRating, na.rm=TRUE))
    pain.sec <- 45*60 #hack, as the experiment was 45 mins max following pain induction
    max_len <- pain_sec+max_padding+1 #for participant column
    row_nums <- with(pain, length(unique(Participant)))
    res_mat <- matrix(NA, nrow=row_nums, ncol=max_len)
    pain_merge <- merge(pain, padding, by.y="PPNo.", by.x="Participant")
    partno <- with(padding, PPNo.)
    part_pain_sec <- apply(pain[,with(pain,
                                      grep("^X", x=names(pain)))],
                           c(1,2), function (x) rep(x, times=60))
    for(i in seq_along(partno)) {
        print(partno[i])
        nowpart <- partno[i]
        len.part <- padding[with(padding, PPNo.==nowpart),]
        start.time <- len.part[,3]
        if(is.na(start.time)) {
            next
        }
        mypadding <- vector(mode="numeric", length=start.time)
        full_dat <- c(mypadding, part_pain_sec[,i,])
        res_mat[i,1:length(full_dat) + 1] <- full_dat
        res_mat
    }
        res_mat
    
    }
##' {Another interpolation function TODO: figure out which one of these is actually used} 
##' {Yeah, I have two different interpolation functions, both probably incorrect in different ways}
##' @title interpolate2
##' @param painscores a dataframe containing pain ratings
##' @param painmetadata a dataframe containing meta-data (when squeezing stop, when pain was applied, etc)
##' @return interpolations
##' @author Richie Morrisroe
interpolate2 <- function(painscores, painmetadata) {
    pain_ratings_min <- with(painmetadata, floor((SqueezStop+60)/60))
    pain_ratings_min <- as.data.frame(pain_ratings_min)
    pain_ratings_min[,"Participant"] <- painmetadata$PPNo.
    names(pain_ratings_min)[1] <- "padding"
    resmat <- matrix(0, nrow=nrow(pain_ratings_min), ncol=45+with(pain_ratings_min, max(padding, na_rm=TRUE)))
    partno <- pain_ratings_min$Participant
    painscores_real <- grep("^X", x=names(painscores))
    for (i in seq_along(partno)) {
        print(i)
        partpad <- pain_ratings_min[with(pain_ratings_min,Participant==partno[i]),1]
        if(is_na(partpad)) {
            next}
        
        painratings <- as.numeric(painscores[with(painscores, Participant==partno[i]),painscores_real])
        if(i== 27 ) {
            ##something weird happens, obviously:)
        }
        padding <- rep(0, times=partpad)
        padpluspain <- c(padding, painratings)
        resmat[i,1:length(padpluspain)] <- padpluspain
        resmat
    }
    resmat
}
