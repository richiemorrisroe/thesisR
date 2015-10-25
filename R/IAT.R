##' {A function to calculate the IAT score (the D measure from Greenwald (2003)}
##'
##' {This function takes the response time data for each participant (one block of stimuli in each row, with Participant variable repeated and calculates the IAT score for each Pp using either a median or mean aggregation. It assumes many things about the structure of the input, and should not be used as-is (I will fix it at some point, however).} ..
##' @title calc_iat_scores
##' @param data - a dataframe containing the stimuli names and response times, with one row per block
##' @param Code the participant identifier code
##' @param method either use mean or median to calculate scores
##' @param words a vector of strings giving the names of the stimuli
##' @return a dataframe containing the mean response times for each of the critical blocks (3 and 5) and the IAT score calculated by the specified method
##' @author Richard Morrisroe
calc_iat_scores <- function(data, Code, method=c("mean", "median"), words) {
  if(nrow(data) %% 5 !=0) {
    warning("not all participants have complete responses")
##good old magic function arguments
  partlen <- with(data, tapply(Block, Code,length))
  droppart <- partlen[partlen != 5]
  drop <- which(data$Code == names(droppart))
  data <- data[- (drop),]
  }
  if(method == "mean") {
    func <- method[1]
  }
  else {
    func <- method[2]
  }

  data2 <- data[,c(Code, words)]
  block3 <- data2[data$Block == "Block 3",]
  block5 <- data2[data$Block == "Block 5",]
  block1 <- data2[data$Block == "Block 1",]
  block2 <- data2[data$Block == "Block 2",]
  block4 <- data2[data$Block == "Block 4",]
  stimblock3 <- block3[,words]
  stand.dev3 <- apply(stimblock3[,words], 1, sd, na.rm=TRUE)
  stimblock5 <- block5[,words]
  stand.dev5 <-  apply(stimblock5[,words], 1, sd, na.rm=TRUE)
  if(method=="mean") {
  b3score <- apply(stimblock3[,words], 1, mean, na.rm=TRUE)
  b5score <- apply(stimblock5[,words], 1,  mean, na.rm=TRUE)
}
  else {
    b3score <- apply(stimblock3[,words], 1, median, na.rm=TRUE)
  b5score <- apply(stimblock5[,words], 1,  median, na.rm=TRUE)
  }
  block3scores <- as.data.frame(cbind(b3score, stand.dev3))
  block5scores <- as.data.frame(cbind(b5score, stand.dev5))
  scores <-  cbind(block3[,Code],block3scores, block5scores)
  overallsd <-  (stand.dev3 + stand.dev5)/2
  diff <-  b5score - b3score
iatscore <-  diff / overallsd
  res <- data.frame(scores=scores,
                    IAT=iatscore,
                    Block3=stimblock3,
                    Block5=stimblock5
                  )
}
##' {Get the difference between two sets of IAT scores}
##' {As above}
##' @title iat_diff
##' @param x an IAT score
##' @param y an IAT score
##' @return A dataframe containing the differences between the two scores
##' @author Richie Morrisroe
iat_diff <- function(x, y) {
  res <- matrix(NA, ncol=length(x), nrow=nrow(x))
  for (i in 1:length(x)) {
    print(i)
    res[,i] <- mapply("-", x[,i], y[,i])
    res
}
  stimnames <- names(x)
  stimnames2 <- paste(stimnames, ".Diff", sep="")
  names(res) <- stimnames2
  res
}
