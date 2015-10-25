##' @title recode_many
##' @param data the dataframe containing the items to be recoded
##' @param vars the variables to recode
##' @param Recodings A list of some recodings - need to look at how function is used to understand what the hell this is
##' @return a transformed dataframe with the new recodings
##' @author Richie Morrisroe
##' @importFrom car recode
recode_many <- function (data, vars, Recodings){
  varlist <- list(vars)
dataret <- data
  for (i in 1:length(vars)) {
    dataret[,i] <- car::recode(data[,i], recodes=Recodings)
  }
return(dataret)
}
##' creates the RAND MOS sum scores
##'
##' This is unlikely to ever be useful to anyone bar me or potential replicators of my thesis
##' @title create_sum_scores
##' @param data a dataframe containing the RAND MOS data
##' @return a dataframe containing the sum scores for each variable
##' @author Richie Morrisroe
create_sum_scores <- function(data) {
    data2 <- data
    data2$physfun <- rowMeans(
        data[,
             grep("RANDQ[3456789]$|RANDQ[1][012]$",x=names(data))],
        na.rm=TRUE)
    data2$rolelim <- rowMeans(
        data[,grep(
            "RANDQ[1][3456]$",
            x=names(data))],
        na.rm=TRUE)
    data2$rolelimem <-
        rowMeans(
            data[,
                 grep("RANDQ[1][789]$",
                      x=names(data))],
            na.rm=TRUE)
    data2$energyfat <- rowMeans(
        data[,
             grep(
                 "RANDQ[2][379]$|RANDQ31$",
                 x=names(data))],
        na.rm=TRUE)
    data2$emwellbeing <- rowMeans(
        data[,
             grep(
                 "RANDQ[2][4568]$|RANDQ30$",
                 x=names(data))],
        na.rm=TRUE)
    data2$socialfunctioning <- rowMeans(
        data[,
             grep(
                 "RANDQ20|RANDQ32",
                 x=names(data))],
        na.rm=TRUE)
    data2$pain <- rowMeans(
        data[,
             grep("RANDQ[2][12]$",
                   x=names(data))],
        na.rm=TRUE)
    data2$generalhealth <- rowMeans(
        data[,
             grep("RANDQ1$|RANDQ[3][3456]$",
                  x=names(data))],
        na.rm=TRUE)
    data2$mindfulness <- rowMeans(
        data[,
             grep("MAAS",
                  x=names(data))],
        na.rm=TRUE)
    data2$optimism <- rowMeans(
        data[,
             grep("LOTR",
                  x=names(data))],
        na.rm=TRUE)
return(data2)
}
