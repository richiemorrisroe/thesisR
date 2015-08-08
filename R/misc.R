##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title show_tree2
##' @param e an R expression
##' @param write  where to send the results
##' @return a quoted function object
##' @author Richard Morrisroe
show_tree2 <-  function(e, write=cat) {
    if(!require(codetools)) {
        stop("Codetools must be installed to use this function")
    }
    quoted <- quote(e)
    showTree(quoted, write=write)
}
