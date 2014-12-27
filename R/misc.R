showTree2 <-  function(e, write=cat) {
    if(!require(codetools)) {
        stop("Codetools must be installed")
    }
    quoted <- quote(e)
    showTree(quoted, write=write)
}
    
