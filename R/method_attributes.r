
for (x in c('nrow', 'ncol', 'colnames', 'rownames', 'colnames<-', 'rownames<-'))
    setGeneric(x)
rm(x)

#' @description `nrow`, `ncol`, `colnames`, `rownames`, `names`: Getting the data properties,
#'  similar to base R functions.
#' @rdname Epoch-method
#' @return nrow: Number of rows in the data
#' @export
setMethod("nrow", "Epoch", function(x) {
    nrow(x@data)
})

#' @rdname Epoch-method
#' @return ncol: Number of columns in the data
#' @export
setMethod("ncol", "Epoch", function(x) {
    ncol(x@data)
})

#' @rdname Epoch-method
#' @return colnames: electrode names of the data
#' @export
setMethod("colnames", "Epoch", function(x) {
    x$times
})

#' @rdname Epoch-method
#' @export
setMethod("colnames<-", "Epoch", function(x, value) {
    x$times <- value
    x
})

#' @rdname Epoch-method
#' @return rownames: time points of the data
#' @export
setMethod("rownames", "Epoch", function(x) {
    x$electrodes
})

#' @rdname Epoch-method
#' @export
setMethod("rownames<-", "Epoch", function(x, value) {
    x$electrodes <- value
    x
})

