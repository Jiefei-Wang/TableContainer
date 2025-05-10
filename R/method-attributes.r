
for (x in c('dim', 'dimnames', 'nrow', 'ncol', 'colnames', 'rownames', 'colnames<-', 'rownames<-'))
    setGeneric(x)
rm(x)


#' Container Methods
#' 
#' @param x A TableContainer object.
#' 
#' @return 
#' `dim`, `dimnames`, `nrow`, and `ncol`: the respective dimensions, dimnames, number of rows, and number of columns of the TableContainer object. When the table slot is NULL, the dimensions are derived from the rowData and colData slots.
#' @rdname TableContainer-method
#' @export
setMethod("dim", "TableContainer", function(x) {
    mat <- tblData(x)
    if (!is.null(mat)) {
        dim(mat)
    } else {
        rd <- rowData(x)
        cd <- colData(x)
        nr <- if (!is.null(rd)) nrow(rd) else 0L
        nc <- if (!is.null(cd)) nrow(cd) else 0L
        c(nr, nc)
    }
})

#' @rdname TableContainer-method
#' @export
setMethod("dimnames", "TableContainer", function(x) {
    mat <- tblData(x)
    if (!is.null(mat)) {
        dimnames(mat)
    } else {
        rn <- rownames(rowData(x))
        cn <- rownames(colData(x))
        list(rn, cn)
    }
})

#' @rdname TableContainer-method
#' @export
setMethod("nrow", "TableContainer", function(x) {
    dim_data <- dim(x)
    dim_data[1]
})

#' @rdname TableContainer-method
#' @export
setMethod("ncol", "TableContainer", function(x) {
    dim_data <- dim(x)
    dim_data[2]
})

#' @rdname TableContainer-method
#' @export
setMethod("rownames", "TableContainer", function(x) {
    mat <- tblData(x)
    rownames(mat)
})

#' @rdname TableContainer-method
#' @export
setMethod("rownames<-", "TableContainer", function(x, value) {
    mat <- tblData(x)
    rownames(mat) <- value
    tblData(x) <- mat
    x
})

#' @rdname TableContainer-method
#' @export
setMethod("colnames", "TableContainer", function(x) {
    mat <- tblData(x)
    colnames(mat)
})

#' @rdname TableContainer-method
#' @export
setMethod("colnames<-", "TableContainer", function(x, value) {
    mat <- tblData(x)
    colnames(mat) <- value
    tblData(x) <- mat
    x
})