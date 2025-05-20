

#' @param i Row indices for subsetting. If only `i` is provided, it will return the entire row(s).
#' @param j Column indices for subsetting.
#' @param drop Not used.
#' @param ... Additional arguments.
#' @rdname TableContainer-method
#' @return `[`: A new TableContainer object with the selected data.
#' @export
setMethod("[", signature(x = "TableContainer"), 
          function(x, i, j, ..., drop = TRUE) {
    call <- match.call()

    i_is_provided <- "i" %in% names(call)
    j_is_provided <- "j" %in% names(call)


    if (!i_is_provided && !j_is_provided) {
        return(x)
    }

    if (!i_is_provided){
        i <- seq_len(nrow(x))
    }
    if (!j_is_provided) {
        j <- seq_len(ncol(x))
    }

    
    # Create a new TableContainer object with the selected data
    mat <- tblData(x)[i,j, drop = FALSE]
    rd <- rowData(x)[i, , drop = FALSE]
    cd <- colData(x)[j, , drop = FALSE]
    md <- metaData(x)

    .TableContainer(
        table = mat,
        rowData = rd,
        colData = cd,
        metaData = md
    )
})

