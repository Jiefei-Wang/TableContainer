# Update union type for matrix-like objects
setClassUnion("tableAlike", c("matrix", "data.frame", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))



#' The TableContainer Class
#'
#' A container for a matrix and associated row/column annotations. This is
#' similar in concept to
#' Bioconductor's SummarizedExperiment but with no Bioconductor dependencies.
#'
#' @slot table A matrix, data.frame, or NULL.
#' @slot rowData A data.frame or NULL object describing the rows.
#' Each row of the matrix corresponds to a row in the rowData object.
#' @slot colData A data.frame or NULL object describing the columns.
#' Each column of the matrix corresponds to a row in the colData object.
#' @slot metadata A list or NULL containing arbitrary metadata associated 
#' with the overall data.
#'
#' @exportClass TableContainer
.TableContainer <- setClass("TableContainer",
    slots = list(
        table = "tableAlike",      # The single matrix-like object
        rowData = "data.frameOrNULL", # Row annotations
        colData = "data.frameOrNULL", # Column annotations
        metadata = "listOrNULL"       # Experiment metadata
    )
)


# Update the validity method
setValidity("TableContainer", function(object) {
    msg <- NULL
    tbl <- tblData(object)
    rd <- rowData(object)
    cd <- colData(object)

    # Get expected dimensions, preferring matrix-like object if present
    expected_nr <- NULL
    expected_nc <- NULL
    mat_rownames <- NULL
    mat_colnames <- NULL

    dim_data <- dim(object)
    expected_nr <- dim_data[1]
    expected_nc <- dim_data[2]

    # Check rowData consistency
    if (!is.null(rd)) {
        if (nrow(rd) != expected_nr) {
            msg <- c(msg, paste("Number of rows in 'rowData' (", nrow(rd),
                                ") must match number of rows in 'table' (", expected_nr, ")", sep=""))
        }
    }

    # Check colData consistency
    if (!is.null(cd)) {
        if (nrow(cd) != expected_nc) {
            msg <- c(msg, paste("Number of rows in 'colData' (", nrow(cd),
                                ") must match number of columns in 'table' (", expected_nc, ")", sep=""))
        }
    }

    if (is.null(msg)) TRUE else msg
})

#' The constructor function for TableContainer
#' 
#' Creates a TableContainer object with the specified matrix, rowData,
#' colData, and metadata.
#'  
#' @param table A matrix, data.frame, or NULL.
#' @param rowData A data.frame or NULL object describing the rows.
#' @param colData A data.frame or NULL object describing the columns.
#' @param metadata A list or NULL containing arbitrary metadata associated
#' with the overall data.
#' 
#' @examples 
#' tbl <- matrix(1:12, nrow = 3, ncol = 4)
#' row_dt <- data.frame(row1 = 1:3, row2 = letters[1:3])
#' col_dt <- data.frame(col1 = 1:4, col2 = letters[1:4])
#' metadata <- list(meta1 = "meta1", meta2 = "meta2")
#' 
#' TableContainer(
#'    table = tbl,
#'    rowData = row_dt,
#'    colData = col_dt,
#'    metadata = metadata
#'  )
#' 
#' @return A TableContainer object.
#' @export
TableContainer <- function(table = NULL, rowData = NULL, colData = NULL, metadata = list()) {

    if (!is.null(table) && !is(table, "tableAlike")) {
        stop("'matrix' must be a matrix, data.frame, or NULL")
    }

    # Use matrix-like object to define dimensions and dimnames if annotations are NULL
    if (!is.null(table)) {
        ref_dim <- dim(table)
        ref_rownames <- rownames(table)
        ref_colnames <- colnames(table)

        if (!is.null(rowData)) {
            if (!is(rowData, "data.frameOrNULL")) stop("'rowData' must be a data.frame or NULL")
        }

        if (!is.null(colData)) {
            if (!is(rowData, "data.frameOrNULL")) stop("'colData' must be a data.frame or NULL")
        }
    }

    if (!is.list(metadata)) {
        stop("'metadata' must be a list")
    }

    if (is.null(metadata)) metadata <- list()

    .TableContainer(
        table = table,
        rowData = rowData,
        colData = colData,
        metadata = metadata)
}

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