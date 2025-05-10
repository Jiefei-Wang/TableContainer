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
#' @slot metaData A list or NULL containing arbitrary metadata associated
#' with the overall data.
#'
#' @exportClass TableContainer
.TableContainer <- setClass("TableContainer",
    slots = list(
        table = "tableAlike", # The single matrix-like object
        rowData = "data.frameOrNULL", # Row annotations
        colData = "data.frameOrNULL", # Column annotations
        metaData = "listOrNULL" # Experiment metadata
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
        if (nrow(rd) != expected_nr && ncol(rd) != 0 && nrow(rd) != 0) {
            msg <- c(msg, glue("Number of rows in 'rowData' ({nrow(rd)}) must match number of rows in 'table' ({expected_nr})"))
        }
    }

    # Check colData consistency
    if (!is.null(cd)) {
        if (nrow(cd) != expected_nc && ncol(cd) != 0 && nrow(cd) != 0) {
            msg <- c(msg, glue(
                "Number of rows in 'colData' ({ncol(cd)}) must match number of columns in 'table' ({expected_nc})"
            ))
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
#' @param metaData A list or NULL containing arbitrary metadata associated
#' with the overall data.
#'
#' @examples
#' tbl <- matrix(1:12, nrow = 3, ncol = 4)
#' row_dt <- data.frame(row1 = 1:3, row2 = letters[1:3])
#' col_dt <- data.frame(col1 = 1:4, col2 = letters[1:4])
#' meta_dt <- list(meta1 = "meta1", meta2 = "meta2")
#'
#' TableContainer(
#'     table = tbl,
#'     rowData = row_dt,
#'     colData = col_dt,
#'     metaData = meta_dt
#' )
#'
#' @return A TableContainer object.
#' @export
TableContainer <- function(table = NULL, rowData = NULL, colData = NULL, metaData = NULL) {
    if (!is.null(table) && !is(table, "tableAlike")) {
        stop("'matrix' must be a matrix, data.frame, or NULL")
    }
    
    if (!is(rowData, "data.frameOrNULL")) {
        stop("'rowData' must be a data.frame or NULL")
    }

    if (!is(colData, "data.frameOrNULL")) {
        stop("'colData' must be a data.frame or NULL")
    }

    # Use matrix-like object to define dimensions and dimnames if annotations are NULL
    if (!is.null(table)) {
        ref_dim <- dim(table)
        ref_rownames <- rownames(table)
        ref_colnames <- colnames(table)
    }

    if (!is.list(metaData) && !is.null(metaData)) {
        stop("'metadata' must be a list or NULL")
    }


    .TableContainer(
        table = table,
        rowData = rowData,
        colData = colData,
        metaData = metaData
    )
}
