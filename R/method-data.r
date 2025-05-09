
#' @param object A TableContainer object.
#' @param value A matrix, data.frame, or NULL.
#' @rdname TableContainer-method
#' 
#' @return 
#' `tblData`, `rowData`, `colData`, and `metadata`: the respective slots of the TableContainer object. 
#' 
#' `tblData<-`, `rowData<-`, `colData<-`, and `metadata<-`: update the respective slots and return the modified object.
#' @export
tblData <- function(object) {
    object@table
}

#' @rdname TableContainer-method
#' @export
`tblData<-` <- function(object, value) {
    object@table <- value
    validObject(object)
    object
}

#' @rdname TableContainer-method
#' @export
rowData <- function(object) {
    object@rowData
}

#' @rdname TableContainer-method
#' @export
`rowData<-` <- function(object, value) {
    object@rowData <- value
    validObject(object)
    object
}

#' @rdname TableContainer-method
#' @export
colData <- function(object) {
    object@colData
}

#' @rdname TableContainer-method
#' @export
`colData<-` <- function(object, value) {
    object@colData <- value
    validObject(object)
    object
}

#' @rdname TableContainer-method
#' @export
metadata <- function(object) {
    object@metadata
}

#' @rdname TableContainer-method
#' @export
`metadata<-` <- function(object, value) {
    object@metadata <- value
    validObject(object)
    object
}