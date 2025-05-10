#' @param object A TableContainer object.
#' @param value A matrix, data.frame, or NULL.
#' @rdname TableContainer-method
#' 
#' @return 
#' `tblData`, `rowData`, `colData`, and `metadata`: the respective slots of the TableContainer object. 
#' 
#' `tblData<-`, `rowData<-`, `colData<-`, and `metadata<-`: update the respective slots and return the modified object.
#' @export
setGeneric("tblData", function(object) standardGeneric("tblData"))

#' @rdname TableContainer-method
#' @export
setGeneric("tblData<-", function(object, value) standardGeneric("tblData<-"))

#' @rdname TableContainer-method
#' @export
setGeneric("rowData", function(object) standardGeneric("rowData"))

#' @rdname TableContainer-method
#' @export
setGeneric("rowData<-", function(object, value) standardGeneric("rowData<-"))

#' @rdname TableContainer-method
#' @export
setGeneric("colData", function(object) standardGeneric("colData"))

#' @rdname TableContainer-method
#' @export
setGeneric("colData<-", function(object, value) standardGeneric("colData<-"))

#' @rdname TableContainer-method
#' @export
setGeneric("metaData", function(object) standardGeneric("metaData"))

#' @rdname TableContainer-method
#' @export
setGeneric("metaData<-", function(object, value) standardGeneric("metaData<-"))

#' @rdname TableContainer-method
#' @export
setMethod("tblData", "TableContainer", function(object) {
    object@table
})

#' @rdname TableContainer-method
#' @export
setMethod("tblData<-", "TableContainer", function(object, value) {
    object@table <- value
    validObject(object)
    object
})

#' @rdname TableContainer-method
#' @export
setMethod("rowData", "TableContainer", function(object) {
    object@rowData
})

#' @rdname TableContainer-method
#' @export
setMethod("rowData<-", "TableContainer", function(object, value) {
    object@rowData <- value
    validObject(object)
    object
})

#' @rdname TableContainer-method
#' @export
setMethod("colData", "TableContainer", function(object) {
    object@colData
})

#' @rdname TableContainer-method
#' @export
setMethod("colData<-", "TableContainer", function(object, value) {
    object@colData <- value
    validObject(object)
    object
})

#' @rdname TableContainer-method
#' @export
setMethod("metaData", "TableContainer", function(object) {
    object@metaData
})

#' @rdname TableContainer-method
#' @export
setMethod("metaData<-", "TableContainer", function(object, value) {
    object@metaData <- value
    validObject(object)
    object
})


#' Data Conversion
#' 
#' @rdname Data-conversion
#' 
#' @param x A TableContainer object.
#' @return `as.matrix`: A matrix representation of the TableContainer object.
setMethod("as.matrix", "TableContainer", function(x) {
    tblData(x)
})


#' @rdname Data-conversion
#' 
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed. See `base::data.frame` for more details.
#' @param optional Logical. If `TRUE`, setting row names is optional. See `base::data.frame` for more details.
#' @param ... additional arguments
#' @return `as.data.frame`: A data frame representation of the TableContainer object.
#' 
#' @export
setMethod("as.data.frame", "TableContainer", function(x, ...) {
    as.data.frame(as.matrix(x), ...)
})

