#' @rdname Epoch-method
#' @export
setMethod("as.matrix", "Epoch", function(x) {
    dt <- x@data
    colnames(dt) <- x$times
    dt
})

#' @rdname Epoch-method
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed. See `base::data.frame` for more details.
#' @param optional Logical. If `TRUE`, setting row names is optional. See `base::data.frame` for more details.
#' @param ... additional arguments
#' @export
setMethod("as.data.frame", "Epoch", function(x, ...) {
    as.data.frame(as.matrix(x), ...)
})

