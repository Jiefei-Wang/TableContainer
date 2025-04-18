
#' @description `[`: Subset an Epoch object using matrix indexing syntax
#'
#' @param i Row (electrode) indices
#' @param j Column (time) indices
#' @rdname Epoch-method
#' @export
setMethod("[", "Epoch", function(x, i, j) {
    if (!missing(i)){
        i <- checkIndex(i, x$electrodes)
    }
    
    new_data <- x@data[i, j, drop = FALSE]

    if (missing(j)) {
        newTimes <- x@times
    } else {
        newTimes <- x@times[j]
    }

    Epoch(
        data = new_data,
        times = newTimes
    )
})
