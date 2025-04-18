
#' Epoch Methods
#'
#'
#' @description
#' `$electrodes`: Get or set electrode names
#' `$times`: Get or set time points
#' `$timeRange`: Get time range if time points are defined
#' `$data`: Get or set data matrix
#'
#' @param x Epoch object
#' @param name a value name, must be one of 'electrodes', 'times',
#' 'timeRange', 'data'
#' @param value Value to set
#' @rdname Epoch-method
#' @export
setMethod("$", "Epoch", function(x, name) {
    if (!name %in% names(x)) {
        stop(glue("Invalid field name: {name}, must be one of {paste0(names(x), collapse = ', ')}"))
    }
    switch(name,
        electrodes = rownames(x@data),
        times = x@times,
        timeRange = if (!is.null(x@times)) range(x@times) else NULL,
        data = x@data,
        stop(glue("Unexpected field name {name}"))
    )
})


#' @rdname Epoch-method
setMethod("$<-", "Epoch", function(x, name, value) {
    if (!name %in% names(x)) {
        stop(glue("Invalid field name: {name}, must be one of {paste0(names(x), collapse = ', ')}"))
    }
    if (name == "electrodes") {
        rownames(x@data) <- value
    }

    if (name == "times") {
        x@times <- value
    }

    if (name == "timeRange") {
        if (length(value) != 2) {
            stop("timeRange must be a numeric vector of length 2")
        }
        x@times <- seq(value[1], value[2], length.out = nrow(x@data))
    }

    if (name == "data") {
        colNms <- rownames(value)
        rowNms <- colnames(value)

        rownames(value) <- x$electrodes
        colnames(value) <- x$times

        x <- Epoch(value, electrodes = colNms, times = rowNms)
    }

    invisible(x)
})



#' @rdname Epoch-method
#' @return names: Return all available properties for an Epoch object
#' @export
setMethod("names", "Epoch", function(x) {
    c("electrodes", "times", "data", "timeRange")
})

#' @rdname Epoch-method
#' @export
setMethod("names<-", "Epoch", function(x, value) {
    stop("Cannot set names for Epoch object")
    invisible(x)
})
