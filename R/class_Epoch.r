setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))

#' @title Epoch Class
#' @description S4 class to handle epoch data with electrodes and time points
#' @slot data a tibble containing epoch data (columns=time points, rows=electrodes)
#' @slot times Numeric vector containing time range
.Epoch <- setClass("Epoch",
    slots = list(
        data = "matrix",
        times = "numericOrNULL"
    )
)



#' Constructor for Epoch class
#' @param data Matrix containing epoch data (rows=electrodes, columns=time points)
#' @param electrodes Optional character vector for electrode names, if not provided, column names of data are used. If both are NULL, electrodes are named E1, E2, ...
#' @param timeRanges Optional numeric vector of 2 containing start 
#' and end time points. Only one of times or timeRanges can be non-null
#' @param times Optional numeric vector of time points. Only one of times or
#' timeRanges can be non-null
#' @export
#' @return An Epoch object
Epoch <- function(data, electrodes = NULL, timeRanges = NULL, times = NULL) {
    if (!is.null(times) && !is.null(timeRanges)) {
        stop("Only one of times or timeRanges can be non-null")
    }
    if (!is.null(timeRanges) && length(timeRanges) != 2) {
        stop("timeRanges must be a numeric vector of length 2")
    }
    if (!is.null(times) && length(times) != ncol(data)) {
        stop("Length of times must be equal to number of columns in data")
    }
    if (!is.null(electrodes) && nrow(data) != length(electrodes)) {
        stop("Length of electrodes must be equal to number of rows in data")
    }

    # set default time points if not provided
    if (is.null(times)) {
        if (is.null(timeRanges)) {
            ## check if data has colnames as time points
            if (!is.null(colnames(data))) {
                times <- tryToNum(colnames(data))
            }
        } else {
            times <- seq(timeRanges[1], timeRanges[2], length.out = nrow(data))
        }
    } else {
        times <- as.numeric(times)
    }


    # Set default electrode names if not provided
    if (is.null(electrodes)) {
        electrodes <- if (!is.null(rownames(data))) {
            rownames(data)
        } else {
            paste0("E", seq_len(nrow(data)))
        }
    }

    rownames(data) <- electrodes
    colnames(data) <- NULL

    # Create new Epoch object
    .Epoch(
        data = data,
        times = times
    )
}

###############################
## getter and setter
###############################



###############################
## Data Conversion Methods
###############################


###############################
## other Methods
###############################
#' @description `truncateTime`: Truncating time range
#'
#' @param from Numeric value specifying start of new time range
#' @param to Numeric value specifying end of new time range
#' @return truncateTime: Truncated object
#' @rdname Epoch-method
#' @export
setGeneric("truncateTime", function(x, from, to) standardGeneric("truncateTime"))

#' @rdname Epoch-method
#' @export
setMethod("truncateTime", "Epoch", function(x, from, to) {
    if (is.null(x$times)) {
        if (!isWholeNumber(from) || !isWholeNumber(to)) {
            stop("Time points is not defined for this Epoch object, from and to must be whole numbers")
        }
        indices <- seq(from, to)
        newTimes <- NULL
    } else {
        # current time points
        times <- x$times
        # Find indices within new time range
        indices <- which(times >= from & times <= to)
        newTimes <- times[indices]
    }

    newData <- x$data[, indices, drop = FALSE]

    # Create new Epoch object with truncated data
    Epoch(
        data = newData,
        times = newTimes
    )
})


#' @param object Epoch object
#' @rdname Epoch-method
#' @export
setMethod("show", "Epoch", function(object) {
    dt <- object$data
    pprint(dt, rowdots = 4, coldots = 4, digits = 3)
    cat("\n")
    if (!is.null(object$times)) {
        timeRange <- range(object$times)
        cat(glue("Time range: {timeRange[1]} to {timeRange[2]}"))
        cat("\n")
    }
    cat("Use $ to access its methods. see \"?`Epoch-method`\"\n")
    invisible(object)
})
