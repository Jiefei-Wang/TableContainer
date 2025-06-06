# Helper to show a line of items (e.g., column names) fitting terminal width
.make_item_line <- function(label, items, available_width,
                            total_item_count = NULL, # Explicitly pass total if items is a subset
                            item_label_singular = "var",
                            item_label_plural = "vars",
                            empty_msg = "NULL") {
    if (is.null(total_item_count)) {
        total_item_count <- if (is.null(items)) 0 else length(items)
    }

    item_count_str <- if (total_item_count == 1) item_label_singular else item_label_plural
    prefix <- glue("{label}[{total_item_count} {item_count_str}] ")

    if (total_item_count == 0 || is.null(items) || length(items) == 0) {
        msg <- glue("{label}{empty_msg}")
        return(as.character(msg))
    }

    # Available width for the items themselves after prefix
    item_space <- available_width - nchar(prefix)

    # If no space for items, just return the prefix
    if (item_space <= 3 && total_item_count > 0) {
        msg <- glue("{prefix}...")
        return(as.character(msg))
    }


    current_line_items_part <- ""
    num_shown <- 0
    actual_items_to_list <- if (!is.null(names(items))) names(items) else as.character(items)
    if (is.null(actual_items_to_list) && total_item_count > 0) {
        actual_items_to_list <- rep("?", total_item_count)
    } # Placeholder if names are NULL but count > 0
    else if (is.null(actual_items_to_list)) actual_items_to_list <- character(0)


    for (i in seq_along(actual_items_to_list)) {
        item <- actual_items_to_list[i]
        sep <- if (num_shown > 0) ", " else ""

        prospective_line <- paste0(current_line_items_part, sep, item)

        # Ellipsis might be needed if this is not the last *shown* item OR if total count is greater
        needs_ellipsis_suffix <- (i < length(actual_items_to_list)) || (length(actual_items_to_list) < total_item_count)
        ellipsis_chars <- if (needs_ellipsis_suffix) ", ..." else ""

        if (nchar(prospective_line) + nchar(ellipsis_chars) <= item_space) {
            current_line_items_part <- prospective_line
            num_shown <- num_shown + 1
        } else {
            if (num_shown > 0 && needs_ellipsis_suffix) { # We showed some, now add ellipsis
                current_line_items_part <- paste0(current_line_items_part, ", ...")
            } else if (num_shown == 0 && needs_ellipsis_suffix) { # First item itself too long with ellipsis
                # Try to fit first item truncated + ellipsis
                needed_for_item_and_ellipsis <- item_space - nchar(", ...")
                if (needed_for_item_and_ellipsis > 0) {
                    current_line_items_part <- paste0(substr(item, 1, needed_for_item_and_ellipsis), "...")
                } else { # Not even space for "X..."
                    current_line_items_part <- "..."
                }
            } else if (num_shown == 0 && !needs_ellipsis_suffix && nchar(item) > item_space) { # Only one item, too long
                current_line_items_part <- paste0(substr(item, 1, item_space - 3), "...")
            }
            # If it's the last item and it just doesn't fit, current_line_items_part remains as is from previous.
            break
        }
    }
    # If all provided 'actual_items_to_list' were shown, but they represent a subset of 'total_item_count'
    if (num_shown == length(actual_items_to_list) && length(actual_items_to_list) < total_item_count && num_shown > 0) {
        if (!grepl("...", current_line_items_part, fixed = TRUE)) { # ensure not adding duplicate "..."
            current_line_items_part <- paste0(current_line_items_part, ", ...")
        }
    } else if (num_shown == 0 && total_item_count > 0) { # No items were shown, but there should be some
        current_line_items_part <- "..." # Default to ... if nothing fits
    }
    msg <- glue("{prefix}{current_line_items_part}")
    return(as.character(msg))
}



# Get the width of the terminal
get_terminal_width <- function() {
    width <- tryCatch(
        {
            console_width()
        },
        error = function(e) {
            # Fallback to a default width if console_width fails
            80
        }
    )
    width
}

#' Common functions for formatting table cells
#'
#' @param x A vector of values to format.
#' @param max_len The maximum length for the formatted string.
#' @details
#' `pretty_number` : keep all digits if we can keep it within the width limit. Otherwise, use scientific notation to reduce length. If the number still cannot fit within the length limit, return the shortest result.
#' @seealso [format_tbl()] for formatting tables
#'
#' @examples
#' ## Format a number
#' pretty_number(1234567890, max_len = 20)
#' pretty_number(1234567890, max_len = 8)
#' pretty_number(1234567890, max_len = 3)
#' @export
#' @rdname prettier
pretty_number <- function(x, max_len = 20) {
    # Try regular format
    regular <- format(x, scientific = FALSE, trim = TRUE)
    if (nchar(regular) <= max_len) {
        return(regular)
    }

    # Try scientific with default digits
    sci <- format(x, scientific = TRUE, digits = 4)
    if (nchar(sci) <= max_len) {
        return(sci)
    }

    # Try decreasing digits in scientific format until it fits
    for (d in 3:1) {
        sci_short <- format(x, scientific = TRUE, digits = d)
        if (nchar(sci_short) <= max_len) {
            return(sci_short)
        }
    }

    # If nothing fits, return as is
    sci_short
}

.common_formatter <- function(x, max_len = 20) {
    if (is.numeric(x)) {
        x2 <- pretty_number(x, max_len = max_len)
    } else {
        x2 <- as.character(x)
        if (nchar(x2) > max_len) {
            x2 <- substr(x2, 1, max(max_len - 3, 0))
            x2 <- paste0(x2, "...")
        }
    }
    x2
}


#' Common functions for formatting table cells
#'
#' @param x A vector of values to format.
#' @details
#' `common_formatter` : For numeric, call `pretty_number` to format the number. For non-numeric, truncate the string and append "..." if it exceeds the width limit.
#' @examples
#' ## format character
#' common_formatter("this is a long string", max_len = 40)
#' common_formatter("this is a long string", max_len = 20)
#' @export
#' @rdname prettier
common_formatter <- function(x, max_len = 20) {
    vapply(x, .common_formatter, character(1), max_len = max_len, USE.NAMES = FALSE)
}


#' Format a table for display. The maximum number of columns
#' displayed is limited by the terminal width.
#'
#' @param tbl A table-like object (e.g., matrix, data.frame).
#' @param max_tbl_width The maximum width for the table display.
#' @param soft_table_width A softer width limit for the table display, allowing the last column to exceed it.
#' @param max_row The maximum number of rows to display.
#' @param cell_formatter A function to format individual cells.
#' @param delimiter The delimiter to use for separating columns.
#' @param truncate_marker A marker to indicate truncation of the table.
#' @param header_name A string to print as the first element in the column names row. It only works when `include_col_names = TRUE`.
#' @param leading_text A string to print before the table. This can be used to indent the entire table. It can be a single string or a vector of strings. If it is a vector, it will be recycled to match the number of rows in the table.
#' @param include_row_names Logical, whether to include row names in the display.
#' @param include_col_names Logical, whether to include column names in the display.
#'
#' @return A formatted string vector representing the table. The length of the vector is the number of rows in the table.
#' @seealso [common_formatter()] for the cell formatting function and [pretty_number()] for formatting numeric values.
#'
#' @examples
#' ## Format the table
#' tbl <- data.frame(
#'     x = c(1, 123, 123456678, 1235678887644),
#'     y = c("abc", "this is a long string", "another long string", "yet another long string"),
#'     z = c(TRUE, FALSE, TRUE, FALSE),
#'     d = runif(4) * 100000
#' )
#' formatted_tbl <- format_tbl(tbl, max_tbl_width = 50, max_row = 3)
#' cat(formatted_tbl, sep = "\n")
#'
#' @export
format_tbl <- function(
    tbl, max_tbl_width, soft_table_width = max_tbl_width,
    max_row = 8,
    cell_formatter = common_formatter,
    delimiter = " ",
    truncate_marker = " ...",
    header_name = "",
    leading_text = NULL,
    include_row_names = TRUE, include_col_names = TRUE) {
    if (is.null(tbl)) {
        return(NULL)
    }

    ## calculate the number of rows we need to display
    max_row <- min(max_row, nrow(tbl))
    max_row_with_col <- max_row + include_col_names
    row_indices <- seq_len(max_row)

    ## check the column names
    if (include_col_names) {
        col_names <- colnames(tbl)
        if (is.null(col_names)) {
            col_names <- paste0("[,", seq_len(ncol(tbl)), "]")
        }
    } else {
        col_names <- NULL
    }

    ## check the row names
    if (include_row_names) {
        row_names <- rownames(tbl)
        if (is.null(row_names)) {
            row_names <- paste0("[", seq_len(max_row), ",]")
        }
    } else {
        row_names <- NULL
    }


    col_widths <- c()
    total_len <- 0
    char_tbl <- c()

    ## Add the leading text
    if (!is.null(leading_text)){
        max_col_width <- max(nchar(leading_text))
        # col_widths <- c(col_widths, max_col_width)
        total_len <- total_len + max_col_width
        # char_tbl <- cbind(char_tbl, leading_text)
    }

    ## Add the row names
    if (include_row_names) {
        col_dt <- row_names[row_indices]
        if (include_col_names) {
            col_dt <- c(header_name, col_dt)
        }
        max_col_width <- max(c(nchar(col_dt), 0), na.rm = TRUE)
        col_widths <- c(col_widths, max_col_width)
        total_len <- total_len + max_col_width
        char_tbl <- cbind(char_tbl, col_dt)
    }

    col_i <- 0
    for (i in seq_len(ncol(tbl))) {
        col_dt <- vapply(tbl[row_indices, i], cell_formatter, character(1), USE.NAMES = FALSE)
        if (include_col_names) {
            col_dt <- c(col_names[i], col_dt)
        }
        max_col_width <- max(c(nchar(col_dt), 0), na.rm = TRUE)
        col_widths <- c(col_widths, max_col_width)
        total_len <- total_len + nchar(delimiter) + max_col_width
        ## For soft table width limit, we only allow the last column to exceed the limit
        if (total_len > soft_table_width && i != ncol(tbl)) {
            break
        }
        ## For hard table width limit, we stop if the total length exceeds the limit
        if (total_len > max_tbl_width) {
            break
        }
        col_i <- i
        char_tbl <- cbind(char_tbl, col_dt)
    }

    if (length(char_tbl) == 0) {
        return(NULL)
    }


    ## left align the columns with space padding
    for (i in seq_len(ncol(char_tbl))) {
        char_tbl[, i] <- formatC(char_tbl[, i], width = col_widths[i], flag = "-")
    }

    char_vec <- apply(char_tbl, 1, paste0, collapse = delimiter)

    ## add leading text
    if (!is.null(leading_text)) {
        leading_text <- rep(leading_text, length.out = length(char_vec))
        char_vec <- paste0(leading_text, char_vec)
    }

    ## add truncate marker to the last column if it is truncated
    for (i in seq_along(char_vec)) {
        if (col_i != ncol(tbl)) {
            if (include_row_names && i == 1) {
                next
            }
            char_vec[i] <- paste0(char_vec[i], truncate_marker)
        }
    }



    attr(char_vec, "col_i") <- col_i
    char_vec
}



#' Show method for TableContainer and its components
#'
#' The show method uses `.printTable` to display the table and `.printMeta` to display the rowMeta, colMeta, and metaData.
#'
#' @param tbl A table-like object (e.g., matrix, data.frame).
#' @param ... Additioanl arguments passed to the `format_tbl` function.
#'
#'
#' @rdname show-TableContainer-method
#' @export
.printTable <- function(tbl, ...) {
    args <- list(...)
    term_width <- get_terminal_width()

    if (!is.null(tbl)) {
        nr <- nrow(tbl)
        nc <- ncol(tbl)
        max_row <- 10
        soft_table_width <- term_width - 4 # Leave space for ellipsis
        hard_table_width <- term_width

        default_args <- list(
            tbl = tbl,
            max_tbl_width = hard_table_width,
            soft_table_width = soft_table_width,
            max_row = max_row,
            cell_formatter = common_formatter,
            delimiter = " ",
            truncate_marker = " ...",
            header = "",
            leading_text = " ",
            include_row_names = TRUE, include_col_names = TRUE
        )

        # Merge default arguments with user-provided arguments
        args <- modifyList(default_args, args)
        tbl_str <- do.call(format_tbl, args)

        if (!is.null(tbl)) {
            for (i in seq_along(tbl_str)) {
                cat(tbl_str[i], "\n")
            }
        }
        # --- Ellipsis for Rows ---
        if (max_row < nr) {
            cat("...\n")
        }
        cat(glue("[{nr} rows x {nc} cols]\n"))
    } else {
        cat("[table is empty]\n")
    }

    cat("\n") # Separator
}

#' @param meta A list of metadata items.
#' @param name A string representing the name of the metadata.
#'
#' @rdname show-TableContainer-method
#' @export
.printMeta <- function(meta, name) {
    term_width <- get_terminal_width()
    if (!is.null(meta) && length(meta) > 0) {
        # If names are NULL but there are elements, create placeholder like "[[1]], [[2]]"
        display_meta_items <- names(meta)
        if (is.null(display_meta_items)) {
            display_meta_items <- paste0("[[", seq_along(meta), "]]")
        }
        for (i in seq_along(display_meta_items)) {
            if (display_meta_items[i] == "") {
                display_meta_items[i] <- paste0("[[", i, "]]")
            }
        }

        msg <- .make_item_line(glue("{name}: "), display_meta_items, term_width,
            total_item_count = ncol(meta), item_label_singular = "var", item_label_plural = "vars"
        )
        cat(msg, "\n")
    }
}



#' @param object A TableContainer object.
#' @importMethodsFrom methods show
#' @rdname show-TableContainer-method
#' @export
setMethod(
    "show", "TableContainer",
    function(object) {
        tbl <- tblData(object)
        rd <- rowData(object)
        cd <- colData(object)
        md <- metaData(object)

        # --- Table Preview ---
        cat("# TableContainer:\n")
        .printTable(tbl, header = "")


        # --- rowData ---
        .printMeta(rd, "rowData")

        # --- colData ---
        .printMeta(cd, "colData")

        # --- metadata ---
        .printMeta(md, "metaData")
    }
)
