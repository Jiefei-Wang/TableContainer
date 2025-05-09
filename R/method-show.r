# Helper to show a line of items (e.g., column names) fitting terminal width
.show_item_line <- function(label, items, available_width,
                            total_item_count = NULL, # Explicitly pass total if items is a subset
                            item_label_singular = "var",
                            item_label_plural = "vars",
                            empty_msg = "none") {

    if (is.null(total_item_count)) {
        total_item_count <- if (is.null(items)) 0 else length(items)
    }

    item_count_str <- if (total_item_count == 1) item_label_singular else item_label_plural
    prefix <- paste0(label, "[", total_item_count, " ", item_count_str, "] ")

    if (total_item_count == 0 || is.null(items) || length(items) == 0) {
        cat(paste0(label, empty_msg, "\n"))
        return()
    }
    
    # Available width for the items themselves after prefix
    item_space <- available_width - nchar(prefix)
    
    if (item_space <= 3 && total_item_count > 0) { # Not enough space for even "..." after prefix
        # Try to print at least the prefix, truncated if it's too long for the whole line
        if (nchar(prefix) > available_width) {
             cat(paste0(substr(prefix, 1, available_width - 3), "...\n"))
        } else {
             cat(paste0(prefix, "...\n")) # Prefix fits, but no items do
        }
        return()
    } else if (item_space <= 0) { # Prefix itself is too long or fills line
        cat(paste0(substr(prefix, 1, available_width - 3), "...\n"))
        return()
    }


    current_line_items_part <- ""
    num_shown <- 0
    actual_items_to_list <- if(!is.null(names(items))) names(items) else as.character(items)
    if (is.null(actual_items_to_list) && total_item_count > 0) actual_items_to_list <- rep("?", total_item_count) # Placeholder if names are NULL but count > 0
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

    cat(paste0(prefix, current_line_items_part, "\n"))
}


#' Pretty show method for TableContainer
#' @param object A TableContainer object.
#' @importMethodsFrom methods show
#' @export
setMethod("show", "TableContainer",
    function(object) {
        tbl <- tblData(object)
        rd <- rowData(object)
        cd <- colData(object) 
        md <- metadata(object)

        term_width <- console_width()
        cat(paste0("# TableContainer: ",
                   if(is.null(tbl)) "0" else nrow(tbl), " rows x ",
                   if(is.null(tbl)) "0" else ncol(tbl), " cols"),
            " (", class(tbl)[1], ")\n")

        # --- Table Preview ---
        if (!is.null(tbl) && nrow(tbl) > 0 && ncol(tbl) > 0) {
            nr <- nrow(tbl)
            nc <- ncol(tbl)
            
            max_r_show <- 7 # Max rows to print in preview
            r_to_show <- min(nr, max_r_show)
            
            # --- Determine Row Names Width ---
            rnames <- rownames(tbl)
            if (is.null(rnames) && r_to_show > 0) rnames <- paste0("[", 1:nr, ",]") # Default row indicators
            
            rnames_width <- 0
            if (r_to_show > 0) {
                 rnames_disp <- rnames[1:r_to_show]
                 if (is.null(rnames_disp)) rnames_disp <- paste0("[", 1:r_to_show, ",]")
                 rnames_width <- max(nchar(rnames_disp)) + 1 # +1 for a space
            }

            # --- Determine How Many Columns to Show & Their Widths ---
            c_to_show <- 0
            available_for_data_cols <- term_width - rnames_width - 20 # for potential leading space or final "..."
            available_for_data_cols <- max(available_for_data_cols, 10)
            
            
            # Store calculated widths for columns that will be shown
            final_cell_widths <- numeric(0) 
            # Store formatted character data for the preview
            char_data_preview_cols <- list() 
            
            temp_cnames_full <- colnames(tbl)
            if (is.null(temp_cnames_full) && nc > 0) temp_cnames_full <- paste0("[,", 1:nc, "]")
            
            current_total_width_used_by_data <- 0

            if (nc > 0) {
                for (k_col in 1:nc) {
                    col_data_vector <- tbl[1:r_to_show, k_col]
                    
                    # Format data for width calculation (and later printing)
                    if (is.numeric(col_data_vector)) {
                        is_int_like <- all(abs(col_data_vector - round(col_data_vector)) < 1e-6, na.rm = TRUE) &&
                                       all(abs(col_data_vector) < 1e7, na.rm = TRUE)
                        if (is_int_like && !anyNA(col_data_vector[is.finite(col_data_vector)])) {
                            formatted_col_k <- format(round(col_data_vector), scientific = FALSE, trim = TRUE)
                        } else {
                            formatted_col_k <- formatC(col_data_vector, digits = 3, format = "g", flag = "#", width=1)
                        }
                    } else if (is.logical(col_data_vector)) {
                        formatted_col_k <- ifelse(is.na(col_data_vector), "NA", ifelse(col_data_vector, " T", " F")) # Padded T/F
                    }
                    else {
                        formatted_col_k <- as.character(col_data_vector)
                    }
                    formatted_col_k[is.na(tbl[1:r_to_show, k_col])] <- "NA" # Ensure NAs are "NA"
                    
                    current_col_name <- temp_cnames_full[k_col]
                    width_for_this_col_content <- max(nchar(formatted_col_k), na.rm = TRUE)
                    width_for_this_col_name <- nchar(current_col_name)
                    
                    # Actual width for this column: max of data, name; add 1 for inter-col space
                    # Cap individual column width to prevent one very wide column hogging space
                    this_col_total_width <- min(max(width_for_this_col_content, width_for_this_col_name, 3L) + 1L, 15L + 1L)


                    # Check if adding this column (plus potential "..." if it's not the last actual col) fits
                    ellipsis_for_cols_width <- if (c_to_show + 1 < nc) 4 else 0 # " ..." = 4 chars
                    
                    if (current_total_width_used_by_data + this_col_total_width + ellipsis_for_cols_width <= available_for_data_cols) {
                        current_total_width_used_by_data <- current_total_width_used_by_data + this_col_total_width
                        c_to_show <- c_to_show + 1
                        final_cell_widths <- c(final_cell_widths, this_col_total_width -1) # Store content width
                        char_data_preview_cols[[k_col]] <- formatted_col_k
                    } else {
                        break # Cannot fit this column
                    }
                }
            }
            
            # --- Print Column Headers ---
            if (c_to_show > 0) {
                cat(formatC("", width = rnames_width)) # Align with row names space
                for (j in 1:c_to_show) {
                    # Use actual column name from temp_cnames_full, corresponding to the selected columns
                    actual_col_idx_in_original_table <- which(sapply(char_data_preview_cols, function(x) !is.null(x)))[j]
                    col_header_to_print <- temp_cnames_full[actual_col_idx_in_original_table]
                    cat(formatC(col_header_to_print, width = final_cell_widths[j], flag = "-"), " ")
                }
                if (c_to_show < nc) cat("...")
                cat("\n")
            } else if (nc > 0) { # No columns fit, but columns exist
                cat(formatC("", width = rnames_width))
                cat("... (columns hidden due to width)\n")
            }

            # --- Print Rows of Data ---
            actual_shown_col_indices <- which(sapply(char_data_preview_cols, function(x) !is.null(x)))
            for (i in 1:r_to_show) {
                cat(formatC(rnames[i], width = rnames_width -1 , flag = "-"), " ")
                
                for (j_idx in 1:c_to_show) {
                    original_col_idx <- actual_shown_col_indices[j_idx]
                    val_str <- char_data_preview_cols[[original_col_idx]][i]
                    cat(formatC(val_str, width = final_cell_widths[j_idx], flag = "-"), " ")
                }
                if (c_to_show < nc) cat("...")
                cat("\n")
            }
            
            # --- Ellipsis for Rows ---
            if (r_to_show < nr) {
                cat(formatC(paste0("[...", nr - r_to_show, " more rows]"), width = rnames_width -1, flag = "-"), " ")
                cat("\n")
            }

        } else if (!is.null(tbl) && (nrow(tbl) == 0 || ncol(tbl) == 0)) {
            cat("  (table is empty or has 0 rows/columns)\n")
        } else {
            cat("  (table is NULL)\n")
        }
        
        cat("---\n") # Separator

        # --- rowData ---
        if (!is.null(rd)) {
            .show_item_line("rowData: ", colnames(rd), term_width,
                            total_item_count = ncol(rd), item_label_singular = "var", item_label_plural = "vars")
        } else {
            cat("rowData: NULL\n")
        }

        # --- colData ---
        if (!is.null(cd)) {
            .show_item_line("colData: ", colnames(cd), term_width,
                            total_item_count = ncol(cd), item_label_singular = "var", item_label_plural = "vars")
        } else {
            cat("colData: NULL\n")
        }
        
        # --- metadata ---
        meta_names <- if(!is.null(md)) names(md) else NULL
        meta_count <- if(!is.null(md)) length(md) else 0
        
        if (meta_count > 0) {
             # If names are NULL but there are elements, create placeholder like "[[1]], [[2]]"
            display_meta_items <- meta_names
            if (is.null(meta_names) && meta_count > 0) {
                display_meta_items <- paste0("[[", seq_len(min(meta_count, 5)), "]]") # Show first few placeholders
            }

            .show_item_line("metadata: ", display_meta_items, term_width,
                            total_item_count = meta_count, item_label_singular = "element", item_label_plural = "elements")
        } else if (!is.null(md) && meta_count == 0) { # Empty list
            cat("metadata: list(0)\n")
        } else { # metadata is NULL
            cat("metadata: NULL\n")
        }
    }
)
