library(testthat)

test_that("TableContainer slot accessor", {
    set.seed(123)
    row_num <- 20
    col_num <- 20

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    row_dt <- data.frame(row1 = 1:row_num, row2 = letters[1:row_num])
    col_dt <- data.frame(col1 = 1:col_num, col2 = letters[1:col_num])
    metadata <- list(meta1 = "meta1", meta2 = "meta2")

    container <- TableContainer(tbl, rowData = row_dt, colData = col_dt, metaData = metadata)


    # Test slot accessors
    expect_equal(tblData(container), tbl)
    expect_equal(rowData(container), row_dt)
    expect_equal(colData(container), col_dt)
    expect_equal(metaData(container), metadata)
})


test_that("TableContainer valid slot replacement", {
    set.seed(123)
    row_num <- 20
    col_num <- 20

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    row_dt <- data.frame(row1 = 1:row_num, row2 = letters[1:row_num])
    col_dt <- data.frame(col1 = 1:col_num, col2 = letters[1:col_num])
    metadata <- list(meta1 = "meta1", meta2 = "meta2")

    container <- TableContainer(tbl, rowData = row_dt, colData = col_dt, metaData = metadata)

    # Test slot replacement
    new_tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    new_row_dt <- data.frame(
        row_new1 = 1:row_num,
        row_new2 = letters[1:row_num],
        row_new3 = letters[1:row_num]
    )
    new_col_dt <- data.frame(
        col_new1 = 1:col_num,
        col_new2 = letters[1:col_num],
        col_new3 = letters[1:col_num]
    )
    new_metadata <- list(
        meta_new1 = "meta_new1", meta_new2 = "meta_new2",
        meta_new3 = "meta_new3"
    )
    expect_no_error(tblData(container) <- new_tbl)
    expect_no_error(rowData(container) <- new_row_dt)
    expect_no_error(colData(container) <- new_col_dt)
    expect_no_error(metaData(container) <- new_metadata)

    # Check if the slots are updated correctly
    expect_equal(tblData(container), new_tbl)
    expect_equal(rowData(container), new_row_dt)
    expect_equal(colData(container), new_col_dt)
    expect_equal(metaData(container), new_metadata)

    # marginal case for NULL values
    expect_no_error(tblData(container) <- NULL)
    expect_equal(tblData(container), NULL)

    expect_no_error(rowData(container) <- NULL)
    expect_equal(rowData(container), NULL)

    expect_no_error(colData(container) <- NULL)
    expect_equal(colData(container), NULL)

    expect_no_error(metaData(container) <- NULL)
    expect_equal(metaData(container), NULL)

    ## set the data back
    expect_no_error(tblData(container) <- tbl)
    expect_no_error(rowData(container) <- row_dt)
    expect_no_error(colData(container) <- col_dt)
    expect_no_error(metaData(container) <- metadata)
})



test_that("TableContainer invalid slot replacement", {
    set.seed(123)
    row_num <- 20
    col_num <- 20

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    row_dt <- data.frame(row1 = 1:row_num, row2 = letters[1:row_num])
    col_dt <- data.frame(col1 = 1:col_num, col2 = letters[1:col_num])
    metadata <- list(meta1 = "meta1", meta2 = "meta2")

    container <- TableContainer(tbl, rowData = row_dt, colData = col_dt, metaData = metadata)

    # Test slot replacement (unmatched dimensions)
    unmatched_row_num <- row_num + 1
    unmatched_col_num <- col_num + 1
    new_tbl <- matrix(runif(unmatched_row_num * col_num), nrow = unmatched_row_num, ncol = col_num)
    new_row_dt <- data.frame(
        row_new1 = 1:unmatched_row_num,
        row_new2 = letters[1:unmatched_row_num]
    )
    new_col_dt <- data.frame(
        col_new1 = 1:unmatched_col_num,
        col_new2 = letters[1:unmatched_col_num]
    )

    expect_error(tblData(container) <- new_tbl)
    expect_error(rowData(container) <- new_row_dt)
    expect_error(colData(container) <- new_col_dt)
})
