library(testthat)

test_that(".show_item_line handles NULL and empty inputs", {
    expect_equal(.make_item_line("Test: ", NULL, 80), "Test: NULL")
    expect_equal(.make_item_line("Test: ", character(0), 80), "Test: NULL")
})

test_that(".show_item_line handles long items with truncation", {
    items <- c("item1", "item2", "item3", "item4", "item5")
    expect_equal(.make_item_line("Items: ", items, 20), "Items: [5 vars] ...")
    expect_equal(.make_item_line("Items: ", items, 40), "Items: [5 vars] item1, item2, item3, ...")
    expect_equal(.make_item_line("Items: ", items, 60), "Items: [5 vars] item1, item2, item3, item4, item5")
})

test_that(".show_item_line handles single item", {
    expect_equal(.make_item_line("Single: ", "item1", 80), "Single: [1 var] item1")
})

test_that("pretty_number formats numbers correctly", {
    expect_equal(pretty_number(1234567890, max_len = 20), "1234567890")
    expect_equal(pretty_number(1234567890, max_len = 8), "1.23e+09")
    expect_equal(pretty_number(1234567890, max_len = 3), "1e+09")
})

test_that("common_formatter handles numeric and character inputs", {
    expect_equal(common_formatter(c(1234567890, 123), max_len = 8), c("1.23e+09", "123"))
    expect_equal(common_formatter(c("short", "this is a long string"), max_len = 10), c("short", "this is..."))
})

test_that("format_tbl handles NULL and empty tables", {
    expect_null(format_tbl(NULL, max_tbl_width = 80))
    expect_equal(format_tbl(data.frame(), max_tbl_width = 80), structure("", col_i = 0))
})

test_that("format_tbl handles tables with no row or column names", {
    tbl <- matrix(1:4, nrow = 2)
    expect_equal(
        format_tbl(tbl, max_tbl_width = 80),
        structure(
            c(
                "     [,1] [,2]",
                "[1,] 1    3   ",
                "[2,] 2    4   "
            ),
            col_i = 2L
        )
    )
})

test_that("format_tbl truncates wide column", {
    tbl <- data.frame(
        x = c(1, 123, 123456678, 1235678887644),
        y = c("abc", "this is a long string", "another long string", "yet another long string"),
        z = c(TRUE, FALSE, TRUE, FALSE)
    )
    formatted_tbl <- format_tbl(tbl, max_tbl_width = 50, max_row = 3)
    expect_equal(formatted_tbl, structure(c(
        "  x         y                    z    ",
        "1 1         abc                  TRUE ",
        "2 123       this is a long st... FALSE",
        "3 123456678 another long string  TRUE "
    ), col_i = 3L))
})


test_that("format_tbl truncates wide column and table", {
    tbl <- data.frame(
        x = c(1, 123, 123456678, 1235678887644),
        y = c("abc", "this is a long string", "another long string", "yet another long string"),
        z = c(TRUE, FALSE, TRUE, FALSE)
    )
    formatted_tbl <- format_tbl(tbl, max_tbl_width = 20, max_row = 3)
    expect_equal(formatted_tbl, structure(c(
        "  x        ",
        "1 1         ...",
        "2 123       ...",
        "3 123456678 ..."
    ), col_i = 1L))
})

test_that("show method", {
    set.seed(123)
    row_num <- 20
    col_num <- 20

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    row_dt <- data.frame(row1 = 1:row_num, row2 = letters[1:row_num])
    col_dt <- data.frame(col1 = 1:col_num, col2 = letters[1:col_num])
    metadata <- list(meta1 = "meta1", meta2 = "meta2")

    container <- TableContainer(tbl, rowData = row_dt, colData = col_dt, metaData = metadata)

    ## Cannot test for the output as it depends on the console width
    expect_no_error(capture.output(show(container)))
})

test_that("show method handles NULL attributes", {
    set.seed(123)
    row_num <- 3
    col_num <- 3

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    container <- TableContainer(tbl)

    ## Cannot test for the output as it depends on the console width
    expect_no_error(capture.output(show(container)))
})



test_that("show method handles NULL attributes", {
    set.seed(123)
    row_num <- 3
    col_num <- 3

    tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
    container <- TableContainer(tbl, rowData = data.frame(), colData = data.frame(), metaData = list())

    ## Cannot test for the output as it depends on the console width
    expect_no_error(capture.output(show(container)))
})
