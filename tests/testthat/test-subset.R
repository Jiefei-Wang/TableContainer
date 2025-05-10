library(testthat)

test_that("TableContainer subsetting returns consistent slots", {
  set.seed(123)
  row_num <- 20
  col_num <- 20

  tbl <- matrix(runif(row_num * col_num), nrow = row_num, ncol = col_num)
  row_dt <- data.frame(row1 = 1:row_num, row2 = letters[1:row_num])
  col_dt <- data.frame(col1 = 1:col_num, col2 = letters[1:col_num])
  metadata <- list(meta1 = "meta1", meta2 = "meta2")

  container <- TableContainer(tbl, rowData = row_dt, colData = col_dt, metaData = metadata)

  # [3] returns single row
  container_i <- container[3]
  expect_equal(tblData(container_i), tbl[3, , drop = FALSE])
  expect_equal(rowData(container_i), row_dt[3, , drop = FALSE])
  expect_equal(colData(container_i), col_dt)

  # [1:5,] returns multiple rows
  container_ij_row <- container[1:5, ]
  expect_equal(tblData(container_ij_row), tbl[1:5, ])
  expect_equal(rowData(container_ij_row), row_dt[1:5, , drop = FALSE])
  expect_equal(colData(container_ij_row), col_dt)

  # [,1:4] returns multiple columns
  container_ij_col <- container[, 1:4]
  expect_equal(tblData(container_ij_col), tbl[, 1:4])
  expect_equal(rowData(container_ij_col), row_dt)
  expect_equal(colData(container_ij_col), col_dt[1:4, , drop = FALSE])

  # [6:10,11:15] returns submatrix
  container_ij <- container[6:10, 11:15]
  expect_equal(tblData(container_ij), tbl[6:10, 11:15])
  expect_equal(rowData(container_ij), row_dt[6:10, , drop = FALSE])
  expect_equal(colData(container_ij), col_dt[11:15, , drop = FALSE])

  
  # [6, ] returns a single row
  container_row6 <- container[6, ]
  expect_equal(tblData(container_row6), tbl[6, , drop = FALSE])
  expect_equal(rowData(container_row6), row_dt[6, , drop = FALSE])
  expect_equal(colData(container_row6), col_dt)

  # [, 7] returns a single column
  container_col7 <- container[, 7]
  expect_equal(tblData(container_col7), tbl[, 7, drop = FALSE])
  expect_equal(rowData(container_col7), row_dt)
  expect_equal(colData(container_col7), col_dt[7, , drop = FALSE])

  # [8, 9] returns a 1x1 matrix with correct row/column metadata
  container_cell <- container[8, 9]
  expect_equal(tblData(container_cell), tbl[8, 9, drop = FALSE])
  expect_equal(rowData(container_cell), row_dt[8, , drop = FALSE])
  expect_equal(colData(container_cell), col_dt[9, , drop = FALSE])
})
