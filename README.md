
# TableContainer

`TableContainer` is an R package that provides a lightweight and flexible container for managing tabular data with associated row and column annotations. It is inspired by Bioconductor's `SummarizedExperiment` but does not rely on Bioconductor dependencies, making it easier to integrate into various workflows.

## Features

- **Matrix-like Data Storage**: Store data in a matrix or data frame format.
- **Row and Column Annotations**: Add metadata to rows and columns using data frames.
- **Flexible Metadata**: Attach arbitrary metadata to the container.
- **Automatic Annotation Updates**: Automatically update annotations when subsetting the table.


## Installation

To install the development version of `TableContainer`:

```r
remote::install_github("Jiefei-Wang/TableContainer")
```

To install the stable version from CRAN:

```r
install.packages("TableContainer")
```

## Usage

### Creating a TableContainer

You can create a `TableContainer` object using the `TableContainer()` constructor:

```R
library(TableContainer)

# Example data
tbl <- matrix(1:12, nrow = 3, ncol = 4)
row_dt <- data.frame(row1 = 1:3, row2 = letters[1:3])
col_dt <- data.frame(col1 = 1:4, col2 = letters[1:4])
metadata <- list(meta1 = "meta1", meta2 = "meta2")

# Create a TableContainer
container <- TableContainer(
  table = tbl,
  rowData = row_dt,
  colData = col_dt,
  metadata = metadata
)
```

Example output:
```
> container
# TableContainer: 3 rows x 4 cols  ( matrix )
     [,1]  [,2]  [,3]  [,4]
[1,]  1     4     7     10
[2,]  2     5     8     11
[3,]  3     6     9     12
---
rowData: [2 vars] row1, row2
colData: [2 vars] col1, col2
metadata: [2 elements] meta1, meta2
```

### Subsetting

Subset the `TableContainer` object while maintaining consistency in annotations:

```R
# Subset rows and columns
subset_container <- container[1:2, 2:4]
```

Example output:
```
> subset_container
# TableContainer: 2 rows x 3 cols  ( matrix )
     [,1]  [,2]  [,3]
[1,]  4     7     10
[2,]  5     8     11
---
rowData: [2 vars] row1, row2
colData: [2 vars] col1, col2
metadata: [2 elements] meta1, meta2
```

### Accessing and Modifying Data

You can access and modify the table, row annotations, column annotations, and metadata using accessor methods:

```R
# Access data
tblData(container)
rowData(container)
colData(container)
metadata(container)

# Modify data
tblData(container) <- matrix(13:24, nrow = 3, ncol = 4)
rowData(container) <- data.frame(newRow = 1:3)
colData(container) <- data.frame(newCol = 1:4)
metadata(container) <- list(newMeta = "updated metadata")
```

## Documentation

For detailed documentation, see the help files for the `TableContainer` class and its methods:

```R
?TableContainer
```

## License

This package is licensed under the MIT License. 

## Author

Developed by Jiefei Wang (<szwjf08@gmail.com>).
