### Overview

There are two storage architectures for a regular grid over a
two-coordinate system: as a matrix and as a “long” tabular data.frame or
data.table. This document describes change from the tabular format to
the matrix format. Or rather, to the *array* format in the case of
having multiple channels (variables) measured over the grid:

### Example

``` r
library(data.table)
dt <- data.table(long=c(-97,-96,-95,-97,-96,-95),lat=c(38,38,38,39,39,39), s1=c(rep(0,3),rep(1,3)),s2=c(rep(2,3),rep(3,3)),s3=c(rep(4,3),rep(5,3)),s4=c(rep(6,3),rep(7,3)) ) 


#note that lat is first, even though long is "x"
#I'm not just following convention here, this is the order necessary for the following script to work:
coords <- c("lat","long") 

setkeyv(dt, coords) #note that the example already comes sorted by lat then long, but I assert it here
m <- as.matrix(dt[, setdiff(names(dt),coords),with=FALSE])
#note the switch: long becomes the first dimension of the array, lat becomes the second dimension
dimnames <- list(unique(dt[[coords[2]]]), unique(dt[[coords[1]]]), colnames(m))
dims <- sapply(dimnames,length)
a <- array(m, dim=dims, dimnames=dimnames) #note m is a matrix but it's treated as a vector by array()
dt
```

    ##    long lat s1 s2 s3 s4
    ## 1:  -97  38  0  2  4  6
    ## 2:  -96  38  0  2  4  6
    ## 3:  -95  38  0  2  4  6
    ## 4:  -97  39  1  3  5  7
    ## 5:  -96  39  1  3  5  7
    ## 6:  -95  39  1  3  5  7

``` r
m
```

    ##      s1 s2 s3 s4
    ## [1,]  0  2  4  6
    ## [2,]  0  2  4  6
    ## [3,]  0  2  4  6
    ## [4,]  1  3  5  7
    ## [5,]  1  3  5  7
    ## [6,]  1  3  5  7

``` r
dimnames
```

    ## [[1]]
    ## [1] -97 -96 -95
    ## 
    ## [[2]]
    ## [1] 38 39
    ## 
    ## [[3]]
    ## [1] "s1" "s2" "s3" "s4"

``` r
dims
```

    ## [1] 3 2 4

``` r
a
```

    ## , , s1
    ## 
    ##     38 39
    ## -97  0  1
    ## -96  0  1
    ## -95  0  1
    ## 
    ## , , s2
    ## 
    ##     38 39
    ## -97  2  3
    ## -96  2  3
    ## -95  2  3
    ## 
    ## , , s3
    ## 
    ##     38 39
    ## -97  4  5
    ## -96  4  5
    ## -95  4  5
    ## 
    ## , , s4
    ## 
    ##     38 39
    ## -97  6  7
    ## -96  6  7
    ## -95  6  7

### Functions

``` r
tabular_to_array <- function(x,...){
  UseMethod("tabular_to_array",x)
}


#x is a data.table: columns correspond to the eventual dim3 and rows are all combinations of dim1 and dim2
#dim1_col and dim2_col are column names whose values contain the eventual dimnames of the return 
 #nrow(x) should equal length(unique(x$dim1_col))*length(unique(x$dim2_col))
tabular_to_array.data.table <- function(x,dim1_col,dim2_col){
  stopifnot(nrow(x) == uniqueN(x[[dim1_col]])*uniqueN(x[[dim2_col]]))
  k <- key(x)
  on.exit(setkeyv(x,k)) #restore column order on exit
  setkeyv(x, c(dim2_col,dim1_col))
  value_columns <- setdiff(names(x),c(dim2_col,dim1_col))
  v <- with(x, do.call(c, lapply(as.list(value_columns),as.name) )) #concatenate the value columns to one long vector
  #avoid this approach since array will as.vector() on a matrix, resulting in  copy:
  #m <- as.matrix(x[, setdiff(names(x),c(dim2_col,dim1_col)),with=FALSE]) 
  dimnames <- list(unique(x[[dim1_col]]), unique(x[[dim2_col]]), value_columns)
  dims <- sapply(dimnames,length)
  array(v, dim=dims, dimnames=dimnames)
}



#x is a tabular matrix: columns correspond to the eventual dim3 and rows are all combinations of dim1 and dim2
#dimnames1 and dimnames2 are the set of names for dimensions 1 and 2 of the resulting array. names for dimension 3 will be take from column names of x

#dimnames1[i] should correspond to the set of row indices {i + j*(length(dimnames1)) } where j = 0:(length(dimnames2)-1)
 #that is, if length of dimnames1  is 10 and length of dimnames2 is 5, 
 #then dimnames1[1] corresponds to rows 1, 11, 21, 31, 41,
 #and dimnames1[2] corresponds to rows 2, 12, 22, 32, 42
#dimnames2[k] should be a unique vector corresponding to the set of row indices {1:length(dimnames1) + (k-1)*length(dimnames1)} where j = (1,dimnames2+1, 2*dimnames1+1,...) 
#that is, if length of dimnames1  is 10 and length of dimnames2 is 5, 
 #then dimnames2[1] corresponds to rows 1:10
 #and dimnames2[2] corresponds to rows 11:20

#nrow(x) should equal length(dimnames1)*length(dimnames2)
#rows of x must be sorted first on dim2 and then on dim1:
 #that is, as x[1,] -> x[m, ] 
  #the indices/coordinates/dimnames of dim1 will increment for every row in x, 
  #and the indices/coordinates of dim2 will increment only after incrementing through a complete set of dim1 indices 
tabular_to_array.matrix <- function(x,dimnames1, dimnames2){
  stopifnot(nrow(x)==length(dimnames1)*length(dimnames2))
  dimnames <- list(dimnames1, dimnames2, colnames(x))
  dims <- sapply(dimnames,length)
  array(c(x), dim=dims, dimnames=dimnames)
}
```

### Using the data.table method

``` r
dt <- data.table(long=c(-97,-96,-95,-97,-96,-95),lat=c(38,38,38,39,39,39), s1=c(rep(0,3),rep(1,3)),s2=c(rep(2,3),rep(3,3)),s3=c(rep(4,3),rep(5,3)),s4=c(rep(6,3),rep(7,3)) ) 

z <- tabular_to_array(dt, "long","lat")
z
```

    ## , , s1
    ## 
    ##     38 39
    ## -97  0  1
    ## -96  0  1
    ## -95  0  1
    ## 
    ## , , s2
    ## 
    ##     38 39
    ## -97  2  3
    ## -96  2  3
    ## -95  2  3
    ## 
    ## , , s3
    ## 
    ##     38 39
    ## -97  4  5
    ## -96  4  5
    ## -95  4  5
    ## 
    ## , , s4
    ## 
    ##     38 39
    ## -97  6  7
    ## -96  6  7
    ## -95  6  7

### subsetting an array in R

`drop=FALSE` keeps the original dimension

``` r
z[1,,,drop=FALSE]
```

    ## , , s1
    ## 
    ##     38 39
    ## -97  0  1
    ## 
    ## , , s2
    ## 
    ##     38 39
    ## -97  2  3
    ## 
    ## , , s3
    ## 
    ##     38 39
    ## -97  4  5
    ## 
    ## , , s4
    ## 
    ##     38 39
    ## -97  6  7

`drop=TRUE` collapses into a lower-rank array (in this case, a matrix)
if possible. I find the specific convention R uses to map the old
dimensions to the new dimensions counterintuitive:

``` r
z[1,,] #drop=TRUE weirdness. not what I would expect
```

    ##    s1 s2 s3 s4
    ## 38  0  2  4  6
    ## 39  1  3  5  7

``` r
t(z[1,,])
```

    ##    38 39
    ## s1  0  1
    ## s2  2  3
    ## s3  4  5
    ## s4  6  7

### Using the matrix method:

``` r
z2 <- tabular_to_array(as.matrix(dt[,-(1:2)]), dimnames1=unique(dt$long),dimnames2=unique(dt$lat))
identical(z, z2)
```

    ## [1] TRUE

Above, we took the unique of the dt columns. this works because it’s
sorted on both lat and long. And `as.matrix(dt)`, dropping the coord
columns, works because it’s sorted on long first then lat (ie, according
to specification).

### reorder dt

the data.table method is robust to reordering of x since x is sorted
internally:

``` r
setkey(dt, long,lat) #note now we're sorting on long first then lat
z3 <- tabular_to_array(dt, "long","lat")
identical(z,z3)
```

    ## [1] TRUE

But the matrix method is sensitive to the order of the rows in the
matrix. The following is complete nonsense, and there’s no
transformation to fix it since the dimnames are misspecified:

``` r
z4 <- tabular_to_array(as.matrix(dt[,-(1:2)]), dimnames1=unique(dt$long),dimnames2=unique(dt$lat))
identical(z, z4)
```

    ## [1] FALSE

That’s not to say we can’t work with a matrix that’s sorted in the
opposite way. Since we are sorting on long first then lat, we just need
to switch the dimnames.

``` r
z5 <- tabular_to_array(as.matrix(dt[,-(1:2)]), dimnames1=unique(dt$lat),dimnames2=unique(dt$long))
```

This results in an array that, if transposed correctly, is identical to
`z`:

``` r
dim(z5)
```

    ## [1] 2 3 4

``` r
dim(z)
```

    ## [1] 3 2 4

``` r
z5perm <- aperm(z5, c(2,1,3)) #switch the first and second dimensions
identical(z, z5perm)
```

    ## [1] TRUE

### performance:

These comments are mostly guesses, but they’d be easy enough to test.

-   speed
    -   assuming both are sorted beforehand. the data.table approach
        might be slower, depending on how the columns were generated,
        since the columns aren’t necessarily contiguous in memory.
-   memory
    -   the data.table approach copies the data when coercing from
        data.table to vector. since data.tables are lists, copying
        *from* a data.table is unavoidable.
    -   array may make a copy when coercing from matrix to vector.
        Somewhat surprsingly, this means the data.table method and the
        matrix method probably have the same memory overhead (1
        intermediate copy each). This is because matrices and vectors
        apparently can’t point to the same object in memory:
        <a href="https://stackoverflow.com/questions/66021039/why-does-as-vector-deep-copy-a-matrix" class="uri">https://stackoverflow.com/questions/66021039/why-does-as-vector-deep-copy-a-matrix</a>
