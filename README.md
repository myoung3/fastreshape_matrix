# fastreshape_matrix

R has many ways to reshape data.frames between wide and long, but none of these are optimized for reshaping regular data between a long table and a matrix/array.

This codebase is a work in progress. 

fastreshape_matrix.R contains code for turning a matrix into a data.table. test.R contains timings for this.

reshape_to_array.Rmd/reshape_to_array.md contain code turning a data.table or matrix in [x,y, z1, z2, ...,zn] format into an rank 3 array of dimensions: `(length(unique(x)), length(unique(y)), n)`

