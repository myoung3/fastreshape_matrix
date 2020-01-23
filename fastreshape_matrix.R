
library(data.table)

reshape_matrix <- function(x,rnames=NULL,cnames=NULL){
  if(is.null(rnames)){
    if(is.null(rownames(x))){
      stop("since rownames of x are missing, specify rnames")
    }
    rnames <- rownames(x)
  }
  if(is.null(rnames)){
    if(is.null(colnames(x))){
      stop("since colnames of x are missing, specify cnames")
    }
    cnames <- colnames(x)
  }
  
  
  
  as_f <- if(is.integer(x)){
    as.integer
  }else{
    if(is.numeric(x)){
      as.numeric
    }else{
      if(is.character(x)){
        as.character
      }
      
    }
  }
  
  
  outDT <- data.table(value=rep(as_f(NA), length(x)),
                      rowname_=rep( rnames,
                                 times=ncol(x)),
                      colname_=rep( cnames,each=nrow(x))
  )
  rows <- 1L:nrow(x)
  for(i in 1L:ncol(x)){
    set(outDT, i=rows,j="value",value=x[,i])
    rows <- (i*nrow(x)+1L):((i+1L)*nrow(x))
  }
  setcolorder(outDT, c("rowname_","colname_","value"))
  outDT[]
}


fast_reshape_matrix <- function(x,rnames=NULL,cnames=NULL){
  if(is.null(rnames)){
    if(is.null(rownames(x))){
      stop("since rownames of x are missing, specify rnames")
    }
    rnames <- rownames(x)
  }
  if(is.null(rnames)){
    if(is.null(colnames(x))){
      stop("since colnames of x are missing, specify cnames")
    }
    cnames <- colnames(x)
  }
  
  
  if("rowname_" %in% cnames){
    stop("rowname_ is reserved. cannot be in cnames")
  }
  x_w <- as.data.table(x)
  setnames(x_w, cnames)
  x_w[, rowname_:=rnames]
  outDT <- melt(x_w,id.vars="rowname_",variable.name="colname_")
  setcolorder(outDT, c("rowname_","colname_","value"))
  outDT[, colname_:=as.character(colname_)]
  outDT[]
}
