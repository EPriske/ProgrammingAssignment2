
## This function will get the inverse of a Matrix and help us with the computing
## if we need the inverse again, storing the result in Cache.

CacheMatrix <- function(x = matrix()){
  INV <- NULL
  SET <- function(y){
    x <<- y
    INV <<- NULL
  }
  GET <- function(){x}
  SETI <- function(BerechnetINV){INV<<-BerechnetINV}
  GETI <- function(){INV}
  list(SET = SET, GET = GET, SETI = SETI, GETI = GETI)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

CacheRes <- function(x, ...){
  INV <- x$GETI()
  if(!is.null(INV)){
    message("Getting Cached Data")
    return(INV)
  }
  data <- x$GET()
  INV <- solve(data, ...)
  x$SETI(INV)
  INV
}
