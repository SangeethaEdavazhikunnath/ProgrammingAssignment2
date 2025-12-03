## finding inverse of matrix


## makeCacheMatrix create a matrix that can cache its inverse
## cacheSolve find inverse of matrix that returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y) {
  x<<-y
  inv<<-NULL
}
get<-function()x
setInverse<-function(inverse)inv<<-inverse
getInverse<-function()inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## cacheSolve compute inverse of matrix

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  ##if inverse already catched, return it
  if (!is.null(inv)){
    message("Getting catched inverse")
    return(inv)
  }
  ##otherwise compute inverse
  data<-x$get()
  inv<-solve(data, ...)
        ## cache the inverse
  x$setInverse(inv)
  inv
}

