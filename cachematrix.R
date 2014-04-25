## These functions calculate and cache the inverses of matrices and then return
## the cached inverses when the same matrix is sent.  


## makeCacheMatrix() function sets and returns matrices and calculates their 
## inverses, deposits them in a cache and then returns the inverse matix when 
## the inverse of the same matrices were requested 

makeCacheMatrix <- function(x = matrix()) {
     invMtrx <- NULL
     set <- function(y) {
          x <<- y
          invMtrx <<- NULL
     }
     get <- function() {x}
     setInv <- function(inv) {invMtrx <<- inv}
     getInv <- function() {invMtrx}
     
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invMtrx <- x$getInv()
     if(!is.null(invMtrx)) {
          message("getting cached data")
          return(invMtrx)
     }
     
     data <- x$get()
     invMtrx <- solve(data, ...)
     x$setInv(invMtrx)
     invMtrx
}
