## These code segments use lists to store pre-calculated inverse matrix (cache), 
## so that we can save lots of time by avoid computing for the same matrix.  

## makeCacheMatrix create the lists to store the pre-computed "cache" data for 
## each matrix, it also provide four methods (functions) to access these data.
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setIM: set the value of the Inverse of a Matrix
## 4. getIM: get the value of the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL
    set <- function(y) {
        x <<- y
        IM <<- NULL
    }
    get <- function() x
    setIM <- function(inverse) IM <<- inverse
    getIM <- function() IM
    list(set = set, get = get,
         setIM = setIM,
         getIM = getIM)
}


## cacheSolve calculate the inverse of matrix, this function will check whether
## the inputed matrix are claculted or not: if yes, return the store result 
## directly; if not, calculate it and sotre it back to corresponding list entry.
cacheSolve <- function(x, ...) {
    IM <- x$getIM()
    if(!is.null(IM)) {
        message("getting cached data")
        return(IM)
    }
    data <- x$get()
    IM <- solve(data, ...)
    x$setIM(IM)
    IM
}
