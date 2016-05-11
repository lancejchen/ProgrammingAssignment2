## Put comments here that give an overall description of what your
## functions do
# 1. MakeCacheMatrix(x=matrix()) is for creating a cache environment and return
#    a list(each element contains one or more than one element, therefore could
#    be described as a special "Matrix" as the assignment required) in that 
#    environment. Its input is the matrix which is requested to get its inverse 
#    matrix.
# 2. cacheSolve(x, ...) is used for getting inverse matrix. It will check the 
#    list object first to know whether the cache exist in that environment. 
#    Use the cache if it already exists and create its inverse matrix if not.
#    its input argument x is the result of MakeCacheMatrix(x=matrix())

# Example for usage. 
# > cache <- MakeCacheMatrix(x=matrix(1:4,2,2))
# > inverseMatrix <- cacheSolve(cache, ...)

## Write a short comment describing this function
#    MakeCacheMatrix(x=matrix()) is for creating a cache environment and return
#    a list in that environment. Its input is the matrix which is requested to 
#    get its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
#    cacheSolve(x, ...) is used for getting inverse matrix. It will check the 
#    list object first to know whether the cache exist in that environment. 
#    Use the cache if it already exists and create its inverse matrix if not.
#    its input argument x is the result of MakeCacheMatrix(x=matrix())
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    # m is the inverse of 'x'
    m
}
