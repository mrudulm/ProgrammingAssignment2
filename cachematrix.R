## This function creates a special "matrix" (list) object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


# The following function calculates his function computes the inverse of the special
# "matrix" (list) returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


# Steps to Execute the function to test the code
# > matx <- matrix(c(-1 , 3 ,-3 ,0,-6,5,-5 ,-3,1), 3,3 ,byrow = TRUE)
# > matx
# [,1] [,2] [,3]
# [1,]   -1    3   -3
# [2,]    0   -6    5
# [3,]   -5   -3    1
# > cachmat <- makeCacheMatrix (matx)
# > class(cachmat)
# [1] "list"
# > cacheSolve(cachmat)
# [,1]      [,2]       [,3]
# [1,]  1.500000  1.000000 -0.5000000
# [2,] -4.166667 -2.666667  0.8333333
# [3,] -5.000000 -3.000000  1.0000000
