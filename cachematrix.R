## Coursera Assignment - R Programming - Assignment 2: Lexical Scoping

# These 2 functions are useful because they allow the user to calculate the inverse of their matrix and cache the results.
# This can help avoid recomputing the same inverse matrix repeatedly and allow the user to quickly access the stored result.

# This first function, makeCacheMatrix() helps facilitate the caching by: 
#           setting the value of the matrix, getting the value of the matrix, 
#           setting the value of the inverse, and getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_mat <<- inverse
    getinverse <- function() inv_mat
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This 2nd function cacheSolve(), returns the inverse of the matrix from the cache if it has already been calculated.  If not,
#  then it will calculate the inverse and set the value in the cache.

cacheSolve <- function(x, ...) {
    inv_mat <- x$getinverse()
    if(!is.null(inv_mat)) {
        message("getting cached matrix")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setinverse(inv_mat)
    inv_mat
}


###################################################################################################
#  Testing to see if new functions work

test_matrix <- matrix(c(1:4), 2, 2)

# Quick view of test_matrix:

#        [,1] [,2]
#   [1,]   1    3
#   [2,]   2    4


test_cachematrix <- makeCacheMatrix(test_matrix)

cacheSolve(test_cachematrix)

# Quick view of Output of cacheSolve(test_cachematrix) -- No cache in first run

#        [,1] [,2]
#   [1,]  -2   1.5
#   [2,]   1  -0.5


# Test again -- see if it is retrieved from cache

cacheSolve(test_cachematrix)

# getting cached matrix

#        [,1] [,2]
#   [1,]  -2   1.5
#   [2,]   1  -0.5

#  Success!
