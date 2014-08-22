## These two functions are used to compute the inverse of an invertible matrix
## and cache the result for future need to avoid repeatly computation.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ##'inv' is used to store the inversed matrix, reset to NULL
        set <- function(y) {  ## set the input matrix
                x <<- y       ## pass newly set matrix to object x
                inv <<- NULL  ## reset inverse of matrix to NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve ## set the inverse of matrix and 
                                                    ## store it to 'inv'
        getinverse <- function() inv                ## get the inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)               ## return newly created object

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data ...")
                return(inv)  ## if inverse of matrix has been caculated, 
                             ## fetch cached inverse
        }
        matrice <- x$get()
        inv <- solve(matrice, ...) ## calculate the inverse of matrix
        x$setinverse(inv)
        inv     ## Return a matrix 'inv' that is the inverse of 'x'
}
