## Matrix inversion is usually a costly computation so we use two functions
## to cache the inverse of a matrix to reuse it at a later stage.


## The function makeCacheMatrix creates a list which includes a function
## that set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix and 
## get the value of the inverse of the matrix

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

##This function returns the inverse of the matrix after checking
##if the inverse already exists. If it exists, then it takes it returns it 
## from cache. If it doesn't, it computes the inverse and sets the value
##in the cache

cacheInv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("retrieve cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


