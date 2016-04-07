## R-Programming Assignment week 3 - cached Matrix - C. K. - Assigment completed on 4/7/2016 
## The following functions cache the evaluation of the inverse of a matrix. If the inverse has not been 
## calculated yet it computes the results and caches them. If the inverse has already been computed it 
## reads the stored (cached) value and provides it
## One way to test the functions is as follows
## Creating a matrix in the workspace e.g. y<-matrix(rnorm(1:25), nrow=5,ncol=5)
## cached_m <- makeCacheMatrix(y)
## cacheSolve(cached_m) when called forthe first time, will compute the inverse of cached_m
## cacheSolve(cached_m) when called the 2nd (or subsequent) time(s) should print the same result but
## with the message "getting cached data" which indicates that the data has been retrived from a previously
## cached computation
## 
##Warning: This method works only if the created matrix has an inverse. This matrix will not exist if
## columns (or rows) are the same or linear combinations of each other, thus beware of creating matrices 
## containing a same elements or an enumeration from 1 to a square of an integer "a" in a "a x a" matrix like
## in z<-matrix(1:25, nrow=5,ncol=5). The latter value of z has no inverse. Tip: Normal randomized values
## will yield inversible matrices more often than not.

## The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse<- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the setInverse function.

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
    m
}
