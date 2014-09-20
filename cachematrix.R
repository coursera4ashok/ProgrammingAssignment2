## Cache the expensive computation of inverse and retrieve
## whenever required

## Exposes three methods
## set -- set the value of matrix
## get -- returns the matrix
## setnv -- Cache the value of inv
## getnv -- retrieve the value of cached inverted matrix
##          returns NULL if inverse is not cached

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinv <- function(value) invMatrix <<- value
        getinv <- function() invMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## A method to solve the cache method
## This method uses the cache if one is available.

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

## test
m<-matrix(c(3,8,9,8,4,8,12,1,3),nrow=3,ncol=3,byrow=TRUE)
# Cache the vector
cm<-makeCacheMatrix(m);
m2<-cacheSolve(cm);

# just for testing solve m
m1<-solve(m)
# print m1
m1
# print m2
m2

# check if the values are the same
m1==m2
# this should print 9 (3x3) TRUE
