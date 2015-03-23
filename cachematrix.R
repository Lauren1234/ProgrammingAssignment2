## The following function creates a list of functions that create the 
## "special" matrix. This is a matrix object that can be read and written
## to by using the set and get functions.
makeCacheMatrix <- function (x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        ## The output of this function will be a list of the setinv and getinv functions.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
cacheSolve<-function(x) {
        ## Looking to see if the inverse of x has already been defined.
        inv <- x$getinv()
        ## If the inverse of the matrix already exists then the message "getting cached data"
        ## will appear.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getinv()
        ## Takes the inverse of the x$get() matrix, if the matrix is not null.
        inv <- solve(x$get())
        ## Sets the inverse into the object "x" so that the next time "x" is called, cacheSolve
        ## will take the inverse.
        x$setinv(inv)
        ## "inv" will eturn the inverse as the output.
        inv
}

