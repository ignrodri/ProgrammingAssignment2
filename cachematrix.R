## The functions makeCacheMatrix and cacheSolve attempt to cache the inverse of a matrix
## This is an assignment homework for the course "R programming" by Coursera
## Please see https://www.coursera.org/course/rprog

## This function creates an object from a matrix
## set: sets the original matrix
## get: gets the original matrix
## setinv: sets the inverted matrix
## getinv: gets the inverted matrix

## Warning: the setinv function will set the inverse irrespective on whether the result is correct or not

## To clear the cache, get and set the original matrix, as in this example
## m <- makeCacheMatrix(mat)
## x1 <- cacheSolve(m)
## m$set(m$get())
## x1 <- cacheSolve(m)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function inverts a matrix
## The matrix "mat" has to be passed as an argument as cacheSolve(makeCacheMatrix(mat))
## Example:
## m <- makeCacheMatrix(mat)
## invmat <- cacheSolve(m)

## NB: Additional arguments passed to cacheSolve are passed on to solve
## These additional arguments are ignored if an inverted matrix has already been precomputed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
