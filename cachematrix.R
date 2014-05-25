## The functions makeCacheMatrix and cacheSolve attempt to cache the inverse of a matrix
## This is an assignment homework for the course "R programming" by Coursera
## Please see https://www.coursera.org/course/rprog

## This function creates an object from a matrix
## set: sets the matrix which is inverted
## get: gets the matrix which is inverted
## setinv: sets the inverted matrix
## getinv: gets the inverted matrix

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
