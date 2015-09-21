## Coursera:R Programming Assignment 2

##makeCacheMatrix function creates a special "matrix", which
##really a list containing a function to
## 1) set the value of the matrix 
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
                }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set=set, get=get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## cacheSolve function calculates the inverse special "matrix"
## created with the above function. But it returns the saved
## inverse matrix from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
            s <- x$getinverse()
            if(!is.null(s)){
                    message("getting cached data")
                    return(s)
                        }
            data <- x$get()
            s <- solve(data, ...)
            x$setinverse(s)
            s
}
