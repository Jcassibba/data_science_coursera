##These two functions will create a matrix and return its inverse

##this function will create a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function (y){
              x <<- y
              z <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## this function will take the matrix created in makeCacheMatrix
##and return its inverse

cacheSolve <- function(x,...) {
        z <- x$getInverse()
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(z)
        i
}
