
##assignment is to write a pair of functions that cache 
##the inverse of a matrix

## makeCacheMatrix - function to create a matrix can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) a <<- inverse
        getinverse <- function() a
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve - function which computes the inverse of matrix 
## which is returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
## Return a matrix that is the inverse of 'x'
        value <- x$get()
        a <- solve(value, ...)
        x$setinverse(a)
        a
}

## input matrix given should always be a square matrix and invertible



## "Testing functions"
##> y <- makeCacheMatrix(matrix(c(3, 6, 7, 1, 3, 6, 2, 9, 4), 3, 3))
##> y$get()
##     [,1] [,2] [,3]
##[1,]    3    1    2
##[2,]    6    3    9
##[3,]    7    6    4
##> y$getinverse()
##NULL
##> cacheSolve(y)
##           [,1]        [,2]        [,3]
##[1,]  0.7368421 -0.14035088 -0.05263158
##[2,] -0.6842105  0.03508772  0.26315789
##[3,] -0.2631579  0.19298246 -0.05263158
##> y$getinverse()
##           [,1]        [,2]        [,3]
##[1,]  0.7368421 -0.14035088 -0.05263158
##[2,] -0.6842105  0.03508772  0.26315789
##[3,] -0.2631579  0.19298246 -0.05263158