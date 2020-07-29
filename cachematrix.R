## The file contain two functions which can cache the inverse of a matrix.


## Function makeCacheMatrix can make a special matrix which contain the values of matrix and inverse of that matrix.
## 1. a <- makeCacheMatrix(matrix) creates a cached matrix.
## 2. a$set(matrix) changes the stored matrix.
## 3. a$get() returns the stored matrix.
## 4. a$setinverse(matrix) changes the inverse of the matrix
## 5. a$getinverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv<<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function cacheSolve returns the inverse of the matrix
## 1. If the inverse has already been calculated, get the inverse from the cache and skip computation.
## 2. Calculate the inverse of the matrix and sets the elements of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        Inv<- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv<- solve(data, ...)
        x$setinverse(Inv)
        Inv
}
