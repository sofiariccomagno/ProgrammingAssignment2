## The first function makes a cache matrix and stores its inverse in the cache memory,
## while the second goes through the cache memory to see if the inverse had already been
## computed so that it can be printed without the need to be computed again.

## makeCacheMatrix is a function that
## 1. sets the matrix (set)
## 2. gets the matrix (get)
## 3. sets the inverse of the matrix (set.inverse.matrix)
## 4. gets the inverse of the matrix (get.inverse.matrix)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  set.inverse.matrix <- function(solve) im <<- solve
  get.inverse.matrix <- function() im
  list(set = set, get = get,
       set.inverse.matrix = set.inverse.matrix,
       get.inverse.matrix = get.inverse.matrix)
}

## cacheSolve computes the inverse matrix of the matrix created by the makeCacheMatrix
## function, but it first checks the cache memory, in case it had been done before.
## If the matrix is already in the cache memory, it won't be computed again. 
## If not, it computes with the set.inverse.matrix function.

cacheSolve <- function(x, ...) {
  im <- x$get.inverse.matrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$set.inverse.matrix(im)
  im
}


