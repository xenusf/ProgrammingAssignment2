## Functions to cache inverse of a matrix to avoid repeated computation

## First function returns list of 4 functions that:
## 1) set the matrix
## 2) return the matrix
## 3) set the inverse of the matrix
## 4) return the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
## declare variable to store matrix inverse
  mx_inverse <- NULL
## function to set matrix to argument 'y'
## <<- operator ensures assignment outside function closure
  set <- function(y) {
    x <<- y
    mx_inverse <<- NULL
  }
## function to return matrix 'x'
  get <- function() x
## function to set matrix inverse, assign inverse to 'mx_inverse' outside function closure
  setinverse <- function(solve) mx_inverse <<- solve
## function to return matrix inverse
  getinverse <- function() mx_inverse
## return list of the four functions created above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Second function checks whether matrix inverse already calculated,
## returns cached inverse if so, otherwise computes and returns inverse

cacheSolve <- function(x, ...) {

  ## get inverse of x, if already calculated
  mx_inverse <- x$getinverse()
  ## return cached inverse, end function if cached data exists
  if(!is.null(mx_inverse)) {
    message("getting cached data")
    return(mx_inverse)
  }
  ## assign matrix contents to object 'data'
  data <- x$get()
  ## compute matrix inverse, assign to 'mx_inverse'
  mx_inverse <- solve(data, ...)
  ## cache computed inverse
  x$setinverse(mx_inverse)
  ## return inverse
  mx_inverse
  
  
}
