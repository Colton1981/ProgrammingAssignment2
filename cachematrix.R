## The following two functions cache the inverse of a matrix

## makeCacheMatrix: Function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(
      set=set, 
      get=get, 
      setinverse=setinverse, 
      getinverse=getinverse
      )

}


## cacheSolve: function computes the inverse of the special matrix

cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data.")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
        
}
