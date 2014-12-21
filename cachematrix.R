## makeCacheMatrix and cacheSolve compute the inverse of the matrix inputted and 
## caches the matrix and inverse of the matrix computed.

## makeCacheMatrix store the matrix inputted as well as the inverse of the matrix computed

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL # set the matrix value to NULL
  set <-function(y){ # set the matrix value
    x <<-y          ## caches the matrix imputted
    m<<- NULL       ## set the matrix value to NULL
    
  }
  get <-function() x # get the matrix inputted
  setmat <- function(solve) m<<- solve #cache the inverse of the matrix computed
  getmat <- function() m # get the cached inverse of the matrix
  list(set = set, get = get, 
       setmat = setmat,
       getmat = getmat)
  

}


## cacheSolve checks to see if the matrix has been inputted before. If the matrix has been cached, the function will return the 
## inverse of the matrix that has been cached. Otherwise, it will compute the inverse of the matrix inputted.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
  m <- x$getmat() #assign the inverse of matrix to m 
  if(!is.null(m)){    #check if inverse has been cached
    message("getting cached data") 
    return(m)         # return the cached inverse
  }
  data <- x$get()        # run the getinverse function to get the value of matrix inputted
  m <- solve(data, ...)  # compute the value of the inverse of the matrix
  x$setmat(m)        # cache the inverse computted
  m                      # return the inverse
}
