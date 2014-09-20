##The two matrices work together to take an input(X) convert it to a square matrix and calulate the inverse.

##The makeCacheMatrix takes an input value converts to a square matrix and can cache it environment with the value of the inverse matrix and input matrix for easy retrival.
makeCacheMatrix<- function(x) ##input that is convertible to a square matrix.
{
  mat<- matrix(x,2,2) ##converts input(x) to a square matrix(2*2). Can change the dimensions to any square matrix
  m <- NULL  ##inverse value defaults to a null before it calculated, but can receive set value from cacheSolvec alculated.
  set <- function(y) {  
    y1<<- matrix(y,2,2)  ##converts input(y) to a cached square matrix
    mat <<- y1         ##feeds the cached matrix to mat matrix.
    m <<- NULL        ##provides the cached 'null' inverse value fo the new input above before the cachesolvematrix is called.
  }
  get <- function() mat  
  set_inv <- function(solve) m <<- solve 
  get_inv <- function() m  ##gets the input value as a matrix(mat)
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

##This function gets the inverse of the matrix from the makeCacheMatrix, it will return the cached inverse is the input matrix haas been calculated befor and is in cache.
cacheSolve <- function(mat, ...) {   
  m <- mat$get_inv()    ##solves for the inverse of X
  if(!is.null(m)) {
    message("getting cached data") ##message to return if inverse is already calculated and in in cache
    return(m)
  }
  data <- mat$get()    ##gets the mat matrix
  m <- solve(data, ...) ## solves for the inverse
  mat$set_inv(m)        ##sets the claculated inverse to the above matrix
  m  ##returns the calculated inverse.
}
