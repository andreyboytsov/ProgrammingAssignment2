## The functions create infrastructure for caching the computations of
## inverted matrix. for the chosen matrix they create a list of functions,
## which allow manipulating tha matrix and its cached solved value.

## This function takes a matrix as an argument and returns
## a list of functions, which maintains the cached solved
## (inverted) matrix. The resulting list contains follwonig functions:
##  set - inputs new matrix.
##  get - retrieve current matrix
##  getsolve - retrieve cached value (NULL means no value is cached at the moment).
##  setsolve - manually input the solved matrix (if you happen to know it already)
##Use it only if you are sure that the proposed solved matrix is correct.
## Parameters:
##  m - the matrix
makeCacheMatrix <- function(m = matrix(1,1,1)) {

  # The variable 'cachedInversion' will contain cached inverted matrix.
  #At startup we don't know inverted matrix
  cachedInversion <- NULL
  
  ## This function replaces the matrix with the new one
  ## Parameters: newM - new matrix.
  set <- function(newM) {
    ## We are manipulating objects of the upper layer, so <<- operator is necessary
    m <<- newM # Replacing matrix with new value
    cachedInversion <<- NULL # Matrix was changed, cached value is no longer reliable
  }
  # This function gets the underlying matrix
  get <- function() { m }
  # This function returns solved matrix value
  setsolve <- function(solved) { cachedInversion <<- solved }
  # This function retrieves cached solve value (even null)
  getsolve <- function() { cachedInversion }
  # Resulting list contains the functions, defined above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
  
}


## This function solves the matrix and caches the value for further use.
## Parameters:
##   m - list of caching functions for chosen matrix.
## If you have some matrix 'matr', you can obtain caching functions
## using m <- makeCacheMatrix(matr), then the resulting 'm' will be the input for
## cacheSolve function.
##  You can input more parameters, which will be directly input into solve function.
## See ?solve for the list of additional parameters.
############## WARNING! #####################
## 1. Function is designed to compute and cache inverted matrices, not solving SLEs.
## You are strongly encouraged not to use b parameter.
## 2. tol (tolerance) parameter is NOT cached. If there is a cached value, which was computed with
## another tolerance parameter, cached value will be returned anyway.
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Let's see if we ave chaced value
  solved <- m$getsolve()
  ## NULL means no cached value, non-null value is the actually cached one.
  if(!is.null(solved)) {
      # Yes, we do! No need to calculate, return cached value.
      # print("getting cached solved matrix") # It is debug code. Comment out after testing!
      return(solved) # Cache worked. Return and go on.      
  }
  # If we reached that spot, than we don't have any cached solved values.
  # Let's get the matrix, calculate solved value and cache it.
  matr <- m$get() # Step 1. get the matrix.
  solvedMatrix <- solve(matr, ...) #Step 2. Solve it.
  ## Actually, fucntion solve is used for solving linear equaltions of AX=b type.
  ## However, if b is not provided, it returns inverted matrix.
  m$setsolve(solvedMatrix) #Step 3. Cache the solved value. 
  solvedMatrix #Return solved matrix.
}
