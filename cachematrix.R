# The next couple of functions help calculate the inverse matrix of a
# matrix object 'x' and retrieve that inverse matrix from a cached envi-
# ronment inside the cacheSolve function, if and when the value of x 
# does not change. This is particularly useful if the result of the 
# chacheSolve function is stored onto an object and you want to call up
# this object repetitively. If the calculated inverse were not cached,
# the saved object would need to calculate the inverse each time. See
# reproducible example below.


## The first function sets a null object to which the calculated inverse
# will be assigned eventually. Then, creates a series of fucntions. 
# The first of these, assigns the matrix object indicated as
# argument to an object inside the function environment called 'x'. The get
# function, returns the matrix object 'x'. Lastly, setinv and getinv are
# functions which assign an argument to the object 'I' and return it. The
# output of the makeCacheMatrix is a list of local functions and a local
# matrix object. To take advantage of closure scoping, you may want to 
# save the output to a new object.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) I <<- inverse 
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The second function takes as argument the object to which the first 
# function's output was assigned. This function assigns a value to I,
# which should be NULL if it is the first time the inverse is to be 
# calculated for x. Then, an object is created in the local envirnment
# to which the matrix oject is saved. The inverse is then calculated and 
# stored locally to 'I'. Note how the setinv function caches 'I' into 
# the cacheSolve envirnment as 'I'. If 'I' already exists (and hence 
# its value has not changed) a message is returned signalling that I had 
# was not recalculated, but returned from the cache environment. 

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data) %*% data
  x$setinv(I)
  I      
}

# Reproducible example:

x <- matrix(rnorm(25),5,5)

test <- makeCacheMatrix(x)

cacheSolve(test)

