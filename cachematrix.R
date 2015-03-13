## The functions written below will calculate the inverse of a matrix if the matrix is new or will return the inverse
## if for that particular matrix it has already been calculated. The essence of it lies in using the 
## scoping rules in R to make use of a stored input of an object from one environment in another environment.
## The value of the object will remain intact even though we move out of the environment in which it was defined. 

## This function creates a list of 4 functions which contains the function that will create the matrix of which
## we need to calculate the inverse of and also contains the function which HOLDS the value of the previously
## calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes an input in the form of the list created in the previous function. Using the listed 
## functions inside the makeCacheMatrix function, this function first checks if the object created 
## using makeCacheMatrix is already HOLDING the value of the inverse needed.
## If so, it returns the 'cached' value. This makes use of the scoping in R.
## If there is no value held or the matrix of which we need the inverse is new, then it calculates the inverse
## making use of the new matrix it gets from the get function inside the object created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
                i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
