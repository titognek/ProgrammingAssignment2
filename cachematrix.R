## The function makeCacheMatrix generates an object cointaining 4 lists:
## "get"      "getsolve" "set"      "setsolve" which are used to set and get either the imput matrix
## or the inverse matrix calculated with solve().

makeCacheMatrix <- function(x = matrix()) { # define the function to create an object that will be used in the funtion cacheSolve
        m <- NULL # initialize the variable m in the makeCacheMatrix environment
        set <- function(y) { # define the set function in the parent environment
            x <<- y # the set function assigns the input variable y to x
            m <<- NULL # and initialize m
        }
        get <- function() x # define the get function that retrieves the input matrix x from the parent environment
        setsolve <- function(solve) m <<- solve # define the funtion to set the inverse matrix object found in the parent environment to be assigned to m
        getsolve <- function() m # define the get funtion to retrieve the inverse matrix object
        list(set = set, get = get, # return the makeCaheMatrix objects as a list
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function cacheSolve checks if an inverse matrix of the input has been already calculated and stored in the memory
## and if exist, it return the object, if not it calculates and returns it.

cacheSolve <- function(x, ...) { 
        m <- x$getsolve() # assign the inverse matrix calculated from the input matrix x to the variable m, 
        
        if (!is.null(m)) { # if the inverse matrix of input x was already calculated and stored in the memory, this objects is retrieved
            message("getting cached data") # from the memory and returned with out wasting computational resourses. The message will be displayed.
            return(m) # if m is Null, thus the invese matrix was not calculated from the imput x or a new input matrix is provided
        }
        
        data <- x$get() # the input matrix is retrieved and passed to the variable data
        m <- solve(data, ...) # the inverse matrix is calculated
        x$setsolve(m) # and passed to the m element
        m # and returned as output of the computation
}

matx <- matrix(c(1,0,5,2,1,6,3,5,0), nrow = 3, ncol = 3) # define an invetible matrix 
aMatx <- makeCacheMatrix(matx) # run the first function with the invertible matrix as input
cacheSolve(aMatx) # run the secon function with the returned object from the first function to 
                # obtained the output with the inverse matrix of the input.
