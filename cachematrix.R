# This pair of functions is used to cache the inverse of a matrix
# Written by: Viridis Liew
# A submission for Coursera R Programming Assignment 2



# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(origMatrix = matrix()) {

    inverseMatrix <- NULL  ##variable to store the inversematrix after the solve

    # Function to set the original matrix if user calls makeCacheMatrix$set()
    set <- function(inputmatrix) {
        origMatrix <<- inputmatrix ##set what the matrix user passed as argument to the origMatrix
        inverseMatrix <<- NULL ##Since new matrix set the inversematrix as NULL
    }

    # Function to return the origMatrix
    get <- function() {
        origMatrix
    }

    # Function to calculate the inverseMatrix using the solve R function and store in inverseMatrix
    setInversematrix <- function(solve) {
        inverseMatrix <<- solve # Apply the solve R function and store result in inverseMatrix

    }

    # Returns the inverseMatrix calcuated from the function
    getInverseMatrix <- function() {
        inverseMatrix
    }

    # Store the functions and variables defined in a list in memory
    return(list(set = set, get = get, setInversematrix = setInversematrix, getInverseMatrix = getInverseMatrix))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache (list in the makeCacheMatrix)

cacheSolve <- function(x, ...) {

    #check if there is an inverseMatrix already calculated in the matrix passed to function
    inverseMatrix <- x$getInverseMatrix()

    # check if the inverseMatrix is NOT NUll = inverse has already been calculated and in memory
    if(!is.null(inverseMatrix)) {
        message("Getting cached inverse matrix")
        return(inverseMatrix) # Return the inverse matrix in memory and break out of function
    }
    else
    {
        # calculate the inverse of matrix and store inverse matrix in memory
        origMatrix <- x$get() # get the matrix from the makeCacheMatrix function in the list
        inverseMatrix <- solve(origMatrix, ...) #calculate the inverse matrix
        x$setInversematrix(inverseMatrix) # set the calculated inverse matrix to list in memory
        return(inverseMatrix) # Return the inverse matrix
    }


}
