## This file contains two functions:
## 
## The first function defines an environment containing 
## a matrix and it's inverse as well as functions for setting
## and getting the matrix data and the corresponding inverse
## matrix data

## The first function defines the environment containg the 
## functions "pointers" as well as the data

makeCacheMatrix <- function(matrixData = matrix()) {
        # Initialy the inverse of the matrix is NOT DEFINED
        matrixInverse <- NULL
        # Define a function for storing the matrix itself
        setMatrixData <- function(tmpVar) {
                # Set the matrix data
                matrixData <<- tempVar
                # The inverse is still undefined
                matrixInverse <<- NULL
        }
        # Then define a function to return the matrix itself
        getMatrixData <- function() matrixData
        # Then define a function to store the inverse of the matrix
        setInverseMatrixData <- function(tmpVar) matrixInverse <<- solve(tmpVar)
        # Then define a function to return the inverse of the matrix
        getMatrixInverseData <- function() matrixInverse
        # Finally return this environment
        list(setMatrixData = setMatrixData,
             getMatrixData = getMatrixData,
             setInverseMatrixData = setInverseMatrixData,
             getInverseMatrixData = getMatrixInverseData)
}


## The second function is used to call the different functions defined 
## in the object above in such a way that if the matrix inverse
## is ALREADY SET it will not be recalculated

cacheSolve <- function(x, ...) {
        ## First check of an inverse has already been calculated
        matrixInv <- x$getInverseMatrixData()
        ## If it is defined we do not need to calculate it
        if(!is.null(matrixInv)) {
                message("Getting the already cached inverse matrix!")
                return(matrixInv)
        }
        ## If not, we need to calculate it
        matrixData <- x$getMatrixData()
        matrixInv <- x$setInverseMatrixData(matrixData, ...)
        # Finally return the inverse
        matrixInv
}
