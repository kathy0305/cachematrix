## Writing a Pair of functions that can cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there is abenifit to caching the inverse
## of a matrix rather than computing it repeatedly.

## Our first function, will create a special "matrix" object that can cache its inverse.
## Computing the inverse of a square matrix can be done with the 'solve' function in R
## We are going to assume that the matrix supplied is always invertible.

## I will follow the exmaple provided by Prof.Peng
## of course in this case we r using a Matrix and not a vector. We also getting the inverse and not the mean


## In this function called 'makeCacheMatrix' we will be simply doing 5 things:
## Set the value of the Matrix
## Get the value of the Matrix
## Set the value of the Inverse Matrix
## Get the value of the Inverse Matrix
## Return a list of the methods


## This function creates a special Matrix object that can cache its object
makeCacheMatrix <- function(x = matrix()) {
        
        
        ## first we initiallize the cached matrix to null.
        m <- NULL
        
        ##Create or set the matrix
        ## will be using the <<- operator so that we can assign value to the matrix
        ## outside our working enviroment
        
        set <- function(y) {
                x <<- y
                m <<- NULL   ##again making sure we start with NUll
                
        }
        
        ## Get the function
        get <- function()
                x
        
        ## set the inverse matrix
        setInvMat <- function (inverse)
                ##populate m
                m <<- inverse
        
        ## get the inverse matrix
        getInvMat <- function()
                m
        
        ## now simply list them all
        list(set=set,get=get,setInvMat= setInvMat, getInvMat= getInvMat)
}



##this function computes the inverse of the special matrix returend by 'makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMat()
        
        ##if the inverse has already been calculated (and the matrxi has not changed).
        ## then 'cache solve should  the inverse from the cache
        if (!is.null(m)) {
                ## if m exist, then tell that we are retrieving the data
                message ("getting cached data")
                return (m)
        }
        
        ## if m does not exits then we need to calculate it.
        ## get the data and get the inverse via the 'solve' function
        
        data <- x$get()
        m <- solve(data, ...)
        
        ## now set the inverse to the matrix
        x$setInvMat(m)
        m
}


## To make sure it works we create a 2x2 invertible matrix

##        TestMatrix <- matrix (c(2,2,3,2), ncol = 2)

## Pass this matrix through function makeCacheMatrix and store the matrix 
##       TestMatrix2 <- makeCacheMatrix(TestMatrix)
##       TestMatrix2$get()  ## make sure we get the same matrix
##       > TestMatrix2$getInvMat()
##      [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0

##pass it through cacheSolve function to see if we get the inverse
##       cacheSolve(TestMatrix2)

##Lets see if it works with 3x3 matrix
##      TestMatrix3 <- matrix (c(1,0,5,2,1,6,3,4,0), ncol = 3)

##> Testmatrix4 <- makeCacheMatrix(TestMatrix3)

##> Testmatrix4$get()
##      [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> cacheSolve(Testmatrix4)

##   getting cached data

##      [,1]  [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1









