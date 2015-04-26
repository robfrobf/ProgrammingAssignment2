
## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse which we call "inverse".

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                         ## initialize matrix to NULL
        set <- function(y) {
                x<<- y
                inverse <<- NULL                ## initialize cached matix to NULL
        
                        }
        get<-function() x
        setmatrix<-function(solve) inverse <<- solve    ##Use Solve to compute matrix inverse
                                                        ## and cache it
        getmatrix<-function() inverse
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)  ##modeling example to 
                                                                         ## set and get
                                         }
 

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        inverse<-x$getmatrix()
        if(!is.null(inverse)){ 
                                        ##testing whether inverse exists
                message ("getting cached inverse matrix")
                return(inverse)
        }
        matrix<-x$get()
        inverse<-solve(matrix, ...)
        x$setmatrix(inverse)
        return(inverse)
}





