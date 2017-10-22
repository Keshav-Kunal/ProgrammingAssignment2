## Below are two functions that help compute the inverse of a matrix.
## In case, the inverse has already been calculated and the same matrix is passed,
## the program returns the inverse from the cache

## 'makeCacheMatrix' takes in an invertible matrix
## and creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function(){
                x
        }
        setInverse<-function(invr){
                i<<-invr
        }
        getInverse<-function(){
                i
        }
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## 'cacheSolve' calculates the inverse of the special matrix returned by the 'makeCacheMatrix' function above.
## If an matrix identical to the previous one is passed, then 'cacheSolve' retrieves its inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getInverse()
        ## Return a matrix that is the inverse of 'x'i
        if(!is.null(i)){
                message("getting cached matrix")
                return(i)
        }
        originalMatrix<-x$get()
        i<-solve(originalMatrix)
        x$setInverse(i)
        i
}

##E.g.
##>source("cachematrix.R")
##>abc<-makeCacheMatrix(matrix(c(3,2,0,0,0,1,2,-2,1),3,3))
##>cacheSolve(abc)
##     [,1] [,2] [,3]
##[1,]  0.2  0.2    0
##[2,] -0.2  0.3    1
##[3,]  0.2 -0.3    0
##
##>cacheSolve(abc)
##getting cached matrix
##     [,1] [,2] [,3]
##[1,]  0.2  0.2    0
##[2,] -0.2  0.3    1
##[3,]  0.2 -0.3    0
