## This assignment is to build 2 functions
##1.) makeCacheMatrix function is to create a special "matrix" object
##2.) cacheSolve function to compute the inverse of the special "matrix" 
##    OR to retrieve the inverse from the cache if the "matrix" was
##    calculated (and the matrix has not changed)


## makeCacheMatrix function is built for creating a special "matrix" to run the following:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the inverse of matrix
##4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function (y){
          x<<-y
          inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inverse<<-solve
        getinverse<-function() inverse
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## from the makeCacheMatrix above. This function first check the inverse 
## hase already been calculated (and the matrix has not changed), then
## then the cachSolve function should retrieve the inverse of matrix the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
