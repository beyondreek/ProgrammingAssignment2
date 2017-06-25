##This script creates two functions
####
####makeCacheMatrix() to cache matrix , its inverse and
###
####cacheSolve() that calculate the inverse of the matrix
####
####
####
###You would use type in console as follows:-
####
####Say you create a square matrix-
####
####      mat<-matrix(c(5,6, 7, 8, 9, 11,13,15,16), nrow=3,ncol=3)
####
####Then you call makeCacheMatrix() passing  the matrix "mat" to it and store the output in "leca" :-
####
####      leca<-makeCacheMatrix(mat)
####
####Then you call  cacheaSolve() by passing "leca" to it and storing the output in "doka" :-
####
####      doka<-cacheSolve(leca)
####
####
####To verify if the inverse in "doka" is correct, type in "print(mat%*%doka)
#####
#####
#####In summary, type in the following in console to check the functions:
####
#### 
####      mat <- matrix(c(3,4,5,6), nrow=2,ncol=2)
####      leca <- makeCacheMatrix(mat)
####      doka <- cacheSolve(leca)
####      print(leca$get()%*%doka)

## The following makeCacheMAtrix()- caches the matrix , its inverse and
############################returns a list containing functions to:
################            1. set a matrix
################            2. get a matrix
################            3. set the inverse of the matrix in step 1
################            4. get the inverse of the matrix
## for this script we will assume the input matrix is square and invertible


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        #The variable "get" contains the inpuit matrix "x"
        get<-function() x
        
        # Records the variable "solve" in "i"
        setinverse<-function(solve) i<<-solve
        
        # Returns the variable "i"
        getinverse<-function() i
        
        #Returns a list of the outputs of the fuctions set, get, setinverse and getinverse
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## This function takes a square , invertible matrix. If there is an inverse in cache already , 
##it returns the cached value. Else, it calculates the matrix inverse, caches the result
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## "x" is the output of makeCacheMatrix()
        ##Return a inverse matrix of the input to the function makeCacheMatrix()
        
        i<-x$getinverse()
        
        
        ##If the inverse has been calculated already, it retrieves the result from cache
        if(!is.null(i)){
                message("getting cahced data")
                return(i)
        }
        
        
        data<-x$get()
        
        ##Calculate the inverse of the input matrix to the function makeCacheMatrix() 
        i<-solve(data,...)
        
        ##Cache the calculated inverse
        x$setinverse(i)
        
        ##Return the inverse
        i

        
}
