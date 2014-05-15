## MakeCacheMatrix take a invertible matrix to make a special object store 4 functions
## cacheSolve use the object returned by MakeCacheMatrix to caculated the reverse of 
## the matrix. if calculated,get it from cache.

## Make a special list with four functions.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <-function(y){
                x<<-y
                inv<<-NULL
        }
        get <-function(){
                x
        }
        setinv<-function(inverse){
                inv<<-inverse
        }
        getinv<-function(){
                inv
        }
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}


## Return a matrix that is the inverse of X, if cacluted, get it from cache

cacheSolve <- function(x, ...) {
        #get the inverse from cache
        inv<-x$getinv()
        
        #if inverse is gotten from cache, return it.
        if(!is.null(inv)){
                message("Getting cached inverse")
                return(inv)
        }
        
        #If not calculated, get matrix from the special matrix x
        d<-x$get() 
        
        #calcuting inverse
        inverse<-solve(d) 
        
        #cache the cacculated inverse. 
        #Since <<- used, assign value to inv 
        #which is in the environment of MakeCacheMatrix()
        x$setinv(inverse) 
        
        inverse
}
