
## Returns a list of functions that set the value of matrix,get the vale of matrix,
## set the value of inverse and get the value of inverse. 
## (This is the list of parameters required by the function - cachesolve() )

makeCacheMatrix<-function(x=matrix()) {
        #to store the cached inverse matrix
        i<-NULL
        
        #to set the value of the matrix
        set<-function(y) {
                x<<-y
                i<<-NULL
        }

        #to get the value of the matrix
        get<-function()x
        
        #to set the value of mean
        seti<-function(inverse)i<<-inverse

        #to set the value of mean
        geti<-function()i

        #return the matrix with created functions
        list(set=set,get=get,seti=seti,geti=geti)
}

## This computes the inverse matrix. It checks the cache for the inverse.
## If found in cache, it returns the value. Else it computes it and then return the value.
## (This increases efficiency as this function performs faster in a loop of repetitive datasets)

cacheSolve <- function(x, ...) {
        #get the value of i to check if it is present in cache
        i<-x$geti()
        
        #return the inverse if it is found in cache
        if (!is.null(i)) {
        #specify that it is taken from cache
        message("getting cached data")
        return(i)
        }

        #calculate the inverse if not in cache
        data<-x$get()
        i<-solve(data, ...)
        
        #cache the calculated inverse
        x$seti(i)
        
        #return the cached inverse matrix
        i
}
