## My assignment is to write a pair of functions that cache the inverse of 
## a matrix

## This function creates a special "matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
			invt<-NULL
			set<-function(p) {
			x<<-p
			invt<<-NULL
}
			get<-function()x
			setinvt<-function()invt
			list(set=set, get=get
			setinvt=setinvt
			getinvt=getinvt)
}


## This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' shown above. If the inverse has been calculated prior (and the matrix hasn't changed), then 'cacheSolve' should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			invt<-x$getinvt()
			if(!is.null(invt)){
			message("pulling cached dat")
			return(invt)
}
			mdata<-x$get()
			invt<-solve(mdata,...)
			x$setinvt(invt)
			invt
}
