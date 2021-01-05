# Programming Assignment 2: Lexical Scoping
# makeCacheMatrix: This function creates a special "matrix" 
#  object that can cache its inverse.
makeCacheMatrix<-function(x=matrix()) 
{ mtxi <-NULL

set<-function(y) 
{ x<<-y
mtxi<<-NULL
}

get<-function() x 
setinverse<-function(inv)
  mtxi<<-inv 
getinverse<-function()
  mtxi 
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix"
cacheSolve<-function(x, ...) 
  
{ mtxi<-x$getinverse()
if(!is.null(mtxi)) 
{ message("Loading...")
  return(mtxi) }
mat <- x$get()
mtxi <- solve(mat, ...)
x$setinverse(mtxi)
mtxi
}


