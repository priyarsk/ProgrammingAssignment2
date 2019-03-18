makeCacheMatrix<-function(x=matrix()){ ##define the argument as matrix 
  inv<-NULL                            ## give the inv as null
  set<-function(y){                    ##set function is assigned
    x<<-y                              ## Parent environment
    inv<<-NULL                        
  }
  get<-function()x                           ## get the value of the matrix
  setinverse<-function(inverse)inv<<-inverse  ## set the value of invertible matrix
  getinverse<-function()inv                   ## get the value of invertible matrix
  list(set=set,
       get=get,                               ## helps to find the $function opertion
       setinverse=setinverse,
       getinverse=getinverse)
}

##This function cachesolve will bring the output of the previous Matrix and check the inverse 
##matrix from makeCacheMAtrix. If Inverse matrix is empty, then it gets the original matrix data
##and set the invertible matrix by using the solve function. If the inverse matrix has some 
##value in it, it returns a message as "getting cached down"

cachesolve<-function(x,...){  ##get the value of the invertible matrix from above           
  inv<-x$getinverse()
  if(!is.null(inv)){          ## if inverse matrix is not null
    message("getting cached inverse") ## type msg as "getting cached inverse"
    return(inv)                        ## return the matrix
  }
  mat<-x$get()              ## get the original data
  inv<-solve(mat, ...)      ## use solve function to inverse
  x$setinverse(inv)         ## set the invertible matrix
  inv                       ## return
}