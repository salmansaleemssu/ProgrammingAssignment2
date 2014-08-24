##The functions 1) makeCacheMatrix and 2)cacheSolve below, cache the inverse of a matrix rather than compute it repeatedly.

##makeCacheMatrix function below creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix())
{
        m<-NULL
        set<-function(y)
        {
                x<<-y
                m<<-NULL
        }
        get<-function(){x}
        setmatrix<-function(solvedvalue) 
        {
                m<<-solvedvalue
        }
        getmatrix<-function(){m}
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
        
}

## This function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve<-function(smartmatrix)
{
        #if inverse matrix is not calculated then find the inverse
        if(is.null(smartmatrix$getmatrix()))
        {
                print("New Calculation")
                smartmatrix$setmatrix(solve(smartmatrix$get()))
                return(smartmatrix$getmatrix())
        }
        else
        {
                print("Cached value")
                return(smartmatrix$getmatrix())
        }
}