
funcion <- function()
{
  for (m in c("u","v")){
    w <- 2  # objeto local (a la función)
    cat("Valor de w = ",w,"\n")
    z <- get(m) # recupera y asigna el objeto global de nombre "u"
    print(lm(z[,w] ~ z[,w-1]))
  }
  cat("Fin del programa","\n")
}

dos <- function(u)
{
  u <<- 2*u  # Uso de la super-asignación mediante <<-
  z <- 2*z   # asignación local, nótese que hace uso de la global z
  w <- z     # asignación local a partir de z local
  return(list(u.local=u,z.local=z,w.local=w))
}

IncX <- function()
{
  inc <- function() {
    
    x <<- x+1
    cat("3. en funcion INC x= ",x,"\n")
  }
  
  if(exists("x")) {
      cat("1. Sí existía x= ",x,"\n") }
  else
    {
    x <- 3
    cat("2. No existía x, se asigna: ",x,"\n")
    }
  

  inc()
  return(x)
}

contador<-function(){
  cont<-0
  
  f<-function(){
    cont <<- cont+1
    cat("Este conteo tiene el valor...",cont,"\n")
    
  }
  #z<-f()  #SVB
  #return(z) #SVB
  return(f)   # Regresa una función, por lo que debe ser asignada a una variable antes de usarla
}


guardarMatrizInversa <- function(x = matrix()) {
  m <- NULL
  
  asignar <- function(y) {
    x <<- y
    m <<- NULL   # cada vez que se inicializa x (matriz ori), m (inversa) se limpia
  }
  
  obtener <- function() x
  asignarinv <- function(inv) m <<- inv
  obtenerinv <- function() m
  list(asignar = asignar, obtener = obtener,
       asignarinv = asignarinv,
       obtenerinv = obtenerinv)
}


calcularMatrizInversa <- function(f) {
  m <- f$obtenerinv()
  if(!is.null(m)) {
    message("Recuperando la inversa previamente calculada")
    return(m)
  }
  matriz <- f$obtener()
  m <- solve(matriz)
  f$asignarinv(m)
  m
}

####------------

makeVector <- function(x = numeric()) {
  m <- NULL
  
  set <- function(y) {
    cat("Antes x= ",x,"\n")
    x <<- y
    cat("Después x= ",x,"\n")
    m <<- NULL           #siempre que se cambia x, se inicializa m (mean)
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  cat("Cached Mean= ", m, "\n")
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}



##-MODULO 2 ASSIGMENT 2-----------
  
##
## My function "makeCacheMatrix" creates a special "vector", which is really a list containing a function to:
##    1. set the value of the matrix ... (set)
##    2. get the value of the matrix ... (get)
##    3. set the inverse of the matrix ... (setInv)
##    4. get the inverse of the matrix ... (getInv)
##
## My function "cacheSolve" calculates the Inverse of the special "vector" created with the "makeCacheMatrix" 
##    function. However, it first checks to see if the Inverse has already been calculated. If so, it gets the 
##    Inverse from the cache and skips the computation. Otherwise, it calculates the Inverse of the data (the 
##    matrix) and sets the value of the Inverse in the cache via the setInv function.
##

## makeCacheMatrix ------------------------------------
##    This function creates 4 functions and returns them in a list, so that they can be called in the global 
##    environment, once this function is assigned to a "variable"; for example, let's say:
##    > f<-makeCacheMatrix()
##    > m1<-matrix(1:9,ncol=3)
##    > f$set(m1)    # stores the matrix "m1"
##    > f$get()      # gets the matrix that is stored
##    > f$setInv(Invm1)    # stores the matrix "Invm1", assuming that it is the inverse of the matrix "m1" 
##    > f$getInv()   # gets the inverse of the matrix that has been stored via the "setInv" function

makeCacheMatrix <- function(x = matrix()) {
  m= NULL  # Initializing the inverse of the matrix
  
  set<-function(y){    # this function stores the matrix given as argument "y"
    x<<-y
    m<<-NULL           # each time this function is called, the "inverse" (m) is initialized
    
  }
  get<-function() x    # this function will retrieve the matrix that was stored using the "set" function
  
  setInv<-function(invX)  m<<-invX  # this function stores the matrix given as argument "invX", assuming that it 
  # is the inverse of the matrix that was stored with the "set" function
  getInv<-function() m # this function will retrieve the "inverse" matrix that was stored using the "setInv" function 
  
  list(set=set, get=get, setInv = setInv, getInv = getInv) # a list with the functions is returned 
  
}

## cacheSolve ------------------------------------
##    This function checks if the inverse of the matrix is already stored and returns it, otherwise it:
##      1. retrieves the matrix that was stored with the "set" function
##      2. calculates its inverse
##      3. stores the inverse of the matrix using the "setInv" function
##      4. returns the inverse matrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInv()  # retrieves the "inverse" of the matrix
  if(!is.null(m)){   # validates if the inverse already exists to return it 
    message("Getting cached data")   
    return(m)
    
  }
  
  data<-x$get()  # if the inverse isn't stored already,retrieves the matrix that was stored with the "set" function
  m<-solve(data) # calculates its inverse
  x$setInv(m)    # stores the inverse of the matrix using the "setInv" function
  m              # returns the inverse matrix
  
}
