# List "vector" comprehension
library(comprehenr)

# Crear Vector con serie de Fibonacci
Fibo <-  function(limit){
  a <- 1
  b <- 1
  fibo <- append(c(NULL), a)
  fibo <- append(fibo, b)
  for (i in 1:(limit-2)){
    c <- a + b
    fibo <- append(fibo, c)
    a <- b
    b <- c
  }
  return(fibo)
}

# Vector creado por el usuario
Rand <- function(start, end, size){
  return(sample(start:end, size = size))
}

# Encontrar coincidencia entre los dos vectores
Nums <- function(fibo, user){
  nums <- to_vec(for(i in fibo) if(i %in% user) i)
  return(nums)
}

# Encontrar la posicion de la lista de los numeros de Fibonacci en el vector del usuario
Ind <- function(nums, fibo){
  ind <- to_vec(for(i in nums) match(i, fibo))
  return(ind)
}

# Funcion de manejo de errores
Errors <- function(start, end, size){
  if(start < 2){
    stop("El número de inicio debe ser mayor a 2")
  }
  if(start >= end){
    str <- "El número final debe ser mayor al número de inicio."
    str2 <- "El número de inicio ingresado fue"
    str3 <- "y el final fue"
    error <- paste(str, str2, as.character(start), str3, as.character(end), sep = " ")
    stop(error)
  }
}

# Dataframe de coincidencias
DFCoin <- function(nums, ind){
  print(nums)
  print(ind)      
  df <- data.frame(
    "num_fibo" = nums,
    "pos_serie" = ind
  )
  return(df)
}

# Funcion principal
Main <- function(){
  start <- as.integer(readline(prompt = "Ingrese el número de inicio (debe ser mayor a 2): "))
  end <- as.integer(readline(prompt = "Ingrese el número final: "))
  size <- as.integer(readline(prompt = "Ingrese el tamaño del vector: "))
  Errors(start, end, size)
  
  fibo <- Fibo(end)
  user <- Rand(start, end, size)
  
  fibo_coin <- Nums(fibo, user)
  pos <- Ind(fibo_coin, fibo)
  
  df <- data.frame(
    "num_fibo" = fibo_coin,
    "pos_serie" = pos
  )
  print(df)
}

Main()
