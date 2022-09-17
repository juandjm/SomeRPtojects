# 1. CALCULO DE BONOS----------------------------------------

# Manejo de fechas
library(lubridate)

# Transformar numeros en notacion de dinero
library(formattable)

# Convertir string a fecha
Date <- function(date){
  return(as.Date(date, '%d/%m/%Y'))
}

# Retornar los dias entre la fecha de negociacion y vencimiento
DaysCalc <- function(start, end){
  start <- Date(start)
  end <- Date(end)
  # Manejo de errores, no pueden haber dias negativos
  if(start > end){
    stop("La fecha de negociación debe ser menor a la de vencimiento del bono")
  }else{
    return(as.integer(end - start))
  }
}

# Calcular la cantidad de cupones entre la fecha de negociacion y vencimiento
Coup <- function(start, end){
  start <-  Date(start)
  end <-  Date(end)
  days <- DaysCalc(start, end)
  
  # Correcion de la contidad de cupones debido a la variacion de dias en algunos años
  if (substring(end,9,10) == substring(start,9,10) & substring(end,6,7) == substring(start,6,7)){
    return(floor(days/365))
  }
  return(ceiling(days/365))
}

# Calcular valor de los cupones
CoupValue <- function(face_value, principal){
  return(principal*face_value)
}

# Calcular fecha de pago del primer cupon
CoupDate <- function(coups, end){
  periods <- coups - 1
  date <- Date(end) - 365*periods

  # Correcion de fecha del primer cupon en caso que su dia no coincida con la fecha de vto,
  # debido a los años bisiestos
  if (substring(date, 9,10) != substring(Date(end), 9, 10)){
    date <- paste(substring(end, 1, 2), substring(date, 6,7), substring(date, 1, 4), sep = "/")
    return(date)
  }else{
    date <- paste(substring(end, 1, 6), substring(date, 1,4), sep = "")
    return(date)
  }
}

# Calcular valor de giro
PV <- function(start, end, face, tir, principal){
  coups_cant <- Coup(start, end)
  coup_value <- CoupValue(face, principal)
  coup_date <- CoupDate(coups_cant, end)
  days <- as.integer(DaysCalc(start, coup_date))
  pv <- 0
  for (i in 1:coups_cant){
    if(i == coups_cant){
      pv <-  pv + (coup_value + principal)/(1 + tir)**(days/365)
    }else{
      pv <-  pv + coup_value/(1 + tir)**(days/365)
    }
    days = days + 365
  }
  return(pv)
}

# Calcular el cupon corrido
LostInt <- function(start, end, rate){
  coups_cant <- Coup(start, end)
  coup_date <- CoupDate(coups_cant, end)
  year <-  as.integer(substring(coup_date, 7, 10)) - 1
  prev_coup <- Date(paste(substring(end,1,6), substring(year,1,4), sep = ""))
  days <-  DaysCalc(prev_coup, start)
  return(days*rate/365)
}

# Funcion principal de la valuacion
Val <- function(neg, vto, face, tir, principal){
  pv <- PV(neg, vto, face, tir, principal)
  ps <- pv/principal
  cc <- LostInt(neg, vto, face)
  pl <- (ps - cc) * 100

  # Prints de la valuación
  pv_mess <- "El bono tiene un valor de giro de"
  val_mess <- "se está vendiendo"
  if (pl < 100){
    string <- "con descuento."
  }else if (pl > 100){
    string <- "con prima."
  }else{
    string <- "a la par."
  }
  pv_mess <- paste(pv_mess, currency(pv), sep = " ")
  pv_mess <- paste(pv_mess, ",", sep = "")
  val_mess <- paste(pv_mess ,val_mess, string, sep = " ")

  print(val_mess)
}

# Ejemplos

start <- "25/02/2008"
end <- "11/09/2050"
prin <- 24000000
tir <- 0.1469
fac <- 0.12598

Val(start, end, fac, tir, prin)

start <- "16/08/2022"
end <- "07/12/2024"
prin <- 150000000
tir <- 0.09565
fac <- 0.18256

Val(start, end, fac, tir, prin)

start <- "01/01/2022"
end <- "01/01/2023"
prin <- 150000000
tir <- 0.09565
fac <- 0.18256

Val(start, end, fac, tir, prin)
