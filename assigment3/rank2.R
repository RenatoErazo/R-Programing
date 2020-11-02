rank2 <- function(state, restriccion, num = "best") {
  ## Read outcome data
  ## Check that state and restriccion are valid
  
  require(stringi)
  
  restriccion <- stri_trans_general(restriccion, id = "Title")
 
  
  
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  estadoOk <- sum(sapply(outcome$State,function(x) x == state)) == 0
  
  if (estadoOk) {
    
    stop('Invalid state')
  }
  
  tipoRest <- c("Heart Attack", "Heart Failure", "Pneumonia")
  siTipo <- sum(sapply(tipoRest,function(x) x == restriccion)) == 0
  if(siTipo){
    
    stop('Invalid outcome')
  }
  ## 2.-Validar que sean "best", "worst" número y menor al número de provincias
  
  ## 2.1.-  --- Primero se valida si es un número y si este es menora a los casos del estado
  
  ## 2.1.1.- contruimos el nombre de la columna para rate
  
  library(dplyr)
  prefijo <- "Hospital.30.Day.Death..Mortality..Rates.from"
  indice <- sub(" ",".",restriccion)
  completo <- paste(prefijo,indice,sep = ".")
  outcome <- outcome %>% select(State,Hospital.Name,ZIP.Code,completo)
  
  
  ## ---- Cambio los nombre de columnas mas simple
  
  names(outcome) = c("Estado","Hospital","ZIP","Rate")
  
  ## --- Solo recuperar datos del estado/provincia seleccionado
  outcome <- subset(outcome,Estado == state)
  
 
  if( num != "best" && num != "worst" && num%%1 != 0 ) {
    stop("invalid num")
  }
  
  #texto a número
  
  outcome$Rate  <- as.numeric(outcome$Rate)
  
  ## --- Eliminar NA
  
  outcome <- outcome %>% filter(!is.na(Rate))
  
  ##En este momento se puede validar el número de filas
  if(num == "best")
    num <- 1
  if(num == "worst")
    num <- nrow(outcome)
  if(num > nrow(outcome))
    stop(paste('El numero de datos validos son: ',nrow(outcome),'NA',sep = " "))
  
  #Ordenar por Rate y nombre de hospital
  
  outcome <- outcome %>% arrange(Rate,Hospital)
  
  
  ## Return hospital name in that state with the given rank
  
  outcome[num,]$Hospital
  
  ## 30-day death rate
  
  
  
}

