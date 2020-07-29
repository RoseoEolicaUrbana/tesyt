
# Funciones ----------------------------------------------------------
'
################################################
NO SE SI ESTO ES CORRECTO PERO ESTOY SUPONIENDO
QUE LOS DATOS QUE SE DESCARGAN DE LA PÁGINA 
ESTAN SIEMPRE EN EL TIEMPO ACTUAL DE MI ORDENADOR
ESTO HAY QUE REVISARLO... EN UTC SEGURO QUE NO ESTAN
PERO BUENO, PARA IR TIRANDO NO ESTA NADA MAL
################################################
'
cambio_to_UTC<-function(x){
  Fecha<- (x$Date)
  a<- hour(with_tz(Sys.time(), tzone = Sys.timezone()))
  b<- hour(with_tz(Sys.time(), tzone = "UTC"))
  corregir_hora<- a-b
  
  x$Date<- Fecha-hm(paste(corregir_hora,":00"))
  return(x)
}

'
################################################
UNA COSA QUE HE ENCONTRADO EN Rblogers  Y QUE ME 
GUSTA MUCHO, UNA FUNCION PARA HACER UN REPLACE DE 
ELEMENTOS... HASTA AHORA SUELO UTILIZAR MOGOLLON DE 
NESTED IFELSES Y ESO ES UNA GUARRADA
################################################
'
NESTED_IFELSES_SOLUTION <- function(x, search, replace, default = NULL) {
  
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         decode.fun(tail(search,  -1L),
                                    tail(replace, -1L),
                                    default)(x))
    }
  
  return(decode.fun(search, replace, default)(x))
}


'
################################################
CON NESTED IFELSES SOLUTION HACEMOS LO DE 
SUSTITUIR LAS DIRECCIONES POR NUMERICO.
################################################
'

Dir_to_numeric<- function(VECTOR_CHARACTER_DIR){
  
  DIR_STRING<- c("North-northeast", "East-northeast",
                 "East-southeast","South-Southeast", 
                 "South-southwest","West-southwest",
                 "West-northwest","North-northwest",
                 "Northeast","Southeast",
                 "Southwest","Northwest",
                 "North","East",
                 "South","West")
  
  DIR_NUM<- c(22.5,  67.5,  112.5,
              155,  202.5,  247.5,
              292.5,337.5,45,135,225,
              315,0,90,180,270)
  
  NESTED_IFELSES_SOLUTION(VECTOR_CHARACTER_DIR, search = DIR_STRING, replace = DIR_NUM)
  
  
  
}



# NEW FILTER FUNCTION -----------------------------------------------------

WIND_DATA_FILTER<- function(VECTOR_WIND,
                            MAX_WIND=50/3.6,
                            DIF_MAX_WIND= 20/3.6,
                            CONSECUTIVE_VALUES= 20){
  
  #Nivel 1 -- TURN MAX AND NEGATIVE VALUES TO ERRORS
  N1_ERROR<- which(VECTOR_WIND>MAX_WIND | VECTOR_WIND<0 )     #<0
  
  #Nivel 2 -- TEMPORAL COHERENCE -- TAKE VERY CHANGING DATA AS ERROR
  N2_ERROR<- which(diff(VECTOR_WIND)>DIF_MAX_WIND)
  
  #Nivel 3 -- TEMPORAL COHERENCE OF SERIE -- TAKE NON CHANGING CONSECUTIVE DATA AS ERROR  
  rle_x = diff(VECTOR_WIND) %>% rle()
  
  # Compute endpoints of run
  end = cumsum(rle_x$lengths)
  start = c(1, lag(end)[-1] + 1)
  
  # Display results
  CONSECUTIVE_dF<- data.frame(Repited_value= rle_x$values,start, end) %>%  
    mutate(CONSECUTIVE= end-start) %>% 
    filter(Repited_value==0 & CONSECUTIVE>CONSECUTIVE_VALUES)
  
  N3_ERROR<- vector()
  if(!nrow(CONSECUTIVE_dF)==0){
    for (i in 1:nrow(CONSECUTIVE_dF)) {
      N3_ERROR<- c(N3_ERROR, CONSECUTIVE_dF[i, "start"]:CONSECUTIVE_dF[i, "end"])
      
    }
  }
 
  
  # REMOVE DATA AND RETURN VECTOR
  VECTOR_WIND[c(N1_ERROR, N2_ERROR, N3_ERROR) %>% unique()]<-NA 
  
  return(VECTOR_WIND)
  
  
}



#FUNCIÓN PARA RELLENAR CON EL VALOR NON-NA MAS CERCANO
REPLACE_NNONA <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}




