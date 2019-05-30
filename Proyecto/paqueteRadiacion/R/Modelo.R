#' @title Modelo
#' @description Modelo para obtener la radiacion solar.
#' @param a es el albedo de la superficie de la tierra
#' @param b Radiacion solar estra-atmosferica en la superficie
#' @param c radiacion solar dispersa
#' @param d radación solar total
#' @param e angulo de inclinacion de la superficie horizontal
#' @param f angulo entre la direccion de la radiacion iniciente a la superficie y la normal a la superficie.
#' @param g es el angulo cenital solar.
#' @return Radiacion solar en kwh/m^2
#' @examples
#' Modelo(1,1,1,1,1,1,1)
#' @export
Modelo<- function(a,b,c,d,e,f,g) {
  for(i in 1:12){
    p[i] <- a[i+3]
    G0[i] <- b[i+3]
    Gdh[i]<- c[i+3]
    Gh[i]<- d[i+3]
    B[i]<- e[i+3]
    te[i]<- f[i+3]
    tez[i] <- g[i+3]

    Ai=(Gh[i]-Gdh[i])/G0[i]
    result[i]=(Gh[i]-Gdh[i])*(cos(te[i])/cos(tez[i]))+Gdh[i]*(Ai*(cos(te[i])/cos(tez[i]))+(1-Ai)*(1+cos(B[i]))/2)+Gh[i]*p[i]*(1-cos(B[i]))/2
  }
  return(result)
}
