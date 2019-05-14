#Karen Celis
#David Lopez
#Camilo Muñoz

#Parcial2
#Compilar por separado
#-------------------------------------------------------------------------------------------
#////Primer Punto
#------------------------------------------------------------------------------------------


x = c(5.5,8.2,12.4,19.0)
y = c(83.0,94.5,105.0,92.0)
  
install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)

t = poly.calc(x,y)
plot(t, xlim = c(4,20), ylim = c(0,150), col = "red")
rm()
#Lagrange

f <- function(x)
{               #declaramos un objeto de tipo 'closure' (una funcion
    #que regresa una funcion) para poder reutilizar el codigo
    #y de paso la vectorizamos
    #x (vector-double): punto en el cual evaluar la funcion 
    mapply(sin, x)  #regresamos el valor de la funcion en el vector 
    
  }
oneLagrange_pol <- function(dataX, index, x)
{
    #dataX (vector-double): puntos donde se conoce la funcion
    #index (int): numero que indica que termino estamos calculando
    #x (double): punto en el que se evalua el polinomio
    L_i <- 1.                   #inicializamos el coeficiente del i-esimo termino
    for(i in 1:length(dataX))
    {
      if(i != index)
      {
        #evaluamos el coeficiente de 
        L_i <- L_i*( (x - dataX[i] )/(dataX[index] - dataX[i]) )
      }
    }
    
    return(L_i)  #regresamos el coeficiente index-esimo 
  }
Eval_pLagrange <- function(dataX, dataY, x)
{
    #dataX (vector-double): puntos donde se conoce la funcion
    #dataY (vector-double): puntos donde se conoce el valor de la funcion
    #x (double): punto en el que se evalua el polinomio
    f_aprox <- 0.         #inicializamos el polinomio
    for(i in 1:length(dataX))
    {
      #calculamos iterativamente el polinomio de Laprange
      f_aprox <- f_aprox + dataY[i]*oneLagrange_pol(dataX, i, x)
    }
    return(f_aprox)  #regresamos el valor del polinomio en el punto
  }  
Lagrange <- function(dataX, dataY, m, a, b )
{
    #dataX (vector): puntos a evaluar donde se conoce la funcion
    #dataY (vector): valor de la funcion conocida en los puntos dataX
    #m (int):       numero de valores a evaluar
    #a,b (double): limite inferior y superior del dominio a interpolar
    
    soporte <- seq(a, b, length = m)   #construimos puntos para probar la                                                                 #interpolacion
    f_soporte <- soporte*0             #reservamos memoria para guardar los                                                               #valores interpolados 
    for(i in 1:length(soporte))
    {
      #para cada punto en el soporte se evalua el polinomio
      f_soporte[i] <- Eval_pLagrange(dataX, dataY , soporte[i]  )
      
    }
    return(f_soporte)      #regresamos el vector con los valores interpolados
  }
 
par(new = TRUE)
#A 
PolinomioLagrangeX = poly.calc(x,Lagrange(x,y,4,5.5,19.0))
PolinomioLagrangeX
plot(PolinomioLagrangeX,main = "Azul Lagrange, Rojo calculado por poly.calc",xlim = c(4,20), ylim = c(0,150), col = "blue")
#B
PolinomioLagrangeX(15)
#C
R = PolinomioLagrangeX(seq(15.3,15.5,0.001))
diferencia = abs((100-R)*100000)
error = 100-R
tabla2 = data.frame(seq(15.3,15.5,0.001), R,diferencia)
tabla3 = data.frame(seq(15.3,15.5,0.001), R)
mini = which.min(tabla2$diferencia)
mini
tabla3[mini,]
#D
R = PolinomioLagrangeX(seq(10,20,0.0001))
tabla1 = data.frame(seq(10,20,0.0001), R)
maxi = which.max(tabla1$R)
tabla1[maxi,]

#TablaFinal 
errorRelativo = abs((Lagrange(x,y,4,5.5,19.0)-y)/y)
errorAbsoluto = abs(Lagrange(x,y,4,5.5,19.0)-y)
tablaFinal1 = data.frame(x,y,Lagrange(x,y,4,5.5,19.0),errorRelativo, errorAbsoluto) 
tablaFinal1

#Azul polinomio de Lagrange, Rojo polinomio calculado por calc




#-------------------------------------------------------------------------------------------
#////Segundo Punto
#------------------------------------------------------------------------------------------
#Para el parcial se uso los ajustes de curva ya que necsitamos saber un valor que se 
#encuentra dentro de los valores que se encuentran en la tabla dada para la desarrollar 
#el problema, es decir que cuando deseamos averiguar un valor dentro de los parametros 
#dados (que se puedan encontrar en un tabla) se usa el ajuste de curva. 
#Por el contrario, cuando se desea predecir un valor ya sea a futura o 
#lo que haya sucedido para estos casos se usa la interpolación o también en 
#casos donde no se conoce la función (x).
###2A
rm()

x <- c(0.25,0.50,0.75,1)
y <- c(0.10,0.25,0.70,1)

poblacion= x[1:4]; ingreso = y[1:4]
polyAjuste = poly.calc(poblacion,ingreso)
polyAjuste
#2B
polyAjuste(0.60)
plot(poblacion,ingreso,pch=19, cex=1, col = "purple", asp=1) 
curve(polyAjuste,add=T) 

evaluarfuncion <- function(f, a){
  f(a)
}

polinomio <- function(x) 0.7 - 4.5*x + 9.6*x^2 - 4.8*x^3 

num <- evaluarfuncion(polinomio, 0.60)
print(num)
e = (0.42 - num)/0.42
cat("Error:",round(e,3))


#Interpolacion de Newton o diferencias divididas
#2C



rm()
install.packages("rSymPy")
#instalar paquete
library(rSymPy)

x <- c(0.25,0.50,0.75,1)
fx <- c(0.10,0.25,0.70,1)

divided.differences <- function(x, y, x0) {
  require(rSymPy)
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Aproximación de la interpolacion'=as.numeric(approx), 
              'funcion interpolada'=f, 
              'Tabla de diferencias divididas '=q))
}

divided.differences(x, fx, 0.60)


