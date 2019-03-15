#Punto 4
list.of.packages <- c("PolynomF")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(PolynomF)


x <- c(40,50,60,70,80)
y <- c(35,83,153,193,215)

lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}
cat("Usando Lagrange: ",lagrange(x,y,55))
error = 120 - lagrange(x,y,55)
cat ("Error: ", error)


funcion<-function(x) 3343-239.3667*x+6.183333*x**2-0.06733333*x**3+0.0002666667*x**4


xdatos = x[1:5]; ydatos = y[1:5]
polinomioAjuste = poly.calc(xdatos,ydatos)
cat ("Polinomio: ", polinomioAjuste)
plot(datx,daty,pch=19, cex=1, col = "purple", asp=1) 
curve(polinomioAjuste,add=T) 


valor <- funcion(55)
cat(num)
valor = 120 - num
cat("Valor: Error:",round(error,7))