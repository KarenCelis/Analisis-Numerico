#Parcial 1
#1b->ii

library(pracma)
library(Matrix)

# Remueve todos los objetos creados
rm(list=ls())

A = matrix(c(  4, -1, -1, -1,
               -1,  4, -1, -1,
               -1, -1,  4, -1,
               -1, -1, -1,  4), nrow=4, byrow=TRUE)# Matriz Original
b = matrix(c(-exp(1),5,6,0), nrow=4, byrow=TRUE)



Modificada = matrix(c(  4, -1, -1, -1,
                  -1.15,  4, -1, -1,
                  -1, -1,  4, -1,
                  -1, -1, -1,  4), nrow=4, byrow=TRUE)#Matriz Modificada


solve(A,b)#Solucion matriz A
print(norm(D," F"))

tol = 1e-9
sol = itersolve(A, b, tol=0.001 , method = "Gauss-Seidel")#Solucion matriz Modificada
#print(sol)

#Cota error de la solucion 
Cota = matrix(c(Modificada-A), nrow=4, byrow=4)
c= norm(A)

#Norma del Error relativo de la matriz
#print(Cota)
#Num de Condicion
condi=cond(A)

ERelativ=0.15/c
round(ERelativ,6)
cat("Error Rel:", ERelativ)
cots=condi*ERelativ
cat("Cota para el Error Rel de la solucion es:" , cots, "\n")
#