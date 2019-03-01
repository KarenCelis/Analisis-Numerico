#Parcial 1
#2.a interseccion f(x)-g(x)

f=function(x){tan(pi*x)}
g=function(x){sin(pi*x)}
F1=function(x){f(x)-g(x)}
#E<-10^-9
#x0<-1
#x1<--2
interseccion= function(x0,x1,E,F1){
  error=1000
  cat("\t","x0","\t","x1","\t","E","\n")
  sigF<-x1-F1(x1)*(x1-x0)/(F1(x1)-F1(x0))
  while(error>E){
    error=abs(x1-x0)/abs(x1)
    cat("\t",x0,"\t",x1,"\t",error,"\n")
    x0=x1
    x1=sigF
    sigF<-x1-f(x1)*(x1-x0)/(f(x1)-f(x0))
  }
}
interseccion(1,-2,0.000000001,F1)
