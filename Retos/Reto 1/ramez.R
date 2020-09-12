f = function(x) cos(x)
g = function(x) -sin(x)
gradospoli = 5
ndatos = gradospoli

plot(g,xlim = c(-2,2),ylim = c(-2,2),ylab = "Y",col = "red",main = "Funcion cos(x)")
plot(g,xlim = c(-2,2),ylim = c(-2,2),ylab = "Y")

x = c()
y = c()
suma = (2*(2^-8))/(ndatos-1)

i =1
while (length(x)<ndatos)
{
  if (length(x) == 0 )
  {
    x[i]= -2^-8;
    i = i +1
  }
  
  x[i] = (x[i-1]+ (suma))
  i = i+1
  
} 
j = 1
while (j < ndatos+1 )
  
{
  y[j]= f(x[j])
  j = j+1
}

tablota = data.frame (x,y)
y[j]=g(2^-8)

n = rbind(c(1,x[1],(x[1])^2,(x[1])^3,(x[1])^4,(x[1])^5),
          c(1,x[2],(x[2])^2,(x[2])^3,(x[2])^4,(x[2])^5),
          c(1,x[3],(x[3])^2,(x[3])^3,(x[3])^4,(x[3])^5),
          c(1,x[4],(x[4])^2,(x[4])^3,(x[4])^4,(x[4])^5),
          c(1,x[5],(x[5])^2,(x[5])^3,(x[5])^4,(x[5])^5),
          c(0,1,2*(2^-8),3*(2^-8)^2,4*(2^-8)^3,5*(2^-8)^4))

z=solve(n,y)
print(z)

h = function(x) z[1]+(z[2]*x)+(z[3]*x^2)+(z[4]*x^3)+(z[5]*x^4)

par(new = TRUE)
plot(h,xlim = c(-2,2),col= "blue",ylim=c(-2,2), main = "APROXIMACIÓN REMEZ",ylab = "Y")

#Variables Para calcular el error
ErroAbsoluto = 0
ErrorRelativo = 0
ValorXpunto = pi/256

#############
#Errores 
#############

ErroAbsoluto = abs((f(ValorXpunto)- h(ValorXpunto))*10^-90)
ErrorRelativo  = ((ErroAbsoluto / h(ValorXpunto))*100)*10^-90


####################
#Tabla con los datos
####################
RemezTablita = data.frame(ErrorRelativo, ErroAbsoluto,ValorXpunto)
#Se imprime la Tabla

cat("Dado el punto  ",ValorXpunto)
cat("El Error Relativo es de :" ,ErrorRelativo)
cat("El Error Absoluto es de :" ,ErroAbsoluto)

