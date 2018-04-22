#統計模擬第一題
#生成函數法
#蒙特卡羅法
#基本生成
m=1000
p=c()
for(i in 1:1000){
  x<-runif(m)
  itg=mean(1/(pi*(1+x^2)))
  p=c(p,1/2-itg)
}
var0=var(p)*m;var0
#對偶變數Antithetic Variables(1)
p=c()
for(i in 1:1000){
  x<-runif(m/2);y=1-x;x=c(x,y)
  itg=mean(1/(pi*(1+x^2)))
  p=c(p,1/2-itg)
}
var1=var(p)*m;var1
vareffi=(var0-var1)/var0;vareffi
#控制變量法(2)
p=c()
x<-runif(m)
A<-x
B<-(exp(x)-1)/(exp(1)-1)
cor(A,B)
a <- -cov(A,B) / var(B)#估計c*
for(i in 1:1000){
  x<-runif(m/2);y=1-x;x=c(x,y)
  itg=mean((1/(pi*(1+x^2)))-a*(x-1/2))
  p=c(p,itg)
}
var2=var(p)*m;var2
vareffi=(var0-var2)/var0;vareffi



