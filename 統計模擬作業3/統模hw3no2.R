#統計模擬第二題
#生成函數法
#蒙特卡羅法
#基本生成
m=1000
p=c()
for(i in 1:1000){
  x<-runif(m)
  itg=mean((exp(x)-1)/(exp(1)-1))
  p=c(p,itg)
}
var0=var(p)*m;var0
#對偶變數Antithetic Variables(1)
p=c()
for(i in 1:1000){
  x<-runif(m/2);y=1-x;x=c(x,y)
  itg=mean((exp(x)-1)/(exp(1)-1))
  p=c(p,itg)
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
vv<-var(A)-((cov(A,B))^2)/var(B);(var(A)-vv)/var(A)
for(i in 1:1000){
  x<-runif(m)
  itg=mean(((exp(x)-1)/(exp(1)-1))+a*(x-1/2))
  p=c(p,itg)
}
var2=var(p)*m;var2
vareffi=(var0-var2)/var0;vareffi

#控制變量2(如果兩個變量完全正相關?)


