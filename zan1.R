plot(density(rnorm(100)),col="red") 
#графическая подсистема 
x=5 
x 
#задания переменной значения 
x<-c(1,3,6,7) 
char<-c("8","edge","turn8k") 
logic<-c(TRUE,FALSE,TRUE,FALSE) 
#типы векторов 
dynam<-c(8,"dds",TRUE) 
dynam<-c(1,0,1,1,TRUE) 
dynam 
#динамическая типизация 
log(x,10) 
#функции 
x=10 
x<5 
x>5 
x==10 
#логические операции 
x^10 
x+10 
#математические операции 
x=10/(5-5) 
x 
#задание 
x<-c(2,4,5,3,-4,0) 
x*5 
y<-c(5,3) 
x*y 
z<-c(1,2,3,4) 
x*z 
#правила повтора векторов 
length(x) 
#измерение длины вектора 
x<-c(3,4,6,-7,8) 
mean = sum(x)/ length(x) 
mean 
#больше базовых функций 
x=2:10 
y=5:1 
z=-5:10 
w=-5:-10 
x 
y 
z 
w 
z=seq(2,5,.5) 
x=seq(3,9.1,.1) 
y=seq(9,3,-.2) 
length(x) 
length(y) 
x*y 
x=c("A","B","C") 
rep(x,4) 
#последовательности 
x<-1:10 
y<-x<=-5 
mode(x) 
mode(y) 
mode(mode(x)) 
#логические вектора 
x=1:20 
x[2] 
x[0] 
x[5:9] 
x[c(3,7,8,9,20)] 
x[-19] 
x[-(1:6)] 
x[x>10] 
x[TRUE] 
y=c(TRUE,FALSE) 
x[y] 
#индексация векторов 
vector<-c() 
v2<-c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE) 
vector=c(v2,vector); 
vector=c(F,vector); 
x=1:3 
x=c(5,x) 
x 
#добавление элементов к вектору 
x=1:5 
names(x) 
names(x)<-c("Первый","Второй","Третий","Четвертый","Первый") 
names(x) 
x["Первый"] 
#имена элементов векторов 
log(3)==log(3,base=exp(1)) 
x=seq(5,100,.1) 
x[length(x)] 
#свойства векторов 
x<-c(-10,2,78,34,-98,23,0,-8) 
order(x) 
sort(x) 
sort(x)==x[order(x)] 
log(exp(1))