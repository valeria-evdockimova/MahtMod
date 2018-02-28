Вычислить среднее для каждой колонки таблицы iris, за исключением колонки “Species” и соберите результат в список (list)
iris 
i=iris 
i 
head(i) 
mean(i$Sepal.Length) 
x=mean(i$Sepal.Length) 
x 
mean(i$Sepal.Width) 
y=mean(i$Sepal.Width) 
y 
mean(i$Petal.Length) 
z=mean(i$Petal.Length) 
z 
mean(i$Petal.Width) 
h=mean(i$Petal.Width) 
h 
list(x,y,x,h)