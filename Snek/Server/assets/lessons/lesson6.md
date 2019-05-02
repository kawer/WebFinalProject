# Arreglos

Un arreglo es una estructura de datos que te permite tener una lista de datos en una variable.
Si tu quisieras almacenar una lista de datos, en lugar de hacer una variable para cada uno, puedes usar un arreglo.
Ejemplo: 

```python
x = [5, 10, 15, 20]
```

En este arreglo tengo los valores de 1, 2, 3 y 4 todos separados y los puedo accesar cuando quiera.

#### Accesar datos

Una manera para accesar datos es sabiendo la posición en la que el dato está. En python y muchos de los lenguajes de programación, los arreglos e indices siempre empiezan en 0. 
Por ejemplo, tomando en cuenta el ejemplo anterior, podemos hacer esto.

```python 

print x[0] # 5
print x[1] # 10
print x[2] # 15
print x[3] # 20
```

#### Modicar datos

Para modificar los datos de un arreglo, es muy similar a como se accesan. Solamente tiene que fijar el nuevo valor al arreglo en un índice.
Por ejemplo:

```python
print x[0] # 1
x[0] = 1000
print x[0] # 1000

```
Ahora cuando accedamos al arreglo el valor en la posición 0 va a ser 1000. 
