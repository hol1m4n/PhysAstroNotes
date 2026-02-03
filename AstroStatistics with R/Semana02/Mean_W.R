# Función k-ésima media podada

mediaP = function(x,k){
  n = length(x)
  xs = sort(x)
  xp = xs[(k+1):(n-k)] #eliminar k primeros y los k últimos datos
  mean(xp)
}

# k-ésima media Winsorizada

mediaW = function(x, k) {
  x = sort(x)
  n = length(x)
  x[1:k] = x[k+1] # inserta dato x[k+1] desde x[1] hasta x[k]
  x[(n-k+1):n] = x[n-k] # inserta dato x[n-k] desde x[n-k+1] hasta x[n]
  mean(x)
}

x = c( 8.2, 51.4, 39.02, 90.5, 44.69, 83.6, 73.76, 81.1, 38.81, 68.51)
k = 2
cat(mean(x)," ", mediaP(x,k)," ", mediaW(x,k))
