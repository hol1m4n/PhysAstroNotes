# define support set of X
x = seq(-1.5, 1.5, by = 0.01)

# obtain uniform kernel function values
uniform1 = dunif(x, min = -0.25, max = 0.25)
uniform2 = dunif(x, min = -1.00, max = 1.00)

plot(x, uniform1, type = 'l', ylab = 'f(x)', 
     xlab = 'x', main = '2 Uniform Kernels with Different Bandwidths', col = 'red')
lines(x, uniform2, col = 'blue')

# add legend; must specify 'lty' option, because these are line plots
legend(0.28, 1.5, c('Uniform(-0.25, 0.25)', 'Uniform(-1.00, 1.00)'),
       lty = c(1,1), col = c('red', 'blue'), box.lwd = 0)


#Used for reproducibility
set.seed(1)
#Two Normals mixed
data = c(rnorm(100,-10,1),rnorm(100,10,1))

#Normal Density
phi = function(x) exp(-.5*x^2)/sqrt(2*pi)
#True Density
tpdf = function(x) phi(x+10)/2+phi(x-10)/2

#Bandwidth estimated by Silverman's Rule of Thumb
h = sd(data)*(4/3/length(data))^(1/5)

#Kernel Density
Kernel2 = function(x) mean(phi((x-data)/h)/h)

#Element wise application
kpdf = function(x) sapply(x,Kernel2)

#Linear Space
x=seq(-25,25,length=1000)

plot(x,tpdf(x),type="l",ylim=c(0,0.23),col="red")

par(new=T)

#Plot Kernel Density with Silverman's Rule of Thumb
plot(x,kpdf(x),type="l",ylim=c(0,0.23),xlab="",ylab="",axes=F)
