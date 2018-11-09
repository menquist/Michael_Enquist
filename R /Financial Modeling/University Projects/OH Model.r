akf = 3
akc = 2
alf = 1
alc = 2
K = 3000
L = 2000

L1 = L/alf
L2 = L/alc
K1 = K/akf
K2 = K/akc


A <- matrix(c(akf,akc,alf,alc), nrow = 2, byrow = T)
A

endow_home <- matrix(c(K,L), nrow = 2)
endow_home

production <- solve(A) %*% endow_home
production


# Rec theorem

K3 = 4000

endow_home_2 <- matrix(c(K3,L), nrow = 2)
production_2 <- solve(A) %*% endow_home_2
production_2



# Example usage
df <- data.frame(x1 = c(0,K1,0,K2), y1 = c(L1,0,L2,0))
df
x <- c(0, K1)
y <- c(L1, 0)
plot(df$x1, df$y1, "o", pch=20)  # bezier() generates smoothed curves from these points
abline(h = production[1,1], v = production[2,1], col = "gray60")
abline(h = production_2[1,1], v = production_2[2,1], col = "red")
points(bezier(df$x1, df$y1, type="l", col="red"))



factor_input_1 <- matrix(c(akf*production[1,1], akc*production[2,1], alf*production[1,1], alc*production[2,1]), nrow = 2, byrow = T)
factor_input_1

factor_input_2 <- matrix(c(akf*production_2[1,1], akc*production_2[2,1], alf*production_2[1,1], alc*production_2[2,1]), nrow = 2, byrow = T)
factor_input_2

# percentage change
QF_K <- (factor_input_2[1,1]-factor_input_1[1,1])/(factor_input_1[1,1])
QF_K

QF_L <- (factor_input_2[2,1]-factor_input_1[2,1])/(factor_input_1[2,1])
QF_L

QC_K <- (factor_input_2[1,2]-factor_input_1[1,2])/(factor_input_1[1,2])
QC_K

QC_L <- (factor_input_2[2,2]-factor_input_1[2,2])/(factor_input_1[2,2])
QC_L


##########################################################################
Fakf = 1
Fakc = .1
Falf = .5
Falc = 7
FK = 1000
FL = 8000
B <- matrix(c(Fakf,Fakc,Falf,Falc), nrow = 2, byrow = T)
B

endow_Foreign <- matrix(c(FK,FL), nrow = 2)
endow_Foreign

Fproduction <- solve(B) %*% endow_Foreign
Fproduction


#------

b = 1
cap = 1500
lab = 500
alpha = (1/3)



prod = b * cap ^ alpha * lab ^ ( 1 - alpha )

kt <- ""
kl <- ""
  
K = kt + kl

#---------------------

u <- function(x, y) 3*x + 2*y

x <- seq(0, L, by=1)
y <- seq(K, 0, by=-1)
a <- c(100)

persp(x, y, outer(x, y, u), ticktype="detailed")
contour(x, y, outer(x, y, u), levels=a)


# raw data 

x <- c(1,2,3,4,5,6,7,8)
y <- c(10,10,10,15,15,30,60,90)

# note- for the contour function to work below, x and y 
# These must be in ascending order-based on the contour documentation
#  which is the opposite of how the data was presented at
# about.com 

# put x and y in a matrix

xy <- as.matrix(cbind(x,y))

# transpose xy

xyT <- t(xy) 

# define function z as a matrix

z <- as.matrix(sqrt(x*y))

# plot the countour plot / specified level sets

contour(xyT,z) # all levels

contour(xyT,z, levels =c(10,20,50)) # specified levels for z

# *------------------------------------------------------------------
# | PART 2:   simulated data  (sort of) - not really indifference curves            
# *-----------------------------------------------------------------


rm(list=ls()) # get rid of any existing data 

ls() # display active data -should be null 

# define x and y

x <- seq(0,100,by=10)
y <- seq(0,20,by=2)

# put x and y in a matrix

xy <- as.matrix(cbind(x,y))

# transpose xy

xyT <- t(xy) 

# define function z as a matrix

z <- as.matrix(sqrt(x*y))

contour(xyT,z)


###########################################################################
akf = 10
akc = 2
alf = 2
alc = 1
K = 200
L = 200

x<- seq(0,100, 1)
y<- seq(0,100, 1)
a <- c(100,200,300)

f <- function(x,y) {
  K= akf*x + akc*y 
  L= alf*x + alc*y  }


z <- outer(x,y,f)
fx <- function(x,y,h=0.001) (f(x+h,y)-f(x,y))/h
fy <- function(x,y,h=0.001) (f(x,y+h)-f(x,y))/h
zfx <- outer(x,y,fx)
zfy <- outer(x,y,fy)
#persp(x,y,z, theta=-30,phi=15,ticktype="detailed")
contour(x,y,zfx,level=a) 
#contour(x,y,zfy,level=a, add=T, col="red")
contour(x, y,z)
image(x,y,z)
contour(x,y,z,add=T)


z <- as.data.frame(z)
b <- ggplot(z, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = factor_input_1[1,1], x2 = factor_input_1[1,2], y1 = factor_input_1[2,1], y2 = factor_input_1[2,2])
b <- ggplot(geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df))

