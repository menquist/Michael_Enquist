# Random draws from a normal distribution
norm1 <- rnorm(1000, 6, 10)
# Constant
a <- 4
# Transformation
y1 <- a - norm1

# Sample distribution 2
norm2 <- rnorm(10000, 0, 4)
b <- 3

y2 <- b*norm2

# Sample distribution 3
norm3  <- rnorm(1500, 0, 5)
a <- 4
b <- 3

y3 <- a + b*norm3

# Sample from 2 normal distributions
x1 <- rnorm(100)
x2 <- rnorm(100)

# Construct constants
beta0 <- 0
beta1 <- 2
beta2 <- 8

# Construct equation
z <- beta0 + beta1*x1 + beta2*x2 + rnorm(100)

# Build a matrix from above
mat <- cbind(z, x1, x2)

# Examine plots
pairs(mat)
