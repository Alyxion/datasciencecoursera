add2 <- function(x, y)
{
    x+y
}

above10 <- function(x)
{
    use <- x > 10
    x[use]
}

above <- function(x,n = 10) {
    use <- x>n
    x[use]
}

columnMean <- function(y, removeNA = TRUE) {
    nc <- ncol(y)
    means <- numeric(nc)
    for(i in 1:nc) {
        means[i] <- mean(y[, i], na.rm = removeNA)
    }
    means
}

make.power <- function(n)
{
    pow <- function(x) {
        x^n
    }
    pow
}

cube <- make.power(3)

# Lexical scoping

y <- 10
sw
f <- function(x) {
    y <- 2
    y ^ 2 + g(x)
}

g <- function(x)
{
    x*y
}