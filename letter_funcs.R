# HYPHAE HACKERS

# H Y P H A E C K R S

# Letters to be changed: A, R, N

letterH <- function() {
  xi = c(rep(-40, 7), seq(-20, 20, 20), rep(40, 7))
  yi = c(seq(60, -60, -20), rep(0, 3), seq(60, -60, -20))
  centers = cbind(xi, yi)
  
  return(centers)
}

letterY <- function() {
  xi <- c(rep(0, 4), seq(-60, -20, 20), seq(20, 60, 20))
  yi <- c(seq(-60, 0, 20), seq(60, 20, -20), seq(20, 60, 20))
  
  return(cbind(xi, yi))
}


letterP <- function() {
  xi <- c(rep(-40, 7), -20,0, 20, 20, 0, -20)
  yi <- c(seq(60, -60, -20),0, 0, 20, 40, 60, 60)
  
  return(cbind(xi, yi))
}

letterA <- function() {
  xi <- c(seq(-10, 10, 20), seq(-60, 0, 10), seq(0, 60, 10)) # -60, -40, -20)
  yi <- c(rep(0, 2), seq(-60, 60, 20), seq(60, -60, -20)) #-60, -40, -20)
  
  return(cbind(xi, yi))
}

plot(letterA())

letterE <- function() {
  xi <- c(rep(-40, 7), rep(seq(-20, 40, 20), 3))
  yi <- c(seq(60, -60, -20), rep(60, 4), rep(0, 4), rep(-60, 4))
  
  return(cbind(xi, yi))
}

letterK <- function() {
  xi <- c(rep(-40, 7), seq(-20, 40, 20), seq(40, -20, -20))
  yi <- c(seq(60, -60, -20), seq(0, 60, 20), seq(-60, 0, 20))
  
  return(cbind(xi, yi))
}

letterR <- function() {
  xi <- c(rep(-40, 7), -20,0, 20, 20, 0, -20, seq(-20, 20, 20))
  yi <- c(seq(60, -60, -20),0, 0, 20, 40, 60, 60, seq(-20, -60, -20))
  
  return(cbind(xi, yi))
}

letterC <- function() {
  # a half circle
  # y = +- sqrt(r^2-x^2)
  r = 36
  
  xi <- seq(0, -40, -5)
  yi <- c(-sqrt(r^2-xi^2), +sqrt(r^2-xi^2))
  # yi[is.nan(yi)] <- 0
  
  return(cbind(xi, yi))
}

letterS <- function() {
  
  xi <- c(rep(0, 3), 40, -40, 20, -20, 20, -20, rep(-40, 2), -20, rep(40, 2), 20)
  yi <- c(seq(-60, 60, 60), 50, -50, 55, -55, -55, 55, 40, 20, 10, -40, -20, -10)
  
  return(cbind(xi, yi))
}