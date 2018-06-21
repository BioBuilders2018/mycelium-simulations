#install.packages("plot3D", repos = "http://cran.us.r-project.org")

library(plot3D)

# set working dir
setwd("~/GitHub/mycelium-simulations/")

massDensityMycelium <- function(V, pf, A, N, li) {
  # V: volume of representative volume element (RVE)
  # pf: hyphae wall material density
  # A: the cross-section area of the tubular hyphae
  # N: number of filaments in the network
  # li the length of the ith filament
  
  if (length(li) == N) {
    sumLi = Reduce("+", li)
    
    pm = (1/V) * pf * A * sumLi
    
    return(pm)
  }
}

pdf("fig/PM_Islam_2017.pdf", height = 10, width = 12)

# V vs pf
V = seq(1, 10, length.out = 50)
pf = seq(1, 20, length.out = 50)

# NOT VARYING
A = 2
li = 1:6
N = length(li)

z <- matrix(NA, nrow = length(V), ncol = length(pf))
for (i in 1:length(V)) {
  for (j in 1:length(pf)) {
  z[i, j] <- massDensityMycelium(V[i], pf[j], A, N, li)
  }
}

M <- mesh(V, pf)
x <- M$x
y <- M$y

par(mar = c(1, 1, 3, 1))
titleName <- paste0("Parameter mapping: V vs pf\nA=", A, ", N=", N, ", li=(",  paste( unlist(li), collapse=', '), ")")
perspbox(x, y, z, xlab = "Volume (V)", ylab = "Hyphae wall material density (pf)", zlab = "Mycelium mass density", ticktype = "detailed")
surf3D(x, y , z, colvar = z, add = T)
title(titleName)

# V vs A
V = seq(1, 10, length.out = 50)
A = seq(1, 10, length.out = 50)

# NOT VARYING
pf = 2
li = 1:6
N = length(li)

z <- matrix(NA, nrow = length(V), ncol = length(A))
for (i in 1:length(V)) {
  for (j in 1:length(A)) {
    z[i, j] <- massDensityMycelium(V = V[i], A = A[j], pf = pf, N = N, li = li)
  }
}

M <- mesh(V, A)
x <- M$x
y <- M$y

par(mar = c(1, 1, 3, 1))
titleName <- paste0("Parameter mapping: V vs A\npf=", pf, ", N=", N, ", li=(",  paste( unlist(li), collapse=', '), ")")
perspbox(x, y, z, xlab = "Volume (V)", ylab = "Cross-section area (A)", zlab = "Mycelium mass density", ticktype = "detailed")
surf3D(x, y , z, colvar = z, add = T)
title(titleName)

# V vs N
V = seq(1, 5, length.out = 50)
N = seq(1, 200, 2) 
  
# NOT VARYING
pf = 2
A = 2

z <- matrix(NA, nrow = length(V), ncol = length(N))
for (i in 1:length(V)) {
  for (j in 1:length(N)) {
    li = seq(1, 10, length.out = N[j])
    z[i, j] <- massDensityMycelium(V = V[i], N = N[j], pf = pf, A = A, li = li)
  }
}

M <- mesh(V, N)
x <- M$x
y <- M$y

par(mar = c(1, 1, 3, 1))
titleName <- paste0("Parameter mapping: V vs N\npf=", pf, ", A=", A)
perspbox(x, y, z, xlab = "Volume (V)", ylab = "Number of filaments (N)", zlab = "Mycelium mass density", ticktype = "detailed")
surf3D(x, y , z, colvar = z, add = T)
title(titleName)

dev.off()
