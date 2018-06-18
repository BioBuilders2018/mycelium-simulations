#install.packages("rgl", repos = "http://cran.us.r-project.org")
#install.packages("plot3D", repos = "http://cran.us.r-project.org")

library(rgl)
library(plot3D)

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

V = seq(1, 20, 2)
pf = 1:10

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

zlim <- range(z ,na.rm=T)
colors <- rainbow(length(z))[z-zlim[1]+1] # assign colors to heights for each point

persp3d(V, pf, z, col = colors)
legend3d("topright", legend = levels(cut(zlim), 10), col = colors, pch = 20)

surf3D(cbind(V), cbind(pf) , cbind(z))
