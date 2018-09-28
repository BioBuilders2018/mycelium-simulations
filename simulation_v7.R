#######################################
# Authors: Hannah Martiny & Chris Workman
# DTU Biobuilders project for iGEM 2018
#######################################

library(stats)
library(scales)
library(RColorBrewer)

# setwd("~/../Google Drev (hannahmariemartiny@gmail.com)/iGEM 2018/Dry lab/Mycelium Growth/simulation/")
# setwd("~/Google Drive/Biobuilders/iGEM 2018/Dry lab/Mycelium Growth/simulation/")
setwd('~/GitHub/mycelium-simulations/')

source("functions_v1.R")
source("IntersectLineSeg.R")

#######################################
#hyCols = rev(brewer.pal(9, "PuBu"))
# hyCols = rev(brewer.pal(9, "OrRd"))
# hyCols = rev(brewer.pal(9, "BuGn"))
# hyCols = rev(brewer.pal(9, "PuRd"))
# hyCols = rev(brewer.pal(9, "PuRd"))
hyCols = rev(brewer.pal(9, "YlGnBu")[3:9])

# PARAMETERS USED IN THE SIMULATION
leftright = c(-1, 1)    # branching left or right
minTheta = 10           #*pi/180 - minimum angle a branch can occur
maxTheta = 100          #*pi/180 - maximum angle a branch can occur

avgRate = 50            # average tip extension rate (µm/h), source: Spohr et al. 1998
q  = 0.005              # branching frequency (maybe need to be scaled of what the time step is)
N  = 5000               # max simulation rounds
M  = 1e4                # max hyphae
tstep = 0.0005          # time step (h)

ktip1 = 80              # initial tip extension rate, value estimated from Spohr et al 1998 figure 5
maxktip = 155           # maximum tip extension rate, value estimated from Spohr et al 1998 figure 5
ktip2 = maxktip - ktip1 # difference between ktip1 and maxktip
Kt = 5                  # saturation constant

###########################################
# Single square as growth area
# put init_n spores on centers and define maxium width
centers = matrix(c(0,0), 1, 2)
width = 50
init_n = 10

# lists for saving results during the simulation
snapShots = list()
hyphae <- list()

# initializes the first spore(s) and growth angles
hNum = 1
for(ii in 1:dim(centers)[1]) {
  for(jj in 1:init_n) {
    rxy = runif(2, -width, width) + as.integer(centers[ii,])
    iTheta = round( runif(min=0, max=360, 1) )
    hyphae[[hNum]] <- c(x0=rxy[1], y0=rxy[2], x=rxy[1], y=rxy[2], angle=iTheta, nevents=0, t=0, l=0) ## Initial spores
    hNum = hNum + 1
  }
}

# save initial state of simulation and plot it
snapShots[["0"]] = data.frame(do.call(rbind, hyphae))
par(mfrow=c(1,1))
plot_hyphae(x=snapShots[["0"]], tMax=100, hcols=hyCols, xlim=c(-100,100), ylim=c(-100,100))

S = array(10000, N)   # track substrate level over time
dls = array(0, N)     # track biomass densities over time
r = 1                 # forgot specific name, but used to scale biomass density

# actual simulation
i = 0; m = 0
while (i <= N & m < M) {
  i = i + 1
  m = length(hyphae) # number of hyphaese at this step

  # loop through hyphae, find new coordinates for tips
  dl = 0 # track biomass at this step
  for (j in 1:m) {
    angle = hyphae[[j]]["angle"][[1]]
    hi = as.numeric(hyphae[[j]][1:4])
    
    # tip extension vlaue, calculated as the accelerated growth * substrate limitation 
    # equation of the Monod type
    extension = tipExtensionMonod(ktip1 = ktip1, ktip2 = ktip2, Kt = Kt, l = hyphae[[j]]["l"][[1]], S=S[i], Ks=200)
    dx = extension * tstep * cos(angle*pi/180) # new coordinate in x-axis
    dy = extension * tstep * sin(angle*pi/180) # new coordinate in y-axis
    
    # biomass just created for hyphae j
    dl = dl + sqrt(dx^2 + dy^2)
    
    # update data frame
    hyphae[[j]]["x"] = hi[3] + dx
    hyphae[[j]]["y"] = hi[4] + dy
    hyphae[[j]]["l"] = hyphal_length(hyphae[[j]])
    b = hyphae[[j]]["nevents"]
    
    qApl = q
    #qApl = q/(b+1)
    
    if (runif(1.0) < qApl) { # branching event (q should depend on l and/or nevents)
      # the direcction a new branch will grow
      newdir = sample(leftright, 1)
      newangle = angle + newdir*round( runif(min=minTheta, max=maxTheta, 1) )

      # nevents goes one up, as there have been branching event for this hyphae
      hyphae[[j]]["nevents"] = hyphae[[j]]["nevents"] + 1

      # add new hyphae
      hyphae[[length(hyphae) + 1]] <- c(x0 = hi[3], y0 = hi[4],
                                        x  = hi[3], y  = hi[4],
                                        angle=newangle, nevents=0, t=i, l=0)
    }
  }
  ## Subtract the resources consumed in the last round of growth
  S[i+1] = S[i] - min(r*dl, S[i])
  
  # save biomass at this timestep
  dls[i+1] = dl


  if(i %% 100 == 0) {
    print(paste("Iteration =", i, "hNum =", length(hyphae) ))
    snapShots[[as.character(i)]] = data.frame(do.call(rbind, hyphae))
  }
}

# create data structures and x-y limits used for plotting
m = length(hyphae)
dat = data.frame(do.call(rbind, hyphae))
x_range <- range(dat[, c("x", "x0")])
y_range <- range(dat[, c("y", "y0")])
tMax = max(dat[,"t"])

# plot(as.matrix(cbind(as.integer(names(extensions))*tstep, extensions)), ylab = "average extension rate")

################################################
names(snapShots)
################################################

# Simple gallery
pdf("fig/example_n10.pdf", useDingbats=FALSE)
par(mfrow=c(2, 3), mar=c(2,2,3,1))
for(i in names(snapShots)[seq(2, length(snapShots), 3.4)]) {
  sst = sprintf("%.3f", as.integer(i)*tstep)
  plot_hyphae(x=snapShots[[i]], tMax=max(dat[,"t"]), hcols=hyCols, xlim=x_range, ylim=y_range, main=paste("Time =", sst))
}
plot_hyphae(x=dat, tMax=max(dat[,"t"]), hcols=hyCols, xlim=x_range, ylim=y_range, main=paste("Time =", max(dat[,"t"])*tstep))
dev.off()


################################################
# Animation of the mycelium growth
# Create a MANY png's and then combine them into one gif
if(TRUE) {
  for(i in names(snapShots)) {
    sst = sprintf("%.3f", as.integer(i)*tstep)
    #print(sst)
    fname = paste("fig/Animate_", sprintf("%04d", as.integer(i)) ,".png", sep="")
    print(fname)
    png(file=fname, width=800, height=800)#, bg = "transparent")
    par(mar=c(2,2,3,1))
    plot_hyphae(x=snapShots[[i]], tMax=max(dat[,"t"]), hcols=hyCols, xlim=x_range, ylim=y_range, main="", bty='n', yaxt='n',  xaxt='n', ann=FALSE)
    dev.off()
  }
  setwd("fig")
  cmd = sprintf("convert *.png -delay 12 -loop 0 Simulation.gif\n", init_n)
  system(cmd)
  system("rm *Animate_*.png\n")
  setwd("..")
}

################################################
# Make summary of total hyphal lengths and tip count at time points
growthSummary = data.frame(hyphae = unlist(lapply(snapShots, nrow)), total_l=NA)
for(i in rownames(growthSummary)){
  growthSummary[i,"total_l"] = sum(snapShots[[i]][,"l"])
}

# Boring views...
# Maybe better with ggplot2? (probably)
# pdf(file="Summaries_n1_v1.pdf", useDingbats = FALSE)
par(mfrow=c(2,2))
hours = as.integer(rownames(growthSummary))*tstep
plot(hours, growthSummary$total_l, pch=20, ylim=c(5, max(growthSummary$total_l)),
     log="y", type='b', xlab="Hours", ylab="Length (um), Tip number")
points(hours, growthSummary$hyphae, pch=21, col='red', type='b')
legend("topleft", legend=c("Total hyphal length", "Hyphal tips"), fill=c("black", "red"))

hist(dat[,"l"], xlab="Length (um)", main="Hyphal length")
hist(dat[,"nevents"], xlab="Branches", main="Number of branches per hyphae")
# dev.off()

# some more views
# plots of substrate and biomass changes during simulation
par(mfrow=c(2, 1))
plot(1:i, S[1:i], type='l', ylim=c(0, max(S)), main = "Substrate concentration over time", xlab = "t", ylab = "S")
plot(1:i, dls[1:i], type='l', ylim=c(0, max(dls)), main= "Biomass production", xlab="t", ylab="biomass")

################################################
# Density calculations
w = 10
xrng = c(-70, 70)
yrng = c(-70, 70)
bbs = grid_bounding_boxes(w=w, xrng=xrng, yrng=yrng)
n = dim(bbs)[1]
hyphae2boxes = hyphae_hits(hl=hyphae, bbs=bbs)
d = hyphal_length_by_RAE(hl=hyphae, h2bbs=hyphae2boxes, bbs=bbs, plotting=FALSE)
round(100*(sum(d)/sum(dat[,"l"])),2) ## % covered
#nPerBox = colSums(hyphae2boxes)

# Inspect the process for a sample of BBs
#rinds = 70; xy = bbs[rinds,]
#plot(NA, type='n', xlim=c(xy[1]-w, xy[1]+w), ylim=c(xy[2]-w, xy[2]+w))
#rinds = sample(1:n, 20)
#plot(NA, type='n', xlim=c(-100, 100), ylim=c(-100,100))
##for(i in 1:dim(bbs)[1]) polygon(x=bbs[i,c(1,1,3,3)], y=bbs[i,c(2,4,4,2)], lty=3)
#dsamp = hyphal_length_by_RAE(hl=hyphae, h2bbs=hyphae2boxes[,rinds, drop=FALSE], bbs=bbs[rinds,,drop=FALSE], plotting=TRUE)


################################################
hist(d, breaks=seq(0, max(d+10),10))

################################################

# Plot the density like a heatmap
range(d, na.rm=TRUE)
cpal = alpha(colorRampPalette(brewer.pal(9, "Reds"))(round(1.1*max(d))), 0.5)
par(mfrow=c(1,1))
xyrng = c(-80,80)

pdf(file="density_map_v3.pdf", useDingbats = FALSE)
plot_hyphae(x=dat, tMax=max(dat[,"t"]), hcols=hyCols, xlim=x_range, ylim=y_range, main=paste("Time =", max(dat[,"t"])*tstep))
#plot(NA, type='n', xlim=x_range, ylim=y_range, xlab="", ylab="", xaxt='n', yaxt='n', bty='n')
for(j in 1:n)
  polygon(x=bbs[j,c(1,1,3,3)], y=bbs[j,c(2,4,4,2)], lty=1, col=cpal[round(d[j])+1], border=NA)
dev.off()
################################################

