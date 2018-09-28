library(stats)
library(scales)
library(RColorBrewer)

setwd("~/../Google Drev (hannahmariemartiny@gmail.com)/iGEM 2018/Dry lab/Mycelium Growth/simulation/")
setwd("~/Google Drive/Biobuilders/iGEM 2018/Dry lab/Mycelium Growth/simulation/")

source("functions_v1.R")
source("IntersectLineSeg.R")
source("letter_funcs.R")

#######################################
#hyCols = rev(brewer.pal(9, "PuBu"))
# hyCols = rev(brewer.pal(9, "OrRd"))
# hyCols = rev(brewer.pal(9, "BuGn"))
# hyCols = rev(brewer.pal(9, "PuRd"))
# hyCols = rev(brewer.pal(9, "PuRd"))
hyCols = rev(brewer.pal(9, "YlGnBu")[3:9])

leftright = c(-1, 1)
minTheta = 10 #*pi/180
maxTheta = 100 #*pi/180

avgRate = 50  # average tip extension rate (µm/h), source: Spohr et al. 1998
q  = 0.005    # branching frequency, need to be scaled of what the time step is
N  = 5000     # max simulation rounds
M  = 1e4      # max hyphae
tstep = 0.0005 # time step (h)

q  = 0.005    # branching frequency, need to be scaled of what the time step is
N  = 5000     # max simulation rounds
M  = 1e4      # max hyphae
tstep = 0.0005 # time step (h)

#######################################
# animated letters in HYPHAE HACKERS
teamname <- strsplit("HYPHAEHACKERS", "")[[1]]

for (letter in unique(teamname)) {
  print(letter)
  
  fullLetter = paste0("letter", letter)
  
  centers = get(fullLetter)()
  width = 10
  init_n = 10
  
  snapShots = list()
  hyphae <- list()
  hNum = 1
  for(ii in 1:dim(centers)[1]) {
    for(jj in 1:init_n) {
      rxy = runif(2, -width, width) + as.integer(centers[ii,])
      #iTheta = round( runif(min=0, max=2*pi, 1) )
      iTheta = round( runif(min=0, max=360, 1) )
      hyphae[[hNum]] <- c(x0=rxy[1], y0=rxy[2], x=rxy[1], y=rxy[2], angle=iTheta, nevents=0, t=0, l=0) ## Initial spores
      hNum = hNum + 1
    }
  }
  
  snapShots[["0"]] = data.frame(do.call(rbind, hyphae))
  
  i = 0; m = 0
  while (i <= N & m < M) {
    i = i + 1
    m = length(hyphae)
    
    # loop through hyphae, find new coordinates for tips
    for (j in 1:m) {
      angle <- hyphae[[j]]["angle"][[1]]
      
      # new coordinates for growth
      hyphae[[j]]["x"][[1]] <- hyphae[[j]]["x"][[1]] + avgRate*cos(angle*pi/180)*tstep
      hyphae[[j]]["y"][[1]] <- hyphae[[j]]["y"][[1]] + avgRate*sin(angle*pi/180)*tstep
      hyphae[[j]]["l"] = l = hyphal_length(hyphae[[j]])
      
      if (runif(1.0) < q) { # branching event (q should depend on l and/or nevents)
        newdir <- sample(leftright, 1)
        #newangle <- angle + newdir*sample(newdegrees, 1)
        newangle <- angle + newdir*round( runif(min=minTheta, max=maxTheta, 1) )
        
        # nevents goes one up, as there have been branching event for this hyphae
        hyphae[[j]]["nevents"] <- hyphae[[j]]["nevents"][[1]] + 1
        
        # add new hyphae
        hyphae[[length(hyphae) + 1]] <- c(x0 = hyphae[[j]]["x"][[1]],
                                          y0 = hyphae[[j]]["y"][[1]],
                                          x  = hyphae[[j]]["x"][[1]],
                                          y  = hyphae[[j]]["y"][[1]],
                                          angle=newangle, nevents=0, t=i, l=0)
      }
    }
    
    if(i %% 10 == 0) {
      # print(paste(i, j, probBranching(kb = kbran,l = hyphae[[j]]["l"][[1]])))
      # print(paste("Iteration =", i, "hNum =", length(hyphae) ))
      snapShots[[as.character(i)]] = data.frame(do.call(rbind, hyphae))
      # extension = (1/nrow(snapShots[[as.character(i)]])) * ((sum(snapShots[[as.character(i)]]["l"])-sum(snapShots[[as.character(i-100)]]["l"]))/(tstep*100))
      # extensions[[as.character(i)]] = extension
      # points(i, nrow(snapShots[[as.character(i)]]))
    }
  }
  
  m = length(hyphae)
  dat = data.frame(do.call(rbind, hyphae))  ## 'df' is a function
  x_range <- range(dat[, c("x", "x0")], na.rm=T)
  y_range <- range(dat[, c("y", "y0")], na.rm=T)
  tMax = max(dat[,"t"])
  
  if(TRUE) {
    for(i in names(snapShots)) {
      sst = sprintf("%.3f", as.integer(i)*tstep)
      #print(sst)
      fname = paste("ani/Animate_", sprintf("%s_%04d", letter, as.integer(i)) ,".png", sep="")
      # print(fname)
      png(file=fname, width=800, height=800)#, bg = "transparent")
      par(mar=c(2,2,3,1))
      #plot_hyphae(x=snapShots[[i]], tMax=max(dat[,"t"]), hcols=hyCols, main=paste("Time =", sst))
      plot_hyphae(x=snapShots[[i]], tMax=max(dat[,"t"]), hcols=hyCols, xlim=x_range, ylim=y_range, main="", bty='n', yaxt='n',  xaxt='n', ann=FALSE)
      dev.off()
    }
    # setwd("ani")
    # cmd = sprintf("magick convert -delay 12 -loop 0 *.png  Simulation_H_v2.gif\n", init_n)
    # shell(cmd)
    # system("rm *Animate_*.png\n")
    # setwd("..")
  }
}
