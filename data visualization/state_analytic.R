# source: https://gist.github.com/toddwschneider/6678635
# reference: http://www.r-bloggers.com/whats-the-most-concave-state-in-the-u-s-using-r-to-solve-a-geography-puzzle/

library(maptools)
library(geosphere)

# load USA state-level spatial data
# download from http://gadm.org
# click the 'download' tab
# select county = 'united states', file format = 'R', click ok
# download 'level 1' for state-level data
load("USA_adm1.RData")

# get coordinate matrix for a state
getCoordinatesByStateName = function(state) {
  ix = which(gadm$NAME_1 == state)
  sdf = gadm[ix,]
  coords = do.call(rbind, lapply(sdf@polygons[[1]]@Polygons, function(p) { p@coords }))
  return(coords)
}

# take a matrix of coordinates, remove points that are less than minDistance km apart
simplifyCoords = function(coords, minDistance = 10, maxToSkip = 1000) {
  ncoords = nrow(coords)
  i = 1
  simplified = c()
  while(i < nrow(coords)) {
    upper = min(ncoords, i + maxToSkip)
    coordsToConsider = matrix(coords[i:upper,], ncol = 2)
    dists = spDistsN1(coordsToConsider, coords[i,], longlat = TRUE)
    ixToSelect = which(dists > minDistance)[1]
    if(is.na(ixToSelect)) { ixToSelect = length(dists) }
    simplified = rbind(simplified, coordsToConsider[ixToSelect,])
    i = i + ixToSelect - 1
  }
  return(simplified)
}

# plot a primary state and optionally some surrounding states
plotStates = function(primary, additional = NULL, pcol = '#cccccc', acol = '#eeeeee', ...) {
  s1 = which(gadm$NAME_1 == primary)
  s2 = which(gadm$NAME_1 %in% additional)
  ix = c(s1, s2)
  sdf = gadm[ix, ]
  nstates = length(ix)
  cols = c(pcol, rep(acol, nstates - 1))
  plot(sdf, col=cols, lwd = 0.1, ...)
}

analyzeState = function(state, minDistance, animate = FALSE, gcPoints = 100) {
  fullCoords = getCoordinatesByStateName(state)
  simplifiedCoords = simplifyCoords(fullCoords, minDistance)
  ncoords = nrow(simplifiedCoords)
  print(ncoords)
  maxStates = 0
  maxp1 = NA
  maxp2 = NA
  maxStateNames = NA
  for(p1Index in 1:(ncoords - 1)) {
    print(p1Index)
    p1 = simplifiedCoords[p1Index,]
    for(p2Index in (p1Index + 1):ncoords) {
      p2 = simplifiedCoords[p2Index,]
      line = gcIntermediate(p1, p2, n = gcPoints, addStartEnd = FALSE)
      points = SpatialPoints(data.frame(x = line[,1], y = line[,2]),
                             proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      states = unique(over(points, gadm)$NAME_1)
      states = states[!is.na(states)]
      if(length(states) > maxStates) {
        maxStates = length(states)
        maxp1 = p1
        maxp2 = p2
        maxStateNames = as.character(states)
      }
      if(animate) {
        plotStates(state, states[states != state])
        lines(line)
      }
    }
  }
  return(list(n = maxStates, p1 = maxp1, p2 = maxp2, states = maxStateNames))
}

# states you want to calculate
stateNames = c('New York', 'Maryland', 'West Virginia')

# run the calculations and save results
# before run this code you need to create a folder name: "results"

for(state in stateNames) {
  print(state)
  a <- proc.time()
  result = analyzeState(state, minDistance = 10)
  dump("result/", file = paste("results/", state, ".R", sep="")) # why did he use "/" in paste("results/", state, ".R", sep=""))
  b <- proc.time() - a
}

# check the results and make some plots!

for(file in dir("results/", pattern = "*.R")) {
  source(paste("results/", file, sep=""))
  #source("resultsNew York.R")
  state = strsplit(file, ".", fixed = TRUE)[[1]][1]
  otherStates = result$states[result$states != state]
  line = gcIntermediate(result$p1, result$p2, n = 100, addStartEnd = TRUE)
  png(filename=paste("results/", state, ".png", sep=""), width=480, heigh=480)
  plotStates(state, otherStates)
  lines(line)
  points(line[c(1, nrow(line)),], pch=19, cex=0.5, col='red')
  dev.off()
  xlim = sort(c(result$p1[1], result$p2[1])) + c(-0.1, 0.1)
  ylim = sort(c(result$p1[2], result$p2[2])) + c(-0.1, 0.1)
  png(filename=paste("results/", state, "_zoom.png", sep=""), width=480, heigh=480)
  plotStates(state, otherStates, xlim = xlim, ylim = ylim)
  lines(line)
  points(line[c(1, nrow(line)),], pch=19, cex=0.5, col='red')
  dev.off()
}
