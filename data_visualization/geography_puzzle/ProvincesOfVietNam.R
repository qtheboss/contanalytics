# source: https://gist.github.com/toddwschneider/6678635
#reference: http://www.r-bloggers.com/whats-the-most-concave-state-in-the-u-s-using-r-to-solve-a-geography-puzzle/

library(maptools)
library(geosphere)

# load VietNam-level spatial data
# download from http://gadm.org
# click the 'download' tab
# select county = 'Viet Nam', file format = 'R', click ok
# download all levels from level data

# Load data.........

load("Viet Nam/VNM_adm0.RData")
level_0 = gadm
load("Viet Nam/VNM_adm1.RData")
level_1 = gadm
load("Viet Nam/VNM_adm2.RData")
level_2 = gadm 
load("Viet Nam/VNM_adm3.RData")
level_3 = gadm
load("Viet Nam/VNM_adm4.RData")
level_4 = gadm
  
# Data Understanding

# Overview of Vietnam
pid_0  = level_0$ID_0
name_0 = level_0$NAME_ENGLISH

# These are 8 locals in VietNam
pid_1 = level_1$ID_1
name_1 = level_1$NAME_1

# These are provinces in VietNam
pid_2 = level_2$ID_2
name_2 = level_2$NAME_2

# These are districts in VietNam
pid_3 = level_3$ID_3
name_3 = level_3$NAME_3

# These are towns in VietNam
pid_4 = level_4$ID_4
name_4 = level_4$NAME_4

# We will use data level_2

# get coordinate matrix for a province
getCoordinatesByProvinceName = function(province){
  print(paste("Get coordinate matrix for the ", "-", province, sep = " ") )   
  ix = which(level_2$NAME_2 == province)
  sdf = level_2[ix,]
  coords = do.call(rbind,lapply(sdf@polygons[[1]]@Polygons, function(p){p@coords}))
}

# take a matrix of coordinates, remove points that are less than minDistance km apart
simplifyCoords = function(coords, minDistance = 10, maxToSkip = 1000) {
  print("simplifyCoords function........ ")
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

# plot a primary province and optionally some surrounding provinces
plotprovinces = function(primary, additional = NULL, pcol = '#cccccc', acol = '#eeeeee', ...) {
  print("Plot province.........")
  s1 = which(level_2$NAME_2 == primary)
  s2 = which(level_2$NAME_2 %in% additional)
  ix = c(s1, s2)
  sdf = level_2[ix, ]
  nprovinces = length(ix)
  cols = c(pcol, rep(acol, nprovinces - 1))
  plot(sdf, col=cols, lwd = 0.1, ...)
}

analyzeProvince = function(province, minDistance, animate = FALSE, gcPoints = 100) {
  print("Analyzeprovince.......")
  fullCoords = getCoordinatesByProvinceName(province)
  simplifiedCoords = simplifyCoords(fullCoords, minDistance)
  ncoords = nrow(simplifiedCoords)
  print(ncoords)
  maxprovinces = 0
  maxp1 = NA
  maxp2 = NA
  maxprovinceNames = NA
  for(p1Index in 1:(ncoords - 1)) {
    print(p1Index)
    p1 = simplifiedCoords[p1Index,]
    for(p2Index in (p1Index + 1):ncoords) {
      p2 = simplifiedCoords[p2Index,]
      line = gcIntermediate(p1, p2, n = gcPoints, addStartEnd = FALSE)
      points = SpatialPoints(data.frame(x = line[,1], y = line[,2]),
                             proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      provinces = unique(over(points, level_2)$NAME_2)
      provinces = provinces[!is.na(provinces)]
      if(length(provinces) > maxprovinces) {
        maxprovinces = length(provinces)
        maxp1 = p1
        maxp2 = p2
        maxprovinceNames = as.character(provinces)
      }
      if(animate) {
        plotprovinces(province, provinces[provinces != province])
        lines(line)
      }
      
    }
  }
  return(list(n = maxprovinces, p1 = maxp1, p2 = maxp2, provinces = maxprovinceNames))
}

# provinces you want to calculate
provinceNames = c("Hà Nội", "Lâm Đồng", "Quảng Nam","Hải Phòng","Hồ Chí Minh city")
# run the calculations and save results

# You need to create folder "results_vietnam"
for(province in provinceNames) {
  print(province)
  a <- proc.time()
  result = analyzeProvince(province, minDistance = 10)
  dump("result", file = paste("results_vietnam/", province, ".R", sep=""))
  b <- proc.time() - a  
  print("this is running time: ")
  print(b)
}

# check the results and make some plots!
for(file in dir("results_vietnam/", pattern = "*.R")) {
  source(paste("results_vietnam/", file, sep=""))
  province = strsplit(file, ".", fixed = TRUE)[[1]][1]
  otherProvinces = result$provinces[result$provinces != province]
  line = gcIntermediate(result$p1, result$p2, n = 100, addStartEnd = TRUE)
  png(filename=paste("results_vietnam/", province, ".png", sep=""), width=480, heigh=480)
  plotprovinces(province, otherProvinces)
  lines(line)
  points(line[c(1, nrow(line)),], pch=19, cex=0.5, col='red')
  dev.off()
  xlim = sort(c(result$p1[1], result$p2[1])) + c(-0.1, 0.1)
  ylim = sort(c(result$p1[2], result$p2[2])) + c(-0.1, 0.1)
  png(filename=paste("results_vietnam/", province, "_zoom.png", sep=""), width=480, heigh=480)
  plotprovinces(province, otherProvinces, xlim = xlim, ylim = ylim)
  lines(line)
  points(line[c(1, nrow(line)),], pch=19, cex=0.5, col='red')
  dev.off()
}



