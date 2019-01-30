rm(list = ls()) ; gc()
setwd("C:/Users/Jeon/Documents/GitHub/TrendClustering")
library(mapdata)
library(lubridate)
library(kriging)
library(lattice)
Sys.setlocale("LC_ALL", 'korean')
site    = read.csv(file = './data/site.csv',stringsAsFactors = F)
tmp1 = unlist(strsplit(site$L, c("˚")))
tmp2= tmp1[seq(1,length(tmp1), by = 2)]
tmp3= tmp1[seq(2,length(tmp1), by = 2)]
tmp4 = unlist(strsplit(tmp3,'´'))
longitude = round(as.numeric(tmp2)+ as.numeric(tmp4)/60,2)

tmp1 = unlist(strsplit(site$A, c("˚")))
tmp2= tmp1[seq(1,length(tmp1), by = 2)]
tmp3= tmp1[seq(2,length(tmp1), by = 2)]
tmp4 = unlist(strsplit(tmp3,'´'))
latitude = round(as.numeric(tmp2)+ as.numeric(tmp4)/60,2)

## load the result of estimates 
coefMat = read.csv(file = './result/rbetaMat.csv')
coef = unlist(coefMat[5,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef, pixels=300)
map.1 = kriged$map

layout(matrix(c(1,2),1,2,byrow = TRUE), c(6,1.5), c(6), T)
jpar<- colorRampPalette(c("blue", "white","red"))
j.col<- jpar(64)[20:64]

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)

# 8 clusters
coef = unlist(coefMat[9,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)


# 12 clusters
coef = unlist(coefMat[13,])
num.coef = length(unique(coef))
longitude1 = c(longitude, 33,38.5,33,38.5 )
latitude1 = c(latitude, 126,125,133,133)
coef = c(coef,0.7,0.7,0.7,0.7)  
rdata1 = data.frame(latitude = latitude1, longitude = longitude1 ,coef = coef)
kriged <- kriging(x = rdata1$latitude, y = rdata1$longitude, rdata1$coef,pixels=300)
map.1 = kriged$map

image(kriged, xlim = c(125,132), ylim = c(33,38.5), 
      zlim = c(-1,3),
      col = j.col,
      main = "Trend map",
      sub = paste(num.coef, "clusters"), cex.sub = 1.2,
      oldstyle = T,
      ylab = 'latitude',
      xlab='logitude')
map("worldHires", region = "south korea", add= T, lwd = 1)
for ( i in 1:64)
{
  points( rdata1[i,1],rdata1[i,2], col = 'black',bg = 'black',cex = 0.7, pch = 20 )
}

plot(x = c(0,1), y = c(-1,3) , type ="n", xlim = c(-0.2,0.2), xaxt = "n", 
     xlab = "", ylab = "trend coef")
z = matrix( seq(-1,3, length = 64), 1, 64)
image(x = 0, seq(-1,3, length = 64), z = z, 
      add = T,
      col = j.col, 
      xlim = c(-0.2,0.2),
      zlim = c(-1,3),
      oldstyle = T)

