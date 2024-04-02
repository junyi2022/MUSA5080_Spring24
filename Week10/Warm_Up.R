library(spData)
library(sf)
library(mapview)

#Exercise from here:
#https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html


map <- st_read(system.file("shapes/boston_tracts.shp",
                           package = "spData"), quiet = TRUE)

map$vble <- map$MEDV
mapview(map, zcol = "vble")

# Neighbors
library(spdep)
nb <- poly2nb(map, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

# Global Moran's I

gmoran <- moran.test(map$vble, nbw,
                     alternative = "two.sided")
gmoran

# Local Moran's I
lmoran <- localmoran(map$vble, nbw, alternative = "greater")
head(lmoran)

#create maps
library(tmap)
tmap_mode("view")


map$lmI <- lmoran[, "Ii"] # local Moran's I
map$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
map$lmp <- lmoran[, "Pr(z > E(Ii))"]

p1 <- tm_shape(map) +
  tm_polygons(fill = "vble", title = "Home Prices", style = "quantile") +
  tm_borders(col = "white", lwd = 0.75)+
  tm_layout(legend.outside = TRUE)

p2 <- tm_shape(map) +
  tm_polygons(fill = "lmI", title = "Local Moran's I",
              style = "quantile") +
  tm_borders(col = "white", lwd = 0.75)+
  tm_layout(legend.outside = TRUE)

p3 <- tm_shape(map) +
  tm_polygons(fill = "lmZ", title = "Z-score",
              breaks = c(-Inf, 1.65, Inf)) +
  tm_borders(col = "white", lwd = 0.75)+
  tm_layout(legend.outside = TRUE)

p4 <- tm_shape(map) +
  tm_polygons(fill = "lmp", title = "p-value",
              breaks = c(-Inf, 0.05, Inf)) +
  tm_borders(col = "white", lwd = 0.75)+
  tm_layout(legend.outside = TRUE)

tmap_arrange(p1, p2, p3, p4)


map$lmZ_class = cut(map$lmZ , breaks = c(-Inf, -1.96, 1.96, Inf))

tm_shape(map) + 
  tm_polygons(fill = "lmZ_class",
                            title = "Local Moran's I",
                            labels = c("Negative SAC", "No SAC", "Positive SAC"),
                            palette =  c("blue", "white", "red")) +
tm_layout(legend.outside = TRUE)

#Find clusters
lmoran <- localmoran(map$vble, nbw, alternative = "two.sided")
head(lmoran)

#Use info from scatterplot
mp <- moran.plot(as.vector(scale(map$vble)), nbw)

head(mp)

##Create a quadrant variable:
map$quadrant <- NA
# high-high
map[(mp$x >= 0 & mp$wx >= 0) & (map$lmp <= 0.05), "quadrant"]<- "HH"
# low-low
map[(mp$x <= 0 & mp$wx <= 0) & (map$lmp <= 0.05), "quadrant"]<- "LL"
# high-low
map[(mp$x >= 0 & mp$wx <= 0) & (map$lmp <= 0.05), "quadrant"]<- "HL"
# low-high
map[(mp$x <= 0 & mp$wx >= 0) & (map$lmp <= 0.05), "quadrant"]<- "LH"
# non-significant
map[(map$lmp > 0.05), "quadrant"] <- "Not Significant"


#simplest of all maps
tm_shape(map) + tm_polygons("quadrant") 



