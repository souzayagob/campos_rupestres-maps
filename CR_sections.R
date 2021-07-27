#Function to get a equivalent colour with a different degree of transparency. From: https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
makeTransparent = function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}

#Choose the best colours to represent each section. 
#'show_col' plots colours corresponding to each colour code.
library(scales)
show_col(c("#008805",
           "#0000A3",
           "#FF0005",
           "#FF7F00"))

#======================================================================================================#

#Loading packages
library(rgdal)
library(raster)
library(rgeos)
library(latticeExtra)

#Defining projection
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#Reading shapefiles
cr <- readOGR("shapefiles/shapefilecamporupestre/cr.shp") #https://doi.org/10.1007/s11104-015-2637-8
br <- readOGR("shapefiles/br_unidades_da_federacao/BRUFE250GC_SIR.shp", encoding = "UTF-8") #IBGE: https://downloads.ibge.gov.br/downloads_geociencias.htm

#Projecting
proj4string(cr) <- crswgs84
br <- spTransform(br, crswgs84)

#Setting a transparent grid covering the extension of the campos rupestres
br_cr <- raster(extent(-50.5, -38.5, -23.25, -8.5),
                       crs = crswgs84, nrows = 1, ncols = 1)
br_cr <- rasterToPolygons(br_cr)

#Plotting multiple polygons: the Brazilian terrestrial territory, the transparent grid
#and the campos rupestres
#plot(br)
#plot(br_cr, col = "#BEBEBE7F", add = TRUE)
#plot(cr, col = "black", add = T)

#======================================================================================================#

#Defining grids that correspond to the Brazilian Central Plateau and the Serra da Canastra
#Arguments at function 'extent' go in this order: xmin, xmax, ymin, ymax
#These will be required bellow

#Brazilian Central Plateau
cr_plateau <- raster(extent(-50.5, -45, -18, -12), crs = crswgs84, nrows = 1, ncols = 1)
cr_plateau <- rasterToPolygons(cr_plateau)
plot(cr_plateau)
plot(cr, col = "black", add = T)

#Serra da Canastra
cr_canastra <- raster(extent(-50.5, -45.5, -23.25, -18), crs = crswgs84, nrows = 1, ncols = 1)
cr_canastra <- rasterToPolygons(cr_canastra)
plot(cr_canastra)
plot(cr, col = "black", add = T)

#======================================================================================================#

#The code bellow will delimit the extension of each section in the campos rupestres by intersecting the campos rupestres shapefile with different polygons
#Note that the Southern Espinhaço and the Northern Espinhaço are delimitedusing federative unities, while the Brazilian Central Plateau and the Serra da Canastra are delimited using the polygons generated above

#Cropping the campos rupestres shapefile in order to remove disjunct areas
campos_rup <- crop(cr, extent(c(-50.5, -38.5, -23.25, -8.5)))

#Retrieving polygons of the federative unities (Minas Gerais and Bahia)
br_mg <- br[br$NM_ESTADO %in% c("MINAS GERAIS"), ]
br_ba <- br[br$NM_ESTADO %in% c("BAHIA"), ]

#Delimiting sections
#Each row in 'campos_rup' represents a polygon, which will be categorised according to the following conditions
campos_rup@data$sector <- c() 
for(i in 1:nrow(campos_rup)){
  if(gIntersects(campos_rup[i, ], cr_canastra)){
    campos_rup@data$sector[i] <- "Serra da Canastra"
  } else if(gIntersects(campos_rup[i, ], br_mg)){
    campos_rup@data$sector[i] <- "Southern Espinhaço"
  } else if(gIntersects(campos_rup[i, ], br_ba)){
    campos_rup@data$sector[i] <- "Northern Espinhaço"
  } else if(gIntersects(campos_rup[i, ], cr_plateau)){
    campos_rup@data$sector[i] <- "Brazilian Central Plateau"
  }
}

#Ordering the categories
campos_rup$sector <- factor(campos_rup$sector, 
                           c("Southern Espinhaço", "Northern Espinhaço",
                             "Serra da Canastra",
                             "Brazilian Central Plateau"),
                           ordered = is.ordered(campos_rup$sector))

#======================================================================================================#

#Loading an elevation raster 
alt <- raster('HYP_HR_SR/HYP_HR_SR.tif')
e <- as(extent(-50.5, -38.5, -23.25, -8.5), 'SpatialPolygons')
crs(e) <- crswgs84
r <- crop(alt, e)
rr <- mask(r, br)

#Defining the colour for each section
campos_rup$color_in <- c()
for(i in 1:nrow(campos_rup@data)){
  if(campos_rup$sector[i] == "Southern Espinhaço"){
    campos_rup$color_in[i] <- makeTransparent("#0000A3", alpha = 0.65)
  } else if(campos_rup$sector[i] == "Northern Espinhaço"){
    campos_rup$color_in[i] <- makeTransparent("#008805", alpha = 0.65)
  } else if(campos_rup$sector[i] == "Serra da Canastra"){
    campos_rup$color_in[i] <- makeTransparent("#FF7F00", alpha = 0.65)
  } else if(campos_rup$sector[i] == "Brazilian Central Plateau"){
    campos_rup$color_in[i] <- makeTransparent("#FF0005", alpha = 0.65)
  }
}

#Composing the map
cr_plot  <- spplot(campos_rup, zcol = "sector",
       xlim = c(-50.5, -38.5), ylim = c(-23.25, -8.5), 
       sp.layout = list(list(br, "black", 
                             fill = "transparent", alpha = 0.7)),
       colorkey = FALSE,
       scales = list(draw = FALSE),
       col = campos_rup$color_in,
       col.regions = makeTransparent(c("#0000A3", "#008805", "#FF7F00", "#FF0005"), alpha = 0.5)) + as.layer(spplot(rr, col.regions = grey(1:100/100, alpha = 0.7), 
                                                                maxpixels = 2e10), 
                                                         under = TRUE)

#Generating files for manual editing, if needed
#png("cr_plot.png",
    #height = 4, width = 4, units = 'in', res=300); cr_plot; dev.off()
#png("br_plot.png",
    #height = 4, width = 4, units = 'in', res=300)
#plot(br)
#plot(br_cr, col = "#BEBEBE7F", add = TRUE)
#plot(cr, col = "black", add = TRUE)
#dev.off()
