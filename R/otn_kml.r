
require("raster")
require("PBSmapping")
require("geosphere")
require("RMySQL")
require("chron")
require("gdistance")
require("rgeos")
require("maptools")
require("rJava")
require("stringr")
require("kmlbuilder")
require("lubridate")
library("chron")
library("stringr")
library("kmlbuilder")
library("lubridate")


astats = function(da){
  names(x) = c("PID", "plon", "plat", "capdate", "kms")
  da = merge(x, y, by = c("PID","capdate"))
  ##REMOVE YEARS
  da = da[which(da$sampyear %in%  as.character(years) ),]
  da$sample_num = NULL
  da$sample_id = NULL
  da$trip = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  x = nrow(da)
  
  
  if(x == 0) return(FALSE)
  names(da) = c("PID", "capdate", "plon","plat","kms","caparea", "caplat", "caplong", "capyear", "triparea", "tripsub", "sampyear", "sampdate", "samplat", "samplon")
  
  
  mat = paste("(",are,")", sep = "")
  ind = which(!grepl(mat, da$tripsub, fixed = TRUE))
  if(length(ind)>0)
    da = da[-ind,]
  
  #  
  #  ii = is.in(are, da$samplon, da$samplat)
  #  jj = is.in(are, da$caplon, da$caplat)
  #  ind = which(!(ii | jj))
  #  
  #  
  #  if(length(ind)>0) 
  #    da = da[-ind,]
  #  
  #  
  #REMOVE GULF ENTRIES
  ind = which(as.character(da$caparea) == "GULF" & as.character(da$triparea) == "GULF"  & as.numeric(as.character(da$sampyear)) <= 2014 )
  # if(length(ind)>0) 
  #  da = da[-ind,]
  
  
  
  x = nrow(da)
  
  if(x == 0) return(FALSE)
  
  z = NULL
  z$ret = nrow(da)
  z$retuni = length(unique(da$PID))
  
  
  mov = data2Poly(da)
  ind = which(mov$X == 0)
  ind = which(mov$PID %in% mov$PID[ind])
  if(length(ind)>0) 
    mov = mov[-ind,]
  
  ind = which(is.na(mov$D))
  ind = which(mov$PID %in% mov$PID[ind])
  if(length(ind)>0) 
    mov = mov[-ind,]
  
  dates = as.character(mov$D)
  mov$D = chron(dates, format = "y-m-d")
  
  x = split(mov, mov$PID)
  ndays = NULL
  nkm = NULL
  pid = NULL
  tofirst = NULL
  tofkm = NULL
  for (i in 1:length(x)) {
    chunk = data.frame(x[i])
    names(chunk) = c("PID", "POS", "X", "Y", "D", "K")
    dd = as.character(chunk$D)
    day = 0
    km = 0
    fkm = chunk$K[2]
    fday = (chunk$D[2] - chunk$D[1])
    for(j in 1:((nrow(chunk))-1)){
      day = day + (chunk$D[j+1] - chunk$D[j])
      
      km = km + as.numeric(chunk$K[j+1])
    }
    ndays = c(ndays, day)
    nkm = c(nkm, km)
    pid = c(pid, chunk$PID[1])
    tofirst = c(tofirst, fday)
    tofkm = c(tofkm, fkm)
  }
  daysince = cbind(pid, ndays, nkm)
  daysince = data.frame(daysince)
  
  ind = which(as.numeric(daysince$ndays) <= 10)
  
  
  if(length(ind)>0) 
    daysince = daysince[-ind,]
  x = nrow(daysince)
  
  if(x == 0) return(FALSE)
  
  #m = calcLength (mov, rollup = 3, close = FALSE)
  
  z$mov = mean(daysince$nkm)
  z$lmov = max(daysince$nkm)
  pdf("distances.pdf")
  hist(daysince$nkm,breaks=100, col="red",main="Distances Travelled",xlab="Distance(km)")
  dev.off()
  pdf("days.pdf")
  hist(daysince$ndays,breaks=100, col="red",main="Days To Last Known Capture",xlab="Time(days)")
  dev.off()
  pdf("tofirstdays.pdf")
  hist(tofirst,breaks=100, col="red", main="Days To First Capture",xlab="Time(days)")
  dev.off()
  z$day = mean(daysince$ndays)
  z$lday = max(daysince$ndays)
  
  #z$spe = z$mov/(z$day*0.0328549) #Days to month, km/month output
  z$spe = sum(daysince$nkm)/sum(daysince$ndays*0.0328549) #Days to month, km/month output
  distance = daysince$km
  return(z)
}

#stab <- file.path("C:", "workspace", "data","maps", "mpa", "St Ann's Bank.kml")
degminsec2decdeg = function(positions){
  ldeg = substr(positions, 1, 2)
  lmin = substr(positions, 4, 5)
  lsec = substr(positions, 7, 8)
  lsec = as.numeric(lsec)/60
  lmin = (as.numeric(lmin)+lsec)/60
  ldecded = as.numeric(ldeg)+lmin
  
  return(ldecded)
  
  
}

GEaddMPA = function(years){
  
  mykml$addLabelStyle(styleid = "ps1", color = "red", scale = 10)
  mykml$addLabelStyle(styleid = "ps2", color = "red", scale = 10)
  mykml$addPolyStyle(styleid = "ps2", color = "red", transparency = .5)
  mykml$addPolyStyle(styleid = "ps1", color = "red", transparency = .6)
  mykml$addFolder("MPA", name = "MPA")
  mykml$addPolyStyle(styleid = "ps3", color = "darkgreen", transparency = .1)
  mpa = mykml$getFolder("MPA")
  
  for(i in 1:length(years)){
    # closed = file.path("C:", "workspace", "data","maps", "mpa", "SCclosed.csv")
    # protected = file.path("C:", "workspace", "data","maps", "mpa", "SCprotected.csv")
    
    # da = read.csv(closed)
    # da2 = read.csv(protected)
    #Convert to decimal deg
    # da$lat = degminsec2decdeg(da$dmslat)
    # da$lon = degminsec2decdeg(da$dmslon)*-1
    # da2$lat = degminsec2decdeg(da2$dmslat)
    # da2$lon = degminsec2decdeg(da2$dmslon)*-1
    
    #  year = years[i]
    #  da3 = da
    #  da$TimeSpanStart = gsub("YYYY", year, da$TimeSpanStart)
    #   da$TimeSpanEnd = gsub("YYYY", year, da$TimeSpanEnd)
    #  da$description = paste("closed from ", da$TimeSpanStart, " to ", da$TimeSpanEnd, sep = "")
    
    #  da2$description = paste("Always Closed")
    #   ind = which(as.Date(da$TimeSpanStart) < zmin  & as.Date(da$TimeSpanEnd) < zmin)
    #   if(length(ind) > 0) da = da[-ind,]
    #   ind = which(as.Date(da$TimeSpanStart) > zmax  & as.Date(da$TimeSpanEnd) > zmax)
    #   if(length(ind) > 0) da = da[-ind,]
    #   
    #   
    #   da$TimeSpanStart[which(as.Date(da$TimeSpanStart) < zmin)] = as.character(zmin)
    #   da$TimeSpanEnd[which(as.Date(da$TimeSpanEnd) > zmax)] = as.character(zmax)
    #   
    #   #da$TimeSpanStart = as.character(as.Date(da$TimeSpanStart) - 1)
    #   da$TimeSpanEnd = as.character(as.Date(da$TimeSpanEnd) + 1)
    #   mpa = mykml$addFolder(MPA, name = "MPA")
    
    #  mpa$addPolygon(da, inFolder = "SC_Closed", styleUrl = "ps1")
    
    #  mpa$addPolygon(da2, inFolder = "SC_Protected", styleUrl = "ps2")
    #  da3$TimeSpanStart = NULL
    #   da3$TimeSpanEnd = NULL
    #  if(i == 1)
    #    mpa$addPolygon(da3, inFolder = "SC_Closed", styleUrl = "ps3")
    
    
    
    
    #mykml$preview()
  }
  #  mpa$addNetworkLink(href = stab, name= "St.Ann's Bank")
}
#Create the kml root object
mykml = RKmlObject()
mykml$addAbstractView(type = "lookat", viewid = "Rview", latitude = 45.5, longitude = -62, range = 1000000)
#fn = file.path("E:", "maps", "801_LL_WGS84_PCT_clip_red.tif")
#mykml$addFolder("Overlays", name = "Overlays")

#overlays = mykml$getFolder("Overlays")

#overlays$addGroundOverlay(fn = fn, name = "801_Chart", AbstractView = "Rview", transparency = .7 )

#fn = file.path("E:", "maps", "CMHT2011.tif")
#overlays$addGroundOverlay(fn = fn, name = "Habitat 2011", AbstractView = "Rview", visibility = 0, transparency = .3 )


#fn = file.path("C:", "workspace", "data", "maps", "Layers", "CommercialMaleHabitat2013RGBFT.tif")
#overlays$addGroundOverlay(fn = fn, name = "Habitat 2013", AbstractView = "Rview", transparency = .3 )

#emer <- file.path("E:", "workspace", "data","maps", "layers", "EMERA.kml")
mpa = mykml$addFolder("MPA", name = "MPA")
#mykml$getFolder("MPA")$addNetworkLink(href = stab, name= "St.Ann's Bank")

GEaddMPA(c("2015"))
plotReceivers = function(){
  
  
  mykml$addIconStyle(styleid = "rec",scale = .6, href = "http://maps.google.com/mapfiles/kml/shapes/square.png", color = "blue", transparency = 1)
  #mykml$addLabelStyle(styleid = "rec",scale = .6, color = "white")
  
  mykml$addFolder("Receivers", name = "Receivers")
  
  dir =  file.path( "E:", "OTN", "receivers" )
  files = list.files(dir)
  for(i in 1:length(files)){
    mykml$getFolder("Receivers")$addFolder(files[i], name = files[i])
    da = read.csv( file.path(dir,files[i]) , sep=",", header=T ) 
    nam = names(da)
    
    nam[which(nam == "DEPLOY_LAT")] = "lat"
    nam[which(nam == "DEPLOY_LONG")] = "lon"
    nam[which(nam == "deploy_date")] = "TimeStamp"
    nam[which(nam == "stn_long")] = "lon"
    nam[which(nam == "stn_lat")] = "lat"
    # nam[which(nam == "STATION_NO")] = "name"
    
    names(da) = nam
    if("DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss." %in% nam){
      dpd = matrix(unlist(strsplit(as.character(da$DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss.), "T")), ncol = 2, byrow = T)[,1]
      
      da$TimeStamp = dpd
      
    }
    mykml$getFolder("Receivers")$getFolder(files[i])$addPoint(da, styleUrl = "rec" )
    
  }
}
plotReleases = function(){
  rel = NULL
  dir =  file.path( "E:", "OTN", "releases" )
  files = list.files(dir)
  
  
  
  mykml$addLineStyle(styleid = "line2013e", color = "yellow", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2013w", color = "orange", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2015e", color = "darkgoldenrod1", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2015w", color = "orange3", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2017a", color = "darkgoldenrod4", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2017b", color = "lightgoldenrod2", transparency = 1, width = 4)
  
  mykml$addLineStyle(styleid = "line2015gbhA", color = "purple", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2015gbhB", color = "magenta", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2015gbhC", color = "pink", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2018gbh", color = "deeppink3", transparency = 1, width = 4)
  
  
  mykml$addLineStyle(styleid = "line2015gulfA", color = "green", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2015gulfB", color = "greenyellow", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2016gulfA", color = "green", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2016gulfB", color = "greenyellow", transparency = 1, width = 4)
  
  
  
  mykml$addLineStyle(styleid = "line201523A", color = "red", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line201523B", color = "red3", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line201523C", color = "red4", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line201623A", color = "red", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line201623B", color = "red3", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line201623C", color = "red4", transparency = 1, width = 4)
  
  
  mykml$addLineStyle(styleid = "line2016mpaA", color = "red2", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line2018mpaA", color = "orangered", transparency = 1, width = 4)
  
  mykml$addLineStyle(styleid = "line20174XA", color = "blue", transparency = 1, width = 4)
  mykml$addLineStyle(styleid = "line20174XB", color = "lightblue", transparency = 1, width = 4)
  
  mykml$addLineStyle(styleid = "line2017Gulf", color = "chartreuse2", transparency = 1, width = 4)
  
  
  
  mykml$addIconStyle(styleid = "rel2013e", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "yellow", transparency = 1)
  mykml$addIconStyle(styleid = "rel2013w", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "orange", transparency = 1)
  mykml$addIconStyle(styleid = "rel2015e", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "darkgoldenrod1", transparency = 1)
  mykml$addIconStyle(styleid = "rel2015w", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "orange3", transparency = 1)
  mykml$addIconStyle(styleid = "rel2017a", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "darkgoldenrod4", transparency = 1)
  mykml$addIconStyle(styleid = "rel2017b", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "lightgoldenrod2", transparency = 1)
  
  
  
  
  
  mykml$addIconStyle(styleid = "rel2015gbhA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "purple", transparency = 1)
  mykml$addIconStyle(styleid = "rel2015gbhB", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "magenta", transparency = 1)
  mykml$addIconStyle(styleid = "rel2015gbhC", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "pink", transparency = 1)
  mykml$addIconStyle(styleid = "rel2018gbh", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "deeppink3", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "rel2015gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "rel2015gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "greenyellow", transparency = 1)
  mykml$addIconStyle(styleid = "rel2016gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "rel2016gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "greenyellow", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "rel201523A", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "rel201523B", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "rel201523C", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red4", transparency = 1)
  mykml$addIconStyle(styleid = "rel201623A", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "rel201623B", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "rel201623C", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red4", transparency = 1)
  
  
  mykml$addIconStyle(styleid = "rel2016mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "red2", transparency = 1)
  mykml$addIconStyle(styleid = "rel2018mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "orangered", transparency = 1)
  
  mykml$addIconStyle(styleid = "rel20174XA", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "blue", transparency = 1)
  mykml$addIconStyle(styleid = "rel20174XB", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "lightblue", transparency = 1)
  
  mykml$addIconStyle(styleid = "rel2017Gulf", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "chartreuse2", transparency = 1)
  
  
  
  #     mykml$addIconStyle(styleid = "rel201623A", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "grey1", transparency = 1)
  #     mykml$addIconStyle(styleid = "rel201623B", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "grey2", transparency = 1)
  #     mykml$addIconStyle(styleid = "rel201623C", href = "http://maps.google.com/mapfiles/kml/shapes/triangle.png", color = "grey3", transparency = 1)
  #     
  
  
  mykml$addIconStyle(styleid = "cap2013e", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "yellow", transparency = 1)
  mykml$addIconStyle(styleid = "cap2013w", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "orange", transparency = 1) 
  mykml$addIconStyle(styleid = "cap2015e", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "darkgoldenrod1", transparency = 1)
  mykml$addIconStyle(styleid = "cap2015w", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "orange3", transparency = 1)
  mykml$addIconStyle(styleid = "cap2017a", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "darkgoldenrod4", transparency = 1)
  mykml$addIconStyle(styleid = "cap2017b", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "lightgoldenrod2", transparency = 1)
  
  mykml$addIconStyle(styleid = "cap2015gbhA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "purple", transparency = 1)
  mykml$addIconStyle(styleid = "cap2015gbhB", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "magenta", transparency = 1)
  mykml$addIconStyle(styleid = "cap2015gbhC", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "pink", transparency = 1)
  mykml$addIconStyle(styleid = "cap2018gbh", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "deeppink3", transparency = 1)
  
  
  mykml$addIconStyle(styleid = "cap2015gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "cap2015gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "greenyellow", transparency = 1)
  mykml$addIconStyle(styleid = "cap2016gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "cap2016gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "greenyellow", transparency = 1)
  
  mykml$addIconStyle(styleid = "capunknown", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "white", transparency = 1)
  
  mykml$addIconStyle(styleid = "cap201523A", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "cap201523B", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "cap201523C", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red4", transparency = 1)
  mykml$addIconStyle(styleid = "cap201623A", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "cap201623B", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "cap201623C", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red4", transparency = 1)
  
  mykml$addIconStyle(styleid = "cap2016mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "red2", transparency = 1)
  mykml$addIconStyle(styleid = "cap2018mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "orangered", transparency = 1)
  
  mykml$addIconStyle(styleid = "cap20174XA", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "blue", transparency = 1)
  mykml$addIconStyle(styleid = "cap20174XB", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "lightblue", transparency = 1)
  
  mykml$addIconStyle(styleid = "cap2017Gulf", href = "http://maps.google.com/mapfiles/kml/shapes/donut.png", color = "chartreuse2", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "dcap2013e", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "yellow", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2013w", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "orange", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2015e", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "darkgoldenrod1", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2015w", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "orange3", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2017a", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "darkgoldenrod4", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2017b", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "lightgoldenrod2", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "dcap2015gbhA", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "purple", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2015gbhB", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "magenta", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2015gbhC", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "pink", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2018gbh", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "deeppink3", transparency = 1)
  
  
  mykml$addIconStyle(styleid = "dcap2015gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2015gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "greenyellow", transparency = 1)
  mykml$addIconStyle(styleid = "dcapunknown", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "white", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201523A", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201523B", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201523C", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red4", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201623A", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201623B", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "dcap201623C", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red4", transparency = 1)
  
  mykml$addIconStyle(styleid = "dcap2016mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "red2", transparency = 1)
  mykml$addIconStyle(styleid = "dcap2018mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "orangered", transparency = 1)
  
  mykml$addIconStyle(styleid = "dcap20174XA", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "blue", transparency = 1)
  mykml$addIconStyle(styleid = "dcap20174XB", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "lightblue", transparency = 1)
  
  mykml$addIconStyle(styleid = "dcap2017Gulf", href = "http://maps.google.com/mapfiles/kml/shapes/forbidden.png", color = "chartreuse2", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "fcap2013e", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "yellow", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2013w", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "orange", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2015e", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "darkgoldenrod1", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2015w", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "orange3", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2017a", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "darkgoldenrod4", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2017b", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "lightgoldenrod2", transparency = 1)
  
  
  
  mykml$addIconStyle(styleid = "fcap2015gbhA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "purple", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2015gbhB", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "magenta", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2015gbhC", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "pink", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2018gbh", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "deeppink3", transparency = 1)
  
  mykml$addIconStyle(styleid = "fcap2015gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2015gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "greenyellow", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2016gulfA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "green", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2016gulfB", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "greenyellow", transparency = 1)
  
  mykml$addIconStyle(styleid = "fcapunknown", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "white", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201523A", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201523B", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201523C", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red4", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201623A", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201623B", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red3", transparency = 1)
  mykml$addIconStyle(styleid = "fcap201623C", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red4", transparency = 1)
  
  mykml$addIconStyle(styleid = "fcap2016mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "red2", transparency = 1)
  mykml$addIconStyle(styleid = "fcap2018mpaA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "orangered", transparency = 1)
  
  mykml$addIconStyle(styleid = "fcap20174XA", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "blue", transparency = 1)
  mykml$addIconStyle(styleid = "fcap20174XB", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "lightblue", transparency = 1)
  
  mykml$addIconStyle(styleid = "fcap2017Gulf", href = "http://maps.google.com/mapfiles/kml/shapes/arrow-reverse.png", color = "chartreuse2", transparency = 1)
  
  
  
  
  #######
  
  mykml$addLabelStyle(styleid = "rel2013e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2013w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2015e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2015w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2017a", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2017b", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel2015gbhA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2015gbhB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2015gbhC", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2018gbh", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel2015gulfA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2015gulfB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2016gulfA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2016gulfB", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel201523A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel201523B", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel201523C", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel201623A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel201623B", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel201623C", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel2016mpaA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel2018mpaA", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel20174XA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "rel20174XB", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "rel2017Gulf", scale = .6, color = "white")
  
  
  
  
  mykml$addLabelStyle(styleid = "cap2013e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2013w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2015e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2015w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2017a", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2017b", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "cap2015gbhA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2015gbhB",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2015gbhC", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2018gbh", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "cap2015gulfA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2015gulfB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2016gulfA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2016gulfB", scale = .6, color = "white")
  
  
  mykml$addLabelStyle(styleid = "capunknown", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201523A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201523B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201523C", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201623A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201623B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap201623C", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "cap2016mpaA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap2018mpaA", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "cap20174XA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "cap20174XB", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "cap2017Gulf", scale = .6, color = "white")
  
  
  
  mykml$addLabelStyle(styleid = "dcap2013e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2013w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2015e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2015w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2017a", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2017b", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "dcap2015gbhA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2015gbhB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2015gbhC", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2018gbh", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "dcap2015gulfA",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2015gulfB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcapunknown", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201523A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201523B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201523C", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201623A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201623B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap201623C", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "dcap2016mpaA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap2018mpaA", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "dcap20174XA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "dcap20174XB", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "dcap2017Gulf", scale = .6, color = "white")
  
  
  
  mykml$addLabelStyle(styleid = "fcap2013e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2013w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2015e", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2015w", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2017a", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2017b", scale = .6, color = "white")
  
  
  mykml$addLabelStyle(styleid = "fcap2015gbhA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2015gbhB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2015gbhC", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2018gbh", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "fcap2015gulfA",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2015gulfB", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2016gulfA",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2016gulfB", scale = .6, color = "white")
  
  
  mykml$addLabelStyle(styleid = "fcapunknown", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201523A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201523B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201523C", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201623A", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201623B",scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap201623C", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "fcap2016mpaA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap2018mpaA", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "fcap20174XA", scale = .6, color = "white")
  mykml$addLabelStyle(styleid = "fcap20174XB", scale = .6, color = "white")
  
  mykml$addLabelStyle(styleid = "fcap2017Gulf", scale = .6, color = "white")
  
  
  
  mykml$addFolder("Releases", name = "Releases")
  
  dir =  file.path( "E:", "OTN", "releases" )
  files = list.files(dir)
  for(i in 1:length(files)){
    
    mykml$getFolder("Releases")$addFolder(files[i], name = files[i])
    da = read.csv( file.path(dir,files[i]) , sep=",", header=T ) 
    da = da[,1:40]
    nam = names(da)
    nam[which(nam == "RELEASE_LATITUDE")] = "lat"
    nam[which(nam == "RELEASE_LONGITUDE")] = "lon"
    nam[which(nam == "deploy_date")] = "TimeStamp"
    nam[which(nam == "stn_long")] = "lon"
    nam[which(nam == "stn_lat")] = "lat"
    
    names(da) = nam
    
    
    if("UTC_RELEASE_DATE_TIME" %in% nam){
      dpd = matrix(unlist(strsplit(as.character(da$UTC_RELEASE_DATE_TIME), "T")), ncol = 2, byrow = T)[,1]
      da$TimeStamp = dpd
      
    }
    
    da$styleUrl = NA
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA 19" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015gulfA"
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA 19" & as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015gulfB"
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA_19" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2016gulfA"
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA_19" & as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2016gulfB"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "Glace Bay Hole" & as.character(da$RELEASE_GROUP) == "3")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015gbhA"
    ind = which(as.character(da$RELEASE_LOCATION) == "Glace Bay Hole" & as.character(da$RELEASE_GROUP) == "4")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015gbhB"
    ind = which(as.character(da$RELEASE_LOCATION) == "NENS_GBH")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2018gbh"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "Glace Bay Hole" & as.character(da$RELEASE_GROUP) == "5")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015gbhC"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "release1" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201523A"
    ind = which(as.character(da$RELEASE_LOCATION) == "release2" & as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201523B"
    ind = which(as.character(da$RELEASE_LOCATION) == "release3" & as.character(da$RELEASE_GROUP) == "3")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201523C"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "East of proposed Maritime Link" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2013e"
    ind = which(as.character(da$RELEASE_LOCATION) == "West of proposed Maritime Link"& as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2013w"
    
    
    ind = which(as.character(da$RELEASE_LOCATION) == "East of proposed Maritime Link" & as.character(da$RELEASE_GROUP) == "4")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015e"
    ind = which(as.character(da$RELEASE_LOCATION) == "West of proposed Maritime Link"& as.character(da$RELEASE_GROUP) == "3")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2015w"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "East of Neils Harbour" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2017a"
    ind = which(as.character(da$RELEASE_LOCATION) == "East of Neils Harbour"& as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2017b"
    
    
    
    
    ind = which(as.character(da$RELEASE_LOCATION) == "South of St.Annes Bank" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201623A"
    ind = which(as.character(da$RELEASE_LOCATION) == "South of St.Annes Bank"& as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201623B"
    ind = which(as.character(da$RELEASE_LOCATION) == "South of St.Annes Bank"& as.character(da$RELEASE_GROUP) == "3")
    if(length(ind) > 0) da$styleUrl[ind] = "rel201623C"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "Southern St.Anns Bank")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2016mpaA"
    ind = which(as.character(da$RELEASE_LOCATION) == "Southern St.Anns Bank 2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2018mpaA"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA 4X" & as.character(da$RELEASE_GROUP) == "1")
    if(length(ind) > 0) da$styleUrl[ind] = "rel20174XA"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "SCFA 4X" & as.character(da$RELEASE_GROUP) == "2")
    if(length(ind) > 0) da$styleUrl[ind] = "rel20174XB"
    
    ind = which(as.character(da$RELEASE_LOCATION) == "Gulf")
    if(length(ind) > 0) da$styleUrl[ind] = "rel2017Gulf"
    
    
    
    
    da2 = data.frame()
    dasub = split(da, da$RELEASE_GROUP)
    for(ij in 1:length(dasub)){
      
      nda=NULL
      nda = data.frame(dasub[ij])
      names(nda) = names(da)
      nda$name = paste(nrow(nda), " Crab Tagged", sep = "")
      nda$date = unlist(strsplit(as.character(nda$UTC_RELEASE_DATE_TIME), "T"))[1]
      nda$time = unlist(strsplit(as.character(nda$UTC_RELEASE_DATE_TIME), "T"))[2]
      
      
      craid = paste("Tag IDs: ", paste0(as.character(nda$ANIMAL_ID), collapse = "</br>"), sep = "</br>") 
      dati = strftime(strptime(paste(nda$date, nda$time), "%Y-%m-%d %H:%M:%S"), "(%B %d %Y %H:%M:%S)")
      nda$description = paste("<![CDATA[ </br>Release Time: ", as.character(dati),"</br>Released by:", nda$TAGGER, "</br>", craid,"]]>", sep = "")
      
      da2 = rbind(da2, nda[1,])
      
    }
    
    mykml$getFolder("Releases")$getFolder(files[i])$addPoint(da2)
    
    rel = rbind(rel, da)
    names(rel) = names(da)
  }
  rel$ANIMAL_ID = toupper(rel$ANIMAL_ID)
  return(rel)
}
plotaccousticpaths = function(x, rel){
  statsframe = NULL
  mykml$addFolder(fid = "paths", name = "paths")
  raster.path = file.path("E:", "maps", "depthraster2.tif") #meters
  neighborhood = 16
  type = "random.walk"      
  
  trans = NULL
  r <- raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr>-280 & mr< -60)] = -170
  mr = apply(mr, 2, function(x) dnorm(x,mean=-170,sd=60))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, directions=neighborhood)
  if(type  == "random.walk"){
    trans <- geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans <- geoCorrection(tr, type = "c", scl=FALSE)
  }
  
  
  x$longitude = as.numeric(x$longitude)
  x$latitude = as.numeric(x$latitude)
  da2 = NULL
  x$spliton = paste(as.character(x$yearcollected), as.character(x$julianday), sep="")
  na = names(x)
  das = split(x, x$catalognumber, drop = T)
  
  for(i in 1:length(das)){
    sub = data.frame(das[i])
    names(sub) = na
    
    
    
    da2 = rbind(da2, sub[which(sub$receiver == "release"),][1,])
    relgrpind = which(as.character(sub$receiver) == "release")
    sub = sub[-relgrpind,]
    day = split(sub, sub$spliton, drop = T)
    for(j in 1:length(day)){
      dsub = data.frame(day[j])
      if(nrow(dsub) > 0){
        names(dsub) = na
        dsub$longitude = mean(dsub$longitude)
        dsub$latitude = mean(dsub$latitude)
        da2 = rbind(da2, dsub[1,])
      }
    }
    
  }
  
  da2$TimeStamp = ymd(da2$TimeStamp)
  da2 = da2[order(da2$TimeStamp),]
  pana = names(da2)
  pa = split(da2, da2$catalognumber, drop = T)
  
  for(j in 1:length(pa)){ 
    npa = data.frame(pa[j])
    
    names(npa) = pana
    
    twri = NULL
    if(nrow(npa)>1){
      for(i in 2:nrow(npa)){  
        start <- c(as.numeric(npa$longitude[i-1]), as.numeric(npa$latitude[i-1]))
        end <- c(as.numeric(npa$longitude[i]), as.numeric(npa$latitude[i]))
        days <-  npa$TimeStamp[i] - npa$TimeStamp[i-1]
        if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
          AtoB = rbind(start, end)
        }
        else{
          AtoB = shortestPath(trans, start, end, output="SpatialLines")
        }
        
        
        #cor = rbind(start, cor, end)
        
        
        cor = data.frame(coordinates(AtoB))
        names(cor) = c("x", "y")
        xrep = cor$x[1]
        yrep = cor$y[1]
        for(k in 1:(nrow(cor)-1)){
          
          if(cor$x[k] == xrep){ cor$x[k] = start[1] }
          else{ xrep = 1000000 }
          
          if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
          else{ yrep = 1000000 }
        }
        xrep = cor$x[nrow(cor)]
        yrep = cor$y[nrow(cor)]
        for(k in nrow(cor):2){
          if(cor$x[k] == xrep) cor$x[k] =  end[1]
          else xrep = 1000000
          
          if(cor$y[k] == yrep) cor$y[k] =  end[2]
          else yrep = 1000000
        }
        names(cor) = c("X", "Y")
        
        cor$PID = 1
        cor$POS = 1:nrow(cor)
        
        tpoly = as.PolySet(cor, projection = "LL")
        leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
        
        cor$lat = cor$Y
        cor$lon = cor$X
        cor$pid = cor$PID
        cor$pos = cor$POS
        style = rel$styleUrl[which(rel$ANIMAL_ID == gsub("ZSC-" ,"", npa$catalognumber[1]))]
        style = sub("rel", "line", style)
        pdes  = paste("<![CDATA[ </br>Distance: ", as.character(leng$length), "km</br> days: ",  days,"]]>", sep = "")
        print(pdes)
        print(cor)
        statsframe = rbind(statsframe,c(npa$catalognumber[1], as.character(days), leng))  
        mykml$getFolder("Detections")$getFolder("Paths")$addFolder(fid = as.character(npa$catalognumber[1]), name = as.character(npa$catalognumber[1]))
        mykml$getFolder("Detections")$getFolder("Paths")$getFolder(as.character(npa$catalognumber[1]))$addLineString(cor,  description = pdes,  styleUrl = style)
        #mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$getFolder("path")$addLineString(cor,  styleUrl = style)
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$addFolder(fid = "path", name = "path")
        #   
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$addFolder(fid = "path", name = "path")
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$getFolder("path")$addLineString(cor,  styleUrl = style)
      }
    }
  }
  return(statsframe)
}

generatekml = function(x){

rel = plotReleases()
#plotReceivers()



#   Read in detection data  #


dir =  file.path( "E:", "OTN", "detections", "zsc_matched_detections_2013.csv" )
da = read.csv( dir, sep=",", header=T, colClasses = "character" ) 

dir = file.path( "E:", "OTN", "detections", "zsc_matched_detections_2014.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" )) 

dir =  file.path( "E:", "OTN", "detections", "zsc_matched_detections_2015.csv" )
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

dir =  file.path( "E:", "OTN", "detections", "zsc_matched_detections_2016.csv" )
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

dir =  file.path( "E:", "OTN", "detections", "zsc_matched_detections_2017.csv" )
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

dir =  file.path( "E:", "OTN", "detections", "zsc_matched_detections_2018.csv" )
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))


#                           #
# dir = file.path( "C:", "workspace", "data", "OTN","detections", "temp_matched_detections.csv")
# da = rbind(da, read.csv( dir, sep=",", header=T ))

#                           #
dir = file.path( "E:", "OTN", "detections", "GDL_ZSC_animal_detections2016.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))
#                           #
dir = file.path( "E:", "OTN", "detections", "GDL_ZSC_animal_detections2015.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

# 
# dir = file.path( "C:", "workspace", "data", "OTN","detections", "zsc_qualified_detections_2015.csv")
# da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))
# 
# 
dir = file.path( "E:", "OTN", "detections", "zsc_unqualified.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))
#dir = file.path( "E:", "OTN", "detections", "zsc_unqualified_detections_2017.csv")
#da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))
# 

dir = file.path( "E:", "OTN", "detections", "zsc_matched_detections_on_other_deployments_2016.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))
dir = file.path( "E:", "OTN", "detections", "zsc_matched_detections_on_other_deployments_2017.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

dir = file.path( "E:", "OTN", "detections", "zsc_matched_detections_on_other_deployments_2018.csv")
da = rbind(da, read.csv( dir, sep=",", header=T, colClasses = "character" ))

da$catalognumber = as.character(da$catalognumber)
da$catalognumber = gsub("ONT", "OTN", da$catalognumber)
da$catalognumber[which(nchar(da$catalognumber)>11)] = gsub("OTN00", "OTN0", da$catalognumber[which(nchar(da$catalognumber)>11)])
#COrrect or elelase oof tag 

da$catalognumber[which(da$catalognumber == "ZSC-OTN0020"  & mdy_hm(as.character(da$datecollected)) > mdy_hm("07/30/2014 12:00"))] = "ZSC-OTN0168" 
print(da[which(da$catalognumber == "ZSC-OTN0168"),])

da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0168", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0169", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))

da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0170", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0171", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0172", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0173", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0174", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0175", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0183", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 11:36",	"UTC",
                 "-60.699883", "46.997467",	NA,	"2015",	"8",	"11",	"230",	"11.55",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))







da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0176", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0177", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0178", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0179", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0180", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0181", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


da = rbind(da, c(NULL,"ZSC", "ZSC-OTN0182", "Chionoecetes opilio",	"snow crab",
                 NA,	"release",	"ZSC",	"SCFA 19",	"release",	NA,	NA,	"release",
                 "A69-1601",	NA,	NA,	"release",	NA,	NA,	"8/11/2015 18:56",	"UTC",
                 "-61.022783", "46.83255",	NA,	"2015",	"8",	"11",	"230",	"18.56",	NA,	NA,	"GULF",	NA,	
                 "Zisserson, B., Choi, J., Cook, A., Cameron, B. 2013. Scotian Shelf snow crab tagging.",	"ZSC-OTN0168-release"))


ind = which(da$local_area == "Doctors Cove")
if(length(ind)>0)
  da = da[-ind,]

#   Date formating          # 
ind = which(nchar(da$monthcollected)<2)
if(length(ind) > 0 )da$monthcollected[ind] = paste("0", da$monthcollected[ind], sep = "")
ind = which(nchar(da$daycollected)<2)
if(length(ind) > 0 )da$daycollected[ind] = paste("0", da$daycollected[ind], sep = "")
da$TimeStamp = paste(da$yearcollected, da$monthcollected, da$daycollected, sep = "-")
da$name = da$catalognumber
da$timeofday[which(!grepl("\\.", da$timeofday))] = paste(da$timeofday[which(!grepl("\\.", da$timeofday))], ".0", sep = "")
da$hr = matrix(unlist(strsplit(as.character(da$timeofday), "[.]")),  ncol=2, byrow=TRUE)[,1]
da$min = as.character(ceiling(as.numeric(paste(".", matrix(unlist(strsplit(as.character(da$timeofday), "[.]")),  ncol=2, byrow=TRUE)[,2], sep=""))*60))


ind = which(nchar(da$hr)<2)
if(length(ind) > 0 )da$hr[ind] = paste("0", da$hr[ind], sep = "")
ind = which(nchar(da$min)<2)
if(length(ind) > 0 )da$min[ind] = paste("0", da$min[ind], sep = "")
da$longitude = as.character(abs(as.numeric(da$longitude))*-1)
dacpy = da
relgrpind = which(as.character(da$detectedby) == "release")

da = da[-relgrpind,]
relgrpind = which(as.character(da$receiver) == "release")

da = da[-relgrpind,]
da$styleUrl = "capunknown"
numlist = unique(da$catalognumber)
for(i in 1:length(numlist)){
  da$styleUrl[which(as.character(da$catalognumber) == as.character(numlist[i]))] = sub("rel", "cap", rel$styleUrl[which(sub("ZSC-", "", as.character(numlist[i])) == toupper(rel$ANIMAL_ID))])
}



# The following code simplifies the data by grouping detections. If more than three hours pass
# a new detection is recorded and the timespans are auusted
da2 = NULL
da$spliton = paste(as.character(da$station), as.character(da$catalognumber), sep="")
na = names(da)
das = split(da, da$spliton, drop = T)

for(i in 1:length(das)){
  sub = data.frame(das[i])
  names(sub) = na
  
  prevchron = NULL
  
  for(k in 1:nrow(sub)){
    print(sub)
    curchron = as.chron(mdy_hm(as.character(sub$datecollected[k])))
    if(is.na(curchron))curchron = as.chron(ymd_hms(as.character(sub$datecollected[k])))
    if(is.na(curchron))curchron = as.chron(dmy_hm(as.character(sub$datecollected[k])))
    if(is.null(prevchron)){
      newent = sub[k,]
      newent$TimeSpanStart = newent$TimeStamp
      newent$TimeSpanEnd = newent$TimeStamp
      newent$stime = format(curchron, sep = " ", c("month day, year",  "hh:mm:ss"))
      newent$etime = format(curchron, sep = " ", c("month day, year",  "hh:mm:ss"))
    }
    else{
      if(curchron - prevchron > times("03:00:00")){
        newent$TimeSpanEnd = sub$TimeStamp[k-1]
        newent$etime = format(prevchron, sep = " ", c("month day, year",  "hh:mm:ss"))
        da2 = rbind(da2, newent)
        newent = sub[k,]
        newent$TimeSpanStart = newent$TimeStamp
        newent$stime = format(curchron, sep = " ", c("month day, year",  "hh:mm:ss"))
        
      }
      else{
        newent$TimeSpanEnd = sub$TimeStamp[k]
        newent$etime = format(curchron, sep = " ", c("month day, year",  "hh:mm:ss"))      
      }        
    }    
    if(k == nrow(sub)){
      newent$TimeSpanEnd = sub$TimeStamp[k]
      newent$etime = format(curchron, sep = " ", c("month day, year",  "hh:mm:ss"))
      da2 = rbind(da2, newent)
    }
    prevchron = curchron
    
  }
  
}

# Description does not need to be this complicated. Wrapping in CDATA allows for nice 
# presentation via html

da2$description = paste("<![CDATA[ </br>Start Time: ", as.character(da2$stime),"</br>End Time: ", as.character(da2$etime), "</br>Station: ", da2$station,"</br>Detected by:", da2$detectedby, "]]>", sep = "")


mykml$addFolder("Detections", name = "Detections")
mykml$getFolder("Detections")$addFolder("Paths", name = "Paths")
# I want a seperate folder in the kml tree for each tag. 
# for loop that adds data to the proper folder 
for(i in 1:nrow(da2)){
  if(!is.null(mykml$getFolder("Detections")$getFolder(as.character(da2$catalognumber[i])))){
    mykml$getFolder("Detections")$getFolder(as.character(da2$catalognumber[i]))$addPoint(da2[i,], altitudeMode = "relativeToGround")
  }
  else{
    mykml$getFolder("Detections")$addFolder(as.character(da2$catalognumber[i]), name = as.character(da2$catalognumber[i]))
    mykml$getFolder("Detections")$getFolder(as.character(da2$catalognumber[i]))$addPoint(da2[i,], altitudeMode = "relativeToGround")
  }
}


#Fisherman removed tags, so we want to add their removals to this kml
dir =  file.path( "E:", "OTN","detections", "removal.csv" )
remove =  read.csv( dir, sep=",", header=T, colClasses = "character" ) 
remove$name = remove$tag_id
#remove$date = paste(as.character(remove$daycollected),as.character(remove$monthcollected),as.character(remove$yearcollected), sep = "/")

remove$date = chron(as.character(remove$date), format = c(dates="d/m/y"), out.format = c(dates="day month year"))
remove$description = paste("<![CDATA[ </br>TAG REMOVED FROM CRAB </br>Removal Time: ", as.character(remove$date), "]]>", sep = "")
tokml = matrix(unlist(strsplit(as.character(chron(dates(remove$date), out.format = c(dates = "d-m-y"))), "-")), ncol =3, byrow=T)
tokmly = matrix(unlist(strsplit(as.character(chron(dates(remove$date), out.format = c(dates = "d-m-yy"))), "-")), ncol =3, byrow=T)
remove$TimeStamp = paste(tokmly[,3], tokml[,2], tokml[,1], sep = "-" )
remove$styleUrl = NA
#imagepath = "http://enssnowcrab.com/kml/kmldeadcrab.png" 
#imagepath = file.path("C:", "workspace", "data", "OTN", "kmldeadcrab.png") 
#mykml$addIconStyle(styleid = "deadstyle", color = "grey", href = imagepath, scale = .5)
#mykml$addLabelStyle(styleid = "deadstyle", color = "#000000", transparency = .8, scale = .5)
for(i in 1:nrow(remove)){
  remove$styleUrl[i] = sub("rel", "dcap", rel$styleUrl[which(sub("ZSC-", "", as.character(remove$tag_id[i])) == rel$ANIMAL_ID)])
}
for(i in 1:nrow(remove)){
  dacpy = rbind(dacpy, c("ZSC", remove$tag_id[i], "Chionoecetes opilio", "snow crab", NA, 
                         "fisher", NA, NA, remove$person[i], NA,
                         NA, NA,  "A69-1601",  NA,	NA,	"pinger",
                         NA, NA, NA,	"UTC",	remove$longitude[i],	remove$latitude[i], NA, 
                         year(remove$date[i]),	month(remove$date[i]), day(remove$date[i]),
                         NA, "12.00",	NA,	NA,	NA, NA, NA, NA, paste(year(remove$date[i]),  month(remove$date[i]), day(remove$date[i]), sep ="-"),
                         remove$tag_id[i], "12",	"00"  ))
  if(!is.null(mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i])))){
    mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i]))$addPoint(remove[i,], altitudeMode = "relativeToGround" )
  }
  else{
    mykml$getFolder("Detections")$addFolder(as.character(remove$tag_id[i]), name = as.character(remove$tag_id[i]))
    mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i]))$addPoint(remove[i,], altitudeMode = "relativeToGround" )
  }
}




#Fisherman returned tags, so we want to add their returns to this kml
dir =  file.path( "E:", "OTN","detections", "returns.csv" )
remove =  read.csv( dir, sep=",", header=T, colClasses = "character" ) 
remove$name = remove$tag_id
#remove$date = paste(as.character(remove$daycollected),as.character(remove$monthcollected),as.character(remove$yearcollected), sep = "/")

remove$date = chron(as.character(remove$date), format = c(dates="d/m/y"), out.format = c(dates="day month year"))
remove$description = paste("<![CDATA[ </br>TAG CAPTUED AND RETURNED TO WATER </br>Capture Time: ", as.character(remove$date), "]]>", sep = "")
tokml = matrix(unlist(strsplit(as.character(chron(dates(remove$date), out.format = c(dates = "d-m-y"))), "-")), ncol =3, byrow=T)
tokmly = matrix(unlist(strsplit(as.character(chron(dates(remove$date), out.format = c(dates = "d-m-yy"))), "-")), ncol =3, byrow=T)
remove$TimeStamp = paste(tokmly[,3], tokml[,2], tokml[,1], sep = "-" )
remove$styleUrl = NA
#imagepath = "http://enssnowcrab.com/kml/kmldeadcrab.png" 
#imagepath = file.path("C:", "workspace", "data", "OTN", "kmldeadcrab.png") 
#mykml$addIconStyle(styleid = "deadstyle", color = "grey", href = imagepath, scale = .5)
#mykml$addLabelStyle(styleid = "deadstyle", color = "#000000", transparency = .8, scale = .5)

for(i in 1:nrow(remove)){
  print(remove[i,])
  remove$styleUrl[i] = sub("rel", "fcap", rel$styleUrl[which(sub("ZSC-", "", as.character(remove$tag_id[i])) == toupper(rel$ANIMAL_ID))])
}
for(i in 1:nrow(remove)){
  tmp <- as.POSIXlt(remove$date[i], format = "%d%b%y")
  tmp$yday
  dacpy = rbind(dacpy, c("ZSC", remove$tag_id[i], "Chionoecetes opilio", "snow crab", NA, 
                         "fisher", NA, NA, remove$person[i], NA,
                         NA, NA,  "A69-1601",	NA,	NA,	"pinger",
                         NA, NA, NA,	"UTC",	remove$longitude[i],	remove$latitude[i], NA, 
                         year(remove$date[i]),	month(remove$date[i]), day(remove$date[i]),
                         as.character(tmp$yday), "12.00",	NA,	NA,	NA, NA, NA, NA, paste(year(remove$date[i]),  month(remove$date[i]), day(remove$date[i]), sep ="-"),
                         remove$tag_id[i], "12",	"00"  ))
  if(!is.null(mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i])))){
    mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i]))$addPoint(remove[i,], altitudeMode = "relativeToGround" )
  }
  else{
    mykml$getFolder("Detections")$addFolder(as.character(remove$tag_id[i]), name = as.character(remove$tag_id[i]))
    mykml$getFolder("Detections")$getFolder(as.character(remove$tag_id[i]))$addPoint(remove[i,], altitudeMode = "relativeToGround" )
  }
}



sf = plotaccousticpaths(dacpy, rel)


sft = sf
sft = cbind(unlist(sft[,1]),unlist(sft[,2]),unlist(sft[,3]),unlist(sft[,4])) 
sft = data.frame(sft, stringsAsFactors = F)
names(sft) = c("ID", "Days", "PID", "Dist")
sft$Days = as.numeric(sft$Days)
sft$Dist = as.numeric(sft$Dist)
sft$Dist = as.numeric(as.character(sft$Dist))
sfsum = aggregate(cbind(sft$Days, sft$Dist), by=list(Category=sft$ID), FUN=sum)
names(sfsum) = c("ID", "Days", "Km")
darel = dacpy[which(dacpy$receiver == "release"),]
sfsum$rellat = NA
sfsum$rellon = NA
sfsum$relyear = NA
for(i in 1:nrow(sfsum)){
  sfsum$rellat[i] = darel$latitude[which(darel$catalognumber == sfsum$ID[i])[1]] 
  sfsum$rellon[i] = darel$longitude[which(darel$catalognumber == sfsum$ID[i])[1]] 
  sfsum$relyear[i] = darel$yearcollected[which(darel$catalognumber == sfsum$ID[i])[1]] 
  
}
sfsum$relarea = absolutely.in.area2("unknown", sfsum$rellon, sfsum$rellat)
library(easyGgplot2)
library(colorRamps) 


ggplot2.scatterplot(data=sfsum, backgroundColor="white", size=5, mainTitle="Kilometers vs. Days by Release Area", xName='Days',yName='Km', groupName="relyear", faceting=TRUE, facetingVarNames="relarea")

mykml$preview()


mykml$writekml(file.path( "E:", "OTN", "otn.kml" ))
}