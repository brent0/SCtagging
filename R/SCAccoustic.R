#' @title  get.acoustic.releases
#' @description  Write release data to Oracle Database
#' @import ROracle 
#' @return dataframe
#' @export
get.acoustic.releases = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  
  
  drv <- dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  
  
  respat <- ROracle::dbSendQuery(con, paste("select * from SCT_ACOUSTIC_RELEASES", gstring, sep = ""))
  respat <- fetch(respat)
  ROracle::dbDisconnect(con)
  
  return(respat)
}
#' @title  get.acoustic.detections
#' @description  Get accoustic detection data from Oracle Database
#' @import ROracle 
#' @return dataframe
#' @export
get.acoustic.detections = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  
  
  drv <- dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  
  
  respat <- ROracle::dbSendQuery(con, paste("select * from SCT_ACOUSTIC_DETECTIONS", gstring, sep = ""))
  respat <- fetch(respat)
  ROracle::dbDisconnect(con)
  
  return(respat)
}
#' @title  write.acoustic.releases
#' @description  Write release data to Oracle Database
#' @import ROracle 
#' @return dataframe
#' @export
write.acoustic.releases = function(dir = file.path("E:", "OTN", "releases")){
  
  dftowrite = NULL
  
  
  files = list.files(dir, full.names = T)
  
  
  for(i in 1:length(files)){
    
    da = read.csv(files[i], sep=",", header=T ) 
    print(names(da))

    dftowrite = rbind(dftowrite, da)
    names(dftowrite) = names(dftowrite)
  }
  
  dftowrite$ANIMAL_ID = toupper(dftowrite$ANIMAL_ID)
  

  names(dftowrite) = c("ANIMAL_ID", "TAG_TYPE", "TAG_MANUFACTURER", "TAG_MODEL", "TAG_SERIAL_NUMBER", "TAG_ID_CODE", "TAG_CODE_SPACE", "TAG_IMPLANT_TYPE",
                       "TAG_IMPLANT_METHOD", "TAG_ACTIVATION_DATE", "EST_TAG_LIFE", "TAGGER", "TAG_OWNER_PI", "TAG_OWNER_ORGANIZATION", "COMMON_NAME_E",
                       "SCIENTIFIC_NAME", "CAPTURE_LOCATION", "CAPTURE_LATITUDE", "CAPTURE_LONGITUDE", "WILD_OR_HATCHERY", "STOCK", "LENGTH_m", "WEIGHT_kg",
                       "LENGTH_TYPE", "LENGTH2_m", "LENGTH2_TYPE", "LIFE_STAGE", "AGE", "AGE_UNITS", "SEX", "DNA_SAMPLE_TAKEN", "TREATMENT_TYPE", "RELEASE_GROUP",
                       "RELEASE_LOCATION", "RELEASE_LATITUDE", "RELEASE_LONGITUDE", "UTC_RELEASE_DATE_TIME", "CAPTURE_DEPTH_m","TEMP_CHANGE_C", "HOLDING_TEMP_C",
                       "PREOP_HOLD_PERIOD", "POSTOP_HOLD_PERIOD", "SURGERY_LOCATION", "DATE_OF_SURGERY", "SURGERY_LATITUDE", "SURGERY_LONGITUDE", "SEDATIVE",
                       "SEDATIVE_CONC_ppm", "ANAESTHETIC", "BUFFER", "ANAES_CONC_ppm", "BUF_CONC_IN_ANAES_ppm", "ANAES_CONC_IN_RECIRC_ppm",
                       "BUFF_CONC_IN_RECIRC_ppm", "DISSOLVED_OXYGEN_ppm", "COMMENTS")
  
  
  
  
  # con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  respat =  get.acoustic.releases()
  ind  = which(!paste(dftowrite$ANIMAL_ID, dftowrite$TAG_SERIAL_NUMBER, sep = ".") %in% paste(respat$ANIMAL_ID, respat$TAG_SERIAL_NUMBER, sep = "."))
  if(length(ind>0)){
    dftowrite = dftowrite[ind,]
    drv <- DBI::dbDriver("Oracle")
    con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    dbWriteTable(con,"SCT_ACOUSTIC_RELEASES", dftowrite, append = TRUE)
    dbDisconnect(con)
    
  }
  else(
    print("No new records to add.")
  )
  
}

#' @title  get.receiver.data
#' @description  Get receiver data from OTN wms server
#' @return dataframe
#' @export
get.receiver.data = function(){
  x = read.csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:stations_series&bbox=41.0,-68,50.5,-56,urn:ogc:def:crs:EPSG:4326&outputFormat=csv')
return(x)
}

#' @title  write.acoustic.detections
#' @description  Write release data to Oracle Database
#' @import ROracle lubridate
#' @return dataframe
#' @export
write.acoustic.detections = function(dir = file.path("E:", "OTN", "detections")){
  
  dftowrite = NULL
  
  
  files = list.files(dir, full.names = T)
  
  
  for(i in 1:length(files)){
    if(grepl("removal", files[i])){
      dx = read.csv(files[i], sep=",", header=T ) 
      dx$collectioncode = "ZSC"
      dx$catalognumber = dx$tag_id
      dx$scientificname = "Chionoecetes opilio"
      dx$commonname = "snow crab"
      dx$datelastmodified = as.character(format.Date(dmy(dx$date), format = "%m/%d/%Y"))
      dx$detectedby = "fisher removal"
      dx$receiver_group = "fisher"   
      dx$station = "Trap"           
      dx$receiver = da$person         
      dx$bottom_depth = NA
      dx$receiver_depth = NA
      dx$tagname = dx$ser
      dx$codespace = dx$spa
      dx$sensorname = NA 
      dx$sensorraw = NA
      dx$sensortype = NA
      dx$sensorvalue = NA
      dx$sensorunit = NA
      dx$datecollected = as.character(format.Date(dmy(dx$date), format = "%m/%d/%Y"))
      dx$timezone = "UTC"
      #dx$longitude
      #dx$latitude
      dx$st_setsrid_4326 =NA
      dx$yearcollected = year(dmy(dx$date))
      dx$monthcollected = month(dmy(dx$date))
      dx$daycollected = day(dmy(dx$date))
      dx$julianday = format(dmy(dx$date), "%j")
      dx$timeofday = 12
      dx$datereleasedtagger = NA
      dx$datereleasedpublic = NA
      dx$local_area = "SCOTIAN SHELF"
      dx$notes = NA
      dx$citation = NA
      dx$unqdetecid = NA
      dx$tag_id = NULL
      dx$ser = NULL
      dx$spa = NULL
      dx$date = NULL
      dx$person = NULL
      dftowrite = rbind(dftowrite, da)
      next()
    }
    if(grepl("returns.csv", files[i])){
      dx = read.csv(files[i], sep=",", header=T ) 
      dx$collectioncode = "ZSC"
      dx$catalognumber = dx$tag_id
      dx$scientificname = "Chionoecetes opilio"
      dx$commonname = "snow crab"
      dx$datelastmodified = as.character(format.Date(dmy(dx$date), format = "%m/%d/%Y"))
      dx$detectedby = "fisher return"
      dx$receiver_group = "fisher"   
      dx$station = "Trap"           
      dx$receiver = da$person         
      dx$bottom_depth = NA
      dx$receiver_depth = NA
      dx$tagname = dx$ser
      dx$codespace = dx$spa
      dx$sensorname = NA 
      dx$sensorraw = NA
      dx$sensortype = NA
      dx$sensorvalue = NA
      dx$sensorunit = NA
      dx$datecollected = as.character(format.Date(dmy(dx$date), format = "%m/%d/%Y"))
      dx$timezone = "UTC"
      #dx$longitude
      #dx$latitude
      dx$st_setsrid_4326 =NA
      dx$yearcollected = year(dmy(dx$date))
      dx$monthcollected = month(dmy(dx$date))
      dx$daycollected = day(dmy(dx$date))
      dx$julianday = format(dmy(dx$date), "%j")
      dx$timeofday = 12
      dx$datereleasedtagger = NA
      dx$datereleasedpublic = NA
      dx$local_area = "SCOTIAN SHELF"
      dx$notes = NA
      dx$citation = NA
      dx$unqdetecid = NA
      dx$tag_id = NULL
      dx$ser = NULL
      dx$spa = NULL
      dx$date = NULL
      dx$person = NULL
      dftowrite = rbind(dftowrite, da)
      next()
    }
    if(grepl("returns_emera", files[i])){
      
      next()
    }   
    if(grepl("qualified_detections", files[i])){
      
      next()
    }
    da = read.csv(files[i], sep=",", header=T ) 
    print(i)
    if(is.na(ymd_hms(da$datecollected)[1])){
      da$datecollected = as.character(format.Date(mdy_hm(da$datecollected), format = "%m/%d/%Y %H:%M:%S"))
      
    }
    else{
      da$datecollected = as.character(format.Date(ymd_hms(da$datecollected), format = "%m/%d/%Y %H:%M:%S"))
      
    }
    #da$datecollected = as.character(format.Date(dmy(da$date), format = "%m/%d/%Y"))

    dftowrite = rbind(dftowrite, da)
    names(dftowrite) = names(dftowrite)
  }

  #da$catalognumber[which(da$catalognumber == "ZSC-OTN0020"  & mdy_hm(as.character(da$datecollected)) > mdy_hm("07/30/2014 12:00"))] = "ZSC-OTN0168" 
  #da$catalognumber = gsub("ONT", "OTN", da$catalognumber)
 # da$catalognumber[which(nchar(da$catalognumber)>11)] = gsub("OTN00", "OTN0", da$catalognumber[which(nchar(da$catalognumber)>11)])
  #COrrect or elelase oof tag 
  
  
  names(dftowrite) = c("COLLECTIONCODE","CATALOGNUMBER","SCIENTIFICNAME","COMMONNAME","DATELASTMODIFIED"
                       ,"DETECTEDBY","RECEIVER_GROUP","STATION","RECEIVER","BOTTOM_DEPTH","RECEIVER_DEPTH"
                       ,"TAGNAME","CODESPACE","SENSORNAME","SENSORRAW","SENSORTYPE","SENSORVALUE","SENSORUNIT"
                       ,"DATECOLLECTED","TIMEZONE","LONGITUDE","LATITUDE","ST_SETSRID_4326","YEARCOLLECTED"
                       ,"MONTHCOLLECTED","DAYCOLLECTED","JULIANDAY","TIMEOFDAY","DATERELEASEDTAGGER"
                       ,"DATERELEASEDPUBLIC","LOCAL_AREA","NOTES","CITATION","UNQDETECID")
  ind = which(dftowrite$LOCAL_AREA == "Doctors Cove")
  if(length(ind) >0){
    dftowrite = dftowrite[-ind,]
  }
  ind = which(dftowrite$local_area == "Malpeque Bay")
  if(length(ind)>0)
    dftowrite = dftowrite[-ind,]
  ind = which(dftowrite$local_area == "Various locations")
  if(length(ind)>0)
    dftowrite = dftowrite[-ind,]
  ind = which(dftowrite$local_area == "Saint John River")
  if(length(ind)>0)
    dftowrite = dftowrite[-ind,]
  ind = which(dftowrite$local_area == "Navy Island")
  if(length(ind)>0)
    dftowrite = dftowrite[-ind,]
  
  # con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  respat =  get.acoustic.detections()
  ind  = which(!paste(dftowrite$CATALOGNUMBER, dftowrite$DATECOLLECTED, sep = ".") %in% paste(respat$CATALOGNUMBER, respat$DATECOLLECTED, sep = "."))
  if(length(ind>0)){
    dftowrite = dftowrite[ind,]
    drv <- DBI::dbDriver("Oracle")
    con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  
     dbWriteTable(con,"SCT_ACOUSTIC_DETECTIONS", dftowrite, append = TRUE)
    dbDisconnect(con)
    
  }
  else(
    print("No new records to add.")
  )
  
}



#' @title  compress.detections
#' @description  Simplifies the data by grouping detections.
#' @import ROracle lubridate
#' @return dataframe
#' @export
compress.detections = function(hours.thres = 3){
# The following code simplifies the data by grouping detections. If more than three hours pass
# a new detection is recorded and the timespans are ajusted
  da = get.acoustic.detections()
  da$TimeStamp = ymd_hms(da$DATECOLLECTED)
  da$TimeStamp[which(is.na(da$TimeStamp))] = dmy_hm(da$DATECOLLECTED[which(is.na(da$TimeStamp))]) 
  da$TimeStamp[which(is.na(da$TimeStamp))] = dmy_hms(da$DATECOLLECTED[which(is.na(da$TimeStamp))]) 
  da$TimeStamp[which(is.na(da$TimeStamp))] = mdy_hm(da$DATECOLLECTED[which(is.na(da$TimeStamp))]) 
  da$TimeStamp[which(is.na(da$TimeStamp))] = mdy_hms(da$DATECOLLECTED[which(is.na(da$TimeStamp))]) 
  da = da[order(da$TimeStamp),]
  
  
  da2 = NULL
da$spliton = paste(as.character(da$STATION), as.character(da$CATALOGNUMBER), sep="")
na = names(da)
das = split(da, da$spliton, drop = T)

for(i in 1:length(das)){
  sub = data.frame(das[i])
  names(sub) = na
  print(sub$spliton[1])
  prevchron = NULL

  for(k in 1:nrow(sub)){
    
    curchron = mdy_hm(sub$DATECOLLECTED[k])
    if(is.na(curchron))curchron = ymd_hms(sub$DATECOLLECTED[k])
    if(is.na(curchron))curchron = dmy_hm(sub$DATECOLLECTED[k])
    if(is.na(curchron))curchron = mdy_hms(sub$DATECOLLECTED[k]) 
    if(is.na(curchron))curchron = dmy_hms(sub$DATECOLLECTED[k])
    if(is.null(prevchron)){
      newent = sub[k,]
      newent$TimeSpanStart = newent$TimeStamp
      newent$TimeSpanEnd = newent$TimeStamp
      newent$stime = curchron
      newent$etime = curchron
    }
    else{
   
      if(curchron - prevchron > hours(hours.thres)){
        newent$TimeSpanEnd = sub$TimeStamp[k-1]
        newent$etime = prevchron
        da2 = rbind(da2, newent)
        newent = sub[k,]
        newent$TimeSpanStart = newent$TimeStamp
        newent$stime = curchron
      }
      else{
        newent$TimeSpanEnd = sub$TimeStamp[k]
        newent$etime = curchron    
      }        
    }    
    if(k == nrow(sub)){
      newent$TimeSpanEnd = sub$TimeStamp[k]
      newent$etime = curchron
      da2 = rbind(da2, newent)
    }
    prevchron = curchron
    
  }
  
}
return(da2)
}
#' @title  write.acoustic.paths
#' @description  Write calculated path data to Oracle Database
#' @import ROracle geosphere PBSmapping raster gdistance lubridate
#' @return dataframe
#' @export
write.accoustic.paths = function(){
 # x = get.acoustic.detections()
  x = compress.detections()
  rel = get.acoustic.releases()
 
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
  
  statsframe = NULL
  pathframe = NULL
  
  da2 = NULL
  x$spliton = paste(as.character(x$YEARCOLLECTED), as.character(x$JULIANDAY), sep="")
  na = names(x)
  das = split(x, x$CATALOGNUMBER, drop = T)
  
  for(i in 1:length(das)){
    sub = data.frame(das[i])
    names(sub) = na
    
    
    
    da2 = rbind(da2, sub[which(sub$RECEIVER == "release"),][1,])
    relgrpind = which(as.character(sub$RECEIVER) == "release")
    sub = sub[-relgrpind,]
    day = split(sub, sub$spliton, drop = T)
    for(j in 1:length(day)){
      dsub = data.frame(day[j])
      if(nrow(dsub) > 0){
        names(dsub) = na
        dsub$LONGITUDE = mean(dsub$LONGITUDE)
        dsub$LATITUDE = mean(dsub$LATITUDE)
        da2 = rbind(da2, dsub[1,])
      }
    }
    
  }
  
  #da2$TimeStamp = ymd(da2$TimeStamp)
  da2 = da2[order(da2$TimeStamp),]
  pana = names(da2)
  pa = split(da2, da2$CATALOGNUMBER, drop = T)
  
  for(j in 1:length(pa)){ 
    npa = data.frame(pa[j])
    
    names(npa) = pana
    
    twri = NULL
    if(nrow(npa)>1){
      for(i in 2:nrow(npa)){  
        start <- c(as.numeric(npa$LONGITUDE[i-1]), as.numeric(npa$LATITUDE[i-1]))
        end <- c(as.numeric(npa$LONGITUDE[i]), as.numeric(npa$LATITUDE[i]))
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
        cor$pid = npa$CATALOGNUMBER[1]
        cor$cid = i-1
        cor$pos = cor$POS
        cor$lat = cor$Y
        cor$lon = cor$X
 
      
        cor$Y = NULL
        cor$X = NULL
        cor$PID = NULL
        cor$POS = NULL
        
          #  style = rel$styleUrl[which(rel$ANIMAL_ID == gsub("ZSC-" ,"", npa$catalognumber[1]))]
       # style = sub("rel", "line", style)
      #  pdes  = paste("<![CDATA[ </br>Distance: ", as.character(leng$length), "km</br> days: ",  days,"]]>", sep = "")
        #print(pdes)
   
        statsframe = rbind(statsframe,cbind(npa$CATALOGNUMBER[1], as.character(i-1), as.character(npa$TimeStamp[i-1]), as.character(npa$TimeStamp[i]), leng$length))  
      pathframe = rbind(pathframe, cor)  
      
        
         # mykml$getFolder("Detections")$getFolder("Paths")$addFolder(fid = as.character(npa$catalognumber[1]), name = as.character(npa$catalognumber[1]))
        #mykml$getFolder("Detections")$getFolder("Paths")$getFolder(as.character(npa$catalognumber[1]))$addLineString(cor,  description = pdes,  styleUrl = style)
        #mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$getFolder("path")$addLineString(cor,  styleUrl = style)
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$addFolder(fid = "path", name = "path")
        #   
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$addFolder(fid = "path", name = "path")
        #   mykml$getFolder("Detections")$getFolder(as.character(npa$catalognumber[1]))$getFolder("path")$addLineString(cor,  styleUrl = style)
      }
    }
  }
  pfr = as.data.frame(pathframe)
  sfr = as.data.frame(statsframe)
  names(sfr) = c("PID", "CID", "SDATE", "EDATE", "DIST")
  names(pfr) = c("PID", "CID", "POS", "LAT", "LONG")
  drv <- DBI::dbDriver("Oracle")
  con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    dbWriteTable(con,"SCT_ACCOUSTIC_PATHS", pfr, overwrite = T)
    dbWriteTable(con,"SCT_ACCOUSTIC_PATH", sfr, overwrite = T)

  }
MCsubdetectionsOTN = function(quality = 1){
  require("chron")
  q = quality
  direct = "C://project"
  
  
  
  # addRaster(file.path(direct,"mapping", "maps","Raster", "bathyWGS_PCT.tif"), quality = 1)
  
  
  
  dir = file.path(direct,"OTN", "detections")
  fnames = list.files(dir)
  data = NULL
  for(i in 1 : length(fnames)){
    if(grepl(".csv", fnames[i])){
      datasub = read.csv(file.path(dir, fnames[i]))
      data = rbind(data, datasub)
    }
  }
  dd = as.character(data$datecollected)
  
  d = strsplit(dd, " ")
  dd = matrix(unlist(d), ncol=2, byrow=TRUE)[,1] 
  dt = matrix(unlist(d), ncol=2, byrow=TRUE)[,2]
  
  #dt = unlist(strsplit(dd, " "))[2]
  #  data = data[order(as.Date(data$datecollected, format="%Y-%m-%d hh:mm:ss")),]    
  data$datecollected = chron(dd, dt, format=c(dates="Y-m-d", times = "h:m:s"))
  data = data[order(data$datecollected),]
  
  
  data$PID = as.character(data$catalognumber)
  data$PID = gsub("ZSC-OTN", "", data$PID)
  data$PID = as.numeric(data$PID)
  
  data$X = as.numeric(as.character(data$longitude))
  data$Y = as.numeric(as.character(data$latitude))
  
  da = unique(data$PID)
  for(k in 1:length(da)){
    dat = data[which(data$PID == da[k]),]
    
    if(nrow(dat)>1){
      
      dat$POS = 1:nrow(dat)
      
      xlim = c(min(dat$X)-.10, max(dat$X)+.10)
      ylim = c(min(dat$Y)-.10, max(dat$Y)+.10)
      plotRaster( file.path(direct,"mapping", "maps","Charts", "801_LL_WGS84_PCT.tif"),
                  xlab="Longitude", main = paste("OTN Detections: ",dat$catalognumber[1], sep=""), ylab="Latitude", outer = T, axes=T, tck=0,
                  fade = .5, tckLab=F, xlim = xlim, ylim = ylim, quality = 1, cellcount = NULL)
      
      
      addlinesSCAREA(lwd = 1)
      addMPA()
      
      
      setdata = as.PolySet(data.frame(dat), projection = "LL")
      addLines(setdata, lwd = 1*q, col = "blue", arrows = T, length = .1)
      dat$label = as.character(dat$datecollected)
      dat$PID = 1:nrow(dat)
      ddata = as.PolyData(data.frame(dat), projection = "LL")
      addLabels(ddata, font = .5, cex = 1*q, col = "black")
      addEmera()
      addDivisions(xlim, ylim)
      degAxis(1, lwd = 1*q)
      degAxis(2, lwd = 1*q)
      
    }
    
  }
  
}
OTNreport = function(){
  direct = "C://project"
  
  fn = file.path(direct, "OTN","detections2015.pdf")
  
  pdf(file = fn)
  
  MCdetectionsOTN(quality = 1)
  MCsubdetectionsOTN(quality = 1)
  plottags(are = "nens_gulf", years = "2009,2010,2011,2012,2013", cex.main = .5)
  plottagsEMERA(are = "nens_gulf", years = "2009,2010,2011,2012,2013,2014", quality = .2)
  dev.off()
}
stats = function(area, years){
  
  data = plottags(area, years, shortpath = T, plot = F)
  names(data) = c("id", "first_dis", "tot_dis", "first_days", "tot_days")
  
  subdata = data[which(as.numeric(as.character(data$first_days)) > 10),]
  subdata = subdata[which(as.numeric(as.character(subdata$first_dis)) > 0),]
  print(   paste("Mean displacement to first capture: ", mean(as.numeric(as.character(subdata$first_dis)))))
  
  
  print(paste("Max displacement to first capture: ", max(as.numeric(as.character(subdata$first_dis)))))
  print(paste("Max displacement to last capture: ", max(as.numeric(as.character(data$tot_dis)))))
  
  print(paste("Mean days to first capture: ", mean(as.numeric(as.character(subdata$first_days)))))
  print(paste("Max days to first capture: ", max(as.numeric(as.character(subdata$first_days)))))
  print(paste("Max days to last capture: ", max(as.numeric(as.character(data$tot_days)))))
  
  kmm = sum(as.numeric(as.character(subdata$tot_dis)))/sum(as.numeric(as.character(subdata$tot_days)))
  kmm = (kmm*365.26)/12
  print(paste("km/month: ", kmm ))
  
  
  
  pdf("distances.pdf")
  hist(as.numeric(as.character(subdata$first_dis)),breaks=100, col="red",main="Distances Travelled (Shortest Path)",xlab="Distance(km)")
  dev.off()
  pdf("days.pdf")
  hist(as.numeric(as.character(subdata$tot_days)),main="Days To Last Known Capture",xlab="Time(days)")
  dev.off()
  pdf("tofirstdays.pdf")
  hist(as.numeric(as.character(subdata$first_days)),breaks=100, col="red", main="Days To First Capture",xlab="Time(days)")
  dev.off()
  
  return(data)
  
}