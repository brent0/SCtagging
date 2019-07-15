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
write.acoustic.releases = function(dir = file.path("D:", "OTN", "releases")){
  
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
write.acoustic.detections = function(dir = file.path("D:", "OTN", "detections")){
  
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
      print(prevchron)
      print(curchron)
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
