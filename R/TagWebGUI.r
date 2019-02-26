



#' @title  ret_ent
#' @description  Function that enters returndata entered in the html app 
#' @import RODBC jsonlite stringr sp rgeos
#' @return message to webpage
#' @export
ret_ent <- function(ddata){
  
  
  conn = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  # Check connection
  if (conn == -1){
    return(toJSON("Connection failed"))
  }
  out = ""
  
  ent = myUrlEncode(ddata)
  
  ent = unlist(str_split(ent, "&"))
  
  #radio-choice-2 
  reg = "";
  rc2 = "";
  per = "";
  tid = "";
  lat = "";
  lon = "";
  date = "";
  depth = "";
  ves = "";
  
  add = "";
  str = "";
  rou = "";
  loc = "";
  pro = "";
  cou = "";
  poc = "";
  ema = "";
  phoa = "";
  phob = "";
  com = "";
  shellcond = "";
  
  
  for(i in 1:length(ent)){
    if(ent[i] != ""){
      
      sa = unlist(str_split(ent[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "radio-choice-2")
        rc2 = sa[2]
      if(sa[1] == "per")
        per = sa[2]
      if(sa[1] == "tid")
        tid = sa[2]
      if(sa[1] == "lat")
        lat = sa[2]
      if(sa[1] == "lon")
        lon = sa[2]
      if(sa[1] == "date")
        date = sa[2]
      if(sa[1] == "depth")
        depth = sa[2]
      if(sa[1] == "ves")
        ves = sa[2]
      
      if(sa[1] == "add")
        add = sa[2]
      if(sa[1] == "str")
        str = sa[2]
      if(sa[1] == "rou")
        rou = sa[2]
      if(sa[1] == "loc")
        loc = sa[2]
      if(sa[1] == "pro")
        pro = sa[2]
      if(sa[1] == "cou")
        cou = sa[2]
      if(sa[1] == "poc")
        poc = sa[2]
      if(sa[1] == "ema")
        ema = sa[2]
      if(sa[1] == "phoa")
        phoa = sa[2]
      if(sa[1] == "phob")
        phob = sa[2]
      if(sa[1] == "comments")
        com = sa[2]
      if(sa[1] == "shellcond")
        shellcond = sa[2]
      
    }
    
  }
  
  lat = str_replace(lat, "N","")
  lon = str_replace(lon, "W","")
  
  rlat = as.character(conpos(lat))
  rlon = as.character((conpos(lon)*-1))
  
  statsarea = "ENS"
  subarea = ""
  
  
  
  
  if(absolutely.in.area("cfa23", rlon, rlat)){ 
    statsarea = "sens"
    subarea = "(cfa23)(all)(ens)(sens)(allandgulf)(cfa23zoom)(all.holes)"
  }
  if(absolutely.in.area("cfa24", rlon, rlat)){ 
    statsarea = "sens"
    subarea = "(cfa24)(all)(ens)(sens)(allandgulf)(cfa24zoom)(all.holes)"
  }
  if(absolutely.in.area("gulf", rlon, rlat)){ 
    statsarea = "gulf"
    subarea = "(gulf)(nens_gulf)(allandgulf)"
  }
  if(absolutely.in.area("nens", rlon, rlat)){ 
    statsarea = "nens"
    subarea = "(all)(ens)(nens)(nens_gulf)(allandgulf)"
  }
  if(absolutely.in.area("cfa4x", rlon, rlat)){ 
    statsarea = "cfa4x"
    subarea = "(all)(ens)(cfa4x)(allandgulf)"
  }
  
  
  
  df = unlist(str_split(date, "/"))
  
  
  
  
  
  year = df[3]
  mon = df[1]
  day = df[2]
  dat = paste(day, mon, year, sep = "/")
  
  
  
  ret = 2
  if(rc2 == "choice-1")ret = 1
  
  
  add = unlist(str_split(add, ","))[1]
  
  if(add == ""){
    if(rou != "")
      add = paste(str, rou, sep = ", ")
    else add = str
  }
  gstring = ""
  if(reg == "g"){
    gstring = "_GULF"
  }
  toda1 = paste("insert into SCT_CAPTURE", gstring, " (TAG, CAPTURE_DATE, PERSON, PERSON_B, LAT_DDMM_MM, LONG_DDMM_MM, LAT_DD_DDDD, LONG_DD_DDDD, FATHOMS, 
                RELCODE, COMMENTS, CAPTAIN, VESSEL, YEAR, STATSAREA, CARAPACE_COND, REWARDED, SUBAREA) 
                values ('", tid,"', to_date('", dat,"', 'dd/mm/yyyy'),'", SQLsafty(per),"','NA','", lat,"','", lon,"','", rlat,"','", rlon,"','", depth,
                "','", ret,"','", SQLsafty(com),"','", SQLsafty(per),"','", SQLsafty(ves),"','", year,"','", statsarea,"','", shellcond,
                "','N','", subarea, "');", sep = "")
  
  que1 = paste("select count(NAME) num from SCT_PEOPLE", gstring, "  where NAME = '", per,"'", sep = "")
  toda2 = paste("insert into SCT_PEOPLE", gstring, "  (NAME, CIVIC, TOWN, PROV, POST, EMAIL, PHO1, PHO2, COUNTRY) values ('", SQLsafty(per),"','", SQLsafty(add),"','", SQLsafty(loc),"','", SQLsafty(pro),"','", poc,"','", SQLsafty(ema),"','", SQLsafty(phoa),"','", SQLsafty(phob), "','", SQLsafty(cou), "');", sep = "")
  toda4 = paste("update SCT_PEOPLE", gstring, "  set NAME = '", SQLsafty(per),"', CIVIC = '", SQLsafty(add),"', TOWN = '", SQLsafty(loc),"', PROV = '", SQLsafty(pro),"', POST = '", poc,"', EMAIL = '", SQLsafty(ema),"', PHO1 = '", SQLsafty(phoa),
                "', PHO2 = '", SQLsafty(phob),"', COUNTRY = '", SQLsafty(cou),"' where NAME = '", per, "';", sep = "")
  
  result = RODBC::sqlQuery(conn, que1)
  
  
  toda = toda2
  out = paste("New person added to SCT_PEOPLE", gstring, "  table: ", per, sep = "")
  if(result == 1){
    toda = toda4
    out = paste("Updated data in SCT_PEOPLE", gstring, "  table for person: ", per, sep = "")
  }
  
  out = paste(out, "\n", sep = "")
  
  result = RODBC::sqlQuery(conn, toda)
  
  result = RODBC::sqlQuery(conn, toda1)
  out = paste(out, "New entry added to SCT_CAPTURE", gstring, "  with TAG_ID: ", tid, sep = "")
  out = paste(out, "\n", sep = "")
  out = paste(out, "\n", sep = "")
  
  odbcClose(conn)
  return(out)
}


#' @title  SC_Stats_Sample
#' @description  Function generates stats for display on webpage 
#' @import RODBC jsonlite
#' @return message to webpage
#' @export
SC_Stats_Sample <- function(area = "", years = "", region = "SoctianShelf"){
  
  # $runscript = "/home3/enssnowc/public_html/snowcrabgroup/tag/init.r";
  # $runsampscript = "/home3/enssnowc/public_html/snowcrabgroup/tag/initsamp.r";
  # $runstats = "/home3/enssnowc/public_html/snowcrabgroup/tag/initstats.r";
  y = unlist(strsplit(years, ","))
  
}
#FUNCTION THAT PRINTS THE DATA TO FILE THAT WILL BE READ BY SERVER

#' @title  SC_Stats_Capture
#' @description  Function generates stats for display on webpage 
#' @import RODBC jsonlite
#' @return message to webpage
#' @export
SC_Stats_Capture = function(are= "", years = "", region = "SoctianShelf"){
  y = unlist(strsplit(years, ","))
  are = as.character(are)
  y = as.character(y)
  
  str = ""
  
  if(are == "all"){
    str = paste(str,"</br>Statistics for", are," areas", years, sep=" ")
  }else{
      str = paste(str, "</br>Statistics for", as.character(are), years, sep=" ")
    }
    if(min(as.numeric(unlist(strsplit(years, ","))), na.rm=T) < 2004){
      mess = FALSE
    }else{
      
      mess = tagReturned_Year(are, years)
      return(mess)
    }

    if(mess == FALSE){
      str = paste(str, "</br></br>There were no tags returned or the data is not available for this year selection", sep="")
    }else{
      str = paste(str, "</br></br>The following data is from captures in the area and years selected:&nbsp", sep="")
      str = paste(str, "</br>&nbsp&nbsp&nbspNumber of tags returned with position and date:&nbsp ", sep="")
      str = paste(str, mess$a, sep="")
      str = paste(str, "</br>&nbsp&nbsp&nbspNumber of individuals returning tags:&nbsp ", sep="")
      str = paste(str, mess$b, sep="")
    }

    mess = tagApplied(are, years)
    if(mess == FALSE){
      str = paste(str, "</br></br>There were no tags released", sep="")
    }else{
      str = paste(str, "</br></br>The following data is from crab that have been tagged in the area and years selected:&nbsp", sep="")
      str = paste(str, "</br>&nbsp&nbsp&nbspTags Released:&nbsp ", sep="")
      str = paste(str, mess, sep="")
    }
    mess = tagReturned_Applied(are, years)
    if(mess == FALSE){
      str = paste(str,"</br>&nbsp&nbsp&nbspThere were no valid returns to create stats", sep="")
    }else{
      
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Number of these tags returned: &nbsp", sep="")
      str = paste(str,mess$ret, sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Number of crab captured more than once:&nbsp", sep="")
      x = as.numeric(mess$ret)
      y =  as.numeric(mess$retuni)
      x = x - y
      str = paste(str,x)
      str = paste(str,"</br>Data from these released crab with captures that occured more than 10 days from tag site:&nbsp", sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Average movement (km):&nbsp", sep="")
      str = paste(str,mess$mov, sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Largest movement (km):&nbsp", sep="")
      str = paste(str,mess$lmov, sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Average number of days to capture:&nbsp", sep="")
      str = paste(str,mess$day, sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Longest number of days between release site and capture:&nbsp", sep="")
      str = paste(str,mess$lday, sep="")
      str = paste(str,"</br>&nbsp&nbsp&nbsp	Ave. rate of movement (total km/ total months):&nbsp", sep="")
      str = paste(str,mess$spe, sep="")
      
    }
    
    
    
    return(str)
    
}

#' @title  tagApplied
#' @description  Helper function to generate stats 
#' @import RODBC stringr
#' @return stats message
#' @export
tagApplied = function(are, years, rm.gulf = T){
  if(length(years)>1)years = paste(as.character(years), collapse = ",")
  yea = str_replace_all(years, ",", "') OR (SCT_TRIP.YEAR = '")
  
  # local_port = "3308"
  # 
  # SCtunnel = openportSC(local.port = local_port)
  # 
  # con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  # rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  # rs <- dbSendQuery(con, statement = paste("SELECT bio.tag_id, bio.sample_num, trip.trip_id, trip.year, trip.statsarea, sample.trip, sample.sample_id, sample.lat_DD_DDDD, sample.long_DD_DDDD
  #                                          FROM bio, trip
  #                                          JOIN sample
  #                                          WHERE bio.sample_num = sample.sample_id 
  #                                          AND sample.trip = trip.trip_id 
  #                                          AND ( YEAR =", years , ");",sep = ""))
  # 
  # da <- fetch(rs, n = -1)   # extract all rows
  # dbDisconnect(con) 
  # closeportSC(SCtunnel)
  # 
  
  
  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  da = NULL
  
  
  query = paste("SELECT SCT_BIO.TAG_ID,
                SCT_TRIP.YEAR,
                SCT_TRIP.CFA,
                SCT_SAMPLE.LAT_DD_DDDD,
                SCT_SAMPLE.LONG_DD_DDDD  
                FROM SCT_BIO
                INNER JOIN SCT_SAMPLE
                ON SCT_BIO.SAMPLE_NUM = SCT_SAMPLE.SAMPLE_ID
                INNER JOIN SCT_TRIP
                ON SCT_TRIP.TRIP_ID = SCT_SAMPLE.TRIP
                WHERE (SCT_TRIP.YEAR = '", yea, "');", sep="")
  
  da = sqlQuery(con, query )
  odbcClose(con)
  
  
  
  da$sample_id = NULL
  x = nrow(da)
  if(x == 0) return(NA)
  names(da) = c("PID", "sampyear", "samparea", "samplat", "samplon")
  
  # ii = is.in(are, da$samplon, da$samplat)
  ii = absolutely.in.area2(are, da$samplon, da$samplat)
  ind = which(ii == FALSE)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  #REMOVE GULF ENTRIES
  # ind = which( as.character(da$samparea) == "GULF" )
  if(rm.gulf){
  ind = which(as.character(da$samparea) == "GULF"  & as.numeric(as.character(da$sampyear)) <= 2014 )
  
  if(length(ind)>0)
    da = da[-ind,]
  }
  
  ind = which( as.numeric(da$sampyear) < 2004 )
  
  if(length(ind)>0){ 
    return("This query includes years for which this information is not available")
  }else{
    return(nrow(da))
  }
  
  
  
}

  #' @title  tagReturned_Year
  #' @description  Helper function to generate stats 
  #' @import RODBC stringr
  #' @return stats message
  #' @export
tagReturned_Year = function(are, years, region = "SoctianShelf"){
    yea = as.numeric(unlist(strsplit(years, ",")))
  
    yea = str_replace_all(years, ",", "') OR (SCT_CAPTURE.YEAR = '")
 
    toda = paste("SELECT SCT_CAPTURE.PERSON,
            SCT_CAPTURE.LAT_DD_DDDD,
            SCT_CAPTURE.LONG_DD_DDDD,
            SCT_CAPTURE.STATSAREA,
            SCT_CAPTURE.YEAR
            FROM SCT_CAPTURE
            WHERE (SCT_CAPTURE.YEAR = '", yea, "')", sep="")
    con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    res = sqlQuery(con, toda)
    odbcClose(con)
 
    x = nrow(res)

    #Remove data outside of query area	
    ii = absolutely.in.area2(are, res$LONG_DD_DDDD, res$LAT_DD_DDDD) 

    ind = which(!ii)
    if(length(ind)>0) 
      res = res[-ind,]
    
    x = nrow(res)
    if(x == 0) return(FALSE)
    y = length(unique(res$PERSON))
    
    z = NULL
    z$a=x
    z$b=y-1  # -1 because unknown is not a person and database contains entries for person 'unknown'
    
    return(z)
  }
  
  
  #' @title  tagReturned_Applied
  #' @description  Helper function to generate stats 
  #' @import RODBC lubridate
  #' @return stats message 
  #' @export
tagReturned_Applied = function(are, years, rm.gulf = T){
    #TAGS APPLIED IN GIVEN AREA AND YEAR
    maxkm = 50
    z = NULL
    z$ret = NA
    z$retuni = NA
    z$mov = NA
    z$lmov = NA
    z$day = NA
    z$lday = NA
    z$spe = NA
    
    yea = as.numeric(unlist(strsplit(years, ",")))
    y = get.capturedata()
    x = get.path()

    names(x) = c("PID", "CID", "capdate", "kms")
    x$capdate = dmy(x$capdate)
    da = merge(x, y, by = c("PID","capdate"))
   
    ##REMOVE YEARS
    da = da[which(da$sampyear %in% yea),]
    da$sample_num = NULL
    da$sample_id = NULL
    da$trip = NULL
    da$trip_id = NULL
    da$captain = NULL
    da$Reported = NULL
    
    da$sample_id = NULL
    nr = nrow(da)
    
    
    if(nr == 0) return(z)
    names(da) = c("PID","capdat", "CID", "km", "caparea","caplat", "caplong",  "capyear", "triparea", "sampyear", "sampdate", "samplat", "samplon")
    
    ii = absolutely.in.area2(are, da$samplon, da$samplat)
    #jj = absolutely.in.area2(are, da$caplon, da$caplat)
   # ind = which(!(ii | jj))
    ind = which(!ii)
    
    if(length(ind)>0) 
      da = da[-ind,]
    
    if(rm.gulf){
    #REMOVE GULF ENTRIES
    #ind = which( as.character(da$caparea) == "GULF" & as.character(da$triparea) == "GULF"  )
    ind = which(as.character(da$triparea) == "GULF"  & as.numeric(as.character(da$sampyear)) <= 2014 )
    if(length(ind)>0) 
      da = da[-ind,]
    }

    nr = nrow(da)
    
    if(nr == 0) return(z)
    
  
    
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
    
    #dates = as.character(mov$D)
  #  mov$D = as.POSIXct(as.numeric(dates), origin = '1970-01-01')
     mov$D = ymd(mov$D)
   if(nrow(mov) == 0)return(z)
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
      if(km > maxkm){
        print(chunk)
        print(km)
        maxkm = km
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
    
    if(x == 0) return(z)
    
    #m = calcLength (mov, rollup = 3, close = FALSE)
    
    z$mov = mean(daysince$nkm)
    z$lmov = max(daysince$nkm)
   pdf(file.path(data_root, "bio.snowcrab", "data", "tagging", paste("distances", are, years,".pdf", sep = "_")))
    hist(daysince$nkm,breaks=100, col="red",main="Distances Travelled",xlab="Distance(km)")
   dev.off()
   pdf(file.path(data_root, "bio.snowcrab", "data", "tagging", paste("days.pdf", are, years,".pdf", sep = "_")))
   hist(daysince$ndays,breaks=100, col="red",main="Days To Last Known Capture",xlab="Time(days)")
   dev.off()
   pdf(file.path(data_root, "bio.snowcrab", "data", "tagging", paste("tofirstdays.pdf", are, years,".pdf", sep = "_")))
   hist(tofirst,breaks=100, col="red", main="Days To First Capture",xlab="Time(days)")
   dev.off()
    z$day = mean(daysince$ndays)
    z$lday = max(daysince$ndays)
    
    #z$spe = z$mov/(z$day*0.0328549) #Days to month, km/month output
    z$spe = sum(daysince$nkm)/sum(daysince$ndays*0.0328549) #Days to month, km/month output
    distance = daysince$nkm
    return(z)
    
  }
alldata = function(are, years){
    are = "all"
    years = 2004:2017
    y = get.capturedatacc()
    x = get.paths()
    names(x) = c("PID", "plon", "plat", "capdate", "kms")
    x$capdate = dmy(x$capdate)
    y$sampdat = ymd(y$sampdat)
    da = merge(x, y, by = c("PID","capdate"))
    da$difdays = as.numeric(da$capdate - da$sampdat)
    da$kms = as.numeric(da$kms)
    da$kmperd = da$kms/da$difdays
    ii = absolutely.in.area2(are, da$samplon, da$samplat)
    jj = absolutely.in.area2(are, da$caplon, da$caplat)
    ind = which(!(ii | jj))
    
    
    if(length(ind)>0) 
      da = da[-ind,]
    
    write.csv(da, "outcc.csv")
  }
  
  
  addPaths <- function(dx){
    dx$plat = NA
    dx$plon = NA
    dx$km = NA
    pidinr = 1
    for(i in 1:nrow(dx)){
      pat = get.paths(as.character(dx$PID[i]))
      dx$plat[i] = pat$LAT[1]
      dx$plon[i] = pat$LON[1]
      dx$km[i] = pat$DIST[1]
      inr = 2
      while(dx$PID[i+1] == dx$PID[i]){
        i = i+1
        dx$plat[i] = pat$LAT[inr]
        dx$plon[i] = pat$LON[inr]
        dx$km[i] = pat$DIST[inr]
        inr = inr + 1
      }
      
    }
    
    return(dx)
  }
  
  #' @title  sample_ent
  #' @description  Function that enters release data entered in the html app 
  #' @import RODBC jsonlite stringr opencpu
  #' @return message to webpage 
  #' @export
  sample_ent <- function(bdata, sdata){
    
    
    conn = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    # Check connection
    if (conn == -1){
      return(toJSON("Connection failed"))
    }
    
    samp = myUrlEncode(sdata)
    
    samp = unlist(str_split(samp, "&"))
    rc1 = ""
    dat = ""
    sam = ""
    ves = ""
    cfa = ""
    capt = ""
    dep = ""
    com = ""
    lat = ""
    lon = ""
    
    for(i in 1:length(samp)){
      if(samp[i] != ""){
        
        sa = unlist(str_split(samp[i], "="))
        
        
        
        if(sa[1] == "radio-choice-1")
          rc1 = sa[2]
        if(sa[1] == "date")
          dat = sa[2]
        if(sa[1] == "samp")
          sam = sa[2]
        if(sa[1] == "ves")
          ves = sa[2]
        if(sa[1] == "cfa")
          cfa = sa[2]
        if(sa[1] == "capt")
          capt = sa[2]
        if(sa[1] == "dep")
          dep = sa[2]
        if(sa[1] == "com")
          com = sa[2]
        if(sa[1] == "lat")
          lat = sa[2]
        if(sa[1] == "lon")
          lon = sa[2]
        
        
      }
      
    }
    
    
    lat = str_replace(lat, "N","")
    lon = str_replace(lon, "W","")
    
    
    rlat = as.character(conpos(lat))
    rlon = as.character((conpos(lon)*-1))
    
    
    df = unlist(str_split(dat, "/"))
    
    year = df[3]
    mon = df[1]
    day = df[2]
    
    
    
    dat = paste(day, mon, year, sep = "/")
    
    
    
    sta = ""
    res = ""
    samp = ""
    sampsql = ""
    out = ""
    wrisamp = FALSE
    # //////////////////////////////////////////
    #   //Check if sample num exists, if so get sample num
    # // else get num row of sample
    # 
    # 
    
    #sql = paste("SELECT TRIP_ID from SCT_TRIP where DMY = '", dat,  "' AND TECHNICIAN = '",sam,"'", sep = "")
    
    sql = paste("SELECT TRIP_ID from SCT_TRIP where RELEASE_DATE = to_date('", dat,"', 'dd/mm/yyyy') AND TECHNICIAN = '",sam,"'", sep = "")
    
    
    
    result = RODBC::sqlQuery(conn, sql)
    exis = nrow(result)
    
    rowid = ""
    
    if (exis > 0){
      res = result[,1]
    }
    
    
    
    if (exis == 0) {            
      
      sql = "select TRIP_ID from SCT_TRIP"
      result = RODBC::sqlQuery(conn, sql)
      res = nrow(result) + 300 
      
      
      if(cfa == "xxxx") sta = "4X"
      if(cfa == "nens") sta = "NENS"
      if(cfa == "23") sta = "SENS"
      if(cfa == "24") sta = "SENS"  
      if(cfa == "gulf") sta = "GULF"  				 
      suba = ""
      if(sta == "NENS") suba =  "(all)(ens)(nens)(nens_gulf)(allandgulf)"
      if(sta == "SENS"){
        if(cfa == "23")
          suba =  "(cfa23)(all)(ens)(sens)(allandgulf)(cfa23zoom)(cfa24zoom)(all.holes)"
        if(cfa == "24")
          suba =  "(cfa24)(all)(ens)(sens)(allandgulf)(cfa24zoom)(cfa24zoom)(all.holes)"
      }
      if(sta == "4X") suba =  "(all)(ens)(cfa4x)(allandgulf)";
      if(sta == "GULF") suba =  "(allandgulf)";
      
      
      
      
      sql = paste("INSERT INTO SCT_TRIP (TRIP_ID, TECHNICIAN, VESSEL, CFA, YEAR, STATSAREA, REPORTED, CAPTAIN, SUBAREA, RELEASE_DATE) VALUES( '",res,"' , '",sam,"' , '",SQLsafty(ves),"' , '",cfa,"' , '",year,"' , '",sta ,"' , 0 , '",SQLsafty(capt) ,"' , '",suba,"' , to_date('", dat,"', 'dd/mm/yyyy'));", sep = "")
      
      result = RODBC::sqlQuery(conn, sql)
      
      
      
      #   fwrite($myfile, $sql);
      if (length(result) == 0){
        out =  paste(out, "\nNew Trip ", res, " Successfully Added.")
      }
      else{
        out =  paste(out,"\nError: ",  result)
        return(out)
        die()
      }
      
    }
    
    
    
    sql = paste("SELECT SAMPLE_ID FROM SCT_SAMPLE where TRIP = '",res,"' AND LAT_DD_DDDD = '",rlat,"' AND LONG_DD_DDDD = '",rlon,"'", sep = "")
    result = RODBC::sqlQuery(conn, sql)
    res2 = nrow(result) 
    
    if (res2 > 0){
      samp = result[,1]
    }
    if (res2 == 0) {   
      sql = "select SAMPLE_ID from SCT_SAMPLE"
      result = RODBC::sqlQuery(conn, sql)
      samp = as.character(nrow(result) + 3500) 
      
      sampsql = paste("INSERT INTO SCT_SAMPLE VALUES( '",samp,"' , '",res,"' , '",lat,"' , '",lon,"'  ,  '",rlat,"' , '",rlon,"' , '",dep,"' , '",SQLsafty(com),"')", sep = "")
      wrisamp = TRUE;
      
    }
    
    
    
    
    dd = as.data.frame(fromJSON(bdata)[2:nrow(fromJSON(bdata)),])
    names(dd) = fromJSON(bdata)[1,]
    
    
    # $i = 0;
    # $b = "";
    writedata = TRUE
    for(i in 1:nrow(dd)){
      if(i > 0){
        if(!is.na(dd$`Tag Num`[i])){
          
          
          sql = paste("SELECT TAG_ID FROM SCT_BIO where TAG_ID = '", dd$`Tag Num`[i],"'", sep = "")
          result = RODBC::sqlQuery(conn, sql)
          ntn = nrow(result) 
          if(ntn > 0) {
            out = paste(out, "\nCrab with tag " , dd$`Tag Num`[i], " has already been added!! ", sep = "")
            writedata = FALSE;
            
          } 
          
        }
      }
    }		
    
    
    
    if(writedata){
      for(i in 1:nrow(dd)){
        if(i > 0){
          if(!is.na(dd$`Tag Num`[i])){
            if(is.null(dd$`Durometer`[i])) dd$`Durometer`[i] = NA
            sql = paste("INSERT INTO SCT_BIO VALUES ('",samp,"', '",dd$`Tag Num`[i],"', '",dd$`Carapace`[i],"', '",dd$`Claw`[i],"','",dd$`Shell Cond`[i],"','",dd$`Durometer`[i],"')", sep = "")
            result = RODBC::sqlQuery(conn, sql)
            
            #   fwrite($myfile, $sql);
            if (length(result) == 0){
              
              out =  paste(out, "\nCrab with  tag " , dd$`Tag Num`[i], " successfully added", sep = "")
            }
            else{
              out =  paste(out,"\nError: ",  result)
              return(out)
              die()
            }
            
          }
        }
        
      }
      if(wrisamp){
        result = RODBC::sqlQuery(conn, sampsql)
        
        
        if (length(result) == 0){
          out = paste(out,"\nSample from trip ",res, " with pos ",lat, " " ,lon, " successfully added", sep = "")
        }
        else{
          out =  paste(out, "\nError: " ,sampsql , "\n" , result, "\n", sep = "")
          return(out)
          die()
        }
        
      }
    }
    
    out = paste(out,"\n\n", sep = "")
    
    odbcClose(conn)
    return(out)
    
  }
  
  #' @title  THE MAIN GUI FUNCTION!
  #' @description  Opens web page of options for data entry
  #' @import opencpu
  #' @export
  enter_data_app <- function(){
    ocpu_start_app("SCtagging", no_cache = TRUE)
    
  }
  
  
  #' @title  SC_samp_Ent
  #' @description  Enter data to sample table
  #' @import RMySQL RODBC
  SC_samp_Ent <- function(tid = NA, lat = NA, lon = NA, dlat = NA, dlon = NA, fat = NA, com = NA){
    sid = as.integer(SCT_nrows("SCT_SAMPLE") + 1)
    # Exists sample?
  }
  
  #' @title  SCT_nrows
  #' @description  Get the number of rows from specified table
  #' @param table The name of the table from which to determine the current number of rows
  #' @import RODBC
  SCT_nrows <- function(table = ""){
    
    con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    nrows = NULL
    nrows = sqlQuery(con, paste("select count(*) from ", table, sep = "") )
    odbcClose(con)
    return(nrows)
  }
  
  #' @title  SQLsafty
  #' @description  Relapce ' with '' to perserve entries
  #' @param var The variables to modify
  #' @export
  SQLsafty <- function(var) {
    
    return(gsub("'", "''", var))
  }
  
  
  #' @title  myUrlEncode
  #' @description  Decode json url
  #' @param string The url to decode
  #' @export
  myUrlEncode <- function(string) {
    entities = c("%21", "%2A", "%27", "%28", "%29", "%3B", "%3A", "%40", "%26", "%3D", "%2B", "%24", "%2C", "%2F", "%3F", "%25", "%23", "%5B", "%5D", "%C2%B0", "\\+", "%0D%0A")
    replacements = c("!", "*", "'", "(", ")", ";", ":", "@", "%26", "%3D", "+", "$", ",", "/", "?", "%", "#", "[", "]", "", " ", "  ")
    
    
    for(i in 1:length(entities)){
      print(entities[i])
      string = gsub(entities[i], replacements[i], string)
    }
    return(string)
  }
  
  #' @title  conpos
  #' @description  Decode positional data
  #' @param string The pos to decode
  #' @return the decoded position 
  conpos = function(string) {
    
    if(substr(string, 4, 4) == '.'){
      string = paste(substr(string, 1, 2),'0',substr(string, 3, nchar(string)), sep = "")
    }
    de = as.integer(substr(string, 1, 2)) 
    min = as.integer(substr(string, 3, 4))
    dmin = as.integer(substr(string, 6, 7))/100
    dm = (min + dmin)/60
    nu = de + dm
    return(nu)
  }
  
  #' @title  auto_availableP
  #' @description Function that help autopopulate people in the html form
  #' @import RODBC jsonlite
  #' @export
  autoavailableP = function(options = "", region = ""){
    con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    res = ""
    if(region == "ss"){
      res = sqlQuery(con, "select NAME from SCT_PEOPLE" )
    }
    if(region == "g"){
      res = sqlQuery(con, "select NAME from SCT_PEOPLE_GULF" )
    }
    
    
    odbcClose(con)
    
    return(toJSON(res))
    
  }
  #' @title  auto_availableT
  #' @description Function that help autopopulate Tag id in the html form
  #' @import RODBC jsonlite
  #' @export
  autoavailableT = function(region = ""){
    
    con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    
    res = ""
    if(region == "ss"){
      res = sqlQuery(con, "select TAG_ID from SCT_BIO" )
    }
    if(region == "g"){
      res = sqlQuery(con, "select TAG_ID from SCT_BIO_GULF" )
    }
    odbcClose(con)
    
    return(toJSON(res))
    
  }
  
  #' @title  autoaddData
  #' @description Function that help autopopulate people in the html form
  #' @param name The persons name from which to obtain current info
  #' @import RODBC jsonlite
  #' @export
  autoaddData = function(name = "", region = ""){
    gstring = ""
    if(region == "g") gstring = "_GULF"
    con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    res = sqlQuery(con, paste("select * from SCT_PEOPLE", gstring,"  where NAME = '", name, "'", sep = "" ))
    odbcClose(con)
    
    return(toJSON(res))
    
  }
  
  
  