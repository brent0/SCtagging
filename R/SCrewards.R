

# library("ROracle")
# library("stringi")
# library("rgdal")
# library("PBSmapping")
# library("sp")
# library("raster")
# library("lubridate")
# library("rgeos")
# library('spatstat') 
# library('shadowtext') 
# library('ggplot2') 
# library('ggmap') 
# library('ggthemes') 
# library('geosphere') 
# library('RStoolbox')

# library("knitr", lib.loc="~/R/win-library/3.6")
# library("rmarkdown", lib.loc="~/R/win-library/3.6")

#' @title  getBetween
#' @description  Return from a text between search pattern. Helps facilitate letter format changes. 
#' @param text The text feild to search
#' @param pattern The pattern to match
#' @export
getBetween = function(text = "", pattern = ""){
  text = substr(text, gregexpr(pattern, text)[[1]][1], gregexpr(pattern, text)[[1]][2]-1)
  text = substr(text, regexpr(">", text)[1]+1, regexpr("</", text)[1]-1)
  return(text)
}
#' @title  tag.all.rewarded
#' @description  Update the database to reflect that awards were sent. 
#' @import ROracle DBI
#' @export
tag.all.rewarded = function(update = F){
  if(update){
    gstring= ""
    if(region == "Gulf") gstring = "_GULF"
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    
    toda = "Update SCT_CAPTURE set REWARDED = 'N' where REWARDED = 'M'"
    ROracle::dbSendQuery(con, toda) 
  }
}
#' @title  rewards.knit.document
#' @description  The main function call to create reward pdf from R. 
#' @import knitr rmarkdown
#' @export
rewards.knit.document = function(region = "ScotianShelf"){
  if(region == "ScotianShelf"){
    render('knit_rewards.Rmd', 'all')
    message("PDF of reward letters created. Review carefully and if happy print double sided. Once rewardes are sent out by mail call tag.all.rewarded(update = T) to update the database to reflect that the tags included in this report have been rewarded")
  }
  ##Put other regions letter generation Rmd file to render here: 
}

#' @title  generate.reward.data
#' @description  Gather Reward data in preparation for document creation with rewards.knit.document() . 
#' @import ROracle DBI stringi
#' @export
generate.reward.data = function(region = "ScotianShelf"){
  # Letter text and formatting, be carefull when changing text to be aware of return line formatting
  lettertxt = "<PARAGRAPH addresslabel>  
Brent Cameron   
DFO Snow Crab Science  
Population Ecology Division, ESS  
PO Box 1006 Dartmouth, NS  
B2Y 4A2  
</PARAGRAPH addresslabel>
<PARAGRAPH A>Brent Cameron Snow Crab Technician  
 Department of Fisheries and Oceans  
 Bedford Institute of Oceanography  
 P.O. Box 1006, Dartmouth, N.S.  
 B2Y 4A2  
 Ph. (902) 403-8403</PARAGRAPH A>
<PARAGRAPH mytagcapturedbutihavenoreturns>  <name>,  
 \\newline  
  Thank you for participating in the tagging program. I thought you would be interested to know that a crab you had previously released was captured again and information sent in. This information is shown in a chart provided. Recaptures such as these will help us better track the movement of crab across the Scotian shelf.    
</PARAGRAPH mytagcapturedbutihavenoreturns>
<PARAGRAPH B><name>,  
 \\newline  
Thanks for returning the snow crab <tag/tags> caught last season. I have included a token of our appreciation for your efforts in returning this information. I have also included a chart showing the release and recapture positions for the <tag/tags>. The information provided by such tagging recaptures is helpful in determining the movement of snow crab throughout the Scotian Shelf. </PARAGRAPH B>
<PARAGRAPH info>  
 \\newline  
The tagged crab you caught <was/were/wereall> tagged in the <yeartagged/yearstagged/season/seasons>.</PARAGRAPH info>
<PARAGRAPH capturedbefore>  
 \\newline  
<onebefore/somebefore> of the tagged crab you caught <wasb/wereb> captured before and released. Helpful knowledge is gained from captures such as these, especially if they are once again returned to the water. The data for this is shown in the charts provided. </PARAGRAPH capturedbefore>
<PARAGRAPH capturedafter>  
 \\newline  
<oneafter/someafter> of the tagged crab you caught and released in the past <wasa/werea> captured this season. Thank you for releasing <this/these> crab. This knowledge is very helpful in tracking the movements of crab. The data for this is shown in the charts provided.</PARAGRAPH capturedafter>
<PARAGRAPH capturedbeforeandafter>  
 \\newline  
<onebefore/somebefore> of the tagged crab you caught <wasb/wereb> captured before and released. As well, <oneafter/someafter> of the tagged crab you caught and released in the past <wasa/werea> captured this season. Helpful knowledge is gained from captures such as these, especially if they are once again returned to the water. This knowledge is very helpful in tracking the movements of crab. The data for this is shown in the charts provided.</PARAGRAPH capturedbeforeandafter>
<PARAGRAPH notreleased>  
 \\newline  
In the future, please release all tagged crab back to the water alive after recording the tag number. This tag number can then be returned to DFO Science with the relevant information such as date and location of capture. It will be treated in the same manner as when we receive the actual tag. We hope that additional knowledge will be gained by tracking subsequent recaptures of individual crab over time. </PARAGRAPH notreleased>
<PARAGRAPH released>  
 \\newline  
Thank you for releasing all tagged crab back to the water alive after recording the tag number and relevant information such as date and location of capture. The data you submitted will be treated in the same manner as when we receive the actual tag. We hope that additional knowledge will be gained by tracking subsequent recaptures of individual crab over time. </PARAGRAPH released>
<PARAGRAPH mixedrelret>  
 \\newline  
Thank you for releasing some of the tagged crab back to the water. The relevant data that you submit from these releases will be treated in the same manner as when we receive the actual tag. We hope that additional knowledge will be gained by tracking subsequent recaptures of individual crab over time. In the future, please release all tagged crab back to the water.  </PARAGRAPH mixedrelret>
<PARAGRAPH unknownrel>  
 \\newline  
It was unclear whether you released or retained the tagged crab.In the future please include this data along with the other relevant data. Our hope is that all tagged crab will be released back to the water so that additional knowledge will be gained by tracking subsequent recaptures of individual crab over time. </PARAGRAPH unknownrel>
<PARAGRAPH final>  
 \\newline  
I have included a one page information sheet on our tagging program. On the reverse side of this sheet is a form which can easily be used to record all required information on any tagged crab you may catch in the future. This entire form can be mailed to DFO Science at the end of the snow crab season. </PARAGRAPH final>
<PARAGRAPH end> 
 \\newline  
  Thanks for your help.  
 \\newline  
 \\newline  
 \\newline  
Brent Cameron
  </PARAGRAPH end>"

  gstring= ""
  if(region == "Gulf") gstring = "_GULF"
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  
  da = NULL
  query = paste("SELECT SCT_BIO", gstring,".TAG_ID,
                SCT_BIO", gstring,".SAMPLE_NUM,
                SCT_CAPTURE", gstring,".CAPTURE_DATE,
                SCT_CAPTURE", gstring,".STATSAREA,
                SCT_CAPTURE", gstring,".LAT_DD_DDDD AS CAPLAT,
                SCT_CAPTURE", gstring,".LONG_DD_DDDD AS CAPLON,
                SCT_CAPTURE", gstring,".YEAR,
                SCT_CAPTURE", gstring,".RELCODE,
                SCT_CAPTURE", gstring,".REWARDED,
                SCT_TRIP", gstring,".TRIP_ID,
                SCT_TRIP", gstring,".STATSAREA AS STATSAREA1,
                SCT_TRIP", gstring,".YEAR      AS YEAR1,
                SCT_TRIP", gstring,".CAPTAIN,
                SCT_TRIP", gstring,".REPORTED,
                SCT_TRIP", gstring,".RELEASE_DATE,
                SCT_SAMPLE", gstring,".LAT_DD_DDDD AS RELLAT,
                SCT_SAMPLE", gstring,".LONG_DD_DDDD AS RELLON,
                SCT_SAMPLE", gstring,".SAMPLE_ID,
                SCT_PEOPLE", gstring,".NAME,
                SCT_PEOPLE", gstring,".CIVIC,
                SCT_PEOPLE", gstring,".TOWN, 
                SCT_PEOPLE", gstring,".PROV, 
                SCT_PEOPLE", gstring,".POST
                FROM SCT_CAPTURE", gstring,"
                INNER JOIN SCT_BIO", gstring,"
                ON SCT_BIO", gstring,".TAG_ID = SCT_CAPTURE", gstring,".TAG
                INNER JOIN SCT_SAMPLE", gstring,"
                ON SCT_BIO", gstring,".SAMPLE_NUM = SCT_SAMPLE", gstring,".SAMPLE_ID
                INNER JOIN SCT_TRIP", gstring,"
                ON SCT_SAMPLE", gstring,".TRIP = SCT_TRIP", gstring,".TRIP_ID
                INNER JOIN SCT_PEOPLE", gstring,"
                ON SCT_PEOPLE", gstring,".NAME = SCT_CAPTURE", gstring,".PERSON
                ORDER BY SCT_TRIP", gstring,".CAPTAIN,
                SCT_TRIP", gstring,".TRIP_ID,
                SCT_BIO", gstring,".TAG_ID,
                SCT_CAPTURE", gstring,".CAPTURE_DATE", sep = "")
  
  resbio <- ROracle::dbSendQuery(con, query) 
  da <-  fetch(resbio) #Get all capture data
  tid = da$TAG_ID[which(da$REWARDED == 'N')] #Get tag ids of unrewarded returns
  da = da[which(da$TAG_ID %in% tid),] #Get all required data for plotting tag histories of unrewarded 
  da$CAPTURE_DATE = as.character(as.Date(da$CAPTURE_DATE))
  da$RELEASE_DATE = as.character(as.Date(da$RELEASE_DATE))
  dx = unique(da)
  previd = ""
  
  da = dx
  
  da$CAPTURE_DATE = ymd(da$CAPTURE_DATE)
  da$RELEASE_DATE = ymd(da$RELEASE_DATE)
  
  da = da[order(da$CAPTURE_DATE),] #Proper history order for positional reformatting
  da = da[order(da$TAG_ID),] #Proper tag order for positional reformatting
  #loop through and set position and dates of previous release location to 
  #the proper release not the sample release
  for(i in 1:nrow(da)){
    if(da$TAG_ID[i] == previd){
      da$RELLAT[i] = da$CAPLAT[i-1]
      da$RELLON[i] = da$CAPLON[i-1]
      da$RELEASE_DATE[i] = da$CAPTURE_DATE[i-1]
    }
    previd = da$TAG_ID[i] 
  }
  perlist = list() #set up list to hold relevant data
  
  #Loop thru each person who needs to be rewarded.
  persplit = split(da, da$NAME)
  for(i in 1:length(persplit)){
   if(i < 3000){ #Test toggle so full report isnt generated while testing ex change 3000 to 3
    per = list()
    per$data = persplit[[i]]
    per$name = per$data$NAME[1]
    #print(per$name)
    #Add all relevant tag data to persons frame
    per$matcheddata = da[which(da$TAG_ID %in% per$data$TAG_ID),]
    if(per$name != 'unknown'){
    #Following id for code folding to exclude reading leter creation step
    if(TRUE){  
    per$addresslabel = getBetween(lettertxt, "PARAGRAPH addresslabel" )
    per$paraA = getBetween(lettertxt, "PARAGRAPH A")
    per$paraB = getBetween(lettertxt, "PARAGRAPH B")
    per$paraB = sub("<name>", per$name, per$paraB)
    if(length(unique(per$data$TAG_ID))>1){
      per$paraB = gsub("<tag/tags>", "tags", per$paraB)
    }else{
      per$paraB = gsub("<tag/tags>", "tag", per$paraB)
    }
    if(all(per$data$REWARDED == "Y")){
      per$mytagcapturedbutihavenoreturns = getBetween(lettertxt, "PARAGRAPH mytagcapturedbutihavenoreturns" ) 
    }else{
      per$mytagcapturedbutihavenoreturns = ""
    }
    per$info = getBetween(lettertxt, "PARAGRAPH info" )
    if(length(unique(per$data$TAG_ID))>1){
      if(length(unique(per$data$YEAR1))>1){
        per$info = gsub("<was/were/wereall>", "were", per$info)
        per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(stri_reverse(sub(",", "dna ", stri_reverse(paste0(unique(per$data$YEAR1), collapse = ", ")))), " seasons", sep = ""), per$info)
      }else{
        per$info = gsub("<was/were/wereall>", "were all", per$info)
        per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(unique(per$data$YEAR1), " season", sep = ""), per$info)
      }
    }else{
      per$info = gsub("<was/were/wereall>", "was", per$info)
      per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(unique(per$data$YEAR1), " season", sep = ""), per$info)
    }
  
    capbefore = F
    capbeforegto = F
    capafter = F
    capaftergto = F
    cb = 0
    ca = 0
    utid = unique(per$data$TAG_ID)
    for(k in 1:length(utid)){
      cb = max(cb, length(which(per$matcheddata$CAPTURE_DATE[which(per$matcheddata$TAG_ID == utid[k])] <  per$data$CAPTURE_DATE[which(per$data$TAG_ID == utid[k])])))
      if(cb > 0) capbefore = T
      ca = max(ca, length(which(per$matcheddata$CAPTURE_DATE[which(per$matcheddata$TAG_ID == utid[k])] >  per$data$CAPTURE_DATE[which(per$data$TAG_ID == utid[k])])))
      if(ca > 0) capafter = T
    }
    if(capbefore & !capafter){
      per$capbefore = getBetween(lettertxt, "PARAGRAPH capturedbefore" )
      if(cb == 1){
        per$capbefore = gsub("<onebefore/somebefore>", "One", per$capbefore)
        per$capbefore = gsub("<wasb/wereb>", "was", per$capbefore)
      }
      if(cb > 1){
        per$capbefore = gsub("<onebefore/somebefore>", "Some", per$capbefore)
        per$capbefore = gsub("<wasb/wereb>", "were", per$capbefore)
      }
    }else{
      per$capbefore = ""
    }
    if(!capbefore & capafter){
      per$capafter = getBetween(lettertxt, "PARAGRAPH capturedafter" )
    if(ca == 1){
      per$capafter = gsub("<oneafter/someafter>", "One", per$capafter)
      per$capafter = gsub("<wasa/werea>", "was", per$capafter)
    }
    if(ca > 1){
      per$capafter = gsub("<oneafter/someafter>", "Some", per$capafter)
      per$capafter = gsub("<wasa/werea>", "were", per$capafter)
    }
    }else{
      per$capafter = ""
    }  
    if(capbefore & capafter){
      per$capturedbeforeandafter = getBetween(lettertxt, "PARAGRAPH capturedbeforeandafter" )
      if(ca == 1){
        per$capturedbeforeandafter = gsub("<oneafter/someafter>", "one", per$capturedbeforeandafter)
        per$capturedbeforeandafter = gsub("<wasa/werea>", "was", per$capturedbeforeandafter)
      }
      if(ca > 1){
        per$capturedbeforeandafter = gsub("<oneafter/someafter>", "some", per$capturedbeforeandafter)
        per$capturedbeforeandafter = gsub("<wasa/werea>", "were", per$capturedbeforeandafter)
      }
      if(cb == 1){
        per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "One", per$capturedbeforeandafter)
        per$capturedbeforeandafter = gsub("<wasb/wereb>", "was", per$capturedbeforeandafter)
      }
      if(cb > 1){
        per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "Some", per$capturedbeforeandafter)
        per$capturedbeforeandaftere = gsub("<wasb/wereb>", "were", per$capturedbeforeandafter)
      }
      }else{
        per$capturedbeforeandafter = ""
      }  
      if(all(per$data$RELCODE == "1")){
        per$released = getBetween(lettertxt, "PARAGRAPH released" )
      }else{
        per$released = ""
      }
      if(all(per$data$RELCODE == "2")){
        per$notreleased = getBetween(lettertxt, "PARAGRAPH notreleased" )
      }else{
        per$notreleased = ""
      }
      if(all(per$data$RELCODE == "3")){
        per$unknownrel = getBetween(lettertxt, "PARAGRAPH unknownrel" )
      }else{
        per$notreleased = ""
      }
      if( per$released == "" & per$notreleased == "" & per$notreleased == ""){
        per$mixedrelret = getBetween(lettertxt, "PARAGRAPH mixedrelret" )
      }else{
        per$mixedrelret = ""
      }
    }
    per$final = getBetween(lettertxt, "PARAGRAPH final" )
    per$end = getBetween(lettertxt, "PARAGRAPH end" )

    #Generates charts for this person and returns a list of file locations to them
    per$charts = rewards.chart(name = per$name, data = per$matcheddata)

    perlist[[length(perlist)+1]] = per
    }
    } #End person loop 
  }#End test toggle
  return(perlist)
}

#' @title  rewards.chart
#' @description  Create Reward charts. 
#' @param name The name of the person
#' @param data The data to chart
#' @import rgeos sp spatstat shadowtext ggplot2 ggmap ggthemes ggrepel geosphere RStoolbox
rewards.chart = function(name = "", data = NULL){
  
  # The lable colors that are later randomized, opted for the following colors that are color blind friendly
  collist <- c("#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # collist = c('green', 'orange', 'blue', 'red', 'yellow', 'purple', 'pink')
  
  outlist = c()
  clist = list()
  tx = split(data, data$TAG_ID)
  #loop thru each tag series to define boundaries and create geometeries 
  for(i in 1:length(tx)){
    collist = collist[sample.int(7, 7)]
    curt = tx[[i]]
    path = get.pathdata.tid(tid = curt$TAG_ID[1])
    path = path[order(as.numeric(path$CID), as.numeric(path$POS)),]
    path$LON = as.numeric(path$LON)
    path$LAT = as.numeric(path$LAT)
    globalextent = extent(min(path$LON), max(path$LON), min(path$LAT), max(path$LAT))
    
    linlats = cbind(curt$RELLAT)
    linlats = rbind(linlats, curt$CAPLAT[nrow(curt)])
    linlons = cbind(curt$RELLON)
    linlons = rbind(linlons, curt$CAPLON[nrow(curt)])

    linestring = SpatialLines(list(Lines(Line(cbind(as.numeric(linlons),as.numeric(linlats))), ID=name)))
    bb = data.frame(linestring@bbox)
   
    coords1 <- as.matrix(data.frame(cbind(c(bb[1,1], bb[1,1], bb[1,2], bb[1,2], bb[1,1]), c(bb[2,1], bb[2,2], bb[2,2], bb[2,1], bb[2,1]))))
    bbox.polyc = Polygon(coords = coords1, hole = F)
    bbox.polyc = Polygons(list(bbox.polyc),1)
    bbox.polyc = SpatialPolygons(list(bbox.polyc))
    
    s = (bb[1,2] - bb[1,1])/2
    coords2 <- as.matrix(data.frame(cbind(c(bb[1,1]-s, bb[1,1]-s, bb[1,2]+s, bb[1,2]+s, bb[1,1]-s), c(bb[2,1]-s, bb[2,2]+s, bb[2,2]+s, bb[2,1]-s, bb[2,1]-s))))
    bbox.polyc2 = Polygon(coords = coords2, hole = F)
    bbox.polyc2 = Polygons(list(bbox.polyc2),1)
    bbox.polyc2 = SpatialPolygons(list(bbox.polyc2))
    
    clisind = length(clist)+1
    
    if(length(clist) > 0){
    #loop through each persons data to test if tags can be combined with the map of the current tag 
    for(j in 1:length(clist)){
      # Test if bounding boxes intersect")
      if(gIntersects(clist[[j]]$bbox.poly, bbox.polyc2)){
        # Test if the bounding boxes are reasonably proportional to one annother
        if(min(gArea(clist[[j]]$bbox.poly), gArea(bbox.polyc))/max(gArea(bbox.polyc),gArea(clist[[j]]$bbox.poly)) > .083333 ){
          intersects = F 
          #loop through current tag line lists to test overlaps
          for(k in 1:length(clist[[j]]$linelist)){
             if(gIntersects(linestring, clist[[j]]$linelist[[k]])){
               intersects = T
             }
          }
          if(!intersects) clisind = j #if we get here tag can be combined at this location
        }
      }
    }
    }
    # If we adding to a current map we need to do the following
    if(clisind <= length(clist)){
      temp = clist[[clisind]]
   
      temp$linelist[[length(temp$linelist)+1]] = linestring
      temp$pathlist[[length(temp$pathlist)+1]] = path
      temp$datalist[[length(temp$datalist)+1]] = curt
      temp$globextent = extent(min(temp$globextent@xmin, globalextent@xmin), max(temp$globextent@xmax, globalextent@xmax), min(temp$globextent@ymin, globalextent@ymin), max(temp$globextent@ymax, globalextent@ymax))
     
       clist[[clisind]] = temp
    # If we did not find an good map to add to we need to add a new entry  
    }else{
      clist[[clisind]] = list(bbox.poly = bbox.polyc, linelist = list(linestring), pathlist = list(path), datalist = list(curt), globextent = globalextent)
    }
  }
  # Now that combinations are made where desired, set up geographies and 
  # plotting constraints for each map in persons clist
  for(i in 1:length(clist)){
    mdata = clist[[i]]
   
    xmin = xmin(mdata$globextent)
    xmax = xmax(mdata$globextent)
    ymin = ymin(mdata$globextent)
    ymax = ymax(mdata$globextent)
     
    xlen = xmax - xmin
    ylen = ymax - ymin

    #Make plotting region visually more square
    while(xlen < ylen){
      xmax = xmax+.1
      xmin = xmin-.1
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.1
      ymin = ymin-.1
      ylen =  ymax - ymin
    }
    
    while(xlen < ylen){
      xmax = xmax+.001
      xmin = xmin-.001
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.001
      ymin = ymin-.001
      ylen =  ymax - ymin
    }
    #visually scale plotting area a bit wider
    scale = (ymax-ymin)/100
    xmax = xmax+scale
    xmin = xmin-scale
  
    xlim = c(xmin, xmax)
    ylim = c(ymin, ymax)

    #rasters for plot backgrounds
    l1 = brick(file.path(bio.datadirectory, 'bio.snowcrab', 'maps', 'Charts', '801_LL_WGS84.tif'))
    l2 = brick(file.path(bio.datadirectory, 'bio.snowcrab', 'maps', 'Charts', 'atl_merged.tif'))
    #Git hub dosnt allow large files user will need to have these stored charts folder
    #l1 = brick(system.file("extdata", "background_rasters", "801_LL_WGS84.tif", package = "SCtagging"))
    #l2 = brick(system.file("extdata", "background_rasters", "atl_merged.tif", package = "SCtagging"))

    #Expand region
    xmin = xmin - ylen/2  
    xmax = xmax + ylen/2
    ymin = ymin - ylen/2  
    ymax = ymax + ylen/2
    ylen = ymax-ymin
    xlen = xmax-xmin
 
    e = extent(xmin, xmax, ymin, ymax)
    e2 = e #e2 will be representing inset plot boundary, needs to be larger geographic area than main plot
    ear = xlen*ylen
    e2ar = ear
    while(e2ar/20 < ear){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)  
    }
    # Also needs to be significantly large enough to get an idea on where in the world we are 
    while(e2ar < 20){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)  
    }

    rectan = data.frame(x = c(e@xmin, e@xmin, e@xmax, e@xmax), ex = c(e@xmin, e@xmax, e@xmax, e@xmin), y = c(e@ymin, e@ymax, e@ymax, e@ymin), ey = c(e@ymax, e@ymax, e@ymin, e@ymin))
    #Get distance for plotting scalebar
    dist = round(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8, -1)
    #Round differently for smaller areas
    if(dist < 10){
      dist = ceiling(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8)
    }
    hei = ylen/100  
    #Main plot 
    mp = ggRGB(l1, r=1, g=2, b=3, maxpixels = 800000, ext = e, ggObj = T)+
         ggtitle(paste(name, i, sep = "-")) + xlab("Longitude") + ylab("Latitude")+
         ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, dist_unit= "km", location = "bottomleft", anchor = c(x = xmin+2*hei, y=ymin+5*hei), transform = T, dist = dist, st.size=4, height=hei, model = 'WGS84')+
         ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, location = "topleft", scale = 0.06, symbol = 10, anchor = c(x = xmin+hei, y=ymax-hei))+
         scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
         geom_hline(yintercept=ymax, color = "red", size = 2)+
         geom_vline(xintercept=xmax, color = "red", size = 2)+
         theme(plot.margin=unit(c(1,.5,.5,-.2),"cm"), 
               title = element_text(size=8),
               axis.title = element_text( size=15, face=2),
               axis.line = element_line(size = 1, colour = "red", linetype=1),
               axis.text.x = element_text( angle = 0, size=15),
               axis.text.y = element_text( angle = 0, size=15))
      #Add movement paths for each tag in current plot
      for(j in 1:length(mdata$pathlist)){
        pl = mdata$pathlist[[j]]
        #loop through each pach segment
        for(l in unique(pl$CID)){
          seg = pl[which(pl$CID == l),]
          mp = mp+geom_path(data=seg, aes(x = as.numeric(LON), y = as.numeric(LAT)),
                          arrow = arrow(length = unit(0.2, "cm")), colour = 'red', alpha = 0.85, size = 1.7)
        }
      }
      # Randomized the label colors, make each plot look a bit more unique
      collist = collist[sample.int(length(collist), length(collist))]
     
      labelframe = NULL
      dat = NULL
      coords = NULL
      #Create a dataframe of all labels for this plot. This is needed for 
      #so that the label repel works on all labels for this plot
      for(j in 1:length(mdata$datalist)){
        reldata = mdata$datalist[[j]][1,]
        reldata$RELLON = as.numeric(reldata$RELLON)
        reldata$RELLAT = as.numeric(reldata$RELLAT)
        labelframe = rbind(labelframe, cbind(reldata$RELLON, reldata$RELLAT, collist[j],  paste("Tag:", reldata$TAG_ID," ",as.character(reldata$RELEASE_DATE), sep="")))
        mdata$datalist[[j]]$label = as.character(mdata$datalist[[j]]$CAPTURE_DATE)
        mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME == name)] = paste("You Captured: ", mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME == name)], sep = "") 
        labelframe = rbind(labelframe, cbind(as.numeric(mdata$datalist[[j]]$CAPLON), as.numeric(mdata$datalist[[j]]$CAPLAT), collist[j], as.character(mdata$datalist[[j]]$label)))
      }
      labelframe = as.data.frame(labelframe)
      names(labelframe) = c("x", "y","colID", "lab")
      labelframe$ID = 1:nrow(labelframe) 
      labelframe$x = as.numeric(as.character(labelframe$x))
      labelframe$y = as.numeric(as.character(labelframe$y))
      labelframe$colID = as.character(labelframe$colID)
      labelframe$lab = as.character(labelframe$lab)
      #Add labels to main plot
      mp = mp+geom_label_repel(data=labelframe, aes(x = x, y = y, label = lab),
                            fill = alpha(labelframe$colID,0.5), fontface = "bold",                        
                            color="black", segment.color = 'black', segment.size = .7,
                            family = "Helvetica",
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.5, "lines"))
  
      #Inset plot
      ip = ggplotGrob(
              ggRGB(l2, r=1, g=2, b=3, maxpixels = 100000, ext = e2, ggObj = T)+
              theme_map()+
              theme(panel.border = element_rect(colour = "black", fill = NA),
              plot.margin=unit(c(0,0,0,0),"cm"))+
              scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
              geom_segment(data=rectan, aes(x = x, y = y, xend = ex, yend = ey),colour=c("red"))
           )
      #Output plot, a combination of the main plot with inset plot positioned correctly
      g3 <- mp +
            annotation_custom(grob = ip, xmin = e@xmax-((e@xmax-e@xmin)/4), xmax = e@xmax+((e@xmax-e@xmin)/20),
                  ymin = e@ymax-((e@ymax-e@ymin)/4), ymax = e@ymax +((e@ymax-e@ymin)/20))
      #Save to file
      fn = system.file("extdata", "rewards", "maps", package = "SCtagging")
      ggsave(filename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), path = fn, plot = g3, width = 11, height = 10)
      #list of maps locations on disk
      outlist = c(outlist, paste(fn, paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), sep = '/'))
  }
  return(outlist)
}


