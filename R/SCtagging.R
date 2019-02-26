


simulate.movement = function(movement.rate = NULL, canvas = NULL, release.pos = NULL, ndays = 365*4, bestof = 4, ncrab = 100 ,type = "least.cost"){
  if(is.null(canvas)) canvas = file.path("C:", "workspace","data", "maps", "depthraster2.tif")
  if(is.null(movement.rate)) movement.rate = ave(max.rate.tag(n = 10))
  if(is.null(release.pos)) release.pos = c(-59.9, 46.5)   
  raster.path = canvas
  r <- raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr>-280 & mr< -60)] = -170
  mr = apply(mr, 2, function(x) dnorm(x,mean=-170,sd=60))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, directions=16)
  if(type  == "random.walk"){
    trans <- geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans <- geoCorrection(tr, type = "c", scl=FALSE)
  }
  crab = data.frame(1:100)
  crab$path = ""
  crab$curlon = release.pos[1]
  crab$curlat = release.pos[2]
  for(i in 1:ndays){
    
    for(c in 1:nrow(crab)){
      
      cost = 100000000000
      dir = sample(1:360, bestof, replace=TRUE)
      temppos = NULL
      for(b in 1:bestof){
        
        dp = destPoint(c(crab$curlon[c], crab$curlat[c]), dir[b], movement.rate)
        cd = costDistance(tr, c(crab$curlon[c], crab$curlat[c]), dp)
        
        if(cd < cost){
          temppos = dp
          cost = cd
        }
      }
      crab$path[c] = paste(crab$path[c], temppos[1], ",",temppos[2], sep ="")
      crab$curlon[c] = temppos[1]
      crab$curlat[c] = temppos[2]
      
    }
  }
  
  
}
max.rate.tag = function(n = 1){
  data = get.capturedata()
  ind = which(data$area == 'GULF')
  if(length(ind)>0) data = data[-ind,]
  
  ind = which(data$caplat == '0')
  if(length(ind)>0) data = data[-ind,]
  
  
  dist = distHaversine(cbind(as.numeric(data$rellon), as.numeric(data$rellat)), cbind(as.numeric(data$caplon) ,as.numeric(data$caplat)), r=6378137)
  inter = interval(data$sampdat, data$capdate)
  days = time_length(as.duration(inter), "days")
  
  
  
  
  #Remove days less than 1 year, account for drift
  data$dist = dist
  data$days = days
  ind = which(data$days <= 365 )
  if(length(ind)>0) data = data[-ind,]
  ind = which(is.na(data$days))
  if(length(ind)>0) data = data[-ind,]
  
  
  data$m.per.day = data$dist/data$days
  data = data[order(data$m.per.day, decreasing = T),]
  
  #remove gulf program data  
  ind = which(grepl("G", data$PID))
  if(length(ind)>0) data = data[-ind,]
  
  data$m.per.day
  return(data$m.per.day[1:n])
}
statsfigures = function(){
  out = data.frame()
  outall = data.frame()
  outarea = data.frame()
  years = 2004:2018
  nens = NULL
  sens = NULL
  xxxx = NULL
  for(i in 1:length(years)){
    print(years[i])
    zn = tagReturned_Applied("nens", as.character(years[i]))
    zta = tagApplied("nens", as.character(years[i]))
    zpn = tagReturned_Year("nens", as.character(years[i]))
    zs = tagReturned_Applied("sens", as.character(years[i]))
    zsa = tagApplied("sens", as.character(years[i]))
    zps = tagReturned_Year("sens", as.character(years[i]))
    zf = tagReturned_Applied("cfa4x", as.character(years[i]))
    zfa = tagApplied("cfa4x", as.character(years[i]))
    zpx = tagReturned_Year("cfa4x", as.character(years[i]))
    za = tagReturned_Applied("all", as.character(years[i]))
    zaa = tagApplied("all", as.character(years[i]))
    zpa = tagReturned_Year("all", as.character(years[i]))
    if(!is.numeric(zsa))zsa = 0
    if(!is.numeric(zta))zta = 0
    if(!is.numeric(zfa))zfa = 0
    if(!is.numeric(zaa))zaa = 0
    if(zpn == FALSE){
      zpn = NULL
      zpn$a = 0
      zpn$b = 0
    }
    if(zpx == FALSE){
      zpx = NULL
      zpx$a = 0
      zpx$b = 0
    }
    if(!is.logical(zn)){
      nens = c(nens, zn$retuni/zta)
      zn$year = as.character(years[i])
      zn$area = "nens"
      zn$tapp = zta
      zn$peo = zpn$a
      zn$upeo = zpn$b
      out = rbind(out, data.frame(zn))
    }else{
      nens = c(nens, NA)
    }
    if(!is.logical(zs)){
      sens = c(sens, zs$retuni/zsa)
      zs$year = as.character(years[i])
      zs$area = "sens"
      zs$tapp = zsa
      zs$peo = zps$a
      zs$upeo = zps$b
      out = rbind(out, data.frame(zs))
      
      
    }else{
      sens = c(sens, NA)
    }
    if(!is.logical(zf)){  
      xxxx = c(xxxx, zf$retuni/zfa)
      zf$year = as.character(years[i])
      zf$area = "cfa4x"
      zf$tapp = zfa
      zf$peo = zpx$a
      zf$upeo = zpx$b
      out = rbind(out, data.frame(zf))
      
    }else{
      xxxx = c(xxxx, NA)
    }
    if(!is.logical(za)){  
      za$year = as.character(years[i])
      za$area = "all"
      za$tapp = zaa
      za$peo = zpa$a
      za$upeo = zpa$b
      outall = rbind(outall, data.frame(za))
      
    }
    #     if(!is.logical(zn) && !zta == 0)
    #      nens = c(nens, zn$retuni)
    #     else nens = c(nens, NA)
    #     
    #     if(!is.logical(zs))
    #      sens = c(sens, zs$retuni)
    #     else sens = c(sens, NA)
    #     
    #     if(!is.logical(zf))  
    #      xxxx = c(xxxx, zf$retuni)
    #     else xxxx = c(xxxx, NA)
  }
  years = "2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018"
  za = tagReturned_Applied("all", as.character(years))
  zaa = tagApplied("all", as.character(years))
  zpa = tagReturned_Year("all", as.character(years))
  if(!is.logical(za)){  
    za$year = as.character(years[i])
    za$area = "all"
    za$tapp = zaa
    za$peo = zpa$a
    za$upeo = zpa$b
    outall = rbind(outall, data.frame(za))
    
  }
  write.csv( out, file.path("D:", "SCtagging", "stats_year_area.csv"))
  write.csv( outall, file.path("D:", "SCtagging", "stats_year.csv"))
  
  
  #####################################################

  
  regions = c("cfanorth", "cfasouth", "cfa4x")
  k <- data.frame(nens, sens, xxxx)
  k = as.data.frame( k )
  colnames(k) = regions
  rownames(k) = unlist(strsplit(years, ","))
  k = k[3:nrow(k),]
  uyrs = as.numeric(rownames(k) ) 
  pts = c(19, 22, 24)
  lns = c(1, 1, 1)
  cols = c("goldenrod2", "coral3",  "mediumpurple2")
  yrange = range (k, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]

  fn = file.path("C:", "Users","cameronbj","Desktop","Presentation","returns.pdf" )
  pdf(file=fn, width=7, height=7, bg='white')
  m=1
  miss <- !is.na(k[,m])
  plot( uyrs[miss], na.omit(k[,m]),  type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange, ylab="Return Rate(applied/returned)", xlab="Year")
  
  m=2
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  m=3
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  legend("topright", c("N-ENS", "S-ENS", "4X"), bty="n", lty=lns, lwd=2, pch=pts, col=cols, cex=1.2 )
  
  dev.off()
  
  

  k <- data.frame(out$upeo[which(out$area == "nens")], out$upeo[which(out$area == "sens")], out$upeo[which(out$area == "cfa4x")])
  k = as.data.frame( k )
  colnames(k) = regions
  rownames(k) =   unlist(strsplit(years, ","))
  k = k[3:nrow(k),]
  uyrs = as.numeric(rownames(k) ) 
  pts = c(19, 22, 24)
  lns = c(1, 1, 1)
  cols = c("goldenrod2", "coral3",  "mediumpurple2")
  yrange = range (k, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1]+1, xrange[2], 2)
  fn = file.path("C:", "Users","cameronbj","Desktop","Presentation","people.pdf" )
  pdf(file=fn, width=7, height=7, bg='white')
  m=1
  miss <- !is.na(k[,m])
  plot( uyrs[miss], na.omit(k[,m]),  type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange, ylab="Number of People", xlab="Year")
  
  m=2
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  m=3
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  legend("topleft", c("N-ENS", "S-ENS", "4X"), bty="n", lty=lns, lwd=2, pch=pts, col=cols, cex=1.2 )
  
  dev.off()
  
  
  
  
  
  
  
  
  
  
  k <- data.frame(out$spe[which(out$area == "nens")], out$spe[which(out$area == "sens")], out$spe[which(out$area == "cfa4x")])
  k = as.data.frame( k )
  colnames(k) = regions
  rownames(k) =   unlist(strsplit(years, ","))
  k = k[3:nrow(k),]
  uyrs = as.numeric(rownames(k) ) 
  pts = c(19, 22, 24)
  lns = c(1, 1, 1)
  cols = c("goldenrod2", "coral3",  "mediumpurple2")
  yrange = range (k, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1]+1, xrange[2], 2)
  fn = file.path("C:", "Users","cameronbj","Desktop","Presentation","movrate.pdf" )
  pdf(file=fn, width=7, height=7, bg='white')
  m=1
  miss <- !is.na(k[,m])
  plot( uyrs[miss], na.omit(k[,m]),  type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange, ylab="Movement Rate (km/month)", xlab="Year")
  
  m=2
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  m=3
  miss <- !is.na(k[,m])
  lines( uyrs[miss], na.omit(k[,m]), type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], xlim=xrange, ylim=yrange)
  
  legend("topleft", c("N-ENS", "S-ENS", "4X"), bty="n", lty=lns, lwd=2, pch=pts, col=cols, cex=1.2 )
  
  dev.off()
  ########################################################
  
outarea = NULL
  
  zn = tagReturned_Applied("nens", as.character(years))
  zta = tagApplied("nens", as.character(years))
  zs = tagReturned_Applied("sens", as.character(years))
  zsa = tagApplied("sens", as.character(years))
  zf = tagReturned_Applied("cfa4x", as.character(years))
  zfa = tagApplied("cfa4x", as.character(years))
  za = tagReturned_Applied("all", as.character(years))
  zaa = tagApplied("all", as.character(years))
  zpx = tagReturned_Year("cfa4x", as.character(years))
  zpn = tagReturned_Year("nens", as.character(years))
  zps = tagReturned_Year("sens", as.character(years))
  zn$year = "all"
  zn$area = "nens"
  zn$tapp = zta
  zn$peo = zpn$a
  zn$upeo = zpn$b
  outarea = rbind(outarea, data.frame(zn))
  zs$year = "all"
  zs$area = "sens"
  zs$tapp = zsa
  zs$peo = zps$a
  zs$upeo = zps$b
  outarea = rbind(outarea, data.frame(zs))
  zf$year = "all"
  zf$area = "cfa4x"
  zf$tapp = zfa
  zf$peo = zpx$a
  zf$upeo = zpx$b
  outarea = rbind(outarea, data.frame(zf))       
  zna = tagReturned_Applied("all", as.character(years))
  ztaa = tagApplied("all", as.character(years))
  zpaa = tagReturned_Year("all", as.character(years))
  
  zna$year = "all"
  zna$area = "all"
  zna$tapp = ztaa
  zna$peo = zpaa$a
  zna$upeo = zpaa$b
  outarea = rbind(outarea, data.frame(zna))   

  write.csv( outarea, file.path("D:", "SCtagging", "stats_area.csv"))
  
  
  
  #######################################################################
  
  
  cdd = get.capturedata()
  cdd$abscaparea = absolutely.in.area2(area = "unknown", abslon = cdd$caplon, abslat = cdd$caplat)
  cdd$abssamparea = absolutely.in.area2(area = "unknown", abslon = cdd$rellon, abslat = cdd$rellat)
  print(cdd[which(cdd$abscaparea == "NENS" & cdd$abssamparea == "SENS"),])
  print(cdd[which(cdd$abscaparea == "SENS" & cdd$abssamparea == "NENS"),])
  
 cdd4x = cdd[which(cdd$abssamparea == "CFA-4X" & as.numeric(as.character(cdd$sampyear)) >= 2008 ),]
counts = as.data.frame(table(as.character(cdd4x$PID)))
which(counts$Freq > 1)
which(counts$Freq > 2)
  
print(cdd[which(cdd$abscaparea == "CFA-4X" & cdd$abssamparea == "SENS"),])
print(cdd[which(cdd$abscaparea == "SENS" & cdd$abssamparea == "CFA-4X"),])

cp = get.path()
cp$CDATE = dmy(cp$CDATE)
ad = merge(cp, cdd, by.x = c("TID", "CDATE"), by.y = c("PID", "capdate") )

  #######################################################################
  
  
  # 
  # mydf = cbind(mydf, years)
  # mydfm <- data.frame(nens, sens, xxxx)
  # 
  # 
  # 
  # 
  # 
  # 
  # plot(NA,xlim=c(0,nrow(mydfm)+1),ylim=c(min(mydfm,na.rm=TRUE)-1,max(mydfm,na.rm=TRUE)+1))
  # mapply(function(x,color){
  #   dat <- na.omit(cbind(1:length(x),x))
  #   lines(dat[,1],dat[,2],col=color)
  # },mydfm,c("red","blue","green"))
  

  
  
  arl = read.csv("D:/SCtagging/arel.csv")
  apa = read.csv("D:/SCtagging/acpath.csv")
  names(apa) = c("rows", "id", "Days", "PID", "Km")
  apa$id = gsub("ZSC-", "", apa$id)
  apa$Area = NA
  
  arl$Area = absolutely.in.area2("unknown", arl$lon, arl$lat)
  
  for(i in 1:nrow(apa)){
    apa$Area[i] = arl$Area[which(arl$ANIMAL_ID == apa$id[i])]
    
  }
  
  ggplot2.scatterplot(data=apa, backgroundColor="white", size=5, xName='Days',yName='Km', groupName="Area", legendTitle = "Release Area", xtickLabelRotation = 90)
  
  
  
  ad$DIST = as.numeric(ad$DIST)

ad$Days = ad$CDATE - as.Date(ad$sampdat)
ad$Days = as.numeric(ad$Days)
  ad$Sample.Area = ad$abssamparea
  
  ad$Km = ad$DIST
  xc = unlist(rainbow(length(unique(ad$sampyear))+3))
xc = xc[4:length(xc)]
#dt  = ad[which(ad$Sample.Area!='GULF'),]
dt  = ad[which(ad$Days > 10),]
dt  = dt[which(dt$caplat != 0),]
dt  = dt[which(dt$abssamparea != "GULF"),]
avli = sum(dt$DIST)/sum(dt$Days)
avd = dt$Days*avli
dt$year.group = NA
# dt$year.group[which(dt$sampyear >= 1996 & dt$sampyear <= 1998)] = "1996-1998"
# dt$year.group[which(dt$sampyear >= 1999 & dt$sampyear <= 2003)] = "1999-2003"
# dt$year.group[which(dt$sampyear >= 2004 & dt$sampyear <= 2008)] = "2004-2008"
# dt$year.group[which(dt$sampyear >= 2009 & dt$sampyear <= 2013)] = "2009-2013"
# dt$year.group[which(dt$sampyear >= 2014)] = "2014-2017"

dt$year.group[which(dt$sampyear >= 2004 & dt$sampyear <= 2008)] = "2004-2008"
dt$year.group[which(dt$sampyear >= 2009 & dt$sampyear <= 2013)] = "2009-2013"
dt$year.group[which(dt$sampyear >= 2014)] = "2014-2018"
ind = which(is.na(dt$year.group))
if(length(ind>0)) dt = dt[-ind,]


ggplot2.scatterplot(data=dt, backgroundColor="white", size=5, xName='Days',yName='Km', groupName="Sample.Area", faceting=TRUE, facetingVarNames="year.group", legendTitle = "Release Area", facetingDirection="vertical", xtickLabelRotation = 90, addRegLine=TRUE, regLineColor="blue",addConfidenceInterval=TRUE, smoothingMethod="loess")

  ggplot2.scatterplot(data=dt, backgroundColor="white", mainTitle="Kilometers vs. Days by Release Area", xName='Days',yName='Km', groupName="Sample.Area", addRegLine=TRUE, regLineColor="blue",addConfidenceInterval=TRUE, smoothingMethod="loess")
  ggplot2.scatterplot(data=dt, backgroundColor="white", size=5, mainTitle="Kilometers vs. Days by Release Area", groupColors=xc, xName='Days',yName='Km', groupName="sampyear", faceting=TRUE, facetingVarNames="Sample.Area")
plot(avli, type="l")



dt$relarea = absolutely.in.area2("unknown", dt$rellon, dt$rellat)
dt$caparea2 = absolutely.in.area2("unknown", dt$caplon, dt$caplat)

ggplot2.barplot(data=dt[which(dt$relarea == "SENS"),], xName='relarea', yName="caparea2", position=position_dodge())

  }
agescript = function(){
  
  da = get.capturedatacc()
  da = da[which(as.numeric(da$sampyear) > 2004),]
  cc2 = da[which(da$sampcc == "2"),]
  cc2$days =  ymd(cc2$capdate) - ymd(cc2$sampdat) 
  cyea = c(mean(cc2$days[which(cc2$capcc == "2")])/365.25,
           mean(cc2$days[which(cc2$capcc == "3")])/365.25,
           mean(cc2$days[which(cc2$capcc == "4")])/365.25)
  
  plot(c(2 ,3, 4), cyea, type = "h", lwd = 5, axes = F,ylim = c(0, 2.5), main = "Released as cc2",ylab = "years", xlab = "Carapace Condition")
  axis(1, at=c(2 ,3, 4),labels=c("cc2", "cc3", "cc4"), las=2)
  axis(2, at=c(0 ,1, 2),labels=c("0", "1", "2"), las=2)
  text(c(2 ,3, 4)+c(.1,-.1,-.1), cyea, label = c(paste("n=", length(which(cc2$capcc == "2")), sep ="" ), paste("n=", length(which(cc2$capcc == "3")), sep ="" ), paste("n=", length(which(cc2$capcc == "4")), sep ="" )), pos = 3, cex = 0.8, col = "red")
  
  cc3 = da[which(da$sampcc == "3"),]
  cc3$days =  ymd(cc3$capdate) - ymd(cc3$sampdat) 
  cyea = c(mean(cc3$days[which(cc3$capcc == "3")])/365.25,
           mean(cc3$days[which(cc3$capcc == "4")])/365.25,
           mean(cc3$days[which(cc3$capcc == "5")])/365.25)
  plot(c(1 ,2, 3), cyea, type = "h", lwd = 5, axes = F,ylim = c(0, 3.5), main = "Released as cc3",ylab = "years", xlab = "Carapace Condition")
  axis(1, at=c(1 ,2, 3),labels=c("cc3", "cc4", "cc5"), las=2)
  axis(2, at=c(0 ,1, 2, 3),labels=c("0", "1", "2", "3"), las=2)
  text(c(1 ,2, 3)+c(.14,-.1,-.1), cyea, label = c(paste("n=", length(which(cc3$capcc == "3")), sep ="" ), paste("n=", length(which(cc3$capcc == "4")), sep ="" ), paste("n=", length(which(cc3$capcc == "5")), sep ="" )), pos = 3, cex = 0.8, col = "red")
  
  
  cc4 = da[which(da$sampcc == "4"),]
  cc4$days =  ymd(cc4$capdate) - ymd(cc4$sampdat) 
  cyea = c(mean(cc4$days[which(cc4$capcc == "4")])/365.25)
  plot(c(1), cyea, type = "h", lwd = 5, axes = F,ylim = c(0, 3.5), main = "Released as cc4",ylab = "years", xlab = "Carapace Condition")
  axis(1, at=c(1),labels=c("cc4"), las=2)
  axis(2, at=c(0 ,1, 2, 3),labels=c("0", "1", "2", "3"), las=2)
  text(c(1)+c(.14), cyea, label = c(paste("n=", length(which(cc4$capcc == "4")), sep ="" )), pos = 3, cex = 0.8, col = "red")
  
  hist(as.numeric(cc3$days[which(cc3$capcc == "3")]/365.25), breaks = 10, cex.main = 1, main = "Captured as CC3", xlab = "years")
  hist(as.numeric(cc3$days[which(cc3$capcc == "4")]/365.25), breaks = 10, cex.main = 1, main = "Captured as CC4", xlab = "years")
  
  hist(as.numeric(cc2$days[which(cc2$capcc == "2")]/365.25), cex.main = 1, main = "Captured as CC2", xlab = "years")
  hist(as.numeric(cc2$days[which(cc2$capcc == "3")]/365.25), cex.main = 1, main = "Captured as CC3", xlab = "years")
  hist(as.numeric(cc2$days[which(cc2$capcc == "4")]/365.25), cex.main = 1, main = "Captured as CC4", xlab = "years")
  
  hist(as.numeric(cc4$days[which(cc4$capcc == "4")]/365.25), cex.main = 1, main = "Captured as CC4", xlab = "years")
  hist(as.numeric(cc4$days[which(cc4$capcc == "5")]/365.25), cex.main = 1, main = "Captured as CC5", xlab = "years")
  
  
}

capture.history.spa= function(region = "ScotianShelf"){
  mcform = get.releases()
  cd = get.capturedata.oracle()

  mcform$begin.time = NA
  mcform$initial.age = NA
  mcform$time.intervals = NA
  mcform$ch = NA

  for(i in 1: nrow(mcform)){
    mcform$begin.time[i] = ymd(mcform$sampdat[i])
    mcform$initial.age[i] = mcform$cc[i]
    subcd = cd[which(cd$PID == mcform$PID[i]),]
    print(subcd)
    ch = h = c(1, 0, 0, 0, 0, 0, 0)
    h[as.numeric(subcd$year) - as.numeric(unique(subcd$sampyear[1])) +1] = 1
    mcform$ch[i] = paste(h, collapse = "")
    
    if(nrow(subcd)>0){
    yearlis = c()
    for(j in 1:nrow(subcd)){
      if(!subcd$year[j] %in% yearlis){
      yearlis = c(yearlis, subcd$year[j])
      if(is.na(mcform$time.intervals[i])){
        print(subcd$capdate[j] - subcd$sampdat[j])
        mcform$time.intervals[i] = subcd$capdate[j] - subcd$sampdat[j]
      }
      else{
        mcform$time.intervals[i] = c(mcform$time.intervals[i], subcd$capdate[j] - subcd$sampdat[j])
      }
  
      }
    }
    }
  }
mc = mcform
begin.time = mc$begin.time
initial.age = mc$initial.age
time.intervals =  mc$time.intervals
mc$begin.time = NULL
mc$initial.age = NULL
mc$time.intervals = NULL
x=process.data(data = mc, initial.ages = initial.age, time.intervals = time.intervals )
x=process.data(data = mc, initial.ages = initial.age )

x = process.data(data = mc)
resight.matrix(y)

naive.survival(x)

mcmc_mode(x)
accumulate_data(mc)

}



#' @title  get.capturedatacc
#' @description  Return  capture data
#' @import ROracle RMySQL rJava
#' @return dataframe
#' @export
get.capturedatacc = function(){
  local_port = "3309"
  
  SCtunnel = openportSC(local.port = local_port)
  
  con <- RMySQL::dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- RMySQL::dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- RMySQL::dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, bio.cc, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year, capture.carapace_cond, capture.tagcode 
                    from capture join bio where bio.tag_id  = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.subarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- RMySQL::fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  closeportSC(SCtunnel)
  da = unique(da)
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  da$sample_id = NULL
  
  names(da) = c("PID", "sampcc", "capdate", "caparea","caplat", "caplon", "year", "capcc", "relcode", "area", "subarea", "sampyear", "sampdat", "samplat", "samplon")
  previd = ""
  # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$samplat[i] = da$caplat[i-1]
      da$samplon[i] = da$caplon[i-1]
      da$sampdat[i] = da$capdat[i-1]
      da$sampcc[i] = da$capcc[i-1]
    }
    previd = da$PID[i] 
  }
  
  
 
  
  
  return(da)
  
}
#' @title  get.capturedata.oracle
#' @description  Get all tag data view from oracle database 
#' @import ROracle
#' @return dataframe of data
#' @export
get.capturedata.oracle = function(){
  gstring= ""
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  res <- ROracle::dbSendQuery(con, paste("select * from SCT_ALL", gstring, sep = "")) 
  res <- ROracle::fetch(res)
  
  names(res) = c("PID", "caplat", "caplon", "capdate", "caparea", "year", "csubarea", "rewarded", "sampdat", "area", "subarea", "sampyear","samplat", "samplon", "carapace", "chela", "cc")

  
  return(res)
  
}
get.capturedata.ens = function(){
  local_port = "3308"
  
  SCtunnel = openportSC(local.port = local_port)
  
  con <- dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  
 rs <- dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.subarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
                    from capture join bio where bio.tag_id  = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.subarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  closeportSC(SCtunnel)
  da = unique(da)
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea", "csubarea","caplat", "caplon", "year", "area", "subarea", "sampyear", "sampdat", "samplat", "samplon")
  previd = ""
 # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$samplat[i] = da$caplat[i-1]
      da$samplon[i] = da$caplon[i-1]
      da$sampdat[i] = da$capdat[i-1]
    }
    previd = da$PID[i] 
  }
  
  names(da) = c("PID", "capdate", "caparea", "csubarea","caplat", "caplon", "year", "area", "subarea", "sampyear", "sampdat", "rellat", "rellon")
  
  
  
  return(da)
  
}
get.alldata.ens = function(){
  local_port = "3308"
  
  SCtunnel = openportSC(local.port = local_port)
  
  con <- dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  
  rs <- dbSendQuery(con, statement = "Select * from alldata;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  closeportSC(SCtunnel)
  da = unique(da)
  
  names(da) = c("PID", "caplat", "caplon","capdate", "caparea", "year","csubarea", "rewarded","reldat","area", "subarea","relyear", "rellat", "rellon", "carapace", "chela", "cc")
  previd = ""
  # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$rellat[i] = da$caplat[i-1]
      da$rellon[i] = da$caplon[i-1]
      da$reldat[i] = da$capdat[i-1]
    }
    previd = da$PID[i] 
  }
  
  return(da)
  
}



#' @title  Generate Rewards 
#' @description  Open java program that generates reward letters.
#' @export
generate.rewards = function(){
  
  try(if(!exists("bio.datadirectory"))
    stop("You must define the bio.datadirectory")
  )  
  
  try(if(!exists("enssnowc.user") | !exists("enssnowc.password"))
    stop("You must define enssnowc.user and enssnowc.password")
  )  
  
  
  system(paste("java -jar", system.file("extdata", "TagApp", "taggingApp.jar", package = "SCtagging"), enssnowc.user, enssnowc.password, bio.datadirectory, "reward", sep = " "))
  
}
enter.returns.private = function(){
  
  try(if(!exists("ecomod.datadirectory"))
    stop("You must define the ecomod.datadirectory")
  )  
  
  try(if(!exists("enssnowc.user") | !exists("enssnowc.password"))
    stop("You must define enssnowc.user and enssnowc.password")
  )  
  setwd(ecomod.datadirectory)
  
  system(paste("java -jar data/taggingApp.jar", enssnowc.user, enssnowc.password, ecomod.datadirectory, "enter", sep = " "))
  
}
enter.releases = function(){
  browseURL("http://www.enssnowcrab.com/snowcrabgroup/tagentry.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}
view.stats.private = function(){
  browseURL("http://www.enssnowcrab.com/snowcrabgroup/tagging.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}
view.stats.public = function(){
  browseURL("http://www.enssnowcrab.com/tagging.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  return(TRUE)
  
}
enter.returns.public = function(){
  browseURL("http://www.enssnowcrab.com/tagentry.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}


#' @title  Shortest Paths 
#' @description  Creates and writes the shortest paths to database
#' @param rasterpath The canvas on which to calculate shortest path, defaults to depth raster.
#' @param neighborhood The number of adjacent cell over which to calculate, defaults to 16.
#' @param type The type of calculation, either 'random.walk' or 'least.cost'. Defaults to random.walk
#' @param redo Set redo = TRUE if you want to rewrite all data, FALSE to only update new entries
#' @param region Either 'ScotianShelf' or 'Gulf'
#' @import PBSmapping raster gdistance ROracle RMySQL rJava
#' @return dataframe
#' @export
shortestpaths.SC = function(raster.path = system.file("extdata", "depthraster2.tif", package = "SCtagging"), neighborhood = 16, type = "random.walk", redo = F, region = "ScotianShelf"){
  drv <- dbDriver("Oracle")
    gstring = ""
  if(region == "Gulf") gstring = "_GULF"
  x = get.capturedata(region)
  x$PID = as.character(x$PID)
  # x$capdate = as.POSIXct(as.numeric(as.character(x$capdate)), origin = "1960-01-01") 
  trans = NULL
  r = raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr>-280 & mr< -60)] = -170
  mr = apply(mr, 2, function(x) dnorm(x,mean=-170,sd=60))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, neighborhood)
  if(type  == "random.walk"){
    trans = geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans = geoCorrection(tr, type = "c", scl=FALSE)
  }
  # 
  # local_port = "3309"
  # SCtunnel = openportSC(local.port = local_port)
  # 
  # con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  # 
  
  
  
  
  #rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  #da <- fetch(rs, n = -1)   # extract all rows
  
  # dbDisconnect(con) 
  # closeportSC(SCtunnel)
  # 
  dftowrite = NULL
  df2towrite = NULL
  dxtowrite = NULL
  append = F
  overwrite = T
  if(!redo){
    con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    respat <- ROracle::dbSendQuery(con, "select * from SCT_PATHS") 
    da <- fetch(respat)
    ROracle::dbDisconnect(con)
    append = T
    overwrite = F    
    goodind = which(paste(as.character(x$PID), as.character(x$capdate)) %in% paste(as.character(da$id), as.character(da$cdat)))
    if(length(goodind) > 0) x = x[-goodind,]
    zeroind = which(as.numeric(x$caplat) == 0 | x$caplat == 'unknown')
    if(length(zeroind) > 0) x = x[-zeroind,]
    
    
    
    for(i in 1:nrow(x)){
      
      start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
      end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
      
      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }
      else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
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
      
      
      dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y)
      dxtowrite = rbind(dxtowrite, dxp)
      df2towrite = rbind(df2towrite, cbind(x$PID[i], count, x$capdat[i], leng$length))
      
      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), x$capdat[i], leng$length))
      
    }
    
    
  }
  else{
    count  = 1
    previd = ""
    for(i in 1:nrow(x)){
      if(x$PID[i] == previd){
        count = count+1
      }
      else{
        previd = x$PID[i]
        count = 1
      }
      start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
      end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
      
      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }
      else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
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
        if(cor$x[k] == xrep){ cor$x[k] =  end[1] }
        else{ xrep = 1000000 }
        
        if(cor$y[k] == yrep){ cor$y[k] =  end[2] }
        else{ yrep = 1000000}
      }
      
      names(cor) = c("X", "Y")
      cor$PID = 1
      cor$POS = 1:nrow(cor)
      tpoly = as.PolySet(cor, projection = "LL")
      leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
      
      dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y)
      dxtowrite = rbind(dxtowrite, dxp)
      df2towrite = rbind(df2towrite, cbind(x$PID[i], count, x$capdat[i], leng$length))
      
      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), x$capdat[i], leng$length))
      
      
    }
    
  }
  
  if(!is.null(dftowrite)){
    dftowrite = data.frame(dftowrite)
    df2towrite = data.frame(df2towrite)
    str(df2towrite)
    names(dftowrite) = c("ID", "LON", "LAT", "CDATE", "DIST")
    names(df2towrite) = c("TID", "CID", "CDATE", "DIST")
    dxtowrite = data.frame(dxtowrite)
    names(dxtowrite) = c("TID", "CID", "POS", "LON", "LAT")
    drv <- dbDriver("Oracle")
    if(redo){
      
      con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
      
      if(dbExistsTable(con, "SCT_PATHS"))dbSendQuery(con, "DROP TABLE SCT_PATHS")
      if(dbExistsTable(con, "SCT_PATH"))dbSendQuery(con, "DROP TABLE SCT_PATH")
      
      dbDisconnect(con)
    }
    Sys.setenv(TZ = "America/Halifax")
    Sys.setenv(ORA_SDTZ = "America/Halifax")
    
    dftowrite$CDATE = as.POSIXct(as.numeric(as.character(dftowrite$CDATE)), origin = '1970-01-01')
    df2towrite$CDATE = as.POSIXct(as.numeric(as.character(df2towrite$CDATE)), origin = '1970-01-01')
    
    #attr(dftowrite$LON, "ora.type") <- 'CLOB'
    #attr(dftowrite$LON, "ora.encoding") <- 'UTF-8'
    #attr(dftowrite$LAT, "ora.type") <- 'CLOB'
    # attr(dftowrite$LAT, "ora.encoding") <- 'UTF-8'
    
    
    ## Use username/password authentication.
    
    
    ###################################################
    
    
    # con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    # dbWriteTable(con, "SCT_PATHS", dx, date = T, row.names = F)
    # dbDisconnect(con)
    
    ###################NEED TO UPDATE, FIND SOLUTION THE SLOW CLOB VALUES. PROCEEDING WITH MYSQL FOR NOW
    
    xx= NULL
    xy=NULL
    xx = as.character(as.Date(dftowrite$CDATE))
    xx[which(is.na(xx))] = "1111-11-11"
    xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
    
    dftowrite$CDATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
    dftowrite$CDATE[which(dftowrite$CDATE == "11/11/1111")] = NA
    
    xx= NULL
    xy=NULL
    xx = as.character(as.Date(df2towrite$CDATE))
    xx[which(is.na(xx))] = "1111-11-11"
    xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
    
    df2towrite$CDATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
    df2towrite$CDATE[which(df2towrite$CDATE == "11/11/1111")] = NA
    
    
    
    # local_port = "3309"
    # SCtunnel = openportSC(local.port = local_port)
    # 
    # con <- dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
    # 
    # rs <- RMySQL::dbWriteTable(con, "paths",  dftowrite, row.names = F, overwrite = T, append = FALSE)
    # rs <- RMySQL::dbWriteTable(con, "path",  dxtowrite, row.names = F, overwrite = T, append = FALSE)
    # 
    # dbDisconnect(con) 
    # closeportSC(SCtunnel)

    con <- dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
    
    dbWriteTable(con,"SCT_PATHS", dxtowrite)
    dbWriteTable(con,"SCT_PATH", df2towrite)
    dbDisconnect(con)
    
    print("New paths calculated and written to paths table.")
  }
  else{
    print("No new paths created.")  
  }
  return(dftowrite)
}
#' @title  mirror2esnssite
#' @description  ESSENTIAL FUNCTION. Must call after any data entry in order for the website to be up to date! 
#' @import ROracle RMySQL DBI rJava
#' @return status message in case it was called by webpage
#' @export
mirror2esnssite = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf")gstring = "_GULF"
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  resbio <- ROracle::dbSendQuery(con, paste("select * from SCT_BIO", gstring, sep = "")) 
  resbio <- ROracle::fetch(resbio)
  ressam <- ROracle::dbSendQuery(con, paste("select * from SCT_SAMPLE", gstring, sep = "")) 
  ressam <- ROracle::fetch(ressam)
  restri <- ROracle::dbSendQuery(con, paste("select * from SCT_TRIP", gstring, sep = "")) 
  restri <- ROracle::fetch(restri)
  rescap <- ROracle::dbSendQuery(con, paste("select * from SCT_CAPTURE", gstring, sep = "")) 
  rescap <- ROracle::fetch(rescap)
  
  respeo <- ROracle::dbSendQuery(con, paste("select * from SCT_PEOPLE", gstring, sep = "")) 
  respeo <- ROracle::fetch(respeo)
  #respat <- ROracle::dbSendQuery(con, paste("select * from SCT_PATHS", gstring, sep = "")) 
  #respat <- fetch(respat)
  resall <- get.capturedata.oracle()
  
  ROracle::dbDisconnect(con)
  resall$capdate = as.character(as.Date(resall$capdate))
  resall$sampdat = as.character(as.Date(resall$sampdat))
  
  xx = as.character(as.Date(restri$RELEASE_DATE))
  xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
  
  restri$RELEASE_DATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
  
  xx = as.character(as.Date(rescap$CAPTURE_DATE))
  xx[which(is.na(xx))] = "1111-11-11"
  xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
  
  rescap$CAPTURE_DATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
  rescap$CAPTURE_DATE[which(rescap$CAPTURE_DATE == "11/11/1111")] = NA
  
  
  restri = restri[, c("TRIP_ID", "TECHNICIAN", "VESSEL", "CFA", "RELEASE_DATE", "YEAR", "STATSAREA", "REPORTED", "CAPTAIN", "SUBAREA")]
  rescap = rescap[, c("TAG", "CAPTURE_DATE", "PERSON", "PERSON_B", "LAT_DDMM_MM", "LONG_DDMM_MM", "LAT_DD_DDDD", "LONG_DD_DDDD", "FATHOMS", "RELCODE", "COMMENTS", "CAPTAIN", "VESSEL","YEAR", "STATSAREA", "CARAPACE_COND","REWARDED", "SUBAREA")]
  
  names(resbio) = c("sample_num", "tag_id", "carapace_w", "chela_h", "cc", "durometer")
  
  names(ressam) = c("sample_id", "trip", "Lat_DDMM_MM", "long_DDMM_MM", "Lat_DD_DDDD", "Long_DD_DDDD", "fathoms", "comment")
  names(restri) = c("trip_id", "technician", "vessel", "cfa", "date", "year", "statsarea", "Reported", "captain", "subarea")
  names(rescap) = c("tag", "date", "person", "person_B", "Lat_DDMM_MM", "long_DDMM_MM", "Lat_DD_DDDD", "Long_DD_DDDD", "fathoms", "tagcode", "comment", "captain", "vessel", "year", "statsarea", "carapace_cond", "rewarded", "subarea")
  # names(respat) = c("id", "lon", "lat", "cdat", "dist")
  names(respeo) = tolower(names(respeo))
  
  
  
  local_port = "3308"
  SCtunnel = openportSC(local.port = local_port)
  if(region == "ScotianShelf")con <- RMySQL::dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  if(region == "Gulf")con <- RMySQL::dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_gulf", sep = ""), password = enssnowc.password, dbname = "enssnowc_GulfTag",  port = as.numeric(local_port), host = "localhost")
  
  rs <- RMySQL::dbWriteTable(con, "bio", resbio, row.names = F, overwrite = T, append = FALSE)
  rs <- RMySQL::dbWriteTable(con, "capture", rescap, row.names = F, overwrite = T, append = FALSE)
  #  rs <- RMySQL::dbWriteTable(con, "paths", respat, row.names = F, overwrite = T, append = FALSE)
  rs <- RMySQL::dbWriteTable(con, "sample", ressam, row.names = F, overwrite = T, append = FALSE)
  rs <- RMySQL::dbWriteTable(con, "trip", restri, row.names = F, overwrite = T, append = FALSE)
  rs <- RMySQL::dbWriteTable(con, "people", respeo, row.names = F, overwrite = T, append = FALSE)
  rs <- RMySQL::dbWriteTable(con, "alldata", resall, row.names = F, overwrite = T, append = FALSE)
  
  RMySQL::dbDisconnect(con) 
  closeportSC(SCtunnel)
  
  return("mirror successfull !!")
}

#' @title  create.tag.kml
#' @description  Function that generates a kml plot of tag data 
#' @param preview Boolean default TRUE
#' @param write Boolean default TRUE
#' @param tofile path to write the resulting kml
#' @import RMySQL chron geosphere kmlbuilder
#' @export
create.tag.kml = function(preview = T, write = T, tofile = NULL){
  require(kmlbuilder)
  local_port = "3308"
  SCtunnel = openportSC(local.port = local_port)
  
  
  con <- dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
                    from capture join bio where bio.tag_id = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  
  rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  path <- fetch(rs, n = -1)   # extract all rows
  
  sa <- dbSendQuery(con, statement = "SELECT trip.trip_id, trip.statsarea, trip.year, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id ;")
  
  samps <- fetch(sa, n = -1)   # extract all rows
  
  closeportSC(SCtunnel)
  dbDisconnect(con)
  
  
  
  da$sample_id = NULL
  
  da$trip_id = NULL
  
  
  
  names(samps) = c("area", "inArea","inFolder", "date", "lat", "lon", "unk")
  
  samps$inFolder = paste("rel_", samps$inFolder, sep = "") 
  
  
  
  names(path) = c("id","lon", "lat", "cdat", "dist")
  
  path = path[order(path$cdat),]
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  ddif = NULL
  ddate = NULL
  ind = which(as.character(da$caplat) == "0")
  da = da[-ind,]
  
  ind = which(as.character(da$year) == "")
  if(length(ind) > 0 )da = da[-ind,]
  
  ind = which(as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF")
  
  da = da[-ind,]
  dup = 0
  
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  mattxt = "unknown unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  da = da[order(da$capdat),]
  outframe = NULL
  outarrow = NULL
  
  dx = split(da, da$sampyear)
  for(i in 1:length(dx)){
    
    
    out = NULL
    da_y = dx[[i]]
    
    
    da_tid = split(da_y, da_y$PID)
    
    for(j in 1:length(da_tid)){
      fin_da = da_tid[[j]]
      sampchron = chron(fin_da$sampdat[1], format = "y-m-d")
      
      
      
      fin_path = path[which(path$id == fin_da$PID[1]),]
      sub_out = NULL
      description = "<![CDATA[<b> datetxt  <br><br> disttxt</b>]]>"
      disttxt = ""
      datetxt = ""
      tid = ""
      
      for(k in 1:nrow(fin_path)){
        capchron = chron(  fin_da$capdate[k], format = "y-m-d")
        
        date = as.character(   fin_da$capdate[k])
        
        if(is.na(capchron)){
          date = paste("No date returned, received in ",fin_da$year[k])
          if( fin_da$caparea[k] == "NENS") capchron = chron(paste( fin_da$year[k],"-07-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "SENS") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "GULF") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "4X") capchron = chron(paste( fin_da$year[k],"-03-15", sep=""), format = "y-m-d")
          
        }
        dif = capchron - sampchron
        dif = as.numeric(dif)
        
        cc = ""
        hr = ""
        lst = ""
        if(is.na(dif)){ cc = "000000"
        hr = "http://enssnowcrab.com/R/tag/go/blarrow.png"
        lst = "lsc1"
        } 
        else if(dif < 180){ cc = "0000ff" 
        hr = "http://enssnowcrab.com/R/tag/go/rarrow.png"
        lst = "lsc2"
        }
        else if(dif < 545){ 
          #cc = "003800" 
          cc = "00AA00" 
          #hr = "http://enssnowcrab.com/R/tag/go/dgarrow.png" 
        hr = "http://enssnowcrab.com/R/tag/go/garrow.png" 
        lst = "lsc3"
        }
        else if(dif < 1090){ cc = "ff0000" 
        hr = "http://enssnowcrab.com/R/tag/go/barrow.png"
        lst = "lsc4"
        }
        else if(dif < 1455){ cc = "00ffff" 
        hr = "http://enssnowcrab.com/R/tag/go/yarrow.png"
        lst = "lsc5"
        
        }
        else{ cc = "7f0055" 
        hr = "http://enssnowcrab.com/R/tag/go/parrow.png" 
        lst = "lsc6"
        }
        
        
        
        lon = unlist(str_split(fin_path[k,]$lon, ","))
        lat = unlist(str_split(fin_path[k,]$lat, ","))
        dist = "unknown"
        
        if(k==1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste("DISPLACEMENT: <br>   From release to 1st capture: ", signif(llen, digits=3), "km <br>", sep = "")
          if(grepl("No date", date))
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", date, sep = "")
          else
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          bearfro = (length(lon)-3) : length(lon)
          bearfro = bearfro[bearfro > 0]
          
          icon_heading = bearing(p1 = c(as.numeric(lon[bearfro[1]]),as.numeric(lat[bearfro[1]] )), p2 = c(as.numeric(lon[bearfro[length(bearfro)]]),as.numeric(lat[bearfro[length(bearfro)]] )))
          
          
        }
        if(k>1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste(disttxt, "<br>   To next capture:", signif(llen, digits=3), "km ") 
          if(grepl("No date", date))
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", date, sep = "")
          else
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          bearfro = (length(lon)-3) : length(lon)
          bearfro = bearfro[bearfro > 0]
          icon_heading = bearing(p1 = c(as.numeric(lon[bearfro[1]]),as.numeric(lat[bearfro[1]] )), p2 = c(as.numeric(lon[bearfro[length(bearfro)]]),as.numeric(lat[bearfro[length(bearfro)]] )))
          
        }
        
        
        
        pid = paste(fin_da$PID[k], k, sep = ".")
        
        line_color = rep(cc, length(lon))
        icon_href = rep(hr, length(lon))
        #line_href = rep(lst, length(lon))
        if(is.null(sub_out)){
          sub_out = data.frame(cbind(pid, lon, lat, line_color, line_width = 2))
        }else{
          sub_out = rbind(sub_out, cbind(pid, lon, lat, line_color, line_width = 2))
        }
        
        
        if(is.null(outarrow)){
          outarrow = data.frame(cbind(pid, da_y$sampyear[1], lon[length(lon)], lat[length(lat)], icon_heading, hr, .4))
        }else{
          outarrow = rbind(outarrow, cbind(pid, da_y$sampyear[1], lon[length(lon)], lat[length(lat)],icon_heading, hr, .4))
        }
        
      }
      
      description = sub("disttxt", disttxt, description)
      description = sub("datetxt", datetxt, description)
      sub_out$description = description
      sub_out$POS = 1:nrow(sub_out)
      
      if(is.null(out)){
        out = sub_out
      }else{
        out = rbind(out, sub_out)
      }
      
      
    }
    
    out$inFolder = da_y$sampyear[1]
    
    if(is.null(outframe)){
      outframe = out
    }else{
      outframe = rbind(outframe, out)
    }
    
    
    
    
  }
  outframe$pid = gsub("G", "9999", outframe$pid)
  mykml = RKmlObject()
  
  
  mykml$addIconStyle(styleid = "iconstyle1", color = "yellow", href = "http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png", scale = .4)
  
  
  mykml$addFolder(fid = "Releases", name = "Releases") #Demonstrate adding folder
  mykml$addPoint(samps, styleUrl = "iconstyle1")
  
  mykml$addLineStyle(styleid = "lsc1", color = "000000", width = 15)
  mykml$addLineStyle(styleid = "lsc2", color = "0000ff", width = 15)
  #mykml$addLineStyle(styleid = "lsc3", color = "003800", width = 8)
  mykml$addLineStyle(styleid = "lsc3", color = "00AA00", width = 15)
  mykml$addLineStyle(styleid = "lsc4", color = "ff0000", width = 15)
  mykml$addLineStyle(styleid = "lsc5", color = "00ffff", width = 15)
  mykml$addLineStyle(styleid = "lsc6", color = "7f0055", width = 15)
  
  names(outarrow) = c("pid", "inFolder", "lon", "lat", "icon_heading", "icon_href", "icon_scale")
  
  mykml$addPoint(outarrow)
  mykml$addLineString(outframe, altitudeMode = "relativeToGround")
  print(getwd())
  #mykml$addScreenOverlay(fn = "E:\\SCtagging\\data\\taglegend.png", size_x = .2, size_y = .2, screen_x = .8, screen_y = .8 )
  
  if(preview){
    mykml$preview()
  }
  if(write){
    if(is.null(tofile)){
      mykml$writekml(path = file.path(getwd(), "kml_outputs", "SCtagging_kml.kml"))
    }
    else{
      mykml$writekml(path = tofile)
      
    }
  }
  
}

#' @title  create.tag.geojson
#' @description  Write tag movement data to GeoJSON javascript functions. This is used by the enssnowcrab website to display tag movement data by year. The previous tag kml does not display properly in google maps so this method was implemented. It looks better than the old kml since geoJSON has built in arrow markers.
#' @param filename path to write the resulting kml
#' @import RMySQL chron sp geosphere stringr
#' @export
create.tag.geojson = function(filename = NULL){
  
  main = "function add..year..(map, arr, inf, ls){..markers..}" 
  markers = "arr[..arindex..] = new google.maps.Polyline({strokeColor: '#..color..', strokeWeight: 2, path: [..positions..], icons: [{ icon: ..ls.., offset: '100%'}], map: map}); arr[..arindex..].addListener('click', function(event) { inf.setContent('..description..'); inf.position = event.latLng; inf.open(map); inf.setPosition(event.latLng);});"
  positions = "{lat: ..lat.., lng: ..lon..}"
  
  
  # local_port = "3308"
  # SCtunnel = openportSC(local.port = local_port)
  # 
  # 
  # con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  # rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  # rs <- dbSendQuery(con, statement = "Select * from 
  #                   (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
  #                   from capture join bio where bio.tag_id = capture.tag) t1 
  #                   JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
  #                   from trip join sample where sample.trip = trip.trip_id)t2
  #                   ON t1.sample_num = t2.sample_id  
  #                   ORDER BY captain, trip_id, tag_id, t1.date;")
  # 
  # da <- fetch(rs, n = -1)   # extract all rows
  da = get.capturedata()

 # rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  #path <- fetch(rs, n = -1)   # extract all rows
  # closeportSC(SCtunnel)
  # dbDisconnect(con)
  
 # names(path) = c("id","lon", "lat", "cdat", "dist")
  
 # path = path[order(path$cdat),]
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  ddif = NULL
  ddate = NULL
  ind = which(as.character(da$caplat) == "0")
  da = da[-ind,]
  
  ind = which(as.character(da$year) == "")
  if(length(ind) > 0 )da = da[-ind,]
  ind = which(as.character(da$area) == "GULF"  & as.numeric(as.character(da$sampyear)) <= 2014 )
  #ind = which(as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF")
  
  da = da[-ind,]
  dup = 0
  

  
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  mattxt = "unknown unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  da = da[order(da$capdat),]
  outframe = NULL
  outarrow = NULL
  jsfinal = ""
  dx = split(da, da$sampyear)
  for(i in 1:length(dx)){
    markernum = 0
    ymain = main
    out = NULL
    da_y = dx[[i]]
    
    ymain = sub("..year..", da_y$sampyear[1], ymain)
    
    da_tid = split(da_y, as.character(da_y$PID))
    allmarkers = ""
    for(j in 1:length(da_tid)){
      fin_da = da_tid[[j]]
      
      sampchron = chron(as.character(fin_da$sampdat[1]), format = "y-m-d")
      
     # fin_path = path[which(path$id == fin_da$PID[1]),]
      fin_path = get.pathdata.tid(region = "ScotianShelf", tid = fin_da$PID[1])
      description2 = "<b> datetxt  <br><br> disttxt</b>"
      disttxt = ""
      datetxt = ""
      tid = ""
      markerblock = ""
      
      
      for(k in 1:length(unique(fin_path$CID))){

        
        capchron = chron(as.character(fin_da$capdate[k]), format = "y-m-d")
        
        date = as.character(fin_da$capdate[k])
        
        if(is.na(capchron)){
          date = paste("No date returned, received in ",fin_da$year[k])
          if( as.character(fin_da$caparea[k]) == "NENS") capchron = chron(paste( as.character(fin_da$year[k]),"-07-15", sep=""), format = "y-m-d")
          if( as.character(fin_da$caparea[k]) == "SENS") capchron = chron(paste( as.character(fin_da$year[k]),"-08-15", sep=""), format = "y-m-d")
          if( as.character(fin_da$caparea[k]) == "GULF") capchron = chron(paste( as.character(fin_da$year[k]),"-08-15", sep=""), format = "y-m-d")
          if( as.character(fin_da$caparea[k]) == "4X") capchron = chron(paste( as.character(fin_da$year[k]),"-03-15", sep=""), format = "y-m-d")
          
        }
        dif = capchron - sampchron
        dif = as.numeric(dif)
        arrls = "ls[0]"
        cc2 = ""
        if(is.na(dif)){ arrls = "ls[0]"
        cc2 = "000000" 
        } 
        else if(dif < 180){ arrls = "ls[1]"
        cc2 = "ff0000"
        }
        else if(dif < 545){ arrls = "ls[2]"
        cc2 = "00aa00"
        }
        else if(dif < 1090){ arrls = "ls[3]"
        cc2 = "0000ff"
        }
        else if(dif < 1455){ arrls = "ls[4]"
        cc2 = "ffff00"
        }
        else{ 
          arrls = "ls[5]"
          cc2 = "55007f"
        }
        
        
        
        #lon = unlist(str_split(fin_path[k,]$lon, ","))
        #lat = unlist(str_split(fin_path[k,]$lat, ","))
        lon = fin_path$LON[which(fin_path$CID == k)]
        lat = fin_path$LAT[which(fin_path$CID == k)]
        
        if(k==1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste("DISPLACEMENT: <br>   From release to 1st capture: ", signif(llen, digits=3), "km <br>", sep = "")
          if(grepl("No date", date))
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", date, sep = "")
          else
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          
          
        }
        if(k>1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste(disttxt, "<br>   To next capture:", signif(llen, digits=3), "km ") 
          if(grepl("No date", date))
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", date, sep = "")
          else
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          
        }
        
        ymarker = markers
        tespos = paste(paste("{lat: ", lat, ",lng: ", lon, "}", sep = ""), collapse = ",")
        ymarker = sub("..positions..", tespos, ymarker)
        ymarker = sub("..color..", cc2, ymarker)     
        ymarker = gsub("..arindex..", markernum, ymarker)
        ymarker = sub("..ls..", arrls, ymarker)
        markernum = markernum + 1
        markerblock = paste(markerblock, ymarker, sep=" 
                            ")
        
        pid = paste(fin_da$PID[k], k, sep = ".")
        
        
        
        
      }
      
      description2 = sub("disttxt", disttxt, description2)
      description2 = sub("datetxt", datetxt, description2)
      markerblock = gsub("..description..", description2, markerblock)
      
      
      
      allmarkers = paste(allmarkers, markerblock, sep = "
                         ")
      
    }
    ymain = sub("..markers..", allmarkers, ymain)
    jsfinal = paste(jsfinal, ymain, sep = "
                    ")
    
    
    
  }
  if(is.null(filename)){
    sink("jsfinal.js")
    cat(jsfinal)
    sink()
  }
  else{
    sink(filename)
    cat(jsfinal)
    sink()  
  }
  
  }

tags.poly.intersect.plot = function(){
  
  da = get.capturedata()
  
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  
  ind = which(as.character(da$caplat) == "0.0")
  if(length(ind)>0) da = da[-ind,]
  ind = which(as.character(da$caplat) == "0")
  if(length(ind)>0) da = da[-ind,]
  
  ii = is.in(are, da$samplon, da$samplat)
  jj = is.in(are, da$caplon, da$caplat)
  ind = which(!(ii | jj))
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  #REMOVE GULF ENTRIES
  ind = which( as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF"  )
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  library(stringr)
  yea = years
  syear = da$sampyear
  if(years != "all"){
    years = strsplit(years, ",")
    years = unlist(years)
    
    syear = match(syear, years)
    
    indic = which(is.na(syear) == TRUE)
    
    if(length(indic)>0) 
      da = da[-indic,]
    
  }
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  
  
  
  dup = NULL
  for (i in 1:nrow(da)) {
    
    if(i > 1){
      if(da$PID[i] != da$PID[i-1]){
        dup = 1
        
        X = c(X, da$samplon[i])
        Y = c(Y, da$samplat[i])
        POS = c(POS, dup[1])
        PID = c(PID, da$PID[i])
        colour = c(colour, "white")
      }
      sampchron = chron(da$sampdat[i], format = "y-m-d")
      capchron = chron(da$capdat[i], format = "y-m-d")
      
      if(is.na(capchron)){
        if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
        
      }
      dif = capchron - sampchron
      dif = as.numeric(dif)
      
      cc = ""
      if(is.na(dif)){ cc = "black" } 
      else if(dif < 180){ cc = "red" }
      else if(dif < 545){ cc = "darkgreen" }
      else if(dif < 1090){ cc = "blue" }
      else if(dif < 1455){ cc = "darkgoldenrod2" }
      else{ cc = "purple"}
      dup = dup+1
      
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      POS = c(POS, dup[1])
      PID = c(PID, da$PID[i])
      colour = c(colour, cc)
    }
    else{
      X = c(X,da$samplon[i])
      Y = c(Y,da$samplat[i])
      POS = c(POS, 1)
      PID = c(PID,da$PID[i])
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      POS = c(POS, 2)
      PID = c(PID, da$PID[i])
      colour = c(colour, "white")
      
      sampchron = chron(da$sampdat[i], format = "y-m-d")
      capchron = chron(da$capdat[i], format = "y-m-d")
      
      if(is.na(capchron)){
        if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
        
      }
      
      dif = capchron - sampchron
      dif = as.numeric(dif)
      
      cc = ""
      if(is.na(dif)){ cc = "black" } 
      else if(dif < 180){ cc = "red" }
      else if(dif < 545){ cc = "darkgreen" }
      else if(dif < 1090){ cc = "blue" }
      else if(dif < 1455){ cc = "darkgoldenrod2" }
      else{ cc = "purple"}
      colour = c(colour, cc)
      dup = 2
    }
  }
  co = c("black", "red", "darkgreen", "blue", "darkgoldenrod2", "purple")
  na = c("unknown" ,"same season", "1 season", "2 seasons", "3 seasons", "4+ seasons")
  cocode = cbind(co, na) 
  cocode = data.frame(cocode)
  dbDisconnect(con)
  
  x = cbind(PID, POS, X, Y, colour)
  x = data.frame(x)
  x$PID = as.character(x$PID)
  x$POS = trunc(as.numeric(as.character(x$POS)))
  x$X = as.numeric(as.character(x$X))
  x$Y = as.numeric(as.character(x$Y))
  
  
  icoco = match(as.character(cocode$co), as.character(unique(x$colour)))
  icoco = which(is.na(icoco) == TRUE)
  
  cocode = cocode[-icoco,]
  x$PID = as.numeric(x$PID)
  
  x$z = colour
  x = as.PolySet(data.frame(x), projection = "LL")
  
  dir = file.path("C:", "project","mapping", "maps", "Emera Line", "ENL_SubseaCable_2km_StudyArea.shp"  )
  el= importShapefile(dir)
  
  
  sp1 = PolySet2SpatialPolygons(el, close_polys=TRUE)
  sp2 = PolySet2SpatialLines(x)
  
  tlis = gCrosses(sp2, sp1, byid = T)
  
  sp2 = sp2[which(tlis == T),]
  
  x = SpatialLines2PolySet(sp2)
  
  xlim = c(min(x$X)-.2, max(x$X)+.2)
  ylim = c(min(x$Y)-.2, max(x$Y)+.2)
  plotRaster( file.path(direct,"mapping", "maps","Charts", "801_LL_WGS84_PCT.tif"),
              xlab="Longitude", main = paste("Snow Crab (Spaghetti) Emera Line Crossings | Year(s):", yea, sep=" "), ylab="Latitude", outer = T, axes=T, tck=0,
              fade = .5, cex.main = .5, tckLab=F, xlim = xlim, ylim = ylim, quality = quality, cellcount = NULL)
  
  
  addlinesSCAREA(lwd = 1)
  addMPA()
  
  addDivisions(xlim, ylim)
  
  #addLines(x, col = "blue", cex = .5, arrows = T, length = .05)
  
  addPolys(el, col = rgb(.8, 0, 0, .3))
  #x = x[which(x$PID %in% pids),]
  
  
  
  
  for (i in 1:length(sp2)) {
    chunk = as.data.frame(unlist(coordinates(sp2[i]), recursive = F)[1])
    
    names(chunk) = c("X", "Y")
    
    dd = c("blue")
    
    if(!shortpath){
      arrows(x0 = chunk$X[1:length(chunk$X)-1], y0 =  chunk$Y[1:length(chunk$Y)-1], x1 = chunk$X[2:length(chunk$X)] , y1 = chunk$Y[2:length(chunk$Y)], col = dd, angle= 20, code=2, length = 0.06)
    }
    else{
      len = shortest(chunk$X, chunk$Y, dd)
      
      
    }
    
  }
}

recaps.plot = function(){
  
  da = get.capturedata()
  
  ##REMOVE YEARS
  da = da[which(da$sampyear %in%  as.character(years) ),]
  da$sample_num = NULL
  da$sample_id = NULL
  da$trip = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  
  mat = paste("(",are,")", sep = "")
  ind = which(!grepl(mat, da$subarea, fixed = TRUE))
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
  if(length(ind)>0) 
    da = da[-ind,]
  
  da$recap = 1 
  previd = ""
  # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    
    if(da$PID[i] == previd){
      da$recap[i] = da$recap[i-1] + 1
    }
    previd = da$PID[i] 
  }
  z = NULL
  z$ret = nrow(da)
  z$retuni = length(unique(da$PID))
  
  fr = da[which(da$area == "NENS" & da$caparea == "SENS"),]
  
}


#' @title  get.paths
#' @description  Return path data
#' @import ROracle RMySQL
#' @return dataframe
#' @export
get.paths = function(){
  
  gstring = ""
  local_port = "3309"
  
  SCtunnel = openportSC(local.port = local_port)
  
  con <- RMySQL::dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- RMySQL::dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rx <- RMySQL::dbSendQuery(con, statement = "Select * from paths;")
  
  dx <- RMySQL::fetch(rx, n = -1)   # extract all rows
  RMySQL::dbDisconnect(con)
  closeportSC(SCtunnel)
  # 
  #dx = dbReadTable(con, "SCT_PATHS", ora.number = T)
  # drv <- dbDriver("Oracle")
  # con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  # 
  # 
  # respat <- ROracle::dbSendQuery(con, paste("select * from SCT_PATHS", gstring, " where ID = '",tid ,"'", sep = "")) 
  # respat <- fetch(respat)
  # ROracle::dbDisconnect(con)
  
  return(dx)
}

#' @title  openportSC
#' @description  Open a ssh tunnel for ENS site satabase opperations
#' @import rJava
#' @param user Database username
#' @param password Database password
#' @param host database server
#' @param local.port what port to tunnel through
#' @param remote.port what port to tunnel through
#' @export
openportSC = function(user = enssnowc.user, password = enssnowc.password, host = "enssnowcrab.com", local.port = NULL, remote.port = "3306" ){
  
  if(is.null(local.port)) local.port = "3308"
  
  tunnel = NULL
  .jinit()
  
  .jaddClassPath(dir(system.file("extdata", "ssh", package = "SCtagging"), full.names = T))
  .jclassPath()
  
  tunnel <- .jnew("ssh/Sshtunnel")
  .jcall(tunnel,, "setvariables" , user, password, host, local.port, remote.port )
  .jcall(tunnel,, "openTunnel")
  return(tunnel)
} 
#' @title  closeportSC
#' @description  close a named ssh tunnel for ENS site database opperations
#' @import rJava 
#' @export
closeportSC = function(tunnel){
  .jcall(tunnel,, "closeTunnel")
}
#' @title  gettableSQL
#' @description  Get site table 
#' @import ROracle RMySQL DBI rJava
#' @return dataframe
#' @export
gettableSQL = function(table){
  local_port = "3308"
  
  SCtunnel = openportSC(local.port = local_port)
  
  con <- RMySQL::dbConnect(RMySQL::MySQL(), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- RMySQL::dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- RMySQL::dbSendQuery(con, statement = paste("Select * from ",table, sep=""))
  da <- RMySQL::fetch(rs, n = -1)   # extract all rows
  RMySQL::dbDisconnect(con) 
  closeportSC(SCtunnel)
  return(da)
}

#' @title  get.releases
#' @description  Return capture data
#' @import RODBC
#' @return dataframe
#' @export
get.releases = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  con = RODBC::odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  da = NULL
  
  
  query = paste("SELECT SCT_BIO", gstring,".TAG_ID,
                SCT_BIO", gstring,".SAMPLE_NUM,
                SCT_BIO", gstring,".CC,
              SCT_BIO", gstring,".CARAPACE_W,
              SCT_BIO", gstring,".CHELA_H,  
       SCT_BIO", gstring,".DUROMETER, 
              SCT_TRIP", gstring,".TRIP_ID,
                SCT_TRIP", gstring,".STATSAREA AS STATSAREA1,
                SCT_TRIP", gstring,".YEAR      AS YEAR1,
                SCT_TRIP", gstring,".RELEASE_DATE,
                SCT_SAMPLE", gstring,".LAT_DD_DDDD              AS LAT_DD_DDDD1,
                SCT_SAMPLE", gstring,".LONG_DD_DDDD             AS LONG_DD_DDDD1,
                SCT_SAMPLE", gstring,".SAMPLE_ID
                FROM SCT_BIO", gstring,"
                JOIN SCT_SAMPLE", gstring,"
                ON SCT_BIO", gstring,".SAMPLE_NUM = SCT_SAMPLE", gstring,".SAMPLE_ID
                INNER JOIN SCT_TRIP", gstring,"
                ON SCT_SAMPLE", gstring,".TRIP = SCT_TRIP", gstring,".TRIP_ID
                ORDER BY SCT_TRIP", gstring,".TRIP_ID,
                SCT_BIO", gstring,".TAG_ID,
                SCT_TRIP", gstring,".RELEASE_DATE;", sep = "")
  
  # resbio <- ROracle::dbSendQuery(con, query) 
  # da <- fetch(resbio)
  da = RODBC::sqlQuery(con, query )
  # ROracle::dbDisconnect(con)
  RODBC::odbcClose(con)
  # da <- RODBC::fetch(rs, n = -1)   # extract all rows
  #RODBC::dbDisconnect(con) 
  # closeportSC(SCtunnel)
  da = unique(da)
  da$SAMPLE_NUM = NULL
  da$TRIP_ID = NULL
  da$SAMPLE_ID = NULL
  
  names(da) = c("PID", "cc", "cw", "ch", "dur", "area", "sampyear", "sampdat", "rellat", "rellon")
 
  return(da)
}
#' @title  get.capturedata
#' @description  Return capture data
#' @import RODBC
#' @return dataframe
#' @export
get.capturedata = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  # local_port = "3308"
  # 
  # SCtunnel = openportSC(local.port = local_port)
  # 
  # con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  # rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  
  
  # rs <- dbSendQuery(con, statement = "Select * from 
  #                     (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
  #                     from capture join bio where bio.tag_id  = capture.tag) t1 
  #                     JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
  #                     from trip join sample where sample.trip = trip.trip_id)t2
  #                     ON t1.sample_num = t2.sample_id  
  #                     ORDER BY captain, trip_id, tag_id, t1.date;")
   #drv <- DBI::dbDriver("Oracle")
 #  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)

  
  con = RODBC::odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  da = NULL
  
  
  query = paste("SELECT SCT_BIO", gstring,".TAG_ID,
                SCT_BIO", gstring,".SAMPLE_NUM,
                SCT_CAPTURE", gstring,".CAPTURE_DATE,
                SCT_CAPTURE", gstring,".STATSAREA,
                SCT_CAPTURE", gstring,".LAT_DD_DDDD,
                SCT_CAPTURE", gstring,".LONG_DD_DDDD,
                SCT_CAPTURE", gstring,".YEAR,
                SCT_TRIP", gstring,".TRIP_ID,
                SCT_TRIP", gstring,".STATSAREA AS STATSAREA1,
                SCT_TRIP", gstring,".YEAR      AS YEAR1,
                SCT_TRIP", gstring,".CAPTAIN,
                SCT_TRIP", gstring,".REPORTED,
                SCT_TRIP", gstring,".RELEASE_DATE,
                SCT_SAMPLE", gstring,".LAT_DD_DDDD              AS LAT_DD_DDDD1,
                SCT_SAMPLE", gstring,".LONG_DD_DDDD             AS LONG_DD_DDDD1,
                SCT_SAMPLE", gstring,".SAMPLE_ID
                FROM SCT_CAPTURE", gstring,"
                INNER JOIN SCT_BIO", gstring,"
                ON SCT_BIO", gstring,".TAG_ID = SCT_CAPTURE", gstring,".TAG
                INNER JOIN SCT_SAMPLE", gstring,"
                ON SCT_BIO", gstring,".SAMPLE_NUM = SCT_SAMPLE", gstring,".SAMPLE_ID
                INNER JOIN SCT_TRIP", gstring,"
                ON SCT_SAMPLE", gstring,".TRIP = SCT_TRIP", gstring,".TRIP_ID
                ORDER BY SCT_TRIP", gstring,".CAPTAIN,
                SCT_TRIP", gstring,".TRIP_ID,
                SCT_BIO", gstring,".TAG_ID,
                SCT_CAPTURE", gstring,".CAPTURE_DATE;", sep = "")
  
  # resbio <- ROracle::dbSendQuery(con, query) 
  # da <- fetch(resbio)
  da = RODBC::sqlQuery(con, query )
  # ROracle::dbDisconnect(con)
  RODBC::odbcClose(con)
# da <- RODBC::fetch(rs, n = -1)   # extract all rows
 #RODBC::dbDisconnect(con) 
  # closeportSC(SCtunnel)
  da = unique(da)
  da$SAMPLE_NUM = NULL
  da$TRIP_ID = NULL
  da$CAPTAIN = NULL
  da$REPORTED = NULL
  da$SAMPLE_ID = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  previd = ""
  # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$samplat[i] = da$caplat[i-1]
      da$samplon[i] = da$caplon[i-1]
      da$sampdat[i] = da$capdat[i-1]
    }
    previd = da$PID[i] 
  }
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "rellat", "rellon")
  
  
  
  return(da)
  
}

#' @title  get.pathdata.tid
#' @description  Return calculated paths by supplied TID
#' @import RODBC
#' @return dataframe
#' @export
get.pathdata.tid = function(region = "ScotianShelf", tid = ""){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  con = RODBC::odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  da = NULL
  
  
  query = paste("SELECT * FROM SCT_PATHS", gstring, " where SCT_PATHS", gstring,".TID = '", tid, "';", sep = "")
  
  da = RODBC::sqlQuery(con, query )
  da = da[order(da$CID, da$POS),]
  RODBC::odbcClose(con)
 
  
  return(da)
  
}

#' @title  get.path
#' @description  Return path data
#' @import ROracle
#' @return dataframe
#' @export
get.path = function(region = "ScotianShelf"){
  gstring = ""
  if(region == "Gulf"){
    gstring = "_GULF"
  }
  
  
  drv <- dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.snowcrab.user, password = oracle.snowcrab.password, dbname = oracle.snowcrab.server)
  
  
  respat <- ROracle::dbSendQuery(con, paste("select * from SCT_PATH", gstring, sep = ""))
  respat <- fetch(respat)
  ROracle::dbDisconnect(con)
  
  return(respat)
}

#' @title  data2Poly
#' @description  Create polydata
#' @import PBSmapping
#' @return dataframe
#' @export
data2Poly = function(da){
  out = NULL
  X = NULL
  Y = NULL
  D = NULL
  POS = NULL
  PID = NULL
  dup = NULL
  K = NULL
  for (i in 1:nrow(da)) {
    sampchron = as.character(da$sampdat[i])
    capchron = as.character(da$capdat[i])
    km = da$km[i]
    
    if(i > 1){
      
      if(da$PID[i] != da$PID[i-1]){
        dup = 1
        
        X = c(X, da$samplon[i])
        Y = c(Y, da$samplat[i])
        D = c(D, sampchron)
        POS = c(POS, dup[1])
        PID = c(PID, da$PID[i])
          K = c(K, 0)
      }
      
      dup = dup+1
      
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      D = c(D, capchron)
      POS = c(POS, dup[1])
      PID = c(PID, da$PID[i])
        K = c(K, da$km[i])
      
    }
    else{
      X = c(X,da$samplon[i])
      Y = c(Y,da$samplat[i])
      D = c(D, sampchron)
      POS = c(POS, 1)
      PID = c(PID,da$PID[i])
          K = c(K, 0)
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      D = c(D, capchron)
      POS = c(POS, 2)
      PID = c(PID, da$PID[i])
         K = c(K, da$km[i])
      dup = 2
      
    }
  }
  
  out = cbind(PID, POS, X, Y, D, K)
  out = data.frame(out)
  
  out$PID = as.character(out$PID)
  out$PID = as.numeric(gsub("G", "111111", out$PID))
  out$POS = trunc(as.numeric(as.character(out$POS)))
  out$X = as.numeric(as.character(out$X))
  out$Y = as.numeric(as.character(out$Y))
  out$K = as.numeric(as.character(out$K))
  #out = head(out,480)
  return(as.PolySet(out, projection = "LL"))
}
is.in = function(are2, lon, lat){
  lon = as.numeric(as.character(lon))
  lat = as.numeric(as.character(lat))
  fn =system.file("extdata", "areas", "areaborders.csv", package = "SCtagging")
  borders= read.csv(file=fn, head=T, sep=",")
  
  b=borders[which(borders$area==are2),]
  
  yli=c(b$slat,b$nlat)
  xli=c(-(b$wlon),-(b$elon))
  
  len = length(lon)
  ew = c(1:len)
  sn = c(1:len)
  fin = c(1:len)
  ew[1:len] = FALSE
  sn[1:len] = FALSE
  fin[1:len] = FALSE
  
  ew[(lon>xli[1]) & (lon<xli[2])] = TRUE
  
  sn[(lat>yli[1]) & (lat<yli[2])] = TRUE
  
  fin[(ew == TRUE) & (sn == TRUE)] = TRUE
  
  
  return(fin)
}


#' @title  absolutely.in.area
#' @description  Function that determins if a position is inside defined polygons 
#' @param area The area name exa. 'cfa23' 'cfa24' 'nens' 'sens' 'gulf' 'cfa4x'
#' @param lon The longitude of position in question
#' @param lat The latitude of position in question
#' @import sp rgeos
#' @return TRUE or FALSE
#' @export
absolutely.in.area = function(area, abslon, abslat){
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  
  
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  
  
  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  
  
  pb = SpatialPoints(rbind(c(as.numeric(abslon), as.numeric(abslat))))
  
  
  
  # all.holes,44,45.5,61.4,58
  bo = FALSE
  if(area == "cfa23"){
    if(gContains(twothree, pb)) bo = TRUE
  }
  
  if(area == "cfa23zoom"){
    if(gContains(twothreez, pb)) bo = TRUE
  }
  if(area == "cfa24"){
    if(gContains(twofour, pb)) bo = TRUE
  }
  if(area == "cfa24zoom"){
    if(gContains(twofourz, pb)) bo = TRUE
  }
  if(area == "cfa4x"){
    if(gContains(xxxx, pb)) bo = TRUE
  }
  if(area == "nens"){
    if(gContains(nens, pb)) bo = TRUE
  }  
  if(area == "nens_gulf"){
    if(gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }  
  
  
  if(area == "sens"){
    if(gContains(twofour, pb) | gContains(twothree, pb)) bo = TRUE
  } 
  if(area == "gulf"){
    if(gContains(gulf, pb)) bo = TRUE
  } 
  if(area == "all" | area == "ens"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb)) bo = TRUE
  }
  if(area == "allandgulf"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }
  if(area == "all.holes"){
    if(gContains(holes, pb)) bo = TRUE
  }  
  
  
  return(bo)
  
}
#' @title  absolutely.in.area2
#' @description  Function that determines if a list of positions are inside defined polygons 
#' @param area The area name exa. 'cfa23' 'cfa24' 'nens' 'sens' 'gulf' 'cfa4x'
#' @param lon The longitude list
#' @param lat The latitude list
#' @import sp rgeos
#' @return list of TRUE or FALSE values
#' @export
absolutely.in.area2 = function(area, abslon, abslat){
  
  are = rep("unk", length(abslat))
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  
  
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  
  
  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  bol = NULL
  for(i in 1:length(abslon)){
  pb = SpatialPoints(rbind(c(as.numeric(abslon[i]), as.numeric(abslat[i]))))
  
  
  
  # all.holes,44,45.5,61.4,58
  bo = FALSE
  if(area == "cfa23"){
    if(gContains(twothree, pb)) bo = TRUE
  }
  
  if(area == "cfa23zoom"){
    if(gContains(twothreez, pb)) bo = TRUE
  }
  if(area == "cfa24"){
    if(gContains(twofour, pb)) bo = TRUE
  }
  if(area == "cfa24zoom"){
    if(gContains(twofourz, pb)) bo = TRUE
  }
  if(area == "cfa4x" | area == "unknown"){
    if(gContains(xxxx, pb)){
      bo = TRUE
      are[i] = "CFA-4X"
      
      }
  }
  if(area == "nens" | area == "unknown"){
    
    if(gContains(nens, pb)){
      bo = TRUE
      are[i] = "NENS"
      }
  }  
  if(area == "nens_gulf"){
    if(gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }  
  
  
  if(area == "sens" | area == "unknown"){
    
    if(gContains(twofour, pb) | gContains(twothree, pb)){
      are[i] = "SENS"
       bo = TRUE
    }
  } 
  if(area == "gulf" | area == "unknown"){
    
    if(gContains(gulf, pb)){
      bo = TRUE
      are[i] = "GULF" 
    }
  } 
  if(area == "all" | area == "ens"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb)) bo = TRUE
  }
  if(area == "allandgulf"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }
  if(area == "all.holes"){
    if(gContains(holes, pb)) bo = TRUE
  }  
  bol = c(bol, bo)

  
  
    }
  
  if(area == "unknown"){
    return(are)
  }
  return(bol)
  
}


