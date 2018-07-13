require(jsonlite)
args <- commandArgs(TRUE)

write("F", file = "R/tag/errfile.txt",
      ncolumns = 1,
      append = FALSE, sep = " ")

rec <- args[1]
rec2 <- args[2]
print(rec)
print(rec2)
#rec = substr(rec ,2, nchar(rec)-1)

rec = unlist(strsplit(rec, "xxxx"))
rec2 = unlist(strsplit(rec2, "xxxx"))

tnum <- rec[1]
dat <- rec[2]
lat <- rec[3]
lon <- rec[4]
dep <- rec[5]
ret <- rec[6]
ves <- rec[7]
db <- rec[8]
per <- rec2[1]

add <- rec2[2]
loc <- rec2[3]
pro <- rec2[4]
cou <- rec2[5]
poc <- rec2[6]
ema <- rec2[7]
com <- rec2[8]
pho1 <- rec2[9]
pho2 <- rec2[10]

print(lat)
lf <- as.numeric(unlist(strsplit(lat, "\302\260"))[1])
lf2 <- as.numeric(sub("N", "", unlist(strsplit(lat, "\302\260"))[2]))/60
lf <- lf + lf2

lof <- as.numeric(unlist(strsplit(lon, "\302\260"))[1])
lof2 <- as.numeric(sub("W", "", unlist(strsplit(lon, "\302\260"))[2]))/60
lof <- (lof + lof2)*-1


print(tnum)
print("*")
print(dat)
print("*")
print(lat)
print("*")
print(lon)
print("*")
print(lf)
print("*")
print(lof)
print("*")
print(dep)
print("*")
print(ret)
print("*")
print(ves)
print("*")
print(per)
print("*")
print(add)
print("*")
print(loc)
print("*")
print(cou)
print("*")
print(pro)
print("*")
print(cou)
print("*")
print(poc)
print("*")
print(ema)
print("*")
print(com)

#per <- gsub("'","''",per)

toda1 = paste('insert into capture (tag, lat_DDMM_MM, long_DDMM_MM, lat_DD_DDDD, long_DD_DDDD, date, person, fathoms, tagcode, vessel, captain, comment) values ("', tnum,'","', lat,'","', lon,'","', lf,'","', lof,'","', dat,'","', per,'","', dep,'","', ret,'","', ves,'","', per,'","', com, '");', sep = "")
 
 print(toda1)
 
que1 = paste('select count(name) num from people where name ="', per,'"', sep = "")
print(que1)
toda2 = paste('insert into people (name, civic, town, prov, post, email, pho1, pho2, country) values ("', per,'","', add,'","', loc,'","', pro,'","', poc,'","', ema,'","', pho1,'","', pho2, '","', cou, '");', sep = "")
print(toda2)
toda4 = paste('update people set name = "', per,'", civic = "', add,'", town = "', loc,'", prov = "', pro,'", post = "', poc,'", email = "', ema,'", pho1 = "', pho1,'", pho2 = "', pho2,'", country = "', cou,'" where name = "', per, '";', sep = "")
print(toda4)
source("/home3/enssnowc/public_html/gulf/R/tag/src/tagreturnchart.r")

if(plottag(toda1, toda2, que1, toda4, lat, lon, tnum, per, dat, db))
   write("T", file = "R/tag/errfile.txt",
      ncolumns = 1,
      append = FALSE, sep = " ")
