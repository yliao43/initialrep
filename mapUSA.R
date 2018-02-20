#  In class exercise....
#  Load all the bridges data. 
#    identify bridge quality and make a map of all US counties where you color each county 
#    by the number of failing or bad bridges.  
#  The code below does something like this for *just* Wisconsin.




# install.packages("ggplot2")
# # install.packages("plyr")
# install.packages("choroplethr")
# install.packages("choroplethrMaps")
# # install.packages("dplyr")
# install.packages("dtplyr")
# 
library(plyr)
library(choroplethr)
library(choroplethrMaps)
library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
# library(dtplyr)

# I like downloading straight from url's.  
# If I was planning to share this code in a long time, I would worry about stability of the accessibility (url/data/format) and maybe backup locally.
# For class, downloading is straightforward.

# this gets all the data.  
# dat <- fread("curl https://www.fhwa.dot.gov/bridge/nbi/2017hwybronlyonefile.zip | funzip") %>% as.tbl

# # sometimes, you can't get all the data in one go... but you know that the url's have a predictable pattern.  
# #  for example, here is AK's (i.e. alaska's) data for 2016...
# dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
# tmp = fread(dest) 
# tmp = as.tbl(tmp) # remember to convert to tibble after fread.  otherwise, you have a data.table and get super strange errors.

# # if we wanted to download each state separately... need to get the state abbriviations...
# states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/479/data/stateAbv.txt")
# states=states[-(1:12),]
# states[51,] = c("WashDC", "DC")
# states[52,] = c("Puerto Rico", "PR")
# dat=list()


# then do the loop!
# for(i in 2:51){
#   dest=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
#   dat[[i]] = fread(dest, colClasses = classes ) %>% as.tbl  # for loop to define data could be really bad.  Does list make it ok?  idk.
# }
# lapply(dat, dim)
# colnames(dat[[1]])



# #  alternatively, there is a fancier way...
# # first, imagine that the files are stored locally...
# # see code chunk from ftp://cran.r-project.org/pub/R/web/packages/tidyr/vignettes/tidy-data.html
# library(plyr)
# paths <- dir("fluna_991992-20160919110712", pattern = "\\.txt$", full.names = TRUE)
# names(paths) <- basename(paths)
# ldply(paths, read.csv, stringsAsFactors = FALSE)
# # same idea can be used for web data...
# dest= rep("", 52)
# for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
# x16 = ldply(dest, fread, colClasses = classes)  

data = fread('2017HwyBridgesDelimitedAllStates.txt')
# let's dig into the values and see if there are any crazy things going on...
M = data %>% as.tbl
M = M[,-14]
is.na(M) %>% rowSums %>% hist
is.na(M) %>% colSums %>% hist(breaks = 100)
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
str(M)
M = M[,-as.vector(bad)]
str(M)
jold =1
for(j in jold:ncol(M)){
  nc = nchar(M[,j], keepNA = T)
  print(j)
  print(summary(nc))
  print(sum(is.na(nc)))
  print("")
}
colnames(M)[j]
M = M[,-j]
jold = j

colnames(M)
# [1] "STATE_CODE_001"          "STRUCTURE_NUMBER_008"    "RECORD_TYPE_005A"        "ROUTE_PREFIX_005B"       "SERVICE_LEVEL_005C"      "ROUTE_NUMBER_005D"       "DIRECTION_005E"          "HIGHWAY_DISTRICT_002"   
# [9] "COUNTY_CODE_003"         "PLACE_CODE_004"          "FEATURES_DESC_006A"      "FACILITY_CARRIED_007"    "MIN_VERT_CLR_010"        "KILOPOINT_011"           "LRS_INV_ROUTE_013A"      "LAT_016"                
# [17] "LONG_017"                "DETOUR_KILOS_019"        "TOLL_020"                "MAINTENANCE_021"         "OWNER_022"               "FUNCTIONAL_CLASS_026"    "YEAR_BUILT_027"          "TRAFFIC_LANES_ON_028A"  
# [25] "TRAFFIC_LANES_UND_028B"  "ADT_029"                 "YEAR_ADT_030"            "DESIGN_LOAD_031"         "APPR_WIDTH_MT_032"       "MEDIAN_CODE_033"         "DEGREES_SKEW_034"        "STRUCTURE_FLARED_035"   
# [33] "RAILINGS_036A"           "TRANSITIONS_036B"        "APPR_RAIL_036C"          "APPR_RAIL_END_036D"      "HISTORY_037"             "NAVIGATION_038"          "NAV_VERT_CLR_MT_039"     "NAV_HORR_CLR_MT_040"    
# [41] "OPEN_CLOSED_POSTED_041"  "SERVICE_ON_042A"         "SERVICE_UND_042B"        "STRUCTURE_KIND_043A"     "STRUCTURE_TYPE_043B"     "APPR_KIND_044A"          "APPR_TYPE_044B"          "MAIN_UNIT_SPANS_045"    
# [49] "APPR_SPANS_046"          "HORR_CLR_MT_047"         "MAX_SPAN_LEN_MT_048"     "STRUCTURE_LEN_MT_049"    "LEFT_CURB_MT_050A"       "RIGHT_CURB_MT_050B"      "ROADWAY_WIDTH_MT_051"    "DECK_WIDTH_MT_052"      
# [57] "VERT_CLR_OVER_MT_053"    "VERT_CLR_UND_REF_054A"   "VERT_CLR_UND_054B"       "LAT_UND_REF_055A"        "LAT_UND_MT_055B"         "LEFT_LAT_UND_MT_056"     "DECK_COND_058"           "SUPERSTRUCTURE_COND_059"
# [65] "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"        "OPR_RATING_METH_063"     "INV_RATING_METH_065"     "STRUCTURAL_EVAL_067"     "DECK_GEOMETRY_EVAL_068"  "UNDCLRENCE_EVAL_069"    
# [73] "POSTING_EVAL_070"        "WATERWAY_EVAL_071"       "APPR_ROAD_EVAL_072"      "DATE_OF_INSPECT_090"     "FRACTURE_092A"           "UNDWATER_LOOK_SEE_092B"  "SPEC_INSPECT_092C"       "STRAHNET_HIGHWAY_100"   
# [81] "PARALLEL_STRUCTURE_101"  "TRAFFIC_DIRECTION_102"   "HIGHWAY_SYSTEM_104"      "FEDERAL_LANDS_105"       "DECK_STRUCTURE_TYPE_107" "SURFACE_TYPE_108A"       "MEMBRANE_TYPE_108B"      "DECK_PROTECTION_108C"   
# [89] "NATIONAL_NETWORK_110"    "BRIDGE_LEN_IND_112"      "SCOUR_CRITICAL_113"      "FUTURE_ADT_114"          "YEAR_OF_FUTURE_ADT_115"  "FED_AGENCY"              "DATE_LAST_UPDATE"        "TYPE_LAST_UPDATE"       
# [97] "DEDUCT_CODE"             "PROGRAM_CODE"            "PROJ_NO"                 "STATUS_WITH_10YR_RULE"   "SUFFICIENCY_ASTERC"      "SUFFICIENCY_RATING"      "STATUS_NO_10YR_RULE"    

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C"  )

# x = M[,match(keep, colnames(M))]

x = select(data, one_of(keep))  # see chapter 5 (section 4) in r4ds.

wi = filter(x, STATE_CODE_001 == 55)

library(ggplot2)
ggplot(data = wi) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))
wi = filter(wi,LONG_017 > 0)
ggplot(data = wi) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))

min2dec = function(x){
  substr(x,3,8) %>% return
}
hist(wi$LAT_016 %>% min2dec %>% as.numeric)

min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(wi$LAT_016[1])
hist(wi$LAT_016 %>% min2dec %>% as.numeric)

wi = mutate(wi,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = lon))

wi = filter(wi,lon<100)
ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = -lon))

ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = -lon,col =TOLL_020))
ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = -lon,col =YEAR_BUILT_027))
ggplot(data = wi) +geom_point(mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027))
ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = TOLL_020)) +geom_point() + geom_smooth()

ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)


# make function to rate bridge as NA, good, bad, fail, using 
# colnames(wi)[10:13]
# "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

# cond "condition" is the minimum of the given ratings. 
wi = mutate(wi, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

wi$rate = rateIt(wi$cond)
table(wi$cond)
table(wi$rate)
wi = filter(wi, cond>1)
ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + geom_smooth()

map = ggplot(data = wi, mapping = aes(y = lat, x = lon))
map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  

# where are these bad roads?!!??
ggplot(data = wi, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
colnames(wi)

# use a data playground!

wi = mutate(wi, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
# why doesn't this work (??)...
# unite(wi, col = tmp, STATE_CODE_001, COUNTY_CODE_003 , sep = "") 
wi = wi %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi$fips %>% head

wi = wi %>% mutate(good = (rate == "good"))
table(wi$good)
dat = wi %>% group_by(fips) %>% summarize(propGoodRoads = mean(good))

# dat = wi %>% group_by(good) %>% summarize(tmp = mean(lat))

dat %>% transmute(region = fips, value = propGoodRoads) %>% county_choropleth(state_zoom = "wisconsin")
