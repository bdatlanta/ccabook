# function to convert column names to lower case valid R names
tolow = function(datain){
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  return(datain)}

# enhanced function to replace multiple and final periods in column names
tolow2 = function(datain){
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  return(datain)}

tolow2a = function(datain){
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  return(datain)}

# same as tolow2, but replaces all periods with underscores
tolow3 = function(datain){
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  colnames(datain) = gsub("\\.", "_", colnames(datain))  
  return(datain)}

# copies dataframe to enlarged clipboard
clip = function(datain) {
  write.table(datain, file="clipboard-16384", sep="\t", row.names=FALSE, col.names=TRUE)
  result = 'clipboard written'
  return(result)}

# extracts string using starting position and length
mid = function(instr,startpos,length){
  return(substr(instr,startpos,startpos+length-1))}

# converts numeric year and month to lubridate
num2date = function(year,month) {
  ldate = ymd(paste0(year,"-",month,"-",1))
  return(ldate) }

# plot functions for quick plotting of spatial data
plota = function(datain){plot(datain,add=TRUE)}
plotg = function(datain){plot(st_geometry(datain))}
plotag = function(datain){plot(st_geometry(datain),add=TRUE)}
plotga = function(datain){plot(st_geometry(datain),add=TRUE)}

# creates drawdown variable with four parts
#    variable name
#    variable subtype (fuel, source, etc.)
#    sector (ac, cc, ic, rc, ei, ag, fo)
#    optional units
makevar = function(p1,p2="x",p3="x",p4="x"){
  dot = "."
  outvar = paste0(p1,dot,p2,dot,p3,dot,p4)
  return(outvar)}

makefname = function(p1,p2="x",p3="x",p4="x"){
  dash = "-"
  outfname = paste0(p1,dash,p2,dash,p3,dash,p4)
  return(outfname)}

# retrieves one of four parts of drawdown variable by part numbper
getvar = function(invarname,partnum){
  dot = "\\."
  slist = str_split_fixed(invarname,dot,4)
  return(slist[,partnum]) }

# version 2 : retrieves one of four parts of drawdown variable by character
getvar2 = function(invarname,partname="Bad part name"){
  dot = "\\."
  slist = str_split_fixed(invarname,dot,4)
  if (str_sub(partname,1,1) == "v") {partnum = 1}
  else if (str_sub(partname,1,1) == "f") {partnum = 2}
  else if (str_sub(partname,1,1) == "s") {partnum = 3}
  else if (str_sub(partname,1,1) == "u") {partnum = 4}
  else return("Bad part name")
  return(slist[,partnum]) }

# version 3: combines version 1 and version 2
getvar3 = function(invarname,inpart="Bad part input"){
  dot = "\\."
  slist = str_split_fixed(invarname,dot,4)
  if      (str_sub(inpart,1,1) == "v") {partnum = 1}
  else if (str_sub(inpart,1,1) == "f") {partnum = 2}
  else if (str_sub(inpart,1,1) == "s") {partnum = 3}
  else if (str_sub(inpart,1,1) == "u") {partnum = 4}
  else if (inpart == 1) {partnum = 1}
  else if (inpart == 2) {partnum = 2}
  else if (inpart == 3) {partnum = 3}
  else if (inpart == 4) {partnum = 4}
  else return("Bad part input; should be 1,2,3,4,'v','f','s','u'")
  return(slist[,partnum]) }

# for compatibility with unmodified older code
# assigns new version to both old versions
getvar = getvar3
getvar2 = getvar3



getsector = function(){
  return(getvar2(varname,"sector")) }

getfuel = function(){
  return(getvar2(varname,"fuel")) }

getunits = function(){
  return(getvar2(varname,"units")) }

# for standard format dataset, aggregate months to years
month2year = function(indf) {
  outdf = indf %>% 
    mutate(year = year(ldate)) %>% 
    group_by(geocode,varname,year) %>% 
      summarize(datavalue = sum(datavalue,na.rm=TRUE)) %>% 
      ungroup()
  return(outdf) }




# wgs84 and nad83 projections defined via EPSG number and proj4 for rasters

wgs84 = 4326
rwgs84 = "+init=EPSG:4326"

nad83 = 4269
rnad83 = "+init=EPSG:4269"


# Alternate proj4 and WKT USGS Albers equal area projections
# spatialreference.org: ESRI:102003

usgsalb = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' 

usgsalb2 = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",
            GEOGCS["GCS_North_American_1983",
            DATUM["North_American_Datum_1983",
            SPHEROID["GRS_1980",6378137,298.257222101]],
            PRIMEM["Greenwich",0],
            UNIT["Degree",0.017453292519943295]],
            PROJECTION["Albers_Conic_Equal_Area"],
            PARAMETER["False_Easting",0],
            PARAMETER["False_Northing",0],
            PARAMETER["longitude_of_center",-96],
            PARAMETER["Standard_Parallel_1",29.5],
            PARAMETER["Standard_Parallel_2",45.5],
            PARAMETER["latitude_of_center",37.5],
            UNIT["Meter",1],
            AUTHORITY["EPSG","102003"]]'

# Alternate proj4 and WKT Georgia Lambert Conic projections
# spatialreference.org: SR-ORG:8828

galcc = '+proj=lcc +lat_1=31.41666666666667 +lat_2=34.28333333333333 +lat_0=0 +lon_0=-83.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs' 

galcc2 = 'PROJCS["NAD_1983_Georgia_Statewide_Lambert",
          GEOGCS["GCS_North_American_1983",
          DATUM["North_American_Datum_1983",
          SPHEROID["GRS_1980",6378137.0,298.257222101]],
          PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],
          PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["False_Easting",0.0],
          PARAMETER["False_Northing",0.0],
          PARAMETER["Central_Meridian",-83.5],
          PARAMETER["Standard_Parallel_1",31.41666666666667],
          PARAMETER["Standard_Parallel_2",34.28333333333333],
          PARAMETER["Latitude_Of_Origin",0.0],
          UNIT["Foot_US",0.3048006096012192]]'


# downloads eia data series or vector of series names
geteia = function(serieslist) {
  
  df = data.frame(seriesid=character(),
                  geocode=character(),
                  ldate=as.Date(character()),
                  datavalue=as.numeric())
  for (s in serieslist) {
    print(paste0("Getting series: ", s))
    d1 = eia_series(s)$data[[1]] %>% 
      mutate(seriesid = s) %>% 
      mutate(geocode = 
               ifelse(str_detect(seriesid,".GA.") |
                        str_detect(seriesid,"-GA-") | 
                        str_detect(seriesid,".GA-") |
                        str_detect(seriesid,"-GA.") |
                        str_detect(seriesid,"_GA_"),
                        "g13000",
                ifelse(str_detect(seriesid,".US.") |
                         str_detect(seriesid,"-US-") | 
                         str_detect(seriesid,".US-") |
                         str_detect(seriesid,"-US.") |
                         str_detect(seriesid,"_US_"),
                        "g00000",
                ifelse(str_detect(seriesid,".AL.") |
                         str_detect(seriesid,"-AL-") | 
                         str_detect(seriesid,".AL-") |
                         str_detect(seriesid,"-AL.") |
                         str_detect(seriesid,"_AL_"),
                        "g01000",
                        "gxxxxx")))) %>% 
      select(seriesid, geocode, ldate=date, datavalue=value)
    df = df %>% add_row(d1) }
  return(df) }

geteianocache = function(serieslist) {
  
  df = data.frame(seriesid=character(),
                  geocode=character(),
                  ldate=as.Date(character()),
                  datavalue=as.numeric())
  for (s in serieslist) {
    print(paste0("Getting series: ", s))
    d1 = eia_series(s,cache=FALSE)$data[[1]] %>% 
      mutate(seriesid = s) %>% 
      mutate(geocode = 
               ifelse(str_detect(seriesid,".GA.") |
                        str_detect(seriesid,"-GA-") | 
                        str_detect(seriesid,".GA-") |
                        str_detect(seriesid,"-GA.") |
                        str_detect(seriesid,"_GA_"),
                      "g13000",
                      ifelse(str_detect(seriesid,".US.") |
                               str_detect(seriesid,"-US-") | 
                               str_detect(seriesid,".US-") |
                               str_detect(seriesid,"-US.") |
                               str_detect(seriesid,"_US_"),
                             "g00000",
                             ifelse(str_detect(seriesid,".AL.") |
                                      str_detect(seriesid,"-AL-") | 
                                      str_detect(seriesid,".AL-") |
                                      str_detect(seriesid,"-AL.") |
                                      str_detect(seriesid,"_AL_"),
                                    "g01000",
                                    "gxxxxx")))) %>% 
      select(seriesid, geocode, ldate=date, datavalue=value)
    df = df %>% add_row(d1) }
  return(df) }

slope = function(x,y){
  s = cov(x,y,use="pairwise.complete.obs") / var(x,na.rm=TRUE)
  return(s) }

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret) }

bboxmod = function(ingeom,nvalue,svalue,evalue,wvalue){
  inbbox = st_bbox(ingeom)
  xrange = inbbox$xmax - inbbox$xmin # range of x values
  yrange = inbbox$ymax - inbbox$ymin # range of y values
  inbbox[1] = inbbox[1] + (wvalue * xrange) # xmin - left/west
  inbbox[3] = inbbox[3] + (evalue * xrange) # xmax - right/east
  inbbox[2] = inbbox[2] + (svalue * yrange) # ymin - bottom/south
  inbbox[4] = inbbox[4] + (nvalue * yrange) # ymax - top/north  
  inbbox %>%  
    st_as_sfc() }

# short replacement for unique
uu = function(inarg){
  unique(inarg) }




