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
# same as tolow2, but replaces all periods with underscores
tolow3 = function(datain){
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  colnames(datain) = gsub("\\.", "_", colnames(datain))  
  return(datain)}
# copies dataframe to clipboard
clip = function(datain) {
  write.table(datain, file="clipboard-16384", sep="\t", row.names=FALSE, col.names=TRUE)
  result = 'clipboard written'
  return(result)}
# extracts string using starting position and length
mid = function(instr,startpos,length){
  return(substr(instr,startpos,startpos+length-1))}
num2date = function(year,month) {
  ldate = ymd(paste0(year,"-",month,"-",1))
  return(ldate) }




