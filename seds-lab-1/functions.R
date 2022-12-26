# function to convert column names to lower case valid R names
tolow = function(datain){
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  return(datain)}

# same as tolow, but replaces all periods with underscores
tolow3 = function(datain){
  colnames(datain) = make.names(colnames(datain), unique = TRUE, allow_ = TRUE)
  colnames(datain) = tolower(colnames(datain))
  colnames(datain) = gsub("\\.\\.+", ".", colnames(datain))
  colnames(datain) = str_remove(colnames(datain), "\\.$")
  colnames(datain) = gsub("\\.", "_", colnames(datain))  
  return(datain)}

# function to copy dataframe to enlarged clipboard
clip = function(datain) {
  write.table(datain, file="clipboard-16384", sep="\t", row.names=FALSE, col.names=TRUE)
  result = 'clipboard written'
  return(result)}

# function to extract string using starting position and length
mid = function(instr,startpos,length){
  return(substr(instr,startpos,startpos+length-1))}