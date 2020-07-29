corr <- function(directory, threshold = 0) {
  
  # Reading in all files and making a large data.table
  lst <- lapply(file.path(directory, list.files(path = directory, pattern="*.csv")), data.table::fread)
  dt <- rbindlist(lst)
  
  # Only keep completely observed cases
  dt <- dt[complete.cases(dt),]
  
  # Apply threshold
  dt <- dt[, .(nobs = .N, corr = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
  return(dt[, corr])
}

cr <- corr('C:/Users/Maga/Desktop/datasciencecoursera/rprog_data_specdata/specdata')                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("C:/Users/Maga/Desktop/datasciencecoursera/rprog_data_specdata/specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("C:/Users/Maga/Desktop/datasciencecoursera/rprog_data_specdata/specdata", 2000)                
n <- length(cr)                
cr <- corr("C:/Users/Maga/Desktop/datasciencecoursera/rprog_data_specdata/specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))