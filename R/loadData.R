## retrieve zip file if not already downloaded, then unzip it in the data/ directory

archive_url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
file_name <- 'Coursera-SwiftKey.zip'
data_dir <- 'data'

loadDataFile <- function() {
    ## Check data directory 
    if(!file.exists(data_dir)) {
        dir.create(data_dir)
    }
    
    ## Download file if needed
    final_path <- paste(data_dir, file_name, sep='/')
    if(!file.exists(final_path)) {
        download.file(archive_url,
                      final_path,
                      method='curl')
        
    }
    
    ## Unzip it in data dir
    unzip(final_path, exdir=data_dir)
}

#' Load a particular file from corpus dataset
#' 
#' @description This function helps loading data from corpus dataset, depending on the locale and document type
#' @param locale The locale of the documents, can be de_DE, en_US, fi_FI, ru_RU
#' @param type The document type: blogs, news or twitter
#' @param n integer. The (maximal) number of lines to read. Negative values indicate that one should read up to the end of input on the connection.
#' @value a vector containing a document by row
#' 
loadData <- function(locale, type, n = -1L) {
  filePath <- paste(data_dir,
                    'final',
                    locale, 
                    paste(locale, type, 'txt', sep = '.'),
                    sep = '/')
  con <- file(filePath)
  ## Create data.table
  ## It was faster to use data.table and fread than read.csv
  data <- readLines(con, encoding = 'UTF-8', n = n)    
  close(con)
  data
}