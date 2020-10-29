#from https://stackoverflow.com/questions/25485216/how-to-get-list-files-from-a-github-repository-folder-using-r

library(httr)

#get a list of files we want
data_files <- GET("https://api.github.com/repos/kelpecosystems/observational_data/contents/cleaned_data/")

#what are the actual links
download_links <- sapply(content(data_files), 
       function(.x) .x$download_url)

#download the files
download_keendata <- function(.x){
  outfile <- paste0("data/keen/",
                    gsub("https://raw.githubusercontent.com/kelpecosystems/observational_data/master/cleaned_data/", "", .x))
  
  download.file(.x, outfile)
}

#do it!
sapply(download_links, download_keendata)
