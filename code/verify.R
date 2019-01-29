
library(googledrive)
library(googlesheets)
library(tidyverse)


setwd("E:/ppgdb/reformat_verification")

folders <- drive_ls("Plant PopGen Project/formatted_datasets")

sheet <- gs_title("Plant PopGen datasets")
q <- gs_read(sheet, ws="reformatting")




# download data from google drive
download_data <- function(cmp){ # one row of the components tibble
      dr <- cmp$folder$name
      suppressWarnings(dir.create(dr))
      f <- drive_ls(as_id(folder$id))
      for(i in 1:nrow(f)) suppressMessages(drive_download(as_id(f$id[i]), 
                                                          paste0(dr, "/", f$name[i]),
                                                          overwrite=T))
      return(cmp)
}


# check filenames
v_filenames <- function(cmp){ # directory path
      
      fn <- list.files(cmp$folder$name)
      
      if(!any(grepl("\\.R", fn))) stop("Error in filename check: missing R script")
      if(!any(fn %in% c("genepop.txt",
                        paste0("genepop_", cmp$i, ".txt")))) stop("Error in filename check: missing or misnamed genepop.txt file")
      if(!any(fn %in% c("populations.csv",
                        paste0("populations_", cmp$i, ".csv")))) stop("Error in filename check: missing or misnamed poulations.csv file")
      return(cmp)
}

# check populations file
v_populations <- function(cmp){
      
      fn <- list.files(cmp$folder$name)
      pfn <- fn[fn %in% c("populations.csv", paste0("populations_", cmp$i, ".csv"))]
      p <- read.csv(paste0(cmp$folder$name,  "/", pfn), stringsAsFactors = F)
      
      if(ncol(p) != 4) stop("incorrect number of columns in population location file")
      if(all.equal(names(p), c("ID", "name", "longitude", "latitude")) != TRUE &
         all.equal(names(p), c("ID", "names", "longitude", "latitude")) != TRUE){
            stop("incorrect variable names in population location file")
      }
      if(any(is.na(p))) stop("NA values in location file")
      if(class(p$ID) != "integer") stop("population IDs are not integers but should be")
      return(cmp)
}


# check genepop file


# check genepop-populations agreement


# check script to see if it regenerates components



####

q$component <- paste0(q$'dataset ID', q$'component ID')

verify <- function(cmp){
      cmp %>% 
            download_data() %>%
            v_filenames() %>%
            v_populations()
}


for(i in q$component[25:42]){
      message(i)
      
      # download data
      folder <- folders[folders$name==i,]
      if(nrow(folder)==0) folder <- folders[folders$name==sub(paste(letters, collapse="|"), "", i),]
      
      #ddf <- download_data(folder)
      
      cmp <- list(i=i, folder=folder)
      
      v <- try(verify(cmp))
      
      if(class(v) != "try-error") v <- "PASS"
      v <- as.character(v)
      v <- gsub("\\\n", "", v)
      q$verification[q$component==i] <- v
}



# write test results to google spreadsheet
gs_edit_cells(sheet, "reformatting", 
              input=q$verification, 
              anchor=paste0("R2C", which(names(q)=="verification")))




