

library(googledrive)
library(googlesheets)
library(tidyverse)


package <- function(cmp, folders){
      ci <- paste0(cmp$'dataset ID', cmp$'component ID')
      folder <- folders[folders$name==ci,]
      if(nrow(folder)==0) folder <- folders[folders$name==sub(paste(letters, collapse="|"), "", ci),]
      cmp <- list(gs = cmp,
                  i=ci, 
                  folder=folder)
      return(cmp)
}


# download data from google drive
download_data <- function(cmp){ # one row of the components tibble
      
      dr <- cmp$folder$name
      if(dir.exists(dr)) unlink(dr, recursive=T)
      suppressWarnings(dir.create(dr))
      f <- drive_ls(as_id(cmp$folder$id))
      for(i in 1:nrow(f)) suppressMessages(drive_download(as_id(f$id[i]), 
                                                          paste0(dr, "/", f$name[i]),
                                                          overwrite=T))
      cmp$tests$download <- "pass"
      return(cmp)
}

# run script and verify that it regenerates outputs
v_script <- function(cmp){
      
      wd <- cmp$folder$name
      fn <- list.files(wd, full.names=T)
      
      # copy input files to temp dir
      td <- paste0(wd, "/test/")
      dir.create(td)
      outputs <- fn[(grepl("\\.txt|\\.csv", fn) & grepl("/genepop|/populations", fn))]
      inputs <- setdiff(fn, outputs)
      fc <- file.copy(inputs, paste0(td, basename(inputs)))
      
      # run R script
      tdf <- list.files(td, full.names=T)
      rsn <- tdf[grepl("\\.R", tdf)]
      if(length(rsn)==0) stop("No R script found")
      ss <- try(source(rsn, chdir=T))
      if(class(ss)=="try-error"){
            e <- gsub("\\\n", "", as.character(ss))
            stop(paste("Script failed with the following error:", e))
      }
      
      # check if outputs are reproduced
      if(!all(file.exists(paste0(td, basename(outputs))))) stop("Script runs but does not reproduce output files")
      
      cmp$tests$script <- "pass"
      return(cmp)
}

# check filenames
v_filenames <- function(cmp){ # directory path
      
      fn <- list.files(paste0(cmp$folder$name, "/test"))
      
      if(!any(grepl("\\.R", fn))) stop("Error in filename check: missing R script")
      if(!any(fn %in% c("genepop.txt",
                        paste0("genepop_", cmp$i, ".txt")))) stop("Error in filename check: missing or misnamed genepop.txt file")
      if(!any(fn %in% c("populations.csv",
                        paste0("populations_", cmp$i, ".csv")))) stop("Error in filename check: missing or misnamed poulations.csv file")
      cmp$tests$filenames <- "pass"
      return(cmp)
}

# check populations file
v_populations <- function(cmp){
      
      fn <- list.files(paste0(cmp$folder$name, "/test"))
      pfn <- fn[fn %in% c("populations.csv", paste0("populations_", cmp$i, ".csv"))]
      p <- read.csv(paste0(cmp$folder$name,  "/", pfn), stringsAsFactors = F)
      
      if(ncol(p) != 4) stop("incorrect number of columns in population location file")
      if(all.equal(names(p), c("ID", "name", "longitude", "latitude")) != TRUE &
         all.equal(names(p), c("ID", "names", "longitude", "latitude")) != TRUE){
            stop("incorrect variable names in population location file")
      }
      if(any(is.na(p))) stop("NA values in location file")
      if(class(p$ID) != "integer") stop("population IDs are not integers but should be")
      cmp$tests$populations <- "pass"
      return(cmp)
}


# check genepop file
v_genepop <- function(cmp){
      cmp$tests$genepop <- ""
      return(cmp)
}

# check genepop-populations agreement
v_alignment <- function(cmp){
      cmp$tests$alignment <- ""
      return(cmp)
}


# execute tests and write results to google spreadsheet
validate <- function(cmp, folders, sheet){
      
      cmp <- package(cmp, folders=folders)
      
      for(fun in list(download_data,
                      v_script,
                      v_filenames,
                      v_populations,
                      v_genepop,
                      v_alignment)){
            #message(deparse(substitute(fun)))
            cmpi <- try(fun(cmp))
            if(class(cmpi) == "try-error") break()
            cmp <- cmpi
      }
      
      v <- unlist(cmp$tests)
      
      if(class(cmpi) == "try-error"){
            e <- as.character(cmpi)
            e <- gsub("\\\n", "", e)
            v <- c(v, e)
            v <- c(v, rep("", 6))[1:6]
      }
      
      v <- c(timestamp=as.character(Sys.time()), v)
      
      gs_edit_cells(sheet, "reformatting", 
                    input=v, 
                    anchor=paste0("R", cmp$gs$rownum+1,
                                  "C", which(names(cmp$gs)=="last_validated")),
                    byrow=T)
      
      unlink(cmp$folder$name, recursive=T)
      
      return(v)
}
