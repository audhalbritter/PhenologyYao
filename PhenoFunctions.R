############################
#### PHENOLOGY FUNCITON ####
############################

#### READ EXCEL SHEET ####
ReadExcelSheets <- function(sheet){
  print(sheet) # to check which wheet is read in, can be removed
  dat <- read_excel("2016_phenology data_Snow Fence.xlsx", sheet = sheet, skip = 1, col_names = FALSE) # read excel file
  dat <- dat[,-1] # remove first column with chinese names
  metainfo <- grep("^Site", dat$X1) # grap all rows including metainfo for each plot (site, date, week, etc)
  print(diff(metainfo)) # check the rownumbers for metainfo
  metainfo2 <- data.frame(start = metainfo, end = c(metainfo[-1] -1, nrow(dat))) # create a dataframe with start and end of each plot
  
  # loop over each plot and extract data, plus metainfo
  dat2 <- plyr::adply(metainfo2, 1, function(i){
    x <- dat[(i$start + 2):i$end, ] # extract phenology data
    colnames(x) <- c("sp", "Notes", paste(rep(c("b", "f", "s", "r"), 9), c(1:36), sep = ".")) # give new names
    kk <- dat$X1[i$start] # metainfo
    # use regular expression to extract metainfo
    x$plot <- gsub("Site *. *(\\S*).*", "\\1", kk)
    x$date <- lubridate::ymd(gsub(".*Date *. *(\\S*).*", "\\1", kk))
    x$week <- gsub(".*Week *. *(\\S*).*", "\\1", kk)
    x$photonumber <- gsub(".*Photo numer *. *(\\S*).*", "\\1", kk)
    x
  })
  dat2 <- dat2[,-(1:2)]
}



#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE PER PLOT ####
CalcSums <- function(dat){
  dat$nr.b <- apply(dat[,c(seq(3,35,4))],1,sum, na.rm=TRUE)
  dat$nr.b[dat$nr.b == 0] <- NA
  dat$nr.f <- apply(dat[,c(seq(4,36,4))],1,sum, na.rm=TRUE)
  dat$nr.f[dat$nr.f == 0] <- NA
  dat$nr.s <- apply(dat[,c(seq(5,37,4))],1,sum, na.rm=TRUE)
  dat$nr.s[dat$nr.s == 0] <- NA
  dat$nr.r <- apply(dat[,c(seq(6,38,4))],1,sum, na.rm=TRUE)
  dat$nr.r[dat$nr.r == 0] <- NA
  return(dat)
}