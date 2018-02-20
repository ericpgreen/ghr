# packages
  list.of.packages <- c("data.table", "httr", "knitr")
  new.packages <- list.of.packages[!(list.of.packages %in% 
                                       installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(data.table)
  library(httr)

# rounding functions
  rd0 <- function(y) sprintf("%.0f", round(y, 0))
  rd1 <- function(y) sprintf("%.1f", round(y, 1))
  rd2 <- function(y) sprintf("%.2f", round(y, 2))
  rd3 <- function(y) sprintf("%.3f", round(y, 3))
  
# get the data
  response <- GET("https://www.dropbox.com/s/wvm6ykkomwjj37o/1880nc.csv.zip?dl=1",
  write_disk("1880nc.csv.zip", overwrite = T),
  progress())
  f <- unzip("1880nc.csv.zip")
  dat <- fread(f[1])
  
# create a variable for ever married
  dat$everMarried <- ifelse(dat$marst=="Divorced" |
                            dat$marst=="Married, spouse absent" |
                            dat$marst=="Married, spouse present" |
                            dat$marst=="Widowed", 1, 0)
  
# convert age variable from categorical to numeric
# make some simplifying assumptions about age categories
  dat$ageN <- ifelse(dat$age=="100 (100+ in 1960-1970)", 100,
              ifelse(dat$age=="112 (112+ in the 1980 internal data)", 112,
              ifelse(dat$age=="115 (115+ in the 1990 internal data)", 115,
              ifelse(dat$age=="90 (90+ in 1980 and 1990)", 90,
              ifelse(dat$age=="Less than 1 year old", 0, dat$age)))))
  dat$ageN <- as.numeric(dat$ageN)
  ageMPop <- mean(dat$ageN)
  marriedBefore15 <- nrow(dat[dat$everMarried==1 & dat$ageN<15,])
  
# single, never married
  dat15 <- dat[dat$ageN>=15,]
  N <- nrow(dat15)
  #print(paste0("Denominator is ", N))
  #table(dat15$marst)
  singleCt <- table(dat15$marst)[names(table(dat15$marst))=="Never married/single"]
  singleP <- singleCt/N
  
  