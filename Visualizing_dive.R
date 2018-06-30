## --------------------------------------------
## Visualizing the dive profile
## Access:
## Name: Shunya Teramura
## Date: June 2018
## --------------------------------------------

## 使わない変数をdelete
rm(list = ls())

## pathの設定
path_depth <- ("/Users/teramurashunya/Documents/003_data/Depth_all/Kami_Depth+Temp+Kami.txt")
path_time  <- ("/Users/teramurashunya/Documents/003_data/Time_all/Time_data.csv")
path_fig   <- ("/Users/teramurashunya/Documents/004_fig")

## wdの設定
setwd("/Users/teramurashunya/Documents/003_data")

## ---------------------------------------------------

## select data, "daytime" & ”depth"

data_depth_temp <- read.table(path_depth, skip = 7, header = T, sep = ",")
data_time       <- read.csv(path_time, header = F, sep = ",")
data_depth_only <- data_depth_temp[, 1]
data_depth_time <- data.frame(data_time, data_depth_only)
names(data_depth_time) <- c("daytime", "depth")
data_depth_time$daytime <- as.POSIXct(data_depth_time$daytime, format = "%Y-%m-%d %H:%M:%S") ## Your dates need to be in POSIXct format

#短いスパンでMVPs
data_depth_short <- data_depth_time[1:1000000,]

## ----------------------------------------------------

## definition of the function

#
divide_dive_func <- function(dive) {
  # dive: dataframe[daytime, depth]
  number_columns <- 7
  parameters_all_dive <- data.frame(matrix(rep(NA, number_columns), nrow=1))[numeric(0), ]
  colnames(parameters_all_dive) <- c("date", "number_of_dive", "number_start", "number_end", "time_start", "time_end", "max_depth")
  parameters_one_dive <- data.frame("date" = NaN, "number_of_dive" = 0, "number_start" = 0, "number_end" = 0,"time_start" = NaN, "time_end" = NaN, "max_depth" = 0)


  for (i in 2:nrow(dive)){
    surface  = 50
    num      = 0
    if ((dive[i-1,2] < surface) && (dive[i,2] >= surface)){
      parameters_one_dive$date            <- substr(dive[i,1],1,10)
      parameters_one_dive$number_of_dive  <- num
      parameters_one_dive$number_start    <- i
      parameters_one_dive$time_start      <- substr(dive[i,1],1,19)
    } else if ((dive[i-1,2] >= surface) && (dive[i,2] < surface)){
      parameters_one_dive$number_end      <- i
      parameters_one_dive$time_end        <- substr(dive[i,1],1,19)
      parameters_one_dive$max_depth       <- max(dive[parameters_one_dive$number_start:parameters_one_dive$number_end,2])

      parameters_all_dive                 <- rbind(parameters_all_dive, parameters_one_dive)
    }
  }

  return(parameters_all_dive)
}

plot_dive <- function(dive, dive_index, number_dive_display = 1){
  #何番目の潜水からいくつ表示するか
  plot(dive$depth, xlim = c(result[dive_index,3], result[dive_index+number_dive_display-1,4]), type="l")
}

if(as.numeric(substr(data_depth_short[6971,1],9,10)) - as.numeric(substr(data_depth_short[6970,1],9,10)) == 1){
  print(1)
  }


Broken_Stick_Algorithm <- function(dive) {
  #
}
