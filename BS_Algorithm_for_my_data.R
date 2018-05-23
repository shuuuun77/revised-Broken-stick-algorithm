##--------------------------------------------------------
## Broken Stick Algorithm for My data
## ref   : (/Users/teramurashunya/documents/002_R/参考文献_R/Algorithm of the automated and optimised broken stick method.R)
## access: (/Users/teramurashunya/documents/002_R/BS_Algorithm_for_my_data.R)

## Name: Shunya Teramura
## Date: May 2018

##--------------------------------------------------------

rm(list = ls())

data_depth_path <- ("/Users/teramurashunya/Documents/003_data/Depth_all/Kami_Depth+Temp+Kami.txt")
data_time_path  <- ("/Users/teramurashunya/Documents/003_data/Time_all/Time_data.csv")
fig_path        <- ("/Users/teramurashunya/Documents/004_fig")


setwd("/Users/teramurashunya/Documents/003_data")


##--------------------------------------------------------
## select data "daytime", "depth", ("num")
## daytime :  csv
## depth   :  csv
## num     :  NaN

Time_data  <- read.csv(data_time_path, header = F, sep = ",")
Depth_data <- read.table(data_depth_path, skip = 7, header = T, sep = ",")
Depth      <- Depth_data[, 1]
dt         <- data.frame(Time_data, Depth)
names(dt)  <- c("daytime", "depth")
dt$daytime <- as.POSIXct(dt$daytime, format = "%Y-%m-%d %H:%M:%S")  ## Your dates need to be in POSIXct format

##--------------------------------------------------------------------------------

###  Optimised Broken Stick Algorithm

### 1. Creation of output dataframes ------------------------------------------------

#num  <-  unique(dt$num) ## number of dives you have
#num.list  <-  num

# broken stick dataframe
dbs   <-   data.frame("num"=rep(0,1), "all.dur"=0, "start"=0, "end"=0, "depth_start"=0, "depth_end"=0, "seg"=0, "npoints"=0, "dur"=0, "dur.per"=0, "coef"=0, "mean_depth"=0, "max.depth"=0, "sinuosity"=0, "mean_err"=0, "foraging"=0)

## num         : dive number
## all.dur     : total dive duration
## start       : date of segment start
## end         : date of segment end
## depth_start : depth of segment start
## seg         : broken stick segment number
## npoints     : number of points summarising the dive
## dur         : duration of each segment
## dur.per     : % of total duration
## coef        : slope coefficient of the segment
## mean_depth  : mean depth of segment
## max.depth   : maximum dive depth
## sinuosity   : vertical sinuosity associated to this part of the dive
## mean_err    : mean distance between original dive profile and the reconstructed one for the optimal number of broken stick points
## foraging    : behaviour according to vertical sinuosity threshold

## dataframe in which the dives for which the fit doesn't work will be stored.
## Needs to have a same column names exactly
ncdv <- data.frame("daytime" = rep(0, 1), "depth" = 0, "num" = 0)

### 2. calculate each dive -----------------------------------------------------

  # plot(as.numeric(dt$daytime), dt$depth, ylim = c(max(dt$depth),0), t = "l", ylab = "depth (m)", xlab = "", xaxt = "n")
  # Use only if you want to check your dive
  # axis.POSIXct(1, x = dt$daytime, format="%H:%M:%S", labels = TRUE, cex.lab = 0.5)

  np  <- c(3:30)     ## number of broken stick iterations to see which optimal number of points summarise your dive
  npe <- rep(NA,28)   ## vector where the average distance between original and reconstructed dive profile is stored
  npo <- rep(NA,28)   ## vector where the number of broken stick points describing the dive profile is stored

### 3. Finding the optimal number of Broken points for each dive ---------------

 #### Loop to define the mean distance depending on the number of broken stick points

   for (k in 1:length(np)){
       npp = np[k] # selection of the number of iteration: from 3 to 30

       # 2 lines below: selection of the depth and time for the 2 surface points and the maximum depth point
       ref <- c(dt$depth[1], max(dt$depth), dt$depth[nrow(dt)])
       tim <- c(as.numeric(dt$daytime[1]), as.numeric(dt$daytime[dt$depth == max(dt$depth)][1]), as.numeric(dt$daytime[nrow(dt)]))

       for (i in 1:npp){
         # plot(as.numeric(dt$daytime), dt$depth, ylim = c(max(dt$depth), 0), t = "l", ylab = "depth (m)", xlab = "",xaxt = "n")
         # plot only if you want to see how the broken stick algorithm is working
         # points(tim, ref, pch = 19, cex= 1, col = "red")
         # idem
         # linear interpolation between broken stick points at TDR time interval
         interp <- approx(tim, ref, xout = dt$daytime, method = "linear")
         # lines(interp, col="red")
         # idem

         dif_x <- as.numeric(interp$x - dt$daytime)   # time differences between original and reconstructed profiles
         dif_y <- interp$y - dt$depth                  # depth differences between original and reconstructed profiles
         dst   <- sqrt(dif_x^2 + dif_y^2)              # calculate distances between original and reconstructed profiles

         ii    <- which(dst == max(dst))[1]            # index of the data point of maximum difference between original and reconstructed profiles
         # points(dt$daytime[ii], dt$depth[ii], col = "blue", pch = 19, cex = 1)
         # idem

         tim <- c(as.numeric(tim), as.numeric(dt$daytime[ii]))      # add new broken stick point time
         tim <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "jst") + tim
         ref <- c(ref, dt$depth[ii])                                # add new broken stick point depth
       }

       npe[k] = mean(dst)   # average distance between original and reconstructed dive profiles
       npo[k] = length(tim) # number of broken stick points describing the dive profile
     }


 #### Defining the optimal number of broken stick points

     f <- data.frame(npe = npe, npo = npo)
     # plot(f$npo, f$npe, xlab = "nb of points", ylab = "mean error")   # plot of mean distance between original and reconstructed dive profiles
                                                                        # according to the number of broken stick points describing the dive
                                                                        # activate only if you want to check

     # Use of a gompertz model to find the curve which best fit our data
     # parameters -----------------------------------------
     Asym <- 0
     b2   <- -5
     b3   <- 0.9
     fm1  <- -999

     # gompertz model to fit an asymptote curve to the mean distance between original and reconstructed dive profiles plot
     try(fm1 <- nls(npe ~ SSgompertz(npo, Asym, b2, b3), data = f, control = nls.control(maxiter=500)), TRUE)

     if (class(fm1) == "nls"){
       # if the model converged, we can go to the next steps
       # summary(fm1)
       tt <- predict(fm1, f$npe)


       # plot of the mean distance
       # png(paste(fig_path, "_", substr(dt$daytime[1],1,10), .png, sep = ""), pointsize = 12*1.5, height = 480*1.5, width = 480*1.5)

       par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
       tit <- paste("dive" "_", substr(dt$daytime[1], 1, 10))
       plot(f$npo, f$npe, xlab = "nb of points", ylab = "mean error", main = tit)
       lines(na.omit(f$npo), tt[1:28], col = "red")

       # Plot the linear approximation between the first and last point of the fitted curve
       t        <- data.frame(npe = c(f$npe[1], f$npe[28]), npo = c(f$npo[1], f$npo[28]))
       interp   <- approx(c(f$npo[1], f$npo[28]), c(tt[1], tt[28]), xout = f$npo, method = "linear")
       interp$x <- interp$x[!is.na(interp$x)]
       interp$y <- interp$y[!is.na(interp$y)]
       lines(interp$x, interp$y, col="blue")

       # Looking for the inflexion point which is the furthest point between the fitted curve and the approximation
       dif_x <- interp$x - na.omit(f$npo)
       dif_y <- interp$y - tt[1:28]
       dst   <- sqrt(dif_x^2 + dif_y^2)
       dm    <- f$npo[which(dst == max(dst))]

       points(f$npo[which(dst == max(dst))], f$npe[which(dst == max(dst))], pch=19, col="red")      ## inflexion point


 #### optimal broken stick method for each dive

       # The two lines below select the optimal number of broken stick points (in their order of appearance in the BS iteration)
       # example: surface start point, max. depth point, surface end point + x other points
       tim = tim[1:dm]
       ref = ref[1:dm]

       tim2    <- sort(tim)
       dep_tim <- as.data.frame(cbind(ref,tim))
       dep_tim <- dep_tim[order(tim),]

       dbs2 <- data.frame("num" = rep(0, (nrow(dep_tim)-1)), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,  "dur" = 0, "dur.per" = 0, "coef" = 0,  "mean_depth" = 0,  "max.depth" = 0, "sinuosity" = 0, "mean_err" = 0)

       # Loop to calculate the different metrics for each broken stick segments
       for (n in 1 : (nrow(dep_tim) - 1)){
         x1                  = dep_tim$tim[n]          # start of BS segment
         x2                  = dep_tim$tim[n+1]        # end of BS segment
         dbs2$num[n]         = ndive
         dbs2$all.dur[n]     = difftime(dt$daytime[nrow(dt)], dt$daytime[1], tz, units = c("secs"))   #dive duration
         dbs2$start[n]       = x1
         dbs2$end[n]         = x2
         dbs2$depth_start[n] = dep_tim$ref[n]            # depth of start of BS segment
         dbs2$depth_end[n]   = dep_tim$ref[n+1]          # depth of end of BS segment
         dbs2$seg[n]         = n                         # segment number
         dbs2$npoints[n]     = nrow(dep_tim)             # optimal BS points summarising the original dive profile
         dbs2$dur[n]         = difftime(tim2[n+1], tim2[n], tz, units = c("secs"))                                        # duration of the segment in sec.
         dbs2$dur.per[n]     = (dbs2$dur[n]/dbs2$all.dur[n]) * 100                                                        # % of segment duration according to total dive duration
         dbs2$coef[n]        = (dep_tim$ref[n+1] - dep_tim$ref[n])/(x2 - x1)                                              # slope coefficient of the segment
         dbs2$mean_depth[n]  = mean(dt$depth[which(as.numeric(dt$daytime) == x1):which(as.numeric(dt$daytime) == x2)])    # mean depth of the segment
         # calculated from original profile depths
         dbs2$max.depth[n]   = max(dt$depth)             # dive max. depth

         # Calculation of vertical sinuosity
         d_bs = abs(dep_tim$ref[n+1] - dep_tim$ref[n])   # Vertical distance swum between 2 BS points
         d_obs = sum(abs(diff(dt$depth[which(dt$daytime == x1):which(dt$daytime == x2)])))        # sum of all the vertical distances from the original
         # profile between the two corresponding BS depth points

         dbs2$sinuosity[n] = d_bs/d_obs                        # vertical sinuosity index
         dbs2$mean_err[n]  = f$npe[which(dst == max(dst))]     # mean distance between original and reconstructed dive profiles for the optimal
         # number of BS points summarising the dive.
       }

#### Dive plot: original dive profile and Broken stick reconstructed profile
       sg <- unique(dbs2$seg)
       cl <- c("blue", "red")
       dbs2$code[dbs2$sinuosity]

       plot(as.numeric(dt$daytime), dt$depth, ylim = c(max(dt$depth), 0), t = "l", ylab = "depth (m)", xlab = "", xaxt = "n")
       points(tim, ref, pch = 19, cex = 1, col = "black")
       lines(approx(tim, ref, xout = dt$daytime, method = "linear"), col = "black")
       for(i in 1 : length(sg)){
         lines(c(dbs2$start[dbs2$seg == sg[i]], dbs2$end[dbs2$seg == sg[i]]), c(dbs2$depth_start[dbs2$seg == sg[i]],
         dbs2$depth_end[dbs2$seg == sg[i]]), col = cl[dbs2$foraging][dbs2$seg == sg[i]], lwd = 2.5)
         }
       axis.POSIXct(1, x = dt$daytime, format = "%H:%M:%S", labels = TRUE, cex.lab = 0.5)
       dev.off()
       dbs <-rbind(dbs,dbs2)
     } else {ncdv <- rbind(ncdv, dt)}               # allows to keep somewhere the data for which the fit of the Gompertz model didn't work
   # end of if loop for 60 s
   # save(dbs, file = "BS_fitmet_WED_08_samp.RData")
   # end of for loop for dive number

     dbs<- dbs[-1,]

     hist(dbs$sinuosity, xlab = "sinuosity", breaks = seq(0, 1, 0.1), main = "")      ## See line 191
     abline(v = 0.9, col = "red", lwd = 2)

     ncdv <- ncdv[-1, ]
     # save(ncdv, file = "BS_err_WED_08_samp.RData")
     # save(dbs, file = "BS_fitmet_WED_08_samp.RData")
