#Function to generate text and graphical capabilities analyses on a data set
vectis.cap <- function(data,
                       distribution = "normal",
                       USL = NA,
                       LSL = NA,
                       target = NA,
                       main = "Process Capability",
                       sub = "",
                       groupsize = 1,
                       mrlength = 2,
                       alpha = 0.05,
                       tol = 5.15,
                       unbias_sub = TRUE,
                       unbias_overall = FALSE,
                       density = FALSE,
                       binwidth = -1,
                       plot = TRUE,
                       name = "Measurement",
                       footer = TRUE
                       )
{
  library(ggplot2)
  library(grid)
  
  if (is.na(target)){
  stop("Target not specified")
  }
  if (is.na(LSL) && is.na(USL)){
  stop("Upper and Lower Specification Limits not specified")
  }
  if (groupsize < 1 || groupsize > 50){
  stop("Group Size must be between 1 and 50")
  }
  if (mrlength < 2){
  stop("Moving Range Length must be greater than or equal to 2")
  }
  
  Lookup <-
    {structure(list(N = 1:100, 
                   c4 = c(NA, 0.797885, 0.886227, 0.921318, 
                          0.939986, 0.951533, 0.959369, 0.96503, 0.969311, 0.972659, 0.97535, 
                          0.977559, 0.979406, 0.980971, 0.982316, 0.983484, 0.984506, 0.98541, 
                          0.986214, 0.986934, 0.987583, 0.98817, 0.988705, 0.989193, 0.98964, 
                          0.990052, 0.990433, 0.990786, 0.991113, 0.991418, 0.991703, 0.991969, 
                          0.992219, 0.992454, 0.992675, 0.992884, 0.99308, 0.993267, 0.993443, 
                          0.993611, 0.99377, 0.993922, 0.994066, 0.994203, 0.994335, 0.99446, 
                          0.99458, 0.994695, 0.994806, 0.994911, 0.995013, 0.99511, 0.995204, 
                          0.995294, 0.995381, 0.995465, 0.995546, 0.995624, 0.995699, 0.995772, 
                          0.995842, 0.99591, 0.995976, 0.99604, 0.996102, 0.996161, 0.996219, 
                          0.996276, 0.99633, 0.996383, 0.996435, 0.996485, 0.996534, 0.996581, 
                          0.996627, 0.996672, 0.996716, 0.996759, 0.9968, 0.996841, 0.99688, 
                          0.996918, 0.996956, 0.996993, 0.997028, 0.997063, 0.997097, 0.997131, 
                          0.997163, 0.997195, 0.997226, 0.997257, 0.997286, 0.997315, 0.997344, 
                          0.997372, 0.997399, 0.997426, 0.997452, 0.997478), 
                   c5 = c(NA, 0.603, 0.463, 0.389, 0.341, 0.308, 0.282, 0.262, 0.246, 0.232, 
                          0.22, 0.21, 0.202, 0.194, 0.187, 0.181, 0.175, 0.17, 0.166, 0.161, 
                          0.157, 0.153, 0.15, 0.147, 0.144, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA), 
                   d2 = c(1, 1.128, 1.693, 2.059, 2.326, 2.534, 
                          2.704, 2.847, 2.97, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 
                          3.532, 3.588, 3.64, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 
                          3.931, 3.965, 3.997, 4.028, 4.058, 4.086, 4.113, 4.139, 4.164, 
                          4.189, 4.213, 4.236, 4.258, 4.28, 4.301, 4.322, 4.342, 4.361, 
                          4.38, 4.398, 4.415, 4.432, 4.449, 4.466, 4.482, 4.498, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                   d3 = c(0.82, 0.8525, 0.8884, 0.8794, 0.8641, 0.848, 0.8332, 
                          0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.763, 0.7562, 
                          0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 
                          0.7121, 0.7084, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                   d4 = c(1, 0.954, 1.588, 1.978, 2.257, 2.472, 2.645, 2.791, 
                          2.915, 3.024, 3.121, 3.207, 3.285, 3.356, 3.422, 3.482, 3.538, 
                          3.591, 3.64, 3.686, 3.73, 3.771, 3.811, 3.847, 3.883, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), 
              .Names = c("N", "c4", "c5", "d2", "d3", "d4"), class = "data.frame", 
              row.names = c(NA, -100L))}
    
#   x <- c(3,5,2,3,7,4,9,1,7,5,8,3)
#   distribution = "normal"
#   USL = 9
#   LSL = 1
#   target = 5
#   main = "Capabilities Analysis"
#   sub = ""
#   groupsize = 1
#   mrlength = 2
#   alpha = 0.05
#   tol = 5.15
#   unbias_sub = TRUE
#   unbias_overall = TRUE
  
  # Estimate the standard deviation within subgroups by the average of the moving range 
  # Add other methods here for subgroup size of 1
  
  
  if (groupsize == 1){
    R_i <- vector(mode = "numeric", length = (length(data[!is.na(data)])-(mrlength-1)))
    range_temp <- vector(mode = "numeric", length = mrlength)
    for(i in 1:(length(data[!is.na(data)])-(mrlength-1))){
      for(j in i:(i+mrlength-1)){
        range_temp[j+1-i]<-data[j]
      }
      R_i[i] <- max(range_temp) - min(range_temp)
    }
    Rbar <- sum(R_i)/(length(data[!is.na(data)])-(mrlength-1))
    
    if (unbias_sub) {
      S_within <<- Rbar/(Lookup$d2[mrlength])
      } else {
      S_within <<- Rbar
      }
  }
  
  if (groupsize > 1){
    #Add Here
  }

  # Calculate overall standard deviation and apply the unbiasing constant if desired
  if (unbias_overall) {
    S_overall <- sd(data)/(Lookup$c4[length(data[!is.na(data)])])
    } else {
    S_overall <- sd(data)
    }

  mu <- mean(data)
  
  
  # Process Data 
  {Proc_Data <- vector(mode = "numeric", length = 8)
  names(Proc_Data) <- c("LSL","Target","USL","Sample Mean","Number of Obs.",
                        "StDev(Within)","StDev(Overall)","Group Size")
  Proc_Data["LSL"] <- LSL
  Proc_Data["Target"] <- target
  Proc_Data["USL"] <- USL
  Proc_Data["Sample Mean"] <- mu
  Proc_Data["Number of Obs."] <- length(data[!is.na(data)])
  Proc_Data["StDev(Within)"] <- S_within
  Proc_Data["StDev(Overall)"] <- S_overall
  Proc_Data["Group Size"] <- groupsize}
    
  # Potential Capability Matrix
  {CPS <- vector(mode = "numeric", length = 5)
  names(CPS) <- c("Cp","CPL", "CPU", "Cpk", "CCpk")
  CPS["Cp"] <- (USL - LSL)/(tol*S_within)
  CPS["CPL"] <- (mu - LSL)/(.5*tol*S_within)
  CPS["CPU"] <- (USL - mu)/(.5*tol*S_within)
  CPS["Cpk"] <- min(CPS["CPU"],CPS["CPL"])
  CPS["CCpk"] <- min(USL-target,target-LSL)/(.5*tol*S_within)}
  
  # Overall Capability Matrix
  {PPS <- vector(mode = "numeric", length = 5)
  names(PPS) <- c("Pp","PPL", "PPU", "Ppk", "Cpm")
  PPS["Pp"] <- (USL - LSL)/(tol*S_overall)
  PPS["PPL"] <- (mu - LSL)/(.5*tol*S_overall)
  PPS["PPU"] <- (USL - mu)/(.5*tol*S_overall)
  PPS["Ppk"] <- min(PPS["PPU"],PPS["PPL"])
  PPS["Cpm"] <- min(USL-target,target-LSL)/(.5*tol*sd(data))}
  
  #Expected Within/Overall/Observed Performance
  {PERF <- vector(mode = "numeric", length = 9)
  names(PERF) <- c("PWLL","PWGU","PWT","POLL","POGU","POT","OBLL","OBGU","OBT")
  PERF["PWLL"] <- 1e6*(1-pnorm((mu-LSL)/S_within))
  PERF["PWGU"] <- 1e6*(1-pnorm((USL-mu)/S_within))
  PERF["PWT"] <- sum(PERF["PWLL"],PERF["PWGU"])
  PERF["POLL"] <- 1e6*(1-pnorm((mu-LSL)/S_overall))
  PERF["POGU"] <- 1e6*(1-pnorm((USL-mu)/S_overall))
  PERF["POT"] <- sum(PERF["POLL"],PERF["POGU"]) 
  PERF["OBLL"] <- 1e6*(length(data[data<LSL])/length(data[!is.na(data)]))
  PERF["OBGU"] <- 1e6*(length(data[data>USL])/length(data[!is.na(data)]))
  PERF["OBT"] <- sum(PERF["OBLL"],PERF["OBGU"])} 

  if(plot){
  
  #Determine max densities for plot limits
  if(density) dens_max <- max(density(data)[[2]]) else dens_max <- 0
  freq_max <- max(hist(as.vector(data), plot = FALSE)$density)
  with_max <- dnorm(mean(data), mean = mean(data),sd = S_within)
  over_max <- dnorm(mean(data), mean = mean(data),sd = S_overall)
    
  #Calculate the binwidth if not specified
  if (binwidth == -1) {
    #Freedman-Diaconis
    binwidth = 2 * IQR(data) / (length(data[!is.na(data)])^(1/3))
    #Square-root choice
    #binwidth = diff(range(data))/sqrt(length(data[!is.na(data)]))
  }
  
  #Create Plots
  data <- data.frame(data=data)
  
  #define function for aes that evaluates expressions
  aes_now <- function(...) {structure(list(...),  class = "uneval")}

  #Initial plot definition
  p <- ggplot(data, aes(x = data)) +
              {theme(plot.margin = unit(c(2,0,.5,0), "lines"), 
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.background = element_rect(fill = "white", color = "gray0"), 
                    plot.background = element_rect(fill = "cornsilk", color = NA),
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(size = 15),
                    legend.background = element_rect(fill = "white", color = "gray0"),
                    legend.key = element_rect(fill = NA, color = NA),
                    legend.key.width = unit(3,"lines"),
                    legend.key.height = unit(1,"lines")) + 
              coord_cartesian(ylim = c(0, max(1.05 * dens_max, 1.05 * freq_max, 
                                       1.05 * with_max, 1.05 * over_max)),
                              xlim = c(min(min(data),
                                           1.1 * LSL - 0.1 * USL, target - 3 * S_within, 
                                           target - 3 * S_overall),
                                       max(max(data),
                                           1.1 * USL - 0.1 * LSL, target + 3 * S_within, 
                                           target + 3 * S_overall))) +
              xlim(min(min(data),
                       1.1 * LSL - 0.1 * USL, 
                       target - 3 * S_within, 
                       target - 3 * S_overall),
                   max(max(data),
                       1.1 * USL - 0.1 * LSL, 
                       target + 3 * S_within, 
                       target + 3 * S_overall)) +
              ylim(0, max(1.05 * dens_max, 1.05 * freq_max, 
                          1.05 * with_max, 1.05 * over_max))}
  
  #Add histogram
  p <- p + geom_histogram(aes(y=..density..),        
                          binwidth = binwidth, 
                          color = "black", fill = "slategray1", position = "identity")
  
  #Add Density
  if(density) {p <- p + geom_line(stat="density", size = 1.1, 
                                  aes(color = "density"), position="identity", linetype = 1)}
  #Add Spec Limits and labels
  p <- p + geom_vline(xintercept = LSL, linetype = 5, size = .65, color = "red3") 
  p <- p + geom_vline(xintercept = target, linetype = 5, size = .65, color = "green3")
  p <- p + geom_vline(xintercept = USL, linetype = 5, size = .65, color = "red3") 

  p <- p + geom_text(aes_now(label = c("USL"), x = c(USL), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "red3", size=4)
  p <- p + geom_text(aes_now(label = c("LSL"), x = c(LSL), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "red3", size=4)
  p <- p + geom_text(aes_now(label = c("Target"), x = c(target), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "green3", size=4)
  
  #Add within and overall distribution lines
  p <- p + stat_function(fun = dnorm,args=list(mean = mu, sd = S_within), 
                         aes(color = "dwith", linetype = "dwith"), size = 1.1, linetype = 2)
  p <- p + stat_function(fun = dnorm,args=list(mean = mu, sd = S_overall), 
                         aes(color = "dover", linetype = "dover"), size = 1.1, linetype = 1)
  
  #Add Legend (currently disabled by theme)
  p <- p +    
    scale_color_manual("Legend",
                              labels = c("Within","Overall","Density"), 
                              breaks = c("dwith","dover","density"),
                              values = c("dwith"="red3",
                                         "dover"="gray0",
                                         "density"="dodgerblue3"))

  
  # Create Process Data Legend
  Proc_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(.2,1))+
    theme(plot.margin = unit(c(2,0.1,2,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Process Data"), 
                  x = .5, y = 1, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("LSL"), 
                  x = .05, y = .9, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Target"), 
                  x = .05, y = .8, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("USL"), 
                  x = .05, y = .7, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Sample Mean"), 
                  x = .05, y = .6, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Sample N"), 
                  x = .05, y = .5, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("StDev(Within)"), 
                  x = .05, y = .4, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("StDev(Overall)"), 
                  x = .05, y = .3, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    
    geom_text(aes_now(label = sprintf("%.3f",LSL), 
                      x = .6, y = .9, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",target), 
                      x = .6, y = .8, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",USL), 
                      x = .6, y = .7, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.6f",mu), 
                      x = .6, y = .6, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = length(data[!is.na(data)]), 
                      x = .6, y = .5, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.8f",S_within), 
                      x = .6, y = .4, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.8f",S_overall), 
                      x = .6, y = .3, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  # Manually Create Chart Legend  
  Leg_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(.2,1))+
    theme(plot.margin = unit(c(2,1,6,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Legend"), 
                  x = .5, y = .95, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4) +
    geom_segment(aes(x = .5, xend = .1, y = .65, yend = .65), 
                 size = 1, colour = "red3", linetype = 2,position = "identity")+
    geom_segment(aes(x = .5, xend = .1, y = .475, yend = .475), 
                 size = 1, colour = "gray0", linetype = 1,position = "identity")+
    geom_text(aes(label = c("Within"), 
                  x = .6, y =.65, 
                  family = "sans"), 
              hjust = 0, vjust = 0.2,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Overall"), 
                  x = .6, y = .5, 
                  family = "sans"), 
              hjust = 0, vjust = 0.45,
              color = "gray0", size=4)

    # check if density is plotted and add to legend
    if(density){
    Leg_leg <- Leg_leg +
    geom_segment(aes(x = .5, xend = .1, y = .3, yend = .3), 
                     size = 1, colour = "dodgerblue3", linetype = 1,position = "identity") +
    geom_text(aes(label = c("Density"), 
                  x = .6, y = .3, 
                  family = "sans"), 
              hjust = 0, vjust = 0.3,
              color = "gray0", size=4)} 
  }
  
  # Create Cp Legend
  CPM_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(0,1))+
    theme(plot.margin = unit(c(6,1,7,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Potential (Within) Capability"), 
                  x = .5, y = 1, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Cp"), 
                  x = .2, y = .9, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Lower CL"), 
                  x = .2, y = .8, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Upper CL"), 
                  x = .2, y = .7, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("CPL"), 
                  x = .2, y = .6, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("CPU"), 
                  x = .2, y = .5, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Cpk"), 
                  x = .2, y = .4, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Lower CL"), 
                  x = .2, y = .3, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Upper CL"), 
                  x = .2, y = .2, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("CCpk"), 
                  x = .2, y = .1, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    
    geom_text(aes_now(label = sprintf("%.3f",CPS["Cp"]), 
                      x = .6, y = .9, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .8, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .7, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",CPS["CPL"]), 
                      x = .6, y = .6, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",CPS["CPU"]), 
                      x = .6, y = .5, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",CPS["Cpk"]), 
                      x = .6, y = .4, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .3, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .2, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",CPS["CCpk"]), 
                      x = .6, y = .1, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Create Pp Legend
  PPM_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(0,1))+
    theme(plot.margin = unit(c(4,1,2,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Overall Capability"), 
                  x = .5, y = 1, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Pp"), 
                  x = .2, y = .9, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Lower CL"), 
                  x = .2, y = .8, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Upper CL"), 
                  x = .2, y = .7, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPL"), 
                  x = .2, y = .6, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPU"), 
                  x = .2, y = .5, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Ppk"), 
                  x = .2, y = .4, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Lower CL"), 
                  x = .2, y = .3, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Upper CL"), 
                  x = .2, y = .2, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Cpm"), 
                  x = .2, y = .1, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    
    geom_text(aes_now(label = sprintf("%.3f",PPS["Pp"]), 
                      x = .6, y = .9, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .8, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .7, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",PPS["PPL"]), 
                      x = .6, y = .6, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",PPS["PPU"]), 
                      x = .6, y = .5, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",PPS["Ppk"]), 
                      x = .6, y = .4, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .3, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3s","-"), 
                      x = .6, y = .2, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.3f",PPS["Cpm"]), 
                      x = .6, y = .1, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Create Observed Performance Legend
  OBS_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(0.5,1))+
    theme(plot.margin = unit(c(0,0.1,0,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Observed Performance"), 
                  x = .5, y = .975, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM < LSL"), 
                  x = .1, y = .85, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM > LSL"), 
                  x = .1, y = .75, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM Total"), 
                  x = .1, y = .65, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
   
    geom_text(aes_now(label = sprintf("%.2f",PERF["OBLL"]), 
                      x = .6, y = .85, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["OBGU"]), 
                      x = .6, y = .75, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["OBT"]), 
                      x = .6, y = .65, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Create Expected (Overall) Performance Legend
  Eover_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(0.5,1))+
    theme(plot.margin = unit(c(0,.1,0,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Exp. Overall Performance"), 
                  x = .5, y = .975, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM < LSL"), 
                  x = .1, y = .85, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM > LSL"), 
                  x = .1, y = .75, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM Total"), 
                  x = .1, y = .65, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    
    geom_text(aes_now(label = sprintf("%.2f",PERF["POLL"]), 
                      x = .6, y = .85, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["POGU"]), 
                      x = .6, y = .75, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["POT"]), 
                      x = .6, y = .65, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Create Expected (Within) Performance Legend
  Ewith_leg <- {ggplot()+
    xlim(c(0,1))+ylim(c(0.5,1))+
    theme(plot.margin = unit(c(0,0.1,0,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Exp. Within Performance"), 
                  x = .5, y = .975, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM < LSL"), 
                  x = .1, y = .85, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM > LSL"), 
                  x = .1, y = .75, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("PPM Total"), 
                  x = .1, y = .65, 
                  family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    
    geom_text(aes_now(label = sprintf("%.2f",PERF["PWLL"]), 
                      x = .6, y = .85, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["PWGU"]), 
                      x = .6, y = .75, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = sprintf("%.2f",PERF["PWT"]), 
                      x = .6, y = .65, 
                      family = "sans"), 
              hjust = -.0, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Create Q-Q Plot
  y_qq <- quantile(as.numeric(data[,1]),c(.25,.75))
  x_qq <- qnorm(c(.25,.75))
  slope_qq <- diff(y_qq)/diff(x_qq)
  int_qq <- y_qq[1L] - slope_qq * x_qq[1L]
  
  qq <- {ggplot(data, aes(sample=data, shape = 16, color = "red3")) + 
               stat_qq() + scale_shape_identity()+
               geom_abline(slope = slope_qq, intercept = int_qq) +
    theme(plot.margin = unit(c(11,0.1,4,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white", color = "gray0"), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
  }
  
  #Crate Q-Q Plot Legend
  QQ_leg <- {ggplot()+ xlim(c(0,1))+ylim(c(0,1))+
    theme(plot.margin = unit(c(-3,0.1,0,0), "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = NA, color = NA), 
          plot.background = element_rect(fill = NA, color = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(label = c("Q-Q Plot"), 
                  x = .5, y = .95, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Anderson-Darling Test"), 
                  x = .5, y = .25, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes(label = c("Shapiro-Wilk Test"), 
                  x = .5, y = 0.1, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = paste("A = ", sprintf("%.5f",ad.test(x)[[1]]), 
                                    "       p = ", sprintf("%.5f",ad.test(x)[[2]]), sep=""), 
                    x = .5, y = .175, 
                    family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)+
    geom_text(aes_now(label = paste("W = ", sprintf("%.5f",shapiro.test(x)[[1]]), 
                                    "       p = ",sprintf("%.5f",shapiro.test(x)[[2]]), sep=""), 
                  x = .5, y = .025, 
                  family = "sans"), 
              hjust = .5, vjust = 1,
              color = "gray0", size=4)
  }
  
  #Initialize Plot Layout
  #Builds 1x3 grid for title at top, footer at bottom and one large 
  #container in center to hold the ggplot object
  
    #Plot
    grid.newpage()
    vp.canvas<-viewport(name="canvas",
                        width=unit(11,"inches"),
                        height=unit(8.5,"inches"),
                        layout=grid.layout(3,1,
                        heights=unit(c(3,1,1), c("lines", "null", "lines"))))
    pushViewport(vp.canvas)
    grid.rect(gp=gpar(col="gray0", 
                      lwd=0, 
                      fill="cornsilk"))
    
    #Title
    vp.title<-viewport(layout.pos.col=1, 
                       layout.pos.row=1, 
                       name="title")
    pushViewport(vp.title)
    grid.text(paste(main," of ", name, sep = ""), 
              gp=gpar(fontsize=18), 
              just = c("center","bottom"))
    grid.text(paste("\n",sub), 
              gp=gpar(fontsize=15), 
              just = c("center","center"))
    popViewport()
    
    #Footer
    if(footer){
    vp.subtitle<-viewport(layout.pos.col=1, layout.pos.row=3, name="footer")
    pushViewport(vp.subtitle)
    grid.text(R.Version()$version.string, 
              x = unit(1, "lines"), 
              gp=gpar(col="gray3",fontsize=10),
              just = c("left","center"))
    grid.text(format(Sys.time(), "%b %d, %Y"), 
              x = unit(1, "npc") - unit(1,"lines"), 
              gp=gpar(col="gray3",fontsize=10),
              just = c("right","center"))
    popViewport()}
    
    #Container
    vp.container<-viewport(layout.pos.col=1, layout.pos.row=2, name="container")
    pushViewport(vp.container)
    grid.text ( "container")

    #Build the ggplot items
    gt1 <- ggplot_gtable(ggplot_build(p))
    gt2 <- ggplot_gtable(ggplot_build(Proc_leg))
    gt3 <- ggplot_gtable(ggplot_build(Leg_leg))
    gt4 <- ggplot_gtable(ggplot_build(CPM_leg))
    gt5 <- ggplot_gtable(ggplot_build(PPM_leg))
    gt6 <- ggplot_gtable(ggplot_build(OBS_leg))
    gt7 <- ggplot_gtable(ggplot_build(Ewith_leg))
    gt8 <- ggplot_gtable(ggplot_build(Eover_leg))
    gt9 <- ggplot_gtable(ggplot_build(qq))
    gt10 <- ggplot_gtable(ggplot_build(QQ_leg))

    #Disable clipping
    #Required for hanging text above plot
    gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
    
    #Define a 4x4 gtable to hold the objects  
    gt <- gtable(widths = unit(c(1, 1, 1, 1), "null"), 
                 height = unit(c(12, 1, 1, 5), c("lines", "null", "null", "lines")))
    
    #Add items to gtable locations  
    gt <- gtable_add_grob(gt, gt1[,-5], 1, 2, b = 3, r = 3)
    gt <- gtable_add_grob(gt, gt2[,], 1, 1)
    gt <- gtable_add_grob(gt, gt3[,], 1, 4)
    gt <- gtable_add_grob(gt, gt4[,], 1, 4, b = 3)
    gt <- gtable_add_grob(gt, gt5[,], 2, 4, b = 4)
    gt <- gtable_add_grob(gt, gt6[,], 4, 1)
    gt <- gtable_add_grob(gt, gt7[,], 4, 2)
    gt <- gtable_add_grob(gt, gt8[,], 4, 3)
    gt <- gtable_add_grob(gt, gt9[,], 1, 1, b = 3)
    gt <- gtable_add_grob(gt, gt10[,], 2, 1, b= 3)
  
  #Render the plot  
  grid.draw(gt)
 
  
}
  
  #Define output
  output <- list(Proc_Data,CPS,PPS,PERF)
  class(output) <- 'myclass'
  return(output)
}

#Format Text Output
print.myclass <- function(x) {
  cat("Capabiliy Analysis","\n")
  print(noquote(cbind(`Process Data` = unlist(x[[1]]))), digits = 4)
  cat("\n")
  print(noquote(cbind(`Potential Capability` = unlist(x[[2]]))), digits = 4)
  cat("\n")
  print(noquote(cbind(`Overall Capability` = unlist(x[[3]]))), digits = 4)
  cat("\n")
  print(noquote(cbind(`Performance` = unlist(x[[4]]))), digits = 4)
  return(invisible(x))
}