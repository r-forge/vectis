vectis.cap <- function(data,
                       distribution = "normal",
                       USL = NA,
                       LSL = NA,
                       target = NA,
                       main = "Capabilities Analysis",
                       sub = "",
                       groupsize = 1,
                       mrlength = 2,
                       alpha = 0.05,
                       tol = 5.15,
                       unbias_sub = TRUE,
                       unbias_overall = FALSE,
                       density = FALSE,
                       binwidth = -1
                       )
{
  library(ggplot2)
  library(grid)
  
  if (is.na(target)){
  stop("Target not specified")
  }
  if (is.na(LSL) & is.na(USL)){
  stop("Upper and Lower Specification Limits not specified")
  }
  if (groupsize < 1 | groupsize > 50){
  stop("Group Size must be between 1 and 50")
  }
  if (mrlength < 2){
  stop("Moving Range Length must be greater than or equal to 2")
  }
  
  Lookup <-
    structure(list(N = 1:100, 
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
              row.names = c(NA, -100L))
    
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

  if (unbias_overall) {
    S_overall <- sd(data)/(Lookup$c4[length(data[!is.na(data)])])
    } else {
    S_overall <- sd(data)
    }

  mu <- mean(data)
  
  # Process Data
  Proc_Data <- vector(mode = "numeric", length = 8)
  names(Proc_Data) <- c("LSL","Target","USL","Sample Mean","Number of Obs.",
                        "StDev(Within)","StDev(Overall)","Group Size")
  Proc_Data["LSL"] <- LSL
  Proc_Data["Target"] <- target
  Proc_Data["USL"] <- USL
  Proc_Data["Sample Mean"] <- mu
  Proc_Data["Number of Obs."] <- length(data[!is.na(data)])
  Proc_Data["StDev(Within)"] <- S_within
  Proc_Data["StDev(Overall)"] <- S_overall
  Proc_Data["Group Size"] <- groupsize
  
  
  # Potential Capability Matrix
  CPS <- vector(mode = "numeric", length = 5)
  names(CPS) <- c("Cp","CPL", "CPU", "Cpk", "CCpk")
  CPS["Cp"] <- (USL - LSL)/(tol*S_within)
  CPS["CPL"] <- (mu - LSL)/(.5*tol*S_within)
  CPS["CPU"] <- (USL - mu)/(.5*tol*S_within)
  CPS["Cpk"] <- min(CPS["CPU"],CPS["CPL"])
  CPS["CCpk"] <- min(USL-target,target-LSL)/(.5*tol*S_within)
  
  # Overall Capability Matrix
  PPS <- vector(mode = "numeric", length = 5)
  names(PPS) <- c("Pp","PPL", "PPU", "Ppk", "Cpm")
  PPS["Pp"] <- (USL - LSL)/(tol*S_overall)
  PPS["PPL"] <- (mu - LSL)/(.5*tol*S_overall)
  PPS["PPU"] <- (USL - mu)/(.5*tol*S_overall)
  PPS["Ppk"] <- min(PPS["PPU"],PPS["PPL"])
  PPS["Cpm"] <- min(USL-target,target-LSL)/(.5*tol*sd(data))
  
  #Expected Within/Overall/Observed Performance
  PERF <- vector(mode = "numeric", length = 9)
  names(PERF) <- c("PWLL","PWGU","PWT","POLL","POGU","POT","OBLL","OBGU","OBT")
  PERF["PWLL"] <- 1e6*(1-pnorm((mu-LSL)/S_within))
  PERF["PWGU"] <- 1e6*(1-pnorm((USL-mu)/S_within))
  PERF["PWT"] <- sum(PERF["PWLL"],PERF["PWGU"])
  PERF["POLL"] <- 1e6*(1-pnorm((mu-LSL)/S_overall))
  PERF["POGU"] <- 1e6*(1-pnorm((USL-mu)/S_overall))
  PERF["POT"] <- sum(PERF["POLL"],PERF["POGU"]) 
  PERF["OBLL"] <- 1e6*(length(data[data<LSL])/length(data[!is.na(data)]))
  PERF["OBGU"] <- 1e6*(length(data[data>USL])/length(data[!is.na(data)]))
  PERF["OBT"] <- sum(PERF["OBLL"],PERF["OBGU"]) 

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
  
# Create Plots
  data <- as.data.frame(data)
  aes_now <- function(...) {structure(list(...),  class = "uneval")}

  p <- ggplot(data, aes(x = data)) +
              theme(plot.margin = unit(c(3,1,1,1), "lines"), 
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.background = element_rect(fill = NA, color = "gray0"),    
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(size = 15)) + 
       coord_cartesian(ylim = c(0, max(1.05 * dens_max, 1.05 * freq_max, 
                                       1.05 * with_max, 1.05 * over_max)),
                       xlim = c(min(min(data),1.1 * LSL - 0.1 * USL, target - 3 * S_within, 
                                    target - 3 * S_overall),
                                max(max(data),1.1 * USL - 0.1 * LSL, target + 3 * S_within, 
                                    target + 3 * S_overall))) +
           xlim(min(min(data),1.1 * LSL - 0.1 * USL, target - 3 * S_within, target - 3 * S_overall),
                max(max(data),1.1 * USL - 0.1 * LSL, target + 3 * S_within, target + 3 * S_overall)) +
           ylim(0, max(1.05 * dens_max, 1.05 * freq_max, 
                       1.05 * with_max, 1.05 * over_max))

  
  p <- p + geom_histogram(aes(y=..density..),        
                          binwidth = binwidth, 
                          color = "black", fill = "slategray1", position = "identity")
  
  if(density) {p <- p + geom_line(stat="density", size = 1.1, 
                                  color = "dodgerblue3", position="identity")}
  
  p <- p + geom_vline(xintercept = LSL, linetype = 5, size = .65, color = "red3") 
  p <- p + geom_vline(xintercept = target, linetype = 5, size = .65, color = "green3")
  p <- p + geom_vline(xintercept = USL, linetype = 5, size = .65, color = "red3") 
   
  p <- p + geom_text(aes_now(label = c("USL"), x = c(USL), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "red3", size=5)
  p <- p + geom_text(aes_now(label = c("LSL"), x = c(LSL), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "red3", size=5)
  p <- p + geom_text(aes_now(label = c("Target"), x = c(target), y = Inf, family = "sans"), 
                     hjust = .5, vjust = -1, color = "green3", size=5)
  
  p <- p + stat_function(fun = dnorm,args=list(mean = mu, sd = S_within), 
                         color = "red3", size = 1.1, linetype = 1)
  p <- p + stat_function(fun = dnorm,args=list(mean = mu, sd = S_overall), 
                         color = "gray0", size = 1.1, linetype = 2)
  
#   p <- p + opts(panel.background = theme_rect())
  
#   p <- p + annotate(geom = "text", 
#            x = LSL, 
#            y = 0, 
#            label = "LSL", 
#            hjust = -0.1, 
#            size = 5, color = "darkred") 
#   p <- p + annotate(geom = "text",
#            x = target, 
#            y = 0, 
#            label = "TAR",
#            hjust = -0.1,
#            size = 5, color = "green3")
#   p <- p + annotate(geom = "text",
#            x = USL, 
#            y = 0, 
#            label = "USL",
#            hjust = 1.1, 
#            size = 5, color = "darkred") 
  
  # Disable Clipping
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
    
  print(gt)
  
  output <- list(Proc_Data,CPS,PPS,PERF)
  class(output) <- 'myclass'
  return(output)
}

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