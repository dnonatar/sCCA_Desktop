  # Both data must have the same number of samples
file1_name= strsplit(args[1],'[.]')[[1]][1]
file2_name= strsplit(args[2],'[.]')[[1]][1]

input1 <- read.csv(file.path("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/input_store",args[1]), sep = ',')
input1 <- input1[,-1]
input1 <- as.matrix(apply(input1,2,as.numeric))
sd1 <- apply(input1,2,sd)
input1 <- input1[,which(sd1 != 0)]  # choose only OTUs with non-zero standard deviation
#head(t(input1))

input2 <- read.csv(file.path("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/input_store",args[2]), sep = ',')
input2 <- input2[,-1]
input2 <- as.matrix(apply(input2,2,as.numeric))
sd2 <- apply(input2,2,sd)
input2 <- input2[,which(sd2 != 0)]

library(PMA)
set.seed(1105)
ccaPerm = CCA.permute(x = input1, z = input2,
                           typex = "standard", typez = "standard", 
                           nperms = 30, niter = 5, standardize = T)
penXtemp = ccaPerm$bestpenaltyx
penZtemp = ccaPerm$bestpenaltyz
ccaRslt = CCA(x = input1, z = input2,
                   typex = "standard", typez = "standard",
                   penaltyx = penXtemp, penaltyz = penZtemp,
                   K = 2, niter = 5, standardize = T)
sum(ccaRslt$u != 0)
sum(ccaRslt$v != 0)

ccaScoreU = input1 %*% ccaRslt$u
ccaScoreV = input2 %*% ccaRslt$v
ccaScores = cbind(ccaScoreU, ccaScoreV)
colnames(ccaScores) = c("U1", "U2", "V1", "V2")
ccaScores = as.data.frame(ccaScores)
#number of each type should be flexible
len = dim(input1)[1]
ccaScores$type = c(rep("class 1", len/2), rep("class 2", len/2))


library(ggplot2)
myCCAPlot = function(x = U1, y = U2, col = V1, shape = type, data = ccaScores,
                     xyName = file1_name, coloName = file2_name,
                     textVjust = -1.0, elliLev = 0.6, ...){
  jitterPara = list(...)
  if(!"height" %in% names(jitterPara)){
    jitterPara = c(jitterPara, height = 0.01) 
  } else if(!"width" %in% names(jitterPara)){
    jitterPara = c(jitterPara, width = 0.01)
  }
  x = deparse(substitute(x))
  y = deparse(substitute(y))
  col = deparse(substitute(col))
  shape = deparse(substitute(shape))
  myPlot1 = ggplot(data, aes(x = data[,x], y = data[,y],
                             col = data[,col], shape = data[,shape])) +
    geom_point(size = 4) +
    scale_color_continuous(name = paste0("First Component \nScores of ",
                                         coloName),
                           low = "blue", high = "red") +
    geom_text(aes(label = rownames(data)),
              col = "black", size = 5, vjust = textVjust,
              position = do.call("position_jitter", args = jitterPara)) +
    ## The position_jitter will make the values within a group 
    ## a litter bit separate.
    ## On ther other hand, position_dodge will separate the values between groups.
    scale_x_continuous(paste0("First Component Scores of ",
                              xyName)) +
    scale_y_continuous(paste0("Second Component Scores of ",
                              xyName)) +
    labs(title = paste0("Sparse CCA Scores for ", xyName, " as Base")) +
    theme(legend.title = element_text(size = 12),
          plot.title = element_text(size = 16, vjust = 2.0, face = "bold"),
          legend.text = element_text(size = 10)) +
    #stat_ellipse(aes(fill = data[,shape]), level = elliLev, alpha = 0.2,
                 #geom = "polygon", linetype = 2) +
    scale_fill_discrete(name = "Class",
                        labels = c("class 1", "class 2")) +
    scale_shape_discrete(name = "Class",
                         labels = c("class 1", "class 2"))
  myPlot1
}


## Write output files (1 csv + 2 pdf)


pdf(file.path("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/output",paste(file1_name,'.pdf', sep="")),bg="transparent")
myCCAPlot()
dev.off()
pdf(file.path("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/output",paste(file2_name,'.pdf', sep="")),bg="transparent")
myCCAPlot(V1, V2, U1, xyName = file2_name, coloName = file1_name)
dev.off()

outfile = file.path("/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/output",  
                    paste("randname","-", "ccaImmunScores", ".csv",sep=""))
write.csv(x = ccaScores, file = outfile)
