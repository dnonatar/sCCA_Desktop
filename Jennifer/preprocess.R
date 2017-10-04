j1 = read.csv("./Jennifer/cohen_colon-count.csv")
j2 = read.csv("./Jennifer/Cohen-stool.txt-count.csv",header = T)

j1 = j1[order(j1[,1]),]
j2 = j2[order(j2[,1]),]
dim(j1)
dim(j2)
j1[1:5,1]
