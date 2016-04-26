
PROJHOME <- "/home/rstudio/R/clustering/"
csvdir <- "~/daniel/data/"

PROJHOME <- "~/R/budget/"
csvdir <- "~/working data/budget/"

library(readxl)
library(cluster)
library(dendextend)
library(ape)

rawschoolfunding <- read.csv(file.path(csvdir, "FY16 Data for Tableau1.csv"))
rawschoolfunding <- read_excel(file.path(csvdir,"FY16 Data for Tableau.xlsx"))


#########clean#########
remove.accounting.symbols <- function(str1){
  char <- gsub("%", "", str1)
  char <- gsub("[$]", "", char)
  char <- gsub("-", "0", char)
  char <- gsub(",", "", char)
  num <- as.numeric(char)
  return(num)
}

remove.accounting.from.df.vars <- function(df, li.vars){
  for(var in li.vars){
    df[[var]] <- remove.accounting.symbols(df[[var]])
  }
  return(df)
}

largeschools <- c("20K490", "31R440", "28Q505", "26Q495")




colnames(rawschoolfunding) <- c(colnames(rawschoolfunding)[1:3], "FSF", "numstudents", "pctIEP", 
                                "salary", "titleone", "AIDP", "prioritydollars", colnames(rawschoolfunding)[11:16]) 
rawschoolfunding <- rawschoolfunding[-c(71:73),]

rawschoolfunding <- remove.accounting.from.df.vars(rawschoolfunding, c(4:7, 9:16))
rawschoolfunding$Portfolio <- factor(rawschoolfunding$Portfolio)
rawschoolfunding$titleone <- factor(rawschoolfunding$titleone)
rawschoolfunding$is_title_one <- logical(length(rawschoolfunding$titleone))
rawschoolfunding[rawschoolfunding$titleone == "Non Title I",]$is_title_one <- FALSE
rawschoolfunding[rawschoolfunding$titleone == "Title I",]$is_title_one <- TRUE
rawschoolfunding$titleone <- NULL
#########support functions#########
fn.create.is.zero <- function(df, li.vars){
  colnms <- colnames(df)[li.vars]
  for(colm in colnms){
    newcolm <- paste0("is_", colm, "_zero")
    df[[newcolm]] <- df[[colm]]==0
  }
  return(df)
}

fn.per.capita <- function(df, li.vars){
  colnms <- colnames(df)[li.vars]
  for(colm in colnms){
    df[[colm]] <- df[[colm]] / df[["numstudents"]]
  }
  return(df)  
}

#########create#########
rawschoolfunding <- fn.create.is.zero(rawschoolfunding, c(8:15))
rawschoolfunding$isSIGall_zero <- rawschoolfunding$`is_SIG - Campus_zero` & rawschoolfunding$is_SIG_zero
rawschoolfunding$isTLall_zero <- rawschoolfunding$`is_TL Safe School_zero` & rawschoolfunding$`is_TL Transitional Relief_zero`

#########impute#########
rawschoolfunding[is.na(rawschoolfunding$salary),]$salary <- median(rawschoolfunding$salary)

# per student funding
rawschoolfunding <- fn.per.capita(rawschoolfunding, c(8:15))
rawschoolfunding[, c("FSF", "pctIEP", "salary")] <- scale(rawschoolfunding[, c("FSF", "pctIEP", "salary")] , center = TRUE, scale = T)

#########remove schools from clustering#########
rawschoolfunding$is_largeSchool <- ifelse(rawschoolfunding$dbn %in% largeschools, T, F )
rawschoolfunding$is_CTE_or_T <- ifelse(rawschoolfunding$Portfolio %in% c("CTE", "T"), T, F) 
rawschoolfunding$is_sixtwelve <- ifelse(rawschoolfunding$Portfolio %in% c("6-12"), T, F) 

str_cond_is612 <- "& rawschoolfunding$is_sixtwelve ==F"
for (weight_sixtwelve in c(0.1,1,10)){
for (weight_scaledvars in c(0.001,1,1000,10000)){
  
  for(i in c("")){            #"", str_cond_is612 
    schoolfunding <- rawschoolfunding
    schoolfunding[, c("FSF", "pctIEP", "salary")] <- (rawschoolfunding[, c("FSF", "pctIEP", "salary")] ) * weight_scaledvars
    schoolfunding$numstudents <- NULL
    schoolfunding <- schoolfunding[
      eval(parse( text = 
                    paste0("rawschoolfunding$is_largeSchool ==F & rawschoolfunding$is_CTE_or_T ==F" , i ))), ]
    
    numschools <- length(schoolfunding$dbn)
    #& rawschoolfunding$is_sixtwelve ==F
    
    textissixtwelve <-  ifelse(schoolfunding$is_sixtwelve, "(6-12)", "")
    row.names(schoolfunding) <- paste0(textissixtwelve, schoolfunding$School)
    
    #########gower#########
    len_otherweights <- length(schoolfunding) - 6
    g.dist = daisy(schoolfunding[3:length(schoolfunding)], metric="gower", weights = 
                     c(weight_sixtwelve, rep.int(weight_scaledvars,3), rep.int(1,len_otherweights)) ) 
    mostattributes(g.dist) <- list(dim = c(numschools,numschools) , Labels = row.names(schoolfunding),
                                            Diag = FALSE,Upper= FALSE,method= "euclidean", Size = numschools)
  
    #########cluster#########
    hc.c = hclust(g.dist, method="complete")
#     hc.m = hclust(g.dist, method="median")
#     hc.a = hclust(g.dist, method="average")
#     #########plot version 2#########
#     pdf(file.path(csvdir, paste0(i, "plota.pdf")), paper = "USr")
#     plot(as.phylo(hc.a), cex = 0.5, label.offset = 0)
#     dev.off()
#     
#     pdf(file.path(csvdir, paste0(i, "plotm.pdf")), paper = "USr")
#     plot(as.phylo(hc.m), cex = 0.5, label.offset = 0)
#     dev.off()
    par(mar=c(1,1,1,1))
    pdf(file.path(csvdir, paste0(i,weight_sixtwelve,"_", weight_scaledvars  , "plotc.pdf")), width=10, height=10, paper = "USr")
    plot(as.phylo(hc.c), cex = 0.5, label.offset = 0)
    title(main = paste0("6-12 has weight ", weight_sixtwelve, ". And Salary/IEP/FSF has weight ", weight_scaledvars))
    dev.off()
  }
}
}








#& rawschoolfunding$is_sixtwelve ==F
schoolfunding <- rawschoolfunding[rawschoolfunding$is_largeSchool ==F & rawschoolfunding$is_CTE_or_T ==F & rawschoolfunding$is_sixtwelve ==F , ]
numschools <- length(schoolfunding$dbn
#& rawschoolfunding$is_sixtwelve ==F
#

row.names(schoolfunding) <- schoolfunding$School
#########gower#########
g.dist = daisy(schoolfunding[3:length(schoolfunding)], metric="gower" ) 
hc.m = hclust(g.dist, method="median")



#########cluster#########
hc.c = hclust(g.dist, method="complete")

#########cluster#########
hc.a = hclust(g.dist, method="average")

plot(as.phylo(hc.a), cex = 0.5, label.offset = 0)
plot(as.phylo(hc.m), cex = 0.5, label.offset = 0)
plot(as.phylo(hc.c), cex = 0.5, label.offset = 0)


# #########plot version 1#########
# 
# source("dendrogram.R")
# #windows(height=12, width=24)
# layout(matrix(1:1, nrow=1))
# #layout(matrix(1:3, nrow=1))
# RIGHTMARGIN <- 15
# par(mar = c(3,3,3,RIGHTMARGIN))
# plot(dend.a, horiz =  TRUE)
# par(mar = c(3,3,3,RIGHTMARGIN))
# plot(dend.m, horiz =  TRUE)
# par(mar = c(3,3,3,RIGHTMARGIN))


par(mar = c(3,3,3,RIGHTMARGIN))
plot(dend.c, 
     main = "schools", 
     horiz =  TRUE,  nodePar = list(cex = .007))
