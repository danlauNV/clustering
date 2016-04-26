#threedends
library(dendextend)
fn.dend <- function(hc){
  dend <- as.dendrogram(hc)
  # order it the closest we can to the order of the observations:
  #dend <- rotate(dend, 1:150)
  
  # Color the branches based on the clusters:
  dend <- color_branches(dend, k=7) #, groupLabels=iris_species)
  
  # Manually match the labels, as much as possible, to the real classification of the flowers:
  # labels_colors(dend) <-
  #   rainbow_hcl(3)[sort_levels_values(
  #     as.numeric(attributes(schools.distns)$names)[order.dendrogram(dend)]
  #   )]
  
  # We shall add the flower type to the labels:
  textissixtwelve <-  ifelse(schoolfunding$is_sixtwelve, "(6-12)", "")
  labels(dend) <- paste(
    textissixtwelve[order.dendrogram(dend)], 
    as.character(schoolfunding$School)[order.dendrogram(dend)],
    #                       as.character(school.data$schoolNickname)[order.dendrogram(dend)],
    #                       as.character(school.data$nvCic)[order.dendrogram(dend)],
    
    sep = " ")
  # We hang the dendrogram a bit:
  dend <- hang.dendrogram(dend,hang_height=0.1)
  # reduce the size of the labels:
  # dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.5)
  return(dend)
}

#
#########cluster#########
hc.m = hclust(g.dist, method="median")
dend.m <- fn.dend(hc.m)


#########cluster#########
hc.c = hclust(g.dist, method="complete")
dend.c <- fn.dend(hc.c)

#########cluster#########
hc.a = hclust(g.dist, method="average")
dend.a <- fn.dend(hc.a)








# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(dendextend)
# dend <- as.dendrogram(hc)
# # order it the closest we can to the order of the observations:
# dend <- rotate(dend, 1:150)
# 
# # Color the branches based on the clusters:
# dend <- color_branches(dend, k=7) #, groupLabels=iris_species)
# 
# # Manually match the labels, as much as possible, to the real classification of the flowers:
# # labels_colors(dend) <-
# #   rainbow_hcl(3)[sort_levels_values(
# #     as.numeric(attributes(schools.distns)$names)[order.dendrogram(dend)]
# #   )]
# 
# # We shall add the flower type to the labels:
# labels(dend) <- paste(as.character(schoolfunding$School)[order.dendrogram(dend)],
#                       #                       as.character(school.data$schoolNickname)[order.dendrogram(dend)],
#                       #                       as.character(school.data$nvCic)[order.dendrogram(dend)],
#                       #                       "(",school.data$num_algstudents[order.dendrogram(dend)],")", 
#                       sep = " ")
# # We hang the dendrogram a bit:
# dend <- hang.dendrogram(dend,hang_height=0.1)
# # reduce the size of the labels:
# # dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
# dend.c <- set(dend, "labels_cex", 0.5)
# 
# 
# #########cluster#########
# hc = hclust(g.dist, method="average")
# RIGHTMARGIN <- 15
# library(dendextend)
# dend <- as.dendrogram(hc)
# # order it the closest we can to the order of the observations:
# dend <- rotate(dend, 1:150)
# 
# # Color the branches based on the clusters:
# dend <- color_branches(dend, k=7) #, groupLabels=iris_species)
# 
# # Manually match the labels, as much as possible, to the real classification of the flowers:
# # labels_colors(dend) <-
# #   rainbow_hcl(3)[sort_levels_values(
# #     as.numeric(attributes(schools.distns)$names)[order.dendrogram(dend)]
# #   )]
# 
# # We shall add the flower type to the labels:
# labels(dend) <- paste(as.character(schoolfunding$School)[order.dendrogram(dend)],
#                       #                       as.character(school.data$schoolNickname)[order.dendrogram(dend)],
#                       #                       as.character(school.data$nvCic)[order.dendrogram(dend)],
#                       #                       "(",school.data$num_algstudents[order.dendrogram(dend)],")", 
#                       sep = " ")
# # We hang the dendrogram a bit:
# dend <- hang.dendrogram(dend,hang_height=0.1)
# # reduce the size of the labels:
# # dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
# dend.s <- set(dend, "labels_cex", 0.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# hc.m = hclust(g.dist, method="median")
# hc.s = hclust(g.dist, method="single")
# hc.c = hclust(g.dist, method="complete")