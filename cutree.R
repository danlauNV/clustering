# USERMIN, USERMAX
# #not using userdistance : elseideal is min of (USERDISTANCE equivalent, USERMAX)
# if USERDISTANCE equivalent is < USERMIN, then use  USERMIN
# 
# for each cut, for each school, find size of peer group 
# #if there exists peer group size is between USERMIN and USERMAX, use the largest one
# #else use the peer group that is largest under the USERMIN, and add number-short
# 
# to add: 
# find the numbershort + largest size(not including me) closest in the similarity matrix
# union this with the peer group

# for each cut, for each school, find size of peer group 

# #if there exists peer group size is between USERMIN and USERMAX, use the largest one
# #else use the peer group that is largest that is under the USERMIN, and add number-short
# 
# to add: 
# find the numbershort + largest size(not including me) closest in the similarity matrix
# union this with the peer group



#####################ARCHIVE #########################
numschools <- length(attributes(g.dist)$Labels)
m.dist <- mat.or.vec(numschools,numschools )
m.dist[lower.tri(m.dist, diag=FALSE)] <- g.dist
m.dist <- m.dist + t(m.dist) - diag(diag(m.dist)) 
#
indx <- which(attributes(g.dist)$Labels == "HERITAGE SCHOOL, THE")
m.dist[,indx ] 
m.dist[,indx ][order(m.dist[,indx])]

attributes(g.dist)$Labels[[40]]

