#load probabilistic web
pnet <- read.csv(file = "data/Med_probmet.csv") 
head(pnet)
#convert to matrix
rownames(pnet) <- pnet$Pollinator
pnet <- as.matrix(pnet[,-1])
head(pnet)

#explore
hist(pnet)
mean(pnet) #almost 0
hist(pnet/max(pnet))


#lets create a fake metaweb

pnet <- matrix(runif(50*50, 0, 1), nrow = 50, ncol = 50)
hist(pnet) #not realistic...
rownames(pnet) <- paste("animal",1:50)
colnames(pnet) <- paste("plant",1:50)

#lets predict a given community
animals <- paste("animal",1:10)
plants <- paste("plant",1:10)

#subset
pnet_i <- pnet[animals, plants]

#predict ideal community-----
#rbinom(1, 1, 0.5)
pnet_r <- matrix(rbinom(length(pnet_i), size = 1, prob = pnet_i),
                  nrow = nrow(pnet_i), ncol = ncol(pnet_i))

#now I can calculate connectance
c_pnet_r <- sum(pnet_r)/length(pnet_r)

#and I can do it several times and calculate mean and se.

#predict real community-----
#make probability of occurring
animals2 <- data.frame(animals, a_abundance = runif(10))
plants2 <- data.frame(plants, p_abundance = runif(10))

#calculate new probabilities as p(i)*p(j)*p(ij)
K <- animals2$a_abundance %o% plants2$p_abundance
pnet_ab <- pnet_i * K #this creates low p(ij), reescale?

pnet_r <- matrix(rbinom(length(pnet_ab), size = 1, prob = pnet_ab),
                 nrow = nrow(pnet_ab), ncol = ncol(pnet_ab))

#now I can calculate connectance
c_pnet_r <- sum(pnet_r)/length(pnet_r)

#and I can do it several times and calculate mean and se.

#fix total interacions-----
#Let assume we will record 200 interactions. How are those likely to be distributed?

N_links <- 200 #sampling effort.
#N_links should be proportional to mean p(ij), this is because otherwise, 
#communities with low abundance will show similar patterns, but the probabilty 
#of encounter is not the same.
N_links <- round(1000 * mean(pnet_ab), 0) #informed by real networks total expected interactions

#Which p(i), p(j) or p(ij) better describe the orginal network can be tested using Z-scores.
pnet_ab_r <- matrix(rmultinom(n = 1, size = N_links, prob = pnet_ab),
                       nrow = nrow(pnet_ab), ncol = ncol(pnet_ab))
#now I can calculate connectance
c_pnet_ab_r <- length(which(pnet_ab_r > 0))/length(pnet_ab_r)

#now let's correct for non independency of the p(ij)----

#Redistribute their links WITHIN species according to 
  #in and out degrees
  #col and row sums
  #indirect paths of length n
#Test if this optimize the network or not in an iterative proces.
colSums(pnet_ab_r)
rowSums(pnet_ab_r)

#Alternativelly, one can calculate error rates per ij and see where the errors are made
  #cells with large col/rowsums?








