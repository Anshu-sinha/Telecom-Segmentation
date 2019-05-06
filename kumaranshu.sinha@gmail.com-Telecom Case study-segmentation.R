# Clearing the global environment
rm(list = ls())

# setting the directory and Reading the file
setwd("F:/BA-R programming/STAT case study/2. TELECOM CASE STUDY - SEGMENTATION")
telco <- read.csv("Telco Segmentation Data.csv",header = T)

#Exploring data
View(telco)
str(telco)
names(telco)

#Converting factor variables into dummy
telco$GENDER <- as.factor(telco$GENDER)
library(caret)
dummy <-  dummyVars(~GENDER, data = telco)
dummy_Gender <- data.frame(predict(dummy,telco))
View(dummy_Gender)

# Joining the dummy_Gender with the original file
telco_new <- data.frame(cbind(telco,dummy_Gender))

# Removing the unwanted data not required
telco_new$CUST_ID <- NULL
telco_new$GENDER <- NULL

# user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

# Applying the mystats func onto telco_new
Diag_stat <- sapply(telco_new,FUN = mystats)
write.csv(Diag_stat,"Diag_stat.csv")

#Outlier Treatment
Outlier_treat <- function(x){
  UC1 = quantile(x,p=0.99,na.rm = T)
  LC1 = quantile(x,p=0.01,na.rm = T)
  
  x= ifelse(x>UC1,UC1,x)
  x= ifelse(x<LC1,LC1,x)
  return(x)
}

# Applying the outlier treatment for the telco_new datadata
telco_new <- data.frame(sapply(telco_new,FUN = Outlier_treat))
View(telco_new)

#Let's create another data from telco_new
input_data <- telco_new


                                    ## Factor Analysis##
# corelation Matrix
corrm<- cor(telco_new)                                 
View(corrm)

# Loading the required Packages
require(psych)
require(GPArotation)

# Deciding number of factors using scree plot & kaiser test(number of eigen values over 1)
# Ploting the scree
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 

#Eigen Values
eigen(corrm)$values                                                     
require(dplyr)

# Calculating the Variance, Cumulative Variance, Percent Values and Cumulative Percent Values 
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  

View(eigen_values)

# Exporting the Eigen Summary
write.csv(eigen_values, "EigenValues.csv")  

# Conducting Factor Analysis
fact <- fa(r=corrm, 9, rotate="varimax", fm="minres") # "ml" was not working properly here why???
print(fact) 
  # Sorting the Loading
fact_SORT<-fa.sort(fact)                                         
ls(fact_SORT)                                                  
fact_SORT$loadings
  # Finding the eigen values from the result
fact_SORT$e.values                                            

# Capturing the Loading into data frame
Loadings<-data.frame(fact_SORT$loadings[1:ncol(telco),]) 
View(Loadings)
write.csv(Loadings, "loadings.csv")


#Preparation of final Data
#standardizing the whole data
inputdata_final = scale(input_data)
View(inputdata_final)

#-------------------------------------------------------------------------------------------------------------
# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()

minClust <- 3      # Hypothesized minimum number of segments
maxClust <- 12      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11)            # For reproducibility
  km.out[i] <- list(kmeans(inputdata_final, centers = centr))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(inputdata_final)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

#Next, we plot the silhouette average widths for the choice of clusters. The best cluster is the one with the 
#largest silhouette average width, which turns out to be 5 clusters.

# Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

#-------------------------------------------------------------------------------------------------------------

#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
# cluster_six <- kmeans(inputdata_final,6)

cluster_three$cluster
cluster_three$size
cluster_three$iter

telco_clust<-cbind(telco_new,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                 km_clust_5=cluster_five$cluster) # ,km_clust_6=cluster_six$cluster)
View(telco_clust)

#Graph based on k-means - Optional
require(cluster)
clusplot(inputdata_final, cluster_five$cluster, color = TRUE, lines =6,labels = 2)

#Profiling
#Converting into factors
telco_clust$km_clust_3=factor(telco_clust$km_clust_3)
telco_clust$km_clust_4=factor(telco_clust$km_clust_4)
telco_clust$km_clust_5=factor(telco_clust$km_clust_5)
#telco_clust$km_clust_6=factor(telco_clust$km_clust_6)

require(tables)
profile<-tabular(1+VOICE_OUT_CALLS+OUT_COMMUNITY_VOICE+OUT_CALLS_NONWORK+IN_CALLS_PEAK+VOICE_OUT_MINS+
                   OUT_COMMUNITY_SMS+GENDER.M+OUT_MINS_ROAMING+OUT_CALLS_INTERNATIONAL+DAYS_OUT+ EVENTS_CALLS ~
                   mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5), #+(mean*km_clust_6),
                 data=telco_clust)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5),#+(length*km_clust_6),
                 data=telco_clust)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)




