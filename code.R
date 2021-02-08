# import dataset

Demograph <- read_excel("C:/Users/Mudit/Desktop/EXL/Data.xlsx", 
                        +     sheet = "Demographics")
View(Demograph)
Demograph<-Demograph[,-2]


####------------ convert multi category vars to binary vars (in this case marital status)---------------------- 

# first converting chars to factors
Demograph$Marital_Status2<-factor(Demograph$Marital_Status)
#  now to binary categorical vars
demo<-cbind(with(Demograph, model.matrix(~ Marital_Status2 + 0)))
str(demo)
demo<-as.data.frame(demo)
# passed to original demograph database
Demograph$Divorced<-factor(demo$Marital_Status2D)
Demograph$Married<-factor(demo$Marital_Status2M)
Demograph$Single<-factor(demo$Marital_Status2S)
# cleaning original demograph to get only required vars
Demograph_new<-Demograph[,c(1:3,5:6,8:10)]
str(Demograph_new)
# convert gender to factors
Demograph_new$Gender<-factor(Demograph_new$Gender,label=c("F","M"))
Demograph_new$Owns_Home<-factor(Demograph_new$Owns_Home,label=c("N","Y"))

#####------------END----------------------------------------------------------

# ----------------importing rest of the databases--------------------------

Ping_bad <- read_excel("/media/mudit/4CB0792EB079201E/Users/Mudit/Desktop/EXL/Data.xlsx", 
                  +     sheet = "Ping_Information")
Cat_Map <- read_excel("/media/mudit/4CB0792EB079201E/Users/Mudit/Desktop/EXL/Data.xlsx", 
                      +     sheet = "Category Mapping")
Category_Mapping<-Cat_Map
Store_Map <- read_excel("/media/mudit/4CB0792EB079201E/Users/Mudit/Desktop/EXL/Data.xlsx", 
                        +     sheet = "Store Mapping")
Store_Mapping<-Store_Map

#  ----------importing done--------------------------------------

## Cleaning 

# Outliers analysis of pings
source("https://goo.gl/4mthoF")
ping_remo<-ping_bad
outlierKD(ping_remo, lat)
ping_remo<-na.omit(ping_remo)

#Removing Outer Points#
ping1<-ping_remo[ping_remo$lng< -122.405 & ping_remo$lng> -122.408,]
min(ping1$lng)
pingNew<-ping1[ping1$lat> 37.783 & ping1$lng< 37.785,]
remove(ping1)
Ping<-pingNew

#Strip time and date
Ping$TimeDate<-strptime(Ping$seenTimestamp, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ")
Ping$Date<-as.Date(Ping$TimeDate)
pingNew<-Ping

#Store segregation on the basis of floors
store_0<-Store_Mapping[Store_Mapping$Floor_name=="Concourse Level",]
store_1<-Store_Mapping[Store_Mapping$Floor_name=="Level 1",]
store_2<-Store_Mapping[Store_Mapping$Floor_name=="Level 2",]
store_3<-Store_Mapping[Store_Mapping$Floor_name=="Level 3",]
store_4<-Store_Mapping[Store_Mapping$Floor_name=="Level 4/N1",]

## Floor0

Ping_0<-pingNew[pingNew$floor=="Concourse",]
#finding nearest neighbours of pings at Concourse level
k <- knn(store_0[,4:5], Ping_0[,3:4],store_0$Store_Name, k = 1, algorithm="brute")
indices <- attr(k, "nn.index")

#Store Allocated to each ping
Ping_0$StoreAlloted<-store_0$Store_Name[indices]
result_0<-Ping_0[,c(1,2,9,10)]
# getting it in the required form with total pings and date visited
## result_0_2<-count(result_0, vars=c("Shopper_ID","StoreAlloted","Date"))
library(plyr)
result_0_1<-count(result_0, vars=c("Shopper_ID","floor","StoreAlloted","Date"))
## result_0_3<-aggregate(result_0_1$freq,by=list(result_0_1$Shopper_ID,result_0_1$StoreAlloted),sum)
result_0_5<-aggregate(result_0_1$freq,by=list(result_0_1$Shopper_ID,result_0_1$floor,result_0_1$StoreAlloted),sum)
result_0_6<-ddply(result_0_1,.(Shopper_ID,floor,StoreAlloted),nrow)
library(gtools)
# sort 
result_0_7<-result_0_5[mixedorder(result_0_5$Group.1),]
result_0_6<-result_0_6[mixedorder(result_0_6$Shopper_ID),]
result_0_6$TotalPings<-result_0_7$x
names(result_0_6)<-c("Shopper_ID","Floor","StoreAlloted","DaysVisited","TotalPings")
Floor0<-result_0_6

# Floor1

Ping_1<-pingNew[pingNew$floor=="Floor 1",]
store_1<-Store_Mapping[Store_Mapping$Floor_Index==1,]

k <- knn(store_1[,4:5], Ping_1[,3:4],store_1$Store_Name, k = 1, algorithm="brute")
indices <- attr(k, "nn.index")


Ping_1$StoreAlloted<-store_1$Store_Name[indices]
result_1<-Ping_1[,c(1,2,9,10)] 

result_1_1<-count(result_1, vars=c("Shopper_ID","floor","StoreAlloted","Date"))
result_1_2<-aggregate(result_1_1$freq,by=list(result_1_1$Shopper_ID,result_1_1$floor,result_1_1$StoreAlloted),sum)
result_1_3<-ddply(result_1_1,.(Shopper_ID,floor,StoreAlloted),nrow)

result_1_2<-result_1_2[mixedorder(result_1_2$Group.1),]
result_1_3<-result_1_3[mixedorder(result_1_3$Shopper_ID),]
result_1_3$TotalPings<-result_1_2$x
names(result_1_3)<-c("Shopper_ID","Floor","StoreAlloted","DaysVisited","TotalPings")
Floor1<-result_1_3

# Floor2

Ping_2<-pingNew[pingNew$floor=="Floor 2",]
store_2<-Store_Mapping[Store_Mapping$Floor_Index==2,]

k <- knn(store_2[,4:5], Ping_2[,3:4],store_2$Store_Name, k = 1, algorithm="brute")
indices <- attr(k, "nn.index")

Ping_2$StoreAlloted<-store_2$Store_Name[indices]
result_2<-Ping_2[,c(1,2,9,10)] 

result_2_1<-count(result_2, vars=c("Shopper_ID","floor","StoreAlloted","Date"))
result_2_2<-aggregate(result_2_1$freq,by=list(result_2_1$Shopper_ID,result_2_1$floor,result_2_1$StoreAlloted),sum)
result_2_3<-ddply(result_2_1,.(Shopper_ID,floor,StoreAlloted),nrow)

result_2_2<-result_2_2[mixedorder(result_2_2$Group.1),]
result_2_3<-result_2_3[mixedorder(result_2_3$Shopper_ID),]
result_2_3$TotalPings<-result_2_2$x
names(result_2_3)<-c("Shopper_ID","Floor","StoreAlloted","DaysVisited","TotalPings")
Floor2<-result_2_3

# Floor3

Ping_3<-pingNew[pingNew$floor=="Floor 3",]
store_3<-Store_Mapping[Store_Mapping$Floor_Index==3,]

k <- knn(store_3[,4:5], Ping_3[,3:4],store_3$Store_Name, k = 1, algorithm="brute")
indices <- attr(k, "nn.index")

Ping_3$StoreAlloted<-store_3$Store_Name[indices]
result_3<-Ping_3[,c(1,2,9,10)] 

result_3_1<-count(result_3, vars=c("Shopper_ID","floor","StoreAlloted","Date"))
result_3_2<-aggregate(result_3_1$freq,by=list(result_3_1$Shopper_ID,result_3_1$floor,result_3_1$StoreAlloted),sum)
result_3_3<-ddply(result_3_1,.(Shopper_ID,floor,StoreAlloted),nrow)

result_3_2<-result_3_2[mixedorder(result_3_2$Group.1),]
result_3_3<-result_3_3[mixedorder(result_3_3$Shopper_ID),]
result_3_3$TotalPings<-result_3_2$x
names(result_3_3)<-c("Shopper_ID","Floor","StoreAlloted","DaysVisited","TotalPings")
Floor3<-result_3_3

# FLOOR 4

Ping_4<-pingNew[pingNew$floor=="Floor 4",]
store_4<-Store_Mapping[Store_Mapping$Floor_Index==4,]

k <- knn(store_4[,4:5], Ping_4[,3:4],store_4$Store_Name, k = 1, algorithm="brute")
indices <- attr(k, "nn.index")
#dist<-attr(k,"nn.dist")


Ping_4$StoreAlloted<-store_4$Store_Name[indices]
result_4<-Ping_4[,c(1,2,9,10)] 

result_4_1<-count(result_4, vars=c("Shopper_ID","floor","StoreAlloted","Date"))
result_4_2<-aggregate(result_4_1$freq,by=list(result_4_1$Shopper_ID,result_4_1$floor,result_4_1$StoreAlloted),sum)
result_4_3<-ddply(result_4_1,.(Shopper_ID,floor,StoreAlloted),nrow)

result_4_2<-result_4_2[mixedorder(result_4_2$Group.1),]
result_4_3<-result_4_3[mixedorder(result_4_3$Shopper_ID),]
result_4_3$TotalPings<-result_4_2$x
names(result_4_3)<-c("Shopper_ID","Floor","StoreAlloted","DaysVisited","TotalPings")
Floor4<-result_4_3


#combine rows

Allfloors<-do.call("rbind",list(Floor0,Floor1,Floor2,Floor3,Floor4))

#############Finding max for each shopper###########

# use aggregate to create new data frame with the maxima
TM.agg <- aggregate(Score ~ Shopper, TopStore, max)
# then simply merge with the original
TM.max <- merge(TM.agg, TopStore)

TM.max<-TM.max[mixedorder(TM.max$Shopper),]
TopVisitedStore<-TM.max
n_occur <- data.frame(table(TopVisitedStore$Shopper))
n_occur[n_occur$Freq > 1,]
############Resolving Ties###########
TopVisitedStore<-TopVisitedStore[-28,]
TopVisitedStore<-TopVisitedStore[-c(38,39),]
TopVisitedStore<-TopVisitedStore[-66,]
TopVisitedStore<-TopVisitedStore[-193,]
TopVisitedStore<-TopVisitedStore[-207,]
TopVisitedStore<-TopVisitedStore[-289,]
TopVisitedStore<-TopVisitedStore[-293,]
TopVisitedStore<-TopVisitedStore[-474,]
##############Ties Resolved###################
ResultTopVisited<-TopVisitedStore[,c(1,3)]

bloomingdale<-TopVisitedStore[TopVisitedStore$Store=="Bloomingdale's",]
b1<-bloomingdale[,c(1,3)]


#Scoring Constraint 1 ([1,1],[2,2] and similar cases)
#Learn conditional updation of values
TopStore1<-TopStore#To prevent overriding in TopStore
TopStore1$ScoreNew[(!TopStore1$Score%%110)] <- TopStore1$Score[(!TopStore1$Score%%110)] - 100 
TopStore1$ScoreNew[(TopStore1$Score%%110)!=0]<-TopStore1$Score[(TopStore1$Score%%110)!=0]

#Scoring Constraint 2 (Bloomingdale's Cap 20)
DaysVisited.agg <- aggregate(DaysVisited ~ Shopper, TopStore1, max)
DaysVisited.max <- merge(DaysVisited.agg, TopStore1)
d1<- DaysVisited.max[,c(1,2)]
d1<-unique(d1)

try <- merge(TopStore1,b1,by="Shopper", all=TRUE)
try1 <- merge(try,d1,by="Shopper")
try1$Store.y[is.na(try1$Store.y)] <- "NB"
try1B<-try1[try1$Store.x=="Bloomingdale's",]
try1B_1<-try1B[,-c(4,5,6,9)]
try1$ScoreNewBloom<- ifelse(try1$Store.y=="Bloomingdale's",
                            ifelse(try1$Store.x=="Bloomingdale's",
                                   ifelse(try1$DaysVisited.x<try1$DaysVisited.y,
                                          101*try1$DaysVisited.x+9*20, 101*try1$DaysVisited.x+9*try1$TotalPings),
                                   101*try1$DaysVisited.x+9*try1$TotalPings),
                            101*try1$DaysVisited.x+9*try1$TotalPings)


try2<-try1[,c(1,2,5,6,9)]
names(try2)<-c("Shopper", "Store", "Score","Score1","ScoreB")
try2$ScoreB1[(!try2$ScoreB%%110)] <- try2$ScoreB[(!try2$ScoreB%%110)] - 100  
try2$ScoreB1[(try2$ScoreB%%110)!=0]<-try2$ScoreB[(try2$ScoreB%%110)!=0]

try2B<-try2[try2$Store=="Bloomingdale's",]
try2B$Score-try2B$ScoreB1
try3<-try2[,c(1,2,6)]
#############Finding max for each shopper###########

# use aggregate to create new data frame with the maxima
TM.agg <- aggregate(ScoreB1 ~ Shopper, try2, max)
# then simply merge with the original
TM.max <- merge(TM.agg, try2)

TM.max<-TM.max[mixedorder(TM.max$Shopper),]
TopVisitedStoreNew<-TM.max
n_occur <- data.frame(table(TopVisitedStoreNew$Shopper))
n_occur[n_occur$Freq > 1,]

############Resolving Ties###########
TopVisitedStoreNew<-TopVisitedStoreNew[-12,]
TopVisitedStoreNew<-TopVisitedStoreNew[-12,]
TopVisitedStoreNew<-TopVisitedStoreNew[-13,]
TopVisitedStoreNew<-TopVisitedStoreNew[-13,]
TopVisitedStoreNew<-TopVisitedStoreNew[-16,]
TopVisitedStoreNew<-TopVisitedStoreNew[-27,]
TopVisitedStoreNew<-TopVisitedStoreNew[-38,]
TopVisitedStoreNew<-TopVisitedStoreNew[-38,]
TopVisitedStoreNew<-TopVisitedStoreNew[-66,]
TopVisitedStoreNew<-TopVisitedStoreNew[-103,]
TopVisitedStoreNew<-TopVisitedStoreNew[-103,]
TopVisitedStoreNew<-TopVisitedStoreNew[-160,]
TopVisitedStoreNew<-TopVisitedStoreNew[-162,]
TopVisitedStoreNew<-TopVisitedStoreNew[-206,]
TopVisitedStoreNew<-TopVisitedStoreNew[-210,]
TopVisitedStoreNew<-TopVisitedStoreNew[-226,]
TopVisitedStoreNew<-TopVisitedStoreNew[-290,]
TopVisitedStoreNew<-TopVisitedStoreNew[-314,]
TopVisitedStoreNew<-TopVisitedStoreNew[-341,]
TopVisitedStoreNew<-TopVisitedStoreNew[-369,]
TopVisitedStoreNew<-TopVisitedStoreNew[-419,]
TopVisitedStoreNew<-TopVisitedStoreNew[-473,]
TopVisitedStoreNew<-TopVisitedStoreNew[-496,]
TopVisitedStoreNew<-TopVisitedStoreNew[-497,]
##############Ties Resolved###################
TopVisitedStores<-TopVisitedStoreNew[,c(1,3)]

table(TopVisitedStores$Store)

### Fine Category #####

# 7 stores had discrepancy in the way annd was written in apparel and accessories.so fixed
Category_Mapping$Fine_Category[Category_Mapping$Fine_Category=="Apparel and Accessories"]="Apparel And Accessories"
TopStore<- merge(TopStore, try3, by=c("Shopper" , "Store"))

TopStore2<-TopStore

TopStore2$Fine_Cat<-Category_Mapping$Fine_Category[match(TopStore2$Store,Category_Mapping$Store_Name)]
TopStore2$Unite<-paste(TopStore$DaysVisited,TopStore$TotalPings,sep = "_")
TopStore2<-TopStore2[!(TopStore2$Unite %in% c("1_1","2_2")),] # obtained after removal of bogus cases

result_cat<-ddply(TopStore2,.(Shopper,Fine_Cat),nrow)
names(result_cat)<-c("Shopper", "Fine_Cat","Freq_Score")

# obtained a frequency count.The category with max freq is to be finally assigned to that customer
# in case there is a tie for the topmost freq , then ties to be resolved

# how to reslove ties
# among the tied categories,the category with the highest sum of scores , select that category

cat_sum<-aggregate(TopStore2$Score,by=list(TopStore2$Shopper,TopStore2$Fine_Cat),sum)
names(cat_sum)<-c("Shopper", "Fine_Cat","Pref_Score")
# category wise scores obtained for each customer
##########
#>>>>>>>Merging both scores into 1 data frame<<<<<<<
top_cat<-merge(result_cat,cat_sum,by=c("Shopper","Fine_Cat"))
top_cat$Freq_Score <-scale(top_cat$Freq_Score)
top_cat$Pref_Score<-scale(top_cat$Pref_Score)
summary(top_cat$Freq_Score)
summary(top_cat$Pref_Score)
top_cat$SCORE<-100*(top_cat$Freq_Score+top_cat$Pref_Score)
#############Finding max for each shopper###########

# use aggregate to create new data frame with the maxima
cat.agg <- aggregate(SCORE ~ Shopper, top_cat, max)
names(cat.agg)<-c("Shopper","SCORE")
# then simply merge with the original
cat.max<-merge(top_cat,cat.agg,all=FALSE)

cat.max<-cat.max[mixedorder(cat.max$Shopper),]
TopVisitedCategory<-cat.max
n_occur <- data.frame(table(TopVisitedCategory$Shopper))
n_occur[n_occur$Freq > 1,]
####Studying 206########
s206<-TopStore[TopStore$Shopper=="Shopper_206",]
s206<-s206[,c(1,2,3,4,6)]
names(s206)<-c("Shopper","Store_Name","Days_Visited","TP")
s206<-merge(s206,Category_Mapping)
###############Resolving Tie#################
TopVisitedCategory<-TopVisitedCategory[-13,]
TopVisitedCategory<-TopVisitedCategory[-66,]
TopVisitedCategory<-TopVisitedCategory[-206,]
TopVisitedCategory<-TopVisitedCategory[-341,]
TopVisitedCategory<-TopVisitedCategory[-380,]
###############################################
TopVisitedCategories<-TopVisitedCategory[,c(1,3)]
Answer<-merge(TopVisitedStores,TopVisitedCategories,by="Shopper")
Answer<-Answer[mixedorder(Answer$Shopper),]

####### -------------- PREVIOUS WORK DONE.........-----------------------------------

###-----find why 656 customer coming and not 657-----

#Demograph_new2<-merge(Demograph_new,Answer[,c(1,3)],by="Shopper_Id")
names(Answer)<-c("Shopper_Id","Store","Fine_Cat")
table(Demograph_new$Shopper_Id%in%Answer$Shopper_Id)
Demograph_new$Shopper_Id[Demograph_new$Shopper_Id%in%Answer$Shopper_Id=='FALSE']
# Shopper445 is removed bcz it had only one case of2_2.On inspection its category comes out to be QSR.
#Assign this manually in Answers

Answer2<-rbind(Answer,c("Shopper_445","Wetzel's Pretzels","Qsr Restaurants"))
# Do ordering again
Answer2<-Answer2[mixedorder(Answer2$Shopper_Id),]

###----error found and rectified----------------------------------------------------


## add corresponding top visited category to demograph_new

Demograph_new2<-merge(Demograph_new,Answer2,by='Shopper_Id')
Demograph_new2<-Demograph_new2[mixedorder(Demograph_new2$Shopper_Id),]

## ---DONE-------------------------------


###--------------------- Clustering PAM--------------------------------------

# convert children var to factr var
Demograph_new2$NoOfChildren<-factor(Demograph_new2$Number_of_Children_under_18_years_of_age)
str(
  Demograph_new2
)

cluster_data<-Demograph_new2[,-5] # remove the numeric children var from clusterdata

hist(Demograph_new2$Age)
# almost normal.Log ratio not required

gower_dist <- daisy(cluster_data[, c(2:7,10)], metric = "gower")
                
summary(gower_dist)

gower_mat <- as.matrix(gower_dist)
# Output most similar pair
cluster_data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
cluster_data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:30){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:30, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:30, sil_width)


##k= 15 

pam_fit <- pam(gower_dist, diss = TRUE, k = 15)

##---Cluster Interpretation-----------

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
cluster_data[pam_fit$medoids, ]

pam_results <- cluster_data %>%
  dplyr::select(c(2:10)) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


#---Cluster Interpretation(using visuALISATION)-----------------------

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         Fine_Cat = cluster_data$Fine_Cat)


# Cluster Plot
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# Mosaic Plot
# add the cluster membership information into dataset 
cluster_data$cluster <- pam_fit$clustering

# category assigned to every cluster
with(cluster_data, print(table(cluster, Fine_Cat)))

mosaicplot(~cluster + Fine_Cat, data = cluster_data,
           main =list("Breakdown",col="gray20"),
           xlab="Cluster", ylab="Fine_Cat",col = c("gray70","mediumseagreen"),
           type = "deviance", border = NA)

# percentage of customer in each cluster
print(round(100 * table(cluster_data$cluster) / nrow(cluster_data), digits = 1))

######-----------creation of proprtion for other clustering---------------------

TopStore$Fine_Cat<-Category_Mapping$Fine_Category[match(TopStore$Store,Category_Mapping$Store_Name)]

new<-ddply(TopStore,.(Shopper,Fine_Cat),nrow)
names(new)<-c("Shopper", "Fine_Cat","Freq_Score")

# obtained a frequency count.The category with max freq is to be finally assigned to that customer
# in case there is a tie for the topmost freq , then ties to be resolved

# how to reslove ties
# among the tied categories,the category with the highest sum of scores , select that category

cat_sum2<-aggregate(TopStore$Score,by=list(TopStore$Shopper,TopStore$Fine_Cat),sum)
names(cat_sum2)<-c("Shopper", "Fine_Cat","Pref_Score")
# category wise scores obtained for each customer

#>>>>>>>Merging both scores into 1 data frame<<<<<<<
cluster_cat<-merge(new,cat_sum2,by=c("Shopper","Fine_Cat"))

#top_cat$Freq_Score <-scale(top_cat$Freq_Score)
#top_cat$Pref_Score<-scale(top_cat$Pref_Score)
#summary(top_cat$Freq_Score)
#summary(top_cat$Pref_Score)

prop<-cluster_cat %>%
  group_by(Shopper) %>%
  mutate(ByFreq = Freq_Score / sum(Freq_Score))
prop2<-prop %>%
  group_by(Shopper) %>%
  mutate(Bypref = Pref_Score / sum(Pref_Score))
prop2$Finalprop<-(prop2$ByFreq+prop2$Bypref)/2

temp<-prop2[,c(1,2,7)]
cluster_data<-spread(temp,Fine_Cat,Finalprop)
new2<-new3
cluster_data[is.na(cluster_data)]<-0 # assigning na values 0

####----------Creation of cluster data done--------------------------------------


######-----Visualisation before clustering to get some idea

#### Age and top three visited categories
## first create binary vars for these three categories

# first converting chars to factors
Demograph_new2$Fine_Cat2<-factor(Demograph_new2$Fine_Cat)
#  now to binary categorical vars
demo<-cbind(with(Demograph_new2, model.matrix(~ Fine_Cat2 + 0)))
demo<-demo[,c(1,2,4)]
demo<-as.data.frame(demo)
# passed to original demograph database
Demograph_new2$IsApparel<-factor(demo$`Fine_Cat2Apparel And Accessories`,labels = c("No","Yes"))
Demograph_new2$IsFurniture<-factor(demo$`Fine_Cat2Furniture And Decor`,labels = c("No","Yes"))
Demograph_new2$IsQSR<-factor(demo$`Fine_Cat2Qsr Restaurants`,labels = c("No","Yes"))

## Age and Apparel

# Lattice plot by Age used lattice package 
library(lattice) 
lattice_plot_age <- histogram(~Age | IsApparel, data = Demograph_new2,bw='nrd0', type = "density", main=list(label="Lattice Plot between Age and Apparel TopVisitors",cex=1.0, col= "gray20"),strip = strip.custom(bg="gray20", par.strip.text=list(col="white",cex=.8, font=3)), xlab=list(label="Age of Customer",cex=.8, font=3),ylab=list(label="Density", cex=.8, font=3),col = c("mediumseagreen"), scales=list(cex=0.5), par.settings = list(axis.line = list(col = 'gray')), layout = c(2,1))
print(lattice_plot_age)

# Bar Plot by age used ggplot2 
library(ggplot2) 
ggplot(data=Demograph_new2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=IsApparel, fill= IsApparel), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ scale_fill_manual(values = c("gray70","mediumseagreen"))+ scale_colour_manual(values = c("gray70","mediumseagreen"))+ 
  labs(x="Age", y="Apparel")+ ggtitle("Apparel Store Visit", "by Age")
# younger folks less than 25 show greater prefernce to furniture and older guyz bw 55 and 65

## Age and Furniture

# Bar Plot by age used ggplot2 
library(ggplot2) 
ggplot(data=Demograph_new2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=IsFurniture, fill= IsFurniture), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ scale_fill_manual(values = c("gray70","mediumseagreen"))+ scale_colour_manual(values = c("gray70","mediumseagreen"))+ 
  labs(x="Age", y="Furniture")+ ggtitle("Furnitue Store Visit", "by Age")
# adults(bw 30 and 60) show greater preference to furniture stores than other stores

## Age and QSR

# Bar Plot by age used ggplot2 
library(ggplot2) 
ggplot(data=Demograph_new2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=IsQSR, fill= IsQSR), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ scale_fill_manual(values = c("gray70","mediumseagreen"))+ scale_colour_manual(values = c("gray70","mediumseagreen"))+ 
  labs(x="Age", y="QSR")+ ggtitle("QSR Store Visit", "by Age")
# older people show very less prefernce for QSR

## concatinating all categories

# Bar Plot by age used ggplot2 
library(ggplot2) 
ggplot(data=Demograph_new2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=Fine_Cat2, fill= Fine_Cat2), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ 
  labs(x="Age", y="Stores")+ ggtitle("Store Visit", "by Age")
+ scale_fill_manual(values = c("gray70","mediumseagreen"))+ scale_colour_manual(values = c("gray70","mediumseagreen"))

Demograph_new2$CatCon<-revalue(Demograph_new2$Fine_Cat2,c("Misc."="Other","Shoe Stores"="Other","Sports Stores"="Other","Toy Stores"="Other"))

library(ggplot2) 
ggplot(data=Demograph_new2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=CatCon, fill= CatCon), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ 
  labs(x="Age", y="Fine_Category")+ ggtitle("Top Category Visit", "by Age")
# less than 25 show prefernce for apparels and 25 - 50 for furniture

# For not only top categories but all categories

TopStore2$Age<-Demograph_new2$Age[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]
TopStore2$Fine_Cat<-factor(TopStore2$Fine_Cat)
str(TopStore2$Fine_Cat)

library(ggplot2) 
ggplot(data=TopStore2, aes(x=Age),..density..) + 
  geom_density(aes(x=Age, colour=Fine_Cat, fill= Fine_Cat), alpha=0.3)+ 
  geom_blank()+
  scale_x_continuous(limits=c(0,100))+ 
  labs(x="Age", y="Stores")+ ggtitle("QSR Store Visit", "by Age")

library(lattice) 
#lattice_plot_age <- histogram(~Age | Fine_Cat, data = TopStore2,bw='nrd0', type = "density", main=list(label="Lattice Plot between Age and Apparel TopVisitors",cex=1.0, col= "gray20"),strip = strip.custom(bg="gray20", par.strip.text=list(col="white",cex=.8, font=3)), xlab=list(label="Age of Customer",cex=.8, font=3),ylab=list(label="Density", cex=.8, font=3),col = c("mediumseagreen"), scales=list(cex=0.5), par.settings = list(axis.line = list(col = 'gray')), layout = c(2,1))

lattice_plot_age <- histogram(~Age | Fine_Cat, data = TopStore2,bw='nrd0', type = "density")
print(lattice_plot_age)
# a nice graph obtained . But not much can be ascertained

# Or with Histogram 
ggplot(data=TopStore2, aes(x=Age),..density..) + 
  geom_histogram(aes(x=Age, colour=Fine_Cat, fill= Fine_Cat), alpha=0.7)+ 
  scale_x_continuous(limits=c(0,100))+ scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000","#fffac8","#f58231","#aa6e28","#808080","#e6beff","#d2f53c","#911eb4"))+ scale_colour_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000","#fffac8","#f58231","#aa6e28","#808080","#e6beff","#d2f53c","#911eb4"))+ 
  labs(x="Age", y="Subscibe")+ ggtitle("Subcribe the Term of Deposit", "by Age")
# see later if anything possible

#### ------------------------------DONE---------Age and top three visited categories------------------------------


#### Gnder vs All Categories


# Mosaic Plot- Gender
# -----------------
mosaicplot(~ Fine_Cat2 + Gender, data = Demograph_new2,main =list("Subcribe the Term of Deposit", col="gray20"),
          xlab="Store Categories", ylab="Gender",col = c("gray70","mediumseagreen"), type = "deviance", border = NA)

# males are more likely to go to QSr and females are more likely to got to for apparel shopping

#wrt total pings
TopStore$Gender<-Demograph_new2$Gender[match(TopStore$Shopper,Demograph_new2$Shopper_Id)]

mosaicplot(~ Fine_Cat + Gender, data = TopStore,main =list("Subcribe the Term of Deposit", col="gray20"),
           xlab="Store Categories", ylab="Gender",col = c("gray70","mediumseagreen"), type = "deviance", border = NA)
# no definite conclusion


# Plot 
#----------------------- 
library(data.table) 
dt <- setDT(Demograph_new2)[,list(count = .N), by = .(Fine_Cat2,Gender)][,list(Gender = Gender, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat2] 
ggplot(data=dt, aes(x=Fine_Cat2, y= count, fill=Gender)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Personal Loan", y="Subcribe") + scale_fill_manual(values = c("gray70","mediumseagreen"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")
# again shows that males are more likely to go to QSR than Apparel and females to apparel


dt <- setDT(TopStore)[,list(count = .N), by = .(Fine_Cat,Gender)][,list(Gender = Gender, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat] 
ggplot(data=dt, aes(x=Fine_Cat, y= count, fill=Gender)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Personal Loan", y="Subcribe") + scale_fill_manual(values = c("gray70","mediumseagreen"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")
# again though by  less margin males more likely to go to QSR and females to apparel

### Done with Gender----------------------------------------------


## wrt floors and gender


Allfloors$Gender<-Demograph_new2$Gender[match(Allfloors$Shopper_ID,Demograph_new2$Shopper_Id)]


dt <- setDT(Allfloors)[,list(count = .N), by = .(Floor,Gender)][,list(Gender = Gender, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Floor] 
ggplot(data=dt, aes(x=Floor, y= count, fill=Gender)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Floor", y="Gender") + scale_fill_manual(values = c("gray70","mediumseagreen"))+ ggtitle("Distribution of Floors", "by gender")


## No of chidren vs all categories

TopStore$Children<-Demograph_new2$NoOfChildren[match(TopStore$Shopper,Demograph_new2$Shopper_Id)]


dt <- setDT(TopStore)[,list(count = .N), by = .(Fine_Cat,Children)][,list(Children = Children, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat] 
ggplot(data=dt, aes(x=Fine_Cat, y= count, fill=Children)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Personal Loan", y="Subcribe") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")


dt <- setDT(Demograph_new2)[,list(count = .N), by = .(Fine_Cat2,NoOfChildren)][,list(NoOfChildren = NoOfChildren, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat2] 
ggplot(data=dt, aes(x=Fine_Cat2, y= count, fill=NoOfChildren)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Personal Loan", y="Subcribe") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")


## Owns Home vs All Categories

dt <- setDT(Demograph_new2)[,list(count = .N), by = .(Fine_Cat2,Owns_Home)][,list(Owns_Home = Owns_Home, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat2] 
ggplot(data=dt, aes(x=Fine_Cat2, y= count, fill=Owns_Home)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Store", y="Home") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")

##------ Marital Status 

Demograph_new2$Marital<-Demograph$Marital_Status2

dt <- setDT(Demograph_new2)[,list(count = .N), by = .(Fine_Cat2,Marital)][,list(Marital = Marital, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat2] 
ggplot(data=dt, aes(x=Fine_Cat2, y= count, fill=Marital)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Store", y="Home") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Store Categories", "by Marital Status")

dt <- setDT(TopStore)[,list(count = .N), by = .(Fine_Cat,Marital)][,list(Marital = Marital, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Fine_Cat] 
ggplot(data=dt, aes(x=Fine_Cat, y= count, fill=Marital)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Store", y="Home") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Store Categories", "by Marital Status")




# Mosaic
mosaicplot(~ Fine_Cat2 + Marital ,data = Demograph_new2,main =list("Subcribe the Term of Deposit", col="gray20"),xlab="Store Categories", ylab="Marital",col = c("gray70","mediumseagreen","red"), type = "deviance", border = NA)
# married ppl - furniture single - misc
#wrt total pings

TopStore$Marital<-Demograph_new2$Marital[match(TopStore$Shopper,Demograph_new2$Shopper_Id)]

# Mosaic
mosaicplot(~ Fine_Cat + Marital ,data = TopStore,main =list("Subcribe the Term of Deposit", col="gray20"),xlab="Store Categories", ylab="Marital",col = c("gray70","mediumseagreen","red"), type = "deviance", border = NA)

#-----Marital Status Done------------------------

#######CLUSTERING USING ONLY PROPORTIONS OF STORE PREFERENCE


# Running the k-means algorithm -------------------------------------------------
library(cluster) # Needed for silhouette function

kmeansDat <- cluster_data[,-1]  # Extract only customer columns
kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 15      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeansDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

#######END----------------


#### clustering restart wrt first method


demo_sample<-Demograph_new2[Demograph_new2$Fine_Cat=="Furniture And Decor",]
demo_sample<-demo_sample[,c(1,2,3,4,6:8,9,11,12)]


cluster_furniture<-demo_sample[,-c(1,8,10)] # prepare for clustering


gower_dist <- daisy(cluster_furniture, metric = "gower")

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)
# Output most similar pair
cluster_furniture[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
cluster_furniture[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


##k= 8

pam_fit <- pam(gower_dist, diss = TRUE, k = 8)

##---Cluster Interpretation-----------

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
cluster_data[pam_fit$medoids, ]

pam_results <- demo_sample %>%
  dplyr::select(-1) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


#---Cluster Interpretation(using visuALISATION)-----------------------

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         Shopper = demo_sample$Shopper_Id)


# Cluster Plot
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# Mosaic Plot
# add the cluster membership information into dataset 
cluster_furniture$cluster <- pam_fit$clustering

# category assigned to every cluster
with(cluster_data, print(table(cluster, Fine_Cat)))

mosaicplot(~cluster + Fine_Cat, data = cluster_data,
           main =list("Breakdown",col="gray20"),
           xlab="Cluster", ylab="Fine_Cat",col = c("gray70","mediumseagreen"),
           type = "deviance", border = NA)

# percentage of customer in each cluster
print(round(100 * table(cluster_furniture$cluster) / nrow(cluster_furniture), digits = 1))




###--------------------- Clustering PAM--------------------------------------

TopStore2$Married<-Demograph_new2$Married[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]
TopStore2$Divorced<-Demograph_new2$Divorced[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]
TopStore2$Single<-Demograph_new2$Single[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]
TopStore2$Children<-Demograph_new2$NoOfChildren[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]
TopStore2$OwnsHome<-Demograph_new2$Owns_Home[match(TopStore2$Shopper,Demograph_new2$Shopper_Id)]

final_cluster<-TopStore2[,-c(3,4,5,7)]


hist(final_cluster$Age)
# almost normal.Log ratio not required

gower_dist <- daisy(final_cluster[, -c(1:3)], metric = "gower")

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)
# Output most similar pair
final_cluster[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
final_cluster[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:13){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:13, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:13, sil_width)


##k= 13 

pam_fit <- pam(gower_dist, diss = TRUE, k = 13)

##---Cluster Interpretation-----------

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
cluster_data[pam_fit$medoids, ]

pam_results <- final_cluster %>%
  dplyr::select(-c(1:2)) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


#---Cluster Interpretation(using visuALISATION)-----------------------

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         Fine_Cat = cluster_data$Fine_Cat)


# Cluster Plot
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# Mosaic Plot
# add the cluster membership information into dataset 
final_cluster$cluster <- pam_fit$clustering

# category assigned to every cluster
see<-as.data.frame(with(final_cluster, print(table(cluster, Fine_Cat))))

mosaicplot(~cluster + Fine_Cat, data = final_cluster,
           main =list("Breakdown",col="gray20"),
           xlab="Cluster", ylab="Fine_Cat",col = c("gray70","mediumseagreen"),
           type = "deviance", border = NA)

# percentage of customer in each cluster
print(round(100 * table(cluster_data$cluster) / nrow(final_cluster), digits = 1))


##33##


plz<-see %>%
  group_by(cluster) %>%
  mutate(Prop = Freq / sum(Freq))
plz2<-aggregate(plz$Prop,by=list(plz$cluster,plz$Fine_Cat),max)
plz3<-aggregate(plz$Prop,by=list(plz$Fine_Cat),max)
plz3$Cluster<-plz$cluster[match(plz3$x,plz$Prop)]

##3# clustering of age and category


gower_dist <- daisy(final_cluster[, c(3,4)], metric = "gower")

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)
# Output most similar pair
final_cluster[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
final_cluster[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:15){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width)


##k= 9
pam_fit <- pam(gower_dist, diss = TRUE, k = 9)

##---Cluster Interpretation-----------

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
cluster_data[pam_fit$medoids, ]

pam_results <- final_cluster %>%
  dplyr::select(-c(1:2)) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#######GOD DAMN IT !! ----- FINAL CLUSTERING--------

# assign demographic variable to answer2

Answer2$Age<-Demograph$Age[match(Answer2$Shopper_Id,Demograph$Shopper_Id)]
Answer2$Gender<-Demograph$Gender[match(Answer2$Shopper_Id,Demograph$Shopper_Id)]
Answer2$Home<-Demograph$Owns_Home[match(Answer2$Shopper_Id,Demograph$Shopper_Id)]
Answer2$MaritalStatus<-Demograph$Marital_Status2[match(Answer2$Shopper_Id,Demograph$Shopper_Id)]
Answer2$Children<-Demograph_new2$NoOfChildren[match(Answer2$Shopper_Id,Demograph_new2$Shopper_Id)]
Answer2$Children<-ordered(Answer2$Children)  
str(Answer2)
Answer2$Gender<-factor(Answer2$Gender)
Answer2$Home<-factor(Answer2$Home)
Answer2$Fine_Cat<-factor(Answer2$Fine_Cat)

Answer2<-Answer2[,-c(9)]

# first try using all variables

gower_dist <- daisy(Answer2[,-c(1,2)], metric = "gower")

summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
Answer2[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
Answer2[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:30){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:30, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:30, sil_width)


##k= 25
pam_fit <- pam(gower_dist, diss = TRUE, k = 25)

##---Cluster Interpretation-----------

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
mediods<-Answer2[pam_fit$medoids, ]

pam_results <- Answer2 %>%
  dplyr::select(-c(1:2)) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#---Cluster Interpretation(using visuALISATION)-----------------------

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         Shopper = Answer2$Shopper_Id)


# Cluster Plot
ggplot(aes(x = X, y = Y), data = tsne_data) + xlab("Composite_Score1") + ylab("Composite_Score2")+
  geom_point(aes(color = cluster)) + scale_color_manual(values=color) + ggtitle("Clusters based on Demographics")


color<-c("#e6194b","#3cb44b","#ffe119","#0082c8","#f58231","#911eb4","#46f0f0","#f032e6","#d2f53c","#000000","#FFFFFF","#000080","#aa6e28","#fabebe","#e6beff","#fffac8","#800000","#aaffc3","#808000","#ffd8b1","#808080","#FF00FF","#0087ff","#87005f","#87d700","#87d7ff")


# Mosaic Plot
# add the cluster membership information into dataset 
Answer2$cluster <- pam_fit$clustering

# category assigned to every cluster
see4<-as.data.frame(with(Answer2, print(table(cluster, Fine_Cat))))

mosaicplot(~cluster + Fine_Cat, data = final_cluster,
           main =list("Breakdown",col="gray20"),
           xlab="Cluster", ylab="Fine_Cat",col = c("gray70","mediumseagreen"),
           type = "deviance", border = NA)

# percentage of customer in each cluster
print(round(100 * table(cluster_data$cluster) / nrow(final_cluster), digits = 1))


###---wrt floors and categories-----

#--combining all the pings floorwise and then assiging each pings its category according to store alloted-----
result_bind<-do.call("rbind",list(result_0,result_1,result_2,result_3,result_4))
result_bind$Category<-Category_Mapping$Fine_Category[match(result_bind$StoreAlloted,Category_Mapping$Store_Name)]

colors3<-c("#e6194b","#3cb44b","#ffe119","#0082c8","#f58231","#911eb4","#46f0f0","#f032e6","#d2f53c","#fffac8","#008080","#000080","#aa6e28")

#color4<-c("Black","red","green","yellow","blue","orange","pink","grey","cyan","moccasin","palegreen2","tan2","orchid4")
#ggplot(data=dt, aes(x=Fine_Cat2, y= count, fill=NoOfChildren)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  #labs(x="Personal Loan", y="Subcribe") + scale_fill_manual(values = c("#e6194b","#3cb44b","#ffe119","#0082c8","#46f0f0","#000000"))+ ggtitle("Subcribe the Term of Deposit", "by Personal Loan")
#Store_Mapping2<-Store_Mapping
#Store_Mapping2$Category<-Category_Mapping$Fine_Category[Store_Mapping2$Store_Name,Category_Mapping$Store_Name]

# plot of pings arranged floorwise and category wise
Floorwise_Category<- setDT(result_bind)[,list(count = .N), by = .(floor,Category)][,list(Category = Category, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = floor] 

p2<-ggplot(data=Floorwise_Category, aes(x=floor, y= count, fill=Category)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5))+ 
  labs(x="Floor", y="Categories") + scale_fill_manual(values=colors3)+ ggtitle("   Distribution of Pings Categories", "    by Floors")

p2<-p2+ theme(axis.title.y =element_blank(),axis.title.x = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())

#### plot for % of stores on each floor by category
##Floorcat<-merge(Store_Mapping,Category_Mapping,by='Store_Name')
Store_Mapping2<-Store_Mapping
Store_Mapping2$Category<-Category_Mapping$Fine_Category[match(Store_Mapping2$Store_Name,Category_Mapping$Store_Name)]
Store_Mapping4<-Store_Mapping2[1:160,] # removing other floors 5,6,,7

floor<-c("Concourse","Floor 1","Floor 2","Floor 3","Floor 4")
index2<-c(0,1,2,3,4)
Store_Mapping4$Floor<-floor[match(Store_Mapping4$Floor_Index,index2)]

#Store_Mapping3<- setDT(Store_Mapping4)[,list(count = .N), by = .(Floor_name,Category)][,list(Category = Category, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Floor_name] 
Store_Mapping5<- setDT(Store_Mapping4)[,list(count = .N), by = .(Floor,Category)][,list(Category = Category, count = count, percent_fmt = paste0(formatC(count*100/sum(count),digits = 3), "%"),percent_num = count/sum(count)), by = Floor] 


p3<-ggplot(data=Store_Mapping5, aes(x=Floor, y= count, fill=Category)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = count),position = position_stack(vjust = 0.5))+ 
  labs(x="Floor", y="Categories") + scale_fill_manual(values=colors3)+ ggtitle("Distribution of Store Categories", "by Floors")

p3<-p3+ theme(axis.title.y =element_blank(),axis.title.x = element_blank())
p3
###New Layout distribution

NewLayout<-Store_Mapping5[,1:3]
# concourse
NewLayout2<-NewLayout
NewLayout2<-NewLayout2[-c(2,3,7,9),]
NewLayout2$count[NewLayout2$Floor=="Concourse"]=c(29,2,9,1,6,2)
#First Floor
NewLayout2$count[NewLayout2$Floor=="Floor 1"]=c(1,18,1,7,10,1)
# Second Floor
NewLayout2<-NewLayout2[-c(16,17),]
NewLayout2$count[NewLayout2$Floor=="Floor 2"]=c(20,8,2,2,1,1,1)
# Third floor
NewLayout2<-NewLayout2[-c(20,23,24,25),]
NewLayout2$count[NewLayout2$Floor=="Floor 3"]=c(22,2,1,3,3,1)
# fourth floor
NewLayout2<-NewLayout2[-c(27,30),]
NewLayout2$count[NewLayout2$Floor=="Floor 4"]=c(4,1,1,2)

NewLayout2<-NewLayout2[-19,]
NewLayout2$count[19]=21

## plot new layout chart

newlay<-ggplot(data=NewLayout2, aes(x=Floor, y= count, fill=Category)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = count),position = position_stack(vjust = 0.5))+ 
  labs(x="Floor", y="Categories") + scale_fill_manual(values=colors3)+ ggtitle("Distribution of Categories(New Layout)")

newlay<-newlay+theme_gray()+theme(axis.title.y =element_blank(),axis.title.x = element_blank())
newlay


### ---new layout----

# using plotly
plotly1<-ggplotly(p3)
plotly2<-ggplotly(p2)
subplot(plotly1,plotly2)

# Ploting these plots side by side
multiplot(p3,p2,cols=2) # multiplot comes in Rmisc package

# arranging 

ggarrange(p3,p2,newlay,labels = c("A","B","C"),ncol = 2,nrow=2,common.legend = TRUE,legend = "right")


# first plot of OLD LAYOUT
ggarrange(p3,p2,labels = c("A","B"),ncol = 2,nrow = 1,common.legend = TRUE,legend = "right")

## second plot of old layout vs new layout

oldlay2<-ggplot(data=Store_Mapping5, aes(x=Floor, y= count, fill=Category)) + geom_bar(stat = "identity", width=0.7) + geom_text(aes(label = count),position = position_stack(vjust = 0.5))+ 
  labs(x="Floor", y="Categories") + scale_fill_manual(values=colors3)+ ggtitle("Distribution of Categories(Old Layout)")

oldlay2<-oldlay2+theme_gray()+theme(axis.title.y =element_blank(),axis.title.x = element_blank())
oldlay2  

ggarrange(oldlay2,newlay,labels = c("A","B"),ncol = 2,nrow = 2,common.legend = TRUE,legend = "right",heights = c(1,1))

#elongate these graph and write something
text<-paste("Similar Categories are clubbed together as far as possible",
            "Stores(and categories) are shifted from one floor to another on the basis",
            "of the density zones and the popularity on those floors")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")

#ggarrange(oldlay,newlay,text.p,labels = c("A","B"),ncol = 2,nrow = 2,common.legend = TRUE,legend = "right",heights = c(1,1,0.5))

# other method

ggdraw() +
  draw_plot(oldlay2, x = 0, y = .3, width = .5, height = .7)+
  draw_plot(newlay, x = .5, y = .3, width = .5, height = .7) +
  draw_plot(gtable, x = 0, y = 0, width = 1.25, height = .3)+
  draw_plot(gtable2,x=0 , y=0 ,width = 1.65,height=0.3)
  
## create a table

Category<-c("Apparel And Accessories","Furniture And Decor","Electronics Stores")
Floors<-c("1,2,3","1,2","3")
table<-data.frame(Category,Floors)
gtable<-ggtexttable(table, rows = NULL, 
            theme = ttheme("mOrange"))

Category<-c("Toy Stores","Shoe Stores","Sports Stores")
Floors<-c("3","Concourse","Concourse")
table2<-data.frame(Category,Floors)
gtable2<-ggtexttable(table2, rows = NULL, 
                    theme = ttheme("mOrange"))

###-----plotting for layout done.You need to beautify this graph later on------------


## plotly mapping store category wise with different colors


colors5<-c("#e6194b","#3cb44b","#0082c8","#911eb4","#46f0f0","#f032e6","#d2f53c","#800000","#000080","#aa6e28")

floor3<-plot_ly(data=Store_Mapping4[Store_Mapping4$Floor_Index==3],x=~longitude,y=~latitude,color= ~Category, colors = colors5, marker=list(size=15))
floor3

floor1<-plot_ly(data=Store_Mapping4[Store_Mapping4$Floor_Index==1],x=~longitude,y=~latitude,color= ~Category, colors = colors5, marker=list(size=15))
floor1<-floor1 %>% layout(xaxis=list(showticklabels = FALSE))
floor1<-floor1 %>% layout(yaxis=list(showticklabels = FALSE))
floor1

floor2<-plot_ly(data=Store_Mapping4[Store_Mapping4$Floor_Index==2],x=~longitude,y=~latitude,color= ~Category, colors = colors5, marker=list(size=15))
floor2<-floor2 %>% layout(xaxis=list(showticklabels = FALSE))
floor2<-floor2 %>% layout(yaxis=list(showticklabels = FALSE))

floor2

mp<-subplot(floor1,floor2)
mp 




