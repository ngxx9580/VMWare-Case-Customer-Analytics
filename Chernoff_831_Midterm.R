library("mice")
library("dplyr")
library("fastDummies")
library("sqldf")
library("randomForest")
library("ROSE")
library("pROC")
library("caret")

Train <- read.csv(file.choose())
Test <- read.csv(file.choose())

Train$rowid <- seq.int(nrow(Train))
Test$rowid <- seq.int(nrow(Test)) + 50006

train_rowid <- Train$rowid
test_rowid <- Test$rowid

dim(Train) # 50006 707
dim(Test)  # 50006 707

combined <- rbind(Train, Test)

#################################### Missing value imputation 

# audience set --- need high level dataset filtering
sum(is.na(combined$db_country)) # 1736
sum(is.na(combined$db_audience)) # 1763

combined <- filter(combined, is.na(combined$db_country)==FALSE)
# impute data  --- fill in most common value "residential"
combined$db_audience[is.na(combined$db_audience)] <- names(sort(-table(combined$db_audience)))[1]

filtered_rowid <- combined$rowid

# split the dataset 
views <- (combined[,2:7])
view.breakdown <- (combined[,63:208])
events <- (combined[,c(13,21:24,27,34)])
audience <- (combined[,c(265,269)])
downloads <- (combined[,c(271:293)]) 
registration <- (combined[,209:244])
launchpurchase <- (combined[,c(306:327,330:336)])
bookingamt <- (combined[,c(368:436,440:449)])
purchasedate <- (combined[,527:584])
channel <- (combined[,484:488])
ftrdate <- (combined[,598:603])
target_var <- (combined[706])

glimpse(downloads)

######################################### Feature engineering 

# Views - aggregate to high level
evaluation_view <- rowSums(select(view.breakdown,contains("eval")))
hol_view <- rowSums(select(view.breakdown,contains("hol")))
total_view <- cbind(evaluation_view,hol_view,views)

# Downloads - aggregate to high level
downloads$prod_downloads_calc <- rowSums(select(downloads,contains("_prod")))
downloads <- select(downloads,pdf_downloads,whitepaper_downloads,datasheet_downloads,prod_downloads_calc)

# Registration - aggregate to high level
registration$reg_hol_total_calc <- rowSums(select(registration,contains("hol")))
registration$reg_eval_total_calc <- rowSums(select(registration,contains("eval")))
registration<- select(registration,reg_hol_total_calc,reg_eval_total_calc)

# Audience -----------------------------------------------------------------------------

length(unique(audience$db_country)) # 193
length(unique(audience$db_audience)) # 113
names(sort(-table(audience$db_country)))[1:5] 
#  "US" "DE" "GB" "JP" "CN"
names(sort(-table(audience$db_audience)))[1:5]
# "Residential"  "Wireless->Mobile Network" "SOHO" "SMB"  "SMB->Software & Technology"

# take top 5 audience and group the rest as 'Other'
audience$db_audience[!audience$db_audience %in% c("Residential","Wireless->Mobile Network",
                                                 "SOHO", "SMB","SMB->Software & Technology")] <- "Other"

audience$db_country[!audience$db_country %in% c("US","DE","GB","JP","CN")] <- "Other"

audience_dummy <- fastDummies::dummy_cols(audience,remove_first_dummy = TRUE)
audience_dummy <- select(audience_dummy, -db_country,-db_audience)

# channel - impute NA by 0 ------------------------------------------------------------

for (i in 1:ncol(channel)) {
  channel[i][is.na(channel[i])] <- 0
}

for (i in 1:ncol(channel)) {
  channel[i] <- round(channel[i],2)
}

# purchase -----------------------------------------------------------------------------
# Launchpurchase - if value is 9999 set it to 0 and the rest to 1 for count

for (i in 1:ncol(launchpurchase)) {
  launchpurchase[i] <- ifelse(launchpurchase[i] == 9999,0,1)
}

launchpurchase$dist_count_prod_purch <- rowSums(launchpurchase)
launchpurchase <- select(launchpurchase,dist_count_prod_purch)

# purchasedate - replace 9999 with 0
for (i in 1:ncol(purchasedate)) {
  purchasedate[i][purchasedate[i] == 9999] <- 0
}

purchasedate$days_since_first_purch <-round(rowMeans(select(purchasedate,contains("first"))),0)
purchasedate$days_since_last_purch <-round(rowMeans(select(purchasedate,contains("last"))),0)

purchase <- cbind(launchpurchase, purchasedate)
purchase <- select(purchase,dist_count_prod_purch,days_since_first_purch,days_since_last_purch)

# Bookingamt - fill in NA by 0 ----------------------------------------------------------
for (i in 1:ncol(bookingamt)){
    bookingamt[i][is.na(bookingamt[i])] <- 0
}

bookingamt <- select(bookingamt,-contains("tot")) # remove total columns to avoid double count
bookingamt$tot_2013_bookingamt <- round(rowSums(select(bookingamt,contains("2013"))),2)
bookingamt$tot_2014_bookingamt <- round(rowSums(select(bookingamt,contains("2014"))),2)
bookingamt$tot_2015_bookingamt <- round(rowSums(select(bookingamt,contains("2015"))),2)

bookingamt <- select(bookingamt,tot_2013_bookingamt,tot_2014_bookingamt,tot_2015_bookingamt)

# ftrdate - fill in 'NaN' and 'Null' to avoid empty match ----------------------------------
ftrdate$min_date <- apply(ftrdate,1,FUN=min, na.rm= TRUE)
ftrdate$max_date <- apply(ftrdate,1,FUN=max, na.rm= TRUE)

ftrdate$min_date[is.na(ftrdate$min_date)] <- 'NaN' 
ftrdate$max_date[is.na(ftrdate$max_date)] <- 'NaN' 

for (i in 1:ncol(ftrdate)) {
  ftrdate[i][is.na(ftrdate[i])] <- 'Null'
}

ftrdate$first_download_ind <- ifelse(ftrdate$min_date == ftrdate$ftr_first_date_any_download,1,0)
ftrdate$first_reg_ind <- ifelse(ftrdate$min_date == ftrdate$ftr_first_date_seminar_page_view | 
                                  ftrdate$min_date == ftrdate$ftr_first_date_webinar_page_view,1,0)

ftrdate$first_hol_ind <- ifelse(ftrdate$min_date == ftrdate$ftr_first_date_hol_page_view,1,0)
ftrdate$last_hol_ind <-ifelse(ftrdate$max_date == ftrdate$ftr_first_date_hol_page_view  &
                                ftrdate$min_date != ftrdate$max_date,1,0)

ftrdate <- select(ftrdate,first_download_ind, first_reg_ind,first_hol_ind, last_hol_ind)

# combine all subset -------------------------------------------------------------------
combined_clean <- cbind(total_view,downloads,registration,purchase,bookingamt,ftrdate,audience_dummy,channel,target_var)

glimpse(combined_clean)
md.pattern(combined_clean)

sqldf("select target,count(*) from combined_clean group by target")
#target count(*)
#1      0    95550
#2      1     1568
#3      2       18
#4      3       35
#5      4      175
#6      5      930

# add back rowid
combined_clean <- cbind(filtered_rowid,combined_clean)

dim(combined_clean) # 98276 41

names(combined_clean) <- gsub(" ", "_", names(combined_clean))
names(combined_clean)[names(combined_clean) == 'db_audience_SMB->Software_&_Technology'] <- 'db_audience_softwareTech'
names(combined_clean)[names(combined_clean) == 'db_audience_Wireless->Mobile_Network'] <- 'db_audience_mobileNetwork'     
       
######################################### Train-Test split 

filter_train_rowid <- intersect(train_rowid,filtered_rowid)
featured_train <- filter(combined_clean, filtered_rowid %in% filter_train_rowid) %>%
                  select(-filtered_rowid)

filter_test_rowid <- intersect(test_rowid,filtered_rowid)
featured_test <- filter(combined_clean, filtered_rowid %in% filter_test_rowid) %>%
                 select(-filtered_rowid)

dim(featured_test) #49202    40
dim(featured_train) #49074    40

featured_train$target <- as.factor(featured_train$target)
featured_test$target <- as.factor(featured_test$target)

################################################## Binary Classification 

# binary target engineering ------------
featured_train$binary_target<-if_else(featured_train$target==0, 0, 1)
table(featured_train$binary_target) # 47747  1327
featured_test$binary_target<-if_else(featured_test$target==0, 0, 1)
table(featured_test$binary_target) # 47803  1399

VMlog0 <- glm(binary_target ~ ., select(featured_train, -target), family=binomial)
summary(VMlog0)

VMtest0 <- predict(VMlog0, newdata=featured_test, type="response")
VMtest0
hist(VMtest0)

confusion.matrix0<-table(featured_test$binary_target, VMtest0 >= 0.15)
Count.correct0<-confusion.matrix0[1,1]+confusion.matrix0[2,2]
Count.wrong0<-confusion.matrix0[1,2]+confusion.matrix0[2,1]
Accuracy.rate0<-Count.correct0/(Count.correct0+Count.wrong0)
Accuracy.rate0 #0.9829

# To generate the ROC curve
test_prob0 = predict(VMlog0, newdata = featured_test, type = "response")
test_roc0 = roc(featured_test$binary_target ~ test_prob0, plot = TRUE, print.auc = TRUE)
# AUC 0.994

# Balance your data set----------------------------------

featured_train2 <- ROSE(binary_target ~ ., data = featured_train, seed = 1)$data
table(featured_train2$binary_target)

VMlog1 <- glm(binary_target ~ ., select(featured_train2, -target), family=binomial)
summary(VMlog1)

VMtest <- predict(VMlog1, newdata=featured_test, type="response")
VMtest
hist(VMtest)

confusion.matrix<-table(featured_test$binary_target, VMtest >= 0.15)
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]
Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
Accuracy.rate #0.947

test_prob = predict(VMlog1, newdata = featured_test, type = "response")
test_roc = roc(featured_test$binary_target ~ test_prob, plot = TRUE, print.auc = TRUE)
# AUC 0.994

# 2nd model with low P-value var removed ----------------------------------
VMlog2 <- glm(binary_target ~ ., select(featured_train2, -target,-tot_visits, 
                                        -days_since_first_purch, -tot_2013_bookingamt,
                                        -tot_2014_bookingamt,-tot_2015_bookingamt,
                                        -db_country_GB, -channel_oem_total,
                                        -channel_web_total,-channel_direct_total,
                                        -channel_partner_total,-channel_support_total), family=binomial)
summary(VMlog2)

VMtest2 <- predict(VMlog2, newdata=featured_test, type="response")
VMtest2
hist(VMtest2)

confusion.matrix2<-table(featured_test$binary_target, VMtest >= 0.15)
Count.correct2<-confusion.matrix2[1,1]+confusion.matrix2[2,2]
Count.wrong2<-confusion.matrix2[1,2]+confusion.matrix2[2,1]
Accuracy.rate2<-Count.correct2/(Count.correct2+Count.wrong2)
Accuracy.rate2 #0.947

test_prob2 = predict(VMlog2, newdata = featured_test, type = "response")
test_roc2 = roc(featured_test$binary_target ~ test_prob2, plot = TRUE, print.auc = TRUE)
#AUC 0.994


##################################################### Random forest

sqldf("select target,count(*) from featured_train group by target")
# 0    47747
# 1      771
# 2        9
# 3       19
# 4       85
# 5      443

rf_100_2_full = randomForest(target ~ ., data=featured_train, ntree=100, mtry=2, importance=TRUE)
rf_100_2_full
# OOB estimate of  error rate: 0.42%
# Confusion matrix:
#   0   1 2 3 4   5 class.error
# 0 47747   0 0 0 0   0  0.00000000
# 1     7 729 0 0 1  34  0.05447471
# 2     1   8 0 0 0   0  1.00000000
# 3     4  10 0 5 0   0  0.73684211
# 4     9  40 0 1 0  35  1.00000000
# 5    34  21 0 0 0 388  0.12415350

rf_pred_1 <- predict(rf_100_2_full, newdata=featured_test, type="response")
unique(data.frame(rf_pred_1))
# 1            0
# 73           5
# 131          1
# 6755         3

rf_conf_1 <- confusionMatrix(rf_pred_1, featured_test$target)
rf_conf_1
# Accuracy : 0.9959          
# 95% CI : (0.9953, 0.9964)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : < 2.2e-16

# try undersampling subsetting take 300 rows ----------------------

featured_train_target0 <- filter(featured_train,target==0)
featured_train_nottarget0 <- filter(featured_train,!target==0)
featured_train_subset1 <- rbind(featured_train_target0[1:300,],featured_train_nottarget0)

sqldf("select target,count(*) from featured_train_subset1 group by target")
# 0      300
# 1      771
# 2        9
# 3       19
# 4       85
# 5      443

rf_100_2_subset_1 = randomForest(target ~ ., select(featured_train_subset1, -binary_target), ntree=100, mtry=2, importance=TRUE)
rf_100_2_subset_1

# OOB estimate of  error rate: 13.28%
# Confusion matrix:
#   0   1 2 3 4   5 class.error
# 0 278  10 0 0 0  12  0.07333333
# 1  11 725 0 0 0  35  0.05966278
# 2   0   8 0 0 0   1  1.00000000
# 3   2  14 0 3 0   0  0.84210526
# 4   1  45 0 0 0  39  1.00000000
# 5  11  27 0 0 0 405  0.08577878

rf_pred_2 <- predict(rf_100_2_subset_1, newdata=featured_test, type="response")
unique(data.frame(rf_pred_2))
# 1             0
# 2             5
# 6             1
# 6755          3
# 40251         4

rf_conf_2 <- confusionMatrix(rf_pred_2, featured_test$target)
rf_conf_2
# Accuracy : 0.6463         
# 95% CI : (0.642, 0.6505)
# No Information Rate : 0.9716  

# try increasing ntree ---------------------------------

rf_300_2_subset_1= randomForest(target ~ ., select(featured_train_subset1, -binary_target), ntree=300, mtry=2, importance=TRUE)
rf_300_2_subset_1
# OOB estimate of  error rate: 12.6%
# Confusion matrix:
#   0   1 2 3 4   5 class.error
# 0 277  17 0 0 0   6  0.07666667
# 1  13 720 0 0 0  38  0.06614786
# 2   0   8 0 0 0   1  1.00000000
# 3   2  14 0 3 0   0  0.84210526
# 4   0  43 0 0 0  42  1.00000000
# 5   4  17 0 0 0 422  0.04740406

rf_pred_3 <- predict(rf_300_2_subset_1, newdata=featured_test, type="response")
unique(data.frame(rf_pred_3))
# 1             5
# 3             0
# 6             1
# 6755          3
# 40251         4

rf_conf_3 <- confusionMatrix(rf_pred_3, featured_test$target)
rf_conf_3
# Accuracy : 0.6579          
# 95% CI : (0.6537, 0.6621)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : 1               

# try increasing mtry ----------------------------------

rf_300_3_subset_1 = randomForest(target ~ ., select(featured_train_subset1, -binary_target), ntree=300, mtry=3, importance=TRUE)
rf_300_3_subset_1
# OOB estimate of  error rate: 10.82%
# Confusion matrix:
#   0   1 2 3 4   5 class.error
# 0 287  10 0 0 0   3  0.04333333
# 1   3 727 0 0 0  41  0.05706874
# 2   0   8 0 0 0   1  1.00000000
# 3   2  11 0 6 0   0  0.68421053
# 4   0  41 0 0 0  44  1.00000000
# 5   0  12 0 0 0 431  0.02708804

rf_pred_4 <- predict(rf_300_3_subset_1, newdata=featured_test, type="response")
unique(data.frame(rf_pred_4))
# 1            0
# 17           1
# 73           5
# 6755         3
# 9095         4

rf_conf_4 <- confusionMatrix(rf_pred_4, featured_test$target)
rf_conf_4
# Accuracy : 0.8404          
# 95% CI : (0.8372, 0.8437)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : 1       

# try increasing mtree with mtry =3 --------------------------

rf_800_3_subset_1 = randomForest(target ~ ., select(featured_train_subset1, -binary_target), ntree=800, mtry=3, importance=TRUE)
rf_800_3_subset_1
# OOB estimate of  error rate: 10.2%
# Confusion matrix:
#   0   1 2 3 4   5 class.error
# 0 292   5 0 0 0   3  0.02666667
# 1   0 729 0 0 1  41  0.05447471
# 2   0   8 0 0 0   1  1.00000000
# 3   2  10 0 7 0   0  0.63157895
# 4   0  40 0 0 0  45  1.00000000
# 5   0  10 0 0 0 433  0.02257336

rf_pred_5 <- predict(rf_800_3_subset_1, newdata=featured_test, type="response")
unique(data.frame(rf_pred_5))
# 1             0
# 17            1
# 73            5
# 6755          3
# 37232         4

rf_conf_5 <- confusionMatrix(rf_pred_5, featured_test$target)
rf_conf_5
# Accuracy : 0.7873          
# 95% CI : (0.7836, 0.7909)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : 1     

# trying smaller subset ---------------------------------

featured_train_subset2 <- sqldf("SELECT * FROM 
(SELECT *,ROW_NUMBER() OVER (PARTITION BY target) AS RowNum FROM featured_train
) as temp WHERE RowNum <= 10")

featured_train_subset2 <- select(featured_train_subset2,-RowNum,-binary_target)

rf_100_2_subset_2 = randomForest(target ~ ., data=featured_train_subset2, ntree=100, mtry=2, importance=TRUE)
rf_100_2_subset_2
# OOB estimate of  error rate: 50.85%
# Confusion matrix:
#   0 1 2 3 4 5 class.error
# 0 10 0 0 0 0 0         0.0
# 1  3 7 0 0 0 0         0.3
# 2  2 4 0 2 1 0         1.0
# 3  2 0 1 7 0 0         0.3
# 4  3 1 0 2 2 2         0.8
# 5  2 1 0 0 4 3         0.7

rf_pred_6 <- predict(rf_100_2_subset_2, newdata=featured_test, type="response")
unique(data.frame(rf_pred_6))
# 1           0
# 13          1
# 17          3
# 25          5
# 160         2
# 290         4

rf_conf_6 <- confusionMatrix(rf_pred_6, featured_test$target)
rf_conf_6
# Accuracy : 0.8588          
# 95% CI : (0.8557, 0.8619)
# No Information Rate : 0.9716  
# P-Value [Acc > NIR] : 1  

# try oversampling by binary 0/1 with ROSE -------------------------

featured_train_multi <- ROSE(binary_target ~ ., data = featured_train, seed = 1)$data
table(featured_train$binary_target)

sqldf("select target,count(*) from featured_train_multi group by target")

rf_100_2_full_over = randomForest(target ~ ., select(featured_train_multi, -binary_target), ntree=100, mtry=2, importance=TRUE)
rf_100_2_full_over
# OOB estimate of  error rate: 7.82%
# Confusion matrix:
#   0     1 2   3  4    5  class.error
# 0 24495    15 0   0  0    9 0.0009788327
# 1     0 13477 0   0  8  871 0.0612287545
# 2     0   144 0   0  0   16 1.0000000000
# 3     0   202 0 148  0   26 0.6063829787
# 4     0   823 0  10 36  664 0.9765166341
# 5     0  1049 0   0  1 7080 0.1291512915

rf_pred_1_over <- predict(rf_100_2_full_over, newdata=featured_test, type="response")
unique(data.frame(rf_pred_1_over))
# 1                  0
# 131                1
# 1486               5
# 21517              3

rf_conf_1_over <- confusionMatrix(rf_pred_1_over, featured_test$target)
rf_conf_1_over
# Accuracy : 0.9747          
# 95% CI : (0.9733, 0.9761)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : 8.811e-06       

# Oversample more records of target 2, 3, 4, 5 ------------------

# Manual oversampling
target2345 <- filter(featured_train,target==0 | target==2 | target==3 | target==4 | target==5)
sqldf("select target,count(*) from target2345 group by target")
target2345_over <- ROSE(binary_target ~ ., data=target2345, seed=1,)$data
table(target2345_over$target)

# combine oversampled records of target 2,3,4,5, and initial featured_train_multi
target2345_over_filtered <- filter(target2345_over, target==2 | target==3 | target==4 | target==5)
featured_train_multi2 <- rbind(featured_train_multi,target2345_over_filtered)

sqldf("select target,count(*) from featured_train_multi2 group by target")
# 0    24519
# 1    14356
# 2      557
# 3     1214
# 4     5160
# 5    27415

rf_100_2_full_over2 = randomForest(target ~ ., select(featured_train_multi2, -binary_target), ntree=100, mtry=2, importance=TRUE)
rf_100_2_full_over2
# OOB estimate of  error rate: 9.43%
# Confusion matrix:
#   0     1  2   3    4     5 class.error
# 0 24477    11  0   0    1    30 0.001712957
# 1     0 12831  4   0  130  1391 0.106227361
# 2     0   415 65   0    0    77 0.883303411
# 3     0   370  0 769   42    33 0.366556837
# 4     0   939  0  49 1483  2689 0.712596899
# 5     0   632  0   0   92 26691 0.026408900

rf_pred_1_over2 <- predict(rf_100_2_full_over2, newdata=featured_test, type="response")
unique(data.frame(rf_pred_1_over2))
# 1                   0
# 131                 1
# 445                 4
# 835                 5
# 12162               3

rf_conf_1_over2 <- confusionMatrix(rf_pred_1_over2, featured_test$target)
rf_conf_1_over2
# Accuracy : 0.974           
# 95% CI : (0.9726, 0.9754)
# No Information Rate : 0.9716          
# P-Value [Acc > NIR] : 0.0004642    







