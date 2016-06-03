## History
# 0: inital algo using srch_destination_id
# 1: incorporate the mapk(mean average precision) to measure the prediction.

###########################################################
## read the training and test data set

library(data.table)
# fread() returns a data.table
#system.time(train_dt <- fread("train.1000.csv", header = TRUE))
#system.time(test_dt <- fread("test.1000.csv", header = TRUE))

system.time(train_dt <- fread("new_train.csv", header = TRUE))
system.time(test_dt <- fread("new_test.csv", header = TRUE))

#temp_dt = train_dt[, list(srch_destination_id, hotel_cluster)]

###########################################################
## get the total number of oberservations for each - dest_id & hotel_cluster
## use data.table notation of doing J expr BY group - refer to datacamp's cheat sheet.
## The total observation count is avaliable with column name "N"
dest_id_n_hotel_cluster_grp_count = train_dt[, .N, by = list(srch_destination_id, hotel_cluster)]


## need to get the top 5 often used hotel cluster for each dest_id.

# inputs : hotel_cluster and frequency (occurrence, n)
get_top_five <- function(hc, n) {
 
  # get the ordered list interms of freq. 
  # so sort them in decreasing order
  hc_ordered <- hc[order(n, decreasing = TRUE)]
  #print(hc_ordered)
  
  # we need 5, if no match, we may get 0.
  result <- min(5, length(hc_ordered))
  
  ## return the result with hc separated by spaces
  paste(hc_ordered[1:result],  collapse = " ")
}
  
## again use the J expr and BY group of data table  
## The package data.table creates the column with name V1
dest_id_n_top_5_hotel_cluster = dest_id_n_hotel_cluster_grp_count[ ,get_top_five(hotel_cluster, N), by=srch_destination_id]

##**
##**
target <- test_dt[order(id), list(id, hotel_cluster)]

# this is the test data, and we are going to predict this, so remove it.
test_dt$hotel_cluster = NULL

##**



## Now, look at the test data set and if the dest_id matches,
## recommend the predicted hotel cluster
# use merge: specify both tables x & y
#          : specify the column name for merge
#          : in case of no match, add the row from x and put NA in the respective column.
recommended_hc <- merge(test_dt, dest_id_n_top_5_hotel_cluster, by = "srch_destination_id", all.x = TRUE)

## extract the id and hotel clusters
result_dt <- recommended_hc[order(id), list(id, V1)]

## set the col names
setnames(result_dt, c("id", "hotel_cluster"))

###########################################################
# Exact matches : 
#    a) user_id and srch_desination_id => use the previously used hotel cluster
#    b) orig_destination_distance is same => use the previously used hotel cluster
###########################################################

## a) for every user_id and srch_destination_id  if the hotel cluster exists, 
## and the booking is done then use as first preference for prediction.
#temp_dt = train_dt[, list(user_id, srch_destination_id, hotel_cluster)]

temp1_dt = train_dt[, list(user_id, srch_destination_id, hotel_cluster, is_booking ==1)]

temp2_dt = temp1_dt[, .N, by = list(user_id, srch_destination_id, hotel_cluster)]


temp3_dt  = temp2_dt[ ,get_top_five(hotel_cluster, N), by=list(user_id, srch_destination_id)]

recommended2_hc <- merge(test_dt, temp3_dt, by = c("user_id", "srch_destination_id"), all.x = TRUE)

# extract the id and hotel clusters
result2_dt <- recommended2_hc[order(id), list(id, V1)]

## set the col names
setnames(result2_dt, c("id", "hotel_cluster"))

##########################################################
## b) if user_origination_distance is same, then use the previously used hotel cluster
##########################################################

#temp3_dt = train_dt[, list(orig_destination_distance, hotel_cluster, is_booking==1)]

# don't consider is_booking for now
t1_dt = train_dt[, list(orig_destination_distance, hotel_cluster)]

t2_dt = t1_dt[, .N, by = list(orig_destination_distance, hotel_cluster)]

# get the freq
t3_dt  = t2_dt[ ,get_top_five(hotel_cluster, N), by=orig_destination_distance]

# ignore if dest distance is NA
t4_dt = t3_dt[complete.cases(t3_dt),]

merge_dt <- merge(test_dt, t4_dt, by = "orig_destination_distance", all.x = TRUE)

# extract the id and hotel clusters
result3_dt <- merge_dt[order(id), list(id, V1)]

## set the col names
setnames(result3_dt, c("id", "hotel_cluster"))

###########################################################
## combine the result hotel recommendation.
##
combine_dt = result_dt
combine_dt$new_hotel_cluster = paste(result2_dt$hotel_cluster, result_dt$hotel_cluster)
combine_dt$new_hotel_cluster = paste(result3_dt$hotel_cluster, combine_dt$new_hotel_cluster)

#apply(combine_dt, 1, function(x) paste(na.omit(x),collapse=" ") )

## NAs are introdued by paste().
#replace NA with empty string.
replace_NA <- function(hc) {
  hc_mod <- gsub("NA", "", hc)
  hc_mod
}
  
# reset the column
combine_dt$hotel_cluster = NULL
combine_dt$hotel_cluster = combine_dt$new_hotel_cluster
combine_dt$new_hotel_cluster = NULL

#combine_temp_dt <- combine_dt[ ,replace_NA(hotel_cluster), by = list(id)]
#names(combine_temp_dt)[2] <- "hotel_cluster"


## paste adds NA if data is not available.
## need to omit during the operation or replace NA later
get_unique_five <- function(hc) {
  #hc_mod = gsub("NA", "", hc)
  #print(hc)
  temp <- unlist(strsplit(hc, " "))

  temp_unique <- unique(temp)
  hc_unique <- temp_unique[temp_unique != "NA"]
  #print(hc_unique)
  result <- min(5, length(hc_unique))
  paste(hc_unique[1:result],  collapse = " ")
}

combine_temp_dt <- combine_dt[, get_unique_five(hotel_cluster), by=list(id)]
names(combine_temp_dt)[2] <- "hotel_cluster"

final_dt <- combine_temp_dt[ ,replace_NA(hotel_cluster), by = list(id)]

names(final_dt)[2] <- "hotel_cluster"
 

###########################################################
## create the submission file
##

## write the csv file
write.csv(final_dt, file = "expedia_submission.csv", row.names = FALSE, quote = FALSE)


################################ Measure Prediction ######################
##for mapk - mean average precision
library(Metrics)

## get the predicted list
system.time(predicted_dt <- fread("expedia_submission.csv", header = TRUE))
 
target_list = as.list(target$hotel_cluster)

convert_to_list <- function(record) {
  hc <- record[2]
  #print(hc)
  x <- as.integer(unlist(strsplit(hc, " ")))
  #print(x)
  x
}

predicted_list <- apply(predicted_dt, 1, convert_to_list)

convert_to_list2 <- function(hc) {
  #print(hc)
  x <- as.integer(unlist(strsplit(hc, " ")))
  #print(x)
  x
}

n <- dim(predicted_dt)[1]
new_list <- list()
for(i in 1: n) {
   temp <- convert_to_list2(predicted_dt$hotel_cluster[i])
   if ( length(temp) == 0 ) { 
     #print("temp is empty") 
     temp <- 1
   }
 
   new_list[[i]] <- temp
   #print (temp)
}
#predicted_hc <- predicted_dt$hotel_cluster
temp <- predicted_dt[,convert_to_list2(hotel_cluster)]
                          
predicted_list <- new_list

mapk(5, target_list, predicted_list)



