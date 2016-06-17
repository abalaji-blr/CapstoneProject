## History
# 0: inital algo using srch_destination_id
# 1: incorporate the mapk(mean average precision) to measure the prediction.

###########################################################

## read the training and test data set

library(data.table)

# for complete dataset

#system.time(train_dt <- fread("new_train.csv", header = TRUE))
#system.time(test_dt <- fread("new_test.csv", header = TRUE))

# to read the sampled dataset
system.time(train_dt <- fread("train.25000.csv", header = TRUE))
system.time(test_dt <- fread("test.1000.csv", header = TRUE))

 
###########################################################
## Feature #1: Identify often used hotel cluster for a given destination
##             Use is_booking flag and set appropriate weights.
###########################################################
 
## based on is_booking, give weightage to the hotel cluster
## if is_booking == 1 then weightage = 1
## else weightage = 0.15
## any other magic weightage numbers?
##
compute_weightage <- function(booking_flag) {
  sum(booking_flag) * 0.85 + length(booking_flag) * 0.15
}

## use data.table notation of doing J expr BY group - refer to datacamp's cheat sheet.
## The total observation count is avaliable with column name "N"
dest_id_n_hotel_cluster_grp_count = train_dt[, compute_weightage(is_booking), by = list(srch_destination_id, hotel_cluster)]


## need to get the top 5 often used hotel cluster for each dest_id.
# inputs : hotel_cluster and weights
get_top_five <- function(hc, n) {
 
  # get the ordered list interms of weights.
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
dest_id_n_top_5_hotel_cluster = dest_id_n_hotel_cluster_grp_count[ ,get_top_five(hotel_cluster, V1), by=srch_destination_id]


## Merge test & train dataset by dest id.
# use merge: specify both tables x & y
#          : specify the column name for merge
#          : in case of no match, add the row from x and put NA in the respective column.
recommended_hc <- merge(test_dt, dest_id_n_top_5_hotel_cluster, by = "srch_destination_id", all.x = TRUE)

## extract the id and hotel clusters
result_dt <- recommended_hc[order(id), list(id, V1)]

## set the col names
setnames(result_dt, c("id", "hotel_cluster"))

 
##########################################################
## Feature #2: if orig_destination_distance is same, 
##                then use the previously used hotel cluster
##########################################################

# select only the relevant fields
t1_dt = train_dt[, list(orig_destination_distance, hotel_cluster, is_booking)]

# group the relevant records
t2_dt = t1_dt[, compute_weightage(is_booking), by = list(orig_destination_distance, hotel_cluster)]

# get the top 5
t3_dt  = t2_dt[ ,get_top_five(hotel_cluster, V1), by=orig_destination_distance]

# ignore if dest distance is NA
t4_dt = t3_dt[complete.cases(t3_dt),]

# merge test & train datasets by orig_destination_distance
merge_dt <- merge(test_dt, t4_dt, by = "orig_destination_distance", all.x = TRUE)

# extract the id and hotel clusters
result3_dt <- merge_dt[order(id), list(id, V1)]

# these are huge dataframes, remove them to release the memory.
rm("t1_dt", "t2_dt", "t3_dt", "t4_dt", "merge_dt")

## set the col names
setnames(result3_dt, c("id", "hotel_cluster"))


###########################################################
## combine the result hotel recommendation.
##
combine_dt = result_dt
combine_dt$new_hotel_cluster = paste(result3_dt$hotel_cluster, combine_dt$hotel_cluster)

# reset the column
combine_dt$hotel_cluster = NULL
combine_dt$hotel_cluster = combine_dt$new_hotel_cluster
combine_dt$new_hotel_cluster = NULL


# get unique five
get_unique_five <- function(hc) {
  temp <- unlist(strsplit(hc, " "))

  temp_unique <- unique(temp)
  # ignore NA
  hc_unique <- temp_unique[temp_unique != "NA"]
  #print(hc_unique)
  result <- min(5, length(hc_unique))
  paste(hc_unique[1:result],  collapse = " ")  
}

combine_temp_dt <- combine_dt[, get_unique_five(hotel_cluster), by=list(id)]
names(combine_temp_dt)[2] <- "hotel_cluster"

#replace NA with empty string.
replace_NA <- function(hc) {
  hc_mod <- gsub("NA", "", hc)
  hc_mod
}

final_dt <- combine_temp_dt[, replace_NA(hotel_cluster), by=list(id)]

names(final_dt)[2] <- "hotel_cluster"
 

###########################################################
## create the submission file
##

## write the csv file
write.csv(final_dt, file = "expedia_submission.csv", row.names = FALSE, quote = FALSE)

#Done