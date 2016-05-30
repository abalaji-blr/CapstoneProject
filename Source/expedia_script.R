
## read the training and test data set

library(data.table)
# fread() returns a data.table
system.time(train_dt <- fread("train.25000.csv", header = TRUE))

system.time(test_dt <- fread("test.1000.csv", header = TRUE))

#temp_dt = train_dt[, list(srch_destination_id, hotel_cluster)]

## get the total number of oberservations for each - dest_id & hotel_cluster
## use data.table notation of doing J expr BY group - refer to datacamp's cheat sheet.
## The total observation count is avaliable with column name "N"
dest_id_n_hotel_cluster_grp_count = train_dt[, .N, by = list(srch_destination_id, hotel_cluster)]


## need to get the top 5 often used hotel cluster for each dest_id.

# inputs : hotel_cluster and frequency (occurrence, n)
get_top_five <- function(hc, n) {
  #print("input:")
  #print(hc)
  #print("freq")
  #print(n)
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

## write the csv file
write.csv(result_dt, file = "expedia_submission.csv", row.names = FALSE, quote = FALSE)

