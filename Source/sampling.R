## sampling the training dataset and create the new test dataset.

# for splitting the training data 
library(caTools)

system.time(train_dt <- fread("train.25000.csv", header = TRUE))

#to make it reproducible
set.seed(123)

## specify the column name
split = sample.split(train_dt$user_id, SplitRatio = 0.75)

new_train_dt = subset(train_dt, split == TRUE)
new_test_dt  = subset(train_dt, split == FALSE)

write.csv(new_train_dt, file = "new_train.csv", row.names = FALSE, quote = FALSE)
write.csv(new_test_dt, file = "new_test.csv", row.names = TRUE, quote = FALSE)

