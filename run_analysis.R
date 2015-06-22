activity_measurement <- function()
{
   library(dplyr)
  ## fiel path for all the downloadables 
  fpath <- file.path(getwd(), "UCI HAR Dataset")
  
  fpath.test <- file.path(fpath,"test", "X_test.txt")
  fpath.test.labl <- file.path(fpath,"test", "y_test.txt")
  fpath.test.subject <- file.path(fpath,"test", "subject_test.txt")
  
  fpath.train <- file.path(fpath,"train", "X_train.txt")
  fpath.train.labl <- file.path(fpath,"train", "y_train.txt")
  fpath.train.subject <- file.path(fpath,"train","subject_train.txt")
  
  fpath.feature <- file.path(fpath, "features.txt")
  fpath.activity <- file.path(fpath, "activity_labels.txt")
  
  
  ## read test,train and feature files to memory 
  test   <- read.table(fpath.test, header=FALSE, stringsAsFactors=FALSE)
  test.acty <- read.table(fpath.test.labl, header=FALSE, stringsAsFactors=FALSE)
  test.subject <- read.table(fpath.test.subject, header=FALSE, stringsAsFactors=FALSE)
  
  train <- read.table(fpath.train, header=FALSE, stringsAsFactors=FALSE)
  train.acty <- read.table(fpath.train.labl, header=FALSE, stringsAsFactors=FALSE)
  train.subject <- read.table(fpath.train.subject, header=FALSE, stringsAsFactors=FALSE)
  
  
  feature <- read.table(fpath.feature, header=FALSE, stringsAsFactors=FALSE)
  activity <- read.table(fpath.activity, header=FALSE, stringsAsFactors=FALSE)
  ## merge test , train datasets and the corresponding lables
  test.act <- cbind(test,test.acty,test.subject)
  train.act <-cbind(train,train.acty,train.subject)
  total.dat <- rbind(test.act,train.act)
  names(total.dat) <- c(make.unique(feature$V2),"act","subject")
  
  req.feature <- grep("^[A-Za-z][A-Za-z-]*(mean[()]|std[()])[A-Za-z0-9(-)]*", names(total.dat), value=FALSE) 
  activity.mean.std.measurement <- total.dat %>%
                                   select(c(req.feature,562,563)) %>%
                                  mutate(activity.type=activity$V2[act]) 
               
  summary.activity.subject <-  activity.mean.std.measurement %>%
                               group_by(activity.type,subject)  %>%
                               summarise_each(funs(mean))
  
  write.table(summary.activity.subject,file="summary_activity_subject.txt",row.name=FALSE)
} 


  
   