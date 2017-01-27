run_analysis<-function(){
    #read in the test, train and metadata datasets
    features<-read.table("./features.txt")
    activity_labels<-read.table("./activity_labels.txt")
    
    train_x<-read.table("./train/X_train.txt")
    train_y<-read.table("./train/y_train.txt")
    train_sub<-read.table("./train/subject_train.txt")
    
    test_x<-read.table("./test/X_test.txt")
    test_y<-read.table("./test/y_test.txt")
    test_sub<-read.table("./test/subject_test.txt")
    
    #set col names
    names(train_sub)<-"Subject Number"
    names(train_y)<-"Activity Type"
    names(train_x)<-features[,2]   
    names(test_sub)<-"Subject Number"
    names(test_y)<-"Activity Type"
    names(test_x)<-features[,2]
    
    #combine x,y,sub for train and test
    train<-cbind(train_sub,train_y,train_x)
    test<-cbind(test_sub,test_y,test_x)
    
    #merge 
    combinedData<-rbind(test,train)
    
    #Get the columns of means and std
    meanStdCols<-grep("mean|std",features[,2])
    msData<-combinedData[,c(1,2,as.vector(meanStdCols)+2)]
    
    #Output data
    if(!file.exists("tidyData.txt")){
        file.create("tidyData.txt")
    }
    write.table(msData,"tidyData.txt")
    
    #clear workspace
    rm(test_x)
    rm(test_y)
    rm(test_sub)
    rm(train_x)
    rm(train_y)
    rm(train_sub)
    rm(features)
    rm(test)
    rm(train)
    rm(combinedData)
    
    #step 5 commences 
    #create data.Frame
    mat<-matrix(nrow = 180,ncol=81)
    finalMeans<-data.frame(mat)
    rm(mat)
    names(finalMeans)<-names(msData)
    
    #find mean of each unique subject, activity pair
    combos<-as.numeric(row.names(unique(msData[,1:2])))
    temp<-vector(length=81)
    for(i in seq(1:180)){
        temp[1:2]<-msData[combos[i],1:2]
        val<-msData[msData[combos[i],1] == msData[,1] &
                        msData[combos[i],2] == msData[,2],]
        finalMeans[i,]<-apply(val,2,mean)
    }
    finalMeans<-finalMeans[order(finalMeans[,1],finalMeans[,2]),]
    row.names(finalMeans)<-NULL
    
    #Write out data;
    if(!file.exists("step5TidyData.txt")){
        file.create("step5TidyData.txt")
    }
    write.table(finalMeans,"step5TidyData.txt",row.names = FALSE)
}
