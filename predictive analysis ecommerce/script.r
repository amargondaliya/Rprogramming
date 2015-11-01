
#####Data extraction using Google analytics package##################
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd"></pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd"></pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Confusion matrix
confusion_matrix &lt;- table(test_data$revisit,test.predicted)

#Model Accuracy
accuracy &lt;- sum(diag(confusion_matrix))*100/nrow(test_data)
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd"> #Apply model on testing data
test.predicted.prob &lt;- predict(logit.model,newdata=test_data[,-ncol(test_data)],type="response")
test.predicted &lt;- round(test.predicted.prob)
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Logistic regression model
logit.model &lt;- glm(train_data$revisit~.,data=train_data,family = binomial("logit"))
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Tranining and Tesing data
index &lt;- sample(1:nrow(data),size=nrow(data)*0.8)
train_data &lt;- data[index,]
test_data &lt;- data[-index,]
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Remove visitor id
data &lt;- data[,-1]
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Response variable
data$revisit &lt;- as.numeric(data$sessionCount>1&data$daysSinceLastSession<10)</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Remove query parameters from the url
data$landingPagePath &lt;- gsub("\\?.*","",data$landingPagePath)
data$landingPagePath &lt;- gsub("http://pingax.com/","",data$landingPagePath)
data$exitPagePath &lt;- gsub("\\?.*","",data$exitPagePath)
data$exitPagePath &lt;- gsub("http://pingax.com/","",data$exitPagePath)
</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#Rename column name
names(data)[names(data) == 'dimension1'] &lt;- 'visitorId'</pre>
<pre style="background: none repeat scroll 0 0 #eeeeee;border: 1px solid #dddddd">#read Dataset
data &lt;- read.csv("data.csv",stringsAsFactors=F)</pre>
# Create a new Google Analytics API object
ga <- RGoogleAnalytics()

# Authorize your account and paste the accesstoken 
query <- QueryBuilder()
access_token <- query$authorize()

# Create a new Google Analytics API object
ga <- RGoogleAnalytics()
ga.profiles <- ga$GetProfileData(access_token)

# List the GA profiles 
ga.profiles

#Build the query string 
query$Init(start.date = "2014-04-09",
          end.date = "2014-12-09",
          dimensions = "ga:dimension1,ga:medium,ga:landingPagePath,ga:exitPagePath,ga:userType,ga:sessionCount,ga:daysSinceLastSession",
          metrics = "ga:sessions,ga:pageviews,ga:uniquePageviews,ga:sessionDuration",
          #sort = "ga:visits",
          max.results = 11000,
          table.id = paste("ga:",ga.profiles$id[3],sep="",collapse=","),
          access_token=access_token)

# Make a request to get the data from the API
ga.data <- ga$GetReportData(query)

# Look at the returned data
head(ga.data)

#Save extracted data points
write.csv(ga.data,"data.csv",row.names=F)



###############Data Preprocessing part ########################################

#assign ga.data to data
data <- ga.data

set.seed(110)

#read Dataset
data <- read.csv("data_2.csv",stringsAsFactors=F)

#Rename column name
names(data)[names(data) == 'dimension1'] <- 'visitorId'

#Remove query parameters from the url
data$landingPagePath <- gsub("\\?.*","",data$landingPagePath)
data$landingPagePath <- gsub("http://pingax.com/","",data$landingPagePath)
data$exitPagePath <- gsub("\\?.*","",data$exitPagePath)
data$exitPagePath <- gsub("http://pingax.com/","",data$exitPagePath)

#Response variable
data$revisit <- as.numeric(data$sessionCount>1&data$daysSinceLastSession<10)

#Remove visitor id
data <- data[,-1]


#Tranining and Tesing data
index <- sample(1:nrow(data),size=nrow(data)*0.8)


train_data <- data[index,]

test_data <- data[-index,]

#Distribution
table(train_data$revisit)
table(test_data$revisit)

#Logistic regression model
logit.model <- glm(train_data$revisit~.,data=train_data,family = binomial("logit"))


#Apply model on testing data
test.predicted.prob <- predict(logit.model,newdata=test_data[,-ncol(test_data)],type="response")
test.predicted <- round(test.predicted.prob)

#Confusion matrix
confusion_matrix <- table(test_data$revisit,test.predicted)

#Model Accuracy
accuracy <- sum(diag(confusion_matrix))*100/nrow(test_data)

#ROC curve function
roc.curve=function(s,print=FALSE){
  Ps=(test.predicted.prob>s)*1
  FP=sum((Ps==1)*(test_data$revisit==0))/sum(test_data$revisit==0)
  TP=sum((Ps==1)*(test_data$revisit==1))/sum(test_data$revisit==1)
  if(print==TRUE){
    print(table(Observed=test_data$revisit,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

#Set classification threshold
threshold <- 0.5

#Obtain roc components
roc.curve(threshold,print=TRUE)

#Create ROC plot
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="False Positive Rate",ylab="True Positive Rate")


#Processing part
sum(test_data$landingPagePath=="/logistic-regression-wirh-r-step-by-step-implementation-part-2/")
sum(test_data$landingPagePath=="/2013/11/")
sum(test_data$landingPagePath=="/2014/06/")
sum(test_data$landingPagePath=="/2014/09/")  
sum(test_data$landingPagePath=="/jeevanjyot/post2.png")
sum(-test_data$landingPagePath%in%c("/logistic-regression-wirh-r-step-by-step-implementation-part-2/","/2013/11/","/2014/06/","/2014/09/","/jeevanjyot/post2.png"))

test_data <- test_data[!test_data$landingPagePath%in%c("/logistic-regression-wirh-r-step-by-step-implementation-part-2/","/2013/11/","/2014/06/","/2014/09/","/jeevanjyot/post2.png"),]

test_data <- test_data[!test_data$exitPagePath%in%c("/build-predictive-model-big-data-using-r-mysql-part-1/","/category/r/goo","/tag/predictive-analsyis/"),]

data <- data[!data$landingPagePath%in%remove_l,]
data <- data[!data$exitPagePath%in%remove_e,]
write.csv(data,"data_2.csv",row.names=F)
data
remove_l <- c("/linear-regression-with-r-step-by-step-implementation-part-2/?fb_action_ids=10152334858547875&fb_action_types=og.likes",
            "/http://pingax.com/mongodb-schema-design/",
            "/search?q=cache:i8RPi3FKgcoJ:pingax.com/mongodb-basics-with-java/+&cd=6&hl=pt-BR&ct=clnk&gl=br&client=firefox-a",
            "/2014/04/",
            "/http://pingax.com/predictive-analysis-in-ecommerce/",
            "/2014/05/",
            "/2014/03/",
            "/logistic-regression-wirh-r-step-by-step-implementation-part-2/",
            "/tag/linear-regression/",
            "/build-predictive-model-big-data-using-r-mysql-part-1",
            "/translate_c?depth=1&hl=ko&prev=/search?q=eclipse+mapreduce+example&newwindow=1&sa=X&espv=2&biw=1280&bih=635&rurl=translate.google.co.kr&sl=en&u=http://pingax.com/getting-started-with-mapreduce-using-eclipse-2/&usg=ALkJrhhKLcawJQqYnXqHdXc5WnIrqKBWoA",
            "/getting-started-with-mapreduce-using-eclipse-2/http:/www.orzota.com/mapreduce-tutorial/",
            "/logistic-regression-r-step-step-implementation-part-2/?fb_action_ids=10204612433100956&fb_action_types=og.likes",
            "/build-predictive-model-big-data-using-r-mysql-part-1/",
            "/2013/11/",
            "/2014/09/",
            "/search?q=cache:GVNdMJcbU30J:pingax.com/category/r/+&cd=1&hl=zh-TW&ct=clnk&gl=tw",
            "/?p=597&preview=true",
            "/logistic regression r step by step",
            "/jeevanjyot/post2.png",
            "/tag/logistic-regression-model/",
            "/category/uncategorized/",
            "/translate_c?depth=1&hl=es&prev=/search?q=r+hadoop+integration&start=10&client=tablet-android-samsung&sa=N&espv=1&biw=1280&bih=800&rurl=translate.google.com.co&sl=en&u=http://pingax.com/how-can-r-and-hadoop-be-used-together/&usg=ALkJrhiVplW9TziqZNPLIZhPun99ssNCCw",
            "/build-predictive-model-big-data-using-r-mysql-part-2/?preview=true&preview_id=594&preview_nonce=5b771f7b17&post_format=standard",
            "/translate_c?depth=1&hl=ko&prev=/search?q=regularization&start=80&newwindow=1&sa=N&biw=1024&bih=706&rurl=translate.google.co.kr&sl=en&u=http://pingax.com/regularization-implementation-r/&usg=ALkJrhh1H3Br3g0uWXgvePHa6RJlopusiA",
            "/mongodb-schema-dhttp:/webapplog.com/express-js-4-node-js-and-mongodb-rest-api-tutorial/esign/",
            "/trick-convert-mongo-shell-query-equivalent-java-objects/?utm_reader=feedly",
            "/2014/06/",
            "/page/3/",
            "/introduction-mongodb/?x=21&y=9")


remove_e <- c("/contact-us/?fb_action_ids=10152334096047875&fb_action_types=og.likes",
              "/tag/linear-regression/",
              "/author/sagar/",
              "/translate_c?depth=1&hl=es&prev=/search?q=r+hadoop+integration&start=10&client=tablet-android-samsung&sa=N&espv=1&biw=1280&bih=800&rurl=translate.google.com.co&sl=en&u=http://pingax.com/how-can-r-and-hadoop-be-used-together/&usg=ALkJrhiVplW9TziqZNPLIZhPun99ssNCCw",
              "/tag/machine-learning/",
              "/linear-regression-with-r-step-by-step-implementation-part-3/",
              "/http://pingax.com/linear-regression-with-r-step-by-step-implementation-part-2/",
              "/search?q=cache:i8RPi3FKgcoJ:pingax.com/mongodb-basics-with-java/+&cd=6&hl=pt-BR&ct=clnk&gl=br&client=firefox-a",
              "/introduction-mongodb/?x=21&y=9",
              "/search?q=cache:GVNdMJcbU30J:pingax.com/category/r/+&cd=1&hl=zh-TW&ct=clnk&gl=tw",
              "/jeevanjyot/post2.png",
              "/tag/bais-and-variance/",
              "/mongodb-schema-dhttp:/webapplog.com/express-js-4-node-js-and-mongodb-rest-api-tutorial/esign/",
              "/logistic regression r step by step",
              "/http://pingax.com/mongodb-schema-design/",
              "/trick-convert-mongo-shell-query-equivalent-java-objects/?utm_reader=feedly",
              "/category/r/goo",
              "/indexing-with-mongodb/?format=pdf",
              "/translate_c?depth=1&hl=ko&prev=/search?q=regularization&start=80&newwindow=1&sa=N&biw=1024&bih=706&rurl=translate.google.co.kr&sl=en&u=http://pingax.com/regularization-implementation-r/&usg=ALkJrhh1H3Br3g0uWXgvePHa6RJlopusiA",
              "/logistic-regression-wirh-r-step-by-step-implementation-part-2/",
              "/logistic-regression-r-step-step-implementation-part-2/?fb_action_ids=10204612433100956&fb_action_types=og.likes",
              "/translate_c?depth=1&hl=ko&prev=/search?q=eclipse+mapreduce+example&newwindow=1&sa=X&espv=2&biw=1280&bih=635&rurl=translate.google.co.kr&sl=en&u=http://pingax.com/getting-started-with-mapreduce-using-eclipse-2/&usg=ALkJrhhKLcawJQqYnXqHdXc5WnIrqKBWoA",
              "/build-predictive-model-big-data-using-r-mysql-part-1/",
              "/querying-with-shell/",
              "/phpmyadmin",
              "/getting-started-with-mapreduce-using-eclipse-1/",
              "/build-predictive-model-on-big-data-using-r-and-mysql-part-1/?preview=true&preview_id=590&preview_nonce=1d52b1a6c2&post_format=standard",
              "/logistic-regression-wirh-r-step-by-step-implementation-part-2")
