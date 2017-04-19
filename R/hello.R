cross_validatelm<-function(df,linear_model,n_iter,sr)
{
  df <- as.data.frame(df)
  library("rpart")
  #We will compare two models- predictor
  mean_subset<-c();
  mean_all<-c();
  dep<-all.vars(terms(linear_model))[1]
  indep<-list()
  relation_all=as.formula(paste(dep,'.',sep="~"))
  i<-1
  while(i<length(all.vars(terms(linear_model)))){
    indep[[i]]<-all.vars(terms(linear_model))[i+1]
    i<-i+1
  }
  b <- paste(indep, collapse ="+")
  relation_subset<-as.formula(paste(dep,b,sep="~"))
  for(i in 1:n_iter){
    sample <- sample.int(n = nrow(df), size = floor(sr*nrow(df)), replace = F)
    train <- df[sample, ]
    testing  <- df[-sample, ]
    first.lm<-lm(relation_subset, data=train)
    second.lm<-lm(relation_all, data=train)
    pred1.lm<-predict(first.lm,newdata=testing)
    pred2.lm<-predict(second.lm, newdata=testing)
    mean1<-mean((as.numeric(pred1.lm)-testing[,dep])^2)
    mean2<-mean((as.numeric(pred2.lm)-testing[,dep])^2)
    mean_subset<-c(mean_subset,mean1)
    mean_all<-c(mean_all,mean2)
  }

  return (data.frame(accuracy_subset=mean_subset,accuracy_all= mean_all))
}
