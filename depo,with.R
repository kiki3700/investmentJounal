#deposit
deposit<-function(price,date){
  arrow_1_cash_balance<-read.csv('data/arrow_1/arrow_1_cash_balance.csv',row.names=1)
  arrow_1_cash<-read.csv('data/arrow_1/arrow_1_cash.csv',row.names = 1)
  #cash
  arrow_1_cash<-rbind(arrow_1_cash,data.frame(date=date,distribution=price,balance=last(arrow_1_cash$balance)+price,'function'='deposit'))
  print(arrow_1_cash)
  write.csv(arrow_1_cash,'data/arrow_1/arrow_1_cash.csv')
  #cash balance
  if(tail(rownames(arrow_1_cash_balance),1)==date){
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-(arrow_1_cash_balance[nrow(arrow_1_cash_balance),]+price)
  }else{
    arrow_1_cash_balance<-rbind(arrow_1_cash_balance,arrow_1_cash_balance[nrow(arrow_1_cash_balance),])
    rownames(arrow_1_cash_balance[nrow(arrow_1_cash_balance),])<-date
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-arrow_1_cash_balance[nrow(arrow_1_cash_balance),]+price
  }
  print(arrow_1_cash_balance)
  write.csv(arrow_1_cash_balance,'data/arrow_1_cash_balance.csv')
}
#withraw
withraw<-function(price,date){
  arrow_1_cash_balance<-read.csv('data/arrow_1/arrow_1_cash_balance.csv',row.names=1)
  arrow_1_cash<-read.csv('data/arrow_1/arrow_1_cash.csv',row.names = 1)
  #cash
  arrow_1_cash<-rbind(arrow_1_cash,data.frame(date=date,distribution=price,balance=last(arrow_1_cash$balance)-price,'function'='deposit'))
  print(arrow_1_cash)
  write.csv(arrow_1_cash,'data/arrow_1/arrow_1_cash.csv')
  #cash balance
  if(tail(rownames(arrow_1_cash_balance),1)==date){
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-arrow_1_cash_balance[nrow(arrow_1_cash_balance),]-price
  }else{
    arrow_1_cash_balance<-rbind(arrow_1_cash_balance,arrow_1_cash_balance[nrow(arrow_1_cash_balance),])
    rownames(arrow_1_cash_balance[nrow(arrow_1_cash_balance),])<-date
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-arrow_1_cash_balance[nrow(arrow_1_cash_balance),]-price
  }
  print(arrow_1_cash_balance)
  write.csv(arrow_1_cash_balance,'data/arrow_1_cash_balance.csv')
}