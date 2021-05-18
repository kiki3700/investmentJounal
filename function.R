library(quantmod)
library(stringr )
library(PerformanceAnalytics)
library(magrittr)
library(lubridate)
library(timetk)
library(xts)
library(httr)
library(rvest)
library(readr)
#buy
buy<-function(ticker,price,unit,date,wichport){
  #데이터 호출
  arrow_1_unit<-read.csv('data/arrow_1/arrow_1_unit.csv',row.names = 1,stringsAsFactors = F)
  arrow_1_stock_list<-read.csv('data/arrow_1/arrow_1_stock_list.csv',row.names = 1,stringsAsFactors = F)
  arrow_1_trading<-read.csv('data/arrow_1/arrow_1_trading.csv',row.names = 1,stringsAsFactors = F)
  arrow_1_cash_balance<-read.csv('data/arrow_1/arrow_1_cash_balance.csv',row.names=1,stringsAsFactors = F)
  arrow_1_cash<-read.csv('data/arrow_1/arrow_1_cash.csv',row.names = 1,stringsAsFactors = F)
  price<-price%>%as.numeric()
  unit<-as.numeric(unit)
  #cash에서 출금
  arrow_1_cash<-rbind(arrow_1_cash,data.frame('date'=date,distribution=-(price*unit),balance=last(arrow_1_cash$balance)-(price*unit),'function'='buy'))
  print(arrow_1_cash)
  write.csv(arrow_1_cash,'data/arrow_1/arrow_1_cash.csv')
  #cash_balance에 기입
  if(rownames(arrow_1_cash_balance)[nrow(arrow_1_cash_balance)]==date){
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-arrow_1_cash_balance[nrow(arrow_1_cash_balance),]-(price*unit)
  }else{
    arrow_1_cash_balance<-rbind(arrow_1_cash_balance,arrow_1_cash_balance[nrow(arrow_1_cash_balance),])
    rownames(arrow_1_cash_balance)[nrow(arrow_1_cash_balance)]<-date
    arrow_1_cash_balance[nrow(arrow_1_cash_balance),]<-arrow_1_cash_balance[nrow(arrow_1_cash_balance),]-(price*unit)
  }
  print(arrow_1_cash_balance)
  write.csv(arrow_1_cash_balance,'data/arrow_1/arrow_1_cash_balance.csv')
  #trading
  arrow_1_trading$ticker<-str_pad(arrow_1_trading$ticker,6,side=c('left'),0)
  arrow_1_trading<-rbind(arrow_1_trading, data.frame('date'=date, 'ticker'=ticker, 'price'=price, 'unit'=unit, buy.sell='buy', realized_profit=0))
  print(arrow_1_trading)
  write.csv(arrow_1_trading,'data/arrow_1_trading.csv')
  #stock list
  arrow_1_stock_list$ticker<-str_pad(arrow_1_stock_list$ticker,6,side=c('left'),0)
  if(ticker%in%arrow_1_stock_list$'ticker'){
    arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,2]<-arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,2]+unit
    arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,4]<-arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,4]+(price*unit)
    arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,3]<-arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,4]/arrow_1_stock_list[arrow_1_stock_list$'ticker'%in%ticker,2]
  }else{
    arrow_1_stock_list=rbind(arrow_1_stock_list,data.frame('ticker'=ticker,'unit'=unit,ave_price=price,total_value=(price*unit),p_date=date,s_date=NA))
  }
  print(arrow_1_stock_list)
  write.csv(arrow_1_stock_list,'data/arrow_1/arrow_1_stock_list.csv')
  #unit
  #1날짜갱신안됬을때
  colnames(arrow_1_unit)<-substr(colnames(arrow_1_unit),2,7)
  if(!date%in%rownames(arrow_1_unit)){
    arrow_1_unit<-rbind(arrow_1_unit,arrow_1_unit[nrow(arrow_1_unit),])
    row.names(arrow_1_unit)[nrow(arrow_1_unit)]<-date
    #1-1갱신 후 새로운종목 추가
    if(ticker%in%colnames(arrow_1_unit)){
      arrow_1_unit[nrow(arrow_1_unit),ticker]<-arrow_1_unit[nrow(arrow_1_unit),ticker]+unit
    }else{
      #1-2갱신 후 기존 종목 수정 
      new_unit<-data.frame(x=c(rep(NA,(nrow(arrow_1_unit)-1)),unit))
      colnames(new_unit)<-ticker
      arrow_1_unit<-cbind(arrow_1_unit,new_unit)
      arrow_1_unit[nrow(arrow_1_unit),ticker]<-arrow_1_unit[nrow(arrow_1_unit),ticker]+unit
    }
  }else{#2날짜가 갱신 되어 있을때
    if(ticker%in%colnames(arrow_1_unit)){
      #2-1새로운 종목 추가
      
      arrow_1_unit[nrow(arrow_1_unit),ticker]<-arrow_1_unit[nrow(arrow_1_unit),ticker]+unit
    }else{
      #2-2기존 종목 수정
      new_unit<-data.frame(x=c(rep(NA,(nrow(arrow_1_unit)-1)),unit))
      colnames(new_unit)<-ticker
      arrow_1_unit<-cbind(arrow_1_unit,new_unit)
    }
  }
  print(arrow_1_unit)
  write.csv(arrow_1_unit,'data/arrow_1/arrow_1_unit.csv')
}
