
## Cash Flow Generating function
gen_CashFlow <- function(settleDate,maturityDate, Coupon,Freq = 1){
  if(Freq==2){
    paymentDate <-seq.Date(maturityDate, settleDate, by = "-6 month")%>%rev()
    paymentDate[paymentDate>=settleDate]
    CF <- c(rep(Coupon/Freq, length(paymentDate)-1), 100 + Coupon/Freq)
  }else if(Freq==1){
    paymentDate <-seq.Date(maturityDate, settleDate, by = "-1 year")%>%rev()
    CF <- c(rep(Coupon/Freq, length(paymentDate)-1), 100 + Coupon/Freq)
  }else{
    paymentDate <- maturityDate
    CF <- 100 + Coupon 
  }
  return(list(paymentDate = paymentDate, CF = CF))
}
#' @input 1. Trading Date(Settle Date); 2. MAturity Date; 3, Coupon; 4. Payment Frequency
#' @output a list inclue `paymentDate` and `CashFlow` 
#' @example gen_CashFlow(settleDate = as.Date("2015-01-10"), 
#'          maturityDate = as.Date("2020-12-10"),Coupon = 3.66,Freq = 1)
#'      #    $paymentDate
#'      # [1] "2015-12-10" "2016-12-10" "2017-12-10" "2018-12-10" "2019-12-10"
#'      #[6] "2020-12-10"

## Accrued  Interest Caculating Function

calcAccrued <- function(settleDate, maturityDate, Coupon, Freq){
  CF <- gen_CashFlow(settleDate,maturityDate, Coupon,Freq)
  days2prepayment <- as.numeric(365/Freq -(min(CF$paymentDate) - settleDate))
  return(Coupon*days2prepayment/365)
}
#' @import gen_CashFlow
#' @input 1. Trading Date(Settle Date); 2. MAturity Date; 3, Coupon; 4. Payment Frequency
#' @output  Accrued  Interest
#' @example calcAccrued(settleDate = as.Date("2015-01-10"), 
#'          maturityDate = as.Date("2020-12-10"),Coupon = 3.66,Freq = 1)
#'  #        [1] 0.3108493

constrCouponbonds <- function(Dates,data,names = NULL){
  #Date = as.Date("2014-07-15")
  couponbonds <- list()
  if(length(names)==length(Dates)){
    names = names
  }else if(length(names)==1){
    names = rep(names, length(Dates))
  }else if(length(names)==0){
    names = as.character(Dates)
  } 
  for(i in seq_along(Dates)){
    Date <- Dates[i]
    name <- names[i]
    Date  <- as.Date(Date)
    dat <- data[TradingDate==Date]
    ISIN <- dat$ISIN
    ISSUEDATE <- dat$ISSUEDATE
    MATURITYDATE <- dat$MATURITYDATE
    PRICE <- dat$PRICE
    COUPONRATE <- dat$COUPONRATE
    ACCRUED <- dat$ACCRUED
    CASHFLOWS <- list()
    CF <- apply(dat, MARGIN = 1, 
                FUN = function(x) gen_CashFlow(settleDate =as.Date(x[1]), 
                                               maturityDate = as.Date(x[4]), 
                                               Coupon = as.numeric(x[5]), 
                                               Freq =  as.numeric(x[7])))
    names(CF) <- ISIN
    CF_LEN <- list.map(CF, item~{length(item$CF)})%>%unlist()
    CASHFLOWS$ISIN <- lapply(1:length(CF_LEN), function(i) rep(names(CF_LEN)[i], CF_LEN[i]))%>%unlist()
    CASHFLOWS$CF <- list.map(CF, item~{item$CF})%>%unlist()
    names(CASHFLOWS$CF) <- NULL
    CASHFLOWS$DATE <- list.map(CF, item~{item$paymentDate})%>%unlist()%>%as.Date(origin  = as.Date("1970-01-01"))
    names(CASHFLOWS$DATE) <- NULL
    group <- list(ISIN=ISIN, MATURITYDATE = MATURITYDATE,
                  ISSUEDATE = ISSUEDATE, COUPONRATE = COUPONRATE,PRICE = PRICE,
                  ACCRUED = ACCRUED, CASHFLOWS = CASHFLOWS, TODAY = Date)
    group <- list(group)
    names(group) <- name
    couponbonds[names(group)] <- group
  }
  class(couponbonds) <- "couponbonds"
  return(couponbonds)
} # Constrct A `couponbonds` object.

#' @import gen_CashFlow
#' @input 1. Trading Date(Settle Date); 2, data.table include this Trading Date Market Info
#' @output  a `couponbonds` object 
#' @example constrCouponbonds(Date = "2014-10-10", 
#' data = coupon.IB, 
#' name = "CHINA")
#' 


constrDycouponBond <- function(from, to, data, name =NULL){
  dateRange <- as.Date(c(from, to))
  dat <- data[TradingDate%between%dateRange]
  tradingdays <- unique(dat$TradingDate)
  dycouponbonds <- lapply(tradingdays, constrCouponbonds, data = dat,name = name)
  class(dycouponbonds) <- "dyncouponbonds"
  return(dycouponbonds)
} # Construct ``

#' @import constrCouponbonds
#' @input 1. From Date; 2. To Date; 3. data.table include this Trading Date Market Info; 4. Name
#' @output `dyncouponbonds` object 
#' @example constrCouponbonds(Date = "2014-10-10", 
#' data = coupon.IB, 
#' name = "CHINA")
#'