power <- function(startdate,enddate,kwh,public,houses){
  library("lubridate")
  startdate <- as.Date(startdate,format="%Y-%m-%d")
  enddate <- as.Date(enddate,format="%Y-%m-%d")                                                   
  if(month(startdate) %in% c(6,7,8,9) & month(enddate) %in% c(6,7,8,9)){  #都在夏季用電時段
    summerdays <- difftime(enddate,startdate,units = 'days')
    normaldays = 0
    summerdays <- as.integer(summerdays)
  }
  else if(month(startdate) %in% c(6,7,8,9) & month(enddate) %in% c(10,11,12)){ #夏天開始，非夏結束
    summerdays <- difftime('2022-10-01',startdate,units = 'days')
    normaldays <- difftime(enddate,'2022-10-01',units = 'days')
    summerday <- as.integer(summerdays)
    summerdays <- summerday + 1
    normalday <- as.integer(normaldays)
    normaldays <- normalday + 1
  }
  else if(month(startdate) %in% c(1,2,3,4,5) & month(enddate) %in% c(6,7,8,9)){   #非夏開始，夏天結束
    summerdays <- difftime(enddate,'2022-06-01',units = 'days')
    normaldays <- difftime('2022-06-01',startdate,units = 'days')
    summerday <- as.integer(summerdays)
    summerdays <- summerday + 1
    normalday <- as.integer(normaldays)
    normaldays <- normalday + 1
  }
  else{            #非夏開始，非夏結束
    normaldays <- difftime(enddate,startdate,units='days')
    summerdays = 0
    normaldays <- as.integer(normaldays)
  }
  days <- summerdays + normaldays
  summer <- (summerdays/days)  
  normal <- (normaldays/days) 
  publicprice <- (public / houses) 
  kwhone <- kwh / 2
  
cat("總天數:",days,"\n")
cat('夏天天數:',summerdays,"\n")
cat('非夏天天數:',normaldays,"\n")

if (kwh <= 40){
    powerprice <- 1.63 * 40 + publicprice
}
  else if (kwh > 40 & kwh <= 120){
    powerprice <- (1.63 * kwh * summer)+(1.63*kwh*normal)+publicprice
  }
  else if (kwh > 120 & kwh <= 330){
    powerprice <- (1.63*240*summer)+(1.63*240*normal)+(2.38*(kwh-240)*summer)+(2.10*(kwh-240)*normal)+publicprice
  }
  else if(kwh >330 & kwh<= 500){
    powerprice <- (1.63 * 240 * summer)+(1.63*240*normal)+(2.38*420*summer)+(2.10*420*normal)+(3.52*(kwh-240-420)*summer)+(2.89*(kwh-240-420)*normal)+publicprice
  }
  else if(kwh >500 & kwh<= 700){
    powerprice <- (1.63 * 240 * summer)+(1.63*240*normal)+(2.38*420*summer)+(2.10*420*normal)+(3.52*37*summer)+(2.89*37*normal)+publicprice
  }
  else if(kwh >700 & kwh <= 1000){
    powerprice <- (1.63 * 240 * summer)+(2.38*420*summer)+(3.52*340*summer)+(4.8*400*summer)+(5.66*(kwh-240-420-340-400)*summer)+(1.63*240*normal)+(2.10*420*normal)+(2.89*340*normal)+(3.94*400*normal)+(4.6*(kwh-240-420-340-400)*normal)+publicprice
  }
  else{
    powerprice <- (1.63 * 240 * summer)+(2.38*420*summer)+(3.52*340*summer)+(4.8*400*summer)+(5.66*600*summer)+(6.41*(kwh-240-420-340-400-600)*summer)+(1.63*240*normal)+(2.10*420*normal)+(2.89*340*normal)+(3.94*400*normal)+(4.6*600*normal)+(5.03*(kwh-240-420-340-400-600)*normal)+publicprice
  }
  
cat("此區間內電價為:",round(powerprice))

}

power('2022-09-26','2022-11-23',697,0,1)




      
