s_owner <- unique(SO$Глобальные.конечный.собственник..ГКС....Название)
s_comps <- SO$Название.компании.Latin.alphabet[1:2209]
calc <- data.frame()


#SO$Presense <- NULL
presence_func_industry <- function(calc){
  k <- unique(calc$industry)
  df <- cbind(data.frame(calc$industry), calc$Глобальные.конечный.собственник..ГКС....Название)
  names(df) <- c('industry', 'guo')
  for(year in c(2012:2020)){
    title <- paste0('Presence.', year)
    pr <- paste0('Общие.активы.тыс.доллар.США.', year)
    calc$owned_vol <- abs(calc$ISH...Прямой../ 100 * calc[pr])
    df[title] <- rep(NA, nrow(df))
    for (i in k){
      s <- sum(calc$owned_vol[calc$industry == i & !is.na(calc$owned_vol)])
      df[title][calc$industry == i] <-calc$owned_vol[calc$industry == i] / s 
    }
  }
  return(df)
}

calc_gen <- rbind(SO,rbind(POD,POF))
calc <- presence_func_industry(calc_gen)

prs_gen <- data.frame(c(2012:2020))
names(prs_gen) <- 'year'
prs_gen$Presence <- rep(NA, nrow(prs_gen))

for(year in c(2012:2020)){
  pr <- paste0('Общие.активы.тыс.доллар.США.', year)
  calc_gen$owned_vol <- abs(calc_gen$ISH...Прямой../ 100 * calc_gen[pr])
  calc_gen$share <- calc_gen$owned_vol / sum(calc_gen$owned_vol[!is.na(calc_gen$owned_vol)])
  prs_gen$Presence[prs_gen$year == year] <- sum(calc_gen$share[calc_gen$Глобальные.конечный.собственник..ГКС....Название %in% s_owner & !is.na(calc_gen$share)])
}

prs <- aggregate(calc[(calc$guo %in% s_owner & !is.na(calc$Presence.2020)),c(3:11)],by = list(calc$industry[calc$guo %in% s_owner & !is.na(calc$Presence.2020)]), FUN = sum)

sum(SO$Presense[SO$industry == "ACCOMMODATION AND FOOD SERVICE ACTIVITIES" & !(SO$Глобальные.конечный.собственник..ГКС....Название %in% s_owner)])







write.table(POD, file = 'POD_fix.csv',sep = ';', fileEncoding = 'UTF-8')
prs <- read.csv('C:/Users/Dmitry/Desktop/НИР/Data/CSV/Presense.csv')






for ( i in c(2012:2020)){
  k <- paste0('ROA.EBIT',i)
  l <- paste0('Прибыль.на.общую.сумму.активов..ROA..при.использовании.отчета.о.прибылях.и.убытках.до.налогообложения.....',i)
  coef_frame[k] <-SO[l]
}

for ( i in c(2012:2020)){
  k <- paste0('resource.allocation',i)
  l <- paste0('Прибыль.убыток.за.период....Чистая.прибыль..тыс.доллар.США.',i)
  g <-paste0('Общие.активы.тыс.доллар.США.',i)
  coef_frame[k] <-SO[l] / SO[g]
}

#so_save <- SO
#SO <- SO[,1:287]

#func for coefs
seriesfunc <- function(df ,name ,prm1 ,prm2 ,year_start = 2012,year_end = 2020){
  for ( i in c(year_start:year_end)){
    k <- paste0(prm1,i)
    l <- paste0(prm2,i)
    g <- paste0(name, i)
    df[g] <- df[k] / df[l]
  }
  return(df)
}


#no need now
for(i in c(SO,POD,POF)){
  i <- seriesfunc(i, 'equity.cap' ,'Прибыль.убыток.за.период....Чистая.прибыль..тыс.доллар.США.' ,'Прибыль.на.акционерный.капитал..ROE..на.основе.чистого.дохода.....')
}


write.csv(prs, 'C:/Users/Dmitry/Desktop/НИР/Data/CSV/State_Presence.csv')
write.table(coef_frame, file = 'SO_coefs.csv',sep = ';', fileEncoding = 'UTF-8')
SO$Прибыль.на.акционерный.капитал..ROE..на.основе.чистого.дохода.....2012
