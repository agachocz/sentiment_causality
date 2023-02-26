# loglikelihood function

lh <- function(params, st = c(1, 0, 0, 0), return_ksi = F, dynamic = T) { # return_ksi jest True, jeśli chcemy zwracać macierze ksi
  
  alpha1 = params[1]
  beta1 = params[2]
  alpha2 = params[3]
  beta2 = params[4]
  a1 = params[5]
  b1 = params[6]
  s1 = params[7]
  a2 = params[8]
  b2 = params[9]
  s2 = params[10]
  
  
  ksi = st # początkowe ksi t|t
  ksi_lag <- c(NA, NA, NA, NA) # początkowe ksi t+1|t, nie istnieje dla pierwszego t
  
  if(return_ksi){
    # zainicjowanie macierzy wszystkich ksi
    ksi_all <- ksi
    ksi_lag_all <- ksi_lag
  }
  
  
  log_lh <- 0
  
  if(!dynamic){
    p1 = exp(alpha1)/(1+exp(alpha1))
    p2 = exp(alpha2)/(1+exp(alpha2))
    
    p =  matrix(c(p1, 1-p1, 0, 0,  
                  0, 0, 1-p2, p2,
                  p1, 1-p1, 0, 0,  
                  0, 0, 1-p2, p2), nrow = 4, byrow = TRUE)
  }
  
  n <- length(x)
  
  for(i in 2:n) {
    
    if(dynamic){
      eq1 <- alpha1 + beta1*x[i]
      eq2 <- alpha2 + beta2*x[i]
      p1 <- exp(eq1)/(1+exp(eq1))
      p2 <- exp(eq2)/(1+exp(eq2))
      
      p =  matrix(c(p1, 1-p1, 0, 0,  
                    0, 0, 1-p2, p2,
                    p1, 1-p1, 0, 0,  
                    0, 0, 1-p2, p2), nrow = 4, byrow = TRUE)
    }
    
    # new version
    d = c(
      dnorm(y[i], mean=(a1 + b1*(y[i-1]-a1)), s1), # st = 1, s(t-1) = 1
      dnorm(y[i], mean=(a2 + b1*(y[i-1]-a1)), s2), # st = 2, s(t-1) = 1
      dnorm(y[i], mean=(a1 + b2*(y[i-1]-a2)), s1), # st = 1, s(t-1) = 2
      dnorm(y[i], mean=(a2 + b2*(y[i-1]-a2)), s2) # st = 2, s(t-1) = 2
    )
    
    ksi_lag <- t(p)%*%ksi
    # ksi lag
    denom <- sum(c(1, 1, 1, 1)%*%(ksi_lag*d))
    ksi <- (ksi_lag*d)/denom
    
    #print(log(denom))
    
    if(return_ksi){ # dołączenie kolejnych kolumn do macierzy ksi
      ksi_lag_all <- cbind(ksi_lag_all, ksi_lag)
      ksi_all <- cbind(ksi_all, ksi)
    }
    
    log_lh <- log_lh + log(denom) # logarytm funkcji wiarygodności
    
    #print(log_lh)
    
  }
  
  if(return_ksi){
    return(list(ksi_all, ksi_lag_all))
  } else {return(-log_lh)} # z minusem dla optim, bez dla GA
  
}

lh_test <- function(params, st = c(1, 0, 0, 0), return_ksi = F, dynamic = T) { # return_ksi jest True, jeśli chcemy zwracać macierze ksi
  
  a1 = params[1]
  b1 = params[2]
  s1 = params[3]
  a2 = params[1]
  b2 = params[2]
  s2 = params[3]
  alpha1 = params[7]
  beta1 = params[8]
  alpha2 = params[9]
  beta2 = params[10]
  
  ksi = st # początkowe ksi t|t
  ksi_lag <- c(NA, NA, NA, NA) # początkowe ksi t+1|t, nie istnieje dla pierwszego t
  
  if(return_ksi){
    # zainicjowanie macierzy wszystkich ksi
    ksi_all <- ksi
    ksi_lag_all <- ksi_lag
  }
  
  
  log_lh <- 0
  
  if(!dynamic){
    p1 = exp(alpha1)/(1+exp(alpha1))
    p2 = exp(alpha2)/(1+exp(alpha2))
    
    p =  matrix(c(p1, 1-p1, 0, 0,  
                  0, 0, 1-p2, p2,
                  p1, 1-p1, 0, 0,  
                  0, 0, 1-p2, p2), nrow = 4, byrow = TRUE)
  }
  
  n <- length(x)
  
  for(i in 2:n) {
    
    if(dynamic){
      eq1 <- alpha1 + beta1*x[i]
      eq2 <- alpha2 + beta2*x[i]
      p1 <- exp(eq1)/(1+exp(eq1))
      p2 <- exp(eq2)/(1+exp(eq2))
      
      p =  matrix(c(p1, 1-p1, 0, 0,  
                    0, 0, 1-p2, p2,
                    p1, 1-p1, 0, 0,  
                    0, 0, 1-p2, p2), nrow = 4, byrow = TRUE)
    }
    
    # new version
    d = c(
      dnorm(y[i], mean=(a1 + b1*(y[i-1]-a1)), s1), # st = 1, s(t-1) = 1
      dnorm(y[i], mean=(a2 + b1*(y[i-1]-a1)), s2), # st = 2, s(t-1) = 1
      dnorm(y[i], mean=(a1 + b2*(y[i-1]-a2)), s1), # st = 1, s(t-1) = 2
      dnorm(y[i], mean=(a2 + b2*(y[i-1]-a2)), s2) # st = 2, s(t-1) = 2
    )
    
    ksi_lag <- t(p)%*%ksi
    # ksi lag
    denom <- sum(c(1, 1, 1, 1)%*%(ksi_lag*d))
    ksi <- (ksi_lag*d)/denom
    
    #print(log(denom))
    
    if(return_ksi){ # dołączenie kolejnych kolumn do macierzy ksi
      ksi_lag_all <- cbind(ksi_lag_all, ksi_lag)
      ksi_all <- cbind(ksi_all, ksi)
    }
    
    log_lh <- log_lh + log(denom) # logarytm funkcji wiarygodności
    
    #print(log_lh)
    
  }
  
  if(return_ksi){
    return(list(ksi_all, ksi_lag_all))
  } else {return(-log_lh)} # z minusem dla optim, bez dla GA
  
}


y <- daily_data$WIG
x <- daily_data$EconomicGrowth
  
  m <- mean(y)
  s <- sd(y)
  
  start = c(alpha1 = 3, beta1 = 0, alpha2 = 4, beta2 = 0,
            a1=m, b1=0.1, s1=s, a2=m, b2 = 0.1, s2 = s)
  
  static <- optim(start, lh, dynamic = FALSE, hessian = TRUE)
  ksi_static <- lh(static$par, return_ksi = TRUE, dynamic = FALSE)
  
  dynamic <- optim(static$par, lh, hessian = TRUE)
  dynamic <- optim(dynamic$par, lh, hessian = TRUE)
  ksi_dynamic <- lh(dynamic$par, return_ksi = TRUE)
  
  results_EG_PL <- list(
    dynamic = dynamic, 
    ksi_dynamic = ksi_dynamic,
    static = static,
    ksi_static = ksi_static
  )
  
  
  stars <- function(p_val){
    stars = ''
    if(is.na(p_val)) return('NA')
    else if (p_val <= 0.01) {
      stars = '***'
    } else if (p_val <= 0.05) {
      stars = '**'
    } else if (p_val <= 0.1) {
      stars = '*'
    }
    return(stars)
  }
  
  # formatting table (csv)
  
format_table <- function(results, indicator){
  
  LM <- 2*(results$static$value - results$dynamic$value)
  p_val <- 1-pchisq(LM, 2)
  statistic <- paste0(format(round(LM, 3), nsmall = 3), stars(p_val))
  
  par = c(results$dynamic$par[5:10], results$dynamic$par[1:4])
  d <- diag(solve(results$dynamic$hessian))
  d <- c(d[5:10], d[1:4])
  p_vals = 2*(1-pnorm(abs(par/(d^0.5))))
  str <- vector()
  for(i in 1:length(p_vals)){
    str[i] = stars(p_vals[i])
  }
  
  params = paste0(format(round(par, 3), nsmall = 3), 
                  " (", format(round(p_vals, 3), nsmall = 3), ")")
  
  row1 <- data.frame(
    Indicator = indicator,
    LM = statistic, state = 'I',
    a = params[1], b = params[2], sigma = params[3],
    alpha = params[7], beta = params[8]
  )
  
  row2 <- data.frame(
    Indicator = NA,
    LM = NA, state = 'II',
    a = params[4], b = params[5], sigma = params[6],
    alpha = params[9], beta = params[10]
  )
  
  return(rbind(row1, row2))
}
  
table_CS <- format_table(results_CS_PL, "Current Situation")
table_Vol <- format_table(results_Vol_PL, "Volume")
table_EG <- format_table(results_EG_PL, "Economic Growth")  

table_all_PL <- rbind(table_EG, table_CS, table_Vol)


# test the occurrence of two distinct 
test_results <- list()

for(i in countries){
  y <- rates_short[rates_short$Date %in% zew_daily_clean$Date, i]
  
  m_s1 <- mean(y[1200:1600])
  m_s2 <- mean(y[1:1000])
  s_s1 <- sd(y[1200:1600])
  s_s2 <- sd(y[1:1000])
  
  start = c(a1=m_s1, b1=0.2, s1=s_s1, a2=m_s2, b2 = 0.1, s2 = s_s2, 
            alpha1 = 3, beta1 = 1, alpha2 = 4, beta2 = 1)
  
  # base <- optim(start[c(1:7, 9)], lh, dynamic = FALSE)
  
  free <- optim(start, lh, dynamic = FALSE, hessian = TRUE)
  restricted <- optim(start, lh_test, dynamic = FALSE, hessian = TRUE)
  
  statistic <- 2*(restricted$value - free$value)
  p.val <- 1-pchisq(statistic, 2)
  
  test_results[[i]] <- data.frame(Market = i, LM = statistic, p = p.val)
}

write.csv(rbindlist(test_results), "two regimes test.csv")

