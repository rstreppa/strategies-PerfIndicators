library(plyr)
library(DataCombine)
library(xts)
library(taRifx)

source("EMD_Prophet_v4_fn_utility.R") 

##########################
# Performance Indicators #
##########################

perf_indicators <- function(oos_df, oos_xts, oos_idx)
{
  oos_df  <- plyr::rename(oos_df, c("xts" = "SPX"))
  p_res   <- DropNA(oos_df[c("SPX", "pos_res")], Var = "pos_res", message = FALSE)
  d_res   <- as.Date(rownames(p_res))
  p_res_s <- shift.data.frame(p_res, n=-1,wrap=FALSE,pad=TRUE)
  pnl_res <- p_res_s["pos_res"]*(p_res["SPX"]-p_res_s["SPX"])
  pnl_res[is.na(pnl_res)] <- 0.0
  AUM_res <- sum(abs(p_res["pos_res"])*p_res["SPX"])/length(p_res[,"SPX"])
  
  xts_res  <- xts(x = pnl_res, order.by=d_res, tzone="America/New York") #indices 0 and 1
  temp_res <- merge(oos_xts, xts_res, join='left', fill=0.0)
  oos_df[ ,"pnl_res"] <- temp_res[ , "pos_res"]
  oos_df[ ,"ret"]     <- oos_df[ ,"pnl_res"]/AUM_res
  
  cumperf   <- cumprod(1+oos_df[ ,"ret"])
  deltat    <- as.numeric(oos_idx[length(oos_idx)]-oos_idx[1], units="days")/365.25
  freq      <- length(oos_df[ ,"ret"]) / deltat
  aSR       <- mean(oos_df[ ,"ret"]) / sd(oos_df[ ,"ret"]) * sqrt(freq)
  aRoR      <- as.numeric((cumperf[length(cumperf)] - 1.0) / deltat * 100.0)
  freq_days <- deltat * 365.25 / length(d_res)
  retspx    <- oos_df[ ,"SPX"]/lag(oos_df[ ,"SPX"], 1)-1
  retspx[is.na(retspx)] <- 0.0
  cumspx    <- cumprod(1+retspx)
  tCost     <- NA
  tRate     <- NA 
  
  perf_ind  <- list("aSR"=aSR, "aRoR"=aRoR, "freq_days"=freq_days, "tCost"=tCost, "tRate"=tRate,
                    "cumperf"=cumperf, "cumspx"=cumspx)
  return(perf_ind)
}