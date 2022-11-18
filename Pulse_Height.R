library(plyr)


# Function to plot Pulse height spectrum of the entire dataset 

# This will take a our condensed dframe as input 

# This is a first attempt but a workable one nonetheless 

avgpulse <- function(df) {
  
  meanadc <- c()
  
  meanadc[1] <- mean(c(df$V1, df$V31, df$V61, df$V91), na.rm = T)
  meanadc[2] <- mean(c(df$V2, df$V32, df$V62, df$V92), na.rm = T)
  meanadc[3] <- mean(c(df$V3, df$V33, df$V63, df$V93), na.rm = T)
  meanadc[4] <- mean(c(df$V4, df$V34, df$V64, df$V94), na.rm = T)
  meanadc[5] <- mean(c(df$V5, df$V35, df$V65, df$V95), na.rm = T)
  meanadc[6] <- mean(c(df$V6, df$V36, df$V66, df$V96), na.rm = T)
  meanadc[7] <- mean(c(df$V7, df$V37, df$V67, df$V97), na.rm = T)
  meanadc[8] <- mean(c(df$V8, df$V38, df$V68, df$V98), na.rm = T)
  meanadc[9] <- mean(c(df$V9, df$V39, df$V69, df$V99), na.rm = T)
  meanadc[10] <- mean(c(df$V10, df$V40, df$V70, df$V100), na.rm = T)
  
  meanadc[11] <- mean(c(df$V11, df$V41, df$V71, df$V101), na.rm = T)
  meanadc[12] <- mean(c(df$V12, df$V42, df$V72, df$V102), na.rm = T)
  meanadc[13] <- mean(c(df$V13, df$V43, df$V73, df$V103), na.rm = T)
  meanadc[14] <- mean(c(df$V14, df$V44, df$V74, df$V104), na.rm = T)
  meanadc[15] <- mean(c(df$V15, df$V45, df$V75, df$V105), na.rm = T)
  meanadc[16] <- mean(c(df$V16, df$V46, df$V76, df$V106), na.rm = T)
  meanadc[17] <- mean(c(df$V17, df$V47, df$V77, df$V107), na.rm = T)
  meanadc[18] <- mean(c(df$V18, df$V48, df$V78, df$V108), na.rm = T)
  meanadc[19] <- mean(c(df$V19, df$V49, df$V79, df$V109), na.rm = T)
  meanadc[20] <- mean(c(df$V20, df$V50, df$V80, df$V110), na.rm = T)
  
  meanadc[21] <- mean(c(df$V21, df$V51, df$V81, df$V111), na.rm = T)
  meanadc[22] <- mean(c(df$V22, df$V52, df$V82, df$V112), na.rm = T)
  meanadc[23] <- mean(c(df$V23, df$V53, df$V83, df$V113), na.rm = T)
  meanadc[24] <- mean(c(df$V24, df$V54, df$V84, df$V114), na.rm = T)
  meanadc[25] <- mean(c(df$V25, df$V55, df$V85, df$V115), na.rm = T)
  meanadc[26] <- mean(c(df$V26, df$V56, df$V86, df$V116), na.rm = T)
  meanadc[27] <- mean(c(df$V27, df$V57, df$V87, df$V117), na.rm = T)
  meanadc[28] <- mean(c(df$V28, df$V58, df$V88, df$V118), na.rm = T)
  meanadc[29] <- mean(c(df$V29, df$V59, df$V89, df$V119), na.rm = T)
  meanadc[30] <- mean(c(df$V30, df$V60, df$V90, df$V120), na.rm = T)
  
  ph_df <- cbind.data.frame(Time_bin = c(1:30), meanadc)
  
  phplot <- ggplot(data=ph_df, aes(x=Time_bin, y=meanadc)) +
    geom_point(col="firebrick", size=2) + 
    geom_line(col="firebrick3", lwd=0.9) + 
    labs(x="Time bin", y="ADC", title ="Average Pulse Height") + 
    theme_bw()
  return(list(phplot, meanadc))
  
  
}


#q <- avgpulse(df)
# q[[1]]    -------> returns the plot
# q[[2]]    -------> returns the dataframe of values 

# ===================================================
# WAIT A SEC !  I think we can do the above in a much for iterative way with a for loop
# There are 30 timbins so 30 values per signal in our df !


# -------------------============-----------===============------------=========
# Here is an alternative (a better way too !)

avgpulse2 <- function(df, pc="firebrick", lc="brown3") {
  
  # Total number of signals in dataset df 
  num_sigs = nrow(df)

  m_adc <- c()
  
  for(i in 1:30) {
    m_adc[i] <- sum(c(df[, i], df[,i+30], df[, i+60]), na.rm = T)/num_sigs
  }
  
  ph_df <- cbind.data.frame(Time_bin = c(0:29), m_adc)
  
  phplot <- ggplot(data=ph_df, aes(x=Time_bin, y=m_adc)) +
    geom_point(col=pc, size=2.3) + 
    geom_line(col=lc, lwd=0.91) + 
    labs(x="Time bin (100ns)", y="ADC", title ="Average Pulse Height") + 
    theme_bw() +  
    theme(plot.title = element_text(hjust = 0.5))    # center title text 
  
  ggsave("ph.png", plot = phplot, width = 13, height = 6)
  return(list(phplot, ph_df))
  
}


#-----------------------------------------------------------------------------

# 

















#---------------------------------------------------------------------------
# We now need a function to compute the avg adc values per signal and 
# append them to a list or vector 
# In this way we could get the adc value per timebin and plot a heatmap 

# One approch is to essentially apply our function above to each row of 
# our data and get the stats for each row/signal appended to a dataframe ! 

imgph <- function(df) {
  
  # Firsr create an empty df 
  
  idf <- data.frame()

  # get total signals present in df
  #num_sigs = nrow(df)
  
  for(j in 1:nrow(df)) {
    
    m_adc <- c()
    
    for(i in 1:30) {
      m_adc[i] <- sum(c(df[j, i], df[j, i+30], df[j, i+60]), na.rm = T)/1
    }
    
    df_vals <- cbind.data.frame(TimeBin=c(0:29),mean_adc_vals = m_adc)
    idf <- rbind.data.frame(idf, df_vals)
  }
  
  #idf <- idf %>% mutate(madcv = floor(mean_adc_vals)) %>% 
  #  group_by(TimeBin, madcv) %>% 
  #  count()
  
  idf2 <- idf %>% mutate(madc = round_any(mean_adc_vals, 5)) %>%
    group_by(TimeBin, madc) %>% 
    count()

  upperlim <- 1.55*max(avgpulse2(df)[[2]][,2])


  
  php <- ggplot() +
    geom_tile(data = idf2, aes(x=TimeBin, y=madc, fill=n), na.rm = T) +ylim(c(0, upperlim)) +
    labs(fill="counts", x="Time Bin (100ns)", y="ADC", title = "Pulse Height Spectrum")+
    scale_fill_viridis_c() + 
    geom_point(data = avgpulse2(df)[[2]], aes(x=Time_bin, y=m_adc) ,col="red", size=1.2) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("phimg.png", plot = php, width = 11, height = 6)
  return(list(idf, php))
  
}


#------------------------------------------------------------------------------
# Image pulse height per sm 


imgphsm <- function(df) {
  
  sm <- df$sm[1]
  # Firsr create an empty df 
  
  idf <- data.frame()
  
  # get the total signals present in the df 
  #num_sigs = nrow(df)


  for(j in 1:nrow(df)) {
    
    m_adc <- c()
    
    for(i in 1:30) {
      m_adc[i] <- sum(c(df[j, i], df[j, i+30], df[j, i+60]), na.rm = T)/1
    }
    
    df_vals <- cbind.data.frame(TimeBin=c(0:29),mean_adc_vals = m_adc)
    idf <- rbind.data.frame(idf, df_vals)
  }
  
  #idf <- idf %>% mutate(madcv = floor(mean_adc_vals)) %>% 
  #  group_by(TimeBin, madcv) %>% 
  #  count()
  
  idf2 <- idf %>% mutate(madc = round_any(mean_adc_vals, 5)) %>%
    group_by(TimeBin, madc) %>%
    count()

  upperlim <- 1.55*max(avgpulse2(df)[[2]][,2])


  
  php <- ggplot() +
    geom_tile(data = idf2, aes(x=TimeBin, y=madc, fill=n), na.rm = T) +ylim(c(0, upperlim)) +
    labs(fill="counts", x="Time Bin (100ns)", y="ADC", title = paste0("SM ", sm))+
    scale_fill_viridis_c() + 
    geom_point(data = avgpulse2(df)[[2]], aes(x=Time_bin, y=m_adc) ,col="red", size=0.8) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  #ggsave("phimgsm.png", plot = php, width = 14, height = 7)
  return(list(idf, php))
  
}




#========================================================
# Implementation of heatmap pulse heights per SM 
#========================================================


get_imgphsm <- function(df) {

  s0 <- imgphsm(df %>% filter(sm==0))[[2]]
  s1 <- imgphsm(df %>% filter(sm==1))[[2]]
  s2 <- imgphsm(df %>% filter(sm==2))[[2]]
  s3 <- imgphsm(df %>% filter(sm==3))[[2]]
  s4 <- imgphsm(df %>% filter(sm==4))[[2]]
  s5 <- imgphsm(df %>% filter(sm==5))[[2]]
  s6 <- imgphsm(df %>% filter(sm==6))[[2]]
  s7 <- imgphsm(df %>% filter(sm==7))[[2]]
  s8 <- imgphsm(df %>% filter(sm==8))[[2]]
  s9 <- imgphsm(df %>% filter(sm==9))[[2]]
  s10 <- imgphsm(df %>% filter(sm==10))[[2]]
  s11 <- imgphsm(df %>% filter(sm==11))[[2]]
  s12 <- imgphsm(df %>% filter(sm==12))[[2]]
  s13 <- imgphsm(df %>% filter(sm==13))[[2]]
  s14 <- imgphsm(df %>% filter(sm==14))[[2]]
  s15 <- imgphsm(df %>% filter(sm==15))[[2]]
  s16 <- imgphsm(df %>% filter(sm==16))[[2]]
  s17 <- imgphsm(df %>% filter(sm==17))[[2]]


# The code below should produce the supermodule plots in an Rplots.pdf file when run 
# on the server but we can ggsave them for surety !
  Supermodplot <- (s0 + s1 + s2)/(s3 + s4 + s5)/(s6 + s7 + s8)
#Supermodplot
  ggsave("imgph_sm0_8.png", plot = Supermodplot, width = 10, height = 6)


  Supermodplot2 <- (s9 + s10 + s11)/(s12 + s13 + s14)/(s15 + s16 + s17)
#Supermodplot2
  ggsave("imgph_sm9_17.png", plot = Supermodplot2, width = 10, height = 6)

#ggsave("SMs0_to_8.png", plot= Supermodplot, width = 9, height = 7)
#ggsave("SMs9_to_17.png", plot= Supermodplot2, width = 9, height = 7)


}







#-----------------------------------------------------------------------------

#php <- ggplot() +
#  geom_tile(data = qqqstat, aes(x=TimeBin, y=madcv, fill=n), na.rm = T) +ylim(c(0, 80)) +
#  labs(fill="counts", x="Time Bin (100ns)", y="ADC", title = "Pulse Height Spectrum")+
#  scale_fill_viridis_c() + 
#  geom_point(data = cbind.data.frame(t=c(1:30), m=q[[2]]), aes(x=t, y=m) ,col="red", size=1.2) +
#  theme_bw()
  
#php

#ggsave("ph.png", plot = php, width = 9, height = 6)



#ggplot() + 
#  stat_bin_2d(data=qqq, aes(x=TimeBin, y=mean_adc_vals), bins = 29, na.rm = T, drop = F) + 
#  ylim(c(0, 80))+
#  scale_fill_viridis_c() + 
#  geom_point(data = cbind.data.frame(t=c(1:30), m=q[[2]]), aes(x=t, y=m) ,col="red", size=2) +
#  labs(x='Time bin', y="ADC") + 
#  theme(legend.position="bottom", legend.text = element_text(angle=90))
