###########################################################################################
###########################################################################################
# - Creates Figure 7: plots of the largest weight (by time period) in each of the 1000 simulated data sets under the sequential trials analysis (IPACW) compared with the MSM-IPTW analysis
# - The plot is saved in the folder "results" as figure7.pdf.
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#data sets for use in ggplot: maximum weight in each time period
#------------------------

dat.weights.max.visit0=data.frame(weights_msm_max_sw_scen1=weights_msm_max_sw_scen1[,1],weights_msm_max_swL_scen1=weights_msm_max_swL_scen1[,1],
                                   weights_msm_max_swtrunc_scen1=weights_msm_max_swtrunc_scen1[,1],weights_msm_max_swLtrunc_scen1=weights_msm_max_swLtrunc_scen1[,1],
                                   weights_seq_max_sw_scen1=weights_seq_max_sw_scen1[,1],weights_seq_max_swtrunc_scen1=weights_seq_max_swtrunc_scen1[,1],
                                   #
                                   weights_msm_max_sw_scen2=weights_msm_max_sw_scen2[,1],weights_msm_max_swL_scen2=weights_msm_max_swL_scen2[,1],
                                   weights_msm_max_swtrunc_scen2=weights_msm_max_swtrunc_scen2[,1],weights_msm_max_swLtrunc_scen2=weights_msm_max_swLtrunc_scen2[,1],
                                   weights_seq_max_sw_scen2=weights_seq_max_sw_scen2[,1],weights_seq_max_swtrunc_scen2=weights_seq_max_swtrunc_scen2[,1],
                                   #
                                   weights_msm_max_sw_scen3=weights_msm_max_sw_scen3[,1],weights_msm_max_swL_scen3=weights_msm_max_swL_scen3[,1],
                                   weights_msm_max_swtrunc_scen3=weights_msm_max_swtrunc_scen3[,1],weights_msm_max_swLtrunc_scen3=weights_msm_max_swLtrunc_scen3[,1],
                                   weights_seq_max_sw_scen3=weights_seq_max_sw_scen3[,1],weights_seq_max_swtrunc_scen3=weights_seq_max_swtrunc_scen3[,1])

dat.weights.max.visit1=data.frame(weights_msm_max_sw_scen1=weights_msm_max_sw_scen1[,2],weights_msm_max_swL_scen1=weights_msm_max_swL_scen1[,2],
                                   weights_msm_max_swtrunc_scen1=weights_msm_max_swtrunc_scen1[,2],weights_msm_max_swLtrunc_scen1=weights_msm_max_swLtrunc_scen1[,2],
                                   weights_seq_max_sw_scen1=weights_seq_max_sw_scen1[,2],weights_seq_max_swtrunc_scen1=weights_seq_max_swtrunc_scen1[,2],
                                   #
                                   weights_msm_max_sw_scen2=weights_msm_max_sw_scen2[,2],weights_msm_max_swL_scen2=weights_msm_max_swL_scen2[,2],
                                   weights_msm_max_swtrunc_scen2=weights_msm_max_swtrunc_scen2[,2],weights_msm_max_swLtrunc_scen2=weights_msm_max_swLtrunc_scen2[,2],
                                   weights_seq_max_sw_scen2=weights_seq_max_sw_scen2[,2],weights_seq_max_swtrunc_scen2=weights_seq_max_swtrunc_scen2[,2],
                                   #
                                   weights_msm_max_sw_scen3=weights_msm_max_sw_scen3[,2],weights_msm_max_swL_scen3=weights_msm_max_swL_scen3[,2],
                                   weights_msm_max_swtrunc_scen3=weights_msm_max_swtrunc_scen3[,2],weights_msm_max_swLtrunc_scen3=weights_msm_max_swLtrunc_scen3[,2],
                                   weights_seq_max_sw_scen3=weights_seq_max_sw_scen3[,2],weights_seq_max_swtrunc_scen3=weights_seq_max_swtrunc_scen3[,2])

dat.weights.max.visit2=data.frame(weights_msm_max_sw_scen1=weights_msm_max_sw_scen1[,3],weights_msm_max_swL_scen1=weights_msm_max_swL_scen1[,3],
                                   weights_msm_max_swtrunc_scen1=weights_msm_max_swtrunc_scen1[,3],weights_msm_max_swLtrunc_scen1=weights_msm_max_swLtrunc_scen1[,3],
                                   weights_seq_max_sw_scen1=weights_seq_max_sw_scen1[,3],weights_seq_max_swtrunc_scen1=weights_seq_max_swtrunc_scen1[,3],
                                   #
                                   weights_msm_max_sw_scen2=weights_msm_max_sw_scen2[,3],weights_msm_max_swL_scen2=weights_msm_max_swL_scen2[,3],
                                   weights_msm_max_swtrunc_scen2=weights_msm_max_swtrunc_scen2[,3],weights_msm_max_swLtrunc_scen2=weights_msm_max_swLtrunc_scen2[,3],
                                   weights_seq_max_sw_scen2=weights_seq_max_sw_scen2[,3],weights_seq_max_swtrunc_scen2=weights_seq_max_swtrunc_scen2[,3],
                                   #
                                   weights_msm_max_sw_scen3=weights_msm_max_sw_scen3[,3],weights_msm_max_swL_scen3=weights_msm_max_swL_scen3[,3],
                                   weights_msm_max_swtrunc_scen3=weights_msm_max_swtrunc_scen3[,3],weights_msm_max_swLtrunc_scen3=weights_msm_max_swLtrunc_scen3[,3],
                                   weights_seq_max_sw_scen3=weights_seq_max_sw_scen3[,3],weights_seq_max_swtrunc_scen3=weights_seq_max_swtrunc_scen3[,3])

dat.weights.max.visit3=data.frame(weights_msm_max_sw_scen1=weights_msm_max_sw_scen1[,4],weights_msm_max_swL_scen1=weights_msm_max_swL_scen1[,4],
                                   weights_msm_max_swtrunc_scen1=weights_msm_max_swtrunc_scen1[,4],weights_msm_max_swLtrunc_scen1=weights_msm_max_swLtrunc_scen1[,4],
                                   weights_seq_max_sw_scen1=weights_seq_max_sw_scen1[,4],weights_seq_max_swtrunc_scen1=weights_seq_max_swtrunc_scen1[,4],
                                   #
                                   weights_msm_max_sw_scen2=weights_msm_max_sw_scen2[,4],weights_msm_max_swL_scen2=weights_msm_max_swL_scen2[,4],
                                   weights_msm_max_swtrunc_scen2=weights_msm_max_swtrunc_scen2[,4],weights_msm_max_swLtrunc_scen2=weights_msm_max_swLtrunc_scen2[,4],
                                   weights_seq_max_sw_scen2=weights_seq_max_sw_scen2[,4],weights_seq_max_swtrunc_scen2=weights_seq_max_swtrunc_scen2[,4],
                                   #
                                   weights_msm_max_sw_scen3=weights_msm_max_sw_scen3[,4],weights_msm_max_swL_scen3=weights_msm_max_swL_scen3[,4],
                                   weights_msm_max_swtrunc_scen3=weights_msm_max_swtrunc_scen3[,4],weights_msm_max_swLtrunc_scen3=weights_msm_max_swLtrunc_scen3[,4],
                                   weights_seq_max_sw_scen3=weights_seq_max_sw_scen3[,4],weights_seq_max_swtrunc_scen3=weights_seq_max_swtrunc_scen3[,4])

dat.weights.max.visit4=data.frame(weights_msm_max_sw_scen1=weights_msm_max_sw_scen1[,5],weights_msm_max_swL_scen1=weights_msm_max_swL_scen1[,5],
                                   weights_msm_max_swtrunc_scen1=weights_msm_max_swtrunc_scen1[,5],weights_msm_max_swLtrunc_scen1=weights_msm_max_swLtrunc_scen1[,5],
                                   weights_seq_max_sw_scen1=weights_seq_max_sw_scen1[,5],weights_seq_max_swtrunc_scen1=weights_seq_max_swtrunc_scen1[,5],
                                   #
                                   weights_msm_max_sw_scen2=weights_msm_max_sw_scen2[,5],weights_msm_max_swL_scen2=weights_msm_max_swL_scen2[,5],
                                   weights_msm_max_swtrunc_scen2=weights_msm_max_swtrunc_scen2[,5],weights_msm_max_swLtrunc_scen2=weights_msm_max_swLtrunc_scen2[,5],
                                   weights_seq_max_sw_scen2=weights_seq_max_sw_scen2[,5],weights_seq_max_swtrunc_scen2=weights_seq_max_swtrunc_scen2[,5],
                                   #
                                   weights_msm_max_sw_scen3=weights_msm_max_sw_scen3[,5],weights_msm_max_swL_scen3=weights_msm_max_swL_scen3[,5],
                                   weights_msm_max_swtrunc_scen3=weights_msm_max_swtrunc_scen3[,5],weights_msm_max_swLtrunc_scen3=weights_msm_max_swLtrunc_scen3[,5],
                                   weights_seq_max_sw_scen3=weights_seq_max_sw_scen3[,5],weights_seq_max_swtrunc_scen3=weights_seq_max_swtrunc_scen3[,5])


#-------------------------------------
#Plots for scenario 1
#-------------------------------------

#time [0,1)

# range(dat.weights.max.visit0$weights_msm_max_swL_scen1)
# range(dat.weights.max.visit0$weights_seq_max_sw_scen1)

plotweights_msm_swL_seq_sw_scen1_visit0=ggplot(dat.weights.max.visit0, aes(x=weights_msm_max_swL_scen1,y=weights_seq_max_sw_scen1)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,1.8,0.2),limits=c(0,1.8))+
  scale_y_continuous(breaks=seq(0,1.8,0.2),limits=c(0,1.8))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 1: times [0,1)")+geom_abline(intercept=0,slope=1)


#time [1,2)

# range(dat.weights.max.visit1$weights_msm_max_swL_scen1)
# range(dat.weights.max.visit1$weights_seq_max_sw_scen1)

plotweights_msm_swL_seq_sw_scen1_visit1=ggplot(dat.weights.max.visit1, aes(x=weights_msm_max_swL_scen1,y=weights_seq_max_sw_scen1)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,8,1),limits=c(0,8))+
  scale_y_continuous(breaks=seq(0,8,1),limits=c(0,8))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 1: times [1,2)")+geom_abline(intercept=0,slope=1)

#time [2,3)

# range(dat.weights.max.visit2$weights_msm_max_swL_scen1)
# range(dat.weights.max.visit2$weights_seq_max_sw_scen1)

plotweights_msm_swL_seq_sw_scen1_visit2=ggplot(dat.weights.max.visit2, aes(x=weights_msm_max_swL_scen1,y=weights_seq_max_sw_scen1)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,16,5),limits=c(0,16))+
  scale_y_continuous(breaks=seq(0,16,5),limits=c(0,16))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 1: times [2,3)")+geom_abline(intercept=0,slope=1)

#time [3,4)

# range(dat.weights.max.visit3$weights_msm_max_swL_scen1)
# range(dat.weights.max.visit3$weights_seq_max_sw_scen1)

plotweights_msm_swL_seq_sw_scen1_visit3=ggplot(dat.weights.max.visit3, aes(x=weights_msm_max_swL_scen1,y=weights_seq_max_sw_scen1)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,30,5),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,30,5),limits=c(0,30))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 1: times [3,4)")+geom_abline(intercept=0,slope=1)

#time [4,5)]

# range(dat.weights.max.visit4$weights_msm_max_swL_scen1)
# range(dat.weights.max.visit4$weights_seq_max_sw_scen1)

plotweights_msm_swL_seq_sw_scen1_visit4=ggplot(dat.weights.max.visit4, aes(x=weights_msm_max_swL_scen1,y=weights_seq_max_sw_scen1)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,90,10),limits=c(0,90))+
  scale_y_continuous(breaks=seq(0,90,10),limits=c(0,90))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 1: times [4,5)")+geom_abline(intercept=0,slope=1)

#-------------------------------------
#Plots for scenario 2
#-------------------------------------

#time [0,1)

# range(dat.weights.max.visit0$weights_msm_max_swL_scen2)
# range(dat.weights.max.visit0$weights_seq_max_sw_scen2)

plotweights_msm_swL_seq_sw_scen2_visit0=ggplot(dat.weights.max.visit0, aes(x=weights_msm_max_swL_scen2,y=weights_seq_max_sw_scen2)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,3.1,0.5),limits=c(0,3.1))+
  scale_y_continuous(breaks=seq(0,3.1,0.5),limits=c(0,3.1))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 2: times [0,1)")+geom_abline(intercept=0,slope=1)


#time [1,2)

# range(dat.weights.max.visit1$weights_msm_max_swL_scen2)
# range(dat.weights.max.visit1$weights_seq_max_sw_scen2)

plotweights_msm_swL_seq_sw_scen2_visit1=ggplot(dat.weights.max.visit1, aes(x=weights_msm_max_swL_scen2,y=weights_seq_max_sw_scen2)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,8,1),limits=c(0,8))+
  scale_y_continuous(breaks=seq(0,8,1),limits=c(0,8))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 2: times [1,2)")+geom_abline(intercept=0,slope=1)

#time [2,3)

# range(dat.weights.max.visit2$weights_msm_max_swL_scen2)
# range(dat.weights.max.visit2$weights_seq_max_sw_scen2)

plotweights_msm_swL_seq_sw_scen2_visit2=ggplot(dat.weights.max.visit2, aes(x=weights_msm_max_swL_scen2,y=weights_seq_max_sw_scen2)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,36,10),limits=c(0,36))+
  scale_y_continuous(breaks=seq(0,36,10),limits=c(0,36))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 2: times [2,3)")+geom_abline(intercept=0,slope=1)

#time [3,4)

# range(dat.weights.max.visit3$weights_msm_max_swL_scen2)
# range(dat.weights.max.visit3$weights_seq_max_sw_scen2)

plotweights_msm_swL_seq_sw_scen2_visit3=ggplot(dat.weights.max.visit3, aes(x=weights_msm_max_swL_scen2,y=weights_seq_max_sw_scen2)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,14,5),limits=c(0,14))+
  scale_y_continuous(breaks=seq(0,14,5),limits=c(0,14))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 2: times [3,4)")+geom_abline(intercept=0,slope=1)

#time [4,5)

# range(dat.weights.max.visit4$weights_msm_max_swL_scen2)
# range(dat.weights.max.visit4$weights_seq_max_sw_scen2)

plotweights_msm_swL_seq_sw_scen2_visit4=ggplot(dat.weights.max.visit4, aes(x=weights_msm_max_swL_scen2,y=weights_seq_max_sw_scen2)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,14,5),limits=c(0,14))+
  scale_y_continuous(breaks=seq(0,14,5),limits=c(0,14))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 2: times [4,5)")+geom_abline(intercept=0,slope=1)

#-------------------------------------
#Plots for scenario 3
#-------------------------------------

#time [0,1)

# range(dat.weights.max.visit0$weights_msm_max_swL_scen3)
# range(dat.weights.max.visit0$weights_seq_max_sw_scen3)

plotweights_msm_swL_seq_sw_scen3_visit0=ggplot(dat.weights.max.visit0, aes(x=weights_msm_max_swL_scen3,y=weights_seq_max_sw_scen3)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,2.2,0.5),limits=c(0,2.2))+
  scale_y_continuous(breaks=seq(0,2.2,0.5),limits=c(0,2.2))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 3: times [0,1)")+geom_abline(intercept=0,slope=1)


#time [1,2)

# range(dat.weights.max.visit1$weights_msm_max_swL_scen3)
# range(dat.weights.max.visit1$weights_seq_max_sw_scen3)

plotweights_msm_swL_seq_sw_scen3_visit1=ggplot(dat.weights.max.visit1, aes(x=weights_msm_max_swL_scen3,y=weights_seq_max_sw_scen3)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,1500,500),limits=c(0,1500))+
  scale_y_continuous(breaks=seq(0,1500,500),limits=c(0,1500))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 3: times [1,2)")+geom_abline(intercept=0,slope=1)

#time [2,3)

# range(dat.weights.max.visit2$weights_msm_max_swL_scen3)
# range(dat.weights.max.visit2$weights_seq_max_sw_scen3)

plotweights_msm_swL_seq_sw_scen3_visit2=ggplot(dat.weights.max.visit2, aes(x=weights_msm_max_swL_scen3,y=weights_seq_max_sw_scen3)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  scale_y_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 3: times [2,3)")+geom_abline(intercept=0,slope=1)

#time [3,4)

# range(dat.weights.max.visit3$weights_msm_max_swL_scen3)
# range(dat.weights.max.visit3$weights_seq_max_sw_scen3)

plotweights_msm_swL_seq_sw_scen3_visit3=ggplot(dat.weights.max.visit3, aes(x=weights_msm_max_swL_scen3,y=weights_seq_max_sw_scen3)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  scale_y_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 3: times [3,4)")+geom_abline(intercept=0,slope=1)

#time [4,5)

# range(dat.weights.max.visit4$weights_msm_max_swL_scen3)
# range(dat.weights.max.visit4$weights_seq_max_sw_scen3)

plotweights_msm_swL_seq_sw_scen3_visit4=ggplot(dat.weights.max.visit4, aes(x=weights_msm_max_swL_scen3,y=weights_seq_max_sw_scen3)) + theme_bw()+
  geom_point(alpha=0.2)+ geom_rug()+
  scale_x_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  scale_y_continuous(breaks=seq(0,10000,2500),limits=c(0,10000))+
  ylab("Sequential trials")+xlab("MSM-IPTW")+
  ggtitle("Scenario 3: times [4,5)")+geom_abline(intercept=0,slope=1)

#------------------------
#Create and save Figure 7
#------------------------

figure7=arrangeGrob(plotweights_msm_swL_seq_sw_scen1_visit0,
             plotweights_msm_swL_seq_sw_scen1_visit1,
             plotweights_msm_swL_seq_sw_scen1_visit2,
             plotweights_msm_swL_seq_sw_scen1_visit3,
             plotweights_msm_swL_seq_sw_scen1_visit4,
 #            
             plotweights_msm_swL_seq_sw_scen2_visit0,
             plotweights_msm_swL_seq_sw_scen2_visit1,
             plotweights_msm_swL_seq_sw_scen2_visit2,
             plotweights_msm_swL_seq_sw_scen2_visit3,
             plotweights_msm_swL_seq_sw_scen2_visit4,
  #           
             plotweights_msm_swL_seq_sw_scen3_visit0,
             plotweights_msm_swL_seq_sw_scen3_visit1,
             plotweights_msm_swL_seq_sw_scen3_visit2,
             plotweights_msm_swL_seq_sw_scen3_visit3,
             plotweights_msm_swL_seq_sw_scen3_visit4,ncol=5)

ggsave("results/figure7.pdf", figure7)

