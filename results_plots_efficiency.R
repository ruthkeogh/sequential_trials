###########################################################################################
###########################################################################################
# - Creates Figure 5: estimates of bias in risk differences under the treatment strategies "always treated" and "never treated" using the two methods.  
# - Creates Supplementary Figure 1 and Supplementary Figure 3, which show the estimates of bias obtained using different weights. 
# - The plots are saved in the folder "results" as figure5.pdf, suppfigure1.pdf, suppfigure3.pdf.
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#Calculate empirical variance of estimates of risk differences.
#------------------------

var_survdiff_msm_sw_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_sw_scen1[,x]-survdiff_true[x])})
var_survdiff_msm_sw_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_sw_scen2[,x]-survdiff_true[x])})
var_survdiff_msm_sw_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_sw_scen3[,x]-survdiff_true[x])})

var_survdiff_msm_swL_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swL_scen1[,x]-survdiff_true[x])})
var_survdiff_msm_swL_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swL_scen2[,x]-survdiff_true[x])})
var_survdiff_msm_swL_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swL_scen3[,x]-survdiff_true[x])})

var_survdiff_msm_swtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swtrunc_scen1[,x]-survdiff_true[x])})
var_survdiff_msm_swtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swtrunc_scen2[,x]-survdiff_true[x])})
var_survdiff_msm_swtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swtrunc_scen3[,x]-survdiff_true[x])})

var_survdiff_msm_swLtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swLtrunc_scen1[,x]-survdiff_true[x])})
var_survdiff_msm_swLtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swLtrunc_scen2[,x]-survdiff_true[x])})
var_survdiff_msm_swLtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_msm_swLtrunc_scen3[,x]-survdiff_true[x])})

var_survdiff_seq_sw_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_sw_scen1[,x]-survdiff_true[x])})
var_survdiff_seq_sw_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_sw_scen2[,x]-survdiff_true[x])})
var_survdiff_seq_sw_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_sw_scen3[,x]-survdiff_true[x])})

var_survdiff_seq_swtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_swtrunc_scen1[,x]-survdiff_true[x])})
var_survdiff_seq_swtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_swtrunc_scen2[,x]-survdiff_true[x])})
var_survdiff_seq_swtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){var(survdiff_seq_swtrunc_scen3[,x]-survdiff_true[x])})

#------------------------
#Calculate relative efficiency of the sequential trials approach relative to the MSM-IPTW approach, for estimating the risk differences.
#Thee are stored in data frames used in ggplot below.
#------------------------

dat.eff.msm_sw.seq_sw=data.frame(t.hor,
                         eff_scen1=var_survdiff_msm_sw_scen1/var_survdiff_seq_sw_scen1,
                         eff_scen2=var_survdiff_msm_sw_scen2/var_survdiff_seq_sw_scen2,
                         eff_scen3=var_survdiff_msm_sw_scen3/var_survdiff_seq_sw_scen3)

dat.eff.msm_swL.seq_sw=data.frame(t.hor,
                                eff_scen1=var_survdiff_msm_swL_scen1/var_survdiff_seq_sw_scen1,
                                eff_scen2=var_survdiff_msm_swL_scen2/var_survdiff_seq_sw_scen2,
                                eff_scen3=var_survdiff_msm_swL_scen3/var_survdiff_seq_sw_scen3)

dat.eff.msm_swtrunc.seq_swtrunc=data.frame(t.hor,
                                eff_scen1=var_survdiff_msm_swtrunc_scen1/var_survdiff_seq_swtrunc_scen1,
                                eff_scen2=var_survdiff_msm_swtrunc_scen2/var_survdiff_seq_swtrunc_scen2,
                                eff_scen3=var_survdiff_msm_swtrunc_scen3/var_survdiff_seq_swtrunc_scen3)

dat.eff.msm_swLtrunc.seq_swtrunc=data.frame(t.hor,
                                          eff_scen1=var_survdiff_msm_swLtrunc_scen1/var_survdiff_seq_swtrunc_scen1,
                                          eff_scen2=var_survdiff_msm_swLtrunc_scen2/var_survdiff_seq_swtrunc_scen2,
                                          eff_scen3=var_survdiff_msm_swLtrunc_scen3/var_survdiff_seq_swtrunc_scen3)


#------------------------
#some functions used to create the plots
#------------------------

#function used to add a line to a plot

addlinetoplot.lty <- function(dataset, varx, vary,vcol) { 
  list(
    geom_line(data=dataset, aes_string(x=varx, y=vary,colour=vcol),size=1.5) 
  )
}
cols=c("black"="#000000","grey"="#999999", "orange"="#E69F00", "lblue"="#56B4E9", 
       "green"="#009E73", "dblue"="#0072B2", "red"="#D55E00", "pink"="#CC79A7")

#------------------------
#Create and save Figure 6
#------------------------

figure6=ggplot(dat.eff.msm_swL.seq_sw,aes(x=t.hor,y=eff_scen1))+theme_bw()+
  ylab("Relative efficiency")+xlab("Time")+
  addlinetoplot.lty(dat.eff.msm_swL.seq_sw,"t.hor","eff_scen1",vcol='"black"')+
  addlinetoplot.lty(dat.eff.msm_swL.seq_sw,"t.hor","eff_scen2",vcol='"grey"')+
  addlinetoplot.lty(dat.eff.msm_swL.seq_sw,"t.hor","eff_scen3",vcol='"orange"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,4,0.5),limits=c(0,4))+
  scale_colour_manual(name  =NULL,breaks=c("black","grey","orange"),values=cols,
                      labels=c("Scenario 1","Scenario 2","Scenario 3"))+
  geom_hline(yintercept=1,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  theme(legend.position=c(0.9,0.9),legend.text=element_text(size = 12))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(1,"line"))+
  theme(legend.key = element_rect(fill = NA))
ggsave("results/figure6.pdf", figure6)

#------------------------
#Create and save Supplementary Figure 2
#------------------------

suppfigure2=ggplot(dat.eff.msm_sw.seq_sw,aes(x=t.hor,y=eff_scen1))+theme_bw()+
  ylab("Relative efficiency")+xlab("Time")+
  addlinetoplot.lty(dat.eff.msm_sw.seq_sw,"t.hor","eff_scen1",vcol='"black"')+
  addlinetoplot.lty(dat.eff.msm_sw.seq_sw,"t.hor","eff_scen2",vcol='"grey"')+
  addlinetoplot.lty(dat.eff.msm_sw.seq_sw,"t.hor","eff_scen3",vcol='"orange"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,10,0.5),limits=c(0,10))+
  scale_colour_manual(name  =NULL,breaks=c("black","grey","orange"),values=cols,
                      labels=c("Scenario 1","Scenario 2","Scenario 3"))+
  geom_hline(yintercept=1,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  theme(legend.position=c(0.9,0.9),legend.text=element_text(size = 12))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(1,"line"))+
  theme(legend.key = element_rect(fill = NA))
ggsave("results/suppfigure2.pdf", suppfigure2)

#------------------------
#Create and save Supplementary Figure 4
#------------------------

suppfigure4=ggplot(dat.eff.msm_swLtrunc.seq_swtrunc,aes(x=t.hor,y=eff_scen1))+theme_bw()+
  ylab("Relative efficiency")+xlab("Time")+
  addlinetoplot.lty(dat.eff.msm_swLtrunc.seq_swtrunc,"t.hor","eff_scen1",vcol='"black"')+
  addlinetoplot.lty(dat.eff.msm_swLtrunc.seq_swtrunc,"t.hor","eff_scen2",vcol='"grey"')+
  addlinetoplot.lty(dat.eff.msm_swLtrunc.seq_swtrunc,"t.hor","eff_scen3",vcol='"orange"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,4,0.5),limits=c(0,4))+
  scale_colour_manual(name  =NULL,breaks=c("black","grey","orange"),values=cols,
                      labels=c("Scenario 1","Scenario 2","Scenario 3"))+
  geom_hline(yintercept=1,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  theme(legend.position=c(0.9,0.9),legend.text=element_text(size = 12))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(1,"line"))+
  theme(legend.key = element_rect(fill = NA))
ggsave("results/suppfigure4.pdf", suppfigure4)

#The following plot is not incuded in the paper
#efficiency: sequential trials (swtrunc) versus MSM-IPW (swtrunc)

# ggplot(dat.eff.msm_swtrunc.seq_swtrunc,aes(x=t.hor,y=eff_scen1))+theme_bw()+
#   ylab("Relative efficiency")+xlab("Time")+
#   addlinetoplot.lty(dat.eff.msm_swtrunc.seq_swtrunc,"t.hor","eff_scen1",vcol='"black"')+
#   addlinetoplot.lty(dat.eff.msm_swtrunc.seq_swtrunc,"t.hor","eff_scen2",vcol='"grey"')+
#   addlinetoplot.lty(dat.eff.msm_swtrunc.seq_swtrunc,"t.hor","eff_scen3",vcol='"orange"')+
#   scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
#   scale_y_continuous(breaks=seq(0,4,0.5),limits=c(0,4))+
#   scale_colour_manual(name  =NULL,breaks=c("black","grey","orange"),values=cols,
#                       labels=c("Scenario 1","Scenario 2","Scenario 3"))+
#   geom_hline(yintercept=1,linetype="dashed", color="red",size=1.5)+
#   theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
#   theme(legend.position=c(0.9,0.9),legend.text=element_text(size = 12))+
#   theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(1,"line"))+
#   theme(legend.key = element_rect(fill = NA))





