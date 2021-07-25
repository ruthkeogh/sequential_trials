###########################################################################################
###########################################################################################
# - Takes the results generated in analysis_msm.R and analysis_seq.R (for scenarios 1,2,3) and renames things with "_scen1", "_scen2", "_scen3".
# - I could have included this within the files analysis_msm.R and analysis_seq.R.
# - This file enables someone to take the results stored in the "results_detailed_msm" and "results_detailed_seq" folders and create tables and figures, without having to actually run the simulations themselves.
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#-----------------------
#true values

load(file="results_detailed_msm/surv1_true.RData")
load(file="results_detailed_msm/surv0_true.RData")
load(file="results_detailed_msm/survdiff_true.RData")

#-----------------------
#MSM-IPTW - scenario 1

load(file="results_detailed_msm/surv1_msm_sw_scen1.RData")
load(file="results_detailed_msm/surv0_msm_sw_scen1.RData")
load(file="results_detailed_msm/survdiff_msm_sw_scen1.RData")

load(file="results_detailed_msm/surv1_msm_swL_scen1.RData")
load(file="results_detailed_msm/surv0_msm_swL_scen1.RData")
load(file="results_detailed_msm/survdiff_msm_swL_scen1.RData")

load(file="results_detailed_msm/surv1_msm_swtrunc_scen1.RData")
load(file="results_detailed_msm/surv0_msm_swtrunc_scen1.RData")
load(file="results_detailed_msm/survdiff_msm_swtrunc_scen1.RData")

load(file="results_detailed_msm/surv1_msm_swLtrunc_scen1.RData")
load(file="results_detailed_msm/surv0_msm_swLtrunc_scen1.RData")
load(file="results_detailed_msm/survdiff_msm_swLtrunc_scen1.RData")

load(file="results_detailed_msm/weights_max_sw_scen1.RData")
load(file="results_detailed_msm/weights_max_swL_scen1.RData")
load(file="results_detailed_msm/weights_max_swtrunc_scen1.RData")
load(file="results_detailed_msm/weights_max_swLtrunc_scen1.RData")

load(file="results_detailed_msm/num_by_visit_msm_scen1.RData")
load(file="results_detailed_msm/prop_by_visit_msm_scen1.RData")

load(file="results_detailed_msm/num_alwaystreat_msm_scen1.RData")
load(file="results_detailed_msm/prop_alwaystreat_msm_scen1.RData")

load(file="results_detailed_msm/num_nevertreat_msm_scen1.RData")
load(file="results_detailed_msm/prop_nevertreat_msm_scen1.RData")

surv1_msm_sw_scen1=surv1_msm_sw
surv0_msm_sw_scen1=surv0_msm_sw
survdiff_msm_sw_scen1=survdiff_msm_sw

surv1_msm_swL_scen1=surv1_msm_swL
surv0_msm_swL_scen1=surv0_msm_swL
survdiff_msm_swL_scen1=survdiff_msm_swL

surv1_msm_swtrunc_scen1=surv1_msm_swtrunc
surv0_msm_swtrunc_scen1=surv0_msm_swtrunc
survdiff_msm_swtrunc_scen1=survdiff_msm_swtrunc

surv1_msm_swLtrunc_scen1=surv1_msm_swLtrunc
surv0_msm_swLtrunc_scen1=surv0_msm_swLtrunc
survdiff_msm_swLtrunc_scen1=survdiff_msm_swLtrunc

weights_msm_max_sw_scen1=weights_max_sw
weights_msm_max_swtrunc_scen1=weights_max_swtrunc
weights_msm_max_swL_scen1=weights_max_swL
weights_msm_max_swLtrunc_scen1=weights_max_swLtrunc

num_by_visit_msm_scen1=num_by_visit_msm
prop_by_visit_msm_scen1=prop_by_visit_msm

num_alwaystreat_msm_scen1=num_alwaystreat_msm
prop_alwaystreat_msm_scen1=prop_alwaystreat_msm

num_nevertreat_msm_scen1=num_nevertreat_msm
prop_nevertreat_msm_scen1=prop_nevertreat_msm

#-----------------------
#MSM-IPTW - scenario 2

load(file="results_detailed_msm/surv1_msm_sw_scen2.RData")
load(file="results_detailed_msm/surv0_msm_sw_scen2.RData")
load(file="results_detailed_msm/survdiff_msm_sw_scen2.RData")

load(file="results_detailed_msm/surv1_msm_swL_scen2.RData")
load(file="results_detailed_msm/surv0_msm_swL_scen2.RData")
load(file="results_detailed_msm/survdiff_msm_swL_scen2.RData")

load(file="results_detailed_msm/surv1_msm_swtrunc_scen2.RData")
load(file="results_detailed_msm/surv0_msm_swtrunc_scen2.RData")
load(file="results_detailed_msm/survdiff_msm_swtrunc_scen2.RData")

load(file="results_detailed_msm/surv1_msm_swLtrunc_scen2.RData")
load(file="results_detailed_msm/surv0_msm_swLtrunc_scen2.RData")
load(file="results_detailed_msm/survdiff_msm_swLtrunc_scen2.RData")

load(file="results_detailed_msm/weights_max_sw_scen2.RData")
load(file="results_detailed_msm/weights_max_swL_scen2.RData")
load(file="results_detailed_msm/weights_max_swtrunc_scen2.RData")
load(file="results_detailed_msm/weights_max_swLtrunc_scen2.RData")

load(file="results_detailed_msm/num_by_visit_msm_scen2.RData")
load(file="results_detailed_msm/prop_by_visit_msm_scen2.RData")

load(file="results_detailed_msm/num_alwaystreat_msm_scen2.RData")
load(file="results_detailed_msm/prop_alwaystreat_msm_scen2.RData")

load(file="results_detailed_msm/num_nevertreat_msm_scen2.RData")
load(file="results_detailed_msm/prop_nevertreat_msm_scen2.RData")

surv1_msm_sw_scen2=surv1_msm_sw
surv0_msm_sw_scen2=surv0_msm_sw
survdiff_msm_sw_scen2=survdiff_msm_sw

surv1_msm_swL_scen2=surv1_msm_swL
surv0_msm_swL_scen2=surv0_msm_swL
survdiff_msm_swL_scen2=survdiff_msm_swL

surv1_msm_swtrunc_scen2=surv1_msm_swtrunc
surv0_msm_swtrunc_scen2=surv0_msm_swtrunc
survdiff_msm_swtrunc_scen2=survdiff_msm_swtrunc

surv1_msm_swLtrunc_scen2=surv1_msm_swLtrunc
surv0_msm_swLtrunc_scen2=surv0_msm_swLtrunc
survdiff_msm_swLtrunc_scen2=survdiff_msm_swLtrunc

weights_msm_max_sw_scen2=weights_max_sw
weights_msm_max_swtrunc_scen2=weights_max_swtrunc
weights_msm_max_swL_scen2=weights_max_swL
weights_msm_max_swLtrunc_scen2=weights_max_swLtrunc

num_by_visit_msm_scen2=num_by_visit_msm
prop_by_visit_msm_scen2=prop_by_visit_msm

num_alwaystreat_msm_scen2=num_alwaystreat_msm
prop_alwaystreat_msm_scen2=prop_alwaystreat_msm

num_nevertreat_msm_scen2=num_nevertreat_msm
prop_nevertreat_msm_scen2=prop_nevertreat_msm

#-----------------------
#MSM-IPTW - scenario 3

load(file="results_detailed_msm/surv1_msm_sw_scen3.RData")
load(file="results_detailed_msm/surv0_msm_sw_scen3.RData")
load(file="results_detailed_msm/survdiff_msm_sw_scen3.RData")

load(file="results_detailed_msm/surv1_msm_swL_scen3.RData")
load(file="results_detailed_msm/surv0_msm_swL_scen3.RData")
load(file="results_detailed_msm/survdiff_msm_swL_scen3.RData")

load(file="results_detailed_msm/surv1_msm_swtrunc_scen3.RData")
load(file="results_detailed_msm/surv0_msm_swtrunc_scen3.RData")
load(file="results_detailed_msm/survdiff_msm_swtrunc_scen3.RData")

load(file="results_detailed_msm/surv1_msm_swLtrunc_scen3.RData")
load(file="results_detailed_msm/surv0_msm_swLtrunc_scen3.RData")
load(file="results_detailed_msm/survdiff_msm_swLtrunc_scen3.RData")

load(file="results_detailed_msm/weights_max_sw_scen3.RData")
load(file="results_detailed_msm/weights_max_swL_scen3.RData")
load(file="results_detailed_msm/weights_max_swtrunc_scen3.RData")
load(file="results_detailed_msm/weights_max_swLtrunc_scen3.RData")

load(file="results_detailed_msm/num_by_visit_msm_scen3.RData")
load(file="results_detailed_msm/prop_by_visit_msm_scen3.RData")

load(file="results_detailed_msm/num_alwaystreat_msm_scen3.RData")
load(file="results_detailed_msm/prop_alwaystreat_msm_scen3.RData")

load(file="results_detailed_msm/num_nevertreat_msm_scen3.RData")
load(file="results_detailed_msm/prop_nevertreat_msm_scen3.RData")

surv1_msm_sw_scen3=surv1_msm_sw
surv0_msm_sw_scen3=surv0_msm_sw
survdiff_msm_sw_scen3=survdiff_msm_sw

surv1_msm_swL_scen3=surv1_msm_swL
surv0_msm_swL_scen3=surv0_msm_swL
survdiff_msm_swL_scen3=survdiff_msm_swL

surv1_msm_swtrunc_scen3=surv1_msm_swtrunc
surv0_msm_swtrunc_scen3=surv0_msm_swtrunc
survdiff_msm_swtrunc_scen3=survdiff_msm_swtrunc

surv1_msm_swLtrunc_scen3=surv1_msm_swLtrunc
surv0_msm_swLtrunc_scen3=surv0_msm_swLtrunc
survdiff_msm_swLtrunc_scen3=survdiff_msm_swLtrunc

weights_msm_max_sw_scen3=weights_max_sw
weights_msm_max_swtrunc_scen3=weights_max_swtrunc
weights_msm_max_swL_scen3=weights_max_swL
weights_msm_max_swLtrunc_scen3=weights_max_swLtrunc

num_by_visit_msm_scen3=num_by_visit_msm
prop_by_visit_msm_scen3=prop_by_visit_msm

num_alwaystreat_msm_scen3=num_alwaystreat_msm
prop_alwaystreat_msm_scen3=prop_alwaystreat_msm

num_nevertreat_msm_scen3=num_nevertreat_msm
prop_nevertreat_msm_scen3=prop_nevertreat_msm

#-----------------------
#Sequential trials - scenario 1

load(file="results_detailed_seq/surv1_seq_sw_scen1.RData")
load(file="results_detailed_seq/surv0_seq_sw_scen1.RData")
load(file="results_detailed_seq/survdiff_seq_sw_scen1.RData")

load(file="results_detailed_seq/surv1_seq_swtrunc_scen1.RData")
load(file="results_detailed_seq/surv0_seq_swtrunc_scen1.RData")
load(file="results_detailed_seq/survdiff_seq_swtrunc_scen1.RData")

load(file="results_detailed_seq/weights_max_seq_sw_scen1.RData")
load(file="results_detailed_seq/weights_max_seq_swtrunc_scen1.RData")

load(file="results_detailed_seq/num_by_visit_seq_scen1.RData")
load(file="results_detailed_seq/prop_by_visit_seq_scen1.RData")

load(file="results_detailed_seq/num_alwaystreat_seq_scen1.RData")
load(file="results_detailed_seq/prop_alwaystreat_seq_scen1.RData")

load(file="results_detailed_seq/num_nevertreat_seq_scen1.RData")
load(file="results_detailed_seq/prop_nevertreat_seq_scen1.RData")

surv1_seq_sw_scen1=surv1_seq_sw
surv0_seq_sw_scen1=surv0_seq_sw
survdiff_seq_sw_scen1=survdiff_seq_sw

surv1_seq_swtrunc_scen1=surv1_seq_swtrunc
surv0_seq_swtrunc_scen1=surv0_seq_swtrunc
survdiff_seq_swtrunc_scen1=survdiff_seq_swtrunc

weights_seq_max_sw_scen1=weights_max_sw
weights_seq_max_swtrunc_scen1=weights_max_swtrunc

num_by_visit_seq_scen1=num_by_visit_seq
prop_by_visit_seq_scen1=prop_by_visit_seq

num_alwaystreat_seq_scen1=num_alwaystreat_seq
prop_alwaystreat_seq_scen1=prop_alwaystreat_seq

num_nevertreat_seq_scen1=num_nevertreat_seq
prop_nevertreat_seq_scen1=prop_nevertreat_seq

#-----------------------
#Sequential trials - scenario 2

load(file="results_detailed_seq/surv1_seq_sw_scen2.RData")
load(file="results_detailed_seq/surv0_seq_sw_scen2.RData")
load(file="results_detailed_seq/survdiff_seq_sw_scen2.RData")

load(file="results_detailed_seq/surv1_seq_swtrunc_scen2.RData")
load(file="results_detailed_seq/surv0_seq_swtrunc_scen2.RData")
load(file="results_detailed_seq/survdiff_seq_swtrunc_scen2.RData")

load(file="results_detailed_seq/weights_max_seq_sw_scen2.RData")
load(file="results_detailed_seq/weights_max_seq_swtrunc_scen2.RData")

load(file="results_detailed_seq/num_by_visit_seq_scen2.RData")
load(file="results_detailed_seq/prop_by_visit_seq_scen2.RData")

load(file="results_detailed_seq/num_alwaystreat_seq_scen2.RData")
load(file="results_detailed_seq/prop_alwaystreat_seq_scen2.RData")

load(file="results_detailed_seq/num_nevertreat_seq_scen2.RData")
load(file="results_detailed_seq/prop_nevertreat_seq_scen2.RData")

surv1_seq_sw_scen2=surv1_seq_sw
surv0_seq_sw_scen2=surv0_seq_sw
survdiff_seq_sw_scen2=survdiff_seq_sw

surv1_seq_swtrunc_scen2=surv1_seq_swtrunc
surv0_seq_swtrunc_scen2=surv0_seq_swtrunc
survdiff_seq_swtrunc_scen2=survdiff_seq_swtrunc

weights_seq_max_sw_scen2=weights_max_sw
weights_seq_max_swtrunc_scen2=weights_max_swtrunc

num_by_visit_seq_scen2=num_by_visit_seq
prop_by_visit_seq_scen2=prop_by_visit_seq

num_alwaystreat_seq_scen2=num_alwaystreat_seq
prop_alwaystreat_seq_scen2=prop_alwaystreat_seq

num_nevertreat_seq_scen2=num_nevertreat_seq
prop_nevertreat_seq_scen2=prop_nevertreat_seq

#-----------------------
#Sequential trials - scenario 3

load(file="results_detailed_seq/surv1_seq_sw_scen3.RData")
load(file="results_detailed_seq/surv0_seq_sw_scen3.RData")
load(file="results_detailed_seq/survdiff_seq_sw_scen3.RData")

load(file="results_detailed_seq/surv1_seq_swtrunc_scen3.RData")
load(file="results_detailed_seq/surv0_seq_swtrunc_scen3.RData")
load(file="results_detailed_seq/survdiff_seq_swtrunc_scen3.RData")

load(file="results_detailed_seq/weights_max_seq_sw_scen3.RData")
load(file="results_detailed_seq/weights_max_seq_swtrunc_scen3.RData")

load(file="results_detailed_seq/num_by_visit_seq_scen3.RData")
load(file="results_detailed_seq/prop_by_visit_seq_scen3.RData")

load(file="results_detailed_seq/num_alwaystreat_seq_scen3.RData")
load(file="results_detailed_seq/prop_alwaystreat_seq_scen3.RData")

load(file="results_detailed_seq/num_nevertreat_seq_scen3.RData")
load(file="results_detailed_seq/prop_nevertreat_seq_scen3.RData")

surv1_seq_sw_scen3=surv1_seq_sw
surv0_seq_sw_scen3=surv0_seq_sw
survdiff_seq_sw_scen3=survdiff_seq_sw

surv1_seq_swtrunc_scen3=surv1_seq_swtrunc
surv0_seq_swtrunc_scen3=surv0_seq_swtrunc
survdiff_seq_swtrunc_scen3=survdiff_seq_swtrunc

weights_seq_max_sw_scen3=weights_max_sw
weights_seq_max_swtrunc_scen3=weights_max_swtrunc

num_by_visit_seq_scen3=num_by_visit_seq
prop_by_visit_seq_scen3=prop_by_visit_seq

num_alwaystreat_seq_scen3=num_alwaystreat_seq
prop_alwaystreat_seq_scen3=prop_alwaystreat_seq

num_nevertreat_seq_scen3=num_nevertreat_seq
prop_nevertreat_seq_scen3=prop_nevertreat_seq
