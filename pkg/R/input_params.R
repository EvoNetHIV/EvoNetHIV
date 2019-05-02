


#' @export
input_params<-function(


  # Returns list of parameters and default values, unless modified by user


#-- Basic model setup parameters -------------#
#used in top-level run scripts
    model_name   = "evomodel",
    hpc          = FALSE, #on hyak?
    hyak_par     = FALSE, #on hyak and parallelized run?
    cp_int        = NULL, #checpoint save interval on nestim_hpc hyak runs
    output_path  =  getwd(),
    fast_edgelist = FALSE,
    nsims        = 1,
    initial_pop  = 100, #initial popn
    n_steps      = 365*2,
    initial_infected  = 20,
    model_sex    = "msm",
    popsumm_frequency=1, #frequency of timesteps should popsumm stats be calculated
    ncores =1, #16 if running on hyak using EpiModelHPC
  #runtime printing options  
    scrolling_output = TRUE,
    print_frequency = 10, # Set to 10 to print to output screen every 10 days. Default should be 1.
    network_print_frequency = 100,
  #runtime plotting options  
    plot_nw      = TRUE,
    plot_mean_degree_by_age = FALSE, # Set to true to see plots of mean degree broken out by age.
  #output options  
    save_network  = FALSE,
    save_coital_acts = FALSE,
    save_vl_list = FALSE,    #TRUE to graph individual agent VL
    save_infection_matrix=FALSE,
    save_partner_list = FALSE,    
    save_RData_file= F, # for John's scripts,
    save_summary_figs = F, # for John's scripts,
    vital=FALSE, #epimodel requires this parameter, should be False
    
    # new parameters for alternative restart routines
    start_timestep = 1, #parameter for EpiModel, should be "1" if new simulation if re-starting simulation, value should be "n_steps+1" (n_steps from original sim.)
    restart_val = "none", # "save" = save simulation, "restart" = restart simulation
    restart_time = 1e9, # time that base simulation results get saved
    restart_tx_type = "CD4_low",
    
  #--QA/QC terms ----------------------------#
    QA_QC_pause_time = 3,  # Time delay in seconds that the computer waits so that users can view QA/QC stats 
  
  #-- Network estimation terms -------------#
  #used in  nw_setup(...) ->  setup_initialize_network(...) for
  #ergm/tergm estimation/simulation
    modes         = 1, #epimodel param, will change to 2 in "input_params_derived" if hetero model
    nw_form_terms = "~edges + offset(nodematch('role', diff=TRUE, keep=1:2))",
    nw_coef_form  = c(-Inf, -Inf),
    target_stats  = 100*0.7/2,
    relation_dur  = 50,
    d_rate =        3e-05, # default value in epimodel's netest
    nw_constraints = " ~.",
    dissolution="~offset(edges)",
    rm_offset_rel = F, # temporary solution to remove same-role (MSM simulations) and same-sex (heterosexual simulations) relationships. If = T, function remove_offset_relationships will be called in initialize_module.

#-- viral load progression / spvl parameters -------------#
 #used in "initialize_infecteds_vl" (for initial population)
 # and "transmission_bookkeeping_module" for secondary infections
    VL_Function  = "aim2",  # Other option is "aim3" (aim 3 code)
    vl_peak_agent_flag    = FALSE,
    transmission_model    = "hughes",
    max_spvl_allowed      = 7.0,
    min_spvl_allowed      = 2.0,
    max_time_inf_initial_pop = 365*2,
    MutationVariance      = 0.01,
  #used in "viral_update_aim2", default VL progression 
    V0                    = 1e-4,
    vl_peak_acute         = 7.7e6,            # Average viral load during primary infection; from Little et al 1999
    vl_max_aids           = 2.4e6,            # Piatak 1993
    vl_increase_AIDS      = 1.0041122,         #determines slope of vl increase after aids onset; defaault value implies 400-fold increase over 4 years. E.g. O’Brien and Hendrickson 2013.
    t_peak                = 21.0,
    t_acute               = 90,
    t_acute_phase2        = 32,       # Changed on 3/29/16 to give more realistic acute phase dynamics with the revised acute phase code
    acute_decline_phase2_rate = -.03,
    vl_decay_rate_phase2 = 0.02,        #new on 3/2/16, revised acute decline code
    AverageLogSP0         = 4.5,
    VarianceLogSP0        = 0.8,
    vl_full_supp           = 13.0,       			# final viral load after complete treatment
    vl_undetectable        = 50.0,
    vl_exp_decline_tx     = -0.6,
    MaxPPP                = 0.0,      # Maximum per pathogen pathogenecity (PPP). Viruses w/ PPP's > 0.0 kill hosts faster for a given SPVL
    max_vl_viralcont_spvl = 7,
    min_vl_viralcont_spvl = 2.5,
    prog_rate             = 0.14,     # per year rate.  Note VL progression cartoons typically show ~0.5 Log increase in VL over a 8-year
                                      # period.  That suggests a progression rate of ln(10^0.5) / 8 = ~0.14 per year
# ---- Transmission parameters ----------------------------------
#used in "transmission", main transmission function
    InfRateBaseline       = 0.0000003,			  # 0.0001178/365		# Lingappa 2010
    InfRateExponent       = 3.52,					    # Lingappa 2010
    MaxInfRate            = 0.002,		    	  # Asymptotic function from Fraser 2007, Assuming P_inf(1 year) = 1 - (1 _ P_inf(1 day))^365
    VHalfMaxInfRate       = 13938,				    # Fraser 2007
    HillCoeffInfRate      = 1.02, 				    # Fraser 2007
    shape_parameter       = 3.46,					    # 3.46 is from Fraser
    Dmax                  = 9271, 					  # from Fraser, maximum time in days of asymptomatic state
    D50                   = 3058, 					  # from Fraser, spVL at which duration is half maximum
    Dk                    = 0.41, 					  # 0.41 is from Fraser, Hill coefficient
    Heritability          = 0.36,     # desired population heritability estimate is 0.36 (from Fraser; should vary by population). This value, in conjunction with the default mutation variance, seems to yield a stable population heritability of 3.6.  But user should always beware!
    Flat_Viral_Load       = 0,        # Setting this to 1 forces VL to be the same value during the entire primary infection period
    trans_lambda         = 0.000247,  # From Steve Goodreau's summary of discussions he had with Jim Hughes
    trans_lambda_init    = 0.003,
    trans_lambda_alpha   = 5,
    trans_RR_uses_condoms= 0.22,      # From Table 4 from Hughes et al. (2012, JID)
    trans_RR_LogV        = 2.89,      # From Table 4 from Hughes et al. (2012, JID)
    trans_VLbase         = 4.0,       # From Steve Goodreau's summary of discussions he had with Jim Hughes
    trans_RR_circumcised = 0.53,      # From Table 4 from Hughes et al. (2012, JID)
    trans_RR_age         = 0.67,      # Older people have lower risk (Note: 0.82 in the publication. 0.67 comes from Steve after talking to Jim Hughes) [Note that Table 3 from Hughes et al. 2012 gives 0.82]
    max_age_RR_age       = 45,    #threshold age, where ages above threshold have same RR
    trans_base_age       = 35,        # From Steve Goodreau's summary of discussions he had with Jim Hughes
    trans_RR_STI         = 2.7,       # placeholder for generic sti
    trans_RR_insertive_anal_msm = 2.9,  #From Patel 2014, but adjusted for circumcision. See Evonet params.xls and V-A transmission ratios.xlsx. Was 5 until 7/6/2016
    trans_RR_receptive_anal_msm = 17.3, #From Patel 2014, but adjusted for circumcision. See Evonet params.xls and V-A transmission ratios.xlsx. Was 10 until 7/6/2016
    trans_RR_ins_ai      = 2.9,
    trans_RR_rec_ai      = 17.3,
    trans_RR_acute_phase = 1.0,       #increased infectiousness during acute phase
    trans_RR_receptive_vaginal=1,
    trans_RR_vaccine     = 0.01,      #placeholder, 5/3/16
    perc_virus_vaccine_sens = 0.99,  #placeholder 5/3/16

# Parameters dynamic CD4 function (revised 11/11/15)
# Function?
  cd4_homo_input     = 0.04,  # Rate of addition of CD4 T-cells per day when CD4 < 1000
  k_cd4              = 0.127,  # Rate at which virus kills CD4 T-cells [using log10(virus)]
  vl_kill_cd4        = 0.38,   # VL below which virus no longer kills off CD4 T-cells
  min_prop_blood     = 0.5,   # Proportion of CD4 T-cells that stay in blood no matter how high VL gets
  V_half_redist      = 4e5,   # Viral load at which 50% of mobilizable CD4 T-cells migrate to lymph nodes
  CD4count_end_stage = 1,     # CD4 count at which patients die of AIDS


# -- Viral Load / Virulence parameters specific for Aim 3 modeling
#   Note with changes made 8/31/15: V_peak, V_SPVL, and V_AIDS are now defined by Aim 2 values given above.
    Save_VL_Histories  = FALSE,
    Max_Allowable_Loci = 5,
    prob_CYP_6_slow  = 0,
    step_size_C    = 0.0004,   # Step size in C program simulator
    s_CD4       = 10,        # Rate of input of CD4 T-cells from the thymus
    m_CD4       = 0.01,      # Natural death rate of CD4 T-cells
    k           = 0.0000001, # Rate at which viruses kill off CD4 T-cells
    r_inf_cells = 1.8,       # growth rate of infected cells at start of infection
    r_r_inf_cells  = 1.3,    # growth rate of drug resistant infected cells at start of infection
    d_inf_cells  = 0.6,      # death rate of infected cells
    f_M         = 0.02,      # Fraction of target cells that become moderately long-lived infected cells upon infection
    f_L         = 1e-6,      # Fraction of target cells that become very long-lived (latently) infected cells upon infection
    d_M         = 0.04,      # Death rate of moderately long-lived infected cells
    d_L         = 0.001,    # Death rate of latently infected cells
    p_inf_cells = 1000,      # Rate at which actively infected cells produce virus
    p_M         = 100,       # Rate at which moderately long-lived infected cells produce virus
    p_L         = 10,        # Rate at which latently infected cells produce virus
    M_act       = 0.01,      # Rate at which M cells convert into productively infected cells, I
    L_act       = 0.0005,    # Rate at which latently infected cells convert into productively infected cells
    c           = 50,        # Clearance rate of free virus (Initial rate prior to virus damaging the immune system)
    mu          = 3e-5,      # Rate at which viruses mutate from being sensitive to being resistant
    TransBottleneck = 3e8,  # Reduction in viral load associated with transmission
    no_loci     = 5,         # Number of loci conferring drug resistance
    cost1       = 0.05,      # Fitness cost of having a drug resistance mutation at locus 1
    cost2       = 0.05,      # Fitness cost of having a drug resistance mutation at locus 2
    cost3       = 0.05,      # Fitness cost of having a drug resistance mutation at locus 3
    cost4       = 0.05,      # Fitness cost of having a drug resistance mutation at locus 4
    cost5       = 0.05,      # Fitness cost of having a drug resistance mutation at locus 5 (note: this mut negates cost at locus 2)
    cost_reduct4on2 = 0.9,   # Extent to which mutation 4 mitigates the cost of mutation 2
    cost_reduct5on1 = 0.5,   # Extent to which mutation 5 mitigates the cost of mutation 1

    additive_fitness = 0,    # 1: fitness = 1 - cost1 - cost2 -... - cost5.  <>1: multiplicative: Fitness = (1-cost1)*(1-cost2)*...(1-cost5)
 
    DrugDose1 =  200, DrugDose2 = 200, DrugDose3 = 200.0,  DrugDose4 = 200.0, # Dose of drugs taken
    drug_decay1  = 1.0,      # Per day clearance rate of drug 1
    drug_decay2  = 1.0,      # Per day clearance rate of drug 2
    drug_decay3  = 1.0,      # Per day clearance rate of drug 3
    drug_decay4  = 1.0,      # Per day clearance rate of drug 3
    drug_2nd_decay1 = 0.1,   # Second-phase decay rate drug 1
    drug_2nd_decay2 = 0.1,   # Second-phase decay rate drug 2
    drug_2nd_decay3 = 0.1,   # Second-phase decay rate drug 3
    drug_2nd_decay4 = 0.1,   # Second-phase decay rate drug 4
    conc_2nd_phase1   = 1.0e-70,  # Drug concentration at which second-phase starts for drug 1
    conc_2nd_phase2   = 1.0e-70,  # Drug concentration at which second-phase starts for drug 2
    conc_2nd_phase3   = 1.0e-70,  # Drug concentration at which second-phase starts for drug 3
    conc_2nd_phase4   = 1.0e-70,  # Drug concentration at which second-phase starts for drug 4
 
    min_adherence1 = 0,       # Each person has an randomly choosen adherence level ranging from min_adherence to max_adherence.
    max_adherence1 = 1,
    min_adherence2 = 0,       # Each person has an randomly choosen adherence level ranging from min_adherence to max_adherence.
    max_adherence2 = 1,
    min_adherence3 = 0,       # Each person has an randomly choosen adherence level ranging from min_adherence to max_adherence.
    max_adherence3 = 1,
    min_adherence4 = 0,       # Each person has an randomly choosen adherence level ranging from min_adherence to max_adherence.
    max_adherence4 = 1,
    adherence_type = 1:2, #1=random, 2=cyclic
    adherence_type_prob=c(1,0),#default: all agents adherence type 1,
    adherence_days_high=5,#for cyclic adherence
    adherence_days_low=2,#for cyclic adherence
    aherence_days_high_prob=0.9,#for cyclic adherence
    aherence_days_low_prob=0.1,#for cyclic adherence
    BaseIC50Drug1 = 200.0, BaseIC50Drug2 = 200.0, BaseIC50Drug3 = 200.0,  # Concentration of drug that blocks V by 50%
    BaseIC50Drug4 = 2.0,  # Drug 4 is some super-effective 2nd-line therapy combo

    Interaction_Model_Drugs12 = 1,   # 1 = Bliss independence, 2 = Simple saturation (Huang et al. 2003),
                                     # 3 = Lowe additivity (not yet implemented)
                                     # Background: Current parameterizations assumes that drugs 1 and 2 are both NRTIs
                                     # Therefore, they may compete for the active site, reducing the degree of inhibition
                                     # Interaction_Model_Drugs12 implements different math ideas for how they may combine 
    FC_D1_Mut1    = 50.0,   # Effect of mutation 1 on the IC50 value of drug 1.  (Fold-change from baseline)
    FC_D1_Mut2    = 1.0,
    FC_D1_Mut3    = 1.0,
    FC_D1_Mut4    = 1.0,
    FC_D1_Mut5    = 1.0,

    FC_D2_Mut1    = 1.0,
    FC_D2_Mut2    = 50.0,
    FC_D2_Mut3    = 1.0,
    FC_D2_Mut4    = 1.0,
    FC_D2_Mut5    = 1.0,

    FC_D3_Mut1    = 1.0,
    FC_D3_Mut2    = 1.0,
    FC_D3_Mut3    = 10.0,
    FC_D3_Mut4    = 5.0,
    FC_D3_Mut5    = 1.0,

    FC_D4_Mut1    = 1.0,
    FC_D4_Mut2    = 1.0,
    FC_D4_Mut3    = 1.0,
    FC_D4_Mut4    = 1.0,
    FC_D4_Mut5    = 1.0,

    StochasticCut = 1.0e-6,  # Density of cells (or viruses) below which cell changes occur stochastically
    AbsoluteCut   = 1.0e-7,  # Density of cells (or viruses) that correspond to 1 cell per body.

    Dosing_Interval =1,#Dosing_Interval =1 means once daily dosing,Dosing_Interval = 2 means twice daily dosing
    Therapy_Type = 1,#Therapy_Type = 1 eans three individual pills Therapy_Type = 2 means that drugs 1 and 2 are contained within a single pill, Therapy_Type = 3 means that all three drugs are contained within a single pill

    # Stockout parameters (e.g., no gets Drug 1 btw StopDrug1 and RestartDrug1)
    # These apply to all patients regardless of their adherence
    StopDrug1    = 1000000000.0, # Stock out time (in days) for drug 1 
    RestartDrug1 = 1000000000.0, # Drug 1 becomes available again 
    StopDrug2    = 1000000000.0, # Stock out time (in days) for drug 2 
    RestartDrug2 = 1000000000.0, # Drug 2 becomes available once again 
    StopDrug3    = 1000000000.0, # Stock out (in days) for drug 3
    RestartDrug3 = 1000000000.0, # Drug 3 becomes available once again 
    StopDrug4    = 1000000000.0, # Stock out (in days) for drug 3
    RestartDrug4 = 1000000000.0, # Drug 3 becomes available once again 

# -- Parameters related to viral load testing
 # "testing" fxn
    prob_elig_vl_test         = 0.75,
    time_on_tx_for_vl_testing = 365/2,
    vl_testing_interval       = 365/2,

# -- Parameters related to PrEP / Vaccine models that use alleles to model resistance
 #John's Prep?
    PrEP_Model = FALSE,

# -- Parameters related to drug resistance testing and start of 2nd line therapy (aim 3 only)
    resist_testing_model = "interval",
    mean_resist_test_interval = 182,
    time_on_tx_for_resist_testing = 182, # Days on tx before clinicians test for resistance
    VL_thres_resist_testing = 1e3, # Viral load at which clinicians will do a resistance test
    no_muts_switch_2nd_line = 1,  # Num resist mutations needed to trigger switch to 2nd-line therapy
   
#-- vital dyamics model  parameters -------------#
   #used to get ASMR/initial aged distributions
    # input_parameters_derived(...) -> input_parameters_asmr(...) ->
    #                               -> input_parameters_age_distribution(...)
    min_age                  = 18,
    max_age                  = 55,
    age_dist_new_adds        = "min_age", # "mixed" (some min_age, others older)
                               #or "linear_decline_18_55"
    prop_new_agents_min_age   = 0.45, #for "mixed" see above line
    asmr_data_male            = "usa_men_18_to_100",#other option: "south_africa_male"
    asmr_data_female          = "south_africa_female",
    initial_agedata_male      = "usa_men_18_to_100", #other options:"south_africa_male_16_to_100", "stable_age_no_hiv_dist"
    initial_agedata_female    = "south_africa_female_16_to_100_2014", #other options: "stable_age_no_hiv_dist"
  # "deaths(...) -> vital_death_aids() or vital_death_aged_out or vital_death_non_aids  
    aids_death_model         = "cd4",        # c("Gamma_Death","daily_prob","cd4")
    death_rate_constant      = 0.000003,   	 # 0.000003 is from CASCADE
    death_rate_exponent      = 6.45,         # 6.45 is from CASCADE
    cd4_cat1_death_prob      = 0.0000112,    #prob. of death for cd4 cat1
    cd4_cat2_death_prob      = 0.0000148,    #prob  of death for cd4 cat2
    cd4_cat3_death_prob      = 0.0000333,    #prob of death for cd4 cat3
    cd4_cat4_treated_death_prob = 0.0000760, #prob death for cd4 cat4(aids) on tx
    cd4_prob_incr_nadir      = 0.03,         #prob of improving one cd4 cat from nadir
    cd4_prob_incr_nadir_minus= 0.0005,      #prob of improving one cd4 cat from nadir -1
    time_in_aids             = 475,
#births module
 #vital_births_calculate_new    
    birth_model              = "poisson_birth_numbers",   # "births=deaths", "poisson_birth_numbers", "exponential_growth","constant_rate", "constant_number", "exponential_growth"
    baseline_input_exp_growth = 0.007, # Used with "exponential_growth"  MUST BE SCALED BY HAND TO GET A STABLE AGE DISTRIBUTIONbirth_model              = "poisson_birth_numbers",   # "births=deaths", "poisson_birth_numbers", "constant_rate", "constant_number"
    contstant_birth_number   = 0,
    constant_birth_rate      = 0.0001306,
    poisson_birth_lambda_base     = 0.01370, #scaled to init pop size in 'input_derived_parameters', 7/720/16
    pop_growth_rate_annual   = 0.01, # as proportion, x100 for percent
    constant_rate_spread_out = .01, #birth model: "constant_rate_spread_out"
    births_per_year          = 1, #birth model: "constant_number_spread_out"

#-- social / treatment /testing  parameters -------------#
 # "testing"
    testing_model            = "interval",
    mean_test_interval_male  = 365,
    mean_test_interval_female = 442,
    mean_test_interval_under25 = 365,
    no_past_partners_time_prep = 365, 
    min_past_partners_prep = 1,
    min_current_partners_prep = 1, 
    min_pos_partner_duration = 50,
    prep_recent_test = 183,
    percent_eligible_on_prep = 1,
    start_prep_campaign = 5e5,
    prob_tx_droput          = 0,
    compact_el_divisor=1e5,
    ave_rel_dur_start = 5*365,
    test_result_delay = 35, # Delay between being infected and having sufficient antibodies for diagnosis
    reduction_test_interval_enhanced = 0.1,  # fage reduction in test interval for those identified for "enhanced" testing
    prob_enhanced_testing_before_campaign = 0.3, # Percentage of population that gets tested more frequently after scale-up campaign
    prob_enhanced_testing_after_campaign = 0.7, # Percentage of population that gets tested more frequently after scale-up campaign
    testing_limit = "percent_agents", # For models in which there is an upper limit to number tested. Other is "percent_agents_minus_diagnosed"
    tx_type               = NA,
    mean_trtmnt_delay = 1,
    mean_time_tx = 0, # used when tx_type = "time_dist"
    sd_time_tx = 0, # used when tx_type = "time_dist"
    start_treatment_campaign = 5e5,
    tx_limit = "absolute_num", # used with social_treatment_module_multiple_criteria.  
    max_num_treated_begin = 5e5,  # Num during pre-campaign, used with social_treatment_module_multiple_criteria
    max_num_treated = 5e5,       # Num during big campaign, used with social_treatment_module_multiple_criteria
    proportion_treated       = 1,
    yearly_incr_tx         = 0, # Setting this to 0.1 would mean 10% more people get treated each year
    proportion_treated_begin = 0.0,
    start_treat_before_big_campaign = 5e5,
    num_randomly_chosen_start_campaign = 0,  # Number of people outside the prioritized group who get treated (this is actually an outcome rather than a parameter)
    num_treated_start_campaign = 0,  # Total number of people treated at the start of the campaign (this is actually an outcome rather than a parameter)
    num_enhanced_start_campaign = 0, # number diagnosed or receiving enhanced testing (this is actually a variable)
   # Below four parameters are used in social_treatment_sex_age module
	    cov_prob      = c(0.0, 0.01, 0.021, 0.030, 0.049, 0.100, 0.191, 0.283, 0.402, 0.78), # Coverage levels corresponding to years below
	    cov_prob_yrs  = c(0, 11:18, 23), # Years at which coverage level changes
	    cov_prob_scal = matrix(c(0.7242, 0.9955, 1.2593, 0.5895, 0.8103, 1.0251), ncol = 2, dimnames = list(rep("", 3), c("f", "m"))), # values are by sex, then by age within sex
	    cov_prob_ageg = list(c(15, 25), c(25, 35), c(35, 56)),

    # -- additional treatment parameters --- #
     under_25_flag           =  FALSE,
    tx_in_acute_phase        = FALSE,
    tx_schedule_props        =c("F"=1,"V"=0,"N"=0,"P"=0),
    treatment_threshold      = 1e4,     #VL raw,not log10 transformed
    cd4_treatment_threshold  = 0,
    cd4_trt_guidelines_chgs  = NA, # Default for SA eligibility changes: list(4, 3:4, 2:4, 1:4)
    attr_treatment_threshold  = 5,  # Under development.  When finished, this parameter will 
    # allow one to target therapy to attribute groups
    # "attr_treatment_threshold" and higher (e.g. target therapy
    #  to people with the most connnections)
    min_inf_time_for_treat   = 0,     # number of timesteps (days)
    treat_thresh_partners    = 0,
    max_treated              = NA, #relevant for "social_treatment_john" module,
    #this value is set within module at start of tx campaign,
    #NA value is flat that indicates it hasn't been initialized yet
  
    prob_care                = 1.0,      # Probability of being eligible for care before the "under care" campaign
    prob_eligible_ART        = 1.0,
    prob_eligible_2nd_line_ART = 1.0,
    second_line_elig         = "vl_only",

# -- targeted efforts to get more people under care after "scale_up" compaign --------#
    start_scale_up_campaign = 5e5,     # At this time, increase the number the number of people eligible for care
    prob_care_before_campaign = 1.0,       # Probability of being eligible for care after the "under care" campaign (should be >= prob_care)
    prob_care_after_campaign = 1.0,       # Probability of being eligible for care after the "under care" campaign (should be >= prob_care)
    scale_up_type             = "random",  # Method for prioritizing people for "under care" campaign.  See the new routine
                                           # "social_bring_new_groups_into_care" for list of other options (e.g., "AIDS", "under30")
    min_age_recruit_for_care  = 0.0,   # Parameter that allows targeting of people of intermediate ages to get under care
    max_age_recruit_for_care  = 100,   # Parameter that allows targeting of people of intermediate ages to get under care

# -- vaccine parameters  --------#
 #"vaccination" fxn
    preventative_campaign    = F,
    start_vacc_campaign      = 5e5,
    perc_vaccinated          = 0.99,
    target_vacc_att          = FALSE,
    vacc_eff_duration        =  365*3,
    risk_comp_cond           = F,    # Set to T to induce reduction in condom use among vaccinated susceptibles and vaccinated, infected, undiagnosed individuals.
    risk_comp_cond_rr        = 0.70,
    risk_comp_degree         = F,    # Set to T to induce increase in degree among vaccinated susceptible and vaccinated, infected, undiagnosed individuals
    risk_comp_degree_rr      = 1.3,
    vacc_therapeutic_campaign = F, #flag whether protective (=F) or therapeutic vaccine(=T) in effect
    spvl_decrement_vaccine    = 1.0,
    vacc_multi_eff           = F, # flag for vaccine model with distribution of vaccine efficacies
#coital acts module
  #social_coital-acts
    prob_sex_by_age          = FALSE,
    prob_sex_age_19          = 0.285, # used when prob_sex_by_age == TRUE
    max_age_sex              = 75,   # used when prob_sex_by_age == TRUE
    aids_sex_cutoff_prop     = 0.47,  #proportion of time-in-aids afterwhich coital acts cease; Holllingsworth 2008
    mean_sex_acts_day        = 0.2,
    disclosure_prob          = 0.9,    # PUMA - seems high and may want to revisit; also should perhaps impact condom use rather than coital freq (as per Mardham)
    act_redux_discl          = 0.0,    # MARDHAM
 #"social_condom_use"
    condom_prob              = 0.5,
    condom_prob_sd           = 0.0373, # standard deviation for condom prob from MARDHAM simulation
    condom_prob_change       = F,      # set to true for condom_prob to be 0 initially, and increase as a hill function governed by below parameters
    condom_prob_max          = 0.5,   # parameter used in hill function if condom_prob_change == T
	  condom_prob_inflect      = 365*12, # parameter used in hill function if condom_prob_change == T
	  condom_prob_pow          = 4.1,      # parameter used in hill function if condom_prob_change == T
    RR_cond_male_concurrent  = 1.438,
    RR_cond_fem_concurrent   = 1.0,
    percent_condom_users= 1, # non-users never use condoms when partnered with other non-users.  users will use condoms with some probability
    condom_use_rel_dur = FALSE,
    condom_use_age = FALSE,
    age_condom_use_halves = 50, # Only used when condom_use_age is true
    individual_condom_prob   = F, #set to T for condom probability to be at the agent level, First added for PrEP work by SES 5/2019 
    individual_condom_prob_var   = 0.5, #Agent level condom probability, First added for PrEP work by SES 5/2019 
#sti/circumcision probabilites for agents (used in "vital new additions" fxn)  
    circum_prob              = 0.85,
    sti_prob                 = 0.0, #used in "vital_new_additions"
# miscellaneous/clarificaiton needed
    sti_prob_att             = NA,
   circum_prob_chg          = c(0.45,   0.5,    0.55,   0.6,    0.65,   0.7,    0.75,   0.85),
   circum_prob_yr_chg       = NA, # c(12*365, 13*365, 15*365, 16*365, 18*365, 20*365, 22*365, 24*365),
    prop_AI                  = 0.10,
    mean_prop_acts_AI        = 0.4266,
    sd_prop_acts_AI          = 0.2146,
  #msm role: "social_role_msm"  
   role_props               = c('I'=0.24, 'R'=0.27, 'V'=0.49), #to activate msm roles, set role props to c("I"=x,"R"=y,"V"=z), where x+y+z=1 and are >=0
    role_trans_mat          = matrix(c(1,0,0,0,1,0,0,0,1),
                                     nrow=3,dimnames=list(c("I","R","V"))),
    prob_iev                 = 0.4,         # Average of Mardham and PUMA
# Generic attribute transition
 #"social_generic_att_transition
    generic_nodal_att_values        = NA,   # names of generic attributes (eg, 1:5)
    generic_nodal_att_values_props  = NA,   # proportions of each att in initial pop
    generic_nodal_att_values_props_births = NA, #how new values distributed with addtns to pop
    generic_nodal_att_no_categories = NA,   # how many generic att categories
    generic_nodal_att_trans_mat     = NA,    # matrix of per timestep transition probs, each row sums to one
# ----------    CD4 data   ----------------------------------------------------  
   # Probabilities for initial CD4 value (based on SPVL)
   cd4_init_probs =  structure(list(cd4_500     = c(0.88, 0.87, 0.85, 0.78, 0.73, 0.71, 0.64, 0, 0),
                      cd4_500_350 = c(.12,  0.12, 0.12, 0.19, 0.21, 0.25, 0.27, 0, 0),
                      cd4_350_200 = c(0.0,  0.01, 0.03, 0.03, 0.05, 0.04, 0.09, 1, 1)),
                      .Names = c("cd4_500+", "cd4_500_350", "cd4_350_200"),
                      class = "data.frame",
                      row.names = c("spvl<3",       "spvl_3.0_3.5", "spvl_3.5_4.0",
                                    "spvl_4.0_4.5", "spvl_4.5_5.0", "spvl_5.0_5.5",
                                    "spvl_5.5_6.0", "spvl_6.0_6.5", "spvl>6.5")),
  # Mean passage time for CD4 categories 1,2,3,4   
  CD4_lookup =structure(list(cd4_500  = c(6.08, 4.69, 3.94, 2.96, 2.25, 1.47, 0.95, 0.32, 0.30),
                 cd4_500_350  = c(5.01, 2.52, 4.07, 3.09, 2.32, 1.55, 1.19, 0.59, 0.46),
                 cd4_350_200  = c(3.60,  3.68, 2.38, 3.81, 3.21, 2.27, 1.00,  0.68, 0.37),
                 cd4_200  = c(4.67, 4.11, 3.54, 2.98, 2.42, 1.86, 1.29, 0.73, 0.17)),
                .Names = c("cd4_500+", "cd4_500_350", "cd4_350_200", "cd4_200-"),
                class = "data.frame",
                row.names = c("spvl<3", "spvl_3.0_3.5", "spvl_3.5_4.0", "spvl_4.0_4.5",
                          "spvl_4.5_5.0",
                          "spvl_5.0_5.5", "spvl_5.5_6.0", "spvl_6.0_6.5", "spvl>6.5"))


){
    
  evo_args <- as.list(environment())
  return(evo_args)

}
