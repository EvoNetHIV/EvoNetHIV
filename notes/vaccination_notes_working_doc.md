
## Evonet changes to support phase 4 evaluation after a vaccine, with sequence evolution; also includes changes for future support of a representation of a phase 2b/3 efficacy trial.


### Vaccination, sequence evolution, sieve effects.

The background is that we published a paper exploring the effects on a vaccine's efficacy of the waning population of susceptible viruses after rollout. Due to changes over time in the relative proportion of exposing viruses that are susceptible to the vaccine's effects, we expect that a vaccine with a sieve effect will have highest efficacy at rollout and the efficacy will wane as the proportion shifts due to the removal of susceptible viruses from circulation. For the present project we are extending the model to allow for more than two viral types (susceptible and resistant), as well as for other changes to support representation of virus-specific features ("marks") and interactions between these features and other aspects of the evonet model.  Specifically, our goals are twofold:

 1) To support more nuanced evaluation of the impacts on a network after rolling out a partially effective vaccine in which the effect of treatment can depend on both features of the person (such as immune response to vaccination) as well as features of the virus (such as the specific "mark" of the virus). This requires representing more nuance than a binary mark, and in particular to support representing emergence of resistant variants it requires generation of variation in the mark within the simulation.

 2) To support employment of evonet for evaluating statistical methodology for estimating sieve effects in the context of an efficacy trial (phase 2b or phase 3).

Our first goal is to accomplish #1 but we are designing the changes to also ensure we can accomplish #2.

A key goal of #2 will be to evaluate methods that differentiate between acquisition sieve effects, which are characterized by dfferential infection risk across viruses (a function of the viral "mark" value at exposure), from post-acquisition effects, which are characterized by differential evolution within-host after acquisition. Thus one design requirement is sufficient complexity to represent that a vaccine's effect may impact both the acquisition probability and the evolution of the virus post-acquisition.  Also, to represent the sieve effects hypothesized to have been present in the rv144 vaccine trial, another design requirement is sufficient complexity to represent a non-binary "response to vaccination" parameter that could modify the acquisition risk in a mark-dependent manner.

### Code changes to support representing vaccination, sequence evolution, and sieve effects.

### Background notes

The dat data structure contains everything.  It is passed around and return-on-modify quite a bit, which could be a (future?) efficiency issue. A simple fix might be to replace it transparently with a reference object.

To these ends we are making the following changes to Evonet. We believe that these are approximately minimally sufficient to address the requirements. The inputs are a set of functions, here shown with the "existing model: 1" choices.  Since we always follow these naming conventions with model numbers, we do this lexically and automate it. See for example transmission_main_module.R, where we access the appropriate calculate_theta function directly.
  draw_m = draw_m1
  calculate_theta = calculate_theta1

1) We are modifying the code where the vaccine's effect on transmission probabilty is employed. Specifically, we are changing the computation of that effect from a constant probability of infection-avoidance employed only if the person is vaccinated, to a probability (theta) of transmission that is computed at the moment of potential infection.  Note that for now we only include phi in calculating theta, so it is considered the conditional probability of transmission based on the parameters represented in phi, given that the transmission would otherwise occur.
 i. m <- draw_m( dat, at ): first drawing a single mark (m) from the transmitter's within-host distribution of viral marks (see below).
 ii. theta <- calculate_theta( dat, m, at ): next using this value (m), along with a person-specific vaccine response parameter (phi), to compute the probability (theta) that the infection occurs after considering the value of phi (for an event that would otherwise have lead to transmission despite vaccination).

2) We are modifying a few other bits of the code to support this change:
 i. Replacing the binary vaccination status indicator with an arbitrary real vector, phi, that represents the parameters necessary to evaluate vaccine-induced impacts on acquisition risk as well as on viral evolution. Note that this also can support pre-existing or otherwise intervention-independent immunity.
   a. Example: to support our former simpler model, phi can be just 0 or 1 to represent vaccination. [note this is what is presently in phi1, although I think phi1 represents both an initialization and an update for phi.. see below].
   b. Example: to support the V2-targeting antibody correlate of vaccine efficacy demonstrated in RV144, phi could be a continuous value representing the magnitude and affinity of V2-targeting antibodies induced by vaccination, or zero for unvaccinated folks.  To represent some pre-existing V2 targeting immunity this could be allowed to be non-zero even prior to vaccination.
   c. Example: to support the set of correlates identified in the RV144 primary and followup analyses, the phi vector could contain multiple such values eg V2-targeting, V3-targeting, C1-targeting antibodies as well as T cell correlates (e.g. polyfunctionality as measured by COMPASS).

 ii. dat$pop$phi <- daily_update_phi( dat$pop, at ) : This function is run daily for all participants, allowing phi to be changing over time, eg to represent waning vaccine response, or a boosted vaccine response. Note that this replaces and extends the existing mechanism by which time since vaccination is used to set the vaccination status to 0 after an elapsed time since vaccination.
   a. Example: V2-targeting antibodies decline over time since vaccination. Could be faster in some people, eg dependent on BMI.
   b. Example: Repeated antibody infusion ala AMP results in discontinuous spikes up followed by continuous logarithmic decay.
   c. Example: Differential time scales of emerging, boosting, and waning adaptive immune responses resulting in different components of phi updating according to different functions.

 iii. Replacing the binary mark indicator with an arbitrary real vector, m, that represents the parameters necessary to represent viral evolution and the diversity that is relevant to the model. Note that we are not attempting to represent entire sequences, although this representation could in principle support that. Note that a person has multiple viruses so a person does not "have a mark m" but they instead "have a distribution of marks, from which we know how to draw m" -- see below.
   a. Example: to support our former simpler model, m can be a binary indicator ("susceptible").
   b. Example: to support the two binary vaccine-mismatch sieve effects at K169X and I181X in the viral envelope proten gp120, as described in Rolland and Edlefsen et al 2011, m can be a vector of two binary marks.
   c. Example: to support the analysis in Edlefsen and Rolland et al 2013 of differential efficacy by overall Hamming distance across gp120, m can be (or contain) a count of AA mismatches, or a continuous-valued distance measure (such as phylogenetic distance after fitting a phylogenetic model using an HIV-specific substitution matrix).

 iv. Implementing a time-varying distribution of marks for each infected person. For now we are representing this as a) a set of parameters for specifying the distribution from which m is drawn upon a potential-transmission event as described above, and b) a function for updating these parameters over time, which can be modified by the vaccine status phi. For a continuous-valued element (j) of the vector m, this will be a pair of continuous values mu_j and sigma_j, representing the center and dispersion of the value of m. For a dichotomous element (i) of m, mu_i is the probability that m_i = 1 and sigma_i is ignored. This representation can support most distributions we can think of. Note that to represent multple founders or super-infection, we will need to allow multiple modes of the mark distribution, eg by duplicating mu (for each founder) and adding mixing proportions. The chosen representation is sufficiently flexible to support this.
    a. Example: at the time of acquisition, mu = m (the mark of the transmitted virus); sigma = 0. Then each day sigma increases until it reaches some maximum.
    b. Example: the daily update to (mu,sigma) could depend on phi, e.g. to represent the conjecture (M. Rolland unpublished) that V2-specific pressure in the RV144 trial induced post-acquisition selection pressure on the evolving virus that continued to restrict its diversity months after the end of the trial.
    c. Example: to represent multiple infection founders, the first entry of mu can indicate the number of distinct sub-vectors contained in mu, and the daily update can represent a convergence of the subpopulations over time (as for example Carolyn Williamson has shown in CAPRISA Illumina data).

### Initial approaches for making these changes:

1) First implement the existing model from our two-mark paper using the proposed changes.  Ensure regression tests pass, etc.

2) First new use of the new model will target a two-binary-mark case to represent K169X and I181X. Here we will employ only acquisition sieve effects, so that the mark update step will be independent of phi.
