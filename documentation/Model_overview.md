EvoNetHIV Model Overview
================
Last updated June 2021

------------------------------------------------------------------------
A software package for modeling HIV epidemics and viral evolution in sexual networks. The key advances of EvoNetHIV, with respect to HIV epidemic modeling, are the inclusion of advanced social network models and HIV evolutionary potential into a reasonably standard HIV epidemic model.

------------------------------------------------------------------------
General model overview
----------------------

EvoNetHIV is a stochastic agent-based model for simulating HIV epidemics. The model incorporates sexual network structure, within-host viral and immune system dynamics, antiretroviral therapy, vaccination, condom use, circumcision, demography, and viral evolution. Each agent (i.e., individual) is a discrete entity that has over 30 attributes. Important agent attributes include sex, age, HIV infection status, time of infection, viral load, set point viral load, antiretroviral treatment status, and vaccination status. Some attributes are regularly updated, such as age or viral load (for infected individuals) while others (e.g. sex, set point viral load), do not change once assigned to the agent. Agents are “born” (added to the model as uninfected individuals), age, and depart from the model, either through death from AIDS, death from background mortality, or aging out. Agents can become infected and infect other agents. Agents cannot be infected twice (experience dual or super-infection).

Because EvoNetHIV is a stochastic model, identical parameter values or starting conditions for different model runs will produce different results. Many input parameters are mean or variance values of probability distributions used to draw random numbers from the specified probability distribution. For example, the parameter for mean sex acts per ongoing relationship per time step is the mean value of draws from a Poisson distribution. Model dynamics can occur on daily timesteps; thus fine-scale epidemiological and behavioral processes can be simulated.

EvoNetHIV is written in the R programming language as a series of interchangeable modules and user-specified parameter values that control different components of the system (e.g. network structure; sexual behavior; within-host dynamics (viral load, disease progression), and treatment or other public health interventions). EvoNetHIV uses a modular framework from the [EpiModel](http://www.epimodel.org/) API.

### Model components

**Network structure**

Agents can be either isolates or have one or more concurrent relationships with other agents. This network structure is dynamic and changes according to the specified network statistics. The network module is built using exponential random graph models in the [statnet](http://www.statnet.org/) framework, which provides a language for specifying arbitrarily complex network models.

**Sexual behavior**

Important behavioral dynamics include sex roles (for MSM), frequency of sex, condom use, HIV testing frequency, and adherence to treatment. Behavioral dynamics can be specified by the user.

-   *MSM model*. When the MSM model structure is selected, each agent is assigned either an *insertive*, *receptive*, or *versatile* role. Insertive agents can form relationships with versatile or receptive agents; receptive agents can form relationships with versatile or insertive agents; and versatile agents can form relationships with all three agent types. Under our default parameters, receptive agents have a higher per-act probability of getting infected.

-   *Heterosexual model*. When the heterosexual model structure is selected, only relationships between opposite sexes are allowed. Under our default parameters, the population tends to a 50-50 sex ratio. Although our default parameters assume that men and women are equally susceptible, parameters governing the relative suspectibility of men and women can be changed.

**Within-host dynamics**

-   *Set point viral load (SPVL)*. The viral load at the beginning of the chronic phase of infection is referred to as the set point viral load (SPVL) (in untreated infections). SPVL is partially heritable across transmission pairs:  SPVL for newly infected agents is determined by a combination of the donor’s SPVL and by environmental (random) variation. The contribution of the donor’s SPVL to the recipient’s SPVL is determined by the assumed (user-defined) population-level heritability of HIV SPVL.

-   *Viral load progression*. The model divides infection into three phases: an acute phase when viral load rises and declines rapidly; a chronic (or latent) phase when viral load increases slowly over a period of years; and an AIDS phase when viral load increases rapidly to a high AIDS-specific viral load. Viral loads are not static within these phases, though, as the model tracks VL dynamically for each individual for each model timestep.

-   *CD4+ T cell dynamics*. The model follows CD4 dynamics in HIV infection as described in Cori et al (2015). In the absence of antiretroviral treatment, the model relates individual SPVL to the starting CD4 count category (individual CD4 count immediately after infection) and to disease progression based on waiting times in four CD4 count categories: CD4≥500; 500&gt;CD4≥350; 350&gt;CD4≥200; CD4&lt;200 (AIDS).

**Public health actions**

-   *Antiretroviral treatment (ART)*. ART can be provided to HIV-infected individuals based on user-defined categories of eligibility, with eligibility determined by either CD4 count or by time elapsed since infection (or both). Viral load after ART initiation drops to 50 copies/mL.

-   *Vaccination*. HIV vaccines can be protective or disease-modifying. Protective vaccines protects vaccinated individuals from infection by decreasing the per-act rate of transmission, but do not affect rates of subsequent disease progression or the viral load of a vaccinated individual if that person becomes infected. Disease-modifying vaccines do not protect vaccinated individuals from protection but instead lower the SPVL in infected individuals by a user-specified amount. The model also includes viral diversity with respect to vaccination, via a user-specified number of possible viral genotypes with varying levels of vaccine sensitivity and resistance.
