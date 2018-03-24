Viral load and CD4 progression
================

Viral Load and Setpoint Viral Load
----------------------------------

• At infection, agents are assigned a set-point viral load (SPVL), using a function that incorporates infector SPVL, assumed heritability of SPVL, and a randomly determined host-specific factor (stochastic white noise), and heritability. The maximum possible viral load (all viral dynamics on log10 scale) is 7.0 and the minimum is 2.0.

• Initial VL starts out at -4.0 rises exponentially to a peak acute phase level, and then has a biphasic decay to SPVL. The acute phase last 50 days. Default value for peak acute phase VL is constant for all agents but can be set to be a function of an agent’s SPVL. After the end of the acute phase, VL increases linearly, starting at SPVL, at a pre-specified chronic phase rate of increase. At the onset of AIDS, VL increases linearly at a pre-specified rate of increase for the AIDS phase until it reaches the assumed maximum VL value for AIDS at which it remains until agent’s death. (The onset of AIDS is triggered by CD4 values described below.)

• With commencement of ART, VL will decrease linearly over 30 days to a “undetectable” level of 1.7 and remain there for the duration of treatment. If treatment stops, VL will then increase linearly over 30 days to the VL level at the start of treatment and then progress as if treatment did not occur.

Example of viral load progression with and without ART. For this example, ART begins at 5 years since infection. Without ART, AIDS begins at about 10 years since infection and death at about 13 years. While acute phase duration is fixed at 90 days, start of AIDS and time of AIDS death are stochastic.
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/vl1.png)  

CD4 Dynamics
------------

• CD4 counts for an agent is a recorded as an ordinal variable with 4 categories; categories 1,2,3, and 4 represent CD4&gt;500, 500&gt;CD4&gt;350, 350&gt;CD4&gt;200, 200&gt;CD4&gt;0, respectively. Category 4 represents AIDS stage.

• At infection, an agent is probabilistically assigned an initial CD4 category (1,2, or 3) where higher SPVL values increases the probability of assignment to categories 2 or 3. For each daily timestep, an infected agent in categories 1,2, or 3 can move to the following CD4 category (representing lower CD4 counts) or remain in the current category based on the outcome of a Bernoulli trial with the probability of success (moving to the next category) equal to the inverse of the mean passage time (in days) for a given SPVL and CD4 category. Passage time in CD4 category 4 is fixed for each SPVL category; at the end of passage time in category 4, an agent is categorized as having died of AIDS.

• With commencement of ART, an agent’s CD4 category is set to 1 and remains there until treatment stops or agent’s death. If treatment stops before agent’s death, the CD4 category of an agent is set equal to the value at the start of treatment.

Example of CD4 progression with corresponding VL progression (no treatment)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/vl2.png)  

