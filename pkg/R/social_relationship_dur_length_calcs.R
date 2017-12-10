#note: 2 fxns here
# 1) update_partnership_durs
# 2) update_compact_el_and_rel_durs


update_partnership_durs <- function (new_compact_el, previous_compact_el, previous_durs) {
  # Utility function that updates dat$partnership_durs
  #
  # Inputs: ** Added to evonet circa 9/18/17 **
  #   1. new_compact_el      -- Current compact edgelist [vector of real numbers] 
  #   2. previous_compact_el -- Compact edgelist from the previous time point [vector of real numbers]
  #   3. previous_durs    -- Number of days the partners listed in previous_compact_el have been together [vector of integers whose length equals previous_compact_el]
  #
  # Outputs:
  #   1. partnership_durs -- Revised list of relationship durs [vector of real numbers whose length equals new_compact_el]
  #  
  # Expected uses
  #   1. Called by update_compact_el_and_rel_durs
  #
  # Notes:
  #   1.  Module assumes fast edgelist.
  #   2.  This routine is extremely inefficient (takes approximately 1 second per time step for a network of size 3000 with md 0.7)
  #         -- Not so slow, however, that it requires special treatment for average evonet application
  #         -- Could, in principle, be sped up by Rcpp C routine, vectorization, or clever programming (these might reduce run times to ~0.1 seconds per time step)
  #   3.  Compact edgelist data structure only works for networks with less than ~100,000 links
  #   4.  Routine assumes that there are no duplicated entries in the fast-edgelist data structure (true for evonet)
  
  revised_durs <- numeric(length(new_compact_el))
  for (ii in 1: length(new_compact_el)) {
    revised_durs[ii] <- 0
    for (jj in 1:length(previous_compact_el)) {
      if (new_compact_el[ii] == previous_compact_el[jj]) {
        revised_durs[ii] <- previous_durs[jj]+1
      }
    }
  }
  return(revised_durs)
}

update_compact_el_and_rel_durs <- function(dat,at) {
  # Updates compact edgelist data vector together with accompanying vector of relationship durations
  #
  # Inputs
  #   1. dat$el -- existing fast edgelist -- [matrix with two columns]
  #   2. dat$compact_el -- compacted form of edgelist -- [vector of real numbers]           ** Added to evonet circa 9/18/17 **
  #   3. dat$partnership_durs -- partnership durations -- [vector of real numbers]  ** Added to evonet circa 9/18/17 **
  #   4. dat$param$compact_el_divisor [integer] [must be power of 10 greater than the number of expected links, e.g. 100,000]  ** Added to evonet circa 9/18/17 **
  #   5. dat$param$ave_rel_dur_start -- average relation duration at t = 0 [integer]  ** Added to evonet circa 9/18/17 ** 
  #
  # Outputs
  #   1. dat$compact_el [vector of real numbers]            ** Added to evonet circa 9/18/17 **
  #   2. dat$partnership_durs [vector of real numbers]   ** Added to evonet circa 9/18/17 **
  # 
  # Notes:
  #   1. "compact_el" is a compacted form of dat$el 
  #          Part before decimal gives the first partner, part after the decimal gives the second partner
  #            e.g., 102.00134 means that agent 102 is partnered with agent 134
  #   2. Indices for partership_durs correspond to indices for compact_el
  #            e.g., compact_el[3] = 34.00067 & partnership_durs[3] = 80, means that agents 34 and 67 have been together for 80 days
  #   3. Routine will fail when the number links exceeds "compact_el_divisor"
  #   4. Maximum vaulue of compact_el_divisor may be machine dependent!
  #   5. Initial durations independent of age (thus model may be out of equilibrium the first few years)
  #   6. Module assumes fast edgelist
  
  # Make local copies of variables and parameters for readability (James you can delete these if you feel that they don't improve readability)
  curr_el = dat$el[[1]] #  Big questions about the [[1]][[1]] format.  Maybe just one [[1]] for actual run.  Need to verify that this will work for nsims > 1
  col1=dat$attr$id[curr_el[,1]]
  col2=dat$attr$id[curr_el[,2]]
  
  j_divisor = dat$param$compact_el_divisor
  
  
  if (length(col1)>=j_divisor) { cat("Error: individual partnership length calculation routine cannot handle edgelists with more than", j_divisor," elements\n" ); exit(1) }
  
  if (at ==2) {
    
    dat$compact_el <- col1+col2/j_divisor
    
    # Make assumptions about initial distribution of parternship lengths
    dat$partnership_durs <- dat$compact_el                   # Poor man's method for creating vector of 0's whose length equals the current compact edgelist
    dat$partnership_durs[1:length(dat$partnership_durs)] <- runif(length(dat$partnership_durs), min=0, max=2*dat$param$ave_rel_dur_start) # Not tested.  
    
  } else {
    
    #these might not be filled in when at=2 if no sex 
    if(is.null(dat$compact_el)){dat$compact_el <- col1+col2/j_divisor}
    if(is.null(dat$partnership_durs)){dat$partnership_durs <- dat$compact_el}
    
    # Save previous version of the compact edgelist
    previous_compact_el <- dat$compact_el
    
    # Get revised compact edgelist based on changes that have occurred to dat$el since previous time step
    new_compact_el <- col1+col2/j_divisor
    
    # Call function to update partnership durations
    previous_durs <- dat$partnership_durs
    dat$partnership_durs <- update_partnership_durs(new_compact_el, previous_compact_el, previous_durs)
    
    # Update compact edgelist
    dat$compact_el <- new_compact_el
  }
  return(dat)
}
