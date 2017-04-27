

# Assume no space travel: N = 1.75 Billion = 1750000000 years = 17500000 centuries
# Source: http://www.livescience.com/39775-how-long-can-earth-support-life.html
N = 17500000 # Total timeunits
pop = 7000000000
# eps := probability we are wiped out in time = t given humanity still exists
# pi := prob we exist at time = t given eps
# pi_t = prod((1-eps)[1:t])
genPi = function(eps){
  pi = rep(NA, length(eps))
  for(i in 1:length(eps)){
   pi[i] = prod((1-eps)[1:i])
   if(pi[i] == 0){break} #Once we reach computational 0, stop wasting loops
   if(i %% 100 == 0){print(pi[i])}
  }
  pi = pi[1:length(pi) < i] #Everything else will be comp. 0
  return(pi)
}


# Here is the problem- R can't handle probabilities as small as we will get to
# They will be computationally zero orders of magnitude too soon
# Examples:
.8^17500000
.8^17500
.8^3000
#Here is why we don't need to worry about hitting computational zero
small = 2.225074e-308 #Smallest possible number in R
small*N*pop #By the time we have reached this number, the effects are negligible


# One timeunit is one century.
# Scenario 0: no change in current X-risk at all
eps0 = rep(.19, N)
pi0 = genPi(eps0)
expPop0 = pop*sum(pi0)
#rm(pi0, eps0)

# Scenario 1: Current reduction of X-risk by 10%
eps1 = c(.19*.9, rep(.19, N-1))
pi1 = genPi(eps1)
expPop1 = pop*sum(pi1)
#rm(pi1, eps1)
expPop1 - expPop0

# Scenario 2: Current reduction of X-risk by 10%, decaying impact in future
decayRate = .5
impacts = rep(0, N)
c = 0
impact = .1
while(impact > 0){
  impact = .1*decayRate^c
  impacts[c+1] = impact
  c = c+1
}
eps2 = rep(.19, N)*(1-impacts)
pi2 = genPi(eps2)
expPop2 = pop*sum(pi2)
expPop2 - expPop0


