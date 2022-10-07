#install.packages("odin")
library(odin)
library(ggplot2)

child_recr = odin::odin({
  ##TIME STEPS IS WEEKS

  ##number of social/ gender classes
  N_gender <- user(2)

  #kids coming in - total 7336 girls and 7842 boys yearly - so 141 girls and 151 boys weekly
  #they come in in a block and then we send them to their states by dividing up percentages
  age_in[] <- user(0) # should be set to [151, 141]

  excl_in[] <- user(0) #class breakdown - pc girls, boys excluded
  close_in[] <- user(0) #class breakdown - pc girls, boys close

  surv_in_i[] <- user(0) #pc of girls, boys in INCLUDED that are surveilled at 10
  surv_in_e[] <- user(0.07) #pc of girls, boys in EXCLUDED that are surveilled at 10
  surv_in_c[] <- user(1) #pc of girls, boys in CLOSE that are surveilled at 10


  #kids coming OUT oh lol this is going to be so hard.....
  u1_out[] <- user(0)  #percentage of exiters leaving from this state, boy and girl
  us1_out[] <- user(0)
  ur1_out[] <- user(0)
  w1_out[] <- user(0)
  ws1_out[] <- user(0)
  wr1_out[] <- user(0)
  j1_out[] <- user(0)
  jr1_out[] <- user(0)
  jw1_out[] <- user(0)
  jrw1_out[] <- user(0)
  u2_out[] <- user(0)
  us2_out[] <- user(0)
  ur2_out[] <- user(0)
  w2_out[] <- user(0)
  ws2_out[] <- user(0)
  wr2_out[] <- user(0)
  j2_out[] <- user(0)
  jr2_out[] <- user(0)
  jw2_out[] <- user(0)
  jrw2_out[] <- user(0)
  u3_out[] <- user(0)
  us3_out[] <- user(0)
  ur3_out[] <- user(0)
  w3_out[] <- user(0)
  ws3_out[] <- user(0)
  wr3_out[] <- user(0)
  j3_out[] <- user(0)
  jr3_out[] <- user(0)
  jw3_out[] <- user(0)
  jrw3_out[] <- user(0)

  ## parameters values - default in parentheses - timestep is weekly? lets try weekly
  alpha <- user(1.5) #changes degree of assortativity in mixing across social classes ('1 -> infinity' is equiv to 'totally assortative -> totally proportional')
  x[,] <- user(0)


  contact <- user(70) #contact rate per week - ten a day for 7 days?

  recruit_1[] <- user(0.001) #recruitment per contact? included -  you need 1000 contacts
  recruit_2[] <- user(0.002) #recruitment per contact? excluded - you need 500 contacts
  recruit_3[] <- user(0.005) #recruitment per contact? close - you need 200 contacts|?
  recruit_j[] <- user(0.005) #recruitment per contact? in jail - you need 200 contacts|?

  #d <- user(0.05) #probability of desisting at a critical point eg hospitalisation after an injury
  desist <- user(0.001) #probability of desisting at other times

  #arrest <- user(0.02) #rate of arrest bc of county lines involvement for the working group

  missing_i[] <- user(0) #rate of missing episodes for the included group
  missing_e[] <- user(0.005) #rate of missing episodes for the excluded group
  missing_c[] <- user(0.005) #rate of missing episodes for the close group

  ss_i[] <- user(0.005) #rate of stop&search for the included group
  ss_e[] <- user(0.005) #rate of stop&search  for the excluded group
  ss_c[] <- user(0.005) #rate of stop&search  for the close group

  # red <- user(0.637) #recidivism rate - For children released from custody in the year ending March 2020, 63.7%  re-offended
  # mortality <- user(0) #death rate from causes other than county lines
  # h[] <- user(0.25) #rate at which people in harm group commit harm... once a monthish?
  #m <- user(0.01) #percentage of harms that are fatal (the better letter would prob be f)...1%?

  remand <- user(0.12) #percentage of bail episode that are remand to custody
  r2c <- user(0.33) #percentage of custody remands that result in a custodial sentence

  end_rem <- user(0.175) #rate that sets the length of the average custody remand - should be 40 so 5.7 weeks, 1/5.7
  end_cust <- user(0.05) #rate that sets the length of the custodial sentence - meant to be 139 so lets say 20 weeks, 1/20


  surv_multiplier <- user(1.5) #figure this out properly but its some sort of elevated risk of criminalisation that you get from beign already known to the police
  cl_multiplier <- user(1.2) #some elevated risk of going missing, getting cautioned/sentenced etc if you're working CL (20%)


  school_ex_i[] <- user(0) #rates of school exclusion, included
  school_ex_e[] <- user(0) #rates of school exclusion, excluded

  impov <- user(0) #rates of school impoverishment, can be a neg number(?) maybe not...

  fte1[] <- user(0) #some kind of rate of included first time entrants
  fte2[] <- user(0) #some kind of rate of excluded first time entrants
  fte3[] <- user(0) #some kind of rate of close first time entrants total (should work out as like 600 a year, 23% female 77% male)
  rep[] <- user(0) #some kind of rate of repeated offending

  custody_fte[] <- user(0.01) #percentage of fte's that get a custodial sentence (of men, 1.5% - of women, 0.3%)
  custody_rep[] <- user(0.14) #percentage that get custodial sentence, of repeat offenders, who dont get remanded to custody... lol

  deter <- user(0) #rate of CL workers resolving to stop while doing a custodial sentence (note, they can be recruited back later)

  ##initial conditions
  #the old equal split for gender doesnt work bc girls are less likely to be close
  #do we have to do that equation bit or can everything just be explicitly set?
  #dN_1_ini[] <- user(0) #total initial pop included
  #dN_2_ini[] <- user(0) #total initial pop excluded
  #d N_3_ini[] <- user(0) #total initial pop close
  UN_1_ini[] <- user(78927) #number initially uninvolved, no police contact
  UN_2_ini[] <- user(33999) #number initially uninvolved, no police contact - 93.6% of the excluded
  UN_3_ini[] <- user(0) #number initially uninvolved, no police contact - zero for close, all already flagged
  US_1_ini[] <- user(0) #number initially uninvolved, surveilled - zero for included
  US_2_ini[] <- user(2429) #number initially uninvolved, surveilled - 6.6% of the excluded
  US_3_ini[] <- user(6071) #number initially uninvolved, surveilled - 100% of the close
  UR_1_ini[] <- user(4) #number initially uninvolved, in CSJ - lets say 800 start in the CSJ (i just picked), and i split between 3
  UR_2_ini[] <- user(396) #number initially uninvolved, in CSJ
  UR_3_ini[] <- user(400) #number initially uninvolved, in CSJ
  WN_1_ini[] <- user(0) #number initially working, no police contact
  WN_2_ini[] <- user(0) #number initially working, no police contact
  WN_3_ini[] <- user(0) #number initially working, no police contact
  WS_1_ini[] <- user(0) #number initially working, surveilled
  WS_2_ini[] <- user(1) #number initially working, surveilled
  WS_3_ini[] <- user(5) #number initially working, surveilled -lets start small working & hope it takes off?
  WR_1_ini[] <- user(0) #number initially working, in CSJ
  WR_2_ini[] <- user(2) #number initially working, in CSJ
  WR_3_ini[] <- user(2) #number initially working, in CSJ
  J_1_ini[] <- user(1) #number initially on custodial sentence
  J_2_ini[] <- user(37) #number initially on custodial sentence
  J_3_ini[] <- user(30) #number initially on custodial sentence - prev of custodial sentence is 38 so, just split it
  JW_1_ini[] <- user(0) #number of CL initially on custodial sentence
  JW_2_ini[] <- user(0) #number of CL initially on custodial sentence
  JW_3_ini[] <- user(1) #number of CL initially on custodial sentence
  JR_1_ini[] <- user(0) #number initially remanded to custody - none
  JR_2_ini[] <- user(4) #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
  JR_3_ini[] <- user(5.5) #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
  JRW_1_ini[] <- user(0) #number of CL initially remanded to custody
  JRW_2_ini[] <- user(0) #number of CL initially remanded to custody -
  JRW_3_ini[] <- user(1) #number of CL initially remanded to custody -


  initial(UN_1[]) <- UN_1_ini[i] #the old equal split for gender doesnt work bc girls are less likely to be close
  initial(UN_2[]) <- UN_2_ini[i]
  initial(UN_3[]) <- UN_3_ini[i]
  initial(US_1[]) <- US_1_ini[i]
  initial(US_2[]) <- US_2_ini[i]
  initial(US_3[]) <- US_3_ini[i]
  initial(UR_1[]) <- UR_1_ini[i]
  initial(UR_2[]) <- UR_2_ini[i]
  initial(UR_3[]) <- UR_3_ini[i]

  initial(WN_1[]) <- WN_1_ini[i]
  initial(WN_2[]) <- WN_2_ini[i]
  initial(WN_3[]) <- WN_3_ini[i]
  initial(WS_1[]) <- WS_1_ini[i]
  initial(WS_2[]) <- WS_2_ini[i]
  initial(WS_3[]) <- WS_3_ini[i]
  initial(WR_1[]) <- WR_1_ini[i]
  initial(WR_2[]) <- WR_2_ini[i]
  initial(WR_3[]) <- WR_3_ini[i]

  initial(J_1[]) <- J_1_ini[i]
  initial(J_2[]) <- J_2_ini[i]
  initial(J_3[]) <- J_3_ini[i]
  initial(JR_1[]) <- JR_1_ini[i]
  initial(JR_2[]) <- JR_2_ini[i]
  initial(JR_3[]) <- JR_3_ini[i]
  initial(JW_1[]) <- JW_1_ini[i]
  initial(JW_2[]) <- JW_2_ini[i]
  initial(JW_3[]) <- JW_3_ini[i]
  initial(JRW_1[]) <- JRW_1_ini[i]
  initial(JRW_2[]) <- JRW_2_ini[i]
  initial(JRW_3[]) <- JRW_3_ini[i]

  ## setting dinemsions for everything
  dim(U) <- N_gender
  dim(U_1) <- N_gender
  dim(U_2) <- N_gender
  dim(U_3) <- N_gender
  dim(UN_1) <- N_gender
  dim(UN_2) <- N_gender
  dim(UN_3) <- N_gender
  dim(US_1) <- N_gender
  dim(US_2) <- N_gender
  dim(US_3) <- N_gender
  dim(UR_1) <- N_gender
  dim(UR_2) <- N_gender
  dim(UR_3) <- N_gender

  dim(W) <- N_gender
  dim(W_1) <- N_gender
  dim(W_2) <- N_gender
  dim(W_3) <- N_gender
  dim(WN_1) <- N_gender
  dim(WN_2) <- N_gender
  dim(WN_3) <- N_gender
  dim(WS_1) <- N_gender
  dim(WS_2) <- N_gender
  dim(WS_3) <- N_gender
  dim(WR_1) <- N_gender
  dim(WR_2) <- N_gender
  dim(WR_3) <- N_gender

  dim(J) <- N_gender
  dim(JCL) <- N_gender
  dim(J_1) <- N_gender
  dim(J_2) <- N_gender
  dim(J_3) <- N_gender
  dim(JR) <- N_gender
  dim(JR_1) <- N_gender
  dim(JR_2) <- N_gender
  dim(JR_3) <- N_gender

  dim(JW_1) <- N_gender
  dim(JW_2) <- N_gender
  dim(JW_3) <- N_gender
  dim(JRW_1) <- N_gender
  dim(JRW_2) <- N_gender
  dim(JRW_3) <- N_gender

  dim(S) <- N_gender
  dim(S_1) <- N_gender
  dim(S_2) <- N_gender
  dim(S_3) <- N_gender
  dim(R) <- N_gender
  dim(R_1) <- N_gender
  dim(R_2) <- N_gender
  dim(R_3) <- N_gender

  dim(N) <- N_gender
  dim(N_1) <- N_gender
  dim(N_2) <- N_gender
  dim(N_3) <- N_gender

  #dim(COM) <- N_gender
  dim(COM_1) <- N_gender
  dim(COM_2) <- N_gender
  dim(COM_3) <- N_gender


  ##dim(N_1_ini) <- N_gender
  ##dim(N_2_ini) <- N_gender
  ##dim(N_3_ini) <- N_gender

  dim(UN_1_ini) <- N_gender
  dim(UN_2_ini) <- N_gender
  dim(UN_3_ini) <- N_gender
  dim(US_1_ini) <- N_gender
  dim(US_2_ini) <- N_gender
  dim(US_3_ini) <- N_gender
  dim(UR_1_ini) <- N_gender
  dim(UR_2_ini) <- N_gender
  dim(UR_3_ini) <- N_gender
  dim(WN_1_ini) <- N_gender
  dim(WN_2_ini) <- N_gender
  dim(WN_3_ini) <- N_gender
  dim(WS_1_ini) <- N_gender
  dim(WS_2_ini) <- N_gender
  dim(WS_3_ini) <- N_gender
  dim(WR_1_ini) <- N_gender
  dim(WR_2_ini) <- N_gender
  dim(WR_3_ini) <- N_gender
  dim(J_1_ini) <- N_gender
  dim(J_2_ini) <- N_gender
  dim(J_3_ini) <- N_gender
  dim(JR_1_ini) <- N_gender
  dim(JR_2_ini) <- N_gender
  dim(JR_3_ini) <- N_gender
  dim(JW_1_ini) <- N_gender
  dim(JW_2_ini) <- N_gender
  dim(JW_3_ini) <- N_gender
  dim(JRW_1_ini) <- N_gender
  dim(JRW_2_ini) <- N_gender
  dim(JRW_3_ini) <- N_gender


  ##dim(births_1) <- N_gender
  ##dim(births_2) <- N_gender
  ##dim(births_3) <- N_gender


  dim(recruit_1) <-N_gender
  dim(recruit_2) <-N_gender
  dim(recruit_3) <-N_gender
  dim(recruit_j) <-N_gender
  ##dim(h) <-N_gender
  dim(test_prev_CL) <- N_gender
  dim(test_custody_ratio) <- N_gender
  dim(test_total_fte) <- N_gender
  dim(test_total_rep) <- N_gender

  dim(II) <- N_gender
  dim(IE) <- N_gender
  dim(IC) <- N_gender
  dim(EI) <- N_gender
  dim(EE) <- N_gender
  dim(EC) <- N_gender
  dim(CI) <- N_gender
  dim(CE) <- N_gender
  dim(CC) <- N_gender

  dim(prop1_r) <- N_gender
  dim(prop2_r) <- N_gender
  dim(prop3_r) <- N_gender
  dim(cl_pc) <- N_gender #new & dodgy but goes here bc its mixing
  dim(x) <- c(N_gender, N_gender)
  dim(mix1_r) <- c(N_gender, N_gender)
  dim(mix2_r) <- c(N_gender, N_gender)
  dim(mix3_r) <- c(N_gender, N_gender)

  #pdimensions of the new dodgy stuff
  dim(age_in) <- N_gender
  dim(incl_in) <- N_gender
  dim(excl_in) <- N_gender
  dim(close_in) <- N_gender
  dim(surv_in_i) <- N_gender
  dim(surv_in_e) <- N_gender
  dim(surv_in_c) <- N_gender

  dim(fte1) <- N_gender
  dim(fte2) <- N_gender
  dim(fte3) <- N_gender
  dim(fte_1) <- N_gender
  dim(fte_cl_1) <- N_gender
  dim(fte_2) <- N_gender
  dim(fte_cl_2) <- N_gender
  dim(fte_3) <- N_gender
  dim(fte_cl_3) <- N_gender
  dim(custody_fte) <- N_gender

  dim(rep) <- N_gender
  dim(rep_1) <- N_gender
  dim(rep_cl_1) <- N_gender
  dim(rep_2) <- N_gender
  dim(rep_cl_2) <- N_gender
  dim(rep_3) <- N_gender
  dim(rep_cl_3) <- N_gender
  dim(custody_rep) <- N_gender

  dim(release_1) <- N_gender
  dim(release_cl_1) <- N_gender
  dim(release_2) <- N_gender
  dim(release_cl_2) <- N_gender
  dim(release_3) <- N_gender
  dim(release_cl_3) <- N_gender

  dim(missing_i) <- N_gender
  dim(missing_e) <- N_gender
  dim(missing_c) <- N_gender

  dim(ss_i) <- N_gender
  dim(ss_e) <- N_gender
  dim(ss_c) <- N_gender

  dim(school_ex_i) <- N_gender#rates of school exclusion, included
  dim(school_ex_e) <- N_gender #rates of school exclusion, excluded
  dim(ex_n) <- N_gender
  dim(ex_s) <- N_gender
  dim(ex_r) <- N_gender
  dim(ex_cl_n) <- N_gender
  dim(ex_cl_s) <- N_gender
  dim(ex_cl_r) <- N_gender

 dim(u1_out) <- N_gender
 dim(us1_out) <- N_gender
 dim(ur1_out) <- N_gender
 dim(w1_out) <- N_gender
 dim(ws1_out) <- N_gender
 dim(wr1_out) <- N_gender
 dim(j1_out) <- N_gender
 dim(jr1_out) <- N_gender
 dim(jw1_out) <- N_gender
 dim(jrw1_out) <- N_gender
 dim(u2_out) <- N_gender
 dim(us2_out) <- N_gender
 dim(ur2_out) <- N_gender
 dim(w2_out) <- N_gender
 dim(ws2_out) <- N_gender
 dim(wr2_out) <- N_gender
 dim(j2_out) <- N_gender
 dim(jr2_out) <- N_gender
 dim(jw2_out) <- N_gender
 dim(jrw2_out) <- N_gender
 dim(u3_out) <- N_gender
 dim(us3_out) <- N_gender
 dim(ur3_out) <- N_gender
 dim(w3_out) <- N_gender
 dim(ws3_out) <- N_gender
 dim(wr3_out) <- N_gender
 dim(j3_out) <- N_gender
 dim(jr3_out) <- N_gender
 dim(jw3_out) <- N_gender
 dim(jrw3_out) <- N_gender



  ##intermediate quantities

  N_1[] <- U_1[i] + US_1[i]  + UR_1[i] + W_1[i] + WS_1[i] + WR_1[i] + J_1[i] + JW_1[i] + JR_1[i] + JRW_1[i] ##total numbers in included class
  N_2[] <- U_2[i] + US_2[i]  + UR_2[i] + W_2[i] + WS_2[i] + WR_2[i] + J_2[i] + JW_2[i] + JR_2[i] + JRW_2[i]
  N_3[] <- U_3[i] + US_3[i]  + UR_3[i] + W_3[i] + WS_3[i] + WR_3[i] + J_3[i] + JW_3[i] + JR_3[i] + JRW_3[i]
  N[] <- N_1[i] + N_2[i] + N_3[i]

  U_1[] <- UN_1[i] + US_1[i] + UR_1[i]
  U_2[] <- UN_2[i] + US_2[i] + UR_2[i]
  U_3[] <- UN_3[i] + US_3[i] + UR_3[i]

  W_1[] <- WN_1[i] + WS_1[i] + WR_1[i]
  W_2[] <- WN_2[i] + WS_2[i] + WR_2[i]
  W_3[] <- WN_3[i] + WS_3[i] + WR_3[i]

  U[] <- U_1[i] + U_2[i] + U_3[i]
  W[] <- W_1[i] + W_2[i] + W_3[i]

  J[] <- J_1[i] + J_2[i] + J_3[i] + JW_1[i] + JW_2[i] + JW_3[i]
  JR[] <- JR_1[i] + JR_2[i] + JR_3[i] + JRW_1[i] + JRW_2[i] + JRW_3[i]
  JCL[] <- JW_1[i] + JW_2[i] + JW_3[i] + JRW_1[i] + JRW_2[i] + JRW_3[i]

  S[] <- US_1[i] + US_2[i] + US_3[i] + WS_1[i] + WS_2[i] + WS_3[i]##total numbers surveilled include both those working & not
  S_1[] <- US_1[i] + WS_1[i]
  S_2[] <- US_2[i] + WS_2[i]
  S_3[] <- US_3[i] + WS_3[i]

  R[] <- UR_1[i] + UR_2[i] + UR_3[i] + WR_1[i] + WR_2[i] + WR_3[i] ##total numbers under restrictions include both those working & not
  R_1[] <- UR_1[i] + WR_1[i]
  R_2[] <- UR_2[i] + WR_2[i]
  R_3[] <- UR_3[i] + WR_3[i]
  ##I[] <- U_1[i] + W_1[i] + H_1[i] + J_1[i] - considering a re-name - you would have to go change the mixing matrices
  ##E[] <- U_2[i] + W_2[i] + H_3[i] + J_2[i]
  ##C[] <- U_3[i] + W_3[i] + H_2[i] + J_3[i]

  ##currently in the community, to make mixing easier later
  COM_1[] <- N_1[i] - (J_1[i] + JR_1[i] + JW_1[i] + JRW_1[i])
  COM_2[] <- N_2[i] - (J_2[i] + JR_2[i] + JW_2[i] + JRW_2[i])
  COM_3[] <- N_3[i] - (J_3[i] + JR_3[i] + JW_2[i] + JRW_2[i])
  #COM[] <-  COM_1[i] + COM_2[i] + COM_3[i]


  #####################FIX NEEDED################
  #births_1[] <- mortality*N_1[i] + m*h[i]*W_1[i] #sum of all mortality <- births to maintain stable pop
  #births_2[] <- mortality*N_2[i] + m*h[i]*W_2[i]
  #births_3[] <- mortality*N_3[i] + m*h[i]*W_3[i]


  #####################DIFFERENTIAL EQUATIONS###############################################################
  ##############################################################################################################################

  #task
  #1. - does the charging process need a delay
  #2. whats the vibe with mortality

  #3. ADD FLOW BETWEEN CLASSES

  ##differential equations - INCLUSION#####################
  ##univolved
  deriv(UN_1[]) <- (1-surv_in_i[i])*incl_in[i]*age_in[i] + desist*WN_1[i] - (missing_i[i]+ss_i[i])*UN_1[i] - fte1[i]*UN_1[i] - contact*recruit_1[i]*UN_1[i]*sum(mix1_r[,i]) - UN_1[i]*(school_ex_i[i]+impov) - u1_out[i]
  deriv(US_1[]) <- surv_in_i[i]*incl_in[i]*age_in[i] + (missing_i[i]+ss_i[i])*UN_1[i] + desist*WS_1[i] - contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i]) - fte1[i]*surv_multiplier*US_1[i] - US_1[i]*(school_ex_i[i]+impov) - us1_out[i]
  deriv(UR_1[]) <- fte1[i]*(1-custody_fte[i])*(UN_1[i]+surv_multiplier*US_1[i]) + release_1[i] + desist*WR_1[i] - contact*recruit_1[i]*UR_1[i]*sum(mix1_r[,i]) - UR_1[i]*(school_ex_i[i]+impov) - ur1_out[i]
  ##working
  deriv(WN_1[]) <- contact*recruit_1[i]*UN_1[i]*sum(mix1_r[,i]) - (cl_multiplier*missing_i[i]+ss_i[i])*WN_1[i] - fte1[i]*WN_1[i] - desist*WN_1[i] - WN_1[i]*(school_ex_i[i]+impov) - w1_out[i]
  deriv(WS_1[]) <-contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i]) + (cl_multiplier*missing_i[i]+ss_i[i])*WN_1[i] - desist*WS_1[i] - fte1[i]*surv_multiplier*WS_1[i] - WS_1[i]*(school_ex_i[i]+impov) - ws1_out[i]
  deriv(WR_1[]) <- contact*recruit_1[i]*UR_1[i]*sum(mix1_r[,i]) + fte1[i]*(1-custody_fte[i])*(WN_1[i]+surv_multiplier*WS_1[i]) + release_cl_1[i] - desist*WR_1[i]  - WR_1[i]*(school_ex_i[i]+impov) - wr1_out[i]

  ##jailed (remand and sentenced)
  deriv(JR_1[]) <- remand*(fte_1[i] + rep_1[i]) - end_rem*JR_1[i] - contact*recruit_j[i]*cl_pc[i]*JR_1[i] - impov*JR_1[i] - jr1_out[i]
  deriv(JRW_1[]) <- remand*(fte_cl_1[i] + rep_cl_1[i]) + contact*recruit_j[i]*cl_pc[i]*JR_1[i] - end_rem*JRW_1[i] - impov*JRW_1[i] - jrw1_out[i]

  deriv(J_1[]) <- (1-remand)*(custody_fte[i]*fte_1[i] + custody_rep[i]*rep_1[i]) + r2c*end_rem*JR_1[i] + deter*JW_1[i] - end_cust*J_1[i] - contact*recruit_j[i]*cl_pc[i]*J_1[i] - impov*J_1[i] - j1_out[i]
  deriv(JW_1[]) <- (1-remand)*(custody_fte[i]*fte_cl_1[i] + custody_rep[i]*rep_cl_1[i]) + r2c*end_rem*JRW_1[i] + contact*recruit_j[i]*cl_pc[i]*J_1[i] - deter*JW_1[i] - end_cust*JW_1[i] - impov*JW_1[i] - jw1_out[i]


  ##differential equations - EXCLUSION#####################
  ##univolved
  deriv(UN_2[]) <- impov*UN_1[i] + (1-surv_in_e[i])*excl_in[i]*age_in[i] + desist*WN_2[i] - (missing_e[i]+ss_e[i])*UN_2[i] - fte2[i]*UN_2[i] - contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - u2_out[i]
  deriv(US_2[]) <- impov*US_1[i] + surv_in_e[i]*excl_in[i]*age_in[i] + (missing_e[i]+ss_e[i])*UN_2[i] + desist*WS_2[i] - contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i]) - fte2[i]*surv_multiplier*US_2[i] - us2_out[i]
  deriv(UR_2[]) <-impov* UR_1[i] + fte2[i]*(1-custody_fte[i])*(UN_2[i]+surv_multiplier*US_2[i]) + release_2[i] + desist*WR_2[i] - contact*recruit_2[i]*UR_2[i]*sum(mix2_r[,i]) - ur2_out[i]
  ##working
  deriv(WN_2[]) <-impov* WN_1[i] + contact*recruit_2[i]*UN_2[i]*sum(mix2_r[,i]) - fte2[i]*WN_2[i] - (cl_multiplier*missing_e[i]+ss_e[i])*WN_2[i] - desist*WN_2[i] - w2_out[i]
  deriv(WS_2[]) <- impov*WS_1[i] + contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i]) + (cl_multiplier*missing_e[i]+ss_e[i])*WN_2[i] - desist*WS_2[i] - fte2[i]*surv_multiplier*WS_2[i] - ws2_out[i]
  deriv(WR_2[]) <- impov*WR_1[i] + contact*recruit_2[i]*UR_2[i]*sum(mix2_r[,i]) + fte2[i]*(1-custody_fte[i])*(WN_2[i]+surv_multiplier*WS_2[i]) + release_cl_2[i] - desist*WR_2[i] - wr2_out[i]
  ##jailed (remand and sentenced)
  deriv(JR_2[]) <- impov*JR_1[i] + remand*(fte_2[i] + rep_2[i]) - end_rem*JR_2[i] - contact*recruit_j[i]*cl_pc[i]*JR_2[i] - jr2_out[i]
  deriv(JRW_2[]) <- impov*JRW_1[i] + remand*(fte_cl_2[i] + rep_cl_2[i]) + contact*recruit_j[i]*cl_pc[i]*JR_2[i] - end_rem*JRW_2[i] - jrw2_out[i]

  deriv(J_2[]) <- impov*J_1[i] + (1-remand)*(custody_fte[i]*fte_2[i] + custody_rep[i]*rep_2[i]) + r2c*end_rem*JR_2[i] + deter*JW_2[i] - end_cust*J_2[i] - contact*recruit_j[i]*cl_pc[i]*J_2[i] - j2_out[i]
  deriv(JW_2[]) <- impov*JW_1[i] + (1-remand)*(custody_fte[i]*fte_cl_2[i] + custody_rep[i]*rep_cl_2[i]) + r2c*end_rem*JRW_2[i] + contact*recruit_j[i]*cl_pc[i]*J_2[i] - deter*JW_2[i] - end_cust*JW_2[i] - jw2_out[i]

  ##differential equations - CLOSE PROXIMITY -#####################
  ##univolved
  deriv(UN_3[]) <- ex_n[i] + (1-surv_in_c[i])*close_in[i]*age_in[i] + desist*WN_3[i] - (missing_c[i]+ss_c[i])*UN_3[i] - fte3[i]*UN_3[i] - contact*recruit_3[i]*UN_3[i]*sum(mix3_r[,i]) - u3_out[i]
  deriv(US_3[]) <- ex_s[i] + surv_in_c[i]*close_in[i]*age_in[i] + (missing_c[i]+ss_c[i])*UN_3[i] + desist*WS_3[i] - contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i]) - fte3[i]*surv_multiplier*US_3[i] - us3_out[i]
  deriv(UR_3[]) <- ex_r[i] + fte3[i]*(1-custody_fte[i])*(UN_3[i]+surv_multiplier*US_3[i]) + release_3[i] + desist*WR_3[i] - contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) - ur3_out[i]
  ##working
  deriv(WN_3[]) <- ex_cl_n[i] + contact*recruit_3[i]*UN_3[i]*sum(mix3_r[,i]) - (cl_multiplier*missing_c[i]+ss_c[i])*WN_3[i] - fte3[i]*WN_3[i] - desist*WN_3[i] - w3_out[i]
  deriv(WS_3[]) <- ex_cl_s[i] + contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i]) + (cl_multiplier*missing_c[i]+ss_c[i])*WN_3[i] - desist*WS_3[i] - fte3[i]*surv_multiplier*WS_3[i] - ws3_out[i]
  deriv(WR_3[]) <- ex_cl_r[i] + contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) + fte3[i]*(1-custody_fte[i])*(WN_3[i]+surv_multiplier*WS_3[i]) + release_cl_3[i] - desist*WR_3[i] - wr3_out[i]
  ##jailed (remand and sentenced)
  deriv(JR_3[]) <- remand*(fte_3[i] + rep_3[i]) - end_rem*JR_3[i] - contact*recruit_j[i]*cl_pc[i]*JR_3[i] - jr3_out[i]
  deriv(JRW_3[]) <- remand*(fte_cl_3[i] + rep_cl_3[i]) + contact*recruit_j[i]*cl_pc[i]*JR_3[i] - end_rem*JRW_3[i] - jrw3_out[i]

  deriv(J_3[]) <- (1-remand)*(custody_fte[i]*fte_3[i] + custody_rep[i]*rep_3[i]) + r2c*end_rem*JR_3[i] + deter*JW_3[i] - end_cust*J_3[i] - contact*recruit_j[i]*cl_pc[i]*J_3[i] - j3_out[i]
  deriv(JW_3[]) <- (1-remand)*(custody_fte[i]*fte_cl_3[i] + custody_rep[i]*rep_cl_3[i]) + r2c*end_rem*JRW_3[i] + contact*recruit_j[i]*cl_pc[i]*J_3[i] - deter*JW_3[i] - end_cust*JW_3[i] - jw3_out[i]


  #making the mixing matrix help
  II[] <- N_1[i]/N[i] + (1-(N_1[i]/N[i]))/alpha
  IE[] <- N_2[i]/N[i]*(alpha-1)/alpha
  IC[] <- N_3[i]/N[i]*(alpha-1)/alpha

  EI[] <- N_1[i]/N[i]*(alpha-1)/alpha
  EE[] <- N_2[i]/N[i] + (1-(N_2[i]/N[i]))/alpha
  EC[] <- N_3[i]/N[i]*(alpha-1)/alpha

  CI[] <- N_1[i]/N[i]*(alpha-1)/alpha
  CE[] <- N_2[i]/N[i]*(alpha-1)/alpha
  CC[] <- N_3[i]/N[i] + (1-(N_3[i]/N[i]))/alpha

  #back to making the mixing matrix - note that no one outside of jail mixes with those inside
  prop1_r[] <- II[i]*(W_1[i]/COM_1[i]) + IE[i]*(W_2[i]/COM_2[i]) + IC[i]*(W_3[i]/COM_3[i])
  prop2_r[] <- EI[i]*(W_1[i]/COM_1[i]) + EE[i]*(W_2[i]/COM_2[i]) + EC[i]*(W_3[i]/COM_3[i])
  prop3_r[] <- CI[i]*(W_1[i]/COM_1[i]) + CE[i]*(W_2[i]/COM_2[i]) + CC[i]*(W_3[i]/COM_3[i])
  mix1_r[,] <- prop1_r[i]*x[j,i]
  mix2_r[,] <- prop2_r[i]*x[j,i]
  mix3_r[,] <- prop3_r[i]*x[j,i]

  #mixing in jail - note that no assortative mixing by class in jail at all - total random mixing wrt class
  #however, completely assortative wrt gender
  cl_pc[] <- JCL[i] / (J[i] + JR[i])

  #to make the ODEs a little more readable
  #total first time entrants (from no contact & surveilled, both working and not) for each class group
  #note to self - you should really probably have an extra chance of entering if you're actually in CL
  #maybe add another multiplier lol, change the name of crim_multiplier (too general) to surv_multiplier
  fte_1[] <- fte1[i]*(UN_1[i]+surv_multiplier*US_1[i])
  fte_cl_1[] <- cl_multiplier*fte1[i]*(WN_1[i]+surv_multiplier*WS_1[i])
  fte_2[] <- fte2[i]*(UN_2[i]+surv_multiplier*US_2[i])
  fte_cl_2[] <- cl_multiplier*fte2[i]*(WN_2[i]+surv_multiplier*WS_2[i])
  fte_3[] <- fte3[i]*(UN_3[i]+surv_multiplier*US_3[i])
  fte_cl_3[] <- cl_multiplier*fte3[i]*(WN_3[i]+surv_multiplier*WS_3[i])

  #total repeat offenses (from those already in the CSJ, both working and not) for each class group
  rep_1[] <- rep[i]*UR_1[i]
  rep_cl_1[] <- cl_multiplier*rep[i]*WR_1[i]
  rep_2[] <- rep[i]*UR_2[i]
  rep_cl_2[] <- cl_multiplier*rep[i]*WR_2[i]
  rep_3[] <- rep[i]*UR_3[i]
  rep_cl_3[] <- cl_multiplier*rep[i]*WR_3[i]

  #total released from custody (both sentences and remand) back into the CSJ group
  release_1[] <- (1-r2c)*end_rem*JR_1[i] + end_cust*J_1[i]
  release_cl_1[] <- (1-r2c)*end_rem*JRW_1[i] + end_cust*JW_1[i]
  release_2[] <- (1-r2c)*end_rem*JR_2[i] + end_cust*J_2[i]
  release_cl_2[] <- (1-r2c)*end_rem*JRW_2[i] + end_cust*JW_2[i]
  release_3[] <- (1-r2c)*end_rem*JR_3[i] + end_cust*J_3[i]
  release_cl_3[] <- (1-r2c)*end_rem*JRW_3[i] + end_cust*JW_3[i]

  #school exclusions
  ex_n[] <- UN_1[i]*school_ex_i[i] + UN_2[i]*school_ex_e[i]
  ex_s[] <- US_1[i]*school_ex_i[i] + US_2[i]*school_ex_e[i]
  ex_r[] <- UR_1[i]*school_ex_i[i] + UR_2[i]*school_ex_e[i]

  ex_cl_n[] <- cl_multiplier*(WN_1[i]*school_ex_i[i] + WN_2[i]*school_ex_e[i])
  ex_cl_s[] <- cl_multiplier*(WS_1[i]*school_ex_i[i] + WS_2[i]*school_ex_e[i])
  ex_cl_r[] <- cl_multiplier*(WR_1[i]*school_ex_i[i] + WR_2[i]*school_ex_e[i])


  #making stuff neater?
  incl_in[] <- 1 - excl_in[i] - close_in[i]

  #tests - replicates known/observable CL dynamcis/ policing/CJS attributes?
  #for example, 55% percent of cautions/sentences are of first time entrants to the CSJ
  #what about girl v boy dynamics...

  test_prev_CL[] <- W[i]/N[i]
  #remand should be about 20% of total population in custody
  test_custody_ratio[] <- JR[i]/(JR[i] + J[i])

  test_total_rep[] <- rep_1[i] + rep_cl_1[i] + rep_2[i] + rep_cl_2[i] + rep_3[i] + rep_cl_3[i]
  test_total_fte[] <- fte_1[i] + fte_cl_1[i] + fte_2[i] + fte_cl_2[i] + fte_3[i] + fte_cl_3[i]


  ##generated quantities
  output(test_prev_CL) <- test_prev_CL
  output(test_custody_ratio) <- test_custody_ratio
  output(test_total_rep) <- test_total_rep
  output(test_total_fte) <- test_total_fte
  output(U) <- U
  output(U_1) <- U_1
  output(U_2) <- U_2
  output(U_3) <- U_3
  output(W) <- W
  output(W_1) <- W_1
  output(W_2) <- W_2
  output(W_3) <- W_3
  output(JR) <- JR
  output(J) <- J
  output(S) <- S
  output(R) <- R
  output(S_1) <- S_1
  output(R_1) <- R_1
  output(S_2) <- S_2
  output(R_2) <- R_2
  output(S_3) <- S_3
  output(R_3) <- R_3



}, target='r')        #or c

#with this parameter set and the structure of the model, cusotdy ratio works alright
pars <- list(x = rbind(c(0.8, 0.2), #first row is group 1's mixing group 1,2,3,4,5
                       c(0.2, 0.8)),
             alpha = 1.25,
             UN_1_ini = c(41042,37884), #number initially uninvolved, no police contact
             UN_2_ini = c(17679,16319), #number initially uninvolved, no police contact - 93.6% of the excluded
             UN_3_ini = c(0,0), #number initially uninvolved, no police contact - zero for close, all already flagged
             US_1_ini = c(0,0), #number initially uninvolved, surveilled - zero for included
             US_2_ini = c(1263,1165), #number initially uninvolved, surveilled - 6.6% of the excluded
             US_3_ini = c(4249,1821), #number initially uninvolved, surveilled - 100% of the close
             UR_1_ini = c(4,2), #number initially uninvolved, in CSJ - lets say 800 start in the CSJ (i just picked), and i split between 3
             UR_2_ini = c(356,39), #number initially uninvolved, in CSJ
             UR_3_ini = c(360,40), #number initially uninvolved, in CSJ
             WN_1_ini = c(0,0), #number initially working, no police contact
             WN_2_ini = c(0,0), #number initially working, no police contact
             WN_3_ini = c(0,0), #number initially working, no police contact
             WS_1_ini = c(0,0), #number initially working, surveilled
             WS_2_ini = c(1,0), #number initially working, surveilled
             WS_3_ini = c(5,1), #number initially working, surveilled -lets start small working & hope it takes off?
             WR_1_ini = c(0,0), #number initially working, in CSJ
             WR_2_ini = c(1,0), #number initially working, in CSJ
             WR_3_ini = c(1,0), #number initially working, in CSJ
             J_1_ini = c(1,0), #number initially on custodial sentence
             J_2_ini = c(7,0), #number initially on custodial sentence
             J_3_ini = c(25,5), #number initially on custodial sentence - prev of custodial sentence is 38 so, just split it
             JW_1_ini = c(0,0), #number of CL initially on custodial sentence
             JW_2_ini = c(0,0), #number of CL initially on custodial sentence
             JW_3_ini = c(1,0), #number of CL initially on custodial sentence
             JR_1_ini = c(0,0), #number initially remanded to custody - none
             JR_2_ini = c(4,0), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JR_3_ini = c(3,1.5), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JRW_1_ini = c(0,0), #number of CL initially remanded to custody
             JRW_2_ini = c(0,0), #number of CL initially remanded to custody -
             JRW_3_ini = c(0,0), #number of CL initially remanded to custody -
             age_in = c(0,0), #boys, girls  coming in weekly - should be set to [151, 141]
             excl_in = c(0.29,0.19), #percent boys, girls  coming in going to excluded
             close_in = c(0.01,0.01), #percent boys, girls  coming in going to close
             surv_in_i = c(0,0), #pc of boys, girls  in INCLUDED that are surveilled at 10
             surv_in_e = c(0.07,0.07), #pc of gboys, girls  in EXCLUDED that are surveilled at 10
             surv_in_c = c(0.6,0.5), #pc of boys, girls in CLOSE that are surveilled at 10
             missing_i = c(0,0), #rate of missing episodes for the included group
             missing_e = c(0.003,0.003), #rate of missing episodes for the excluded group
             missing_c = c(0.03,0.03), #rate of missing episodes for the close group
             ss_i = c(0.00003529546,0.000002799211), #rate of stop&search for the included group
             ss_e = c(0.001087095,0.00009400705), #rate of stop&search  for the excluded group
             ss_c = c(0.003954082,0.0009812287), #rate of stop&search  for the close group
             recruit_1 = c(0.0005,0.0001),
             recruit_2 = c(0.001,0.0009),
             recruit_3 = c(0.005,0.003),
             recruit_j = c(0.005,0.003),
             custody_fte = c(0.015,0.003), #percentage of fte's that get a custodial sentence (of men, 1.5% - of women, 0.3%)
             custody_rep = c(0.14,0.04), #percentage of repeat offenders that get a custodial sentence (that weren't remanded to custody)
             fte1 = c(0.00001088204,0.000003463907), #some kind of rate of included first time entrants
             fte2 = c(0.0003351652,0.0001170569), #some kind of rate of excluded first time entrants
             fte3 = c(0.001219093,0.001214229), #some kind of rate of close first time entrants total (should work out as like 600 a year, 23% female 77% male)
             rep = c(0.002,0.001), #some kind of rate of repeated offences (should work out as like 452
             school_ex_i = c(0.0009615385,0.0004807692), #rc(0,0),#ates of school exclusion, included
             school_ex_e = c(0.005769231,0.001923077), #c(0,0),
             impov = 0.003, # 0.0003205128 pretty steep impoverishment rate of like 1 pc of pop a year or something like that?
             u1_out = c(0,0),  #percentage of exiters leaving from this state, boy and girl
             us1_out = c(0,0),
             ur1_out = c(0,0),
             w1_out = c(0,0),
             ws1_out = c(0,0),
             wr1_out = c(0,0),
             j1_out = c(0,0),
             jr1_out = c(0,0),
             jw1_out = c(0,0),
             jrw1_out = c(0,0),
             u2_out = c(0,0),
             us2_out = c(0,0),
             ur2_out = c(0,0),
             w2_out = c(0,0),
             ws2_out = c(0,0),
             wr2_out = c(0,0),
             j2_out = c(0,0),
             jr2_out = c(0,0),
             jw2_out = c(0,0),
             jrw2_out = c(0,0),
             u3_out = c(0,0),
             us3_out = c(0,0),
             ur3_out = c(0,0),
             w3_out = c(0,0),
             ws3_out = c(0,0),
             wr3_out = c(0,0),
             j3_out = c(0,0),
             jr3_out = c(0,0),
             jw3_out = c(0,0),
             jrw3_out = c(0,0)
    )





mod <- child_recr(user = pars)
t <- seq(0, 520, length.out = 1040)  #from 0 to 10 years, two steps a week or something
CL_data <- as.data.frame(mod$run(t))


U_data <- data.frame(CL_data[,grep('U',colnames(CL_data))])
W_data <- data.frame(CL_data[,grep('W',colnames(CL_data))])
J_data <- data.frame(CL_data[,grep('J',colnames(CL_data))])
R_data <- data.frame(CL_data[,grep('R',colnames(CL_data))])
S_data <- data.frame(CL_data[,grep('S',colnames(CL_data))])

incl_data <- data.frame(CL_data[,grep('_1',colnames(CL_data))])
excl_data <- data.frame(CL_data[,grep('_2',colnames(CL_data))])
close_data <- data.frame(CL_data[,grep('_3',colnames(CL_data))])

prev_CL_data <- data.frame(CL_data[,grep('prev_CL',colnames(CL_data))])
custody_ratio_data <- data.frame(CL_data[,grep('custody_ratio',colnames(CL_data))])
total_rep_data <- data.frame(CL_data[,grep('total_rep',colnames(CL_data))])
total_fte_data <- data.frame(CL_data[,grep('total_fte',colnames(CL_data))])

ggplot(data = incl_data) +
  geom_line(mapping = aes(x=t, y=U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_1.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_1.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_1.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_1.1.+JR_1.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_1.2.+JR_1.2.), color = "pink")

ggplot(data = excl_data) +
  geom_line(mapping = aes(x=t, y=U_2.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_2.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_2.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_2.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_2.1.+JR_2.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_2.2.+JR_2.2.), color = "pink")

ggplot(data = close_data) +
  geom_line(mapping = aes(x=t, y=U_3.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_3.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_3.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_3.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_3.1.+JR_3.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_3.2.+JR_3.2.), color = "pink")




ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=U_1.2.), color = "blue") +
geom_line(mapping = aes(x=t, y=U_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=U_2.2.), color = "green") +
geom_line(mapping = aes(x=t, y=U_3.1.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=U_3.2.), color = "purple")

ggplot(data = W_data) +
  geom_line(mapping = aes(x=t, y=W_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_1.2.), color = "blue") +
  geom_line(mapping = aes(x=t, y=W_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_2.2.), color = "green") +
  geom_line(mapping = aes(x=t, y=W_3.1.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=W_3.2.), color = "purple")

ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=U_1.2.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=U_2.2.), color = "green") +
  geom_line(mapping = aes(x=t, y=U_3.1.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=U_3.2.), color = "purple")

ggplot(data = S_data) +
  geom_line(mapping = aes(x=t, y=S_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=S_1.2.), color = "blue") +
  geom_line(mapping = aes(x=t, y=S_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=S_2.2.), color = "green") +
  geom_line(mapping = aes(x=t, y=S_3.1.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=S_3.2.), color = "purple")

ggplot(data = J_data) +
  geom_line(mapping = aes(x=t, y=J_1.1.+JR_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=J_1.2.+JR_1.2.), color = "blue") +
  geom_line(mapping = aes(x=t, y=J_2.1.+JR_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=J_2.2.+JR_2.2.), color = "green") +
  geom_line(mapping = aes(x=t, J_3.1.+JR_3.2.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=J_3.2.+JR_3.2.), color = "purple")

ggplot(data = R_data) +
  geom_line(mapping = aes(x=t, y=R_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=R_1.2.), color = "blue") +
  geom_line(mapping = aes(x=t, y=R_2.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=R_2.2.), color = "green") +
  geom_line(mapping = aes(x=t, y=R_3.1.), color = "yellow") +
  geom_line(mapping = aes(x=t, y=R_3.2.), color = "purple")



ggplot(data = total_fte_data) +
  geom_line(mapping = aes(x=t, y=test_total_fte.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=test_total_fte.2.), color = "blue")

ggplot(data = prev_CL_data) +
  geom_line(mapping = aes(x=t, y=test_prev_CL.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=test_prev_CL.2.), color = "blue")


ggplot(data = J_data) +
  geom_line(mapping = aes(x=t, y=J.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=JR.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=J.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=JR.2.), color = "green")

ggplot(data = S_data) +
  geom_line(mapping = aes(x=t, y=S.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=S.2.), color = "blue")

ggplot(data = R_data) +
  geom_line(mapping = aes(x=t, y=R.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=R.2.), color = "blue")

ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=U.2.), color = "blue")




ggplot(data = custody_ratio_data) +
  geom_line(mapping = aes(x=t, y=test_custody_ratio.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=test_custody_ratio.2.), color = "blue")



ggplot(data = incl_data) +
  geom_line(mapping = aes(x=t, y=U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_1.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_1.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_1.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_1.1.+JR_1.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_1.2.+JR_1.2.), color = "pink")

ggplot(data = excl_data) +
  geom_line(mapping = aes(x=t, y=U_2.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_2.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_2.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_2.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_2.1.+JR_2.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_2.2.+JR_2.2.), color = "pink")

ggplot(data = close_data) +
  geom_line(mapping = aes(x=t, y=U_3.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_3.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_3.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_3.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=J_3.1.+JR_3.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=J_3.2.+JR_3.2.), color = "pink")


ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U_data$U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=U_data$U_1.2.), color = "blue")

ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U_data$U_2.1.), color = "green") +
  geom_line(mapping = aes(x=t, y=U_data$U_2.2.), color = "black")

ggplot(data = U_data) +
  geom_line(mapping = aes(x=t, y=U_data$U_3.1.), color = "orange") +
  geom_line(mapping = aes(x=t, y=U_data$U_3.2.), color = "pink")










CL_DF  <- data.frame(CL_data) %>%
  tidyr::gather(outpt, value, 7:9)


ggplot(data = CL_DF, aes(x=t, y = value, group = outpt, colour = outpt)) + geom_line() + scale_y_continuous(labels = scales::percent)


