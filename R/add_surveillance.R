#install.packages("odin")
library(odin)
library(ggplot2)

child_recr = odin::odin({
  ##two mixing matrices - one for recruitment thats strongly assortative
  ##and the other for harm that's very close to random
  ##the mixing part for recruitment goes over N because its about the total population whereas
  ##the one for harm goes over (w+H) because its only about those

  ##actually the two different splits - leading to a 3 dimensional array - i dont think that would work


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
  u2_out[] <- user(0)
  us2_out[] <- user(0)
  ur2_out[] <- user(0)
  w2_out[] <- user(0)
  ws2_out[] <- user(0)
  wr2_out[] <- user(0)
  j2_out[] <- user(0)
  jr2_out[] <- user(0)
  u3_out[] <- user(0)
  us3_out[] <- user(0)
  ur3_out[] <- user(0)
  w3_out[] <- user(0)
  ws3_out[] <- user(0)
  wr3_out[] <- user(0)
  j3_out[] <- user(0)
  jr3_out[] <- user(0)

  ## parameters values - default in parentheses - timestep is weekly? lets try weekly
  alpha <- user(1) #changes degree of assortativity in mixing across social classes (1 -> infinity = totally assortative -> totally proportional)
  x[,] <- user(0)


  contact <- user(70) #contact rate per week - ten a day for 7 days?

  recruit_1[] <- user(0.001) #recruitment per contact? you need 1000 contacts
  recruit_2[] <- user(0.002) #recruitment per contact? you need 500 contacts
  recruit_3[] <- user(0.005) #recruitment per contact? you need 200 contacts|?

  d <- user(0.05) #probability of desisting at a critical point eg hospitalisation after an injury
  desist <- user(0.005) #probability of desisting at other times

  arrest <- user(0.02) #rate of arrest bc of county lines involvement for the working group
  #lets say i want them working about 1 year, so 52 time steps, so 1/52


  surv_rate <- user(0.002) #10pc are criminalised a year

  release <- user(0.01) #rate of release from youth offender institutes (avg sentence length 18 months  https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/956621/youth-justice-statistics-2019-2020.pdf)
  red <- user(0.637) #recidivism rate - For children released from custody in the year ending March 2020, 63.7%  re-offended
  mortality <- user(0) #death rate from causes other than county lines
  h[] <- user(0.25) #rate at which people in harm group commit harm... once a monthish?
  m <- user(0.01) #percentage of harms that are fatal (the better letter would prob be f)...1%?



  remand <- user(0.12) #percentage of bail episode that are remand to custody
  r2c <- user(0.33) #percentage of custody remands that result in a custodial sentence
  rem_length <- user() #rate that sets the length of the average custody remand ()?
  #should rem length be called rem_release (in contrast to cust_release or soemthing)


  fte[] <- user()#rsome kind of rate of first time entrants (should work out as like 600 a year, 23% female 77% male)
  crim_multiplier <- user() #figure this out properly but its some sort of elevated risk of criminalisation that you get from beign already known to the police

  custody_fte[] <- user() #percentage of fte's that get a custodial sentence (of men, 1.5% - of women, 0.3%)
  custody_rep[] <- user() #percentage that get custodial sentence, of repeat offenders, who dont get remanded to custody... lol


  ##initial conditions
  #the old equal split for gender doesnt work bc girls are less likely to be close
  #do we have to do that equation bit or can everything just be explicitly set?
  #dN_1_ini[] <- user(0) #total initial pop included
  #dN_2_ini[] <- user(0) #total initial pop excluded
  #d N_3_ini[] <- user(0) #total initial pop close
  U_1_ini[] <- user(78927) #number initially uninvolved, no police contact
  U_2_ini[] <- user(33999) #number initially uninvolved, no police contact - 93.6% of the excluded
  U_3_ini[] <- user(0) #number initially uninvolved, no police contact - zero for close, all already flagged
  US_1_ini[] <- user(0) #number initially uninvolved, surveilled - zero for included
  US_2_ini[] <- user(2429) #number initially uninvolved, surveilled - 6.6% of the excluded
  US_3_ini[] <- user(6071) #number initially uninvolved, surveilled - 100% of the close
  UR_1_ini[] <- user(4) #number initially uninvolved, in CSJ - lets say 800 start in the CSJ (i just picked), and i split between 3
  UR_2_ini[] <- user(396) #number initially uninvolved, in CSJ
  UR_3_ini[] <- user(400) #number initially uninvolved, in CSJ
  W_1_ini[] <- user(0) #number initially working, no police contact
  W_2_ini[] <- user(0) #number initially working, no police contact
  W_3_ini[] <- user(0) #number initially working, no police contact
  WS_1_ini[] <- user(0) #number initially working, surveilled
  WS_2_ini[] <- user(1) #number initially working, surveilled
  WS_3_ini[] <- user(5) #number initially working, surveilled -lets start small working & hope it takes off?
  WR_1_ini[] <- user(0) #number initially working, in CSJ
  WR_2_ini[] <- user(2) #number initially working, in CSJ
  WR_3_ini[] <- user(2) #number initially working, in CSJ
  J_1_ini[] <- user(1) #number initially on custodial sentence
  J_2_ini[] <- user(37) #number initially on custodial sentence
  J_3_ini[] <- user(30) #number initially on custodial sentence - prev of custodial sentence is 38 so, just split it
  JR_1_ini[] <- user(0) #number initially remanded to custody - none
  JR_2_ini[] <- user(4) #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
  JR_3_ini[] <- user(5.5) #number initially remanded to custody - prevalence of remand is 9.5 so just split between these

  initial(U_1[]) <- U_1_ini[i] #the old equal split for gender doesnt work bc girls are less likely to be close
  initial(U_2[]) <- U_2_ini[i]
  initial(U_3[]) <- U_3_ini[i]
  initial(US_1[]) <- US_1_ini[i]
  initial(US_2[]) <- US_2_ini[i]
  initial(US_3[]) <- US_3_ini[i]
  initial(UR_1[]) <- UR_1_ini[i]
  initial(UR_2[]) <- UR_2_ini[i]
  initial(UR_3[]) <- UR_3_ini[i]

  initial(W_1[]) <- W_1_ini[i]
  initial(W_2[]) <- W_2_ini[i]
  initial(W_3[]) <- W_3_ini[i]
  initial(WS_1[]) <- WS_1_ini[i]
  initial(WS_2[]) <- WS_2_ini[i]
  initial(WS_3[]) <- WS_3_ini[i]
  initial(WR_1[]) <- WR_1_ini[i]
  initial(WR_2[]) <- WR_2_ini[i]
  initial(WR_3[]) <- WR_3_ini[i]

  initial(J_1[]) <- J_1_ini[i]
  initial(J_2[]) <- J_2_ini[i]
  initial(J_3[]) <- J_3_ini[i]
  initial(JR_1[]) <- JCL_1_ini[i]
  initial(JR_2[]) <- JCL_2_ini[i]
  initial(JR_3[]) <- JCL_3_ini[i]

  ## setting dinemsions for everything
  dim(U) <- N_gender
  dim(U_1) <- N_gender
  dim(U_2) <- N_gender
  dim(U_3) <- N_gender
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
  dim(WS_1) <- N_gender
  dim(WS_2) <- N_gender
  dim(WS_3) <- N_gender
  dim(WR_1) <- N_gender
  dim(WR_2) <- N_gender
  dim(WR_3) <- N_gender

  dim(J) <- N_gender
  dim(J_1) <- N_gender
  dim(J_2) <- N_gender
  dim(J_3) <- N_gender
  dim(JR) <- N_gender
  dim(JR_1) <- N_gender
  dim(JR_2) <- N_gender
  dim(JR_3) <- N_gender

  dim(S) <- N_gender
  dim(R) <- N_gender

  dim(N) <- N_gender
  dim(N_1) <- N_gender
  dim(N_2) <- N_gender
  dim(N_3) <- N_gender

  dim(COM) <- N_gender
  dim(COM_1) <- N_gender
  dim(COM_2) <- N_gender
  dim(COM_3) <- N_gender


  ##dim(N_1_ini) <- N_gender
  ##dim(N_2_ini) <- N_gender
  ##dim(N_3_ini) <- N_gender

  dim(U_1_ini) <- N_gender
  dim(U_2_ini) <- N_gender
  dim(U_3_ini) <- N_gender
  dim(US_1_ini) <- N_gender
  dim(US_2_ini) <- N_gender
  dim(US_3_ini) <- N_gender
  dim(UR_1_ini) <- N_gender
  dim(UR_2_ini) <- N_gender
  dim(UR_3_ini) <- N_gender
  dim(W_1_ini) <- N_gender
  dim(W_2_ini) <- N_gender
  dim(W_3_ini) <- N_gender
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

  ##dim(births_1) <- N_gender
  ##dim(births_2) <- N_gender
  ##dim(births_3) <- N_gender


  dim(recruit_1) <-N_gender
  dim(recruit_2) <-N_gender
  dim(recruit_3) <-N_gender
  dim(h) <-N_gender
  dim(test_prev_CL) <- N_gender
  dim(test_incarcerated) <- N_gender

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
  dim(prop_h) <- N_gender
  dim(x) <- c(N_gender, N_gender)
  dim(y) <- c(N_gender, N_gender)
  dim(mix1_r) <- c(N_gender, N_gender)
  dim(mix2_r) <- c(N_gender, N_gender)
  dim(mix3_r) <- c(N_gender, N_gender)
  dim(mix_h)<- c(N_gender, N_gender)


  #pdimensions of the new dodgy stuff
  dim(age_in) <- N_gender
  dim( excl_in) <- N_gender
  dim(close_in) <- N_gender
  dim(surv_in_i) <- N_gender
  dim(surv_in_e) <- N_gender
  dim(surv_in_c) <- N_gender

 dim(u1_out) <- N_gender
 dim( us1_out) <- N_gender
 dim(ur1_out) <- N_gender
 dim(w1_out) <- N_gender
 dim(ws1_out) <- N_gender
 dim(wr1_out) <- N_gender
 dim(j1_out) <- N_gender
 dim(jr1_out) <- N_gender
 dim(u2_out) <- N_gender
 dim(us2_out) <- N_gender
 dim(ur2_out) <- N_gender
 dim(w2_out) <- N_gender
 dim(ws2_out) <- N_gender
 dim(wr2_out) <- N_gender
 dim(j2_out) <- N_gender
 dim(jr2_out) <- N_gender
 dim(u3_out) <- N_gender
 dim(us3_out) <- N_gender
 dim(ur3_out) <- N_gender
 dim(w3_out) <- N_gender
 dim(ws3_out) <- N_gender
 dim(wr3_out) <- N_gender
 dim(j3_out) <- N_gender
 dim(jr3_out) <- N_gender



  ##intermediate quantities

  N_1[] <- U_1[i] + US_1[i]  + UR_1[i] + W_1[i] + WS_1[i] + WR_1[i] + J_1[i] + JR_1[i] ##total numbers in included class
  N_2[] <- U_2[i] + US_2[i]  + UR_2[i] + W_2[i] + WS_2[i] + WR_2[i] + J_2[i] + JR_2[i]
  N_3[] <- U_3[i] + US_3[i]  + UR_3[i] + W_3[i] + WS_3[i] + WR_3[i] + J_3[i] + JR_3[i]
  N[] <- N_1[i] + N_2[i] + N_3[i]

  U_1[] <- UN_1[i] + US_1[i] + UR_1[i]
  U_2[] <- UN_2[i] + US_2[i] + UR_2[i]
  U_3[] <- UN_3[i] + US_3[i] + UR_3[i]

  W_1[] <- WN_1[i] + WS_1[i] + WR_1[i]
  W_2[] <- WN_2[i] + WS_2[i] + WR_2[i]
  W_3[] <- WN_3[i] + WS_3[i] + WR_3[i]

  U[] <- U_1[i] + U_2[i] + U_3[i]
  W[] <- W_1[i] + W_2[i] + W_3[i]


  J[] <- J_1[i] + J_2[i] + J_3[i]
  JR[] <- JR_1[i] + JR_2[i] + JR_3[i]
  S[] <- US_1[i] + US_2[i] + US_3[i] + WS_1[i] + WS_2[i] + WS_3[i] ##total numbers surveilled include both those working & not
  R[] <- UR_1[i] + UR_2[i] + UR_3[i] + WR_1[i] + WR_2[i] + WR_3[i] ##total numbers under restrictions include both those working & not

  ##I[] <- U_1[i] + W_1[i] + H_1[i] + J_1[i] - considering a re-name - you would have to go change the mixing matrices
  ##E[] <- U_2[i] + W_2[i] + H_3[i] + J_2[i]
  ##C[] <- U_3[i] + W_3[i] + H_2[i] + J_3[i]

  ##currently in the community, to make mixing easier later
  COM_1[] <- N_1[i] - (J_1[i] + JR_1[i])
  COM_2[] <- N_2[i] - (J_2[i] + JR_2[i])
  COM_3[] <- N_3[i] - (J_3[i] + JR_3[i])
  COM[] <-  COM_1[i] + COM_2[i] + COM_3[i]

  incl_in[] <- 1 - excl_in[i] - close_in[i]

  #####################FIX NEEDED################
  #births_1[] <- mortality*N_1[i] + m*h[i]*W_1[i] #sum of all mortality <- births to maintain stable pop
  #births_2[] <- mortality*N_2[i] + m*h[i]*W_2[i]
  #births_3[] <- mortality*N_3[i] + m*h[i]*W_3[i]


  ##will i say only involved kids go missing? like that's how police notice kids?
  ##so we've got TriED and then sen_cust (90 kids in birmingham per year) and CHARGED and then rem_cust
  ##and we've got TriED and then sen_rest (880 kids per year) and CHARGED and then rem_rest

  #####################DIFFERENTIAL EQUATIONS###############################################################
  ##############################################################################################################################
  #TASKS
  #1. FIX RECRUITMENT

  #2. FIX JAIL MIXING - so no one out of jail mixes with those in - DONE - second is set up recruitment in jail
  #3. MAKE FLOWS BETWEEN CLASS STATES


  ##differential equations - INCLUSION#####################
  ##univolved
  deriv(UN_1[]) <- (1-surv_in_i[i])*incl_in[i]*age_in[i] + desist*W_1[i] + d*h[i]*W_1[i] - fte*UN_1 - surveil_i*UN_1[1] - contact*recruit_1[i]*UN_1[i]*sum(mix1_r[,i]) - mortality*UN_1[i] - u1_out[i]
  deriv(US_1[]) <- surv_in_i[i]*incl_in[i]*age_in[i] + surveil_i*UN_1[i] + (1-ss_arrest)*ss_i[i]*UN_1[i] + (1-red)*release*JCL_1[i] - contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i]) - fte*crim_multiplier*US_1 - us1_out[i]
  deriv(UR_1[]) <- fte*(1-custody_fte)*(UN_1+crim_multiplier*US_1) + ss_arrest*ss_i[i]* + arrest_i[i]*US_1[i]- contact*recruit_1[i]*UR_1[i]*sum(mix1_r[,i]) - ur1_out[i]
  ##working
  deriv(WN_1[]) <- contact*recruit_1[i]*UN_1[i]*sum(mix1_r[,i]) - surveil_i*WN_1[i] - fte*WN_2 - arrest*WN_1[i] - desist*WN_1[i] - mortality*WN_1[i] - missing*WN_1[i] - w1_out[i]
  deriv(WS_1[]) <- surveil_i*WN_1[i] + contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i]) + missing*WN_1[i] + arrest_i[i]*WN_1[i] + red*release*JCL_1[i] - fte*crim_multiplier*WS_1 - ws1_out[i]
  deriv(WR_1[]) <- contact*recruit_1[i]*UR_1[i]*sum(mix1_r[,i]) + fte*(1-custody_fte)*(WN_1+crim_multiplier*WS_1) - wr1_out[i]


  ##going_in: custody_fte*(1-remand)*fte_1[i]  remand*fte_1[i],   remand*rep_1[i]  custody_rep*(1-remand)*rep_1[i]
  ##you get charged - if you're already in the CSJ then theres this bit where the people who get remanded
  ##flow out instantly and the people who dont flow out like a month or so later (if they get a custodial sentence)
  ##is this a delay differential equation lol (might be)



  coming out: (1-r2c)*rem_length*JR_1[i]      release*J_1[i]
  where do these go - theyve been in jail, - they could have been recruited....


  ##UPDATEABLE (how to make it updateable) - lilke the current) percentage of CL people in jail...
   cl_pc[i] <- (WN_1[i]+WS_1[i]+WR_1[i])/(UN_1[i]+WN_1[i]+US_1[i]+WS_1[i]+WR_1[i]+UR_1[i])

   contact*recruit_j[i]*J[i]*sum(mix_j[,i])

   mix_j[,i] is obviously nothing at all (no girls and boys in same prison)

  ##jailed (remand and sentenced)
  deriv(JR_1[]) <- remand*(fte_1[i] + rep_1[i]) - rem_length*JR_1[i] - jr1_out[i]
  deriv(J_1[]) <- (1-remand)*(custody_fte*fte_1[i] + custody_rep*rep_1[i]) + r2c*rem_length*JR_1[i] - release*J_1[i] - mortality*JCL_1[i] - j1_out[i]

  ##differential equations - EXCLUSION#####################
  ##univolved
  deriv(UN_2[]) <- (1-surv_in_e[i])*excl_in[i]*age_in[i] + desist*W_2[i] + d*h[i]*W_2[i] - fte*UN_2 - surveil_e*U_2[1] - contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - mortality*U_2[i] - u2_out[i]
  deriv(US_2[]) <- surv_in_e[i]*excl_in[i]*age_in[i] +surveil_e*U_2[i] + ss_e[i]*U_2[i] + arrest_e[i]*U_2[i] + (1-red)*release*JCL_2[i] - contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i]) - fte*crim_multiplier*US_2 - us2_out[i]
  deriv(UR_2[]) <- fte*(1-custody_fte)*(UN_2+crim_multiplier*US_2)- contact*recruit_2[i]*UW_2[i]*sum(mix2_r[,i]) - ur2_out[]
  ##working
  deriv(WN_2[]) <- contact*recruit_2[i]*UN_2[i]*sum(mix2_r[,i]) - surveil_e*W_2[i] - fte*WN_2 - arrest*WN_2[i] - desist*WN_2[i] - missing*WN_2[i] - mortality*WN_2[i] - w2_out[i]
  deriv(WS_2[]) <- surveil_e*WN_2[i] + contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i]) + missing*WN_2[i] + arrest_e[i]*WN_2[i] + red*release*JCL_2[i] - fte*crim_multiplier*WS_2 - ws2_out[i]
  deriv(WR_2[]) <- contact*recruit_2[i]*UR_2[i]*sum(mix2_r[,i]) + fte*(1-custody_fte)*(WN_2+crim_multiplier*WS_2) - wr2_out[i]
  ##jailed (remand and sentenced)
  deriv(JR_2[]) <- remand*(fte_2[i] + rep_2[i]) - rem_length*JR_2[i] - jr2_out[i]
  deriv(J_2[]) <- (1-remand)*(custody_fte*fte_2[i] + custody_rep*rep_2[i]) + r2c*rem_length*JR_2[i] - release*JCL_2[i] - mortality*JCL_2[i] - j2_out[i]

  ##differential equations - CLOSE PROXIMITY -#####################
  ##NOTE TO SELF - I AM WORKING EVERYTHIGN OUT ON CLSOE PROXIMITY FIRST!! THEN I WILL FIX!! ## ## ## ## ## ## ##
  ##univolved
  deriv(UN_3[]) <- (1-surv_in_c[i])*close_in[i]*age_in[i] + desist*WN_3[i] + d*h[i]*WN_3[i] - surveil_c*UN_3[i] - fte*UN_3 - contact*recruit_3[i]*UN_3[i]*sum(mix3_r[,i]) - mortality*UN_3[i] - u3_out[i]
  deriv(US_3[]) <- surv_in_c[i]*close_in[i]*age_in[i] + surveil_c*U_3[i] + arrest_c[i]*UN_3[i] + (1-red)*release*JCL_3[i] - contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i]) - fte*crim_multiplier*US_3 - us3_out[i]
  deriv(UR_3[]) <- fte*(1-custody_fte)*(UN_3+crim_multiplier*US_3) - contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) - ur3_out[i]
  ##working
  deriv(WN_3[]) <- contact*recruit_3[i]*UN_3[i]*sum(mix3_r[,i]) - surveil_c*WN_3[i] - arrest*WN_3[i] - fte*WN_3 - desist*WN_3[i] - missing*WN_3[i] - mortality*WN_3[i] - w3_out[i]
  deriv(WS_3[]) <- surveil_c*WN_3[i] + contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i]) + missing*WN_3[i] + arrest_c[i]*WN_3[i] + red*release*JCL_3[i] - fte*crim_multiplier*WS_3 - ws3_out[i]
  deriv(WR_3[]) <- contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) + fte*(1-custody_fte)*(WN_3+crim_multiplier*WS_3) - wr3_out[i]
  ##jailed (remand and sentenced)
  deriv(JR_3[]) <- remand*(fte_3[i] + rep_3[i]) - rem_length*JR_3[i] - jr3_out[i]
  deriv(J_3[]) <- (1-remand)*(custody_fte*fte_3[i] + custody_rep*rep_3[i]) + r2c*rem_length*JR_3[i] - release*JR_3[i] - mortality*JR_3[i] - j3_out[i]

  #NOTE!!!!! fix the JCL J remand THING!!!!!!!!!

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

  #to make the ODEs a little more readable
  #total first time entrants (from no contact & surveilled, both working and not) for each class group
  #note to self - you should really probably have an extra chance of entering if you're actually in CL
  #maybe add another multiplier lol, change the name of crim_multiplier (too general) to surv_multiplier
  fte_1[] <- fte*((UN_1[i]+WN_1[i])+crim_multiplier*(US_1[i]+WS_1[i]))
  fte_2[] <- fte*((UN_2[i]+WN_2[i])+crim_multiplier*(US_2[i]+WS_2[i]))
  fte_3[] <- fte*((UN_3[i]+WN_3[i])+crim_multiplier*(US_3[i]+WS_3[i]))

  #total repeat offenses (from those already in the CSJ, both working and not) for each class group
  rep_1[] <- rep*(WR_1[i]+UR_1[i])
  rep_2[] <- rep*(WR_2[i]+UR_2[i])
  rep_3[] <- rep*(WR_3[i]+UR_3[i])



  #tests - replicates known/observable CL dynamcis/ policing/CJS attributes?
  #for example, 55% percent of cautions/sentences are of first time entrants to the CSJ
  #what about girl v boy dynamics...

  test_prev_CL[] <- W[i]/N[i]
  #remand should be about 20% of total population in custody
  test_custody_ratio[] <- JR[i]/(JR[i] + J[i])


  ##generated quantities
  output(test_prev_CL) <- test_prev_CL
  output(test_custody_ratio) <- test_custody_ratio
  output(U) <- U
  output(W) <- W
  output(JR) <- JR
  output(J) <- J
  output(S) <- S
  output(R) <- R

}, target='r')        #or c



pars <- list(x = rbind(c(0.8, 0.2), #first row is group 1's mixing group 1,2,3,4,5
                       c(0.2, 0.8)),
             alpha = 1.25,
             W_1_ini = c(0,0), #number initially working
             W_2_ini = c(0,0), #number initially working
             W_3_ini = c(1,0), #number initially working
             H_1_ini = c(0,0), #number initially harming
             H_2_ini = c(0,0), #number initially harming
             H_3_ini = c(1,0), #number initially harming
             J_1_ini = c(0,0), #number initially in jail
             J_2_ini = c(0,0), #number initially in jail
             J_3_ini = c(0,0), #number initially in jail
             recruit_1 = c(0.0005,0.0001),
             recruit_2 = c(0.001,0.0009),
             recruit_3 = c(0.005,0.003),
             h = c(0.25,0.125)
)


mod <- child_recr(user = pars)
t <- seq(0, 260, length.out = 520)  #from 0 to 5 years, two steps a week or something
CL_data <- as.data.frame(mod$run(t))


U_data <- data.frame(CL_data[,grep('U',colnames(CL_data))])
W_data <- data.frame(CL_data[,grep('W',colnames(CL_data))])
H_data <- data.frame(CL_data[,grep('H',colnames(CL_data))])
J_data <- data.frame(CL_data[,grep('J',colnames(CL_data))])
prev_CL_data <- data.frame(CL_data[,grep('prev_CL',colnames(CL_data))])


incl_data <- data.frame(CL_data[,grep('1',colnames(CL_data))])
excl_data <- data.frame(CL_data[,grep('2',colnames(CL_data))])
close_data <- data.frame(CL_data[,grep('3',colnames(CL_data))])


ggplot(data = incl_data) +
  geom_line(mapping = aes(x=t, y=U_1.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_1.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_1.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_1.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=H_1.1.), color = "black") +
  geom_line(mapping = aes(x=t, y=J_1.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=H_1.2.), color = "grey") +
  geom_line(mapping = aes(x=t, y=J_1.2.), color = "pink")

ggplot(data = excl_data) +
  geom_line(mapping = aes(x=t, y=U_2.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_2.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_2.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_2.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=H_2.1.), color = "black") +
  geom_line(mapping = aes(x=t, y=J_2.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=H_2.2.), color = "grey") +
  geom_line(mapping = aes(x=t, y=J_2.2.), color = "pink")

ggplot(data = close_data) +
  geom_line(mapping = aes(x=t, y=U_3.1.), color = "red") +
  geom_line(mapping = aes(x=t, y=W_3.1.), color = "blue") +
  geom_line(mapping = aes(x=t, y=U_3.2.), color = "orange") +
  geom_line(mapping = aes(x=t, y=W_3.2.), color = "green")+
  geom_line(mapping = aes(x=t, y=H_3.1.), color = "black") +
  geom_line(mapping = aes(x=t, y=J_3.1.), color = "purple") +
  geom_line(mapping = aes(x=t, y=H_3.2.), color = "grey") +
  geom_line(mapping = aes(x=t, y=J_3.2.), color = "pink")


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


