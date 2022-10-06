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
  age_in[] <- user() # should be set to [151, 141]

  excl_in[] <- user() #class breakdown - pc girls, boys excluded
  close_in[] <- user() #class breakdown - pc girls, boys close

  surv_in_i[] <- user() #pc of girls, boys in INCLUDED that are surveilled at 10
  surv_in_e[] <- user() #pc of girls, boys in EXCLUDED that are surveilled at 10
  surv_in_c[] <- user() #pc of girls, boys in CLOSE that are surveilled at 10


  #kids coming OUT oh lol this is going to be so hard.....
  U_1_out[] <- user(0)
  US_1_out[] <- user(0)
  UR_1_out[] <- user(0)
  W_1_out[] <- user(0)
  WS_1_out[] <- user(0)
  WR_1_out[] <- user(0)
  J_1_out[] <- user(0)
  JCL_1_out[] <- user(0)
  U_2_out[] <- user(0)
  US_2_out[] <- user(0)
  UR_2_out[] <- user(0)
  W_2_out[] <- user(0)
  WS_2_out[] <- user(0)
  WR_2_out[] <- user(0)
  J_2_out[] <- user(0)
  JCL_2_out[] <- user(0)
  U_3_out[] <- user(0)
  US_3_out[] <- user(0)
  UR_3_out[] <- user(0)
  W_3_out[] <- user(0)
  WS_3_out[] <- user(0)
  WR_3_out[] <- user(0)
  J_3_out[] <- user(0)
  JCL_3_out[] <- user(0)

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

  custody_fte[] <- user() #percentage of fte's that get a custodial sentence (of men, 1.5% - of women, 0.3%)
  crim_multiplier <- user() #figure this out properly but its some sort of elevated risk of criminalisation that you get from beign already known to the police

  ##initial conditions
  #the old equal split for gender doesnt work bc girls are less likely to be close
  #do we have to do that equation bit or can everything just be explicitly set?
  N_1_ini[] <- user(0) #total initial pop included
  N_2_ini[] <- user(0) #total initial pop excluded
  N_3_ini[] <- user(0) #total initial pop close
  U_1_ini[] <- user(0) #number initially uninvolved, no police contact
  U_2_ini[] <- user(0) #number initially uninvolved, no police contact
  U_3_ini[] <- user(0) #number initially uninvolved, no police contact
  US_1_ini[] <- user(0) #number initially uninvolved, surveilled
  US_2_ini[] <- user(0) #number initially uninvolved, surveilled
  US_3_ini[] <- user(0) #number initially uninvolved, surveilled
  UR_1_ini[] <- user(0) #number initially uninvolved, some formal restriction
  UR_2_ini[] <- user(0) #number initially uninvolved, some formal restriction
  UR_3_ini[] <- user(0) #number initially uninvolved, some formal restriction
  W_1_ini[] <- user(0) #number initially working, no police contact
  W_2_ini[] <- user(0) #number initially working, no police contact
  W_3_ini[] <- user(0) #number initially working, no police contact
  WS_1_ini[] <- user(0) #number initially working, surveilled
  WS_2_ini[] <- user(0) #number initially working, surveilled
  WS_3_ini[] <- user(0) #number initially working, surveilled
  WR_1_ini[] <- user(0) #number initially working, some formal restriction
  WR_2_ini[] <- user(0) #number initially working, some formal restriction
  WR_3_ini[] <- user(0) #number initially working, some formal restriction
  J_1_ini[] <- user(0) #number initially in jail
  J_2_ini[] <- user(0) #number initially in jail
  J_3_ini[] <- user(0) #number initially in jail
  JCL_1_ini[] <- user(0) #number initially in jail for CL-related offence
  JCL_2_ini[] <- user(0) #number initially in jail for CL-related offence
  JCL_3_ini[] <- user(0) #number initially in jail for CL-related offence

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
  initial(JCL_1[]) <- JCL_1_ini[i]
  initial(JCL_2[]) <- JCL_2_ini[i]
  initial(JCL_3[]) <- JCL_3_ini[i]

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
  dim(JCL_1) <- N_gender
  dim(JCL_2) <- N_gender
  dim(JCL_3) <- N_gender

  dim(N) <- N_gender
  dim(N_1) <- N_gender
  dim(N_2) <- N_gender
  dim(N_3) <- N_gender

  dim(N_1_ini) <- N_gender
  dim(N_2_ini) <- N_gender
  dim(N_3_ini) <- N_gender

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
  dim(JCL_1_ini) <- N_gender
  dim(JCL_2_ini) <- N_gender
  dim(JCL_3_ini) <- N_gender

  dim(births_1) <- N_gender
  dim(births_2) <- N_gender
  dim(births_3) <- N_gender
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

  ##intermediate quantities
  N[] <- U_1[i] + US_1[i]  + UR_1[i] + W_1[i] + WS_1[i] + WR_1[i] + J_1[i] + JCL_1[i] + U_2[i] + US_2[i]  + UR_2[i] + W_2[i] + WS_2[i] + WR_2[i] + J_2[i] + JCL_2[i] + U_3[i] + US_3[i]  + UR_3[i] + W_3[i] + WS_3[i] + WR_3[i] + J_3[i] + JCL_3[i]
  N_1[] <- U_1[i] + US_1[i]  + UR_1[i] + W_1[i] + WS_1[i] + WR_1[i] + J_1[i] + JCL_1[i] ##total numbers in included class
  N_2[] <- U_2[i] + US_2[i]  + UR_2[i] + W_2[i] + WS_2[i] + WR_2[i] + J_2[i] + JCL_2[i]
  N_3[] <- U_3[i] + US_3[i]  + UR_3[i] + W_3[i] + WS_3[i] + WR_3[i] + J_3[i] + JCL_3[i]
  U[] <- U_1[i] + U_2[i] + U_3[i] + US_1[i] + US_2[i] + US_3[i] + UR_1[i] + UR_2[i] + UR_3[i]
  W[] <- W_1[i] + W_2[i] + W_3[i] + WS_1[i] + WS_2[i] + WS_3[i] + WR_1[i] + WR_2[i] + WR_3[i]
  J[] <- J_1[i] + J_2[i] + J_3[i] + JCL_1[i] + JCL_2[i] + JCL_3[i]  ##total numbers in jail include both those in for CL & not
  S[] <- US_1[i] + US_2[i] + US_3[i] + WS_1[i] + WS_2[i] + WS_3[i] ##total numbers surveilled include both those working & not
  R[] <- UR_1[i] + UR_2[i] + UR_3[i] + WR_1[i] + WR_2[i] + WR_3[i] ##total numbers under restrictions include both those working & not

  ##I[] <- U_1[i] + W_1[i] + H_1[i] + J_1[i] - considering a re-name - you would have to go change the mixing matrices
  ##E[] <- U_2[i] + W_2[i] + H_3[i] + J_2[i]
  ##C[] <- U_3[i] + W_3[i] + H_2[i] + J_3[i]

  incl_in[] <- 1 - excl_in[i] - close_in[i]

  #####################FIX NEEDED################
  #births_1[] <- mortality*N_1[i] + m*h[i]*W_1[i] #sum of all mortality <- births to maintain stable pop
  #births_2[] <- mortality*N_2[i] + m*h[i]*W_2[i]
  #births_3[] <- mortality*N_3[i] + m*h[i]*W_3[i]


  ##will i say only involved kids go missing? like that's how police notice kids?
  ##so we've got TriED and then sen_cust (90 kids in birmingham per year) and CHARGED and then rem_cust
  ##and we've got TriED and then sen_rest (880 kids per year) and CHARGED and then rem_rest
  ##differential equations - INCLUSION
  ##univolved
  deriv(U_1[]) <- (1-surv_in_i[i])*incl_in[i]*age_in[i] + desist*W_1[i] + d*h[i]*W_1[i] - surveil_i*U_1[1] - contact*recruit_1[i]*U_1[i]*sum(mix1_r[,i]) - mortality*U_1[i]
  deriv(US_1[]) <- surv_in_i[i]*incl_in[i]*age_in[i] + surveil_i*U_1[1] + (1-ss_arrest)*ss_i[i]*U_1[i] + (1-red)*release*JCL_1[i] - contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i])
  deriv(UR_1[]) <- ss_arrest*ss_i[i]* + arrest_i[i]*US_1[i] - - contact*recruit_1[i]*UR_1[i]*sum(mix1_r[,i])
  ##working
  deriv(W_1[]) <- contact*recruit_1[i]*U_1[i]*sum(mix1_r[,i]) - surveil_i*W_1[1] - arrest*W_1[i] - desist*W_1[i] - mortality*W_1[i] - missing*W_1[i]
  deriv(WS_1[]) <- surveil_i*W_1[1] + contact*recruit_1[i]*US_1[i]*sum(mix1_r[,i]) + missing*W_1[i] + arrest_i[i]*W_1[i] + red*release*JCL_1[i]
  deriv(WR_1[]) <-contact*recruit_1[i]*UW_1[i]*sum(mix1_r[,i]) +
  ##jailed (remand and sentenced)
  deriv(JR_1[]) <- charge[i]*remand*WHATSTATE - rem_length*J_1[i]
  deriv(JCL_1[]) <- prosecute*W_1[i] + r2c*rem_length*J_2[i] - release*JCL_1[i] - mortality*JCL_1[i]


  ##differential equations - EXCLUSION
  ##univolved
  deriv(U_2[]) <- (1-surv_in_e[i])*excl_in[i]*age_in[i] + desist*W_2[i] + d*h[i]*W_2[i] - surveil_e*U_2[1] - contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - mortality*U_2[i]
  deriv(US_2[]) <- surv_in_e[i]*excl_in[i]*age_in[i] +surveil_e*U_2[1] + ss_e[i]*U_2[i] + arrest_e[i]*U_2[i] + (1-red)*release*JCL_2[i] - contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i])
  deriv(UR_2[]) <- - contact*recruit_2[i]*UW_2[i]*sum(mix2_r[,i])
  ##working
  deriv(W_2[]) <- contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - surveil_e*W_2[1] - arrest*W_2[i] - desist*W_2[i] - missing*W_2[i] - mortality*W_2[i]
  deriv(WS_2[]) <- surveil_e*W_2[1] + contact*recruit_2[i]*US_2[i]*sum(mix2_r[,i]) + missing*W_2[i] + arrest_e[i]*W_2[i] + red*release*JCL_2[i]
  deriv(WR_2[]) <- contact*recruit_2[i]*UW_2[i]*sum(mix2_r[,i]) +
  ##jailed (remand and sentenced)
  deriv(J_2[]) <- charge[i]*remand*WHATSTATE - rem_length*J_2[i]
  deriv(JCL_2[]) <- prosecute*W_2[i] + r2c*rem_length*J_2[i] - release*JCL_2[i] - mortality*JCL_2[i]

  ##differential equations - CLOSE PROXIMITY -
  ##NOTE TO SELF - I AM WORKING EVERYTHIGN OUT ON CLSOE PROXIMITY FIRST!! THEN I WILL FIX!! ## ## ## ## ## ## ##
  ##univolved
  deriv(U_3[]) <- (1-surv_in_c[i])*close_in[i]*age_in[i] + desist*W_3[i] + d*h[i]*W_3[i] - surveil_c*U_3[1] - contact*recruit_3[i]*U_3[i]*sum(mix3_r[,i]) - mortality*U_3[i]
  deriv(US_3[]) <- surv_in_c[i]*close_in[i]*age_in[i] + surveil_c*U_3[1] + arrest_c[i]*U_3[i] + (1-red)*release*JCL_3[i] - contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i])
  deriv(UR_3[]) <- - contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) + fte*(1-custody_fte)*(U_3+crim_multiplier*US_3)
  ##working
  deriv(W_3[]) <- contact*recruit_3[i]*U_3[i]*sum(mix3_r[,i]) - surveil_c*W_3[1] - arrest*W_3[i] - fte*W_3 - desist*W_3[i] - missing*W_3[i] - mortality*W_3[i]
  deriv(WS_3[]) <- surveil_c*W_3[1] + contact*recruit_3[i]*US_3[i]*sum(mix3_r[,i]) + missing*W_3[i] + arrest_c[i]*W_3[i] + red*release*JCL_3[i] - fte*crim_multiplier*WS_3
  deriv(WR_3[]) <- contact*recruit_3[i]*UR_3[i]*sum(mix3_r[,i]) + fte*(1-custody_fte)*(W_3+crim_multiplier*WS_3)
  ##jailed (remand and sentenced)
  deriv(J_3[]) <- charge[i]*remand*WHATSTATE - rem_length*J_3[i]
  deriv(JCL_3[]) <- prosecute*W_3[i] + r2c*rem_length*J_3[i] - release*JCL_3[i] - mortality*JCL_3[i]


  #making the mixing matrix help
  ##first, weird mixing bits for recruitment that you'll probably need to take out later
  II[] <- N_1[i]/N[i] + (1-(N_1[i]/N[i]))/alpha
  IE[] <- N_2[i]/N[i]*(alpha-1)/alpha
  IC[] <- N_3[i]/N[i]*(alpha-1)/alpha

  EI[] <- N_1[i]/N[i]*(alpha-1)/alpha
  EE[] <- N_2[i]/N[i] + (1-(N_2[i]/N[i]))/alpha
  EC[] <- N_3[i]/N[i]*(alpha-1)/alpha

  CI[] <- N_1[i]/N[i]*(alpha-1)/alpha
  CE[] <- N_2[i]/N[i]*(alpha-1)/alpha
  CC[] <- N_3[i]/N[i] + (1-(N_3[i]/N[i]))/alpha

  #back to making the mixing matrix
  prop1_r[] <- II[i]*(W_1[i]/N_1[i]) + IE[i]*(W_2[i]/N_2[i]) + IC[i]*(W_3[i]/N_3[i])
  prop2_r[] <- EI[i]*(W_1[i]/N_1[i]) + EE[i]*(W_2[i]/N_2[i]) + EC[i]*(W_3[i]/N_3[i])
  prop3_r[] <- CI[i]*(W_1[i]/N_1[i]) + CE[i]*(W_2[i]/N_2[i]) + CC[i]*(W_3[i]/N_3[i])
  mix1_r[,] <- prop1_r[i]*x[j,i]
  mix2_r[,] <- prop2_r[i]*x[j,i]
  mix3_r[,] <- prop3_r[i]*x[j,i]


  #tests - replicates known/observable CL dynamcis/ policing/CJS attributes?
  #for example 25% of the total prison population should be remands
  #also, 55% percent of cautions/sentences are of first time entrants to the CSJ
  #what about girl v boy dynamics....



  test_prev_CL[] <- W[i]/N[i]
  test_incarcerated[] <- J[i]


  ##generated quantities
  output(test_prev_CL) <- test_prev_CL
  output(test_incarcerated) <- test_incarcerated
  output(U) <- U
  output(W) <- W
  output(H) <- H
  output(J) <- J

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


