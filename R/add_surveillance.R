#install.packages("odin")
library(odin)
library(ggplot2)

child_recr = odin::odin({
  ##two mixing matrices - one for recruitment thats strongly assortative
  ##and the other for harm that's very close to random
  ##the mixing part for recruitment goes over N because its about the total population whereas
  ##the one for harm goes over (w+H) because its only about those

  ##actually the two different splits - leading to a 3 dimensional array - i dont think that would work

  ##number of social/ gender classes
  N_gender <- user(2)

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
  red <- user(0.5) #recidivism rate
  mortality <- user(0) #death rate from causes other than county lines
  h[] <- user(0.25) #rate at which people in harm group commit harm... once a monthish?
  m <- user(0.01) #percentage of harms that are fatal (the better letter would prob be f)...1%?



  ##initial conditions
  N_1_ini <- user(700) #total initial pop
  N_2_ini <- user(250) #total initial pop
  N_3_ini <- user(50) #total initial pop
  W_1_ini[] <- user(0) #number initially working
  W_2_ini[] <- user(0) #number initially working
  W_3_ini[] <- user(0) #number initially working
  J_1_ini[] <- user(0) #number initially in jail
  J_2_ini[] <- user(0) #number initially in jail
  J_3_ini[] <- user(0) #number initially in jail

  initial(U_1[]) <- N_1_ini/(N_gender) - (W_1_ini[i] + J_1_ini[i])
  initial(U_2[]) <- N_2_ini/(N_gender) - (W_2_ini[i] + J_2_ini[i])
  initial(U_3[]) <- N_3_ini/(N_gender) - (W_3_ini[i] + J_3_ini[i])
  initial(W_1[]) <- W_1_ini[i]
  initial(W_2[]) <- W_2_ini[i]
  initial(W_3[]) <- W_3_ini[i]
  initial(J_1[]) <- J_1_ini[i]
  initial(J_2[]) <- J_2_ini[i]
  initial(J_3[]) <- J_3_ini[i]

  ## defining dimension
  dim(U) <- N_gender
  dim(U_1) <- N_gender
  dim(U_2) <- N_gender
  dim(U_3) <- N_gender
  dim(US_1) <- N_gender
  dim(US_2) <- N_gender
  dim(US_3) <- N_gender
  dim(W) <- N_gender
  dim(W_1) <- N_gender
  dim(W_2) <- N_gender
  dim(W_3) <- N_gender
  dim(WS_1) <- N_gender
  dim(WS_2) <- N_gender
  dim(WS_3) <- N_gender
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

  dim(W_1_ini) <- N_gender
  dim(W_2_ini) <- N_gender
  dim(W_3_ini) <- N_gender
  dim(J_1_ini) <- N_gender
  dim(J_2_ini) <- N_gender
  dim(J_3_ini) <- N_gender

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
  N[] <- U_1[i] + U_2[i] + U_3[i] + U_1[i] + W_2[i] + W_3[i] + J_1[i] + J_2[i] + J_3[i]
  N_1[] <- U_1[i] + W_1[i] + J_1[i]
  N_2[] <- U_2[i] + W_2[i] + J_2[i]
  N_3[] <- U_3[i] + W_3[i] + J_3[i]
  U[] <- U_1[i] + U_2[i] + U_3[i]
  W[] <- W_1[i] + W_2[i] + W_3[i]
  J[] <- J_1[i] + J_2[i] + J_3[i]
  ##I[] <- U_1[i] + W_1[i] + H_1[i] + J_1[i] - considering a re-name - you would have to go change the mixing matrices
  ##E[] <- U_2[i] + W_2[i] + H_3[i] + J_2[i]
  ##C[] <- U_3[i] + W_3[i] + H_2[i] + J_3[i]

  births_1[] <- mortality*N_1[i] + m*h[i]*W_1[i] #sum of all mortality <- births to maintain stable pop
  births_2[] <- mortality*N_2[i] + m*h[i]*W_2[i]
  births_3[] <- mortality*N_3[i] + m*h[i]*W_3[i]

  deriv(H_1[]) <- (1-d-m)*h[i]*W_1[i]*sum(mix_h[,i]) - m*h[i]*H_1[i]*sum(mix_h[,i]) - arrest_h*H_1[i] - desist*H_1[i] - mortality*H_1[i]
  deriv(HS_1[]) <- s_i[i]*H_1[i] + red*release*JCL_1[i]


  ##differential equations - INCLUSION
  deriv(U_1[]) <- births_1[i] + desist*W_1[i] + d*h[i]*W_1[i] - contact*recruit_1[i]*U_1[i]*sum(mix1_r[,i]) - mortality*U_1[i]
  deriv(US_1[]) <- arrest_i[i]*U_1[i] + (1-red)*release*JCL_1[i]
  deriv(W_1[]) <- contact*recruit_1[i]*U_1[i]*sum(mix1_r[,i]) - arrest*W_1[i] - desist*W_1[i] - mortality*W_1[i]
  deriv(WS_1[]) <- arrest_i[i]*W_1[i] + red*release*JCL_1[i]
  deriv(O_1[]) ##subject to a police order?
  deriv(J_1[]) <-
  deriv(JCL_1[]) <- prosecute*W_1[i] - release*JCL_1[i] - mortality*JCL_1[i]

  ##differential equations - EXCLUSION
  deriv(U_2[]) <- births_2[i] + desist*W_2[i] + d*h[i]*W_2[i] - contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - mortality*U_2[i]
  deriv(US_2[]) <- arrest_e[i]*U_2[i] + (1-red)*release*JCL_2[i]
  deriv(W_2[]) <- contact*recruit_2[i]*U_2[i]*sum(mix2_r[,i]) - arrest*W_2[i] - desist*W_2[i] - mortality*W_2[i]
  deriv(WS_2[]) <- arrest_e[i]*W_2[i] + red*release*JCL_2[i]
  deriv(J_2[]) <-
  deriv(JCL_2[]) <- prosecute*W_2[i] - release*JCL_2[i] - mortality*JCL_2[i]

  ##differential equations - CLOSE PROXIMITY
  deriv(U_3[]) <- births_3[i] + desist*W_3[i] + d*h[i]*W_3[i] - contact*recruit_3[i]*U_3[i]*sum(mix3_r[,i]) - mortality*U_3[i]
  deriv(US_3[]) <- arrest_c[i]*U_3[i] + (1-red)*release*JCL_3[i]
  deriv(W_3[]) <- contact*recruit_3[i]*U_3[i]*sum(mix3_r[,i]) - arrest*W_3[i] - desist*W_3[i] - mortality*W_3[i]
  deriv(WS_3[]) <- arrest_c[i]*W_3[i] + red*release*JCL_3[i]
  deriv(J_3[]) <-
  deriv(JCL_3[]) <- prosecute*W_3[i] - release*JCL_3[i] - mortality*JCL_3[i]

  #test of key CL dynamics
  test_prev_CL[] <- W[i]/N[i]
  test_incarcerated[] <- J[i]



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


