
##going_in: custody_fte*(1-remand)*fte_1[i]  remand*fte_1[i],   remand*rep_1[i]  custody_rep*(1-remand)*rep_1[i]
##you get charged - if you're already in the CSJ then theres this bit where the people who get remanded
##flow out instantly and the people who dont flow out like a month or so later (if they get a custodial sentence)
##is this a delay differential equation lol (might be)

cl_pc[i]

##coming out: release_1[] ie (1-r2c)*end_rem*JR_1[i] + end_cust*J_1[i] (ie releases from both remand and sentences)
##where do these go - theyve been in jail, - they could have been recruited....
##cl_pc[i] is the currently percentage of CL among jail people
##cl_pc[i]*release_1[i] goes to WR_1, (1-cl_pc[i])*release_1[i] goes to UR_1
##NO!!!!!!! because you're probably more likely to be CL when you come out?
##so the people who are CL are closer to the end of their sentence? (on balance of prob?)
##wait, does all of that make no sense at all, because we're just like working w averages really?

##UPDATEABLE (how to make it updateable) - lilke the current) percentage of CL people in jail...
##UANOTHER delay equation? ???????

cl_pc[i] <- (WN_1[i]+WS_1[i]+WR_1[i])/(UN_1[i]+WN_1[i]+US_1[i]+WS_1[i]+WR_1[i]+UR_1[i])

cl already here + cl recruited in this timestep, then add cl that just came in
non-cl already here - cl recruited in this timestep, then add non-cl that just came in

total in -
  sentence: (1-remand) * (custody_fte*fte*((UN_1[i]+WN_1[i])+crim_multiplier*(US_1[i]+WS_1[i])) + custody_rep*rep*(WR_1[i]+UR_1[i]))
remand: remand * fte*((UN_1[i]+WN_1[i])+crim_multiplier*(US_1[i]+WS_1[i])) + rep*(WR_1[i]+UR_1[i]))

divide into total CL in - sent: (1-remand) * (custody_fte*fte*(WN_1[i]+crim_multiplier*WS_1[i]) + custody_rep*rep*WR_1[i])
rem: remand * fte*(WN_1[i]+crim_multiplier*WS_1[i]) + rep*WR_1[i])


total non-CL in- sent: (1-remand) * (custody_fte*fte*(UN_1[i]+crim_multiplier*US_1[i]) + custody_rep*rep*UR_1[i])
remand: remand * fte*(UN_1[i]+crim_multiplier*US_1[i]) + rep*UR_1[i])

cl[i] <- cl[i-1] *
  d_CL <- contact*recruit_j[i]*cl_pc*(1-cl_pc[i])*J[i]


new_rec_cl[i] <- contact*recruit_j[i]*cl_pc*(1-cl_pc[i])*J[i]

mix_j[,i] <- cl_pc[i]*z[j,i]

z[j,i] is obviously nothing at all(id matrix) (no girls and boys in same prison)
so we can get rid of it


x <- 0.01
y <- x*(1-x)
y

##cl_pc*(1-cl_pc[i]) <- this term is biggest when its exactly half and half, which makes sense
