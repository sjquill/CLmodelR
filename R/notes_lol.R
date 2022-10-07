
##two mixing matrices - one for recruitment thats strongly assortative
##and the other for harm that's very close to random
##the mixing part for recruitment goes over N because its about the total population whereas
##the one for harm goes over (w+H) because its only about those

##actually the two different splits - leading to a 3 dimensional array - i dont think that would work



##will i say only involved kids go missing? like that's how police notice kids?
##so we've got TriED and then sen_cust (90 kids in birmingham per year) and CHARGED and then rem_cust
##and we've got TriED and then sen_rest (880 kids per year) and CHARGED and then rem_rest

test?
#what about recidivism rate, or even prop CL of those entering jail v prop CL of those released



#TASKS
#1. FIX RECRUITMENT - i took out mixing from jail, what else was there......nothing?
#2. FIX JAIL MIXING - so no one out of jail mixes with those in - DONE - second is set up recruitment in jail
# could i do this with ONE SINGLE NEW OD EQation that just keeps track of number of CL in Cusdtody???..prob not...ok lol i think you need 4 jail eqs!!!
#wait lol, what about jail as a deterrant? (ha) - ok added
#3. MAKE FLOWS BETWEEN CLASS STATES

WAIT - JAIL MIXING ISNT DONE because i forgot about the other people in jail.... from outside birmingham


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


new_rec_cl[i] <- contact*recruit_j[i]*cl_pc[i]*(J_1[i] or JR_1)

mix is prop CL in jail cl_pc[i]

cl_pc[i] <- (JRW_1,2,3[i] + JW_1,2,3[i])/ (JR_1[i] + JRW_1[i] + J_1[i] + JW_1[i])




JR_1[] + JRW_1[]
J_1[] JW_1[]

mix_j[,i] <- cl_pc[i]*z[j,i]

z[j,i] is obviously nothing at all(id matrix) (no girls and boys in same prison)
so we can get rid of it


x <- 0.01
y <- x*(1-x)
y

##cl_pc*(1-cl_pc[i]) <- this term is biggest when its exactly half and half, which makes sense



#wait lol, what about jail as a deterrant? (ha)
so everyone still goes back to R because you cant leave the CSJ once youre in it
but who goes to WR and who goes to UR!!!

  what about, a certain number who enter jail immediately quit on entering & dont take part in the recruitment mixing

we had a redicivism thing happening
+ (1-red)*release*JCL_1[i]

or... you can 'desist' in jail - as in you resolve not to take part anymore


deter

we also used to have this desistence after first harm.

+ d*h[i]*W_1[i]


#TASK
#1. surveillance!!!!!!!!!!!!!!1
#2. (or sub1) arrest and missing

#OK emergecny SHORT CUT - just do stop and search and missing for transition to surveillance!!!





pars <- list(x = rbind(c(0.8, 0.2), #first row is group 1's mixing group 1,2,3,4,5
                       c(0.2, 0.8)),
             alpha = 1.25,
             UN_1_ini = c(41042,37884), #number initially uninvolved, no police contact
             UN_2_ini = c(17679,16319), #number initially uninvolved, no police contact - 93.6% of the excluded
             UN_3_ini = c(0.0001,0.0001), #number initially uninvolved, no police contact - zero for close, all already flagged
             US_1_ini = c(0.0001,0.0001), #number initially uninvolved, surveilled - zero for included
             US_2_ini = c(1263,1165), #number initially uninvolved, surveilled - 6.6% of the excluded
             US_3_ini = c(4249,1821), #number initially uninvolved, surveilled - 100% of the close
             UR_1_ini = c(4,2), #number initially uninvolved, in CSJ - lets say 800 start in the CSJ (i just picked), and i split between 3
             UR_2_ini = c(356,39), #number initially uninvolved, in CSJ
             UR_3_ini = c(360,40), #number initially uninvolved, in CSJ
             WN_1_ini = c(0.0001,0.0001), #number initially working, no police contact
             WN_2_ini = c(0,0), #number initially working, no police contact
             WN_3_ini = c(0,0), #number initially working, no police contact
             WS_1_ini = c(0,0), #number initially working, surveilled
             WS_2_ini = c(1,0), #number initially working, surveilled
             WS_3_ini = c(5,1), #number initially working, surveilled -lets start small working & hope it takes off?
             WR_1_ini = c(0,0), #number initially working, in CSJ
             WR_2_ini = c(1,0), #number initially working, in CSJ
             WR_3_ini = c(1,1), #number initially working, in CSJ
             J_1_ini = c(1,0), #number initially on custodial sentence
             J_2_ini = c(7,0), #number initially on custodial sentence
             J_3_ini = c(25,5), #number initially on custodial sentence - prev of custodial sentence is 38 so, just split it
             JW_1_ini = c(0,0), #number of CL initially on custodial sentence
             JW_2_ini = c(0,1), #number of CL initially on custodial sentence
             JW_3_ini = c(1,0), #number of CL initially on custodial sentence
             JR_1_ini = c(0,0), #number initially remanded to custody - none
             JR_2_ini = c(4,0), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JR_3_ini = c(3,1.5), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JRW_1_ini = c(0,0), #number of CL initially remanded to custody
             JRW_2_ini = c(0,1), #number of CL initially remanded to custody -
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
             rep = c(0.002,0.001) #some kind of rate of repeated offences (should work out as like 452
)


pars <- list(x = rbind(c(0.8, 0.2), #first row is group 1's mixing group 1,2,3,4,5
                       c(0.2, 0.8)),
             alpha = 1.25,
             UN_1_ini = c(41042,37884), #number initially uninvolved, no police contact
             UN_2_ini = c(17679,16319), #number initially uninvolved, no police contact - 93.6% of the excluded
             UN_3_ini = c(0.0001,0.0001), #number initially uninvolved, no police contact - zero for close, all already flagged
             US_1_ini = c(0.0001,0.0001), #number initially uninvolved, surveilled - zero for included
             US_2_ini = c(1263,1165), #number initially uninvolved, surveilled - 6.6% of the excluded
             US_3_ini = c(4249,1821), #number initially uninvolved, surveilled - 100% of the close
             UR_1_ini = c(4,2), #number initially uninvolved, in CSJ - lets say 800 start in the CSJ (i just picked), and i split between 3
             UR_2_ini = c(356,39), #number initially uninvolved, in CSJ
             UR_3_ini = c(360,40), #number initially uninvolved, in CSJ
             WN_1_ini = c(0.0001,0.0001), #number initially working, no police contact
             WN_2_ini = c(1,1), #number initially working, no police contact
             WN_3_ini = c(1,1), #number initially working, no police contact
             WS_1_ini = c(1,1), #number initially working, surveilled
             WS_2_ini = c(1,1), #number initially working, surveilled
             WS_3_ini = c(5,1), #number initially working, surveilled -lets start small working & hope it takes off?
             WR_1_ini = c(1,1), #number initially working, in CSJ
             WR_2_ini = c(1,1), #number initially working, in CSJ
             WR_3_ini = c(1,1), #number initially working, in CSJ
             J_1_ini = c(1,1), #number initially on custodial sentence
             J_2_ini = c(7,1), #number initially on custodial sentence
             J_3_ini = c(25,5), #number initially on custodial sentence - prev of custodial sentence is 38 so, just split it
             JW_1_ini = c(1,1), #number of CL initially on custodial sentence
             JW_2_ini = c(1,1), #number of CL initially on custodial sentence
             JW_3_ini = c(1,1), #number of CL initially on custodial sentence
             JR_1_ini = c(1,1), #number initially remanded to custody - none
             JR_2_ini = c(4,1), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JR_3_ini = c(3,1.5), #number initially remanded to custody - prevalence of remand is 9.5 so just split between these
             JRW_1_ini = c(1,1), #number of CL initially remanded to custody
             JRW_2_ini = c(1,1), #number of CL initially remanded to custody -
             JRW_3_ini = c(1,1), #number of CL initially remanded to custody -
             age_in = c(151,141), #boys, girls  coming in weekly - should be set to [151, 141]
             excl_in = c(0.29,0.19), #percent boys, girls  coming in going to excluded
             close_in = c(0.01,0.01), #percent boys, girls  coming in going to close
             surv_in_i = c(1,1), #pc of boys, girls  in INCLUDED that are surveilled at 10
             surv_in_e = c(0.07,0.07), #pc of gboys, girls  in EXCLUDED that are surveilled at 10
             surv_in_c = c(0.6,0.5), #pc of boys, girls in CLOSE that are surveilled at 10
             missing_i = c(1,1), #rate of missing episodes for the included group
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
             u1_out = c(1,1),  #percentage of exiters leaving from this state, boy and girl
             us1_out = c(1,1),
             ur1_out = c(1,1),
             w1_out = c(1,1),
             ws1_out = c(1,1),
             wr1_out = c(1,1),
             j1_out = c(1,1),
             jr1_out = c(1,1),
             jw1_out = c(1,1),
             jrw1_out = c(1,1),
             u2_out = c(1,1),
             us2_out = c(1,1),
             ur2_out = c(1,1),
             w2_out = c(1,1),
             ws2_out = c(1,1),
             wr2_out = c(1,1),
             j2_out = c(1,1),
             jr2_out = c(1,1),
             jw2_out = c(1,1),
             jrw2_out = c(1,1),
             u3_out = c(1,1),
             us3_out = c(1,1),
             ur3_out = c(1,1),
             w3_out = c(1,1),
             ws3_out = c(1,1),
             wr3_out = c(1,1),
             j3_out = c(1,1),
             jr3_out = c(1,1),
             jw3_out = c(1,1),
             jrw3_out = c(1,1)
)

