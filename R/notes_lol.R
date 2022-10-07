
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




