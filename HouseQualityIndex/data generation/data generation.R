setwd("/Users/shenxiang/Desktop/NYCHVS Project/data")
setwd("/Users/xiashe/Desktop/jsm/data")
library(readr)

f1=read.csv("NYCHVS 1991 Occupied File for ASA Challenge_CSV.csv",header=T)
f2=read.csv("NYCHVS 1993 Occupied File for ASA Challenge_CSV.csv",header=T)
f3=read.csv("NYCHVS 1996 Occupied File for ASA Challenge_CSV.csv",header=T)
f4=read.csv("NYCHVS 1999 Occupied File for ASA Challenge_CSV.csv",header=T)
f5=read.csv("NYCHVS 2002 Occupied File for ASA Challenge_CSV.csv",header=T)
f6=read.csv("NYCHVS 2005 Occupied File for ASA Challenge_CSV.csv",header=T)
f7=read.csv("NYCHVS 2008 Occupied File for ASA Challenge_CSV.csv",header=T)
f8=read.csv("NYCHVS 2011 Occupied File for ASA Challenge_CSV.csv",header=T)
f9=read.csv("NYCHVS 2014 Occupied File for ASA Challenge_CSV.csv",header=T)
f10=read.csv("NYCHVS 2017 Occupied File for ASA Challenge_CSV.csv",header=T)
f10[,137:216] <- NULL


f1use=data.frame(1991,f1$borough,f1$sub,f1$tsc, #tsc 0 for own 1 for rent
            f1$ybr,f1$hcr,f1$npr,f1$mgrent,f1$hhinc,
            f1$X_29,
            f1$X_1b,f1$X_1c,#f1$X_1f,
            f1$X_3,f1$X_4a,f1$X_5,f1$X_6,
            f1$X_7a,f1$X_7b,f1$X_7c,
            f1$X_13,f1$X_14,f1$X_15a,
            f1$X_d1,f1$X_d2,f1$X_d3,f1$X_d4,f1$X_d5,f1$X_d6,
            f1$X_e1,f1$X_e2,f1$X_e3,f1$X_e4,f1$X_e5,
            f1$X_f1,f1$X_f2,f1$X_f3,f1$X_f7,f1$X_f7,f1$X_f6,
            f1$X_g1,f1$X_g2,f1$X_g3,f1$X_g4,f1$X_g5,f1$X_g6,
            f1$X_h,f1$X_i,
            8,8,8, #for j1,j2,j3
            f1$X_20,f1$X_21,f1$X_22a,
            f1$X_23a,8,8,#for 23bc
            f1$X_24a,f1$X_24b,  #building and room
            f1$X_25a,f1$X_25b,f1$X_25c,
            f1$X_26a,f1$X_26b,f1$X_26c,f1$X_27,
            #f1$X_28a2,f1$X_28b2,f1$X_28c,f1$X_28d2,f1$X_28e2,
            f1$X_32a,f1$X_32b,f1$X_33,
            f1$X_35a,f1$X_35c,f1$X_36a,f1$X_36b,f1$X_37a,f1$X_37b,f1$X_38a,f1$X_39)


f2use=data.frame(1993,f2$borough,f2$sub,f2$tsc, #tsc 0 for own 1 for rent
            f2$ybr,f2$hcr,f2$npr,f2$mgrent,f2$hhinc,
            f2$X_29,
            f2$X_1b,f2$X_1c,#f2$X_1f,
            f2$X_3,f2$X_4a,f2$X_5,f2$X_6,
            f2$X_7a,f2$X_7b,f2$X_7c,
            f2$X_13,f2$X_14,f2$X_15a, #mortgage
            f2$X_d1,f2$X_d2,f2$X_d3,f2$X_d4,f2$X_d5,f2$X_d6,
            f2$X_e1,f2$X_e2,f2$X_e3,f2$X_e4,f2$X_e5,
            f2$X_f1,f2$X_f2,f2$X_f3,f2$X_f7,f2$X_f7,f2$X_f6,
            f2$X_g1,f2$X_g2,f2$X_g3,f2$X_g4,f2$X_g5,f2$X_g6,
            f2$X_h,f2$X_i,
            8,8,8, #for j1,j2,j3
            f2$X_20,f2$X_21,f2$X_22a,
            f2$X_23a,8,8,#for 23bc
            f2$X_24a,f2$X_24b,  #building and room
            f2$X_25a,f2$X_25b,f2$X_25c,
            f2$X_26a,f2$X_26b,f2$X_26c,f2$X_27,
            #f2$X_28a2,f2$X_28b2,f2$X_28c,f2$X_28d2,f2$X_28e2,
            f2$X_32a,f2$X_32b,f2$X_33,
            f2$X_35a,f2$X_35c,f2$X_36a,f2$X_36b,f2$X_37a,f2$X_37b,f2$X_38a,f2$X_39)


f3use=data.frame(1996,f3$borough,f3$sub,f3$tsc, #tsc 0 for own 1 for rent
            f3$ybr,f3$hcr,f3$npr,f3$mgrent,f3$hhinc,
            f3$X_29,
            f3$X_1b,f3$X_1c,#f3$X_1f,
            f3$X_3,f3$X_4a,f3$X_5,f3$X_6,
            f3$X_7a,f3$X_7b,f3$X_7c,
            f3$X_13,f3$X_14,f3$X_15a,
            f3$X_d1,f3$X_d2,f3$X_d3,f3$X_d4,f3$X_d5,f3$X_d6,
            f3$X_e1,f3$X_e2,f3$X_e3,f3$X_e4,f3$X_e5,
            f3$X_f1,f3$X_f2,f3$X_f3,f3$X_f4,f3$X_f5,f3$X_f6,
            f3$X_g1,f3$X_g2,f3$X_g3,f3$X_g4,f3$X_g5,f3$X_g6,
            f3$X_h,f3$X_i,
            f3$X_j1,f3$X_j2,f3$X_j3, #for j1,j2,j3
            f3$X_20,f3$X_21,f3$X_22a,
            f3$X_23a,f3$X_23b,f3$X_23c,#for 23bc
            f3$X_24a,f3$X_24b,  #building and room
            f3$X_25a,f3$X_25b,f3$X_25c,
            f3$X_26a,f3$X_26b,f3$X_26c,f3$X_27,
            #f3$X_28a2,f3$X_28b2,f3$X_28c,f3$X_28d2,f3$X_28e2,
            f3$X_32a,f3$X_32b,f3$X_33,
            f3$X_35a,f3$X_35c,f3$X_36a,f3$X_36b,f3$X_37a,f3$X_37b,f3$X_38a,f3$X_39)

f4use=data.frame(1999,f4$borough,f4$sub,f4$tsc, #tsc 0 for own 1 for rent
            f4$ybr,f4$hcr,f4$npr,f4$mgrent,f4$hhinc,
            f4$X_29,
            f4$X_1b,f4$X_1c,#f4$X_1f,
            f4$X_3,f4$X_4a,f4$X_5,f4$X_6,
            f4$X_7a,f4$X_7b,f4$X_7c,
            f4$X_13,f4$X_14,f4$X_15a, #mortgage
            f4$X_d1,f4$X_d2,f4$X_d3,f4$X_d4,f4$X_d5,f4$X_d6,
            f4$X_e1,f4$X_e2,f4$X_e3,f4$X_e4,f4$X_e5,
            f4$X_f1,f4$X_f2,f4$X_f3,f4$X_f4,f4$X_f5,f4$X_f6,
            f4$X_g1,f4$X_g2,f4$X_g3,f4$X_g4,f4$X_g5,f4$X_g6,
            f4$X_h,f4$X_i,
            f4$X_j1,f4$X_j2,f4$X_j3, #for j1,j2,j3
            f4$X_20,f4$X_21,f4$X_22a,
            f4$X_23a,f4$X_23b,f4$X_23c,#for 23bc
            f4$X_24a,f4$X_24b,  #building and room
            f4$X_25a,f4$X_25b,f4$X_25c,
            f4$X_26a,f4$X_26b,f4$X_26c,f4$X_27,
            #f4$X_28a2,f4$X_28b2,f4$X_28c,f4$X_28d2,f4$X_28e2,
            f4$X_32a,f4$X_32b,f4$X_33,
            f4$X_35a,f4$X_35c,f4$X_36a,f4$X_36b,f4$X_37a,f4$X_37b,f4$X_38a,f4$X_39)


f5use=data.frame(2002,f5$borough,f5$sub,f5$tsc, #tsc 0 for own 1 for rent
            f5$ybr,f5$hcr,f5$npr,f5$mgrent,f5$hhinc,
            f5$X_29,
            f5$X_1b,f5$X_1c,#f5$X_1f,
            f5$X_3,f5$X_4a,f5$X_5,f5$X_6,
            f5$X_7a,f5$X_7b,f5$X_7c,
            f5$X_13,f5$X_14,f5$X_15a, #mortgage
            f5$X_d1,f5$X_d2,f5$X_d3,f5$X_d4,f5$X_d5,f5$X_d6,
            f5$X_e1,f5$X_e2,f5$X_e3,f5$X_e4,f5$X_e5,
            f5$X_f1,f5$X_f2,f5$X_f3,f5$X_f4,f5$X_f5,f5$X_f6,
            f5$X_g1,f5$X_g2,f5$X_g3,f5$X_g4,f5$X_g5,f5$X_g6,
            f5$X_h,f5$X_i,
            f5$X_j1,f5$X_j2,f5$X_j3, #for j1,j2,j3
            f5$X_20,f5$X_21,f5$X_22a,
            f5$X_23a,f5$X_23b,f5$X_23c,#for 23bc
            f5$X_24a,f5$X_24b,  #building and room
            f5$X_25a,f5$X_25b,f5$X_25c,
            f5$X_26a,f5$X_26b,f5$X_26c,f5$X_27,
            #f5$X_28a2,f5$X_28b2,f5$X_28c,f5$X_28d2,f5$X_28e2,
            f5$X_32a,f5$X_32b,f5$X_33,
            f5$X_35a,f5$X_35c,f5$X_36a,f5$X_36b,f5$X_37a,f5$X_37b,f5$X_38a,f5$X_39)

f6use=data.frame(2005,f6$borough,f6$sub,f6$tsc, #tsc 0 for own 1 for rent
            f6$ybr,f6$hcr,f6$npr,f6$mgrent,f6$hhinc,
            f6$X_29,
            f6$X_1b,f6$X_1c,#f6$X_1f,
            f6$X_3,f6$X_4a,f6$X_5,f6$X_6,
            f6$X_7a,f6$X_7b,f6$X_7c,
            f6$X_13,f6$X_14,f6$X_15a, #mortgage
            f6$X_d1,f6$X_d2,f6$X_d3,f6$X_d4,f6$X_d5,f6$X_d6,
            f6$X_e1,f6$X_e2,f6$X_e3,f6$X_e4,f6$X_e5,
            f6$X_f1,f6$X_f2,f6$X_f3,f6$X_f4,f6$X_f5,f6$X_f6,
            f6$X_g1,f6$X_g2,f6$X_g3,f6$X_g4,f6$X_g5,f6$X_g6,
            f6$X_h,f6$X_i,
            f6$X_j1,f6$X_j2,f6$X_j3, #for j1,j2,j3
            f6$X_20,f6$X_21,f6$X_22a,
            f6$X_23a,f6$X_23b,f6$X_23c,#for 23bc
            f6$X_24a,f6$X_24b,  #building and room
            f6$X_25a,f6$X_25b,f6$X_25c,
            f6$X_26a,f6$X_26b,f6$X_26c,f6$X_27,
            #f6$X_28a2,f6$X_28b2,f6$X_28c,f6$X_28d2,f6$X_28e2,
            f6$X_32a,f6$X_32b,f6$X_33,
            f6$X_35a,f6$X_35c,f6$X_36a,f6$X_36b,f6$X_37a,f6$X_37b,f6$X_38a,f6$X_39)


f7use=data.frame(2008,f7$borough,f7$sub,f7$tsc, #tsc 0 for own 1 for rent
            f7$ybr,f7$hcr,f7$npr,f7$mgrent,f7$hhinc,
            f7$X_29,
            f7$X_1b,f7$X_1c,#f7$X_1f,
            f7$X_3,f7$X_4a,f7$X_5,f7$X_6,
            f7$X_7a,f7$X_7b,f7$X_7c,
            f7$X_13,f7$X_14,f7$X_15a, #mortgage
            f7$X_d1,f7$X_d2,f7$X_d3,f7$X_d4,f7$X_d5,f7$X_d6,
            f7$X_e1,f7$X_e2,f7$X_e3,f7$X_e4,f7$X_e5,
            f7$X_f1,f7$X_f2,f7$X_f3,f7$X_f4,f7$X_f5,f7$X_f6,
            f7$X_g1,f7$X_g2,f7$X_g3,f7$X_g4,f7$X_g5,f7$X_g6,
            f7$X_h,f7$X_i,
            f7$X_j1,f7$X_j2,f7$X_j3, #for j1,j2,j3
            f7$X_20,f7$X_21,f7$X_22a,
            f7$X_23a,f7$X_23b,f7$X_23c,#for 23bc
            f7$X_24a,f7$X_24b,  #building and room
            f7$X_25a,f7$X_25b,f7$X_25c,
            f7$X_26a,f7$X_26b,f7$X_26c,f7$X_27,
            #f7$X_28a2,f7$X_28b2,f7$X_28c,f7$X_28d2,f7$X_28e2,
            f7$X_32a,f7$X_32b,f7$X_33,
            f7$X_35a,f7$X_35c,f7$X_36a,f7$X_36b,f7$X_37a,f7$X_37b,f7$X_38a,f7$X_39)


f8use=data.frame(2011,f8$borough,f8$sub,f8$tsc, #tsc 0 for own 1 for rent
            f8$ybr,f8$hcr,f8$npr,f8$mgrent,f8$hhinc,
            f8$X_29,
            f8$X_1b,f8$X_1c,#f8$X_1f,
            f8$X_3,f8$X_4a,f8$X_5,f8$X_6,
            f8$X_7a,f8$X_7b,f8$X_7c,
            f8$X_13,f8$X_14,f8$X_15a, #mortgage
            f8$X_d1,f8$X_d2,f8$X_d3,f8$X_d4,f8$X_d5,f8$X_d6,
            f8$X_e1,f8$X_e2,f8$X_e3,f8$X_e4,f8$X_e5,
            f8$X_f1,f8$X_f2,f8$X_f3,f8$X_f4,f8$X_f5,f8$X_f6,
            f8$X_g1,f8$X_g2,f8$X_g3,f8$X_g4,f8$X_g5,f8$X_g6,
            f8$X_h,f8$X_i,
            f8$X_j1,f8$X_j2,f8$X_j3, #for j1,j2,j3
            f8$X_20,f8$X_21,f8$X_22a,
            f8$X_23a,f8$X_23b,f8$X_23c,#for 23bc
            f8$X_24a,f8$X_24b,  #building and room
            f8$X_25a,f8$X_25b,f8$X_25c,
            f8$X_26a,f8$X_26b,f8$X_26c,f8$X_27,
            #f8$X_28a2,f8$X_28b2,f8$X_28c,f8$X_28d2,f8$X_28e2,
            f8$X_32a,f8$X_32b,f8$X_33,
            f8$X_35a,f8$X_35c,f8$X_36a,f8$X_36b,f8$X_37a,f8$X_37b,f8$X_38a,f8$X_39)


f9use=data.frame(2014,f9$borough,f9$sub,f9$tsc, #tsc 0 for own 1 for rent
            f9$ybr,f9$hcr,f9$npr,f9$mgrent,f9$hhinc,
            f9$X_29,
            f9$X_1b,f9$X_1c,#f9$X_1f,
            f9$X_3,f9$X_4a,f9$X_5,f9$X_6,
            f9$X_7a,f9$X_7b,f9$X_7c,
            f9$X_13,f9$X_14,f9$X_15a, #mortgage
            f9$X_d1,f9$X_d2,f9$X_d3,f9$X_d4,f9$X_d5,f9$X_d6,
            f9$X_e1,f9$X_e2,f9$X_e3,f9$X_e4,f9$X_e5,
            f9$X_f1,f9$X_f2,f9$X_f3,f9$X_f4,f9$X_f5,f9$X_f6,
            f9$X_g1,f9$X_g2,f9$X_g3,f9$X_g4,f9$X_g5,f9$X_g6,
            f9$X_h,f9$X_i,
            f9$X_j1,f9$X_j2,f9$X_j3, #for j1,j2,j3
            f9$X_20,f9$X_21,f9$X_22a,
            f9$X_23a,f9$X_23b,f9$X_23c,#for 23bc
            f9$X_24a,f9$X_24b,  #building and room
            f9$X_25a,f9$X_25b,f9$X_25c,
            f9$X_26a,f9$X_26b,f9$X_26c,f9$X_27,
            #f9$X_28a2,f9$X_28b2,f9$X_28c,f9$X_28d2,f9$X_28e2,
            f9$X_32a,f9$X_32b,f9$X_33,
            f9$X_35a,f9$X_35c,f9$X_36a,f9$X_36b,f9$X_37a,f9$X_37b,f9$X_38a,f9$X_39)


f10use=data.frame(2017,f10$borough,f10$sub,f10$tsc, #tsc 0 for own 1 for rent
            f10$ybr,f10$hcr,f10$npr,f10$mgrent,f10$hhinc,
            f10$X_29,
            f10$X_1b,f10$X_1c,#f10$X_1f,
            f10$X_3,f10$X_4a,f10$X_5,f10$X_6,
            f10$X_7a,f10$X_7b,f10$X_7c,
            f10$X_13,f10$X_14,99999, #mortgage
            f10$X_d12,f10$X_d12,f10$X_d3,f10$X_d4,f10$X_d5,f10$X_d6,
            f10$X_e1,f10$X_e2,f10$X_e3,f10$X_e4,f10$X_e5,
            f10$X_f1,f10$X_f2,f10$X_f3,f10$X_f4,f10$X_f5,f10$X_f6,
            f10$X_g12,f10$X_g12,f10$X_g3,f10$X_g4,f10$X_g5,f10$X_g6,
            f10$X_h,f10$X_i,
            f10$X_j1,f10$X_j2,f10$X_j3, #for j1,j2,j3
            f10$X_20,f10$X_21,f10$X_22a,
            f10$X_23a,f10$X_23b,f10$X_23c,#for 23bc
            f10$X_24a,f10$X_24b,  #building and room
            f10$X_25a,9,f10$X_25c,
            f10$X_26a,9,f10$X_26c,f10$X_27,
            #f10$X_28a2,f10$X_28b2,f10$X_28c,f10$X_28d2,f10$X_28e2,
            f10$X_32a,f10$X_32b,f10$X_33,
            f10$X_35a,f10$X_35c,f10$X_36a,f10$X_36b,f10$X_37a,f10$X_37b,f10$X_38a,f10$X_39)

col_name=unlist(lapply(f3use[1,],as.character))
attributes(col_name) <- NULL
f1use=f1use[-1,]
f2use=f2use[-1,]
f3use=f3use[-1,]
f4use=f4use[-1,]
f5use=f5use[-1,]
f6use=f6use[-1,]
f7use=f7use[-1,]
f8use=f8use[-1,]
f9use=f9use[-1,]
f10use=f10use[-1,]
colnames(f1use)=col_name
colnames(f2use)=col_name
colnames(f3use)=col_name
colnames(f4use)=col_name
colnames(f5use)=col_name
colnames(f6use)=col_name
colnames(f7use)=col_name
colnames(f8use)=col_name
colnames(f9use)=col_name
colnames(f10use)=col_name

data=rbind(f1use,f2use,f3use,f4use,f5use,f6use,f7use,f8use,f9use,f10use)
colnames(data) <- c('Year',"Borough","Sub_Borough","Type_of_Schedule","Year_Built",
                   "Household_Composition","Number_of_Persons","Rent","Household_Income",
                   "Length_of_Lease","Sex","Age",#"Race",
                   "Recent_Place_Lived", "Year_Moved","First_Occupants","Reason_for_Moving","Householder_Birth",                                                    
                   "Father_Birth","Mother_Birth","Value","Mortgage_Status","Mortgage",                                                  
                    "Condition of Exterior Walls: Missing brick, siding, or other outside wall materi",
                    "Condition of Exterior Walls: Sloping or bulging outside walls",                   
                    "Condition of Exterior Walls: Major cracks in outside walls",                      
                    "Condition of Exterior Walls: Loose or hanging cornice, roofing, or other materia",
                    "Condition of Exterior Walls: None of these problems with walls",                  
                    "Condition of Exterior Walls: Unable to observe walls",                            
                    "Condition of Windows: Broken or missing windows",                                 
                    "Condition of Windows: Rotten/loose windows",                                      
                    "Condition of Windows: Boarded up windows",                                        
                    "Condition of Windows: None of these problems with windows",                       
                    "Condition of Windows: Unable to observe windows",                                 
                    "Condition of Stairways (Exterior and Interior): Loose, broken, or missing stair",
                    "Condition of Stairways (Exterior and Interior): Loose, broken, or missing steps", 
                    "Condition of Stairways (Exterior and Interior): None of these problems with stair",
                    "Condition of Stairways (Exterior and Interior): No interior steps or stairways",  
                    "Condition of Stairways (Exterior and Interior): No exterior steps or stairways",  
                    "Condition of Stairways (Exterior and Interior): Unable to observe stairways",     
                    "Condition of floors: Sagging or sloping floors",                                 
                    "Condition of floors: Slanted or shifted doorsills or door frames",                
                    "Condition of floors: Deep wear in floors causing depressions",                    
                    "Condition of floors: Holes or missing flooring",                                  
                    "Condition of floors: None of these problems with floors",                         
                    "Condition of floors: Unable to observe floors",                                   
                    "Condition of building",                                                           
                    "Any buildings with broken or boarded up windows",                                 
                    "Wheelchair Access - Street",                                                      
                    "Wheelchair Access - Elevator",                                                    
                    "Wheelchair Access - Residence",                                                   
                    "Number of Units in Building",                                                     
                    "Owner in building",                                                               
                    "Stories in building",                                                             
                    "Passenger elevator in building",                                                  
                    "Sidewalk to elevator without using steps",                                        
                    "Sidewalk to unit without using steps",                                            
                    "Number of rooms",                                                                 
                    "Number of bedrooms",                                                              
                    "Complete plumbing facilities",                                                    
                    "Exclusive use of plumbing facilities",                                            
                    "Toilet breakdowns",                                                               
                    "Kitchen facilities",                                                              
                    "Exclusive use of kitchen facilities",                                             
                    "Kitchen facilities functioning",                                                  
                    "Type of heating fuel",                                                            
                    "Heating equipment breakdown",                                                     
                    "Number of heating equipment breakdowns",                                          
                    "Additional source(s) of heat",                                                    
                    "Presence of mice or rats",                                                        
                    "Exterminator service",                                                            
                    "Cracks of holes in interior walls",                                               
                    "Holes in floors",                                                                 
                    "Broken plaster or peeling paint on inside walls",                                 
                    "Broken plaster or peeling paint large",                          
                    "Water leakage inside",                                          
                    "Respondent rating of residential structures in neighborhood")

write.csv(data,file="data.csv",row.names = F)
