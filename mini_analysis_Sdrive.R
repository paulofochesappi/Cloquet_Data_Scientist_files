


library(readxl)
library(data.table)
library(magrittr)
library(dplyr)
library(arules)

Loop_1 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/IBR Production Dates - Filterabilty Values and CLQ Batches Data 20231218.xlsx", sheet = 2)[,c(1,2,3,4)]; 
Loop_1 = cbind(Loop_1, "Loop_1"); colnames(Loop_1) <- NULL

Loop_2 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/IBR Production Dates - Filterabilty Values and CLQ Batches Data 20231218.xlsx", sheet = 5, skip = 1)[,c(1,2,3,4)]; 
Loop_2 = cbind(Loop_2, "Loop_2"); colnames(Loop_2) <- NULL

combined = rbindlist(list(Loop_1, Loop_2))
colnames(combined) = c("IBR Production Date", "KW", "Batch ID", "CLQ Production Date", "Loop")
combined

rm(Loop_1, Loop_2)


# file_1 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/A to Z.xlsx", sheet = 1)[,c(1:26)]; 
# file_2 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/AA to AZ.xlsx", sheet = 1)[,c(2:27)];
# file_3 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/BA to BZ.xlsx", sheet = 1)[,c(2:27)];
# file_4 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/CA to CZ.xlsx", sheet = 1)[,c(2:27)];
# file_5 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/DA to DZ.xlsx", sheet = 1)[,c(2:27)];
# file_6 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/EA to EZ.xlsx", sheet = 1)[,c(2:27)];
# file_7 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/FA to FZ.xlsx", sheet = 1)[,c(2:27)];
# file_8 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/GA to GZ.xlsx", sheet = 1)[,c(2:27)];
# file_9 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/HA to HZ.xlsx", sheet = 1)[,c(2:27)];
# file_10 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/IA to IZ.xlsx", sheet = 1)[,c(2:27)];
# file_11 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/JA to JZ.xlsx", sheet = 1)[,c(2:27)];
# file_12 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/KA to KZ.xlsx", sheet = 1)[,c(2:27)];
# file_13 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/LA to LZ.xlsx", sheet = 1)[,c(2:27)];
# file_14 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/MA to MZ.xlsx", sheet = 1)[,c(2:27)];
# file_15 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/NA to NZ.xlsx", sheet = 1)[,c(2:27)];
# file_16 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/OA to OF.xlsx", sheet = 1)[,c(2:7)];
# 
# 
# bc_data = cbind(file_1,file_2,file_3,file_4,file_5,file_6,file_7,file_8,file_9,file_10,file_11,file_12,file_13,file_14,file_15,file_16)
# 
# rm(file_1,file_2,file_3,file_4,file_5,file_6,file_7,file_8,file_9,file_10,file_11,file_12,file_13,file_14,file_15,file_16)




bc_data = read_excel("C:/Users/PCOfoche/Desktop/fullstendig/mini_analysis.xlsx", sheet = 1)[,c(1:6)]



unified <- combined %>% inner_join(bc_data, by=c('Batch ID'))

rm(combined, bc_data)

unified$Loop[unified$Loop=="Loop_1"] <- 1;  unified$Loop[unified$Loop=="Loop_2"] <- 2
unified$Loop = as.numeric(unified$Loop)


sum(is.na(unified))  # 4030975
(nrow(unified) * (ncol(unified)-6)) # 16900236
( sum(is.na(unified)) / (nrow(unified) * (ncol(unified)-6)) ) * 100  # 23.85159 %
#count NA values in each column
sapply(unified, function(x) sum(is.na(x)))
nrow(unified) # 42894


# 530-TIC-238.PV | D0 DIL STATIC MIX TEMP
# 530-TIC-240.PV | D0 PRS FIL TEMP
# 546FIQ610.PRESS_TEMP_COMP.PV | Steam flow to dryer.
# 1022 | MBL REA
# 1023 | Impreg REA
# 1042 | WL EA
# 1044 | WL Sulf
# 1055 | 2nd PO COD                                                                            1056 | 2nd Stage COD
# 1074 | OWL Sulfidity                                                                           1107 | D0 Tower In pH
# 1110 | D0 Out Residual ClO2                                                                          1108 | D0 Tower Out pH
# 1112 | D1 Tower In pH                                                                          1114 | D1 Tower Out pH 
# 1117 | D1 QuikBrite                                                                          1121 | EOP Tower In pH 
# 1122 | EOP Tower Out pH                                                                            1124 | EOP QuikBrite 
# 1151 | CLO2 Strength to Bleach Plant                                                                 1153 | CLO2 Strength to Storage 
# 1154 | Generator Chlorate Molarity                                                               1156 | Chlorate Mix Tank Molarity 
# 1591 | CLO2 Strength to Storage DCS PV                                                                     1673 | Pre O2 Kappa Mettler 
# 1674 | Pre O2 Kappa R1                                                                    1675 | Post O2 Kappa Mettler 
# 1677 | Pre Bleach Kappa Mettler                                                                                
# 2187 | C13 Total Accepts                                                                 2248 | Overlength (>45mm) Flail 
# 2249 | Overthick (>8mm) Flail                                                                   2250 | Accept (6mm-8mm) Flail 
# 2252 | Pins (<2mm Thick) Flail                                                                       2255 | Bark Content Flail 
# 2670 | CLO2 Strength to Bleach Plant DCS PV                                                      2902 | R10 Cooling tower chlorine residual 
# 2915 | Total Ash                                                                          2916 | Acid Insolubles 
# 2917 | Calcium                                                                    2921 | Alkali Solubility S10 
# 2922 | Alkali solubility S18                                                                    2923 | Alkali Resistance R10 
# 2924 | Alkali Resistance R18                                                                        2925 | Extractives_Resin 
# 3041 | Ash -                                                                     3044 | NL REA Transfer Pump 
# 3059 | 2nd PO Viscosity                                                                      3060 | 2nd Stage Viscosity 
# 3065 | Chip Moisture C10                                                                        3066 | Chip Moisture C11 
# 3068 | Pre Bleach Viscosity                                                                         3079 | Post O2 Kappa R2 
# 3080 | Pre O2 Kappa R2                                                                      3094 | Pre Bleach Kappa R2 
# 3104 | 1PO Filtrate pH                                                        3259 | BP Cooling towr chlorine residual 
# 3263 | Prebleach COD                                                               3513 | North Bleached Cons Actual 
# 3514 | North Bleached Cons PV                                                               3515 | South Bleached Cons Actual 
# 3516 | South Bleached Cons PV                                                                                3520 | WW pH Lab 
# Accept (2mm-4mm) C13 | 10-C13-4mm.BT
# Accept (4mm-6mm) C13 | 10-C13-6mm.BT                                                                         Acid+ClO2 Dosage (CALC) 
# Bale Moisture SPREAD (CALC)
# Bark Content Post-CTS | 10-BS-Bark.BT                                                                                         
# BLP_530_AC_271.PV on D0 | CLO2 CONC                                                                 BLP_530_AI_206.PV | D0 RES CLO2 
# BLP_530_AI_273.PV | pH                                                                   BLP_530_AI_387.PV | H2O2 CONC 
# BLP_530_AI_425H.PV | D1 RES CLO2                                                        BLP_530_AI_2114.PV | OZONE CONCENTRATION 
# BLP_530_AI_2190.PV | PH DOSING OZONE LRC                                                           BLP_530_AI_2445.PV | O3 COMPRESSOR PH 
# BLP_530_AI_2577.PV | O3 CONC GEN OUTLET                                                                  BLP_530_AIC_207.PV | D0 MIX PH 
# BLP_530_AIC_307.PV | EOP PH CNTRL                                                      BLP_530_AIC_425.PV | D1 SO2/ClO2 AFTER TWR 
# BLP_530_AIC_957.PV | D0PR WASH WATER pH                                                     BLP_530_AIC_957.PV_Raw | D0PR WASH WATER pH 
# BLP_530_AIC_2225.PV | Z Feed pH                                                    BLP_530_CI_142.PV | PB FILTRATE CONDUCTIVITY 
# BLP_530_CV_2143.PV | STEAM VALVE                                                              BLP_530_CV_2162.PV | MAKE UP VALVE 
# BLP_530_CV_2455.PV | OZON TANK LEVEL                                                          BLP_530_CV_2483.PV | 1st O2 COMP VALVE 
# BLP_530_CV_2499.PV | O2 CONTROL VALVE                                                                BLP_530_CV_2503.PV | COD HEAT EX 
# BLP_530_CV_2514.PV | 2ND O2 COMPRESSOR Drain Valve                                                           BLP_530_CV_2530.PV | O2 CONTROL VALVE 
# BLP_530_CV_2531.PV | 2ND O2 Comp to COD                                                               BLP_530_EOPGRADE.PV | SPEC SW EOP 
# BLP_530_FF_235.NaOH_CONC.PV | NAOH CONC                                                                 BLP_530_FF_235.PV | NAOH DOSAGE 
# BLP_530_FF_271.BCS0_CLO2 | D0 KF Total ClO2                                                        BLP_530_FF_271.CLO2_OP_BIAS | D0 KF Bias 
# BLP_530_FF_271.KAPPA_IN.PV | Kappa before ClO2 stages                                                                 BLP_530_FF_271.OP | CLO2 DOSAGE 
# BLP_530_FF_271.PV | CLO2 DOSAGE                                                                 BLP_530_FF_271.SP | CLO2 DOSAGE 
# BLP_530_FF_335.PV | NAOH DOSAGE                                                                   BLP_530_FF_371.PV | O2 DOSAGE 
# BLP_530_FF_471.BCS1_CLO2 | D1 KF Total ClO2                                                        BLP_530_FF_471.CLO2_OP_BIAS | D0 KF Bias 
# BLP_530_FF_471.OP | CLO2 DOSAGE                                                                 BLP_530_FF_471.PV | CLO2 DOSAGE 
# BLP_530_FF_471.SP | CLO2 DOSAGE                                                                 BLP_530_FF_765.PV | H2O2 DOSAGE 
# BLP_530_FF_953.PV | ACID DOSAGE - PV                           BLP_530_FFIC221H.PV | Total WATER>D0PR WASH WATER HEADER - Calculated 
# BLP_530_FFIC2207.PV | Ozone Flow to Mixer #2                                                    BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1 
# BLP_530_FFIC_131.PV | PRBL DIL CNV                                                              BLP_530_FFIC_135.PV | ACID>PB CNVR 
# BLP_530_FFIC_135.Ratio_PV | Acid to Prebleach Conveyor Dosage                                        BLP_530_FFIC_221.PV | WHITE WATER>D0PR WASH WATER HEADER 
# BLP_530_FFIC_222.PV | FLT>D0 TWRDL                                                              BLP_530_FFIC_235.PV | NAOH>D0 DLCV 
# BLP_530_FFIC_271.OP | CLO2> D0 MIX                                                              BLP_530_FFIC_271.PV | CLO2> D0 MIX 
# BLP_530_FFIC_271.SP | CLO2> D0 MIX                                                               BLP_530_FFIC_322.PV | FLT>EOP TWR 
# BLP_530_FFIC_327.PV | HW>EOP PRS                                                                BLP_530_FFIC_328.PV | HW>EOP PRS 
# BLP_530_FFIC_329.PV | HW>EOP PRS                                                              BLP_530_FFIC_371.PV | O2> EO MIXER 
# BLP_530_FFIC_424.PV | SODBSF>D1TWR                                                               BLP_530_FFIC_473.OP | CLO2 > D1MX 
# BLP_530_FFIC_473.PV | CLO2 > D1MX                                                               BLP_530_FFIC_473.SP | CLO2 > D1MX 
# BLP_530_FFIC_630.PV | SW BLHD CAUSTIC                                                     BLP_530_FFIC_711.OP on D0 | D0 FILT<CLO2 HX 
# BLP_530_FFIC_711.PV on D0 | D0 FILT<CLO2 HX                                                              BLP_530_FFIC_953.PV | ACID>EOP DCV 
# BLP_530_FI_385.PV | H2O2 > EOP                                                       BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW 
# BLP_530_FI_2208.PV | Ozone to #1 Mixer Flow                                                  BLP_530_FI_2222.PV | Z-stage Reactor Feed Flow 
# BLP_530_FI_2408.PV | 1ST O2 COMP COOLING WATER                                                          BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR 
# BLP_530_FY_271.PV | D0 KAPPA CONTROL                                           BLP_530_LI_615A.TONS.PV | SW Bleached Tower Inventory 
# BLP_530_LI_645A.TONS.PV | HW Bleached Tower Inventory                                                BLP_530_LI_2203A.PV | NUCLEAR ZSTG BLOW TUBE LVL 
# BLP_530_LI_2203B.PV | PRESSURE ZSTG BLOW TUBE LVL                                    BLP_530_LIC_675.TONS.PV | Transtion Bleached Tower Inventory 
# BLP_530_PDI_2204.PV | DIFFERENTIAL PRESSURE BETWEEN OZONE MIXER #2 and GAS AND STOCK FEED                                                  BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS 
# BLP_530_PI_2204.PV | MIXER 2 OZONE GAS SUPPLY PRESSURE                                                  BLP_530_PI_2206.PV | OZONE GAS SUPPLY PRESSURE 
# BLP_530_PI_2214.PV | ZSTG REACT PRESSURE                                                        BLP_530_PI_2215.PV | ZSTG REACT PRESSURE 
# BLP_530_PI_2216.PV | ZSTG REACT PRESSURE                                                 BLP_530_PI_2236.PV | ZSTG BLW TB EXH TO 03 SCBR 
# BLP_530_PI_2237.PV | gas pressure to COD                                               BLP_530_PI_2297.PV | ZSTG HP SEAL WTR PMP DISCHRG 
# BLP_530_PI_2417.PV | 2nd O2 COMP PRESSURE                                                            BLP_530_PI_2428.PV | 2ND O2 PRESSURE 
# BLP_530_PIC_150.PV | PRBL HYD DRV                                                               BLP_530_PIC_250.PV | D0PR HYD DRV 
# BLP_530_PIC_350.PV | EOP HYD DRV                                                               BLP_530_PIC_450.PV | D1PR HYD DRV 
# BLP_530_PIC_2205.PV | ZSTG REACT STCK BFOR 1MIX                                                            BLP_530_SIC_2210.PV | ZSTG FEED PUMP 
# BLP_530_TI_153.PV | Pre-Bleach Press Oil Temp                                                                BLP_530_TI_474.PV | CLO2> D1 MIX 
# BLP_530_TIC_130.PV | PB DLN HT                                                                 BLP_530_TIC_205.PV | D0 MIX DCH 
# BLP_530_TIC_238.PV | D0 DIL STATIC MIX TEMP                                                            BLP_530_TIC_240.PV | D0 PRS FIL TEMP 
# BLP_530_TIC_308.PV | 160#>EOPMIX                                                                BLP_530_TIC_330.PV | STM>EOP HTR 
# BLP_530_VI_608.PV | BLEACHED HD MC PMP VIB                                                  BLP_546_FIC_005.PV | STH BLCH HD TO PULP DRYER 
# BLP_546_FIC_015.PV | NTH BLCH HD TO PULP DRYER                                                   BLP_546_FIC_027.PV | BLCH STOCK TO PULP DRYER 
# BLP_546_PIC_006.PV | STH BLCH HD TO PULP DRYER                                                  BLP_546_PIC_016.PV | NTH BLCH HD TO PULP DRYER 
# C13 Accepts | 10-C13-ACC.BT                                           CHIP MOISTURE ASPEN | DIG_COMM_TABLE.COMT.CHF_ASP_MST 
# CHIP MOISTURE MAPLE | DIG_COMM_TABLE.COMT.CHF_MAP_MST                                                                
# D0 Tower In VISCOSITY CALCULATED OFF PB KAPPA, Z-STAGE DOSAGE, AND TEMP | BLP_530_D0_VISC.In.PV                                        D1 ClO2 residual quenched based on Bisulfite Dose (CALC) 
# D1 H2O2 residual quenched based on Bisulfite Dose (CALC)                                                                            D1 Out Residual ClO2 
# DIG_CHIP_FEED.PCT_ASPEN                                                                         DIG_CHIP_FEED.PCT_MAPLE 
# DIGESTER ID                                                  Dxx_CHF_TOTALS.BWI_C13 | DIGx C-13 Belt Weight 
# Dxx_CHF_TOTALS.PCT_ASPEN                                                                        Dxx_CHF_TOTALS.PCT_MAPLE 
# Dxx_CO_TOTALS.PRI_RETURN                                                                        Dxx_CO_TOTALS.SEC_RETURN 
# Dxx_DCH_TOTALS.FQ_3158                                                    Dxx_DPL_TIMER.OPT | DIG x DISPLACEMENT TIMER 
# Dxx_DPL_TOTALS.HEAT_TRANSFER | DIGx DPL Heat Transfer                                                                           Dxx_ILF_TOTALS.FQ_067 
# Dxx_ILF_TOTALS.FQ_096                                                                          Dxx_ILF_TOTALS.FQ_2113 
# Dxx_ILF_TOTALS.FQ_2501                                                                          Dxx_ILF_TOTALS.FQ_2502 
# Dxx_ILF_TOTALS.NL_RETURN                                                                  Dxx_NE_TOTALS.NE_MBL_ALKALI.PV 
# Dxx_PH_TOTALS.FQ_140 | DIGx 140# Steam Total                                                    Dxx_PH_TOTALS.FQ_3158 | DIGx 55# Steam Total 
# Dxx_SEQ_NE_TIME (CALC)                                                              Dxx_SEQ_TIMES.PREHYDROLYSIS_Dur.PV 
# Dxx_SEQ_TIMES.Step0_Dur.PV                                                                      Dxx_SEQ_TIMES.Step1_Dur.PV 
# Dxx_SEQ_TIMES.Step2_Dur.PV                                                                      Dxx_SEQ_TIMES.Step3_Dur.PV 
# Dxx_SEQ_TIMES.Step4_Dur.PV                                                                      Dxx_SEQ_TIMES.Step5_Dur.PV 
# Dxx_SEQ_TIMES.Step6_Dur.PV                                                                      Dxx_SEQ_TIMES.Step7_Dur.PV 
# Dxx_SEQ_TIMES.Step8_Dur.PV                                                                      Dxx_SEQ_TIMES.Step9_Dur.PV 
# Dxx_SEQ_TIMES.Step10_Dur.PV                                                                     Dxx_SEQ_TIMES.Step11_Dur.PV 
# Dxx_SEQ_TIMES.Step12_Dur.PV                                                                     Dxx_SEQ_TIMES.Step13_Dur.PV 
# Dxx_SEQ_TIMES.Step14_Dur.PV                                                                     Dxx_SEQ_TIMES.Step15_Dur.PV 
# Dxx_SEQ_TIMES.Step16_Dur.PV                                                                     Dxx_SEQ_TIMES.Step17_Dur.PV 
# Dxx_SEQ_TIMES.Step18_Dur.PV                                                                     Dxx_SEQ_TIMES.Step19_Dur.PV 
# Dxx_SEQ_TIMES.Step20_Dur.PV                                                                     Dxx_SEQ_TIMES.Step21_Dur.PV 
# Dxx_SEQ_TIMES.Step22_Dur.PV                                                                     Dxx_SEQ_TIMES.Step23_Dur.PV 
# Dxx_SEQ_TIMES.Step24_Dur.PV                                                                     Dxx_SEQ_TIMES.Step25_Dur.PV 
# Dxx_SEQ_TIMES.Step26_Dur.PV                                                                     Dxx_SEQ_TIMES.Step27_Dur.PV 
# Dxx_SEQ_TIMES.Step28_Dur.PV                                           Dxx_UCM.RECIPE.CHF_BD_W_A | CHP BONE DRY WGT ALK CALC 
# Dxx_UCM.RECIPE.CO_CLP_AC | CO LIQ ALKALI CONC                                            Dxx_UCM.RECIPE.CO_MINAD_LB | CO MIN ALKALI TO DIG LB 
# Dxx_UCM.RECIPE.CO_MINAD_PCT | CO MIN ALKALI TO DIG %                                               Dxx_UCM.RECIPE.ILF_MBL_AC | ILF-W MBL ALKALI CONC 
# Dxx_UCM.RECIPE.NE_MIN_A_LB | NE MIN ADD ALK TO DIG LB                                           Dxx_UCM.RECIPE.NE_MIN_A_PCT | NE MIN ADD ALK TO DIG % 
# Dxx_UCM.RECIPE.NE_TA_LB | NE TOTAL ALKALI LB                                                    Dxx_UCM.RECIPE.NE_TA_PCT | NE TOTAL ALKALI % 
# Dxx_VPS_TABLE.ACTUAL.CHF_WT                                                   Dxx_VPS_TABLE.ACTUAL.CO_HF | DIGx CO H FACTOR 
# Dxx_VPS_TABLE.ACTUAL.CO_HFTRA_DIG | DIGx CO TEMP                                                                 Dxx_VPS_TABLE.ACTUAL.CO_MBL_RTN 
# Dxx_VPS_TABLE.ACTUAL.CO_MIN_A                                 Dxx_VPS_TABLE.ACTUAL.CO_MIN_A_LB | DIGx CO MIN ALKALI TO DIG LB 
# Dxx_VPS_TABLE.ACTUAL.CO_MIN_VOL | DIGx CO LIQ MIN VOL                                                               Dxx_VPS_TABLE.ACTUAL.CO_NL_MAXRTN 
# Dxx_VPS_TABLE.ACTUAL.DCH_DPL_VOL | DIGx DCH DILU LIQ VOL                                                                Dxx_VPS_TABLE.ACTUAL.DPL_HBL_RTN 
# Dxx_VPS_TABLE.ACTUAL.DPL_MBL_RTN                                                                 Dxx_VPS_TABLE.ACTUAL.DPL_NL_RTN 
# Dxx_VPS_TABLE.ACTUAL.DPL_VOLUME | DIGx DPL LIQ VOL                                                                Dxx_VPS_TABLE.ACTUAL.NE_CO_NLRTN 
# Dxx_VPS_TABLE.ACTUAL.NE_HRV | DIGx NE HYDROLYSTATE REM VOL                                                                  Dxx_VPS_TABLE.ACTUAL.NE_MBL_AC 
# Dxx_VPS_TABLE.ACTUAL.NE_MIN_AA                                                               Dxx_VPS_TABLE.ACTUAL.NE_MIN_AA_LB 
# Dxx_VPS_TABLE.ACTUAL.NE_MIN_WL | DIGx NE ADD WL VOL                                                                  Dxx_VPS_TABLE.ACTUAL.NE_NL_RTN 
# Dxx_VPS_TABLE.ACTUAL.NE_TA                                         Dxx_VPS_TABLE.ACTUAL.NE_TA_LB | DIGx NE TOTAL ALKALI LB 
# Dxx_VPS_TABLE.ACTUAL.NE_TIME                                             Dxx_VPS_TABLE.ACTUAL.NE_TLV | DIGx NE TOTAL LIQ VOL 
# Dxx_VPS_TABLE.ACTUAL.PH_PF | DIGx P FACTOR                               Dxx_VPS_TABLE.TARGET-ACTUAL.NE_TLV | DIGx NE TOTAL LIQ VOL (CALC) 
# Dxx_VPS_TABLE.TARGET.CHF_WT                                                   Dxx_VPS_TABLE.TARGET.CO_HF | DIGx CO H FACTOR 
# Dxx_VPS_TABLE.TARGET.CO_MIN_A_LB | DIGx CO MIN ALKALI TO DIG LB                                           Dxx_VPS_TABLE.TARGET.CO_MIN_VOL | DIGx CO LIQ MIN VOL 
# Dxx_VPS_TABLE.TARGET.DPL_VOLUME | DIGx DPL LIQ VOL                                             Dxx_VPS_TABLE.TARGET.NE_MIN_WL | DIGx NE ADD WL VOL 
# Dxx_VPS_TABLE.TARGET.NE_TA_LB | DIGx NE TOTAL ALKALI LB                                             Dxx_VPS_TABLE.TARGET.NE_TLV | DIGx NE TOTAL LIQ VOL 
# Dxx_VPS_TABLE.TARGET.PH_PF | DIGx P FACTOR                                                                      Dxx_VPS_TABLE.TARGET.WL_AC 
# Flail Total Accepts | 10-PC-ACC.bt                                                               H factor + P factor actual (Calc) 
# H factor + P factor target (Calc)                                                                             
# HWL TO NL ACC CIRC | DIG_521_FFIC2410.PV                                                        HWL TO PRI HBL ACC | DIG_521_FFIC_095.PV 
# IQS Caustic Feed | 562-FIC-786.PV                                                                                
# Overlength(>45mm) Flail | 10-PC-45mm.BT                                                          Overthick (>8mm) Flail | 10-PC-10mm.BT 
# P Factor Actual - Target (CALC)                                                          Pins (<2mm thick) Flail | 10-PC-3mm.BT 
# PRE BL MICRO KAPPA TO VISCOSITY CORRECTED TO LAB TEST | BLP_530_PB_VISC.PV                                            PRE BL MICRO KAPPA TO VISCOSITY | WSH_530_AI_506B.PV 
# Pre Bleach Viscosity Lab Test  | 30-PB-visc.bt                                                        Sodium Bisulfite D1 Dose (lb/ton) (CALC) 
# Sodium Bisulfite total Dose (lb/ton) (CALC)                                           Total D1 ClO2 residual based on Bisulfite Dose (CALC) 
# Total D1 H2O2 residual based on Bisulfite Dose (CALC)                                                                      
# WSH_525_AI_806A.PV | Pre O2 Kappa Anlyzr                                                       WSH_525_AI_876A.PV | Post O2 Kappa Anlyzr 
# WSH_525_CI_487.PV | 2POW Filtrate Conductivity                                                         WSH_525_FFIC_154.DILPV | DILN ADJUST PV 
# WSH_525_FFIC_304.DILPV | DILN ADJUST PV                                        WSH_525_FFIC_341.RATIO_CALC.PV | OXID WL>2STG RATIO CALC 
# WSH_525_FFIC_354.DILPV | DILN ADJUST PV                       WSH_525_FFIC_405.RATIO_CALC.PV | BP FILT/H WRT TO 2ND POW DIL CONVYR CALC 
# WSH_525_FFIC_406.DILPV | DILN FACTOR PV                                          WSH_525_FFIC_852.RATIO_CALC.PV | O2% DOSAGE RATIO CALC 
# WSH_525_FFIC_903. RATIO_SP.PV | O3% DOSAGE RATIO SP                                          WSH_525_FFIC_903.RATIO_CALC.PV | O3% DOSAGE RATIO CALC 
# WSH_525_FIBERLENGTH_POST02.PV | POST O2 KAPPA FIBER LEN                                           WSH_525_FIBERLENGTH_PRE02.PV | PRE O2 KAPPA FIBER LEN 
# WSH_525_PIC_168.PV | 1STG HYD DRV PV                                                             WSH_525_PIC_318.PV | 2STG HYDR BIAS 
# WSH_525_PIC_368.PV | 1POW HYDR BIAS                                                             WSH_525_PIC_418.PV | 2POW HYDR BIAS 
# WSH_525_TI_161.PV | 1STG LVL FIL PV                                                                WSH_525_TI_311.PV | 2STG LVL FIL 
# WSH_525_TI_343.PV | 1POW Filtrate Temp                                                             WSH_525_TI_361.PV | 1POW LVL FIL PV 
# WSH_525_TI_411.PV | 2POW LVL FIL PV                                                               WSH_530_AI_506A.PV | PRE BL KAPPA 



#write.csv(unified, file = "raw_unified.csv")

# raw_unified = read.csv("raw_unified.csv")[,-c(1,2,4,7,24,47,49,54,55,56,66,67,69,70,71,72,73,74,76,77,78,79,80,81,82,83,98,99,100,101,102,103,104,105,107,108,109,110,111,112,134,145,154,158,168,
#                                               204,206,226,227,254,257,258,259,260,261,262,278,295,297,298,299,301,302,303,304,305,309,332,333,334,335,337,338,339,340,350,388)]



raw_unified = unified[, c("CLQ Production Date", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS",
                          "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", "BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "Loop", "KW")]


raw_unified = as.data.frame(raw_unified)


nrow(raw_unified)
ncol(raw_unified)


raw_with_NAs = raw_unified


raw_unified = na.omit(raw_unified)
nrow(raw_unified)

raw_for_date_plots = raw_unified[, c("CLQ Production Date", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS",
                                     "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", "BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "Loop", "KW")]


raw_iris = raw_for_date_plots[,-c(1)]   # removing Batch_Production_Date

raw_iris = sapply(raw_iris, as.numeric)


# raw_iris = raw_iris[which(iris[,"BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1"] >= 0),]

iris = raw_iris

# "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FFIC2208.dosage_PV"


set.seed(1); irisDisc1 <- (discretize(iris[,1], method = "cluster", breaks = 10,
                                      labels = c("x1_1", "x1_2","x1_3","x1_4","x1_5","x1_6","x1_7","x1_8","x1_9","x1_10")))
set.seed(1); irisDisc2 <- (discretize(iris[,2], method = "cluster", breaks = 10,
                                      labels = c("x2_1", "x2_2","x2_3","x2_4","x2_5","x2_6","x2_7","x2_8","x2_9","x2_10")))
set.seed(1); irisDisc3 <- (discretize(iris[,3], method = "cluster", breaks = 10,
                                      labels = c("x3_1", "x3_2","x3_3","x3_4","x3_5","x3_6","x3_7","x3_8","x3_9","x3_10")))
set.seed(1); irisDisc4 <- (discretize(iris[,4], method = "cluster", breaks = 10,
                                      labels = c("x4_1", "x4_2","x4_3","x4_4","x4_5","x4_6","x4_7","x4_8","x4_9","x4_10")))
set.seed(1); irisDisc5 <- (discretize(iris[,5], method = "cluster", breaks = 2,
                                      labels = c("y1_1", "y1_2")))
set.seed(1); irisDisc6 <- (discretize(iris[,6], method = "cluster", breaks = 10,
                                      labels = c("y2_1", "y2_2","y2_3","y2_4","y2_5","y2_6","y2_7","y2_8","y2_9","y2_10")))





irisDisc <- cbind.data.frame(irisDisc1, irisDisc2, irisDisc3, irisDisc4, irisDisc5, irisDisc6)


library(data.table)
targets_y1 = c("y1_1"); 
targets_y2 = c("y1_2"); 
targets_y3 = c("y2_1","y2_2","y2_3","y2_4"); 
targets_y4 = c("y2_1");



rbind(CJ(targets_y1, targets_y3), CJ(targets_y2, targets_y4), use.names=FALSE)  -> targets_raw; targets_raw <- as.data.frame(targets_raw)
cbind.data.frame(as.character(irisDisc5), as.character(irisDisc6)) -> target_variable















write.csv(irisDisc, file = "irisDisc.csv")                    
dataset = read.transactions('irisDisc.csv', sep = ',', rm.duplicates = TRUE)




f <- function(x) {
  if(is.list(x) ) lapply(x,f)
  else ifelse(length(x) == 0 | typeof(x)=="double", 0, x)
}





overall <- function(x) {
  for (i in x) {
    
    
    y =  rbind(
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[1], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[11], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[12]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[2], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[12], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[13]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[3], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[13], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[14]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[4], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[14], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[15]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[5], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[15], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[16]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[6], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[16], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[17]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[7], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[17], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[18]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[8], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[18], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[19]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[9], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[19], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[20]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[10], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[20], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[21])
    )
    colnames(y) <- c("Level", "Minimum", "Maximum")
    return(
      y
    )
    
  }
  
}





overall_2 <- function(x) {
  for (i in x) {
    
    y =  rbind(
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[1], attr(x = i, which = "discretized:breaks")[1], attr(x = i, which = "discretized:breaks")[2]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[2], attr(x = i, which = "discretized:breaks")[2], attr(x = i, which = "discretized:breaks")[3]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[3], attr(x = i, which = "discretized:breaks")[3], attr(x = i, which = "discretized:breaks")[4]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[4], attr(x = i, which = "discretized:breaks")[4], attr(x = i, which = "discretized:breaks")[5]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[5], attr(x = i, which = "discretized:breaks")[5], attr(x = i, which = "discretized:breaks")[6]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[6], attr(x = i, which = "discretized:breaks")[6], attr(x = i, which = "discretized:breaks")[7]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[7], attr(x = i, which = "discretized:breaks")[7], attr(x = i, which = "discretized:breaks")[8]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[8], attr(x = i, which = "discretized:breaks")[8], attr(x = i, which = "discretized:breaks")[9]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[9], attr(x = i, which = "discretized:breaks")[9], attr(x = i, which = "discretized:breaks")[10]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[10], attr(x = i, which = "discretized:breaks")[10], attr(x = i, which = "discretized:breaks")[11])
    )
    colnames(y) <- c("Level", "Minimum", "Maximum")
    return(
      y
    )
  }
}

overall_2(irisDisc[4])

# attr(x = irisDisc4, which = "discretized:breaks")[1]

length(irisDisc4)





settings <- function(i) {
  h = as.data.frame(cbind(overall(irisDisc[i]), colnames(iris[i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}




settings_2 <- function(i) {
  h = as.data.frame(cbind(overall_2(irisDisc[i]), colnames(iris)[i]))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}




finallist = c()
overall_settings <- function(mylist) {
  for (i in as.numeric(mylist)) {
    finallist <- rbind(finallist,settings(i))
  }
  return (finallist)
}




library(ggplotify)
library(gridExtra)




nrow(iris)
Loop_1_total = length(which(iris[,"Loop"] == 1))
Loop_2_total = length(which(iris[,"Loop"] == 2))
Loop_1_GOOD = length(which(iris[,"Loop"] == 1 & iris[,"KW"] < 257.6))
Loop_2_GOOD = length(which(iris[,"Loop"] == 2 & iris[,"KW"] < 154.3))
Loop_1_BAD = length(which(iris[,"Loop"] == 1)) - length(which(iris[,"Loop"] == 1 & iris[,"KW"] < 257.6))
Loop_2_BAD = length(which(iris[,"Loop"] == 2)) - length(which(iris[,"Loop"] == 2 & iris[,"KW"] < 154.3))
(Loop_1_GOOD + Loop_2_GOOD) / (Loop_1_total + Loop_2_total)
nrow(iris) == (Loop_1_total + Loop_2_total)


# Note that a new column(s) can also be created for discretizing the data according the specific intervals of interest


# sum(do.call(paste0, targets_raw) %in% do.call(paste0, target_variable))   # need to be swapped
sum(do.call(paste0, target_variable) %in% do.call(paste0, targets_raw))
nrow(target_variable)

# green_gen = sum(do.call(paste0, targets_raw) %in% do.call(paste0, target_variable)) / nrow(target_variable)   # need to be swapped
green_gen = sum(do.call(paste0, target_variable) %in% do.call(paste0, targets_raw)) / nrow(target_variable)
red_gen = 1 - green_gen




library(prodlim)

















green_perc_x1_1 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw))))
red_perc_x1_1 = 1 - green_perc_x1_1

green_perc_x1_2 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw))))
red_perc_x1_2 = 1 - green_perc_x1_2

green_perc_x1_3 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw))))
red_perc_x1_3 = 1 - green_perc_x1_3

green_perc_x1_4 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw))))
red_perc_x1_4 = 1 - green_perc_x1_4

green_perc_x1_5 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw))))
red_perc_x1_5 = 1 - green_perc_x1_5

green_perc_x1_6 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw))))
red_perc_x1_6 = 1 - green_perc_x1_6

green_perc_x1_7 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw))))
red_perc_x1_7 = 1 - green_perc_x1_7

green_perc_x1_8 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))
red_perc_x1_8 = 1 - green_perc_x1_8

green_perc_x1_9 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw))))
red_perc_x1_9 = 1 - green_perc_x1_9

green_perc_x1_10 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw))))
red_perc_x1_10 = 1 - green_perc_x1_10



{ red_lift_x1_1 = 100*(red_perc_x1_1 - red_gen) / red_gen };    {green_lift_x1_1 = 100*(green_perc_x1_1 - green_gen) / green_gen} ; if  (red_perc_x1_1 == green_perc_x1_1) {red_lift_x1_1 = 0}; if  (red_perc_x1_1 == green_perc_x1_1) {green_lift_x1_1 = 0};
{ red_lift_x1_2 = 100*(red_perc_x1_2 - red_gen) / red_gen };   {green_lift_x1_2 = 100*(green_perc_x1_2 - green_gen) / green_gen} ; if  (red_perc_x1_2 == green_perc_x1_2) {red_lift_x1_2 = 0}; if  (red_perc_x1_2 == green_perc_x1_2) {green_lift_x1_2 = 0};
{ red_lift_x1_3 = 100*(red_perc_x1_3 - red_gen) / red_gen };   {green_lift_x1_3 = 100*(green_perc_x1_3 - green_gen) / green_gen} ; if  (red_perc_x1_3 == green_perc_x1_3) {red_lift_x1_3 = 0}; if  (red_perc_x1_3 == green_perc_x1_3) {green_lift_x1_3 = 0};
{ red_lift_x1_4 = 100*(red_perc_x1_4 - red_gen) / red_gen };    {green_lift_x1_4 = 100*(green_perc_x1_4 - green_gen) / green_gen} ; if  (red_perc_x1_4 == green_perc_x1_4) {red_lift_x1_4 = 0}; if  (red_perc_x1_4 == green_perc_x1_4) {green_lift_x1_4 = 0};
{ red_lift_x1_5 = 100*(red_perc_x1_5 - red_gen) / red_gen };    {green_lift_x1_5 = 100*(green_perc_x1_5 - green_gen) / green_gen} ; if  (red_perc_x1_5 == green_perc_x1_5) {red_lift_x1_5 = 0}; if  (red_perc_x1_5 == green_perc_x1_5) {green_lift_x1_5 = 0};
{ red_lift_x1_6 = 100*(red_perc_x1_6 - red_gen) / red_gen };    {green_lift_x1_6 = 100*(green_perc_x1_6 - green_gen) / green_gen} ; if  (red_perc_x1_6 == green_perc_x1_6) {red_lift_x1_6 = 0}; if  (red_perc_x1_6 == green_perc_x1_6) {green_lift_x1_6 = 0};
{ red_lift_x1_7 = 100*(red_perc_x1_7 - red_gen) / red_gen };   {green_lift_x1_7 = 100*(green_perc_x1_7 - green_gen) / green_gen} ; if  (red_perc_x1_7 == green_perc_x1_7) {red_lift_x1_7 = 0}; if  (red_perc_x1_7 == green_perc_x1_7) {green_lift_x1_7 = 0};
{ red_lift_x1_8 = 100*(red_perc_x1_8 - red_gen) / red_gen };   {green_lift_x1_8 = 100*(green_perc_x1_8 - green_gen) / green_gen} ; if  (red_perc_x1_8 == green_perc_x1_8) {red_lift_x1_8 = 0}; if  (red_perc_x1_8 == green_perc_x1_8) {green_lift_x1_8 = 0};
{ red_lift_x1_9 = 100*(red_perc_x1_9 - red_gen) / red_gen };   {green_lift_x1_9 = 100*(green_perc_x1_9 - green_gen) / green_gen} ; if  (red_perc_x1_9 == green_perc_x1_9) {red_lift_x1_9 = 0}; if  (red_perc_x1_9 == green_perc_x1_9) {green_lift_x1_9 = 0};
{ red_lift_x1_10 = 100*(red_perc_x1_10 - red_gen) / red_gen };   {green_lift_x1_10 = 100*(green_perc_x1_10 - green_gen) / green_gen} ; if  (red_perc_x1_10 == green_perc_x1_10) {red_lift_x1_10 = 0}; if  (red_perc_x1_10 == green_perc_x1_10) {green_lift_x1_10 = 0};


FP_1 = (
  coalesce( if (f(green_lift_x1_1 > red_lift_x1_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x1_2 > red_lift_x1_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_3 > red_lift_x1_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_4 > red_lift_x1_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_5 > red_lift_x1_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_6 > red_lift_x1_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_7 > red_lift_x1_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_8 > red_lift_x1_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x1_9 > red_lift_x1_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_10 > red_lift_x1_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
)



TN_1 = (
  coalesce( if (f(green_lift_x1_1 < red_lift_x1_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x1_2 < red_lift_x1_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_3 < red_lift_x1_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_4 < red_lift_x1_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_5 < red_lift_x1_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_6 < red_lift_x1_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_7 < red_lift_x1_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_8 < red_lift_x1_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x1_9 < red_lift_x1_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_10 < red_lift_x1_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
)


TP_1 = (
  coalesce( if (f(green_lift_x1_1 > red_lift_x1_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x1_2 > red_lift_x1_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_3 > red_lift_x1_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_4 > red_lift_x1_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_5 > red_lift_x1_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_6 > red_lift_x1_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_7 > red_lift_x1_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_8 > red_lift_x1_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x1_9 > red_lift_x1_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_10 > red_lift_x1_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
)


FN_1 = (
  coalesce( if (f(green_lift_x1_1 < red_lift_x1_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x1_2 < red_lift_x1_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_3 < red_lift_x1_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_4 < red_lift_x1_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_5 < red_lift_x1_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_6 < red_lift_x1_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_7 < red_lift_x1_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_8 < red_lift_x1_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x1_9 < red_lift_x1_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x1_10 < red_lift_x1_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
)


x_1_table = as.data.frame.matrix(rbind(c(green_perc_x1_1 * summary(irisDisc1)[1], green_perc_x1_2 * summary(irisDisc1)[2],green_perc_x1_3 * summary(irisDisc1)[3],
                                         green_perc_x1_4 * summary(irisDisc1)[4], green_perc_x1_5 * summary(irisDisc1)[5],green_perc_x1_6 * summary(irisDisc1)[6],
                                         green_perc_x1_7 * summary(irisDisc1)[7], green_perc_x1_8 * summary(irisDisc1)[8],green_perc_x1_9 * summary(irisDisc1)[9],
                                         green_perc_x1_10 * summary(irisDisc1)[10]),
                                       c(red_perc_x1_1 * summary(irisDisc1)[1], red_perc_x1_2 * summary(irisDisc1)[2],red_perc_x1_3 * summary(irisDisc1)[3],
                                         red_perc_x1_4 * summary(irisDisc1)[4], red_perc_x1_5 * summary(irisDisc1)[5],red_perc_x1_6 * summary(irisDisc1)[6],
                                         red_perc_x1_7 * summary(irisDisc1)[7], red_perc_x1_8 * summary(irisDisc1)[8],red_perc_x1_9 * summary(irisDisc1)[9],
                                         red_perc_x1_10 * summary(irisDisc1)[10])))



Precision_1 = TP_1 / (TP_1 + FP_1); Precision_1 = coalesce(Precision_1, 0)
Recall_1 = TP_1 / (TP_1 + FN_1); Recall_1 = coalesce(Recall_1, 0)
F1_Score_1 = coalesce ( 2 * (Precision_1 * Recall_1) / (Precision_1 + Recall_1), 0)
MCC_1 = (TP_1 * TN_1 - FP_1 * FN_1) / sqrt ( (TP_1 + FP_1) * (TP_1 + FN_1) * (TN_1 + FP_1) * (TN_1 + FN_1) )
MCC_adjusted_1 = (MCC_1 + 1) / 2
P_four_1 = (4 * TP_1 * TN_1) / (4 * TP_1 * TN_1 + (TP_1 + TN_1) * (FP_1 + FN_1)); P_four_1 = coalesce(P_four_1, 0)


par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
pp1a = as.grob(
  
  expression(barplot(ylab="Lift (%)",
                     c(if (red_lift_x1_1 > green_lift_x1_1) {red_lift_x1_1} else {green_lift_x1_1},
                       if (red_lift_x1_2 > green_lift_x1_2) {red_lift_x1_2} else {green_lift_x1_2},
                       if (red_lift_x1_3 > green_lift_x1_3) {red_lift_x1_3} else {green_lift_x1_3},
                       if (red_lift_x1_4 > green_lift_x1_4) {red_lift_x1_4} else {green_lift_x1_4},
                       if (red_lift_x1_5 > green_lift_x1_5) {red_lift_x1_5} else {green_lift_x1_5},
                       if (red_lift_x1_6 > green_lift_x1_6) {red_lift_x1_6} else {green_lift_x1_6},
                       if (red_lift_x1_7 > green_lift_x1_7) {red_lift_x1_7} else {green_lift_x1_7},
                       if (red_lift_x1_8 > green_lift_x1_8) {red_lift_x1_8} else {green_lift_x1_8},
                       if (red_lift_x1_9 > green_lift_x1_9) {red_lift_x1_9} else {green_lift_x1_9},
                       if (red_lift_x1_10 > green_lift_x1_10) {red_lift_x1_10} else {green_lift_x1_10}),
                     col = c(
                       if (red_lift_x1_1 > green_lift_x1_1) {"red"} else {"green"},
                       if (red_lift_x1_2 > green_lift_x1_2) {"red"} else {"green"},
                       if (red_lift_x1_3 > green_lift_x1_3) {"red"} else {"green"},
                       if (red_lift_x1_4 > green_lift_x1_4) {"red"} else {"green"},
                       if (red_lift_x1_5 > green_lift_x1_5) {"red"} else {"green"},
                       if (red_lift_x1_6 > green_lift_x1_6) {"red"} else {"green"},
                       if (red_lift_x1_7 > green_lift_x1_7) {"red"} else {"green"},
                       if (red_lift_x1_8 > green_lift_x1_8) {"red"} else {"green"},
                       if (red_lift_x1_9 > green_lift_x1_9) {"red"} else {"green"},
                       if (red_lift_x1_10 > green_lift_x1_10) {"red"} else {"green"}
                     )
  )
  ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(settings_2(1)$Tag), cex.main = 0.7,
                  c(if (red_lift_x1_1 > green_lift_x1_1) {red_lift_x1_1} else if (red_lift_x1_1 < green_lift_x1_1) {green_lift_x1_1} else {green_lift_x1_1},
                    if (red_lift_x1_2 > green_lift_x1_2) {red_lift_x1_2} else if (red_lift_x1_2 < green_lift_x1_2) {green_lift_x1_2} else {green_lift_x1_2},
                    if (red_lift_x1_3 > green_lift_x1_3) {red_lift_x1_3} else if (red_lift_x1_3 < green_lift_x1_3) {green_lift_x1_3} else {green_lift_x1_3},
                    if (red_lift_x1_4 > green_lift_x1_4) {red_lift_x1_4} else if (red_lift_x1_4 < green_lift_x1_4) {green_lift_x1_4} else {green_lift_x1_4},
                    if (red_lift_x1_5 > green_lift_x1_5) {red_lift_x1_5} else if (red_lift_x1_5 < green_lift_x1_5) {green_lift_x1_5} else {green_lift_x1_5},
                    if (red_lift_x1_6 > green_lift_x1_6) {red_lift_x1_6} else if (red_lift_x1_6 < green_lift_x1_6) {green_lift_x1_6} else {green_lift_x1_6},
                    if (red_lift_x1_7 > green_lift_x1_7) {red_lift_x1_7} else if (red_lift_x1_7 < green_lift_x1_7) {green_lift_x1_7} else {green_lift_x1_7},
                    if (red_lift_x1_8 > green_lift_x1_8) {red_lift_x1_8} else if (red_lift_x1_8 < green_lift_x1_8) {green_lift_x1_8} else {green_lift_x1_8},
                    if (red_lift_x1_9 > green_lift_x1_9) {red_lift_x1_9} else if (red_lift_x1_9 < green_lift_x1_9) {green_lift_x1_9} else {green_lift_x1_9},
                    if (red_lift_x1_10 > green_lift_x1_10) {red_lift_x1_10} else if (red_lift_x1_10 < green_lift_x1_10) {green_lift_x1_10} else {green_lift_x1_10}),
                  col = c(
                    if (red_lift_x1_1 > green_lift_x1_1) {"red"} else {"green"},
                    if (red_lift_x1_2 > green_lift_x1_2) {"red"} else {"green"},
                    if (red_lift_x1_3 > green_lift_x1_3) {"red"} else {"green"},
                    if (red_lift_x1_4 > green_lift_x1_4) {"red"} else {"green"},
                    if (red_lift_x1_5 > green_lift_x1_5) {"red"} else {"green"},
                    if (red_lift_x1_6 > green_lift_x1_6) {"red"} else {"green"},
                    if (red_lift_x1_7 > green_lift_x1_7) {"red"} else {"green"},
                    if (red_lift_x1_8 > green_lift_x1_8) {"red"} else {"green"},
                    if (red_lift_x1_9 > green_lift_x1_9) {"red"} else {"green"},
                    if (red_lift_x1_10 > green_lift_x1_10) {"red"} else {"green"}
                  ),
                  names.arg=c(
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[2],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[3],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[4],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[5],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[6],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[7],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[8],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[9],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[10],1)),
                    paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[11],1))
                  ),
  ),   title(paste(unique(settings_2(1)$Tag), expression("-> {"), round(100*P_four_1, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
  y= (c(if (red_lift_x1_1 > green_lift_x1_1) {red_lift_x1_1} else {green_lift_x1_1},
        if (red_lift_x1_2 > green_lift_x1_2) {red_lift_x1_2} else {green_lift_x1_2},
        if (red_lift_x1_3 > green_lift_x1_3) {red_lift_x1_3} else {green_lift_x1_3},
        if (red_lift_x1_4 > green_lift_x1_4) {red_lift_x1_4} else {green_lift_x1_4},
        if (red_lift_x1_5 > green_lift_x1_5) {red_lift_x1_5} else {green_lift_x1_5},
        if (red_lift_x1_6 > green_lift_x1_6) {red_lift_x1_6} else {green_lift_x1_6},
        if (red_lift_x1_7 > green_lift_x1_7) {red_lift_x1_7} else {green_lift_x1_7},
        if (red_lift_x1_8 > green_lift_x1_8) {red_lift_x1_8} else {green_lift_x1_8},
        if (red_lift_x1_9 > green_lift_x1_9) {red_lift_x1_9} else {green_lift_x1_9},
        if (red_lift_x1_10 > green_lift_x1_10) {red_lift_x1_10} else {green_lift_x1_10}))+4,
  labels=as.character(sapply(c(if (red_lift_x1_1 > green_lift_x1_1) {red_lift_x1_1} else {green_lift_x1_1},
                               if (red_lift_x1_2 > green_lift_x1_2) {red_lift_x1_2} else {green_lift_x1_2},
                               if (red_lift_x1_3 > green_lift_x1_3) {red_lift_x1_3} else {green_lift_x1_3},
                               if (red_lift_x1_4 > green_lift_x1_4) {red_lift_x1_4} else {green_lift_x1_4},
                               if (red_lift_x1_5 > green_lift_x1_5) {red_lift_x1_5} else {green_lift_x1_5},
                               if (red_lift_x1_6 > green_lift_x1_6) {red_lift_x1_6} else {green_lift_x1_6},
                               if (red_lift_x1_7 > green_lift_x1_7) {red_lift_x1_7} else {green_lift_x1_7},
                               if (red_lift_x1_8 > green_lift_x1_8) {red_lift_x1_8} else {green_lift_x1_8},
                               if (red_lift_x1_9 > green_lift_x1_9) {red_lift_x1_9} else {green_lift_x1_9},
                               if (red_lift_x1_10 > green_lift_x1_10) {red_lift_x1_10} else {green_lift_x1_10}),round))
  )))



pp1b = as.grob(expression(barplot(as.matrix(x_1_table),col=c("green4","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc1, which = "discretized:breaks")[1],attr(x = irisDisc1, which = "discretized:breaks")[11])),
                          text(x=barplot(as.matrix(x_1_table),col=c("green4","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[2],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[3],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[4],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[5],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[6],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[7],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[8],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[9],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[10],1)),
                            paste0(round(attr(x = irisDisc1, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc1, which = "discretized:breaks")[11],1))
                          )),
                          y= (summary(irisDisc1))+20, labels=as.character((summary(irisDisc1))))
                          
))


num1 = grid.arrange(grobs=list(as.ggplot(pp1a),as.ggplot(pp1b)))
















green_perc_x2_1 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw))))
red_perc_x2_1 = 1 - green_perc_x2_1

green_perc_x2_2 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw))))
red_perc_x2_2 = 1 - green_perc_x2_2

green_perc_x2_3 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw))))
red_perc_x2_3 = 1 - green_perc_x2_3

green_perc_x2_4 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw))))
red_perc_x2_4 = 1 - green_perc_x2_4

green_perc_x2_5 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw))))
red_perc_x2_5 = 1 - green_perc_x2_5

green_perc_x2_6 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw))))
red_perc_x2_6 = 1 - green_perc_x2_6

green_perc_x2_7 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw))))
red_perc_x2_7 = 1 - green_perc_x2_7

green_perc_x2_8 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))
red_perc_x2_8 = 1 - green_perc_x2_8

green_perc_x2_9 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw))))
red_perc_x2_9 = 1 - green_perc_x2_9

green_perc_x2_10 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw))))
red_perc_x2_10 = 1 - green_perc_x2_10



{ red_lift_x2_1 = 100*(red_perc_x2_1 - red_gen) / red_gen };    {green_lift_x2_1 = 100*(green_perc_x2_1 - green_gen) / green_gen} ; if  (red_perc_x2_1 == green_perc_x2_1) {red_lift_x2_1 = 0}; if  (red_perc_x2_1 == green_perc_x2_1) {green_lift_x2_1 = 0};
{ red_lift_x2_2 = 100*(red_perc_x2_2 - red_gen) / red_gen };   {green_lift_x2_2 = 100*(green_perc_x2_2 - green_gen) / green_gen} ; if  (red_perc_x2_2 == green_perc_x2_2) {red_lift_x2_2 = 0}; if  (red_perc_x2_2 == green_perc_x2_2) {green_lift_x2_2 = 0};
{ red_lift_x2_3 = 100*(red_perc_x2_3 - red_gen) / red_gen };   {green_lift_x2_3 = 100*(green_perc_x2_3 - green_gen) / green_gen} ; if  (red_perc_x2_3 == green_perc_x2_3) {red_lift_x2_3 = 0}; if  (red_perc_x2_3 == green_perc_x2_3) {green_lift_x2_3 = 0};
{ red_lift_x2_4 = 100*(red_perc_x2_4 - red_gen) / red_gen };    {green_lift_x2_4 = 100*(green_perc_x2_4 - green_gen) / green_gen} ; if  (red_perc_x2_4 == green_perc_x2_4) {red_lift_x2_4 = 0}; if  (red_perc_x2_4 == green_perc_x2_4) {green_lift_x2_4 = 0};
{ red_lift_x2_5 = 100*(red_perc_x2_5 - red_gen) / red_gen };    {green_lift_x2_5 = 100*(green_perc_x2_5 - green_gen) / green_gen} ; if  (red_perc_x2_5 == green_perc_x2_5) {red_lift_x2_5 = 0}; if  (red_perc_x2_5 == green_perc_x2_5) {green_lift_x2_5 = 0};
{ red_lift_x2_6 = 100*(red_perc_x2_6 - red_gen) / red_gen };    {green_lift_x2_6 = 100*(green_perc_x2_6 - green_gen) / green_gen} ; if  (red_perc_x2_6 == green_perc_x2_6) {red_lift_x2_6 = 0}; if  (red_perc_x2_6 == green_perc_x2_6) {green_lift_x2_6 = 0};
{ red_lift_x2_7 = 100*(red_perc_x2_7 - red_gen) / red_gen };   {green_lift_x2_7 = 100*(green_perc_x2_7 - green_gen) / green_gen} ; if  (red_perc_x2_7 == green_perc_x2_7) {red_lift_x2_7 = 0}; if  (red_perc_x2_7 == green_perc_x2_7) {green_lift_x2_7 = 0};
{ red_lift_x2_8 = 100*(red_perc_x2_8 - red_gen) / red_gen };   {green_lift_x2_8 = 100*(green_perc_x2_8 - green_gen) / green_gen} ; if  (red_perc_x2_8 == green_perc_x2_8) {red_lift_x2_8 = 0}; if  (red_perc_x2_8 == green_perc_x2_8) {green_lift_x2_8 = 0};
{ red_lift_x2_9 = 100*(red_perc_x2_9 - red_gen) / red_gen };   {green_lift_x2_9 = 100*(green_perc_x2_9 - green_gen) / green_gen} ; if  (red_perc_x2_9 == green_perc_x2_9) {red_lift_x2_9 = 0}; if  (red_perc_x2_9 == green_perc_x2_9) {green_lift_x2_9 = 0};
{ red_lift_x2_10 = 100*(red_perc_x2_10 - red_gen) / red_gen };   {green_lift_x2_10 = 100*(green_perc_x2_10 - green_gen) / green_gen} ; if  (red_perc_x2_10 == green_perc_x2_10) {red_lift_x2_10 = 0}; if  (red_perc_x2_10 == green_perc_x2_10) {green_lift_x2_10 = 0};


FP_2 = (
  coalesce( if (f(green_lift_x2_1 > red_lift_x2_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x2_2 > red_lift_x2_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_3 > red_lift_x2_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_4 > red_lift_x2_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_5 > red_lift_x2_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_6 > red_lift_x2_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_7 > red_lift_x2_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_8 > red_lift_x2_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x2_9 > red_lift_x2_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_10 > red_lift_x2_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
)



TN_2 = (
  coalesce( if (f(green_lift_x2_1 < red_lift_x2_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x2_2 < red_lift_x2_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_3 < red_lift_x2_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_4 < red_lift_x2_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_5 < red_lift_x2_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_6 < red_lift_x2_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_7 < red_lift_x2_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_8 < red_lift_x2_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x2_9 < red_lift_x2_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_10 < red_lift_x2_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
)


TP_2 = (
  coalesce( if (f(green_lift_x2_1 > red_lift_x2_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x2_2 > red_lift_x2_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_3 > red_lift_x2_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_4 > red_lift_x2_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_5 > red_lift_x2_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_6 > red_lift_x2_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_7 > red_lift_x2_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_8 > red_lift_x2_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x2_9 > red_lift_x2_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_10 > red_lift_x2_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
)


FN_2 = (
  coalesce( if (f(green_lift_x2_1 < red_lift_x2_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x2_2 < red_lift_x2_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_3 < red_lift_x2_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_4 < red_lift_x2_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_5 < red_lift_x2_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_6 < red_lift_x2_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_7 < red_lift_x2_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_8 < red_lift_x2_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x2_9 < red_lift_x2_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x2_10 < red_lift_x2_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
)


x_2_table = as.data.frame.matrix(rbind(c(green_perc_x2_1 * summary(irisDisc2)[1], green_perc_x2_2 * summary(irisDisc2)[2],green_perc_x2_3 * summary(irisDisc2)[3],
                                         green_perc_x2_4 * summary(irisDisc2)[4], green_perc_x2_5 * summary(irisDisc2)[5],green_perc_x2_6 * summary(irisDisc2)[6],
                                         green_perc_x2_7 * summary(irisDisc2)[7], green_perc_x2_8 * summary(irisDisc2)[8],green_perc_x2_9 * summary(irisDisc2)[9],
                                         green_perc_x2_10 * summary(irisDisc2)[10]),
                                       c(red_perc_x2_1 * summary(irisDisc2)[1], red_perc_x2_2 * summary(irisDisc2)[2],red_perc_x2_3 * summary(irisDisc2)[3],
                                         red_perc_x2_4 * summary(irisDisc2)[4], red_perc_x2_5 * summary(irisDisc2)[5],red_perc_x2_6 * summary(irisDisc2)[6],
                                         red_perc_x2_7 * summary(irisDisc2)[7], red_perc_x2_8 * summary(irisDisc2)[8],red_perc_x2_9 * summary(irisDisc2)[9],
                                         red_perc_x2_10 * summary(irisDisc2)[10])))



Precision_2 = TP_2 / (TP_2 + FP_2); Precision_2 = coalesce(Precision_2, 0)
Recall_2 = TP_2 / (TP_2 + FN_2); Recall_2 = coalesce(Recall_2, 0)
F1_Score_2 = coalesce ( 2 * (Precision_2 * Recall_2) / (Precision_2 + Recall_2), 0)
MCC_2 = (TP_2 * TN_2 - FP_2 * FN_2) / sqrt ( (TP_2 + FP_2) * (TP_2 + FN_2) * (TN_2 + FP_2) * (TN_2 + FN_2) )
MCC_adjusted_2 = (MCC_2 + 1) / 2
P_four_2 = (4 * TP_2 * TN_2) / (4 * TP_2 * TN_2 + (TP_2 + TN_2) * (FP_2 + FN_2)); P_four_2 = coalesce(P_four_2, 0)


par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
pp2a = as.grob(
  
  expression(barplot(ylab="Lift (%)",
                     c(if (red_lift_x2_1 > green_lift_x2_1) {red_lift_x2_1} else {green_lift_x2_1},
                       if (red_lift_x2_2 > green_lift_x2_2) {red_lift_x2_2} else {green_lift_x2_2},
                       if (red_lift_x2_3 > green_lift_x2_3) {red_lift_x2_3} else {green_lift_x2_3},
                       if (red_lift_x2_4 > green_lift_x2_4) {red_lift_x2_4} else {green_lift_x2_4},
                       if (red_lift_x2_5 > green_lift_x2_5) {red_lift_x2_5} else {green_lift_x2_5},
                       if (red_lift_x2_6 > green_lift_x2_6) {red_lift_x2_6} else {green_lift_x2_6},
                       if (red_lift_x2_7 > green_lift_x2_7) {red_lift_x2_7} else {green_lift_x2_7},
                       if (red_lift_x2_8 > green_lift_x2_8) {red_lift_x2_8} else {green_lift_x2_8},
                       if (red_lift_x2_9 > green_lift_x2_9) {red_lift_x2_9} else {green_lift_x2_9},
                       if (red_lift_x2_10 > green_lift_x2_10) {red_lift_x2_10} else {green_lift_x2_10}),
                     col = c(
                       if (red_lift_x2_1 > green_lift_x2_1) {"red"} else {"green"},
                       if (red_lift_x2_2 > green_lift_x2_2) {"red"} else {"green"},
                       if (red_lift_x2_3 > green_lift_x2_3) {"red"} else {"green"},
                       if (red_lift_x2_4 > green_lift_x2_4) {"red"} else {"green"},
                       if (red_lift_x2_5 > green_lift_x2_5) {"red"} else {"green"},
                       if (red_lift_x2_6 > green_lift_x2_6) {"red"} else {"green"},
                       if (red_lift_x2_7 > green_lift_x2_7) {"red"} else {"green"},
                       if (red_lift_x2_8 > green_lift_x2_8) {"red"} else {"green"},
                       if (red_lift_x2_9 > green_lift_x2_9) {"red"} else {"green"},
                       if (red_lift_x2_10 > green_lift_x2_10) {"red"} else {"green"}
                     )
  )
  ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(settings_2(2)$Tag), cex.main = 0.7,
                  c(if (red_lift_x2_1 > green_lift_x2_1) {red_lift_x2_1} else if (red_lift_x2_1 < green_lift_x2_1) {green_lift_x2_1} else {green_lift_x2_1},
                    if (red_lift_x2_2 > green_lift_x2_2) {red_lift_x2_2} else if (red_lift_x2_2 < green_lift_x2_2) {green_lift_x2_2} else {green_lift_x2_2},
                    if (red_lift_x2_3 > green_lift_x2_3) {red_lift_x2_3} else if (red_lift_x2_3 < green_lift_x2_3) {green_lift_x2_3} else {green_lift_x2_3},
                    if (red_lift_x2_4 > green_lift_x2_4) {red_lift_x2_4} else if (red_lift_x2_4 < green_lift_x2_4) {green_lift_x2_4} else {green_lift_x2_4},
                    if (red_lift_x2_5 > green_lift_x2_5) {red_lift_x2_5} else if (red_lift_x2_5 < green_lift_x2_5) {green_lift_x2_5} else {green_lift_x2_5},
                    if (red_lift_x2_6 > green_lift_x2_6) {red_lift_x2_6} else if (red_lift_x2_6 < green_lift_x2_6) {green_lift_x2_6} else {green_lift_x2_6},
                    if (red_lift_x2_7 > green_lift_x2_7) {red_lift_x2_7} else if (red_lift_x2_7 < green_lift_x2_7) {green_lift_x2_7} else {green_lift_x2_7},
                    if (red_lift_x2_8 > green_lift_x2_8) {red_lift_x2_8} else if (red_lift_x2_8 < green_lift_x2_8) {green_lift_x2_8} else {green_lift_x2_8},
                    if (red_lift_x2_9 > green_lift_x2_9) {red_lift_x2_9} else if (red_lift_x2_9 < green_lift_x2_9) {green_lift_x2_9} else {green_lift_x2_9},
                    if (red_lift_x2_10 > green_lift_x2_10) {red_lift_x2_10} else if (red_lift_x2_10 < green_lift_x2_10) {green_lift_x2_10} else {green_lift_x2_10}),
                  col = c(
                    if (red_lift_x2_1 > green_lift_x2_1) {"red"} else {"green"},
                    if (red_lift_x2_2 > green_lift_x2_2) {"red"} else {"green"},
                    if (red_lift_x2_3 > green_lift_x2_3) {"red"} else {"green"},
                    if (red_lift_x2_4 > green_lift_x2_4) {"red"} else {"green"},
                    if (red_lift_x2_5 > green_lift_x2_5) {"red"} else {"green"},
                    if (red_lift_x2_6 > green_lift_x2_6) {"red"} else {"green"},
                    if (red_lift_x2_7 > green_lift_x2_7) {"red"} else {"green"},
                    if (red_lift_x2_8 > green_lift_x2_8) {"red"} else {"green"},
                    if (red_lift_x2_9 > green_lift_x2_9) {"red"} else {"green"},
                    if (red_lift_x2_10 > green_lift_x2_10) {"red"} else {"green"}
                  ),
                  names.arg=c(
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[2],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[3],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[4],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[5],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[6],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[7],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[8],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[9],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[10],1)),
                    paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[11],1))
                  ),
  ),   title(paste(unique(settings_2(2)$Tag), expression("-> {"), round(100*P_four_2, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
  y= (c(if (red_lift_x2_1 > green_lift_x2_1) {red_lift_x2_1} else {green_lift_x2_1},
        if (red_lift_x2_2 > green_lift_x2_2) {red_lift_x2_2} else {green_lift_x2_2},
        if (red_lift_x2_3 > green_lift_x2_3) {red_lift_x2_3} else {green_lift_x2_3},
        if (red_lift_x2_4 > green_lift_x2_4) {red_lift_x2_4} else {green_lift_x2_4},
        if (red_lift_x2_5 > green_lift_x2_5) {red_lift_x2_5} else {green_lift_x2_5},
        if (red_lift_x2_6 > green_lift_x2_6) {red_lift_x2_6} else {green_lift_x2_6},
        if (red_lift_x2_7 > green_lift_x2_7) {red_lift_x2_7} else {green_lift_x2_7},
        if (red_lift_x2_8 > green_lift_x2_8) {red_lift_x2_8} else {green_lift_x2_8},
        if (red_lift_x2_9 > green_lift_x2_9) {red_lift_x2_9} else {green_lift_x2_9},
        if (red_lift_x2_10 > green_lift_x2_10) {red_lift_x2_10} else {green_lift_x2_10}))+4,
  labels=as.character(sapply(c(if (red_lift_x2_1 > green_lift_x2_1) {red_lift_x2_1} else {green_lift_x2_1},
                               if (red_lift_x2_2 > green_lift_x2_2) {red_lift_x2_2} else {green_lift_x2_2},
                               if (red_lift_x2_3 > green_lift_x2_3) {red_lift_x2_3} else {green_lift_x2_3},
                               if (red_lift_x2_4 > green_lift_x2_4) {red_lift_x2_4} else {green_lift_x2_4},
                               if (red_lift_x2_5 > green_lift_x2_5) {red_lift_x2_5} else {green_lift_x2_5},
                               if (red_lift_x2_6 > green_lift_x2_6) {red_lift_x2_6} else {green_lift_x2_6},
                               if (red_lift_x2_7 > green_lift_x2_7) {red_lift_x2_7} else {green_lift_x2_7},
                               if (red_lift_x2_8 > green_lift_x2_8) {red_lift_x2_8} else {green_lift_x2_8},
                               if (red_lift_x2_9 > green_lift_x2_9) {red_lift_x2_9} else {green_lift_x2_9},
                               if (red_lift_x2_10 > green_lift_x2_10) {red_lift_x2_10} else {green_lift_x2_10}),round))
  )))



pp2b = as.grob(expression(barplot(as.matrix(x_2_table),col=c("green4","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc2, which = "discretized:breaks")[1],attr(x = irisDisc2, which = "discretized:breaks")[11])),
                          text(x=barplot(as.matrix(x_2_table),col=c("green4","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[2],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[3],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[4],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[5],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[6],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[7],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[8],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[9],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[10],1)),
                            paste0(round(attr(x = irisDisc2, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc2, which = "discretized:breaks")[11],1))
                          )),
                          y= (summary(irisDisc2))+20, labels=as.character((summary(irisDisc2))))
                          
))


num2 = grid.arrange(grobs=list(as.ggplot(pp2a),as.ggplot(pp2b)))














green_perc_x3_1 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw))))
red_perc_x3_1 = 1 - green_perc_x3_1

green_perc_x3_2 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw))))
red_perc_x3_2 = 1 - green_perc_x3_2

green_perc_x3_3 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw))))
red_perc_x3_3 = 1 - green_perc_x3_3

green_perc_x3_4 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw))))
red_perc_x3_4 = 1 - green_perc_x3_4

green_perc_x3_5 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw))))
red_perc_x3_5 = 1 - green_perc_x3_5

green_perc_x3_6 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw))))
red_perc_x3_6 = 1 - green_perc_x3_6

green_perc_x3_7 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw))))
red_perc_x3_7 = 1 - green_perc_x3_7

green_perc_x3_8 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))
red_perc_x3_8 = 1 - green_perc_x3_8

green_perc_x3_9 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw))))
red_perc_x3_9 = 1 - green_perc_x3_9

green_perc_x3_10 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw))))
red_perc_x3_10 = 1 - green_perc_x3_10



{ red_lift_x3_1 = 100*(red_perc_x3_1 - red_gen) / red_gen };    {green_lift_x3_1 = 100*(green_perc_x3_1 - green_gen) / green_gen} ; if  (red_perc_x3_1 == green_perc_x3_1) {red_lift_x3_1 = 0}; if  (red_perc_x3_1 == green_perc_x3_1) {green_lift_x3_1 = 0};
{ red_lift_x3_2 = 100*(red_perc_x3_2 - red_gen) / red_gen };   {green_lift_x3_2 = 100*(green_perc_x3_2 - green_gen) / green_gen} ; if  (red_perc_x3_2 == green_perc_x3_2) {red_lift_x3_2 = 0}; if  (red_perc_x3_2 == green_perc_x3_2) {green_lift_x3_2 = 0};
{ red_lift_x3_3 = 100*(red_perc_x3_3 - red_gen) / red_gen };   {green_lift_x3_3 = 100*(green_perc_x3_3 - green_gen) / green_gen} ; if  (red_perc_x3_3 == green_perc_x3_3) {red_lift_x3_3 = 0}; if  (red_perc_x3_3 == green_perc_x3_3) {green_lift_x3_3 = 0};
{ red_lift_x3_4 = 100*(red_perc_x3_4 - red_gen) / red_gen };    {green_lift_x3_4 = 100*(green_perc_x3_4 - green_gen) / green_gen} ; if  (red_perc_x3_4 == green_perc_x3_4) {red_lift_x3_4 = 0}; if  (red_perc_x3_4 == green_perc_x3_4) {green_lift_x3_4 = 0};
{ red_lift_x3_5 = 100*(red_perc_x3_5 - red_gen) / red_gen };    {green_lift_x3_5 = 100*(green_perc_x3_5 - green_gen) / green_gen} ; if  (red_perc_x3_5 == green_perc_x3_5) {red_lift_x3_5 = 0}; if  (red_perc_x3_5 == green_perc_x3_5) {green_lift_x3_5 = 0};
{ red_lift_x3_6 = 100*(red_perc_x3_6 - red_gen) / red_gen };    {green_lift_x3_6 = 100*(green_perc_x3_6 - green_gen) / green_gen} ; if  (red_perc_x3_6 == green_perc_x3_6) {red_lift_x3_6 = 0}; if  (red_perc_x3_6 == green_perc_x3_6) {green_lift_x3_6 = 0};
{ red_lift_x3_7 = 100*(red_perc_x3_7 - red_gen) / red_gen };   {green_lift_x3_7 = 100*(green_perc_x3_7 - green_gen) / green_gen} ; if  (red_perc_x3_7 == green_perc_x3_7) {red_lift_x3_7 = 0}; if  (red_perc_x3_7 == green_perc_x3_7) {green_lift_x3_7 = 0};
{ red_lift_x3_8 = 100*(red_perc_x3_8 - red_gen) / red_gen };   {green_lift_x3_8 = 100*(green_perc_x3_8 - green_gen) / green_gen} ; if  (red_perc_x3_8 == green_perc_x3_8) {red_lift_x3_8 = 0}; if  (red_perc_x3_8 == green_perc_x3_8) {green_lift_x3_8 = 0};
{ red_lift_x3_9 = 100*(red_perc_x3_9 - red_gen) / red_gen };   {green_lift_x3_9 = 100*(green_perc_x3_9 - green_gen) / green_gen} ; if  (red_perc_x3_9 == green_perc_x3_9) {red_lift_x3_9 = 0}; if  (red_perc_x3_9 == green_perc_x3_9) {green_lift_x3_9 = 0};
{ red_lift_x3_10 = 100*(red_perc_x3_10 - red_gen) / red_gen };   {green_lift_x3_10 = 100*(green_perc_x3_10 - green_gen) / green_gen} ; if  (red_perc_x3_10 == green_perc_x3_10) {red_lift_x3_10 = 0}; if  (red_perc_x3_10 == green_perc_x3_10) {green_lift_x3_10 = 0};


FP_3 = (
  coalesce( if (f(green_lift_x3_1 > red_lift_x3_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x3_2 > red_lift_x3_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_3 > red_lift_x3_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_4 > red_lift_x3_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_5 > red_lift_x3_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_6 > red_lift_x3_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_7 > red_lift_x3_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_8 > red_lift_x3_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x3_9 > red_lift_x3_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_10 > red_lift_x3_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
)



TN_3 = (
  coalesce( if (f(green_lift_x3_1 < red_lift_x3_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x3_2 < red_lift_x3_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_3 < red_lift_x3_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_4 < red_lift_x3_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_5 < red_lift_x3_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_6 < red_lift_x3_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_7 < red_lift_x3_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_8 < red_lift_x3_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x3_9 < red_lift_x3_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_10 < red_lift_x3_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
)


TP_3 = (
  coalesce( if (f(green_lift_x3_1 > red_lift_x3_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x3_2 > red_lift_x3_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_3 > red_lift_x3_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_4 > red_lift_x3_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_5 > red_lift_x3_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_6 > red_lift_x3_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_7 > red_lift_x3_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_8 > red_lift_x3_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x3_9 > red_lift_x3_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_10 > red_lift_x3_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
)


FN_3 = (
  coalesce( if (f(green_lift_x3_1 < red_lift_x3_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x3_2 < red_lift_x3_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_3 < red_lift_x3_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_4 < red_lift_x3_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_5 < red_lift_x3_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_6 < red_lift_x3_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_7 < red_lift_x3_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_8 < red_lift_x3_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x3_9 < red_lift_x3_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x3_10 < red_lift_x3_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
)


x_3_table = as.data.frame.matrix(rbind(c(green_perc_x3_1 * summary(irisDisc3)[1], green_perc_x3_2 * summary(irisDisc3)[2],green_perc_x3_3 * summary(irisDisc3)[3],
                                         green_perc_x3_4 * summary(irisDisc3)[4], green_perc_x3_5 * summary(irisDisc3)[5],green_perc_x3_6 * summary(irisDisc3)[6],
                                         green_perc_x3_7 * summary(irisDisc3)[7], green_perc_x3_8 * summary(irisDisc3)[8],green_perc_x3_9 * summary(irisDisc3)[9],
                                         green_perc_x3_10 * summary(irisDisc3)[10]),
                                       c(red_perc_x3_1 * summary(irisDisc3)[1], red_perc_x3_2 * summary(irisDisc3)[2],red_perc_x3_3 * summary(irisDisc3)[3],
                                         red_perc_x3_4 * summary(irisDisc3)[4], red_perc_x3_5 * summary(irisDisc3)[5],red_perc_x3_6 * summary(irisDisc3)[6],
                                         red_perc_x3_7 * summary(irisDisc3)[7], red_perc_x3_8 * summary(irisDisc3)[8],red_perc_x3_9 * summary(irisDisc3)[9],
                                         red_perc_x3_10 * summary(irisDisc3)[10])))



Precision_3 = TP_3 / (TP_3 + FP_3); Precision_3 = coalesce(Precision_3, 0)
Recall_3 = TP_3 / (TP_3 + FN_3); Recall_3 = coalesce(Recall_3, 0)
F1_Score_3 = coalesce ( 2 * (Precision_3 * Recall_3) / (Precision_3 + Recall_3), 0)
MCC_3 = (TP_3 * TN_3 - FP_3 * FN_3) / sqrt ( (TP_3 + FP_3) * (TP_3 + FN_3) * (TN_3 + FP_3) * (TN_3 + FN_3) )
MCC_adjusted_3 = (MCC_3 + 1) / 2
P_four_3 = (4 * TP_3 * TN_3) / (4 * TP_3 * TN_3 + (TP_3 + TN_3) * (FP_3 + FN_3)); P_four_3 = coalesce(P_four_3, 0)


par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
pp3a = as.grob(
  
  expression(barplot(ylab="Lift (%)",
                     c(if (red_lift_x3_1 > green_lift_x3_1) {red_lift_x3_1} else {green_lift_x3_1},
                       if (red_lift_x3_2 > green_lift_x3_2) {red_lift_x3_2} else {green_lift_x3_2},
                       if (red_lift_x3_3 > green_lift_x3_3) {red_lift_x3_3} else {green_lift_x3_3},
                       if (red_lift_x3_4 > green_lift_x3_4) {red_lift_x3_4} else {green_lift_x3_4},
                       if (red_lift_x3_5 > green_lift_x3_5) {red_lift_x3_5} else {green_lift_x3_5},
                       if (red_lift_x3_6 > green_lift_x3_6) {red_lift_x3_6} else {green_lift_x3_6},
                       if (red_lift_x3_7 > green_lift_x3_7) {red_lift_x3_7} else {green_lift_x3_7},
                       if (red_lift_x3_8 > green_lift_x3_8) {red_lift_x3_8} else {green_lift_x3_8},
                       if (red_lift_x3_9 > green_lift_x3_9) {red_lift_x3_9} else {green_lift_x3_9},
                       if (red_lift_x3_10 > green_lift_x3_10) {red_lift_x3_10} else {green_lift_x3_10}),
                     col = c(
                       if (red_lift_x3_1 > green_lift_x3_1) {"red"} else {"green"},
                       if (red_lift_x3_2 > green_lift_x3_2) {"red"} else {"green"},
                       if (red_lift_x3_3 > green_lift_x3_3) {"red"} else {"green"},
                       if (red_lift_x3_4 > green_lift_x3_4) {"red"} else {"green"},
                       if (red_lift_x3_5 > green_lift_x3_5) {"red"} else {"green"},
                       if (red_lift_x3_6 > green_lift_x3_6) {"red"} else {"green"},
                       if (red_lift_x3_7 > green_lift_x3_7) {"red"} else {"green"},
                       if (red_lift_x3_8 > green_lift_x3_8) {"red"} else {"green"},
                       if (red_lift_x3_9 > green_lift_x3_9) {"red"} else {"green"},
                       if (red_lift_x3_10 > green_lift_x3_10) {"red"} else {"green"}
                     )
  )
  ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(settings_2(3)$Tag), cex.main = 0.7,
                  c(if (red_lift_x3_1 > green_lift_x3_1) {red_lift_x3_1} else if (red_lift_x3_1 < green_lift_x3_1) {green_lift_x3_1} else {green_lift_x3_1},
                    if (red_lift_x3_2 > green_lift_x3_2) {red_lift_x3_2} else if (red_lift_x3_2 < green_lift_x3_2) {green_lift_x3_2} else {green_lift_x3_2},
                    if (red_lift_x3_3 > green_lift_x3_3) {red_lift_x3_3} else if (red_lift_x3_3 < green_lift_x3_3) {green_lift_x3_3} else {green_lift_x3_3},
                    if (red_lift_x3_4 > green_lift_x3_4) {red_lift_x3_4} else if (red_lift_x3_4 < green_lift_x3_4) {green_lift_x3_4} else {green_lift_x3_4},
                    if (red_lift_x3_5 > green_lift_x3_5) {red_lift_x3_5} else if (red_lift_x3_5 < green_lift_x3_5) {green_lift_x3_5} else {green_lift_x3_5},
                    if (red_lift_x3_6 > green_lift_x3_6) {red_lift_x3_6} else if (red_lift_x3_6 < green_lift_x3_6) {green_lift_x3_6} else {green_lift_x3_6},
                    if (red_lift_x3_7 > green_lift_x3_7) {red_lift_x3_7} else if (red_lift_x3_7 < green_lift_x3_7) {green_lift_x3_7} else {green_lift_x3_7},
                    if (red_lift_x3_8 > green_lift_x3_8) {red_lift_x3_8} else if (red_lift_x3_8 < green_lift_x3_8) {green_lift_x3_8} else {green_lift_x3_8},
                    if (red_lift_x3_9 > green_lift_x3_9) {red_lift_x3_9} else if (red_lift_x3_9 < green_lift_x3_9) {green_lift_x3_9} else {green_lift_x3_9},
                    if (red_lift_x3_10 > green_lift_x3_10) {red_lift_x3_10} else if (red_lift_x3_10 < green_lift_x3_10) {green_lift_x3_10} else {green_lift_x3_10}),
                  col = c(
                    if (red_lift_x3_1 > green_lift_x3_1) {"red"} else {"green"},
                    if (red_lift_x3_2 > green_lift_x3_2) {"red"} else {"green"},
                    if (red_lift_x3_3 > green_lift_x3_3) {"red"} else {"green"},
                    if (red_lift_x3_4 > green_lift_x3_4) {"red"} else {"green"},
                    if (red_lift_x3_5 > green_lift_x3_5) {"red"} else {"green"},
                    if (red_lift_x3_6 > green_lift_x3_6) {"red"} else {"green"},
                    if (red_lift_x3_7 > green_lift_x3_7) {"red"} else {"green"},
                    if (red_lift_x3_8 > green_lift_x3_8) {"red"} else {"green"},
                    if (red_lift_x3_9 > green_lift_x3_9) {"red"} else {"green"},
                    if (red_lift_x3_10 > green_lift_x3_10) {"red"} else {"green"}
                  ),
                  names.arg=c(
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[2],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[3],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[4],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[5],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[6],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[7],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[8],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[9],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[10],1)),
                    paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[11],1))
                  ),
  ),   title(paste(unique(settings_2(3)$Tag), expression("-> {"), round(100*P_four_3, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
  y= (c(if (red_lift_x3_1 > green_lift_x3_1) {red_lift_x3_1} else {green_lift_x3_1},
        if (red_lift_x3_2 > green_lift_x3_2) {red_lift_x3_2} else {green_lift_x3_2},
        if (red_lift_x3_3 > green_lift_x3_3) {red_lift_x3_3} else {green_lift_x3_3},
        if (red_lift_x3_4 > green_lift_x3_4) {red_lift_x3_4} else {green_lift_x3_4},
        if (red_lift_x3_5 > green_lift_x3_5) {red_lift_x3_5} else {green_lift_x3_5},
        if (red_lift_x3_6 > green_lift_x3_6) {red_lift_x3_6} else {green_lift_x3_6},
        if (red_lift_x3_7 > green_lift_x3_7) {red_lift_x3_7} else {green_lift_x3_7},
        if (red_lift_x3_8 > green_lift_x3_8) {red_lift_x3_8} else {green_lift_x3_8},
        if (red_lift_x3_9 > green_lift_x3_9) {red_lift_x3_9} else {green_lift_x3_9},
        if (red_lift_x3_10 > green_lift_x3_10) {red_lift_x3_10} else {green_lift_x3_10}))+4,
  labels=as.character(sapply(c(if (red_lift_x3_1 > green_lift_x3_1) {red_lift_x3_1} else {green_lift_x3_1},
                               if (red_lift_x3_2 > green_lift_x3_2) {red_lift_x3_2} else {green_lift_x3_2},
                               if (red_lift_x3_3 > green_lift_x3_3) {red_lift_x3_3} else {green_lift_x3_3},
                               if (red_lift_x3_4 > green_lift_x3_4) {red_lift_x3_4} else {green_lift_x3_4},
                               if (red_lift_x3_5 > green_lift_x3_5) {red_lift_x3_5} else {green_lift_x3_5},
                               if (red_lift_x3_6 > green_lift_x3_6) {red_lift_x3_6} else {green_lift_x3_6},
                               if (red_lift_x3_7 > green_lift_x3_7) {red_lift_x3_7} else {green_lift_x3_7},
                               if (red_lift_x3_8 > green_lift_x3_8) {red_lift_x3_8} else {green_lift_x3_8},
                               if (red_lift_x3_9 > green_lift_x3_9) {red_lift_x3_9} else {green_lift_x3_9},
                               if (red_lift_x3_10 > green_lift_x3_10) {red_lift_x3_10} else {green_lift_x3_10}),round))
  )))



pp3b = as.grob(expression(barplot(as.matrix(x_3_table),col=c("green4","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc3, which = "discretized:breaks")[1],attr(x = irisDisc3, which = "discretized:breaks")[11])),
                          text(x=barplot(as.matrix(x_3_table),col=c("green4","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[2],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[3],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[4],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[5],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[6],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[7],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[8],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[9],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[10],1)),
                            paste0(round(attr(x = irisDisc3, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc3, which = "discretized:breaks")[11],1))
                          )),
                          y= (summary(irisDisc3))+20, labels=as.character((summary(irisDisc3))))
                          
))


num3 = grid.arrange(grobs=list(as.ggplot(pp3a),as.ggplot(pp3b)))



























green_perc_x4_1 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw))))
red_perc_x4_1 = 1 - green_perc_x4_1

green_perc_x4_2 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw))))
red_perc_x4_2 = 1 - green_perc_x4_2

green_perc_x4_3 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw))))
red_perc_x4_3 = 1 - green_perc_x4_3

green_perc_x4_4 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw))))
red_perc_x4_4 = 1 - green_perc_x4_4

green_perc_x4_5 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw))))
red_perc_x4_5 = 1 - green_perc_x4_5

green_perc_x4_6 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw))))
red_perc_x4_6 = 1 - green_perc_x4_6

green_perc_x4_7 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw))))
red_perc_x4_7 = 1 - green_perc_x4_7

green_perc_x4_8 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))
red_perc_x4_8 = 1 - green_perc_x4_8

green_perc_x4_9 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw))))
red_perc_x4_9 = 1 - green_perc_x4_9

green_perc_x4_10 = sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  /  length(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw))))
red_perc_x4_10 = 1 - green_perc_x4_10



{ red_lift_x4_1 = 100*(red_perc_x4_1 - red_gen) / red_gen };    {green_lift_x4_1 = 100*(green_perc_x4_1 - green_gen) / green_gen} ; if  (red_perc_x4_1 == green_perc_x4_1) {red_lift_x4_1 = 0}; if  (red_perc_x4_1 == green_perc_x4_1) {green_lift_x4_1 = 0};
{ red_lift_x4_2 = 100*(red_perc_x4_2 - red_gen) / red_gen };   {green_lift_x4_2 = 100*(green_perc_x4_2 - green_gen) / green_gen} ; if  (red_perc_x4_2 == green_perc_x4_2) {red_lift_x4_2 = 0}; if  (red_perc_x4_2 == green_perc_x4_2) {green_lift_x4_2 = 0};
{ red_lift_x4_3 = 100*(red_perc_x4_3 - red_gen) / red_gen };   {green_lift_x4_3 = 100*(green_perc_x4_3 - green_gen) / green_gen} ; if  (red_perc_x4_3 == green_perc_x4_3) {red_lift_x4_3 = 0}; if  (red_perc_x4_3 == green_perc_x4_3) {green_lift_x4_3 = 0};
{ red_lift_x4_4 = 100*(red_perc_x4_4 - red_gen) / red_gen };    {green_lift_x4_4 = 100*(green_perc_x4_4 - green_gen) / green_gen} ; if  (red_perc_x4_4 == green_perc_x4_4) {red_lift_x4_4 = 0}; if  (red_perc_x4_4 == green_perc_x4_4) {green_lift_x4_4 = 0};
{ red_lift_x4_5 = 100*(red_perc_x4_5 - red_gen) / red_gen };    {green_lift_x4_5 = 100*(green_perc_x4_5 - green_gen) / green_gen} ; if  (red_perc_x4_5 == green_perc_x4_5) {red_lift_x4_5 = 0}; if  (red_perc_x4_5 == green_perc_x4_5) {green_lift_x4_5 = 0};
{ red_lift_x4_6 = 100*(red_perc_x4_6 - red_gen) / red_gen };    {green_lift_x4_6 = 100*(green_perc_x4_6 - green_gen) / green_gen} ; if  (red_perc_x4_6 == green_perc_x4_6) {red_lift_x4_6 = 0}; if  (red_perc_x4_6 == green_perc_x4_6) {green_lift_x4_6 = 0};
{ red_lift_x4_7 = 100*(red_perc_x4_7 - red_gen) / red_gen };   {green_lift_x4_7 = 100*(green_perc_x4_7 - green_gen) / green_gen} ; if  (red_perc_x4_7 == green_perc_x4_7) {red_lift_x4_7 = 0}; if  (red_perc_x4_7 == green_perc_x4_7) {green_lift_x4_7 = 0};
{ red_lift_x4_8 = 100*(red_perc_x4_8 - red_gen) / red_gen };   {green_lift_x4_8 = 100*(green_perc_x4_8 - green_gen) / green_gen} ; if  (red_perc_x4_8 == green_perc_x4_8) {red_lift_x4_8 = 0}; if  (red_perc_x4_8 == green_perc_x4_8) {green_lift_x4_8 = 0};
{ red_lift_x4_9 = 100*(red_perc_x4_9 - red_gen) / red_gen };   {green_lift_x4_9 = 100*(green_perc_x4_9 - green_gen) / green_gen} ; if  (red_perc_x4_9 == green_perc_x4_9) {red_lift_x4_9 = 0}; if  (red_perc_x4_9 == green_perc_x4_9) {green_lift_x4_9 = 0};
{ red_lift_x4_10 = 100*(red_perc_x4_10 - red_gen) / red_gen };   {green_lift_x4_10 = 100*(green_perc_x4_10 - green_gen) / green_gen} ; if  (red_perc_x4_10 == green_perc_x4_10) {red_lift_x4_10 = 0}; if  (red_perc_x4_10 == green_perc_x4_10) {green_lift_x4_10 = 0};


FP_4 = (
  coalesce( if (f(green_lift_x4_1 > red_lift_x4_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x4_2 > red_lift_x4_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_3 > red_lift_x4_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_4 > red_lift_x4_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_5 > red_lift_x4_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_6 > red_lift_x4_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_7 > red_lift_x4_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_8 > red_lift_x4_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x4_9 > red_lift_x4_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_10 > red_lift_x4_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
)



TN_4 = (
  coalesce( if (f(green_lift_x4_1 < red_lift_x4_1)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x4_2 < red_lift_x4_2)) as.numeric( { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_3 < red_lift_x4_3)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_4 < red_lift_x4_4)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_5 < red_lift_x4_5)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_6 < red_lift_x4_6)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_7 < red_lift_x4_7)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_8 < red_lift_x4_8)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x4_9 < red_lift_x4_9)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_10 < red_lift_x4_10)) as.numeric(  { sum(is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
)


TP_4 = (
  coalesce( if (f(green_lift_x4_1 > red_lift_x4_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x4_2 > red_lift_x4_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_3 > red_lift_x4_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_4 > red_lift_x4_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_5 > red_lift_x4_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_6 > red_lift_x4_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_7 > red_lift_x4_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_8 > red_lift_x4_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x4_9 > red_lift_x4_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_10 > red_lift_x4_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
)


FN_4 = (
  coalesce( if (f(green_lift_x4_1 < red_lift_x4_1)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
    coalesce( if (f(green_lift_x4_2 < red_lift_x4_2)) as.numeric( { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_3 < red_lift_x4_3)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_4 < red_lift_x4_4)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_5 < red_lift_x4_5)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_6 < red_lift_x4_6)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_7 < red_lift_x4_7)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_8 < red_lift_x4_8)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
    coalesce( if (f(green_lift_x4_9 < red_lift_x4_9)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
    coalesce( if (f(green_lift_x4_10 < red_lift_x4_10)) as.numeric(  { sum(!is.na(row.match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
)


x_4_table = as.data.frame.matrix(rbind(c(green_perc_x4_1 * summary(irisDisc4)[1], green_perc_x4_2 * summary(irisDisc4)[2],green_perc_x4_3 * summary(irisDisc4)[3],
                                         green_perc_x4_4 * summary(irisDisc4)[4], green_perc_x4_5 * summary(irisDisc4)[5],green_perc_x4_6 * summary(irisDisc4)[6],
                                         green_perc_x4_7 * summary(irisDisc4)[7], green_perc_x4_8 * summary(irisDisc4)[8],green_perc_x4_9 * summary(irisDisc4)[9],
                                         green_perc_x4_10 * summary(irisDisc4)[10]),
                                       c(red_perc_x4_1 * summary(irisDisc4)[1], red_perc_x4_2 * summary(irisDisc4)[2],red_perc_x4_3 * summary(irisDisc4)[3],
                                         red_perc_x4_4 * summary(irisDisc4)[4], red_perc_x4_5 * summary(irisDisc4)[5],red_perc_x4_6 * summary(irisDisc4)[6],
                                         red_perc_x4_7 * summary(irisDisc4)[7], red_perc_x4_8 * summary(irisDisc4)[8],red_perc_x4_9 * summary(irisDisc4)[9],
                                         red_perc_x4_10 * summary(irisDisc4)[10])))



Precision_4 = TP_4 / (TP_4 + FP_4); Precision_4 = coalesce(Precision_4, 0)
Recall_4 = TP_4 / (TP_4 + FN_4); Recall_4 = coalesce(Recall_4, 0)
F1_Score_4 = coalesce ( 2 * (Precision_4 * Recall_4) / (Precision_4 + Recall_4), 0)
MCC_4 = (TP_4 * TN_4 - FP_4 * FN_4) / sqrt ( (TP_4 + FP_4) * (TP_4 + FN_4) * (TN_4 + FP_4) * (TN_4 + FN_4) )
MCC_adjusted_4 = (MCC_4 + 1) / 2
P_four_4 = (4 * TP_4 * TN_4) / (4 * TP_4 * TN_4 + (TP_4 + TN_4) * (FP_4 + FN_4)); P_four_4 = coalesce(P_four_4, 0)


par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
pp4a = as.grob(
  
  expression(barplot(ylab="Lift (%)",
                     c(if (red_lift_x4_1 > green_lift_x4_1) {red_lift_x4_1} else {green_lift_x4_1},
                       if (red_lift_x4_2 > green_lift_x4_2) {red_lift_x4_2} else {green_lift_x4_2},
                       if (red_lift_x4_3 > green_lift_x4_3) {red_lift_x4_3} else {green_lift_x4_3},
                       if (red_lift_x4_4 > green_lift_x4_4) {red_lift_x4_4} else {green_lift_x4_4},
                       if (red_lift_x4_5 > green_lift_x4_5) {red_lift_x4_5} else {green_lift_x4_5},
                       if (red_lift_x4_6 > green_lift_x4_6) {red_lift_x4_6} else {green_lift_x4_6},
                       if (red_lift_x4_7 > green_lift_x4_7) {red_lift_x4_7} else {green_lift_x4_7},
                       if (red_lift_x4_8 > green_lift_x4_8) {red_lift_x4_8} else {green_lift_x4_8},
                       if (red_lift_x4_9 > green_lift_x4_9) {red_lift_x4_9} else {green_lift_x4_9},
                       if (red_lift_x4_10 > green_lift_x4_10) {red_lift_x4_10} else {green_lift_x4_10}),
                     col = c(
                       if (red_lift_x4_1 > green_lift_x4_1) {"red"} else {"green"},
                       if (red_lift_x4_2 > green_lift_x4_2) {"red"} else {"green"},
                       if (red_lift_x4_3 > green_lift_x4_3) {"red"} else {"green"},
                       if (red_lift_x4_4 > green_lift_x4_4) {"red"} else {"green"},
                       if (red_lift_x4_5 > green_lift_x4_5) {"red"} else {"green"},
                       if (red_lift_x4_6 > green_lift_x4_6) {"red"} else {"green"},
                       if (red_lift_x4_7 > green_lift_x4_7) {"red"} else {"green"},
                       if (red_lift_x4_8 > green_lift_x4_8) {"red"} else {"green"},
                       if (red_lift_x4_9 > green_lift_x4_9) {"red"} else {"green"},
                       if (red_lift_x4_10 > green_lift_x4_10) {"red"} else {"green"}
                     )
  )
  ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(settings_2(4)$Tag), cex.main = 0.7,
                  c(if (red_lift_x4_1 > green_lift_x4_1) {red_lift_x4_1} else if (red_lift_x4_1 < green_lift_x4_1) {green_lift_x4_1} else {green_lift_x4_1},
                    if (red_lift_x4_2 > green_lift_x4_2) {red_lift_x4_2} else if (red_lift_x4_2 < green_lift_x4_2) {green_lift_x4_2} else {green_lift_x4_2},
                    if (red_lift_x4_3 > green_lift_x4_3) {red_lift_x4_3} else if (red_lift_x4_3 < green_lift_x4_3) {green_lift_x4_3} else {green_lift_x4_3},
                    if (red_lift_x4_4 > green_lift_x4_4) {red_lift_x4_4} else if (red_lift_x4_4 < green_lift_x4_4) {green_lift_x4_4} else {green_lift_x4_4},
                    if (red_lift_x4_5 > green_lift_x4_5) {red_lift_x4_5} else if (red_lift_x4_5 < green_lift_x4_5) {green_lift_x4_5} else {green_lift_x4_5},
                    if (red_lift_x4_6 > green_lift_x4_6) {red_lift_x4_6} else if (red_lift_x4_6 < green_lift_x4_6) {green_lift_x4_6} else {green_lift_x4_6},
                    if (red_lift_x4_7 > green_lift_x4_7) {red_lift_x4_7} else if (red_lift_x4_7 < green_lift_x4_7) {green_lift_x4_7} else {green_lift_x4_7},
                    if (red_lift_x4_8 > green_lift_x4_8) {red_lift_x4_8} else if (red_lift_x4_8 < green_lift_x4_8) {green_lift_x4_8} else {green_lift_x4_8},
                    if (red_lift_x4_9 > green_lift_x4_9) {red_lift_x4_9} else if (red_lift_x4_9 < green_lift_x4_9) {green_lift_x4_9} else {green_lift_x4_9},
                    if (red_lift_x4_10 > green_lift_x4_10) {red_lift_x4_10} else if (red_lift_x4_10 < green_lift_x4_10) {green_lift_x4_10} else {green_lift_x4_10}),
                  col = c(
                    if (red_lift_x4_1 > green_lift_x4_1) {"red"} else {"green"},
                    if (red_lift_x4_2 > green_lift_x4_2) {"red"} else {"green"},
                    if (red_lift_x4_3 > green_lift_x4_3) {"red"} else {"green"},
                    if (red_lift_x4_4 > green_lift_x4_4) {"red"} else {"green"},
                    if (red_lift_x4_5 > green_lift_x4_5) {"red"} else {"green"},
                    if (red_lift_x4_6 > green_lift_x4_6) {"red"} else {"green"},
                    if (red_lift_x4_7 > green_lift_x4_7) {"red"} else {"green"},
                    if (red_lift_x4_8 > green_lift_x4_8) {"red"} else {"green"},
                    if (red_lift_x4_9 > green_lift_x4_9) {"red"} else {"green"},
                    if (red_lift_x4_10 > green_lift_x4_10) {"red"} else {"green"}
                  ),
                  names.arg=c(
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[2],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[3],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[4],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[5],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[6],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[7],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[8],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[9],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[10],1)),
                    paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[11],1))
                  ),
  ),   title(paste(unique(settings_2(4)$Tag), expression("-> {"), round(100*P_four_4, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
  y= (c(if (red_lift_x4_1 > green_lift_x4_1) {red_lift_x4_1} else {green_lift_x4_1},
        if (red_lift_x4_2 > green_lift_x4_2) {red_lift_x4_2} else {green_lift_x4_2},
        if (red_lift_x4_3 > green_lift_x4_3) {red_lift_x4_3} else {green_lift_x4_3},
        if (red_lift_x4_4 > green_lift_x4_4) {red_lift_x4_4} else {green_lift_x4_4},
        if (red_lift_x4_5 > green_lift_x4_5) {red_lift_x4_5} else {green_lift_x4_5},
        if (red_lift_x4_6 > green_lift_x4_6) {red_lift_x4_6} else {green_lift_x4_6},
        if (red_lift_x4_7 > green_lift_x4_7) {red_lift_x4_7} else {green_lift_x4_7},
        if (red_lift_x4_8 > green_lift_x4_8) {red_lift_x4_8} else {green_lift_x4_8},
        if (red_lift_x4_9 > green_lift_x4_9) {red_lift_x4_9} else {green_lift_x4_9},
        if (red_lift_x4_10 > green_lift_x4_10) {red_lift_x4_10} else {green_lift_x4_10}))+4,
  labels=as.character(sapply(c(if (red_lift_x4_1 > green_lift_x4_1) {red_lift_x4_1} else {green_lift_x4_1},
                               if (red_lift_x4_2 > green_lift_x4_2) {red_lift_x4_2} else {green_lift_x4_2},
                               if (red_lift_x4_3 > green_lift_x4_3) {red_lift_x4_3} else {green_lift_x4_3},
                               if (red_lift_x4_4 > green_lift_x4_4) {red_lift_x4_4} else {green_lift_x4_4},
                               if (red_lift_x4_5 > green_lift_x4_5) {red_lift_x4_5} else {green_lift_x4_5},
                               if (red_lift_x4_6 > green_lift_x4_6) {red_lift_x4_6} else {green_lift_x4_6},
                               if (red_lift_x4_7 > green_lift_x4_7) {red_lift_x4_7} else {green_lift_x4_7},
                               if (red_lift_x4_8 > green_lift_x4_8) {red_lift_x4_8} else {green_lift_x4_8},
                               if (red_lift_x4_9 > green_lift_x4_9) {red_lift_x4_9} else {green_lift_x4_9},
                               if (red_lift_x4_10 > green_lift_x4_10) {red_lift_x4_10} else {green_lift_x4_10}),round))
  )))



pp4b = as.grob(expression(barplot(as.matrix(x_4_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc4, which = "discretized:breaks")[1],attr(x = irisDisc4, which = "discretized:breaks")[11])),
                          text(x=barplot(as.matrix(x_4_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[2],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[3],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[4],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[5],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[6],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[7],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[8],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[9],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[10],1)),
                            paste0(round(attr(x = irisDisc4, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc4, which = "discretized:breaks")[11],1))
                          )),
                          y= (summary(irisDisc4))+20, labels=as.character((summary(irisDisc4))))
                          
))


num4 = grid.arrange(grobs=list(as.ggplot(pp4a),as.ggplot(pp4b)))
















P_four_unordered = cbind.data.frame(P_four = c("P_four_1","P_four_2","P_four_3","P_four_4"),
                                    P_four_Score =  c(P_four_1,P_four_2,P_four_3,P_four_4))








P_four_ordered = P_four_unordered[order(P_four_unordered$P_four_Score,decreasing = T),]





# ordered = unordered[order(unordered$Score,decreasing = T),]
# grid.arrange(grobs=PLOTS[as.numeric(rownames(ordered))[1:10]],ncol=5)
# rownames(ordered)
# as.list(as.numeric(rownames(ordered)))
# as.numeric(rownames(ordered))
# as.list(as.numeric(rownames(ordered)))[1:10]



PLOTS = list(num1, num2, num3, num4)

PLOTS_lifts = list(num1[1], num2[1], num3[1], num4[1])




P_four_ordered = P_four_unordered[order(P_four_unordered$P_four_Score,decreasing = T),]
grid.arrange(grobs=PLOTS[as.numeric(rownames(P_four_ordered))[1:4]],ncol=4)











f <- function(x) {
  if(is.list(x) ) lapply(x,f)
  else ifelse(length(x) == 0 | typeof(x)=="double", 0, x)
}






overall_2 <- function(x) {
  for (i in x) {
    
    y =  rbind(
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[1], attr(x = i, which = "discretized:breaks")[1], attr(x = i, which = "discretized:breaks")[2]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[2], attr(x = i, which = "discretized:breaks")[2], attr(x = i, which = "discretized:breaks")[3]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[3], attr(x = i, which = "discretized:breaks")[3], attr(x = i, which = "discretized:breaks")[4]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[4], attr(x = i, which = "discretized:breaks")[4], attr(x = i, which = "discretized:breaks")[5]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[5], attr(x = i, which = "discretized:breaks")[5], attr(x = i, which = "discretized:breaks")[6]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[6], attr(x = i, which = "discretized:breaks")[6], attr(x = i, which = "discretized:breaks")[7]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[7], attr(x = i, which = "discretized:breaks")[7], attr(x = i, which = "discretized:breaks")[8]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[8], attr(x = i, which = "discretized:breaks")[8], attr(x = i, which = "discretized:breaks")[9]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[9], attr(x = i, which = "discretized:breaks")[9], attr(x = i, which = "discretized:breaks")[10]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[10], attr(x = i, which = "discretized:breaks")[10], attr(x = i, which = "discretized:breaks")[11])
    )
    colnames(y) <- c("Level", "Minimum", "Maximum")
    return(
      y
    )
  }
}

overall_2(irisDisc[4])




settings_2 <- function(i) {
  h = as.data.frame(cbind(overall_2(irisDisc[i]), colnames(iris[i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}



settings_2 <- function(i) {
  h = as.data.frame(cbind(overall(irisDisc[i]), colnames(as.data.frame(iris)[2,][i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}



finallist = c()
overall_settings_2 <- function(mylist) {
  for (i in as.numeric(mylist)) {
    finallist <- rbind(finallist,settings_2(i))
  }
  return (finallist)
}










overall <- function(x) {
  for (i in x) {
    
    
    y =  rbind(
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[1], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[11], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[12]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[2], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[12], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[13]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[3], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[13], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[14]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[4], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[14], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[15]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[5], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[15], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[16]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[6], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[16], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[17]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[7], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[17], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[18]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[8], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[18], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[19]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[9], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[19], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[20]),
      c((c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[10], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[20], (c((attributes(i))$levels, (attributes(i))$`discretized:breaks`))[21])
    )
    colnames(y) <- c("Level", "Minimum", "Maximum")
    return(
      y
    )
    
    
  }
  
}








settings <- function(i) {
  h = as.data.frame(cbind(overall(irisDisc[i]), colnames(iris[i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}



settings <- function(i) {
  h = as.data.frame(cbind(overall(irisDisc[i]), colnames(as.data.frame(iris)[2,][i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}


settings_2 <- function(i) {
  h = as.data.frame(cbind(overall_2(irisDisc[i]), colnames(iris[i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}


settings_2 <- function(i) {
  h = as.data.frame(cbind(overall_2(irisDisc[i]), colnames(as.data.frame(iris)[2,][i])))
  colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
  return (h)
}




finallist = c()
overall_settings_2 <- function(mylist) {
  for (i in as.numeric(mylist)) {
    finallist <- rbind(finallist,settings_2(i))
  }
  return (finallist)
}







finallist = c()
overall_settings <- function(mylist) {
  for (i in as.numeric(mylist)) {
    finallist <- rbind(finallist,settings(i))
  }
  return (finallist)
}











settings(4)








library("stringr"); library(ivmte)
str_sub(P_four_ordered$P_four, 8, - 1)
P_four_ordered$tables = as.numeric(str_sub(P_four_ordered$P_four, 8, - 1))
P_four_ordered$Tag = unique(overall_settings(as.numeric(rownames(P_four_ordered)))$Tag)

green_perc_x2_10   # green_perc_x360_10


greens_tables = function(P_four_ordered) {
  for (i in list(head(P_four_ordered$tables,20))) { 
    #for (j in i)
    {
      #return (c(as.factor(paste0("green_perc_x",i,"_1")), as.factor(paste0("green_perc_x",j,"_2")), as.factor(paste0("green_perc_x",j,"_3")), as.factor(paste0("green_perc_x",j,"_4")), as.factor(paste0("green_perc_x",j,"_5")),
      #                                as.factor(paste0("green_perc_x",j,"_6")), as.factor(paste0("green_perc_x",j,"_7")), as.factor(paste0("green_perc_x",j,"_8")), as.factor(paste0("green_perc_x",j,"_9")), as.factor(paste0("green_perc_x",j,"_10"))))}  
      return (list((c(as.factor(paste0("green_perc_x",i,"_1")), as.factor(paste0("green_perc_x",i,"_2")), as.factor(paste0("green_perc_x",i,"_3")), as.factor(paste0("green_perc_x",i,"_4")), as.factor(paste0("green_perc_x",i,"_5")),
                      as.factor(paste0("green_perc_x",i,"_6")), as.factor(paste0("green_perc_x",i,"_7")), as.factor(paste0("green_perc_x",i,"_8")), as.factor(paste0("green_perc_x",i,"_9")), as.factor(paste0("green_perc_x",i,"_10"))))))
    }
  }
}


greens_tables = function(P_four_ordered) {
  for (i in list(head(P_four_ordered$tables,20))) { 
    #for (j in i)
    {
      #return (c(as.factor(paste0("green_perc_x",i,"_1")), as.factor(paste0("green_perc_x",j,"_2")), as.factor(paste0("green_perc_x",j,"_3")), as.factor(paste0("green_perc_x",j,"_4")), as.factor(paste0("green_perc_x",j,"_5")),
      #                                as.factor(paste0("green_perc_x",j,"_6")), as.factor(paste0("green_perc_x",j,"_7")), as.factor(paste0("green_perc_x",j,"_8")), as.factor(paste0("green_perc_x",j,"_9")), as.factor(paste0("green_perc_x",j,"_10"))))}  
      return (list((c((paste0("green_perc_x",i,"_1")), (paste0("green_perc_x",i,"_2")), (paste0("green_perc_x",i,"_3")), (paste0("green_perc_x",i,"_4")), (paste0("green_perc_x",i,"_5")),
                      (paste0("green_perc_x",i,"_6")), (paste0("green_perc_x",i,"_7")), (paste0("green_perc_x",i,"_8")), (paste0("green_perc_x",i,"_9")), (paste0("green_perc_x",i,"_10"))))))
    }
  }
}

greens_tables(P_four_ordered)
unlist(greens_tables(P_four_ordered))
sapply(unlist(greens_tables(P_four_ordered)), sort)
as.numeric(str_sub(P_four_ordered$P_four, 8, -1))
# greens_tables(P_four_ordered)[order(as.numeric(str_sub(P_four_ordered$P_four, 8, -1)),)]    #   ??????????????

# eval(parse(text = levels(as.factor(paste0("green_perc_x",i,"_1")))))






for (i in as.numeric(str_sub(head(P_four_ordered$P_four,20), 8, -1))) {
  print( c (
    (parse(text = levels(as.factor(paste0("green_perc_x",i,"_1"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_2"))))),
    (parse(text = levels(as.factor(paste0("green_perc_x",i,"_3"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_4"))))),
    (parse(text = levels(as.factor(paste0("green_perc_x",i,"_5"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_6"))))), 
    (parse(text = levels(as.factor(paste0("green_perc_x",i,"_7"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_8"))))),
    (parse(text = levels(as.factor(paste0("green_perc_x",i,"_9"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_10"))))))
  )
}




percents = c()
for (i in as.numeric(str_sub(head(P_four_ordered$P_four,20), 8, -1))) {
  for (j in i) {
    print( c (
      cbind(paste0("green_perc_x",j,"_1"), paste0("green_perc_x",j,"_2"), paste0("green_perc_x",j,"_3"), paste0("green_perc_x",j,"_4"), paste0("green_perc_x",j,"_5"),
            paste0("green_perc_x",j,"_6"), paste0("green_perc_x",j,"_7"), paste0("green_perc_x",j,"_8"), paste0("green_perc_x",j,"_9"), paste0("green_perc_x",j,"_10")))
    )
  }
}




percents = c()
green_table = function() {
  for (i in as.numeric(str_sub(head(P_four_ordered$P_four,20), 8, -1))) {
    for (j in i) {
      percents =  
        c(percents, paste0("green_perc_x",j,"_1"), paste0("green_perc_x",j,"_2"), paste0("green_perc_x",j,"_3"), paste0("green_perc_x",j,"_4"), paste0("green_perc_x",j,"_5"),
          paste0("green_perc_x",j,"_6"), paste0("green_perc_x",j,"_7"), paste0("green_perc_x",j,"_8"), paste0("green_perc_x",j,"_9"), paste0("green_perc_x",j,"_10"))
      
    }
  }
  return (percents)
}

green_table()



# sapply(green_table()[1:10])


# sapply(green_table()[101:103], eval(parse))

best_1 = which.max( c( if (exists(as.name(green_table()[1]))) {eval(parse(text = green_table()[1]))}, if  (exists(as.name(green_table()[2]))) {eval(parse(text = green_table()[2]))}, 
                       if  (exists(as.name(green_table()[3]))) {eval(parse(text = green_table()[3]))}, if  (exists(as.name(green_table()[4]))) {eval(parse(text = green_table()[4]))},
                       if  (exists(as.name(green_table()[5]))) {eval(parse(text = green_table()[5]))}, if  (exists(as.name(green_table()[6]))) {eval(parse(text = green_table()[6]))}, 
                       if  (exists(as.name(green_table()[7]))) {eval(parse(text = green_table()[7]))}, if  (exists(as.name(green_table()[8]))) {eval(parse(text = green_table()[8]))},
                       if  (exists(as.name(green_table()[9]))) {eval(parse(text = green_table()[9]))}, if (exists(as.name(green_table()[10]))) {eval(parse(text = green_table()[10]))}  ) )




best_2 = which.max( c( if (exists(as.name(green_table()[11]))) {eval(parse(text = green_table()[11]))}, if  (exists(as.name(green_table()[12]))) {eval(parse(text = green_table()[12]))}, 
                       if  (exists(as.name(green_table()[13]))) {eval(parse(text = green_table()[13]))}, if  (exists(as.name(green_table()[14]))) {eval(parse(text = green_table()[14]))},
                       if  (exists(as.name(green_table()[15]))) {eval(parse(text = green_table()[15]))}, if  (exists(as.name(green_table()[16]))) {eval(parse(text = green_table()[16]))}, 
                       if  (exists(as.name(green_table()[17]))) {eval(parse(text = green_table()[17]))}, if  (exists(as.name(green_table()[18]))) {eval(parse(text = green_table()[18]))},
                       if  (exists(as.name(green_table()[19]))) {eval(parse(text = green_table()[19]))}, if (exists(as.name(green_table()[20]))) {eval(parse(text = green_table()[20]))}  ) )


best_3 = which.max( c( if (exists(as.name(green_table()[21]))) {eval(parse(text = green_table()[21]))}, if  (exists(as.name(green_table()[22]))) {eval(parse(text = green_table()[22]))}, 
                       if  (exists(as.name(green_table()[23]))) {eval(parse(text = green_table()[23]))}, if  (exists(as.name(green_table()[24]))) {eval(parse(text = green_table()[24]))},
                       if  (exists(as.name(green_table()[25]))) {eval(parse(text = green_table()[25]))}, if  (exists(as.name(green_table()[26]))) {eval(parse(text = green_table()[26]))}, 
                       if  (exists(as.name(green_table()[27]))) {eval(parse(text = green_table()[27]))}, if  (exists(as.name(green_table()[28]))) {eval(parse(text = green_table()[28]))},
                       if  (exists(as.name(green_table()[29]))) {eval(parse(text = green_table()[29]))}, if (exists(as.name(green_table()[30]))) {eval(parse(text = green_table()[30]))}  ) )


best_4 = which.max( c( if (exists(as.name(green_table()[31]))) {eval(parse(text = green_table()[31]))}, if  (exists(as.name(green_table()[32]))) {eval(parse(text = green_table()[32]))}, 
                       if  (exists(as.name(green_table()[33]))) {eval(parse(text = green_table()[33]))}, if  (exists(as.name(green_table()[34]))) {eval(parse(text = green_table()[34]))},
                       if  (exists(as.name(green_table()[35]))) {eval(parse(text = green_table()[35]))}, if  (exists(as.name(green_table()[36]))) {eval(parse(text = green_table()[36]))}, 
                       if  (exists(as.name(green_table()[37]))) {eval(parse(text = green_table()[37]))}, if  (exists(as.name(green_table()[38]))) {eval(parse(text = green_table()[38]))},
                       if  (exists(as.name(green_table()[39]))) {eval(parse(text = green_table()[39]))}, if (exists(as.name(green_table()[40]))) {eval(parse(text = green_table()[40]))}  ) )


best_5 = which.max( c( if (exists(as.name(green_table()[41]))) {eval(parse(text = green_table()[41]))}, if  (exists(as.name(green_table()[42]))) {eval(parse(text = green_table()[42]))}, 
                       if  (exists(as.name(green_table()[43]))) {eval(parse(text = green_table()[43]))}, if  (exists(as.name(green_table()[44]))) {eval(parse(text = green_table()[44]))},
                       if  (exists(as.name(green_table()[45]))) {eval(parse(text = green_table()[45]))}, if  (exists(as.name(green_table()[46]))) {eval(parse(text = green_table()[46]))}, 
                       if  (exists(as.name(green_table()[47]))) {eval(parse(text = green_table()[47]))}, if  (exists(as.name(green_table()[48]))) {eval(parse(text = green_table()[48]))},
                       if  (exists(as.name(green_table()[49]))) {eval(parse(text = green_table()[49]))}, if (exists(as.name(green_table()[50]))) {eval(parse(text = green_table()[50]))}  ) )



best_6 = which.max( c( if (exists(as.name(green_table()[51]))) {eval(parse(text = green_table()[51]))}, if  (exists(as.name(green_table()[52]))) {eval(parse(text = green_table()[52]))}, 
                       if  (exists(as.name(green_table()[53]))) {eval(parse(text = green_table()[53]))}, if  (exists(as.name(green_table()[54]))) {eval(parse(text = green_table()[54]))},
                       if  (exists(as.name(green_table()[55]))) {eval(parse(text = green_table()[55]))}, if  (exists(as.name(green_table()[56]))) {eval(parse(text = green_table()[56]))}, 
                       if  (exists(as.name(green_table()[57]))) {eval(parse(text = green_table()[57]))}, if  (exists(as.name(green_table()[58]))) {eval(parse(text = green_table()[58]))},
                       if  (exists(as.name(green_table()[59]))) {eval(parse(text = green_table()[59]))}, if (exists(as.name(green_table()[60]))) {eval(parse(text = green_table()[60]))}  ) )


best_7 = which.max( c( if (exists(as.name(green_table()[61]))) {eval(parse(text = green_table()[61]))}, if  (exists(as.name(green_table()[62]))) {eval(parse(text = green_table()[62]))}, 
                       if  (exists(as.name(green_table()[63]))) {eval(parse(text = green_table()[63]))}, if  (exists(as.name(green_table()[64]))) {eval(parse(text = green_table()[64]))},
                       if  (exists(as.name(green_table()[65]))) {eval(parse(text = green_table()[65]))}, if  (exists(as.name(green_table()[66]))) {eval(parse(text = green_table()[66]))}, 
                       if  (exists(as.name(green_table()[67]))) {eval(parse(text = green_table()[67]))}, if  (exists(as.name(green_table()[68]))) {eval(parse(text = green_table()[68]))},
                       if  (exists(as.name(green_table()[69]))) {eval(parse(text = green_table()[69]))}, if (exists(as.name(green_table()[70]))) {eval(parse(text = green_table()[70]))}  ) )


best_8 = which.max( c( if (exists(as.name(green_table()[71]))) {eval(parse(text = green_table()[71]))}, if  (exists(as.name(green_table()[72]))) {eval(parse(text = green_table()[72]))}, 
                       if  (exists(as.name(green_table()[73]))) {eval(parse(text = green_table()[73]))}, if  (exists(as.name(green_table()[74]))) {eval(parse(text = green_table()[74]))},
                       if  (exists(as.name(green_table()[75]))) {eval(parse(text = green_table()[75]))}, if  (exists(as.name(green_table()[76]))) {eval(parse(text = green_table()[76]))}, 
                       if  (exists(as.name(green_table()[77]))) {eval(parse(text = green_table()[77]))}, if  (exists(as.name(green_table()[78]))) {eval(parse(text = green_table()[78]))},
                       if  (exists(as.name(green_table()[79]))) {eval(parse(text = green_table()[79]))}, if (exists(as.name(green_table()[80]))) {eval(parse(text = green_table()[80]))}  ) )

best_9 = which.max( c( if (exists(as.name(green_table()[81]))) {eval(parse(text = green_table()[81]))}, if  (exists(as.name(green_table()[82]))) {eval(parse(text = green_table()[82]))}, 
                       if  (exists(as.name(green_table()[83]))) {eval(parse(text = green_table()[83]))}, if  (exists(as.name(green_table()[84]))) {eval(parse(text = green_table()[84]))},
                       if  (exists(as.name(green_table()[85]))) {eval(parse(text = green_table()[85]))}, if  (exists(as.name(green_table()[86]))) {eval(parse(text = green_table()[86]))}, 
                       if  (exists(as.name(green_table()[87]))) {eval(parse(text = green_table()[87]))}, if  (exists(as.name(green_table()[88]))) {eval(parse(text = green_table()[88]))},
                       if  (exists(as.name(green_table()[89]))) {eval(parse(text = green_table()[89]))}, if (exists(as.name(green_table()[90]))) {eval(parse(text = green_table()[90]))}  ) )


best_10 = which.max( c( if (exists(as.name(green_table()[91]))) {eval(parse(text = green_table()[91]))}, if  (exists(as.name(green_table()[92]))) {eval(parse(text = green_table()[92]))}, 
                        if  (exists(as.name(green_table()[93]))) {eval(parse(text = green_table()[93]))}, if  (exists(as.name(green_table()[94]))) {eval(parse(text = green_table()[94]))},
                        if  (exists(as.name(green_table()[95]))) {eval(parse(text = green_table()[95]))}, if  (exists(as.name(green_table()[96]))) {eval(parse(text = green_table()[96]))}, 
                        if  (exists(as.name(green_table()[97]))) {eval(parse(text = green_table()[97]))}, if  (exists(as.name(green_table()[98]))) {eval(parse(text = green_table()[98]))},
                        if  (exists(as.name(green_table()[99]))) {eval(parse(text = green_table()[99]))}, if (exists(as.name(green_table()[100]))) {eval(parse(text = green_table()[100]))}  ) )


best_11 = which.max( c( if (exists(as.name(green_table()[101]))) {eval(parse(text = green_table()[101]))}, if  (exists(as.name(green_table()[102]))) {eval(parse(text = green_table()[102]))}, 
                        if  (exists(as.name(green_table()[103]))) {eval(parse(text = green_table()[103]))}, if  (exists(as.name(green_table()[104]))) {eval(parse(text = green_table()[104]))},
                        if  (exists(as.name(green_table()[105]))) {eval(parse(text = green_table()[105]))}, if  (exists(as.name(green_table()[106]))) {eval(parse(text = green_table()[106]))}, 
                        if  (exists(as.name(green_table()[107]))) {eval(parse(text = green_table()[107]))}, if  (exists(as.name(green_table()[108]))) {eval(parse(text = green_table()[108]))},
                        if  (exists(as.name(green_table()[109]))) {eval(parse(text = green_table()[109]))}, if (exists(as.name(green_table()[110]))) {eval(parse(text = green_table()[110]))}  ) )


best_12 = which.max( c( if (exists(as.name(green_table()[111]))) {eval(parse(text = green_table()[111]))}, if  (exists(as.name(green_table()[112]))) {eval(parse(text = green_table()[112]))}, 
                        if  (exists(as.name(green_table()[113]))) {eval(parse(text = green_table()[113]))}, if  (exists(as.name(green_table()[114]))) {eval(parse(text = green_table()[114]))},
                        if  (exists(as.name(green_table()[115]))) {eval(parse(text = green_table()[115]))}, if  (exists(as.name(green_table()[116]))) {eval(parse(text = green_table()[116]))}, 
                        if  (exists(as.name(green_table()[117]))) {eval(parse(text = green_table()[117]))}, if  (exists(as.name(green_table()[118]))) {eval(parse(text = green_table()[118]))},
                        if  (exists(as.name(green_table()[119]))) {eval(parse(text = green_table()[119]))}, if (exists(as.name(green_table()[120]))) {eval(parse(text = green_table()[120]))}  ) )


best_13 = which.max( c( if (exists(as.name(green_table()[121]))) {eval(parse(text = green_table()[121]))}, if  (exists(as.name(green_table()[122]))) {eval(parse(text = green_table()[122]))}, 
                        if  (exists(as.name(green_table()[123]))) {eval(parse(text = green_table()[123]))}, if  (exists(as.name(green_table()[124]))) {eval(parse(text = green_table()[124]))},
                        if  (exists(as.name(green_table()[125]))) {eval(parse(text = green_table()[125]))}, if  (exists(as.name(green_table()[126]))) {eval(parse(text = green_table()[126]))}, 
                        if  (exists(as.name(green_table()[127]))) {eval(parse(text = green_table()[127]))}, if  (exists(as.name(green_table()[128]))) {eval(parse(text = green_table()[128]))},
                        if  (exists(as.name(green_table()[129]))) {eval(parse(text = green_table()[129]))}, if (exists(as.name(green_table()[130]))) {eval(parse(text = green_table()[130]))}  ) )


best_14 = which.max( c( if (exists(as.name(green_table()[131]))) {eval(parse(text = green_table()[131]))}, if  (exists(as.name(green_table()[132]))) {eval(parse(text = green_table()[132]))}, 
                        if  (exists(as.name(green_table()[133]))) {eval(parse(text = green_table()[133]))}, if  (exists(as.name(green_table()[134]))) {eval(parse(text = green_table()[134]))},
                        if  (exists(as.name(green_table()[135]))) {eval(parse(text = green_table()[135]))}, if  (exists(as.name(green_table()[136]))) {eval(parse(text = green_table()[136]))}, 
                        if  (exists(as.name(green_table()[137]))) {eval(parse(text = green_table()[137]))}, if  (exists(as.name(green_table()[138]))) {eval(parse(text = green_table()[138]))},
                        if  (exists(as.name(green_table()[139]))) {eval(parse(text = green_table()[139]))}, if (exists(as.name(green_table()[140]))) {eval(parse(text = green_table()[140]))}  ) )




best_15 = which.max( c( if (exists(as.name(green_table()[141]))) {eval(parse(text = green_table()[141]))}, if  (exists(as.name(green_table()[142]))) {eval(parse(text = green_table()[142]))}, 
                        if  (exists(as.name(green_table()[143]))) {eval(parse(text = green_table()[143]))}, if  (exists(as.name(green_table()[144]))) {eval(parse(text = green_table()[144]))},
                        if  (exists(as.name(green_table()[145]))) {eval(parse(text = green_table()[145]))}, if  (exists(as.name(green_table()[146]))) {eval(parse(text = green_table()[146]))}, 
                        if  (exists(as.name(green_table()[147]))) {eval(parse(text = green_table()[147]))}, if  (exists(as.name(green_table()[148]))) {eval(parse(text = green_table()[148]))},
                        if  (exists(as.name(green_table()[149]))) {eval(parse(text = green_table()[149]))}, if (exists(as.name(green_table()[150]))) {eval(parse(text = green_table()[150]))}  ) )




best_16 = which.max( c( if (exists(as.name(green_table()[151]))) {eval(parse(text = green_table()[151]))}, if  (exists(as.name(green_table()[152]))) {eval(parse(text = green_table()[152]))}, 
                        if  (exists(as.name(green_table()[153]))) {eval(parse(text = green_table()[153]))}, if  (exists(as.name(green_table()[154]))) {eval(parse(text = green_table()[154]))},
                        if  (exists(as.name(green_table()[155]))) {eval(parse(text = green_table()[155]))}, if  (exists(as.name(green_table()[156]))) {eval(parse(text = green_table()[156]))}, 
                        if  (exists(as.name(green_table()[157]))) {eval(parse(text = green_table()[157]))}, if  (exists(as.name(green_table()[158]))) {eval(parse(text = green_table()[158]))},
                        if  (exists(as.name(green_table()[159]))) {eval(parse(text = green_table()[159]))}, if (exists(as.name(green_table()[160]))) {eval(parse(text = green_table()[160]))}  ) )




best_17 = which.max( c( if (exists(as.name(green_table()[161]))) {eval(parse(text = green_table()[161]))}, if  (exists(as.name(green_table()[162]))) {eval(parse(text = green_table()[162]))}, 
                        if  (exists(as.name(green_table()[163]))) {eval(parse(text = green_table()[163]))}, if  (exists(as.name(green_table()[164]))) {eval(parse(text = green_table()[164]))},
                        if  (exists(as.name(green_table()[165]))) {eval(parse(text = green_table()[165]))}, if  (exists(as.name(green_table()[166]))) {eval(parse(text = green_table()[166]))}, 
                        if  (exists(as.name(green_table()[167]))) {eval(parse(text = green_table()[167]))}, if  (exists(as.name(green_table()[168]))) {eval(parse(text = green_table()[168]))},
                        if  (exists(as.name(green_table()[169]))) {eval(parse(text = green_table()[169]))}, if (exists(as.name(green_table()[170]))) {eval(parse(text = green_table()[170]))}  ) )




best_18 = which.max( c( if (exists(as.name(green_table()[171]))) {eval(parse(text = green_table()[171]))}, if  (exists(as.name(green_table()[172]))) {eval(parse(text = green_table()[172]))}, 
                        if  (exists(as.name(green_table()[173]))) {eval(parse(text = green_table()[173]))}, if  (exists(as.name(green_table()[174]))) {eval(parse(text = green_table()[174]))},
                        if  (exists(as.name(green_table()[175]))) {eval(parse(text = green_table()[175]))}, if  (exists(as.name(green_table()[176]))) {eval(parse(text = green_table()[176]))}, 
                        if  (exists(as.name(green_table()[177]))) {eval(parse(text = green_table()[177]))}, if  (exists(as.name(green_table()[178]))) {eval(parse(text = green_table()[178]))},
                        if  (exists(as.name(green_table()[179]))) {eval(parse(text = green_table()[179]))}, if (exists(as.name(green_table()[180]))) {eval(parse(text = green_table()[180]))}  ) )




best_19 = which.max( c( if (exists(as.name(green_table()[181]))) {eval(parse(text = green_table()[181]))}, if  (exists(as.name(green_table()[182]))) {eval(parse(text = green_table()[182]))}, 
                        if  (exists(as.name(green_table()[183]))) {eval(parse(text = green_table()[183]))}, if  (exists(as.name(green_table()[184]))) {eval(parse(text = green_table()[184]))},
                        if  (exists(as.name(green_table()[185]))) {eval(parse(text = green_table()[185]))}, if  (exists(as.name(green_table()[186]))) {eval(parse(text = green_table()[186]))}, 
                        if  (exists(as.name(green_table()[187]))) {eval(parse(text = green_table()[187]))}, if  (exists(as.name(green_table()[188]))) {eval(parse(text = green_table()[188]))},
                        if  (exists(as.name(green_table()[189]))) {eval(parse(text = green_table()[189]))}, if (exists(as.name(green_table()[190]))) {eval(parse(text = green_table()[190]))}  ) )


best_20 = which.max( c( if (exists(as.name(green_table()[191]))) {eval(parse(text = green_table()[191]))}, if  (exists(as.name(green_table()[192]))) {eval(parse(text = green_table()[192]))}, 
                        if  (exists(as.name(green_table()[193]))) {eval(parse(text = green_table()[193]))}, if  (exists(as.name(green_table()[194]))) {eval(parse(text = green_table()[194]))},
                        if  (exists(as.name(green_table()[195]))) {eval(parse(text = green_table()[195]))}, if  (exists(as.name(green_table()[196]))) {eval(parse(text = green_table()[196]))}, 
                        if  (exists(as.name(green_table()[197]))) {eval(parse(text = green_table()[197]))}, if  (exists(as.name(green_table()[198]))) {eval(parse(text = green_table()[198]))},
                        if  (exists(as.name(green_table()[199]))) {eval(parse(text = green_table()[199]))}, if (exists(as.name(green_table()[200]))) {eval(parse(text = green_table()[200]))}  ) )



length(levels(irisDisc4))




shortlist = iris[,unique(overall_settings(rownames(head(P_four_ordered,20)))$Tag)]
colnames(shortlist)
















variables_1_4 = read_excel("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/IBR_KW_Filterability_files/shortlist_mini.xlsx")
test_data = as.data.frame(variables_1_4)[,c(1,5,6,4,3)]
shortlist_new = shortlist


colnames(test_data)

colnames(test_data) = c("Batch End Date", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", 
                        "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", "BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1")



# test_data = sapply(test_data, as.numeric)


colnames(shortlist_new)
shortlist_new <<- shortlist_new
colnames(shortlist_new)

test_data_for_analysis = test_data[,-c(1)]
colnames(test_data_for_analysis)


IBR_KW_Filterability = iris[,"KW"]
shortlist_new = cbind(shortlist_new, IBR_KW_Filterability)
colnames(shortlist_new)

























table = rbind.data.frame(
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10]))) %in% as.vector((colnames(test_data_for_analysis))))
    c( {overall_settings(rownames(head(P_four_ordered,20)))[1:10,][best_1,]},
       if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[1:10,][best_1,][2]) ) {
         paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[1:10,][best_1,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10])]), 1))),collapse="")
       } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[1:10,][best_1,][3])) {
         as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[1:10,][best_1,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10])]), 1))
       } else {
         0
       },
       as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[1:10])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20]))) %in% as.vector((colnames(test_data_for_analysis))))
    c( {overall_settings(rownames(head(P_four_ordered,20)))[11:20,][best_2,]},
       if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[11:20,][best_2,][2]) ) {
         paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[11:20,][best_2,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20])]), 1))),collapse="")
       } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[11:20,][best_2,][3])) {
         as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[11:20,][best_2,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20])]), 1))
       } else {
         0
       },
       as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[11:20])]), 1))),
  
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[21:30,][best_3,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[21:30,][best_3,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[21:30,][best_3,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[21:30,][best_3,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[21:30,][best_3,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[21:30])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[31:40,][best_4,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[31:40,][best_4,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[31:40,][best_4,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[31:40,][best_4,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[31:40,][best_4,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[31:40])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[41:50,][best_5,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50]))) %in% as.vector((colnames(test_data_for_analysis)))) 
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[41:50,][best_5,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[41:50,][best_5,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[41:50,][best_5,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[41:50,][best_5,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[41:50])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[51:60,][best_6,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[51:60,][best_6,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[51:60,][best_6,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[51:60,][best_6,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[51:60,][best_6,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[51:60])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[61:70,][best_7,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[61:70,][best_7,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[61:70,][best_7,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[61:70,][best_7,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[61:70,][best_7,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[61:70])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[71:80,][best_8,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[71:80,][best_8,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[71:80,][best_8,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[71:80,][best_8,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[71:80,][best_8,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[71:80])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[81:90,][best_9,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[81:90,][best_9,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[81:90,][best_9,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[81:90,][best_9,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[81:90,][best_9,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[81:90])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[91:100,][best_10,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[91:100,][best_10,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[91:100,][best_10,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[91:100,][best_10,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[91:100,][best_10,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[91:100])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[101:110,][best_11,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[101:110,][best_11,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[101:110,][best_11,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[101:110,][best_11,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[101:110,][best_11,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[101:110])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[111:120,][best_12,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[111:120,][best_12,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[111:120,][best_12,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[111:120,][best_12,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[111:120,][best_12,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[111:120])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[121:130,][best_13,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[121:130,][best_13,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[121:130,][best_13,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[121:130,][best_13,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[121:130,][best_13,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[121:130])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[131:140,][best_14,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[131:140,][best_14,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[131:140,][best_14,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[131:140,][best_14,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[131:140,][best_14,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[131:140])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[141:150,][best_15,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[141:150,][best_15,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[141:150,][best_15,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[141:150,][best_15,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[141:150,][best_15,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[141:150])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[151:160,][best_16,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[151:160,][best_16,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[151:160,][best_16,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[151:160,][best_16,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[151:160,][best_16,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[151:160])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[161:170,][best_17,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[161:170,][best_17,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[161:170,][best_17,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[161:170,][best_17,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[161:170,][best_17,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[161:170])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[171:180,][best_18,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[171:180,][best_18,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[171:180,][best_18,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[171:180,][best_18,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[171:180,][best_18,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[171:180])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[181:190,][best_19,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[181:190,][best_19,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[181:190,][best_19,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[181:190,][best_19,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[181:190,][best_19,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[181:190])]), 1))),
  
  if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200]))) %in% as.vector((colnames(test_data_for_analysis))))
    c ( {overall_settings(rownames(head(P_four_ordered,20)))[191:200,][best_20,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200]))) %in% as.vector((colnames(test_data_for_analysis))))
          if (as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200])]), 1)) < as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[191:200,][best_20,][2]) ) {
            paste(c("+",as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[191:200,][best_20,][2]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200])]), 1)) > as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[191:200,][best_20,][3])) {
            as.numeric(overall_settings(rownames(head(P_four_ordered,20)))[191:200,][best_20,][3]) - as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(test_data_for_analysis[,unique(overall_settings_2(rownames(head(P_four_ordered,20)))$Tag[191:200])]), 1)))
)



colnames(table) <- c("Level", "Optimal Minimum |", "| Optimal Maximum", "Tag", "Recommended Changes |", "| Actual")



table[,2] = sapply(table[,2], as.numeric); table[,3] = sapply(table[,3], as.numeric); table[,5] = sapply(table[,5], as.numeric); 
table <- table %>%
  mutate(across(where(is.numeric), ~ round(.,2)))
#func_for_appending_plus_sign = function (x) {ifelse(x > 0, paste(c("↑",x),collapse=" "), paste(c("↓",x*(-1)),collapse=" "))}
#func_for_appending_plus_sign = function (x) {ifelse(x > 0, paste(c("+",x),collapse=" "), paste(c("-",x*(-1)),collapse=" "))}
func_for_appending_plus_sign = function (x) {ifelse(x > 0, paste(c("↑ ",x),collapse=" "), paste(c("↓ ",x*(-1)),collapse=" "))}
table[5] = sapply(unlist(sapply(table[5],as.numeric)), func_for_appending_plus_sign)
#func_for_replacing_downward_arrow_sign_and_zero = function (x) {ifelse(x == "↓ 0", replace(x, 1, 0), x)}   # df <- c('apple', 'orange', 'grape', 'banana')  #replace(df, 2, 'blueberry')
#func_for_replacing_downward_arrow_sign_and_zero = function (x) {ifelse(x == "- 0", replace(x, 1, 0), x)}   # df <- c('apple', 'orange', 'grape', 'banana')  #replace(df, 2, 'blueberry')
func_for_replacing_downward_arrow_sign_and_zero = function (x) {ifelse(x == "↓ 0", replace(x, 1, 0), x)}   # df <- c('apple', 'orange', 'grape', 'banana')  #replace(df, 2, 'blueberry')
table[5] = sapply(unlist(table[5]), func_for_replacing_downward_arrow_sign_and_zero)
table = table[,-c(1)]
table











library(knitr); library(kableExtra);  #  https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html     #  https://www.rdocumentation.org/packages/knitr/versions/1.45/topics/kable
kable(table, format = "html", table.attr = "id=\"mytable\"")
kable(head(table, 4), align = 'c', booktabs = TRUE, linesep = " ") %>%
  row_spec(1:4, bold = FALSE, italic = FALSE, color = 'white', background = 'black')













library(rgl)
library(scatterplot3d)
iris = as.data.frame(iris)
with(iris, plot3d(iris[,c(1,2,3)], type ="s", col=as.integer(iris[,"Loop"])))

with(iris, plot3d(iris[,c(1,2,3)], type ="s", col="blue"))





length(iris[,c(1,2,3)])




# library(datasets)
# data(iris)
# summary(iris)
# str(iris)
# length(iris[,1:3])


























library(openxlsx)
library(dplyr)
library(xgboost)
library(caret)

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
### parts = createDataPartition(shortlist$IBR_KW_Filterability, p = .8, list = F)
### train = shortlist[parts, ]
### test = shortlist[-parts, ]



#split into training (70%), testing set (10%), and testing set (20%)
idxtrain <- sample(nrow(shortlist_new),as.integer(nrow(shortlist_new)*0.7)); train = shortlist_new[idxtrain,]
idxNot_train <- which(! 1:nrow(shortlist_new) %in% idxtrain )
idxVal <- sample(idxNot_train,as.integer(length(idxNot_train)*0.333)); Val = shortlist_new[idxVal,]
idxtest <- idxNot_train[which(! idxNot_train %in% idxVal)]; test = shortlist_new[idxtest,]


#define predictor and response variables in training set
train_x = data.matrix(train[, -ncol(train)])
train_y = train[,ncol(train)]


#define predictor and response variables in Validation set
Val_x = data.matrix(Val[, -ncol(train)])
Val_y = Val[,ncol(train)]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -ncol(train)])
test_y = test[, ncol(train)]

#define final training, validation, and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_Val = xgb.DMatrix(data = Val_x, label = Val_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist Val
watchlist_Val = list(train=xgb_train, test=xgb_Val)


# Grid Search to find the best hyperparameter combinations
max.depths = c(3, 5, 7)
etas = c(0.01, 0.001, 0.0001)

best_params = 0
best_score = 0

count = 1
for( depth in max.depths ){
  for( num in etas){
    
    bst_grid = xgb.train(data = xgb_train, 
                         max.depth = depth, 
                         eta=num, 
                         nthread = 4, 
                         nround = 10000, 
                         watchlist = watchlist_Val, 
                         objective = "reg:squarederror", 
                         early_stopping_rounds = 50, 
                         verbose=0)
    
    if(count == 1){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
      count = count + 1
    }
    else if( bst_grid$best_score < best_score){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
    }
  }
}

best_params
best_score


#define watchlist test
watchlist_test = list(train=xgb_train, test=xgb_test)

# applying the best paramaters on the test set
# max_depth of best_params$max_depth, eta of best_params$eta, nthread of best_params$nthread
bst_tuned = xgb.train( data = xgb_train, 
                       max.depth = best_params$max_depth, 
                       eta = best_params$eta, 
                       nthread = best_params$nthread, 
                       nround = 10000, 
                       watchlist = watchlist_test, 
                       objective = "reg:squarederror", 
                       early_stopping_rounds = 50,
                       print_every_n = 500)

y_hat_xgb_grid = predict(bst_tuned, xgb_test)

test_mse = mean(((y_hat_xgb_grid - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse





#fit XGBoost model and display training and Val data at each round
### model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist_Val, nrounds = 200)

### best_nrounds = which(model$evaluation_log$test_rmse == min(model$evaluation_log$test_rmse))

#fit XGBoost model and display training and testing data at each round
### model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist_test, nrounds = 200)

### best_nrounds = which(model$evaluation_log$test_rmse == min(model$evaluation_log$test_rmse))
best_nrounds = which(bst_tuned$evaluation_log$test_rmse == min(bst_tuned$evaluation_log$test_rmse))

#define final model
### final_model = xgboost(data = xgb_train, max.depth = 3, nrounds = best_nrounds, verbose = 0)
final_model = xgboost(data = xgb_train, max.depth = best_params$max_depth, nrounds = best_nrounds, verbose = 0)

# Use the Model to Make Predictions on Test data
pred_y = predict(final_model, test_x)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse

par(mfrow = c(1, 1), mai = c(1, 1, 1, 1))
plot(x=test_y, y=pred_y, main = "Predicted vs Actual plot", ylab = "Predicted IBR KW", xlab="Actual IBR KW", mgp=c(3,1,0)); abline(0,1, col="red", lwd=2)

plot(idxtest, test_y, type="l", col="green", ylab="IBR KW", xlab="Time", lwd=2)   #  plot(idxtest, test_y, type="p", col="green")
lines(idxtest, pred_y, col="purple", lwd=2)
legend(1000, 550, legend=c("Actual", "Predicted"),
       col=c("green", "purple"), lty=1:2, cex=0.8)

# Use the Model to Make Predictions on Unknown data
pred_y_unknown_data = predict(final_model, data.matrix(test_data_for_analysis))


# accuracy = 100 - mean(100*(abs(predicted-actual)/actual))
accuracy = 100 - mean(100*(abs(pred_y-test_y)/test_y))   #   accuracy = 100 - mean(100*(abs(pred_y-test_y)/(mean(test_y) - test_y)))
paste(round(accuracy, 2), "%")


RelativeSquaredError = sum((pred_y - test_y)^2) / sum((mean(test_y) - test_y)^2); RelativeAccuracy = 1 - RelativeSquaredError;
RelativeAbsoluteError = sum(abs(pred_y - test_y)) / sum(abs(mean(test_y) - test_y)); RelativeAbsoluteAccuracy = 1 - RelativeAbsoluteError
caret::RMSE(test_y, pred_y) / mean(test_y); sd(test_y); mean(test_y); sd(test_y) / mean(test_y)  # coefficient of variation
caret::RMSE(test_y, pred_y) / sd(test_y)








mse_test_baseline = (1/length(test_y))*sum((test_y-mean(test_y))^2)
test_R_squared = 1-((caret::RMSE(test_y, pred_y))^2/(mse_test_baseline))
test_adjusted_R_squared = 1-((1-(test_R_squared)^2)*(length(test_y)-1))/(length(test_y)-ncol(test_x)-1)







#Feature importance
library(Ckmeans.1d.dp)
xgb_imp <- xgb.importance(
  feature_names = colnames(xgb_train),
  model = final_model)

Plot_20 = xgb.ggplot.importance(xgb_imp,top_n=18,n_clusters = c(3)) + scale_fill_manual(values=c("lightgreen", "green", 'green4')) +
  ggtitle("Priority List") +
  theme_bw()+
  theme(legend.position="none") + theme(panel.background = element_rect(fill = 'navy', color = 'salmon4')) +
  theme(text = element_text(size = 10, colour = "purple")) + theme(plot.background = element_rect(fill = 'lightblue1', color = 'salmon4')) +
  theme(axis.text = element_text(color = 'brown', face = "bold"))
xgb_imp$Importance















library(corrplot)
library(ggcorrplot)

par(bg = 'olivedrab')
raw_corr_plot <<- cor((na.omit(sapply(as.data.frame(raw_iris[,c(1,2,3,4,5,6)]), as.numeric))),method="s")
# corrplot(raw_iris[,1:3],method = "pie", type="upper", tl.col="white", tl.cex=0.5, addgrid.col="black");
colnames(raw_corr_plot) = c("2ND O2 COMPRESSOR", "ZSTG REACT DISCHRG DEGAS", "Ozone Flow to Mixer #1", "DOSAGE Ozone Flow to Mixer #1", "Loop", "KW")
corrplot(raw_corr_plot, method = "pie", type="upper", tl.col="black", tl.cex=0.8, addgrid.col="black")
par(bg = 'white')









colnames(raw_iris)
countour_iris = as.data.frame(raw_iris[,c(1,2,5)])
library(plotly)
library(reshape2)
countour_iris <- melt(sapply(unname(countour_iris),as.numeric))

p <- ggplot(as.data.frame(countour_iris), aes(Var1, Var2, z= value)) +
  stat_contour(geom="polygon",aes(fill=stat(level))) +
  scale_fill_distiller(palette = "Spectral", direction = -1) + labs(x = "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", y = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS")

ggplotly(p)










colnames(raw_iris)
countour_iris = as.data.frame(raw_iris[,c(1,2,6)])
library(plotly)
library(reshape2)
countour_iris <- melt(sapply(unname(countour_iris),as.numeric), na.rm = TRUE, value.name = "value")

p <- ggplot(as.data.frame(countour_iris), aes(Var1, Var2, z= value, colour=stat(level))) +
  geom_contour() +
  scale_colour_distiller(palette = "YlGn", direction = 1)

ggplotly(p)






countour_iris = (sapply(unname(countour_iris),as.numeric))


colnames(countour_iris) = c("Var1", "Var2", "value")


p <- ggplot(as.data.frame(countour_iris), aes(Var1, Var2, z= value)) +
  geom_contour() +
  scale_colour_distiller(palette = "YlGn", direction = 1)

ggplotly(p)




contour(raw_iris[,1], raw_iris[,2], raw_iris[,6])

z = kde2d(raw_iris[,1], raw_iris[,2], n = 50)

plot(raw_iris[,1], raw_iris[,2], pch = 19)
contour(z, lwd = 2, add = TRUE, col = hcl.colors(10, "Spectral"))



library(hexbin)
plot(hexbin(raw_iris[,1], raw_iris[,2]))


x <- -10:10
y <- -10:10
z <- sqrt(outer(x ^ 2, y ^ 2, "+"))

contour(cbind(raw_iris[,1], t(raw_iris[,2]), diag(raw_iris[,6])))

contour(cbind(raw_iris[,1], (raw_iris[,2]), raw_iris[,6]))

contour(cbind(raw_iris[,1], (raw_iris[,2]), raw_iris[,6]), max.contour.segments=20)


plot(raw_iris[,1], raw_iris[,2], pch = 19)
contour(as.data.frame(raw_iris[,6]), lwd = 2, add = TRUE, col = hcl.colors(10, "Spectral"))



outer(raw_iris[,1], raw_iris[,2], diag(raw_iris[,6]))




diag(raw_iris[,6])








c = 1
old <- data.frame(raw_iris[,1], raw_iris[,2], raw_iris[,6], c)
colnames(old) <- c("A", "B", "C", "D")

p <- plot_ly(old, x = ~A, y = ~B, z = ~C, type = 'scatter3d', mode = 'lines+markers',
             line = list(width = 6, color = ~c, colorscale = 'Viridis'),
             marker = list(size = 3.5, color = ~D, colorscale = 'Blacks', cmin = -20, cmax = 50)) %>%
  layout(
    title = "3D representation of Average.RPM, Average.WOB and Ideal.ROP relationships",
    scene = list(
      xaxis = list(title = "RPMF"),
      yaxis = list(title = "WOBF"),
      zaxis = list(title = "Ideal")
    ))
p





library(plotly)
install.packages("rgl", dependencies = TRUE)
install.packages("manipulateWidget", dependencies = TRUE, type = "binary")
install.packages("reshape2", dependencies = TRUE)
library(reshape2)
install.packages("akima", dependencies = TRUE)
library(akima)



# Interaction plots (3D)
a = interp(raw_iris[,1], raw_iris[,2], raw_iris[,6], duplicate="strip")

plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "surface", colorscale = 'Jet',
        contours = list(
          start = min(a$z),
          end = max(a$z)+1,
          size = 0.5))%>%
  plotly::layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title= 'BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR'),yaxis = list(title= 'BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS'),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE) 





# Interaction plots with contours (3D)
a = interp(raw_iris[,1], raw_iris[,2], raw_iris[,6], nx = 500, ny = 500, duplicate="strip")

a22 = interp(raw_iris[,1], raw_iris[,2], raw_iris[,6], nx = 500, ny = 500, duplicate="strip")

fig <- plot_ly(y=a$y,x=a$x,  z=matrix(a$z, nrow = length(a$y), byrow = TRUE), colorscale = 'Jet') %>%  
  add_surface(surfacecolor=a22$z) %>%
  add_surface(
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)


fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig










# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 150))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)







# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 50))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)








# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 25))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)






# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 10))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)









# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 5))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)






# Interaction plots (2D)
plot_ly(x = a$x, 
        y = a$y,
        z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", colorscale = 'Jet',
        contours = list(
          start = min(raw_iris[,6]),
          end = max(raw_iris[,6])+1,
          size = 10))%>%
  layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title="BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR"),yaxis = list(title="BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS"),
         title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)









library(plotly)
install.packages("rgl", dependencies = TRUE)
install.packages("manipulateWidget", dependencies = TRUE, type = "binary")
install.packages("reshape2", dependencies = TRUE)
library(reshape2)
install.packages("akima", dependencies = TRUE)
library(akima)

a = interp(raw_iris[,1], raw_iris[,2], raw_iris[,6], duplicate="strip")

names(a) <- c("BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "KW")

plot_ly(x = a~`BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR`, 
        y = a~`BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS`,
        z = matrix(a~KW, nrow = length(a~`BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS`), byrow = TRUE),
        type = "surface", colorscale = 'Jet',
        contours = list(
          start = min(z),
          end = max(z)+1,
          size = 0.5))%>%
  plotly::layout(legend=list(y=0.8,yanchor="top"),xaxis = list(title= 'BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR'),yaxis = list(title= 'BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS'),
                 title = "2ND O2 COMPRESSOR and ZSTG REACT DISCHRG DEGAS interactions with KW"
  ) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE) 










sum(raw_iris[,6]>250)
raw_iris[,6]

# Interaction plots (4D)
a = interp(raw_iris[,1], raw_iris[,2], raw_iris[,3], nx = 500, ny = 500, duplicate="strip")

a22 = interp(raw_iris[,1], raw_iris[,2], raw_iris[,6], nx = 500, ny = 500, duplicate="strip")

plot_ly(y=a$y,x=a$x,  z=matrix(a$z, nrow = length(a$y), byrow = TRUE), colorscale = 'Jet') %>%  
  add_surface(surfacecolor=a22$z) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)















Mat = matrix(raw_iris[,6], nrow = 200, ncol = 200, byrow = TRUE)

Mat = matrix(raw_iris[,6], nrow = length(raw_iris[,6]), byrow = TRUE)

Mat = matrix(raw_iris[,6], nrow = length(raw_iris[,6]), ncol = length(raw_iris[,6]), byrow = TRUE)


plot_ly(y=a$y,x=a$x,  z=matrix(a$z, nrow = length(a$y), byrow = TRUE), colorscale = 'Jet') %>%  
  add_surface(surfacecolor=a22$z) %>%
  add_annotations( xref="paper", yref="paper",x=0.95, xanchor="left",
                   y=0.99, yanchor="bottom",yanchor="bottom",text="KW", legendtitle=TRUE, showarrow=FALSE)

plot_ly(y=a$y,x=a$x,  z=matrix(a$z, nrow = length(a$y), byrow = TRUE), color=a22$z<250) %>%  
  add_markers() %>%
  scale_fill_manual(name = "NES",  labels = c("GOOD", "BAD"))




data_for_plotly = as.data.frame(datecomb_222)
colnames(data_for_plotly) = c("Date", unique(settings_2(317)$Tag), unique(settings_2(222)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~"WSH_525_TI_411.PV | 2POW LVL FIL PV", color=~(data_for_plotly[,4] == 1), colors=c("red","green")) %>%
  add_markers() %>%
  scale_fill_manual(name = "NES",  labels = c("GOOD", "BAD"),  values = c(FALSE = "green", TRUE = "red"))






##############################################################################################
###################################  1  ######################################################
##############################################################################################


# TWO PARAMETER MODEL

# aaa = within(shortlist, shortlist$Level_1 <- ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`<1315, 0, 
#                                                     ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`>=1315, 1, 2)))

test_data_for_analysis = test_data[,-c(1)]
colnames(test_data_for_analysis)

shortlist = iris[,unique(overall_settings(rownames(head(P_four_ordered,4)))$Tag)]
shortlist_new = shortlist
shortlist_new <<- shortlist_new

IBR_KW_Filterability = iris[,"KW"]
shortlist_new = cbind(shortlist_new, IBR_KW_Filterability)


shortlist_new = as.data.frame(shortlist_new)
shortlist_new[,'Loop'] <- iris[,'Loop']
shortlist_new$binary_IBR <- shortlist_new$IBR_KW_Filterability

shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 257.6 & shortlist_new$Loop == 1)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 154.3 & shortlist_new$Loop == 2)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 257.6 & shortlist_new$Loop == 1)] <- 0  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 154.3 & shortlist_new$Loop == 2)] <- 0


#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW", "binary_IBR")]
#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)", "binary_IBR")]
shortlist_new_3_params = shortlist_new[,c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "binary_IBR")]
colnames(shortlist_new_3_params)



shortlist_new = as.data.frame(shortlist_new)
with(shortlist_new, plot3d(shortlist_new[,c(1,2,3)], type ="s", col=as.integer(shortlist_new[,"binary_IBR"]+2)))

library(rgl)
# play3d( spin3d( axis = c(0, 0, 1), rpm = 20,dev = cur3d()),startTime = 0, duration = 10 )

movie3d(spin3d(axis=c(0,0,1), rpm=15), duration = 15, dir = "./")


with(shortlist_new, plot3d(shortlist_new[,c(1,2,4)], type ="s", col=as.integer(shortlist_new[,"binary_IBR"]+2)))
with(shortlist_new, plot3d(shortlist_new[,c(1,2,4)][which(shortlist_new[,"BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1"] > 0),], type ="s", col=as.integer(shortlist_new[,"binary_IBR"]+2)))

movie3d(spin3d(axis=c(0,0,1), rpm=4), duration = 15, dir = "./")


# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(shortlist_new_3_params$binary_IBR, SplitRatio = 0.75)
training_set = subset(shortlist_new_3_params, split == TRUE)
test_set = subset(shortlist_new_3_params, split == FALSE)



#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW")]
#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)")]
test_data_for_analysis = test_data_for_analysis[,c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR")]
colnames(test_data_for_analysis)



# Building a Two Parameter Model (Non-GLM "XGBoost Model")
#make this example reproducible
set.seed(0)






###


# Fitting the Non-GLM Model (Gradient Boosted Trees model, i.e. XGBoost)
############# Building the XGBoost Model
#install.packages('xgboost')
library(xgboost)
set.seed(1234)
nonglm_classifier <- xgboost(data = as.matrix(training_set[-3]),
                             label = as.matrix(training_set$binary_IBR),
                             eta = 0.01,
                             max_depth = 5,
                             nround=2000,
                             subsample = 0.5,
                             colsample_bytree = 0.4,
                             set.seed = 1234,
                             eval_metric = "logloss",
                             objective = "binary:logistic",
                             gamma = 0.05,
                             nthread = 3,
                             set.seed(1234)
)






# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy_2 = mean(as.numeric(cv))
accuracy_2



# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
P_four = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
  return(P_four_2_param)
})
P_four_2_param = mean(as.numeric(P_four))
P_four_2_param






# Predicting on the Test set and making Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier, newdata = as.matrix(test_set[-3]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
P_four_2_param




##################################################################





# Building a Two Parameter Model (Non-GLM "XGBoost Model")
set.seed(1234)
nonglm_classifier2 <- xgboost(data = data.matrix(training_set[c(1,2)]),
                              label = training_set$binary_IBR,
                              eta = 0.01,
                              max_depth = 5,
                              nround=2000,
                              subsample = 0.5,
                              colsample_bytree = 0.4,
                              set.seed = 1234,
                              eval_metric = "logloss",
                              objective = "binary:logistic",
                              gamma = 0.05,
                              nthread = 3,
                              set.seed(1234)
)


# Predicting the Test set results and the Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier2, newdata = as.matrix(test_set[c(1,2)]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
P_four_2_param














# Green vs. Red

# Visualizing the Training set results (Non-GLM "XGBoost Model")
set = training_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 0.05)  # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)') 
colnames(grid_set) = c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Training set)',
     xlab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", ylab = "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', #ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# Visualizing the Test set results (Non-GLM "XGBoost Model")
set = test_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 0.05)   # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)')
colnames(grid_set) = c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Test set)',
     xlab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", ylab = "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', # ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))













































##############################################################################################
###################################  2  ######################################################
##############################################################################################


# TWO PARAMETER MODEL

# aaa = within(shortlist, shortlist$Level_1 <- ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`<1315, 0, 
#                                                     ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`>=1315, 1, 2)))

test_data_for_analysis = test_data[,-c(1)]
colnames(test_data_for_analysis)

shortlist = iris[,unique(overall_settings(rownames(head(P_four_ordered,4)))$Tag)]
shortlist_new = shortlist
shortlist_new <<- shortlist_new

IBR_KW_Filterability = iris[,"KW"]
shortlist_new = cbind(shortlist_new, IBR_KW_Filterability)


shortlist_new = as.data.frame(shortlist_new)
shortlist_new[,'Loop'] <- iris[,'Loop']
shortlist_new$binary_IBR <- shortlist_new$IBR_KW_Filterability

shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 257.6 & shortlist_new$Loop == 1)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 154.3 & shortlist_new$Loop == 2)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 257.6 & shortlist_new$Loop == 1)] <- 0  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 154.3 & shortlist_new$Loop == 2)] <- 0


#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW", "binary_IBR")]
#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)", "binary_IBR")]
shortlist_new_3_params = shortlist_new[,c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", "binary_IBR")]
colnames(shortlist_new_3_params)






# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(shortlist_new_3_params$binary_IBR, SplitRatio = 0.75)
training_set = subset(shortlist_new_3_params, split == TRUE)
test_set = subset(shortlist_new_3_params, split == FALSE)



#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW")]
#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)")]
test_data_for_analysis = test_data_for_analysis[,c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")]
colnames(test_data_for_analysis)



# Building a Two Parameter Model (Non-GLM "XGBoost Model")
#make this example reproducible
set.seed(0)






###


# Fitting the Non-GLM Model (Gradient Boosted Trees model, i.e. XGBoost)
############# Building the XGBoost Model
#install.packages('xgboost')
library(xgboost)
set.seed(1234)
nonglm_classifier <- xgboost(data = as.matrix(training_set[-3]),
                             label = as.matrix(training_set$binary_IBR),
                             eta = 0.01,
                             max_depth = 5,
                             nround=2000,
                             subsample = 0.5,
                             colsample_bytree = 0.4,
                             set.seed = 1234,
                             eval_metric = "logloss",
                             objective = "binary:logistic",
                             gamma = 0.05,
                             nthread = 3,
                             set.seed(1234)
)






# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy_2 = mean(as.numeric(cv))
accuracy_2





# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
P_four = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
  return(P_four_2_param)
})
P_four_2_param = mean(as.numeric(P_four))
P_four_2_param








# Predicting on the Test set and making Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier, newdata = as.matrix(test_set[-3]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
accuracy
P_four_2_param



##################################################################





# Building a Two Parameter Model (Non-GLM "XGBoost Model")
set.seed(1234)
nonglm_classifier2 <- xgboost(data = data.matrix(training_set[c(1,2)]),
                              label = training_set$binary_IBR,
                              eta = 0.01,
                              max_depth = 5,
                              nround=2000,
                              subsample = 0.5,
                              colsample_bytree = 0.4,
                              set.seed = 1234,
                              eval_metric = "logloss",
                              objective = "binary:logistic",
                              gamma = 0.05,
                              nthread = 3,
                              set.seed(1234)
)


# Predicting the Test set results and the Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier2, newdata = as.matrix(test_set[c(1,2)]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
P_four_2_param




# Green vs. Red

# Visualizing the Training set results (Non-GLM "XGBoost Model")
set = training_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 2)  # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)') 
colnames(grid_set) = c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Training set)',
     xlab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", ylab = "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', #ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# Visualizing the Test set results (Non-GLM "XGBoost Model")
set = test_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 2)   # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)')
colnames(grid_set) = c("BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Test set)',
     xlab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", ylab = "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', # ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))








































##############################################################################################
###################################  3  ######################################################
##############################################################################################


# TWO PARAMETER MODEL

# aaa = within(shortlist, shortlist$Level_1 <- ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`<1315, 0, 
#                                                     ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`>=1315, 1, 2)))

test_data_for_analysis = test_data[,-c(1)]
colnames(test_data_for_analysis)

shortlist = iris[,unique(overall_settings(rownames(head(P_four_ordered,4)))$Tag)]
shortlist_new = shortlist
shortlist_new <<- shortlist_new

IBR_KW_Filterability = iris[,"KW"]
shortlist_new = cbind(shortlist_new, IBR_KW_Filterability)


shortlist_new = as.data.frame(shortlist_new)
shortlist_new[,'Loop'] <- iris[,'Loop']
shortlist_new$binary_IBR <- shortlist_new$IBR_KW_Filterability

shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 257.6 & shortlist_new$Loop == 1)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 154.3 & shortlist_new$Loop == 2)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 257.6 & shortlist_new$Loop == 1)] <- 0  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 154.3 & shortlist_new$Loop == 2)] <- 0


#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW", "binary_IBR")]
#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)", "binary_IBR")]
shortlist_new_3_params = shortlist_new[,c("BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", "binary_IBR")]
colnames(shortlist_new_3_params)






# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(shortlist_new_3_params$binary_IBR, SplitRatio = 0.75)
training_set = subset(shortlist_new_3_params, split == TRUE)
test_set = subset(shortlist_new_3_params, split == FALSE)



#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW")]
#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)")]
test_data_for_analysis = test_data_for_analysis[,c("BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")]
colnames(test_data_for_analysis)



# Building a Two Parameter Model (Non-GLM "XGBoost Model")
#make this example reproducible
set.seed(0)






###


# Fitting the Non-GLM Model (Gradient Boosted Trees model, i.e. XGBoost)
############# Building the XGBoost Model
#install.packages('xgboost')
library(xgboost)
set.seed(1234)
nonglm_classifier <- xgboost(data = as.matrix(training_set[-3]),
                             label = as.matrix(training_set$binary_IBR),
                             eta = 0.01,
                             max_depth = 5,
                             nround=2000,
                             subsample = 0.5,
                             colsample_bytree = 0.4,
                             set.seed = 1234,
                             eval_metric = "logloss",
                             objective = "binary:logistic",
                             gamma = 0.05,
                             nthread = 3,
                             set.seed(1234)
)






# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy_2 = mean(as.numeric(cv))
accuracy_2







# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
P_four = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
  return(P_four_2_param)
})
P_four_2_param = mean(as.numeric(P_four))
P_four_2_param









# Predicting on the Test set and making Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier, newdata = as.matrix(test_set[-3]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
P_four_2_param




##################################################################





# Building a Two Parameter Model (Non-GLM "XGBoost Model")
set.seed(1234)
nonglm_classifier2 <- xgboost(data = data.matrix(training_set[c(1,2)]),
                              label = training_set$binary_IBR,
                              eta = 0.01,
                              max_depth = 5,
                              nround=2000,
                              subsample = 0.5,
                              colsample_bytree = 0.4,
                              set.seed = 1234,
                              eval_metric = "logloss",
                              objective = "binary:logistic",
                              gamma = 0.05,
                              nthread = 3,
                              set.seed(1234)
)


# Predicting the Test set results and the Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier2, newdata = as.matrix(test_set[c(1,2)]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy





# Green vs. Red

# Visualizing the Training set results (Non-GLM "XGBoost Model")
set = training_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 2)  # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)') 
colnames(grid_set) = c("BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Training set)',
     xlab = "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", ylab = "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', #ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# Visualizing the Test set results (Non-GLM "XGBoost Model")
set = test_set
X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.05)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 2)   # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)')
colnames(grid_set) = c("BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Test set)',
     xlab = "BLP_530_FI_2418.PV | 2ND O2 COMPRESSOR", ylab = "BLP_530_FFIC2208.PV | Ozone Flow to Mixer #1", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', # ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))





































##############################################################################################
###################################  4  ######################################################
##############################################################################################


# TWO PARAMETER MODEL

# aaa = within(shortlist, shortlist$Level_1 <- ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`<1315, 0, 
#                                                     ifelse(`546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)`>=1315, 1, 2)))

test_data_for_analysis = test_data[,-c(1)]
colnames(test_data_for_analysis)

shortlist = iris[,unique(overall_settings(rownames(head(P_four_ordered,4)))$Tag)]
shortlist_new = shortlist
shortlist_new <<- shortlist_new

IBR_KW_Filterability = iris[,"KW"]
shortlist_new = cbind(shortlist_new, IBR_KW_Filterability)


shortlist_new = as.data.frame(shortlist_new)
shortlist_new[,'Loop'] <- iris[,'Loop']
shortlist_new$binary_IBR <- shortlist_new$IBR_KW_Filterability

shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 257.6 & shortlist_new$Loop == 1)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability <= 154.3 & shortlist_new$Loop == 2)] <- 1  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 257.6 & shortlist_new$Loop == 1)] <- 0  
shortlist_new$binary_IBR[which(shortlist_new$IBR_KW_Filterability > 154.3 & shortlist_new$Loop == 2)] <- 0


#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW", "binary_IBR")]
#shortlist = shortlist[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)", "binary_IBR")]
shortlist_new_3_params = shortlist_new[,c("BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", "binary_IBR")]
colnames(shortlist_new_3_params)






# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(shortlist_new_3_params$binary_IBR, SplitRatio = 0.75)
training_set = subset(shortlist_new_3_params, split == TRUE)
test_set = subset(shortlist_new_3_params, split == FALSE)



#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW")]
#test_data_for_analysis = test_data_for_analysis[,c("546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)", "546_IT_400A.PV | TWP BOTTOM LOAD (%)")]
test_data_for_analysis = test_data_for_analysis[,c("BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS")]
colnames(test_data_for_analysis)



# Building a Two Parameter Model (Non-GLM "XGBoost Model")
#make this example reproducible
set.seed(0)






###


# Fitting the Non-GLM Model (Gradient Boosted Trees model, i.e. XGBoost)
############# Building the XGBoost Model
#install.packages('xgboost')
library(xgboost)
set.seed(1234)
nonglm_classifier <- xgboost(data = as.matrix(training_set[-3]),
                             label = as.matrix(training_set$binary_IBR),
                             eta = 0.01,
                             max_depth = 5,
                             nround=2000,
                             subsample = 0.5,
                             colsample_bytree = 0.4,
                             set.seed = 1234,
                             eval_metric = "logloss",
                             objective = "binary:logistic",
                             gamma = 0.05,
                             nthread = 3,
                             set.seed(1234)
)






# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy_2 = mean(as.numeric(cv))
accuracy_2





# Applying k-Fold Cross Validation to evaluate model performance (Non-GLM "XGBoost Model")
#install.packages('caret')
library(caret)
folds = createFolds(training_set$binary_IBR, k = 10)
P_four = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  nonglm_classifier = xgboost(data = as.matrix(training_set[-3]), label = training_set$binary_IBR,
                              objective = "binary:logistic", nrounds=500)
  y_pred = predict(nonglm_classifier, newdata = as.matrix(test_fold[-3]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[,3], y_pred)
  P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
  return(P_four_2_param)
})
P_four_2_param = mean(as.numeric(P_four))
P_four_2_param








# Predicting on the Test set and making Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier, newdata = as.matrix(test_set[-3]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
accuracy
P_four_2_param



##################################################################





# Building a Two Parameter Model (Non-GLM "XGBoost Model")
set.seed(1234)
nonglm_classifier2 <- xgboost(data = data.matrix(training_set[c(1,2)]),
                              label = training_set$binary_IBR,
                              eta = 0.01,
                              max_depth = 5,
                              nround=2000,
                              subsample = 0.5,
                              colsample_bytree = 0.4,
                              set.seed = 1234,
                              eval_metric = "logloss",
                              objective = "binary:logistic",
                              gamma = 0.05,
                              nthread = 3,
                              set.seed(1234)
)


# Predicting the Test set results and the Confusion Matrix (Non-GLM "XGBoost Model")
y_pred = predict(nonglm_classifier2, newdata = as.matrix(test_set[c(1,2)]))
y_pred = (y_pred >= 0.5)
cm = table(test_set[,3], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy
P_four_2_param = (4 * cm[1,1] * cm[2,2]) / (4 * (cm[1,1] * cm[2,2]) + (cm[1,1] + cm[2,2]) * (cm[1,2] + cm[2,1]))
P_four_2_param




# Green vs. Red

# Visualizing the Training set results (Non-GLM "XGBoost Model")
set = training_set
#X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.001)   # for x-axis
X1 = seq(max(0,min(data.frame(set[,1])) - 1), max(data.frame(set[,1])) + 0.1, by = 0.001)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 0.05)  # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)') 
colnames(grid_set) = c("BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Training set)',
     xlab = "BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", ylab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', #ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# Visualizing the Test set results (Non-GLM "XGBoost Model")
set = test_set
#X1 = seq(min(data.frame(set[,1])) - 1, max(data.frame(set[,1])) + 1, by = 0.001)   # for x-axis
X1 = seq(max(0,min(data.frame(set[,1])) - 1), max(data.frame(set[,1])) + 0.1, by = 0.001)   # for x-axis
X2 = seq(min(data.frame(set[,2])) - 1, max(data.frame(set[,2])) + 1, by = 0.05)   # for y-axis
grid_set = expand.grid(X1, X2)
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW')
#colnames(grid_set) = c('546PI415DS.PV | TWP ZONE 3  DRIVE SIDE PRESSURE (PSI)', '546_IT_400A.PV | TWP BOTTOM LOAD (%)')
colnames(grid_set) = c("BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS")
prob_set = predict(nonglm_classifier2, newdata = as.matrix(grid_set))
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(as.matrix(set[,c(1,2)]),
     main = 'XGBoost Model (Test set)',
     xlab = "BLP_530_FFIC2208.dosage_PV | Ozone Flow to Mixer #1", ylab = "BLP_530_PDIC2214.PV | ZSTG REACT DISCHRG DEGAS", # ylab = '546_IT_400A.PV | TWP BOTTOM LOAD (%)', # ylab = 'BLP_530_FI_2193.PV | OZON COMPRESSOR FLOW',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points((set[,c(1,2)]), pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

















































plot(as.POSIXct(test_data[,1]), pred_y_unknown_data)





























library(ggthemes)
ggplot() +
  geom_line(data=((cbind(as.data.frame((as.POSIXct(test_data[,1])))))),
            aes(y = (pred_y_unknown_data), x=(as.POSIXct(test_data[,1])), color = "Predicted"), linewidth=1) +
  xlab("Date") + ylab("IBR KW Filterability") +
  scale_x_datetime(date_breaks = "7 day", date_labels = "%Y/%m/%d") +
  #scale_color_manual(values=c('orange','brown')) + ggtitle("Raw data") +
  #theme_wsj()+ scale_colour_wsj("colors6") +
  theme_wsj()+ scale_colour_wsj(palette = "colors6") +
  ggtitle("IBR KW Filterability (Forecast July 2023 ->)") +
  theme(legend.position = "top", legend.title = element_blank(), text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.key.height = unit(1, "cm"), legend.key.width = unit(5,"cm")) 






ggplot() +
  geom_line(data=((cbind(as.data.frame((as.POSIXct(test_data[,1])))))),
            aes(y = (pred_y_unknown_data), x=(as.POSIXct(test_data[,1])), color = "predicted"), linewidth=1.5) +
  geom_line(data=cbind.data.frame(raw_with_NAs[,1],as.POSIXct(raw_with_NAs[,2])),
            aes(y = ((raw_with_NAs[,1])), x=(as.POSIXct(raw_with_NAs[,2])), color = "predicted"), linewidth=1.5) +
  xlab("Date") + ylab("IBR KW Filterability") +
  scale_x_datetime(date_breaks = "7 day", date_labels = "%Y/%m/%d") +
  #scale_color_manual(values=c('orange','brown')) + ggtitle("Raw data") +
  #theme_wsj()+ scale_colour_wsj("colors6") +
  theme_wsj()+ scale_colour_wsj(palette = "colors6") +
  ggtitle("IBR KW Filterability (Forecast July 2023 ->)") +
  theme(legend.position = "top", legend.title = element_blank(), text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.key.height = unit(1, "cm"), legend.key.width = unit(5,"cm")) 



ggplot() +
  geom_line(data=cbind.data.frame(raw_with_NAs[,7],as.POSIXct(raw_with_NAs[,1])),
            aes(y = ((raw_with_NAs[,7])), x=(as.POSIXct(raw_with_NAs[,1])), color = "Actual"), linewidth=1) +
  xlab("Date") + ylab("IBR KW Filterability") +
  scale_x_datetime(date_breaks = "60 day", date_labels = "%Y/%m/%d") +
  #scale_color_manual(values=c('orange','brown')) + ggtitle("Raw data") +
  #theme_wsj()+ scale_colour_wsj("colors6") +
  theme_wsj(color="blue") + scale_colour_wsj(palette = "black_green") +
  ggtitle("IBR KW Filterability (-> Actual until August 2023)") +
  theme(legend.position = "top", legend.title = element_blank(), text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.key.height = unit(1, "cm"), legend.key.width = unit(5,"cm")) 







geom_line(data=((cbind(as.data.frame(((raw_with_NAs[,1:2])))))),
          aes(y = ((raw_with_NAs[,1])), x=(as.POSIXct.Date(raw_with_NAs[,2])), color = "predicted"), linewidth=1.5)



plot(x=(as.POSIXct(raw_with_NAs[,2])), y = ((raw_with_NAs[,1])))





library(ggthemes)
ggplot() +
  geom_line(data=((cbind(as.data.frame((as.POSIXct(test_data[,1])))))),
            aes(y = (pred_y_unknown_data), x=(as.POSIXct(test_data[,1])), color = "predicted"), linewidth=0.8) +
  xlab("Date") + ylab("IBR KW Filterability") +
  scale_x_datetime(date_breaks = "7 day", date_labels = "%Y/%m/%d") +
  #scale_color_manual(values=c('orange','brown')) + ggtitle("Raw data") +
  #theme_wsj()+ scale_colour_wsj("colors6") +
  theme_wsj()+ scale_colour_wsj(palette = "colors6") +
  ggtitle("IBR KW Filterability (Forecast July 2023 ->)") +
  theme(legend.position = "top", legend.title = element_blank(), text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.key.height = unit(1, "cm"), legend.key.width = unit(5,"cm")) 

















# Library
library(tidyverse)

# Create data
data <- data.frame(
  x=P_four_ordered$Tag[1:4],
  y=round(P_four_ordered$P_four_Score[1:4] * 100, 0)
)

# plot
ggplot(data, aes(x=fct_inorder(x), y=y)) +
  geom_segment( aes(x=fct_inorder(x), xend=fct_inorder(x), y=0, yend=y), color="green", size=1) +
  geom_point( size=5, color="green", fill=alpha("plum4", 0.3), alpha=0.7, shape=19, stroke=2) +
  ylab("P_four Score (%)") + xlab("Process Variables") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0, 100) + 
  theme(panel.background = element_rect(fill = "plum4"), panel.grid.major=element_line(colour="green", size=0.5, linetype = 4), 
        panel.grid.minor=element_line(colour="white", size=0.25,linetype = 3))




# plot
ggplot(data, aes(x=fct_inorder(x), y=y)) +
  geom_segment( aes(x=fct_inorder(x), xend=fct_inorder(x), y=0, yend=y), color="red", size=1) +
  geom_point( size=5, color="red", fill=alpha("plum4", 0.3), alpha=0.7, shape=19, stroke=2) +
  ylab("P_four Score (%)") + xlab("Process Variables") + theme(axis.text.x = element_text(angle = 95, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_text(colour = "blue")) +
  ylim(0, 100) + 
  theme(panel.background = element_rect(fill = "lightyellow1"), plot.background = element_rect(fill = "lightpink"),
        panel.grid.major=element_line(colour="green", size=0.5, linetype = 4), 
        panel.grid.minor=element_line(colour="white", size=0.25,linetype = 3))




# plot
ggplot(data, aes(x=fct_inorder(x), y=y)) +
  geom_segment( aes(x=fct_inorder(x), xend=fct_inorder(x), y=0, yend=y), color="black", size=1) +
  geom_point( size=5, color="black", fill=alpha("plum4", 0.3), alpha=0.7, shape=19, stroke=2) +
  ylab("P_four Score (%)") + xlab("Process Variables") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0, 100) + 
  theme(panel.background = element_rect(fill = "plum4"), panel.grid.major=element_line(colour="green", size=0.5, linetype = 4), 
        panel.grid.minor=element_line(colour="white", size=0.25,linetype = 3))

























raw_for_date_plots$binary_IBR <- raw_for_date_plots$KW

raw_for_date_plots$binary_IBR[which(raw_for_date_plots$KW <= 257.6 & raw_for_date_plots$Loop == 1)] <- 1  
raw_for_date_plots$binary_IBR[which(raw_for_date_plots$KW <= 154.3 & raw_for_date_plots$Loop == 2)] <- 1  
raw_for_date_plots$binary_IBR[which(raw_for_date_plots$KW > 257.6 & raw_for_date_plots$Loop == 1)] <- 0  
raw_for_date_plots$binary_IBR[which(raw_for_date_plots$KW > 154.3 & raw_for_date_plots$Loop == 2)] <- 0



# raw_for_date_plots = raw_for_date_plots[, c(3,2,1,4)]










datecomb_1 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,6], raw_for_date_plots[,2], raw_for_date_plots[,8])
dateplot_1 = ggplot(data=datecomb_1[,c(1:4)],aes(x=datecomb_1[,c(1)], y=datecomb_1[,c(3)], color=(datecomb_1[,c(4)] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(1)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 



datecomb_2 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,6], raw_for_date_plots[,3], raw_for_date_plots[,8])
dateplot_2 = ggplot(data=datecomb_2[,c(1:4)],aes(x=datecomb_2[,1], y=datecomb_2[,3], color=(datecomb_2[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(2)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy"))


datecomb_3 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,6], raw_for_date_plots[,4], raw_for_date_plots[,8])
dateplot_3 = ggplot(data=datecomb_3[,c(1:4)],aes(x=datecomb_3[,1], y=datecomb_3[,3], color=(datecomb_3[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(3)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 


datecomb_4 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,6], raw_for_date_plots[,5], raw_for_date_plots[,8])
dateplot_4 = ggplot(data=datecomb_4[,c(1:4)],aes(x=datecomb_4[,1], y=datecomb_4[,3], color=(datecomb_4[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(4)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) +
  ylim(0, 0.5)









PLOTS_dateplots = list(dateplot_1, dateplot_2, dateplot_3, dateplot_4)

PLOTS_dateplots[10]
PLOTS_dateplots[307]
PLOTS_dateplots[244]
grid.arrange(grobs=PLOTS_dateplots[as.numeric(rownames(P_four_ordered))[1:4]],ncol=2)
grid.arrange(grobs=PLOTS_dateplots[as.numeric(rownames(P_four_ordered))[1:20]],ncol=5)
grid.arrange(grobs=PLOTS_dateplots[as.numeric(rownames(P_four_ordered))[297:316]],ncol=5)
grid.arrange(grobs=PLOTS_dateplots[1:20],ncol=5)
library(DescTools)
grid.arrange(grobs=PLOTS_dateplots[Some(1:316, n=20)],ncol=5)









datecomb_317 = cbind.data.frame(raw_for_date_plots[,319], raw_for_date_plots[,317], raw_for_date_plots[,317], raw_for_date_plots[,320])
dateplot_317 = ggplot(data=datecomb_317[,c(1:4)],aes(x=datecomb_317[,1], y=datecomb_317[,3], color=(datecomb_317[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(317)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 





datecomb_318 = cbind.data.frame(raw_for_date_plots[,319], raw_for_date_plots[,317], raw_for_date_plots[,318], raw_for_date_plots[,320])
dateplot_318 = ggplot(data=datecomb_318[,c(1:4)],aes(x=datecomb_318[,1], y=datecomb_318[,3], color=(datecomb_318[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(318)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 











datecomb_5 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,5], raw_for_date_plots[,5], raw_for_date_plots[,7])
dateplot_5 = ggplot(data=datecomb_5[,c(1:4)],aes(x=datecomb_5[,1], y=datecomb_5[,3], color=(datecomb_5[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(4)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 





datecomb_6 = cbind.data.frame(raw_for_date_plots[,1], raw_for_date_plots[,5], raw_for_date_plots[,6], raw_for_date_plots[,7])
dateplot_6 = ggplot(data=datecomb_6[,c(1:4)],aes(x=datecomb_6[,1], y=datecomb_6[,3], color=(datecomb_6[,4] == 1))) + 
  geom_point(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("red","green")) + geom_point(shape = 19, size = 2) + 
  ylab(unique(settings_2(5)$Tag)) + xlab("Date") + theme(panel.background = element_rect(fill = 'ivory4', color = "purple"), plot.background = element_rect(fill = 'powderblue')) +
  theme(axis.title = element_text(color = "navy")) 







library(plotly)


data_for_plotly = as.data.frame(datecomb_6)
colnames(data_for_plotly) = c("Date", unique(settings_2(4)$Tag), unique(settings_2(5)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~KW, color=~(data_for_plotly[,4] == 1), colors=c("red","green")) 
p









f1 <- list(
  family = "Arial, sans-serif",
  size = 25,
  color = "white"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
  
)
a <- list(
  title = "SALES PER SONG",
  titlefont = f1,
  showgrid = FALSE,
  showticklabels = TRUE,
  showline=TRUE,
  tickangle = 45,
  tickfont = f2
)

data_for_plotly = as.data.frame(datecomb_6)
colnames(data_for_plotly) = c("Date", unique(settings_2(4)$Tag), unique(settings_2(5)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~KW, color=~(data_for_plotly[,4] == 1), colors=c("red","green"))  %>%
  layout(xaxis = a, yaxis = a) %>% 
  layout(plot_bgcolor='black') %>% 
  layout(paper_bgcolor='black') 
p  










f1 <- list(
  family = "Arial, sans-serif",
  size = 25,
  color = "white"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
  
)
a <- list(
  title = "SALES PER SONG",
  titlefont = f1,
  showgrid = FALSE,
  showticklabels = TRUE,
  showline=TRUE,
  tickangle = 45,
  tickfont = f2
)

data_for_plotly = as.data.frame(datecomb_318)
colnames(data_for_plotly) = c("Date", unique(settings_2(317)$Tag), unique(settings_2(318)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~KW, color=~(data_for_plotly[,4] == 1), colors=c("red","green")) %>%  
  layout(plot_bgcolor='rgb(254, 247, 234)') %>% 
  layout(paper_bgcolor='rgb(254, 247, 234)')
p









data_for_plotly = as.data.frame(datecomb_318)
colnames(data_for_plotly) = c("Date", unique(settings_2(317)$Tag), unique(settings_2(318)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~KW, color=~(data_for_plotly[,4] == 1), colors=c("red","green")) %>%
  add_markers() %>%
  scale_fill_manual(name = "NES",  labels = c("GOOD", "BAD"),  values = c(FALSE == "red", TRUE == "green")) 






layout(scene = list(title='Date'),
       yaxis = list(title='Loop')) +
  
  
  
  
  
  data_for_plotly = as.data.frame(datecomb_222)
colnames(data_for_plotly) = c("Date", unique(settings_2(317)$Tag), unique(settings_2(222)$Tag), "binary_IBR_KW")
p <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~"WSH_525_TI_411.PV | 2POW LVL FIL PV", color=~(data_for_plotly[,4] == 1), colors=c("red","green")) %>%
  add_markers() %>%
  scale_fill_manual(name = "NES",  labels = c("GOOD", "BAD"),  values = c(FALSE = "green", TRUE = "red"))







library(plotly)
data_for_plotly = as.data.frame(datecomb_318)
colnames(data_for_plotly) = c("Date", unique(settings_2(317)$Tag), unique(settings_2(318)$Tag), "binary_IBR_KW")

axx <- list(
  backgroundcolor="rgb(200, 200, 230",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)

axy <- list(
  backgroundcolor="rgb(230, 200,230)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)

axz <- list(
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)


fig <- plot_ly(data_for_plotly, x=~Date, y=~Loop, z=~KW, color=~(data_for_plotly[,4] == 1), colors=c("red","green"), type = 'mesh3d') 

fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig


























