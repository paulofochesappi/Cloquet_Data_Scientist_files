

rm_main = function() {
  
  
  
  setwd(getwd())
  
  end_time = format(Sys.time(), "%Y%m%d_%H%M%S")
  start_time =    format(as.POSIXct(end_time, format="%Y%m%d_%H%M%S")-(120*24*60*60), "%Y%m%d_%H%M%S") #"20230421_063500" format
  delta = as.numeric(as.Date(end_time, "%Y%m%d_%H%M%S") - as.Date(start_time, "%Y%m%d_%H%M%S"))
  
  # file_path <- "variable_names.RData"
  # 
  # if (file.exists(file_path) == T) {
  #   load("variable_names.Rdata")
  # } else {
  
  # START of correct code ################################################################
  
  
  millpath <- "sappicloquet"
  memoryname <- "VT - Slaker (Batch)"
  time_tag = "Datetime | Start of batch" # "DateTime"
  #"eyJ1IjoiMTQ5ZmFkNmItMjQxYy00NDAwLThkZjItYzFkMDNiZWRjNGQzIiwiayI6IjBkdEshdV1nRXt1cmwwMU4ifQ=="->token
  "eyJ1IjoiODBjNGE4ZjUtYWI1YS00NDFkLTk0NjgtNTA3NDIzNDMwZWE3IiwiayI6IkZJPUc4bS0vbF53cihdSiwifQ=="->token
  
  library(jsonlite)   # make sure to UNLOAD "rjson" library!
  library(lubridate)
  library(httr)
  
  time_name <- time_tag
  token <- token
  millpath <- millpath
  memoryname <- memoryname
  start_time <- start_time
  end_time <- end_time
  data_group_name <- "data_group_name" # "Advanced Analytics - Slaker Causticizing" # "data_group_name"
  
  ######################################
  #Step 1. Generate list of memory bases
  ######################################
  
  result <- GET(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/all/selector"),
                add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
  
  Output<-content(result, as = "text", encoding = "UTF-8")
  json.content<- fromJSON(Output)
  as.data.frame(json.content$items[c("name","quickStudy","bcId","uuid")])->df_memory
  i<-which(df_memory$name==memoryname)
  memory_uuid<-df_memory$uuid[i]
  memory_id<-df_memory$bcId[i]
  
  print("The list of memory bases has been loaded")
  
  
  #####################################   STEP 2  ###############################################################
  # a) list of numerical parameters
  #######################################
  request_body_json <- jsonlite::toJSON(list(
    dataTypes = list("NUMERICAL")
  ), auto_unbox = TRUE)
  
  result <- POST(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/",memory_uuid,"/variables/selector"),
                 body = request_body_json,
                 add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
  
  Output<-content(result, as = "text", encoding = "UTF-8")
  json.content<- fromJSON(Output)
  as.data.frame(json.content$items)->df_numerical
  print("Numerical parameters have been loaded")
  
  #######################################
  # b) list of categorical parameters
  #######################################
  
  request_body_json <- jsonlite::toJSON(list(
    dataTypes = list("DISCRETE")
  ), auto_unbox = TRUE)
  
  result <- POST(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/",memory_uuid,"/variables/selector"),
                 body = request_body_json,
                 add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
  
  Output<-content(result, as = "text", encoding = "UTF-8")
  json.content<- fromJSON(Output)
  as.data.frame(json.content$items)->df_categorical
  
  print("Categorical parameters have been loaded")
  
  #######################################
  # c) list of datetime parameters
  #######################################
  
  request_body_json <- jsonlite::toJSON(list(
    dataTypes = list("DATETIME")
  ), auto_unbox = TRUE)
  
  result <- POST(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/",memory_uuid,"/variables/selector"),
                 body = request_body_json,
                 add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
  
  Output<-content(result, as = "text", encoding = "UTF-8")
  json.content<- fromJSON(Output)
  as.data.frame(json.content$items)->df_datetime
  print("Date/time parameters have been loaded")
  
  time_id <- df_datetime$bcId[which(df_datetime$standard==time_name)]   # or df_datetime$id    # sometimes Tag or Local could be used
  
  timestamp_var<-paste0("mb",memory_id,"/d",time_id)
  
  #################################   STEP 3  ###############################################################
  ##Generate list of data group
  #################################
  
  if(length(data_group_name)>2)
  {
    
    result <- GET(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/",memory_uuid,"/dataGroups/selector"),
                  add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
    
    Output<-content(result, as = "text", encoding = "UTF-8")
    json.content<- fromJSON(Output)
    as.data.frame(json.content$items[c("name","bcId","uuid")])->df_dataGroups
    
    i<-which(df_dataGroups$name==data_group_name)
    dataGroup_uuid<-df_dataGroups$uuid[i]
    
    print("Datagroup list have been loaded")
    
    #################################
    ##Generate list of data group variables
    #################################
    result <- GET(paste0("https://api.mybraincube.com/braincube/",millpath,"/braincube/mb/",memory_uuid,"/dataGroups/",dataGroup_uuid,"/extended"),
                  add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
    
    Output<-content(result, as = "text", encoding = "UTF-8")
    json.content<- fromJSON(Output)
    as.data.frame(json.content$variables)->df_data_group_variables
    if(nrow(df_numerical>=1))
    {
      df_numerical[which(df_numerical$bcId %in% df_data_group_variables$bcId),]->df_numerical
    }
    if(nrow(df_categorical>=1))
    {
      df_categorical[which(df_categorical$bcId %in% df_data_group_variables$bcId),]->df_categorical
    }
  }
  
  if(nrow(df_numerical)>=1)
  {
    if(nrow(df_categorical)>=1)
    {
      rbind(df_categorical,df_numerical)->df_variables
    }else
    {
      df_numerical->df_variables
    }
  }else
  {
    if(nrow(df_categorical)>=1)
    {
      df_categorical->df_variables
    }
  }
  
  
  #  save(df_categorical, df_datetime, df_memory, df_numerical, df_variables, json.content, timestamp_var, memory_id, millpath, token, file = "variable_names.RData")
  
  
  # }
  
  
  
  
  
  #################################   STEP 4  ###############################################################
  ##Load data
  #################################
  
  y <- list(BETWEEN = c(timestamp_var,start_time,end_time))
  
  df_variables[order(df_variables$id),]->df_variables
  df_variables$bcId <- paste0("mb",memory_id,"/d",df_variables$id)
  
  
  request_body_json <- jsonlite::toJSON(list(order = timestamp_var,
                                             definitions = as.list(unique(append(timestamp_var,df_variables$bcId))),
                                             context=list(dataSource = paste0("mb",memory_id),
                                                          filter = y
                                             )), auto_unbox = TRUE)
  
  print(length(request_body_json))
  
  result <- POST(paste0("https://api.mybraincube.com/braincube/",millpath,"/braindata/mb",memory_id,"/LF"),
                 body = request_body_json,
                 add_headers(.headers = c("x-api-key"=token,"Content-Type"="application/json","Accept"="application/json")))
  
  Output<-content(result, as = "text", encoding = "UTF-8")
  json.content<- fromJSON(Output)
  if(json.content$resultLength>0)
  {
    as.data.frame(json.content$datadefs$data)->df
    colnames(df)[1]<-"DateTime"
    if(nrow(df_numerical)>=1)
    {
      colnames(df)[2:(1+nrow(df_numerical))]<-df_numerical$bcId
      cols.num <- names(df[,2:(1+nrow(df_numerical))])
      df[,cols.num] <- sapply(df[cols.num],as.numeric)
    }
    if(nrow(df_categorical)>=1)
    {
      colnames(df)[(2+nrow(df_numerical)):(1+nrow(df_numerical)+nrow(df_categorical))]<-df_categorical$bcId
    }
  }else
  {
    df<-data.frame(Info = "No data")
  }
  # return(list(df,df_numerical))
  #df = as.data.frame(df)
  #names(df) = t(df_variables$local)[1,]
  df_2 = df[,2:ncol(df)]
  names(df_2) = t(df_variables$local)[1,]
  data_set = cbind(df[1], df_2)
  data_group = data_set[c('METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC094.PV | GL->SLAKER', '565MK1104.PV | SLAKER PUMP',
                          '565SIC1127.PV | PURCHASED LIME', '565SIC1128.PV | REBURN LIME', '565TIC103.PV | SLAKER BOWL',
                          'GL Temp at #2 || 565TIC096C.PV','565HY103H.PV | PURCH/REBURN', 
                          'Lime to GL Ratio Controller | 565FF1128.PV', 'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV', 
                          'METRA CGL Sulfidity S% | _565_CGL_S.PV', 'METRA CGL TTA | _565_CGL_TTA.PV', 
                          'METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV', 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request',
                          '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  
  # END of correct code ################################################################
  
  
  
  
  
  
  ### data_group = head(data_group, -24)   # Going back to 12 hours ago
  ### data_set = head(data_set, -24)   # Going back to 12 hours ago
  
  
  #write.csv(data_group,"C:\\Users\\PCOfoche\\Desktop\\my_appR9\\pulled_data.csv")
  
  
  library(quarto)
  #library(RKEEL)
  library(readxl)
  library(arules)
  library(corrplot)
  library(arules)
  library(arulesViz)
  library(arulesCBA)
  library(readxl)
  library(qcc)
  library(dplyr)
  
  
  ## REPEAT ################
  
  # iris = na.omit(sapply(data_group[,!names(data_group) %in% c("565MK1104.PV | SLAKER PUMP", 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request')], as.numeric))
  # 
  # 
  # irisDisc1 <- (discretize(iris[,1], method = "frequency", breaks = 10,
  #                          labels = c("SDT_RGL_AA_1", "SDT_RGL_AA_2","SDT_RGL_AA_3","SDT_RGL_AA_4","SDT_RGL_AA_5",
  #                                     "SDT_RGL_AA_6","SDT_RGL_AA_7","SDT_RGL_AA_8","SDT_RGL_AA_9","SDT_RGL_AA_10")))
  # irisDisc2 <- (discretize(iris[,2], method = "frequency", breaks = 10,
  #                          labels = c("GL->SLAKER_1", "GL->SLAKER_2","GL->SLAKER_3","GL->SLAKER_4","GL->SLAKER_5",
  #                                     "GL->SLAKER_6","GL->SLAKER_7","GL->SLAKER_8","GL->SLAKER_9","GL->SLAKER_10")))
  # irisDisc3 <- (discretize(iris[,3], method = "frequency", breaks = 10,
  #                          labels = c("PURCHASED_LIME_1", "PURCHASED_LIME_2","PURCHASED_LIME_3","PURCHASED_LIME_4","PURCHASED_LIME_5",
  #                                     "PURCHASED_LIME_6","PURCHASED_LIME_7","PURCHASED_LIME_8","PURCHASED_LIME_9","PURCHASED_LIME_10")))
  # irisDisc4 <- (discretize(iris[,4], method = "frequency", breaks = 10,
  #                          labels = c("REBURN_LIME_1", "REBURN_LIME_2","REBURN_LIME_3","REBURN_LIME_4","REBURN_LIME_5",
  #                                     "REBURN_LIME_6","REBURN_LIME_7","REBURN_LIME_8","REBURN_LIME_9","REBURN_LIME_10")))
  # irisDisc5 <- (discretize(iris[,5], method = "frequency", breaks = 10,
  #                          labels = c("SLAKER_BOWL_1", "SLAKER_BOWL_2","SLAKER_BOWL_3","SLAKER_BOWL_4","SLAKER_BOWL_5",
  #                                     "SLAKER_BOWL_6","SLAKER_BOWL_7","SLAKER_BOWL_8","SLAKER_BOWL_9","SLAKER_BOWL_10")))
  # irisDisc6 <- (discretize(iris[,6], method = "frequency", breaks = 10,
  #                          labels = c("GL_TEMP#2_1", "GL_TEMP#2_2","GL_TEMP#2_3","GL_TEMP#2_4","GL_TEMP#2_5",
  #                                     "GL_TEMP#2_6","GL_TEMP#2_7","GL_TEMP#2_8","GL_TEMP#2_9","GL_TEMP#2_10")))
  # irisDisc7 <- (discretize(iris[,7], method = "frequency", breaks = 10,
  #                          labels = c("GL_LIME_RATIO_1", "GL_LIME_RATIO_2","GL_LIME_RATIO_3","GL_LIME_RATIO_4","GL_LIME_RATIO_5",
  #                                     "GL_LIME_RATIO_6","GL_LIME_RATIO_7","GL_LIME_RATIO_8","GL_LIME_RATIO_9","GL_LIME_RATIO_10")))
  # irisDisc8 <- (discretize(iris[,8], method = "frequency", breaks = 10,
  #                          labels = c("LIME_GL_RATIO_CONTROL_1", "LIME_GL_RATIO_CONTROL_2","LIME_GL_RATIO_CONTROL_3","LIME_GL_RATIO_CONTROL_4","LIME_GL_RATIO_CONTROL_5",
  #                                     "LIME_GL_RATIO_CONTROL_6","LIME_GL_RATIO_CONTROL_7","LIME_GL_RATIO_CONTROL_8","LIME_GL_RATIO_CONTROL_9","LIME_GL_RATIO_CONTROL_10")))
  # irisDisc9 <- (discretize(iris[,9], method = "frequency", breaks = 10,
  #                          labels = c("4C_CAUST_DEG_1", "4C_CAUST_DEG_2","4C_CAUST_DEG_3","4C_CAUST_DEG_4","4C_CAUST_DEG_5",
  #                                     "4C_CAUST_DEG_6","4C_CAUST_DEG_7","4C_CAUST_DEG_8","4C_CAUST_DEG_9","4C_CAUST_DEG_10")))
  # irisDisc10 <- (discretize(iris[,10], method = "frequency", breaks = 10,
  #                           labels = c("SULFIDITY_1", "SULFIDITY_2","SULFIDITY_3","SULFIDITY_4","SULFIDITY_5",
  #                                      "SULFIDITY_6","SULFIDITY_7","SULFIDITY_8","SULFIDITY_9","SULFIDITY_10")))
  # irisDisc11 <- (discretize(iris[,11], method = "frequency", breaks = 10,
  #                           labels = c("CGL_TTA_1", "CGL_TTA_2","CGL_TTA_3","CGL_TTA_4","CGL_TTA_5",
  #                                      "CGL_TTA_6","CGL_TTA_7","CGL_TTA_8","CGL_TTA_9","CGL_TTA_10")))
  # irisDisc12 <- (discretize(iris[,12], method = "frequency", breaks = 10,
  #                           labels = c("Slaker_CAUST_DEG_1", "Slaker_CAUST_DEG_2","Slaker_CAUST_DEG_3","Slaker_CAUST_DEG_4","Slaker_CAUST_DEG_5",
  #                                      "Slaker_CAUST_DEG_6","Slaker_CAUST_DEG_7","Slaker_CAUST_DEG_8","Slaker_CAUST_DEG_9","Slaker_CAUST_DEG_10")))
  # 
  # 
  # 
  # irisDisc <- cbind.data.frame(irisDisc1, irisDisc2, irisDisc3, irisDisc4, irisDisc5, irisDisc6, irisDisc7,
  #                              irisDisc8, irisDisc9, irisDisc10, irisDisc11, irisDisc12)
  # 
  # write.csv(irisDisc, file = "irisDisc.csv")                    
  # dataset = read.transactions('irisDisc.csv', sep = ',', rm.duplicates = TRUE)
  # summary(dataset)
  # ##########################################################################
  # # itemFrequencyPlot(dataset, topN = 10)    # Or n number of observations
  # # itemFrequencyPlot(dataset,topN=15,type="absolute")
  # freq_items<-eclat(dataset, parameter=list(supp=0.05, maxlen=10))
  # inspect(freq_items)
  # 
  # rules<-apriori(dataset, parameter=list(supp=0.01, conf=0.5)) 
  # rules2<-apriori(dataset, parameter=list(supp=0.01, conf=0.6)) 
  # rules3<-apriori(dataset, parameter=list(supp=0.05, conf=0.5))
  # 
  # rules.y<-apriori(data=dataset, parameter=list(supp=0.003,conf = 0.1),
  #                  appearance=list(default="lhs",rhs=c("4C_CAUST_DEG_4","4C_CAUST_DEG_5","4C_CAUST_DEG_6","4C_CAUST_DEG_7")), control=list(verbose=F))
  # 
  # # rules.y<-apriori(data=dataset, parameter=list(supp=0.001,conf = 0.08), 
  # #                  appearance=list(lhs="x1_1",rhs=c("y_1", "y_2","y_3","y_4","y_5","y_6","y_7","y_8","y_9","y_10")), control=list(verbose=F))
  # 
  # rules.y.byconf<-sort(rules.y, by="confidence", decreasing=TRUE)
  # inspect(head(rules.y.byconf,50))
  # 
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), conviction = interestMeasure(rules.y.byconf, measure = "conviction",
  #                                                                                        transactions = dataset))
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), hyperConfidence = interestMeasure(rules.y.byconf, measure = "hyperConfidence",
  #                                                                                             transactions = dataset))
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), jaccard = interestMeasure(rules.y.byconf, measure = "jaccard",
  #                                                                                     transactions = dataset))
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), hyperLift = interestMeasure(rules.y.byconf, measure = "hyperLift",
  #                                                                                       transactions = dataset))
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), oddsRatio = interestMeasure(rules.y.byconf, measure = "oddsRatio",
  #                                                                                       transactions = dataset))
  # 
  # quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), leverage = interestMeasure(rules.y.byconf, measure = "leverage",
  #                                                                                      transactions = dataset))
  # 
  # 
  # 
  # rules <- rules.y.byconf
  # inspect(head(rules,50))
  # inspect(head(sort(rules, by="hyperLift", decreasing=TRUE),20))
  # 
  # 
  # # Removing redundant rules OR more correctly "Removing subset rules"
  # # subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
  # # length(subsetRules)  #> 3913
  # # rules <- rules[-subsetRules] # remove subset rules.
  # 
  # # subset2 = rules[is.redundant(rules)]
  # rules = rules[is.redundant(rules)]
  # 
  # 
  # 
  # 
  # ranges_irisDisc1 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][1], attributes(irisDisc1)$levels, attributes(irisDisc1)$`discretized:breaks`,
  #                                        attributes(irisDisc1)$`discretized:breaks`[2:length(attributes(irisDisc1)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc1)[3] <- "Minimum Limit"; colnames(ranges_irisDisc1)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc2 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][2], attributes(irisDisc2)$levels, attributes(irisDisc2)$`discretized:breaks`,
  #                                        attributes(irisDisc2)$`discretized:breaks`[2:length(attributes(irisDisc2)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc2)[3] <- "Minimum Limit"; colnames(ranges_irisDisc2)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc3 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][3], attributes(irisDisc3)$levels, attributes(irisDisc3)$`discretized:breaks`,
  #                                        attributes(irisDisc3)$`discretized:breaks`[2:length(attributes(irisDisc3)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc3)[3] <- "Minimum Limit"; colnames(ranges_irisDisc3)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc4 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][4], attributes(irisDisc4)$levels, attributes(irisDisc4)$`discretized:breaks`,
  #                                        attributes(irisDisc4)$`discretized:breaks`[2:length(attributes(irisDisc4)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc4)[3] <- "Minimum Limit"; colnames(ranges_irisDisc4)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc5 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][5], attributes(irisDisc5)$levels, attributes(irisDisc5)$`discretized:breaks`,
  #                                        attributes(irisDisc5)$`discretized:breaks`[2:length(attributes(irisDisc5)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc5)[3] <- "Minimum Limit"; colnames(ranges_irisDisc5)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc6 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][6], attributes(irisDisc6)$levels, attributes(irisDisc6)$`discretized:breaks`,
  #                                        attributes(irisDisc6)$`discretized:breaks`[2:length(attributes(irisDisc6)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc6)[3] <- "Minimum Limit"; colnames(ranges_irisDisc6)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc7 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][7], attributes(irisDisc7)$levels, attributes(irisDisc7)$`discretized:breaks`,
  #                                        attributes(irisDisc7)$`discretized:breaks`[2:length(attributes(irisDisc7)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc7)[3] <- "Minimum Limit"; colnames(ranges_irisDisc7)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc8 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][8], attributes(irisDisc8)$levels, attributes(irisDisc8)$`discretized:breaks`,
  #                                        attributes(irisDisc8)$`discretized:breaks`[2:length(attributes(irisDisc8)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc8)[3] <- "Minimum Limit"; colnames(ranges_irisDisc8)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc9 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][9], attributes(irisDisc9)$levels, attributes(irisDisc9)$`discretized:breaks`,
  #                                        attributes(irisDisc9)$`discretized:breaks`[2:length(attributes(irisDisc9)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc9)[3] <- "Minimum Limit"; colnames(ranges_irisDisc9)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc10 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][10], attributes(irisDisc10)$levels, attributes(irisDisc10)$`discretized:breaks`,
  #                                         attributes(irisDisc10)$`discretized:breaks`[2:length(attributes(irisDisc10)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc10)[3] <- "Minimum Limit"; colnames(ranges_irisDisc10)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc11 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][11], attributes(irisDisc11)$levels, attributes(irisDisc11)$`discretized:breaks`,
  #                                         attributes(irisDisc11)$`discretized:breaks`[2:length(attributes(irisDisc11)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc11)[3] <- "Minimum Limit"; colnames(ranges_irisDisc11)[4] <- "Maximum Limit"
  # 
  # ranges_irisDisc12 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][12], attributes(irisDisc12)$levels, attributes(irisDisc12)$`discretized:breaks`,
  #                                         attributes(irisDisc12)$`discretized:breaks`[2:length(attributes(irisDisc12)$`discretized:breaks`)])[1:10,])
  # colnames(ranges_irisDisc12)[3] <- "Minimum Limit"; colnames(ranges_irisDisc12)[4] <- "Maximum Limit"
  # 
  # 
  # 
  # ranges = rbind(ranges_irisDisc9, ranges_irisDisc1, ranges_irisDisc2, ranges_irisDisc3, ranges_irisDisc4, ranges_irisDisc5,
  #                ranges_irisDisc6, ranges_irisDisc7, ranges_irisDisc8, ranges_irisDisc10, ranges_irisDisc11, ranges_irisDisc12)
  # 
  # colnames(ranges)[1] <- "Attributes"; colnames(ranges)[2] <- "Level Codes"
  # ranges
  # 
  # 
  
  
  
  # library(ggplot2)
  # Plot_10 <- plot(sort(rules, by="hyperLift", decreasing=TRUE)[1:10], method="graph", shading = "hyperLift",   control = list(
  #   edges = ggraph::geom_edge_link(
  #     end_cap = ggraph::circle(8, "mm"),
  #     start_cap = ggraph::circle(8, "mm"),
  #     color = "cyan",
  #     arrow = arrow(length = unit(4, "mm"), angle = 30, type = "closed"),
  #     alpha = .5
  #   ),
  #   nodes = ggraph::geom_node_point(aes_string(size = "support", color = "hyperLift")),
  #   nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
  # ),
  # limit = 10
  # ) + 
  #   scale_color_gradient(low = "yellow", high = "red") + 
  #   scale_size(range = c(2, 10)) + theme_dark()
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(corrplot)
  library(ggcorrplot)
  corr_data_group <<- cor((na.omit(sapply(data_group[,!names(data_group) %in% c("565MK1104.PV | SLAKER PUMP", 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request')], as.numeric))),method="s")
  corr_data_group <<- corr_data_group[, c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14)]
  colnames(corr_data_group) = c("Metra 4C Causticizing Degree %","GL->SLAKER","PURCHASED LIME","REBURN LIME","SLAKER BOWL","GL Temp at #2","PURCH/REBURN",
                                "Lime to GL Ratio Controller","CGL Sulfidity S%","CGL TTA","SlakerCausticizing Degree",
                                'SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')
  corr_data_group = t(corr_data_group)
  corr_data_group <<- corr_data_group[, c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14)]
  colnames(corr_data_group) = c("Metra 4C Causticizing Degree %","GL->SLAKER","PURCHASED LIME","REBURN LIME","SLAKER BOWL","GL Temp at #2","PURCH/REBURN",
                                "Lime to GL Ratio Controller","CGL Sulfidity S%","CGL TTA","SlakerCausticizing Degree",
                                'SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')
  par(bg = 'darkgoldenrod3')
  plot.new(); dev.control("enable");
  corrplot(corr_data_group,method = "pie", type="upper", tl.col="white", tl.cex=1.2, addgrid.col="black"); 
  Plot_66 <- recordPlot(); plot.new(); dev.off() ## clean up device
  par(bg = 'white') # restore background color to original white
  # using hierarchical clustering and reordering the correlation matrix
  #Plot_6 <- ggcorrplot(corr_data_group, hc.order = TRUE, outline.col = "black", colors = c("red","white","blue"), tl.srt = 35, method = c("square"))
  #Plot_6 <- recordPlot(); plot.new() ## clean up device
  #par(bg = 'white') # restore background color to original white
  
  
  #Splitting train and test data set
  split_date = as.Date(end_time, "%Y%m%d_%H%M%S") - delta*0.3
  train_set <- data_set[as.Date(data_set$DateTime, "%Y%m%d_%H%M%S") < split_date,]
  test_set <- data_set[-(1:nrow(train_set)),]
  
  
  train = train_set[c('DateTime', 'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC094.PV | GL->SLAKER',
                      '565SIC1127.PV | PURCHASED LIME', '565SIC1128.PV | REBURN LIME', '565TIC103.PV | SLAKER BOWL',
                      'GL Temp at #2 || 565TIC096C.PV','565HY103H.PV | PURCH/REBURN', 
                      'Lime to GL Ratio Controller | 565FF1128.PV', 'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV', 
                      'METRA CGL Sulfidity S% | _565_CGL_S.PV', 'METRA CGL TTA | _565_CGL_TTA.PV', 
                      'METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV', 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request',
                      '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  train[,1] = as.POSIXct(train[,1], format="%Y%m%d_%H%M%S")
  train[,-1] = data.frame(lapply(train[,-1], function(x) as.numeric(as.character(x))),   # new
                          check.names=F, row.names = rownames(train[,-1]))   #  new
  train[is.na(train)] = 0   # new
  #train = na.omit(train)
  
  test = test_set[c('DateTime', 'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC094.PV | GL->SLAKER',
                    '565SIC1127.PV | PURCHASED LIME', '565SIC1128.PV | REBURN LIME', '565TIC103.PV | SLAKER BOWL',
                    'GL Temp at #2 || 565TIC096C.PV','565HY103H.PV | PURCH/REBURN', 
                    'Lime to GL Ratio Controller | 565FF1128.PV', 'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV', 
                    'METRA CGL Sulfidity S% | _565_CGL_S.PV', 'METRA CGL TTA | _565_CGL_TTA.PV', 
                    'METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV', 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request',
                    '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  test[,1] = as.POSIXct(test[,1], format="%Y%m%d_%H%M%S")
  test[,-1] = data.frame(lapply(test[,-1], function(x) as.numeric(as.character(x))),   # new
                         check.names=F, row.names = rownames(test[,-1]))   # new
  test[is.na(test)] = 0   # new
  #test = na.omit(test)
  
  #Transform train and test data to DMatrix form
  library(dplyr)
  library(xgboost)
  train_matrix <- sapply(na.omit(train[,c(-1,-10)]), as.numeric)
  
  pred_matrix <- sapply(na.omit(test[,c(-1,-10)]), as.numeric)
  targets <- train$'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV'
  targets_matrix <- na.omit(sapply(targets, as.numeric))
  
  
  #Cross-validation
  library(caret)
  xgb_trcontrol <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    verboseIter = FALSE,
    returnData = FALSE
  )
  #Building parameters set
  xgb_grid <- base::expand.grid(
    list(
      nrounds = seq(100,200,100),
      max_depth = c(6,15),
      colsample_bytree = 1,
      eta = 0.5,
      gamma = 0,
      min_child_weight = 1,
      subsample = 1)
  )
  
  
  #Building the model
  model_xgb <- caret::train(
    train_matrix,targets_matrix,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = "xgbTree",
    nthread = 10,
    verbosity = 0
  )
  
  model_xgb$bestTune
  
  
  #Making the variables used in forecast object
  fitted <- model_xgb %>%
    stats::predict(train_matrix)# %>%
  
  
  ts_METRA_4C_Causticizing_Degree <- train['DateTime']
  forecast_xgb <- model_xgb %>% stats::predict(pred_matrix)
  
  forecast_ts <- cbind(forecast_xgb,test['DateTime'])
  colnames(forecast_ts) <- c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", "DateTime")
  
  #Preparing forecast object
  forecast_METRA_4C_Causticizing_Degree <- list(
    model = model_xgb$modelInfo,
    method = model_xgb$method,
    mean = forecast_ts,
    x = ts_METRA_4C_Causticizing_Degree,
    fitted = fitted,
    residuals = as.numeric(unlist(ts_METRA_4C_Causticizing_Degree)) - as.numeric(fitted)
  )
  
  
  
  
  #The function to convert decimal time label to wanted format
  date_transform <- function(x) {format(date_decimal(x), "%H")}
  
  #Making a time series varibale for observed data
  observed_values <-  as.data.frame(cbind(test$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`,test['DateTime']))
  colnames(observed_values) <- c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", "DateTime")
  
  #Plot forecasting
  library(ggplot2)
  library(forecast)
  ###################################################################################################################
  # colors <- c("Train" = "blue", "Observed" = "red", "Predicted" = "violet")
  # ggplot() +
  #   geom_line(data = train, aes(x=DateTime, y=`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Train")) +
  #   geom_line(data = observed_values, aes(DateTime, `METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Observed")) +
  #   geom_line(data = forecast_ts, aes(DateTime, `METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Predicted")) + labs(color="Legend") +
  #   scale_color_manual(values = colors)
  
  
  #Feature importance
  library(Ckmeans.1d.dp)
  xgb_imp <- xgb.importance(
    feature_names = colnames(train_matrix),
    model = model_xgb$finalModel)
  # Plot_2 = xgb.ggplot.importance(xgb_imp,n_clusters = c(2))+
  #   ggtitle("") +
  #   theme_bw()+
  #   theme(legend.position="none")
  # xgb_imp$Importance
  
  Plot_20 = xgb.ggplot.importance(xgb_imp,n_clusters = c(3)) + scale_fill_manual(values=c("#e5d0ff", "#bf8bff", 'purple')) +
    ggtitle("Priority List") +
    theme_bw()+
    theme(legend.position="none") + theme(panel.background = element_rect(fill = 'black', color = 'purple')) +
    theme(text = element_text(size = 20)) 
  xgb_imp$Importance
  
  
  
  # colors <- c("Train" = "blue", "Observed" = "red", "Predicted" = "violet")
  # Plot_1 = ggplot() +
  #   geom_line(data = train, aes(x=DateTime, y=`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Train")) +
  #   geom_line(data = observed_values, aes(DateTime, `METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Observed")) +
  #   geom_line(data = forecast_ts, aes(DateTime, `METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, colour = "Predicted")) + labs(color="Legend") +
  #   scale_color_manual(values = colors) + theme(panel.background = element_rect(fill = 'peachpuff', color = 'purple'),
  #                                               panel.grid.major = element_line(color = 'brown', linetype = 'dotted'),
  #                                               panel.grid.minor = element_line(color = 'lightblue', size = 0.5))
  
  
  
  
  
  
  
  
  ########################## VAR #####################################
  
  library(rsconnect)
  library(urca)
  library(vars) 
  library(tsDyn)
  nhor <- 24  # 12 hours of forecast
  df.lev <- data_group[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                          "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
                          "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                          "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                          "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                          'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  df.lev = data.frame(lapply(df.lev, function(x) as.numeric(as.character(x))),  # new
                      check.names=F, row.names = rownames(df.lev))  # new
  df.lev[is.na(df.lev)] = 0 # new
  m.lev  <- as.matrix(df.lev)
  nr_lev <- nrow(df.lev)
  
  ############ 
  # Draw Graph
  # str.main <- c(
  #   "GL Temp at #2 || 565TIC096C.PV","I565LCP103_F3.PV | SLAKER  GL TO LIME RATIO",
  #   "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
  #   "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
  #   "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
  #   "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER")
  # 
  # #x11(width=12, height = 6); 
  # #par(mfrow=c(2,2), mar=c(5,3,3,3))
  # x11(width=12, height = 12);
  # par(mfrow=c(2,6), mar=c(10,2,5,2))
  # for(i in 1:11) {
  #   matplot(m.lev[,i], axes=FALSE,
  #           type=c("l"), col = c("blue"), 
  #           main = str.main[i])
  #   
  #   axis(2) # show y axis
  #   
  #   # show x axis and replace it with 
  #   # an user defined sting vector
  #   axis(1, at=seq_along(1:nrow(df.lev)),
  #        labels=data_set$DateTime, las=2)
  # }
  
  
  
  ############ 
  # layout(matrix(seq(1,11), 3, 3, byrow = TRUE))
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,1])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,2])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,3])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,4])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,5])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,6])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,7])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,8])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,9])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,10])), type = "l")
  # plot(seq(1,length(df$DateTime)), as.numeric((df.lev[,11])), type = "l")
  
  
  
  
  #========================================================
  # VAR model in level
  #========================================================
  
  # lag length
  result = VARselect(sapply(na.omit(df.lev), as.numeric), lag.max = 20,
                     type = "const", season = NULL)
  
  # estimation
  var.model_lev <- VAR(sapply(na.omit(df.lev), as.numeric), p = result$selection['AIC(n)'], 
                       type = "const", season = NULL)
  
  # forecast of lev data
  var.pred <- predict(var.model_lev, n.ahead = nhor)
  #x11(); par(mai=rep(0.4, 4), bg = 'peachpuff'); plot(var.pred);  
  ###par(mar = c(2, 2, 2, 2), bg = 'yellow');
  ###plot.new(); dev.control("enable");
  ###Plot_3 <- recordPlot(); plot.new(); dev.off() ## clean up device
  ###par(bg = 'white') # restore background color to original white
  
  library(bruceR)
  model_summary(var.model_lev)
  causality_test = granger_causality(var.model_lev)
  Granger_Causality_Ranking = causality_test$result[c(which(causality_test$result$Equation=="METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV")),][c(order(causality_test$result[c(which(causality_test$result$Equation=="METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV")),]$p.F, decreasing=FALSE)),]
  Granger_Causality_Ranking = cbind(Granger_Causality_Ranking$Excluded, Granger_Causality_Ranking$sig.F)
  colnames(Granger_Causality_Ranking) = c("Time Series", "Granger causality test significance")
  Granger_Causality_Ranking = as.data.frame(Granger_Causality_Ranking)
  
  
  plot.new(); dev.control("enable");
  #layout(matrix(c(1,2,3,4,5,6,7,7,8,9,10,11), nrow = 6, ncol = 2, byrow = TRUE))
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,13,14,14), nrow = 5, ncol = 3, byrow = TRUE))
  par(mar = c(2, 3, 2, 2), bg = 'black');
  y=c(tail(as.numeric(df.lev$`GL Temp at #2 || 565TIC096C.PV`), n=96), var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),  
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`GL Temp at #2 || 565TIC096C.PV`)],
       y=c(as.numeric(df.lev$`GL Temp at #2 || 565TIC096C.PV`), var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])[1:length(df.lev$`GL Temp at #2 || 565TIC096C.PV`)],
       main="GL Temp at #2 || 565TIC096C.PV",col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.2, tail(sort(y),2)[1]+0.2),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])], 
        var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`), n=96), var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`)],
       y=c(as.numeric(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`), var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])[1:length(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`)],
       main="METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-2, tail(sort(y),2)[1]+2),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])], 
        var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`565HY103H.PV | PURCH/REBURN`), n=96), var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565HY103H.PV | PURCH/REBURN`)],
       y=c(as.numeric(df.lev$`565HY103H.PV | PURCH/REBURN`), var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])[1:length(df.lev$`565HY103H.PV | PURCH/REBURN`)],
       main="565HY103H.PV | PURCH/REBURN", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.5, tail(sort(y),2)[1]+0.5),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])], 
        var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`565TIC103.PV | SLAKER BOWL`), n=96), var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565TIC103.PV | SLAKER BOWL`)],
       y=c(as.numeric(df.lev$`565TIC103.PV | SLAKER BOWL`), var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])[1:length(df.lev$`565TIC103.PV | SLAKER BOWL`)],
       main="565TIC103.PV | SLAKER BOWL", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-1, tail(sort(y),2)[1]+1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])], 
        var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`), n=96), var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`)],
       y=c(as.numeric(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`), var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])[1:length(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`)],
       main="Lime to GL Ratio Controller | 565FF1128.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=(c(head(sort(y),2)[2]-0.01, tail(sort(y),2)[1]+0.01)),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])], 
        var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`565SIC1128.PV | REBURN LIME`), n=96), var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565SIC1128.PV | REBURN LIME`)],
       y=c(as.numeric(df.lev$`565SIC1128.PV | REBURN LIME`), var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])[1:length(df.lev$`565SIC1128.PV | REBURN LIME`)],
       main="565SIC1128.PV | REBURN LIME", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.1, tail(sort(y),2)[1]+0.1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])], 
        var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  y=c(tail(as.numeric(df.lev$`565SIC1127.PV | PURCHASED LIME`), n=96), var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565SIC1127.PV | PURCHASED LIME`)],
       y=c(as.numeric(df.lev$`565SIC1127.PV | PURCHASED LIME`), var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])[1:length(df.lev$`565SIC1127.PV | PURCHASED LIME`)],
       main="565SIC1127.PV | PURCHASED LIME", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.05, tail(sort(y),2)[1]+0.05),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])], 
        var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`), n=96), var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`)],
       y=c(as.numeric(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`), var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])[1:length(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`)],
       main="METRA CGL Sulfidity S% | _565_CGL_S.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.05, tail(sort(y),2)[1]+0.05),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])], 
        var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`565FIC094.PV | GL->SLAKER`), n=96), var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565FIC094.PV | GL->SLAKER`)],
       y=c(as.numeric(df.lev$`565FIC094.PV | GL->SLAKER`), var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])[1:length(df.lev$`565FIC094.PV | GL->SLAKER`)],
       main="565FIC094.PV | GL->SLAKER", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-5, tail(sort(y),2)[1]+5),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])], 
        var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  y=c(tail(as.numeric(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`), n=96), var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`)],
       y=c(as.numeric(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`), var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])[1:length(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`)],
       main="METRA CGL TTA | _565_CGL_TTA.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  # xaxp = c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), 
  #         tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1), 60))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])], 
        var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  
  
  
  
  
  y=c(tail(as.numeric(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`), n=96), var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`)],
       y=c(as.numeric(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`), var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])[1:length(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`)],
       main="METRA SDT RGL AA | _561_RGL_AA.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  # xaxp = c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), 
  #         tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1), 60))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1])], 
        var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  
  
  
  
  
  
  
  
  
  
  y=c(tail(as.numeric(df.lev$`565FIC097.PV | GL->CLASS`), n=96), var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565FIC097.PV | GL->CLASS`)],
       y=c(as.numeric(df.lev$`565FIC097.PV | GL->CLASS`), var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])[1:length(df.lev$`565FIC097.PV | GL->CLASS`)],
       main="565FIC097.PV | GL->CLASS", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  # xaxp = c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), 
  #         tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1), 60))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])], 
        var.pred$fcst$X565FIC097.PV...GL..CLASS[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  y=c(tail(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`), n=96), var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`)],
       y=c(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`), var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])[1:length(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`)],
       main="METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),2)[2]-0.1, tail(sort(y),2)[1]+0.1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])], 
        var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  
  
  
  y=c(tail(as.numeric(df.lev$`565LI061.PV | WL TANK`), n=96), var.pred$fcst$X565LI061.PV...WL.TANK[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565LI061.PV | WL TANK`)],
       y=c(as.numeric(df.lev$`565LI061.PV | WL TANK`), var.pred$fcst$X565LI061.PV...WL.TANK[,1])[1:length(df.lev$`565LI061.PV | WL TANK`)],
       main="565LI061.PV | WL TANK", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=1.5,cex.main=2,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  # xaxp = c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), 
  #         tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1), 60))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565LI061.PV...WL.TANK[,1])], 
        var.pred$fcst$X565LI061.PV...WL.TANK[,1], col = "blue", lwd=4)
  #axis(1,col="yellow");
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  #abline(v = as_datetime(as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit")), col="green", lwd=1);
  #abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S", tz = "America/Detroit"), col="green", lwd=1);
  
  
  
  
  
  
  
  
  
  
  
  
  Plot_21 <- recordPlot(); plot.new(); dev.off() ## clean up device
  par(bg = 'white') # restore background color to original white
  
  
  ############ x11(); par(mai=rep(0.4, 4)); fanchart(var.pred)
  ############ var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV
  # data_set$DateTime[nrow(data_set)]
  # data.frame(datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))
  
  ts = c(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
         seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=nhor, by = 30*60)[1:
                                                                                                                                                        length(var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  
  projected = c(tail(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  par(bg = 'darkseagreen1')
  
  combined = cbind(as.data.frame(ts),projected)
  
  colours <- c("FALSE" = "white", "TRUE" = "dodgerblue")
  
  library(scales); library(ggtext)
  Plot_23 <- ggplot(data=combined[,1:2],aes(ts, projected, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) + geom_line(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("white","dodgerblue")) + geom_point(shape = 1, size = 4) + 
    geom_line(linewidth=1.5) + geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + 
    geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + geom_hline(yintercept = c(80.5,82.5), linetype="dashed", color="green", linewidth=2) +
    ggtitle("Past 3 days plus 12-hour Forecast") + ylab("Metra 4C Causticizing Degree %") + xlab("Date") + ylim(mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.6, mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.6) +
    #geom_text(aes(ts, projected, label = paste(ts[-1]," *")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') + # qcc(t(projected[,2]),ylim=c(78,83),nsigmas=3)
    #geom_text(aes(ts, projected, label = paste("OUT OF CONTROL!")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOC!</b>")), nudge_y=-0.3, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) | abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,2)), ],angle=-45,size=5, color = 'orange') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=+0.5, nudge_x=+0.5, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(rep(82.5,length(ts))) & abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=-0.1, nudge_x=+30*60, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) < mean(rep(80.5,length(ts))) & abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
    theme(panel.background = element_rect(fill = 'black', color = 'purple'),
          panel.grid.major = element_line(color = 'lightblue'),
          panel.grid.minor = element_line(color = 'lightblue', linetype = 'dotted', size = 0.5)) +
    theme(text = element_text(size = 20), axis.text.x=element_text(angle=90, vjust = 0.3, hjust = 1)) +
    annotate("text", label = "Upper Spec Limit", size=6, x=ts[5],y=82.5+0.6, color="green") + annotate("text", label = "Lower Spec Limit", size=6, x=ts[5],y=80.5-0.6, color="green") +
    annotate("text", label = "UCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.55, color="red") + annotate("text", label = "LCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.55, color="red") +
    scale_x_datetime(breaks = breaks_width("3 hours"), minor_breaks = date_breaks("1 hour"), date_labels = "%b %d     %H:%M") 
  
  
  
  
  
  
  
  # plot_predicted_and_actual = ggplot() +
  #   geom_line(data = combined[,1:2], aes(ts, projected, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
  #   geom_point(data = combined[,1:2], aes(ts, projected, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
  #    theme(legend.position = "none") + scale_color_manual(values=c("white","blue3")) + geom_point(shape = 1, size = 4) +
  #   geom_line(linewidth=1.5) + geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + 
  #   geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + geom_hline(yintercept = c(80.5,82.5), linetype="dashed", color="green", linewidth=2) +
  #   ggtitle("Past 3 days plus 12-hour Forecast") + ylab("Metra 4C Causticizing Degree %") + xlab("Date") + ylim(mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.6, mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.6) +
  #   #geom_text(aes(ts, projected, label = paste(ts[-1]," *")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') + # qcc(t(projected[,2]),ylim=c(78,83),nsigmas=3)
  #   #geom_text(aes(ts, projected, label = paste("OUT OF CONTROL!")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOC!</b>")), nudge_y=-0.3, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) | abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,2)), ],angle=-45,size=5, color = 'orange') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=+0.5, nudge_x=+0.5, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(rep(82.5,length(ts))) & abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=-0.1, nudge_x=+30*60, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) < mean(rep(80.5,length(ts))) & abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
  #   theme(panel.background = element_rect(fill = 'black', color = 'purple'),
  #         panel.grid.major = element_line(color = 'lightblue'),
  #         panel.grid.minor = element_line(color = 'lightblue', linetype = 'dotted', size = 0.5)) +
  #   theme(text = element_text(size = 20), axis.text.x=element_text(angle=90, vjust = 0.3, hjust = 1)) +
  #   annotate("text", label = "Upper Spec Limit", size=6, x=ts[5],y=82.5+0.6, color="green") + annotate("text", label = "Lower Spec Limit", size=6, x=ts[5],y=80.5-0.6, color="green") +
  #   annotate("text", label = "UCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.55, color="red") + annotate("text", label = "LCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.55, color="red") +
  #   scale_x_datetime(breaks = breaks_width("3 hours"), minor_breaks = date_breaks("1 hour"), date_labels = "%b %d     %H:%M") +
  #   geom_point(data = total_scoring_combined, aes(x=total_scoring_combined$scoring_ts, y=total_scoring_combined$scoring_projected), colour = "dodgerblue") +
  #   geom_line(data = total_scoring_combined, aes(total_scoring_combined$scoring_ts, total_scoring_combined$scoring_projected), colour = "dodgerblue")
  
  
  
  
  # plot_predicted_and_actual = ggplot(merge(cbind(seq(1,120),combined[,1:2]),   cbind(seq(1,96),total_scoring_combined))) +
  #   geom_line(data = combined[,1:2], aes(x=ts, y=projected, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
  #   geom_point(data = combined[,1:2], aes(x=ts, y=projected, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
  #   theme(legend.position = "none") + scale_color_manual(values=c("white","blue3")) +
  #   geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + 
  #   geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + geom_hline(yintercept = c(80.5,82.5), linetype="dashed", color="green", linewidth=2) +
  #   ggtitle("Past 3 days plus 12-hour Forecast") + ylab("Metra 4C Causticizing Degree %") + xlab("Date") + ylim(mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.6, mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.6) +
  #   #geom_text(aes(ts, projected, label = paste(ts[-1]," *")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') + # qcc(t(projected[,2]),ylim=c(78,83),nsigmas=3)
  #   #geom_text(aes(ts, projected, label = paste("OUT OF CONTROL!")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOC!</b>")), nudge_y=-0.3, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) | abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,2)), ],angle=-45,size=5, color = 'orange') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=+0.5, nudge_x=+0.5, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(rep(82.5,length(ts))) & abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
  #   #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
  #   geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=-0.1, nudge_x=+30*60, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) < mean(rep(80.5,length(ts))) & abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
  #   theme(panel.background = element_rect(fill = 'black', color = 'purple'),
  #         panel.grid.major = element_line(color = 'lightblue'),
  #         panel.grid.minor = element_line(color = 'lightblue', linetype = 'dotted', size = 0.5)) +
  #   theme(text = element_text(size = 20), axis.text.x=element_text(angle=90, vjust = 0.3, hjust = 1)) +
  #   annotate("text", label = "Upper Spec Limit", size=6, x=ts[5],y=82.5+0.6, color="green") + annotate("text", label = "Lower Spec Limit", size=6, x=ts[5],y=80.5-0.6, color="green") +
  #   annotate("text", label = "UCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.55, color="red") + annotate("text", label = "LCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.55, color="red") +
  #   scale_x_datetime(breaks = breaks_width("3 hours"), minor_breaks = date_breaks("1 hour"), date_labels = "%b %d     %H:%M") +
  #   geom_point(data = total_scoring_combined, aes(x=total_scoring_combined$scoring_ts, y=total_scoring_combined$scoring_projected), colour = "dodgerblue") +
  #   geom_line(data = total_scoring_combined, aes(x=total_scoring_combined$scoring_ts, y=total_scoring_combined$scoring_projected), colour = "dodgerblue")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  scoring_data_group = data.frame(lapply(data_group, function(x) as.numeric(as.character(x))),  # new
                                  check.names=F, row.names = rownames(data_group))  # new
  scoring_data_group[is.na(scoring_data_group)] = 0 # new
  
  
  scoring_data_set = data.frame(lapply(data_set[-c(1)], function(x) as.numeric(as.character(x))),  # new
                                check.names=F, row.names = rownames(data_set[-c(1)]))  # new
  scoring_data_set[is.na(scoring_data_set)] = 0 # new
  scoring_data_set = cbind(data_set[c(1)], scoring_data_set)
  
  scoring_data_group = head(scoring_data_group, -24)   # Going back to 12 hours ago   # new
  scoring_data_set = head(scoring_data_set, -24)   # Going back to 12 hours ago       # new
  
  
  
  
  
  
  
  #scoring_data_group = head(data_group, -24)   # Going back to 12 hours ago
  #scoring_data_set = head(data_set, -24)   # Going back to 12 hours ago
  scoring_nhor <- 24  # 12 hours of forecast
  scoring_df.lev <- scoring_data_group[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                                          "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
                                          "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                                          "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                                          "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                                          'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  scoring_df.lev = data.frame(lapply(scoring_df.lev, function(x) as.numeric(as.character(x))),  # new
                              check.names=F, row.names = rownames(scoring_df.lev))  # new
  scoring_df.lev[is.na(scoring_df.lev)] = 0 # new
  scoring_m.lev  <- as.matrix(scoring_df.lev)
  scoring_nr_lev <- nrow(scoring_df.lev)
  #scoring_result = VARselect(sapply(na.omit(scoring_df.lev), as.numeric), lag.max = 20,
  #                           type = "const", season = NULL)
  scoring_result = VARselect(sapply((scoring_df.lev), as.numeric), lag.max = 20,
                             type = "const", season = NULL)
  #scoring_var.model_lev <- VAR(sapply(na.omit(scoring_df.lev), as.numeric), p = result$selection['AIC(n)'], 
  #                             type = "const", season = NULL)
  scoring_var.model_lev <- VAR(sapply((scoring_df.lev), as.numeric), p = result$selection['AIC(n)'], 
                               type = "const", season = NULL)
  scoring_var.pred <- predict(scoring_var.model_lev, n.ahead = scoring_nhor)
  
  scoring_ts = c(tail(as.POSIXct((datetime2=as.POSIXct((scoring_data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
                 seq(from=as.POSIXct((datetime2=as.POSIXct((scoring_data_set$DateTime[nrow(scoring_data_set)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=scoring_nhor, by = 30*60)[1:
                                                                                                                                                                                        length(scoring_var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  
  scoring_projected = c(tail(as.numeric(scoring_df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                        scoring_var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  scoring_combined = cbind(as.data.frame(scoring_ts),scoring_projected);  colnames(scoring_combined) <- c("scoring_ts", "scoring_projected")
  
  plot(head(combined, 96))
  lines(tail(scoring_combined, 96))
  
  
  #return(as.data.frame(scoring_combined))
  
  
  
  
  
  ##################################################################################################################################################################
  
  scoring_data_group_2 = head(scoring_data_group, -24)   # Going back to 12 hours ago   # new
  scoring_data_set_2 = head(scoring_data_set, -24)   # Going back to 12 hours ago       # new
  scoring_nhor_2 <- 24  # 12 hours of forecast
  scoring_df.lev_2 <- scoring_data_group_2[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                                              "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
                                              "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                                              "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                                              "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                                              'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  scoring_df.lev_2 = data.frame(lapply(scoring_df.lev_2, function(x) as.numeric(as.character(x))),  # new
                                check.names=F, row.names = rownames(scoring_df.lev_2))  # new
  scoring_df.lev_2[is.na(scoring_df.lev_2)] = 0 # new
  scoring_m.lev_2  <- as.matrix(scoring_df.lev_2)
  scoring_nr_lev_2 <- nrow(scoring_df.lev_2)
  scoring_result_2 = VARselect(sapply((scoring_df.lev_2), as.numeric), lag.max = 20,
                               type = "const", season = NULL)
  scoring_var.model_lev_2 <- VAR(sapply((scoring_df.lev_2), as.numeric), p = result$selection['AIC(n)'], 
                                 type = "const", season = NULL)
  scoring_var.pred_2 <- predict(scoring_var.model_lev_2, n.ahead = scoring_nhor)
  
  scoring_ts_2 = c(tail(as.POSIXct((datetime2=as.POSIXct((scoring_data_set_2$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
                   seq(from=as.POSIXct((datetime2=as.POSIXct((scoring_data_set_2$DateTime[nrow(scoring_data_set_2)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=scoring_nhor_2, by = 30*60)[1:
                                                                                                                                                                                                length(scoring_var.pred_2$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  scoring_projected_2 = c(tail(as.numeric(scoring_df.lev_2$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                          scoring_var.pred_2$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  scoring_combined_2 = cbind(as.data.frame(scoring_ts_2),scoring_projected_2);   colnames(scoring_combined_2) <- c("scoring_ts", "scoring_projected")
  
  
  
  
  
  
  
  scoring_data_group_3 = head(scoring_data_group_2, -24)   # Going back to 12 hours ago   # new
  scoring_data_set_3 = head(scoring_data_set_2, -24)   # Going back to 12 hours ago       # new
  scoring_nhor_3 <- 24  # 12 hours of forecast
  scoring_df.lev_3 <- scoring_data_group_3[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                                              "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
                                              "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                                              "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                                              "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                                              'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  scoring_df.lev_3 = data.frame(lapply(scoring_df.lev_3, function(x) as.numeric(as.character(x))),  # new
                                check.names=F, row.names = rownames(scoring_df.lev_3))  # new
  scoring_df.lev_3[is.na(scoring_df.lev_3)] = 0 # new
  scoring_m.lev_3  <- as.matrix(scoring_df.lev_3)
  scoring_nr_lev_3 <- nrow(scoring_df.lev_3)
  scoring_result_3 = VARselect(sapply((scoring_df.lev_3), as.numeric), lag.max = 20,
                               type = "const", season = NULL)
  scoring_var.model_lev_3 <- VAR(sapply((scoring_df.lev_3), as.numeric), p = result$selection['AIC(n)'], 
                                 type = "const", season = NULL)
  scoring_var.pred_3 <- predict(scoring_var.model_lev_3, n.ahead = scoring_nhor)
  
  scoring_ts_3 = c(tail(as.POSIXct((datetime2=as.POSIXct((scoring_data_set_3$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
                   seq(from=as.POSIXct((datetime2=as.POSIXct((scoring_data_set_3$DateTime[nrow(scoring_data_set_3)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=scoring_nhor_3, by = 30*60)[1:
                                                                                                                                                                                                length(scoring_var.pred_3$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  scoring_projected_3 = c(tail(as.numeric(scoring_df.lev_3$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                          scoring_var.pred_3$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  scoring_combined_3 = cbind(as.data.frame(scoring_ts_3),scoring_projected_3);  colnames(scoring_combined_3) <- c("scoring_ts", "scoring_projected")
  
  
  
  
  
  
  
  
  scoring_data_group_4 = head(scoring_data_group_3, -24)   # Going back to 12 hours ago   # new
  scoring_data_set_4 = head(scoring_data_set_3, -24)   # Going back to 12 hours ago       # new
  scoring_nhor_4 <- 24  # 12 hours of forecast
  scoring_df.lev_4 <- scoring_data_group_4[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                                              "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
                                              "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                                              "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                                              "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                                              'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  scoring_df.lev_4 = data.frame(lapply(scoring_df.lev_4, function(x) as.numeric(as.character(x))),  # new
                                check.names=F, row.names = rownames(scoring_df.lev_4))  # new
  scoring_df.lev_4[is.na(scoring_df.lev_4)] = 0 # new
  scoring_m.lev_4  <- as.matrix(scoring_df.lev_4)
  scoring_nr_lev_4 <- nrow(scoring_df.lev_4)
  scoring_result_4 = VARselect(sapply((scoring_df.lev_4), as.numeric), lag.max = 20,
                               type = "const", season = NULL)
  scoring_var.model_lev_4 <- VAR(sapply((scoring_df.lev_4), as.numeric), p = result$selection['AIC(n)'], 
                                 type = "const", season = NULL)
  scoring_var.pred_4 <- predict(scoring_var.model_lev_4, n.ahead = scoring_nhor)
  
  scoring_ts_4 = c(tail(as.POSIXct((datetime2=as.POSIXct((scoring_data_set_4$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
                   seq(from=as.POSIXct((datetime2=as.POSIXct((scoring_data_set_4$DateTime[nrow(scoring_data_set_4)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=scoring_nhor_4, by = 30*60)[1:
                                                                                                                                                                                                length(scoring_var.pred_4$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  scoring_projected_4 = c(tail(as.numeric(scoring_df.lev_4$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                          scoring_var.pred_4$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  scoring_combined_4 = cbind(as.data.frame(scoring_ts_4),scoring_projected_4); colnames(scoring_combined_4) <- c("scoring_ts", "scoring_projected")
  
  
  total_scoring_combined = rbind(tail(scoring_combined_4,24),tail(scoring_combined_3,24),tail(scoring_combined_2,24),tail(scoring_combined,24), make.row.names = FALSE)
  colnames(total_scoring_combined) <- c("scoring_ts", "scoring_projected")
  
  
  
  
  
  
  # merge(cbind(seq(1,120),combined[,1:2]),   cbind(seq(1,96),total_scoring_combined))
  
  plot_predicted_and_actual = ggplot(merge(cbind(seq(1,120),combined[,1:2]),   cbind(seq(1,96),total_scoring_combined))) +
    geom_line(data = combined[,1:2], aes(x=ts, y=projected, linewidth=2, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
    geom_point(data = combined[,1:2], aes(x=ts, y=projected, size=3, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) +
    theme(legend.position = "none") + scale_color_manual(values=c("white","blue3")) +
    geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + 
    geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=2,color = "red") + geom_hline(yintercept = c(80.5,82.5), linetype="dashed", color="green", linewidth=2) +
    ggtitle("Past 3 days plus 12-hour Forecast") + ylab("Metra 4C Causticizing Degree %") + xlab("Date") + ylim(mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.6, mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.6) +
    #geom_text(aes(ts, projected, label = paste(ts[-1]," *")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') + # qcc(t(projected[,2]),ylim=c(78,83),nsigmas=3)
    #geom_text(aes(ts, projected, label = paste("OUT OF CONTROL!")), data = combined[abs(combined[,2])>mean(combined[,2]) + (3 * sd(combined[,2])),],angle=30,size=3, color = 'orange') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOC!</b>")), nudge_y=-0.3, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) | abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,2)), ],angle=-45,size=5, color = 'orange') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=+0.5, nudge_x=+0.5, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(rep(82.5,length(ts))) & abs((head(combined[,2], n=120))) < mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])>mean(rep(82.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste(ts," *")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=3, color = 'yellow') +
    #geom_text(aes(ts, projected, label = paste("OUT OF SPEC!")), data = combined[abs(combined[,2])<mean(rep(80.5,length(ts))),][c(TRUE,rep(FALSE,3)), ],angle=30,size=4, color = 'yellow') +
    geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=-0.1, nudge_x=+30*60, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) < mean(rep(80.5,length(ts))) & abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=5, color = 'yellow') +
    theme(panel.background = element_rect(fill = 'black', color = 'purple'),
          panel.grid.major = element_line(color = 'lightblue'),
          panel.grid.minor = element_line(color = 'lightblue', linetype = 'dotted', size = 0.5)) +
    theme(text = element_text(size = 20), axis.text.x=element_text(angle=90, vjust = 0.3, hjust = 1)) +
    annotate("text", label = "Upper Spec Limit", size=6, x=ts[5],y=82.5+0.6, color="green") + annotate("text", label = "Lower Spec Limit", size=6, x=ts[5],y=80.5-0.6, color="green") +
    annotate("text", label = "UCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.55, color="red") + annotate("text", label = "LCL", size=6, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.55, color="red") +
    scale_x_datetime(breaks = breaks_width("3 hours"), minor_breaks = date_breaks("1 hour"), date_labels = "%b %d     %H:%M") +
    geom_point(data = total_scoring_combined, aes(x=total_scoring_combined$scoring_ts, y=total_scoring_combined$scoring_projected), colour = "dodgerblue", size=3, shape=18) +
    geom_line(data = total_scoring_combined, aes(x=total_scoring_combined$scoring_ts, y=total_scoring_combined$scoring_projected), colour = "dodgerblue", linewidth=2, linetype=1)
  
  
  
  library(plotly)
  ggplotly(plot_predicted_and_actual)
  
  
  
  
  
  
  
  
  #wrap ggplot object with ggplotly
  # library(plotly)
  # Plot_27 <- ggplotly(Plot_23)
  
  # plot.new(); dev.control("enable"); 
  # #plot(projected[,1], projected[,2], type = "l", col="blue")  
  # Plot_4 <- recordPlot(); plot.new(); dev.off() ## clean up device# Plot_4
  # par(bg = 'white') # restore background color to original white
  # 
  # all = c(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`), var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])
  # tsx = c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
  #         seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)); par(bg = 'darkseagreen1')
  # plot.new(); dev.control("enable");
  # plot(tsx,all, type="l", col="blue", main="METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", xlab="", ylab=""); 
  # abline(v = as.POSIXct(end_time, format="%Y%m%d_%H%M%S"), col="red");
  # Plot_5 <- recordPlot(); dev.off() ## clean up device
  # par(bg = 'white') # restore background color to original white
  # plot.new();
  
  # #========================================================
  # # VAR model in difference using vars
  # #========================================================
  # 
  # # 1st differenced data
  # df.diff <- diff(as.matrix(sapply(na.omit(df.lev), as.numeric)), lag = 1)
  # colnames(df.diff) <- c("GL Temp at #2 || 565TIC096C.PV","I565LCP103_F3.PV | SLAKER  GL TO LIME RATIO",
  #                        "Lime to GL Ratio Controller | 565FF1128.PV","METRA 4C Causticizing Degree % | _565_4CAU_CE.PV",
  #                        "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
  #                        "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
  #                        "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER")
  # m.diff <- as.matrix(df.diff)
  # 
  # # lag length
  # result_lag = VARselect(df.diff, lag.max = 20,
  #                        type = "const", season = NULL)
  # 
  # # estimation
  # vare_diff <- VAR(df.diff, p = result_lag$selection['AIC(n)'], 
  #                  type = "const", season = NULL)
  # 
  # # forecast of differenced data
  # varf_diff <- predict(vare_diff, n.ahead = nhor)
  # x11(); par(mai=rep(0.4,4)); plot(varf_diff)
  # ############ x11(); par(mai=rep(0.4,4)); fanchart(varf_diff)
  # 
  # # recover lev forecast
  # m.varf_lev_ft <- rbind(m.lev, matrix(NA, nhor, ncol(m.lev)))
  # m.ft_df <- do.call(cbind,lapply(varf_diff$fcst, 
  #                                 function(x) x[,"fcst"]))
  # 
  # 
  # 
  # 
  # # growth to level
  # for(h in (nr_lev+1):(nr_lev+nhor)) {
  #   hf <- h - nr_lev
  #   m.varf_lev_ft[h,] <- sapply(m.varf_lev_ft[h-1,], as.numeric) + m.ft_df[hf,]
  # }
  # 
  # 
  # 
  # # Draw Graph
  # x11(width=8, height = 8); 
  # par(mfrow=c(4,3), mar=c(2,2,2,2))
  # 
  # for(i in 1:11) {
  #   df <- m.varf_lev_ft[,i]
  #   matplot(df, type=c("l"), col = c("blue"), 
  #           main = str.main[i]) 
  #   abline(v=nr_lev, col="blue")
  # }
  # 
  # 
  # #========================================================
  # # VAR model in difference using tsDyn
  # #========================================================
  # 
  # linevare_diff <- lineVar(data = sapply(na.omit(df.lev), as.numeric), lag = 1, include = "const",
  #                          model = "VAR", I = "diff", beta = NULL)       #, exogen = dum_season)
  # 
  # # check if both models (vars & tsDyn) yield same coefficients
  # linevare_diff 
  # do.call(rbind,lapply(vare_diff$varresult, 
  #                      function(x) x$coefficients))
  # 
  # # forecast
  # linevarf_diff <- predict(linevare_diff, n.ahead = nhor) 
  # 
  # # Draw Graph
  # x11(width=8, height = 8); 
  # par(mfrow=c(4,3), mar=c(2,2,2,2))
  # 
  # df <- rbind(df.lev, linevarf_diff)
  # 
  # for(i in 1:11) {
  #   matplot(df[,i], type=c("l"), col = c("blue"), 
  #           main = str.main[i]) 
  #   abline(v=nr_lev, col="red")
  # }
  # 
  # 
  # ts2 = seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(df.lev)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)
  # projected = cbind(as.data.frame(ts2), tail(df$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`, n=nrow(linevarf_diff)))  #  tail(data_frame, n = 4)
  # plot(projected[,1], projected[,2], type = "l", col="blue")
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###################################################################################################################################################################
  
  #library(flextable)
  #library(bruceR)
  # evaluating Granger causality in relation to METRA 4C Causticizing Degree
  #causality_test = granger_causality(var.model_lev)
  #Granger_Causality_Ranking = causality_test$result[c(which(causality_test$result$Equation=="METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV")),][c(order(causality_test$result[c(which(causality_test$result$Equation=="METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV")),]$p.F, decreasing=FALSE)),]
  #Granger_Causality_Ranking = cbind(Granger_Causality_Ranking$Excluded, Granger_Causality_Ranking$sig.F)
  #colnames(Granger_Causality_Ranking) = c("Time Series", "Granger causality test significance")
  #Granger_Causality_Ranking = as.data.frame(Granger_Causality_Ranking)
  
  #return(as.data.frame(Granger_Causality_Ranking))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################## RCA
  
  library(arules)
  library(arulesViz)
  library(arulesCBA)
  library(readxl)
  
  
  
  
  
  
  
  
  
  f <<- function(x) {
    if(is.list(x) ) lapply(x,f)
    else ifelse(length(x) == 0 | typeof(x)=="double", 0, x)
  }
  
  
  
  
  
  
  
  
  
  
  
  overall <<- function(x) {
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
  
  
  
  
  
  overall_2 <<- function(x) {
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
  
  #overall_2(irisDisc[4])
  
  
  
  
  
  h = c()
  settings <<- function(i) {
    h = as.data.frame(cbind(overall(irisDisc[i]), colnames(iris)[i]))
    colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
    return (h)
  }
  
  
  
  h = c()
  settings_2 <<- function(i) {
    h = as.data.frame(cbind(overall_2(irisDisc[i]), colnames(iris)[i]))
    colnames(h) <- c("Level", "Minimum", "Maximum", "Tag")
    return (h)
  }
  
  
  
  
  
  
  finallist = c()
  overall_settings_2 <<- function(mylist) {
    for (i in as.numeric(mylist)) {
      finallist <- rbind(finallist,settings_2(i))
    }
    return (finallist)
  }
  
  
  
  
  
  
  
  finallist = c()
  overall_settings <<- function(mylist) {
    for (i in as.numeric(mylist)) {
      finallist <- rbind(finallist,settings(i))
    }
    return (finallist)
  }
  
  
  
  
  
  
  
  
  iris = na.omit(sapply(data_group[,!names(data_group) %in% c("565MK1104.PV | SLAKER PUMP", 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request')], as.numeric))
  
  iris = iris[,c("GL Temp at #2 || 565TIC096C.PV","565HY103H.PV | PURCH/REBURN",
                 "Lime to GL Ratio Controller | 565FF1128.PV", "METRA CGL Sulfidity S% | _565_CGL_S.PV", "METRA CGL TTA | _565_CGL_TTA.PV",
                 "METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", "565TIC103.PV | SLAKER BOWL",
                 "565SIC1128.PV | REBURN LIME", "565SIC1127.PV | PURCHASED LIME","565FIC094.PV | GL->SLAKER",
                 'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK',
                 "METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")]
  
  
  
  
  set.seed(1); irisDisc1 <<- (discretize(iris[,1], method = "cluster", breaks = 10,
                                         labels = c("x1_1", "x1_2","x1_3","x1_4","x1_5","x1_6","x1_7","x1_8","x1_9","x1_10")))
  set.seed(1); irisDisc2 <<- (discretize(iris[,2], method = "cluster", breaks = 10,
                                         labels = c("x2_1", "x2_2","x2_3","x2_4","x2_5","x2_6","x2_7","x2_8","x2_9","x2_10")))
  set.seed(1); irisDisc3 <<- (discretize(iris[,3], method = "cluster", breaks = 10,
                                         labels = c("x3_1", "x3_2","x3_3","x3_4","x3_5","x3_6","x3_7","x3_8","x3_9","x3_10")))
  set.seed(1); irisDisc4 <<- (discretize(iris[,4], method = "cluster", breaks = 10,
                                         labels = c("x4_1", "x4_2","x4_3","x4_4","x4_5","x4_6","x4_7","x4_8","x4_9","x4_10")))
  set.seed(1); irisDisc5 <<- (discretize(iris[,5], method = "cluster", breaks = 10,
                                         labels = c("x5_1", "x5_2","x5_3","x5_4","x5_5","x5_6","x5_7","x5_8","x5_9","x5_10")))
  set.seed(1); irisDisc6 <<- (discretize(iris[,6], method = "cluster", breaks = 10,
                                         labels = c("x6_1", "x6_2","x6_3","x6_4","x6_5","x6_6","x6_7","x6_8","x6_9","x6_10")))
  set.seed(1); irisDisc7 <<- (discretize(iris[,7], method = "cluster", breaks = 10,
                                         labels = c("x7_1", "x7_2","x7_3","x7_4","x7_5","x7_6","x7_7","x7_8","x7_9","x7_10")))
  set.seed(1); irisDisc8 <<- (discretize(iris[,8], method = "cluster", breaks = 10,
                                         labels = c("x8_1", "x8_2","x8_3","x8_4","x8_5","x8_6","x8_7","x8_8","x8_9","x8_10")))
  set.seed(1); irisDisc9 <<- (discretize(iris[,9], method = "cluster", breaks = 10,
                                         labels = c("x9_1", "x9_2","x9_3","x9_4","x9_5","x9_6","x9_7","x9_8","x9_9","x9_10")))
  set.seed(1); irisDisc10 <<- (discretize(iris[,10], method = "cluster", breaks = 10,
                                          labels = c("x10_1", "x10_2","x10_3","x10_4","x10_5","x10_6","x10_7","x10_8","x10_9","x10_10")))
  set.seed(1); irisDisc11 <<- (discretize(iris[,11], method = "cluster", breaks = 10,
                                          labels = c("x11_1", "x11_2","x11_3","x11_4","x11_5","x11_6","x11_7","x11_8","x11_9","x11_10")))
  set.seed(1); irisDisc12 <<- (discretize(iris[,12], method = "cluster", breaks = 10,
                                          labels = c("x12_1", "x12_2","x12_3","x12_4","x12_5","x12_6","x12_7","x12_8","x12_9","x12_10")))
  set.seed(1); irisDisc13 <<- (discretize(iris[,13], method = "cluster", breaks = 10,
                                          labels = c("x13_1", "x13_2","x13_3","x13_4","x13_5","x13_6","x13_7","x13_8","x13_9","x13_10")))
  set.seed(1); irisDisc14 <<- (discretize(iris[,14], method = "cluster", breaks = 10,
                                          labels = c("y1_1", "y1_2","y1_3","y1_4","y1_5","y1_6","y1_7","y1_8","y1_9","y1_10")))
  
  
  
  
  
  
  
  irisDisc <- cbind.data.frame(irisDisc1, irisDisc2, irisDisc3, irisDisc4, irisDisc5, irisDisc6, irisDisc7, irisDisc8, irisDisc9, irisDisc10, irisDisc11, irisDisc12, irisDisc13, irisDisc14)
  
  
  library(data.table)
  targets_y1 = c("y1_6","y1_7"); 
  
  
  # CJ(targets_y1, unique = TRUE) -> targets_raw; targets_raw <- as.data.frame(targets_raw)
  targets_raw <- as.list(targets_y1) 
  cbind.data.frame(as.character(irisDisc14)) -> target_variable
  
  
  
  
  
  
  
  write.csv(irisDisc, file = "irisDisc.csv")                    
  dataset = read.transactions('irisDisc.csv', sep = ',', rm.duplicates = TRUE)
  
  
  
  
  
  
  
  library(ggplotify)
  library(gridExtra)
  
  
  
  
  
  
  
  
  #sum(do.call(rbind, targets_raw) %in% do.call(paste0, target_variable))
  
  sum(do.call(paste0, target_variable)  %in% do.call(rbind, targets_raw))
  nrow(target_variable)
  
  green_gen = sum(do.call(paste0, target_variable)  %in% do.call(rbind, targets_raw)) / nrow(target_variable)
  red_gen = 1 - green_gen
  
  
  
  
  library(prodlim)
  
  # sum(paste(cbind("x1_1", (targets_raw)), collapse="")  %in% do.call(paste0, c("x1_1", target_variable)) )
  
  # do.call(paste0, c("x1_1", target_variable))
  
  
  
  
  
  
  green_perc_x1_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw))))
  red_perc_x1_1 = 1 - green_perc_x1_1
  
  green_perc_x1_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw))))
  red_perc_x1_2 = 1 - green_perc_x1_2
  
  green_perc_x1_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw))))
  red_perc_x1_3 = 1 - green_perc_x1_3
  
  green_perc_x1_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw))))
  red_perc_x1_4 = 1 - green_perc_x1_4
  
  green_perc_x1_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw))))
  red_perc_x1_5 = 1 - green_perc_x1_5
  
  green_perc_x1_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw))))
  red_perc_x1_6 = 1 - green_perc_x1_6
  
  green_perc_x1_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw))))
  red_perc_x1_7 = 1 - green_perc_x1_7
  
  green_perc_x1_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))
  red_perc_x1_8 = 1 - green_perc_x1_8
  
  green_perc_x1_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw))))
  red_perc_x1_9 = 1 - green_perc_x1_9
  
  green_perc_x1_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw))))
  red_perc_x1_10 = 1 - green_perc_x1_10
  
  
  
  { red_lift_x1_1 <<- 100*(red_perc_x1_1 - red_gen) / red_gen };    {green_lift_x1_1 <<- 100*(green_perc_x1_1 - green_gen) / green_gen} ; if  (red_perc_x1_1 == green_perc_x1_1) {red_lift_x1_1 = 0}; if  (red_perc_x1_1 == green_perc_x1_1) {green_lift_x1_1 = 0};
  { red_lift_x1_2 <<- 100*(red_perc_x1_2 - red_gen) / red_gen };   {green_lift_x1_2 <<- 100*(green_perc_x1_2 - green_gen) / green_gen} ; if  (red_perc_x1_2 == green_perc_x1_2) {red_lift_x1_2 = 0}; if  (red_perc_x1_2 == green_perc_x1_2) {green_lift_x1_2 = 0};
  { red_lift_x1_3 <<- 100*(red_perc_x1_3 - red_gen) / red_gen };   {green_lift_x1_3 <<- 100*(green_perc_x1_3 - green_gen) / green_gen} ; if  (red_perc_x1_3 == green_perc_x1_3) {red_lift_x1_3 = 0}; if  (red_perc_x1_3 == green_perc_x1_3) {green_lift_x1_3 = 0};
  { red_lift_x1_4 <<- 100*(red_perc_x1_4 - red_gen) / red_gen };    {green_lift_x1_4 <<- 100*(green_perc_x1_4 - green_gen) / green_gen} ; if  (red_perc_x1_4 == green_perc_x1_4) {red_lift_x1_4 = 0}; if  (red_perc_x1_4 == green_perc_x1_4) {green_lift_x1_4 = 0};
  { red_lift_x1_5 <<- 100*(red_perc_x1_5 - red_gen) / red_gen };    {green_lift_x1_5 <<- 100*(green_perc_x1_5 - green_gen) / green_gen} ; if  (red_perc_x1_5 == green_perc_x1_5) {red_lift_x1_5 = 0}; if  (red_perc_x1_5 == green_perc_x1_5) {green_lift_x1_5 = 0};
  { red_lift_x1_6 <<- 100*(red_perc_x1_6 - red_gen) / red_gen };    {green_lift_x1_6 <<- 100*(green_perc_x1_6 - green_gen) / green_gen} ; if  (red_perc_x1_6 == green_perc_x1_6) {red_lift_x1_6 = 0}; if  (red_perc_x1_6 == green_perc_x1_6) {green_lift_x1_6 = 0};
  { red_lift_x1_7 <<- 100*(red_perc_x1_7 - red_gen) / red_gen };   {green_lift_x1_7 <<- 100*(green_perc_x1_7 - green_gen) / green_gen} ; if  (red_perc_x1_7 == green_perc_x1_7) {red_lift_x1_7 = 0}; if  (red_perc_x1_7 == green_perc_x1_7) {green_lift_x1_7 = 0};
  { red_lift_x1_8 <<- 100*(red_perc_x1_8 - red_gen) / red_gen };   {green_lift_x1_8 <<- 100*(green_perc_x1_8 - green_gen) / green_gen} ; if  (red_perc_x1_8 == green_perc_x1_8) {red_lift_x1_8 = 0}; if  (red_perc_x1_8 == green_perc_x1_8) {green_lift_x1_8 = 0};
  { red_lift_x1_9 <<- 100*(red_perc_x1_9 - red_gen) / red_gen };   {green_lift_x1_9 <<- 100*(green_perc_x1_9 - green_gen) / green_gen} ; if  (red_perc_x1_9 == green_perc_x1_9) {red_lift_x1_9 = 0}; if  (red_perc_x1_9 == green_perc_x1_9) {green_lift_x1_9 = 0};
  { red_lift_x1_10 <<- 100*(red_perc_x1_10 - red_gen) / red_gen };   {green_lift_x1_10 <<- 100*(green_perc_x1_10 - green_gen) / green_gen} ; if  (red_perc_x1_10 == green_perc_x1_10) {red_lift_x1_10 = 0}; if  (red_perc_x1_10 == green_perc_x1_10) {green_lift_x1_10 = 0};
  
  
  FP_1 = (
    coalesce( if (f(green_lift_x1_1 > red_lift_x1_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x1_2 > red_lift_x1_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_3 > red_lift_x1_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_4 > red_lift_x1_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_5 > red_lift_x1_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_6 > red_lift_x1_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_7 > red_lift_x1_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_8 > red_lift_x1_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x1_9 > red_lift_x1_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_10 > red_lift_x1_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_1 = (
    coalesce( if (f(green_lift_x1_1 < red_lift_x1_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x1_2 < red_lift_x1_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_3 < red_lift_x1_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_4 < red_lift_x1_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_5 < red_lift_x1_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_6 < red_lift_x1_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_7 < red_lift_x1_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_8 < red_lift_x1_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x1_9 < red_lift_x1_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_10 < red_lift_x1_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_1 = (
    coalesce( if (f(green_lift_x1_1 > red_lift_x1_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x1_2 > red_lift_x1_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_3 > red_lift_x1_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_4 > red_lift_x1_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_5 > red_lift_x1_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_6 > red_lift_x1_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_7 > red_lift_x1_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_8 > red_lift_x1_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x1_9 > red_lift_x1_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_10 > red_lift_x1_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_1 = (
    coalesce( if (f(green_lift_x1_1 < red_lift_x1_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x1_2 < red_lift_x1_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_3 < red_lift_x1_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_4 < red_lift_x1_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_5 < red_lift_x1_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_6 < red_lift_x1_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_7 < red_lift_x1_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_8 < red_lift_x1_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x1_9 < red_lift_x1_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x1_10 < red_lift_x1_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc1 == "x1_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_1_table <<- as.data.frame.matrix(rbind(c(green_perc_x1_1 * summary(irisDisc1)[1], green_perc_x1_2 * summary(irisDisc1)[2],green_perc_x1_3 * summary(irisDisc1)[3],
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
  P_four_1 <<- (4 * TP_1 * TN_1) / (4 * TP_1 * TN_1 + (TP_1 + TN_1) * (FP_1 + FN_1)); P_four_1 = coalesce(P_four_1, 0)
  
  
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
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(1)$Tag), cex.main = 0.7,
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
    ),   title(paste(unique(settings(1)$Tag), expression("-> {"), round(100*P_four_1, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
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
  
  
  
  pp1b = as.grob(expression(barplot(as.matrix(x_1_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc1, which = "discretized:breaks")[1],attr(x = irisDisc1, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_1_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x2_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw))))
  red_perc_x2_1 = 1 - green_perc_x2_1
  
  green_perc_x2_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw))))
  red_perc_x2_2 = 1 - green_perc_x2_2
  
  green_perc_x2_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw))))
  red_perc_x2_3 = 1 - green_perc_x2_3
  
  green_perc_x2_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw))))
  red_perc_x2_4 = 1 - green_perc_x2_4
  
  green_perc_x2_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw))))
  red_perc_x2_5 = 1 - green_perc_x2_5
  
  green_perc_x2_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw))))
  red_perc_x2_6 = 1 - green_perc_x2_6
  
  green_perc_x2_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw))))
  red_perc_x2_7 = 1 - green_perc_x2_7
  
  green_perc_x2_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))
  red_perc_x2_8 = 1 - green_perc_x2_8
  
  green_perc_x2_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw))))
  red_perc_x2_9 = 1 - green_perc_x2_9
  
  green_perc_x2_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw))))
  red_perc_x2_10 = 1 - green_perc_x2_10
  
  
  
  { red_lift_x2_1 <<- 100*(red_perc_x2_1 - red_gen) / red_gen };    {green_lift_x2_1 <<- 100*(green_perc_x2_1 - green_gen) / green_gen} ; if  (red_perc_x2_1 == green_perc_x2_1) {red_lift_x2_1 = 0}; if  (red_perc_x2_1 == green_perc_x2_1) {green_lift_x2_1 = 0};
  { red_lift_x2_2 <<- 100*(red_perc_x2_2 - red_gen) / red_gen };   {green_lift_x2_2 <<- 100*(green_perc_x2_2 - green_gen) / green_gen} ; if  (red_perc_x2_2 == green_perc_x2_2) {red_lift_x2_2 = 0}; if  (red_perc_x2_2 == green_perc_x2_2) {green_lift_x2_2 = 0};
  { red_lift_x2_3 <<- 100*(red_perc_x2_3 - red_gen) / red_gen };   {green_lift_x2_3 <<- 100*(green_perc_x2_3 - green_gen) / green_gen} ; if  (red_perc_x2_3 == green_perc_x2_3) {red_lift_x2_3 = 0}; if  (red_perc_x2_3 == green_perc_x2_3) {green_lift_x2_3 = 0};
  { red_lift_x2_4 <<- 100*(red_perc_x2_4 - red_gen) / red_gen };    {green_lift_x2_4 <<- 100*(green_perc_x2_4 - green_gen) / green_gen} ; if  (red_perc_x2_4 == green_perc_x2_4) {red_lift_x2_4 = 0}; if  (red_perc_x2_4 == green_perc_x2_4) {green_lift_x2_4 = 0};
  { red_lift_x2_5 <<- 100*(red_perc_x2_5 - red_gen) / red_gen };    {green_lift_x2_5 <<- 100*(green_perc_x2_5 - green_gen) / green_gen} ; if  (red_perc_x2_5 == green_perc_x2_5) {red_lift_x2_5 = 0}; if  (red_perc_x2_5 == green_perc_x2_5) {green_lift_x2_5 = 0};
  { red_lift_x2_6 <<- 100*(red_perc_x2_6 - red_gen) / red_gen };    {green_lift_x2_6 <<- 100*(green_perc_x2_6 - green_gen) / green_gen} ; if  (red_perc_x2_6 == green_perc_x2_6) {red_lift_x2_6 = 0}; if  (red_perc_x2_6 == green_perc_x2_6) {green_lift_x2_6 = 0};
  { red_lift_x2_7 <<- 100*(red_perc_x2_7 - red_gen) / red_gen };   {green_lift_x2_7 <<- 100*(green_perc_x2_7 - green_gen) / green_gen} ; if  (red_perc_x2_7 == green_perc_x2_7) {red_lift_x2_7 = 0}; if  (red_perc_x2_7 == green_perc_x2_7) {green_lift_x2_7 = 0};
  { red_lift_x2_8 <<- 100*(red_perc_x2_8 - red_gen) / red_gen };   {green_lift_x2_8 <<- 100*(green_perc_x2_8 - green_gen) / green_gen} ; if  (red_perc_x2_8 == green_perc_x2_8) {red_lift_x2_8 = 0}; if  (red_perc_x2_8 == green_perc_x2_8) {green_lift_x2_8 = 0};
  { red_lift_x2_9 <<- 100*(red_perc_x2_9 - red_gen) / red_gen };   {green_lift_x2_9 <<- 100*(green_perc_x2_9 - green_gen) / green_gen} ; if  (red_perc_x2_9 == green_perc_x2_9) {red_lift_x2_9 = 0}; if  (red_perc_x2_9 == green_perc_x2_9) {green_lift_x2_9 = 0};
  { red_lift_x2_10 <<- 100*(red_perc_x2_10 - red_gen) / red_gen };   {green_lift_x2_10 <<- 100*(green_perc_x2_10 - green_gen) / green_gen} ; if  (red_perc_x2_10 == green_perc_x2_10) {red_lift_x2_10 = 0}; if  (red_perc_x2_10 == green_perc_x2_10) {green_lift_x2_10 = 0};
  
  
  FP_2 = (
    coalesce( if (f(green_lift_x2_1 > red_lift_x2_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x2_2 > red_lift_x2_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_3 > red_lift_x2_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_4 > red_lift_x2_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_5 > red_lift_x2_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_6 > red_lift_x2_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_7 > red_lift_x2_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_8 > red_lift_x2_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x2_9 > red_lift_x2_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_10 > red_lift_x2_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_2 = (
    coalesce( if (f(green_lift_x2_1 < red_lift_x2_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x2_2 < red_lift_x2_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_3 < red_lift_x2_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_4 < red_lift_x2_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_5 < red_lift_x2_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_6 < red_lift_x2_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_7 < red_lift_x2_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_8 < red_lift_x2_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x2_9 < red_lift_x2_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_10 < red_lift_x2_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_2 = (
    coalesce( if (f(green_lift_x2_1 > red_lift_x2_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x2_2 > red_lift_x2_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_3 > red_lift_x2_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_4 > red_lift_x2_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_5 > red_lift_x2_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_6 > red_lift_x2_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_7 > red_lift_x2_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_8 > red_lift_x2_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x2_9 > red_lift_x2_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_10 > red_lift_x2_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_2 = (
    coalesce( if (f(green_lift_x2_1 < red_lift_x2_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x2_2 < red_lift_x2_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_3 < red_lift_x2_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_4 < red_lift_x2_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_5 < red_lift_x2_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_6 < red_lift_x2_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_7 < red_lift_x2_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_8 < red_lift_x2_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x2_9 < red_lift_x2_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x2_10 < red_lift_x2_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc2 == "x2_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_2_table <<- as.data.frame.matrix(rbind(c(green_perc_x2_1 * summary(irisDisc2)[1], green_perc_x2_2 * summary(irisDisc2)[2],green_perc_x2_3 * summary(irisDisc2)[3],
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
  P_four_2 <<- (4 * TP_2 * TN_2) / (4 * TP_2 * TN_2 + (TP_2 + TN_2) * (FP_2 + FN_2)); P_four_2 = coalesce(P_four_2, 0)
  
  
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
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(2)$Tag), cex.main = 0.7,
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
    ),   title(paste(unique(settings(2)$Tag), expression("-> {"), round(100*P_four_2, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
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
  
  
  
  pp2b = as.grob(expression(barplot(as.matrix(x_2_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc2, which = "discretized:breaks")[1],attr(x = irisDisc2, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_2_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x3_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw))))
  red_perc_x3_1 = 1 - green_perc_x3_1
  
  green_perc_x3_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw))))
  red_perc_x3_2 = 1 - green_perc_x3_2
  
  green_perc_x3_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw))))
  red_perc_x3_3 = 1 - green_perc_x3_3
  
  green_perc_x3_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw))))
  red_perc_x3_4 = 1 - green_perc_x3_4
  
  green_perc_x3_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw))))
  red_perc_x3_5 = 1 - green_perc_x3_5
  
  green_perc_x3_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw))))
  red_perc_x3_6 = 1 - green_perc_x3_6
  
  green_perc_x3_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw))))
  red_perc_x3_7 = 1 - green_perc_x3_7
  
  green_perc_x3_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))
  red_perc_x3_8 = 1 - green_perc_x3_8
  
  green_perc_x3_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw))))
  red_perc_x3_9 = 1 - green_perc_x3_9
  
  green_perc_x3_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw))))
  red_perc_x3_10 = 1 - green_perc_x3_10
  
  
  
  { red_lift_x3_1 <<- 100*(red_perc_x3_1 - red_gen) / red_gen };    {green_lift_x3_1 <<- 100*(green_perc_x3_1 - green_gen) / green_gen} ; if  (red_perc_x3_1 == green_perc_x3_1) {red_lift_x3_1 = 0}; if  (red_perc_x3_1 == green_perc_x3_1) {green_lift_x3_1 = 0};
  { red_lift_x3_2 <<- 100*(red_perc_x3_2 - red_gen) / red_gen };   {green_lift_x3_2 <<- 100*(green_perc_x3_2 - green_gen) / green_gen} ; if  (red_perc_x3_2 == green_perc_x3_2) {red_lift_x3_2 = 0}; if  (red_perc_x3_2 == green_perc_x3_2) {green_lift_x3_2 = 0};
  { red_lift_x3_3 <<- 100*(red_perc_x3_3 - red_gen) / red_gen };   {green_lift_x3_3 <<- 100*(green_perc_x3_3 - green_gen) / green_gen} ; if  (red_perc_x3_3 == green_perc_x3_3) {red_lift_x3_3 = 0}; if  (red_perc_x3_3 == green_perc_x3_3) {green_lift_x3_3 = 0};
  { red_lift_x3_4 <<- 100*(red_perc_x3_4 - red_gen) / red_gen };    {green_lift_x3_4 <<- 100*(green_perc_x3_4 - green_gen) / green_gen} ; if  (red_perc_x3_4 == green_perc_x3_4) {red_lift_x3_4 = 0}; if  (red_perc_x3_4 == green_perc_x3_4) {green_lift_x3_4 = 0};
  { red_lift_x3_5 <<- 100*(red_perc_x3_5 - red_gen) / red_gen };    {green_lift_x3_5 <<- 100*(green_perc_x3_5 - green_gen) / green_gen} ; if  (red_perc_x3_5 == green_perc_x3_5) {red_lift_x3_5 = 0}; if  (red_perc_x3_5 == green_perc_x3_5) {green_lift_x3_5 = 0};
  { red_lift_x3_6 <<- 100*(red_perc_x3_6 - red_gen) / red_gen };    {green_lift_x3_6 <<- 100*(green_perc_x3_6 - green_gen) / green_gen} ; if  (red_perc_x3_6 == green_perc_x3_6) {red_lift_x3_6 = 0}; if  (red_perc_x3_6 == green_perc_x3_6) {green_lift_x3_6 = 0};
  { red_lift_x3_7 <<- 100*(red_perc_x3_7 - red_gen) / red_gen };   {green_lift_x3_7 <<- 100*(green_perc_x3_7 - green_gen) / green_gen} ; if  (red_perc_x3_7 == green_perc_x3_7) {red_lift_x3_7 = 0}; if  (red_perc_x3_7 == green_perc_x3_7) {green_lift_x3_7 = 0};
  { red_lift_x3_8 <<- 100*(red_perc_x3_8 - red_gen) / red_gen };   {green_lift_x3_8 <<- 100*(green_perc_x3_8 - green_gen) / green_gen} ; if  (red_perc_x3_8 == green_perc_x3_8) {red_lift_x3_8 = 0}; if  (red_perc_x3_8 == green_perc_x3_8) {green_lift_x3_8 = 0};
  { red_lift_x3_9 <<- 100*(red_perc_x3_9 - red_gen) / red_gen };   {green_lift_x3_9 <<- 100*(green_perc_x3_9 - green_gen) / green_gen} ; if  (red_perc_x3_9 == green_perc_x3_9) {red_lift_x3_9 = 0}; if  (red_perc_x3_9 == green_perc_x3_9) {green_lift_x3_9 = 0};
  { red_lift_x3_10 <<- 100*(red_perc_x3_10 - red_gen) / red_gen };   {green_lift_x3_10 <<- 100*(green_perc_x3_10 - green_gen) / green_gen} ; if  (red_perc_x3_10 == green_perc_x3_10) {red_lift_x3_10 = 0}; if  (red_perc_x3_10 == green_perc_x3_10) {green_lift_x3_10 = 0};
  
  
  FP_3 = (
    coalesce( if (f(green_lift_x3_1 > red_lift_x3_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x3_2 > red_lift_x3_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_3 > red_lift_x3_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_4 > red_lift_x3_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_5 > red_lift_x3_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_6 > red_lift_x3_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_7 > red_lift_x3_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_8 > red_lift_x3_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x3_9 > red_lift_x3_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_10 > red_lift_x3_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_3 = (
    coalesce( if (f(green_lift_x3_1 < red_lift_x3_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x3_2 < red_lift_x3_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_3 < red_lift_x3_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_4 < red_lift_x3_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_5 < red_lift_x3_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_6 < red_lift_x3_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_7 < red_lift_x3_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_8 < red_lift_x3_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x3_9 < red_lift_x3_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_10 < red_lift_x3_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_3 = (
    coalesce( if (f(green_lift_x3_1 > red_lift_x3_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x3_2 > red_lift_x3_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_3 > red_lift_x3_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_4 > red_lift_x3_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_5 > red_lift_x3_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_6 > red_lift_x3_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_7 > red_lift_x3_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_8 > red_lift_x3_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x3_9 > red_lift_x3_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_10 > red_lift_x3_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_3 = (
    coalesce( if (f(green_lift_x3_1 < red_lift_x3_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x3_2 < red_lift_x3_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_3 < red_lift_x3_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_4 < red_lift_x3_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_5 < red_lift_x3_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_6 < red_lift_x3_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_7 < red_lift_x3_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_8 < red_lift_x3_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x3_9 < red_lift_x3_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x3_10 < red_lift_x3_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc3 == "x3_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_3_table <<- as.data.frame.matrix(rbind(c(green_perc_x3_1 * summary(irisDisc3)[1], green_perc_x3_2 * summary(irisDisc3)[2],green_perc_x3_3 * summary(irisDisc3)[3],
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
  P_four_3 <<- (4 * TP_3 * TN_3) / (4 * TP_3 * TN_3 + (TP_3 + TN_3) * (FP_3 + FN_3)); P_four_3 = coalesce(P_four_3, 0)
  
  
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
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(3)$Tag), cex.main = 0.7,
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
    ),   title(paste(unique(settings(3)$Tag), expression("-> {"), round(100*P_four_3, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
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
  
  
  
  pp3b = as.grob(expression(barplot(as.matrix(x_3_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc3, which = "discretized:breaks")[1],attr(x = irisDisc3, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_3_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x4_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw))))
  red_perc_x4_1 = 1 - green_perc_x4_1
  
  green_perc_x4_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw))))
  red_perc_x4_2 = 1 - green_perc_x4_2
  
  green_perc_x4_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw))))
  red_perc_x4_3 = 1 - green_perc_x4_3
  
  green_perc_x4_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw))))
  red_perc_x4_4 = 1 - green_perc_x4_4
  
  green_perc_x4_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw))))
  red_perc_x4_5 = 1 - green_perc_x4_5
  
  green_perc_x4_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw))))
  red_perc_x4_6 = 1 - green_perc_x4_6
  
  green_perc_x4_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw))))
  red_perc_x4_7 = 1 - green_perc_x4_7
  
  green_perc_x4_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))
  red_perc_x4_8 = 1 - green_perc_x4_8
  
  green_perc_x4_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw))))
  red_perc_x4_9 = 1 - green_perc_x4_9
  
  green_perc_x4_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw))))
  red_perc_x4_10 = 1 - green_perc_x4_10
  
  
  
  { red_lift_x4_1 <<- 100*(red_perc_x4_1 - red_gen) / red_gen };    {green_lift_x4_1 <<- 100*(green_perc_x4_1 - green_gen) / green_gen} ; if  (red_perc_x4_1 == green_perc_x4_1) {red_lift_x4_1 = 0}; if  (red_perc_x4_1 == green_perc_x4_1) {green_lift_x4_1 = 0};
  { red_lift_x4_2 <<- 100*(red_perc_x4_2 - red_gen) / red_gen };   {green_lift_x4_2 <<- 100*(green_perc_x4_2 - green_gen) / green_gen} ; if  (red_perc_x4_2 == green_perc_x4_2) {red_lift_x4_2 = 0}; if  (red_perc_x4_2 == green_perc_x4_2) {green_lift_x4_2 = 0};
  { red_lift_x4_3 <<- 100*(red_perc_x4_3 - red_gen) / red_gen };   {green_lift_x4_3 <<- 100*(green_perc_x4_3 - green_gen) / green_gen} ; if  (red_perc_x4_3 == green_perc_x4_3) {red_lift_x4_3 = 0}; if  (red_perc_x4_3 == green_perc_x4_3) {green_lift_x4_3 = 0};
  { red_lift_x4_4 <<- 100*(red_perc_x4_4 - red_gen) / red_gen };    {green_lift_x4_4 <<- 100*(green_perc_x4_4 - green_gen) / green_gen} ; if  (red_perc_x4_4 == green_perc_x4_4) {red_lift_x4_4 = 0}; if  (red_perc_x4_4 == green_perc_x4_4) {green_lift_x4_4 = 0};
  { red_lift_x4_5 <<- 100*(red_perc_x4_5 - red_gen) / red_gen };    {green_lift_x4_5 <<- 100*(green_perc_x4_5 - green_gen) / green_gen} ; if  (red_perc_x4_5 == green_perc_x4_5) {red_lift_x4_5 = 0}; if  (red_perc_x4_5 == green_perc_x4_5) {green_lift_x4_5 = 0};
  { red_lift_x4_6 <<- 100*(red_perc_x4_6 - red_gen) / red_gen };    {green_lift_x4_6 <<- 100*(green_perc_x4_6 - green_gen) / green_gen} ; if  (red_perc_x4_6 == green_perc_x4_6) {red_lift_x4_6 = 0}; if  (red_perc_x4_6 == green_perc_x4_6) {green_lift_x4_6 = 0};
  { red_lift_x4_7 <<- 100*(red_perc_x4_7 - red_gen) / red_gen };   {green_lift_x4_7 <<- 100*(green_perc_x4_7 - green_gen) / green_gen} ; if  (red_perc_x4_7 == green_perc_x4_7) {red_lift_x4_7 = 0}; if  (red_perc_x4_7 == green_perc_x4_7) {green_lift_x4_7 = 0};
  { red_lift_x4_8 <<- 100*(red_perc_x4_8 - red_gen) / red_gen };   {green_lift_x4_8 <<- 100*(green_perc_x4_8 - green_gen) / green_gen} ; if  (red_perc_x4_8 == green_perc_x4_8) {red_lift_x4_8 = 0}; if  (red_perc_x4_8 == green_perc_x4_8) {green_lift_x4_8 = 0};
  { red_lift_x4_9 <<- 100*(red_perc_x4_9 - red_gen) / red_gen };   {green_lift_x4_9 <<- 100*(green_perc_x4_9 - green_gen) / green_gen} ; if  (red_perc_x4_9 == green_perc_x4_9) {red_lift_x4_9 = 0}; if  (red_perc_x4_9 == green_perc_x4_9) {green_lift_x4_9 = 0};
  { red_lift_x4_10 <<- 100*(red_perc_x4_10 - red_gen) / red_gen };   {green_lift_x4_10 <<- 100*(green_perc_x4_10 - green_gen) / green_gen} ; if  (red_perc_x4_10 == green_perc_x4_10) {red_lift_x4_10 = 0}; if  (red_perc_x4_10 == green_perc_x4_10) {green_lift_x4_10 = 0};
  
  
  FP_4 = (
    coalesce( if (f(green_lift_x4_1 > red_lift_x4_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x4_2 > red_lift_x4_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_3 > red_lift_x4_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_4 > red_lift_x4_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_5 > red_lift_x4_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_6 > red_lift_x4_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_7 > red_lift_x4_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_8 > red_lift_x4_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x4_9 > red_lift_x4_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_10 > red_lift_x4_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_4 = (
    coalesce( if (f(green_lift_x4_1 < red_lift_x4_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x4_2 < red_lift_x4_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_3 < red_lift_x4_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_4 < red_lift_x4_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_5 < red_lift_x4_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_6 < red_lift_x4_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_7 < red_lift_x4_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_8 < red_lift_x4_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x4_9 < red_lift_x4_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_10 < red_lift_x4_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_4 = (
    coalesce( if (f(green_lift_x4_1 > red_lift_x4_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x4_2 > red_lift_x4_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_3 > red_lift_x4_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_4 > red_lift_x4_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_5 > red_lift_x4_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_6 > red_lift_x4_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_7 > red_lift_x4_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_8 > red_lift_x4_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x4_9 > red_lift_x4_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_10 > red_lift_x4_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_4 = (
    coalesce( if (f(green_lift_x4_1 < red_lift_x4_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x4_2 < red_lift_x4_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_3 < red_lift_x4_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_4 < red_lift_x4_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_5 < red_lift_x4_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_6 < red_lift_x4_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_7 < red_lift_x4_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_8 < red_lift_x4_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x4_9 < red_lift_x4_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x4_10 < red_lift_x4_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc4 == "x4_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_4_table <<- as.data.frame.matrix(rbind(c(green_perc_x4_1 * summary(irisDisc4)[1], green_perc_x4_2 * summary(irisDisc4)[2],green_perc_x4_3 * summary(irisDisc4)[3],
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
  P_four_4 <<- (4 * TP_4 * TN_4) / (4 * TP_4 * TN_4 + (TP_4 + TN_4) * (FP_4 + FN_4)); P_four_4 = coalesce(P_four_4, 0)
  
  
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
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(4)$Tag), cex.main = 0.7,
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
    ),   title(paste(unique(settings(4)$Tag), expression("-> {"), round(100*P_four_4, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
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
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x5_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw))))
  red_perc_x5_1 = 1 - green_perc_x5_1
  
  green_perc_x5_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw))))
  red_perc_x5_2 = 1 - green_perc_x5_2
  
  green_perc_x5_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw))))
  red_perc_x5_3 = 1 - green_perc_x5_3
  
  green_perc_x5_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw))))
  red_perc_x5_4 = 1 - green_perc_x5_4
  
  green_perc_x5_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw))))
  red_perc_x5_5 = 1 - green_perc_x5_5
  
  green_perc_x5_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw))))
  red_perc_x5_6 = 1 - green_perc_x5_6
  
  green_perc_x5_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw))))
  red_perc_x5_7 = 1 - green_perc_x5_7
  
  green_perc_x5_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw))))
  red_perc_x5_8 = 1 - green_perc_x5_8
  
  green_perc_x5_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw))))
  red_perc_x5_9 = 1 - green_perc_x5_9
  
  green_perc_x5_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw))))
  red_perc_x5_10 = 1 - green_perc_x5_10
  
  
  
  { red_lift_x5_1 <<- 100*(red_perc_x5_1 - red_gen) / red_gen };    {green_lift_x5_1 <<- 100*(green_perc_x5_1 - green_gen) / green_gen} ; if  (red_perc_x5_1 == green_perc_x5_1) {red_lift_x5_1 = 0}; if  (red_perc_x5_1 == green_perc_x5_1) {green_lift_x5_1 = 0};
  { red_lift_x5_2 <<- 100*(red_perc_x5_2 - red_gen) / red_gen };   {green_lift_x5_2 <<- 100*(green_perc_x5_2 - green_gen) / green_gen} ; if  (red_perc_x5_2 == green_perc_x5_2) {red_lift_x5_2 = 0}; if  (red_perc_x5_2 == green_perc_x5_2) {green_lift_x5_2 = 0};
  { red_lift_x5_3 <<- 100*(red_perc_x5_3 - red_gen) / red_gen };   {green_lift_x5_3 <<- 100*(green_perc_x5_3 - green_gen) / green_gen} ; if  (red_perc_x5_3 == green_perc_x5_3) {red_lift_x5_3 = 0}; if  (red_perc_x5_3 == green_perc_x5_3) {green_lift_x5_3 = 0};
  { red_lift_x5_4 <<- 100*(red_perc_x5_4 - red_gen) / red_gen };    {green_lift_x5_4 <<- 100*(green_perc_x5_4 - green_gen) / green_gen} ; if  (red_perc_x5_4 == green_perc_x5_4) {red_lift_x5_4 = 0}; if  (red_perc_x5_4 == green_perc_x5_4) {green_lift_x5_4 = 0};
  { red_lift_x5_5 <<- 100*(red_perc_x5_5 - red_gen) / red_gen };    {green_lift_x5_5 <<- 100*(green_perc_x5_5 - green_gen) / green_gen} ; if  (red_perc_x5_5 == green_perc_x5_5) {red_lift_x5_5 = 0}; if  (red_perc_x5_5 == green_perc_x5_5) {green_lift_x5_5 = 0};
  { red_lift_x5_6 <<- 100*(red_perc_x5_6 - red_gen) / red_gen };    {green_lift_x5_6 <<- 100*(green_perc_x5_6 - green_gen) / green_gen} ; if  (red_perc_x5_6 == green_perc_x5_6) {red_lift_x5_6 = 0}; if  (red_perc_x5_6 == green_perc_x5_6) {green_lift_x5_6 = 0};
  { red_lift_x5_7 <<- 100*(red_perc_x5_7 - red_gen) / red_gen };   {green_lift_x5_7 <<- 100*(green_perc_x5_7 - green_gen) / green_gen} ; if  (red_perc_x5_7 == green_perc_x5_7) {red_lift_x5_7 = 0}; if  (red_perc_x5_7 == green_perc_x5_7) {green_lift_x5_7 = 0};
  { red_lift_x5_8 <<- 100*(red_perc_x5_8 - red_gen) / red_gen };   {green_lift_x5_8 <<- 100*(green_perc_x5_8 - green_gen) / green_gen} ; if  (red_perc_x5_8 == green_perc_x5_8) {red_lift_x5_8 = 0}; if  (red_perc_x5_8 == green_perc_x5_8) {green_lift_x5_8 = 0};
  { red_lift_x5_9 <<- 100*(red_perc_x5_9 - red_gen) / red_gen };   {green_lift_x5_9 <<- 100*(green_perc_x5_9 - green_gen) / green_gen} ; if  (red_perc_x5_9 == green_perc_x5_9) {red_lift_x5_9 = 0}; if  (red_perc_x5_9 == green_perc_x5_9) {green_lift_x5_9 = 0};
  { red_lift_x5_10 <<- 100*(red_perc_x5_10 - red_gen) / red_gen };   {green_lift_x5_10 <<- 100*(green_perc_x5_10 - green_gen) / green_gen} ; if  (red_perc_x5_10 == green_perc_x5_10) {red_lift_x5_10 = 0}; if  (red_perc_x5_10 == green_perc_x5_10) {green_lift_x5_10 = 0};
  
  
  FP_5 = (
    coalesce( if (f(green_lift_x5_1 > red_lift_x5_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x5_2 > red_lift_x5_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_3 > red_lift_x5_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_4 > red_lift_x5_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_5 > red_lift_x5_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_6 > red_lift_x5_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_7 > red_lift_x5_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_8 > red_lift_x5_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x5_9 > red_lift_x5_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_10 > red_lift_x5_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_5 = (
    coalesce( if (f(green_lift_x5_1 < red_lift_x5_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x5_2 < red_lift_x5_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_3 < red_lift_x5_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_4 < red_lift_x5_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_5 < red_lift_x5_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_6 < red_lift_x5_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_7 < red_lift_x5_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_8 < red_lift_x5_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x5_9 < red_lift_x5_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_10 < red_lift_x5_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_5 = (
    coalesce( if (f(green_lift_x5_1 > red_lift_x5_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x5_2 > red_lift_x5_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_3 > red_lift_x5_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_4 > red_lift_x5_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_5 > red_lift_x5_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_6 > red_lift_x5_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_7 > red_lift_x5_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_8 > red_lift_x5_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x5_9 > red_lift_x5_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_10 > red_lift_x5_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_5 = (
    coalesce( if (f(green_lift_x5_1 < red_lift_x5_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x5_2 < red_lift_x5_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_3 < red_lift_x5_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_4 < red_lift_x5_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_5 < red_lift_x5_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_6 < red_lift_x5_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_7 < red_lift_x5_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_8 < red_lift_x5_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x5_9 < red_lift_x5_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x5_10 < red_lift_x5_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc5 == "x5_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_5_table <<- as.data.frame.matrix(rbind(c(green_perc_x5_1 * summary(irisDisc5)[1], green_perc_x5_2 * summary(irisDisc5)[2],green_perc_x5_3 * summary(irisDisc5)[3],
                                             green_perc_x5_4 * summary(irisDisc5)[4], green_perc_x5_5 * summary(irisDisc5)[5],green_perc_x5_6 * summary(irisDisc5)[6],
                                             green_perc_x5_7 * summary(irisDisc5)[7], green_perc_x5_8 * summary(irisDisc5)[8],green_perc_x5_9 * summary(irisDisc5)[9],
                                             green_perc_x5_10 * summary(irisDisc5)[10]),
                                           c(red_perc_x5_1 * summary(irisDisc5)[1], red_perc_x5_2 * summary(irisDisc5)[2],red_perc_x5_3 * summary(irisDisc5)[3],
                                             red_perc_x5_4 * summary(irisDisc5)[4], red_perc_x5_5 * summary(irisDisc5)[5],red_perc_x5_6 * summary(irisDisc5)[6],
                                             red_perc_x5_7 * summary(irisDisc5)[7], red_perc_x5_8 * summary(irisDisc5)[8],red_perc_x5_9 * summary(irisDisc5)[9],
                                             red_perc_x5_10 * summary(irisDisc5)[10])))
  
  
  
  Precision_5 = TP_5 / (TP_5 + FP_5); Precision_5 = coalesce(Precision_5, 0)
  Recall_5 = TP_5 / (TP_5 + FN_5); Recall_5 = coalesce(Recall_5, 0)
  F1_Score_5 = coalesce ( 2 * (Precision_5 * Recall_5) / (Precision_5 + Recall_5), 0)
  MCC_5 = (TP_5 * TN_5 - FP_5 * FN_5) / sqrt ( (TP_5 + FP_5) * (TP_5 + FN_5) * (TN_5 + FP_5) * (TN_5 + FN_5) )
  MCC_adjusted_5 = (MCC_5 + 1) / 2
  P_four_5 <<- (4 * TP_5 * TN_5) / (4 * TP_5 * TN_5 + (TP_5 + TN_5) * (FP_5 + FN_5)); P_four_5 = coalesce(P_four_5, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp5a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x5_1 > green_lift_x5_1) {red_lift_x5_1} else {green_lift_x5_1},
                         if (red_lift_x5_2 > green_lift_x5_2) {red_lift_x5_2} else {green_lift_x5_2},
                         if (red_lift_x5_3 > green_lift_x5_3) {red_lift_x5_3} else {green_lift_x5_3},
                         if (red_lift_x5_4 > green_lift_x5_4) {red_lift_x5_4} else {green_lift_x5_4},
                         if (red_lift_x5_5 > green_lift_x5_5) {red_lift_x5_5} else {green_lift_x5_5},
                         if (red_lift_x5_6 > green_lift_x5_6) {red_lift_x5_6} else {green_lift_x5_6},
                         if (red_lift_x5_7 > green_lift_x5_7) {red_lift_x5_7} else {green_lift_x5_7},
                         if (red_lift_x5_8 > green_lift_x5_8) {red_lift_x5_8} else {green_lift_x5_8},
                         if (red_lift_x5_9 > green_lift_x5_9) {red_lift_x5_9} else {green_lift_x5_9},
                         if (red_lift_x5_10 > green_lift_x5_10) {red_lift_x5_10} else {green_lift_x5_10}),
                       col = c(
                         if (red_lift_x5_1 > green_lift_x5_1) {"red"} else {"green"},
                         if (red_lift_x5_2 > green_lift_x5_2) {"red"} else {"green"},
                         if (red_lift_x5_3 > green_lift_x5_3) {"red"} else {"green"},
                         if (red_lift_x5_4 > green_lift_x5_4) {"red"} else {"green"},
                         if (red_lift_x5_5 > green_lift_x5_5) {"red"} else {"green"},
                         if (red_lift_x5_6 > green_lift_x5_6) {"red"} else {"green"},
                         if (red_lift_x5_7 > green_lift_x5_7) {"red"} else {"green"},
                         if (red_lift_x5_8 > green_lift_x5_8) {"red"} else {"green"},
                         if (red_lift_x5_9 > green_lift_x5_9) {"red"} else {"green"},
                         if (red_lift_x5_10 > green_lift_x5_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(5)$Tag), cex.main = 0.7,
                    c(if (red_lift_x5_1 > green_lift_x5_1) {red_lift_x5_1} else if (red_lift_x5_1 < green_lift_x5_1) {green_lift_x5_1} else {green_lift_x5_1},
                      if (red_lift_x5_2 > green_lift_x5_2) {red_lift_x5_2} else if (red_lift_x5_2 < green_lift_x5_2) {green_lift_x5_2} else {green_lift_x5_2},
                      if (red_lift_x5_3 > green_lift_x5_3) {red_lift_x5_3} else if (red_lift_x5_3 < green_lift_x5_3) {green_lift_x5_3} else {green_lift_x5_3},
                      if (red_lift_x5_4 > green_lift_x5_4) {red_lift_x5_4} else if (red_lift_x5_4 < green_lift_x5_4) {green_lift_x5_4} else {green_lift_x5_4},
                      if (red_lift_x5_5 > green_lift_x5_5) {red_lift_x5_5} else if (red_lift_x5_5 < green_lift_x5_5) {green_lift_x5_5} else {green_lift_x5_5},
                      if (red_lift_x5_6 > green_lift_x5_6) {red_lift_x5_6} else if (red_lift_x5_6 < green_lift_x5_6) {green_lift_x5_6} else {green_lift_x5_6},
                      if (red_lift_x5_7 > green_lift_x5_7) {red_lift_x5_7} else if (red_lift_x5_7 < green_lift_x5_7) {green_lift_x5_7} else {green_lift_x5_7},
                      if (red_lift_x5_8 > green_lift_x5_8) {red_lift_x5_8} else if (red_lift_x5_8 < green_lift_x5_8) {green_lift_x5_8} else {green_lift_x5_8},
                      if (red_lift_x5_9 > green_lift_x5_9) {red_lift_x5_9} else if (red_lift_x5_9 < green_lift_x5_9) {green_lift_x5_9} else {green_lift_x5_9},
                      if (red_lift_x5_10 > green_lift_x5_10) {red_lift_x5_10} else if (red_lift_x5_10 < green_lift_x5_10) {green_lift_x5_10} else {green_lift_x5_10}),
                    col = c(
                      if (red_lift_x5_1 > green_lift_x5_1) {"red"} else {"green"},
                      if (red_lift_x5_2 > green_lift_x5_2) {"red"} else {"green"},
                      if (red_lift_x5_3 > green_lift_x5_3) {"red"} else {"green"},
                      if (red_lift_x5_4 > green_lift_x5_4) {"red"} else {"green"},
                      if (red_lift_x5_5 > green_lift_x5_5) {"red"} else {"green"},
                      if (red_lift_x5_6 > green_lift_x5_6) {"red"} else {"green"},
                      if (red_lift_x5_7 > green_lift_x5_7) {"red"} else {"green"},
                      if (red_lift_x5_8 > green_lift_x5_8) {"red"} else {"green"},
                      if (red_lift_x5_9 > green_lift_x5_9) {"red"} else {"green"},
                      if (red_lift_x5_10 > green_lift_x5_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(5)$Tag), expression("-> {"), round(100*P_four_5, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x5_1 > green_lift_x5_1) {red_lift_x5_1} else {green_lift_x5_1},
          if (red_lift_x5_2 > green_lift_x5_2) {red_lift_x5_2} else {green_lift_x5_2},
          if (red_lift_x5_3 > green_lift_x5_3) {red_lift_x5_3} else {green_lift_x5_3},
          if (red_lift_x5_4 > green_lift_x5_4) {red_lift_x5_4} else {green_lift_x5_4},
          if (red_lift_x5_5 > green_lift_x5_5) {red_lift_x5_5} else {green_lift_x5_5},
          if (red_lift_x5_6 > green_lift_x5_6) {red_lift_x5_6} else {green_lift_x5_6},
          if (red_lift_x5_7 > green_lift_x5_7) {red_lift_x5_7} else {green_lift_x5_7},
          if (red_lift_x5_8 > green_lift_x5_8) {red_lift_x5_8} else {green_lift_x5_8},
          if (red_lift_x5_9 > green_lift_x5_9) {red_lift_x5_9} else {green_lift_x5_9},
          if (red_lift_x5_10 > green_lift_x5_10) {red_lift_x5_10} else {green_lift_x5_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x5_1 > green_lift_x5_1) {red_lift_x5_1} else {green_lift_x5_1},
                                 if (red_lift_x5_2 > green_lift_x5_2) {red_lift_x5_2} else {green_lift_x5_2},
                                 if (red_lift_x5_3 > green_lift_x5_3) {red_lift_x5_3} else {green_lift_x5_3},
                                 if (red_lift_x5_4 > green_lift_x5_4) {red_lift_x5_4} else {green_lift_x5_4},
                                 if (red_lift_x5_5 > green_lift_x5_5) {red_lift_x5_5} else {green_lift_x5_5},
                                 if (red_lift_x5_6 > green_lift_x5_6) {red_lift_x5_6} else {green_lift_x5_6},
                                 if (red_lift_x5_7 > green_lift_x5_7) {red_lift_x5_7} else {green_lift_x5_7},
                                 if (red_lift_x5_8 > green_lift_x5_8) {red_lift_x5_8} else {green_lift_x5_8},
                                 if (red_lift_x5_9 > green_lift_x5_9) {red_lift_x5_9} else {green_lift_x5_9},
                                 if (red_lift_x5_10 > green_lift_x5_10) {red_lift_x5_10} else {green_lift_x5_10}),round))
    )))
  
  
  
  pp5b = as.grob(expression(barplot(as.matrix(x_5_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc5, which = "discretized:breaks")[1],attr(x = irisDisc5, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_5_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[2],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[3],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[4],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[5],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[6],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[7],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[8],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[9],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[10],1)),
                              paste0(round(attr(x = irisDisc5, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc5, which = "discretized:breaks")[11],1))
                            )),
                            y= (summary(irisDisc5))+20, labels=as.character((summary(irisDisc5))))
                            
  ))
  
  
  num5 = grid.arrange(grobs=list(as.ggplot(pp5a),as.ggplot(pp5b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x6_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw))))
  red_perc_x6_1 = 1 - green_perc_x6_1
  
  green_perc_x6_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw))))
  red_perc_x6_2 = 1 - green_perc_x6_2
  
  green_perc_x6_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw))))
  red_perc_x6_3 = 1 - green_perc_x6_3
  
  green_perc_x6_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw))))
  red_perc_x6_4 = 1 - green_perc_x6_4
  
  green_perc_x6_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw))))
  red_perc_x6_5 = 1 - green_perc_x6_5
  
  green_perc_x6_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw))))
  red_perc_x6_6 = 1 - green_perc_x6_6
  
  green_perc_x6_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw))))
  red_perc_x6_7 = 1 - green_perc_x6_7
  
  green_perc_x6_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw))))
  red_perc_x6_8 = 1 - green_perc_x6_8
  
  green_perc_x6_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw))))
  red_perc_x6_9 = 1 - green_perc_x6_9
  
  green_perc_x6_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw))))
  red_perc_x6_10 = 1 - green_perc_x6_10
  
  
  
  { red_lift_x6_1 <<- 100*(red_perc_x6_1 - red_gen) / red_gen };    {green_lift_x6_1 <<- 100*(green_perc_x6_1 - green_gen) / green_gen} ; if  (red_perc_x6_1 == green_perc_x6_1) {red_lift_x6_1 = 0}; if  (red_perc_x6_1 == green_perc_x6_1) {green_lift_x6_1 = 0};
  { red_lift_x6_2 <<- 100*(red_perc_x6_2 - red_gen) / red_gen };   {green_lift_x6_2 <<- 100*(green_perc_x6_2 - green_gen) / green_gen} ; if  (red_perc_x6_2 == green_perc_x6_2) {red_lift_x6_2 = 0}; if  (red_perc_x6_2 == green_perc_x6_2) {green_lift_x6_2 = 0};
  { red_lift_x6_3 <<- 100*(red_perc_x6_3 - red_gen) / red_gen };   {green_lift_x6_3 <<- 100*(green_perc_x6_3 - green_gen) / green_gen} ; if  (red_perc_x6_3 == green_perc_x6_3) {red_lift_x6_3 = 0}; if  (red_perc_x6_3 == green_perc_x6_3) {green_lift_x6_3 = 0};
  { red_lift_x6_4 <<- 100*(red_perc_x6_4 - red_gen) / red_gen };    {green_lift_x6_4 <<- 100*(green_perc_x6_4 - green_gen) / green_gen} ; if  (red_perc_x6_4 == green_perc_x6_4) {red_lift_x6_4 = 0}; if  (red_perc_x6_4 == green_perc_x6_4) {green_lift_x6_4 = 0};
  { red_lift_x6_5 <<- 100*(red_perc_x6_5 - red_gen) / red_gen };    {green_lift_x6_5 <<- 100*(green_perc_x6_5 - green_gen) / green_gen} ; if  (red_perc_x6_5 == green_perc_x6_5) {red_lift_x6_5 = 0}; if  (red_perc_x6_5 == green_perc_x6_5) {green_lift_x6_5 = 0};
  { red_lift_x6_6 <<- 100*(red_perc_x6_6 - red_gen) / red_gen };    {green_lift_x6_6 <<- 100*(green_perc_x6_6 - green_gen) / green_gen} ; if  (red_perc_x6_6 == green_perc_x6_6) {red_lift_x6_6 = 0}; if  (red_perc_x6_6 == green_perc_x6_6) {green_lift_x6_6 = 0};
  { red_lift_x6_7 <<- 100*(red_perc_x6_7 - red_gen) / red_gen };   {green_lift_x6_7 <<- 100*(green_perc_x6_7 - green_gen) / green_gen} ; if  (red_perc_x6_7 == green_perc_x6_7) {red_lift_x6_7 = 0}; if  (red_perc_x6_7 == green_perc_x6_7) {green_lift_x6_7 = 0};
  { red_lift_x6_8 <<- 100*(red_perc_x6_8 - red_gen) / red_gen };   {green_lift_x6_8 <<- 100*(green_perc_x6_8 - green_gen) / green_gen} ; if  (red_perc_x6_8 == green_perc_x6_8) {red_lift_x6_8 = 0}; if  (red_perc_x6_8 == green_perc_x6_8) {green_lift_x6_8 = 0};
  { red_lift_x6_9 <<- 100*(red_perc_x6_9 - red_gen) / red_gen };   {green_lift_x6_9 <<- 100*(green_perc_x6_9 - green_gen) / green_gen} ; if  (red_perc_x6_9 == green_perc_x6_9) {red_lift_x6_9 = 0}; if  (red_perc_x6_9 == green_perc_x6_9) {green_lift_x6_9 = 0};
  { red_lift_x6_10 <<- 100*(red_perc_x6_10 - red_gen) / red_gen };   {green_lift_x6_10 <<- 100*(green_perc_x6_10 - green_gen) / green_gen} ; if  (red_perc_x6_10 == green_perc_x6_10) {red_lift_x6_10 = 0}; if  (red_perc_x6_10 == green_perc_x6_10) {green_lift_x6_10 = 0};
  
  
  FP_6 = (
    coalesce( if (f(green_lift_x6_1 > red_lift_x6_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x6_2 > red_lift_x6_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_3 > red_lift_x6_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_4 > red_lift_x6_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_5 > red_lift_x6_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_6 > red_lift_x6_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_7 > red_lift_x6_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_8 > red_lift_x6_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x6_9 > red_lift_x6_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_10 > red_lift_x6_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_6 = (
    coalesce( if (f(green_lift_x6_1 < red_lift_x6_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x6_2 < red_lift_x6_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_3 < red_lift_x6_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_4 < red_lift_x6_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_5 < red_lift_x6_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_6 < red_lift_x6_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_7 < red_lift_x6_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_8 < red_lift_x6_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x6_9 < red_lift_x6_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_10 < red_lift_x6_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_6 = (
    coalesce( if (f(green_lift_x6_1 > red_lift_x6_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x6_2 > red_lift_x6_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_3 > red_lift_x6_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_4 > red_lift_x6_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_5 > red_lift_x6_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_6 > red_lift_x6_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_7 > red_lift_x6_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_8 > red_lift_x6_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x6_9 > red_lift_x6_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_10 > red_lift_x6_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_6 = (
    coalesce( if (f(green_lift_x6_1 < red_lift_x6_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x6_2 < red_lift_x6_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_3 < red_lift_x6_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_4 < red_lift_x6_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_5 < red_lift_x6_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_6 < red_lift_x6_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_7 < red_lift_x6_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_8 < red_lift_x6_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x6_9 < red_lift_x6_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x6_10 < red_lift_x6_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc6 == "x6_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_6_table <<- as.data.frame.matrix(rbind(c(green_perc_x6_1 * summary(irisDisc6)[1], green_perc_x6_2 * summary(irisDisc6)[2],green_perc_x6_3 * summary(irisDisc6)[3],
                                             green_perc_x6_4 * summary(irisDisc6)[4], green_perc_x6_5 * summary(irisDisc6)[5],green_perc_x6_6 * summary(irisDisc6)[6],
                                             green_perc_x6_7 * summary(irisDisc6)[7], green_perc_x6_8 * summary(irisDisc6)[8],green_perc_x6_9 * summary(irisDisc6)[9],
                                             green_perc_x6_10 * summary(irisDisc6)[10]),
                                           c(red_perc_x6_1 * summary(irisDisc6)[1], red_perc_x6_2 * summary(irisDisc6)[2],red_perc_x6_3 * summary(irisDisc6)[3],
                                             red_perc_x6_4 * summary(irisDisc6)[4], red_perc_x6_5 * summary(irisDisc6)[5],red_perc_x6_6 * summary(irisDisc6)[6],
                                             red_perc_x6_7 * summary(irisDisc6)[7], red_perc_x6_8 * summary(irisDisc6)[8],red_perc_x6_9 * summary(irisDisc6)[9],
                                             red_perc_x6_10 * summary(irisDisc6)[10])))
  
  
  
  Precision_6 = TP_6 / (TP_6 + FP_6); Precision_6 = coalesce(Precision_6, 0)
  Recall_6 = TP_6 / (TP_6 + FN_6); Recall_6 = coalesce(Recall_6, 0)
  F1_Score_6 = coalesce ( 2 * (Precision_6 * Recall_6) / (Precision_6 + Recall_6), 0)
  MCC_6 = (TP_6 * TN_6 - FP_6 * FN_6) / sqrt ( (TP_6 + FP_6) * (TP_6 + FN_6) * (TN_6 + FP_6) * (TN_6 + FN_6) )
  MCC_adjusted_6 = (MCC_6 + 1) / 2
  P_four_6 <<- (4 * TP_6 * TN_6) / (4 * TP_6 * TN_6 + (TP_6 + TN_6) * (FP_6 + FN_6)); P_four_6 = coalesce(P_four_6, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp6a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x6_1 > green_lift_x6_1) {red_lift_x6_1} else {green_lift_x6_1},
                         if (red_lift_x6_2 > green_lift_x6_2) {red_lift_x6_2} else {green_lift_x6_2},
                         if (red_lift_x6_3 > green_lift_x6_3) {red_lift_x6_3} else {green_lift_x6_3},
                         if (red_lift_x6_4 > green_lift_x6_4) {red_lift_x6_4} else {green_lift_x6_4},
                         if (red_lift_x6_5 > green_lift_x6_5) {red_lift_x6_5} else {green_lift_x6_5},
                         if (red_lift_x6_6 > green_lift_x6_6) {red_lift_x6_6} else {green_lift_x6_6},
                         if (red_lift_x6_7 > green_lift_x6_7) {red_lift_x6_7} else {green_lift_x6_7},
                         if (red_lift_x6_8 > green_lift_x6_8) {red_lift_x6_8} else {green_lift_x6_8},
                         if (red_lift_x6_9 > green_lift_x6_9) {red_lift_x6_9} else {green_lift_x6_9},
                         if (red_lift_x6_10 > green_lift_x6_10) {red_lift_x6_10} else {green_lift_x6_10}),
                       col = c(
                         if (red_lift_x6_1 > green_lift_x6_1) {"red"} else {"green"},
                         if (red_lift_x6_2 > green_lift_x6_2) {"red"} else {"green"},
                         if (red_lift_x6_3 > green_lift_x6_3) {"red"} else {"green"},
                         if (red_lift_x6_4 > green_lift_x6_4) {"red"} else {"green"},
                         if (red_lift_x6_5 > green_lift_x6_5) {"red"} else {"green"},
                         if (red_lift_x6_6 > green_lift_x6_6) {"red"} else {"green"},
                         if (red_lift_x6_7 > green_lift_x6_7) {"red"} else {"green"},
                         if (red_lift_x6_8 > green_lift_x6_8) {"red"} else {"green"},
                         if (red_lift_x6_9 > green_lift_x6_9) {"red"} else {"green"},
                         if (red_lift_x6_10 > green_lift_x6_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(6)$Tag), cex.main = 0.7,
                    c(if (red_lift_x6_1 > green_lift_x6_1) {red_lift_x6_1} else if (red_lift_x6_1 < green_lift_x6_1) {green_lift_x6_1} else {green_lift_x6_1},
                      if (red_lift_x6_2 > green_lift_x6_2) {red_lift_x6_2} else if (red_lift_x6_2 < green_lift_x6_2) {green_lift_x6_2} else {green_lift_x6_2},
                      if (red_lift_x6_3 > green_lift_x6_3) {red_lift_x6_3} else if (red_lift_x6_3 < green_lift_x6_3) {green_lift_x6_3} else {green_lift_x6_3},
                      if (red_lift_x6_4 > green_lift_x6_4) {red_lift_x6_4} else if (red_lift_x6_4 < green_lift_x6_4) {green_lift_x6_4} else {green_lift_x6_4},
                      if (red_lift_x6_5 > green_lift_x6_5) {red_lift_x6_5} else if (red_lift_x6_5 < green_lift_x6_5) {green_lift_x6_5} else {green_lift_x6_5},
                      if (red_lift_x6_6 > green_lift_x6_6) {red_lift_x6_6} else if (red_lift_x6_6 < green_lift_x6_6) {green_lift_x6_6} else {green_lift_x6_6},
                      if (red_lift_x6_7 > green_lift_x6_7) {red_lift_x6_7} else if (red_lift_x6_7 < green_lift_x6_7) {green_lift_x6_7} else {green_lift_x6_7},
                      if (red_lift_x6_8 > green_lift_x6_8) {red_lift_x6_8} else if (red_lift_x6_8 < green_lift_x6_8) {green_lift_x6_8} else {green_lift_x6_8},
                      if (red_lift_x6_9 > green_lift_x6_9) {red_lift_x6_9} else if (red_lift_x6_9 < green_lift_x6_9) {green_lift_x6_9} else {green_lift_x6_9},
                      if (red_lift_x6_10 > green_lift_x6_10) {red_lift_x6_10} else if (red_lift_x6_10 < green_lift_x6_10) {green_lift_x6_10} else {green_lift_x6_10}),
                    col = c(
                      if (red_lift_x6_1 > green_lift_x6_1) {"red"} else {"green"},
                      if (red_lift_x6_2 > green_lift_x6_2) {"red"} else {"green"},
                      if (red_lift_x6_3 > green_lift_x6_3) {"red"} else {"green"},
                      if (red_lift_x6_4 > green_lift_x6_4) {"red"} else {"green"},
                      if (red_lift_x6_5 > green_lift_x6_5) {"red"} else {"green"},
                      if (red_lift_x6_6 > green_lift_x6_6) {"red"} else {"green"},
                      if (red_lift_x6_7 > green_lift_x6_7) {"red"} else {"green"},
                      if (red_lift_x6_8 > green_lift_x6_8) {"red"} else {"green"},
                      if (red_lift_x6_9 > green_lift_x6_9) {"red"} else {"green"},
                      if (red_lift_x6_10 > green_lift_x6_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(6)$Tag), expression("-> {"), round(100*P_four_6, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x6_1 > green_lift_x6_1) {red_lift_x6_1} else {green_lift_x6_1},
          if (red_lift_x6_2 > green_lift_x6_2) {red_lift_x6_2} else {green_lift_x6_2},
          if (red_lift_x6_3 > green_lift_x6_3) {red_lift_x6_3} else {green_lift_x6_3},
          if (red_lift_x6_4 > green_lift_x6_4) {red_lift_x6_4} else {green_lift_x6_4},
          if (red_lift_x6_5 > green_lift_x6_5) {red_lift_x6_5} else {green_lift_x6_5},
          if (red_lift_x6_6 > green_lift_x6_6) {red_lift_x6_6} else {green_lift_x6_6},
          if (red_lift_x6_7 > green_lift_x6_7) {red_lift_x6_7} else {green_lift_x6_7},
          if (red_lift_x6_8 > green_lift_x6_8) {red_lift_x6_8} else {green_lift_x6_8},
          if (red_lift_x6_9 > green_lift_x6_9) {red_lift_x6_9} else {green_lift_x6_9},
          if (red_lift_x6_10 > green_lift_x6_10) {red_lift_x6_10} else {green_lift_x6_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x6_1 > green_lift_x6_1) {red_lift_x6_1} else {green_lift_x6_1},
                                 if (red_lift_x6_2 > green_lift_x6_2) {red_lift_x6_2} else {green_lift_x6_2},
                                 if (red_lift_x6_3 > green_lift_x6_3) {red_lift_x6_3} else {green_lift_x6_3},
                                 if (red_lift_x6_4 > green_lift_x6_4) {red_lift_x6_4} else {green_lift_x6_4},
                                 if (red_lift_x6_5 > green_lift_x6_5) {red_lift_x6_5} else {green_lift_x6_5},
                                 if (red_lift_x6_6 > green_lift_x6_6) {red_lift_x6_6} else {green_lift_x6_6},
                                 if (red_lift_x6_7 > green_lift_x6_7) {red_lift_x6_7} else {green_lift_x6_7},
                                 if (red_lift_x6_8 > green_lift_x6_8) {red_lift_x6_8} else {green_lift_x6_8},
                                 if (red_lift_x6_9 > green_lift_x6_9) {red_lift_x6_9} else {green_lift_x6_9},
                                 if (red_lift_x6_10 > green_lift_x6_10) {red_lift_x6_10} else {green_lift_x6_10}),round))
    )))
  
  
  
  pp6b = as.grob(expression(barplot(as.matrix(x_6_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc6, which = "discretized:breaks")[1],attr(x = irisDisc6, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_6_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[2],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[3],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[4],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[5],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[6],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[7],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[8],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[9],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[10],1)),
                              paste0(round(attr(x = irisDisc6, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc6, which = "discretized:breaks")[11],1))
                            )),
                            y= (summary(irisDisc6))+20, labels=as.character((summary(irisDisc6))))
                            
  ))
  
  
  num6 = grid.arrange(grobs=list(as.ggplot(pp6a),as.ggplot(pp6b)))
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x7_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw))))
  red_perc_x7_1 = 1 - green_perc_x7_1
  
  green_perc_x7_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw))))
  red_perc_x7_2 = 1 - green_perc_x7_2
  
  green_perc_x7_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw))))
  red_perc_x7_3 = 1 - green_perc_x7_3
  
  green_perc_x7_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw))))
  red_perc_x7_4 = 1 - green_perc_x7_4
  
  green_perc_x7_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw))))
  red_perc_x7_5 = 1 - green_perc_x7_5
  
  green_perc_x7_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw))))
  red_perc_x7_6 = 1 - green_perc_x7_6
  
  green_perc_x7_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw))))
  red_perc_x7_7 = 1 - green_perc_x7_7
  
  green_perc_x7_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw))))
  red_perc_x7_8 = 1 - green_perc_x7_8
  
  green_perc_x7_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw))))
  red_perc_x7_9 = 1 - green_perc_x7_9
  
  green_perc_x7_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw))))
  red_perc_x7_10 = 1 - green_perc_x7_10
  
  
  
  { red_lift_x7_1 <<- 100*(red_perc_x7_1 - red_gen) / red_gen };    {green_lift_x7_1 <<- 100*(green_perc_x7_1 - green_gen) / green_gen} ; if  (red_perc_x7_1 == green_perc_x7_1) {red_lift_x7_1 = 0}; if  (red_perc_x7_1 == green_perc_x7_1) {green_lift_x7_1 = 0};
  { red_lift_x7_2 <<- 100*(red_perc_x7_2 - red_gen) / red_gen };   {green_lift_x7_2 <<- 100*(green_perc_x7_2 - green_gen) / green_gen} ; if  (red_perc_x7_2 == green_perc_x7_2) {red_lift_x7_2 = 0}; if  (red_perc_x7_2 == green_perc_x7_2) {green_lift_x7_2 = 0};
  { red_lift_x7_3 <<- 100*(red_perc_x7_3 - red_gen) / red_gen };   {green_lift_x7_3 <<- 100*(green_perc_x7_3 - green_gen) / green_gen} ; if  (red_perc_x7_3 == green_perc_x7_3) {red_lift_x7_3 = 0}; if  (red_perc_x7_3 == green_perc_x7_3) {green_lift_x7_3 = 0};
  { red_lift_x7_4 <<- 100*(red_perc_x7_4 - red_gen) / red_gen };    {green_lift_x7_4 <<- 100*(green_perc_x7_4 - green_gen) / green_gen} ; if  (red_perc_x7_4 == green_perc_x7_4) {red_lift_x7_4 = 0}; if  (red_perc_x7_4 == green_perc_x7_4) {green_lift_x7_4 = 0};
  { red_lift_x7_5 <<- 100*(red_perc_x7_5 - red_gen) / red_gen };    {green_lift_x7_5 <<- 100*(green_perc_x7_5 - green_gen) / green_gen} ; if  (red_perc_x7_5 == green_perc_x7_5) {red_lift_x7_5 = 0}; if  (red_perc_x7_5 == green_perc_x7_5) {green_lift_x7_5 = 0};
  { red_lift_x7_6 <<- 100*(red_perc_x7_6 - red_gen) / red_gen };    {green_lift_x7_6 <<- 100*(green_perc_x7_6 - green_gen) / green_gen} ; if  (red_perc_x7_6 == green_perc_x7_6) {red_lift_x7_6 = 0}; if  (red_perc_x7_6 == green_perc_x7_6) {green_lift_x7_6 = 0};
  { red_lift_x7_7 <<- 100*(red_perc_x7_7 - red_gen) / red_gen };   {green_lift_x7_7 <<- 100*(green_perc_x7_7 - green_gen) / green_gen} ; if  (red_perc_x7_7 == green_perc_x7_7) {red_lift_x7_7 = 0}; if  (red_perc_x7_7 == green_perc_x7_7) {green_lift_x7_7 = 0};
  { red_lift_x7_8 <<- 100*(red_perc_x7_8 - red_gen) / red_gen };   {green_lift_x7_8 <<- 100*(green_perc_x7_8 - green_gen) / green_gen} ; if  (red_perc_x7_8 == green_perc_x7_8) {red_lift_x7_8 = 0}; if  (red_perc_x7_8 == green_perc_x7_8) {green_lift_x7_8 = 0};
  { red_lift_x7_9 <<- 100*(red_perc_x7_9 - red_gen) / red_gen };   {green_lift_x7_9 <<- 100*(green_perc_x7_9 - green_gen) / green_gen} ; if  (red_perc_x7_9 == green_perc_x7_9) {red_lift_x7_9 = 0}; if  (red_perc_x7_9 == green_perc_x7_9) {green_lift_x7_9 = 0};
  { red_lift_x7_10 <<- 100*(red_perc_x7_10 - red_gen) / red_gen };   {green_lift_x7_10 <<- 100*(green_perc_x7_10 - green_gen) / green_gen} ; if  (red_perc_x7_10 == green_perc_x7_10) {red_lift_x7_10 = 0}; if  (red_perc_x7_10 == green_perc_x7_10) {green_lift_x7_10 = 0};
  
  
  FP_7 = (
    coalesce( if (f(green_lift_x7_1 > red_lift_x7_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x7_2 > red_lift_x7_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_3 > red_lift_x7_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_4 > red_lift_x7_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_5 > red_lift_x7_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_6 > red_lift_x7_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_7 > red_lift_x7_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_8 > red_lift_x7_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x7_9 > red_lift_x7_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_10 > red_lift_x7_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_7 = (
    coalesce( if (f(green_lift_x7_1 < red_lift_x7_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x7_2 < red_lift_x7_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_3 < red_lift_x7_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_4 < red_lift_x7_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_5 < red_lift_x7_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_6 < red_lift_x7_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_7 < red_lift_x7_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_8 < red_lift_x7_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x7_9 < red_lift_x7_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_10 < red_lift_x7_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_7 = (
    coalesce( if (f(green_lift_x7_1 > red_lift_x7_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x7_2 > red_lift_x7_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_3 > red_lift_x7_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_4 > red_lift_x7_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_5 > red_lift_x7_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_6 > red_lift_x7_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_7 > red_lift_x7_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_8 > red_lift_x7_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x7_9 > red_lift_x7_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_10 > red_lift_x7_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_7 = (
    coalesce( if (f(green_lift_x7_1 < red_lift_x7_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x7_2 < red_lift_x7_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_3 < red_lift_x7_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_4 < red_lift_x7_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_5 < red_lift_x7_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_6 < red_lift_x7_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_7 < red_lift_x7_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_8 < red_lift_x7_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x7_9 < red_lift_x7_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x7_10 < red_lift_x7_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc7 == "x7_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_7_table <<- as.data.frame.matrix(rbind(c(green_perc_x7_1 * summary(irisDisc7)[1], green_perc_x7_2 * summary(irisDisc7)[2],green_perc_x7_3 * summary(irisDisc7)[3],
                                             green_perc_x7_4 * summary(irisDisc7)[4], green_perc_x7_5 * summary(irisDisc7)[5],green_perc_x7_6 * summary(irisDisc7)[6],
                                             green_perc_x7_7 * summary(irisDisc7)[7], green_perc_x7_8 * summary(irisDisc7)[8],green_perc_x7_9 * summary(irisDisc7)[9],
                                             green_perc_x7_10 * summary(irisDisc7)[10]),
                                           c(red_perc_x7_1 * summary(irisDisc7)[1], red_perc_x7_2 * summary(irisDisc7)[2],red_perc_x7_3 * summary(irisDisc7)[3],
                                             red_perc_x7_4 * summary(irisDisc7)[4], red_perc_x7_5 * summary(irisDisc7)[5],red_perc_x7_6 * summary(irisDisc7)[6],
                                             red_perc_x7_7 * summary(irisDisc7)[7], red_perc_x7_8 * summary(irisDisc7)[8],red_perc_x7_9 * summary(irisDisc7)[9],
                                             red_perc_x7_10 * summary(irisDisc7)[10])))
  
  
  
  Precision_7 = TP_7 / (TP_7 + FP_7); Precision_7 = coalesce(Precision_7, 0)
  Recall_7 = TP_7 / (TP_7 + FN_7); Recall_7 = coalesce(Recall_7, 0)
  F1_Score_7 = coalesce ( 2 * (Precision_7 * Recall_7) / (Precision_7 + Recall_7), 0)
  MCC_7 = (TP_7 * TN_7 - FP_7 * FN_7) / sqrt ( (TP_7 + FP_7) * (TP_7 + FN_7) * (TN_7 + FP_7) * (TN_7 + FN_7) )
  MCC_adjusted_7 = (MCC_7 + 1) / 2
  P_four_7 <<- (4 * TP_7 * TN_7) / (4 * TP_7 * TN_7 + (TP_7 + TN_7) * (FP_7 + FN_7)); P_four_7 = coalesce(P_four_7, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp7a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x7_1 > green_lift_x7_1) {red_lift_x7_1} else {green_lift_x7_1},
                         if (red_lift_x7_2 > green_lift_x7_2) {red_lift_x7_2} else {green_lift_x7_2},
                         if (red_lift_x7_3 > green_lift_x7_3) {red_lift_x7_3} else {green_lift_x7_3},
                         if (red_lift_x7_4 > green_lift_x7_4) {red_lift_x7_4} else {green_lift_x7_4},
                         if (red_lift_x7_5 > green_lift_x7_5) {red_lift_x7_5} else {green_lift_x7_5},
                         if (red_lift_x7_6 > green_lift_x7_6) {red_lift_x7_6} else {green_lift_x7_6},
                         if (red_lift_x7_7 > green_lift_x7_7) {red_lift_x7_7} else {green_lift_x7_7},
                         if (red_lift_x7_8 > green_lift_x7_8) {red_lift_x7_8} else {green_lift_x7_8},
                         if (red_lift_x7_9 > green_lift_x7_9) {red_lift_x7_9} else {green_lift_x7_9},
                         if (red_lift_x7_10 > green_lift_x7_10) {red_lift_x7_10} else {green_lift_x7_10}),
                       col = c(
                         if (red_lift_x7_1 > green_lift_x7_1) {"red"} else {"green"},
                         if (red_lift_x7_2 > green_lift_x7_2) {"red"} else {"green"},
                         if (red_lift_x7_3 > green_lift_x7_3) {"red"} else {"green"},
                         if (red_lift_x7_4 > green_lift_x7_4) {"red"} else {"green"},
                         if (red_lift_x7_5 > green_lift_x7_5) {"red"} else {"green"},
                         if (red_lift_x7_6 > green_lift_x7_6) {"red"} else {"green"},
                         if (red_lift_x7_7 > green_lift_x7_7) {"red"} else {"green"},
                         if (red_lift_x7_8 > green_lift_x7_8) {"red"} else {"green"},
                         if (red_lift_x7_9 > green_lift_x7_9) {"red"} else {"green"},
                         if (red_lift_x7_10 > green_lift_x7_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(7)$Tag), cex.main = 0.7,
                    c(if (red_lift_x7_1 > green_lift_x7_1) {red_lift_x7_1} else if (red_lift_x7_1 < green_lift_x7_1) {green_lift_x7_1} else {green_lift_x7_1},
                      if (red_lift_x7_2 > green_lift_x7_2) {red_lift_x7_2} else if (red_lift_x7_2 < green_lift_x7_2) {green_lift_x7_2} else {green_lift_x7_2},
                      if (red_lift_x7_3 > green_lift_x7_3) {red_lift_x7_3} else if (red_lift_x7_3 < green_lift_x7_3) {green_lift_x7_3} else {green_lift_x7_3},
                      if (red_lift_x7_4 > green_lift_x7_4) {red_lift_x7_4} else if (red_lift_x7_4 < green_lift_x7_4) {green_lift_x7_4} else {green_lift_x7_4},
                      if (red_lift_x7_5 > green_lift_x7_5) {red_lift_x7_5} else if (red_lift_x7_5 < green_lift_x7_5) {green_lift_x7_5} else {green_lift_x7_5},
                      if (red_lift_x7_6 > green_lift_x7_6) {red_lift_x7_6} else if (red_lift_x7_6 < green_lift_x7_6) {green_lift_x7_6} else {green_lift_x7_6},
                      if (red_lift_x7_7 > green_lift_x7_7) {red_lift_x7_7} else if (red_lift_x7_7 < green_lift_x7_7) {green_lift_x7_7} else {green_lift_x7_7},
                      if (red_lift_x7_8 > green_lift_x7_8) {red_lift_x7_8} else if (red_lift_x7_8 < green_lift_x7_8) {green_lift_x7_8} else {green_lift_x7_8},
                      if (red_lift_x7_9 > green_lift_x7_9) {red_lift_x7_9} else if (red_lift_x7_9 < green_lift_x7_9) {green_lift_x7_9} else {green_lift_x7_9},
                      if (red_lift_x7_10 > green_lift_x7_10) {red_lift_x7_10} else if (red_lift_x7_10 < green_lift_x7_10) {green_lift_x7_10} else {green_lift_x7_10}),
                    col = c(
                      if (red_lift_x7_1 > green_lift_x7_1) {"red"} else {"green"},
                      if (red_lift_x7_2 > green_lift_x7_2) {"red"} else {"green"},
                      if (red_lift_x7_3 > green_lift_x7_3) {"red"} else {"green"},
                      if (red_lift_x7_4 > green_lift_x7_4) {"red"} else {"green"},
                      if (red_lift_x7_5 > green_lift_x7_5) {"red"} else {"green"},
                      if (red_lift_x7_6 > green_lift_x7_6) {"red"} else {"green"},
                      if (red_lift_x7_7 > green_lift_x7_7) {"red"} else {"green"},
                      if (red_lift_x7_8 > green_lift_x7_8) {"red"} else {"green"},
                      if (red_lift_x7_9 > green_lift_x7_9) {"red"} else {"green"},
                      if (red_lift_x7_10 > green_lift_x7_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(7)$Tag), expression("-> {"), round(100*P_four_7, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x7_1 > green_lift_x7_1) {red_lift_x7_1} else {green_lift_x7_1},
          if (red_lift_x7_2 > green_lift_x7_2) {red_lift_x7_2} else {green_lift_x7_2},
          if (red_lift_x7_3 > green_lift_x7_3) {red_lift_x7_3} else {green_lift_x7_3},
          if (red_lift_x7_4 > green_lift_x7_4) {red_lift_x7_4} else {green_lift_x7_4},
          if (red_lift_x7_5 > green_lift_x7_5) {red_lift_x7_5} else {green_lift_x7_5},
          if (red_lift_x7_6 > green_lift_x7_6) {red_lift_x7_6} else {green_lift_x7_6},
          if (red_lift_x7_7 > green_lift_x7_7) {red_lift_x7_7} else {green_lift_x7_7},
          if (red_lift_x7_8 > green_lift_x7_8) {red_lift_x7_8} else {green_lift_x7_8},
          if (red_lift_x7_9 > green_lift_x7_9) {red_lift_x7_9} else {green_lift_x7_9},
          if (red_lift_x7_10 > green_lift_x7_10) {red_lift_x7_10} else {green_lift_x7_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x7_1 > green_lift_x7_1) {red_lift_x7_1} else {green_lift_x7_1},
                                 if (red_lift_x7_2 > green_lift_x7_2) {red_lift_x7_2} else {green_lift_x7_2},
                                 if (red_lift_x7_3 > green_lift_x7_3) {red_lift_x7_3} else {green_lift_x7_3},
                                 if (red_lift_x7_4 > green_lift_x7_4) {red_lift_x7_4} else {green_lift_x7_4},
                                 if (red_lift_x7_5 > green_lift_x7_5) {red_lift_x7_5} else {green_lift_x7_5},
                                 if (red_lift_x7_6 > green_lift_x7_6) {red_lift_x7_6} else {green_lift_x7_6},
                                 if (red_lift_x7_7 > green_lift_x7_7) {red_lift_x7_7} else {green_lift_x7_7},
                                 if (red_lift_x7_8 > green_lift_x7_8) {red_lift_x7_8} else {green_lift_x7_8},
                                 if (red_lift_x7_9 > green_lift_x7_9) {red_lift_x7_9} else {green_lift_x7_9},
                                 if (red_lift_x7_10 > green_lift_x7_10) {red_lift_x7_10} else {green_lift_x7_10}),round))
    )))
  
  
  
  pp7b = as.grob(expression(barplot(as.matrix(x_7_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc7, which = "discretized:breaks")[1],attr(x = irisDisc7, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_7_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[2],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[3],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[4],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[5],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[6],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[7],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[8],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[9],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[10],1)),
                              paste0(round(attr(x = irisDisc7, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc7, which = "discretized:breaks")[11],1))
                            )),
                            y= (summary(irisDisc7))+20, labels=as.character((summary(irisDisc7))))
                            
  ))
  
  
  num7 = grid.arrange(grobs=list(as.ggplot(pp7a),as.ggplot(pp7b)))
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x8_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw))))
  red_perc_x8_1 = 1 - green_perc_x8_1
  
  green_perc_x8_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw))))
  red_perc_x8_2 = 1 - green_perc_x8_2
  
  green_perc_x8_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw))))
  red_perc_x8_3 = 1 - green_perc_x8_3
  
  green_perc_x8_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw))))
  red_perc_x8_4 = 1 - green_perc_x8_4
  
  green_perc_x8_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw))))
  red_perc_x8_5 = 1 - green_perc_x8_5
  
  green_perc_x8_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw))))
  red_perc_x8_6 = 1 - green_perc_x8_6
  
  green_perc_x8_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw))))
  red_perc_x8_7 = 1 - green_perc_x8_7
  
  green_perc_x8_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw))))
  red_perc_x8_8 = 1 - green_perc_x8_8
  
  green_perc_x8_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw))))
  red_perc_x8_9 = 1 - green_perc_x8_9
  
  green_perc_x8_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw))))
  red_perc_x8_10 = 1 - green_perc_x8_10
  
  
  
  { red_lift_x8_1 <<- 100*(red_perc_x8_1 - red_gen) / red_gen };    {green_lift_x8_1 <<- 100*(green_perc_x8_1 - green_gen) / green_gen} ; if  (red_perc_x8_1 == green_perc_x8_1) {red_lift_x8_1 = 0}; if  (red_perc_x8_1 == green_perc_x8_1) {green_lift_x8_1 = 0};
  { red_lift_x8_2 <<- 100*(red_perc_x8_2 - red_gen) / red_gen };   {green_lift_x8_2 <<- 100*(green_perc_x8_2 - green_gen) / green_gen} ; if  (red_perc_x8_2 == green_perc_x8_2) {red_lift_x8_2 = 0}; if  (red_perc_x8_2 == green_perc_x8_2) {green_lift_x8_2 = 0};
  { red_lift_x8_3 <<- 100*(red_perc_x8_3 - red_gen) / red_gen };   {green_lift_x8_3 <<- 100*(green_perc_x8_3 - green_gen) / green_gen} ; if  (red_perc_x8_3 == green_perc_x8_3) {red_lift_x8_3 = 0}; if  (red_perc_x8_3 == green_perc_x8_3) {green_lift_x8_3 = 0};
  { red_lift_x8_4 <<- 100*(red_perc_x8_4 - red_gen) / red_gen };    {green_lift_x8_4 <<- 100*(green_perc_x8_4 - green_gen) / green_gen} ; if  (red_perc_x8_4 == green_perc_x8_4) {red_lift_x8_4 = 0}; if  (red_perc_x8_4 == green_perc_x8_4) {green_lift_x8_4 = 0};
  { red_lift_x8_5 <<- 100*(red_perc_x8_5 - red_gen) / red_gen };    {green_lift_x8_5 <<- 100*(green_perc_x8_5 - green_gen) / green_gen} ; if  (red_perc_x8_5 == green_perc_x8_5) {red_lift_x8_5 = 0}; if  (red_perc_x8_5 == green_perc_x8_5) {green_lift_x8_5 = 0};
  { red_lift_x8_6 <<- 100*(red_perc_x8_6 - red_gen) / red_gen };    {green_lift_x8_6 <<- 100*(green_perc_x8_6 - green_gen) / green_gen} ; if  (red_perc_x8_6 == green_perc_x8_6) {red_lift_x8_6 = 0}; if  (red_perc_x8_6 == green_perc_x8_6) {green_lift_x8_6 = 0};
  { red_lift_x8_7 <<- 100*(red_perc_x8_7 - red_gen) / red_gen };   {green_lift_x8_7 <<- 100*(green_perc_x8_7 - green_gen) / green_gen} ; if  (red_perc_x8_7 == green_perc_x8_7) {red_lift_x8_7 = 0}; if  (red_perc_x8_7 == green_perc_x8_7) {green_lift_x8_7 = 0};
  { red_lift_x8_8 <<- 100*(red_perc_x8_8 - red_gen) / red_gen };   {green_lift_x8_8 <<- 100*(green_perc_x8_8 - green_gen) / green_gen} ; if  (red_perc_x8_8 == green_perc_x8_8) {red_lift_x8_8 = 0}; if  (red_perc_x8_8 == green_perc_x8_8) {green_lift_x8_8 = 0};
  { red_lift_x8_9 <<- 100*(red_perc_x8_9 - red_gen) / red_gen };   {green_lift_x8_9 <<- 100*(green_perc_x8_9 - green_gen) / green_gen} ; if  (red_perc_x8_9 == green_perc_x8_9) {red_lift_x8_9 = 0}; if  (red_perc_x8_9 == green_perc_x8_9) {green_lift_x8_9 = 0};
  { red_lift_x8_10 <<- 100*(red_perc_x8_10 - red_gen) / red_gen };   {green_lift_x8_10 <<- 100*(green_perc_x8_10 - green_gen) / green_gen} ; if  (red_perc_x8_10 == green_perc_x8_10) {red_lift_x8_10 = 0}; if  (red_perc_x8_10 == green_perc_x8_10) {green_lift_x8_10 = 0};
  
  
  FP_8 = (
    coalesce( if (f(green_lift_x8_1 > red_lift_x8_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x8_2 > red_lift_x8_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_3 > red_lift_x8_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_4 > red_lift_x8_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_5 > red_lift_x8_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_6 > red_lift_x8_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_7 > red_lift_x8_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_8 > red_lift_x8_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x8_9 > red_lift_x8_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_10 > red_lift_x8_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_8 = (
    coalesce( if (f(green_lift_x8_1 < red_lift_x8_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x8_2 < red_lift_x8_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_3 < red_lift_x8_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_4 < red_lift_x8_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_5 < red_lift_x8_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_6 < red_lift_x8_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_7 < red_lift_x8_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_8 < red_lift_x8_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x8_9 < red_lift_x8_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_10 < red_lift_x8_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_8 = (
    coalesce( if (f(green_lift_x8_1 > red_lift_x8_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x8_2 > red_lift_x8_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_3 > red_lift_x8_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_4 > red_lift_x8_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_5 > red_lift_x8_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_6 > red_lift_x8_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_7 > red_lift_x8_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_8 > red_lift_x8_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x8_9 > red_lift_x8_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_10 > red_lift_x8_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_8 = (
    coalesce( if (f(green_lift_x8_1 < red_lift_x8_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x8_2 < red_lift_x8_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_3 < red_lift_x8_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_4 < red_lift_x8_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_5 < red_lift_x8_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_6 < red_lift_x8_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_7 < red_lift_x8_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_8 < red_lift_x8_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x8_9 < red_lift_x8_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x8_10 < red_lift_x8_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc8 == "x8_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_8_table <<- as.data.frame.matrix(rbind(c(green_perc_x8_1 * summary(irisDisc8)[1], green_perc_x8_2 * summary(irisDisc8)[2],green_perc_x8_3 * summary(irisDisc8)[3],
                                             green_perc_x8_4 * summary(irisDisc8)[4], green_perc_x8_5 * summary(irisDisc8)[5],green_perc_x8_6 * summary(irisDisc8)[6],
                                             green_perc_x8_7 * summary(irisDisc8)[7], green_perc_x8_8 * summary(irisDisc8)[8],green_perc_x8_9 * summary(irisDisc8)[9],
                                             green_perc_x8_10 * summary(irisDisc8)[10]),
                                           c(red_perc_x8_1 * summary(irisDisc8)[1], red_perc_x8_2 * summary(irisDisc8)[2],red_perc_x8_3 * summary(irisDisc8)[3],
                                             red_perc_x8_4 * summary(irisDisc8)[4], red_perc_x8_5 * summary(irisDisc8)[5],red_perc_x8_6 * summary(irisDisc8)[6],
                                             red_perc_x8_7 * summary(irisDisc8)[7], red_perc_x8_8 * summary(irisDisc8)[8],red_perc_x8_9 * summary(irisDisc8)[9],
                                             red_perc_x8_10 * summary(irisDisc8)[10])))
  
  
  
  Precision_8 = TP_8 / (TP_8 + FP_8); Precision_8 = coalesce(Precision_8, 0)
  Recall_8 = TP_8 / (TP_8 + FN_8); Recall_8 = coalesce(Recall_8, 0)
  F1_Score_8 = coalesce ( 2 * (Precision_8 * Recall_8) / (Precision_8 + Recall_8), 0)
  MCC_8 = (TP_8 * TN_8 - FP_8 * FN_8) / sqrt ( (TP_8 + FP_8) * (TP_8 + FN_8) * (TN_8 + FP_8) * (TN_8 + FN_8) )
  MCC_adjusted_8 = (MCC_8 + 1) / 2
  P_four_8 <<- (4 * TP_8 * TN_8) / (4 * TP_8 * TN_8 + (TP_8 + TN_8) * (FP_8 + FN_8)); P_four_8 = coalesce(P_four_8, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp8a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x8_1 > green_lift_x8_1) {red_lift_x8_1} else {green_lift_x8_1},
                         if (red_lift_x8_2 > green_lift_x8_2) {red_lift_x8_2} else {green_lift_x8_2},
                         if (red_lift_x8_3 > green_lift_x8_3) {red_lift_x8_3} else {green_lift_x8_3},
                         if (red_lift_x8_4 > green_lift_x8_4) {red_lift_x8_4} else {green_lift_x8_4},
                         if (red_lift_x8_5 > green_lift_x8_5) {red_lift_x8_5} else {green_lift_x8_5},
                         if (red_lift_x8_6 > green_lift_x8_6) {red_lift_x8_6} else {green_lift_x8_6},
                         if (red_lift_x8_7 > green_lift_x8_7) {red_lift_x8_7} else {green_lift_x8_7},
                         if (red_lift_x8_8 > green_lift_x8_8) {red_lift_x8_8} else {green_lift_x8_8},
                         if (red_lift_x8_9 > green_lift_x8_9) {red_lift_x8_9} else {green_lift_x8_9},
                         if (red_lift_x8_10 > green_lift_x8_10) {red_lift_x8_10} else {green_lift_x8_10}),
                       col = c(
                         if (red_lift_x8_1 > green_lift_x8_1) {"red"} else {"green"},
                         if (red_lift_x8_2 > green_lift_x8_2) {"red"} else {"green"},
                         if (red_lift_x8_3 > green_lift_x8_3) {"red"} else {"green"},
                         if (red_lift_x8_4 > green_lift_x8_4) {"red"} else {"green"},
                         if (red_lift_x8_5 > green_lift_x8_5) {"red"} else {"green"},
                         if (red_lift_x8_6 > green_lift_x8_6) {"red"} else {"green"},
                         if (red_lift_x8_7 > green_lift_x8_7) {"red"} else {"green"},
                         if (red_lift_x8_8 > green_lift_x8_8) {"red"} else {"green"},
                         if (red_lift_x8_9 > green_lift_x8_9) {"red"} else {"green"},
                         if (red_lift_x8_10 > green_lift_x8_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(8)$Tag), cex.main = 0.7,
                    c(if (red_lift_x8_1 > green_lift_x8_1) {red_lift_x8_1} else if (red_lift_x8_1 < green_lift_x8_1) {green_lift_x8_1} else {green_lift_x8_1},
                      if (red_lift_x8_2 > green_lift_x8_2) {red_lift_x8_2} else if (red_lift_x8_2 < green_lift_x8_2) {green_lift_x8_2} else {green_lift_x8_2},
                      if (red_lift_x8_3 > green_lift_x8_3) {red_lift_x8_3} else if (red_lift_x8_3 < green_lift_x8_3) {green_lift_x8_3} else {green_lift_x8_3},
                      if (red_lift_x8_4 > green_lift_x8_4) {red_lift_x8_4} else if (red_lift_x8_4 < green_lift_x8_4) {green_lift_x8_4} else {green_lift_x8_4},
                      if (red_lift_x8_5 > green_lift_x8_5) {red_lift_x8_5} else if (red_lift_x8_5 < green_lift_x8_5) {green_lift_x8_5} else {green_lift_x8_5},
                      if (red_lift_x8_6 > green_lift_x8_6) {red_lift_x8_6} else if (red_lift_x8_6 < green_lift_x8_6) {green_lift_x8_6} else {green_lift_x8_6},
                      if (red_lift_x8_7 > green_lift_x8_7) {red_lift_x8_7} else if (red_lift_x8_7 < green_lift_x8_7) {green_lift_x8_7} else {green_lift_x8_7},
                      if (red_lift_x8_8 > green_lift_x8_8) {red_lift_x8_8} else if (red_lift_x8_8 < green_lift_x8_8) {green_lift_x8_8} else {green_lift_x8_8},
                      if (red_lift_x8_9 > green_lift_x8_9) {red_lift_x8_9} else if (red_lift_x8_9 < green_lift_x8_9) {green_lift_x8_9} else {green_lift_x8_9},
                      if (red_lift_x8_10 > green_lift_x8_10) {red_lift_x8_10} else if (red_lift_x8_10 < green_lift_x8_10) {green_lift_x8_10} else {green_lift_x8_10}),
                    col = c(
                      if (red_lift_x8_1 > green_lift_x8_1) {"red"} else {"green"},
                      if (red_lift_x8_2 > green_lift_x8_2) {"red"} else {"green"},
                      if (red_lift_x8_3 > green_lift_x8_3) {"red"} else {"green"},
                      if (red_lift_x8_4 > green_lift_x8_4) {"red"} else {"green"},
                      if (red_lift_x8_5 > green_lift_x8_5) {"red"} else {"green"},
                      if (red_lift_x8_6 > green_lift_x8_6) {"red"} else {"green"},
                      if (red_lift_x8_7 > green_lift_x8_7) {"red"} else {"green"},
                      if (red_lift_x8_8 > green_lift_x8_8) {"red"} else {"green"},
                      if (red_lift_x8_9 > green_lift_x8_9) {"red"} else {"green"},
                      if (red_lift_x8_10 > green_lift_x8_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(8)$Tag), expression("-> {"), round(100*P_four_8, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x8_1 > green_lift_x8_1) {red_lift_x8_1} else {green_lift_x8_1},
          if (red_lift_x8_2 > green_lift_x8_2) {red_lift_x8_2} else {green_lift_x8_2},
          if (red_lift_x8_3 > green_lift_x8_3) {red_lift_x8_3} else {green_lift_x8_3},
          if (red_lift_x8_4 > green_lift_x8_4) {red_lift_x8_4} else {green_lift_x8_4},
          if (red_lift_x8_5 > green_lift_x8_5) {red_lift_x8_5} else {green_lift_x8_5},
          if (red_lift_x8_6 > green_lift_x8_6) {red_lift_x8_6} else {green_lift_x8_6},
          if (red_lift_x8_7 > green_lift_x8_7) {red_lift_x8_7} else {green_lift_x8_7},
          if (red_lift_x8_8 > green_lift_x8_8) {red_lift_x8_8} else {green_lift_x8_8},
          if (red_lift_x8_9 > green_lift_x8_9) {red_lift_x8_9} else {green_lift_x8_9},
          if (red_lift_x8_10 > green_lift_x8_10) {red_lift_x8_10} else {green_lift_x8_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x8_1 > green_lift_x8_1) {red_lift_x8_1} else {green_lift_x8_1},
                                 if (red_lift_x8_2 > green_lift_x8_2) {red_lift_x8_2} else {green_lift_x8_2},
                                 if (red_lift_x8_3 > green_lift_x8_3) {red_lift_x8_3} else {green_lift_x8_3},
                                 if (red_lift_x8_4 > green_lift_x8_4) {red_lift_x8_4} else {green_lift_x8_4},
                                 if (red_lift_x8_5 > green_lift_x8_5) {red_lift_x8_5} else {green_lift_x8_5},
                                 if (red_lift_x8_6 > green_lift_x8_6) {red_lift_x8_6} else {green_lift_x8_6},
                                 if (red_lift_x8_7 > green_lift_x8_7) {red_lift_x8_7} else {green_lift_x8_7},
                                 if (red_lift_x8_8 > green_lift_x8_8) {red_lift_x8_8} else {green_lift_x8_8},
                                 if (red_lift_x8_9 > green_lift_x8_9) {red_lift_x8_9} else {green_lift_x8_9},
                                 if (red_lift_x8_10 > green_lift_x8_10) {red_lift_x8_10} else {green_lift_x8_10}),round))
    )))
  
  
  
  pp8b = as.grob(expression(barplot(as.matrix(x_8_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc8, which = "discretized:breaks")[1],attr(x = irisDisc8, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_8_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[2],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[3],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[4],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[5],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[6],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[7],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[8],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[9],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[10],1)),
                              paste0(round(attr(x = irisDisc8, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc8, which = "discretized:breaks")[11],1))
                            )),
                            y= (summary(irisDisc8))+20, labels=as.character((summary(irisDisc8))))
                            
  ))
  
  
  num8 = grid.arrange(grobs=list(as.ggplot(pp8a),as.ggplot(pp8b)))
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x9_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw))))
  red_perc_x9_1 = 1 - green_perc_x9_1
  
  green_perc_x9_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw))))
  red_perc_x9_2 = 1 - green_perc_x9_2
  
  green_perc_x9_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw))))
  red_perc_x9_3 = 1 - green_perc_x9_3
  
  green_perc_x9_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw))))
  red_perc_x9_4 = 1 - green_perc_x9_4
  
  green_perc_x9_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw))))
  red_perc_x9_5 = 1 - green_perc_x9_5
  
  green_perc_x9_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw))))
  red_perc_x9_6 = 1 - green_perc_x9_6
  
  green_perc_x9_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw))))
  red_perc_x9_7 = 1 - green_perc_x9_7
  
  green_perc_x9_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw))))
  red_perc_x9_8 = 1 - green_perc_x9_8
  
  green_perc_x9_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw))))
  red_perc_x9_9 = 1 - green_perc_x9_9
  
  green_perc_x9_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw))))
  red_perc_x9_10 = 1 - green_perc_x9_10
  
  
  
  { red_lift_x9_1 <<- 100*(red_perc_x9_1 - red_gen) / red_gen };    {green_lift_x9_1 <<- 100*(green_perc_x9_1 - green_gen) / green_gen} ; if  (red_perc_x9_1 == green_perc_x9_1) {red_lift_x9_1 = 0}; if  (red_perc_x9_1 == green_perc_x9_1) {green_lift_x9_1 = 0};
  { red_lift_x9_2 <<- 100*(red_perc_x9_2 - red_gen) / red_gen };   {green_lift_x9_2 <<- 100*(green_perc_x9_2 - green_gen) / green_gen} ; if  (red_perc_x9_2 == green_perc_x9_2) {red_lift_x9_2 = 0}; if  (red_perc_x9_2 == green_perc_x9_2) {green_lift_x9_2 = 0};
  { red_lift_x9_3 <<- 100*(red_perc_x9_3 - red_gen) / red_gen };   {green_lift_x9_3 <<- 100*(green_perc_x9_3 - green_gen) / green_gen} ; if  (red_perc_x9_3 == green_perc_x9_3) {red_lift_x9_3 = 0}; if  (red_perc_x9_3 == green_perc_x9_3) {green_lift_x9_3 = 0};
  { red_lift_x9_4 <<- 100*(red_perc_x9_4 - red_gen) / red_gen };    {green_lift_x9_4 <<- 100*(green_perc_x9_4 - green_gen) / green_gen} ; if  (red_perc_x9_4 == green_perc_x9_4) {red_lift_x9_4 = 0}; if  (red_perc_x9_4 == green_perc_x9_4) {green_lift_x9_4 = 0};
  { red_lift_x9_5 <<- 100*(red_perc_x9_5 - red_gen) / red_gen };    {green_lift_x9_5 <<- 100*(green_perc_x9_5 - green_gen) / green_gen} ; if  (red_perc_x9_5 == green_perc_x9_5) {red_lift_x9_5 = 0}; if  (red_perc_x9_5 == green_perc_x9_5) {green_lift_x9_5 = 0};
  { red_lift_x9_6 <<- 100*(red_perc_x9_6 - red_gen) / red_gen };    {green_lift_x9_6 <<- 100*(green_perc_x9_6 - green_gen) / green_gen} ; if  (red_perc_x9_6 == green_perc_x9_6) {red_lift_x9_6 = 0}; if  (red_perc_x9_6 == green_perc_x9_6) {green_lift_x9_6 = 0};
  { red_lift_x9_7 <<- 100*(red_perc_x9_7 - red_gen) / red_gen };   {green_lift_x9_7 <<- 100*(green_perc_x9_7 - green_gen) / green_gen} ; if  (red_perc_x9_7 == green_perc_x9_7) {red_lift_x9_7 = 0}; if  (red_perc_x9_7 == green_perc_x9_7) {green_lift_x9_7 = 0};
  { red_lift_x9_8 <<- 100*(red_perc_x9_8 - red_gen) / red_gen };   {green_lift_x9_8 <<- 100*(green_perc_x9_8 - green_gen) / green_gen} ; if  (red_perc_x9_8 == green_perc_x9_8) {red_lift_x9_8 = 0}; if  (red_perc_x9_8 == green_perc_x9_8) {green_lift_x9_8 = 0};
  { red_lift_x9_9 <<- 100*(red_perc_x9_9 - red_gen) / red_gen };   {green_lift_x9_9 <<- 100*(green_perc_x9_9 - green_gen) / green_gen} ; if  (red_perc_x9_9 == green_perc_x9_9) {red_lift_x9_9 = 0}; if  (red_perc_x9_9 == green_perc_x9_9) {green_lift_x9_9 = 0};
  { red_lift_x9_10 <<- 100*(red_perc_x9_10 - red_gen) / red_gen };   {green_lift_x9_10 <<- 100*(green_perc_x9_10 - green_gen) / green_gen} ; if  (red_perc_x9_10 == green_perc_x9_10) {red_lift_x9_10 = 0}; if  (red_perc_x9_10 == green_perc_x9_10) {green_lift_x9_10 = 0};
  
  
  FP_9 = (
    coalesce( if (f(green_lift_x9_1 > red_lift_x9_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x9_2 > red_lift_x9_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_3 > red_lift_x9_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_4 > red_lift_x9_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_5 > red_lift_x9_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_6 > red_lift_x9_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_7 > red_lift_x9_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_8 > red_lift_x9_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x9_9 > red_lift_x9_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_10 > red_lift_x9_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_9 = (
    coalesce( if (f(green_lift_x9_1 < red_lift_x9_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x9_2 < red_lift_x9_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_3 < red_lift_x9_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_4 < red_lift_x9_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_5 < red_lift_x9_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_6 < red_lift_x9_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_7 < red_lift_x9_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_8 < red_lift_x9_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x9_9 < red_lift_x9_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_10 < red_lift_x9_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_9 = (
    coalesce( if (f(green_lift_x9_1 > red_lift_x9_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x9_2 > red_lift_x9_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_3 > red_lift_x9_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_4 > red_lift_x9_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_5 > red_lift_x9_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_6 > red_lift_x9_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_7 > red_lift_x9_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_8 > red_lift_x9_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x9_9 > red_lift_x9_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_10 > red_lift_x9_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_9 = (
    coalesce( if (f(green_lift_x9_1 < red_lift_x9_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x9_2 < red_lift_x9_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_3 < red_lift_x9_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_4 < red_lift_x9_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_5 < red_lift_x9_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_6 < red_lift_x9_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_7 < red_lift_x9_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_8 < red_lift_x9_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x9_9 < red_lift_x9_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x9_10 < red_lift_x9_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc9 == "x9_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_9_table <<- as.data.frame.matrix(rbind(c(green_perc_x9_1 * summary(irisDisc9)[1], green_perc_x9_2 * summary(irisDisc9)[2],green_perc_x9_3 * summary(irisDisc9)[3],
                                             green_perc_x9_4 * summary(irisDisc9)[4], green_perc_x9_5 * summary(irisDisc9)[5],green_perc_x9_6 * summary(irisDisc9)[6],
                                             green_perc_x9_7 * summary(irisDisc9)[7], green_perc_x9_8 * summary(irisDisc9)[8],green_perc_x9_9 * summary(irisDisc9)[9],
                                             green_perc_x9_10 * summary(irisDisc9)[10]),
                                           c(red_perc_x9_1 * summary(irisDisc9)[1], red_perc_x9_2 * summary(irisDisc9)[2],red_perc_x9_3 * summary(irisDisc9)[3],
                                             red_perc_x9_4 * summary(irisDisc9)[4], red_perc_x9_5 * summary(irisDisc9)[5],red_perc_x9_6 * summary(irisDisc9)[6],
                                             red_perc_x9_7 * summary(irisDisc9)[7], red_perc_x9_8 * summary(irisDisc9)[8],red_perc_x9_9 * summary(irisDisc9)[9],
                                             red_perc_x9_10 * summary(irisDisc9)[10])))
  
  
  
  Precision_9 = TP_9 / (TP_9 + FP_9); Precision_9 = coalesce(Precision_9, 0)
  Recall_9 = TP_9 / (TP_9 + FN_9); Recall_9 = coalesce(Recall_9, 0)
  F1_Score_9 = coalesce ( 2 * (Precision_9 * Recall_9) / (Precision_9 + Recall_9), 0)
  MCC_9 = (TP_9 * TN_9 - FP_9 * FN_9) / sqrt ( (TP_9 + FP_9) * (TP_9 + FN_9) * (TN_9 + FP_9) * (TN_9 + FN_9) )
  MCC_adjusted_9 = (MCC_9 + 1) / 2
  P_four_9 <<- (4 * TP_9 * TN_9) / (4 * TP_9 * TN_9 + (TP_9 + TN_9) * (FP_9 + FN_9)); P_four_9 = coalesce(P_four_9, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp9a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x9_1 > green_lift_x9_1) {red_lift_x9_1} else {green_lift_x9_1},
                         if (red_lift_x9_2 > green_lift_x9_2) {red_lift_x9_2} else {green_lift_x9_2},
                         if (red_lift_x9_3 > green_lift_x9_3) {red_lift_x9_3} else {green_lift_x9_3},
                         if (red_lift_x9_4 > green_lift_x9_4) {red_lift_x9_4} else {green_lift_x9_4},
                         if (red_lift_x9_5 > green_lift_x9_5) {red_lift_x9_5} else {green_lift_x9_5},
                         if (red_lift_x9_6 > green_lift_x9_6) {red_lift_x9_6} else {green_lift_x9_6},
                         if (red_lift_x9_7 > green_lift_x9_7) {red_lift_x9_7} else {green_lift_x9_7},
                         if (red_lift_x9_8 > green_lift_x9_8) {red_lift_x9_8} else {green_lift_x9_8},
                         if (red_lift_x9_9 > green_lift_x9_9) {red_lift_x9_9} else {green_lift_x9_9},
                         if (red_lift_x9_10 > green_lift_x9_10) {red_lift_x9_10} else {green_lift_x9_10}),
                       col = c(
                         if (red_lift_x9_1 > green_lift_x9_1) {"red"} else {"green"},
                         if (red_lift_x9_2 > green_lift_x9_2) {"red"} else {"green"},
                         if (red_lift_x9_3 > green_lift_x9_3) {"red"} else {"green"},
                         if (red_lift_x9_4 > green_lift_x9_4) {"red"} else {"green"},
                         if (red_lift_x9_5 > green_lift_x9_5) {"red"} else {"green"},
                         if (red_lift_x9_6 > green_lift_x9_6) {"red"} else {"green"},
                         if (red_lift_x9_7 > green_lift_x9_7) {"red"} else {"green"},
                         if (red_lift_x9_8 > green_lift_x9_8) {"red"} else {"green"},
                         if (red_lift_x9_9 > green_lift_x9_9) {"red"} else {"green"},
                         if (red_lift_x9_10 > green_lift_x9_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(9)$Tag), cex.main = 0.7,
                    c(if (red_lift_x9_1 > green_lift_x9_1) {red_lift_x9_1} else if (red_lift_x9_1 < green_lift_x9_1) {green_lift_x9_1} else {green_lift_x9_1},
                      if (red_lift_x9_2 > green_lift_x9_2) {red_lift_x9_2} else if (red_lift_x9_2 < green_lift_x9_2) {green_lift_x9_2} else {green_lift_x9_2},
                      if (red_lift_x9_3 > green_lift_x9_3) {red_lift_x9_3} else if (red_lift_x9_3 < green_lift_x9_3) {green_lift_x9_3} else {green_lift_x9_3},
                      if (red_lift_x9_4 > green_lift_x9_4) {red_lift_x9_4} else if (red_lift_x9_4 < green_lift_x9_4) {green_lift_x9_4} else {green_lift_x9_4},
                      if (red_lift_x9_5 > green_lift_x9_5) {red_lift_x9_5} else if (red_lift_x9_5 < green_lift_x9_5) {green_lift_x9_5} else {green_lift_x9_5},
                      if (red_lift_x9_6 > green_lift_x9_6) {red_lift_x9_6} else if (red_lift_x9_6 < green_lift_x9_6) {green_lift_x9_6} else {green_lift_x9_6},
                      if (red_lift_x9_7 > green_lift_x9_7) {red_lift_x9_7} else if (red_lift_x9_7 < green_lift_x9_7) {green_lift_x9_7} else {green_lift_x9_7},
                      if (red_lift_x9_8 > green_lift_x9_8) {red_lift_x9_8} else if (red_lift_x9_8 < green_lift_x9_8) {green_lift_x9_8} else {green_lift_x9_8},
                      if (red_lift_x9_9 > green_lift_x9_9) {red_lift_x9_9} else if (red_lift_x9_9 < green_lift_x9_9) {green_lift_x9_9} else {green_lift_x9_9},
                      if (red_lift_x9_10 > green_lift_x9_10) {red_lift_x9_10} else if (red_lift_x9_10 < green_lift_x9_10) {green_lift_x9_10} else {green_lift_x9_10}),
                    col = c(
                      if (red_lift_x9_1 > green_lift_x9_1) {"red"} else {"green"},
                      if (red_lift_x9_2 > green_lift_x9_2) {"red"} else {"green"},
                      if (red_lift_x9_3 > green_lift_x9_3) {"red"} else {"green"},
                      if (red_lift_x9_4 > green_lift_x9_4) {"red"} else {"green"},
                      if (red_lift_x9_5 > green_lift_x9_5) {"red"} else {"green"},
                      if (red_lift_x9_6 > green_lift_x9_6) {"red"} else {"green"},
                      if (red_lift_x9_7 > green_lift_x9_7) {"red"} else {"green"},
                      if (red_lift_x9_8 > green_lift_x9_8) {"red"} else {"green"},
                      if (red_lift_x9_9 > green_lift_x9_9) {"red"} else {"green"},
                      if (red_lift_x9_10 > green_lift_x9_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(9)$Tag), expression("-> {"), round(100*P_four_9, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x9_1 > green_lift_x9_1) {red_lift_x9_1} else {green_lift_x9_1},
          if (red_lift_x9_2 > green_lift_x9_2) {red_lift_x9_2} else {green_lift_x9_2},
          if (red_lift_x9_3 > green_lift_x9_3) {red_lift_x9_3} else {green_lift_x9_3},
          if (red_lift_x9_4 > green_lift_x9_4) {red_lift_x9_4} else {green_lift_x9_4},
          if (red_lift_x9_5 > green_lift_x9_5) {red_lift_x9_5} else {green_lift_x9_5},
          if (red_lift_x9_6 > green_lift_x9_6) {red_lift_x9_6} else {green_lift_x9_6},
          if (red_lift_x9_7 > green_lift_x9_7) {red_lift_x9_7} else {green_lift_x9_7},
          if (red_lift_x9_8 > green_lift_x9_8) {red_lift_x9_8} else {green_lift_x9_8},
          if (red_lift_x9_9 > green_lift_x9_9) {red_lift_x9_9} else {green_lift_x9_9},
          if (red_lift_x9_10 > green_lift_x9_10) {red_lift_x9_10} else {green_lift_x9_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x9_1 > green_lift_x9_1) {red_lift_x9_1} else {green_lift_x9_1},
                                 if (red_lift_x9_2 > green_lift_x9_2) {red_lift_x9_2} else {green_lift_x9_2},
                                 if (red_lift_x9_3 > green_lift_x9_3) {red_lift_x9_3} else {green_lift_x9_3},
                                 if (red_lift_x9_4 > green_lift_x9_4) {red_lift_x9_4} else {green_lift_x9_4},
                                 if (red_lift_x9_5 > green_lift_x9_5) {red_lift_x9_5} else {green_lift_x9_5},
                                 if (red_lift_x9_6 > green_lift_x9_6) {red_lift_x9_6} else {green_lift_x9_6},
                                 if (red_lift_x9_7 > green_lift_x9_7) {red_lift_x9_7} else {green_lift_x9_7},
                                 if (red_lift_x9_8 > green_lift_x9_8) {red_lift_x9_8} else {green_lift_x9_8},
                                 if (red_lift_x9_9 > green_lift_x9_9) {red_lift_x9_9} else {green_lift_x9_9},
                                 if (red_lift_x9_10 > green_lift_x9_10) {red_lift_x9_10} else {green_lift_x9_10}),round))
    )))
  
  
  
  pp9b = as.grob(expression(barplot(as.matrix(x_9_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc9, which = "discretized:breaks")[1],attr(x = irisDisc9, which = "discretized:breaks")[11])),
                            text(x=barplot(as.matrix(x_9_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[2],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[3],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[4],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[5],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[6],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[7],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[8],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[9],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[10],1)),
                              paste0(round(attr(x = irisDisc9, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc9, which = "discretized:breaks")[11],1))
                            )),
                            y= (summary(irisDisc9))+20, labels=as.character((summary(irisDisc9))))
                            
  ))
  
  
  num9 = grid.arrange(grobs=list(as.ggplot(pp9a),as.ggplot(pp9b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x10_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw))))
  red_perc_x10_1 = 1 - green_perc_x10_1
  
  green_perc_x10_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw))))
  red_perc_x10_2 = 1 - green_perc_x10_2
  
  green_perc_x10_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw))))
  red_perc_x10_3 = 1 - green_perc_x10_3
  
  green_perc_x10_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw))))
  red_perc_x10_4 = 1 - green_perc_x10_4
  
  green_perc_x10_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw))))
  red_perc_x10_5 = 1 - green_perc_x10_5
  
  green_perc_x10_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw))))
  red_perc_x10_6 = 1 - green_perc_x10_6
  
  green_perc_x10_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw))))
  red_perc_x10_7 = 1 - green_perc_x10_7
  
  green_perc_x10_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw))))
  red_perc_x10_8 = 1 - green_perc_x10_8
  
  green_perc_x10_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw))))
  red_perc_x10_9 = 1 - green_perc_x10_9
  
  green_perc_x10_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw))))
  red_perc_x10_10 = 1 - green_perc_x10_10
  
  
  
  { red_lift_x10_1 <<- 100*(red_perc_x10_1 - red_gen) / red_gen };    {green_lift_x10_1 <<- 100*(green_perc_x10_1 - green_gen) / green_gen} ; if  (red_perc_x10_1 == green_perc_x10_1) {red_lift_x10_1 = 0}; if  (red_perc_x10_1 == green_perc_x10_1) {green_lift_x10_1 = 0};
  { red_lift_x10_2 <<- 100*(red_perc_x10_2 - red_gen) / red_gen };   {green_lift_x10_2 <<- 100*(green_perc_x10_2 - green_gen) / green_gen} ; if  (red_perc_x10_2 == green_perc_x10_2) {red_lift_x10_2 = 0}; if  (red_perc_x10_2 == green_perc_x10_2) {green_lift_x10_2 = 0};
  { red_lift_x10_3 <<- 100*(red_perc_x10_3 - red_gen) / red_gen };   {green_lift_x10_3 <<- 100*(green_perc_x10_3 - green_gen) / green_gen} ; if  (red_perc_x10_3 == green_perc_x10_3) {red_lift_x10_3 = 0}; if  (red_perc_x10_3 == green_perc_x10_3) {green_lift_x10_3 = 0};
  { red_lift_x10_4 <<- 100*(red_perc_x10_4 - red_gen) / red_gen };    {green_lift_x10_4 <<- 100*(green_perc_x10_4 - green_gen) / green_gen} ; if  (red_perc_x10_4 == green_perc_x10_4) {red_lift_x10_4 = 0}; if  (red_perc_x10_4 == green_perc_x10_4) {green_lift_x10_4 = 0};
  { red_lift_x10_5 <<- 100*(red_perc_x10_5 - red_gen) / red_gen };    {green_lift_x10_5 <<- 100*(green_perc_x10_5 - green_gen) / green_gen} ; if  (red_perc_x10_5 == green_perc_x10_5) {red_lift_x10_5 = 0}; if  (red_perc_x10_5 == green_perc_x10_5) {green_lift_x10_5 = 0};
  { red_lift_x10_6 <<- 100*(red_perc_x10_6 - red_gen) / red_gen };    {green_lift_x10_6 <<- 100*(green_perc_x10_6 - green_gen) / green_gen} ; if  (red_perc_x10_6 == green_perc_x10_6) {red_lift_x10_6 = 0}; if  (red_perc_x10_6 == green_perc_x10_6) {green_lift_x10_6 = 0};
  { red_lift_x10_7 <<- 100*(red_perc_x10_7 - red_gen) / red_gen };   {green_lift_x10_7 <<- 100*(green_perc_x10_7 - green_gen) / green_gen} ; if  (red_perc_x10_7 == green_perc_x10_7) {red_lift_x10_7 = 0}; if  (red_perc_x10_7 == green_perc_x10_7) {green_lift_x10_7 = 0};
  { red_lift_x10_8 <<- 100*(red_perc_x10_8 - red_gen) / red_gen };   {green_lift_x10_8 <<- 100*(green_perc_x10_8 - green_gen) / green_gen} ; if  (red_perc_x10_8 == green_perc_x10_8) {red_lift_x10_8 = 0}; if  (red_perc_x10_8 == green_perc_x10_8) {green_lift_x10_8 = 0};
  { red_lift_x10_9 <<- 100*(red_perc_x10_9 - red_gen) / red_gen };   {green_lift_x10_9 <<- 100*(green_perc_x10_9 - green_gen) / green_gen} ; if  (red_perc_x10_9 == green_perc_x10_9) {red_lift_x10_9 = 0}; if  (red_perc_x10_9 == green_perc_x10_9) {green_lift_x10_9 = 0};
  { red_lift_x10_10 <<- 100*(red_perc_x10_10 - red_gen) / red_gen };   {green_lift_x10_10 <<- 100*(green_perc_x10_10 - green_gen) / green_gen} ; if  (red_perc_x10_10 == green_perc_x10_10) {red_lift_x10_10 = 0}; if  (red_perc_x10_10 == green_perc_x10_10) {green_lift_x10_10 = 0};
  
  
  FP_10 = (
    coalesce( if (f(green_lift_x10_1 > red_lift_x10_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x10_2 > red_lift_x10_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_3 > red_lift_x10_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_4 > red_lift_x10_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_5 > red_lift_x10_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_6 > red_lift_x10_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_7 > red_lift_x10_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_8 > red_lift_x10_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x10_9 > red_lift_x10_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_10 > red_lift_x10_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_10 = (
    coalesce( if (f(green_lift_x10_1 < red_lift_x10_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x10_2 < red_lift_x10_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_3 < red_lift_x10_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_4 < red_lift_x10_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_5 < red_lift_x10_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_6 < red_lift_x10_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_7 < red_lift_x10_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_8 < red_lift_x10_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x10_9 < red_lift_x10_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_10 < red_lift_x10_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_10 = (
    coalesce( if (f(green_lift_x10_1 > red_lift_x10_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x10_2 > red_lift_x10_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_3 > red_lift_x10_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_4 > red_lift_x10_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_5 > red_lift_x10_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_6 > red_lift_x10_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_7 > red_lift_x10_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_8 > red_lift_x10_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x10_9 > red_lift_x10_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_10 > red_lift_x10_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_10 = (
    coalesce( if (f(green_lift_x10_1 < red_lift_x10_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x10_2 < red_lift_x10_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_3 < red_lift_x10_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_4 < red_lift_x10_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_5 < red_lift_x10_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_6 < red_lift_x10_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_7 < red_lift_x10_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_8 < red_lift_x10_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x10_9 < red_lift_x10_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x10_10 < red_lift_x10_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc10 == "x10_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_10_table <<- as.data.frame.matrix(rbind(c(green_perc_x10_1 * summary(irisDisc10)[1], green_perc_x10_2 * summary(irisDisc10)[2],green_perc_x10_3 * summary(irisDisc10)[3],
                                              green_perc_x10_4 * summary(irisDisc10)[4], green_perc_x10_5 * summary(irisDisc10)[5],green_perc_x10_6 * summary(irisDisc10)[6],
                                              green_perc_x10_7 * summary(irisDisc10)[7], green_perc_x10_8 * summary(irisDisc10)[8],green_perc_x10_9 * summary(irisDisc10)[9],
                                              green_perc_x10_10 * summary(irisDisc10)[10]),
                                            c(red_perc_x10_1 * summary(irisDisc10)[1], red_perc_x10_2 * summary(irisDisc10)[2],red_perc_x10_3 * summary(irisDisc10)[3],
                                              red_perc_x10_4 * summary(irisDisc10)[4], red_perc_x10_5 * summary(irisDisc10)[5],red_perc_x10_6 * summary(irisDisc10)[6],
                                              red_perc_x10_7 * summary(irisDisc10)[7], red_perc_x10_8 * summary(irisDisc10)[8],red_perc_x10_9 * summary(irisDisc10)[9],
                                              red_perc_x10_10 * summary(irisDisc10)[10])))
  
  
  
  Precision_10 = TP_10 / (TP_10 + FP_10); Precision_10 = coalesce(Precision_10, 0)
  Recall_10 = TP_10 / (TP_10 + FN_10); Recall_10 = coalesce(Recall_10, 0)
  F1_Score_10 = coalesce ( 2 * (Precision_10 * Recall_10) / (Precision_10 + Recall_10), 0)
  MCC_10 = (TP_10 * TN_10 - FP_10 * FN_10) / sqrt ( (TP_10 + FP_10) * (TP_10 + FN_10) * (TN_10 + FP_10) * (TN_10 + FN_10) )
  MCC_adjusted_10 = (MCC_10 + 1) / 2
  P_four_10 <<- (4 * TP_10 * TN_10) / (4 * TP_10 * TN_10 + (TP_10 + TN_10) * (FP_10 + FN_10)); P_four_10 = coalesce(P_four_10, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp10a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x10_1 > green_lift_x10_1) {red_lift_x10_1} else {green_lift_x10_1},
                         if (red_lift_x10_2 > green_lift_x10_2) {red_lift_x10_2} else {green_lift_x10_2},
                         if (red_lift_x10_3 > green_lift_x10_3) {red_lift_x10_3} else {green_lift_x10_3},
                         if (red_lift_x10_4 > green_lift_x10_4) {red_lift_x10_4} else {green_lift_x10_4},
                         if (red_lift_x10_5 > green_lift_x10_5) {red_lift_x10_5} else {green_lift_x10_5},
                         if (red_lift_x10_6 > green_lift_x10_6) {red_lift_x10_6} else {green_lift_x10_6},
                         if (red_lift_x10_7 > green_lift_x10_7) {red_lift_x10_7} else {green_lift_x10_7},
                         if (red_lift_x10_8 > green_lift_x10_8) {red_lift_x10_8} else {green_lift_x10_8},
                         if (red_lift_x10_9 > green_lift_x10_9) {red_lift_x10_9} else {green_lift_x10_9},
                         if (red_lift_x10_10 > green_lift_x10_10) {red_lift_x10_10} else {green_lift_x10_10}),
                       col = c(
                         if (red_lift_x10_1 > green_lift_x10_1) {"red"} else {"green"},
                         if (red_lift_x10_2 > green_lift_x10_2) {"red"} else {"green"},
                         if (red_lift_x10_3 > green_lift_x10_3) {"red"} else {"green"},
                         if (red_lift_x10_4 > green_lift_x10_4) {"red"} else {"green"},
                         if (red_lift_x10_5 > green_lift_x10_5) {"red"} else {"green"},
                         if (red_lift_x10_6 > green_lift_x10_6) {"red"} else {"green"},
                         if (red_lift_x10_7 > green_lift_x10_7) {"red"} else {"green"},
                         if (red_lift_x10_8 > green_lift_x10_8) {"red"} else {"green"},
                         if (red_lift_x10_9 > green_lift_x10_9) {"red"} else {"green"},
                         if (red_lift_x10_10 > green_lift_x10_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(10)$Tag), cex.main = 0.7,
                    c(if (red_lift_x10_1 > green_lift_x10_1) {red_lift_x10_1} else if (red_lift_x10_1 < green_lift_x10_1) {green_lift_x10_1} else {green_lift_x10_1},
                      if (red_lift_x10_2 > green_lift_x10_2) {red_lift_x10_2} else if (red_lift_x10_2 < green_lift_x10_2) {green_lift_x10_2} else {green_lift_x10_2},
                      if (red_lift_x10_3 > green_lift_x10_3) {red_lift_x10_3} else if (red_lift_x10_3 < green_lift_x10_3) {green_lift_x10_3} else {green_lift_x10_3},
                      if (red_lift_x10_4 > green_lift_x10_4) {red_lift_x10_4} else if (red_lift_x10_4 < green_lift_x10_4) {green_lift_x10_4} else {green_lift_x10_4},
                      if (red_lift_x10_5 > green_lift_x10_5) {red_lift_x10_5} else if (red_lift_x10_5 < green_lift_x10_5) {green_lift_x10_5} else {green_lift_x10_5},
                      if (red_lift_x10_6 > green_lift_x10_6) {red_lift_x10_6} else if (red_lift_x10_6 < green_lift_x10_6) {green_lift_x10_6} else {green_lift_x10_6},
                      if (red_lift_x10_7 > green_lift_x10_7) {red_lift_x10_7} else if (red_lift_x10_7 < green_lift_x10_7) {green_lift_x10_7} else {green_lift_x10_7},
                      if (red_lift_x10_8 > green_lift_x10_8) {red_lift_x10_8} else if (red_lift_x10_8 < green_lift_x10_8) {green_lift_x10_8} else {green_lift_x10_8},
                      if (red_lift_x10_9 > green_lift_x10_9) {red_lift_x10_9} else if (red_lift_x10_9 < green_lift_x10_9) {green_lift_x10_9} else {green_lift_x10_9},
                      if (red_lift_x10_10 > green_lift_x10_10) {red_lift_x10_10} else if (red_lift_x10_10 < green_lift_x10_10) {green_lift_x10_10} else {green_lift_x10_10}),
                    col = c(
                      if (red_lift_x10_1 > green_lift_x10_1) {"red"} else {"green"},
                      if (red_lift_x10_2 > green_lift_x10_2) {"red"} else {"green"},
                      if (red_lift_x10_3 > green_lift_x10_3) {"red"} else {"green"},
                      if (red_lift_x10_4 > green_lift_x10_4) {"red"} else {"green"},
                      if (red_lift_x10_5 > green_lift_x10_5) {"red"} else {"green"},
                      if (red_lift_x10_6 > green_lift_x10_6) {"red"} else {"green"},
                      if (red_lift_x10_7 > green_lift_x10_7) {"red"} else {"green"},
                      if (red_lift_x10_8 > green_lift_x10_8) {"red"} else {"green"},
                      if (red_lift_x10_9 > green_lift_x10_9) {"red"} else {"green"},
                      if (red_lift_x10_10 > green_lift_x10_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(10)$Tag), expression("-> {"), round(100*P_four_10, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x10_1 > green_lift_x10_1) {red_lift_x10_1} else {green_lift_x10_1},
          if (red_lift_x10_2 > green_lift_x10_2) {red_lift_x10_2} else {green_lift_x10_2},
          if (red_lift_x10_3 > green_lift_x10_3) {red_lift_x10_3} else {green_lift_x10_3},
          if (red_lift_x10_4 > green_lift_x10_4) {red_lift_x10_4} else {green_lift_x10_4},
          if (red_lift_x10_5 > green_lift_x10_5) {red_lift_x10_5} else {green_lift_x10_5},
          if (red_lift_x10_6 > green_lift_x10_6) {red_lift_x10_6} else {green_lift_x10_6},
          if (red_lift_x10_7 > green_lift_x10_7) {red_lift_x10_7} else {green_lift_x10_7},
          if (red_lift_x10_8 > green_lift_x10_8) {red_lift_x10_8} else {green_lift_x10_8},
          if (red_lift_x10_9 > green_lift_x10_9) {red_lift_x10_9} else {green_lift_x10_9},
          if (red_lift_x10_10 > green_lift_x10_10) {red_lift_x10_10} else {green_lift_x10_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x10_1 > green_lift_x10_1) {red_lift_x10_1} else {green_lift_x10_1},
                                 if (red_lift_x10_2 > green_lift_x10_2) {red_lift_x10_2} else {green_lift_x10_2},
                                 if (red_lift_x10_3 > green_lift_x10_3) {red_lift_x10_3} else {green_lift_x10_3},
                                 if (red_lift_x10_4 > green_lift_x10_4) {red_lift_x10_4} else {green_lift_x10_4},
                                 if (red_lift_x10_5 > green_lift_x10_5) {red_lift_x10_5} else {green_lift_x10_5},
                                 if (red_lift_x10_6 > green_lift_x10_6) {red_lift_x10_6} else {green_lift_x10_6},
                                 if (red_lift_x10_7 > green_lift_x10_7) {red_lift_x10_7} else {green_lift_x10_7},
                                 if (red_lift_x10_8 > green_lift_x10_8) {red_lift_x10_8} else {green_lift_x10_8},
                                 if (red_lift_x10_9 > green_lift_x10_9) {red_lift_x10_9} else {green_lift_x10_9},
                                 if (red_lift_x10_10 > green_lift_x10_10) {red_lift_x10_10} else {green_lift_x10_10}),round))
    )))
  
  
  
  pp10b = as.grob(expression(barplot(as.matrix(x_10_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc10, which = "discretized:breaks")[1],attr(x = irisDisc10, which = "discretized:breaks")[11])),
                             text(x=barplot(as.matrix(x_10_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[2],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[3],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[4],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[5],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[6],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[7],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[8],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[9],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[10],1)),
                               paste0(round(attr(x = irisDisc10, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc10, which = "discretized:breaks")[11],1))
                             )),
                             y= (summary(irisDisc10))+20, labels=as.character((summary(irisDisc10))))
                             
  ))
  
  
  num10 = grid.arrange(grobs=list(as.ggplot(pp10a),as.ggplot(pp10b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x11_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw))))
  red_perc_x11_1 = 1 - green_perc_x11_1
  
  green_perc_x11_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw))))
  red_perc_x11_2 = 1 - green_perc_x11_2
  
  green_perc_x11_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw))))
  red_perc_x11_3 = 1 - green_perc_x11_3
  
  green_perc_x11_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw))))
  red_perc_x11_4 = 1 - green_perc_x11_4
  
  green_perc_x11_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw))))
  red_perc_x11_5 = 1 - green_perc_x11_5
  
  green_perc_x11_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw))))
  red_perc_x11_6 = 1 - green_perc_x11_6
  
  green_perc_x11_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw))))
  red_perc_x11_7 = 1 - green_perc_x11_7
  
  green_perc_x11_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw))))
  red_perc_x11_8 = 1 - green_perc_x11_8
  
  green_perc_x11_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw))))
  red_perc_x11_9 = 1 - green_perc_x11_9
  
  green_perc_x11_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw))))
  red_perc_x11_10 = 1 - green_perc_x11_10
  
  
  
  { red_lift_x11_1 <<- 100*(red_perc_x11_1 - red_gen) / red_gen };    {green_lift_x11_1 <<- 100*(green_perc_x11_1 - green_gen) / green_gen} ; if  (red_perc_x11_1 == green_perc_x11_1) {red_lift_x11_1 = 0}; if  (red_perc_x11_1 == green_perc_x11_1) {green_lift_x11_1 = 0};
  { red_lift_x11_2 <<- 100*(red_perc_x11_2 - red_gen) / red_gen };   {green_lift_x11_2 <<- 100*(green_perc_x11_2 - green_gen) / green_gen} ; if  (red_perc_x11_2 == green_perc_x11_2) {red_lift_x11_2 = 0}; if  (red_perc_x11_2 == green_perc_x11_2) {green_lift_x11_2 = 0};
  { red_lift_x11_3 <<- 100*(red_perc_x11_3 - red_gen) / red_gen };   {green_lift_x11_3 <<- 100*(green_perc_x11_3 - green_gen) / green_gen} ; if  (red_perc_x11_3 == green_perc_x11_3) {red_lift_x11_3 = 0}; if  (red_perc_x11_3 == green_perc_x11_3) {green_lift_x11_3 = 0};
  { red_lift_x11_4 <<- 100*(red_perc_x11_4 - red_gen) / red_gen };    {green_lift_x11_4 <<- 100*(green_perc_x11_4 - green_gen) / green_gen} ; if  (red_perc_x11_4 == green_perc_x11_4) {red_lift_x11_4 = 0}; if  (red_perc_x11_4 == green_perc_x11_4) {green_lift_x11_4 = 0};
  { red_lift_x11_5 <<- 100*(red_perc_x11_5 - red_gen) / red_gen };    {green_lift_x11_5 <<- 100*(green_perc_x11_5 - green_gen) / green_gen} ; if  (red_perc_x11_5 == green_perc_x11_5) {red_lift_x11_5 = 0}; if  (red_perc_x11_5 == green_perc_x11_5) {green_lift_x11_5 = 0};
  { red_lift_x11_6 <<- 100*(red_perc_x11_6 - red_gen) / red_gen };    {green_lift_x11_6 <<- 100*(green_perc_x11_6 - green_gen) / green_gen} ; if  (red_perc_x11_6 == green_perc_x11_6) {red_lift_x11_6 = 0}; if  (red_perc_x11_6 == green_perc_x11_6) {green_lift_x11_6 = 0};
  { red_lift_x11_7 <<- 100*(red_perc_x11_7 - red_gen) / red_gen };   {green_lift_x11_7 <<- 100*(green_perc_x11_7 - green_gen) / green_gen} ; if  (red_perc_x11_7 == green_perc_x11_7) {red_lift_x11_7 = 0}; if  (red_perc_x11_7 == green_perc_x11_7) {green_lift_x11_7 = 0};
  { red_lift_x11_8 <<- 100*(red_perc_x11_8 - red_gen) / red_gen };   {green_lift_x11_8 <<- 100*(green_perc_x11_8 - green_gen) / green_gen} ; if  (red_perc_x11_8 == green_perc_x11_8) {red_lift_x11_8 = 0}; if  (red_perc_x11_8 == green_perc_x11_8) {green_lift_x11_8 = 0};
  { red_lift_x11_9 <<- 100*(red_perc_x11_9 - red_gen) / red_gen };   {green_lift_x11_9 <<- 100*(green_perc_x11_9 - green_gen) / green_gen} ; if  (red_perc_x11_9 == green_perc_x11_9) {red_lift_x11_9 = 0}; if  (red_perc_x11_9 == green_perc_x11_9) {green_lift_x11_9 = 0};
  { red_lift_x11_10 <<- 100*(red_perc_x11_10 - red_gen) / red_gen };   {green_lift_x11_10 <<- 100*(green_perc_x11_10 - green_gen) / green_gen} ; if  (red_perc_x11_10 == green_perc_x11_10) {red_lift_x11_10 = 0}; if  (red_perc_x11_10 == green_perc_x11_10) {green_lift_x11_10 = 0};
  
  
  FP_11 = (
    coalesce( if (f(green_lift_x11_1 > red_lift_x11_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x11_2 > red_lift_x11_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_3 > red_lift_x11_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_4 > red_lift_x11_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_5 > red_lift_x11_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_6 > red_lift_x11_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_7 > red_lift_x11_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_8 > red_lift_x11_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x11_9 > red_lift_x11_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_10 > red_lift_x11_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_11 = (
    coalesce( if (f(green_lift_x11_1 < red_lift_x11_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x11_2 < red_lift_x11_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_3 < red_lift_x11_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_4 < red_lift_x11_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_5 < red_lift_x11_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_6 < red_lift_x11_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_7 < red_lift_x11_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_8 < red_lift_x11_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x11_9 < red_lift_x11_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_10 < red_lift_x11_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_11 = (
    coalesce( if (f(green_lift_x11_1 > red_lift_x11_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x11_2 > red_lift_x11_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_3 > red_lift_x11_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_4 > red_lift_x11_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_5 > red_lift_x11_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_6 > red_lift_x11_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_7 > red_lift_x11_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_8 > red_lift_x11_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x11_9 > red_lift_x11_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_10 > red_lift_x11_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_11 = (
    coalesce( if (f(green_lift_x11_1 < red_lift_x11_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x11_2 < red_lift_x11_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_3 < red_lift_x11_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_4 < red_lift_x11_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_5 < red_lift_x11_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_6 < red_lift_x11_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_7 < red_lift_x11_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_8 < red_lift_x11_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x11_9 < red_lift_x11_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x11_10 < red_lift_x11_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc11 == "x11_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_11_table <<- as.data.frame.matrix(rbind(c(green_perc_x11_1 * summary(irisDisc11)[1], green_perc_x11_2 * summary(irisDisc11)[2],green_perc_x11_3 * summary(irisDisc11)[3],
                                              green_perc_x11_4 * summary(irisDisc11)[4], green_perc_x11_5 * summary(irisDisc11)[5],green_perc_x11_6 * summary(irisDisc11)[6],
                                              green_perc_x11_7 * summary(irisDisc11)[7], green_perc_x11_8 * summary(irisDisc11)[8],green_perc_x11_9 * summary(irisDisc11)[9],
                                              green_perc_x11_10 * summary(irisDisc11)[10]),
                                            c(red_perc_x11_1 * summary(irisDisc11)[1], red_perc_x11_2 * summary(irisDisc11)[2],red_perc_x11_3 * summary(irisDisc11)[3],
                                              red_perc_x11_4 * summary(irisDisc11)[4], red_perc_x11_5 * summary(irisDisc11)[5],red_perc_x11_6 * summary(irisDisc11)[6],
                                              red_perc_x11_7 * summary(irisDisc11)[7], red_perc_x11_8 * summary(irisDisc11)[8],red_perc_x11_9 * summary(irisDisc11)[9],
                                              red_perc_x11_10 * summary(irisDisc11)[10])))
  
  
  
  Precision_11 = TP_11 / (TP_11 + FP_11); Precision_11 = coalesce(Precision_11, 0)
  Recall_11 = TP_11 / (TP_11 + FN_11); Recall_11 = coalesce(Recall_11, 0)
  F1_Score_11 = coalesce ( 2 * (Precision_11 * Recall_11) / (Precision_11 + Recall_11), 0)
  MCC_11 = (TP_11 * TN_11 - FP_11 * FN_11) / sqrt ( (TP_11 + FP_11) * (TP_11 + FN_11) * (TN_11 + FP_11) * (TN_11 + FN_11) )
  MCC_adjusted_11 = (MCC_11 + 1) / 2
  P_four_11 <<- (4 * TP_11 * TN_11) / (4 * TP_11 * TN_11 + (TP_11 + TN_11) * (FP_11 + FN_11)); P_four_11 = coalesce(P_four_11, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp11a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x11_1 > green_lift_x11_1) {red_lift_x11_1} else {green_lift_x11_1},
                         if (red_lift_x11_2 > green_lift_x11_2) {red_lift_x11_2} else {green_lift_x11_2},
                         if (red_lift_x11_3 > green_lift_x11_3) {red_lift_x11_3} else {green_lift_x11_3},
                         if (red_lift_x11_4 > green_lift_x11_4) {red_lift_x11_4} else {green_lift_x11_4},
                         if (red_lift_x11_5 > green_lift_x11_5) {red_lift_x11_5} else {green_lift_x11_5},
                         if (red_lift_x11_6 > green_lift_x11_6) {red_lift_x11_6} else {green_lift_x11_6},
                         if (red_lift_x11_7 > green_lift_x11_7) {red_lift_x11_7} else {green_lift_x11_7},
                         if (red_lift_x11_8 > green_lift_x11_8) {red_lift_x11_8} else {green_lift_x11_8},
                         if (red_lift_x11_9 > green_lift_x11_9) {red_lift_x11_9} else {green_lift_x11_9},
                         if (red_lift_x11_10 > green_lift_x11_10) {red_lift_x11_10} else {green_lift_x11_10}),
                       col = c(
                         if (red_lift_x11_1 > green_lift_x11_1) {"red"} else {"green"},
                         if (red_lift_x11_2 > green_lift_x11_2) {"red"} else {"green"},
                         if (red_lift_x11_3 > green_lift_x11_3) {"red"} else {"green"},
                         if (red_lift_x11_4 > green_lift_x11_4) {"red"} else {"green"},
                         if (red_lift_x11_5 > green_lift_x11_5) {"red"} else {"green"},
                         if (red_lift_x11_6 > green_lift_x11_6) {"red"} else {"green"},
                         if (red_lift_x11_7 > green_lift_x11_7) {"red"} else {"green"},
                         if (red_lift_x11_8 > green_lift_x11_8) {"red"} else {"green"},
                         if (red_lift_x11_9 > green_lift_x11_9) {"red"} else {"green"},
                         if (red_lift_x11_10 > green_lift_x11_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(11)$Tag), cex.main = 0.7,
                    c(if (red_lift_x11_1 > green_lift_x11_1) {red_lift_x11_1} else if (red_lift_x11_1 < green_lift_x11_1) {green_lift_x11_1} else {green_lift_x11_1},
                      if (red_lift_x11_2 > green_lift_x11_2) {red_lift_x11_2} else if (red_lift_x11_2 < green_lift_x11_2) {green_lift_x11_2} else {green_lift_x11_2},
                      if (red_lift_x11_3 > green_lift_x11_3) {red_lift_x11_3} else if (red_lift_x11_3 < green_lift_x11_3) {green_lift_x11_3} else {green_lift_x11_3},
                      if (red_lift_x11_4 > green_lift_x11_4) {red_lift_x11_4} else if (red_lift_x11_4 < green_lift_x11_4) {green_lift_x11_4} else {green_lift_x11_4},
                      if (red_lift_x11_5 > green_lift_x11_5) {red_lift_x11_5} else if (red_lift_x11_5 < green_lift_x11_5) {green_lift_x11_5} else {green_lift_x11_5},
                      if (red_lift_x11_6 > green_lift_x11_6) {red_lift_x11_6} else if (red_lift_x11_6 < green_lift_x11_6) {green_lift_x11_6} else {green_lift_x11_6},
                      if (red_lift_x11_7 > green_lift_x11_7) {red_lift_x11_7} else if (red_lift_x11_7 < green_lift_x11_7) {green_lift_x11_7} else {green_lift_x11_7},
                      if (red_lift_x11_8 > green_lift_x11_8) {red_lift_x11_8} else if (red_lift_x11_8 < green_lift_x11_8) {green_lift_x11_8} else {green_lift_x11_8},
                      if (red_lift_x11_9 > green_lift_x11_9) {red_lift_x11_9} else if (red_lift_x11_9 < green_lift_x11_9) {green_lift_x11_9} else {green_lift_x11_9},
                      if (red_lift_x11_10 > green_lift_x11_10) {red_lift_x11_10} else if (red_lift_x11_10 < green_lift_x11_10) {green_lift_x11_10} else {green_lift_x11_10}),
                    col = c(
                      if (red_lift_x11_1 > green_lift_x11_1) {"red"} else {"green"},
                      if (red_lift_x11_2 > green_lift_x11_2) {"red"} else {"green"},
                      if (red_lift_x11_3 > green_lift_x11_3) {"red"} else {"green"},
                      if (red_lift_x11_4 > green_lift_x11_4) {"red"} else {"green"},
                      if (red_lift_x11_5 > green_lift_x11_5) {"red"} else {"green"},
                      if (red_lift_x11_6 > green_lift_x11_6) {"red"} else {"green"},
                      if (red_lift_x11_7 > green_lift_x11_7) {"red"} else {"green"},
                      if (red_lift_x11_8 > green_lift_x11_8) {"red"} else {"green"},
                      if (red_lift_x11_9 > green_lift_x11_9) {"red"} else {"green"},
                      if (red_lift_x11_10 > green_lift_x11_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(11)$Tag), expression("-> {"), round(100*P_four_11, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x11_1 > green_lift_x11_1) {red_lift_x11_1} else {green_lift_x11_1},
          if (red_lift_x11_2 > green_lift_x11_2) {red_lift_x11_2} else {green_lift_x11_2},
          if (red_lift_x11_3 > green_lift_x11_3) {red_lift_x11_3} else {green_lift_x11_3},
          if (red_lift_x11_4 > green_lift_x11_4) {red_lift_x11_4} else {green_lift_x11_4},
          if (red_lift_x11_5 > green_lift_x11_5) {red_lift_x11_5} else {green_lift_x11_5},
          if (red_lift_x11_6 > green_lift_x11_6) {red_lift_x11_6} else {green_lift_x11_6},
          if (red_lift_x11_7 > green_lift_x11_7) {red_lift_x11_7} else {green_lift_x11_7},
          if (red_lift_x11_8 > green_lift_x11_8) {red_lift_x11_8} else {green_lift_x11_8},
          if (red_lift_x11_9 > green_lift_x11_9) {red_lift_x11_9} else {green_lift_x11_9},
          if (red_lift_x11_10 > green_lift_x11_10) {red_lift_x11_10} else {green_lift_x11_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x11_1 > green_lift_x11_1) {red_lift_x11_1} else {green_lift_x11_1},
                                 if (red_lift_x11_2 > green_lift_x11_2) {red_lift_x11_2} else {green_lift_x11_2},
                                 if (red_lift_x11_3 > green_lift_x11_3) {red_lift_x11_3} else {green_lift_x11_3},
                                 if (red_lift_x11_4 > green_lift_x11_4) {red_lift_x11_4} else {green_lift_x11_4},
                                 if (red_lift_x11_5 > green_lift_x11_5) {red_lift_x11_5} else {green_lift_x11_5},
                                 if (red_lift_x11_6 > green_lift_x11_6) {red_lift_x11_6} else {green_lift_x11_6},
                                 if (red_lift_x11_7 > green_lift_x11_7) {red_lift_x11_7} else {green_lift_x11_7},
                                 if (red_lift_x11_8 > green_lift_x11_8) {red_lift_x11_8} else {green_lift_x11_8},
                                 if (red_lift_x11_9 > green_lift_x11_9) {red_lift_x11_9} else {green_lift_x11_9},
                                 if (red_lift_x11_10 > green_lift_x11_10) {red_lift_x11_10} else {green_lift_x11_10}),round))
    )))
  
  
  
  pp11b = as.grob(expression(barplot(as.matrix(x_11_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc11, which = "discretized:breaks")[1],attr(x = irisDisc11, which = "discretized:breaks")[11])),
                             text(x=barplot(as.matrix(x_11_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[2],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[3],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[4],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[5],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[6],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[7],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[8],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[9],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[10],1)),
                               paste0(round(attr(x = irisDisc11, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc11, which = "discretized:breaks")[11],1))
                             )),
                             y= (summary(irisDisc11))+20, labels=as.character((summary(irisDisc11))))
                             
  ))
  
  
  num11 = grid.arrange(grobs=list(as.ggplot(pp11a),as.ggplot(pp11b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x12_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw))))
  red_perc_x12_1 = 1 - green_perc_x12_1
  
  green_perc_x12_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw))))
  red_perc_x12_2 = 1 - green_perc_x12_2
  
  green_perc_x12_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw))))
  red_perc_x12_3 = 1 - green_perc_x12_3
  
  green_perc_x12_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw))))
  red_perc_x12_4 = 1 - green_perc_x12_4
  
  green_perc_x12_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw))))
  red_perc_x12_5 = 1 - green_perc_x12_5
  
  green_perc_x12_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw))))
  red_perc_x12_6 = 1 - green_perc_x12_6
  
  green_perc_x12_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw))))
  red_perc_x12_7 = 1 - green_perc_x12_7
  
  green_perc_x12_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw))))
  red_perc_x12_8 = 1 - green_perc_x12_8
  
  green_perc_x12_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw))))
  red_perc_x12_9 = 1 - green_perc_x12_9
  
  green_perc_x12_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw))))
  red_perc_x12_10 = 1 - green_perc_x12_10
  
  
  
  { red_lift_x12_1 <<- 100*(red_perc_x12_1 - red_gen) / red_gen };    {green_lift_x12_1 <<- 100*(green_perc_x12_1 - green_gen) / green_gen} ; if  (red_perc_x12_1 == green_perc_x12_1) {red_lift_x12_1 = 0}; if  (red_perc_x12_1 == green_perc_x12_1) {green_lift_x12_1 = 0};
  { red_lift_x12_2 <<- 100*(red_perc_x12_2 - red_gen) / red_gen };   {green_lift_x12_2 <<- 100*(green_perc_x12_2 - green_gen) / green_gen} ; if  (red_perc_x12_2 == green_perc_x12_2) {red_lift_x12_2 = 0}; if  (red_perc_x12_2 == green_perc_x12_2) {green_lift_x12_2 = 0};
  { red_lift_x12_3 <<- 100*(red_perc_x12_3 - red_gen) / red_gen };   {green_lift_x12_3 <<- 100*(green_perc_x12_3 - green_gen) / green_gen} ; if  (red_perc_x12_3 == green_perc_x12_3) {red_lift_x12_3 = 0}; if  (red_perc_x12_3 == green_perc_x12_3) {green_lift_x12_3 = 0};
  { red_lift_x12_4 <<- 100*(red_perc_x12_4 - red_gen) / red_gen };    {green_lift_x12_4 <<- 100*(green_perc_x12_4 - green_gen) / green_gen} ; if  (red_perc_x12_4 == green_perc_x12_4) {red_lift_x12_4 = 0}; if  (red_perc_x12_4 == green_perc_x12_4) {green_lift_x12_4 = 0};
  { red_lift_x12_5 <<- 100*(red_perc_x12_5 - red_gen) / red_gen };    {green_lift_x12_5 <<- 100*(green_perc_x12_5 - green_gen) / green_gen} ; if  (red_perc_x12_5 == green_perc_x12_5) {red_lift_x12_5 = 0}; if  (red_perc_x12_5 == green_perc_x12_5) {green_lift_x12_5 = 0};
  { red_lift_x12_6 <<- 100*(red_perc_x12_6 - red_gen) / red_gen };    {green_lift_x12_6 <<- 100*(green_perc_x12_6 - green_gen) / green_gen} ; if  (red_perc_x12_6 == green_perc_x12_6) {red_lift_x12_6 = 0}; if  (red_perc_x12_6 == green_perc_x12_6) {green_lift_x12_6 = 0};
  { red_lift_x12_7 <<- 100*(red_perc_x12_7 - red_gen) / red_gen };   {green_lift_x12_7 <<- 100*(green_perc_x12_7 - green_gen) / green_gen} ; if  (red_perc_x12_7 == green_perc_x12_7) {red_lift_x12_7 = 0}; if  (red_perc_x12_7 == green_perc_x12_7) {green_lift_x12_7 = 0};
  { red_lift_x12_8 <<- 100*(red_perc_x12_8 - red_gen) / red_gen };   {green_lift_x12_8 <<- 100*(green_perc_x12_8 - green_gen) / green_gen} ; if  (red_perc_x12_8 == green_perc_x12_8) {red_lift_x12_8 = 0}; if  (red_perc_x12_8 == green_perc_x12_8) {green_lift_x12_8 = 0};
  { red_lift_x12_9 <<- 100*(red_perc_x12_9 - red_gen) / red_gen };   {green_lift_x12_9 <<- 100*(green_perc_x12_9 - green_gen) / green_gen} ; if  (red_perc_x12_9 == green_perc_x12_9) {red_lift_x12_9 = 0}; if  (red_perc_x12_9 == green_perc_x12_9) {green_lift_x12_9 = 0};
  { red_lift_x12_10 <<- 100*(red_perc_x12_10 - red_gen) / red_gen };   {green_lift_x12_10 <<- 100*(green_perc_x12_10 - green_gen) / green_gen} ; if  (red_perc_x12_10 == green_perc_x12_10) {red_lift_x12_10 = 0}; if  (red_perc_x12_10 == green_perc_x12_10) {green_lift_x12_10 = 0};
  
  
  FP_12 = (
    coalesce( if (f(green_lift_x12_1 > red_lift_x12_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x12_2 > red_lift_x12_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_3 > red_lift_x12_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_4 > red_lift_x12_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_5 > red_lift_x12_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_6 > red_lift_x12_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_7 > red_lift_x12_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_8 > red_lift_x12_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x12_9 > red_lift_x12_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_10 > red_lift_x12_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_12 = (
    coalesce( if (f(green_lift_x12_1 < red_lift_x12_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x12_2 < red_lift_x12_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_3 < red_lift_x12_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_4 < red_lift_x12_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_5 < red_lift_x12_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_6 < red_lift_x12_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_7 < red_lift_x12_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_8 < red_lift_x12_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x12_9 < red_lift_x12_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_10 < red_lift_x12_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_12 = (
    coalesce( if (f(green_lift_x12_1 > red_lift_x12_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x12_2 > red_lift_x12_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_3 > red_lift_x12_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_4 > red_lift_x12_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_5 > red_lift_x12_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_6 > red_lift_x12_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_7 > red_lift_x12_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_8 > red_lift_x12_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x12_9 > red_lift_x12_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_10 > red_lift_x12_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_12 = (
    coalesce( if (f(green_lift_x12_1 < red_lift_x12_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x12_2 < red_lift_x12_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_3 < red_lift_x12_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_4 < red_lift_x12_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_5 < red_lift_x12_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_6 < red_lift_x12_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_7 < red_lift_x12_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_8 < red_lift_x12_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x12_9 < red_lift_x12_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x12_10 < red_lift_x12_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc12 == "x12_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_12_table <<- as.data.frame.matrix(rbind(c(green_perc_x12_1 * summary(irisDisc12)[1], green_perc_x12_2 * summary(irisDisc12)[2],green_perc_x12_3 * summary(irisDisc12)[3],
                                              green_perc_x12_4 * summary(irisDisc12)[4], green_perc_x12_5 * summary(irisDisc12)[5],green_perc_x12_6 * summary(irisDisc12)[6],
                                              green_perc_x12_7 * summary(irisDisc12)[7], green_perc_x12_8 * summary(irisDisc12)[8],green_perc_x12_9 * summary(irisDisc12)[9],
                                              green_perc_x12_10 * summary(irisDisc12)[10]),
                                            c(red_perc_x12_1 * summary(irisDisc12)[1], red_perc_x12_2 * summary(irisDisc12)[2],red_perc_x12_3 * summary(irisDisc12)[3],
                                              red_perc_x12_4 * summary(irisDisc12)[4], red_perc_x12_5 * summary(irisDisc12)[5],red_perc_x12_6 * summary(irisDisc12)[6],
                                              red_perc_x12_7 * summary(irisDisc12)[7], red_perc_x12_8 * summary(irisDisc12)[8],red_perc_x12_9 * summary(irisDisc12)[9],
                                              red_perc_x12_10 * summary(irisDisc12)[10])))
  
  
  
  Precision_12 = TP_12 / (TP_12 + FP_12); Precision_12 = coalesce(Precision_12, 0)
  Recall_12 = TP_12 / (TP_12 + FN_12); Recall_12 = coalesce(Recall_12, 0)
  F1_Score_12 = coalesce ( 2 * (Precision_12 * Recall_12) / (Precision_12 + Recall_12), 0)
  MCC_12 = (TP_12 * TN_12 - FP_12 * FN_12) / sqrt ( (TP_12 + FP_12) * (TP_12 + FN_12) * (TN_12 + FP_12) * (TN_12 + FN_12) )
  MCC_adjusted_12 = (MCC_12 + 1) / 2
  P_four_12 <<- (4 * TP_12 * TN_12) / (4 * TP_12 * TN_12 + (TP_12 + TN_12) * (FP_12 + FN_12)); P_four_12 = coalesce(P_four_12, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp12a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x12_1 > green_lift_x12_1) {red_lift_x12_1} else {green_lift_x12_1},
                         if (red_lift_x12_2 > green_lift_x12_2) {red_lift_x12_2} else {green_lift_x12_2},
                         if (red_lift_x12_3 > green_lift_x12_3) {red_lift_x12_3} else {green_lift_x12_3},
                         if (red_lift_x12_4 > green_lift_x12_4) {red_lift_x12_4} else {green_lift_x12_4},
                         if (red_lift_x12_5 > green_lift_x12_5) {red_lift_x12_5} else {green_lift_x12_5},
                         if (red_lift_x12_6 > green_lift_x12_6) {red_lift_x12_6} else {green_lift_x12_6},
                         if (red_lift_x12_7 > green_lift_x12_7) {red_lift_x12_7} else {green_lift_x12_7},
                         if (red_lift_x12_8 > green_lift_x12_8) {red_lift_x12_8} else {green_lift_x12_8},
                         if (red_lift_x12_9 > green_lift_x12_9) {red_lift_x12_9} else {green_lift_x12_9},
                         if (red_lift_x12_10 > green_lift_x12_10) {red_lift_x12_10} else {green_lift_x12_10}),
                       col = c(
                         if (red_lift_x12_1 > green_lift_x12_1) {"red"} else {"green"},
                         if (red_lift_x12_2 > green_lift_x12_2) {"red"} else {"green"},
                         if (red_lift_x12_3 > green_lift_x12_3) {"red"} else {"green"},
                         if (red_lift_x12_4 > green_lift_x12_4) {"red"} else {"green"},
                         if (red_lift_x12_5 > green_lift_x12_5) {"red"} else {"green"},
                         if (red_lift_x12_6 > green_lift_x12_6) {"red"} else {"green"},
                         if (red_lift_x12_7 > green_lift_x12_7) {"red"} else {"green"},
                         if (red_lift_x12_8 > green_lift_x12_8) {"red"} else {"green"},
                         if (red_lift_x12_9 > green_lift_x12_9) {"red"} else {"green"},
                         if (red_lift_x12_10 > green_lift_x12_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(12)$Tag), cex.main = 0.7,
                    c(if (red_lift_x12_1 > green_lift_x12_1) {red_lift_x12_1} else if (red_lift_x12_1 < green_lift_x12_1) {green_lift_x12_1} else {green_lift_x12_1},
                      if (red_lift_x12_2 > green_lift_x12_2) {red_lift_x12_2} else if (red_lift_x12_2 < green_lift_x12_2) {green_lift_x12_2} else {green_lift_x12_2},
                      if (red_lift_x12_3 > green_lift_x12_3) {red_lift_x12_3} else if (red_lift_x12_3 < green_lift_x12_3) {green_lift_x12_3} else {green_lift_x12_3},
                      if (red_lift_x12_4 > green_lift_x12_4) {red_lift_x12_4} else if (red_lift_x12_4 < green_lift_x12_4) {green_lift_x12_4} else {green_lift_x12_4},
                      if (red_lift_x12_5 > green_lift_x12_5) {red_lift_x12_5} else if (red_lift_x12_5 < green_lift_x12_5) {green_lift_x12_5} else {green_lift_x12_5},
                      if (red_lift_x12_6 > green_lift_x12_6) {red_lift_x12_6} else if (red_lift_x12_6 < green_lift_x12_6) {green_lift_x12_6} else {green_lift_x12_6},
                      if (red_lift_x12_7 > green_lift_x12_7) {red_lift_x12_7} else if (red_lift_x12_7 < green_lift_x12_7) {green_lift_x12_7} else {green_lift_x12_7},
                      if (red_lift_x12_8 > green_lift_x12_8) {red_lift_x12_8} else if (red_lift_x12_8 < green_lift_x12_8) {green_lift_x12_8} else {green_lift_x12_8},
                      if (red_lift_x12_9 > green_lift_x12_9) {red_lift_x12_9} else if (red_lift_x12_9 < green_lift_x12_9) {green_lift_x12_9} else {green_lift_x12_9},
                      if (red_lift_x12_10 > green_lift_x12_10) {red_lift_x12_10} else if (red_lift_x12_10 < green_lift_x12_10) {green_lift_x12_10} else {green_lift_x12_10}),
                    col = c(
                      if (red_lift_x12_1 > green_lift_x12_1) {"red"} else {"green"},
                      if (red_lift_x12_2 > green_lift_x12_2) {"red"} else {"green"},
                      if (red_lift_x12_3 > green_lift_x12_3) {"red"} else {"green"},
                      if (red_lift_x12_4 > green_lift_x12_4) {"red"} else {"green"},
                      if (red_lift_x12_5 > green_lift_x12_5) {"red"} else {"green"},
                      if (red_lift_x12_6 > green_lift_x12_6) {"red"} else {"green"},
                      if (red_lift_x12_7 > green_lift_x12_7) {"red"} else {"green"},
                      if (red_lift_x12_8 > green_lift_x12_8) {"red"} else {"green"},
                      if (red_lift_x12_9 > green_lift_x12_9) {"red"} else {"green"},
                      if (red_lift_x12_10 > green_lift_x12_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(12)$Tag), expression("-> {"), round(100*P_four_12, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x12_1 > green_lift_x12_1) {red_lift_x12_1} else {green_lift_x12_1},
          if (red_lift_x12_2 > green_lift_x12_2) {red_lift_x12_2} else {green_lift_x12_2},
          if (red_lift_x12_3 > green_lift_x12_3) {red_lift_x12_3} else {green_lift_x12_3},
          if (red_lift_x12_4 > green_lift_x12_4) {red_lift_x12_4} else {green_lift_x12_4},
          if (red_lift_x12_5 > green_lift_x12_5) {red_lift_x12_5} else {green_lift_x12_5},
          if (red_lift_x12_6 > green_lift_x12_6) {red_lift_x12_6} else {green_lift_x12_6},
          if (red_lift_x12_7 > green_lift_x12_7) {red_lift_x12_7} else {green_lift_x12_7},
          if (red_lift_x12_8 > green_lift_x12_8) {red_lift_x12_8} else {green_lift_x12_8},
          if (red_lift_x12_9 > green_lift_x12_9) {red_lift_x12_9} else {green_lift_x12_9},
          if (red_lift_x12_10 > green_lift_x12_10) {red_lift_x12_10} else {green_lift_x12_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x12_1 > green_lift_x12_1) {red_lift_x12_1} else {green_lift_x12_1},
                                 if (red_lift_x12_2 > green_lift_x12_2) {red_lift_x12_2} else {green_lift_x12_2},
                                 if (red_lift_x12_3 > green_lift_x12_3) {red_lift_x12_3} else {green_lift_x12_3},
                                 if (red_lift_x12_4 > green_lift_x12_4) {red_lift_x12_4} else {green_lift_x12_4},
                                 if (red_lift_x12_5 > green_lift_x12_5) {red_lift_x12_5} else {green_lift_x12_5},
                                 if (red_lift_x12_6 > green_lift_x12_6) {red_lift_x12_6} else {green_lift_x12_6},
                                 if (red_lift_x12_7 > green_lift_x12_7) {red_lift_x12_7} else {green_lift_x12_7},
                                 if (red_lift_x12_8 > green_lift_x12_8) {red_lift_x12_8} else {green_lift_x12_8},
                                 if (red_lift_x12_9 > green_lift_x12_9) {red_lift_x12_9} else {green_lift_x12_9},
                                 if (red_lift_x12_10 > green_lift_x12_10) {red_lift_x12_10} else {green_lift_x12_10}),round))
    )))
  
  
  
  pp12b = as.grob(expression(barplot(as.matrix(x_12_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc12, which = "discretized:breaks")[1],attr(x = irisDisc12, which = "discretized:breaks")[11])),
                             text(x=barplot(as.matrix(x_12_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[2],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[3],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[4],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[5],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[6],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[7],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[8],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[9],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[10],1)),
                               paste0(round(attr(x = irisDisc12, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc12, which = "discretized:breaks")[11],1))
                             )),
                             y= (summary(irisDisc12))+20, labels=as.character((summary(irisDisc12))))
                             
  ))
  
  
  num12 = grid.arrange(grobs=list(as.ggplot(pp12a),as.ggplot(pp12b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  green_perc_x13_1 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw))))
  red_perc_x13_1 = 1 - green_perc_x13_1
  
  green_perc_x13_2 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw))))
  red_perc_x13_2 = 1 - green_perc_x13_2
  
  green_perc_x13_3 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw))))
  red_perc_x13_3 = 1 - green_perc_x13_3
  
  green_perc_x13_4 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw))))
  red_perc_x13_4 = 1 - green_perc_x13_4
  
  green_perc_x13_5 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw))))
  red_perc_x13_5 = 1 - green_perc_x13_5
  
  green_perc_x13_6 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw))))
  red_perc_x13_6 = 1 - green_perc_x13_6
  
  green_perc_x13_7 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw))))
  red_perc_x13_7 = 1 - green_perc_x13_7
  
  green_perc_x13_8 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw))))
  red_perc_x13_8 = 1 - green_perc_x13_8
  
  green_perc_x13_9 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw))))
  red_perc_x13_9 = 1 - green_perc_x13_9
  
  green_perc_x13_10 = sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw)))))  /  length(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw))))
  red_perc_x13_10 = 1 - green_perc_x13_10
  
  
  
  { red_lift_x13_1 <<- 100*(red_perc_x13_1 - red_gen) / red_gen };    {green_lift_x13_1 <<- 100*(green_perc_x13_1 - green_gen) / green_gen} ; if  (red_perc_x13_1 == green_perc_x13_1) {red_lift_x13_1 = 0}; if  (red_perc_x13_1 == green_perc_x13_1) {green_lift_x13_1 = 0};
  { red_lift_x13_2 <<- 100*(red_perc_x13_2 - red_gen) / red_gen };   {green_lift_x13_2 <<- 100*(green_perc_x13_2 - green_gen) / green_gen} ; if  (red_perc_x13_2 == green_perc_x13_2) {red_lift_x13_2 = 0}; if  (red_perc_x13_2 == green_perc_x13_2) {green_lift_x13_2 = 0};
  { red_lift_x13_3 <<- 100*(red_perc_x13_3 - red_gen) / red_gen };   {green_lift_x13_3 <<- 100*(green_perc_x13_3 - green_gen) / green_gen} ; if  (red_perc_x13_3 == green_perc_x13_3) {red_lift_x13_3 = 0}; if  (red_perc_x13_3 == green_perc_x13_3) {green_lift_x13_3 = 0};
  { red_lift_x13_4 <<- 100*(red_perc_x13_4 - red_gen) / red_gen };    {green_lift_x13_4 <<- 100*(green_perc_x13_4 - green_gen) / green_gen} ; if  (red_perc_x13_4 == green_perc_x13_4) {red_lift_x13_4 = 0}; if  (red_perc_x13_4 == green_perc_x13_4) {green_lift_x13_4 = 0};
  { red_lift_x13_5 <<- 100*(red_perc_x13_5 - red_gen) / red_gen };    {green_lift_x13_5 <<- 100*(green_perc_x13_5 - green_gen) / green_gen} ; if  (red_perc_x13_5 == green_perc_x13_5) {red_lift_x13_5 = 0}; if  (red_perc_x13_5 == green_perc_x13_5) {green_lift_x13_5 = 0};
  { red_lift_x13_6 <<- 100*(red_perc_x13_6 - red_gen) / red_gen };    {green_lift_x13_6 <<- 100*(green_perc_x13_6 - green_gen) / green_gen} ; if  (red_perc_x13_6 == green_perc_x13_6) {red_lift_x13_6 = 0}; if  (red_perc_x13_6 == green_perc_x13_6) {green_lift_x13_6 = 0};
  { red_lift_x13_7 <<- 100*(red_perc_x13_7 - red_gen) / red_gen };   {green_lift_x13_7 <<- 100*(green_perc_x13_7 - green_gen) / green_gen} ; if  (red_perc_x13_7 == green_perc_x13_7) {red_lift_x13_7 = 0}; if  (red_perc_x13_7 == green_perc_x13_7) {green_lift_x13_7 = 0};
  { red_lift_x13_8 <<- 100*(red_perc_x13_8 - red_gen) / red_gen };   {green_lift_x13_8 <<- 100*(green_perc_x13_8 - green_gen) / green_gen} ; if  (red_perc_x13_8 == green_perc_x13_8) {red_lift_x13_8 = 0}; if  (red_perc_x13_8 == green_perc_x13_8) {green_lift_x13_8 = 0};
  { red_lift_x13_9 <<- 100*(red_perc_x13_9 - red_gen) / red_gen };   {green_lift_x13_9 <<- 100*(green_perc_x13_9 - green_gen) / green_gen} ; if  (red_perc_x13_9 == green_perc_x13_9) {red_lift_x13_9 = 0}; if  (red_perc_x13_9 == green_perc_x13_9) {green_lift_x13_9 = 0};
  { red_lift_x13_10 <<- 100*(red_perc_x13_10 - red_gen) / red_gen };   {green_lift_x13_10 <<- 100*(green_perc_x13_10 - green_gen) / green_gen} ; if  (red_perc_x13_10 == green_perc_x13_10) {red_lift_x13_10 = 0}; if  (red_perc_x13_10 == green_perc_x13_10) {green_lift_x13_10 = 0};
  
  
  FP_13 = (
    coalesce( if (f(green_lift_x13_1 > red_lift_x13_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x13_2 > red_lift_x13_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_3 > red_lift_x13_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_4 > red_lift_x13_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_5 > red_lift_x13_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_6 > red_lift_x13_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_7 > red_lift_x13_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_8 > red_lift_x13_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x13_9 > red_lift_x13_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_10 > red_lift_x13_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  
  TN_13 = (
    coalesce( if (f(green_lift_x13_1 < red_lift_x13_1)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x13_2 < red_lift_x13_2)) as.numeric( { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_3 < red_lift_x13_3)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_4 < red_lift_x13_4)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_5 < red_lift_x13_5)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_6 < red_lift_x13_6)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_7 < red_lift_x13_7)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_8 < red_lift_x13_8)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x13_9 < red_lift_x13_9)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_10 < red_lift_x13_10)) as.numeric(  { sum(is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  TP_13 = (
    coalesce( if (f(green_lift_x13_1 > red_lift_x13_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x13_2 > red_lift_x13_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_3 > red_lift_x13_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_4 > red_lift_x13_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_5 > red_lift_x13_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_6 > red_lift_x13_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_7 > red_lift_x13_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_8 > red_lift_x13_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x13_9 > red_lift_x13_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_10 > red_lift_x13_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  FN_13 = (
    coalesce( if (f(green_lift_x13_1 < red_lift_x13_1)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_1"),])), (cbind(targets_raw)))))  } ), 0) +
      coalesce( if (f(green_lift_x13_2 < red_lift_x13_2)) as.numeric( { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_2"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_3 < red_lift_x13_3)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_3"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_4 < red_lift_x13_4)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_4"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_5 < red_lift_x13_5)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_5"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_6 < red_lift_x13_6)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_6"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_7 < red_lift_x13_7)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_7"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_8 < red_lift_x13_8)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_8"),])), (cbind(targets_raw))))) } ), 0)+
      coalesce( if (f(green_lift_x13_9 < red_lift_x13_9)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_9"),])), (cbind(targets_raw)))))  } ), 0)+
      coalesce( if (f(green_lift_x13_10 < red_lift_x13_10)) as.numeric(  { sum(!is.na(match(unname(cbind(target_variable[which(irisDisc13 == "x13_10"),])), (cbind(targets_raw)))))  } ), 0)
  )
  
  
  x_13_table <<- as.data.frame.matrix(rbind(c(green_perc_x13_1 * summary(irisDisc13)[1], green_perc_x13_2 * summary(irisDisc13)[2],green_perc_x13_3 * summary(irisDisc13)[3],
                                              green_perc_x13_4 * summary(irisDisc13)[4], green_perc_x13_5 * summary(irisDisc13)[5],green_perc_x13_6 * summary(irisDisc13)[6],
                                              green_perc_x13_7 * summary(irisDisc13)[7], green_perc_x13_8 * summary(irisDisc13)[8],green_perc_x13_9 * summary(irisDisc13)[9],
                                              green_perc_x13_10 * summary(irisDisc13)[10]),
                                            c(red_perc_x13_1 * summary(irisDisc13)[1], red_perc_x13_2 * summary(irisDisc13)[2],red_perc_x13_3 * summary(irisDisc13)[3],
                                              red_perc_x13_4 * summary(irisDisc13)[4], red_perc_x13_5 * summary(irisDisc13)[5],red_perc_x13_6 * summary(irisDisc13)[6],
                                              red_perc_x13_7 * summary(irisDisc13)[7], red_perc_x13_8 * summary(irisDisc13)[8],red_perc_x13_9 * summary(irisDisc13)[9],
                                              red_perc_x13_10 * summary(irisDisc13)[10])))
  
  
  
  Precision_13 = TP_13 / (TP_13 + FP_13); Precision_13 = coalesce(Precision_13, 0)
  Recall_13 = TP_13 / (TP_13 + FN_13); Recall_13 = coalesce(Recall_13, 0)
  F1_Score_13 = coalesce ( 2 * (Precision_13 * Recall_13) / (Precision_13 + Recall_13), 0)
  MCC_13 = (TP_13 * TN_13 - FP_13 * FN_13) / sqrt ( (TP_13 + FP_13) * (TP_13 + FN_13) * (TN_13 + FP_13) * (TN_13 + FN_13) )
  MCC_adjusted_13 = (MCC_13 + 1) / 2
  P_four_13 <<- (4 * TP_13 * TN_13) / (4 * TP_13 * TN_13 + (TP_13 + TN_13) * (FP_13 + FN_13)); P_four_13 = coalesce(P_four_13, 0)
  
  
  par(mfrow = c(2, 1), mai = c(1, 0.5, 1, 2))
  pp13a = as.grob(
    
    expression(barplot(ylab="Lift (%)",
                       c(if (red_lift_x13_1 > green_lift_x13_1) {red_lift_x13_1} else {green_lift_x13_1},
                         if (red_lift_x13_2 > green_lift_x13_2) {red_lift_x13_2} else {green_lift_x13_2},
                         if (red_lift_x13_3 > green_lift_x13_3) {red_lift_x13_3} else {green_lift_x13_3},
                         if (red_lift_x13_4 > green_lift_x13_4) {red_lift_x13_4} else {green_lift_x13_4},
                         if (red_lift_x13_5 > green_lift_x13_5) {red_lift_x13_5} else {green_lift_x13_5},
                         if (red_lift_x13_6 > green_lift_x13_6) {red_lift_x13_6} else {green_lift_x13_6},
                         if (red_lift_x13_7 > green_lift_x13_7) {red_lift_x13_7} else {green_lift_x13_7},
                         if (red_lift_x13_8 > green_lift_x13_8) {red_lift_x13_8} else {green_lift_x13_8},
                         if (red_lift_x13_9 > green_lift_x13_9) {red_lift_x13_9} else {green_lift_x13_9},
                         if (red_lift_x13_10 > green_lift_x13_10) {red_lift_x13_10} else {green_lift_x13_10}),
                       col = c(
                         if (red_lift_x13_1 > green_lift_x13_1) {"red"} else {"green"},
                         if (red_lift_x13_2 > green_lift_x13_2) {"red"} else {"green"},
                         if (red_lift_x13_3 > green_lift_x13_3) {"red"} else {"green"},
                         if (red_lift_x13_4 > green_lift_x13_4) {"red"} else {"green"},
                         if (red_lift_x13_5 > green_lift_x13_5) {"red"} else {"green"},
                         if (red_lift_x13_6 > green_lift_x13_6) {"red"} else {"green"},
                         if (red_lift_x13_7 > green_lift_x13_7) {"red"} else {"green"},
                         if (red_lift_x13_8 > green_lift_x13_8) {"red"} else {"green"},
                         if (red_lift_x13_9 > green_lift_x13_9) {"red"} else {"green"},
                         if (red_lift_x13_10 > green_lift_x13_10) {"red"} else {"green"}
                       )
    )
    ,text(x=barplot(ylab="Lift (%)",  cex.names = 0.7, #main = unique(overall_settings(13)$Tag), cex.main = 0.7,
                    c(if (red_lift_x13_1 > green_lift_x13_1) {red_lift_x13_1} else if (red_lift_x13_1 < green_lift_x13_1) {green_lift_x13_1} else {green_lift_x13_1},
                      if (red_lift_x13_2 > green_lift_x13_2) {red_lift_x13_2} else if (red_lift_x13_2 < green_lift_x13_2) {green_lift_x13_2} else {green_lift_x13_2},
                      if (red_lift_x13_3 > green_lift_x13_3) {red_lift_x13_3} else if (red_lift_x13_3 < green_lift_x13_3) {green_lift_x13_3} else {green_lift_x13_3},
                      if (red_lift_x13_4 > green_lift_x13_4) {red_lift_x13_4} else if (red_lift_x13_4 < green_lift_x13_4) {green_lift_x13_4} else {green_lift_x13_4},
                      if (red_lift_x13_5 > green_lift_x13_5) {red_lift_x13_5} else if (red_lift_x13_5 < green_lift_x13_5) {green_lift_x13_5} else {green_lift_x13_5},
                      if (red_lift_x13_6 > green_lift_x13_6) {red_lift_x13_6} else if (red_lift_x13_6 < green_lift_x13_6) {green_lift_x13_6} else {green_lift_x13_6},
                      if (red_lift_x13_7 > green_lift_x13_7) {red_lift_x13_7} else if (red_lift_x13_7 < green_lift_x13_7) {green_lift_x13_7} else {green_lift_x13_7},
                      if (red_lift_x13_8 > green_lift_x13_8) {red_lift_x13_8} else if (red_lift_x13_8 < green_lift_x13_8) {green_lift_x13_8} else {green_lift_x13_8},
                      if (red_lift_x13_9 > green_lift_x13_9) {red_lift_x13_9} else if (red_lift_x13_9 < green_lift_x13_9) {green_lift_x13_9} else {green_lift_x13_9},
                      if (red_lift_x13_10 > green_lift_x13_10) {red_lift_x13_10} else if (red_lift_x13_10 < green_lift_x13_10) {green_lift_x13_10} else {green_lift_x13_10}),
                    col = c(
                      if (red_lift_x13_1 > green_lift_x13_1) {"red"} else {"green"},
                      if (red_lift_x13_2 > green_lift_x13_2) {"red"} else {"green"},
                      if (red_lift_x13_3 > green_lift_x13_3) {"red"} else {"green"},
                      if (red_lift_x13_4 > green_lift_x13_4) {"red"} else {"green"},
                      if (red_lift_x13_5 > green_lift_x13_5) {"red"} else {"green"},
                      if (red_lift_x13_6 > green_lift_x13_6) {"red"} else {"green"},
                      if (red_lift_x13_7 > green_lift_x13_7) {"red"} else {"green"},
                      if (red_lift_x13_8 > green_lift_x13_8) {"red"} else {"green"},
                      if (red_lift_x13_9 > green_lift_x13_9) {"red"} else {"green"},
                      if (red_lift_x13_10 > green_lift_x13_10) {"red"} else {"green"}
                    ),
                    names.arg=c(
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[2],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[3],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[4],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[5],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[6],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[7],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[8],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[9],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[10],1)),
                      paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[11],1))
                    ),
    ),   title(paste(unique(settings(13)$Tag), expression("-> {"), round(100*P_four_13, 0), expression("} <-") ), adj = 0.4, line = 1.2, cex.main = 0.7),
    y= (c(if (red_lift_x13_1 > green_lift_x13_1) {red_lift_x13_1} else {green_lift_x13_1},
          if (red_lift_x13_2 > green_lift_x13_2) {red_lift_x13_2} else {green_lift_x13_2},
          if (red_lift_x13_3 > green_lift_x13_3) {red_lift_x13_3} else {green_lift_x13_3},
          if (red_lift_x13_4 > green_lift_x13_4) {red_lift_x13_4} else {green_lift_x13_4},
          if (red_lift_x13_5 > green_lift_x13_5) {red_lift_x13_5} else {green_lift_x13_5},
          if (red_lift_x13_6 > green_lift_x13_6) {red_lift_x13_6} else {green_lift_x13_6},
          if (red_lift_x13_7 > green_lift_x13_7) {red_lift_x13_7} else {green_lift_x13_7},
          if (red_lift_x13_8 > green_lift_x13_8) {red_lift_x13_8} else {green_lift_x13_8},
          if (red_lift_x13_9 > green_lift_x13_9) {red_lift_x13_9} else {green_lift_x13_9},
          if (red_lift_x13_10 > green_lift_x13_10) {red_lift_x13_10} else {green_lift_x13_10}))+4,
    labels=as.character(sapply(c(if (red_lift_x13_1 > green_lift_x13_1) {red_lift_x13_1} else {green_lift_x13_1},
                                 if (red_lift_x13_2 > green_lift_x13_2) {red_lift_x13_2} else {green_lift_x13_2},
                                 if (red_lift_x13_3 > green_lift_x13_3) {red_lift_x13_3} else {green_lift_x13_3},
                                 if (red_lift_x13_4 > green_lift_x13_4) {red_lift_x13_4} else {green_lift_x13_4},
                                 if (red_lift_x13_5 > green_lift_x13_5) {red_lift_x13_5} else {green_lift_x13_5},
                                 if (red_lift_x13_6 > green_lift_x13_6) {red_lift_x13_6} else {green_lift_x13_6},
                                 if (red_lift_x13_7 > green_lift_x13_7) {red_lift_x13_7} else {green_lift_x13_7},
                                 if (red_lift_x13_8 > green_lift_x13_8) {red_lift_x13_8} else {green_lift_x13_8},
                                 if (red_lift_x13_9 > green_lift_x13_9) {red_lift_x13_9} else {green_lift_x13_9},
                                 if (red_lift_x13_10 > green_lift_x13_10) {red_lift_x13_10} else {green_lift_x13_10}),round))
    )))
  
  
  
  pp13b = as.grob(expression(barplot(as.matrix(x_13_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, xlim= c(attr(x = irisDisc13, which = "discretized:breaks")[1],attr(x = irisDisc13, which = "discretized:breaks")[11])),
                             text(x=barplot(as.matrix(x_13_table),col=c("darkgreen","firebrick4"),cex.names = 0.7, ylab="Observations", names.arg=c(
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[1],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[2],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[2],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[3],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[3],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[4],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[4],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[5],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[5],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[6],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[6],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[7],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[7],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[8],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[8],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[9],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[9],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[10],1)),
                               paste0(round(attr(x = irisDisc13, which = "discretized:breaks")[10],1),"↔",round(attr(x = irisDisc13, which = "discretized:breaks")[11],1))
                             )),
                             y= (summary(irisDisc13))+20, labels=as.character((summary(irisDisc13))))
                             
  ))
  
  
  num13 = grid.arrange(grobs=list(as.ggplot(pp13a),as.ggplot(pp13b)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  P_four_unordered = cbind.data.frame(P_four = c("P_four_1","P_four_2","P_four_3","P_four_4","P_four_5","P_four_6","P_four_7","P_four_8","P_four_9","P_four_10","P_four_11","P_four_12","P_four_13"),
                                      P_four_Score =  c(P_four_1,P_four_2,P_four_3,P_four_4,P_four_5,P_four_6,P_four_7,P_four_8,P_four_9,P_four_10,P_four_11,P_four_12,P_four_13))
  
  
  
  P_four_unordered = cbind.data.frame(P_four = c("P_four_1","P_four_2","P_four_3","P_four_4","P_four_5","P_four_6","P_four_7","P_four_8","P_four_9","P_four_10","P_four_11","P_four_12","P_four_13"),
                                      P_four_Score =  c(P_four_1,P_four_2,P_four_3,P_four_4,P_four_5,P_four_6,P_four_7,P_four_8,P_four_9,P_four_10,P_four_11,P_four_12,P_four_13))
  
  
  P_four_ordered = P_four_unordered[order(P_four_unordered$P_four_Score,decreasing = T),]
  
  
  
  # ordered = unordered[order(unordered$Score,decreasing = T),]
  # grid.arrange(grobs=PLOTS[as.numeric(rownames(ordered))[1:10]],ncol=5)
  # rownames(ordered)
  # as.list(as.numeric(rownames(ordered)))
  # as.numeric(rownames(ordered))
  # as.list(as.numeric(rownames(ordered)))[1:10]
  
  
  PLOTS = list(num1, num2, num3, num4, num5, num6, num7, num8, num9, num10, num11, num12, num13)
  
  PLOTS_lifts = list(num1[1], num2[1], num3[1], num4[1], num5[1], num6[1], num7[1], num8[1], num9[1], num10[1], num11[1], num12[1], num13[1])
  
  P_four_ordered = P_four_unordered[order(P_four_unordered$P_four_Score,decreasing = T),]
  grid.arrange(grobs=PLOTS[as.numeric(rownames(P_four_ordered))[1:13]],ncol=5)
  grid.arrange(grobs=PLOTS[as.numeric(rownames(P_four_ordered))[1:13]],ncol=5)
  grid.arrange(grobs=PLOTS_lifts[as.numeric(rownames(P_four_ordered))[1:13]],ncol=5)
  grid.arrange(grobs=PLOTS_lifts[as.numeric(rownames(P_four_ordered))[1:13]],ncol=5)
  #grid.arrange(grobs=PLOTS_lifts[as.numeric(rownames(P_four_ordered))[1:30]],ncol=6)
  
  head(P_four_ordered,35)   
  P_four_unordered[which(is.nan(P_four_unordered$P_four_Score)),];     which(P_four_unordered$P_four_Score > 0.8)  -> important; length(important)
  
  # grid.arrange(grobs=PLOTS[1:10],ncol=5)
  # grid.arrange(grobs=PLOTS[as.numeric(rownames(ordered))[1:10]],ncol=5)
  
  
  settings(10)
  
  library("stringr"); library(ivmte)
  str_sub(P_four_ordered$P_four, 8, - 1)
  P_four_ordered$tables = as.numeric(str_sub(P_four_ordered$P_four, 8, - 1))
  P_four_ordered$Tag = unique(overall_settings(rownames(P_four_ordered))$Tag)
  
  
  green_perc_x10_10
  
  
  greens_tables = function(P_four_ordered) {
    for (i in list(head(P_four_ordered$tables,13))) { 
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
    for (i in list(head(P_four_ordered$tables,13))) { 
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
  
  
  
  
  
  
  for (i in as.numeric(str_sub(head(P_four_ordered$P_four,13), 8, -1))) {
    print( c (
      (parse(text = levels(as.factor(paste0("green_perc_x",i,"_1"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_2"))))),
      (parse(text = levels(as.factor(paste0("green_perc_x",i,"_3"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_4"))))),
      (parse(text = levels(as.factor(paste0("green_perc_x",i,"_5"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_6"))))), 
      (parse(text = levels(as.factor(paste0("green_perc_x",i,"_7"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_8"))))),
      (parse(text = levels(as.factor(paste0("green_perc_x",i,"_9"))))), (parse(text = levels(as.factor(paste0("green_perc_x",i,"_10"))))))
    )
  }
  
  
  
  
  percents = c()
  for (i in as.numeric(str_sub(head(P_four_ordered$P_four,13), 8, -1))) {
    for (j in i) {
      print( c (
        cbind(paste0("green_perc_x",j,"_1"), paste0("green_perc_x",j,"_2"), paste0("green_perc_x",j,"_3"), paste0("green_perc_x",j,"_4"), paste0("green_perc_x",j,"_5"),
              paste0("green_perc_x",j,"_6"), paste0("green_perc_x",j,"_7"), paste0("green_perc_x",j,"_8"), paste0("green_perc_x",j,"_9"), paste0("green_perc_x",j,"_10")))
      )
    }
  }
  
  
  
  
  percents = c()
  green_table = function() {
    for (i in as.numeric(str_sub(head(P_four_ordered$P_four,13), 8, -1))) {
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
                          if  (exists(as.name(green_table()[109]))) {eval(parse(text = green_table()[99]))}, if (exists(as.name(green_table()[110]))) {eval(parse(text = green_table()[110]))}  ) )
  
  
  
  
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
  
  
  
  
  
  
  length(levels(irisDisc4))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  optimal_settings = rbind.data.frame(
    c( {overall_settings_2(rownames(head(P_four_ordered,13)))[1:10,][best_1,]},
       if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[1:10])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[1:10,][best_1,][2]) ) {
         paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[1:10,][best_1,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[1:10])]), 1))),collapse="")
       } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[1:10])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[1:10,][best_1,][3])) {
         as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[1:10,][best_1,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[1:10])]), 1))
       } else {
         0
       },
       as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[1:10])]), 1))),
    
    
    c( {overall_settings_2(rownames(head(P_four_ordered,13)))[11:20,][best_2,]},
       if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[11:20])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[11:20,][best_2,][2]) ) {
         paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[11:20,][best_2,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[11:20])]), 1))),collapse="")
       } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[11:20])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[11:20,][best_2,][3])) {
         as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[11:20,][best_2,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[11:20])]), 1))
       } else {
         0
       },
       as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[11:20])]), 1))),
    
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[21:30,][best_3,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[21:30,][best_3,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[21:30,][best_3,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[21:30,][best_3,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[21:30,][best_3,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[21:30])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[31:40,][best_4,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[31:40,][best_4,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[31:40,][best_4,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[31:40,][best_4,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[31:40,][best_4,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[31:40])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[41:50,][best_5,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50]))) %in% as.vector((colnames(iris)))) 
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[41:50,][best_5,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[41:50,][best_5,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[41:50,][best_5,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[41:50,][best_5,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[41:50])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[51:60,][best_6,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[51:60,][best_6,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[51:60,][best_6,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[51:60,][best_6,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[51:60,][best_6,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[51:60])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[61:70,][best_7,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[61:70,][best_7,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[61:70,][best_7,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[61:70,][best_7,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[61:70,][best_7,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[61:70])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[71:80,][best_8,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[71:80,][best_8,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[71:80,][best_8,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[71:80,][best_8,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[71:80,][best_8,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[71:80])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[81:90,][best_9,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[81:90,][best_9,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[81:90,][best_9,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[81:90,][best_9,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[81:90,][best_9,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[81:90])]), 1))),
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[91:100,][best_10,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[91:100,][best_10,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[91:100,][best_10,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[91:100,][best_10,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[91:100,][best_10,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[91:100])]), 1))),
    
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[101:110,][best_11,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[101:110,][best_11,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[101:110,][best_11,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[101:110,][best_11,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[101:110,][best_11,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[101:110])]), 1))),
    
    
    
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[111:120,][best_12,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[111:120,][best_12,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[111:120,][best_12,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[111:120,][best_12,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[111:120,][best_12,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[111:120])]), 1))),
    
    
    
    
    
    
    c ( {overall_settings_2(rownames(head(P_four_ordered,13)))[121:130,][best_13,]},
        if ((as.vector (unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130]))) %in% as.vector((colnames(iris))))
          if (as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130])]), 1)) < as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[121:130,][best_13,][2]) ) {
            paste(c("+",as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[121:130,][best_13,][2]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130])]), 1))),collapse="")
          } else if ( as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130])]), 1)) > as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[121:130,][best_13,][3])) {
            as.numeric(overall_settings_2(rownames(head(P_four_ordered,13)))[121:130,][best_13,][3]) - as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130])]), 1))
          } else {
            0
          },
        as.numeric(tail(na.omit(iris[,unique(overall_settings_2(rownames(head(P_four_ordered,13)))$Tag[121:130])]), 1)))
    
    
    
  )
  
  
  
  colnames(optimal_settings) <- c("Level", "Optimal Minimum", "Optimal Maximum", "Tag", "Recommended Changes", "Actual Values")
  
  
  
  optimal_settings[,2] = sapply(optimal_settings[,2], as.numeric); optimal_settings[,3] = sapply(optimal_settings[,3], as.numeric); optimal_settings[,5] = sapply(optimal_settings[,5], as.numeric); 
  optimal_settings <- optimal_settings %>%
    mutate(across(where(is.numeric), ~ round(.,2)))
  func_for_appending_plus_sign = function (x) {ifelse(x > 0, paste(c("↑",x),collapse=" "), paste(c("↓",x*(-1)),collapse=" "))}
  #optimal_settings[5] = sapply(unlist(sapply(optimal_settings[5],as.numeric)), func_for_appending_plus_sign)
  func_for_replacing_downward_arrow_sign_and_zero = function (x) {ifelse(x == "↓ 0", replace(x, 1, 0), x)}   # df <- c('apple', 'orange', 'grape', 'banana')  #replace(df, 2, 'blueberry')
  #optimal_settings[5] = sapply(unlist(optimal_settings[5]), func_for_replacing_downward_arrow_sign_and_zero)
  optimal_settings = optimal_settings[,-c(1)]
  optimal_settings = optimal_settings[,c("Actual Values", "Recommended Changes", "Tag", "Optimal Minimum", "Optimal Maximum")]
  optimal_settings = rbind(
    c(round(tail(iris[,c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")], 1), 2),
      if (tail(iris[,c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")], 1) < 80.5 ) {
        paste(c("+",round(80.5 - tail(iris[,c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")], 1),2) ),collapse="")
      } else if ( tail(iris[,c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")], 1) > 82.5) {
        round(82.5 - tail(iris[,c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV")], 1), 2)
      } else {
        0
      },
      unique(settings(14)$Tag),
      80.5,
      82.5),
    optimal_settings)
  optimal_settings[2] = sapply(unlist(sapply(optimal_settings[2],as.numeric)), func_for_appending_plus_sign)
  optimal_settings[2] = sapply(unlist(optimal_settings[2]), func_for_replacing_downward_arrow_sign_and_zero)
  optimal_settings
  
  
  optimal_settings = as.data.frame(optimal_settings)
  rownames(optimal_settings) <- NULL
  
  
  #return(as.data.frame(optimal_settings))
  
  
  
  
  print(1047)
  
  
  #library(flextable)
  #library(knitr)
  # displaying the table
  #kable(optimal_settings, format="pipe", padding=1, align="c")
  
  print(1055)
  print(getwd())
  print(typeof(combined[,1]))
  print(names(combined))
  print(head(combined))
  print(tail(combined))
  #return(list(as.data.frame(ts),data.frame(combined[,2])))
  #return(as.data.frame(c(as.character(combined[1]),combined[2])))
  #print(warnings())
  combined[,1]<-format(as.POSIXct(combined[,1], format="%Y-%m-%d %H:%M:%S")-(6*60*60), "%Y-%m-%d %H:%M:%S")
  combined[,1]<-as.character(combined[,1])
  combined = cbind(combined, UCL = mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), LCL = mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))))
  total_scoring_combined[,1]<-as.character(total_scoring_combined[,1]) #  scoring_combined[,1]<-as.character(scoring_combined[,1])
  #scoring_combined <- cbind ( tail(scoring_combined, 24),  tail(head(combined, 96), 24)  )
  total_scoring_combined <- cbind ( tail(head(combined, 96), 96), tail(total_scoring_combined, 96)    )  #  scoring_combined <- cbind ( tail(head(combined, 96), 24), tail(scoring_combined, 24)    )
  #scoring_combined[c(5)] <- scoring_combined[c(1)]
  total_scoring_combined[,1]<-format(as.POSIXct(total_scoring_combined[,1], format="%Y-%m-%d %H:%M:%S")+(6*60*60), "%Y-%m-%d %H:%M:%S") # scoring_combined[,1]<-format(as.POSIXct(scoring_combined[,1], format="%Y-%m-%d %H:%M:%S")+(6*60*60), "%Y-%m-%d %H:%M:%S")
  total_scoring_combined[c(3)] <- rep(82.5, 24) # scoring_combined[c(3)] <- rep(82.5, 24)
  total_scoring_combined[c(4)] <- rep(80.5, 24) # scoring_combined[c(4)] <- rep(80.5, 24)
  colnames(total_scoring_combined) = c("ts", "Actual", "Upper Specification Limit", "Lower Specification Limit", "scoring_ts", "Predicted") # colnames(scoring_combined) = c("ts", "Actual", "Upper Specification Limit", "Lower Specification Limit", "scoring_ts", "Predicted")
  #return(head(as.data.frame(as.character(ts))))
  #return(combined)
  return(list(combined, optimal_settings, total_scoring_combined)) # return(list(combined, optimal_settings, scoring_combined))
  #return(list(tail(head(combined, 96), 24), scoring_combined, UCL = mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), LCL = mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96))))))
  
}



