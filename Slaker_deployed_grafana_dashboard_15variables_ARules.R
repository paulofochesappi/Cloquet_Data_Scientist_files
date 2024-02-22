

rm_main = function() {
  
  
  
  setwd(getwd())
  
  end_time = format(Sys.time(), "%Y%m%d_%H%M%S")
  start_time =    format(as.POSIXct(end_time, format="%Y%m%d_%H%M%S")-(120*24*60*60), "%Y%m%d_%H%M%S") #"20230421_063500" format
  delta = as.numeric(as.Date(end_time, "%Y%m%d_%H%M%S") - as.Date(start_time, "%Y%m%d_%H%M%S"))
  
  
  file_path <- getwd()
  file_to_load = "variable_names.RData"
  
  
  if (file.exists(file_to_load)) {
    load(paste0(file_path,"/",file_to_load))
  } else {
    
    
    
    
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
    print(3535353535353535)
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
    
    
    save(df_categorical, df_datetime, df_memory, df_numerical, df_variables, json.content, timestamp_var, memory_id, millpath, token, file = "variable_names.RData")
  }
  
  
  
  
  
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
                          'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  
  print(226)
  
  
  #library(RKEEL)
  library(readxl)
  library(arules)
  #library(corrplot)
  library(arules)
  #library(arulesViz)
  library(arulesCBA)
  #library(readxl)
  #library(qcc)
  library(dplyr)
  
  
  
  #library(corrplot)
  #library(ggcorrplot)
  #corr_data_group=cor((na.omit(sapply(data_group[,!names(data_group) %in% c("565MK1104.PV | SLAKER PUMP", 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request')], as.numeric))),method="s")  # eliminating unwanted variables
  #corr_data_group <- corr_data_group[, c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12)]
  #colnames(corr_data_group) = c("Metra 4C Causticizing Degree %","SDT RGL AA","GL->SLAKER","PURCHASED LIME","REBURN LIME","SLAKER BOWL","GL Temp at #2","PURCH/REBURN",
  #                              "Lime to GL Ratio Controller","CGL Sulfidity S%","CGL TTA","SlakerCausticizing Degree")
  #corr_data_group = t(corr_data_group)
  #corr_data_group <- corr_data_group[, c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12)]
  #colnames(corr_data_group) = c("Metra 4C","SDT RGL AA","GL->SLAKER","PURCHASED LIME","REBURN LIME","SLAKER BOWL","GL Temp at #2","PURCH/REBURN",
  #                              "Lime/GL Ratio","CGL Sulfidity S%","CGL TTA","SlakerCausticizing %")
  #par(bg = 'darkgoldenrod3')
  #corrplot(corr_data_group,method = "pie", type="upper", tl.col="white", tl.cex=0.5, addgrid.col="black");   # Plotting the correlation
  #par(bg = 'white')
  
  #return(as.data.frame(corr_data_group))
  
  
  
  
  
  split_date = as.Date(end_time, "%Y%m%d_%H%M%S") - delta*0.3    # 80/20 Train/Test data split
  train_set <- data_set[as.Date(data_set$DateTime, "%Y%m%d_%H%M%S") < split_date,]
  test_set <- data_set[-(1:nrow(train_set)),]
  
  
  train = train_set[c('DateTime', 'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC094.PV | GL->SLAKER',
                      '565SIC1127.PV | PURCHASED LIME', '565SIC1128.PV | REBURN LIME', '565TIC103.PV | SLAKER BOWL',
                      'GL Temp at #2 || 565TIC096C.PV','565HY103H.PV | PURCH/REBURN', 
                      'Lime to GL Ratio Controller | 565FF1128.PV', 'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV', 
                      'METRA CGL Sulfidity S% | _565_CGL_S.PV', 'METRA CGL TTA | _565_CGL_TTA.PV', 
                      'METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV', 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request',
                      'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  train[,1] = as.POSIXct(train[,1], format="%Y%m%d_%H%M%S")
  train[,-1] = data.frame(lapply(train[,-1], function(x) as.numeric(as.character(x))),   # new
                          check.names=F, row.names = rownames(train[,-1]))   # new
  train[is.na(train)] = 0   # new
  #train = na.omit(train)
  
  test = test_set[c('DateTime', 'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC094.PV | GL->SLAKER',
                    '565SIC1127.PV | PURCHASED LIME', '565SIC1128.PV | REBURN LIME', '565TIC103.PV | SLAKER BOWL',
                    'GL Temp at #2 || 565TIC096C.PV','565HY103H.PV | PURCH/REBURN', 
                    'Lime to GL Ratio Controller | 565FF1128.PV', 'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV', 
                    'METRA CGL Sulfidity S% | _565_CGL_S.PV', 'METRA CGL TTA | _565_CGL_TTA.PV', 
                    'METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV', 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request',
                    'METRA SDT RGL AA | _561_RGL_AA.PV', '565FIC097.PV | GL->CLASS', '565LI061.PV | WL TANK')]
  test[,1] = as.POSIXct(test[,1], format="%Y%m%d_%H%M%S")
  test[,-1] = data.frame(lapply(test[,-1], function(x) as.numeric(as.character(x))),   # new
                         check.names=F, row.names = rownames(test[,-1]))   # new
  test[is.na(test)] = 0   # new
  #test = na.omit(test)
  print(284)
  
  library(dplyr)
  library(xgboost)
  train_matrix <- sapply(na.omit(train[,c(-1,-10)]), as.numeric)
  
  pred_matrix <- sapply(na.omit(test[,c(-1,-10)]), as.numeric)
  targets <- train$'METRA 4C Causticizing Degree % | _565_4CAU_CE.PV'
  targets_matrix <- na.omit(sapply(targets, as.numeric))
  
  
  
  library(caret)
  xgb_trcontrol <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    verboseIter = FALSE,
    returnData = FALSE
  )
  
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
  
  
  
  model_xgb <- caret::train(
    train_matrix,targets_matrix,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = "xgbTree",
    nthread = 10,
    verbosity = 0
  )
  
  
  
  
  
  fitted <- model_xgb %>%
    stats::predict(train_matrix)
  
  
  ts_METRA_4C_Causticizing_Degree <- train['DateTime']
  forecast_xgb <- model_xgb %>% stats::predict(pred_matrix)
  
  forecast_ts <- cbind(forecast_xgb,test['DateTime'])
  colnames(forecast_ts) <- c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", "DateTime")
  
  
  forecast_METRA_4C_Causticizing_Degree <- list(
    model = model_xgb$modelInfo,
    method = model_xgb$method,
    mean = forecast_ts,
    x = ts_METRA_4C_Causticizing_Degree,
    fitted = fitted,
    residuals = as.numeric(unlist(ts_METRA_4C_Causticizing_Degree)) - as.numeric(fitted)
  )
  
  
  
  date_transform <- function(x) {format(date_decimal(x), "%H")}
  
  
  observed_values <-  as.data.frame(cbind(test$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`,test['DateTime']))
  colnames(observed_values) <- c("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", "DateTime")
  
  
  #library(ggplot2)
  library(forecast)
  
  library(Ckmeans.1d.dp)
  xgb_imp <- xgb.importance(
    feature_names = colnames(train_matrix),
    model = model_xgb$finalModel)
  print(367)
  
  # Plotting the feature importance of XGBoost model
  xgb.ggplot.importance(xgb.importance(
    feature_names = colnames(train_matrix),
    model = model_xgb$finalModel),n_clusters = c(3)) + scale_fill_manual(values=c("#e5d0ff", "#bf8bff", 'purple')) +
    ggtitle("Priority List") +
    theme_bw()+
    theme(legend.position="none") + theme(panel.background = element_rect(fill = 'black', color = 'purple')) +
    theme(text = element_text(size = 8)) 
  
  #return(as.data.frame(xgb_imp))
  
  
  
  
  
  
  
  
  #library(rsconnect)
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
  
  
  # generating 20 lags
  result = VARselect(sapply(na.omit(df.lev), as.numeric), lag.max = 20,
                     type = "const", season = NULL)
  
  # selecting the optimal lag based on Akaike information criterion (AIC)
  var.model_lev <- VAR(sapply(na.omit(df.lev), as.numeric), p = result$selection['AIC(n)'], 
                       type = "const", season = NULL)
  
  # forecasting for nhor time horizons
  var.pred <- predict(var.model_lev, n.ahead = nhor)
  
  
  #library(bruceR)
  # plotting the time series trends
  #plot.new(); dev.control("enable");
  #layout(matrix(c(1,2,3,4,5,6,7,7,8,9,10,11), nrow = 6, ncol = 2, byrow = TRUE))
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,13,14,14), nrow = 5, ncol = 3, byrow = TRUE))
  par(mar = c(2, 3, 1, 1), bg = 'black');
  y=c(tail(as.numeric(df.lev$`GL Temp at #2 || 565TIC096C.PV`), n=96), var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),  
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`GL Temp at #2 || 565TIC096C.PV`)],
       y=c(as.numeric(df.lev$`GL Temp at #2 || 565TIC096C.PV`), var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])[1:length(df.lev$`GL Temp at #2 || 565TIC096C.PV`)],
       main="GL Temp at #2 || 565TIC096C.PV",col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.2, tail(sort(y),2)[1]+0.2),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1])], 
        var.pred$fcst$GL.Temp.at..2....565TIC096C.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  print(431)
  
  y=c(tail(as.numeric(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`), n=96), var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`)],
       y=c(as.numeric(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`), var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])[1:length(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`)],
       main="METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-2, tail(sort(y),2)[1]+2),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1])], 
        var.pred$fcst$METRA.SlakerCausticizing.Degree....._565_SLKR_CE.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`565HY103H.PV | PURCH/REBURN`), n=96), var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565HY103H.PV | PURCH/REBURN`)],
       y=c(as.numeric(df.lev$`565HY103H.PV | PURCH/REBURN`), var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])[1:length(df.lev$`565HY103H.PV | PURCH/REBURN`)],
       main="565HY103H.PV | PURCH/REBURN", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.5, tail(sort(y),2)[1]+0.5),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1])], 
        var.pred$fcst$X565HY103H.PV...PURCH.REBURN[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`565TIC103.PV | SLAKER BOWL`), n=96), var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565TIC103.PV | SLAKER BOWL`)],
       y=c(as.numeric(df.lev$`565TIC103.PV | SLAKER BOWL`), var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])[1:length(df.lev$`565TIC103.PV | SLAKER BOWL`)],
       main="565TIC103.PV | SLAKER BOWL", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-1, tail(sort(y),2)[1]+1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1])], 
        var.pred$fcst$X565TIC103.PV...SLAKER.BOWL[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`), n=96), var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`)],
       y=c(as.numeric(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`), var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])[1:length(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`)],
       main="Lime to GL Ratio Controller | 565FF1128.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=(c(head(sort(y),2)[2]-0.01, tail(sort(y),2)[1]+0.01)),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1])], 
        var.pred$fcst$Lime.to.GL.Ratio.Controller...565FF1128.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`565SIC1128.PV | REBURN LIME`), n=96), var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565SIC1128.PV | REBURN LIME`)],
       y=c(as.numeric(df.lev$`565SIC1128.PV | REBURN LIME`), var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])[1:length(df.lev$`565SIC1128.PV | REBURN LIME`)],
       main="565SIC1128.PV | REBURN LIME", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.1, tail(sort(y),2)[1]+0.1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1])], 
        var.pred$fcst$X565SIC1128.PV...REBURN.LIME[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  

  print(521)
  
  y=c(tail(as.numeric(df.lev$`565SIC1127.PV | PURCHASED LIME`), n=96), var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565SIC1127.PV | PURCHASED LIME`)],
       y=c(as.numeric(df.lev$`565SIC1127.PV | PURCHASED LIME`), var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])[1:length(df.lev$`565SIC1127.PV | PURCHASED LIME`)],
       main="565SIC1127.PV | PURCHASED LIME", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.05, tail(sort(y),2)[1]+0.05),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1])], 
        var.pred$fcst$X565SIC1127.PV...PURCHASED.LIME[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`), n=96), var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`)],
       y=c(as.numeric(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`), var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])[1:length(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`)],
       main="METRA CGL Sulfidity S% | _565_CGL_S.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.05, tail(sort(y),2)[1]+0.05),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1])], 
        var.pred$fcst$METRA.CGL.Sulfidity.S...._565_CGL_S.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`565FIC094.PV | GL->SLAKER`), n=96), var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565FIC094.PV | GL->SLAKER`)],
       y=c(as.numeric(df.lev$`565FIC094.PV | GL->SLAKER`), var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])[1:length(df.lev$`565FIC094.PV | GL->SLAKER`)],
       main="565FIC094.PV | GL->SLAKER", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-5, tail(sort(y),2)[1]+5),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1])], 
        var.pred$fcst$X565FIC094.PV...GL..SLAKER[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`), n=96), var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`)],
       y=c(as.numeric(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`), var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])[1:length(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`)],
       main="METRA CGL TTA | _565_CGL_TTA.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1])], 
        var.pred$fcst$METRA.CGL.TTA..._565_CGL_TTA.PV[,1], col = "blue", lwd=4)
  
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  y=c(tail(as.numeric(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`), n=96), var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`)],
       y=c(as.numeric(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`), var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1])[1:length(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`)],
       main="METRA SDT RGL AA | _561_RGL_AA.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.05, tail(sort(y),2)[1]+0.05),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1])], 
        var.pred$fcst$METRA.SDT.RGL.AA..._561_RGL_AA.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  y=c(tail(as.numeric(df.lev$`565FIC097.PV | GL->CLASS`), n=96), var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565FIC097.PV | GL->CLASS`)],
       y=c(as.numeric(df.lev$`565FIC097.PV | GL->CLASS`), var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])[1:length(df.lev$`565FIC097.PV | GL->CLASS`)],
       main="565FIC097.PV | GL->CLASS", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-5, tail(sort(y),2)[1]+5),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565FIC097.PV...GL..CLASS[,1])], 
        var.pred$fcst$X565FIC097.PV...GL..CLASS[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  
  
  y=c(tail(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`), n=96), var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`)],
       y=c(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`), var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])[1:length(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`)],
       main="METRA 4C Causticizing Degree % | _565_4CAU_CE.PV", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),2)[2]-0.1, tail(sort(y),2)[1]+0.1),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)))
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])], 
        var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1], col = "blue", lwd=4)
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  
  
  y=c(tail(as.numeric(df.lev$`565LI061.PV | WL TANK`), n=96), var.pred$fcst$X565LI061.PV...WL.TANK[,1])
  plot(c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))), 
         x=seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60))[1:length(df.lev$`565LI061.PV | WL TANK`)],
       y=c(as.numeric(df.lev$`565LI061.PV | WL TANK`), var.pred$fcst$X565LI061.PV...WL.TANK[,1])[1:length(df.lev$`565LI061.PV | WL TANK`)],
       main="565LI061.PV | WL TANK", col.main='white', col.axis='white', axes=T, type="l", col="white", lwd=2, xlab="", ylab="",cex.axis=0.5,cex.main=0.5,
       ylim=c(head(sort(y),1)[1]-0.01, tail(sort(y),1)[1]+0.01),
       xlim=c(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S"))), tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1))) #, format = "%m-%d %H:%M", 
  lines(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60)[1:
                                                                                                                                                 length(var.pred$fcst$X565LI061.PV...WL.TANK[,1])], 
        var.pred$fcst$X565LI061.PV...WL.TANK[,1], col = "blue", lwd=4)
  
  
  axis(1,col="yellow", labels=F, at = as_datetime(seq(as_datetime(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)-96]), format="%Y%m%d_%H%M%S")))), as_datetime(tail(seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S"))), length.out=nhor, by = 30*60), n=1)), by = 3*60*60)));
  axis(2,col="yellow", labels=F);box(col="yellow");
  
  
  
  
  
  #return(as.data.frame(data_set))
  #return(as.data.frame(df.lev))
  #return(as.list.data.frame(var.pred))
  
  
  
  
  
  
  
  print(593)
  
  
  
  ts = c(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=96),
         seq(from=as.POSIXct((datetime2=as.POSIXct((data_set$DateTime[nrow(data_set)]), format="%Y%m%d_%H%M%S")+30*60)), length.out=nhor, by = 30*60)[1:
                                                                                                                                                        length(var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1])])
  
  projected = c(tail(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),n=96),
                var.pred$fcst$METRA.4C.Causticizing.Degree....._565_4CAU_CE.PV[,1]); 
  #par(bg = 'darkseagreen1')
  
  combined = cbind(as.data.frame(ts),projected)
  
#colours <- c("FALSE" = "white", "TRUE" = "dodgerblue")
  
  #library(scales); library(ggtext); library(ggplot2); library(plotly)
  # plotting the METRA 4C Causticizing Degree % forecast
  #ggplotly(
  #ggplot(data=combined[,1:2],aes(ts, projected, width =6, color=(ts>as.POSIXct((datetime2=as.POSIXct(tail(as.POSIXct((datetime2=as.POSIXct((data_set$DateTime), format="%Y%m%d_%H%M%S"))),n=1), format="%Y%m%d_%H%M%S")))))) + geom_line(aes(group=1, colour="FALSE")) + theme(legend.position = "none") + scale_color_manual(values=c("white","dodgerblue")) + geom_point(shape = 1, size = 3) + 
  #  geom_line(linewidth=1) + geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=1,color = "red") + 
  #  geom_hline(yintercept=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))), linetype="solid", linewidth=1,color = "red") + geom_hline(yintercept = c(80.5,82.5), linetype="dashed", color="green", linewidth=1) +
  #  ggtitle("Past 3 days plus 12-hour Forecast") + ylab("Metra 4C Causticizing Degree %") + xlab("Date") + ylim(mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))) - 0.4, mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.4) +
  #  geom_richtext(aes(ts, projected, label = paste("<b>OOC!</b>")), nudge_y=-0.3, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) | abs(na.omit(head(combined[,2], n=96))) < mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,2)), ],angle=-45,size=3, color = 'orange') +
  #  geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")),  nudge_y=+0.5, nudge_x=+0.5, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n=120))) > mean(rep(82.5,length(ts))) & abs(na.omit(head(combined[,2], n=96))) < mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=3, color = 'yellow') +
  #  geom_richtext(aes(ts, projected, label = paste("<b>OOS!</b>")), nudge_y=-0.1, nudge_x=+30*60, fill = NA, label.color = NA, data = combined[abs((head(combined[,2], n= 120))) < mean(rep(80.5,length(ts))) & abs(na.omit(head(combined[,2], n=96))) > mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n=96)))),][c(TRUE,rep(FALSE,3)), ],angle=45,size=3, color = 'yellow') +
  #  theme(panel.background = element_rect(fill = 'gray4', color = 'purple'),
  #        panel.grid.major = element_line(color = 'lightblue'),
  #        panel.grid.minor = element_line(color = 'lightblue', linetype = 'dotted', size = 0.5)) +
  #  theme(text = element_text(size = 10), axis.text.x=element_text(angle=90, vjust = 0.3, hjust = 1)) +
  #  annotate("text", label = "Upper Spec Limit", size=2.5, x=ts[5],y=82.5+0.2, color="green") + annotate("text", label = "Lower Spec Limit", size=2.5, x=ts[5],y=80.5-0.2, color="green") +
  #  annotate("text", label = "UCL", size=2.5, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) + (3 * sd(na.omit(head(combined[,2], n=96)))) + 0.3, color="red") + annotate("text", label = "LCL", size=2.5, x=ts[length(ts)],y=mean(na.omit(head(combined[,2], n=96))) - (3 * sd(na.omit(head(combined[,2], n= 96)))) - 0.3, color="red") +
  #  scale_x_datetime(breaks = breaks_width("3 hours"), minor_breaks = date_breaks("1 hour"), date_labels = "%b %d     %H:%M")
  #)
  
  
  #return(as.data.frame(combined))
  
  
  
  
  
  
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
  
  
  
  
  print(689)
  
  
  
  
  
  
  #library(flextable)
  #library(knitr)
  # displaying the table
  #kable(Granger_Causality_Ranking, format="pipe", padding =1, align="c")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #library(flextable)
  library(arules)
  #library(arulesViz)
  library(arulesCBA)
  #library(readxl)
  
  # performing Association Rules analysis
  iris = na.omit(sapply(data_group[,!names(data_group) %in% c("565MK1104.PV | SLAKER PUMP", 'Slaker Metra Control In Use || _565_SLKR_CE.Ctrl_Request')], as.numeric))
  
  # discretizing the variables
  set.seed(1); irisDisc1 <- (discretize(iris[,1], method = "cluster", breaks = 10,
                           labels = c("x1_1", "x1_2","x1_3","x1_4","x1_5","x1_6","x1_7","x1_8","x1_9","x1_10")))
  set.seed(1); irisDisc2 <- (discretize(iris[,2], method = "cluster", breaks = 10,
                           labels = c("x2_1", "x2_2","x2_3","x2_4","x2_5","x2_6","x2_7","x2_8","x2_9","x2_10")))
  set.seed(1); irisDisc3 <- (discretize(iris[,3], method = "cluster", breaks = 10,
                                        labels = c("x3_1", "x3_2","x3_3","x3_4","x3_5","x3_6","x3_7","x3_8","x3_9","x3_10")))
  set.seed(1); irisDisc4 <- (discretize(iris[,4], method = "cluster", breaks = 10,
                           labels = c("x4_1", "x4_2","x4_3","x4_4","x4_5","x4_6","x4_7","x4_8","x4_9","x4_10")))
  set.seed(1); irisDisc5 <- (discretize(iris[,5], method = "cluster", breaks = 10,
                           labels = c("x5_1", "x5_2","x5_3","x5_4","x5_5","x5_6","x5_7","x5_8","x5_9","x5_10")))
  set.seed(1); irisDisc6 <- (discretize(iris[,6], method = "cluster", breaks = 10,
                           labels = c("x6_1", "x6_2","x6_3","x6_4","x6_5","x6_6","x6_7","x6_8","x6_9","x6_10")))
  set.seed(1); irisDisc7 <- (discretize(iris[,7], method = "interval", breaks = 10,
                           labels = c("x7_1", "x7_2","x7_3","x7_4","x7_5","x7_6","x7_7","x7_8","x7_9","x7_10")))
  set.seed(1); irisDisc8 <- (discretize(iris[,8], method = "cluster", breaks = 10,
                           labels = c("x8_1", "x8_2","x8_3","x8_4","x8_5","x8_6","x8_7","x8_8","x8_9","x8_10")))
  set.seed(1); irisDisc9 <- (discretize(iris[,9], method = "cluster", breaks = 10,
                           labels = c("y_1", "y_2","y_3","y_4","y_5","y_6","y_7","y_8","y_9","y_10")))
  set.seed(1); irisDisc10 <- (discretize(iris[,10], method = "cluster", breaks = 10,
                            labels = c("x10_1", "x10_2","x10_3","x10_4","x10_5","x10_6","x10_7","x10_8","x10_9","x10_10")))
  set.seed(1); irisDisc11 <- (discretize(iris[,11], method = "cluster", breaks = 10,
                            labels = c("x11_1", "x11_2","x11_3","x11_4","x11_5","x11_6","x11_7","x11_8","x11_9","x11_10")))
  set.seed(1); irisDisc12 <- (discretize(iris[,12], method = "cluster", breaks = 10,
                            labels = c("x12_1", "x12_2","x12_3","x12_4","x12_5","x12_6","x12_7","x12_8","x12_9","x12_10")))
  set.seed(1); irisDisc13 <- (discretize(iris[,13], method = "cluster", breaks = 10,
                            labels = c("x13_1", "x13_2","x13_3","x13_4","x13_5","x13_6","x13_7","x13_8","x13_9","x13_10")))
  set.seed(1); irisDisc14 <- (discretize(iris[,14], method = "cluster", breaks = 10,
                            labels = c("x14_1", "x14_2","x14_3","x14_4","x14_5","x14_6","x14_7","x14_8","x14_9","x14_10")))
  set.seed(1); irisDisc15 <- (discretize(iris[,15], method = "cluster", breaks = 10,
                            labels = c("x15_1", "x15_2","x15_3","x15_4","x15_5","x15_6","x15_7","x15_8","x15_9","x15_10")))
  
  
  
  irisDisc <- cbind.data.frame(irisDisc1, irisDisc2, irisDisc3, irisDisc4, irisDisc5, irisDisc6, irisDisc7,
                               irisDisc8, irisDisc9, irisDisc10, irisDisc11, irisDisc12, irisDisc13, irisDisc14, irisDisc15)
  
  write.csv(irisDisc, file = "irisDisc.csv")                    
  dataset = read.transactions('irisDisc.csv', sep = ',', rm.duplicates = TRUE)
  
  # specifying the minimum support, confidence, and range of interest for RHS
  rules.y<-apriori(data=dataset, parameter=list(supp=0.0012,conf = 0.5),
                   appearance=list(default="lhs",rhs=c("y_6","y_7")), control=list(verbose=F))
  
  rules.y.byconf<-sort(rules.y, by="confidence", decreasing=TRUE)
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), conviction = interestMeasure(rules.y.byconf, measure = "conviction",
                                                                                         transactions = dataset))
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), hyperConfidence = interestMeasure(rules.y.byconf, measure = "hyperConfidence",
                                                                                              transactions = dataset))
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), jaccard = interestMeasure(rules.y.byconf, measure = "jaccard",
                                                                                      transactions = dataset))
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), hyperLift = interestMeasure(rules.y.byconf, measure = "hyperLift",
                                                                                        transactions = dataset))
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), oddsRatio = interestMeasure(rules.y.byconf, measure = "oddsRatio",
                                                                                        transactions = dataset))
  
  quality(rules.y.byconf) <- cbind(quality(rules.y.byconf), leverage = interestMeasure(rules.y.byconf, measure = "leverage",
                                                                                       transactions = dataset))
  
  print(783)
  
  rules <- rules.y.byconf
  
  rules = rules[is.redundant(rules)]
  
  rules = subset(rules,subset = size(lhs) <= 4)
  
  IMs <- interestMeasure(rules, c("support", "confidence", "lift", "count", "conviction", "jaccard", "hyperLift", "hyperConfidence",
                                  "oddsRatio", "leverage", "coverage"),
                         transactions = dataset)
  
  
  # creating a table of Attributes, Level Codes, Minimum Limit, Maximum Limit
  ranges_irisDisc1 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][1], attributes(irisDisc1)$levels, attributes(irisDisc1)$`discretized:breaks`,
                                         attributes(irisDisc1)$`discretized:breaks`[2:length(attributes(irisDisc1)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc1)[3] <- "Minimum Limit"; colnames(ranges_irisDisc1)[4] <- "Maximum Limit"
  
  ranges_irisDisc2 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][2], attributes(irisDisc2)$levels, attributes(irisDisc2)$`discretized:breaks`,
                                         attributes(irisDisc2)$`discretized:breaks`[2:length(attributes(irisDisc2)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc2)[3] <- "Minimum Limit"; colnames(ranges_irisDisc2)[4] <- "Maximum Limit"
  
  ranges_irisDisc3 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][3], attributes(irisDisc3)$levels, attributes(irisDisc3)$`discretized:breaks`,
                                         attributes(irisDisc3)$`discretized:breaks`[2:length(attributes(irisDisc3)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc3)[3] <- "Minimum Limit"; colnames(ranges_irisDisc3)[4] <- "Maximum Limit"
  
  ranges_irisDisc4 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][4], attributes(irisDisc4)$levels, attributes(irisDisc4)$`discretized:breaks`,
                                         attributes(irisDisc4)$`discretized:breaks`[2:length(attributes(irisDisc4)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc4)[3] <- "Minimum Limit"; colnames(ranges_irisDisc4)[4] <- "Maximum Limit"
  
  ranges_irisDisc5 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][5], attributes(irisDisc5)$levels, attributes(irisDisc5)$`discretized:breaks`,
                                         attributes(irisDisc5)$`discretized:breaks`[2:length(attributes(irisDisc5)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc5)[3] <- "Minimum Limit"; colnames(ranges_irisDisc5)[4] <- "Maximum Limit"
  
  ranges_irisDisc6 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][6], attributes(irisDisc6)$levels, attributes(irisDisc6)$`discretized:breaks`,
                                         attributes(irisDisc6)$`discretized:breaks`[2:length(attributes(irisDisc6)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc6)[3] <- "Minimum Limit"; colnames(ranges_irisDisc6)[4] <- "Maximum Limit"
  
  ranges_irisDisc7 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][7], attributes(irisDisc7)$levels, attributes(irisDisc7)$`discretized:breaks`,
                                         attributes(irisDisc7)$`discretized:breaks`[2:length(attributes(irisDisc7)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc7)[3] <- "Minimum Limit"; colnames(ranges_irisDisc7)[4] <- "Maximum Limit"
  
  ranges_irisDisc8 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][8], attributes(irisDisc8)$levels, attributes(irisDisc8)$`discretized:breaks`,
                                         attributes(irisDisc8)$`discretized:breaks`[2:length(attributes(irisDisc8)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc8)[3] <- "Minimum Limit"; colnames(ranges_irisDisc8)[4] <- "Maximum Limit"
  
  ranges_irisDisc9 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][9], attributes(irisDisc9)$levels, attributes(irisDisc9)$`discretized:breaks`,
                                         attributes(irisDisc9)$`discretized:breaks`[2:length(attributes(irisDisc9)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc9)[3] <- "Minimum Limit"; colnames(ranges_irisDisc9)[4] <- "Maximum Limit"
  
  ranges_irisDisc10 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][10], attributes(irisDisc10)$levels, attributes(irisDisc10)$`discretized:breaks`,
                                          attributes(irisDisc10)$`discretized:breaks`[2:length(attributes(irisDisc10)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc10)[3] <- "Minimum Limit"; colnames(ranges_irisDisc10)[4] <- "Maximum Limit"
  
  ranges_irisDisc11 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][11], attributes(irisDisc11)$levels, attributes(irisDisc11)$`discretized:breaks`,
                                          attributes(irisDisc11)$`discretized:breaks`[2:length(attributes(irisDisc11)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc11)[3] <- "Minimum Limit"; colnames(ranges_irisDisc11)[4] <- "Maximum Limit"
  
  ranges_irisDisc12 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][12], attributes(irisDisc12)$levels, attributes(irisDisc12)$`discretized:breaks`,
                                          attributes(irisDisc12)$`discretized:breaks`[2:length(attributes(irisDisc12)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc12)[3] <- "Minimum Limit"; colnames(ranges_irisDisc12)[4] <- "Maximum Limit"
  
  ranges_irisDisc13 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][13], attributes(irisDisc13)$levels, attributes(irisDisc13)$`discretized:breaks`,
                                          attributes(irisDisc13)$`discretized:breaks`[2:length(attributes(irisDisc13)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc13)[3] <- "Minimum Limit"; colnames(ranges_irisDisc13)[4] <- "Maximum Limit"
  
  ranges_irisDisc14 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][14], attributes(irisDisc14)$levels, attributes(irisDisc14)$`discretized:breaks`,
                                          attributes(irisDisc14)$`discretized:breaks`[2:length(attributes(irisDisc14)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc14)[3] <- "Minimum Limit"; colnames(ranges_irisDisc14)[4] <- "Maximum Limit"
  
  ranges_irisDisc15 = as.data.frame(cbind(attributes(iris)$dimnames[[2]][15], attributes(irisDisc15)$levels, attributes(irisDisc15)$`discretized:breaks`,
                                          attributes(irisDisc15)$`discretized:breaks`[2:length(attributes(irisDisc15)$`discretized:breaks`)])[1:10,])
  colnames(ranges_irisDisc15)[3] <- "Minimum Limit"; colnames(ranges_irisDisc15)[4] <- "Maximum Limit"
  
  print(845)
  
  ranges_x = rbind(ranges_irisDisc9, ranges_irisDisc1, ranges_irisDisc2, ranges_irisDisc3, ranges_irisDisc4, ranges_irisDisc5,
                   ranges_irisDisc6, ranges_irisDisc7, ranges_irisDisc8, ranges_irisDisc10, ranges_irisDisc11, ranges_irisDisc12,
                   ranges_irisDisc13, ranges_irisDisc14, ranges_irisDisc15)
  
  colnames(ranges_x)[1] <- "Attributes"; colnames(ranges_x)[2] <- "Level Codes"
  
  
  # ranking the variable levels based on hyperlift
  df = inspect(sort(rules, by="hyperLift", decreasing=TRUE))
  df = as.data.frame(sort(table(unlist(strsplit(rep(gsub('[{},]','',df$lhs), df$hyperLift), split = " "))), decreasing = T))
  
  suppressPackageStartupMessages(library(dplyr))
  
  d <- arrange(df, desc(df$Freq)) %>%
    mutate(
      cumsum = cumsum(df$Freq),
      freq = round(df$Freq / sum(df$Freq), 3),
      cum_freq = cumsum(freq)
    )
  
  
  # selecting the best level for each variable
  best_METRA_4C = cbind(tail(as.numeric(df.lev$`METRA 4C Causticizing Degree % | _565_4CAU_CE.PV`),1), cbind("METRA 4C Causticizing Degree % | _565_4CAU_CE.PV","y_6-y_7",80.5,82.5)); colnames(best_METRA_4C) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_METRA_4C <- if (as.numeric(best_METRA_4C[1]) < as.numeric(best_METRA_4C[4]) ) {
    paste(c("+",as.numeric(best_METRA_4C[4]) - as.numeric(best_METRA_4C[1])),collapse="")
  } else if ( as.numeric(best_METRA_4C[1]) > as.numeric(best_METRA_4C[5])) {
    as.numeric(best_METRA_4C[5]) - as.numeric(best_METRA_4C[1])
  } else {
    0
  }
  best_METRA_4C["Recommended Change"] = reco_METRA_4C
  
  
  best_x2 = list(d[which(substr(d$Var1, 1, 3)=="x2_"),][1])
  shortened = c()     # additional filtering is done on this variable to eliminate the lowest two levels based on subject matter advice
  for (i in unlist(best_x2))
    if (!c("x2_1") %in% i == T && !c("x2_2") %in% i == T)
      shortened = append(shortened, (i))
  best_x2 = shortened
  best_x2 = toString(unlist(best_x2)[1])
  best_x2 = cbind(tail(as.numeric(df.lev$`565FIC094.PV | GL->SLAKER`),1), ranges_x[which(ranges_x$`Level Codes`==best_x2),]); colnames(best_x2) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x2 <- if (as.numeric(best_x2[1]) < as.numeric(best_x2[4]) ) {
    paste(c("+",as.numeric(best_x2[4]) - as.numeric(best_x2[1])),collapse="")
  } else if ( as.numeric(best_x2[1]) > as.numeric(best_x2[5])) {
    as.numeric(best_x2[5]) - as.numeric(best_x2[1])
  } else {
    0
  }
  best_x2["Recommended Change"] = reco_x2
  
  
  best_x3 = list(d[which(substr(d$Var1, 1, 3)=="x3_"),][1])
  best_x3 = toString(unlist(best_x3)[1])
  best_x3 = cbind(tail(as.numeric(df.lev$`565SIC1127.PV | PURCHASED LIME`),1), ranges_x[which(ranges_x$`Level Codes`==best_x3),]); colnames(best_x3) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x3 <- if (as.numeric(best_x3[1]) < as.numeric(best_x3[4]) ) {
    paste(c("+",as.numeric(best_x3[4]) - as.numeric(best_x3[1])),collapse="")
  } else if ( as.numeric(best_x3[1]) > as.numeric(best_x3[5])) {
    as.numeric(best_x3[5]) - as.numeric(best_x3[1])
  } else {
    0
  }
  best_x3["Recommended Change"] = reco_x3
  
  print(912)
  best_x4 = list(d[which(substr(d$Var1, 1, 3)=="x4_"),][1])
  best_x4 = toString(unlist(best_x4)[1])
  best_x4 = cbind(tail(as.numeric(df.lev$`565SIC1128.PV | REBURN LIME`),1), ranges_x[which(ranges_x$`Level Codes`==best_x4),]); colnames(best_x4) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x4 <- if (as.numeric(best_x4[1]) < as.numeric(best_x4[4]) ) {
    paste(c("+",as.numeric(best_x4[4]) - as.numeric(best_x4[1])),collapse="")
  } else if ( as.numeric(best_x4[1]) > as.numeric(best_x4[5])) {
    as.numeric(best_x4[5]) - as.numeric(best_x4[1])
  } else {
    0
  }
  best_x4["Recommended Change"] = reco_x4
  
  
  best_x5 = list(d[which(substr(d$Var1, 1, 3)=="x5_"),][1])
  best_x5 = toString(unlist(best_x5)[1])
  best_x5 = cbind(tail(as.numeric(df.lev$`565TIC103.PV | SLAKER BOWL`),1), ranges_x[which(ranges_x$`Level Codes`==best_x5),]); colnames(best_x5) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x5 <- if (as.numeric(best_x5[1]) < as.numeric(best_x5[4]) ) {
    paste(c("+",as.numeric(best_x5[4]) - as.numeric(best_x5[1])),collapse="")
  } else if ( as.numeric(best_x5[1]) > as.numeric(best_x5[5])) {
    as.numeric(best_x5[5]) - as.numeric(best_x5[1])
  } else {
    0
  }
  best_x5["Recommended Change"] = reco_x5
  
  
  best_x6 = list(d[which(substr(d$Var1, 1, 3)=="x6_"),][1])
  best_x6 = toString(unlist(best_x6)[1])
  best_x6 = cbind(tail(as.numeric(df.lev$`GL Temp at #2 || 565TIC096C.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x6),]); colnames(best_x6) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x6 <- if (as.numeric(best_x6[1]) < as.numeric(best_x6[4]) ) {
    paste(c("+",as.numeric(best_x6[4]) - as.numeric(best_x6[1])),collapse="")
  } else if ( as.numeric(best_x6[1]) > as.numeric(best_x6[5])) {
    as.numeric(best_x6[5]) - as.numeric(best_x6[1])
  } else {
    0
  }
  best_x6["Recommended Change"] = reco_x6
  
  
  best_x7 = list(d[which(substr(d$Var1, 1, 3)=="x7_"),][1])
  best_x7 = toString(unlist(best_x7)[1])
  best_x7 = cbind(tail(as.numeric(df.lev$`565HY103H.PV | PURCH/REBURN`),1), ranges_x[which(ranges_x$`Level Codes`==best_x7),]); colnames(best_x7) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x7 <- if (as.numeric(best_x7[1]) < as.numeric(best_x7[4]) ) {
    paste(c("+",as.numeric(best_x7[4]) - as.numeric(best_x7[1])),collapse="")
  } else if ( as.numeric(best_x7[1]) > as.numeric(best_x7[5])) {
    as.numeric(best_x7[5]) - as.numeric(best_x7[1])
  } else {
    0
  }
  best_x7["Recommended Change"] = reco_x7
  
  
  best_x8 = list(d[which(substr(d$Var1, 1, 3)=="x8_"),][1])
  best_x8 = toString(unlist(best_x8)[1])
  best_x8 = cbind(tail(as.numeric(df.lev$`Lime to GL Ratio Controller | 565FF1128.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x8),]); colnames(best_x8) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x8 <- if (as.numeric(best_x8[1]) < as.numeric(best_x8[4]) ) {
    paste(c("+",as.numeric(best_x8[4]) - as.numeric(best_x8[1])),collapse="")
  } else if ( as.numeric(best_x8[1]) > as.numeric(best_x8[5])) {
    as.numeric(best_x8[5]) - as.numeric(best_x8[1])
  } else {
    0
  }
  best_x8["Recommended Change"] = reco_x8
  print(981)
  
  best_x10 = list(d[which(substr(d$Var1, 1, 4)=="x10_"),][1])
  best_x10 = toString(unlist(best_x10)[1])
  best_x10 = cbind(tail(as.numeric(df.lev$`METRA CGL Sulfidity S% | _565_CGL_S.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x10),]); colnames(best_x10) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x10 <- if (as.numeric(best_x10[1]) < as.numeric(best_x10[4]) ) {
    paste(c("+",as.numeric(best_x10[4]) - as.numeric(best_x10[1])),collapse="")
  } else if ( as.numeric(best_x10[1]) > as.numeric(best_x10[5])) {
    as.numeric(best_x10[5]) - as.numeric(best_x10[1])
  } else {
    0
  }
  best_x10["Recommended Change"] = reco_x10
  
  
  best_x11 = list(d[which(substr(d$Var1, 1, 4)=="x11_"),][1])
  best_x11 = toString(unlist(best_x11)[1])
  best_x11 = cbind(tail(as.numeric(df.lev$`METRA CGL TTA | _565_CGL_TTA.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x11),]); colnames(best_x11) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x11 <- if (as.numeric(best_x11[1]) < as.numeric(best_x11[4]) ) {
    paste(c("+",as.numeric(best_x11[4]) - as.numeric(best_x11[1])),collapse="")
  } else if ( as.numeric(best_x11[1]) > as.numeric(best_x11[5])) {
    as.numeric(best_x11[5]) - as.numeric(best_x11[1])
  } else {
    0
  }
  best_x11["Recommended Change"] = reco_x11
  
  
  best_x12 = list(d[which(substr(d$Var1, 1, 4)=="x12_"),][1])
  best_x12 = toString(unlist(best_x12)[1])
  best_x12 = cbind(tail(as.numeric(df.lev$`METRA SlakerCausticizing Degree % | _565_SLKR_CE.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x12),]); colnames(best_x12) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x12 <- if (as.numeric(best_x12[1]) < as.numeric(best_x12[4]) ) {
    paste(c("+",as.numeric(best_x12[4]) - as.numeric(best_x12[1])),collapse="")
  } else if ( as.numeric(best_x12[1]) > as.numeric(best_x12[5])) {
    as.numeric(best_x12[5]) - as.numeric(best_x12[1])
  } else {
    0
  }
  best_x12["Recommended Change"] = reco_x12
  
  
  
  best_x13 = list(d[which(substr(d$Var1, 1, 4)=="x13_"),][1])
  best_x13 = toString(unlist(best_x13)[1])
  best_x13 = cbind(tail(as.numeric(df.lev$`METRA SDT RGL AA | _561_RGL_AA.PV`),1), ranges_x[which(ranges_x$`Level Codes`==best_x13),]); colnames(best_x13) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x13 <- if (as.numeric(best_x13[1]) < as.numeric(best_x13[4]) ) {
    paste(c("+",as.numeric(best_x13[4]) - as.numeric(best_x13[1])),collapse="")
  } else if ( as.numeric(best_x13[1]) > as.numeric(best_x13[5])) {
    as.numeric(best_x13[5]) - as.numeric(best_x13[1])
  } else {
    0
  }
  best_x13["Recommended Change"] = reco_x13
  
  
  
  best_x14 = list(d[which(substr(d$Var1, 1, 4)=="x14_"),][1])
  best_x14 = toString(unlist(best_x14)[1])
  best_x14 = cbind(tail(as.numeric(df.lev$`565FIC097.PV | GL->CLASS`),1), ranges_x[which(ranges_x$`Level Codes`==best_x14),]); colnames(best_x14) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x14 <- if (as.numeric(best_x14[1]) < as.numeric(best_x14[4]) ) {
    paste(c("+",as.numeric(best_x14[4]) - as.numeric(best_x14[1])),collapse="")
  } else if ( as.numeric(best_x14[1]) > as.numeric(best_x14[5])) {
    as.numeric(best_x14[5]) - as.numeric(best_x14[1])
  } else {
    0
  }
  best_x14["Recommended Change"] = reco_x14
  
  
  
  best_x15 = list(d[which(substr(d$Var1, 1, 4)=="x15_"),][1])
  best_x15 = toString(unlist(best_x15)[1])
  best_x15 = cbind(tail(as.numeric(df.lev$`565LI061.PV | WL TANK`),1), ranges_x[which(ranges_x$`Level Codes`==best_x15),]); colnames(best_x15) =
    c("Actual Values","Tag Name","Level Code","Optimal Minimum","Optimal Maximum")
  reco_x15 <- if (as.numeric(best_x15[1]) < as.numeric(best_x15[4]) ) {
    paste(c("+",as.numeric(best_x15[4]) - as.numeric(best_x15[1])),collapse="")
  } else if ( as.numeric(best_x15[1]) > as.numeric(best_x15[5])) {
    as.numeric(best_x15[5]) - as.numeric(best_x15[1])
  } else {
    0
  }
  best_x15["Recommended Change"] = reco_x15
  
  
  
  
  
  
  
  
  # creating the table of the best/optimal levels for all variables
  optimal_settings = rbind(best_METRA_4C,best_x2,best_x3,best_x4,best_x5,best_x6,best_x7,best_x8,best_x10,best_x11,best_x12,best_x13,best_x14,best_x15)[-3]
  optimal_settings[,c(1,3,4,5)] = sapply(optimal_settings[,c(1,3,4,5)], as.numeric)
  optimal_settings <- optimal_settings %>%
    mutate(across(where(is.numeric), ~ round(.,2)))
  optimal_settings = optimal_settings[, colnames(optimal_settings)[c(1,5,2:4)]]
  optimal_settings
  func_for_appending_plus_sign = function (x) {ifelse(x > 0, paste(c("+",x),collapse=" "), paste(c("-",x*(-1)),collapse=" "))}
  optimal_settings[2] = sapply(unlist(optimal_settings[2]), func_for_appending_plus_sign)
  func_for_replacing_downward_arrow_sign_and_zero = function (x) {ifelse(x == "- 0", replace(x, 1, 0), x)}
  optimal_settings[2] = sapply(unlist(optimal_settings[2]), func_for_replacing_downward_arrow_sign_and_zero)
  
  
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



