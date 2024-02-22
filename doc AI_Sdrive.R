

# For Hot Work Forms ################################################################# STARTS HERE ############################################################################################
# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("tesseract")
# install.packages("magick")
# install.packages("magrittr")
library(pdftools)
library(tidyverse)
library(tesseract)
library(magick)
library(magrittr)

#HW = image_read_pdf("shiny full docs.pdf") %>%
#HW = image_read_pdf("shiny full docs.pdf", density=72) %>%
HW = image_read_pdf("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/sep_28.pdf") %>%
#HW = image_read_pdf("shiny full docs.pdf", density=72) %>%
#HW = image_read_pdf("hw27f.pdf") %>%
#HW = image_read_pdf("hw18f.pdf") %>%
#HW = image_read_pdf("hw13f.pdf") %>%
#HW = image_read_pdf("page7.pdf") %>%
  image_resize("2000") %>%
  image_convert(colorspace = 'gray') %>%
  image_trim() %>%
  image_ocr()
HW = strsplit(HW, split = " ", fixed = T)
#HW = HW[9]


new = c()
for (i in HW)
  new = append(new, as.data.frame(i))
MAX = -Inf
for (i in (new)) { x = length(i) 
if (x > MAX) MAX <- x
}
MAX


new = c()
for (i in as.array(HW)) {
  for (j in seq(length((HW)))) {
    new = append(new, (lapply(as.array(HW)[j], "length<-", MAX)))
  }
}

new = as.data.frame(new)
new = new[,1:(ncol(new)/length(HW))]
new = as.data.frame(new)


f <- function(x) {
  if(is.list(x) ) lapply(x,f)
  else ifelse(length(x) == 0 | typeof(x)=="double", 0, x)
}


# FOR BACK PAGE ######################################################################################
HW_back_pages_collected_results <- list()
V = c()
for (Z in colnames(new))
  if (T %in% ("interfere" == (new[Z])) == T)
    V = append(V, (Z))
for (Z in V)
  print (Z)

for (Z in V) {
  
  output = (sum(min(new[Z][which(new[Z]=="fire")+2,] %in%  list("2,", "2.", "2")) |
                  min(new[Z][which(new[Z]=="nearest")+2,] %in%  list("3,", "3.", "3")) |
                  new[Z][which(new[Z]=="trained)")+1,] %in%  list("4,", "4.", "4") |
                  (which(new[Z] == "IN") - which(new[Z] == "4.") < 9) |
                  new[Z][which(new[Z]=="minimum")+9,] %in%  list("Fire", "Sire", "Watch") != 0))
  
  HW_back_pages_collected_results[[Z]] <- output
}

HW_back_pages_collected_results
HW_bp <- as.matrix(HW_back_pages_collected_results)
HW_bp



# FOR FRONT PAGE ######################################################################################
HW_front_pages_collected_results <- list()
g = c()
for (i in colnames(new))
  if (T %in% ("Supervisor" == (new[i])) == T)
    g = append(g, (i))
for (h in g)
  print (h)


for (h in g) {
  
  output = (ifelse(sum(
    f(new[h][which(new[h]=="flame-proof")+1,] %in%  list("Department:")   )  |
      f(new[h][which(new[h]=="Department:")+1,] %in%  list("Date:", "‘Date:") )   |
      
      sum(  if (sum(grepl("Date",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"Date")==T) + 1,] %in% tolower(list("tarpauliss", "tarpaulins"))) } ) != 0   |
      
      sum(  if (sum(grepl("Date:",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"Date:")==T) + 1,] %in% tolower(list("tarpauliss", "tarpaulins"))) } ) != 0   |
      
      
      # if ("‘Date:" %in% new[h][1:nrow(new[h]),]) {
      #   new[h][which(new[h]=="‘Date:")+1,] %in%  list("tarpauliss", "tarpaulins") } else {
      #     new[h][which(new[h]=="Date:")+1,] %in%  list("tarpauliss", "tarpaulins") }  |
      
      f(new[h][which(new[h]=="Location:")+1,] %in%  list("Shift:", "Shift") )   |
      f(new[h][which(new[h]=="Location")+1,] %in%  list("Shift:", "Shift") )   |
      f(new[h][which(new[h]=="Shift")+1,] %in%  list("Combustibles")  )  |
      f(new[h][which(new[h]=="Shift:")+1,] %in%  list("Combustibles")  )  |
      
      sum(  if (sum(grepl("down",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"down")==T) + 1,] %in% tolower(list("Job"))) } ) != 0   |
      
      
      # if ("down." %in% new[h][1:nrow(new[h]),]) {
      #   new[h][which(new[h]=="down.")+1,] %in%  list("Job") } else {
      #     new[h][which(new[h]=="down")+1,] %in%  list("Job") }  |
      
      f(new[h][which(new[h]=="Description:")+1,] %in%  list("3.", "3")   ) |  
      f(min(new[h][which(new[h]=="sparks")+1,] %in%  list("I"))   ) | 
      f(new[h][which(new[h]=="being")+1,] %in%  list("permit")  )  |
      
      
      sum(  if (sum(grepl("performed",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"performed")==T) + 1,] %in% tolower(list("Flame"))) } ) != 0   |
      
      sum(  if (sum(grepl("performed.",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"performed.")==T) + 1,] %in% tolower(list("Flame"))) } ) != 0   |
      
      
      # if ("performeds" %in% new[h][1:nrow(new[h]),]) {
      #   new[h][which(new[h]=="performeds")+1,] %in%  list("Flame") 
      # } else if ("performed." %in% new[h][1:nrow(new[h]),]) {
      #   new[h][which(new[h]=="performed.")+1,] %in%  list("Flame") 
      # } else { 
      #   new[h][which(new[h]=="performed")+1,] %in%  list("Flame") } | 
      
      f(new[h][which(new[h]=="welders")+1,] %in%  list("Hot")   ) |    
      f(new[h][which(new[h]=="flash")+1,] %in%  list("6.")  )  |
      f(new[h][which(new[h]=="of.")+1,] %in%  list("Equipment")   ) |
      f(new[h][which(new[h]=="acknowledge")-2,] %in%  list("charged", "water", "hose")  )  |
      f(new[h][which(new[h]=="LEL")+1,] %in%  list("sappi") )   |
      f(new[h][which(new[h]=="covered.")+1,] %in%  list("ARE")  )  |
      f(which(new[h]=="Record") - which(new[h]=="LEL") == 1   ) |
      f(which(new[h]=="Record") - which(new[h]=="LEL\n.") == 1   ) |
      
        
  (   f(new[h][which(new[h]=="past")-1,] %in%  list("No")   )   &    f(new[h][which(new[h]=="past")-2,] %in%  list("Yes", ":\nYes", "No")   )     )   |
      

    sum(  if (f(new[h][!which(new[h]=="past")-2,] %in%  list("Yes", ":\nYes"))) {
      f(which(new[h]=="IF") - which(new[h]=="PRESENT?") < 3 )  } ) != 0   |    
    
          
      
         
      f(new[h][which(new[h]=="Waived")+1,] %in%  list("HOT")  )  |
      
     
    
    
(sum(!min(new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 5 + 1 , stop = nchar(as.character(unlist(new[h]))) )  == "USED:")+2,] %in%  list("Abrasive", "O")) |
             min(new[h][which(new[h]=="Saw")+1,] %in% list("Hot", "O")) |
             
             #min(!new[h][which(new[h]=="Welding")+1,] %in% list("Portable")) |
             
             
             
       
       # sum(  if (sum(grepl("Welding",new[h][1:nrow(new[h]),]))>=1) {
       #         (new[h][which(str_detect(as.matrix(new[h]),"Welding")==T) + 1,] %in% tolower(list("Portable", "O"))) } ) == 0   |
             
       
    !min(new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 7 + 1 , stop = nchar(as.character(unlist(new[h]))) )  == "Welding")+1,] %in%  list("Portable", "O")) |
             
             # if (": Welding" %in% new[h][1:nrow(new[h]),]) {
             #   !new[h][which(new[h]==": Welding")+1,] %in%  list("Portable") 
             # } else if ("lew Welding" %in% new[h][1:nrow(new[h]),]) {
             #   !new[h][which(new[h]=="lew Welding")+1,] %in%  list("Portable")
             # } else if ("| Welding" %in% new[h][1:nrow(new[h]),]) {
             #   !new[h][which(new[h]=="| Welding")+1,] %in%  list("Portable") 
             # } else { 
             #   !new[h][which(new[h]=="Welding")+1,] %in%  list("Portable") } |             
             
           
           f(min(new[h][which(new[h]=="Grinder")+1,] %in% list("Propane", "O"))   ) |
             f(min(new[h][which(new[h]=="Torch")+1,] %in% list("LH", "O"))   ) |
             #f(min(!new[h][which(new[h]=="Other:")+1,] %in% list("sappi"))) == 0)   ) |
             f(min(!new[h][which(new[h]=="Other:")+1,] %in% list("sappi", "Sappi")))        == 0)          == 0 )    |
      
    
    
 #   substr( as.character(unlist(new[h])) , start =  1 , stop = 2 )
# new[h][which(substr( as.character(unlist(new[h])) , start =  1 , stop = 6 ) == "meters"),]   #  which(substr( as.character(unlist(new[h])) , start =  1 , stop = 6 ) == "meters")
(
  sum(which(substr( as.character(unlist(new[h])) , start =  1 , stop = 8 ) == "PRESENT?") - which(substr( as.character(unlist(new[h])) , start =  1 , stop = 6 ) == "meters") <= 3  |
    f(  which(substr( as.character(unlist(new[h])) , start =  1 , stop = 5 ) == "Meter") + 3  == "_______\nIF"   ) |
      which(substr( as.character(unlist(new[h])) , start =  1 , stop = 4 ) == "Time") + 1  == "__\nABATEMENT"    |
   (new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 9 + 1 , stop = nchar(as.character(unlist(new[h])))  )  == "ABATEMENT") + 4,]) == "__\nTester"   |
(f (new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 6 + 1 , stop = nchar(as.character(unlist(new[h]))) - 1)  == "Tester"),]) == "LEL") 
)       
    
&
  
  
  
  sum( new[h][which(substr( as.character(unlist(new[h])) , start =  1 , stop = 4 ) == "USED") + 1,]  %in% list("Torch", "“\nTorch", "\nTorch")          |
         
         f(which(substr( as.character(unlist(new[h])) , start =  1 , stop = 7 ) == "Grinder") + 3  == "Torch" )   
  )
  
  
  
  # sum( !new[h][which(substr( as.character(unlist(new[h])) , start =  1 , stop = 4 ) == "USED") + 1,]  %in% list("Torch", "“\nTorch", "\nTorch")          |
  #   
  #   f(which(substr( as.character(unlist(new[h])) , start =  1 , stop = 7 ) == "Grinder") + 3  != "Torch" )   
  #  )    

)          |

  

  #  OR

  
  (
    sum(which(substr( as.character(unlist(new[h])) , start =  1 , stop = 8 ) == "PRESENT?") - which(substr( as.character(unlist(new[h])) , start =  1 , stop = 6 ) == "meters") > 3  |
          f(  which(substr( as.character(unlist(new[h])) , start =  1 , stop = 5 ) == "Meter") + 3  != "_______\nIF"   )  |
          which(substr( as.character(unlist(new[h])) , start =  1 , stop = 4 ) == "Time") + 1  != "__\nABATEMENT"    |
 (new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 9 , stop = nchar(as.character(unlist(new[h]))) )  == "ABATEMENT") + 4,]) != "__\nTester"   |
          (f (new[h][which(substr( as.character(unlist(new[h])) , start =  nchar(as.character(unlist(new[h]))) - 6 , stop = nchar(as.character(unlist(new[h]))) )  == "Tester") - 1,]) != "LEL") 
    )       
    
    &
      
      sum( new[h][which(substr( as.character(unlist(new[h])) , start =  1 , stop = 4 ) == "USED") + 1,]  %in% list("Torch", "“\nTorch", "\nTorch")          |
             
             f(which(substr( as.character(unlist(new[h])) , start =  1 , stop = 7 ) == "Grinder") + 3  == "Torch" )   
      )    
    
  )          |  
  
  

  
  
  
  
  
    #  f(new[h][which(new[h]=="Other")+2,] %in%  list("WORK")   ) |
      f(new[h][which(new[h]=="precautions:")+2,] %in%  list("WORK")   ) |
  f(new[h][which(new[h]=="PLANT")+1,] %in%  list("ALARM", "i\nALARM")   )          |
  f(new[h][which(new[h]=="PLANT\nALARM")+1,] %in%  list("SIGNALS")   )          |
      f(new[h][which(new[h]=="Date:")+1,] %in%  list("Time:")   ) |
  f(new[h][which(new[h]=="Time:")+1,] %in%  list("AM")   ) |
  f(new[h][which(new[h]=="Time:")+1,] %in%  list("OAM")   ) |
      f(new[h][which(new[h]=="tour:\n\ni")+1,] %in%  list("Name")   ) |
  f(new[h][which(new[h]=="tour")+1,] %in%  list("Name")   ) |
      f(new[h][which(new[h]=="tour:")+1,] %in%  list("Name"))  ) != 0, 1, 0))
  
  
  
  HW_front_pages_collected_results[[h]] <- output
}

HW_front_pages_collected_results
HW_fp <- as.matrix(HW_front_pages_collected_results)
HW_fp


























# For Confined Space Forms ################################################################# STARTS HERE ############################################################################################
# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("tesseract")
# install.packages("magick")
# install.packages("magrittr")
library(pdftools)
library(tidyverse)
library(tesseract)
library(magick)
library(magrittr)

#CS = image_read_pdf("sample confined space doc.pdf") %>%
#CS = image_read_pdf("MOAB_35.pdf") %>%
CS = image_read_pdf("S:/Lean Six Sigma/Cloquet Advanced Analytics/CLQ Data Scientist documents/test_CS.pdf") %>%
#CS = image_read_pdf("3csf.pdf") %>%
  #CS = image_read_pdf("20csb.pdf") %>%
  image_resize("2000") %>%
  image_convert(colorspace = 'gray') %>%
  image_trim() %>%
  image_ocr()
CS = strsplit(CS, split = " ", fixed = T)


new = c()
for (i in CS)
  new = append(new, as.data.frame(i))
MAX = -Inf
for (i in (new)) { x = length(i) 
if (x > MAX) MAX <- x
}
MAX


new = c()
for (i in as.array(CS)) {
  for (j in seq(length((CS)))) {
    new = append(new, (lapply(as.array(CS)[j], "length<-", MAX)))
  }
}

new = as.data.frame(new)
new = new[,1:(ncol(new)/length(CS))]
new = as.data.frame(new)


f <- function(x) {
  if(is.list(x) ) lapply(x,f)
  else ifelse(length(x) == 0 | typeof(x)=="double", 0, x)
}


# For Front Page ########################################################################################
CS_front_pages_collected_results <- list()
g = c()
for (i in colnames(new))
  if (T %in% ("CAFD" == (new[i])) == T)
    g = append(g, (i))
for (h in g)
  print (h)


for (h in g) {
  
  output = (ifelse(sum(
    
    #    (sum(min(new[h][which(new[h]=="EMPLOYEE")+1,] %in%  list("[]")) |
    (sum( 
      
      sum(  if (sum(grepl("EMPLOYEE",new[h][1:nrow(new[h]),]))>=1) {
        (new[h][which(str_detect(as.matrix(new[h]),"EMPLOYEE")==T) + 1,] %in% list("[]", "[1]", "|", "[J]", "[J\nPSMAREA")) } ) != 0   |
        
        sum(  if (sum(grepl("MILLEMPLOYEE",new[h][1:nrow(new[h]),]))>=1) {
          (new[h][which(str_detect(as.matrix(new[h]),"MILLEMPLOYEE")==T) + 1,] %in% list("[]", "[1]", "|", "[J]", "[J\nPSMAREA")) } ) != 0   |
        
        
        
sum(list(new[h][which(substr( as.character(unlist(new[h])), start =  nchar(as.character(unlist(new[h]))) - 8 + 1 ,stop = nchar(as.character(unlist(new[h]))) )  == "CONFINED") - 1,]) %in% list("(Print", "Name)", "Print", "Name")  ) != 0) 

  != 0 )    |
        
        
        
                                                                ###   sum(  if (sum(grepl("PSMAREA",new[h][1:nrow(new[h]),]))>=1) {
                                                              ###      (new[h][which(str_detect(as.matrix(new[h]),"PSMAREA")==T) + 1,] %in% list("[]", "[1]", "|", "[J]", "[1\nCloquet")) } ) == 0   |
        
        
        # (  if ("PSMAREA" %in% new[h][1:nrow(new[h]),]) {
        #   min(!new[h][which(new[h]=="PSMAREA")+1,] %in% list("[]", "[1]", "|", "[J]")) 
        # } else if ("ra\nPSMAREA" %in% new[h][1:nrow(new[h]),]) {
        #   min(!new[h][which(new[h]=="ra\nPSMAREA")+1,] %in% list("[]", "[1]", "|", "[J]")) 
        # } else if ("ya\nPSMAREA" %in% new[h][1:nrow(new[h]),]) {
        #   min(!new[h][which(new[h]=="ya\nPSMAREA")+1,] %in% list("[]", "[1]", "|", "[J]"))  
        # } else  { min(!new[h][which(new[h]=="me\nPSMAREA")+1,] %in% list("[]", "[1]", "|", "[J]")) }  ) |      
        
        
      
                                                                ### sum(  if (sum(grepl("Revised",new[h][1:nrow(new[h]),]))>=1) {
                                                                ###   (new[h][which(str_detect(as.matrix(new[h]),"Revised")==T) - 1,] %in% tolower(list("CONTRACTOR"))) } ) != 0 )    |
       
       
       
       # (  if ("Revised" %in% new[h][1:nrow(new[h]),]) {
       #   min(!tolower(new[h][which(new[h]=="Revised")-1,]) ==  tolower("CONTRACTOR"))  
       # }   else if  ("Of\nRevised" %in% new[h][1:nrow(new[h]),]) { 
       #   min(!tolower(new[h][which(new[h]=="Of\nRevised")-1,]) ==  tolower("CONTRACTOR"))     
       # }   else if  ("O\nRevised" %in% new[h][1:nrow(new[h]),]) {
       #   min(!tolower(new[h][which(new[h]=="O\nRevised")-1,]) ==  tolower("CONTRACTOR")) 
       # } else { min(!tolower(new[h][which(new[h]=="conTractoR\nRevised")-1,]) ==  tolower("CONTRACTOR")) } ) != 0)    )   |       
       
     
     
     
     
     f(new[h][which(new[h]=="Department")+1,] ==  "Date") |
       f(new[h][which(new[h]=="-\nDepartment")+1,] ==  "Date") |
       f(tolower(new[h][which(new[h]=="Date")+1,]) ==  tolower("Shift") ) |
       f(new[h][which(new[h]=="Times")+1,] ==  "Valid" ) |
       f(new[h][which(new[h]=="Times.")+1,] ==  "Valid" ) |
       f(new[h][which(new[h]=="Number")+1,] ==  "Work" ) |
       f(new[h][which(new[h]=="Description")+1,] ==  "CHECKLIST" ) |
       
       
       (  if ("training-REQUIRED" %in% new[h][1:nrow(new[h]),]) {
         min(new[h][which(new[h]=="training-REQUIRED")+1,] %in%  list("C]", "CT]"))       
       } else  { ("-REQUIRED" %in% new[h][1:nrow(new[h]),])  }  )   |
       
       
       
       f(new[h][which(new[h]=="reviewed--REQUIRED")+1,] %in% list("C]", "CT]")  ) |
       f(min(new[h][which(new[h]=="used")+1,] %in%  list("Cc", "Cl"))   ) |
       f(min(new[h][which(new[h]=="*Bump")-1,] ==  "number")  )  |
       f(new[h][which(new[h]=="test")+1,] ==  "date"  )  |
       f(min(new[h][which(new[h]=="LEL")+1,] ==  "%")  ) |
       f(new[h][which(new[h]=="Tester")-1,] %in% list("Se", "Ses", "*Other")  ) |
       f(which(new[h]=="If") - which(new[h]=="personnel") < 5   )  |
       f(new[h][which(new[h]=="N/A);")+1,] == "6."  ) |
       f(toString(new[h][which(new[h]=="Entry")-1,]) %in% list("Cl", "i") )   |
       
       
       
       sum(  if (sum(grepl("Must",new[h][1:nrow(new[h]),]))>=1) {
         (new[h][which(str_detect(as.matrix(new[h]),"Must")==T) - 1,] %in% list("Name")) } ) != 0  |
       
       
       # (  if ("Must" %in% new[h][1:nrow(new[h]),]) {
       #   min(new[h][which(new[h]=="Must")-1,] ==  "Name)")               
       # }    else if ("S\nMust" %in% new[h][1:nrow(new[h]),])  {
       #   min(new[h][which(new[h]=="S\nMust")-1,] ==  "Name)")
       # }    else if ("S Must" %in% new[h][1:nrow(new[h]),])   {
       #   min(new[h][which(new[h]=="S Must")-1,] ==  "Name)")
       # }    else if  ("va Must" %in% new[h][1:nrow(new[h]),])  {
       #   min(new[h][which(new[h]=="va Must")-1,] ==  "Name)")
       # }  else if  ("ot\nMust" %in% new[h][1:nrow(new[h]),]) {
     #   min(new[h][which(new[h]=="ot\nMust")-1,] ==  "Name)")
     # } else {  min(new[h][which(new[h]=="va\nMust")-1,] ==  "Name)")  }    )     |
     
     
     f(min(new[h][which(new[h]=="Door")+1,] ==  "Location")   ) |
       min(f(new[h][which(new[h]=="alarm")+1,] ==  "Location")  )  |
       
       
       sum(  if (sum(grepl("WHEN",new[h][1:nrow(new[h]),]))>=1) {
         (new[h][which(str_detect(as.matrix(new[h]),"WHEN")==T) - 1,] %in% list("phone")) } )    
     
     # (  if ("WHEN" %in% new[h][1:nrow(new[h]),]) {
     #   min(new[h][which(new[h]=="WHEN")-1,] == "phone")
     # }  else if  ("@\nOMrte\nWHEN" %in% new[h][1:nrow(new[h]),])  {
     #   min(new[h][which(new[h]=="@\nOMrte\nWHEN")-1,] == "phone")
     # }  else if ("on\nWHEN" %in% new[h][1:nrow(new[h]),])  {
     #   min(new[h][which(new[h]=="on\nWHEN")-1,] == "phone")
     # } else { min(new[h][which(new[h]=="drsux\nWHEN")-1,] == "phone")  } ) 
     
    )   != 0, 1, 0))
  
  
  CS_front_pages_collected_results[[h]] <- output
}

CS_front_pages_collected_results  
CS_fp <- as.matrix(CS_front_pages_collected_results)
CS_fp


# For Back Page ######################################################################################
CS_back_pages_collected_results <- list()
V = c()
for (Z in colnames(new))
  if (T %in% ("GFCI" == (new[Z])) == T)
    V = append(V, (Z))
for (Z in V)
  print (Z)


for (Z in V) {
  #   print(ifelse((
  #     
  #     if ("here: |" %in% new[Z][1:nrow(new[Z]),]) {
  #       (new[Z][which(new[Z] == "here: |"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #     } else if ("here: DO" %in% new[Z][1:nrow(new[Z]),]) {
  #       (new[Z][which(new[Z] == "here: DO"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #     } else if ("here:" %in% new[Z][1:nrow(new[Z]),]) {
  #       (new[Z][which(new[Z] == "here:"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #     } else if ("here:\n|" %in% new[Z][1:nrow(new[Z]),]) {
  #       (new[Z][which(new[Z] == "here:\n|"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #     } else if ("here:\nDO" %in% new[Z][1:nrow(new[Z]),]) {
  #       (new[Z][which(new[Z] == "here:\nDO"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #     }  else { (new[Z][which(new[Z] == "here:\n\nStardesd"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|")) }  |
  #       
  #       
  #       
  #       if ("10." %in% new[Z][1:nrow(new[Z]),]) {
  #         (new[Z][which(new[Z] == "10."):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
  #       } else { (new[Z][which(new[Z] == "shower?\n10."):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|")) } |
  #       
  #       new[Z][which(new[Z] == new[Z][nrow(na.omit(new[Z])),]),] == "OUT"),1,0))
  #   
  # }
  
  
  output = (ifelse(sum(
    
    sum(  if (sum(grepl("here",new[Z][1:nrow(new[Z]),]))>=1) {
      (new[Z][which(str_detect(as.matrix(new[Z]),"here")==T) + 1,] %in% list("DO", "|")) } ) != 0 |
      
      
      f(new[Z][which(new[Z]=="here:\nee\neee\nDO")+1,] %in% list("DO", "|", "YOUR") ) |
      
      f(new[Z][which(new[Z]=="\nee\neee\nDO")+1,] %in% list("YOUR") ) |
      f(new[Z][which(new[Z]=="\neee\nDO")+1,] %in% list("YOUR") ) |
      f(new[Z][which(new[Z]=="\nDO")+1,] %in% list("YOUR") ) |
      # } else if ("here: DO" %in% new[Z][1:nrow(new[Z]),]) {
      #   (new[Z][which(new[Z] == "here: DO"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
      # } else if ("here:" %in% new[Z][1:nrow(new[Z]),]) {
      #   (new[Z][which(new[Z] == "here:"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
      # } else if ("here:\n|" %in% new[Z][1:nrow(new[Z]),]) {
      #   (new[Z][which(str_detect(as.matrix(new[Z]),"here")==T) + 1,] %in% list("DO", "|"))
      # } else if ("here:\nDO" %in% new[Z][1:nrow(new[Z]),]) {
      #   (new[Z][which(new[Z] == "here:\nDO"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|"))
      # }  else { (new[Z][which(new[Z] == "here:\n\nStardesd"):which(new[Z] == new[Z][nrow(na.omit(new[Z])),]), ][2] %in% list("DO", "|")) } ) |
      
      
    # sum(  if (sum(grepl("rescue",new[Z][1:nrow(new[Z]),]))>=1) {
    #   which(str_detect(as.matrix(new[Z]),"rescue")==T) - which(str_detect(as.matrix(new[Z]),"goes")==T)  <= 4 } ) != 0 |      
    #   
    #   
    #   sum(  if (sum(grepl("nearest",new[Z][1:nrow(new[Z]),]))>=1) {
    #     which(str_detect(as.matrix(new[Z]),"nearest")==T) - which(str_detect(as.matrix(new[Z]),"Review")==T)  <= 3 } ) != 0 |      
    #   
    #   
      (  new[Z][which(new[Z] == new[Z][nrow(na.omit(new[Z])),]),] == "OUT") )   != 0, 1, 0))
  
  
  CS_back_pages_collected_results[[Z]] <- sum(output)
  
}

CS_back_pages_collected_results
CS_bp <- as.matrix(CS_back_pages_collected_results)
CS_bp

