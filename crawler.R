setwd("~/Dropbox/practice/crawler/dorama")

library(httr)
library(rvest)
library(magrittr)



doc = 
  "http://dorama.info/tw/drama/d_rate.php?year=2016&nt=2&aa=3&season=4&ord=7&page&gk=1&B3=%E6%9F%A5%E8%A9%A2" %>% 
  GET(.) 

 trs = 
  doc %>% 
  content(., as="text") %>% 
  read_html(., encoding = "UTF-8") %>% 
   html_nodes(xpath = "//table[@class='table_r']")

 programs = NULL
 for(i in 2:25){
   programs[[i-1]] = trs %>% html_nodes(xpath=paste0("tr[",i,"]"))
 }
 
 # 取得台劇播放時間 program.time
 program.timeinfo = data.frame()
 for(i in 1:length(programs)){
   
   type1.data = programs[[i]] %>% 
     .[[1]] %>% as.character() %>% 
     str_split(pattern = " ") %>% unlist
   rowspan.ind = type1.data %>% grep("rowspan",.) 
   if(length(rowspan.ind)!=0){
     rowspand =  type1.data[rowspan.ind] %>% str_extract(., "\\d") %>% as.numeric
     a = type1.data[type1.data %>% grep("line_w",.)] %>% gsub("<br/>","",.)
     time = substr(a, (regexpr(">",a)+1), (regexpr("<",a)-1))
     res = data.frame(time, rowspand)
     program.timeinfo = rbind(program.timeinfo, res)
   }
 }
 
 program.time = vector()
 for(i in 1:nrow(program.timeinfo)){
   program.time = c(program.time, rep(program.timeinfo$time[i] %>% as.character, program.timeinfo$rowspand[i]))
 }
 


 # 取得台劇名稱 name, 取得台劇連結 url, 取得台劇收視率 rate 
 
program.timeinfo %<>% dplyr::mutate(cumsum = cumsum(rowspand))
special.name = c(1, program.timeinfo$cumsum+1)
 
 name = vector()
 url = vector()
 rate = vector()
 for(i in 1:length(programs)){
   if(any(i == special.name)){
     name[i] = 
       programs[[i]] %>% 
       .[[1]] %>% html_nodes("td.sz1") %>% .[3] %>% html_text() %>% 
       gsub("\\s", "",.)  
     
     url[i] = 
       programs[[i]] %>% 
       .[[1]] %>% html_nodes("td.sz1") %>% .[3] %>% html_children() %>% html_attr(., "href")
     
     rate[i] = 
     programs[[i]] %>% 
       .[[1]] %>% html_children() %>% .[19] %>% html_text()
     
   }else{
     name[i] =
       programs[[i]] %>% 
       .[[1]] %>% html_nodes("td.sz1") %>% .[2] %>% html_text() %>% 
       gsub("\\s", "",.)  
     
     url[i] = 
       programs[[i]] %>% 
       .[[1]] %>% html_nodes("td.sz1") %>% .[2] %>% html_children() %>% html_attr(., "href")
     
     rate[i] = 
       programs[[i]] %>% 
       .[[1]] %>% html_children() %>% .[18] %>% html_text()
   }
   
 }
 
 url2 =
  url %>% str_replace_all(., "\\D", "") %>% 
  paste0("http://tw.dorama.info/drama/pfd_cast.php?num=", .)
 
 
 GetDirector = function(url){
   data =
     GET(url) %>% 
     content(., as="text", encoding="UTF-8") %>% 
     read_html() %>% 
     html_nodes(css="td.td_frame_cnt_pf_bt") %>% 
     html_children() %>% .[8] %>% html_children() %>% 
     html_nodes(css="td.td_dt_g") %>% .[1] %>% html_node(css="table") %>% 
     html_text()
   
   info = data %>% str_split(., "\\s") %>% .[[1]] 
   ind = info %>%  grep("^導演",.)
   if(length(ind)!=0){
     result = info[ind] %>% gsub("導演：","",.)  
   }else{
     result = "無導演資料"
   }
   
   
   
   
   return(result)
   
 }
 
 director = lapply(url2, GetDirector) %>% unlist
 
 result = data.frame(name, program.time, director, rate)
 
 result
 
 
 
 