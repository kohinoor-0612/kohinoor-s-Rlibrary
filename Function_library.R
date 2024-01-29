cat("讀取我的函數庫")

modcheck <-
function(){
#檢查有沒有安裝這些模組，生成一個清單，按照那個清單安裝
packages <- c("jsonlite", "rvest", "magrittr", "lubridate","httpuv","scales","colorspace",'plot.matrix',"ggplot2","todor")
              
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

#逼逼叫 beepr
#網頁接點httpuv
#colorspace 好用的配色套件
#plot.matrix 將矩陣以熱地圖呈現
#rvest 網頁爬蟲
#jaonlite JSON處理套件


}

#1__________ 物件導向設計相關_____________________
#定義設定物件
setClass("setting",
         slots = list(
           #捲積運算的天數寬度，最佳是35 
           framewide = "numeric",
           #生氣象資料的年數
           keyin_yer = "numeric",
           #實際上使用的年數 是keyin_yer的衍生物
           input_yer = "numeric",
           output_yer = "numeric",
           use_average_staction = "logical",
           #使用的資料集
           input_dataset = "character",
           #維辰表示用節氣外國人會看不懂，就固定用這個區段了！！
           start_of_year = "character",  
           vday = "integer"      #捲積運算的平移長度，這樣子會是9/1~3/31 
         )
)


setting_example <-new("setting",
            framewide = c(14,21,28,35,42,49,56,63), 
            keyin_yer = c(1:11),
            input_yer = c(2:11),
            output_yer = c(2:10),
            use_average_staction = FALSE,
            input_dataset = "grow_avged2",
            start_of_year = "-9-1",  
            vday = c(1:211)
            )


#定義有設定的數據集物件
setClass("data_with_setting",
         slots = list(
          metastinng = "setting" ,
          data_setting = "data.frame"
          
         )
)


data_with_setting_example <- new("data_with_setting",
              metastinng = token,
              data_setting = target
              )


#2__________ 和群組相關的函數_____________________

#將LIST按照種類分離
divid.as.list <- function(inputname,sortby){
  cat("sortby的變數類型為",class(get("inputname")[,sortby]))
  token <- list(1)
  for (i in 1:length(unique(get("inputname")[,sortby]))){
    token [[i]] = subset(get("inputname"),get("inputname")[,sortby] == unique(get("inputname")[,sortby])[i]) 
    names(token)[i] <- unique(get("inputname")[,sortby])[i]
  }
  token
}

#合併LIST
comb.from.list <- function(inputlist){
  token <- inputlist[[1]]
  for(i in 2:length(inputlist)){
    token <- rbind(token,inputlist[[i]])
  }
  token
}
#excel的排序函數
sort.by <- function(inputdf,sortby){
  inputdf[sort.list(inputdf[,sortby]),]
}
 sort.by(iris,"Petal.Length") 


#偷用別人的函數
#原本來自ecospace模組包授權 CC0 作者放棄一切版權
rbind_listdf <-
  function (lists = NULL, seq.by = 100) 
  {
    nr <- length(lists)
    seq.start <- seq.int(1, nr, by = seq.by)
    lseq <- length(seq.start)
    seq.end <- sort(unique(c((seq.start - 1), nr)))
    seq.end <- seq.end[seq.end >= min(seq.start) & seq.end <= 
                         nr]
    seq.start <- seq.start[1:length(seq.end)]
    alphas <- expand.grid(LETTERS[1:26], LETTERS[1:26], LETTERS[1:26], 
                          LETTERS[1:26])
    alphas <- paste(alphas[, 4], alphas[, 3], alphas[, 2], alphas[, 
                                                                  1], sep = "")
    if (lseq > length(alphas)) 
      stop("only 456,976 temporary variables to store more than that many parts. Make seq.by larger (or modify original rbind_listdf function)\n")
    dfs <- paste("df", alphas[seq(lseq)], sep = "")
    for (b in 1:lseq) {
      assign(dfs[b], data.frame())
      for (c in seq.start[b]:seq.end[b]) {
        assign(dfs[b], rbind(get(dfs[b]), lists[[c]]))
      }
    }
    out <- data.frame()
    for (b in 1:lseq) {
      out <- rbind(out, get(dfs[b]))
    }
    return(out)
  }

#輸入 DF 計算平均 組合文字 輸出DF
df.mean <- function(inputdf){
  #生成存檔位置
  output <- data.frame(matrix(ncol = length(colnames(inputdf)), nrow = 1))
  colnames(output) <- colnames(inputdf)
  output$N <- 0
  
  #抓出名稱 去除空白 紀錄種類
  kinds <-unique(inputdf[,1])
  kinds <-kinds[kinds != ""]
  input_class <- sapply(inputdf[1,1:12],function(a)class(a))
  
  #實際運算
  for (j in 1:length(kinds)){
    #寫入名字 
    output[j,1] <- kinds[j]
    local_variable <-  subset(inputdf,inputdf[,1]==kinds[j])
    #算總數
    output$N[j] <-nrow(local_variable)
    #從2開始是因為名字在第一行
    for (i in 2:length(colnames(inputdf))){
      #判斷是否為文字
      if(input_class[i]=="character"){
        #cat(input_class[i],"\n")
        output[j,i]<-paste(unique(local_variable[,i]),collapse =",")
        
     }else{
        output[j,i]<-mean(local_variable[,i],na.rm=TRUE)
      }
    }
  }
  cat("假設第一行是名稱","\n")
  output
}


#3________一些數學小工具_____

#給出重複的元素
duplicated.all <-function(x){
  x[(duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE)),]}

#給出重複元素的對應布林向量
duplicated.all.boolean <-function(x){
  (duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE))}

#將布林向量轉為index
TFtoindex <-function(x){c(1:length(x))[x]}

