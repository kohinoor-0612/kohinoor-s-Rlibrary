modcheck <-
function(){
#檢查有沒有安裝這些模組，生成一個清單，按照那個清單安裝
packages <- c("beepr","jsonlite", "rvest", "magrittr", "lubridate","nortest","httpuv","scales","car","colorspace",'plot.matrix',"ggplot2","todor")

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
#nortest 常態分布檢定套件
#car 變異數相等檢定
#dplyr DATAFRAME小魔法 提供類似EXCEL的操作
}

#我的函數

#2__________ 和群組相關的函數_____________________

#將LIST按照種類分離 
#2025 chatgpt說r有原生的slpit函數可用 split(dataframe, dataframe$group_column)
#以前的我好笨喔
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



#3________一些數學小工具_____
#是否溫度異常
tem_test <- function(temp) {(temp< -19  )}

#給出重複的元素
duplicated.all <-function(x){
  x[(duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE)),]}

#給出重複元素的對應布林向量
duplicated.all.boolean <-function(x){
  (duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE))}

#將布林向量轉為index
TFtoindex <-function(x){c(1:length(x))[x]}

#生成時間表函數
make_timetable <-function(){
timetable <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
timetable <- paste0(timetable,":00:00")
timetable[24] <- "23:59:00"
timetable}

#傳回每一個複製體而不會露單
duplicated.all <-function(x){
  x[(duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE)),]
}

#傳回每一個複製體的布林向量
duplicated.all.boolean <-function(x){
  (duplicated(x,fromLast = TRUE)|duplicated(x,fromLast = FALSE))
}


#4__複雜強大的統計函數------

#輸入 DF 計算平均 組合文字 輸出DF
df.mean <- function(inputdf){
  #生成存檔位置
  output <- data.frame(matrix(ncol = length(colnames(inputdf)), nrow = 1))
  colnames(output) <- colnames(inputdf)
  output$N <- 0

  #抓出名稱 去除空白 紀錄種類
  kinds <-unique(inputdf[,1])
  kinds <-kinds[kinds != ""]
  input_class <- sapply(inputdf, class)


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
  message("假設第一行是名稱","\n")
  output
}



perform_normality_tests <- function(tested_inputdataframe, majorgroupe, minorgroupe, treename) {
  # Initialize an empty dataframe to store results
  savenormality_gpt <- data.frame(dataname = character(), groupe_MA = character(),
                                  groupe_mi = character(), shapiro_pvalue = numeric(),
                                  cvm_pvalue = numeric(), ad_pvalue = numeric())
  # Loop through each tree name and minor group
  for (x in treename) {
    for (i in minorgroupe) {
      # Create a new row with initial values
      new_row <- data.frame(dataname = tested_inputdataframe, groupe_MA = x,
                            groupe_mi = colnames(get(tested_inputdataframe))[i],
                            shapiro_pvalue = NA, cvm_pvalue = NA, ad_pvalue = NA)

      # Extract the relevant data for the current group
      samplegroupe <- get(tested_inputdataframe)[get(tested_inputdataframe)[[majorgroupe]] == x, i]

      # If the data is constant or NA, skip normality tests and append the row directly
      if (all(is.na(samplegroupe)) || length(unique(samplegroupe)) == 1) {
        savenormality_gpt <- rbind(savenormality_gpt, new_row)
      } else {
        # Perform the normality tests
        new_row$shapiro_pvalue <- as.numeric(shapiro.test(samplegroupe)$p.value)
        new_row$cvm_pvalue <- as.numeric(cvm.test(samplegroupe)$p.value)
        new_row$ad_pvalue <- as.numeric(ad.test(samplegroupe)$p.value)

        # Append the results to the dataframe
        savenormality_gpt <- rbind(savenormality_gpt, new_row)
      }
    }
  }

  # Return the resulting dataframe
  return(savenormality_gpt)
}


#-----日期處理相關----

to_julian_ordinal <- function(input_date, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  error_code <- 10000
  
  if (length(input_date) != 1) {
    message("錯誤代碼 10001：輸入長度不是 1")
    return(error_code + 1)
  }
  
  if (is.na(input_date) || (is.character(input_date) && input_date == "")) {
    return(NA)
  }
  
  if (is.numeric(input_date)) {
    message("錯誤代碼 10002：純數字輸入，嘗試當作 YYYYMMDD 解析")
    parsed_date <- tryCatch(as.Date(as.character(input_date), format = "%Y%m%d"), error = function(e) NA)
    if (is.na(parsed_date)) {
      return(error_code + 2)
    }
  } else if (inherits(input_date, "Date")) {
    parsed_date <- input_date
  } else if (inherits(input_date, c("POSIXct", "POSIXlt"))) {
    parsed_date <- as.Date(input_date)
  } else 
    if (is.character(input_date)) {
      # 替換中文符號
      clean_date <- gsub("月|日|號", "/", input_date)
      clean_date <- sub("/$", "", clean_date)  # 去除尾巴斜線
      clean_date <- gsub("\\s+", "", clean_date) # 去除空白
      
      # 補年份 (如果只有月/日)
      if (grepl("^[0-9]{1,2}/[0-9]{1,2}$", clean_date)) {
        clean_date <- paste0(current_year, "/", clean_date)
      }
      
      possible_formats <- c(
        "%Y-%m-%d",
        "%Y/%m/%d",
        "%Y%m%d",
        "%m/%d/%Y"
      )
      
      parsed_date <- NA
      for (fmt in possible_formats) {
        parsed_date <- tryCatch(as.Date(clean_date, format = fmt), error = function(e) NA)
        if (!is.na(parsed_date)) break
      }
      
      if (is.na(parsed_date)) {
        message("錯誤代碼 10003：無法解析字串格式日期")
        return(error_code + 3)
      }
    }
  else {
    message("錯誤代碼 10004：不支援的輸入類型")
    return(error_code + 4)
  }
  
  as.numeric(format(parsed_date, "%j"))
}


#----字串處理相關------
reorder_and_clean <- function(s, order_ref = c("f", "m", "h")) {
  if (is.na(s)) return(NA_character_)
  
  # 拆字串成字元，去除空白
  chars <- trimws(unlist(strsplit(s, split = ",")))
  
  # 過濾掉非 order_ref 和問號以外的字元
  allowed_chars <- c(order_ref, "?")
  chars <- chars[chars %in% allowed_chars]
  
  # 去除問號
  chars <- chars[chars != "?"]
  
  # 按照 order_ref 排序
  chars <- chars[order(match(chars, order_ref))]
  
  # 合併成字串
  paste(chars, collapse = "")
}
