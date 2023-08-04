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