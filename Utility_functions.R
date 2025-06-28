

#叫出所有環境函數
list_env_functions <- function(env = .GlobalEnv) {
  objs <- ls(envir = env)
  funcs <- objs[sapply(objs, function(x) is.function(get(x, envir = env)))]
  return(funcs)
}


#起始log檔
init_log_rds <- function(file_path, schema) {
  if (!file.exists(file_path)) {
    df <- as.data.frame(setNames(
      lapply(schema$types, function(tp) {
        switch(tp,
               "character" = character(),
               "numeric"   = numeric(),
               "integer"   = integer(),
               "logical"   = logical(),
               "POSIXct"   = as.POSIXct(character()),
               stop(paste("不支援的型別:", tp))
        )
      }),
      schema$fields
    ), stringsAsFactors = FALSE)
    
    saveRDS(df, file_path)
    message("初始化 RDS 檔案：", file_path)
    return(df)
  } else {
    df <- readRDS(file_path)
    
    for (i in seq_along(schema$fields)) {
      field <- schema$fields[i]
      type <- schema$types[i]
      if (field %in% names(df)) {
        df[[field]] <- switch(type,
                              "character" = as.character(df[[field]]),
                              "numeric"   = as.numeric(df[[field]]),
                              "integer"   = as.integer(df[[field]]),
                              "logical"   = as.logical(df[[field]]),
                              "POSIXct"   = as.POSIXct(df[[field]]),
                              df[[field]]
        )
      }
    }
    
    return(df)
  }
}




#檔案讀取相關

#回傳的是一堆字串
get_rdspath_in <- function(path){
  rds_files <- list.files(path, pattern = "\\.rds$", full.names = TRUE)
  return(rds_files)
}

# 傳入多個 .rds 檔案完整路徑，讀取並合併為一個 data.frame
merge_rds_from_files <- function(rds_paths) {
  if (length(rds_paths) == 0) {
    stop("請提供至少一個 .rds 檔案路徑")
  }
  
  # 檢查檔案是否存在
  missing <- rds_paths[!file.exists(rds_paths)]
  if (length(missing) > 0) {
    stop("以下檔案不存在：", paste(missing, collapse = ", "))
  }
  
  # 讀取所有 RDS，過濾掉非 data.frame
  all_data <- lapply(rds_paths, function(f) {
    obj <- readRDS(f)
    if (!is.data.frame(obj)) return(NULL)
    obj
  })
  all_data <- Filter(Negate(is.null), all_data)
  
  if (length(all_data) == 0) return(data.frame())
  
  merged_df <- do.call(rbind, all_data)
  return(merged_df)
}