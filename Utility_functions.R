

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



#這個函數會拿輸入的DF當母體 抽樣之後加入噪音(和母體一樣大的標準差) 生出一個合成的假資料DF

generate_synthetic_data <- function(fake_data, seed = 123, min_rep = 10, max_rep = 80) {
  set.seed(seed)
  if (!is.data.frame(fake_data)) {
    stop("輸入的 fake_data 必須是資料框")
  }
  if (min_rep > max_rep) {
    stop("min_rep 不可大於 max_rep")
  }
  
  prime_sample <- unique(na.omit(fake_data[[1]]))
  generated_col1 <- c()
  
  for (sample_val in prime_sample) {
    count <- sum(fake_data[[1]] == sample_val, na.rm = TRUE)
    if (count <= 1) {
      repeat_n <- 1
    } else {
      repeat_n <- sample(min_rep:max_rep, 1)
    }
    generated_col1 <- c(generated_col1, rep(sample_val, repeat_n))
  }
  
  empty_fake_data <- fake_data[rep(1, length(generated_col1)), ]
  empty_fake_data[] <- NA
  empty_fake_data[[1]] <- generated_col1
  
  num_cols <- ncol(fake_data)
  
  # 預先計算母體各數值欄標準差
  numeric_sd <- sapply(fake_data[, 2:num_cols], function(col) {
    if (is.numeric(col)) sd(col, na.rm = TRUE) else NA
  })
  
  for (i in seq_len(nrow(empty_fake_data))) {
    sample_val <- empty_fake_data[i, 1]
    matched_rows <- fake_data[fake_data[[1]] == sample_val, ]
    if (nrow(matched_rows) == 0) next
    
    for (col_idx in 2:num_cols) {
      valid_rows <- matched_rows[!is.na(matched_rows[[col_idx]]), ]
      if (nrow(valid_rows) == 0) {
        empty_fake_data[i, col_idx] <- NA
      } else {
        rand_row <- sample(seq_len(nrow(valid_rows)), 1)
        empty_fake_data[i, col_idx] <- valid_rows[rand_row, col_idx]
      }
    }
  }
  
  # 對數值型欄位加入高斯噪音
  for (col_idx in 2:num_cols) {
    if (!is.na(numeric_sd[col_idx - 1]) && numeric_sd[col_idx - 1] > 0) {
      noise <- rnorm(nrow(empty_fake_data), mean = 0, sd = numeric_sd[col_idx - 1])
      # 加噪音後，確保欄位還是數值型
      empty_fake_data[[col_idx]] <- as.numeric(empty_fake_data[[col_idx]]) + noise
    }
  }
  
  orig_name <- deparse(substitute(fake_data))
  assign(paste0("synth_", orig_name), empty_fake_data, envir = .GlobalEnv)
  
  return(empty_fake_data)
}
