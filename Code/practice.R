setwd("~/Desktop/chineseresearch")
# step1 先抓一个主题
install.packages(c("httr2", "jsonlite", "dplyr", "purrr", "stringr", "tibble", "readr"))
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# 1) 取一页 JSON ----------------------------------------------------------
fetch_json <- function(childtype, p = 1, n = 50,
                       t_key = "zhengcelibrary_gw_bm",
                       type_key = "gwyzcwjk",
                       mintime = "", maxtime = "") {
  
  api_url <- "https://sousuo.www.gov.cn/search-gov/data"
  
  resp <- request(api_url) %>%
    req_url_query(
      t = t_key,
      q = "",
      timetype = "",
      mintime = mintime,
      maxtime = maxtime,
      sort = "score",
      sortType = "1",
      searchfield = "title",
      childtype = as.character(childtype),
      puborg = "",
      pcodeYear = "",
      pcodeNum = "",
      filetype = "",
      p = as.character(p),
      n = as.character(n),
      inpro = "",
      dup = "",
      orpro = "",
      type = type_key
    ) %>%
    req_user_agent("Mozilla/5.0") %>%
    req_headers(
      "Accept"="application/json, text/javascript, */*; q=0.01",
      "X-Requested-With"="XMLHttpRequest",
      "Referer"="https://sousuo.www.gov.cn/zcwjk/policyDocumentLibrary"
    ) %>%
    req_perform()
  
  txt <- resp_body_string(resp)
  fromJSON(txt, simplifyVector = FALSE)
}

# 2) 从某个 catKey 把 listVO 抽成 tibble --------------------------------
extract_cat <- function(json, catKey, childtype, page) {
  rows <- json$searchVO$catMap[[catKey]]$listVO
  if (is.null(rows) || length(rows) == 0) return(tibble())
  
  tibble(
    theme_childtype = as.character(childtype),
    page = page,
    source_catKey = catKey,                         # gongwen / bumenfile
    source_group = if_else(catKey == "gongwen", "国务院/国办", "国务院部门"),
    title = map_chr(rows, ~ .x$title %||% NA_character_),
    pubtimeStr = map_chr(rows, ~ .x$pubtimeStr %||% NA_character_),
    puborg = map_chr(rows, ~ .x$puborg %||% NA_character_),
    pcode = map_chr(rows, ~ .x$pcode %||% NA_character_),
    childtype_text = map_chr(rows, ~ .x$childtype %||% NA_character_),
    url = map_chr(rows, ~ .x$url %||% NA_character_),
    id = map_chr(rows, ~ as.character(.x$id) %||% NA_character_)
  )
}

# 3) 抓一个主题 childtype 下的两个类别（gongwen + bumenfile）--------------
collect_both <- function(childtype,
                         catKeys = c("gongwen", "bumenfile"),
                         page_size = 50,
                         mintime = "", maxtime = "",
                         t_key = "zhengcelibrary_gw_bm",
                         type_key = "gwyzcwjk",
                         polite_sleep = 0.2) {
  
  # 先抓第一页，用来拿 totalCount
  j1 <- fetch_json(childtype = childtype, p = 1, n = page_size,
                   t_key = t_key, type_key = type_key,
                   mintime = mintime, maxtime = maxtime)
  
  totals <- map_int(catKeys, ~ j1$searchVO$catMap[[.x]]$totalCount %||% 0)
  names(totals) <- catKeys
  
  # 逐个 catKey 抓全量
  out <- map_dfr(catKeys, function(catKey) {
    total <- totals[[catKey]]
    if (total == 0) return(tibble())
    
    pages <- ceiling(total / page_size)
    
    map_dfr(1:pages, function(p) {
      message("childtype=", childtype,
              " catKey=", catKey,
              " page=", p, "/", pages,
              " rows=", page_size)
      
      j <- if (p == 1) j1 else fetch_json(childtype = childtype, p = p, n = page_size,
                                          t_key = t_key, type_key = type_key,
                                          mintime = mintime, maxtime = maxtime)
      
      Sys.sleep(polite_sleep)
      extract_cat(j, catKey = catKey, childtype = childtype, page = p)
    })
  })
  
  # 去重（同一条 url 在不同页/重复抓取时不应重复）
  out %>%
    distinct(source_catKey, id, url, .keep_all = TRUE)
}

# 4) 运行：抓 1080 的“国务院 + 部门”并合并 -------------------------------
all_1080 <- collect_both(
  childtype = 1080,
  catKeys = c("gongwen", "bumenfile"),
  page_size = 50
)

nrow(all_1080)
table(all_1080$source_group)

# 时间窗口过滤
all_1080_2125 <- all_1080 %>%
  mutate(pubdate = as.Date(pubtimeStr, format = "%Y.%m.%d")) %>%
  filter(pubdate >= as.Date("2021-01-01"),
         pubdate <= as.Date("2025-12-31"))
nrow(all_1080_2125)
table(all_1080_2125$source_group)

# 保存国民经济主题
write_csv(all_1080, "urls_1080_all.csv")
write_csv(all_1080_2125, "urls_1080_2021_2025.csv")

# 批量抓取 1081–1099 并保存（
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------- 核心：抓一页 ----------
fetch_json <- function(childtype, p = 1, n = 50,
                       t_key = "zhengcelibrary_gw_bm",
                       type_key = "gwyzcwjk",
                       mintime = "", maxtime = "") {
  
  api_url <- "https://sousuo.www.gov.cn/search-gov/data"
  
  resp <- request(api_url) %>%
    req_url_query(
      t = t_key,
      q = "",
      timetype = "",
      mintime = mintime,
      maxtime = maxtime,
      sort = "score",
      sortType = "1",
      searchfield = "title",
      childtype = as.character(childtype),
      puborg = "",
      pcodeYear = "",
      pcodeNum = "",
      filetype = "",
      p = as.character(p),
      n = as.character(n),
      inpro = "",
      dup = "",
      orpro = "",
      type = type_key
    ) %>%
    req_user_agent("Mozilla/5.0") %>%
    req_headers(
      "Accept"="application/json, text/javascript, */*; q=0.01",
      "X-Requested-With"="XMLHttpRequest",
      "Referer"="https://sousuo.www.gov.cn/zcwjk/policyDocumentLibrary"
    ) %>%
    req_perform()
  
  txt <- resp_body_string(resp)
  fromJSON(txt, simplifyVector = FALSE)
}

# ---------- 核心：抽出某个类别(catKey) ----------
extract_cat <- function(json, catKey, childtype, page) {
  rows <- json$searchVO$catMap[[catKey]]$listVO
  if (is.null(rows) || length(rows) == 0) return(tibble())
  
  tibble(
    theme_childtype = as.integer(childtype),
    page = as.integer(page),
    source_catKey = catKey,   # gongwen / bumenfile
    source_group = if_else(catKey == "gongwen", "国务院/国办", "国务院部门"),
    title = map_chr(rows, ~ .x$title %||% NA_character_),
    pubtimeStr = map_chr(rows, ~ .x$pubtimeStr %||% NA_character_),
    puborg = map_chr(rows, ~ .x$puborg %||% NA_character_),
    pcode = map_chr(rows, ~ .x$pcode %||% NA_character_),
    childtype_text = map_chr(rows, ~ .x$childtype %||% NA_character_),
    url = map_chr(rows, ~ .x$url %||% NA_character_),
    id = map_chr(rows, ~ as.character(.x$id) %||% NA_character_),
    summary = map_chr(rows, ~ .x$summary %||% NA_character_)
  )
}

# ---------- 核心：抓一个主题下两个类别（国务院/国办 + 部门） ----------
collect_both <- function(childtype,
                         catKeys = c("gongwen", "bumenfile"),
                         page_size = 50,
                         mintime = "", maxtime = "",
                         polite_sleep = 0.2) {
  
  j1 <- fetch_json(childtype = childtype, p = 1, n = page_size,
                   mintime = mintime, maxtime = maxtime)
  
  totals <- map_int(catKeys, ~ j1$searchVO$catMap[[.x]]$totalCount %||% 0)
  names(totals) <- catKeys
  
  out <- map_dfr(catKeys, function(catKey) {
    total <- totals[[catKey]]
    if (total == 0) return(tibble())
    
    pages <- ceiling(total / page_size)
    
    map_dfr(1:pages, function(p) {
      message("childtype=", childtype,
              " catKey=", catKey,
              " page=", p, "/", pages,
              " rows=", page_size)
      
      j <- if (p == 1) j1 else fetch_json(childtype = childtype, p = p, n = page_size,
                                          mintime = mintime, maxtime = maxtime)
      
      Sys.sleep(polite_sleep)
      extract_cat(j, catKey = catKey, childtype = childtype, page = p)
    })
  })
  
  # 如果没有任何行，就原样返回（并确保列存在）
  if (nrow(out) == 0) {
    return(tibble(
      theme_childtype = integer(),
      page = integer(),
      source_catKey = character(),
      source_group = character(),
      title = character(),
      pubtimeStr = character(),
      puborg = character(),
      pcode = character(),
      childtype_text = character(),
      url = character(),
      id = character(),
      summary = character()
    ))
  }
  
  out %>% distinct(source_catKey, id, url, .keep_all = TRUE)
}

# ================== 你提供的主题表 ==================
themes <- tibble::tribble(
  ~theme_name, ~childtype,
  "财政、金融、审计", 1081,
  "国土资源、能源", 1082,
  "农业、林业、水利", 1083,
  "工业、交通", 1084,
  "商贸、海关、旅游", 1085,
  "市场监管、安全生产监管", 1086,
  "城乡建设、环境保护", 1087,
  "科技、教育", 1088,
  "文化、广电、新闻出版", 1089,
  "卫生、体育", 1090,
  "人口与计划生育、妇女儿童工作", 1091,
  "劳动、人事、监察", 1092,
  "公安、安全、司法", 1093,
  "民政、扶贫、救灾", 1094,
  "民族、宗教", 2919,
  "对外事务", 1096,
  "港澳台工作", 1097,
  "国防", 1098,
  "其他", 1099
)

# ================== 输出目录（建议） ==================
dir.create("data", showWarnings = FALSE)
dir.create("data/01_index", showWarnings = FALSE)

# ====== 批量抓取（先抓全量；你也可以改成只抓2021-2025）======
page_size <- 50
all_other <- purrr::map_dfr(seq_len(nrow(themes)), function(i) {
  ct <- themes$childtype[i]
  nm <- themes$theme_name[i]
  
  message("\n==============================")
  message("START THEME ", ct, " : ", nm)
  message("==============================")
  
  df <- collect_both(childtype = ct, page_size = page_size)
  
  df <- df %>%
    mutate(theme_name = nm)
  
  # 每个主题单独保存（全量）
  out_file <- sprintf("data/01_index/theme_%s_%s_all.csv", ct, nm)
  out_file <- stringr::str_replace_all(out_file, "[/\\\\:*?\"<>|]", "_") # 避免文件名非法字符
  write_csv(df, out_file)
  
  message("SAVED: ", out_file, "  rows=", nrow(df))
  df
})

# 总表保存（全量）
write_csv(all_other, "data/01_index/all_themes_1081_1099_all.csv")

# ======（可选）再生成研究窗口 2021-2025 子集 ======
all_other_2125 <- all_other %>%
  mutate(pubdate = as.Date(pubtimeStr, "%Y.%m.%d")) %>%
  filter(pubdate >= as.Date("2021-01-01"),
         pubdate <= as.Date("2025-12-31"))

write_csv(all_other_2125, "data/01_index/all_themes_1081_1099_2021_2025.csv")

# 快速汇总检查
all_other %>% count(theme_name, source_group, sort = TRUE) %>% print(n = 50)
all_other_2125 %>% count(theme_name, source_group, sort = TRUE) %>% print(n = 50)

# 合并
library(readr)
library(dplyr)
library(stringr)

idx_1080 <- read_csv("themes_1080_2021_2025.csv", show_col_types = FALSE)
idx_other <- read_csv("themes_1081_1099_2021_2025.csv", show_col_types = FALSE)

idx_all <- bind_rows(idx_1080, idx_other)

# 关键清理：去重（同一个 url 只保留一次）
idx_all <- idx_all %>%
  mutate(url = str_trim(url)) %>%
  filter(!is.na(url), url != "") %>%
  distinct(url, .keep_all = TRUE)
nrow(idx_all)
idx_all %>% count(theme_name, source_group, sort = TRUE) %>% print(n = 50)

# 给1080命名
idx_all <- idx_all %>%
  mutate(
    theme_name = if_else(
      is.na(theme_name),
      "国民经济管理、国有资产监管",
      theme_name
    )
  )

idx_all %>% count(theme_name, source_group, sort = TRUE) %>% print(n = 50)
#保存
write_csv(idx_all, "themes_1080_1099_2021_2025.csv")


# 数据清理，抓取正文
library(readr)
library(dplyr)
library(purrr)
library(progress)

# 1) 读入数据
idx_all <- read_csv("themes_1080_1099_2021_2025.csv", show_col_types = FALSE)
urls <- idx_all$url %>% unique() %>% na.omit()

# 2) 设定输出文件（你可以改成你想要的路径）
out_csv <- "gov_text_extracted_partial.csv"
out_rds <- "gov_text_extracted_partial.rds"

# 3) 如果之前已经跑过一部分：读取已完成结果，做断点续跑
done <- if (file.exists(out_csv)) read_csv(out_csv, show_col_types = FALSE) else tibble(url = character())
done_urls <- done$url %>% unique()

todo_urls <- setdiff(urls, done_urls)

cat("Total:", length(urls), "\n")
cat("Done :", length(done_urls), "\n")
cat("Todo :", length(todo_urls), "\n")

# 4) 进度条
pb <- progress_bar$new(
  format = "  downloading [:bar] :current/:total (:percent) eta: :eta",
  total = length(todo_urls),
  clear = FALSE,
  width = 60
)

# 5) 分批抓取 + 每批落盘
batch_size <- 100
batches <- split(todo_urls, ceiling(seq_along(todo_urls) / batch_size))

for (i in seq_along(batches)) {
  message("\nBatch ", i, " / ", length(batches), " ...")
  
  out_i <- map_dfr(batches[[i]], function(u) {
    pb$tick()
    
    # 轻微限速，降低被限流风险
    Sys.sleep(runif(1, 0.2, 0.6))
    
    fetch_one_doc(u)  # <- 你已经验证成功的函数
  })
  
  # 追加写入 CSV（不会覆盖已有进度）
  if (!file.exists(out_csv)) {
    write_csv(out_i, out_csv)
  } else {
    write.table(out_i, out_csv, sep = ",", row.names = FALSE, col.names = FALSE,
                append = TRUE, qmethod = "double", fileEncoding = "UTF-8")
  }
  
  # 同时保存 RDS（更安全，保留完整类型/换行）
  if (file.exists(out_rds)) {
    old <- readRDS(out_rds)
    saveRDS(bind_rows(old, out_i), out_rds)
  } else {
    saveRDS(out_i, out_rds)
  }
}

# 6) 跑完后：生成最终文件
final <- read_csv(out_csv, show_col_types = FALSE)
write_csv(final, "gov_text_extracted_full.csv")

cat("\nSaved to:\n")
cat(" - ", normalizePath(out_csv), "\n")
cat(" - ", normalizePath(out_rds), "\n")
cat(" - ", normalizePath("gov_text_extracted_full.csv"), "\n")


# 其他一些关键的非正文数据已经在文档级别的数据集中已经获得了，因此我们只需要获得公文种类和附件
# 因为政策页面没有专门描述政策公文种类的数据，因此我们按照标题进行抓取。
library(readr)
library(dplyr)
library(stringr)

infer_doctype_from_title <- function(title){
  title <- str_squish(title)
  
  case_when(
    is.na(title) | title == "" ~ NA_character_,
    
    # 法规/条例类
    str_detect(title, "实施条例|条例") ~ "条例",
    
    # 办法类（含暂行/试行/管理办法）
    str_detect(title, "暂行办法|试行办法|管理办法|办法") ~ "办法",
    
    # 规定类（含暂行规定、实施规定等）
    str_detect(title, "规定") ~ "规定",
    
    # 意见类（指导/实施/若干意见）
    str_detect(title, "指导意见|实施意见|若干意见|意见") ~ "意见",
    
    # 通知类（含通告在很多场景也可归公告，但这里保守）
    str_detect(title, "通知") ~ "通知",
    
    # 决定/命令
    str_detect(title, "决定|命令|令") ~ "决定/命令",
    
    # 公告/通告
    str_detect(title, "公告|通告") ~ "公告/通告",
    
    # 通报
    str_detect(title, "通报") ~ "通报",
    
    # 批复
    str_detect(title, "批复") ~ "批复",
    
    # 函
    str_detect(title, "函|复函") ~ "函",
    
    # 纪要
    str_detect(title, "纪要") ~ "纪要",
    
    # 规划/计划/要点/方案（这些在政策里很多）
    str_detect(title, "规划纲要|规划") ~ "规划",
    str_detect(title, "行动计划|工作计划|计划") ~ "计划",
    str_detect(title, "工作要点|要点") ~ "要点",
    str_detect(title, "方案|总体方案|实施方案") ~ "方案",
    
    # 细则/指南
    str_detect(title, "实施细则|细则") ~ "细则",
    str_detect(title, "指南|指引") ~ "指南",
    
    # 任免（如果你需要）
    str_detect(title, "任免|任命|免去") ~ "任免",
    
    TRUE ~ "其他/未识别"
  )
}

# 直接用索引表 title 生成 doc_type
idx <- read_csv("themes_1080_1099_2021_2025.csv", show_col_types = FALSE)

# 注意：你索引表里 title 的列名如果不是 title，就把下面的 title 改成你的列名
idx2 <- idx %>%
  mutate(doc_type = infer_doctype_from_title(title))

# 看覆盖情况
idx2 %>% count(doc_type, sort = TRUE) %>% print(n = 50)

idx2 <- idx2 %>%
  mutate(
    doc_type = case_when(
      doc_type != "其他/未识别" ~ doc_type,
      
      # 技术治理类
      str_detect(title, "标准$") ~ "技术规范-标准",
      str_detect(title, "名录$") ~ "技术规范-名录",
      str_detect(title, "规则$") ~ "技术规范-规则",
      str_detect(title, "准则$") ~ "技术规范-准则",
      str_detect(title, "规程$") ~ "技术规范-规程",
      str_detect(title, "事项$") ~ "清单/事项",
      
      # 信息披露类（明确标记，不混入政策文种）
      str_detect(title, "公布$|公示$") ~ "信息披露",
      
      # 其余保持未识别
      TRUE ~ "其他/未识别"
    )
  )
# 与文档级别合并
write_csv(idx2, "themes_1080_1099_2021_2025_with_doctype.csv")

# 合并文档和正文
clean_url <- function(x) {
  x %>%
    str_trim() %>%
    str_replace("^http://", "https://")
}

# 文档级（主表）
meta <- read_csv("themes_1080_1099_2021_2025_with_doctype.csv", show_col_types = FALSE) %>%
  mutate(url = clean_url(url)) %>%
  distinct(url, .keep_all = TRUE)

# 正文级
text <- read_csv("gov_text_extracted_full.csv", show_col_types = FALSE) %>%
  mutate(url = clean_url(url)) %>%
  distinct(url, .keep_all = TRUE)

merged <- meta %>%
  left_join(text, by = "url")

# 结构优化
merged <- merged %>%
  mutate(
    # 拆分一级 / 二级主题
    topic_lvl1 = str_trim(str_extract(childtype_text, "^[^\\\\]+")),
    topic_lvl2 = str_trim(str_replace(childtype_text, "^[^\\\\]+\\\\?", "")),
    topic_lvl2 = if_else(topic_lvl2 == "" | is.na(topic_lvl2), NA_character_, topic_lvl2),
    
    # 正文可用性标记（NLP / attention 分析用）
    has_text = !is.na(text) & nchar(text) > 200,
    
    # 正文长度（描述性统计、异常检测用）
    text_len = nchar(text)
  )

# 简单删除一些不需要的列
analysis_df <- merged %>%
  select(
    # ---- 核心标识 ----
    url,
    title,
    pubdate,
    source_group,
    puborg,
    pcode,
    doc_type,
    
    # ---- 主题 ----
    topic_lvl1,
    topic_lvl2,
    
    # ---- 正文 ----
    text,
    text_len,
    has_text,
    
    # ---- 可选 ----
    attachments
  )
write_csv(analysis_df,"analysis1.csv")


# 对正文级别的数据进行分段处理，生成一个新表
library(tidyverse)

split_paragraphs <- function(text) {
  if (is.na(text) || str_trim(text) == "") return(tibble())
  
  tibble(para_text = str_split(text, "\\n+")[[1]]) %>%
    mutate(para_text = str_trim(para_text)) %>%
    filter(nchar(para_text) >= 20) %>%     # 去掉标题/空行
    mutate(
      para_len = nchar(para_text),
      para_id = row_number()
    )
}

para_df <- analysis_df %>%
  select(url, doc_type, topic_lvl1, topic_lvl2, pubdate, text) %>%
  mutate(paragraphs = map(text, split_paragraphs)) %>%
  select(-text) %>%
  unnest(paragraphs)

glimpse(para_df)

# 每篇文件平均多少段
para_df %>%
  count(url) %>%
  summarise(mean_paras = mean(n))

write_csv(para_df,"para_analysis.csv")

# 进行描述性统计（EDA）
# 文种和段落结构
para_df %>%
  count(doc_type) %>%
  arrange(desc(n))

para_df %>%
  group_by(doc_type) %>%
  summarise(
    mean_len = mean(para_len),
    median_len = median(para_len),
    n = n()
  )

# 主题
para_df %>%
  count(topic_lvl2, sort = TRUE)
para_df %>%
  count(topic_lvl1, sort = TRUE)

# 时间趋势
para_df %>%
  mutate(year = lubridate::year(pubdate)) %>%
  count(year, doc_type)

# attention 指标一致性
att_test <- para_df %>%
  group_by(url) %>%
  summarise(
    n_paras = n(),
    total_len = sum(para_len),
    capped_len = sum(pmin(para_len, 300))
  )

cor(att_test[, c("n_paras", "total_len", "capped_len")], use = "complete.obs")

#画图
# doc_type 分布柱状图
library(ggplot2)
library(dplyr)
library(showtext)

para_df %>%
  count(doc_type, sort = TRUE) %>%
  ggplot(aes(x = reorder(doc_type, n), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n),
            hjust = -0.1,   
            size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Document Types",
    x = "Document Type",
    y = "Number of Paragraphs"
  )

# 段落长度分布
ggplot(para_df, aes(x = para_len)) +
  geom_histogram(bins = 60, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Paragraph Length",
       x = "Paragraph Length",
       y = "Count") + scale_x_log10()

#不同文档类型的段落长度箱线图
ggplot(para_df, aes(x = reorder(doc_type, para_len, median),
                    y = para_len)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Paragraph Length by Document Type",
       x = "",
       y = "Paragraph Length (log scale)")

#一级主题（topic_lvl1）分布
library(dplyr)
library(scales)

para_df %>%
  count(topic_lvl1) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(pct)) %>%
  ggplot(aes(x = reorder(topic_lvl1, pct), y = pct)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            hjust = 1.1,          
            color = "white",
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  ) +
  labs(title = "Distribution of Level-1 Topics",
       x = NULL,
       y = "Share of Paragraphs")

#年份 × 文档类型 分布
library(lubridate)
para_df$year <- year(as.Date(para_df$pubdate))
para_df %>%
  mutate(year = lubridate::year(as.Date(pubdate))) %>%
  count(year, doc_type) %>%
  ggplot(aes(x = factor(year), y = n, fill = doc_type)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 做LDA
packages <- c("dplyr","stringr","lubridate","purrr","readr",
              "quanteda","topicmodels","tidyr","tibble")
to_install <- packages[!packages %in% installed.packages()[,"Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))

# 做2-gram的
# 1) 构造 para_raw，并新增清洗后的字段（不破坏原文）
para_raw <- para_df %>%
  filter(!is.na(para_text)) %>%
  mutate(
    year = lubridate::year(pubdate),
    para_text = str_squish(para_text),
    # 去标点（中英文常见标点都覆盖），再去空白
    para_text_clean = para_text %>%
      str_replace_all("[[:punct:]]", " ") %>%                 # 英文标点
      str_replace_all("[，。！？；：、“”‘’（）《》【】—…·、]", " ") %>%  # 中文标点补充
      str_replace_all("[0-9０-９]", "") %>%
      str_replace_all("\\s+", "")                              # 去掉所有空白（变成纯中文串）
  )

set.seed(1)

# 2) 建 tokens（字符）→ 2-gram：用 para_text_clean
toks_raw2 <- tokens(para_raw$para_text_clean, what = "character") %>%
  tokens_ngrams(n = 2)

dfm_raw2 <- dfm(toks_raw2)

# 3) 全局 top features（最高频 2-gram）
top2 <- textstat_frequency(dfm_raw2, n = 2000)
write_csv(top2, "raw_top_char2gram.csv")
head(top2, 50)

# 4) 覆盖率（出现在多少比例段落）
feat_docfreq <- colSums(dfm_raw2 > 0) / nrow(dfm_raw2)

docfreq_df <- tibble(
  term = names(feat_docfreq),
  docfreq = as.numeric(feat_docfreq)
) %>% arrange(desc(docfreq))

top_docfreq <- docfreq_df %>% slice(1:500)
write_csv(top_docfreq, "raw_top_docfreq_char2gram.csv")
head(top_docfreq, 50)

writeLines(candidate_stop2, "stop_char2gram_candidates.txt")

# 用其他方式再做一版
library(dplyr)
library(stringr)
library(quanteda)
library(readr)

para_raw_word <- para_df %>%
  filter(!is.na(para_text)) %>%
  mutate(
    para_text_clean = str_squish(para_text)
  )

toks_word <- tokens(
  para_raw_word$para_text_clean,
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
)
# 去中文停用词（的/和/在/为/对/与/等…会大量消失）
toks_word <- tokens_remove(toks_word, pattern = stopwords("zh"))
# 只保留：>=2个汉字 的 token
toks_word <- tokens_keep(toks_word, pattern = "^[\\p{Han}]{2,}$", valuetype = "regex")
# 构建dfm和输出频率/覆盖率
dfm_word <- dfm(toks_word)

# 词频 top（quanteda 自带）
top_word_freq <- topfeatures(dfm_word, 2000)
top_word_df <- tibble(term = names(top_word_freq), freq = as.numeric(top_word_freq))
write.csv(top_word_df, "raw_top_word_freq_quanteda_clean.csv", row.names = FALSE)

# 覆盖率
feat_docfreq_word <- colSums(dfm_word > 0) / nrow(dfm_word)
docfreq_word_df <- tibble(term = names(feat_docfreq_word),
                          docfreq = as.numeric(feat_docfreq_word)) %>%
  arrange(desc(docfreq))
write.csv(docfreq_word_df, "raw_top_word_docfreq_quanteda_clean.csv", row.names = FALSE)

head(top_word_df, 30)
head(docfreq_word_df, 30)

# 在获得词频后，先去除我们不需要的行政类词汇
stop_word_admin <- c(
  # 结构性行政名词
  "管理","服务","工作","部门","机构","单位","组织","行政","政府","人民政府",
  # 行政动作/模板动词
  "加强","开展","实施","推进","推动","落实","提升","建立","制定","做好","促进","提高",
  "引导","统筹","协调","部署","采取","纳入","印发","提出","完成","健全","完善","深化",
  "支持","保障","提供","确保","监督","监管","检查","评估","评价","考核","审查","审核",
  "批准","审批","备案","登记","认定","认证","申报","申请","办理","更新","修订","变更",
  "执行","执法","处罚","处理","整治","防范",
  # 典型模板连接词/抽象泛词
  "相关","有关","应当","按照","根据","结合","符合","以及","包括","其中","方面","内容",
  "方式","范围","条件","原则","规则","指南","办法","方案","措施","流程","程序","清单","目录",
  "事项","职责","责任","问题","情况","结果","重点","重要","相应","具体","有效","有序","良好",
  # 泛指集合/层级/地域（通常是模板）
  "各地","各级","各省","省级","中央","地方","全国","地区","区域","基层",
  # 时间/序号（LDA 垃圾主题来源）
  "年度","第一","第二","第三","第四","二十","十四","十条",
  # 常见功能性词（在政策文本里很“废”）
  "进一步","持续","积极","综合","全面","深入","明确","充分","严格","需要"
)

stop2_admin <- c(
  # A) 行政结构高频（来自你char2 top）
  "管_理","服_务","工_作","部_门","机_构","单_位","组_织","行_政","政_府",
  "加_强","开_展","实_施","推_进","推_动","落_实","提_升","建_立","制_定",
  "做_好","促_进","提_高","引_导","统_筹","协_调","部_署","采_取","纳_入",
  "印_发","提_出","完_成","健_全","完_善","深_化","支_持","保_障","提_供",
  "确_保","监_督","监_管","检_查","评_估","评_价","考_核","审_查","审_核",
  "批_准","审_批","备_案","登_记","认_定","认_证","申_报","申_请","办_理",
  "更_新","修_订","变_更","执_行","执_法","处_罚","处_理","整_治","防_范",
  "相_关","有_关","应_当","按_照","根_据","结_合","符_合","以_及","包_括",
  "其_中","方_面","内_容","方_式","范_围","条_件","原_则","规_则","指_南",
  "办_法","方_案","措_施","流_程","程_序","清_单","目_录","事_项","职_责",
  "责_任","问_题","情_况","结_果","重_点","重_要","相_应","具_体","有_效",
  "有_序","良_好","各_地","各_级","各_省","省_级","中_央","地_方","全_国","地_区","区_域","基_层",
  "进_一","一_步","持_续","积_极","综_合","全_面","深_入","明_确","充_分","严_格","需_要",
  
  # B) char2 特有碎片（你top500里出现很多：跨词/语法残片/日期序号）
  "作_的","定_的","件_的","的_应","的_重","的_通","和_国","可_以","如_下",
  "年_月","月_日","日_起","年_度",
  "第_一","第_二","第_三","第_四","第_十","十_一","十_二","十_三","十_四","十_九","二_十","三_十"
)

# 在删除上述的词汇后，第一轮清洗完毕后，再次查看词频，决定是否需要做第二轮清洗
library(quanteda)
library(dplyr)
library(stringr)

# 1) tokens（词级）
toks_word <- tokens(
  str_squish(para_df$para_text),
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
)

# 2) 删除行政模板词
toks_word_clean <- tokens_remove(toks_word, pattern = stop_word_admin)

# 3) 可选：删中文停用词 & 删单字（强烈建议）
toks_word_clean <- tokens_remove(toks_word_clean, pattern = stopwords("zh"))
toks_word_clean <- tokens_keep(toks_word_clean, pattern = "^[\\p{Han}]{2,}$", valuetype = "regex")

# 4) dfm + trim（去低频噪音）
dfm_word_clean <- dfm(toks_word_clean) %>%
  dfm_trim(min_docfreq = 5)   # 至少出现在5个段落

# 5) 看清洗后的 top features
topfeatures(dfm_word_clean, 30)

# 1) char tokens -> 2-gram
toks_char2 <- tokens(
  str_squish(para_df$para_text),
  what = "character",
  remove_punct = TRUE,
  remove_numbers = TRUE
) %>% tokens_ngrams(n = 2)

# 2) 删除行政/碎片 2-gram
toks_char2_clean <- tokens_remove(toks_char2, pattern = stop2_admin)

# 3) dfm + trim
dfm_char2_clean <- dfm(toks_char2_clean) %>%
  dfm_trim(min_docfreq = 0.005,  # 至少出现在0.5%的段落
           max_docfreq = 0.5,
           docfreq_type = "prop")

# 4) 看清洗后的 top features
topfeatures(dfm_char2_clean, 30)


# 直接跑一遍LDA，先看看数据情况
# A) WORD-LEVEL LDA PIPELINE

library(dplyr)
library(stringr)
library(quanteda)
library(topicmodels)

set.seed(1234)

# 0) 取文本（段落）
txt <- para_df %>%
  filter(!is.na(para_text)) %>%
  mutate(text = str_squish(para_text)) %>%
  pull(text)

# 1) tokens（词级）
toks <- tokens(
  txt,
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
)

# 2) 去中文停用词（注意：stopwords("zh") 已deprecated）
toks <- tokens_remove(toks, stopwords(language = "zh", source = "misc"))

# 3) 只保留 >=2 个汉字（去单字噪音）
toks <- tokens_keep(toks, pattern = "^[\\p{Han}]{2,}$", valuetype = "regex")

# 4) 去行政模板词
toks <- tokens_remove(toks, pattern = stop_word_admin)

# 5) 构建 dfm + trim（去极低频 & 去过于普遍的泛词）
dfm1 <- dfm(toks) %>%
  dfm_trim(min_docfreq = 5) %>%                          # 至少出现在5段
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")     # 出现超过50%段落的词删掉

# 6) quick check：清洗后的 top30
topfeatures(dfm1, 30)

# 7) LDA（先跑 k=10）
dtm1 <- convert(dfm1, to = "topicmodels")
lda10 <- LDA(dtm1, k = 10, control = list(seed = 1234))

# 8) 输出每个topic的前12个词
terms(lda10, 12)

# 继续删除泛政策词汇，再跑一轮
stop_round2 <- c(
  "建设","发展","国家","企业","信息","安全",
  "标准","社会","政策","机制","人员","资源",
  "规定","技术","生产","数据","设施","行业"
)

# 再删一轮
toks2 <- tokens_remove(toks, pattern = stop_round2)

dfm2 <- dfm(toks2) %>%
  dfm_trim(min_docfreq = 5) %>%
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")

topfeatures(dfm2, 30)

dtm2 <- convert(dfm2, to = "topicmodels")

lda2 <- LDA(dtm2, k = 10, control = list(seed = 1234))

terms(lda2, 12)

# 第三轮
stop_round3 <- c(
  "通知","附件","办公","国务院","贯彻","坚持",
  "中华","共和国","中国","人民","进行","及时",
  "要求","意见","公告"
)

toks3 <- tokens_remove(toks2, pattern = stop_round3)

dfm3 <- dfm(toks3) %>%
  dfm_trim(min_docfreq = 5) %>%
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")

dtm3 <- convert(dfm3, "topicmodels")

lda3 <- LDA(dtm3, k = 10, control = list(seed = 1234))

terms(lda3, 12)


#定义主题空间
#为最后的ML做准备，再来一次数据清洗
library(dplyr)
library(stringr)
library(readr)
library(lubridate)

library(tidymodels)
library(textrecipes)

df <- para_df %>%
  mutate(
    url        = str_squish(as.character(url)),
    para_id    = as.integer(para_id),
    para_text  = str_squish(as.character(para_text)),
    doc_type   = str_squish(as.character(doc_type)),
    topic_lvl1 = str_squish(as.character(topic_lvl1)),
    topic_lvl2 = str_squish(as.character(topic_lvl2)),
    
    pubdate = as.Date(pubdate),
    year    = lubridate::year(pubdate),
    
    paragraph_id   = paste0(url, "#", para_id),
    paragraph_text = para_text
  ) %>%
  filter(
    !is.na(paragraph_id), paragraph_id != "",
    !is.na(paragraph_text), paragraph_text != "",
    !is.na(year)
  )
setwd("~/Desktop/chineseresearch")
write_csv(df, "para_df_clean_2021_2025.csv")

# 进行分层抽样
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(purrr)

library(quanteda)
library(topicmodels)

set.seed(1)

# =======================
# 0) 参数
# =======================
n_total <- 500
admin_quantile_keep <- 0.90           # 保留前90%（删最重10%）
boilerplate_share_in_final <- 0.10    # 最终样本里保留10%“高套话池”
lda_k <- 10                           # 你可以用 10（对齐你 lda3），或改成 20

# =======================
# A) 纯行政套话词表（不含 instrument 核心词）
# =======================
admin_boiler_words <- c(
  "加强","推进","推动","落实","完善","健全","深化","提升","促进","统筹","协调","部署",
  "积极","持续","全面","深入","进一步","扎实","切实","稳步","有序","有效","严格","确保",
  "有关","相关","应当","按照","根据","结合","符合","以及","包括","其中","方面","内容",
  "情况","问题","重点","重要","相应","具体","需要","原则",
  "各地","各级","地方","全国","地区","区域","基层",
  "工作","部门","单位","机构","组织"
)
admin_pattern <- paste0(admin_boiler_words, collapse = "|")

# =======================
# 1) 准备数据 + admin_score
# =======================
df2 <- df %>%
  mutate(
    year = as.integer(year),
    topic_lvl1 = str_squish(as.character(topic_lvl1)),
    paragraph_text = str_squish(as.character(paragraph_text)),
    text_len = nchar(paragraph_text)
  ) %>%
  filter(
    !is.na(year),
    !is.na(topic_lvl1), topic_lvl1 != "",
    !is.na(paragraph_text), paragraph_text != "",
    text_len >= 30
  ) %>%
  distinct(paragraph_id, .keep_all = TRUE) %>%
  mutate(
    admin_hits  = str_count(paragraph_text, admin_pattern),
    admin_score = admin_hits / pmax(text_len, 1)
  )

# =======================
# 2) 双池：clean vs boiler
# =======================
cut <- quantile(df2$admin_score, probs = admin_quantile_keep, na.rm = TRUE)
pool_clean  <- df2 %>% filter(admin_score <= cut)
pool_boiler <- df2 %>% filter(admin_score >  cut)

if (nrow(pool_boiler) < 50) {
  message("High-boilerplate pool too small; relax threshold to 0.85 (top 15%).")
  admin_quantile_keep <- 0.85
  cut <- quantile(df2$admin_score, probs = admin_quantile_keep, na.rm = TRUE)
  pool_clean  <- df2 %>% filter(admin_score <= cut)
  pool_boiler <- df2 %>% filter(admin_score >  cut)
}

n_boiler <- round(n_total * boilerplate_share_in_final)
n_clean  <- n_total - n_boiler

message("Pool sizes: clean=", nrow(pool_clean), " boiler=", nrow(pool_boiler))
message("Sample plan: clean=", n_clean, " boiler=", n_boiler)

# =======================
# 3) 用“LDA topic”作为语义锚点（不做Kmeans）
# =======================

# ---- 3.1 LDA：返回与 pool_df 同长度的 dominant_topic_full ----
fit_lda_for_pool <- function(text_vec, k_topics = 10, seed = 1) {
  set.seed(seed)
  
  idx_nonempty <- !is.na(text_vec) & str_squish(text_vec) != ""
  dominant_full <- rep(0L, length(text_vec))  # 0 = 无法入模/空doc
  
  if (sum(idx_nonempty) < 50) {
    return(list(dominant_topic_full = dominant_full))
  }
  
  txt_sub <- text_vec[idx_nonempty]
  
  toks <- tokens(
    txt_sub,
    what = "word",
    remove_punct = TRUE,
    remove_numbers = TRUE
  )
  toks <- tokens_remove(toks, stopwords(language = "zh", source = "misc"))
  toks <- tokens_keep(toks, pattern = "^[\\p{Han}]{2,}$", valuetype = "regex")
  
  idx_has_token <- ntoken(toks) > 0
  if (sum(idx_has_token) < 50) {
    return(list(dominant_topic_full = dominant_full))
  }
  
  toks2 <- toks[idx_has_token]
  map_back_idx <- which(idx_nonempty)[idx_has_token]
  
  dfm0 <- dfm(toks2) %>%
    dfm_trim(min_docfreq = 5) %>%
    dfm_trim(max_docfreq = 0.7, docfreq_type = "prop")
  
  if (ndoc(dfm0) < 50 || nfeat(dfm0) < 50) {
    return(list(dominant_topic_full = dominant_full))
  }
  
  keep_docs <- ntoken(dfm0) > 0
  dfm1 <- dfm0[keep_docs, ]
  map_back_idx2 <- map_back_idx[keep_docs]
  
  if (ndoc(dfm1) < 50 || nfeat(dfm1) < 50) {
    return(list(dominant_topic_full = dominant_full))
  }
  
  dtm <- convert(dfm1, to = "topicmodels")
  
  k2 <- min(k_topics, max(2, nrow(dtm) - 1), max(2, ncol(dtm) - 1))
  lda <- LDA(dtm, k = k2, control = list(seed = seed))
  
  gamma <- posterior(lda)$topics
  dominant <- apply(gamma, 1, which.max)
  
  dominant_full[map_back_idx2] <- as.integer(dominant)
  list(dominant_topic_full = dominant_full)
}

# ---- 3.2 年内：按 dominant_topic 覆盖抽样 ----
make_year_sample_by_topic <- function(dfy, n_take) {
  dfy <- dfy %>%
    mutate(dominant_topic = if_else(is.na(dominant_topic), 0L, as.integer(dominant_topic)))
  
  plan_t <- dfy %>%
    count(dominant_topic, name = "n_t") %>%
    mutate(w = n_t / sum(n_t),
           t_take = round(w * n_take))
  
  diff_t <- n_take - sum(plan_t$t_take)
  if (diff_t != 0) {
    plan_t <- plan_t %>%
      arrange(desc(n_t)) %>%
      mutate(t_take = ifelse(row_number() <= abs(diff_t),
                             t_take + sign(diff_t),
                             t_take))
  }
  plan_t$t_take <- pmax(0, plan_t$t_take)
  
  samp <- dfy %>%
    left_join(plan_t %>% select(dominant_topic, t_take), by = "dominant_topic") %>%
    group_by(dominant_topic) %>%
    group_modify(~{
      k <- .x$t_take[1]
      if (is.na(k) || k <= 0) return(.x[0, ])
      slice_sample(.x, n = min(k, nrow(.x)), replace = FALSE)
    }) %>%
    ungroup() %>%
    select(-t_take)
  
  if (nrow(samp) < n_take) {
    need <- n_take - nrow(samp)
    rest <- dfy %>% anti_join(samp, by = "paragraph_id")
    if (nrow(rest) > 0) {
      samp <- bind_rows(samp, slice_sample(rest, n = min(need, nrow(rest))))
    }
  }
  
  samp
}

# ---- 3.3 从 pool 抽样：先打 dominant_topic，再按 year×topic 抽 ----
sample_from_pool_topic <- function(pool_df, n_take_total, tag, k_topics = 10, seed = 1) {
  if (n_take_total <= 0 || nrow(pool_df) == 0) return(tibble())
  
  lda_obj <- fit_lda_for_pool(pool_df$paragraph_text, k_topics = k_topics, seed = seed)
  
  pool_df <- pool_df %>%
    mutate(
      dominant_topic = lda_obj$dominant_topic_full,
      pool_tag = tag
    )
  
  year_plan <- pool_df %>%
    count(year, name = "n_year") %>%
    mutate(w = n_year / sum(n_year),
           n_take_year = round(w * n_take_total))
  
  diff_n <- n_take_total - sum(year_plan$n_take_year)
  if (diff_n != 0) {
    year_plan <- year_plan %>%
      arrange(desc(n_year)) %>%
      mutate(n_take_year = ifelse(row_number() <= abs(diff_n),
                                  n_take_year + sign(diff_n),
                                  n_take_year))
  }
  year_plan$n_take_year <- pmax(1, year_plan$n_take_year)
  
  out <- year_plan %>%
    mutate(
      data = map(year, ~ pool_df %>% filter(year == .x)),
      samp = map2(data, n_take_year, make_year_sample_by_topic)
    ) %>%
    select(samp) %>%
    unnest(samp) %>%
    distinct(paragraph_id, .keep_all = TRUE) %>%
    slice_head(n = n_take_total)
  
  out
}



# =======================
# 4) 执行抽样（clean + boiler）
# =======================
s_clean  <- sample_from_pool_topic(pool_clean,  n_clean,  tag = "clean",  k_topics = lda_k, seed = 1)
s_boiler <- sample_from_pool_topic(pool_boiler, n_boiler, tag = "boiler", k_topics = lda_k, seed = 1)

sample_df <- bind_rows(s_clean, s_boiler) %>%
  distinct(paragraph_id, .keep_all = TRUE)

# 不足则从clean补抽
if (nrow(sample_df) < n_total) {
  need <- n_total - nrow(sample_df)
  rest <- pool_clean %>% anti_join(sample_df, by = "paragraph_id")
  if (nrow(rest) > 0) {
    sample_df <- bind_rows(sample_df, slice_sample(rest, n = min(need, nrow(rest))))
  }
}

sample_df <- sample_df %>% slice_head(n = n_total)

# =======================
# 5) 输出 label sheet（含 domain_secondary）
# =======================
label_sheet <- sample_df %>%
  transmute(
    paragraph_id,
    url,
    para_id,
    year,
    doc_type,
    topic_lvl1,
    topic_lvl2,
    pool_tag,
    dominant_topic,     # 用于检查覆盖
    admin_score,        # 用于检查套话
    paragraph_text,
    domain_label = "",
    domain_secondary = "",
    instrument_labels = ""
  )

setwd("~/Desktop/chineseresearch")
write_csv(label_sheet, "label_sheet_500_v6.csv")

# =======================
# 6) 快速检查
# =======================
cat("Rows:", nrow(label_sheet), "\n")
print(label_sheet %>% count(pool_tag))
print(label_sheet %>% count(year, sort = TRUE))
print(label_sheet %>% count(dominant_topic, sort = TRUE))



# 利用label后的数据进行模型的训练
# =========================================================
# 0) Packages
# =========================================================
library(tidyverse)
library(tidymodels)
library(tokenizers)
library(purrr)

# IMPORTANT: step_tokenize / step_tfidf are from textrecipes
library(textrecipes)

# =========================================================
# 1) Prepare labeled dataset (500 rows)
#    - domain_label is primary label
#    - domain_secondary is optional secondary label (relaxed evaluation)
# =========================================================
label500 <- label %>%
  filter(domain_label != "") %>%
  transmute(
    text = paragraph_text,
    domain = factor(domain_label),
    domain_secondary = na_if(domain_secondary, ""),     # keep NA for empty
    doc_type = factor(doc_type),
    topic_lvl1 = factor(topic_lvl1),
    topic_lvl2 = factor(topic_lvl2),
    dominant_topic = factor(dominant_topic),
    admin_score = as.numeric(admin_score)
  ) %>%
  mutate(
    text_bigram = map_chr(
      text,
      ~ {
        bg <- tokenizers::tokenize_ngrams(.x, n = 2, n_min = 2)[[1]]
        if (length(bg) == 0) "" else paste(bg, collapse = " ")
      }
    )
  )

# =========================================================
# 2) Train/Test split + CV folds
# =========================================================
set.seed(123)
split <- initial_split(label500, prop = 0.8, strata = domain)
train <- training(split) %>% mutate(row_id = row_number())
test  <- testing(split)  %>% mutate(row_id = row_number())

set.seed(123)
folds <- vfold_cv(train, v = 5, strata = domain)

# =========================================================
# 3) Recipe: TF-IDF (unigram + bigram) + structured features
# =========================================================
rec <- recipe(
  domain ~ text + text_bigram + doc_type + topic_lvl1 + topic_lvl2 + dominant_topic + admin_score,
  data = train
) %>%
  # text unigram
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 8000) %>%
  step_tfidf(text) %>%
  
  # text bigram (already space-separated bigram tokens)
  step_tokenize(text_bigram) %>%
  step_tokenfilter(text_bigram, max_tokens = 8000) %>%
  step_tfidf(text_bigram) %>%
  
  # structured
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(admin_score)

# =========================================================
# 4) Model A: glmnet multinomial (tuned)
# =========================================================
glmnet_mod <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

wf_glmnet <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glmnet_mod)

grid <- grid_regular(penalty(), levels = 20)

set.seed(123)
res_glmnet <- tune_grid(
  wf_glmnet,
  resamples = folds,
  grid = grid,
  metrics = metric_set(f_meas, accuracy),
  control = control_grid(save_pred = TRUE)
)

# macro F1 (CV)
macro_f1_glmnet <- res_glmnet %>%
  collect_predictions() %>%
  f_meas(truth = domain, estimate = .pred_class, estimator = "macro")

macro_f1_glmnet

# confusion matrix (CV) - pick best penalty first, then collect preds for that
best_glmnet <- select_best(res_glmnet, metric = "f_meas")

glmnet_pred_cv <- res_glmnet %>%
  collect_predictions(parameters = best_glmnet) %>%
  left_join(
    train %>%
      mutate(.row = row_number()) %>%
      select(.row, domain_secondary),
    by = ".row"
  )

cm_glmnet <- glmnet_pred_cv %>%
  conf_mat(truth = domain, estimate = .pred_class)

cm_glmnet

# =========================================================
# 5) Model B: Linear SVM (LiblineaR)
# =========================================================
svm_mod <- svm_linear(mode = "classification") %>%
  set_engine("LiblineaR")

wf_svm <- workflow() %>%
  add_recipe(rec) %>%
  add_model(svm_mod)

set.seed(123)
res_svm <- fit_resamples(
  wf_svm,
  resamples = folds,
  metrics = metric_set(f_meas, accuracy),
  control = control_resamples(save_pred = TRUE)
)

macro_f1_svm <- res_svm %>%
  collect_predictions() %>%
  f_meas(truth = domain, estimate = .pred_class, estimator = "macro")

macro_f1_svm

svm_pred_cv <- res_svm %>%
  collect_predictions() %>%
  left_join(
    train %>%
      mutate(.row = row_number()) %>%
      select(.row, domain_secondary),
    by = ".row"
  )

cm_svm <- svm_pred_cv %>%
  conf_mat(truth = domain, estimate = .pred_class)

cm_svm

# =========================================================
# 6) Relaxed accuracy: domain_secondary counts as correct too
# =========================================================
relaxed_metrics <- function(pred_df) {
  pred_df %>%
    mutate(
      correct_relaxed =
        (.pred_class == domain) |
        (!is.na(domain_secondary) & .pred_class == domain_secondary)
    ) %>%
    summarise(
      relaxed_accuracy = mean(correct_relaxed),
      n = n()
    )
}

relaxed_glmnet_cv <- relaxed_metrics(glmnet_pred_cv)
relaxed_svm_cv    <- relaxed_metrics(svm_pred_cv)

relaxed_glmnet_cv
relaxed_svm_cv

# =========================================================
# 7) (Optional) Fit final model on full train and evaluate on test
#    - choose best between glmnet & svm based on CV macro F1 / relaxed accuracy
# =========================================================

# ---- final glmnet on train, evaluate on test ----
final_wf_glmnet <- finalize_workflow(wf_glmnet, best_glmnet)

final_fit_glmnet <- final_wf_glmnet %>% fit(data = train)

test_pred_glmnet <- predict(final_fit_glmnet, new_data = test) %>%
  bind_cols(test %>% select(domain, domain_secondary))

test_macro_f1_glmnet <- test_pred_glmnet %>%
  f_meas(truth = domain, estimate = .pred_class, estimator = "macro")

test_cm_glmnet <- test_pred_glmnet %>%
  conf_mat(truth = domain, estimate = .pred_class)

test_relaxed_glmnet <- relaxed_metrics(
  test_pred_glmnet %>% rename(.pred_class = .pred_class) # keep same colname
)

test_macro_f1_glmnet
test_cm_glmnet
test_relaxed_glmnet

# ---- final svm on train, evaluate on test ----
final_fit_svm <- wf_svm %>% fit(data = train)

test_pred_svm <- predict(final_fit_svm, new_data = test) %>%
  bind_cols(test %>% select(domain, domain_secondary))

test_macro_f1_svm <- test_pred_svm %>%
  f_meas(truth = domain, estimate = .pred_class, estimator = "macro")

test_cm_svm <- test_pred_svm %>%
  conf_mat(truth = domain, estimate = .pred_class)

test_relaxed_svm <- relaxed_metrics(test_pred_svm)

test_macro_f1_svm
test_cm_svm
test_relaxed_svm



# =========================================================
# FINAL PRODUCTION: Train SVM + Batch Predict + Attention
# =========================================================

library(tidyverse)
library(tidymodels)
library(textrecipes)
library(tokenizers)
library(stringr)
library(lubridate)

setwd("~/Desktop/chineseresearch")

label_file <- "label.csv"
para_file  <- "para_analysis.csv"

out_dir   <- "data/07_attention"
model_dir <- "data/06_model"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------
# 1) Helper
# ---------------------------------------------------------
make_bigram_text <- function(x) {
  x <- str_squish(as.character(x))
  bg <- tokenizers::tokenize_ngrams(x, n = 2, n_min = 2)[[1]]
  if (length(bg) == 0) "" else paste(bg, collapse = " ")
}

# ---------------------------------------------------------
# 2) Load & Prepare Label Data
# ---------------------------------------------------------
label_raw <- read_csv(label_file, show_col_types = FALSE)

label500 <- label_raw %>%
  filter(!is.na(domain_label), domain_label != "") %>%
  transmute(
    text = str_squish(paragraph_text),
    domain = factor(domain_label),
    doc_type = factor(doc_type),
    topic_lvl1 = factor(topic_lvl1),
    topic_lvl2 = factor(topic_lvl2),
    dominant_topic = factor(dominant_topic),
    admin_score = as.numeric(admin_score),
    text_bigram = map_chr(text, make_bigram_text)
  )

print(label500 %>% count(domain, sort = TRUE))

# ---------------------------------------------------------
# 3) Recipe
# ---------------------------------------------------------
rec_final <- recipe(
  domain ~ text + text_bigram + doc_type + topic_lvl1 +
    topic_lvl2 + dominant_topic + admin_score,
  data = label500
) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 5000) %>%   # 降维更稳
  step_tfidf(text) %>%
  step_tokenize(text_bigram) %>%
  step_tokenfilter(text_bigram, max_tokens = 5000) %>%
  step_tfidf(text_bigram) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(admin_score)

svm_mod <- svm_linear(mode = "classification") %>%
  set_engine("LiblineaR")

wf_final <- workflow() %>%
  add_recipe(rec_final) %>%
  add_model(svm_mod)

set.seed(123)
final_fit <- fit(wf_final, data = label500)

saveRDS(final_fit, file.path(model_dir, "final_svm_workflow.rds"))

# ---------------------------------------------------------
# 4) Batch Prediction
# ---------------------------------------------------------
para_master <- read_csv(para_file, show_col_types = FALSE) %>%
  mutate(
    paragraph_id = paste0(str_trim(url), "#", as.integer(para_id)),
    pubdate = as.Date(pubdate),
    year = year(pubdate),
    para_len = as.integer(para_len)
  ) %>%
  filter(!is.na(year))

out_pred <- file.path(out_dir, "para_predictions_class.csv")
if (file.exists(out_pred)) file.remove(out_pred)

batch_size <- 1500
n <- nrow(para_master)
idx <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

for (b in seq_along(idx)) {
  message("Predict batch ", b, "/", length(idx))
  
  chunk <- para_master[idx[[b]], ] %>%
    transmute(
      paragraph_id,
      url,
      pubdate,
      year,
      para_len,
      text = str_squish(para_text),
      doc_type,
      topic_lvl1,
      topic_lvl2,
      dominant_topic = "0",
      admin_score = 0,
      text_bigram = map_chr(text, make_bigram_text)
    )
  
  pred_b <- predict(final_fit, new_data = chunk) %>%
    bind_cols(chunk %>% select(paragraph_id, url, pubdate, year, para_len)) %>%
    rename(pred_domain = .pred_class)
  
  if (!file.exists(out_pred)) {
    write_csv(pred_b, out_pred)
  } else {
    write_csv(pred_b, out_pred, append = TRUE)
  }
  
  rm(chunk, pred_b)
  gc()
}

message("Prediction finished.")

# ---------------------------------------------------------
# 5) Compute Attention
# ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)

pred_all <- read_csv("data/07_attention/para_predictions_class.csv",
                     show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    pred_domain = str_squish(pred_domain),  # 清理空格/不可见空白
    weight = pmin(as.integer(para_len), 300)
  ) %>%
  filter(!is.na(year), !is.na(pred_domain), pred_domain != "")

att_fix <- pred_all %>%
  group_by(year, pred_domain) %>%
  summarise(
    n = n(),
    w_sum = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    share = n / sum(n),
    w_share = w_sum / sum(w_sum)
  ) %>%
  ungroup() %>%
  arrange(pred_domain, year)

# 检查：必须全是1
att_fix %>% count(year, pred_domain) %>% summarise(max_n = max(n))

#画图
library(tidyverse)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      text = element_text(family = "Songti SC")
    )
)


p_trend <- att_fix %>%
  ggplot(aes(x = year, y = share, color = pred_domain, group = pred_domain)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2021:2025) +
  labs(
    title = "2021–2025年国务院执行层功能场域结构分布",
    x = "年份",
    y = "注意力占比",
    color = "治理目标"
  )

p_trend

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      text = element_text(family = "PingFang SC"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
)
p_area <- att_fix %>%
  ggplot(aes(x = year, y = share, fill = pred_domain)) +
  geom_area(position = "stack", alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "2021–2025年国务院执行层政策注意力结构",
    x = "年份",
    y = "注意力占比",
    fill = "治理目标"
  )  +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "PingFang SC"),
    plot.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )

p_area

library(tidyverse)
library(scales)

att_top1 <- read_csv("data/07_attention/attention_year_domain.csv",
                     show_col_types = FALSE)

# 验尸：每年=1
att_top1 %>% group_by(year) %>% summarise(sum_share = sum(share), sum_w = sum(w_share))

# 分面对比图（复刻你之前那张）
att_long <- att_top1 %>%
  select(year, pred_domain, share, w_share) %>%
  pivot_longer(c(share, w_share), names_to = "type", values_to = "value") %>%
  mutate(type = recode(type,
                       share = "段落数量占比",
                       w_share = "长度加权占比"))

p_compare <- ggplot(att_long, aes(x = year, y = value, color = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  facet_wrap(~ pred_domain, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "执行强度对比：段落数量 vs 长度加权",
    x = "年份",
    y = "注意力占比",
    color = "指标类型"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "PingFang SC"),
    plot.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )

p_compare


# 查看para和结构的关系
library(tidyverse)
library(lubridate)
library(scales)

# 1) 读段落母表（结构变量都在这里）
para_df <- read_csv("para_analysis.csv", show_col_types = FALSE) %>%
  mutate(
    url = str_trim(url),
    para_id = as.integer(para_id),
    pubdate = as.Date(pubdate),
    year = year(pubdate),
    paragraph_id = paste0(url, "#", para_id)
  )

# 2) 读预测结果（有 pred_domain）
pred <- read_csv("data/07_attention/para_predictions_class.csv", show_col_types = FALSE) %>%
  mutate(
    paragraph_id = str_trim(paragraph_id),
    pred_domain = str_squish(pred_domain),
    weight = pmin(as.integer(para_len), 300)
  )

# 3) 合并
para_pred <- para_df %>%
  left_join(pred %>% select(paragraph_id, pred_domain, weight),
            by = "paragraph_id") %>%
  filter(!is.na(pred_domain))

glimpse(para_pred)

library(vcd)

# doc_type vs pred_domain
tab_doc <- table(para_pred$doc_type, para_pred$pred_domain)
assocstats(tab_doc)$cramer

# topic_lvl1 vs pred_domain
tab_lvl1 <- table(para_pred$topic_lvl1, para_pred$pred_domain)
assocstats(tab_lvl1)$cramer

library(tidymodels)

set.seed(1)

df_reg <- para_pred %>%
  select(pred_domain, doc_type, topic_lvl1, topic_lvl2, year) %>%
  mutate(
    pred_domain = factor(pred_domain),
    doc_type = factor(doc_type),
    topic_lvl1 = factor(topic_lvl1),
    topic_lvl2 = factor(topic_lvl2),
    year = factor(year)
  )

split <- initial_split(df_reg, prop = 0.8, strata = pred_domain)
train <- training(split)
test  <- testing(split)

rec_struct <- recipe(pred_domain ~ doc_type + topic_lvl1 + topic_lvl2 + year, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

mod <- multinom_reg(penalty = 0.01, mixture = 1) %>%  # 一个固定值先跑baseline
  set_engine("glmnet")

wf <- workflow() %>% add_recipe(rec_struct) %>% add_model(mod)
fit_struct <- fit(wf, data = train)

pred_test <- predict(fit_struct, new_data = test) %>%
  bind_cols(test %>% select(pred_domain))

metrics(pred_test, truth = pred_domain, estimate = .pred_class)
conf_mat(pred_test, truth = pred_domain, estimate = .pred_class)

# 计算每个 topic_lvl1 内 domain 的集中度
library(tidyverse)

lvl1_mix <- para_pred %>%
  count(topic_lvl1, pred_domain, name = "n") %>%
  group_by(topic_lvl1) %>%
  mutate(p = n / sum(n)) %>%
  summarise(
    n_total = sum(n),
    HHI = sum(p^2),                            # 越接近1越“一边倒”
    entropy = -sum(p * log(p + 1e-12)),        # 越小越“一边倒”
    top_domain = pred_domain[which.max(p)],
    top_share = max(p),
    .groups = "drop"
  ) %>%
  arrange(desc(top_share))

lvl1_mix %>% print(n = 50)

# 建立“结构基线 vs 语义增量”的证据链
library(tidymodels)

pred_test2 <- pred_test %>%
  mutate(.pred_class = .pred_class)

f_meas(pred_test2, truth = pred_domain, estimate = .pred_class, estimator = "macro")


# “部委 × 场域”的标准分析流程
# 把 puborg / source_group 合并回段落表
setwd("~/Desktop/chineseresearch")
library(tidyverse)
library(lubridate)
library(stringr)

# doc-level metadata: 你之前生成的 analysis_df / analysis1.csv
meta_doc <- read_csv("analysis1.csv", show_col_types = FALSE) %>%
  transmute(
    url = str_trim(url),
    puborg = str_squish(puborg),
    source_group = str_squish(source_group)  # 国务院/国办 vs 国务院部门
  ) %>%
  distinct(url, .keep_all = TRUE)

# para-level + prediction
para_pred <- read_csv("para_analysis.csv", show_col_types = FALSE) %>%
  mutate(
    url = str_trim(url),
    para_id = as.integer(para_id),
    pubdate = as.Date(pubdate),
    year = year(pubdate),
    paragraph_id = paste0(url, "#", para_id)
  ) %>%
  left_join(meta_doc, by = "url") %>%
  left_join(
    read_csv("data/07_attention/para_predictions_class.csv", show_col_types = FALSE) %>%
      transmute(paragraph_id = str_trim(paragraph_id),
                pred_domain = str_squish(pred_domain),
                weight = pmin(as.integer(para_len), 300)),
    by = "paragraph_id"
  ) %>%
  filter(!is.na(pred_domain))

glimpse(para_pred)

# 做一个 puborg 清洗函数（统一分隔符、去括号等）
library(dplyr)
library(stringr)
library(tidyr)

clean_puborg <- function(x){
  x %>%
    str_replace_all("等$", "") %>%   # 去掉尾部“等”
    str_replace_all("[，,、/；;｜|]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}


# 先做一个“部委注意力画像”
para_long_org <- para_pred %>%
  mutate(puborg_clean = clean_puborg(puborg)) %>%
  separate_rows(puborg_clean, sep = " ") %>%
  filter(puborg_clean != "") %>%
  filter(
    puborg_clean != "国务院",
    puborg_clean != "国务院办公厅"
  )

att_org <- para_long_org %>%
  group_by(puborg_clean, pred_domain) %>%
  summarise(
    n = n(),
    w_sum = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(puborg_clean) %>%
  mutate(w_share = w_sum / sum(w_sum)) %>%
  ungroup()

# 过滤小样本（否则 HHI/entropy 全是“假专一”）
min_n <- 300
top_org <- att_org %>%
  group_by(puborg_clean) %>%
  summarise(n_total = sum(n), .groups="drop") %>%
  filter(n_total >= min_n) %>%
  slice_max(n_total, n = 20) %>%
  pull(puborg_clean)

plot_df <- att_org %>% filter(puborg_clean %in% top_org)


library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# 1) 统一清洗（发改委合并 + 去办公厅 + 去空格）
para_clean <- para_long_org %>%
  mutate(
    puborg_clean = str_trim(puborg_clean),
    puborg_clean = str_replace_all(puborg_clean, "\\s+", ""),
    puborg_clean = str_remove(puborg_clean, "办公厅$"),
    puborg_clean = case_when(
      puborg_clean %in% c("发展改革委", "国家发展改革委") ~ "国家发展改革委",
      TRUE ~ puborg_clean
    )
  )

# 2) 选 Top N 机构（按段落量）
top_org <- para_clean %>%
  count(puborg_clean, name = "n_total") %>%
  slice_max(n_total, n = 20) %>%
  pull(puborg_clean)

# 3) 计算 share（只对 Top N）
plot_df_top <- para_clean %>%
  filter(puborg_clean %in% top_org) %>%
  count(puborg_clean, pred_domain, name = "n") %>%
  group_by(puborg_clean) %>%
  mutate(w_share = n / sum(n)) %>%
  ungroup()

# 4) 作图（注意：用 plot_df_top，不要用旧 plot_df）
ggplot(plot_df_top, aes(x = pred_domain, y = w_share, fill = pred_domain)) +
  geom_col() +
  facet_wrap(~ puborg_clean, ncol = 5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "机构—场域注意力画像（Top 20 机构）", x = NULL, y = "注意力占比") +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = cn_font),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 做“机构内部的场域集中度”（拆分联合发文口径）
ministry_mix <- para_long_org %>%   
  count(puborg_clean, pred_domain, name="n") %>%
  group_by(puborg_clean) %>%
  mutate(p = n/sum(n)) %>%
  summarise(
    n_total = sum(n),
    HHI = sum(p^2),
    entropy = pmax(-sum(p*log(p + 1e-12)), 0),
    top_domain = pred_domain[which.max(p)],
    top_share = max(p),
    .groups="drop"
  ) %>%
  filter(n_total >= 300) %>%   # ✅ 核心：过滤小样本
  arrange(desc(top_share))

ministry_mix %>% print(n = 50)

# Doc_type × 场域
doc_domain <- para_pred %>%
  group_by(doc_type, pred_domain) %>%
  summarise(
    n = n(),
    w_sum = sum(weight, na.rm=TRUE),
    .groups="drop"
  ) %>%
  group_by(doc_type) %>%
  mutate(
    share = n/sum(n),
    w_share = w_sum/sum(w_sum)
  ) %>%
  ungroup()
ggplot(doc_domain, aes(doc_type, pred_domain, fill = w_share)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 执行强度差异（文本密度分析）
lvl1_mix <- lvl1_mix %>%
  mutate(
    re_embedding_index = 1 - top_share
  )
doc_domain <- doc_domain %>%
  mutate(
    intensity_diff = w_share - share
  )
domain_intensity <- para_pred %>%
  group_by(pred_domain) %>%
  summarise(
    share = n()/nrow(para_pred),
    w_share = sum(weight)/sum(para_pred$weight),
    .groups="drop"
  ) %>%
  mutate(
    intensity_diff = w_share - share
  )
ggplot(lvl1_mix, aes(x = reorder(topic_lvl1, re_embedding_index),
                     y = re_embedding_index)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

ggplot(domain_intensity,
       aes(x = pred_domain, y = intensity_diff, fill = pred_domain)) +
  geom_col() +
  theme_minimal()
