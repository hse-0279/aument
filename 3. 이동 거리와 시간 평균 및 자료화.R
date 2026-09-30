library(data.table)

# ---- 1. 폴더 경로 및 파일 목록 ----
folder_path <- "output/테스트"

file_list <- list.files(
  path = folder_path,
  pattern = "^trips_sample_with_kakao_[0-9]{8}\\.csv$",
  full.names = TRUE
)

cat("찾은 파일 수:", length(file_list), "\n")
print(file_list)

# ---- 2. 각 파일에서 날짜를 추출하면서 하나로 합치기 ----
all_days <- lapply(file_list, function(f) {
  dt <- fread(f, encoding = "UTF-8")
  
  # 파일명에서 날짜(YYYYMMDD)만 추출
  date_str <- regmatches(f, regexpr("[0-9]{8}", f))
  dt[, source_date := date_str]
  
  dt
})

trips_all_days <- rbindlist(all_days, use.names = TRUE, fill = TRUE)

# ---- 3. 확인 ----
cat("\n합친 후 전체 행 수:", nrow(trips_all_days), "\n")
table(trips_all_days$source_date)




# ---- 2. 이 한 그룹에 대해 평균 계산 ----
group_summary <- trips_all_days[, .(
  n_days = .N,                                   # 이 통행이 며칠치 확보됐는지
  n_valid = sum(!is.na(kakao_duration_sec)),      # 그 중 실제로 성공한 날 수
  
  mean_kakao_duration_sec = mean(kakao_duration_sec, na.rm = TRUE),
  mean_kakao_distance_m   = mean(kakao_distance_m, na.rm = TRUE)
), by = .(person_id, leg_index)]

print(group_summary)

library(data.table)

trips_sample <- fread("output/trips_sample_final_20260912.csv", encoding = "UTF-8")

# ---- 원하는 열만 선택 ----
trips_subset <- trips_sample[, .(direction, distance_bin, person_id, leg_index, mode,
                                 dest_type, dep_time, sim_trav_time, sim_distance)]


# ---- person_id + leg_index 기준으로 병합 ----
trips_merged <- merge(
  trips_subset,
  group_summary[, .(person_id, leg_index, n_days, n_valid,
                    mean_kakao_duration_sec, mean_kakao_distance_m)],
  by = c("person_id", "leg_index"),
  all.x = TRUE   # trips_subset 기준으로 왼쪽 조인 (매칭 안 되는 것도 살려둠)
)

# ---- 확인 ----
head(trips_merged)
nrow(trips_merged)   # trips_subset과 행 수 같아야 함(4725)

# ---- 혹시 매칭 안 된 행(카카오 결과 없음)이 있는지 확인 ----
sum(is.na(trips_merged$mean_kakao_duration_sec))

# ---- sim_trav_time(HH:MM:SS)을 초 단위로 변환 ----
trips_merged[, sim_trav_time_sec := {
  parts <- tstrsplit(sim_trav_time, ":")
  as.numeric(parts[[1]]) * 3600 + as.numeric(parts[[2]]) * 60 + as.numeric(parts[[3]])
}]






library(ggplot2)
library(data.table)

# ---- 1. 절대 오차율(%) 계산 — 카카오(API)를 기준으로 시뮬레이션이 얼마나 벗어났는지 ----
trips_merged[, error_distance_pct := abs(100 * (sim_distance - mean_kakao_distance_m) / mean_kakao_distance_m)]
trips_merged[, error_time_pct     := abs(100 * (sim_trav_time_sec - mean_kakao_duration_sec) / mean_kakao_duration_sec)]

# ---- 2. 박스플롯을 위해 long format으로 변환 ----
plot_data <- rbind(
  trips_merged[, .(direction, distance_bin, error_pct = error_distance_pct, metric = "Travel Distance (%)")],
  trips_merged[, .(direction, distance_bin, error_pct = error_time_pct, metric = "Travel Time (%)")]
)

# ---- 3. 방향(NS/EW) 기준 박스플롯 ----
ggplot(plot_data, aes(x = direction, y = error_pct, fill = direction)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4) +
  facet_wrap(~ metric, ncol = 2) +
  scale_fill_manual(values = c("NS" = "#1D9E75", "EW" = "#D85A30")) +
  labs(title = "카카오 API 대비 시뮬레이션 재현력 평가 (방향별)",
       subtitle = "값이 0에 가까울수록 재현이 잘 된 것",
       x = "방향", y = "절대 오차율 (%)", fill = "방향") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))

# ---- 4. 거리구간(Q1~Q4)까지 세분화 ----
ggplot(plot_data, aes(x = distance_bin, y = error_pct, fill = direction)) +
  geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.3, position = position_dodge(0.8)) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("NS" = "#1D9E75", "EW" = "#D85A30")) +
  labs(title = "카카오 API 대비 시뮬레이션 재현력 평가 (방향 × 거리구간)",
       subtitle = "값이 0에 가까울수록 재현이 잘 된 것",
       x = "거리구간", y = "절대 오차율 (%)", fill = "방향") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(size = 12, face = "bold"))


library(data.table)

# ---- 방향 x 거리구간별 오차율 요약 통계 ----
error_summary <- plot_data[, .(
  n = .N,
  mean = round(mean(error_pct, na.rm = TRUE), 1),
  median = round(median(error_pct, na.rm = TRUE), 1),
  sd = round(sd(error_pct, na.rm = TRUE), 1),
  q25 = round(quantile(error_pct, .25, na.rm = TRUE), 1),
  q75 = round(quantile(error_pct, .75, na.rm = TRUE), 1),
  min = round(min(error_pct, na.rm = TRUE), 1),
  max = round(max(error_pct, na.rm = TRUE), 1)
), by = .(metric, direction, distance_bin)]

# 보기 좋게 정렬
error_summary <- error_summary[order(metric, direction, distance_bin)]
print(error_summary)

# ---- CSV로도 저장 ----
fwrite(error_summary, "output/error_summary_table.csv", bom = TRUE)
