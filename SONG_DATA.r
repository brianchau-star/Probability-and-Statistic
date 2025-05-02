# Đọc dữ liệu từ file CSV
song_data <- read.csv("/Users/mac/Downloads/XSTK/song_data.csv")

# Hiển thị 6 dòng đầu tiên của dữ liệu để kiểm tra sơ bộ
head(song_data)

# Loại bỏ cột 'song_name' vì không cần thiết cho mô hình dự đoán
main_data <- song_data[, !(names(song_data) %in% c("song_name"))]

# Kiểm tra tổng số giá trị bị thiếu (NA) trong toàn bộ dữ liệu
sum(is.na(main_data))

# Kiểm tra số lượng NA trong từng cột
colSums(is.na(main_data))

# Chuyển các biến phân loại sang kiểu factor để xử lý phù hợp
main_data$key <- as.factor(main_data$key)
main_data$audio_mode <- as.factor(main_data$audio_mode)
main_data$time_signature <- as.factor(main_data$time_signature)

# Lọc ra các biến dạng số (liên tục) để thống kê mô tả
num_data <- main_data[sapply(main_data, is.numeric)]

# Tính toán thống kê mô tả cho các biến liên tục
des_stats <- data.frame(
  Mean = sapply(num_data, mean),
  SD = sapply(num_data, sd),
  Min = sapply(num_data, min),
  Q1 = sapply(num_data, function(x) quantile(x, 0.25)),
  Median = sapply(num_data, median),
  Q3 = sapply(num_data, function(x) quantile(x, 0.75)),
  Max = sapply(num_data, max)
)
print(des_stats)

# Tần số xuất hiện của từng mức trong các biến phân loại
table(main_data$key)
table(main_data$audio_mode)
table(main_data$time_signature)

# Loại bỏ các dòng có time_signature là 0 hoặc 1 do số lượng quá nhỏ
main_data <- main_data[!(main_data$time_signature %in% c(0, 1)), ]

# Tải thư viện trực quan hóa
library(ggplot2)
library(ggthemes) # dùng để thêm các theme đẹp hơn cho ggplot

# ---------------------- BIỂU ĐỒ ----------------------

# Histogram thể hiện mức độ phổ biến của bài hát
ggplot(main_data, aes(x = song_popularity)) +
  geom_histogram(binwidth = 5, fill = "#1f77b4", color = "white", alpha = 0.9) +
  labs(title = "Histogram of Song Popularity", x = "Song Popularity", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

# Boxplot mức độ phổ biến theo Key
ggplot(main_data, aes(x = as.factor(key), y = song_popularity)) +
  geom_boxplot(fill = "#2ca02c", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(title = "Song Popularity by Key", x = "Key", y = "Song Popularity") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

# Boxplot theo chế độ âm thanh (audio_mode)
ggplot(main_data, aes(x = as.factor(audio_mode), y = song_popularity)) +
  geom_boxplot(fill = "#ff7f0e", color = "black", outlier.color = "blue", outlier.shape = 17) +
  labs(title = "Song Popularity by Audio Mode", x = "Audio Mode", y = "Song Popularity") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

# Boxplot theo Time Signature
ggplot(main_data, aes(x = as.factor(time_signature), y = song_popularity)) +
  geom_boxplot(fill = "#9467bd", color = "black", outlier.color = "darkred", outlier.shape = 16) +
  labs(title = "Song Popularity by Time Signature", x = "Time Signature", y = "Song Popularity") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))

# ---------------------- SCATTER PLOTS ----------------------

# Vẽ biểu đồ phân tán (scatter) giữa độ phổ biến và từng biến liên tục
# Có kèm đường hồi quy tuyến tính (geom_smooth)

# 1. Duration
ggplot(main_data, aes(x = song_duration_ms, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Duration (ms)", x = "Duration (ms)", y = "Song Popularity") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Các biểu đồ tương tự cho các biến: Acousticness, Danceability, Energy, Instrumentalness,
# Liveness, Loudness, Speechiness, Tempo, Valence
# Scatter plot for Acousticness
ggplot(main_data, aes(x = acousticness, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Acousticness", x = "Acousticness", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Danceability
ggplot(main_data, aes(x = danceability, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Danceability", x = "Danceability", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Energy
ggplot(main_data, aes(x = energy, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Energy", x = "Energy", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Instrumentalness
ggplot(main_data, aes(x = instrumentalness, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Instrumentalness", x = "Instrumentalness", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Liveness
ggplot(main_data, aes(x = liveness, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Liveness", x = "Liveness", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Loudness
ggplot(main_data, aes(x = loudness, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Loudness", x = "Loudness", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Speechiness
ggplot(main_data, aes(x = speechiness, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Speechiness", x = "Speechiness", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Tempo
ggplot(main_data, aes(x = tempo, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Tempo", x = "Tempo", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Scatter plot for Valence
ggplot(main_data, aes(x = audio_valence, y = song_popularity)) +
  geom_point(alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Song Popularity vs Valence", x = "Valence", y = "Song Popularity") +
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
# ---------------------- CORRELATION ----------------------

# Tính và vẽ biểu đồ tương quan giữa các biến liên tục
library(corrplot)
library(RColorBrewer)

cor_matrix <- cor(num_data, use = "complete.obs") # tính ma trận tương quan
corrplot(cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  col = colorRampPalette(brewer.pal(8, "RdYlBu"))(200),
  tl.col = "black", tl.srt = 45,
  addCoef.col = "black", number.cex = 0.7,
  cl.cex = 0.8, tl.cex = 0.9,
  diag = FALSE, mar = c(0, 0, 1, 0)
)

# ---------------------- THỐNG KÊ SUY DIỄN ----------------------

# Kiểm định giả thuyết: mức độ phổ biến trung bình có lớn hơn 60 không?

set.seed(123)
sample_data <- sample(main_data$song_popularity, 5000)

# Kiểm tra phân phối chuẩn
shapiro_test_result <- shapiro.test(sample_data)
print(shapiro_test_result)

# Giá trị kỳ vọng
mu_0 <- 60
n <- length(sample_data)
sample_mean <- mean(sample_data)
sample_sd <- sd(sample_data)

# Tính thống kê kiểm định Z
z_value <- (sample_mean - mu_0) / (sample_sd / sqrt(n))

# p-value cho kiểm định một phía
p_value <- 1 - pnorm(z_value)

cat("Z-value:", z_value, "\n")
cat("p-value:", p_value, "\n")

# So sánh với mức ý nghĩa
alpha <- 0.05
if (p_value < alpha) {
  cat("Bác bỏ H₀: Trung bình lớn hơn 60\n")
} else {
  cat("Không bác bỏ H₀: Trung bình không lớn hơn 60\n")
}

# ---------------------- KIỂM ĐỊNH SO SÁNH 2 NHÓM ----------------------

# So sánh mức độ phổ biến giữa hai nhóm: âm thanh ở mode = 0 (minor) và mode = 1 (major)

# Tách dữ liệu
mode0_data <- main_data[main_data$audio_mode == 0, "song_popularity"]
mode1_data <- main_data[main_data$audio_mode == 1, "song_popularity"]

# Lấy mẫu nhỏ nếu cần để kiểm tra phân phối chuẩn
set.seed(123)
sample_mode0 <- sample(mode0_data, min(length(mode0_data), 5000))
sample_mode1 <- sample(mode1_data, min(length(mode1_data), 5000))

# Kiểm tra phân phối chuẩn
shapiro0 <- shapiro.test(sample_mode0)
shapiro1 <- shapiro.test(sample_mode1)

print("Shapiro test for audio_mode = 0:")
print(shapiro0)
print("Shapiro test for audio_mode = 1:")
print(shapiro1)

# Tính giá trị trung bình, độ lệch chuẩn và kích thước mẫu cho hai nhóm
mean0 <- mean(mode0_data)
mean1 <- mean(mode1_data)
sd0 <- sd(mode0_data)
sd1 <- sd(mode1_data)
n0 <- length(mode0_data)
n1 <- length(mode1_data)

# Tính thống kê Z cho so sánh hai nhóm
z_value <- (mean0 - mean1) / sqrt((sd0^2 / n0) + (sd1^2 / n1))
p_value <- 2 * (1 - pnorm(abs(z_value))) # kiểm định hai phía

# In kết quả
cat("Z-value:", z_value, "\n")
cat("p-value:", p_value, "\n")

# Kết luận
if (p_value < alpha) {
  cat("Bác bỏ H₀: Có sự khác biệt giữa 2 chế độ âm thanh\n")
} else {
  cat("Không bác bỏ H₀: Không có sự khác biệt đáng kể\n")
}

# ---------------------- HỒI QUY ĐA BIẾN ----------------------

# Chia dữ liệu thành tập train và test (80:20)
library(caTools)
set.seed(12)
split <- sample.split(main_data$song_popularity, SplitRatio = 0.8)
train_data <- subset(main_data, split == TRUE)
test_data <- subset(main_data, split == FALSE)

# Mô hình hồi quy tuyến tính với tất cả biến độc lập
model_full <- lm(
  song_popularity ~ song_duration_ms + acousticness + danceability +
    energy + instrumentalness + key + liveness + loudness +
    audio_mode + speechiness + tempo + time_signature + audio_valence,
  data = train_data
)

# Tóm tắt mô hình đầy đủ
summary(model_full)

# Chọn mô hình tối ưu bằng stepwise selection
model_step <- step(model_full, direction = "both")

# Tóm tắt mô hình sau khi chọn biến
summary(model_step)

# Kiem tra cac gia dinh cua mo hinh (normal, linearity, constant variance, outliers)
par(mfrow = c(2, 2))
plot(model_step)

# Kiểm tra mô hình trên tập test (dự đoán và tính R-squared)
predicted <- predict(model_step, newdata = test_data)
actual <- test_data$song_popularity

# Tính R-squared trên tập test
SSE <- sum((predicted - actual)^2)
SST <- sum((mean(train_data$song_popularity) - actual)^2)
R2_test <- 1 - SSE / SST

cat("R-squared trên tập test:", round(R2_test, 4), "\n")
