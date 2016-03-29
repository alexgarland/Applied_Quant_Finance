fs_dev <- sd(first_stock)
fs_range <- (max(first_stock) - min(first_stock))/fs_dev
fs_kurtosis <- kurtosis(first_stock)
fs_skew <- skewness(first_stock)

norm_fs <- rnorm(180, mean = mean(first_stock), sd = sd(first_stock))
norm_fs_range <- (max(norm_fs) - min(norm_fs))/fs_dev
norm_fs_kurtosis <- kurtosis(norm_fs)
norm_fs_skew <- skewness(norm_fs)