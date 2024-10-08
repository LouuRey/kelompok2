# Definisikan fungsi untuk menghitung distribusi frekuensi dengan aturan Sturges
frekuensi_sturges <- function(data) {
  # Tentukan jumlah kelas dengan aturan Sturges
  n <- length(data)
  k <- ceiling(1 + 3.322 * log10(n))  # ceiling untuk membulatkan ke atas

  # Tentukan range data
  data_min <- min(data)
  data_max <- max(data)

  # Tentukan lebar interval
  interval_width <- (data_max - data_min) / k

  # Tentukan batas-batas interval
  breaks <- seq(data_min, data_max, by = interval_width)

  # Buat variabel kosong untuk menyimpan frekuensi
  frekuensi <- numeric(length(breaks) - 1)

  # Loop untuk menghitung frekuensi pada setiap interval
  for (i in 1:(length(breaks) - 1)) {
    frekuensi[i] <- sum(data >= breaks[i] & data < breaks[i+1])
  }

  # Tambahkan frekuensi untuk batas terakhir (nilai yang sama dengan max)
  frekuensi[length(frekuensi)] <- frekuensi[length(frekuensi)] + sum(data == data_max)

  # Buat data frame untuk menyimpan hasilnya
  result <- data.frame(
    "Interval Nilai" = paste(round(breaks[-length(breaks)], 2), round(breaks[-1], 2), sep = "-"),
    "Frekuensi" = frekuensi
  )

  # Kembalikan hasil sebagai data frame
  print(result)
}

# Fungsi untuk menggambar histogram dengan distribusi frekuensi menggunakan aturan Sturges
histogram <- function(data) {
  # Hitung jumlah data
  n <- length(data)

  # Tentukan jumlah kelas dengan aturan Sturges
  k <- ceiling(1 + 3.322 * log10(n))  # ceiling untuk membulatkan ke atas

  # Tentukan range data
  data_min <- min(data)
  data_max <- max(data)

  # Tentukan lebar interval
  interval_width <- (data_max - data_min) / k

  # Tentukan batas-batas interval
  breaks <- seq(data_min, data_max, by = interval_width)

  # Buat variabel kosong untuk menyimpan frekuensi
  frekuensi <- numeric(length(breaks) - 1)

  # Loop untuk menghitung frekuensi pada setiap interval
  for (i in 1:(length(breaks) - 1)) {
    frekuensi[i] <- sum(data >= breaks[i] & data < breaks[i + 1])
  }

  # Tambahkan frekuensi untuk batas terakhir (nilai yang sama dengan max)
  frekuensi[length(frekuensi)] <- frekuensi[length(frekuensi)] + sum(data == data_max)

  # Buat data frame untuk menyimpan hasil frekuensi
  freq_data <- data.frame(
    "Interval Nilai" = paste(round(breaks[-length(breaks)], 2), round(breaks[-1], 2), sep = "-"),
    "Frekuensi" = frekuensi
  )
  # Buat histogram menggunakan barplot
  barplot(freq_data$Frekuensi,
          names.arg = freq_data$Interval.Nilai,
          main = "Histogram Distribusi Frekuensi Sturges",
          xlab = "Interval Nilai",
          ylab = "Frekuensi",
          col = "skyblue",
          border = "black",
          las = 2)  # Mengubah orientasi label x-axis menjadi vertikal
}

# Membuat 100 nilai acak antara 0 hingga 100
set.seed(123)  # Seed agar hasil acak dapat direproduksi
nilai <- sample(0:100, 100, replace = TRUE)


frekuensi_sturges(nilai)

histogram(nilai)
