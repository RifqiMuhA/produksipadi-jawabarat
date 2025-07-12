# Aplikasi Analisis & Peramalan Produksi Padi Jawa Barat

Aplikasi interaktif ini dikembangkan menggunakan **R Shiny** untuk membantu analisis, eksplorasi data, visualisasi spasial, serta peramalan produksi padi di Provinsi Jawa Barat. Aplikasi ini mendukung input data kustom (format `.csv`) dan menyediakan berbagai alat bantu statistik serta pemodelan berbasis SARIMAX.

Hasil Deploy: [padi-jawabarat](https://padi-jawabarat.shinyapps.io/padi-jawabarat/) 
Referensi: [visualisasi-data-iklim-indonesia](https://anandasatriaa.github.io/visualisasi-data-iklim-di-indonesia/distribusi.html)
---

## Struktur Halaman

### 1. Beranda
Menampilkan ringkasan kondisi geografis Provinsi Jawa Barat dan statistik umum produksi padi. Halaman ini menjadi pengantar utama penggunaan aplikasi.

---

### 2. Teori & User Guide
Berisi:
- **Landasan teori** terkait produksi padi dan metode peramalan time series, terutama **SARIMAX (Seasonal ARIMA with eXogenous variables)**.
- **Panduan pengguna** untuk membantu navigasi dan penggunaan fitur aplikasi.
- **Metadata** data yang digunakan serta sumber data 
- **Data** yang bisa didownload untuk bahan analisis
---

### 3. Visualisasi Data

Pengguna dapat mengunggah file `.csv` atau menggunakan data contoh.

#### a. Peta Interaktif
Menampilkan:
- Nilai **maksimum**, **minimum**, **rata-rata**, dan **total produksi padi**.
- Peta sebaran produksi padi berdasarkan warna (warna lebih tua = produksi tinggi).
  - **Nilai Absolut**
  - **Perbandingan Tahunan** (kenaikan/penurunan)

#### b. Grafik Perbandingan
- Perbandingan produksi per kabupaten/kota.
- Opsi tampilan absolut atau perbandingan tahunan.

#### c. Tabel Data
- Tampilan data produksi padi dalam bentuk tabel.
- Bisa berupa nilai absolut maupun persentase perubahan tahunan.

---

### 4. Eksplorasi Data

Fitur drag-and-drop untuk mengelompokkan variabel ke:
- **Variabel Dependen**
- **Variabel Independen**
- **Variabel Kategorikal**

Data bisa diunggah atau menggunakan contoh bawaan.

#### Menu Eksplorasi:

**1. Eksplorasi Independen Saja**
- Histogram
- Boxplot
- Statistik deskriptif
- Uji Normalitas (Shapiro-Wilk)

**2. Eksplorasi Independen + Dependen**
- Scatterplot
- Boxplot
- Korelasi
- ANOVA/Uji T

**3. Eksplorasi Independen + Kategorikal**
- Boxplot
- Violin Plot
- ANOVA/Kruskal-Wallis
- Statistik per kategori

---

### 5. Clustering

Pengguna dapat:
- Unggah file `.csv` atau gunakan data contoh.
- Pilih variabel numerik untuk clustering.
- Tentukan jumlah cluster secara **manual** atau **otomatis** (Elbow Method).

#### Output:
- Grafik Elbow Method
- Scatter Plot
- Boxplot per cluster
- Peta Jawa Barat berdasarkan hasil cluster

---

### 6. Forecasting (Peramalan)

Fokus pada model **SARIMAX** dengan variabel eksogen seperti:
- Curah Hujan
- Suhu
- Kelembapan
- Sinar Matahari

#### Menu:

**1. Input Data**
- Unggah file `.csv` atau gunakan data contoh

**2. Statistik Deskriptif**
- Menampilkan grafik time-series dan ringkasan statistik

**3. Matriks Korelasi**
- Korelasi antar variabel bebas

**4. Pengecekan Asumsi**
- Uji **Stasioneritas** (ADF Test)
- Uji **Normalitas Residual** (Shapiro-Wilk)
- Uji **Autokorelasi Residual** (Ljung-Box)

**5. Forecasting & Tren**
- Peramalan menggunakan SARIMAX
- Output: grafik tren, ringkasan model, AIC/BIC, error metrics

---

## Format File CSV

Pastikan file memiliki format dan struktur kolom yang sesuai dengan contoh data yang telah disediakan.

---

## Teknologi yang Digunakan

- R Shiny
- ggplot2, plotly
- leaflet (untuk peta)
- forecast, tseries, stats (untuk SARIMAX)
- dplyr, tidyr
- cluster, factoextra
- shinyWidgets, shinyjs, shinythemes

---

## Catatan

- Direkomendasikan untuk menggunakan browser desktop (Chrome/Firefox).
- Jika terjadi error saat unggah file, periksa format dan kolom file `.csv` Anda.

---

## Kontributor
- Rifqi Muhadzib Ahdan – 222313350
- Seto Haidar Yudhistira – 222313375
- Zidan Septian – 222313447

---

