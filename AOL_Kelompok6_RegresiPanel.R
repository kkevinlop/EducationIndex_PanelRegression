library(readxl)
Datasets_Kelompok6 <- read_excel("C:/Users/Asus/OneDrive - Bina Nusantara/BINUS_Kuliah/Semester 5/Regression Analysis/AOL/Datasets_Kelompok6.xlsx")
View(Datasets_Kelompok6)

#library(foreign)
#coplot(y~tahun|nama_kabupaten_kota, type = "l", data = Datasets_Kelompok6)

model <- lm(indeks_pendidikan ~ tahun + jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah, data = Datasets_Kelompok6)
summary(model)

fixed.dum <- lm(indeks_pendidikan ~ tahun + jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(nama_kabupaten_kota) - 1, data = Datasets_Kelompok6)
summary(fixed.dum)

install.packages("plm")
library(plm)
fixed <- plm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah, data = Datasets_Kelompok6, index = c("nama_kabupaten_kota", "tahun"), model = "within")
summary(fixed)

random <- plm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah, data = Datasets_Kelompok6, index = c("nama_kabupaten_kota", "tahun"), model = "random")
summary(random)

phtest(fixed, random)
#karena p-value < 0.05, maka pake fixed effect


pFtest()

#fixed <- plm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah, data = Datasets_Kelompok6, index = c("nama_kabupaten_kota", "tahun"), model = "within")
fixed.time <- plm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun), data = Datasets_Kelompok6, index = c("nama_kabupaten_kota", "tahun"), model = "within")
summary(fixed.time)

pFtest(fixed.time, fixed)
# no need to use time-fixed effects

plmtest(fixed, c("time"), type=("bp"))
# no need to use time-fixed effects

pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

pbgtest(fixed)

library(lmtest)
bptest(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun), data = Datasets_Kelompok6, studentize = F)
#hetero

coeftest(fixed)

dwtest(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun), data = Datasets_Kelompok6)


model2 <- lm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6)
summary(model2)

fit <- fitted(model2)

error <- resid(model2)

library(nortest)
lillie.test(error)
# p-value > alpha, maka error  berdistribusi normal -> maka tidak ada outlier.

install.packages("orcutt")
library(orcutt)

morcutt <- cochrane.orcutt(model2, convergence=8, max.iter=100)
summary(morcutt)

str(fixed)

#WLS Regression (untuk pelanggaran homoskedastisitas)
wt <- 1/lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
wls_model <- lm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6,weights=wt)
summary(wls_model)

#Coba2
test <- plm.data(Datasets_Kelompok6, index = c("nama_kabupaten_kota", "tahun"))
summary(test)

morcutt <- cochrane.orcutt(test, convergence=8, max.iter=100)
summary(morcutt)

wt <- 1/lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
wls_model <- lm(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6,weights=wt)
summary(wls_model)

fit.test <- fitted(fixed)

error.test <- resid(fixed)
error.test
library(nortest)
lillie.test(error.test)
# p-value > alpha, maka error  berdistribusi normal -> maka tidak ada outlier.

bptest(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6, studentize = F)
#hetero

coeftest(fixed)

dwtest(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6)

install.packages("robustbase")
library(robustbase)

#robust regression
mrobust <- lmrob(indeks_pendidikan ~ jumlah_ruang_kelas + jumlah_perpustakaan + jumlah_lab + rata_rata_lama_sekolah + factor(tahun)-1, data = Datasets_Kelompok6)
summary(mrobust)
