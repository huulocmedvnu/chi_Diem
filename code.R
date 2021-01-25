
###

data <- readxl::read_excel("Data_cleaned_ver11_1.xlsx")

x.lm <- as.data.frame(data[, c(1:18, 20:25, 27:28)])
x.lm$time_mongcon <- as.numeric(x.lm$time_mongcon)
x.lm <- x.lm[, -c(4, 11)]
y.lm <- as.data.frame(data[, 26])
y.lm$diem_phq <- as.numeric(y.lm$diem_phq)

bma.lm <- BMA::bicreg(x.lm, y.lm$diem_phq)

summary(bma.lm, conditional = T, digits = 2)

m1 <- lm(diem_phq ~ tuoick + diem_gad + bantan, data = data)
sjPlot::tab_model(m1)

#
library(sjPlot)
data$diem_phq <- as.numeric(data$diem_phq)
data$time_mongcon <- as.numeric(data$time_mongcon)
lst_mod <- c()
for (i in names(data)) {
  print(i)
  diem_phq <- data$diem_phq
  if (i != diem_phq) {
    model <- lm(paste("diem_phq ~", i), data = data)
    assign(paste0("model", "_", i), model)
    lst_mod <- c(lst_mod, paste0("model", "_", i))
  }
}
sjPlot::tab_model(model_diem_gad)

###

x.lm <- as.data.frame(data[, c(1:18, 20:27)])
x.lm$time_mongcon <- as.numeric(x.lm$time_mongcon)
x.lm$diem_phq <- as.numeric(x.lm$diem_phq)
x.lm <- x.lm[, -c(4, 11)]
y.lm <- as.data.frame(data[, 28])
y.lm$diem_gad <- as.numeric(y.lm$diem_gad)

bma.lm <- BMA::bicreg(x.lm, y.lm$diem_gad)

summary(bma.lm, conditional = T, digits = 2)
data$diem_phq <- as.numeric(x.lm$diem_phq)
m1 <- lm(diem_gad ~ diem_phq, data = data)
m2 <- lm(diem_gad ~ thunhap, data = data)

tab_model(m1)
tab_model(m2)
