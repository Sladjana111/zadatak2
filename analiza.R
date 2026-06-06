# 1 ==========================
# PRIPREMA PODATAKA
Dataset <- read.csv("Dataset.csv")

# Pregled podataka
head(Dataset)
str(Dataset)

# 2 ==========================
# DESKRIPTIVNA ANALIZA
summary(Dataset[, c("TCA1","TCA2","TCA3","TCA4","TCA5","TCA7","TCA8")])
summary(Dataset[, c("FAH1","FAH2","FAH3","FAH4","FAH5")])
summary(Dataset[, c("IA1","IA2","IA3","IA4","IA5","IA6","IA7","IA8")])
summary(Dataset[, c("WS2","WS3r","WS5","WS6","WS7","WS8")])

# Izracunavanje skorova
Dataset$TCA <- rowMeans(
  Dataset[, c("TCA1", "TCA2", "TCA3", "TCA4", "TCA5", "TCA7", "TCA8")],
  na.rm = TRUE
)

Dataset$FAH <- rowMeans(
  Dataset[, c("FAH1", "FAH2", "FAH3", "FAH4", "FAH5")],
  na.rm = TRUE
)

Dataset$IA <- rowMeans(
  Dataset[, c("IA1", "IA2", "IA3", "IA4", "IA5", "IA6", "IA7", "IA8")],
  na.rm = TRUE
)

Dataset$WS <- rowMeans(
  Dataset[, c("WS2", "WS3r", "WS5", "WS6", "WS7", "WS8")],
  na.rm = TRUE
)

summary(Dataset[, c("TCA", "FAH", "IA", "WS")])

# Histogrami
par(mfrow = c(2,2))
hist(Dataset$TCA, main = "TCA", xlab = "TCA")
hist(Dataset$FAH, main = "FAH", xlab = "FAH")
hist(Dataset$IA,  main = "IA",  xlab = "IA")
hist(Dataset$WS,  main = "WS",  xlab = "WS")
par(mfrow = c(1,1))

# 3 ==========================
# KORELACIONA ANALIZA
test <- cor.test(Dataset$TCA, Dataset$FAH)
test$estimate   
test$p.value

# 4 ==========================
# REGRESIONA ANALIZA
model <- lm(TCA ~ FAH, data = Dataset)
summary(model)

# DIJAGNOSTIKA MODELA
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))
