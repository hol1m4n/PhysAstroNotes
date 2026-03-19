library(data.table)
dataBH = read.table("/home/torrespapaqui/CursoR/Semana04/Dullo_MBH_03.dat",head=TRUE)

library(ggplot2)

mfuvm36 = dataBH$mFUV - dataBH$m36

ggplot(dataBH, aes(x=mFUV-m36, y=MBH)) + geom_point()

# Add the regression line
ggplot(dataBH, aes(x=mFUV-m36, y=MBH)) +
  geom_point() +
  geom_smooth(method=lm)
  
# Add the y error bar
ggplot(dataBH, aes(x=mFUV-m36, y=MBH)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=MBH+MBHlo, ymax=MBH+MBHhi))

# Add the x,y error bars
ggplot(dataBH, aes(x=mFUV-m36, y=MBH)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=MBH+MBHlo, ymax=MBH+MBHhi)) +
  geom_errorbarh(aes(xmin=mFUV-m36+mFUVhi+m36hi, xmax=mFUV-m36-mFUVhi-m36hi))

# fit with Morphology
ggplot(dataBH, aes(x=mFUV-m36, y=MBH, color=dataBH$Type)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=MBH+MBHlo, ymax=MBH+MBHhi)) +
  geom_errorbarh(aes(xmin=mFUV-m36+mFUVhi+m36hi, xmax=mFUV-m36-mFUVhi-m36hi))

# fit with Methodology
ggplot(dataBH, aes(x=mFUV-m36, y=MBH, color=Morpho)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=MBH+MBHlo, ymax=MBH+MBHhi)) +
  geom_errorbarh(aes(xmin=mFUV-m36+mFUVhi+m36hi, xmax=mFUV-m36-mFUVhi-m36hi))

library(latex2exp)

ggplot(dataBH, aes(x=mFUV-m36, y=MBH, color=Morpho)) +
  geom_point() +
  geom_smooth(method=lm, fullrange=TRUE) +
  xlab(TeX("m$_{FUV}$ - m$_{3.6 \\mu m}$ \\[mag\\]")) + ylab(TeX("$\\log\\, (M_{BH}/M_{o})$")) +
  theme(text = element_text(size=20), axis.text.x = element_text(hjust = 1, size=18)) +
  theme(axis.text.y = element_text(hjust = 1, size=18))
