library(data.table)

cepheidas_MW = read.table("/home/holman/PhysAstroNotes/AstroStatistics with R/Semana02/Cepheids_MW.dat", header = TRUE)

A = log10(cepheidas_MW$PERIOD)

cepheidas_M31 = read.table("/home/holman/PhysAstroNotes/AstroStatistics with R/Semana02/Cepheids_M31.dat", header = TRUE)

B = log10(cepheidas_M31$Period)

range(A)
range(B)

ax = seq(0.0,2.2,by=0.2)

# Save first histogram data
hgA <- hist(A, breaks = ax, plot = FALSE)

# Save 2nd histogram data
hgB <- hist(B, breaks = ax, plot = FALSE)

# Plot 1st histogram
plot(hgA)
# Add 2nd histogram
plot(hgB, add = TRUE)

#Matrix with three rows (red, blue, green)
col2rgb(c("lightblue", "lightgreen", "pink"))

#The standard blue and makes it transparent (~50%)
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")

# Plot 1st histogram
plot(hgA, col=mycol)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

# Plot 1st histogram using a transparent color
plot(hgA, col = c1)
# Add 2nd histogram using different color
plot(hgB, col = c2, add = TRUE)


DatA <- c(A)
DatB <- c(B)

range(DatA)
range(DatB)


frec_absA <- table(DatA) 
frec_relA <- prop.table(frec_absA)
frec_absB <- table(DatB) 
frec_relB <- prop.table(frec_absB)

range(frec_relA)
range(frec_relB)

axA = seq(0.0,0.002,by=0.0001)
axB = seq(0.0,0.0004,by=0.0001)

# Save first histogram data
hgA <- hist(frec_relA, breaks = axA, plot = FALSE)

# Save 2nd histogram data
hgB <- hist(frec_relB, breaks = axB, plot = FALSE)

# Plot 1st histogram
plot(hgA)
# Add 2nd histogram
plot(hgB, add = TRUE)

#Matrix with three rows (red, blue, green)
col2rgb(c("lightblue", "lightgreen", "pink"))

#The standard blue and makes it transparent (~50%)
mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")

# Plot 1st histogram
plot(hgA, col=mycol)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

# Plot 1st histogram using a transparent color
plot(hgA, col = c1)
# Add 2nd histogram using different color
plot(hgB, col = c2, add = TRUE)

# la clave para la frecuencia relativa es la propiedad density del hist





