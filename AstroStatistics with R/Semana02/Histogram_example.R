A = c(17,26,28,27,29,28,25,26,34,32,23,29,24,21,26,31,31,22,26,19,36,23,21,16,30)
B = c(27,36,38,37,39,38,35,36,44,42,33,39,34,31,36,41,41,32,36,29,46,33,31,26,40)

# Set the minimum for the breakpoints
b <- min(c(A,B)) - 0.001

# Set the maximum for the breakpoints
e <- max(c(A,B))

# Make a neat vector for the breakpoints
ax <- pretty(b:e, n = 12)

ax

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
