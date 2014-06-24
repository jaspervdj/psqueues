report     <- read.csv("report.csv", header=T)
groups     <- matrix(report$Mean, nrow=4)
colors     <- terrain.colors(5)[1:4]
names      <- c("IntPSQ", "HashPSQ", "OrdPSQ", "PSQueue")
benchmarks <- c("minView", "lookup", "insert", "delete")

svg("report.svg", width=9, height=5)
barplot(groups, beside=T, col=colors, ylab="Time (in seconds)",
        names.arg=benchmarks)
legend("topleft", names, fill=colors)
dev.off()
