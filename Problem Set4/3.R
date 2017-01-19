##a
eigen(matrix(c(0.2,0,0.057,0.3),2,2))
##b
i <- 0:11
phi11sq <- (0.2^i)^2
phi12sq <- (0.57*(0.3^i-0.2^i))^2
phi21sq <- 0
phi22sq <- (0.3^i)^2
sum(phi11sq[1:4])
sum(phi12sq[1:4])
sum(phi11sq[1:4])+sum(phi12sq[1:4])
sum(phi11sq[1:4])/(sum(phi11sq[1:4])+sum(phi12sq[1:4]))
sum(phi12sq[1:4])/(sum(phi11sq[1:4])+sum(phi12sq[1:4]))
sum(phi22sq[1:4])
sum(phi11sq[1:12])
sum(phi12sq[1:12])
sum(phi11sq[1:12])+sum(phi12sq[1:12])
sum(phi11sq[1:12])/(sum(phi11sq[1:12])+sum(phi12sq[1:12]))
sum(phi12sq[1:12])/(sum(phi11sq[1:12])+sum(phi12sq[1:12]))
sum(phi22sq[1:12])
