#một số hàm được sử dụng
#tính tứ phân vị, dùng hàm quantile() hoặc summary()
#na.omit() để loại bỏ giá trị NA
#hàm summary() cho cả giá trị trung bình
#sd() để tính giá trị lệch chuẩn


load ("workspace/preTask.RData")
load ("workspace/mission1.RData")

#số tử nhiều nhất
maxDeath <- max (na.omit(bigTable $ new_deaths) )
minDeath <- min (na.omit(bigTable $ new_deaths) )
#trường hợp này ra số âm nhưng không biết xử lý thế nào

maxCase <- max (na.omit(bigTable $ new_cases) )
minCase <- min (na.omit(bigTable $ new_cases) )

#tính tứ phân vị và giá trị trung bình, giá trị lệch chuẩn
quartileDeath <- summary (na.omit (bigTable $ new_deaths)) [c (2,3,5)]
meanDeath <-summary (na.omit (bigTable $ new_deaths)) [4]
stdDeath <- sd (bigTable$new_deaths, na.rm = TRUE)

quartileCase <- summary (na.omit (bigTable $ new_cases)) [c (2,3,5)]
meanCase <- summary (na.omit (bigTable $ new_cases)) [4]
stdCase <- sd (bigTable $ new_cases, na.rm = TRUE)
