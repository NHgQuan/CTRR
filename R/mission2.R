#một số hàm được sử dụng
#tính tứ phân vị, dùng hàm quantile() hoặc summary()
#na.omit() để loại bỏ giá trị NA


load ("workspace/preTask.RData")
load ("workspace/mission1.RData")

#số tử nhiều nhất
maxDeath <- max (na.omit(bigTable $ new_deaths) )
minDeath <- min (na.omit(bigTable $ new_deaths) )
#trường hợp này ra số âm nhưng không biết xử lý thế nào

maxCase <- max (na.omit(bigTable $ new_cases) )
minCase <- min (na.omit(bigTable $ new_cases) )

#tính tứ phân vị, dùng hàm quantile() hoặc summary()