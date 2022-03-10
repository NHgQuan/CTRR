# một số hàm được sử dụng
# tính tứ phân vị, dùng hàm quantile() hoặc summary()
# na.omit() để loại bỏ giá trị NA
# hàm summary() cho cả giá trị trung bình
# sd() để tính giá trị lệch chuẩn


load("workspace/preTask.RData")
load("workspace/mission1.RData")

rmNaDeathList <- na.omit(bigTable$ new_deaths)
rmNaCaseList <- na.omit(bigTable$ new_cases)


# số tử nhiều nhất
maxDeath <- max(rmNaDeathList)
minDeath <- min(rmNaDeathList)
# trường hợp này ra số âm nhưng không biết xử lý thế nào

maxCase <- max(rmNaCaseList)
minCase <- min(rmNaCaseList)

# tính tứ phân vị và giá trị trung bình, giá trị lệch chuẩn
quartileDeath <- summary(rmNaDeathList)[c(2, 3, 5)]
meanDeath <- summary(rmNaDeathList)[4]
stdDeath <- sd(bigTable$new_deaths, na.rm = TRUE)

quartileCase <- summary(rmNaCaseList)[c(2, 3, 5)]
meanCase <- summary(rmNaCaseList)[4]
stdCase <- sd(bigTable$ new_cases, na.rm = TRUE)


# i5
# câu này không hiểu rõ đề
# đếm xem bao nhiêu outliers của Death
deathIQR <- quartileDeath[3] - quartileDeath[1]
deathOutliers <- length(rmNaDeathList[rmNaDeathList < quartileDeath[1] - 1.5 * deathIQR |
    rmNaDeathList > quartileDeath[3] + 1.5 * deathIQR])

# đếm xem bao nhiêu outliers của Case
caseIQR <- quartileCase[3] - quartileCase[1]
caseOutliers <- length(rmNaCaseList[rmNaCaseList < quartileCase[1] - 1.5 * caseIQR |
    rmNaCaseList > quartileCase[3] + 1.5 * caseIQR])

