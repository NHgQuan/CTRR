load ("workspace/preTask.RData")
load ("workspace/mission1.RData")

#số tử nhiều nhất
maxDeath <- max ( (subset(bigTable, new_deaths >= 0)) $ new_deaths)
minDeath <- min ( (subset(bigTable, new_deaths <= maxDeath)) $ new_deaths) 
#trường hợp này ra số âm nhưng không biết xử lý thế nào

maxCase <- max ( (subset(bigTable, new_cases >= 0)) $ new_cases)
minCase <- min ( (subset(bigTable, new_cases <= maxCase)) $ new_cases) 

