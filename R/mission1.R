library(readr)
library(ggplot2)
library(stringr)

load("workspace/preTask.RData")

# i1
# tính số lượng đất nước
numLoca <- length(unique(bigTable$ location))

# i2
# Bảng hiển thị 10 đất nước đầu tiên
countries <- unique(bigTable[, c(1, 3)])
top10Countries <- head(countries, n = 10)
print (top10Countries)

# i3
# tìm số lượng châu lục trong tập mẫu
numConti <- length(unique(tableNoNull$ continent))
# OWID_AFR không có tên châu lục có cần tính không?


# i45
# tính số lượng dữ liệu thu thập được trong từng châu lục và tổng số
# ở đây, tôi giả sử "dữ liệu" đề bài nói đến chính là số hàng
continents <- unique(tableNoNull$ continent)
values <- c()

for (string in continents) {
    values <- append(values, nrow(subset(tableNoNull, continent == string)))
}
continents <- append(continents, "Sum")
values <- append(values, sum(values)) # cần tính tổng nữa
nValuesOfContinents <- data.frame(continents, values) # bước hoàn thiện bảng cần tính
# vẫn có một châu lục trống

# i5 làm hoàn toàn tương tự như i4
locations <- unique(bigTable$ location)
values <- c()

for (string in locations) {
    values <- append(values, nrow(subset(bigTable, location == string)))
}
locations <- append(locations, "Sum")
values <- append(values, sum(values)) # cần tính tổng nữa
nValuesOfLoca <- data.frame(locations, values)

# hiển thị 10 nước cuối, sử dụng hàm tail(), ngược lại dùng hàm head()
print(tail(nValuesOfLoca, n = 11))

# i6789
print("Châu lục có số lượng dữ liệu thu thập nhỏ nhất")
print(subset(
    nValuesOfContinents,
    values == min(nValuesOfContinents$ values)
))
print("Châu lục có số lượng dữ liệu thu thập lớn nhất")
print(subset(
    nValuesOfContinents,
    values == max(nValuesOfContinents[-nrow(nValuesOfContinents), ]$ values)
)) #- nrow để loại đi cái cuối cùng là sum

print("Nước có số lượng dữ liệu thu thập nhỏ nhất")
print(subset(
    nValuesOfLoca,
    values == min(nValuesOfLoca$ values)
))
print("Nước có số lượng dữ liệu thu thập lớn nhất")
print(subset(
    nValuesOfLoca,
    values == max(nValuesOfLoca[-nrow(nValuesOfLoca), ]$ values)
))


# 10 & 11
# phải lập bảng dữ liệu thu thập được theo từng date
# như trên thôi
dates <- unique(bigTable$ date)
values <- c()
for (string in dates) {
    values <- append(values, nrow(subset(bigTable, date == string)))
}
nValuesOfDates <- data.frame(dates, values)

print("Ngày có số lượng dữ liệu thu thập nhỏ nhất")
print(subset(
    nValuesOfDates,
    values == min(nValuesOfDates$ values)
))
print("Ngày có số lượng dữ liệu thu thập lớn nhất")
print(subset(
    nValuesOfDates,
    values == max(nValuesOfDates$ values)
))


# i12 13 14
# Bây giờ là theo cả Date và cả Châu Lục
contiAndDate <- unique(tableNoNull[, c(2, 4)])
values <- c()
for (i in (1:nrow(contiAndDate))) {
    values <- append(values, nrow(subset(
        tableNoNull,
        date == (contiAndDate[i, ])$ date &
            continent == (contiAndDate[i, ])$ continent
    )))
}
nValuesOfContiAndDate <- data.frame(contiAndDate, values)

print("số lượng dữ liệu thu thập lớn nhất theo ngày và Châu Lục")
print(subset(
    nValuesOfContiAndDate,
    values == max(nValuesOfContiAndDate$ values)
))
print("số lượng dữ liệu thu thập nhỏ nhất theo ngày và Châu Lục")
print(subset(
    nValuesOfContiAndDate,
    values == min(nValuesOfContiAndDate$ values)
))

# i15
# Với một date là k và châu lục t cho trước, hãy cho biết số lượng dữ liệu thu thập được.
# function
findValuesDC <- function(k = "", t = "") {
    print(subset(
        nValuesOfContiAndDate,
        continent == k &
            date == t
    )$ values)
}
# findValuesDC ("Africa", "2/19/2022")

# i16
# tìm các đất nước có số lượng dữ liệu thu thập bằng nhau
# ở đây tôi không biết là in tất cả hay in một số!

# tạo một bảng mới có iso_code ở đầu, đề yêu cầu in iso_cdoe
cached <- c()
nValuesOfLoca2 <- data.frame(append(unique(bigTable$ iso_code), 0), nValuesOfLoca)
for (i in 1:(nrow(nValuesOfLoca) - 1)) {
    for (j in ((i + 1):nrow(nValuesOfLoca))) {
        if (nValuesOfLoca[i, 2] == nValuesOfLoca[j, 2] &
            !(nValuesOfLoca[i, 2] %in% cached)) { # toán tử %in% để kiểm tra xem một phần tử có ở trong một dãy hay không
            print(
                subset(nValuesOfLoca2, values == nValuesOfLoca[i, 2])[, 1]
            )
            cached <- append(cached, nValuesOfLoca[i, 2])
            # ở đây tôi không biết là in tất cả hay in một số!
        }
    }
}
# lỗi ở đâyasdfdaf

# i17 liệt kê tên đất nước, chiều dài iso_code >= 3
print(subset(
    bigTable,
    str_length(iso_code) >= 3
)[, c(1, 3)])

save.image("workspace/mission1.RData")