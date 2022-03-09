library(readr)
library(ggplot2)

# Tính các ký tự 1 2 3 4 từ thực tế
kytu1 <- 4
kytu2 <- 2
kytu3 <- 6
kytu4 <- 3
kq <- (kytu1 + kytu2 + kytu3 + kytu4) %% 6
stt <- c(11, 12, 13)


bigTable <- (read.csv("owid-covid-data.csv", header = TRUE)) # bigTable là bảng thô chưa được chỉnh sửa

# truy cập vào từng phần tử của mảng bằng $
# công việc: sau khi đọc R phải liệt ra các nước Canada,  Greenland và United States thôi
table <- subset(bigTable, location == "Canada" | location == "Greenland" |
    location == "United States") # tách Canada, loction và Greenland ra theo đề bài

allDate <- bigTable$ date # tìm ngày tháng
date <- table$ date

# i1
# tính số lượng đất nước
numLoca <- length(unique(bigTable$ location))

# hiển thị tên 10 đất nước đầu tiên
iso_code <- c("AFG")
country <- c("Afghanistan")
locations <- bigTable$ location
indexCountry <- 1
for (i in seq(1, (length(locations) - 1), by = 1)) {
    if (locations[i] != locations[i + 1]) {
        indexCountry <- indexCountry + 1

        # sử dụng vector <- append (vector, element) để thêm cái phần tử đó vào sau đít vector
        iso_code <- append(iso_code, bigTable$ iso_code[i + 1])
        country <- append(country, bigTable$ country[i + 1])
    }
    if (indexCountry >= 10) {
        break
    }
}

# i2
# Bảng hiển thị 10 đất nước đầu tiên
top10Countries <- data.frame(iso_code, country)

# i3
# tìm số lượng châu lục trong tập mẫu
numConti <- length(unique(bigTable$ continent))
# OWID_AFR không có tên châu lục có cần tính không?


# i45
# tính số lượng dữ liệu thu thập được trong từng châu lục và tổng số
# ở đây, tôi giả sử "dữ liệu" đề bài nói đến chính là số hàng
continents <- unique(bigTable$ continent)
values <- c()

for (string in continents) {
    values <- append(values, nrow(subset(bigTable, continent == string)))
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
    values == max(nValuesOfContinents[- nrow(nValuesOfContinents), ] $ values)
))          #- nrow để loại đi cái cuối cùng là sum

print("Nước có số lượng dữ liệu thu thập nhỏ nhất")
print(subset(
    nValuesOfLoca,
    values == min(nValuesOfLoca$ values)
))
print("Nước có số lượng dữ liệu thu thập lớn nhất")
print(subset(
    nValuesOfLoca,
    values == max(nValuesOfLoca[- nrow(nValuesOfLoca), ] $ values)
))


#10 & 11
#phải lập bảng dữ liệu thu thập được theo từng date
#như trên thôi
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

#i12 13 14
#Bây giờ là theo cả Date và cả Châu Lục
