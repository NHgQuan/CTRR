library(readr)
library(ggplot2)

#Tính các ký tự 1 2 3 4 từ thực tế
kytu1 <- 4
kytu2 <- 2
kytu3 <- 6
kytu4 <- 3

kq <- (kytu1 + kytu2 + kytu3 + kytu4) %% 6
print (kq)
stt <- c (11,12,13)


bigTable <- (read.csv("owid-covid-data.csv", header = TRUE)) #bigTable là bảng thô chưa được chỉnh sửa

# truy cập vào từng phần tử của mảng bằng $
# công việc: sau khi đọc R phải liệt ra các nước Canada,  Greenland và United States thôi
table <- subset(bigTable, location == "Canada" |location == "Greenland" | 
    location == "United States") #tách Canada, loction và Greenland ra theo đề bài

allDate <- bigTable $ date  #tìm ngày tháng
date <- table $ date

#1) tính số lượng đất nước
numLoca <- length ( unique (bigTable $ location) )

#hiển thị tên 10 đất nước đầu tiên
iso_code <- c("AFG")
country <- c("Afghanistan")
indexCountry <- 1
for ( i in seq ( 1, (length(locations) - 1), by = 1 ) ) {
    if (locations[i] != locations [i + 1]) {
        indexCountry <- indexCountry + 1

        #sử dụng vector <- append (vector, element) để thêm cái phần tử đó vào sau đít vector
        iso_code <- append (iso_code, bigTable $ iso_code [i + 1] )
        country <- append (country, bigTable $ country [i + 1] )
    }
    if (indexCountry >= 10) { break }
}

#i2) Bảng hiển thị 10 đất nước đầu tiên
top10Countries <- data.frame (iso_code, country)

#i3) tìm số lượng châu lục trong tập mẫu
numConti <- length ( unique (bigTable $ continent) )
#OWID_AFR không có tên châu lục có cần tính không?


# tính số lượng dữ liệu thu thập được trong từng châu lục và tổng số
#ở đây tôi giả sử là new_cases new_deaths
