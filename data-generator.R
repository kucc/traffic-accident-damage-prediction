library(readr)

dat <- read_csv("train.csv")
test <- read_csv("test.csv")

cctv <- read_csv(
    file = "./external_open/대구 CCTV 정보.csv",
    local = locale(encoding = "euc-kr")
)

security <- read_csv(
    file = "./external_open/대구 보안등 정보.csv",
    local = locale(encoding = "euc-kr")
)


child <- read_csv(
    file = "./external_open/대구 어린이 보호 구역 정보.csv",
    local = locale(encoding = "euc-kr")
)

parking <- read_csv(
    file = "./external_open/대구 주차장 정보.csv",
    local = locale(encoding = "euc-kr")
)

f <- function (x) {
    s <- strsplit(x, " ")[[1]]
    
    s[1] <- "대구광역시"
    
    if (length(s) == 1) {
        return (NA)
    }
    s[2] <- trimws(s[2])
    
    if (length(s) == 2) {
        return (NA)
    }
    
    x <- s[3]
    s[3] <- trimws(s[3])

    n <- nchar(x)
    while (TRUE) {
        a <- substr(x, n, n)
        if (a %in% 0:9) {
            n <- n-1
            next
        }
        if (a == '-') {
            n <- n-1
            next
        }
        break
    }
    
    s[3] <- substr(x, 1, n)
    s[3] <- trimws(s[3])
    
    return (s[3])

}

x <- sapply(cctv$소재지지번주소, f, USE.NAMES = FALSE)
cctv <- cctv %>% 
    rename("시군구" = 소재지지번주소) %>% 
    mutate(시군구 = x)

cctvCount <- cctv %>%
    group_by(시군구) %>% 
    count() %>% 
    rename(n_cctv = n)

x <- sapply(security$소재지지번주소, f, USE.NAMES = FALSE)
security <- security %>% 
    rename("시군구" = 소재지지번주소) %>% 
    mutate(시군구 = x)

securityCount <- security %>%
    group_by(시군구) %>% 
    count() %>% 
    rename(n_security = n)


x <- sapply(child$소재지지번주소, f, USE.NAMES = FALSE)
child <- child %>% 
    rename("시군구" = 소재지지번주소) %>% 
    mutate(시군구 = x)
childCount <- child %>%
    group_by(시군구) %>% 
    count() %>% 
    rename(n_child = n)

x <- sapply(parking$소재지지번주소, f, USE.NAMES = FALSE)
parking <- parking %>% 
    rename("시군구" = 소재지지번주소) %>% 
    mutate(시군구 = x)

parkingCount <- parking %>%
    group_by(시군구) %>% 
    count() %>% 
    rename(n_parking = n)

dat <- dat %>% 
    left_join(cctvCount) %>% 
    left_join(securityCount) %>% 
    left_join(childCount) %>% 
    left_join(parkingCount)

write_csv(dat, file = "accident.csv")
write_csv(cctvCount, file = "cctvCount.csv")
write_csv(securityCount, file = "securityCount.csv")
write_csv(childCount, file = "childCount.csv")
write_csv(parkingCount, file = "parkingCount.csv")

testDat <- test %>% 
    left_join(cctvCount) %>% 
    left_join(securityCount) %>% 
    left_join(childCount) %>% 
    left_join(parkingCount)

write_csv(testDat, file = "testMerge.csv")


median(cctvCount$n_cctv)
median(securityCount$n_security)
median(childCount$n_child)
median(parkingCount$n_parking)

cctvCount %>% 
    ggplot(aes(x = `시군구`, y = n_cctv)) +
    geom_point()

cctvCount %>% 
    arrange(-n_cctv)
