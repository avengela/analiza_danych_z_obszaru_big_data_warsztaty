# Zadanie 1
# Zaimportuje plik gospodarstwa.xlsx

library(readxl)

setwd('C:\\Users\\angel\\R\\warsztaty_big_data')

dane <- read_excel("gospodarstwa.xlsx")

# zadanie 2
# Utwórz zbiór, w którym są gospodarstwa ze wsi.

g_2 <- dane[which(dane$klm == 6), ]

# Zadanie 3
# Utwórz zbiór, w którym są gospodarstwa o dochodach > 2000 zł.

g_3 <- dane[which(dane$dochg > 2000), ]


# Zadanie 4
# Utwórz zbiór, który zawiera gospodarstwa z woejwództwa wielkoposlkiego oraz które zamieszkują wieś i mają dochody powyżej 3000 zł.

g_4 <- dane[which(dane$woj == "30" | dane$klm == 6 & dane$dochg > 3000 ), ]

# Zadanie 5
# Wybierz gospodarstwa domowe z województwa dolnośląskiego i mazowieckiego z miast powyżej 500 mieszkańców.

g_5 <- dane[which((dane$woj == "02" | dane$woj == "14") & dane$klm == 1 ), ]


# Zadanie 6
# Losowo wybierz zbiór 30% gospodarstw domowych.
library(dplyr)
g_6  <- dane %>% sample_frac(0.3)



# Zadanie 7 
# Losowo wybierz 100 gospodarstw.

g_7 <- dane %>% sample_n(100)



# Zadanie 8
# Wybierz gospodarstwa domowe z wierszy 10-15.

g_8 <- dane[10:15, ]

# Zadanie 9
# Wybierz gospodarstwa domowe z danymi tylko dla kolumn woj i wydg ale w odniesieniu jedynie dla gospodarstw województwa wielkopolskiego.

g_9 <- dane[which(dane$woj == "30"), c("woj", "wydg")]

# Zadanie 10
# Wybierz wszystkie gospodarstwa, których dochód jest z przedziału 3000 - 4000 i zostaw tylko tą zmienną w zbiorze.

g_10 <- dane[which(dane$doch > 3000 & dane$doch < 4000), c("woj", "wydg")]

# Zadanie 11
# Wybierz wszystkie kolumny od klm do zut włącznie.

g_11 <- dane[, c(1:4)]

# Zadanie 12
# Wyświetl nazwy wszystkich zmiennych w zbiorze gospodarstwa.

print(names(dane))

# Zadanie 13. 
# Wybierz wszystkie kolumny, które zaczynają się na literę d.

g_13 <- dane[, grep("^d", names(dane))]

# Zadanie 14. 
# Utwórz nową zmienną roznica-dochg-wydg. Pozostaw w zbiorze zmienne dochg, wydg i roznica.

dane$roznica<- dane$dochg - dane$wydg
g_14 <- dane[, c("dochg", "wydg", "roznica")]


# Zadanie 15. 
#Utwórz nowe zmienne x=ln(dochg) oraz y=ln(wydg). Pamiętaj, że In liczymy dla dodatnich wartości.

dane$x <- ifelse(dane$dochg > 0, log(dane$dochg), NA)
dane$y <- ifelse(dane$wydg > 0, log(dane$wydg), NA)



# Zadanie 16. 
#Dokonaj zamiany nazwy zmiennych dochg na dochod oraz wydg na wydatki.

names(dane)[15] <- "dochód"
names(dane)[16] <- "wydatki"


# Zadanie 17. 
#Oblicz ile było gospodarstw domowych ze względu na poszczególne warianty zmiennej klm.

table(dane$klm)

# Zadanie 18. 
#Oblicz ile było gospodarstw domowych w poszczególnych wojewódz- twach.
table(dane$woj)



# Zadanie 19. 
#Oblicz ile było gospodarstw domowych w poszczególnych województwach ze względu na warianty zmiennej d61 (Sytuacja materialna gospodarstwa). 

table(dane$woj, dane$d61)


# Zadanie 20. 
#Oblicz średnie wydatki wszystkich gospodarstw domowych. 

mean(dane$wydatki, na.rm = TRUE)

#Zadanie 21. 
#Oblicz średnie wydatki i dochody wszystkich gospodarstw domowych. 

mean(dane$wydatki, na.rm = TRUE)
mean(dane$dochód, na.rm = TRUE)


#Zadanie 22. 
#Oblicz średnią, min, max, odchylenie standardowe oraz medianę wy- datków.

mean(dane$wydatki, na.rm = TRUE)
min(dane$wydatki, na.rm = TRUE)
max(dane$wydatki, na.rm = TRUE)
median(dane$wydatki, na.rm = TRUE)
sd(dane$wydatki, na.rm = TRUE)



# Zadanie 23. 
#Oblicz średnie wydatki w ramach klasy miejscowości.

tapply(dane$wydatki, dane$klm, mean, na.rm = TRUE)



# Zadanie 24. 
#Stwórz wykres słupkowy z liczebnościami dla klasy miejscowości.
barplot(table(dane$klm), names.arg = levels(dane$klm))


# Zadanie 25. 
# Posortuj rosnąco zbiór gospodarstw domowych ze względu na wydatki. 
g_25 <- dane[order(dane$wydatki),]



#Zadanie 26. 
#Posortuj malejąco zbiór gospodarstw domowych ze względu na do- chody.
g_26 <- dane[order(dane$wydatki, decreasing = TRUE ),]





# Zadanie 27. 
# Posortuj malejąco kod klasy miejscowości a w ramach tej klasy rosnąco wydatki.
g_27 <- dane[order(dane$klm, dane$wydatki, decreasing = c(TRUE, FALSE)),]


# Zadanie 28. 
# Zrób wykres pudełkowy powierzchni mieszkań (zmienna d36) według klasy miejscowości.
boxplot(dane$d36 ~ dane$klm, main = "Powierzchnie mieszkań według klasy miejscowości", xlab = "Klasa miejscowości", ylab = "Powierzchnia mieszkania (w metrach kwadratowych)")



# Zadanie 29. 
# Zrób wykres pudełkowy powierzchni mieszkań według źródła utrzy- mania dla gospodarstw z województwa mazowieckiego.
mazowieckie_data <- subset(dane, dane$woj == "14")
boxplot(mazowieckie_data$d36 ~ mazowieckie_data$zut, main = "Powierzchnie mieszkań według źródła utrzymania w województwie mazowieckim", xlab = "Źródło utrzymania", ylab = "Powierzchnia mieszkania (w metrach kwadratowych)")



# Zadanie 30. 
# Utwórz zbiór wlkp. W zbiorze tym powinny się znaleźć gospodarstwa z województwa wielkopolskiego, których dochód jest z przedziału 2500-3000 zł. Do- chód powinien być posortowany malejąco. Oprócz dochodu mają się znaleźć 4 inne zmienne: klm, woj, zut i wydg. Na tym zbiorze, dla zmiennych dochg i wydg, stwórz korelacyjny diagram rozrzutu.
g_30 <- dane[which(dane$dochód > 2500 & dane$dochód < 3000 & dane$woj == "30"), c("klm", "woj", "zut", "wydatki", "dochód")]
wlkp <- g_30[order(g_30$dochód, decreasing = TRUE ), ]


plot(wlkp$dochód, wlkp$wydatki, type = "p", main = "Korelacyjny diagram rozrzutu dla dochodu i wydatków", xlab = "Dochód (w zł)", ylab = "Wydatki (w zł)")



# Zadanie 31. 
# Utwórz dwie nowe zmienne roznica-dochg-wydg oraz doch_os=dochg/los i pozostaw je jako jedyne w zbiorze danych.

dane$doch_os <- dane$dochód / dane$los

q31  <- dane[ c("roznica", "doch_os")]



# Zadanie 32. 
#Utwórz nową zmienną „klasa", która przyjmuje dwie wartości: „wieś”, gdy gospodarstwo jest ze wsi oraz miasto", gdy gospodarstwo jest z miasta.

dane <- dane %>% mutate(klasa = case_when(klm==1~"Obsrana wieś z jednym Dino",
                       klm>1~"Miasto"))

# Zadanie 33. 
#Utwórz nową zmienną pow", która przyjmuje trzy warianty: „do 60", (60-100> oraz powyżej 100" w zależności od wartości zmiennej d36 (powierzchnia mieszkań).

dane <- dane %>% mutate(pow = case_when(d36<60~"do 60",
                                          d36<100~"60-100",
                                        d36>=100 ~ "powyżej 100"))
