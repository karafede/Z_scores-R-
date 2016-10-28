library(mailR)
library(rJava)

send.mail(from="ftir75@gmail.com",
          to=c("ftir75@gmail.com","karafede@hotmail.com"),
          subject="Test Email",
          body="PFA the desired document",
          html=T,
          smtp=list(host.name = "smtp.gmail.com",
                    port = 465,
                    user.name = "ftir75@gmail.com",
                    passwd = "vaxcrio111",
                    ssl = T),
          authenticate=T,
          attach.files="C:\\RICARDO-AEA\\NEED\\Summary_Energy.csv")
