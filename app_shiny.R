library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(naniar)
library(DT)

load("model.RData")
dat5<-read_excel("D:/2022_Study/ML/learning data/63-2.xlsx",sheet = "1")
glimpse(dat5)
dat5<-dat5[,c(1:5,7:11)]
names(dat5)[]<-c("id","s.id","name","surname","midterm","att","prac","crit","proposal","project")
summary(dat5)
dat5$midterm<-as.numeric(dat5$midterm)
dat5$proposal<-as.numeric(dat5$proposal)
dat5$project<-as.numeric(dat5$project)
dat5[is.na(dat5)]<-0
dat5$att<-round(dat5$att/5*100)
dat5$prac<-round(dat5$prac/5*100)
dat5$s.id<-as.character(dat5$s.id)
mean_score<-(as.numeric(dat5$crit)+as.numeric(dat5$project)+as.numeric(dat5$proposal))/40*20
dat5<-cbind(dat5,mean_score)


sum.mid<-c(nrow(dat5),round(mean(dat5$midterm),2),
           round(sd(dat5$midterm),2),
           round(min(dat5$midterm)),
           round(max(dat5$midterm)))
sum.mid<-as.data.frame(t(sum.mid))
names(sum.mid)[]<-c("N","mean","sd","min","max")

#user interface part
ui<-fluidPage(
    
    h2("ระบบรายงานผลการเรียน"),
    fluidRow(
        column(6,
        textInput("stuID","กรุณากรอกรหัสนิสิต"),
        actionButton("submit","Submit"),
        hr(),
        p(strong("คะแนนสอบกลางภาคของนิสิตตอนเรียนที่ 1")),
        plotOutput("plot",height = "80px",width = "400px"),
        p(strong("ค่าสถิติพื้นฐาน")),
        dataTableOutput("summary",height = "80px",width = "400px"),
    ),
        column(6,
        p(strong("รายงานความก้าวหน้าของนิสิต")),
        textOutput("name"),
        textOutput("surname"),
        textOutput("midEx"),
        textOutput("att"),
        textOutput("work"),
        textOutput("score"),
        h5(strong("คำแนะนำสำหรับนิสิต")),
        textOutput("result"),
        hr(),
        p("หมายเหตุ: คำแนะนำที่ปรากฏมาจากการทำนายโดยใช้ข้อมูลผลการเรียนรู้และพฤติกรรมการเรียนรู้ของนิสิตรุ่นก่อนหน้า เป็นเพียงข้อคาดการณ์เท่านั้น")
    )
    )
)
    
server<-function(input, output, session){
    
    stu.filter<-reactiveValues(data=NULL)
    result.stu<-reactiveValues(data=NULL)
    show.result<-reactiveValues(data=NULL)
    
    observeEvent(input$submit,{
        stu.filter$data<-dat5%>%filter(s.id==input$stuID)
        result.stu$data<-predict(fit.knn2,stu.filter$data[,c(5:7,11)])
        if(result.stu$data==0){show.result$data<-"นิสิตมีโอกาสได้เกรดระหว่าง A ถึง C ควรตั้งใจอย่างเสมอต้นเสมอปลาย"}
        else {show.result$data<-"นิสิตมีโอกาสได้เกรดระหว่าง D+ ถึง F ควรพัฒนาตนเอง"}
    })
    
    output$name<-renderText({
        paste("ชื่อนิสิต: ",stu.filter$data$name)
    })
    output$surname<-renderText({
        paste("นามสกุล: ",stu.filter$data$surname)
    })
    output$midEx<-renderText({
        paste("คะแนนสอบกลางภาค(เต็ม 25): ",stu.filter$data$midterm)
    })
    output$att<-renderText({
        paste("ร้อยละของการเข้าเรียน: ",stu.filter$data$att)
    })
    output$result<-renderText({
        paste("",show.result$data)
    })
    output$work<-renderText({
        paste("ร้อยละของการส่งการบ้าน: ",stu.filter$data$prac)
    })
    output$score<-renderText({
        paste("คะแนนเฉลี่ยของชิ้นงานที่ได้รับ(เต็ม 20):",stu.filter$data$mean_score)
    })
    
    output$plot<-renderPlot({
        ggplot(dat5,aes(x = midterm)) + 
            geom_dotplot(binwidth = 0.5,fill = "maroon2")+
            labs(x="คะแนน",y="")+
            theme(axis.text.y =element_blank(),
                  panel.background = element_blank(),
                  axis.ticks.y=element_blank())
                  
        
    })
    
    output$summary<-renderDT({
        datatable(sum.mid,options = list(dom='t'))
    })
    
}

#create Shiny app!!
shinyApp(ui=ui, server=server)