library(shiny)
data_ex <- data.table::data.table(a=as.integer(c(1,1,1,1)),
                                  b=c(400,100,12,5),
                                  c=c("Driver","Sand Wedge","Lob Wedge","Putter"),
                                  d=c("Fairway","Sand","Green","Hole"),
                                  e=c(100,12,5,0))
data_ex <- data.table::setnames(data_ex, c("Hole","Distance to Hole (Start)","Club",
                                           "End Location","Distance to Hole (End)"))
benchmark.data <- read.csv("DataRaw.csv", header = TRUE)
benchmark.data.m <- benchmark.data
benchmark.data.m$Distance <- benchmark.data$Distance*0.9
func.list <- list()
for (i in 2:ncol(benchmark.data)) {
  func <- approxfun(benchmark.data[,1],benchmark.data[,i])
  func.list[[i]] <- func
  rm(list = c("func", "i"))
}
func.list <- func.list[-1]
names(func.list) <- colnames(benchmark.data)[-1]
func.list.m <- list()
for (i in 2:ncol(benchmark.data.m)) {
  func <- approxfun(benchmark.data.m[,1],benchmark.data.m[,i])
  func.list.m[[i]] <- func
  rm(list = c("func", "i"))
}
func.list.m <- func.list.m[-1]
names(func.list.m) <- colnames(benchmark.data.m)[-1]

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML('body {font-family:"Consolas"}')),
  titlePanel(h2("Strokes Gained Calculator")),
  sidebarLayout(
    sidebarPanel(
      h3("Off-the-Tee"),
      h4("Player performance off the tee on all par-4s and par-5s."),
      h3("Approach-the-Green"),
      h4("Player performance on approach shots; that is, all shots that are not from the tee
          on par-4s and par-5s and are not within 30 yards of the edge of the green (27.432 meters).
          Approach shots include tee shots on par-3s."),
      h3("Around the Green"),
      h4("Player performance on any shot within 30 yards (27.432 meters) of the edge of the green.
          This metric excludes any shots taken on the putting green."),
      h3("Putting"),
      h4("Player performance inside the putting green."),

      tags$hr(),

      fileInput("file1", h3("Choose .CSV File"),
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      h4("Make sure that your data file follows the same structure as the table in the example provided.
         Please provide your preferable distance unit as the calculator will assume meters as default."),
      radioButtons("buttons", h3("Distance Unit"),
                   choices = list("Meters"=1,"Yards/Feet"=2))
    ),
    mainPanel(
      h3("Format of input data"),
      tags$hr(),
      h4("Let us assume we start our round on a 400 yards (365.76 meters) par-4. We tee up and smash
         our first shot of the day with the big stick 300 yards (274.32 meters) in the middle of the
         fairway. From exactly 100 yards to the hole (91.44 meters) we take our sand wedge landing the
         ball inside a green-side bunker. This approach shot was a short-side miss from the pin, but we
         have left ourselves with a clear lie 12 yards from the hole (11 meters). We take our lob wedge
         exploding it onto the green inside 5 feet (1.5 meters) from the hole sanking the putt for par."),
      h4("The table below summarises the hole statistics needed to compute the strokes gained metric. For
         the remaining 17 holes, the shot-tracking procedure is the same."),
      tableOutput("data_ex"),
      h3("How does SG work?"),
      tags$hr(),
      h4("To compute SG we compare ourselves to a baseline. At the current stage of the app, the benchmark
         are PGA Tour pros averages."),
      h4("From 400 yards out (365.76 meters), the average strokes to hole out
         is 3.99. Our tee shot finished in the fairway 100 yards (91.44 meters) from the hole, where the
         average strokes to hole out is 2.8. We have progressed 1.19 strokes “closer” to the hole with
         our first shot (i.e. 3.99 - 2.8 = 1.19). However, we took one stroke which gives us a SG value
         of 0.19 (a decrease of 1.19 in the average strokes to hole out minus one to account for the
         stroke). Our tee shot gained us 0.19 strokes compared to the PGA Tour average."),
      h4("From the fairway at 100 (91.44 meters) yards, as we just saw, the Tour average strokes to hole out
         is 2.8. The shot finished in the sand 12 yards from the hole, where the average strokes to hole out is
         2.45. We progressed 0.35 closer to the hole (i.e. 2.8 - 2.45 = 0.35), but we took a bunker shot
         to do it, so the SG value is -0.65 (a decrease in 0.35 in the average strokes to hole out minus
         one to account for the stroke. Our approach shot to the green was 0.65 strokes below average."),
      h4("A 12 (11 meters) yards bunker shot takes 2.45 strokes for a PGA Tour player to hole. We left it 5 feet
         from the hole, where again the Tour average to hole out is 1.23 strokes. That gives us 2.45 minus 1.23
         equal 1.22 strokes closer to the hole, or a SG score for the bunker shot of 0.22 above Tour average."),
      h4("Our putt started 5 feet (1.5 meters) from the hole, where the Tour average strokes to hole out is 1.23.
         We sank the putt. The decrease in the average strokes to hole out is 1.23, so the strokes gained of the
         putt is 0.23."),
      h4("This is all there is to it! Our SG for our four shots were 0.19, -0.65, 0.22, and 0.23. Our
         above-average shots had positive strokes gained. Our below-average shot had negative strokes gained.
         After the below-average approach shot, we had lost a total of 0.65 strokes, but then we gained 0.5
         strokes with our final two shots. Overall, we had a total strokes gained of -0.01 given that it takes
         a PGA Tour player 3.99 strokes to complete a 400 yard (365.76 meters) par-4; we are not comparing our
         selves to scratch players, but the crème de la crème!"),
      h4("NOTE: “fairway” category also includes the “fairway fringe” or “first cut.”"),
      h3("What about penalty/recovery shots?"),
      tags$hr(),
      h4("Recovery shots ar those where the direct shot to the hole is impeded by trees or other obstacles.
         Even if the golfer decides to hit toward the hole through a small opening in trees, or attempts a hook
         or slice around an obstacle, it is still considered a recovery shot because the golfer is recovering
         from trouble. Suppose a golfer hits a long drive but ends up behind a tree and is forced to chip back
         out onto the fairway for the second shot, that is, the second shot is a recovery shot. Labelling a shot
         a recovery shot is a judgement call, but this calculator accounts for this."),
      h4("")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$data_ex = renderTable(
    data_ex, align = "c", striped = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)

