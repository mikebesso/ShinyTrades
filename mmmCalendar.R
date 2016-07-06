
mmmCalendarAPI <- function(){

  WeekdaySequence <- function(from = Sys.Date() - 14, to = Sys.Date()){
    StartDate <- as.Date(from);
    EndDate <- as.Date(to);
    myDays <- seq(StartDate , EndDate, by = "day");
    excludeDays <- c("Saturday", "Sunday");
    myWeekDays <- subset(myDays, !weekdays(myDays) %in% excludeDays);
    subset(myDays, !weekdays(myDays) %in% excludeDays);
  };


 CurrentWeekday <- function(AsOfDate){
   max(WeekdaySequence(from = Sys.Date() - 4));
 }




 EndPointsDaily <- function(INDEX, DayStartTime = '00:00') {

   if (is.zoo(INDEX) | is.xts(INDEX)) {
     INDEX <- index(INDEX);
   }

   if (DayStartTime != '00:00'){
     Hours_Minutes <- as.integer(unlist(str_split(DayStartTime, ':')));
     INDEX <- INDEX - (3600L * Hours_Minutes[1]) - (60L * Hours_Minutes[2]);
   }

   endpoints(INDEX, 'days');

 }

 ApplyDaily <- function(x, INDEX, DayStartTime = '00:00', FUN, ...){

   EndPoints <- EndPointsDaily(INDEX, DayStartTime);

   apply.daily(x, EndPoints, FUN, ...);

 }


 DetermineDateTimeFormat <- function(x){
   TryFormat <- function(x, format){
     x <- strptime(x, format = format, tz = App$GetTimeZone());
     if (any(is.na(x))){
       return(NULL);
     } else {
       return(format);
     }
   }

   format <- TryFormat(x, "%Y-%m-%dT%H:%M");
   if (is.null(format)){
     format <- TryFormat(x, "%Y-%m-%d %H:%M");
   }
   if (is.null(format)){
     format <- TryFormat(x, "%Y-%m-%d");
   }

   return(format);
 }


  return(
    list(
      WeekdaySequence = WeekdaySequence,
      CurrentWeekday = CurrentWeekday,
      EndPointsDaily = EndPointsDaily,
      ApplyDaily = ApplyDaily,
      DetermineDateTimeFormat = DetermineDateTimeFormat
    )
  )

}


#
# class Easter
# {
#   public static void main(String[] args)
#   {
#     System.out.print("Please enter a year to calculate Easter Sunday\n>");
#     Scanner s = new Scanner(System.in);
#     int inputted = getResult(s);
#     while(inputted <= 0)
#     {
#       System.out.print("Expected a positive year. Please try again:\n>");
#       inputted = getResult(s);
#     }
#     System.out.println(getEasterSundayDate(inputted));
#   }
#
#   private static int getResult(Scanner s)
#   {
#     while(!s.hasNextInt())
#     {
#       System.out.print("Expected a valid year. Please try again:\n>");
#       s.nextLine();
#     }
#     return s.nextInt();
#   }
#
#   public static String getEasterSundayDate(int year)
#   {
#     int a = year % 19,
#     b = year / 100,
#     c = year % 100,
#     d = b / 4,
#     e = b % 4,
#     g = (8 * b + 13) / 25,
#     h = (19 * a + b - d - g + 15) % 30,
#     j = c / 4,
#     k = c % 4,
#     m = (a + 11 * h) / 319,
#     r = (2 * e + 2 * j - k - h + m + 32) % 7,
#     n = (h - m + r + 90) / 25,
#     p = (h - m + r + n + 19) % 32;
#
#     String result;
#     switch(n)
#     {
#       case 1:
#       result = "January ";
#       break;
#       case 2:
#       result = "February ";
#       break;
#       case 3:
#       result = "March ";
#       break;
#       case 4:
#       result = "April ";
#       break;
#       case 5:
#       result = "May ";
#       break;
#       case 6:
#       result = "June ";
#       break;
#       case 7:
#       result = "July ";
#       break;
#       case 8:
#       result = "August ";
#       break;
#       case 9:
#       result = "September ";
#       break;
#       case 10:
#       result = "October ";
#       break;
#       case 11:
#       result = "November ";
#       break;
#       case 12:
#       result = "December ";
#       break;
#       default:
#       result = "error";
#     }
#
#     return result + p;
#   }
# }
