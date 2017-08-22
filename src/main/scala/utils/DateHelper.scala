package utils

import java.sql.Timestamp
import java.text.{DateFormatSymbols, SimpleDateFormat}
import java.util.{Calendar, Date, GregorianCalendar, TimeZone}

import org.joda.time._
import org.joda.time.format.DateTimeFormat

/**
  * Created by Aleksey Voronets on 22.08.17.
  */
object DateHelper extends DateHelper

trait DateHelper {
    final val MINUTES_IN_MSECS = 1000 * 60
    final val HOURS_IN_MSECS = MINUTES_IN_MSECS * 60
    final val DAYS_IN_MSECS = HOURS_IN_MSECS * 24

    def inLast24Hours(d: Date): Boolean = {
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.HOUR, -24)
        val activationCalendar = Calendar.getInstance()
        activationCalendar.setTime(d)
        calendar.before(activationCalendar)
    }

    def createCalendar(date: Date): Calendar = {
        val calendar: Calendar = new GregorianCalendar()
        calendar.setTime(date)
        calendar.setFirstDayOfWeek(Calendar.MONDAY)
        calendar
    }

    protected val monthDatNames = scala.collection.Map(
        "RU" -> Array("января", "февраля", "марта", "апреля", "мая", "июня", "июля", "августа", "сентября", "октября", "ноября", "декабря"),
        "EN" -> Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

    //TODO: not thread safe. Must be killed
    private val birthdayFormat = createMonthsDateFormat("dd MMMM yyyy г.")

    val formalDateFormat = createMonthsDateFormat("\"dd\" MMMM yyyy г.")

    def dayMonthYearDateFormat(langCode: String = "RU") = createMonthsDateFormat("dd MMMM yyyy г.", langCode)

    //TODO: скорей всего в будущем будет более удобно API, построенное на extension methods и implicit TimeZones
    //TODO: плюс по-хорошему нужно кешировать форматы
    private lazy val standardDateFormat = DateTimeFormat.forPattern("dd.MM.yyyy")

    def parseDate(dateStr: String): Date = {
        standardDateFormat.parseLocalDateTime(dateStr).toDate
    }

    def formatDate(date: Date, zone: TimeZone = TimeZone.getDefault): String = {
        standardDateFormat.print(new DateTime(date, DateTimeZone.forTimeZone(zone)))
    }

    private lazy val standardDateTimeFormat = DateTimeFormat.forPattern("dd.MM.yyyy HH:mm")

    // Йода падает когда время попадает на летний/зимний переход времени.
    // Поэтому сделано на SimpleDateFormat. Он добовляет переход автоматом.
    def parseDateTime(dateTimeStr: String, zone: TimeZone = TimeZone.getDefault): Timestamp = {
        val dateFormat : SimpleDateFormat= new SimpleDateFormat("dd.MM.yyyy HH:mm")
        dateFormat.setTimeZone(zone)
        new Timestamp (dateFormat.parse(dateTimeStr).getTime)
    }

    def formatDateTime(dateTime: Timestamp, zone: TimeZone = TimeZone.getDefault): String = {
        standardDateTimeFormat.print(new DateTime(dateTime, DateTimeZone.forTimeZone(zone)))
    }

    private lazy val fileDateTimeFormat = DateTimeFormat.forPattern("dd MMMM yyyy г. в HH:mm")

    def formatFileDateTime(dateTime: Timestamp, zone: TimeZone = TimeZone.getDefault): String = {
        fileDateTimeFormat.print(new DateTime(dateTime, DateTimeZone.forTimeZone(zone)))
    }

    private lazy val standartTimeFormat = DateTimeFormat.forPattern("HH:mm")

    def formatTime(dateTime: Timestamp, zone: TimeZone = TimeZone.getDefault): String = {
        standartTimeFormat.print(new DateTime(dateTime, DateTimeZone.forTimeZone(zone)))
    }

    private def createMonthsDateFormat(formatString: String, langCode: String = "RU") = {
        val format = new SimpleDateFormat(formatString)
        val symbols = new DateFormatSymbols()
        val monthDatName = monthDatNames.get(langCode).getOrElse(monthDatNames.head._2)
        symbols.setMonths(monthDatName)
        format.setDateFormatSymbols(symbols)
        format
    }

    def ddMMMMyyyy(date: Date) = _format(birthdayFormat, date)

    private def _format(formatter: SimpleDateFormat, date: Date) = if (date == null) "" else formatter.format(date)

    def formatBirthday(date: Date): String = ddMMMMyyyy(date)

    def formatWithoutTimeZone(date: Date) = date.toString

    def formatYear(date: Date) = if (date == null) "" else createCalendar(date).get(Calendar.YEAR).toString

    private def dayFormat= new SimpleDateFormat("dd")

    private def monthFormat = new SimpleDateFormat("MMMM")

    def formatDay(date: Date) = _format(dayFormat, date)

    def formatMonth(date: Date) = _format(monthFormat, date)

    /**
      * Разница в месяцах, между date1 и date2.
      */
    def getDiffInYears(date1: Date, date2: Date) = math.abs(Years.yearsBetween(
        new DateTime(date1.getTime), new DateTime(date2.getTime)).getYears)

    def getDiffInMonths(date1: Date, date2: Date) = math.abs(Months.monthsBetween(
        new DateTime(date1.getTime).withDayOfMonth(1),
        new DateTime(date2.getTime).withDayOfMonth(1)).getMonths)

    def getDiffInDays(date1: Date, date2: Date) =
        Days.daysBetween(new DateTime(date1).toDateMidnight(), new DateTime(date2).toDateMidnight() ).getDays()

    def diffInMinutes(date1: Date, date2: Date) = math.abs(Minutes.minutesBetween(
        new DateTime(date1.getTime),
        new DateTime(date2.getTime)).getMinutes)

    def formatTimeInterval(msec: Long): String = {
        val days = msec / DAYS_IN_MSECS
        val hours = msec / HOURS_IN_MSECS - days * 24
        val minutes = msec / MINUTES_IN_MSECS - hours * 60 - days * 24 * 60
        val seconds = msec / 1000 - minutes * 60 - hours * 3600 - days * 24 * 3600

        "%d дней %d часов %02d минут %02d секунд".format(days, hours, minutes, seconds)
    }

    def currentYear = createCalendar(new Date()).get(Calendar.YEAR)
    def now = new Timestamp(new Date().getTime)
    def tomorrow = plusDays(now, 1)
    def plusDays(date: Timestamp, days: Int) = timestamp(datetime(date).plusDays(days))
    def plusYears(date: Timestamp, years: Int) = timestamp(datetime(date).plusYears(years))

    def datetime(date: Date) = new DateTime(date)
    def datetime(date: Timestamp) = new DateTime(date)
    def timestamp(date: Date) = new Timestamp(date.getTime)
    def timestamp(date: DateTime) = new Timestamp(date.toDate.getTime)

    def minutesAgo(minutes: Int) = {
        val now = new DateTime()
        val r = now.minus(Minutes.minutes(minutes))
        r.toDate
    }

    def minusDays(date: Timestamp, days: Int) = timestamp(datetime(date).minusDays(days))

    def secondsAgo(seconds: Int) = new DateTime().minus(Seconds.seconds(seconds)).toDate

    def tsHourBefore = {
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.HOUR, -1)
        new Timestamp(calendar.getTimeInMillis)
    }

    def tsDayBefore = {
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.DATE, -1)
        new Timestamp(calendar.getTimeInMillis)
    }

    def floor(date: Date) = {
        val calendar = createCalendar(date)
        calendar.set(Calendar.HOUR, 0)
        calendar.set(Calendar.MINUTE, 0)
        calendar.set(Calendar.SECOND, 0)
        calendar.set(Calendar.MILLISECOND, 0)

        new Timestamp(calendar.getTimeInMillis)
    }

    def ceiling(date: Date) = {
        val calendar = createCalendar(date)
        calendar.set(Calendar.HOUR, 23)
        calendar.set(Calendar.MINUTE, 59)
        calendar.set(Calendar.SECOND, 59)
        calendar.set(Calendar.MILLISECOND, 999)

        new Timestamp(calendar.getTimeInMillis)
    }

    def curDateYearsBefore(years: Int) = {
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.YEAR, -years)
        calendar.getTime
    }

    def isSameDay(date1: Date, date2: Date) = getDiffInDays(date1, date2) == 0

}
