import * as React from 'react';

const MONTH_NAMES = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
const WEEKDAY_NAMES = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
export class Util {
  static addDays(date: Date, days: number): Date {
    let clone = new Date(date)
    return new Date(clone.setUTCDate(clone.getUTCDate() + days))
  }

  static isFirstDayOfMonth(date: Date): boolean {
    return date.getUTCDate() == 1
  }

  static monthYearString(date: Date): string {
    //TODO: Use Moment.js or something?
    return MONTH_NAMES[date.getUTCMonth()] + ' ' + date.getUTCFullYear().toString()
  }

  static datePart(date: Date): Date {
    let clone = new Date(date)
    return new Date(clone.setUTCHours(0, 0, 0, 0))
  }

  static datesEqual(a: Date, b: Date): boolean {
    return a.valueOf() == b.valueOf()
  }

  static dateDiffDays(a: Date, b: Date): number {
    let aval = Util.datePart(a).valueOf()
    let bval = Util.datePart(b).valueOf()

    return (aval - bval) / (24*60*60*1000)
  }

  static isWeekend(date: Date): boolean {
    return date.getUTCDay() == 0 || date.getUTCDay() == 6
  }

  static weekdayName(date: Date): string {
    return WEEKDAY_NAMES[date.getUTCDay()]
  }

  static monthName(date: Date): string {
    return MONTH_NAMES[date.getUTCMonth()]
  }

  static day(date: Date): number {
    return date.getUTCDate()
  }

  static positionalPostfix(num: number): string {
    if([11, 12, 13].indexOf(num) >= 0) {
      return "th"
    }

    if(num % 10 == 1) {
      return "st"
    }
    
    if(num % 10 == 2) {
      return "nd"
    }
    
    if(num % 10 == 3) {
      return "rd"
    }
    
    return "th"
  }

  static today(): Date {
    return Util.datePart(new Date(Date.now()))
  }

  static previousWeekday(date: Date, weekday: number): Date {
    let daysToSubtract = (date.getUTCDay() - weekday + 7) % 7
    return Util.addDays(date, -daysToSubtract)
  }
}