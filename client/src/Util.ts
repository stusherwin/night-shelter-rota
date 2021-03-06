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
    return Util.datePart(a).valueOf() == Util.datePart(b).valueOf()
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

  static toDateString(date: Date): string {
    return `${date.getUTCFullYear()}-${date.getUTCMonth() + 1}-${date.getUTCDate()}`
  }

  static update<T, U>(t: T, u: U): T & U {
    return Object.assign({}, t, u)
  }

  static dedupeBy<T, U>(items: T[], fn: (t: T) => U): T[] {
    let result = [] as T[]

    for(let i of items) {
      if(!result.find(r => fn(r) == fn(i))) {
        result.push(i)
      }
    }

    return result
  }

  static arraysSame<T>(a: T[], b: T[]): boolean {
    if(a.length != b.length) {
      return false
    }

    for(let i in a) {
      if(a[i] != b[i]) {
        return false
      }
    }

    return true
  }
}

export function pure<T>(func: (props: T) => any) {
  class PureComponentWrap extends React.PureComponent<T> {
    render() {
      return func(this.props)
    }
  }
  return PureComponentWrap
}