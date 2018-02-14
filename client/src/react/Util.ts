import * as React from 'react';

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
    let monthNames = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
    return monthNames[date.getUTCMonth()] + ' ' + date.getUTCFullYear().toString()
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
}