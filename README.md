# prime-calendar

Export [iCalendar](https://tools.ietf.org/html/rfc5545) file contains only prime days, like 20200109, 20200111, etc.

We deploy ical file to <https://gtgteq.github.io/prime-calendar/prime-calendar.ical>.

We use [arithmoi](https://hackage.haskell.org/package/arithmoi)'s `isPrime` function for prime testing.

## usage

```
stack run -- range --start 20200131 --end 20200630
stack run -- term 2020 # equals to: range --start 20200101 --end 20210331
stack run -- year 2020 # equals to: range --start 20200101 --end 20201231
```
