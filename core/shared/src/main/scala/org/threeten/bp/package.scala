/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten

/**
 * The main API for dates, times, instants, and durations.
 *
 * The classes defined here represent the principal date-time concepts, including instants,
 * durations, dates, times, time-zones and periods. They are based on the ISO calendar system, which
 * is the <i>de facto</i> world calendar following the proleptic Gregorian rules. All the classes
 * are immutable and thread-safe.
 *
 * Each date time instance is composed of fields that are conveniently made available by the APIs.
 * For lower level access to the fields refer to the {@link org.threeten.bp.temporal} package. Each
 * class includes support for printing and parsing all manner of dates and times. Refer to the
 * {@link org.threeten.bp.format} package for customization options.
 *
 * The {@link org.threeten.bp.chrono} package contains the calendar neutral API. This is intended
 * for use by applications that need to use localized calendars. It is recommended that applications
 * use the ISO-8601 dates and time classes from this package across system boundaries, such as to
 * the database or across the network. The calendar neutral API should be reserved for interactions
 * with users.
 *
 * ==Dates and Times==
 *
 * {@link org.threeten.bp.Instant} is essentially a numeric timestamp. The current Instant can be
 * retrieved from a {@link org.threeten.bp.Clock}. This is useful for logging and persistence of a
 * point in time and has in the past been associated with storing the result from {@link
 * java.lang.System#currentTimeMillis()}.
 *
 * {@link org.threeten.bp.LocalDate} stores a date without a time. This stores a date like
 * '2010-12-03' and could be used to store a birthday.
 *
 * {@link org.threeten.bp.LocalTime} stores a time without a date. This stores a time like '11:30'
 * and could be used to store an opening or closing time.
 *
 * {@link org.threeten.bp.LocalDateTime} stores a date and time. This stores a date-time like
 * '2010-12-03T11:30'.
 *
 * {@link org.threeten.bp.OffsetTime} stores a time and offset from UTC without a date. This stores
 * a date like '11:30+01:00'. The {@link org.threeten.bp.ZoneOffset ZoneOffset} is of the form
 * '+01:00'.
 *
 * {@link org.threeten.bp.OffsetDateTime} stores a date and time and offset from UTC. This stores a
 * date-time like '2010-12-03T11:30+01:00'. This is sometimes found in XML messages and other forms
 * of persistence, but contains less information than a full time-zone.
 *
 * {@link org.threeten.bp.ZonedDateTime} stores a date and time with a time-zone. This is useful if
 * you want to perform accurate calculations of dates and times taking into account the {@link
 * org.threeten.bp.ZoneId}, such as 'Europe/Paris'. Where possible, it is recommended to use a
 * simpler class. The widespread use of time-zones tends to add considerable complexity to an
 * application.
 *
 * ==Duration and Period==
 *
 * Beyond dates and times, the API also allows the storage of period and durations of time. A {@link
 * org.threeten.bp.Duration} is a simple measure of time along the time-line in nanoseconds. A
 * {@link org.threeten.bp.Period} expresses an amount of time in units meaningful to humans, such as
 * years or hours.
 *
 * ==Additional value types==
 *
 * {@link org.threeten.bp.Year} stores a year on its own. This stores a single year in isolation,
 * such as '2010'.
 *
 * {@link org.threeten.bp.YearMonth} stores a year and month without a day or time. This stores a
 * year and month, such as '2010-12' and could be used for a credit card expiry.
 *
 * {@link org.threeten.bp.MonthDay} stores a month and day without a year or time. This stores a
 * month and day-of-month, such as '--12-03' and could be used to store an annual event like a
 * birthday without storing the year.
 *
 * {@link org.threeten.bp.Month} stores a month on its own. This stores a single month-of-year in
 * isolation, such as 'DECEMBER'.
 *
 * {@link org.threeten.bp.DayOfWeek} stores a day-of-week on its own. This stores a single
 * day-of-week in isolation, such as 'TUESDAY'.
 */
package object bp
