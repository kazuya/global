/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2002, 2005
 *	Tama Communications Corporation
 *
 * This file is part of GNU GLOBAL.
 *
 * GNU GLOBAL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * GNU GLOBAL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <ctype.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#include "die.h"
#include "strbuf.h"

#ifndef isblank
#define isblank(c)	((c) == ' ' || (c) == '\t')
#endif
/*

String buffer: usage and memory status

					[xxx]: string buffer
					'v': current pointer

Function call                           Memory status
----------------------------------------------------------
                                        (not exist)
                                         v
sb = strbuf_open(0);                    []
                                          v
strbuf_putc(sb, 'a');                   [a]
                                          v
char *s = strbuf_value(sb);             [a\0]           s == "a"
                                            v
strbuf_puts(sb, "bc");                  [abc]
                                            v
char *s = strbuf_value(sb);             [abc\0]         s == "abc"
                                            v
int len = strbuf_getlen(sb);            [abc\0]         len == 3
                                         v
strbuf_reset(sb);                       [abc\0]
                                         v
int len = strbuf_getlen(sb);            [abc\0]         len == 0
                                           v
strbuf_puts(sb, "XY");                  [XYc\0]
                                           v
char *s = strbuf_value(sb);             [XY\0]          s == "XY"

fp = fopen("/etc/passwd", "r");                                             v
char *s = strbuf_fgets(sb, fp, 0)       [root:*:0:0:Charlie &:/root:/bin/csh\0]
fclose(fp)				s == "root:*:0:0:Charlie &:/root:/bin/csh"

strbuf_close(sb);                       (not exist)

*/

static void print_and_abort (void);
void (*strbuf_alloc_failed_handler) (void) = print_and_abort;

static void
print_and_abort()
{
	die("short of memory.");
}

#ifdef STRBUF_LINK
STRBUF	top;
/*
 * strbuf_dump: dump string buffers
 */
void
strbuf_dump(msg)
	char *msg;
{
	STRBUF *sb;
	int i = 0;

	fprintf(stderr, "[%s/%s]\n", progname, msg);
	for (sb = top.next; sb && sb != &top; sb = sb->next) {
		char *p = strbuf_value(sb);
		char *end = p + strbuf_getlen(sb);

		*sb->curp = 0;
		fprintf(stderr, "%d\tsize=%d", strbuf_getlen(sb));
		if (strbuf_getlen(sb) <= strlen(p))
			fprintf(stderr, ", value=|%s|\n", p);
		else {
			fprintf(stderr, "\n");
			for (; p < end; p += strlen(p) + 1)
				fprintf(stderr, "\t|%s|", p);
		}
	}
}
#endif
/*
 * __strbuf_expandbuf: expand buffer so that afford to the length data at least.
 *
 *	i)	sb	STRBUF structure
 *	i)	length	required room
 */
void
__strbuf_expandbuf(sb, length)
	STRBUF *sb;
	int length;
{
	int count = sb->curp - sb->sbuf;
	int newsize = sb->sbufsize + (length > EXPANDSIZE ? length : EXPANDSIZE);
	char *newbuf;

	if (sb->alloc_failed)
		return;
	newbuf = (char *)realloc(sb->sbuf, newsize + 1);
	if (newbuf == NULL) {
		(*strbuf_alloc_failed_handler)();
		sb->alloc_failed = 1;
		return;
	}
	sb->sbufsize = newsize;
	sb->sbuf = newbuf;

	sb->curp = sb->sbuf + count;
	sb->endp = sb->sbuf + sb->sbufsize;
}
/*
 * strbuf_open: open string buffer.
 *
 *	i)	init	initial buffer size
 *			if 0 is specified then use default value.
 *	r)	sb	STRBUF structure
 */
STRBUF *
strbuf_open(init)
	int init;
{
	STRBUF *sb = (STRBUF *)calloc(sizeof(STRBUF), 1);

	if (sb == NULL) {
		(*strbuf_alloc_failed_handler)();
		return NULL;
	}
	sb->sbufsize = (init > 0) ? init : INITIALSIZE;
	if (!(sb->sbuf = (char *)malloc(sb->sbufsize + 1))) {
		(*strbuf_alloc_failed_handler)();
		(void)free(sb);
		return NULL;
	}
	sb->curp = sb->sbuf;
	sb->endp = sb->sbuf + sb->sbufsize;

#ifdef STRBUF_LINK
	if (top.next == NULL) {
		top.next = top.prev = sb;
		sb->next = sb->prev = &top;
	} else {
		sb->next = &top;
		sb->prev = top.prev;
		top.prev->next = sb;
		top.prev = sb;
	}
#endif
	return sb;
}
/*
 * strbuf_clear: clear static string buffer.
 *
 *	i)	sb	statically defined string buffer
 *
 * This function is used for the initializing of static string buffer.
 * For the detail, see 'STATIC_STRBUF(sb)' macro in strbuf.h.
 */
void
strbuf_clear(sb)
	STRBUF *sb;
{
	if (sb == NULL)
		die("NULL string buffer. (strbuf_clear)");
	if (strbuf_empty(sb)) {
		sb->sbufsize = INITIALSIZE;
		if (!(sb->sbuf = (char *)malloc(sb->sbufsize + 1))) {
			(*strbuf_alloc_failed_handler)();
			return;
		}
		sb->curp = sb->sbuf;
		sb->endp = sb->sbuf + sb->sbufsize;
	} else {
		strbuf_reset(sb);
	}
}
/*
 * strbuf_nputs: Put string with length
 *
 *	i)	sb	statically defined string buffer
 *	i)	s	string
 *	i)	len	length of string
 */
void
strbuf_nputs(sb, s, len)
	STRBUF *sb;
	const char *s;
	int len;
{
	if (!sb->alloc_failed && len > 0) {
		if (sb->curp + len > sb->endp)
			__strbuf_expandbuf(sb, len);
		while (len-- > 0)
			*sb->curp++ = *s++;
	}
}
/*
 * strbuf_puts: Put string
 *
 *	i)	sb	statically defined string buffer
 *	i)	s	string
 */
void
strbuf_puts(sb, s)
	STRBUF *sb;
	const char *s;
{
	if (!sb->alloc_failed) {
		while (*s) {
			if (sb->curp >= sb->endp)
				__strbuf_expandbuf(sb, 0);
			*sb->curp++ = *s++;
		}
	}
}
/*
 * strbuf_putn: put digit string at the last of buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	n	number
 */
void
strbuf_putn(sb, n)
	STRBUF *sb;
	int n;
{
	if (n == 0) {
		strbuf_putc(sb, '0');
	} else {
		char num[128];
		int i = 0;

		while (n) {
			if (i >= sizeof(num))
				die("Too big integer value.");
			num[i++] = n % 10 + '0';
			n = n / 10;
		}
		while (--i >= 0)
			strbuf_putc(sb, num[i]);
	}
}
/*
 * strbuf_unputc: remove specified char from the last of buffer
 *
 *	i)	sb	STRBUF structure
 *	i)	c	character
 *	r)		0: do nothing, 1: removed
 */
int
strbuf_unputc(sb, c)
	STRBUF *sb;
	int c;
{
	if (sb->curp > sb->sbuf && *(sb->curp - 1) == c) {
		sb->curp--;
		return 1;
	}
	return 0;
}
/*
 * strbuf_value: return the content of string buffer.
 *
 *	i)	sb	STRBUF structure
 *	r)		string
 */
char *
strbuf_value(sb)
	STRBUF *sb;
{
	*sb->curp = 0;
	return sb->sbuf;
}
/*
 * strbuf_trim: trim following blanks.
 *
 *	i)	sb	STRBUR structure
 */
void
strbuf_trim(sb)
	STRBUF *sb;
{
	char *p = sb->curp;

	while (p > sb->sbuf && isblank(*(p - 1)))
		*--p = 0;
	sb->curp = p;
}
/*
 * strbuf_fgets: read whole record into string buffer
 *
 *	o)	sb	string buffer
 *	i)	ip	input stream
 *	i)	flags	flags
 *			STRBUF_NOCRLF	remove last '\n' if exist.
 *			STRBUF_APPEND	append next record to existing data
 *	r)		record buffer (NULL at end of file)
 *
 * Returned buffer has whole record.
 * The buffer end with '\0'.If STRBUF_NOCRLF is set then buffer doesn't
 * include '\r' and '\n'.
 */
char *
strbuf_fgets(sb, ip, flags)
	STRBUF *sb;
	FILE *ip;
	int flags;
{
	if (!(flags & STRBUF_APPEND))
		strbuf_reset(sb);

	if (sb->curp >= sb->endp)
		__strbuf_expandbuf(sb, EXPANDSIZE);	/* expand buffer */
	if (sb->alloc_failed)
		return sb->sbuf;

	for (;;) {
		if (!fgets(sb->curp, sb->endp - sb->curp, ip)) {
			if (sb->curp == sb->sbuf)
				return NULL;
			break;
		}
		sb->curp += strlen(sb->curp);
		if (*(sb->curp - 1) == '\n')
			break;
		else if (feof(ip)) {
			return sb->sbuf;
		}
		__strbuf_expandbuf(sb, EXPANDSIZE);	/* expand buffer */
		if (sb->alloc_failed)
			return sb->sbuf;
	}
	if (flags & STRBUF_NOCRLF) {
		if (*(sb->curp - 1) == '\n')
			*(--sb->curp) = 0;
		if (*(sb->curp - 1) == '\r')
			*(--sb->curp) = 0;
	}
	return sb->sbuf;
}
/*
 * strbuf_sprintf: do sprintf into string buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	s	similar to sprintf()
 *			Currently the following format is supported.
 *			%s, %d, %<number>d, %<number>s
 */
void
#ifdef HAVE_STDARG_H
strbuf_sprintf(STRBUF *sb, const char *s, ...)
#else
strbuf_sprintf(sb, s, va_alist)
	STRBUF *sb;
	char *s;
	va_dcl
#endif
{
	va_list ap;

#ifdef HAVE_STDARG_H
	va_start(ap, s);
#else
	va_start(ap);
#endif
	if (sb->alloc_failed)
		return;
	for (; *s; s++) {
		/*
		 * Put the before part of '%'.
		 */
		{
			const char *p;
			for (p = s; *p && *p != '%'; p++)
				;
			if (p > s) {
				strbuf_nputs(sb, s, p - s);
				s = p;
			}
		}
		if (*s == '\0')
			break;
		if (*s == '%') {
			int c = *++s;
			/*
			 * '%%' means '%'.
			 */
			if (c == '%') {
				strbuf_putc(sb, c);
			}
			/*
			 * If the optional number is specified then
			 * we forward the job to snprintf(3).
			 * o %<number>d
			 * o %<number>s
			 */
			else if (isdigit(c)) {
				char format[32], buf[1024];
				int i = 0;

				format[i++] = '%';
				while (isdigit(*s))
					format[i++] = *s++;
				format[i++] = c = *s;
				format[i] = '\0';
				if (c == 'd' || c == 'x')
					snprintf(buf, sizeof(buf), format, va_arg(ap, int));
				else if (c == 's')
					snprintf(buf, sizeof(buf), format, va_arg(ap, char *));
				else
					die("Unsupported control character '%c'.", c);
				strbuf_puts(sb, buf);
			} else if (c == 's') {
				strbuf_puts(sb, va_arg(ap, char *));
			} else if (c == 'd') {
				strbuf_putn(sb, va_arg(ap, int));
			} else {
				die("Unsupported control character '%c'.", c);
			}
		}
	}
	va_end(ap);
}
/*
 * strbuf_close: close string buffer.
 *
 *	i)	sb	STRBUF structure
 */
void
strbuf_close(sb)
	STRBUF	*sb;
{
#ifdef STRBUF_LINK
	sb->prev->next = sb->next;
	sb->next->prev = sb->prev;
#endif
	if (sb->name)
		(void)free(sb->name);
	(void)free(sb->sbuf);
	(void)free(sb);
}
/*
 * Temporary string buffer for general purpose.
 *
 * Usage:
 *
 *	STRBUF *sbt = strbuf_open_tempbuf();
 *	....
 *	strbuf_puts(sbtemp, "xxx");
 *	...
 *	strbuf_release_tempbuf(sbt);
 *
 */
int used = 0;

STRBUF *
strbuf_open_tempbuf()
{
	STATIC_STRBUF(sb);
	if (used)
		die("Internal error: temporary string buffer is already used.");
	used = 1;
	strbuf_clear(sb);
	return sb;
}
void
strbuf_release_tempbuf(sb)
	STRBUF *sb;
{
	used = 0;
}
#ifdef STRBUF_LINK
/*
 * strbuf_setname: set name to specified string buffer.
 *
 *	i)	sb	STRBUF structure
 *	i)	name	name
 */
void
strbuf_setname(sb, name)
	STRBUF  *sb;
	char	*name;
{
	char *p = strdup(name);
	if (p == NULL) {
		(*strbuf_alloc_failed_handler)();
		sb->alloc_failed = 1;
		return;
	}
	if (sb->name)
		(void)free(sb->name);
	sb->name = p;
}
/*
 * strbuf_getbuf: get named buffer
 *
 *	i)	name	name of buffer
 *	r)		STRBUF structure
 */
STRBUF *
strbuf_getbuf(name)
	char	*name;
{
	STRBUF *sb;

	for (sb = top.next; sb && sb != &top; sb = sb->next)
		if (sb->name && !strcmp(name, sb->name))
			return sb;
	return NULL;
}
/*
 * strbuf_closeall: close all string buffer.
 */
void
strbuf_closeall()
{
	while (top.next != &top)
		strbuf_close(top.next);
	top.next = top.prev = NULL;
}
#endif /* STRBUF_LINK */
