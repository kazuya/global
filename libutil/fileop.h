/*
 * Copyright (c) 2006
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.
 */
#ifndef _FILEOP_H
#define _FILEOP_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>

#include "gparam.h"

#define FILEOP_INPUT	1
#define FILEOP_OUTPUT	2
#define FILEOP_COMPRESS	4

typedef struct {
	int type;
	FILE *fp;
	char command[MAXFILLEN+1];
	char path[MAXPATHLEN+1];
} FILEOP;

FILEOP *open_input_file(const char *);
FILEOP *open_output_file(const char *, int);
FILE *get_descripter(FILEOP *);
void close_file(FILEOP *);

#endif /* ! _FILEOP_H */