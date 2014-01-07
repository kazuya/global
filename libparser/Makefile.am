## Process this file with automake to create Makefile.in
#
# Copyright (c) 2010 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
noinst_LIBRARIES = libgloparser.a

noinst_HEADERS = parser.h internal.h asm_parse.h \
		c_res.h cpp_res.h java_res.h php_res.h asm_res.h asm_parse.h
#		erlang_res.h

libgloparser_a_SOURCES = parser.c C.c Cpp.c asm_parse.c asm_scan.c java.c php.c erl_parse_interface.c

INCLUDES = @INCLUDES@ -I/usr/local/otp/lib/erlang/lib/erl_interface-3.7.14/include

# ld -lc -L/usr/local/otp/lib/erlang/lib/erl_interface-3.7.14/lib /usr/lib/crt1.o ei_test.o -lerl_interface -lei  -o ei_test

libgloparser_a_DEPENDENCIES = $(libgloparser_a_LIBADD)

EXTRA_DIST = reserved.pl HACKING \
		c_res.in c_res.gpf \
		cpp_res.in cpp_res.gpf \
		java_res.in java_res.gpf \
		php_res.in php_res.gpf php.l \
		asm_res.in asm_res.gpf asm_parse.y asm_scan.l
#		erlang_res.in erlang_res.gpf
