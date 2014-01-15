#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>

#include "internal.h"
#include "logging.h"
#include "die.h"
#include "linetable.h"
#include "strbuf.h"

#include "erl_interface.h"
#include "ei.h"

/* #define if erl is invoked manually (for debugging purpose) */
#undef DONT_INVOKE_ERL

#define NODE "global_parser"
#define SERVER "parser"
#define COOKIE "test"

#undef PUT
#define PUT(type, tag, lno, line_image) do {    			\
	char *nl = strchr(line_image, '\n');				\
	if (nl != NULL)							\
		*nl = '\0';						\
	param->put(type, tag, lno, param->file, line_image, param->arg);\
	if (nl != NULL)							\
		*nl = '\n';						\
} while (0)

static ei_cnode ec;

static int connect_parser(ei_cnode* ec);

int invoke_erl()
{
#ifndef DONT_INVOKE_ERL
  static const char* command_template
    = ERL " -pa " PARSE_BEAM_PATH " -noshell -setcookie " COOKIE " -sname " NODE;
  STRBUF *command = strbuf_open(256);
  strbuf_puts(command, command_template);
  strbuf_sprintf(command, "%d &", getpid());
  logging_printf("invoke_erl: command = %s\n", strbuf_value(command));
  if (system(strbuf_value(command)) == -1) {
    die("Failed to invoke the erlang parser.");
  }
  strbuf_close(command);
  /* TODO: find a decent way to wait until the peer comes up */
  sleep(1);
#endif
  int fd;
  fd = connect_parser(&ec);
  if (fd < 0) {
    die("Failed to connect to the erlang parser.");
  }
  return fd;
}

void kill_erl(int fd)
{
#ifndef DONT_INVOKE_ERL
  int index = 0, arity, version;
  ei_x_buff args, result;

  ei_x_new_with_version(&result);
  ei_x_new(&args);

  ei_x_encode_empty_list(&args);
  if (ei_rpc(&ec, fd, "erlang", "halt",
  	     args.buff, args.index, &result) < 0) {
    /* do not care the result */
  }
#endif
}

static int connect_parser(ei_cnode* ec)
{
  erl_init(NULL, 0);
  erl_connect_init(1, COOKIE, 0);
  static short creation = 0;
  if (ei_connect_init(ec, "c1", COOKIE, creation++) < 0) {
    die("Failed to connect to the erlang parser.");
  }
  logging_printf("erl_thiscookie: %s\n", erl_thiscookie());
  logging_printf("erl_thisnodename: %s\n", erl_thisnodename());
  logging_printf("erl_thishostname: %s\n", erl_thishostname());
  logging_printf("erl_thisalivename: %s\n", erl_thisalivename());

  STRBUF *node_name = strbuf_open(32);
#ifndef DONT_INVOKE_ERL
  strbuf_sprintf(node_name, NODE "%d@", getpid());
#else
  strbuf_puts(node_name, NODE "@");
#endif
  strbuf_puts(node_name, erl_thishostname());
  logging_printf("connecting to %s\n", strbuf_value(node_name));
  int fd = ei_connect(ec, strbuf_value(node_name));
  if (fd < 0) {
    die("Failed to connect to node %s (%d).", strbuf_value(node_name), fd);
  }
  strbuf_close(node_name);
  return fd;
}

void erlang(const struct parser_param *param)
{
  if (linetable_open(param->file) == -1) {
    die("Cannot open '%s'.", param->file);
  }

  char* abs_path = realpath(param->file, NULL);
  logging_printf("parsing %s\n", abs_path);
  
  int fd = param->fd;
  int index = 0, arity, version;
  ei_x_buff args, result;

  //  ei_x_new(&result);
  ei_x_new_with_version(&result);
  ei_x_new(&args);

  ei_x_encode_list_header(&args, 1);
  ei_x_encode_string(&args, abs_path);
  ei_x_encode_empty_list(&args);
  if (ei_rpc(&ec, fd, "parse", "get_symbols",
  	     args.buff, args.index, &result) < 0) {
    erl_err_quit("ei_rpc failed");
  }
  if (ei_decode_list_header(result.buff, &index, &arity) < 0) {
    erl_err_quit("ei_decode for list header failed");
  }

  char record_name[MAXATOMLEN];
  char atom[MAXATOMLEN];
  char name[1024];
  long line_number;
  for (int i = 0; i < arity; i++) {
    int tuple_arity;
    ei_decode_tuple_header(result.buff, &index, &tuple_arity);
    ei_decode_atom(result.buff, &index, record_name);
    ei_decode_atom(result.buff, &index, atom);
    ei_decode_string(result.buff, &index, name);
    ei_decode_long(result.buff, &index, &line_number);
    logging_printf("%s %s %ld %s\n", atom, name, line_number, linetable_get(line_number, 0));
    if (strncmp(atom, "refsym", 6) == 0) {
      PUT(PARSER_REF_SYM, name, line_number, linetable_get(line_number, 0));
    } else if (strncmp(atom, "def", 3) == 0) {
      PUT(PARSER_DEF, name, line_number, linetable_get(line_number, 0));
    } else {
      die("invalid symbol type '%s' in file %s.", atom, abs_path);
    }
  }

  free(abs_path);
  ei_x_free(&args);
  ei_x_free(&result);
  linetable_close();
}

