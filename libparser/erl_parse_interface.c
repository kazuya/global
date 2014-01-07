#include <stdio.h>

#include "internal.h"
#include "die.h"
#include "linetable.h"
#include "strbuf.h"

#include "erl_interface.h"
#include "ei.h"

#define NODE "global_parser@"
#define SERVER "parser"

#undef PUT
#define PUT(type, tag, lno, line_image) do {    			\
	char *nl = strchr(line_image, '\n');				\
	if (nl != NULL)							\
		*nl = '\0';						\
	param->put(type, tag, lno, param->file, line_image, param->arg);\
	if (nl != NULL)							\
		*nl = '\n';						\
} while (0)

static int fd = -1;
static ei_cnode ec;

static int connect_parser(ei_cnode* ec)
{
  erl_init(NULL, 0);
  erl_connect_init(1, "test", 0);
  static short creation = 0;
  if (ei_connect_init(ec, "c1", "test", creation++) < 0) {
    die("Failed to connect to the erlang parser.");
  }
  printf("erl_thiscookie: %s\n", erl_thiscookie());
  printf("erl_thisnodename: %s\n", erl_thisnodename());
  printf("erl_thishostname: %s\n", erl_thishostname());
  printf("erl_thisalivename: %s\n", erl_thisalivename());

  STRBUF *node_name = strbuf_open(32);
  strbuf_puts(node_name, NODE);
  strbuf_puts(node_name, erl_thishostname());
  printf("connecting to %s\n", strbuf_value(node_name));
  fd = ei_connect(ec, strbuf_value(node_name));
  if (fd < 0) {
    die("Failed to connect to node %s (%d).", strbuf_value(node_name), fd);
  }
  strbuf_close(node_name);
  printf("connected\n");
  return fd;
}

void erlang(const struct parser_param *param)
{
  if (linetable_open(param->file) == -1) {
    die("Cannot open '%s'.", param->file);
  }

  if (fd < 0) {
    fd = connect_parser(&ec);
  }
  int index = 0, arity, version;
  ei_x_buff args, result;

  //  ei_x_new(&result);
  ei_x_new_with_version(&result);
  ei_x_new(&args);

  ei_x_encode_list_header(&args, 1);
  ei_x_encode_string(&args, param->file);
  ei_x_encode_empty_list(&args);
  if (ei_rpc(&ec, fd, "parse", "get_symbols",
  	     args.buff, args.index, &result) < 0) {
    erl_err_quit("ei_rpc failed");
  }
  if (ei_decode_list_header(result.buff, &index, &arity) < 0) {
    erl_err_quit("ei_decode for list header failed");
  }

  char atom[MAXATOMLEN];
  char name[1024];
  long line_number;
  char line[1024];
  for (int i = 0; i < arity; i++) {
    int tuple_arity;
    ei_decode_tuple_header(result.buff, &index, &tuple_arity);
    ei_decode_atom(result.buff, &index, atom);
    ei_decode_string(result.buff, &index, name);
    ei_decode_long(result.buff, &index, &line_number);
    ei_decode_string(result.buff, &index, line);
    printf("%s %s %ld %s\n", atom, name, line_number, line);
    if (strncmp(atom, "def", 3)) {
      PUT(PARSER_REF_SYM, name, line_number, line);
    } else {
      PUT(PARSER_DEF, name, line_number, line);
    }
  }

  ei_x_free(&args);
  ei_x_free(&result);
  linetable_close();
}

