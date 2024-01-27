/*
 * SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define MAX_TOKEN 64
#define MAX_STRING 2048

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bootstrappable.h>

struct Token {
	char *val;
	struct Token *next;
};
typedef struct Token Token;

#define TYPE_BUILD 1
#define TYPE_IMPROVE 2
#define TYPE_DEFINE 3
#define TYPE_JUMP 4
#define TYPE_MAINT 5

struct Directive {
	Token *tok;
	struct Directive *next;
	int type;
	char *arg; /* The primary argument */
};
typedef struct Directive Directive;

/* Tokenizer. */

/* Skip over a comment. */
char consume_comment(FILE *in) {
	/* Discard the rest of the line. */
	char c = fgetc(in);
	while (c != -1 && c != '\n')
		c = fgetc(in);
	return c;
}

char consume_line(FILE *in, Directive *directive) {
	char c = fgetc(in);

	/* Short-circuit if whole line is comment or blank line. */
	if (c == '#') {
		c = consume_comment(in);
		return c;
	} else if (c == '\n' || c == -1) {
		return c;
	}

	/* Ok, we will have something to put here. */
	directive->next = calloc(1, sizeof(Directive));
	directive = directive->next;

	Token *head = calloc(1, sizeof(Token));
	Token *cur = head;
	char *out;
	int i = 0;
	while (c != -1 && c != '\n') {
		/* Initialize next token. */
		cur->next = calloc(1, sizeof(Token));
		cur = cur->next;
		cur->val = calloc(MAX_TOKEN, sizeof(char));
		out = cur->val;
		/* Copy line to token until a space (or EOL/EOF) or comment is found. */
		while (c != -1 && c != '\n' && c != ' ' && c != '#') {
			out[0] = c;
			out += 1;
			c = fgetc(in);
		}
		/* Go to start of next token. */
		if (c == ' ') {
			c = fgetc(in);
		}
		/* Handle comment. */
		if (c == '#') {
			c = consume_comment(in);
		}
	}

	/* Add information to directive. */
	directive->tok = head->next;

	return c;
}

Directive *tokenizer(FILE *in) {
	Directive *head = calloc(1, sizeof(Directive));
	Directive *cur = head;

	char c;
	while (c != -1) {
		/*
		 * Note that consume_line fills cur->next, not cur.
		 * This avoids having an empty last Directive.
		 */
		c = consume_line(in, cur);
		if (cur->next != NULL) {
			cur = cur->next;
		}
	}
	return head->next;
}

/* Config variables. */

struct Variable {
	char *name;
	char *val;
	struct Variable *next;
};
typedef struct Variable Variable;

Variable *variables;

Variable *load_config() {
	FILE *config = fopen("/steps/bootstrap.cfg", "r");
	/* File does not exist check. */
	if (config == NULL) {
		return NULL;
	}

	char *line = calloc(MAX_STRING, sizeof(char));
	Variable *head = calloc(1, sizeof(Variable));
	Variable *cur = head;
	/* For each line... */
	char *equals;
	while (fgets(line, MAX_STRING, config) != 0) {
		/* Weird M2-Planet behaviour. */
		if (*line == 0) {
			break;
		}
		cur->next = calloc(1, sizeof(Variable));
		cur = cur->next;
		/* Split on the equals. First half is name, second half is value. */
		equals = strchr(line, '=');
		if (equals == 0) {
			fputs("bootstrap.cfg should have the format var=val on each line.", stderr);
			exit(1);
		}
		cur->name = calloc(equals - line + 1, sizeof(char));
		strncpy(cur->name, line, equals - line);
		equals += 1;
		cur->val = calloc(strlen(equals), sizeof(char));
		strncpy(cur->val, equals, strlen(equals) - 1);
		line = calloc(MAX_STRING, sizeof(char));
	}
	variables = head->next;
	fclose(config);
}

void output_config(FILE *out) {
	Variable *variable;
	for (variable = variables; variable != NULL; variable = variable->next) {
		fputs(variable->name, out);
		fputs("=", out);
		fputs(variable->val, out);
		fputs("\n", out);
	}
}

char *get_var(char *name) {
	/* Search through existing variables. */
	Variable *var;
	Variable *last = NULL;
	for (var = variables; var != NULL; var = var->next) {
		if (strcmp(name, var->name) == 0) {
			return var->val;
		}
		last = var;
	}

	/* If the variable is unset, prompt the user. */
	if (variables == NULL) {
		variables = calloc(1, sizeof(Variable));
		var = variables;
	} else {
		last->next = calloc(1, sizeof(Variable));
		var = last->next;
	}
	var->name = calloc(strlen(name) + 1, sizeof(char));
	strcpy(var->name, name);
	var->val = calloc(MAX_STRING, sizeof(char));
	fputs("You have not set a value for ", stdout);
	fputs(name, stdout);
	fputs(" in bootstrap.cfg. Please set it now:\n", stdout);
	while (fgets(var->val, MAX_STRING, stdin) == 0 || var->val[0] == '\n') {
		fputs("Error inputting, try again:\n", stdout);
	}
	if (var->val[0] == 0) {
		fputs("You put in an EOF!\n", stderr);
		exit(1);
	}
	/* Trim the newline. */
	var->val[strlen(var->val)] = 0;
	return var->val;
}

/* Recursive descent interpreter. */

Token *fill(Token *tok, Directive *directive, int type) {
	directive->type = type;
	directive->arg = tok->val;
	return tok->next;
}

Token *logic(Token *tok, char **val) {
	/* logic = "("
	 *         (name |
	 *         (name "==" value) |
         *         (name "!=" value) |
	 *         (logic "||" logic) |
	 *         (logic "&&" logic))
	 *         ")"
	 */

	char *lhs = tok->val;
	char *rhs;
	tok = tok->next;
	if (strcmp(tok->val, ")") == 0) {
		/* Case where it's just a constant. */
		*val = lhs;
		return tok;
	} else if (strcmp(tok->val, "==") == 0) {
		/* Case for equality. */
		rhs = tok->next->val;
		tok = tok->next->next;
		if (strcmp(get_var(lhs), rhs) == 0) {
			lhs = "True";
		} else {
			lhs = "False";
		}
	} else if (strcmp(tok->val, "!=") == 0) {
                /* Case for inequality. */
                rhs = tok->next->val;
                tok = tok->next->next;
                if (strcmp(get_var(lhs), rhs) == 0) {
                        lhs = "False";
                } else {
                        lhs = "True";
                }
        } else {
		fputs("Expected == or != after ", stderr);
		fputs(lhs, stderr);
		fputs(" in logic\n", stderr);
		exit(1);
	}

	if (strcmp(tok->val, ")") == 0) {
		*val = lhs;
		return tok;
	} else if (strcmp(tok->val, "||") == 0) {
		/* OR */
		tok = logic(tok->next, &rhs);
		if (strcmp(lhs, "True") == 0 || strcmp(rhs, "True") == 0) {
			lhs = "True";
		} else {
			lhs = "False";
		}
	} else if (strcmp(tok->val, "&&") == 0) {
		/* AND */
		tok = logic(tok->next, &rhs);
		if (strcmp(lhs, "True") == 0 && strcmp(rhs, "True") == 0) {
			lhs = "True";
		} else {
			lhs = "False";
		}
	} else {
		fputs("Expected || or && in logic\n", stderr);
		exit(1);
	}

	*val = lhs;
	return tok;
}

Token *primary_logic(Token *tok, char **val) {
	/* Starting ( */
	if (strcmp(tok->val, "(") != 0) {
		fputs("Expected logic to begin with (\n", stderr);
		exit(1);
	}
	tok = tok->next;

	tok = logic(tok, val);

	if (strcmp(tok->val, ")") != 0) {
		fputs("Expected logic to end with )\n", stderr);
		exit(1);
	}

	return tok;
}

int eval_predicate(Token *tok) {
	char *result;
	tok = primary_logic(tok, &result);
	return strcmp(result, "True") == 0;
}

Token *define(Token *tok, Directive *directive) {
	/* define = name "=" (logic | constant) */
	char *name = tok->val;
	tok = tok->next;
	if (strcmp(tok->val, "=") != 0) {
		fputs("define of ", stderr);
		fputs(name, stderr);
		fputs(" has a missing equals\n", stderr);
		exit(1);
	}
	tok = tok->next;

	char *val = calloc(MAX_STRING, sizeof(char));
	if (strcmp(tok->val, "(") == 0) {
		/* It is a logic. */
		tok = primary_logic(tok, &val);
	} else {
		/* It is a constant. */
		strcpy(val, tok->val);
	}

	/* Check for predicate. */
	tok = tok->next;
	if (tok != NULL) {
		if (!eval_predicate(tok)) {
			/* Nothing more to do. */
			return tok;
		}
	}

	/* Update existing variable, or else, add to the end of variables. */
	/* Special case: empty variables. */
	if (variables == NULL) {
		variables = calloc(1, sizeof(Variable));
		variables->name = name;
		variables->val = val;
	}

	Variable *var;
	for (var = variables; var->next != NULL; var = var->next) {
		if (strcmp(var->next->name, name) == 0) {
			var->next->val = val;
			break;
		}
	}
	if (var->next == NULL) {
		/* We did not update an existing variable. */
		var->next = calloc(1, sizeof(Variable));
		var->next->name = name;
		var->next->val = val;
	}

	return tok;
}

int interpret(Directive *directive) {
	/* directive = (build | improve | define | jump | maint) predicate? */
	Token *tok = directive->tok;
	if (strcmp(tok->val, "build:") == 0) {
		tok = fill(tok->next, directive, TYPE_BUILD);
	} else if (strcmp(tok->val, "improve:") == 0) {
		tok = fill(tok->next, directive, TYPE_IMPROVE);
	} else if (strcmp(tok->val, "jump:") == 0) {
		tok = fill(tok->next, directive, TYPE_JUMP);
	} else if (strcmp(tok->val, "maint:") == 0) {
		tok = fill(tok->next, directive, TYPE_MAINT);
	} else if (strcmp(tok->val, "define:") == 0) {
		tok = define(tok->next, directive);
		return 1; /* There is no codegen for a define. */
	}

	if (tok != NULL) {
		return !eval_predicate(tok);
	}
	return 0;
}

Directive *interpreter(Directive *directives) {
	Directive *directive;
	Directive *last = NULL;
	for (directive = directives; directive != NULL; directive = directive->next) {
		if (interpret(directive)) {
			/* This means this directive needs to be removed from the linked list. */
			if (last == NULL) {
				/* First directive. */
				directives = directive->next;
			} else {
				last->next = directive->next;
			}
		} else {
			last = directive;
		}
	}
	return directives;
}

void add_to_fiwix_filelist(char *filename) {
	/* Add the filename to fiwix-file-list.txt */
	FILE *fiwix_list = fopen("/steps/lwext4-1.0.0-lb1/files/fiwix-file-list.txt", "r");
	fseek(fiwix_list, 0, SEEK_END);
	long size = ftell(fiwix_list);
	char *contents = calloc(size, sizeof(char));
	fseek(fiwix_list, 0, SEEK_SET);
	fread(contents, 1, size, fiwix_list);
	fclose(fiwix_list);
	fiwix_list = fopen("/steps/lwext4-1.0.0-lb1/files/fiwix-file-list.txt", "w");
	fwrite(contents, 1, size, fiwix_list);
	fputs(filename, fiwix_list);
	fputc('\n', fiwix_list);
	fclose(fiwix_list);
}

/* Script generator. */
FILE *start_script(int id, int bash_build) {
	/* Create the file /steps/$id.sh */
	char *filename = calloc(MAX_STRING, sizeof(char));
	strcpy(filename, "/steps/");
	strcat(filename, int2str(id, 10, 0));
	strcat(filename, ".sh");
	add_to_fiwix_filelist(filename);

	FILE *out = fopen(filename, "w");
	if (out == NULL) {
		fputs("Error opening output file ", stderr);
		fputs(filename, stderr);
		fputs("\n", stderr);
		exit(1);
	}

	if (bash_build) {
		fputs("#!/bin/bash\n", out);
		if (strcmp(get_var("INTERACTIVE"), "True") == 0) {
			if (bash_build != 1) {
				fputs("set -E\ntrap 'env PS1=\"[TRAP] \\w # \" bash -i' ERR\n", out);
			} else {
				fputs("set -E\ntrap 'bash -c '\"'\"'while true; do printf \""
				"[TRAP - use Ctrl+D] $(pwd) # \"; $(cat); done'\"'\"'' ERR\n",
				out);
			}
		} else {
			fputs("set -e\n", out);
		}
		fputs("cd /steps\n", out);
		fputs(". ./bootstrap.cfg\n", out);
		fputs(". ./env\n", out);
		fputs(". ./helpers.sh\n", out);
	} else {
		fputs("set -ex\n", out);
		fputs("cd /steps\n", out);
		output_config(out);
		FILE *env = fopen("/steps/env", "r");
		char *line = calloc(MAX_STRING, sizeof(char));
		while (fgets(line, MAX_STRING, env) != 0) {
			/* Weird M2-Planet behaviour. */
			if (*line == 0) {
				break;
			}
			fputs(line, out);
			line = calloc(MAX_STRING, sizeof(char));
		}
		fclose(env);
	}

	return out;
}

void output_call_script(FILE *out, char *type, char *name, int bash_build, int source) {
	if (bash_build) {
		if (source) {
			fputs(". ", out);
		} else {
			fputs("bash ", out);
		}
	} else {
		fputs("kaem --file ", out);
	}
	fputs("/steps/", out);
	if (strlen(type) != 0) {
		fputs(type, out);
		fputs("/", out);
	}
	fputs(name, out);
	fputs(".sh\n", out);
}

void output_build(FILE *out, Directive *directive, int pass_no, int bash_build) {
	if (bash_build) {
		fputs("build ", out);
		fputs(directive->arg, out);
		fputs(" pass", out);
		fputs(int2str(pass_no, 10, 0), out);
		fputs(".sh\n", out);
	} else {
		fputs("pkg=", out);
		fputs(directive->arg, out);
		fputs("\n", out);
		fputs("cd ${pkg}\n", out);
		fputs("kaem --file pass", out);
		fputs(int2str(pass_no, 10, 0), out);
		fputs(".kaem\n", out);
		fputs("cd ..\n", out);
	}
}

void generate_preseed_jump(int id) {
	FILE *out = fopen("/preseed-jump.kaem", "w");
	fputs("set -ex\n", out);
	fputs("PATH=/usr/bin\n", out);
	fputs("bash /steps/", out);
	fputs(int2str(id, 10, 0), out);
	fputs(".sh\n", out);
	fclose(out);
}

void generate(Directive *directives) {
	/*
	 * We are separating the stages given in the mainfest into a bunch of
	 * smaller scripts. The following conditions call for the creation of
	 * a new script:
	 * - a jump
	 * - build of bash
	 */

	int counter = 0;

	/* Initially, we use kaem, not bash. */
	int bash_build = 0;

	FILE *out = start_script(counter, bash_build);
	counter += 1;

	Directive *directive;
	Directive *past;
	char *filename;
	int pass_no;
	for (directive = directives; directive != NULL; directive = directive->next) {
		if (directive->type == TYPE_BUILD) {
			/* Get what pass number this is. */
			pass_no = 1;
			for (past = directives; past != directive; past = past->next) {
				if (strcmp(past->arg, directive->arg) == 0) {
					pass_no += 1;
				}
			}
			output_build(out, directive, pass_no, bash_build);
			if (strncmp(directive->arg, "bash-", 5) == 0) {
				if (!bash_build) {
					/*
					 * We are transitioning from bash to kaem, the point at which "early
					 * preseed" occurs. So generate the preseed jump script at this point.
					 */
					generate_preseed_jump(counter);
				}
				bash_build += 1;
				/* Create call to new script. */
				output_call_script(out, "", int2str(counter, 10, 0), bash_build, 0);
				fclose(out);
				out = start_script(counter, bash_build);
				counter += 1;
			}
		} else if (directive->type == TYPE_IMPROVE) {
			output_call_script(out, "improve", directive->arg, bash_build, 1);
		} else if (directive->type == TYPE_JUMP) {
			/*
			 * Create /init to call new script.
			 * We actually do this by creating /init.X for some number X, and then
			 * moving that to /init at the appropriate time.
			 */
			filename = calloc(MAX_STRING, sizeof(char));
			if (bash_build) {
				fputs("mv /init /init.bak\n", out);
				/* Move new init to /init. */
				strcpy(filename, "/init.");
				strcat(filename, int2str(counter, 10, 0));
				fputs("cp ", out);
				fputs(filename, out);
				fputs(" /init\n", out);
				fputs("chmod 755 /init\n", out);
			} else {
				strcpy(filename, "/kaem.run.");
				strcat(filename, int2str(counter, 10, 0));
				fputs("cp ", out);
				fputs(filename, out);
				fputs(" /kaem.run\n", out);
				fputs("cp /usr/bin/kaem /init\n", out);
				fputs("chmod 755 /init\n", out);
			}

			output_call_script(out, "jump", directive->arg, bash_build, 1);
			fclose(out);

			/*
			 * This cannot go before here as builder-hex0 does not like having
			 * multiple files open at once!
			 */
			add_to_fiwix_filelist(filename);

			if (bash_build) {
				out = fopen(filename, "w");
				if (out == NULL) {
					fputs("Error opening /init\n", stderr);
					exit(1);
				}
				fputs("#!/bin/bash\n", out);
			} else {
				out = fopen(filename, "w");
				if (out == NULL) {
					fputs("Error opening /kaem.run\n", stderr);
					exit(1);
				}
				fputs("set -ex\n", out);
			}
			output_call_script(out, "", int2str(counter, 10, 0), bash_build, 0);
			fclose(out);
			out = start_script(counter, bash_build);
			counter += 1;
		} else if (directive->type == TYPE_MAINT) {
			output_call_script(out, "maint", directive->arg, bash_build, 1);
		}
	}
	fclose(out);
}

void main(int argc, char **argv) {
	if (argc != 2) {
		fputs("Usage: script-generator <script>\n", stderr);
		exit(1);
	}

	FILE *in = fopen(argv[1], "r");
	if (in == NULL) {
		fputs("Error opening input file\n", stderr);
		exit(1);
	}
	Directive *directives = tokenizer(in);
	fclose(in);
	load_config();
	directives = interpreter(directives);
	generate(directives);
	FILE *config = fopen("/steps/bootstrap.cfg", "w");
	output_config(config);
	fclose(config);
}
