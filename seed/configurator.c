/*
 * SPDX-FileCopyrightText: 2024 Samuel Tyler <samuel@samuelt.me>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define MAX_STRING 2048
#define MAX_SHORT 512
#define MAX_ID 128
#define MAX_VAR 128

#include <bootstrappable.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#define TRUE 1
#define FALSE 0

#define KIND_NONE 0
#define KIND_MENU 1
#define KIND_OPTION 2

#define TYPE_NONE 0
#define TYPE_BOOL 1
#define TYPE_SIZE 2
#define TYPE_STRING 3
#define TYPE_INT 4

struct Entry {
	int kind; // either menu or option
	char *env_var; // name of the environment variable this option is stored in
	char *id; // the id of the configuration item
	char *short_desc; // short description of the config
	char *full_desc; // extended description of the config

	int type; // the type of the configuration option
	char *validation; // any validation rules
	char *val;
	char *default_val;

	struct Entry *children; // submenus
	struct Entry *parent;
	struct Entry *next;
};
typedef struct Entry Entry;

Entry *find_entry(Entry *head, char *id) {
	char *component = strchr(id, '/');
	if (component == NULL) {
		component = id + strlen(id);
	}

	Entry *current;
	Entry *final;
	int len;
	while (1) {
		len = component - id;

		current = head;
		while (current != NULL) {
			/* ensure that the id isn't just a substring of the component but actually is the component */
			if (strlen(current->id) == len && strncmp(id, current->id, len) == 0) {
				/* Found it! */
				final = current;
				head = current->children;
				break;
			}
			current = current->next;
		}
		if (current == NULL) {
			/* Did not find it */
			return NULL;
		}

		if (component[0] == '\0') {
			break;
		}

		component += 1;
		component = strchr(component, '/');
		if (component == NULL) {
			component = id + strlen(id);
		}
	}

	return final;
}

Entry *get_parent(Entry *head, char *id) {
	char *parent_id = calloc(MAX_ID, sizeof(char));
	strcpy(parent_id, id);
	char *final_slash = strrchr(parent_id, '/');
	final_slash[0] = '\0';
	Entry *ret = find_entry(head, parent_id);
	free(parent_id);
	return ret;
}

char read_string(FILE *f, char *out, int length) {
	int i = 0;
	char c = fgetc(f);
	while (c != ' ' && c != '\n' && c != EOF && i < length - 1) {
		out[i] = c;
		i += 1;
		c = fgetc(f);
	}
	if (i >= length - 1) {
		fputs("String too long!\n", stdout);
		fclose(f);
		exit(1);
	}
	out[i] = '\0';
	return c;
}

int set_val(Entry *entry, char *val) {
	if (entry->type == TYPE_BOOL) {
		if (strcmp(val, "True") != 0 && strcmp(val, "False") != 0) {
			fputs("Invalid input: ", stdout);
			fputs(val, stdout);
			fputs(" is not a boolean value\n", stdout);
			return 1;
		}
	} else if (entry->type == TYPE_INT) {
		int intval = strtoint(val);
		if (intval == 0 && strcmp(val, "0") != 0) {
			fputs("Invalid input: ", stdout);
			fputs(val, stdout);
			fputs(" is not an integer\n", stdout);
			return 1;
		}
	} else if (entry->type == TYPE_SIZE) {
		/* We should have either a K, M, G, T, or no letter, at the end of the size. */
		char c = val[strlen(val) - 1];
		if (!(('0' <= c && c <= '9') || c == 'K' || c == 'M' || c == 'G' || c == 'T')) {
			fputs("Invalid input: ", stdout);
			fputc(c, stdout);
			fputs(" is not a valid suffix for a size\n", stdout);
			return 1;
		}
		/* Check it is an integer */
		char *final_char = val + strlen(val) - 1;
		if ('A' <= final_char[0] && final_char[0] <= 'Z') {
			final_char[0] = '\0';
		}
		int intval = strtoint(val);
		if (intval == 0 && strcmp(val, "0") != 0) {
			fputs("Invalid input: ", stdout);
			fputs(val, stdout);
			fputs(" is not a valid size\n", stdout);
			return 1;
		}
		final_char[0] = c;
	} else if (entry->type == TYPE_STRING) {
		/* Validation rules. */
		char *validation = entry->validation;
		char *next;
		int found = FALSE;
		while (validation != NULL) {
			if (validation[0] == '\0') {
				found = TRUE;
				break;
			}
			next = strchr(validation, '|');
			if (next == NULL) {
				if (strcmp(validation, val) == 0) {
					found = TRUE;
				}
				break;
			} else {
				if (strncmp(validation, val, next - validation) == 0) {
					found = TRUE;
				}
			}
			validation = next + 1;
		}
		if (found == FALSE) {
			fputs("Invalid input: ", stdout);
			fputs(val, stdout);
			fputs(" does not match the validation rules ", stdout);
			fputs(entry->validation, stdout);
			fputc('\n', stdout);
			return 1;
		}
	}

	entry->val = calloc(strlen(val) + 1, sizeof(char));
	strcpy(entry->val, val);
	return 0;
}

void read_entry(char c, FILE *conf, Entry *head) {
	Entry *new = calloc(1, sizeof(Entry));

	/* Read the kind */
	if (c == 'm') {
		new->kind = KIND_MENU;
	} else if (c == 'o') {
		new->kind = KIND_OPTION;
	} else {
		fputs("Invalid entry: kind ", stdout);
		fputc(c, stdout);
		fputc('\n', stdout);
		fclose(conf);
		exit(1);
	}
	fgetc(conf);

	/* Read the id */
	new->id = calloc(MAX_ID, sizeof(char));
	c = read_string(conf, new->id, MAX_ID);
	if (c != ' ') {
		fputs("Invalid entry: no variable\n", stdout);
		fclose(conf);
		exit(1);
	}

	/* Read the environment variable */
	new->env_var = calloc(MAX_VAR, sizeof(char));
	c = read_string(conf, new->env_var, MAX_VAR);
	if (c != ' ') {
		fputs("Invalid entry: no data type\n", stdout);
		fclose(conf);
		exit(1);
	}
	if (strcmp(new->env_var, "_") == 0) {
		free(new->env_var);
		new->env_var = NULL;
	}

	/* Read the data type */
	char *data_type = calloc(MAX_ID, sizeof(char));
	read_string(conf, data_type, MAX_ID);
	if (c != ' ') {
		fputs("Invalid entry: no default value\n", stdout);
		fclose(conf);
		exit(1);
	}
	if (strcmp(data_type, "_") == 0) {
		new->type = TYPE_NONE;
	} else if (strcmp(data_type, "bool") == 0) {
		new->type = TYPE_BOOL;
	} else if (strcmp(data_type, "size") == 0) {
		new->type = TYPE_SIZE;
	} else if (strcmp(data_type, "int") == 0) {
		new->type = TYPE_INT;
	} else if (data_type[0] == '"') {
		new->type = TYPE_STRING;
		new->validation = data_type + 1;
		char *closing_quote = strrchr(data_type, '"');
		closing_quote[0] = '\0';
	} else {
		fputs("Invalid entry: unknown type: ", stdout);
		fputs(data_type, stdout);
		fputc('\n', stdout);
		fclose(conf);
		exit(1);
	}
	if (new->type != TYPE_STRING) {
		free(data_type);
	}

	/* Read the default value */
	char *default_val = calloc(MAX_STRING, sizeof(char));
	read_string(conf, default_val, MAX_ID);
	if (strcmp(default_val, "_") != 0) {
		set_val(new, default_val);
		new->default_val = default_val;
	} else {
		new->default_val = NULL;
	}

	/* Read the short description */
	new->short_desc = calloc(MAX_SHORT, sizeof(char));
	int i = 0;
	c = fgetc(conf);
	while (c != '\n' && c != EOF) {
		new->short_desc[i] = c;
		c = fgetc(conf);
		i += 1;
	}

	/* Read the long description */
	new->full_desc = calloc(MAX_STRING, sizeof(char));
	i = 0;
	c = fgetc(conf);
	char prev = '\0';
	while (!(c == '\n' && prev == '\n') && c != EOF) {
		new->full_desc[i] = c;
		prev = c;
		c = fgetc(conf);
		i += 1;
	}

	new->children = NULL;
	new->next = NULL;

	Entry *parent = get_parent(head, new->id);
	new->parent = parent;
	if (parent->children == NULL) {
		parent->children = new;
	} else {
		Entry *current = parent->children;
		while (current->next != NULL) {
			current = current->next;
		}
		current->next = new;
	}
}

Entry *read_config(char *filename) {
	FILE *conf = fopen(filename, "r");
	if (conf == NULL) {
		fputs("Unable to open ", stdout);
		fputs(filename, stdout);
		fputc('\n', stdout);
		exit(0);
	}
	char c = fgetc(conf);

	Entry *head = calloc(1, sizeof(Entry));
	head->id = "";
	head->env_var = "";
	head->next = NULL;
	Entry *current = head;

	while (c != EOF) {
		if (c == '#' || c == '\n') {
			/* Skip comments or empty lines. */
			while (c != '\n' && c != EOF) {
				c = fgetc(conf);
			}
		} else {
			read_entry(c, conf, head);
		}
		c = fgetc(conf);
	}

	fclose(conf);

	return head;
}

Entry *get_env_var(Entry *head, char *var) {
	Entry *ret;
	Entry *current;
	for (current = head->children; current != NULL; current = current->next) {
		if (current->env_var != NULL) {
			if (strcmp(current->env_var, var) == 0) {
				return current;
			}
		}
		if (current->children != NULL) {
			ret = get_env_var(current, var);
			if (ret != NULL) {
				return ret;
			}
		}
	}
	return NULL;
}

int set_cfg_varline(Entry *head, char *line) {
	char *var = calloc(strlen(line) + 1, sizeof(char));
	strcpy(var, line);
	char *val = strchr(var, '=');
	val[0] = '\0';
	val += 1;
	char *newline = strchr(val, '\n');
	if (newline != NULL) {
		newline[0] = '\0';
	}
	Entry *entry = get_env_var(head, var);
	if (entry != NULL) {
		int not_ok = set_val(entry, val);
		if (not_ok) {
			fputs("^ Originated from ", stdout);
			fputs(var, stdout);
			fputs("=", stdout);
			fputs(val, stdout);
			fputs("\n", stdout);
		}
	}
	return entry == NULL;
}

char *set_cfg_values(Entry *head, char **envp) {
	int i = 0;

	FILE *cfg = fopen("/steps/bootstrap.cfg", "r");
	if (cfg == NULL) {
		return "";
	}

	char *extra = calloc(MAX_STRING, sizeof(char));
	char *line = calloc(MAX_STRING, sizeof(char));
	while (fgets(line, MAX_STRING, cfg) != NULL) {
		if (set_cfg_varline(head, line)) {
			if (strncmp("CONFIGURATOR=", line, 13) != 0) {
				strcat(extra, line);
			}
		}
		free(line);
		line = calloc(MAX_STRING, sizeof(char));
	}

	fclose(cfg);

	return extra;
}

void write_cfg_list(Entry *head, FILE *cfg) {
	Entry *current;
	for (current = head->children; current != NULL; current = current->next) {
		if (current->kind == KIND_OPTION && current->val != NULL) {
			fputs(current->env_var, cfg);
			fputs("=", cfg);
			fputs(current->val, cfg);
			fputs("\n", cfg);
		}
		if (current->children != NULL) {
			write_cfg_list(current, cfg);
		}
	}
}

void write_cfg_values(Entry *head, char *extra, int configurator_done) {
	FILE *cfg = fopen("/steps/bootstrap.cfg", "w");
	if (cfg == NULL) {
		fputs("Unable to open /steps/bootstrap.cfg", stderr);
		exit(1);
	}
	if (configurator_done == TRUE) {
		fputs("CONFIGURATOR=False\n", cfg);
	}
	fputs(extra, cfg);
	write_cfg_list(head, cfg);
	fclose(cfg);
}

void print_short_desc(char *short_desc) {
	char *post_markers = strrchr(short_desc, ']');
	if (post_markers == NULL) {
		post_markers = short_desc;
	} else {
		post_markers += 1;
		while (post_markers[0] == ' ') {
			post_markers += 1;
		}
	}
	fputs(post_markers, stdout);
}

void print_recursive_desc(Entry *entry) {
	if (entry->parent != NULL) {
		if (strcmp(entry->parent->id, "") != 0) {
			print_recursive_desc(entry->parent);
			fputs("/", stdout);
			print_short_desc(entry->short_desc);
			return;
		}
	}
	print_short_desc(entry->short_desc);
}

int any_unset(Entry *head);

int check_set(Entry *entry, Entry *head) {
	int ret = 0;
	if (entry->kind == KIND_OPTION && entry->val == NULL) {
		fputs("The configuration option ", stdout);
		print_recursive_desc(entry);
		fputs(" is unset\n", stdout);
		ret = 1;
	}
	if (entry->children != NULL) {
		ret |= any_unset(entry);
	}
	return ret;
}

int any_unset(Entry *head) {
	int ret = 0;
	Entry *current;
	for (current = head->children; current != NULL; current = current->next) {
		ret |= check_set(current, head);
	}
	return ret;
}

void print_menu(Entry *menu, int is_toplevel) {
	if (!is_toplevel) {
		fputs("(0) [MENU] Go up\n", stdout);
	}
	int i = 1;
	Entry *current;
	for (current = menu->children; current != NULL; current = current->next) {
		fputs("(", stdout);
		fputs(int2str(i, 10, FALSE), stdout);
		fputs(") ", stdout);
		if (current->kind == KIND_MENU) {
			fputs("[MENU] ", stdout);
		}
		fputs(current->short_desc, stdout);
		fputc('\n', stdout);
		i += 1;
	}
}

Entry *get_nth_option(Entry *menu, int n) {
	int i = 1;
	Entry *current;
	for (current = menu->children; current != NULL && i < n; current = current->next) {
		i += 1;
	}
	if (current == NULL) {
		fputs("There is no option ", stdout);
		fputs(int2str(n, 10, FALSE), stdout);
		fputs("!\n", stdout);
	}
	return current;
}

void how_to_use(void) {
	fputs(
		"How to navigate around this configuration menu:\n"
		"h or help: at any time, will reprint this help message\n"
		"l or list: shows the current menu options\n"
		"o <num> or open <num>: open a (sub)menu\n"
		"? <num> or describe <num>: provides a more detailed description of an option or menu\n"
		"s <num> <val> or set <num> <val>: set the value of an option\n"
		"g <num> or get <num>: get the value of an option\n"
		"g all or get all: get the value of all options in the menu\n"
		"r <num> or reset <num>: reset the value of an option to the default (if there is one)\n"
		"e or exit: exits the program\n",
	stdout);
}

Entry *extract_num(char **command, Entry *menu) {
	command[0] = strchr(command[0], ' ');
	if (command[0] == NULL) {
		fputs("Expected menu number to operate on!\n", stdout);
	}
	command[0] += 1;
	char *num = command[0];
	char *new = strchr(command[0], ' ');
	if (new == NULL) {
		new = strchr(command[0], '\n');
	}
	command[0] = new;
	command[0][0] = '\0';
	command[0] += 1;
	/* strtoint does not check if it is not a number */
	int i;
	for (i = 0; i < strlen(num); i += 1) {
		if (!('0' <= num[i] && num[i] <= '9')) {
			fputs(num, stdout);
			fputs(" is not a menu number!\n", stdout);
			return NULL;
		}
	}
	int n = strtoint(num);
	return get_nth_option(menu, n);
}

Entry *submenu(char *command, Entry *menu, Entry *head) {
	command = strchr(command, ' ');
	if (strlen(command) < 1) {
		fputs("Expected menu number to operate on!\n", stdout);
	}
	/* 0 is the "go up" menu option */
	if (command[1] == '0') {
		if (strcmp(menu->id, "") == 0) {
			fputs("There is no option 0!\n", stdout);
			return menu;
		}
		return menu->parent;
	}

	Entry *new = extract_num(&command, menu);
	if (new == NULL) {
		return menu;
	}
	if (new->kind != KIND_MENU) {
		fputs("This is not a menu!\n", stdout);
		return menu;
	}
	return new;
}

void print_description(char *command, Entry *menu) {
	Entry *opt = extract_num(&command, menu);
	if (opt != NULL) {
		fputs(opt->full_desc, stdout);
	}
}

void set_opt_value(char *command, Entry *menu) {
	Entry *opt = extract_num(&command, menu);
	if (opt == NULL) {
		return;
	}
	if (opt->kind != KIND_OPTION) {
		fputs("Cannot set a menu's value!\n", stdout);
		return;
	}

	/* Remove the newline */
	char *newline = strchr(command, '\n');
	newline[0] = '\0';
	set_val(opt, command);
}

void print_opt_value(Entry *opt) {
	print_short_desc(opt->short_desc);

	fputs(": ", stdout);
	if (opt->val == NULL) {
		fputs("unset", stdout);
	} else {
		fputs(opt->val, stdout);
	}
	fputc('\n', stdout);
}

void get_opt_value(char *command, Entry *menu) {
	Entry *opt = extract_num(&command, menu);
	if (opt == NULL) {
		return;
	}
	if (opt->kind != KIND_OPTION) {
		fputs("Cannot get a menu's value!\n", stdout);
		return;
	}

	print_opt_value(opt);
}

void get_all_values(Entry *menu) {
	Entry *current;
	for (current = menu->children; current != NULL; current = current->next) {
		if (current->kind == KIND_OPTION) {
			print_opt_value(current);
		}
	}
}

void reset_value(char *command, Entry *menu) {
	Entry *opt = extract_num(&command, menu);
	if (opt == NULL) {
		return;
	}
	if (opt->kind != KIND_OPTION) {
		fputs("Cannot reset a menu's value!\n", stdout);
		return;
	}
	opt->val = opt->default_val;
}

void no_input(Entry *head) {
	fputs("You don't seem to be running under Fiwix or Linux currently.\n", stdout);
	fputs("Likely, you are currently running under builder-hex0.\n", stdout);
	fputs("That's ok! We're going to make some assumptions; namely, that you do need\n", stdout);
	fputs("the kernel bootstrap, and that you'll get a chance to configure later.\n", stdout);
	write_cfg_values(head, "KERNEL_BOOTSTRAP=True\nBUILD_KERNELS=True\n", FALSE);
}

int main(int argc, char **argv, char **envp) {
	/*
	 * Check we are being non-interactive and bootstrap.cfg exists in
	 * which case we do not need to do anything.
	 */
	char *interactivity = getenv("CONFIGURATOR");
	if (interactivity != NULL) {
		if (strcmp(interactivity, "False") == 0) {
			return 0;
		}
	}
	FILE *bootstrap_cfg = fopen("/steps/bootstrap.cfg", "r");
	if (bootstrap_cfg != NULL) {
		char *line = calloc(MAX_STRING, sizeof(char));
		while (fgets(line, MAX_STRING, bootstrap_cfg) != NULL) {
			if (strcmp(line, "CONFIGURATOR=False\n") == 0) {
				fclose(bootstrap_cfg);
				return 0;
			}
			free(line);
			line = calloc(MAX_STRING, sizeof(char));
		}
		fclose(bootstrap_cfg);
	}

	if (argc != 2) {
		fputs("Usage: ", stdout);
		fputs(argv[0], stdout);
		fputs(" <configuration>\n", stdout);
		exit(1);
	}
	Entry *head = read_config(argv[1]);
	char *extra = set_cfg_values(head, envp);

	/*
	 * Check if we are NOT running under fiwix or linux.
	 * If we are not, and need configuration to occur, then we presume that
	 * we will not be able to get any input from the user.
	 */
	struct utsname *kernel = calloc(1, sizeof(struct utsname));
	uname(kernel);
	if (kernel->sysname == NULL) {
		no_input(head);
		return 0;
	} else if (strcmp(kernel->sysname, "Linux") != 0 && strcmp(kernel->sysname, "Fiwix") != 0) {
		no_input(head);
		return 0;
	}

	fputs("Welcome to live-bootstrap!\n", stdout);
	fputs("We need to do some brief configuration before continuing.\n\n", stdout);
	how_to_use();
	fputc('\n', stdout);

	Entry *menu = head;
	print_menu(menu, menu == head);
	char *command = calloc(MAX_STRING, sizeof(char));
	fputs("\nCommand: ", stdout);
	fflush(stdout);
	command = fgets(command, MAX_STRING, stdin);
	while (command != NULL) {
		if (strcmp("h\n", command) == 0 || strcmp("help\n", command) == 0) {
			how_to_use();
		} else if (strcmp("l\n", command) == 0 || strcmp("list\n", command) == 0) {
			print_menu(menu, menu == head);
		} else if (strncmp("o ", command, 2) == 0 || strncmp("open ", command, 5) == 0) {
			menu = submenu(command, menu, head);
			print_menu(menu, menu == head);
		} else if (strncmp("? ", command, 2) == 0 || strncmp("describe ", command, 9) == 0) {
			print_description(command, menu);
		} else if (strcmp("g all\n", command) == 0 || strcmp("get all\n", command) == 0) {
			get_all_values(menu);
		} else if (strncmp("g ", command, 2) == 0 || strncmp("get ", command, 4) == 0) {
			get_opt_value(command, menu);
		} else if (strncmp("s ", command, 2) == 0 || strncmp("set ", command, 4) == 0) {
			set_opt_value(command, menu);
		} else if (strncmp("r ", command, 2) == 0 || strncmp("reset ", command, 6) == 0) {
			reset_value(command, menu);
		} else if (strcmp("e\n", command) == 0 || strcmp("exit\n", command) == 0) {
			if (!any_unset(head)) {
				break;
			}
		} else {
			fputs("Unknown command ", stdout);
			fputs(command, stdout);
		}
		fputs("\nCommand: ", stdout);
		fflush(stdout);
		/*
		 * M2-Planet's fgets does not properly terminate the buffer if there is
		 * already data in it
		 */
		free(command);
		command = calloc(MAX_STRING, sizeof(char));
		command = fgets(command, MAX_STRING, stdin);
	}

	if (any_unset(head)) {
		fputs(
			"Uh oh! You have left me in a tough position - you can't input further because you\n"
			"closed the input stream. But the inputs you gave me are not valid!\n"
			"I'm going to re-exec myself and hope you are able to start again from scratch.\n",
			stderr
		);
		execve(argv[0], argv, envp);
		return 0;
	}

	write_cfg_values(head, extra, TRUE);

	fputs("\nThank you! We will now continue with the bootstrap.\n", stdout);

	return 0;
}
