EMACS = emacs

PROJECT_ROOT_DIR = $(CURDIR)

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file -L $(PROJECT_ROOT_DIR)

SRCS := $(wildcard *.el)
OBJS := $(SRCS:.el=.elc)

.PHONY: all clean compile

all: compile

compile: $(OBJS)

clean:
	-rm -f $(OBJS)

$(OBJS): %.elc: %.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile $^

