CFLAGS+=	-Wall -Werror -D_GNU_SOURCE -std=c99
CSRC=		src/helpers.c
EXCL=		ostumake tests
PKG=		bigarray compiler-libs.bytecomp compiler-libs.common
PKG+=		compiler-libs.toplevel
PROG=		tamasheq

tests:		$(PROG)
	./run_tests.sh

include ostumake/gnu.ocaml.prog.mk
