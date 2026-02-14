SRCS=		u32_8_tools-test.zok
TESTS=          ${SRCS:.zok=.ok}

ZOKRATES=	zokrates
COMPILE_FLAGS=	--debug


all: test

test: ${TESTS}

.SUFFIXES: .zok .ok
.zok.ok:
	@${ZOKRATES} compile ${COMPILE_FLAGS} -i $< \
	    >/dev/null \
	    && echo '$< OK' \
	    || ${ZOKRATES} compile ${COMPILE_FLAGS} -i $<
	@rm out out.r1cs abi.json

clean:
	rm -f ${TESTS}

.PHONY: test clean
