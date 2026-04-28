SRCS=		audit-balances.zok
SRC_MAIN=	audit-balances.zok

PROGRAM=	out
INPUT=		input.json

PROVING_SCHEME=	gm17

TIME=           time -p

ZOKRATES=	zokrates
COMPILE_FLAGS=	--debug
WITNESS_FLAGS=  --verbose
SETUP_FLAGS=	-s ${PROVING_SCHEME}
PROOF_FLAGS=	-s ${PROVING_SCHEME}

PROVING_KEY=	proving.key


all: proof.json

proof.json: witness
	@${TIME} ${ZOKRATES} generate-proof ${PROOF_FLAGS}

witness: ${PROVING_KEY} ${INPUT}
	@${TIME} ${ZOKRATES} compute-witness ${WITNESS_FLAGS} \
	    --abi --stdin < ${INPUT}

${PROVING_KEY}: ${PROGRAM}
	@${TIME} ${ZOKRATES} setup ${SETUP_FLAGS}

${PROGRAM}: ${SRCS} ${SRC_MAIN}
	@${TIME} ${ZOKRATES} compile ${COMPILE_FLAGS} -i ${SRC_MAIN}


verify:
	@${TIME} ${ZOKRATES} verify


clean:
	rm -f abi.json out out.r1cs out.wtns proof.json proving.key \
	    verification.key witness

.PHONY: verify clean
