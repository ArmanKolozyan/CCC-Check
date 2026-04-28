# Experiments with ZoKrates

All proofs have been tested using the G16 system.

## What you need to run the code

You need to have ZoKrates CLI installed: https://zokrates.github.io/gettingstarted.html
Make sure the stdlib env variable is set:
```bash
# Your path may be different
export ZOKRATES_STDLIB=~/.zokrates/stdlib
```

You need to have the following Python library cloned and installed: https://github.com/Zokrates/pycrypto/tree/master

```bash
git clone https://github.com/Zokrates/pycrypto.git
cd pycrypto
pip install -r requirements.txt
```

## How to generate EdDSA sigs

From within the pycrypto repo you can generate an EdDSA signature:

```bash
# Generate private key (field element) and public key (point on BabyJubJub curve)
python cli.py keygen
# => Private and public key
# => 37e334c51386a5c92152f592ef264b82ad52cf2bbfb6cee1c363e67be97732a ab466cd8924518f07172c0f8c695c60f77c11357b461d787ef31864a163f3995

# Generate EdDSA signature
# python cli.py sig-gen <pvt_key> <message>
python cli.py sig-gen 37e334c51386a5c92152f592ef264b82ad52cf2bbfb6cee1c363e67be97732a 11dd22
# => Signature_R Signature_S
# => 172a1794976d7d0272148c4be3b7ad74fd3a82376cd5995fc4d274e3593c0e6c 24e96be628208a9800336d23bd31318d8a9b95bc9bd8f6f01cae207c05062523

# Verify EdDSA signature
# python cli.py sig-verify <public_key> <message> <signature_r> <signature_s>
python cli.py sig-verify ab466cd8924518f07172c0f8c695c60f77c11357b461d787ef31864a163f3995 11dd22 172a1794976d7d0272148c4be3b7ad74fd3a82376cd5995fc4d274e3593c0e6c 24e96be628208a9800336d23bd31318d8a9b95bc9bd8f6f01cae207c05062523
# => True
```

## How to generate experimental EdDSA sig snark proof

From within the eddsa directory in this repo you can generate the ZoKrates input values for the CLI..

```bash
# creates file eddsa_verification_zokrates_cli_inputs.txt
python main.py
```

..which you can then use to compute a witness, generate a proof, and verify it:

```bash
# produces files: out, out.r1cs, abi.json
zokrates compile -i eddsa_verification.zok

# produces files: proving.key & verification.key
zokrates setup

# produces file: witness
cat eddsa_verification_zokrates_cli_inputs.txt | xargs zokrates compute-witness -a

# produces file: proof.json
zokrates generate-proof

zokrates verify
```

## How to generate experimental array handling snark proof

From within the arrays directory do the following:

```bash
# produces files: out, out.r1cs, abi.json
zokrates compile -i array_handling.zok

# produces files: proving.key & verification.key
zokrates setup

# order of parameters: keys[0], keys[1], ownership, addresses, balances
zokrates compute-witness -a \
0 0 0 5 \
0 0 0 0 \
0 0 0 1 \
0 0 0 5 \
2 2 2 3

# produces file: proof.json
zokrates generate-proof

zokrates verify
```

## How to generate keccak snark proof

From within the hashes directory do the following:

```bash
# produces files: out, out.r1cs, abi.json
zokrates compile -i keccak256.zok

# produces files: proving.key & verification.key
zokrates setup

# produces file: witness
zokrates compute-witness -a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# produces file: proof.json
zokrates generate-proof

zokrates verify
```

## How to generate PoA snark proof

From within the poa directory in this repo you can generate the ZoKrates input values for the CLI..

```bash
# creates file poa_zokrates_cli_inputs.txt
# note this only works for N=2
python main.py
```

..which you can then use to compute a witness, generate a proof, and verify it:

```bash
# produces files: out, out.r1cs, abi.json
zokrates compile -i poa.zok

# produces files: proving.key & verification.key
zokrates setup

# produces file: witness
cat poa_zokrates_cli_inputs.txt | xargs zokrates compute-witness -a

# produces file: proof.json
zokrates generate-proof

zokrates verify
```
