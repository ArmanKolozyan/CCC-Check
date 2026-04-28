#!/usr/bin/env python3
"""Generate .pin and .vin files for Noir test programs.

Usage: python3 scripts/noir_gen_witness.py examples/Noir/pf/<test>/target/<test>.json

Requires nargo to be installed. Runs `nargo execute` to solve the witness,
then converts to CirC value map format (.pin/.vin).
"""

import json
import gzip
import struct
import subprocess
import sys
import os

BN254_MODULUS = "21888242871839275222246405745257275088548364400416034343698204186575808495617"


def parse_witness_gz(gz_path):
    """Parse a nargo witness .gz file into a dict of {index: hex_value}."""
    with gzip.open(gz_path, 'rb') as f:
        data = f.read()

    witnesses = {}
    # The format is a bincode-serialized WitnessStack.
    # WitnessStack { stack: Vec<StackItem> }
    # StackItem { index: u32, witness: WitnessMap }
    # WitnessMap is a BTreeMap<Witness, FieldElement>
    # In bincode: vec length (u64), then pairs of (Witness(u32), FieldElement(hex string))
    #
    # Actually looking at the raw bytes more carefully:
    # The WitnessStack has a version byte followed by the serialized data.
    # Let's parse it step by step.

    offset = 0

    # Skip version/header - try to find the actual witness data
    # Format seems to be: 4 bytes version, then bincode Vec
    # Let's try parsing as bincode directly

    # Read stack length (bincode u64)
    if len(data) < 8:
        return witnesses

    stack_len = struct.unpack('<Q', data[offset:offset+8])[0]
    offset += 8

    for _ in range(stack_len):
        if offset + 4 > len(data):
            break
        # StackItem.index: u32
        _item_index = struct.unpack('<I', data[offset:offset+4])[0]
        offset += 4

        # StackItem.witness: WitnessMap (BTreeMap<Witness, FieldElement>)
        # BTreeMap in bincode: length (u64), then key-value pairs
        if offset + 8 > len(data):
            break
        map_len = struct.unpack('<Q', data[offset:offset+8])[0]
        offset += 8

        for _ in range(map_len):
            if offset + 4 > len(data):
                break
            # Witness index (u32)
            w_idx = struct.unpack('<I', data[offset:offset+4])[0]
            offset += 4

            # FieldElement: serialized as a hex string (bincode string = u64 len + bytes)
            if offset + 8 > len(data):
                break
            str_len = struct.unpack('<Q', data[offset:offset+8])[0]
            offset += 8

            if offset + str_len > len(data):
                break
            hex_val = data[offset:offset+str_len].decode('ascii')
            offset += str_len

            witnesses[w_idx] = hex_val

    return witnesses


def get_public_witnesses(artifact_path):
    """Get the set of public witness indices from the ACIR artifact."""
    with open(artifact_path) as f:
        data = json.load(f)

    import base64
    bytecode_b64 = data['bytecode']
    # We can't easily parse ACIR in Python, so we'll use the ABI instead
    # The ABI tells us which parameters are public
    abi = data.get('abi', {})

    # For now, we need to know which witness indices are public.
    # This requires parsing the ACIR circuit, which is complex in Python.
    # Instead, we'll use a simpler approach: just look at the witness count
    # and determine public/private from the ABI layout.

    # Count total parameters to figure out witness mapping
    public_indices = set()
    private_indices = set()
    idx = 0  # Noir witness 0 maps to main_w0

    for param in abi.get('parameters', []):
        count = count_fields(param['type'])
        for i in range(count):
            if param['visibility'] == 'public':
                public_indices.add(idx)
            else:
                private_indices.add(idx)
            idx += 1

    return public_indices


def count_fields(typ):
    """Count the number of field elements in a Noir ABI type."""
    kind = typ['kind']
    if kind == 'field':
        return 1
    elif kind == 'integer':
        return 1
    elif kind == 'boolean':
        return 1
    elif kind == 'array':
        return typ['length'] * count_fields(typ['type'])
    elif kind == 'struct':
        return sum(count_fields(f['type']) for f in typ['fields'])
    elif kind == 'tuple':
        return sum(count_fields(t) for t in typ['fields'])
    elif kind == 'string':
        return typ['length']
    else:
        raise ValueError(f"Unknown ABI type: {kind}")


def hex_to_decimal(hex_str):
    """Convert a hex string (possibly with leading zeros) to decimal."""
    if not hex_str or hex_str == '0' * len(hex_str):
        return '0'
    return str(int(hex_str, 16))


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    artifact_path = sys.argv[1]
    if not os.path.exists(artifact_path):
        print(f"Error: {artifact_path} not found")
        sys.exit(1)

    # Find the nargo project directory
    target_dir = os.path.dirname(artifact_path)
    project_dir = os.path.dirname(target_dir)
    project_name = os.path.basename(project_dir)

    # Run nargo execute to generate the witness
    print(f"Running nargo execute for {project_name}...")
    result = subprocess.run(
        ['nargo', 'execute'],
        cwd=project_dir,
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print(f"nargo execute failed: {result.stderr}")
        sys.exit(1)
    print(result.stdout.strip())

    # Parse the witness
    gz_path = os.path.join(target_dir, f"{project_name}.gz")
    if not os.path.exists(gz_path):
        print(f"Error: witness file {gz_path} not found")
        sys.exit(1)

    witnesses = parse_witness_gz(gz_path)
    print(f"Found {len(witnesses)} witness values")

    # Get public witness indices
    public_indices = get_public_witnesses(artifact_path)
    print(f"Public witnesses: {sorted(public_indices)}")

    # Generate .pin file (all witnesses)
    pin_path = artifact_path + '.pin'
    vin_path = artifact_path + '.vin'

    with open(pin_path, 'w') as f:
        f.write("(let (\n")
        for idx in sorted(witnesses.keys()):
            dec_val = hex_to_decimal(witnesses[idx])
            f.write(f"    (main_w{idx} #f{dec_val}m{BN254_MODULUS})\n")
        f.write(")\n    false\n)\n")

    with open(vin_path, 'w') as f:
        f.write("(let (\n")
        for idx in sorted(witnesses.keys()):
            if idx in public_indices:
                dec_val = hex_to_decimal(witnesses[idx])
                f.write(f"    (main_w{idx} #f{dec_val}m{BN254_MODULUS})\n")
        f.write(")\n    false\n)\n")

    print(f"Generated {pin_path}")
    print(f"Generated {vin_path}")


if __name__ == '__main__':
    main()
