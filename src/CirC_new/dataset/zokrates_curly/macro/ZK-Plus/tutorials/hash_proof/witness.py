from hashlib import sha256


def str_to_512bits(value: str) -> bytes:
    bin_str: int = int(''.join(format(i, '08b') for i in value.encode("utf-8")), base=2)
    padded_bytes: bytes = bin_str.to_bytes(64, "big")
    return padded_bytes


def bytes_to_u32(val: bytes) -> [int]:
    b0 = [str(int.from_bytes(val[i:i+4], "big")) for i in range(0,len(val), 4)]
    return " ".join(b0)


preimage: bytes = str_to_512bits("super secret value")
hash: bytes = sha256(preimage).digest()

print(f"preimage: 0x{preimage.hex()}")
print(f"hash: 0x{hash.hex()}")
print(f"preimage as u32 array: {bytes_to_u32(preimage)}")
print(f"hash as u32 array: {bytes_to_u32(hash)}")
