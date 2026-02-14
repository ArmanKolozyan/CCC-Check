import hashlib


def sha256(preimage: bytearray):
    return hashlib.sha256(preimage).digest()

def sha256_to_u32_array(val: bytearray) -> list[str]:
    array_arrays = [val[i:i + 4] for i in range(0, len(val), 4)]
    u32_array = [str(int.from_bytes(x, 'big', signed=False)) for x in array_arrays]
    return u32_array

msg = (5).to_bytes(64, "big", signed=False)
hash = sha256(msg)

print(*sha256_to_u32_array(hash), *sha256_to_u32_array(msg))