import hashlib


def blake2s(preimage: bytearray):
    return hashlib.blake2s(preimage).digest()

def blake2s_to_u32_array(val: bytearray) -> list[str]:
    array_arrays = [val[i:i + 4] for i in range(0, len(val), 4)]
    u32_array = [str(int.from_bytes(x, 'big', signed=False)) for x in array_arrays]
    return u32_array

msg = (5).to_bytes(64, "big", signed=False)
hash = blake2s(msg)

print(*blake2s_to_u32_array(hash), *blake2s_to_u32_array(msg))