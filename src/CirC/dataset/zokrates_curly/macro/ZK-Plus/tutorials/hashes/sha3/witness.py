import hashlib


def sha3(preimage: bytearray):
    return hashlib.sha3_256(preimage).digest()

def sha3_to_u8_array(val: bytearray) -> list[str]:
    array_arrays = [val[i] for i in range(0, len(val))]
    u32_array = [str(x) for x in array_arrays]
    return u32_array

msg = (5).to_bytes(64, "big", signed=False)
hash = sha3(msg)

print(*sha3_to_u8_array(hash), *sha3_to_u8_array(msg))