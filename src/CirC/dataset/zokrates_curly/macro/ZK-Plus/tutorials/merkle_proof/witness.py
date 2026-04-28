import hashlib


def hash(value: bytes) -> bytes:
    return hashlib.sha256(value).digest()


def hash_to_u32(val: bytes) -> str:
    M0 = val.hex()[:128]
    b0 = [str(int(M0[i:i+8], 16)) for i in range(0,len(M0), 8)]
    return " ".join(b0)


if __name__ == '__main__':
    original_data = [1337, 7, 1989, 51966, 1234, 9999, 0, 6]
    hashed_leafs = [hash(int.to_bytes(leaf, 64, "big")) for leaf in original_data]

    h0 = hash(hashed_leafs[0] + hashed_leafs[1])
    h1 = hash(hashed_leafs[2] + hashed_leafs[3])
    h2 = hash(hashed_leafs[4] + hashed_leafs[5])
    h3 = hash(hashed_leafs[6] + hashed_leafs[7])

    h00 = hashlib.sha256(h0 + h1).digest()
    h01 = hashlib.sha256(h2 + h3).digest()

    root = hashlib.sha256(h00 + h01).digest()

    directionSelector = "1 0 0"

    path = " ".join([hash_to_u32(node) for node in [hashed_leafs[0], h1, h01]])

    print(hash_to_u32(root) + " " + hash_to_u32(hashed_leafs[1]) + " " + directionSelector + " " + path)
