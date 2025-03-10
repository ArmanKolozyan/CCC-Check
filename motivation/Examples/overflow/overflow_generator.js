// THIS CODE IS BASED ON: https://gist.github.com/LCamel/4638804256815beb78e672b3716d0626

const p = 23;
const N = 2;
var fp = []; // false positive

for (var x = 0; x < p; x++) {
    fp[x] = [];
    for (var y = 0; y < p; y++) {
        fp[x][y] = ' ';
        const diff = (x + (1 << N) - y + p) % p;
        if (diff < (1 << (N + 1))) {       // can pass Num2Bits
            fp[x][y] = 'B';
            if ((diff & (1 << N)) == 0) {  // the logic of LessThan for "true"
                fp[x][y] = 'L';
                if (x >= y) {           // false positive
                    fp[x][y] = 'F';
                }
            }
        }
    }
}

// printing grid

console.log("\nB: can pass Num2Bits   L: and LessThan == 1   F: but false positive\n");

// printing column numbers (X-axis)
process.stdout.write("   ");
for (var x = 0; x < p; x++) {
    process.stdout.write(x.toString().padStart(2, " ") + " ");
}
console.log("\n   " + "-".repeat(3 * p)); // horizontal separator

// printing grid with row numbers
for (var y = 0; y < p; y++) {
    process.stdout.write(y.toString().padStart(2, " ") + "| "); //row label
    for (var x = 0; x < p; x++) {
        process.stdout.write(fp[x][y] + "  ");
    }
    console.log();
}
console.log