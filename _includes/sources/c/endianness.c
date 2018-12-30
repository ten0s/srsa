#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

typedef enum {
    BIG,
    LITTLE
} end_t;

end_t endianness() {
    // SOLUTION_BEGIN
    union u_t {
        uint8_t u8; uint16_t u16; uint32_t u32; uint64_t u64;
    } u = { .u64 = 0x4A };
    if (u.u8 == u.u16 && u.u8 == u.u32 && u.u8 == u.u64) {
        return LITTLE;
    } else {
        return BIG;
    }
    // SOLUTION_END
}

int main(void) {
    end_t e = endianness();
    if (e == BIG) {
        printf("big-endian\\n");
    } else if (e == LITTLE) {
        printf("little-endian\\n");
    } else {
        printf("unknown\\n");
    }
    return 0;
}
