
// Test

func printf();

func test() {
    printf("hello world!\n");
}

func main() {
    test();

    var i: i32 = 123;

    i = i + 1;
    i = i * 2;

    // Forward slash is signed division
    i = i / 2;

    // Back slash is unsigned division
    i = i \ 2;
    printf("i: %d\n", i);

    // unary * takes address
    var p: *i32 = *i;

    // unary < dereferences
    <p = 456;
    printf("i: %d\n", i);
}
