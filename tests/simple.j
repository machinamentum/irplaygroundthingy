
// builtin, will translate to an intrinsic call to the IR library's debug interrupt intrinsic
func __debugbreak();

func printf();

func test() {
    printf("hello world!\n");
}

func test2() -> i32 {
    return 234;
}

func main() {
    // __debugbreak();
    test();

    printf("calling test2(): %d\n", test2());


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
