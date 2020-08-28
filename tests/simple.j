
func printf();

func test() {
    printf("hello world!\n");
}

func main() {
    test();

    var i: int = 123;

    i = i + 1;
    printf("i: %d\n", i);
}
