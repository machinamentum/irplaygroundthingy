
library "glfw";

func glfwInit();
func glfwCreateWindow(w: i32, h: i32, title: *i8, unk: *i8, unk1: *i8) -> *i8;
func glfwTerminate();
func glfwMakeContextCurrent(window: *i8);
func glfwWindowShouldClose(window: *i8) -> i8;
func glfwSwapBuffers(window: *i8);
func glfwPollEvents();

func glClear(flags: i32);
func glClearColor(r: f32, g: f32, b: f32, a: f32);

func glBegin(flag: i32);
func glEnd();
func glVertex3f(x: f32, y: f32, z: f32);

func main() {
    
    glfwInit();

    var window: *i8 = glfwCreateWindow(640, 480, "test", 0, 0);
    glfwMakeContextCurrent(window);

    // One-minus trick since there arent booleans, comparisons, or negation
    // instructions in the IR library yet... lol
    while (1 - glfwWindowShouldClose(window)) {
        glClearColor(1.0f, 0.0f, 0.0f, 1.0f);
        glClear(16384); // 0x4000

        glBegin(4); // GL_TRIANGLES
        glVertex3f(0.0f, 0.0f, 0.0f);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glVertex3f(1.0f, 1.0f, 0.0f);
        glEnd();

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwTerminate();
}