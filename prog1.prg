


fun int main()
{

}

fun void chuj(readonly string bbb, bool c)
{
    ;
    ;
    ;

}

fun (int, bool) tuple()
{
    return (= 1, true =);
}

fun bool[] arr() {}

fun (int[], string)[] mix() {}

fun void stmt()
{
    ;
    int a;
    readonly string c;
    g = 18;
    readonly bool a = true;

    readonly string[] a = new string[aaa];

    (int, bool) a = (= 1, true =);
    int[] c = [1, 2, 3];

    readonly int x = c[2];
}


fun void decon()
{
    deconstr (int a, string b, deconstr (bool[] a, int[] c)) = (= 1, "a", (= [true, false], [1, 2, 4, 8] =) =);
}

fun void funCall()
{
    fun1();
    fun2(aa, 1, fun3(xd));
}

fun void tryIf()
{
    if (aa) xd();
    if (bc) if(xddd) {} else ;
}

fun void tryLoops()
{
    while (1)
        print("noreturn");

    for (i = 0 to x)
    {
        print(i);
        if (testI(i))
        {
            i = div(xd, 10);
            return;
        }
    }

    while (true) {
        break;
    }
    
    while (false) {
        continue;
    }
}


fun void nestedFun()
{
    if (a)
    {
        fun int test(int x) {}
        test(1);
    }

    fun void proc() { print("from nested"); }
    proc();
}


fun void testArith()
{
    int a = 1 > 5 && 5 < 4;
}
