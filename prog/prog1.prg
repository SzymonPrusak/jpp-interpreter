


fun int main()
{
    fun int main()
    {
        fun int main() {}
    }
}

fun void chuj( string readonly bbb, bool c)
{
    ;
    ;
    ;
    c = false;
}

fun (int, bool) tuple()
{
    return ~( 1, true )~;
}

fun bool[] arr() {}

fun (int[], string)[] mix() {}

fun void stmt()
{
    ;
    int a;
     string readonly c;
    a = 18;
     bool readonly b = true;

     string[] readonly d = new string[a];

    (int, bool) e = ~( 1, true )~;
    int[] f = [1, 2, 3];

     int readonly x = f[2];
}


fun bool decon()
{
    ~(int a, string b, ~(bool[] a, int[] c)~)~ = ~( 1, "a", ~( [true, false], [1, 2, 4, 8] )~ )~;
    return true;
}

fun void funCall()
{
    decon();
    chuj("aa", decon());
}

fun void tryIf()
{
    if (true) funCall();
    bool a = false;
    if (1 == 5) if(false) {} else ;
}

fun void printS(string s) {}
fun void printI(int i) {}

fun void tryLoops()
{
    while (true)
        printS("noreturn");

    for (i = 0 to 10)
    {
        printI(i);
        if (false)
        {
            int i2 = i % 10;
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
    if (false)
    {
        fun int test(int x) {}
        test(1);
    }

    fun void proc() { printS("from nested"); }
    proc();
}


fun void testArith()
{
    bool a = 1 > 5 && 5 < 4;
}
