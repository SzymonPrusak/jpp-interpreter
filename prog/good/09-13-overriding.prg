

fun void a()
{
    printLnS("outer");
}


fun void b()
{
    fun void c()
    {
        fun void a()
        {
            printLnS("inner");
        }

        a();
    }

    a();
    c();
}


fun void main()
{
    b();
}
